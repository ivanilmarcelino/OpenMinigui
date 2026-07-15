/*
 * =============================================================================
 * Name: Selector Utility
 * Author: Janusz Piwowarski (Original Clipper 5.2e)
 *
 * Description:
 * A high-performance programmatic data extraction and transformation engine.
 * This utility mimics SQL "SELECT INTO" functionality within the Harbour/HMG
 * environment, allowing developers to filter, aggregate (Sum/Average), and
 * group data from DBF work areas into temporary tables. These tables are
 * typically used to power HMG Grid controls or complex reporting modules.
 * =============================================================================
 */

/* 
 * Select List Array (aSelectList) Constants
 * These definitions provide a semantic layer over the multi-dimensional array
 * used to define the target schema and data mapping logic.
 */
#define SEL_BLOCK       1 // Codeblock: Evaluated against source to retrieve data.
#define SEL_NAME        2 // String: Target field name in the destination DBF.
#define SEL_LEN         3 // Numeric: Optional explicit field length.
#define SEL_DEC         4 // Numeric: Optional decimal precision for numeric types.
#define SEL_FLAG        5 // String: Operation mode ('G'=Group, 'T'=Total/Sum, 'A'=Average).
#define SEL_SUMMARY     6 // Logical: If .T., value is accumulated in a global summary array.

/* 
 * One-To-Many Array (aOneToMany) Constants
 * Defines the relational link between a parent work area and a child work area.
 */
#define OTM_CHILD_ALIAS 1 // String: Alias of the child work area.
#define OTM_MOTHER_LINK 2 // Codeblock: Key expression in the parent work area.
#define OTM_CHILD_LINK  3 // Codeblock: Key expression in the child work area.

/*
 * PROCEDURE Selector
 * The primary engine for data extraction and transformation.
 *
 * Parameters:
 *  aSelectList : Array. Field definitions and mapping rules.
 *  xcnFromArea : Numeric/String. Source work area handle or alias.
 *  aOneToMany  : Array. Join logic for parent/child relationships.
 *  lSelect     : Codeblock. Filter condition (SQL "WHERE").
 *  lWhile      : Codeblock. Loop continuation condition (SQL "WHILE").
 *  lRest       : Logical. If .T., processes from current record; else from Top.
 *  xcFile      : String. Path to the target DBF (Default: TEMP.DBF).
 *  xcAlias     : String. Alias for the target work area.
 *  lAdditive   : Logical. If .T., appends to existing target; else recreates.
 *  aSummary    : Array (Ref). Stores running totals for flagged fields.
 *  xEval       : Codeblock. Callback for progress tracking (e.g., HMG ProgressBar).
 *
 * Side Effects:
 *  - Creates/Overwrites physical DBF files.
 *  - Modifies active work area and record pointers.
 *  - Populates the aSummary array if provided.
 */
PROCEDURE Selector( aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval )
   LOCAL nSourceArea
   LOCAL nChildArea := NIL
   LOCAL nTargetArea
   LOCAL aStruct := {}
   LOCAL aGroupExpr := {}
   LOCAL cIndexKey := ""
   LOCAL cIndexName
   LOCAL lCreateIndex := .F.
   LOCAL lAggregate := .F.
   LOCAL lOTM := HB_ISARRAY( aOneToMany )
   LOCAL lEval := HB_ISBLOCK( xEval )
   LOCAL lSummary := ( aSummary != NIL )
   LOCAL nGroupExprCount
   LOCAL xMotherKey

   // Initialize defaults to ensure the engine has valid targets even if parameters are omitted.
   hb_default( @xcFile, "TEMP.DBF" )
   hb_default( @xcAlias, hb_FNameName( xcFile ) )
   hb_default( @lRest, .F. )
   hb_default( @lAdditive, .F. )

   // Generate index filename using the system's default driver extension (.ntx or .cdx).
   cIndexName := hb_FNameExtSet( hb_FNameName( xcFile ), IndexExt() )

   // Switch context to the source work area to begin data evaluation.
   nSourceArea := ResolveSourceArea( xcnFromArea )

   // Identify the child area if a One-To-Many relationship is requested.
   IF lOTM
      nChildArea := Select( aOneToMany[ OTM_CHILD_ALIAS ] )
   ENDIF

   // Analyze the select list to build the DBF structure and determine if indexing is needed for grouping.
   BuildStructure( aSelectList, aStruct, aGroupExpr, @lAggregate, @lCreateIndex, @cIndexKey, aSummary, lSummary, lAdditive )

   nGroupExprCount := Len( aGroupExpr )

   // Create or open the target table. If grouping is used, an index is created to facilitate 'Seek' operations.
   nTargetArea := OpenTargetTable( xcFile, xcAlias, aStruct, lAdditive, lCreateIndex, cIndexName, cIndexKey, nSourceArea )

   // Standard navigation: Start at the beginning unless 'REST' scope is specified.
   IF ! lRest
      dbGoTop()
   ENDIF

   // Synchronize parent to the first valid child record to maintain Inner Join integrity.
   IF lOTM
      AlignParentChild( nSourceArea, nChildArea, aOneToMany, @xMotherKey )
   ENDIF

   // Main processing loop: Iterates through the source data.
   DO WHILE ! Eof() .AND. ( lWhile == NIL .OR. Eval( lWhile ) )

      // Execute callback - ideal for updating HMG UI elements during long-running extractions.
      IF lEval
         Eval( xEval )
      ENDIF

      // Apply the filter condition (the "WHERE" clause).
      IF lSelect == NIL .OR. Eval( lSelect )
         ProcessRecord( aSelectList, aGroupExpr, nGroupExprCount, nTargetArea, lAggregate, aSummary, lSummary )
      ENDIF

      // Move to the next record, handling complex One-To-Many navigation if active.
      NavigateRecords( nSourceArea, nChildArea, aOneToMany, lOTM, @xMotherKey )

   ENDDO

   // Leave the target area selected so the caller can immediately bind it to an HMG Grid or Report.
   dbSelectArea( nTargetArea )
   dbGoTop()
RETURN

/*
 * FUNCTION ResolveSourceArea
 * Determines the work area handle from either a numeric ID or an alias string.
 * Returns: Numeric work area handle.
 */
STATIC FUNCTION ResolveSourceArea( xArea )
   LOCAL nArea := Select()

   IF xArea != NIL
      // Support both numeric handles and string aliases for flexibility.
      nArea := iif( HB_ISNUMERIC( xArea ), xArea, Select( xArea ) )
      dbSelectArea( nArea )
   ENDIF

RETURN nArea

/*
 * PROCEDURE BuildStructure
 * Dynamically constructs the target DBF structure by evaluating source data.
 * This avoids hardcoding schemas and allows for "Schema-on-Read" behavior.
 */
STATIC PROCEDURE BuildStructure( aSelectList, aStruct, aGroupExpr, lAggregate, lCreateIndex, cIndexKey, aSummary, lSummary, lAdditive )
   LOCAL nCount := Len( aSelectList )
   LOCAL n, xVal, cType, cVal, nLenExpr, nDot, cField, nAlias, nLenField, nDec
   LOCAL lHasAvg := .F.
   LOCAL nStart := RecNo()

   // Move to a phantom record (EOF+1) to evaluate expressions safely without triggering data-driven side effects.
   dbGoto( LastRec() + 1 )

   FOR n := 1 TO nCount

      // Evaluate the codeblock to determine the data type and infer length/decimals.
      xVal := Eval( aSelectList[ n, SEL_BLOCK ] )
      cType := ValType( xVal )
      cVal := Transform( xVal, "" )
      nLenExpr := Len( cVal )

      // Clean field names: Remove alias prefixes (e.g., "CUST->NAME" becomes "NAME").
      nAlias := At( "->", aSelectList[ n, SEL_NAME ] )
      cField := iif( nAlias == 0, aSelectList[ n, SEL_NAME ], SubStr( aSelectList[ n, SEL_NAME ], nAlias + 2 ) )

      // Determine field length: Use explicit override if provided, otherwise use inferred length.
      nLenField := iif( aSelectList[ n, SEL_LEN ] != NIL, aSelectList[ n, SEL_LEN ], iif( cType == "D", 8, nLenExpr ) )

      // Determine decimal precision for numeric fields by analyzing the transformed string.
      nDec := iif( aSelectList[ n, SEL_DEC ] != NIL, aSelectList[ n, SEL_DEC ], ;
         iif( cType == "N" .AND. ( nDot := At( ".", cVal ) ) > 0, nLenExpr - nDot, 0 ) )

      // Append the definition to the structure array for dbCreate().
      AAdd( aStruct, { cField, cType, nLenField, nDec } )

      // Flag if the operation requires mathematical aggregation (Sum/Avg).
      IF aSelectList[ n, SEL_FLAG ] $ "TA"
         lAggregate := .T.
      ENDIF

      IF aSelectList[ n, SEL_FLAG ] == "A"
         lHasAvg := .T.
      ENDIF

      // If 'Group By' is requested, build a concatenated index key to identify unique groups.
      IF aSelectList[ n, SEL_FLAG ] == "G"
         IF ! lCreateIndex
            lCreateIndex := .T.
         ELSE
            cIndexKey += "+"
         ENDIF
         cIndexKey += BuildIndexExpr( cField, cType )
         AAdd( aGroupExpr, aSelectList[ n, SEL_BLOCK ] )

      // Initialize the summary array if global totals are requested and we aren't appending to an existing set.
      ELSEIF aSelectList[ n, SEL_SUMMARY ] .AND. lSummary .AND. ! lAdditive
         AAdd( aSummary, 0 )
      ENDIF

   NEXT

   // If calculating averages, add a hidden field to track the denominator (count) for each group.
   IF lHasAvg .AND. ! lAdditive
      AAdd( aStruct, { "_COUNT", "N", 10, 0 } )
   ENDIF

   // Restore the source record pointer.
   dbGoto( nStart )
RETURN

/*
 * FUNCTION BuildIndexExpr
 * Converts field names into string-safe expressions for index key concatenation.
 * This ensures that mixed-type grouping (e.g., Date + Numeric) works correctly.
 */
STATIC FUNCTION BuildIndexExpr( cField, cType )

   SWITCH cType
   CASE "C"
      RETURN cField
   CASE "N"
      RETURN "Str(" + cField + ")"
   CASE "D"
      RETURN "DToS(" + cField + ")"
   CASE "L"
      RETURN 'Iif(' + cField + ',"1","0")'
   ENDSWITCH
RETURN cField

/*
 * FUNCTION OpenTargetTable
 * Manages the physical creation and opening of the temporary result set.
 */
STATIC FUNCTION OpenTargetTable( xcFile, xcAlias, aStruct, lAdditive, lCreateIndex, cIndexName, cIndexKey, nSource )
   LOCAL nArea

   IF lAdditive
      // If additive mode is on, we assume the table is already open and positioned.
      nArea := Select( xcAlias )
   ELSE
      // Create a new physical DBF based on the dynamic structure.
      dbCreate( xcFile, aStruct )
      dbUseArea( .T., NIL, xcFile, xcAlias, .F. )
      nArea := Select()

      // Create the index required for grouping/aggregation logic.
      IF lCreateIndex
         dbCreateIndex( cIndexName, cIndexKey, hb_macroBlock( cIndexKey ) )
      ENDIF

      // Return focus to the source area to begin the data pump.
      dbSelectArea( nSource )
   ENDIF
RETURN nArea

/*
 * PROCEDURE ProcessRecord
 * The core logic for data transfer, grouping, and mathematical aggregation.
 */
STATIC PROCEDURE ProcessRecord( aSelectList, aGroupExpr, nGroupExprCount, nTargetArea, lAggregate, aSummary, lSummary )
   LOCAL lAppend
   LOCAL n, xVal, nSummaryIndex := 0
   LOCAL nFields := Len( aSelectList )
   LOCAL nCountField := ( nTargetArea )->( FieldPos( "_COUNT" ) )
   LOCAL cSeekKey := ""
   LOCAL nPrevCount, nNewCount, nPrevSum

   // Determine if we are creating a new record or updating an existing group.
   IF nGroupExprCount == 0
      IF lAggregate
         // Global aggregation (no grouping): Always update the first record.
         IF ( nTargetArea )->( LastRec() ) == 0
            ( nTargetArea )->( dbAppend() )
            lAppend := .T.
         ELSE
            ( nTargetArea )->( dbGoTop() )
            lAppend := .F.
         ENDIF
      ELSE
         // Standard selection: Every source record creates a new target record.
         ( nTargetArea )->( dbAppend() )
         lAppend := .T.
      ENDIF
   ELSE
      // Grouping logic: Build the seek key from the current source record values.
      FOR n := 1 TO nGroupExprCount
         cSeekKey += ToString( Eval( aGroupExpr[ n ] ) )
      NEXT

      // Seek the group in the target. If not found, create a new group record.
      IF !( nTargetArea )->( dbSeek( cSeekKey, .F. ) )
         ( nTargetArea )->( dbAppend() )
         lAppend := .T.
      ELSE
         lAppend := .F.
      ENDIF
   ENDIF

   // Map source data to target fields and perform calculations.
   FOR n := 1 TO nFields

      IF aSelectList[ n, SEL_SUMMARY ]
         nSummaryIndex++
      ENDIF

      xVal := Eval( aSelectList[ n, SEL_BLOCK ] )

      SWITCH aSelectList[ n, SEL_FLAG ]
      CASE "T" // Total (Summation)
         ( nTargetArea )->( FieldPut( n, ( nTargetArea )->( FieldGet( n ) ) + xVal ) )
         EXIT

      CASE "A" // Average (Running Mean)
         IF lAppend
            // Initialize the average with the first value.
            ( nTargetArea )->( FieldPut( n, xVal ) )
            IF nCountField > 0
               ( nTargetArea )->( FieldPut( nCountField, 1 ) )
            ENDIF
         ELSE
            // Recalculate the mean: ( (OldAvg * OldCount) + NewVal ) / NewCount.
            nPrevCount := ( nTargetArea )->( FieldGet( nCountField ) )
            nNewCount := nPrevCount + 1
            nPrevSum := ( nTargetArea )->( FieldGet( n ) ) * nPrevCount
            ( nTargetArea )->( FieldPut( n, ( nPrevSum + xVal ) / nNewCount ) )
            ( nTargetArea )->( FieldPut( nCountField, nNewCount ) )
         ENDIF
         EXIT

      OTHERWISE // Standard assignment for non-aggregated or grouping fields.
         ( nTargetArea )->( FieldPut( n, xVal ) )
      ENDSWITCH

      // Update the global summary array if the field is flagged for summary reporting.
      IF aSelectList[ n, SEL_SUMMARY ] .AND. lSummary
         aSummary[ nSummaryIndex ] += xVal
      ENDIF

   NEXT
RETURN

/*
 * PROCEDURE AlignParentChild
 * Implements "Inner Join" behavior by skipping parent records that do not
 * have a corresponding match in the child work area.
 */
STATIC PROCEDURE AlignParentChild( nSourceArea, nChildArea, aOneToMany, xMotherKey )

   DO WHILE !( nSourceArea )->( Eof() ) .AND. ;
         !( nChildArea )->( dbSeek( xMotherKey := ( nSourceArea )->( Eval( aOneToMany[ OTM_MOTHER_LINK ] ) ), .F. ) )
      ( nSourceArea )->( dbSkip() )
   ENDDO
RETURN

/*
 * PROCEDURE NavigateRecords
 * Handles iteration logic. In One-To-Many mode, it ensures all child records
 * are processed before moving the parent record pointer.
 */
STATIC PROCEDURE NavigateRecords( nSourceArea, nChildArea, aOneToMany, lOTM, xMotherKey )

   IF ! lOTM
      // Standard single-table navigation.
      ( nSourceArea )->( dbSkip() )
   ELSE
      // Move to the next child record in the join.
      ( nChildArea )->( dbSkip() )

      // If the child pointer reaches EOF or the link key changes, move the parent.
      IF ( nChildArea )->( Eof() ) .OR. xMotherKey != ( nChildArea )->( Eval( aOneToMany[ OTM_CHILD_LINK ] ) )
         ( nSourceArea )->( dbSkip() )

         // Re-align parent to the next valid child (Inner Join logic).
         DO WHILE !( nSourceArea )->( Eof() ) .AND. ;
               !( nChildArea )->( dbSeek( ( nSourceArea )->( Eval( aOneToMany[ OTM_MOTHER_LINK ] ) ), .F. ) )
            ( nSourceArea )->( dbSkip() )
         ENDDO
      ENDIF
   ENDIF
RETURN

/*
 * FUNCTION ToString
 * Normalizes various Harbour data types into a string format.
 * This is essential for building reliable index keys and performing seeks.
 */
STATIC FUNCTION ToString( xVal )
   LOCAL cType := ValType( xVal )

   SWITCH cType
   CASE "N"
      RETURN Str( xVal )
   CASE "D"
      RETURN DToS( xVal )
   CASE "L"
      RETURN iif( xVal, "1", "0" )
   ENDSWITCH
RETURN xVal
