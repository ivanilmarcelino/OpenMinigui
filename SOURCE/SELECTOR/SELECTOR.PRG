/*
      Name: Selector
    Author: Written in Clipper 5.2e by Janusz Piwowarski <januszp@geocities.com>
   Purpose: Emulates a simplified SQL SELECT statement, extracting data from a database based on specified criteria.
 */

#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := iif( <uVar1> == NIL, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := iif( <uVarN> == NIL, <uValN>, <uVarN> ); ]
/*
 * PROCEDURE Selector( aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval )
 *
 * Purpose:
 *   Executes a Clipper/Harbour-based emulation of a simplified SQL SELECT query.
 *   Extracts, processes, and optionally aggregates data from a DBF table (or a one-to-many related set of tables),
 *   then outputs the results to a new or existing DBF file with optional indexing and summary calculations.
 *
 * Parameters:
 *   aSelectList  (ARRAY)  – Definition of the fields/expressions to select.
 *       Format: { expression (CODEBLOCK), alias (STRING), length (NUMERIC|NIL), decimals (NUMERIC|NIL),
 *                 groupFlag (STRING: "G" for GROUP BY, "T" for total, "A" for average, or NIL),
 *                 summaryFlag (LOGICAL: .T. if included in aSummary) }
 *   xcnFromArea  (NUMERIC|STRING|NIL) – Work area number or alias name of the source DBF. NIL = current work area.
 *   aOneToMany   (ARRAY|NIL) – Defines a one-to-many relationship: { childAlias, parentFieldBlock, childFieldBlock }. NIL = no relation.
 *   lSelect      (CODEBLOCK|NIL) – WHERE condition to filter records. NIL = include all.
 *   lWhile       (CODEBLOCK|NIL) – WHILE condition to limit processing. NIL = process until EOF().
 *   lRest        (LOGICAL) – .T. = start from current record, .F. = start from first record.
 *   xcFile       (STRING|NIL) – Output DBF file path. NIL = "TEMP.DBF".
 *   xcAlias      (STRING|NIL) – Alias for output DBF. Defaults to filename (or "temp").
 *   lAdditive    (LOGICAL) – .T. = append to existing DBF, .F. = create new file.
 *   aSummary     (ARRAY|NIL) – Holds running totals for fields marked in aSelectList[6]. NIL = no summary.
 *   xEval        (CODEBLOCK|NIL) – Optional per-record evaluation before appending/updating.
 *
 * Return:
 *   None (side effects only).
 *   - Creates or appends to a DBF.
 *   - Optionally builds an index.
 *   - Optionally fills aSummary array with totals.
 *   - Changes the active work area to the output table and positions at the first record.
 *
 * Purpose (Rationale):
 *   This function enables SQL-like data extraction and transformation in DBF-based Harbour apps without
 *   requiring an SQL engine. It supports grouping, aggregation, conditional filtering, and joining
 *   (one-to-many), making it useful for reporting and batch data processing in legacy systems.
 *
 * Notes:
 *   - Index creation is only done if GROUP BY fields are specified.
 *   - Grouping is implemented via index key expressions, not SQL semantics.
 *   - aOneToMany assumes child records can be located by dbSeek().
 *   - When lAdditive = .T., structure compatibility between source and target is assumed.
 *   - Large datasets may be slower due to per-record Eval() calls.
 */
PROCEDURE Selector( aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval )

   LOCAL nLenSelectList := Len( aSelectList )
   LOCAL xExprValue, cExprValue, cExprType, nExprLen, aStruct := {}
   LOCAL lOTM := HB_ISARRAY( aOneToMany )
   LOCAL nExprAlias, nCurrExpr, nSelectFrom, nSelectChild, nSelectTemp, xValueFromMother, axExpr := {}
   LOCAL lCreateIndex := .F.
   LOCAL cIndexName, cIndexKey := ""
   LOCAL lAppend, lSummary := !( aSummary == NIL )
   LOCAL nCurrent, cSeekExpr, nStartRec
   LOCAL lEval := HB_ISBLOCK( xEval )

   /* Determine and select the source work area */
   IF xcnFromArea == NIL
      nSelectFrom := Select()
   ELSE
      IF HB_ISNUMERIC( xcnFromArea )
         nSelectFrom := xcnFromArea
      ELSEIF HB_ISSTRING( xcnFromArea )
         nSelectFrom := Select( xcnFromArea )
      ENDIF
      dbSelectArea( nSelectFrom )
   ENDIF

   /* Prepare child work area if one-to-many relationship is defined */
   IF lOTM
      nSelectChild := Select( aOneToMany[ 1 ] )
   ENDIF

   /* Set output file and alias defaults */
   IF xcFile == NIL
      xcFile := "TEMP.DBF"
      IF xcAlias == NIL
         xcAlias := "temp"
      ENDIF
   ELSEIF xcAlias == NIL
      xcAlias := hb_FNameName( xcFile )
   ENDIF

   /* Determine default index filename */
   DEFAULT cIndexName := hb_FNameExtSet( hb_FNameName( xcFile ), IndexExt() )

   /* Bookmark current record and move to EOF to prep for reading */
   nStartRec := RecNo()
   dbGoto( LastRec() + 1 )
   IF lOTM
      ( nSelectChild )->( dbGoto( LastRec() + 1 ) )
   ENDIF

   /* Build output structure and determine grouping/summary fields */
   FOR nCurrExpr := 1 TO nLenSelectList
      xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )
      cExprType := ValType( xExprValue )
      cExprValue := Transform( xExprValue, "" )
      nExprLen := Len( cExprValue )

      AAdd( aStruct, { iif( ( nExprAlias := At( "->", aSelectList[ nCurrExpr, 2 ] ) ) == 0, ;
                            aSelectList[ nCurrExpr, 2 ], ;
                            SubStr( aSelectList[ nCurrExpr, 2 ], nExprAlias + 2 ) ), ;
                            cExprType, ;
                            iif( aSelectList[ nCurrExpr, 3 ] <> NIL, aSelectList[ nCurrExpr, 3 ], ;
                                 iif( cExprType == "D", 8, nExprLen ) ), ;
                            iif( aSelectList[ nCurrExpr, 4 ] <> NIL, aSelectList[ nCurrExpr, 4 ], ;
                                 iif( cExprType == "N" .AND. ( nExprAlias := At( ".", cExprValue ) ) > 0, ;
                                      nExprLen - nExprAlias, 0 ) ) } )

      /* Handle GROUP BY */
      IF aSelectList[ nCurrExpr, 5 ] == "G"
         IF !lCreateIndex
            lCreateIndex := .T.
         ELSE
            cIndexKey += "+"
         ENDIF

         DO CASE
         CASE cExprType == "C"
            cIndexKey += aStruct[ nCurrExpr, 1 ]
         CASE cExprType == "N"
            cIndexKey += ( "Str(" + aStruct[ nCurrExpr, 1 ] + ")" )
         CASE cExprType == "D"
            cIndexKey += ( "DToS(" + aStruct[ nCurrExpr, 1 ] + ")" )
         CASE cExprType == "L"
            cIndexKey += ( "Iif(" + aStruct[ nCurrExpr, 1 ] + "," + Chr( 34 ) + "1" + Chr( 34 ) + "," + Chr( 34 ) + "0" + Chr( 34 ) + ")" )
         ENDCASE

         AAdd( axExpr, aSelectList[ nCurrExpr, 1 ] )

      /* Handle SUMMARY fields */
      ELSEIF aSelectList[ nCurrExpr, 6 ] .AND. ( lSummary .AND. !lAdditive )
         AAdd( aSummary, 0 )
      ENDIF
   NEXT

   dbGoto( nStartRec )

   /* Open output table for writing */
   IF lAdditive
      nSelectTemp := Select( xcAlias )
   ELSE
      dbCreate( xcFile, aStruct )
      dbUseArea( .T., NIL, xcFile, xcAlias, .F. )
      nSelectTemp := Select()
      IF lCreateIndex
         dbCreateIndex( cIndexName, cIndexKey, hb_macroBlock( cIndexKey ) )
      ENDIF
      dbSelectArea( nSelectFrom )
   ENDIF

   IF !lRest
      dbGoTop()
   ENDIF

   /* Align source and child cursors for one-to-many */
   IF lOTM
      DO WHILE !Eof() .AND. !( nSelectChild )->( dbSeek( xValueFromMother := ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )
         dbSkip()
      ENDDO
   ENDIF

   /* Main record processing loop */
   DO WHILE !Eof() .AND. ( lWhile == NIL .OR. Eval( lWhile ) )
      IF lEval
         Eval( xEval )
      ENDIF
      IF lSelect == NIL .OR. Eval( lSelect )
         lAppend := .F.
         lCreateIndex := !lCreateIndex
         cSeekExpr := ""

         IF HB_ISARRAY( axExpr ) .AND. ValType( AEval( axExpr, {| e | cSeekExpr += ToString( Eval( e ) ) } ) ) == "A" .OR. !( nSelectTemp )->( dbSeek( cSeekExpr, .F. ) )
            ( nSelectTemp )->( dbAppend() )
            lAppend := .T.
         ENDIF

         nCurrent := 0
         FOR nCurrExpr := 1 TO nLenSelectList
            IF aSelectList[ nCurrExpr, 6 ]
               ++nCurrent
            ENDIF
            IF !lCreateIndex .OR. lAppend .OR. aSelectList[ nCurrExpr, 5 ] $ "TA"
               xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )
               DO CASE
               CASE aSelectList[ nCurrExpr, 5 ] == "T"
                  ( nSelectTemp )->( FieldPut( nCurrExpr, FieldGet( nCurrExpr ) + xExprValue ) )
               CASE aSelectList[ nCurrExpr, 5 ] == "A" .AND. !lAppend
                  ( nSelectTemp )->( FieldPut( nCurrExpr, ( FieldGet( nCurrExpr ) + xExprValue ) / 2 ) )
               OTHERWISE
                  ( nSelectTemp )->( FieldPut( nCurrExpr, xExprValue ) )
               ENDCASE
               IF aSelectList[ nCurrExpr, 6 ] .AND. lSummary
                  aSummary[ nCurrent ] += xExprValue
               ENDIF
            ENDIF
         NEXT
      ENDIF

      /* Advance record pointers */
      IF !lOTM
         ( nSelectFrom )->( dbSkip() )
      ELSE
         ( nSelectChild )->( dbSkip() )
         IF ( nSelectChild )->( Eof() ) .OR. !xValueFromMother == ( nSelectChild )->( Eval( aOneToMany[ 3 ] ) )
            dbSkip()
            IF lOTM
               DO WHILE !Eof() .AND. !( nSelectChild )->( dbSeek( ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )
                  dbSkip()
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   dbSelectArea( nSelectTemp )
   dbGoTop()

RETURN


/*
 * STATIC FUNCTION ToString( cStr )
 *
 * Purpose:
 *   Converts a value of various Harbour types into a consistent string form,
 *   mainly for index key construction and comparison in GROUP BY/seek operations.
 *
 * Parameters:
 *   cStr – Value to be converted. May be Numeric, Date, Logical, or String.
 *
 * Return:
 *   (STRING) – Formatted value as a string.
 *     - Numeric to Str()
 *     - Date to DToS() (YYYYMMDD)
 *     - Logical to "1" for .T., "0" for .F.
 *     - Other to unchanged
 *
 * Notes:
 *   Ensures grouping keys are type-consistent when building index expressions.
 */
STATIC FUNCTION ToString( cStr )

   LOCAL cType := ValType( cStr )

   SWITCH cType
   CASE "N"
      RETURN Str( cStr )
   CASE "D"
      RETURN DToS( cStr )
   CASE "L"
      RETURN iif( cStr, "1", "0" )
   ENDSWITCH

RETURN cStr
