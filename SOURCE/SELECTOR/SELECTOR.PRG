/*
      Name: Selector
    Author: Written in Clipper 5.2e by Janusz Piwowarski <januszp@geocities.com>
   Purpose: Emulates a simplified SQL SELECT statement, extracting data from a database based on specified criteria.

   Input Parameters:
      aSelectList: Array defining fields/expressions to select. Each element is an array: {expression, alias, length, decimals, grouping flag ('G'), summary flag (.T.)}.
      xcnFromArea: Work area (database) to select from (numeric work area or character alias).
      aOneToMany:  Array defining a one-to-many relationship between parent (xcnFromArea) and child database: {child alias, parent field, child field}. NIL if no relationship.
      lSelect:     Code block filter expression (WHERE clause). NIL for all records.
      lWhile:      Code block loop termination condition (WHILE clause). NIL for EOF().
      lRest:       Logical value indicating whether to start from the current record (.T.) or the beginning (.F.).
      xcFile:      Output database file name. NIL for "TEMP.DBF".
      xcAlias:     Alias for the output database file. NIL defaults to filename or "temp".
      lAdditive:   Logical value indicating whether to append to an existing database (.T.) or create a new one (.F.).
      aSummary:    Array to store summary values (sums) for selected fields. NIL for no summary.
      xEval:       Code block to be evaluated for each record.

   Return Value:
      None (Undefined). Modifies the current work area and potentially creates/modifies a database file.
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

   // Determine the work area (database) to select from
   IF xcnFromArea == NIL
      nSelectFrom := Select() // Default to the current work area
   ELSE
      // If work area is specified as numeric, use it directly
      IF HB_ISNUMERIC( xcnFromArea )
         nSelectFrom := xcnFromArea
         // If specified as character, select the area using its alias
      ELSEIF HB_ISSTRING( xcnFromArea )
         nSelectFrom := Select( xcnFromArea )
      ENDIF
      dbSelectArea( nSelectFrom ) // Switch to the selected work area
   ENDIF

   // If one-to-many relationship is defined, select the child area
   IF lOTM
      nSelectChild := Select( aOneToMany[ 1 ] )
   ENDIF

   // Define the output file and alias if not provided
   IF xcFile == NIL
      xcFile := "TEMP.DBF"
      IF xcAlias == NIL
         xcAlias := "temp"
      ENDIF
   ELSEIF xcAlias == NIL
      xcAlias := hb_FNameName( xcFile )
   ENDIF

   // Define the index name based on file name if not provided
   IF cIndexName == NIL
      cIndexName := hb_FNameExtSet( hb_FNameName( xcFile ), IndexExt() )
   ENDIF

   nStartRec := RecNo()
   dbGoto( LastRec() + 1 )

   // If there is a child area, move to the end of the child database as well
   IF lOTM
      ( nSelectChild )->( dbGoto( LastRec() + 1 ) )
   ENDIF

   // Loop through the selection list and process each field/expression
   FOR nCurrExpr := 1 TO nLenSelectList
      xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )
      cExprType := ValType( xExprValue )
      cExprValue := Transform( xExprValue, "" )
      nExprLen := Len( cExprValue )
      // Add the expression's structure to the field structure (alias, type, length, decimals)
      AAdd( aStruct, { iif( ( nExprAlias := At( "->", aSelectList[ nCurrExpr, 2 ] ) ) == 0, aSelectList[ nCurrExpr, 2 ], ;
         SubStr( aSelectList[ nCurrExpr, 2 ], nExprAlias + 2 ) ), cExprType, ;
         iif( aSelectList[ nCurrExpr, 3 ] <> NIL, aSelectList[ nCurrExpr, 3 ], ;
         iif( cExprType == "D", 8, nExprLen ) ), ;
         iif( aSelectList[ nCurrExpr, 4 ] <> NIL, aSelectList[ nCurrExpr, 4 ], ;
         iif( cExprType == "N" .AND. ( nExprAlias := At( ".", cExprValue ) ) > 0, ;
         nExprLen - nExprAlias, 0 ) ) } )

      // If the expression is to be grouped (GROUP keyword)
      IF aSelectList[ nCurrExpr, 5 ] == "G"
         IF .NOT. lCreateIndex
            lCreateIndex := .T.
         ELSE
            cIndexKey += "+"
         ENDIF
         // Build the index key based on the type of the expression (character, numeric, date, logical)
         IF cExprType == "C"
            cIndexKey += aStruct[ nCurrExpr, 1 ]
         ELSEIF cExprType == "N"
            cIndexKey += ( "Str(" + aStruct[ nCurrExpr, 1 ] + ")" )
         ELSEIF cExprType == "D"
            cIndexKey += ( "DToS(" + aStruct[ nCurrExpr, 1 ] + ")" )
         ELSEIF cExprType == "L"
            cIndexKey += ( "Iif(" + aStruct[ nCurrExpr, 1 ] + "," + Chr( 34 ) + "1" + Chr( 34 ) + "," + Chr( 34 ) + "0" + Chr( 34 ) + ")" )
         ENDIF
         // Add the expression to an array of expressions
         AAdd( axExpr, aSelectList[ nCurrExpr, 1 ] )

         // If the expression is marked for summary (SUMMARY keyword)
      ELSEIF aSelectList[ nCurrExpr, 6 ] .AND. ( lSummary .AND. .NOT. lAdditive )
         // Add the expression to the summary array, initializing the value to 0
         AAdd( aSummary, 0 )
      ENDIF
   NEXT

   dbGoto( nStartRec )

   // If additive flag is set, select the target work area, otherwise create a new file
   IF lAdditive
      nSelectTemp := Select( xcAlias )
   ELSE
      dbCreate( xcFile, aStruct )
      dbUseArea( .T., NIL, xcFile, xcAlias, .F. )
      nSelectTemp := Select()
      // Create an index if necessary
      IF lCreateIndex
         dbCreateIndex( cIndexName, cIndexKey, hb_macroBlock( cIndexKey ) )
      ENDIF
      dbSelectArea( nSelectFrom )
   ENDIF

   // If REST is not specified, go to the first record
   IF .NOT. lRest
      dbGoTop()
   ENDIF

   // Process records if there is a one-to-many relationship
   IF lOTM
      DO WHILE .NOT. Eof() .AND. .NOT. ( nSelectChild )->( dbSeek( xValueFromMother := ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )
         dbSkip()
      ENDDO
   ENDIF

   // Main loop for selecting and processing records
   DO WHILE .NOT. Eof() .AND. ( lWhile == NIL .OR. Eval( lWhile ) )
      // Evaluate the expression for each record if an evaluation block is provided
      IF lEval
         Eval( xEval )
      ENDIF
      // Check if the record meets the select condition or if no condition is provided
      IF lSelect == NIL .OR. Eval( lSelect )
         lAppend := .F.
         lCreateIndex := .NOT. lCreateIndex
         cSeekExpr := ""

         // If there's an expression array, evaluate and build the seek expression
         IF HB_ISARRAY( axExpr ) .AND. ValType( AEval( axExpr, {| e | cSeekExpr += ToString( Eval( e ) ) } ) ) == "A" .OR. .NOT. ( nSelectTemp )->( dbSeek( cSeekExpr, .F. ) )
            ( nSelectTemp )->( dbAppend() )
            lAppend := .T.
         ENDIF

         nCurrent := 0

         // Loop through the selection list to process each field
         FOR nCurrExpr := 1 TO nLenSelectList
            IF aSelectList[ nCurrExpr, 6 ]
               ++nCurrent
            ENDIF
            // If index creation is not required or the record is being appended, update the field value
            IF .NOT. lCreateIndex .OR. lAppend .OR. aSelectList[ nCurrExpr, 5 ] $ "TA"
               xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )
               // Handle totals (T) and averages (A)
               IF aSelectList[ nCurrExpr, 5 ] == "T"
                  ( nSelectTemp )->( FieldPut( nCurrExpr, FieldGet( nCurrExpr ) + xExprValue ) )
               ELSEIF aSelectList[ nCurrExpr, 5 ] == "A" .AND. .NOT. lAppend
                  ( nSelectTemp )->( FieldPut( nCurrExpr, ( FieldGet( nCurrExpr ) + xExprValue ) / 2 ) )
               ELSE
                  ( nSelectTemp )->( FieldPut( nCurrExpr, xExprValue ) )
               ENDIF
               // Update summary if required
               IF aSelectList[ nCurrExpr, 6 ] .AND. lSummary
                  aSummary[ nCurrent ] := aSummary[ nCurrent ] + xExprValue
               ENDIF
            ENDIF
         NEXT
      ENDIF
      // Skip records depending on the one-to-many relationship
      IF .NOT. lOTM
         ( nSelectFrom )->( dbSkip() )
      ELSE
         ( nSelectChild )->( dbSkip() )
         IF ( nSelectChild )->( Eof() ) .OR. .NOT. xValueFromMother == ( nSelectChild )->( Eval( aOneToMany[ 3 ] ) )
            dbSkip()
            IF lOTM
               // Continue processing the next record in the one-to-many relationship
               DO WHILE .NOT. Eof() .AND. .NOT. ( nSelectChild )->( dbSeek( ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )
                  dbSkip()
               ENDDO
            ENDIF
         ENDIF
      ENDIF
   ENDDO

   // Select the output area and go to the first record
   dbSelectArea( nSelectTemp )
   dbGoTop()

RETURN

/*
   Converts a value to a string for index key consistency.

   Input Parameters:
      cStr: The value to convert.

   Return Value:
      String representation of the value.
*/
STATIC FUNCTION ToString( cStr )

   LOCAL cType := ValType( cStr )

   // Convert different types to string
   SWITCH cType
   CASE "N"
      RETURN Str( cStr )
   CASE "D"
      RETURN DToS( cStr )
   CASE "L"
      RETURN iif( cStr, "1", "0" )
   ENDSWITCH

RETURN cStr
