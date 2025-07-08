/* ..........................................................................
     Name: Selector
   Params: aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval
   Return: Undefined
  Example: SELECTOR(aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval)

   Author: Written in Clipper 5.2e by Janusz Piwowarski <januszp@geocities.com>
  Version: 1.1
  Description: This function acts as a pseudo-SQL `SELECT` command used to retrieve data from databases.
   ..........................................................................
*/

PROCEDURE Selector( aSelectList, xcnFromArea, aOneToMany, lSelect, lWhile, lRest, xcFile, xcAlias, lAdditive, aSummary, xEval )

   // Local variable initialization
   LOCAL nLenSelectList := Len( aSelectList )   // Length of the selection list
   LOCAL xExprValue, cExprValue, cExprType, nExprLen, aStruct := {}  // Variable placeholders for expressions and structure definition
   LOCAL lOTM := ( ValType( aOneToMany ) == "A" )  // Check if one-to-many relationship is defined
   LOCAL nExprAlias, nCurrExpr, nSelectFrom, nSelectChild, nSelectTemp, xValueFromMother, axExpr := {}  // Additional variables for work areas and expressions
   LOCAL lCreateIndex := .F.  // Flag for whether an index should be created
   LOCAL cIndexName, cIndexKey := ""   // Index name and key placeholders
   LOCAL lAppend, lSummary := .NOT. ( aSummary == NIL )  // Check if summary is required
   LOCAL nCurrent, cSeekExpr, nStartRec  // Variables for keeping track of records and seek expressions

   // Determine the work area (database) to select from
   IF xcnFromArea == NIL
      nSelectFrom := Select()  // Default to the current work area
   ELSE
      // If work area is specified as numeric, use it directly
      IF ValType( xcnFromArea ) == "N"
         nSelectFrom := xcnFromArea
      // If specified as character, select the area using its alias
      ELSEIF ValType( xcnFromArea ) == "C"
         nSelectFrom := Select( xcnFromArea )
      ENDIF
      dbSelectArea( nSelectFrom )  // Switch to the selected work area
   ENDIF

   // If one-to-many relationship is defined, select the child area
   IF lOTM
      nSelectChild := Select( aOneToMany[ 1 ] )  // Select child area based on one-to-many array
   ENDIF

   // Define the output file and alias if not provided
   IF xcFile == NIL
      xcFile := "TEMP.DBF"  // Default file name
      IF xcAlias == NIL
         xcAlias := "temp"  // Default alias
      ENDIF
   ELSEIF xcAlias == NIL
      // Extract alias from the file name
      xcAlias := SubStr( xcAlias := SubStr( xcFile, RAt( "\", xcFile ) + 1 ), 1, At( ".", xcAlias + "." ) - 1 )
   ENDIF

   // Define the index name based on file name if not provided
   IF cIndexName == NIL
      cIndexName := IF( ( nExprAlias := RAt( ".", xcFile ) ) > RAt( "\", xcFile ), SubStr( xcFile, 1, nExprAlias - 1 ), xcFile ) + IndexExt()
   ENDIF

   nStartRec := RecNo()  // Save the current record number
   dbGoto( LastRec() + 1 )  // Move to the end of the database

   // If there is a child area, move to the end of the child database as well
   IF lOTM
      ( nSelectChild )->( dbGoto( LastRec() + 1 ) )
   ENDIF

   // Loop through the selection list and process each field/expression
   FOR nCurrExpr := 1 TO nLenSelectList

      xExprValue := Eval( aSelectList[ nCurrExpr, 1 ] )  // Evaluate the expression
      cExprType := ValType( xExprValue )  // Get the type of the evaluated expression (character, numeric, etc.)
      cExprValue := Transform( xExprValue, "" )  // Convert the expression to a string
      nExprLen := Len( cExprValue )  // Get the length of the string
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
            lCreateIndex := .T.  // Set flag to create index
         ELSE
            cIndexKey += "+"  // Concatenate index keys for grouped fields
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
         AAdd( axExpr, aSelectList[ nCurrExpr, 1 ] )  // Add the expression to an array of expressions

      // If the expression is marked for summary (SUMMARY keyword)
      ELSEIF aSelectList[ nCurrExpr, 6 ] .AND. ( lSummary .AND. .NOT. lAdditive )
         AAdd( aSummary, 0 )  // Add the expression to the summary array, initializing the value to 0
      ENDIF

   NEXT

   dbGoto( nStartRec )  // Go back to the starting record

   // If additive flag is set, select the target work area, otherwise create a new file
   IF lAdditive
      nSelectTemp := Select( xcAlias )
   ELSE
      dbCreate( xcFile, aStruct )  // Create a new database file
      dbUseArea( .T., NIL, xcFile, xcAlias, .F. )  // Open the file in the current work area
      nSelectTemp := Select()
      // Create an index if necessary
      IF lCreateIndex
         dbCreateIndex( cIndexName, cIndexKey, ( &( "{||" + cIndexKey + "}" ) ) )
      ENDIF
      dbSelectArea( nSelectFrom )  // Switch back to the original work area
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
      IF ValType( xEval ) == "B"
         Eval( xEval )
      ENDIF
      // Check if the record meets the select condition or if no condition is provided
      IF lSelect == NIL .OR. Eval( lSelect )
         lAppend := .F.  // Initialize append flag
         lCreateIndex := .NOT. lCreateIndex  // Toggle index creation flag
         cSeekExpr := ""  // Reset the seek expression

         // If there's an expression array, evaluate and build the seek expression
         IF ValType( axExpr ) == "A" .AND. ValType( AEval( axExpr, {| e | cSeekExpr += ToString( Eval( e ) ) } ) ) == "A" .OR. .NOT. ( nSelectTemp )->( dbSeek( cSeekExpr, .F. ) )
            ( nSelectTemp )->( dbAppend() )  // Append a new record if seek fails
            lAppend := .T.
         ENDIF

         nCurrent := 0  // Initialize current index for summary

         // Loop through the selection list to process each field
         FOR nCurrExpr := 1 TO nLenSelectList
            IF aSelectList[ nCurrExpr, 6 ]
               ++nCurrent  // Increment current index for summary
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
         IF ( nSelectChild )->( dbSkip() ) .OR. ( nSelectChild )->( Eof() .OR. .NOT. xValueFromMother == Eval( aOneToMany[ 3 ] ) )
            dbSkip()
            IF lOTM
               // Continue processing the next record in the one-to-many relationship
               DO WHILE .NOT. Eof() .AND. .NOT. ( nSelectChild )->( dbSeek( xValueFromMother := ( nSelectFrom )->( Eval( aOneToMany[ 2 ] ) ), .F. ) )
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


/* ..........................................................................
     Name: ToString
   Params: xType
   Return: String
  Example: ToString(String)
   ..........................................................................
*/

STATIC FUNCTION ToString( cStr )
   LOCAL cType := ValType( cStr )

   // Convert different types to string
   IF cType == "N"
      cStr := Str( cStr )
   ELSEIF cType == "D"
      cStr := DToS( cStr )
   ELSEIF cType == "L"
      cStr := iif( cStr, "1", "0" )
   ENDIF

RETURN cStr
