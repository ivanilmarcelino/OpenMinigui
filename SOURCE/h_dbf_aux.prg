#ifdef __XHARBOUR__
#define __SYSDATA__
#endif

#include "minigui.ch"
#include "dbinfo.ch"
#include "fileio.ch"

*=============================================================================*
*                           Auxiliary Functions
*=============================================================================*

/*
 * FUNCTION HMG_DbfToArray( cFieldList, bFor, bWhile, nNext, nRec, lRest )
 *
 * Converts data from the currently open DBF file into an array of arrays. Each inner array represents a record,
 * and contains the values of the specified fields.
 *
 * Parameters:
 *   cFieldList (STRING): A comma-delimited string of field names to extract from the DBF. If empty, all fields are extracted.
 *   bFor (BLOCK): An optional code block to be evaluated for each record. Only records for which the block evaluates to .T. are included.
 *   bWhile (BLOCK): An optional code block that acts as a WHILE condition. Processing stops when this block evaluates to .F.
 *   nNext (NUMERIC): An optional number of records to process, starting from the current record.
 *   nRec (NUMERIC): An optional record number to start processing from. If specified, the current record pointer is moved to this record.
 *   lRest (LOGICAL): An optional logical value. If .T., processes all remaining records from the current position.
 *
 * Return Value:
 *   ARRAY: An array of arrays. Each inner array represents a record, with elements corresponding to the values of the fields specified in cFieldList.
 *          Returns an empty array if no records match the criteria.
 *
 * Purpose:
 *   This function provides a convenient way to extract data from a DBF file into an array format, which is often easier to work with in HMG applications.
 *   It allows for selective extraction of data based on various criteria (FOR, WHILE, NEXT, REC, REST), making it versatile for different data processing needs.
 *   For example, you might use this function to load data from a customer database into a grid control for display and editing.
 *
 * Notes:
 *   The function preserves the original record pointer position after execution.
 *   The data types of the elements in the returned array will match the data types of the corresponding fields in the DBF.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfToArray( cFieldList, bFor, bWhile, nNext, nRec, lRest )
*-----------------------------------------------------------------------------*
   LOCAL aRet := {}
   LOCAL nRecNo := RecNo()
   LOCAL bLine

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), {| a | cFieldList += "," + a[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

#ifndef __XHARBOUR__
   bLine := hb_macroBlock( "{" + cFieldList + "}" )
#else
   bLine := &( "{||{" + cFieldList + "}}" )
#endif

   dbEval( { || AAdd( aRet, Eval( bLine ) ) }, bFor, bWhile, nNext, nRec, lRest )

   dbGoto( nRecNo )

RETURN aRet

/*
 * FUNCTION HMG_ArrayToDbf( aData, cFieldList, bProgress )
 *
 * Appends data from an array of arrays to the currently open DBF file. Each inner array represents a record,
 * and the elements of the inner array are written to the corresponding fields in the DBF.
 *
 * Parameters:
 *   aData (ARRAY): An array of arrays. Each inner array represents a record to be appended to the DBF.
 *   cFieldList (STRING or ARRAY): A comma-delimited string or an array of field names to which the data should be written.
 *                                 If empty, data is written to all fields in the DBF, in the order they appear in the DBF structure.
 *   bProgress (BLOCK): An optional code block that is evaluated during the process to provide progress updates.
 *                      The block receives two parameters: the current row number and the total number of rows.
 *
 * Return Value:
 *   LOGICAL: .T. if all records were successfully appended to the DBF, .F. if an error occurred during the process (e.g., NetAppend() failed).
 *
 * Purpose:
 *   This function provides a way to efficiently write data from an array into a DBF file. It's useful for importing data from other sources,
 *   such as text files or other databases, into a DBF format. The optional progress block allows for displaying a progress bar or other visual feedback
 *   to the user during the import process.
 *   For example, you might use this function to import data from a CSV file into a DBF file for further processing.
 *
 * Notes:
 *   The function assumes that the structure of the inner arrays in aData matches the structure of the DBF file, either in terms of field order
 *   (if cFieldList is empty) or in terms of the order of field names specified in cFieldList.
 *   The function performs type conversion if the data type of an element in aData does not match the data type of the corresponding field in the DBF.
 *   The function uses NetAppend() to append records to the DBF, which is suitable for networked environments.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_ArrayToDbf( aData, cFieldList, bProgress )
*-----------------------------------------------------------------------------*
   LOCAL aFieldNames, aFieldPositions, aFieldTypes
   LOCAL aRow
   LOCAL uVal
   LOCAL nCols, nRows
   LOCAL nCol, nRow
   LOCAL nFldPos, cFldType
   LOCAL lFieldNamesNoProvided
   LOCAL lSuccess := .T.

   IF ISARRAY( cFieldList )
      aFieldNames := cFieldList
   ELSEIF ISCHARACTER( cFieldList )
      aFieldNames := hb_ATokens( cFieldList, ',' )
   ENDIF

   lFieldNamesNoProvided := ( Empty( aFieldNames ) )
   nCols := iif( lFieldNamesNoProvided, FCount(), Min( FCount(), Len( aFieldNames ) ) )
   aFieldPositions := Array( nCols )
   aFieldTypes := Array( nCols )

   FOR nCol := 1 TO nCols
      aFieldPositions[ nCol ] := iif( lFieldNamesNoProvided, nCol, FieldPos( aFieldNames[ nCol ] ) )
      aFieldTypes[ nCol ] := iif( lFieldNamesNoProvided, FieldType( nCol ), FieldType( aFieldPositions[ nCol ] ) )
   NEXT

   nRows := Len( aData )
   IF ISBLOCK( bProgress )
      Eval( bProgress, 0, nRows )
   ENDIF

   FOR nRow := 1 TO nRows
      aRow := aData[ nRow ]

      IF ! NetAppend()
         lSuccess := .F.
         EXIT
      ENDIF

      FOR nCol := 1 TO nCols
         nFldPos := aFieldPositions[ nCol ]
         cFldType := aFieldTypes[ nCol ]

         IF ! Empty( nFldPos )

            IF ! Empty( uVal := aRow[ nCol ] )

               IF ! ( cFldType $ '+@' )
                  IF ValType( uVal ) != cFldType
                     uVal := ConvertType( uVal, cFldType )
                  ENDIF

                  IF ! Empty( uVal )
                     FieldPut( nFldPos, uVal )
                  ENDIF
               ENDIF

            ENDIF

         ENDIF
      NEXT nCol

      dbUnlock()
      IF ISBLOCK( bProgress )
         Eval( bProgress, nRow, nRows )
      ENDIF
   NEXT nRow

RETURN lSuccess

#ifdef __XHARBOUR__
#include "hbcompat.ch"
#endif

/*
 * STATIC FUNCTION ConvertType( uVal, cTypeDst )
 *
 * Converts a value from one data type to another.
 *
 * Parameters:
 *   uVal (ANY): The value to be converted.
 *   cTypeDst (CHARACTER): A single-character string representing the target data type (e.g., "C" for Character, "N" for Numeric, "D" for Date, "L" for Logical, "T" for DateTime).
 *
 * Return Value:
 *   ANY: The converted value. Returns NIL if the conversion is not supported or fails.
 *
 * Purpose:
 *   This function is used internally by HMG_ArrayToDbf to ensure that the data being written to the DBF file is of the correct data type.
 *   It handles conversions between common data types, such as converting a string to a number or a date.
 *   This ensures data integrity and prevents errors when writing data to the DBF.
 *
 * Notes:
 *   The function supports conversions to Character, Date, Logical, Numeric, and DateTime data types.
 *   If the conversion is not possible, the function returns NIL.
 *   This function is declared as STATIC, meaning it is only accessible within the current source file.
 */
*-----------------------------------------------------------------------------*
STATIC FUNCTION ConvertType( uVal, cTypeDst )
*-----------------------------------------------------------------------------*
   LOCAL cTypeSrc := ValType( uVal )

   IF cTypeDst != cTypeSrc

      DO CASE
      CASE cTypeDst $ 'CM'
         uVal := hb_ValToStr( uVal )

      CASE cTypeDst == 'D'
         DO CASE
         CASE cTypeSrc == 'T'
            uVal := SToD( Left( hb_TToS( uVal ), 8 ) )
         CASE cTypeSrc == 'C'
            uVal := CToD( uVal )
         OTHERWISE
            uVal := BLANK_DATE
         ENDCASE

      CASE cTypeDst == 'L'
         DO CASE
         CASE cTypeSrc $ 'LN'
            uVal := ! Empty( uVal )
         CASE cTypeSrc == 'C'
            uVal := Upper( uVal ) $ "Y,YES,T,.T.,TRUE"
         OTHERWISE
            uVal := .F.
         ENDCASE

      CASE cTypeDst == 'N'
         DO CASE
         CASE cTypeSrc == 'C'
            uVal := Val( uVal )
         OTHERWISE
            uVal := 0
         ENDCASE

      CASE cTypeDst == 'T'
         DO CASE
         CASE cTypeSrc == 'D'
            uVal := hb_SToT( DToS( uVal ) + "000000.000" )
         CASE cTypeSrc == 'C'
            uVal := hb_CToT( uVal )
         OTHERWISE
            uVal := hb_CToT( '' )
         ENDCASE

      OTHERWISE
         uVal := NIL
      ENDCASE

   ENDIF

RETURN uVal

/*
 * FUNCTION HMG_DbfToExcel( cFieldList, aHeader, bFor, bWhile, nNext, nRec, lRest )
 *
 * Exports data from the currently open DBF file to an Excel spreadsheet.
 *
 * Parameters:
 *   cFieldList (STRING): A comma-delimited string of field names to export. If empty, all fields are exported.
 *   aHeader (ARRAY): An optional array of column headers for the Excel spreadsheet. If not provided, the field names from cFieldList are used.
 *   bFor (BLOCK): An optional code block to be evaluated for each record. Only records for which the block evaluates to .T. are included.
 *   bWhile (BLOCK): An optional code block that acts as a WHILE condition. Processing stops when this block evaluates to .F.
 *   nNext (NUMERIC): An optional number of records to process, starting from the current record.
 *   nRec (NUMERIC): An optional record number to start processing from. If specified, the current record pointer is moved to this record.
 *   lRest (LOGICAL): An optional logical value. If .T., processes all remaining records from the current position.
 *
 * Return Value:
 *   LOGICAL: .T. if the data was successfully exported to Excel, .F. if an error occurred (e.g., Excel is not installed).
 *
 * Purpose:
 *   This function provides a convenient way to export data from a DBF file to an Excel spreadsheet for further analysis or reporting.
 *   It allows for selective export of data based on various criteria (FOR, WHILE, NEXT, REC, REST), and provides the option to customize the column headers.
 *   For example, you might use this function to export sales data from a DBF file to Excel for creating charts and reports.
 *
 * Notes:
 *   The function requires that Microsoft Excel is installed on the system.
 *   The function creates a new Excel workbook and exports the data to the first sheet.
 *   The function preserves the original record pointer position after execution.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfToExcel( cFieldList, aHeader, bFor, bWhile, nNext, nRec, lRest )
*-----------------------------------------------------------------------------*
   LOCAL nRecNo := RecNo()
   LOCAL bLine
   LOCAL oExcel, oBook, oSheet, oRange
   LOCAL nCols
   LOCAL nRow := 1

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), { | x | cFieldList += "," + x[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

   hb_default( @aHeader, hb_ATokens( cFieldList, "," ) )

   TRY
      oExcel := CreateObject( "Excel.Application" )
   CATCH
      MsgAlert( "Excel not installed", "Warning" )
      RETURN .F.
   END

   oBook := oExcel:WorkBooks:Add()
   oSheet := oBook:ActiveSheet

   nCols := Len( aHeader )
   oRange := oSheet:Range( oSheet:Columns( nRow ), oSheet:Columns( nCols ) )

   oExcel:ScreenUpdating := .F.

   oRange:Rows( nRow ):Value := aHeader
   oRange:Rows( nRow ):Font:Bold := .T.

   bLine := &( "{||{" + cFieldList + "}}" )

   IF Empty( bWhile ) .AND. Empty( nNext ) .AND. Empty( nRec ) .AND. Empty( lRest )
      dbGoTop()
   ENDIF

   dbEval( { || oRange:Rows( ++nRow ):Value := Eval( bLine ), nRow }, bFor, bWhile, nNext, nRec, lRest )
   dbGoto( nRecNo )

   oRange:AutoFit()

   oExcel:ScreenUpdating := .T.
   oExcel:Visible := .T.

RETURN .T.

#define FIELD_ENTRY_SIZE 32
#define BUFFER_SIZE 32

/*
 * FUNCTION HMG_DbfStruct( cFileName )
 *
 * Reads the structure of a DBF file and returns it as an array of arrays.
 *
 * Parameters:
 *   cFileName (STRING): The name of the DBF file to read.
 *
 * Return Value:
 *   ARRAY: An array of arrays, where each inner array represents a field in the DBF file.
 *          Each inner array contains the following elements:
 *            [1] (STRING): The field name (uppercase).
 *            [2] (STRING): The field type (e.g., "C" for Character, "N" for Numeric, "D" for Date, "L" for Logical).
 *            [3] (NUMERIC): The field length.
 *            [4] (NUMERIC): The number of decimal places (for numeric fields).
 *          Returns NIL if the file cannot be opened.
 *
 * Purpose:
 *   This function allows you to programmatically retrieve the structure of a DBF file without having to open it.
 *   This is useful for tasks such as dynamically creating forms or grids based on the DBF structure, or for validating data before writing it to the DBF.
 *   For example, you might use this function to read the structure of a customer database and then dynamically create a form for editing customer information.
 *
 * Notes:
 *   The function reads the DBF file header to determine the field structure.
 *   The function does not open the DBF file for data access; it only reads the header information.
 *   The function uses shared file access (FO_SHARED) to allow other processes to read the DBF file while it is being read by this function.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfStruct( cFileName )
*-----------------------------------------------------------------------------*
   LOCAL aStruct := {}
   LOCAL hFile, aFieldInfo
   LOCAL cBuffer := Space( BUFFER_SIZE )

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".dbf" )
   ENDIF

   IF ( hFile := FOpen( cFileName, FO_SHARED ) ) >= 0

      IF FRead( hFile, @cBuffer, BUFFER_SIZE ) == FIELD_ENTRY_SIZE

         DO WHILE ( FRead( hFile, @cBuffer, BUFFER_SIZE ) == FIELD_ENTRY_SIZE .AND. !( cBuffer = Chr( 13 ) ) )

            aFieldInfo := Array( 4 )

            aFieldInfo[ 1 ] := Upper( BeforAtNum( Chr( 0 ), cBuffer, 1 ) )
            aFieldInfo[ 2 ] := SubStr( cBuffer, 12, 1 )

            IF aFieldInfo[ 2 ] == 'C'

               aFieldInfo[ 3 ] := Bin2I( SubStr( cBuffer, 17, 2 ) )
               aFieldInfo[ 4 ] := 0

            ELSE

               aFieldInfo[ 3 ] := Asc( SubStr( cBuffer, 17, 1 ) )
               aFieldInfo[ 4 ] := Asc( SubStr( cBuffer, 18, 1 ) )

            ENDIF

            AAdd( aStruct, aFieldInfo )

         ENDDO

      ENDIF

      FClose( hFile )

   ELSE

      aStruct := NIL

   ENDIF

RETURN aStruct

/*
 * FUNCTION HMG_RecToHash( cFieldList, cNames )
 *
 * Converts the current record in the open DBF file into a hash (associative array).
 *
 * Parameters:
 *   cFieldList (STRING): A comma-delimited string of field names to include in the hash. If empty, all fields are included.
 *   cNames (STRING): An optional comma-delimited string of names to use as keys in the hash. If not provided, the field names from cFieldList are used.
 *
 * Return Value:
 *   HASH: A hash (associative array) where the keys are the field names (or the names specified in cNames) and the values are the corresponding field values from the current record.
 *
 * Purpose:
 *   This function provides a convenient way to access the data in the current record of a DBF file using a hash, which can be more readable and easier to work with than accessing fields by position.
 *   It allows you to specify which fields to include in the hash and to customize the keys used to access the data.
 *   For example, you might use this function to convert a customer record into a hash and then pass the hash to a function that generates a personalized email.
 *
 * Notes:
 *   The function preserves the case of the field names (or the names specified in cNames) when creating the hash keys.
 *   The function uses HSetCaseMatch(.F.) to ensure that the hash keys are case-sensitive.
 *   If cNames is provided, it must contain the same number of names as there are fields in cFieldList.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_RecToHash( cFieldList, cNames )
*-----------------------------------------------------------------------------*
   LOCAL hRec := { => }
   LOCAL aVals, aNames

   HSetCaseMatch( hRec, .F. )

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), {| a | cFieldList += "," + a[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

   DEFAULT cNames := cFieldList

   aNames := hb_ATokens( cNames, ',' )

#ifndef __XHARBOUR__
   aVals := hb_macroBlock( '{' + cFieldList + '}' )
#else
   aVals := &( '{' + cFieldList + '}' )
#endif

   AEval( aVals, {| u, i | hSet( hRec, aNames[ i ], u ) }, , Len( aNames ) )

RETURN hRec

/*
 * FUNCTION HMG_HashToRec( hRec, cFieldList )
 *
 * Updates the current record in the open DBF file with data from a hash (associative array).
 *
 * Parameters:
 *   hRec (HASH): A hash (associative array) where the keys are the field names and the values are the new values to be written to the corresponding fields.
 *   cFieldList (STRING): An optional comma-delimited string of field names to update. If empty, all fields in the hash are updated.
 *
 * Return Value:
 *   LOGICAL: .T. if the record was successfully updated, .F. if an error occurred (e.g., the record is locked).
 *
 * Purpose:
 *   This function provides a convenient way to update the data in the current record of a DBF file using a hash.
 *   It allows you to update specific fields in the record by specifying them in cFieldList, or to update all fields in the hash if cFieldList is empty.
 *   For example, you might use this function to update a customer record with data entered by the user in a form.
 *
 * Notes:
 *   The function checks if the DBF file is shared and if the record is locked before attempting to update it.
 *   If the record is locked, the function attempts to lock it before updating it.
 *   The function uses FieldPut() to write the values from the hash to the corresponding fields in the DBF file.
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_HashToRec( hRec, cFieldList )
*-----------------------------------------------------------------------------*
   LOCAL lShared := dbInfo( DBI_SHARED )
   LOCAL lLocked := .F.
   LOCAL lSaved := .F.
   LOCAL nRecNo := RecNo()
   LOCAL aFlds

   IF ! lShared .OR. ;
         ( dbInfo( DBI_ISFLOCK ) .OR. dbRecordInfo( DBRI_LOCKED, nRecNo ) ) .OR. ;
         ( lLocked := dbRLock( nRecNo ) )

      IF Empty( cFieldList )
         hb_HEval( hRec, {| k, v | FieldPut( FieldPos( k ), v ) } )
      ELSE
         aFlds := hb_ATokens( cFieldList, ',' )
         hb_HEval( hRec, {| k, v, p | HB_SYMBOL_UNUSED( k ), FieldPut( FieldPos( aFlds[ p ] ), v ) }, , Len( aFlds ) )
      ENDIF

      IF lLocked
         dbRUnlock( nRecNo )
      ENDIF

      lSaved := .T.

   ENDIF

RETURN lSaved

/*
 * PROCEDURE DbfCopyRec( cnTargetArea, lAppend )
 *
 * Copies the current record from the current work area to another work area.
 *
 * Parameters:
 *   cnTargetArea (CHARACTER or NUMERIC): The alias or work area number of the target DBF file.
 *   lAppend (LOGICAL): If .T., the record is appended to the target DBF. If .F., the current record in the target DBF is overwritten.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure provides a way to copy data between DBF files. It's useful for tasks such as merging data from multiple DBF files into a single DBF file,
 *   or for creating a backup copy of a record before making changes to it.
 *   For example, you might use this procedure to copy a customer record from a temporary DBF file to the main customer database after the user has confirmed the changes.
 *
 * Notes:
 *   The procedure checks if the target DBF file has a field with the same name as a field in the source DBF file, and if the data types of the fields are compatible.
 *   If a matching field is found, the value of the field is copied from the source DBF to the target DBF.
 *   If lAppend is .T., the procedure appends a new record to the target DBF before copying the data.
 */
*-----------------------------------------------------------------------------*
PROCEDURE DbfCopyRec( cnTargetArea, lAppend )
*-----------------------------------------------------------------------------*
   LOCAL nFieldsCnt := FCount()
   LOCAL nCnt
   LOCAL cFieldName
   LOCAL nFieldPos
   LOCAL xFieldValue

   IF ISLOGICAL( lAppend ) .AND. lAppend

      ( cnTargetArea )->( dbAppend() )

   ENDIF

   FOR nCnt := 1 TO nFieldsCnt

      cFieldName := FieldName( nCnt )

      IF ( nFieldPos := ( cnTargetArea )->( FieldPos( cFieldName ) ) ) > 0 .AND. ;
         ValType( xFieldValue := FieldGet( nCnt ) ) == ValType( ( cnTargetArea )->( FieldGet( nFieldPos ) ) )

         ( cnTargetArea )->( FieldPut( nFieldPos, xFieldValue ) )

      ENDIF

   NEXT

RETURN

/*
 * FUNCTION DbfModStru( cDbfName, aModStru )
 *
 * Modifies the structure of a DBF file.
 *
 * Parameters:
 *   cDbfName (STRING): The name of the DBF file to modify.
 *   aModStru (ARRAY): An array of arrays, where each inner array represents a field to be modified.
 *                     Each inner array should contain the following elements:
 *                       [1] (STRING): The new field name (padded to 10 characters).
 *                       [2] (STRING): The new field type (padded to 1 character).
 *
 * Return Value:
 *   NUMERIC: An error code. 0 if the structure was successfully modified, otherwise a non-zero error code indicating the reason for the failure.
 *
 * Purpose:
 *   This function allows you to programmatically modify the structure of a DBF file.
 *   This is useful for tasks such as renaming fields, changing field types, or adding new fields to a DBF file.
 *   For example, you might use this function to rename a field in a customer database to better reflect its contents.
 *
 * Notes:
 *   The function opens the DBF file in exclusive mode (FO_EXCLUSIVE) to prevent other processes from accessing it while the structure is being modified.
 *   The function reads the DBF file header and the field descriptors, modifies the field descriptors according to the aModStru array, and then writes the modified field descriptors back to the DBF file.
 *   This function is potentially dangerous and should be used with caution, as it can corrupt the DBF file if used incorrectly.
 *   It is recommended to back up the DBF file before using this function.
 */
*-----------------------------------------------------------------------------*
FUNCTION DbfModStru( cDbfName, aModStru )
*-----------------------------------------------------------------------------*
   LOCAL nSize
   LOCAL cBuffer := Space( BUFFER_SIZE )
   LOCAL cBuffSize
   LOCAL hDbfHandle
   LOCAL nErrorCode
   LOCAL nStru

   nSize := Len( aModStru ) * BUFFER_SIZE
   cBuffSize := Space( nSize )
   hDbfHandle := FOpen( cDbfName, FO_EXCLUSIVE + FO_READWRITE )
   nErrorCode := FError()

   IF nErrorCode == 0

      IF FRead( hDbfHandle, @cBuffer, BUFFER_SIZE ) <> FIELD_ENTRY_SIZE

         nErrorCode := FError()

      ELSEIF FRead( hDbfHandle, @cBuffSize, nSize ) <> nSize

         nErrorCode := FError()

      ELSE

         FOR nStru := 1 TO Len( aModStru )

            cBuffSize := Stuff( cBuffSize, 1 + BUFFER_SIZE * ( nStru - 1 ), 10, PadR( aModStru[ nStru, 1 ], 10 ) )
            cBuffSize := Stuff( cBuffSize, 12 + BUFFER_SIZE * ( nStru - 1 ), 1, PadR( aModStru[ nStru, 2 ], 1 ) )

         NEXT

         FSeek( hDbfHandle, BUFFER_SIZE, FS_SET )

         IF FWrite( hDbfHandle, cBuffSize, nSize ) <> nSize

            nErrorCode := FError()

         ENDIF

      ENDIF

      FClose( hDbfHandle )

   ENDIF

RETURN nErrorCode
