/*
 * MiniGUI DBF Header Info Test
 * (c) 2010 Grigory Filatov <gfilatov@inbox.ru>
 *
 * This program demonstrates creating and accessing DBF file headers using MiniGUI.
 */

#include "minigui.ch"  // Includes MiniGUI definitions
#include "dbstruct.ch" // Includes DBF structure constants
#include "fileio.ch"   // Includes file I/O constants

/*
 * PROCEDURE Main()
 *
 * This is the main procedure of the application. It defines the main window
 * and menu, and sets up the event handling.
 *
 * Purpose:
 *   This procedure serves as the entry point for the application. It creates
 *   the main window with a menu that allows the user to retrieve and display
 *   the header information of a DBF file. It demonstrates the usage of the
 *   GetHeaderInfo() function and the AChoice() function for displaying the
 *   results in a user-friendly manner. The application provides a simple GUI
 *   for interacting with the DBF header information retrieval functionality.
 *
 * Notes:
 *   The procedure assumes that the 'test.dbf' file exists or will be created
 *   by the filltable() procedure.
 */
PROCEDURE Main()

   LOCAL aResult

   filltable( 100 )

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 450 ;
      HEIGHT 450 ;
      TITLE 'DBF Header Info' ;
      MAIN

   DEFINE MAIN MENU

      DEFINE POPUP "Test"

         MENUITEM 'Get Header Info' ACTION ( aResult := GetHeaderInfo( 'test.dbf' ), ;
            AChoice( ,,,, aResult, "Header Info of TEST.DBF" ) )

         SEPARATOR

         ITEM 'Exit' ACTION Form_1.Release()

      END POPUP

   END MENU

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN


/*
 * PROCEDURE filltable( nCount )
 *
 * Creates a DBF file named 'test.dbf' and populates it with sample data.
 *
 * Parameters:
 *   nCount : The number of records to create in the DBF file.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure is used to create a sample DBF file for testing purposes.
 *   It defines the structure of the DBF file (fields and their types) and
 *   then populates it with a specified number of records, each containing
 *   randomly generated data. This allows the application to have a DBF file
 *   to work with when demonstrating the GetHeaderInfo() function.
 *
 * Notes:
 *   If the 'test.dbf' file already exists, it will be opened, emptied (ZAP),
 *   and then repopulated with new data. The data is randomly generated, so
 *   each execution will result in different data.
 */
PROCEDURE filltable ( nCount )

   LOCAL aDbf[ 11 ][ 4 ], i

   IF !File( 'test.dbf' )
      aDbf[ 1 ][ DBS_NAME ] := "First"
      aDbf[ 1 ][ DBS_TYPE ] := "Character"
      aDbf[ 1 ][ DBS_LEN ]  := 20
      aDbf[ 1 ][ DBS_DEC ]  := 0
      //
      aDbf[ 2 ][ DBS_NAME ] := "Last"
      aDbf[ 2 ][ DBS_TYPE ] := "Character"
      aDbf[ 2 ][ DBS_LEN ]  := 20
      aDbf[ 2 ][ DBS_DEC ]  := 0
      //
      aDbf[ 3 ][ DBS_NAME ] := "Street"
      aDbf[ 3 ][ DBS_TYPE ] := "Character"
      aDbf[ 3 ][ DBS_LEN ]  := 30
      aDbf[ 3 ][ DBS_DEC ]  := 0
      //
      aDbf[ 4 ][ DBS_NAME ] := "City"
      aDbf[ 4 ][ DBS_TYPE ] := "Character"
      aDbf[ 4 ][ DBS_LEN ]  := 30
      aDbf[ 4 ][ DBS_DEC ]  := 0
      //
      aDbf[ 5 ][ DBS_NAME ] := "State"
      aDbf[ 5 ][ DBS_TYPE ] := "Character"
      aDbf[ 5 ][ DBS_LEN ]  := 2
      aDbf[ 5 ][ DBS_DEC ]  := 0
      //
      aDbf[ 6 ][ DBS_NAME ] := "Zip"
      aDbf[ 6 ][ DBS_TYPE ] := "Character"
      aDbf[ 6 ][ DBS_LEN ]  := 10
      aDbf[ 6 ][ DBS_DEC ]  := 0
      //
      aDbf[ 7 ][ DBS_NAME ] := "Hiredate"
      aDbf[ 7 ][ DBS_TYPE ] := "Date"
      aDbf[ 7 ][ DBS_LEN ]  := 8
      aDbf[ 7 ][ DBS_DEC ]  := 0
      //
      aDbf[ 8 ][ DBS_NAME ] := "Married"
      aDbf[ 8 ][ DBS_TYPE ] := "Logical"
      aDbf[ 8 ][ DBS_LEN ]  := 1
      aDbf[ 8 ][ DBS_DEC ]  := 0
      //
      aDbf[ 9 ][ DBS_NAME ] := "Age"
      aDbf[ 9 ][ DBS_TYPE ] := "Numeric"
      aDbf[ 9 ][ DBS_LEN ]  := 2
      aDbf[ 9 ][ DBS_DEC ]  := 0
      //
      aDbf[ 10 ][ DBS_NAME ] := "Salary"
      aDbf[ 10 ][ DBS_TYPE ] := "Numeric"
      aDbf[ 10 ][ DBS_LEN ]  := 6
      aDbf[ 10 ][ DBS_DEC ]  := 0
      //
      aDbf[ 11 ][ DBS_NAME ] := "Notes"
      aDbf[ 11 ][ DBS_TYPE ] := "Character"
      aDbf[ 11 ][ DBS_LEN ]  := 70
      aDbf[ 11 ][ DBS_DEC ]  := 0

      dbCreate( "test", aDbf )
   ENDIF

   USE test
   ZAP

   FOR i := 1 TO nCount
      APPEND BLANK

      REPLACE   first      WITH   'first'   + Str( i )
      REPLACE   last       WITH   'last'    + Str( i )
      REPLACE   street     WITH   'street'  + Str( i )
      REPLACE   city       WITH   'city'    + Str( i )
      REPLACE   state      WITH   Chr( hb_RandomInt( 65, 90 ) ) + Chr( hb_RandomInt( 65, 90 ) )
      REPLACE   zip        WITH   AllTrim( Str( hb_RandomInt( 9999 ) ) )
      REPLACE   hiredate   WITH   Date() -20000 + i
      REPLACE   married    WITH   ( hb_RandomInt() == 1 )
      REPLACE   age        WITH   hb_RandomInt( 99 )
      REPLACE   salary     WITH   hb_RandomInt( 10000 )
      REPLACE   notes      WITH   'notes' + Str( i )
   NEXT i

   USE

RETURN


/*
 * FUNCTION AChoice( t, l, b, r, aInput, cTitle, dummy, nValue )
 *
 * Displays a window with a listbox populated from an array and allows the user
 * to select an item.
 *
 * Parameters:
 *   t      : Top coordinate of the window (unused).
 *   l      : Left coordinate of the window (unused).
 *   b      : Bottom coordinate of the window (unused).
 *   r      : Right coordinate of the window (unused).
 *   aInput : An array of arrays, where each sub-array contains the value and
 *            description to be displayed in the listbox. The first element
 *            of the sub-array is the value, and the second element is the
 *            description.
 *   cTitle : The title of the window (optional, defaults to "Please, select").
 *   dummy  : Unused parameter.
 *   nValue : The initial value of the listbox (optional, defaults to 1).
 *
 * Returns:
 *   The value of the selected item in the listbox, or 0 if the user cancels
 *   the selection.
 *
 * Purpose:
 *   This function provides a simple way to display a list of options to the
 *   user and allow them to select one. It's used here to display the DBF
 *   header information retrieved by the GetHeaderInfo() function in a
 *   user-friendly format. The function creates a modal window with a listbox
 *   containing the data from the aInput array. The user can select an item
 *   from the listbox and click "OK" to return the selected value, or click
 *   "Cancel" to return 0.
 *
 * Notes:
 *   The function uses the AChoice window definition. The parameters t, l, b,
 *   and r are not used within the function, but are included for compatibility
 *   with other AChoice implementations.
 */
FUNCTION AChoice( t, l, b, r, aInput, cTitle, dummy, nValue )

   LOCAL aItems := {}

   HB_SYMBOL_UNUSED( t )
   HB_SYMBOL_UNUSED( l )
   HB_SYMBOL_UNUSED( b )
   HB_SYMBOL_UNUSED( r )
   HB_SYMBOL_UNUSED( dummy )

   DEFAULT cTitle TO "Please, select", nValue TO 1

   AEval( aInput, {|x| AAdd( aItems, x[ 2 ] + ": " + hb_ValToStr( x[ 1 ] ) ) } )

   DEFINE WINDOW Win_2 ;
      AT 0, 0 ;
      WIDTH 400 HEIGHT 400 + IF( IsXPThemeActive(), 7, 0 ) ;
      TITLE cTitle ;
      TOPMOST ;
      NOMAXIMIZE NOSIZE ;
      ON INIT Win_2.Button_1.SetFocus

   @ 335, 190 BUTTON Button_1 ;
      CAPTION 'OK' ;
      ACTION {|| nValue := Win_2.List_1.Value, Win_2.Release } ;
      WIDTH 80

   @ 335, 295 BUTTON Button_2 ;
      CAPTION 'Cancel' ;
      ACTION {|| nValue := 0, Win_2.Release } ;
      WIDTH 80

   @ 20, 15 LISTBOX List_1 ;
      WIDTH 360 ;
      HEIGHT 300 ;
      ITEMS aItems ;
      VALUE nValue ;
      FONT "Ms Sans Serif" ;
      SIZE 12 ;
      ON DBLCLICK {|| nValue := Win_2.List_1.Value, Win_2.Release }

   ON KEY ESCAPE ACTION Win_2.Button_2.OnClick

   END WINDOW

   CENTER WINDOW Win_2
   ACTIVATE WINDOW Win_2

RETURN nValue


#define FIELD_ENTRY_SIZE  32
#define FIELD_NAME_SIZE   11

/*
 * FUNCTION GetHeaderInfo( database )
 *
 * Retrieves header information from a DBF file and returns it as an array.
 *
 * Parameters:
 *   database : The name of the DBF file to open.  If the filename does not
 *              end with ".DBF", it will be appended.
 *
 * Returns:
 *   An array of arrays, where each inner array contains two elements:
 *     - The first element is a string representing a piece of header information.
 *     - The second element is a string describing the information.
 *   Returns an empty array ({}) if the file cannot be opened.
 *
 * Purpose:
 *   This function is designed to read and interpret the header structure of a DBF file.
 *   It extracts key information such as the file type, last update date, number of records,
 *   header size, record size, and field definitions. This information can be used for
 *   various purposes, such as validating the DBF file, displaying file metadata, or
 *   programmatically accessing the data within the DBF file.
 *
 * Notes:
 *   - The function assumes a standard DBF file structure.
 *   - It handles potential date damage in the header but only displays a message; it does not correct the data.
 *   - The function uses hexadecimal conversions to interpret the header data.
 *   - The function reads the field definitions and stores them in the returned array.
 */
FUNCTION GetHeaderInfo( database )

   LOCAL aRet := {}  // Initialize an empty array to store the header information.
   LOCAL nHandle     // File handle for the opened DBF file.
   LOCAL dbfhead     // String to store the first 4 bytes of the DBF header.
   LOCAL h1, h2, h3, h4 // Temporary variables to store hexadecimal representations of bytes.
   LOCAL dbftype     // String to store the DBF file type (e.g., "03h" for dBase III).
   LOCAL headrecs    // String to store the number of records in the file (as read from the header).
   LOCAL headsize    // String to store the header size (as read from the header).
   LOCAL recsize     // String to store the record size (as read from the header).
   LOCAL nof         // Number of fields in the DBF file.
   LOCAL fieldlist   // Array to store the field definitions.
   LOCAL nfield      // Loop counter for iterating through the fields.
   LOCAL nPos        // File position for reading field definitions.
   LOCAL cFieldname  // String to store the name of a field.
   LOCAL cType       // String to store the data type of a field (e.g., "C", "N", "D").
   LOCAL cWidth, nWidth // String and numeric variables to store the width of a field.
   LOCAL nDec, cDec  // Numeric and string variables to store the number of decimal places in a field.

   IF .NOT. '.DBF' $ Upper( database )
      database += '.DBF' // Append ".DBF" to the filename if it's missing.
   ENDIF

   IF ( nHandle := FOpen( database, FO_READ ) ) == -1 // Attempt to open the DBF file in read-only mode.
      msgStop( 'Can not open file ' + Upper( database ) + ' for reading!' ) // Display an error message if the file cannot be opened.
      RETURN aRet                // Return an empty array to indicate failure.
   ENDIF

   dbfhead := Space( 4 )         // Allocate a 4-byte string to store the initial header data.
   FRead( nHandle, @dbfhead, 4 ) // Read the first 4 bytes of the file into the dbfhead variable.

   h1 := FT_BYT2HEX( SubStr( dbfhead, 1, 1 ) )   // Extract the first byte and convert it to its hexadecimal representation. This byte indicates the DBF file type.
   dbftype := h1 // Store the DBF file type.
   h2 := FT_BYT2HEX( SubStr( dbfhead, 2, 1 ) )   // Extract the second byte (year) and convert it to hexadecimal.
   h3 := FT_BYT2HEX( SubStr( dbfhead, 3, 1 ) )   // Extract the third byte (month) and convert it to hexadecimal.
   h4 := FT_BYT2HEX( SubStr( dbfhead, 4, 1 ) )   // Extract the fourth byte (day) and convert it to hexadecimal.

   IF hex2dec( h3 ) > 12 .OR. hex2dec( h4 ) > 31 // Check for invalid month or day values in the date.
      MsgInfo( 'Date damage in header!' ) // Display a warning message if the date is invalid.
   ENDIF

   AAdd( aRet, { '0x' + dbftype, 'Type of file' } ) // Add the file type to the result array.
   AAdd( aRet, { StrZero( hex2dec( h4 ), 2 ) + '.' + StrZero( hex2dec( h3 ), 2 ) + '.' + StrZero( hex2dec( h2 ) -if( hex2dec( h2 ) > 100, 100, 0 ), 2 ), 'Last update (DD.MM.YY)' } ) // Add the last update date to the result array.  The if statement handles the year 2000 and later.

   headrecs := Space( 4 ) // Allocate a 4-byte string to store the number of records.
   FSeek( nHandle, 4, FS_SET ) // Move the file pointer to byte 4 (offset) from the beginning of the file.
   FRead( nHandle, @headrecs, 4 ) // Read the next 4 bytes into the headrecs variable.

   h1 := FT_BYT2HEX( SubStr( headrecs, 1, 1 ) ) // Extract the first byte and convert it to hexadecimal.
   h2 := FT_BYT2HEX( SubStr( headrecs, 2, 1 ) ) // Extract the second byte and convert it to hexadecimal.
   h3 := FT_BYT2HEX( SubStr( headrecs, 3, 1 ) ) // Extract the third byte and convert it to hexadecimal.
   h4 := FT_BYT2HEX( SubStr( headrecs, 4, 1 ) ) // Extract the fourth byte and convert it to hexadecimal.
   headrecs := Int( hex2dec( h1 ) + 256 * hex2dec( h2 ) + ( 256 ** 2 ) * hex2dec( h3 ) + ( 256 ** 3 ) * hex2dec( h4 ) ) // Convert the hexadecimal representation of the number of records to an integer.

   AAdd( aRet, { headrecs, 'Number of records' } ) // Add the number of records to the result array.

   headsize := Space( 2 ) // Allocate a 2-byte string to store the header size.
   FRead( nHandle, @headsize, 2 ) // Read the next 2 bytes into the headsize variable.

   h1 := FT_BYT2HEX( SubStr( headsize, 1, 1 ) ) // Extract the first byte and convert it to hexadecimal.
   h2 := FT_BYT2HEX( SubStr( headsize, 2, 1 ) ) // Extract the second byte and convert it to hexadecimal.
   headsize := hex2dec( h1 ) + 256 * hex2dec( h2 ) // Convert the hexadecimal representation of the header size to an integer.

   AAdd( aRet, { headsize, 'Header size' } ) // Add the header size to the result array.

   recsize := Space( 2 ) // Allocate a 2-byte string to store the record size.
   FRead( nHandle, @recsize, 2 ) // Read the next 2 bytes into the recsize variable.

   h1 := FT_BYT2HEX( SubStr( recsize, 1, 1 ) ) // Extract the first byte and convert it to hexadecimal.
   h2 := FT_BYT2HEX( SubStr( recsize, 2, 1 ) ) // Extract the second byte and convert it to hexadecimal.
   recsize := hex2dec( h1 ) + 256 * hex2dec( h2 ) // Convert the hexadecimal representation of the record size to an integer.

   AAdd( aRet, { recsize, 'Record size' } ) // Add the record size to the result array.

   nof := Int( headsize / 32 ) - 1  // Calculate the number of fields in the DBF file.  Each field entry is 32 bytes long, and the header ends with a terminator byte.

   AAdd( aRet, { nof, 'Fields count' } ) // Add the number of fields to the result array.

   fieldlist := {}                      // Initialize an empty array to store the field definitions.
   FOR nField = 1 TO nof                // Iterate through each field in the DBF file.
      nPos := nField * FIELD_ENTRY_SIZE // Calculate the file offset for the current field's definition.
      FSeek( nHandle, nPos, FS_SET )    // Goto File Offset of the nField-th Field
      cFieldName := Space( FIELD_NAME_SIZE ) // Allocate a string to store the field name.
      FRead( nHandle, @cFieldName, FIELD_NAME_SIZE ) // Read the field name from the file.
      cFieldName := StrTran( cFieldName, Chr( 0 ), ' ' ) // Replace null characters with spaces.
      cFieldName := RTrim( SubStr( cFieldName, 1, At( ' ', cFieldName ) ) ) // Trim trailing spaces from the field name.

      cType := Space( 1 ) // Allocate a string to store the field type.
      FRead( nHandle, @cType, 1 ) // Read the field type from the file.

      FSeek( nHandle, 4, FS_RELATIVE ) // Skip 4 bytes (field displacement).
      IF ctype == 'C' // If the field type is character.
         cWidth := Space( 2 ) // Allocate a string to store the field width.
         FRead( nHandle, @cWidth, 2 ) // Read the field width from the file.
         h1 := FT_BYT2HEX( SubStr( cWidth, 1, 1 ) ) // Extract the first byte and convert it to hexadecimal.
         h2 := FT_BYT2HEX( SubStr( cWidth, 2, 1 ) ) // Extract the second byte and convert it to hexadecimal.
         nWidth := hex2dec( h1 ) + 256 * hex2dec( h2 ) // Convert the hexadecimal representation of the field width to an integer.
         nDec := 0 // Set the number of decimal places to 0 for character fields.
      ELSE // If the field type is not character.
         cWidth := Space( 1 ) // Allocate a string to store the field width.
         FRead( nHandle, @cWidth, 1 ) // Read the field width from the file.
         nWidth := hex2dec( FT_BYT2HEX( cWidth ) ) // Convert the hexadecimal representation of the field width to an integer.
         cDec := Space( 1 ) // Allocate a string to store the number of decimal places.
         FRead( nHandle, @cDec, 1 ) // Read the number of decimal places from the file.
         nDec := hex2dec( FT_BYT2HEX( cDec ) ) // Convert the hexadecimal representation of the number of decimal places to an integer.
      ENDIF
      AAdd( fieldlist, { cFieldName, cType, nWidth, nDec } ) // Add the field definition to the field list.
   NEXT

   FClose( nHandle ) // Close the DBF file.

   AAdd( aRet, { '', 'Fields structure' } ) // Add a separator to the result array.
   AEval( fieldlist, {|x, i| AAdd( aRet, { x[ 1 ] + " - " + x[ 2 ] + "(" + hb_ntos( x[ 3 ] ) + "," + hb_ntos( x[ 4 ] ) + ")", hb_ntos( i ) } ) } ) // Iterate through the field list and add each field's information to the result array.

RETURN aRet // Return the array containing the header information.


#define HEXTABLE "0123456789ABCDEF"

/*
 * FUNCTION HEX2DEC( cHexNum )
 *
 * Converts a hexadecimal number (string) to its decimal equivalent.
 *
 * Parameters:
 *   cHexNum : The hexadecimal number to convert (string).
 *
 * Returns:
 *   The decimal equivalent of the hexadecimal number (numeric).
 *
 * Purpose:
 *   This function is used to convert hexadecimal values read from the DBF
 *   header into decimal values that can be used for calculations and
 *   interpretation.  It's a utility function used by GetHeaderInfo.
 *
 * Notes:
 *   - The function handles hexadecimal numbers of any length.
 *   - It converts the input to uppercase for case-insensitive conversion.
 */
FUNCTION HEX2DEC( cHexNum )

   LOCAL n, nDec := 0, nHexPower := 1 // Initialize variables: n (loop counter), nDec (decimal result), nHexPower (power of 16).

   FOR n := Len( cHexNum ) TO 1 STEP -1 // Iterate through the hexadecimal number from right to left.
      nDec += ( At( subs( Upper( cHexNum ), n, 1 ), HEXTABLE ) - 1 ) * nHexPower // Calculate the decimal value of the current digit and add it to the result.
      nHexPower *= 16 // Update the power of 16 for the next digit.
   NEXT

RETURN nDec // Return the decimal equivalent of the hexadecimal number.


/*
 * FUNCTION FT_BYT2HEX( cByte, plusH )
 *
 * Converts a single byte (character) to its hexadecimal representation (string).
 *
 * Parameters:
 *   cByte : The byte (character) to convert.
 *   plusH : Optional logical value. If .T., appends "h" to the hexadecimal string. Default is .F.
 *
 * Returns:
 *   The hexadecimal representation of the byte (string).
 *
 * Purpose:
 *   This function is used to convert individual bytes read from the DBF header
 *   into their hexadecimal string representation. This is useful for debugging
 *   and for displaying the raw header data.
 *
 * Notes:
 *   - The function only works with single-byte characters.
 *   - The plusH parameter allows for adding the "h" suffix, which is a common
 *     convention for representing hexadecimal numbers.
 */
FUNCTION FT_BYT2HEX( cByte, plusH )

   LOCAL xHexString // Variable to store the hexadecimal string.

   DEFAULT plusH := .F. // Set the default value of plusH to .F.

   IF ValType( cByte ) == "C"                                             // Check if the input is a character.
      xHexString := SubStr( HEXTABLE, Int( Asc( cByte ) / 16 ) + 1, 1 ) ; // Get the first hexadecimal digit.
         + SubStr( HEXTABLE, Int( Asc( cByte ) % 16 ) + 1, 1 ) ;          // Get the second hexadecimal digit.
         + iif( plusH, "h", '' )                                          // Append "h" if plusH is .T.
   ENDIF

RETURN xHexString // Return the hexadecimal string.
