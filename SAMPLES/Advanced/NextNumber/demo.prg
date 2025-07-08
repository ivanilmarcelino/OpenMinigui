/*

 BadaSystem
 Program       : nextnumber
 Modulo        : demo
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 29/07/2022
 Update        : 30/07/2022

*/

#include "minigui.ch" // Includes the MiniGUI header file, providing access to MiniGUI functions and definitions.
#include "dbstruct.ch" // Includes the DBSTRUCT header file, providing access to database structure related definitions.

REQUEST DBFCDX  // Requests the DBFCDX RDD (Replaceable Database Driver)

//-----------------------------------------------------------------------------
// PROCEDURE Main()
//
// Description:
//   This is the main procedure of the application. It defines and activates
//   the main window, sets up the database environment, and handles user
//   interactions.
//-----------------------------------------------------------------------------
PROCEDURE Main()

   rddSetDefault( "DBFCDX" ) // Sets the default RDD to DBFCDX, which is used for accessing DBF files.

   DEFINE WINDOW test ; // Defines a window named 'test'.
         AT 0, 0 ; // Sets the window's position to the top-left corner of the screen.
         WIDTH 400 ; // Sets the window's width to 400 pixels.
         HEIGHT 580 ; // Sets the window's height to 580 pixels.
         TITLE "TEST NEXT NUMBER" ; // Sets the window's title.
         MAIN ; // Designates this window as the main window of the application.
         NOMAXIMIZE ; // Disables the maximize button on the window.
         FONT 'ARIAL' SIZE 9 ; // Sets the font for the window's controls.
         ON INIT OpenTable() ; // Specifies the OpenTable() procedure to be executed when the window is initialized.  This is where the database table is opened or created.
         ON RELEASE CloseTable() // Specifies the CloseTable() procedure to be executed when the window is released (closed). This is where the database table is closed.


      @ 10, 10 BROWSE Browse_1 ; // Defines a browse control named 'Browse_1'.
         WIDTH 380 ; // Sets the browse control's width.
         HEIGHT 480 ; // Sets the browse control's height.
         HEADERS { 'Numeric code', 'Character code' } ; // Sets the column headers for the browse control.
         WIDTHS { 170, 170 } ; // Sets the column widths for the browse control.
         WORKAREA test ; // Specifies the work area (database table) to be used by the browse control.
         FIELDS { 'test->codigo_n', 'test->codigo_c' } // Specifies the fields from the work area to be displayed in the browse control.


      @ 500, 70 BUTTON PROCESO CAPTION '&Process' ACTION Proceso() // Defines a button named 'PROCESO' with the caption "&Process" and associates it with the Proceso() procedure.
      @ 500, 240 BUTTON EXIT_ALL CAPTION '&Exit' ACTION test.RELEASE // Defines a button named 'EXIT_ALL' with the caption "&Exit" and associates it with the window's RELEASE event (closing the window).


   END WINDOW

   CENTER WINDOW test // Centers the window on the screen.

   ACTIVATE WINDOW test // Activates the window, making it visible and responsive to user input.

RETURN

//-----------------------------------------------------------------------------
// PROCEDURE OpenTable()
//
// Description:
//   This procedure opens the "test.dbf" table if it exists, or creates it
//   if it doesn't. It also creates indexes on the 'codigo_n' and 'codigo_c'
//   fields.
//-----------------------------------------------------------------------------
PROCEDURE OpenTable()

   FIELD codigo_n, codigo_c IN test // Declares the fields 'codigo_n' and 'codigo_c' as belonging to the 'test' work area.  This is important for accessing the fields without specifying the alias.
   LOCAL aStructure := {} // Declares a local array variable 'aStructure' and initializes it as an empty array.  This array will hold the structure of the database table if it needs to be created.

   IF File( "test.dbf" ) // Checks if the file "test.dbf" exists.
      dbUseArea( .T., , "test", "test", .T. ) // Opens the "test.dbf" table in a new work area, aliased as "test". The .T. parameters indicate shared access and that the table should be opened even if it's already open.
      SET ORDER TO TAG codigo_n // Sets the active index order to the tag "codigo_n".
   ELSE // If the file "test.dbf" does not exist:
      AAdd( aStructure, { "codigo_n", "N", 4, 0 } ) // Adds a structure element to the 'aStructure' array, defining the 'codigo_n' field as a numeric field with a length of 4 and 0 decimal places.
      AAdd( aStructure, { "codigo_c", "C", 4, 0 } ) // Adds a structure element to the 'aStructure' array, defining the 'codigo_c' field as a character field with a length of 4.

      dbCreate( "test", aStructure ) // Creates the "test.dbf" table using the structure defined in the 'aStructure' array.
      USE test ALIAS test NEW shared // Opens the newly created "test.dbf" table in a new work area, aliased as "test", with shared access.
      INDEX ON codigo_n TAG codigo_n // Creates an index on the 'codigo_n' field and assigns it the tag "codigo_n".
      INDEX ON codigo_c TAG codigo_c // Creates an index on the 'codigo_c' field and assigns it the tag "codigo_c".
      SET ORDER TO TAG codigo_n // Sets the active index order to the tag "codigo_n".
   ENDIF

RETURN

//-----------------------------------------------------------------------------
// PROCEDURE CloseTable()
//
// Description:
//   This procedure closes all open database tables.
//-----------------------------------------------------------------------------
PROCEDURE CloseTable()

   dbCloseAll() // Closes all open database tables.

RETURN

//-----------------------------------------------------------------------------
// PROCEDURE Proceso()
//
// Description:
//   This procedure appends a new record to the "test" table and populates
//   the 'codigo_n' and 'codigo_c' fields with the next available numbers,
//   using the NextNumber() function. It also includes error handling to
//   prevent values exceeding the maximum allowed length.
//-----------------------------------------------------------------------------
PROCEDURE Proceso()

   LOCAL aStructure := {} // Declares a local array variable 'aStructure' and initializes it as an empty array.
   LOCAL nValor1, nValor2 // Declares local numeric variables 'nValor1' and 'nValor2'.

   aStructure := test->( dbStruct() ) // Retrieves the structure of the "test" table and assigns it to the 'aStructure' array.  The dbStruct() function returns an array describing the structure of the current database.

   test->( dbAppend() ) // Appends a new, blank record to the "test" table.
   nValor1 := NextNumber( "test", "codigo_n", 1 ) // Calls the NextNumber() function to get the next available number for the 'codigo_n' field, using increment 1.  The result is stored in 'nValor1'.
   nValor2 := NextNumber( "test", "codigo_c", 2 ) // Calls the NextNumber() function to get the next available number for the 'codigo_c' field, using increment 2.  The result is stored in 'nValor2'.

   IF Len( nValor2 ) > aStructure[ 2 ][ DBS_LEN ] // Checks if the length of the generated value for 'codigo_c' exceeds the maximum allowed length defined in the table structure.  aStructure[2][DBS_LEN] gets the length of the second field (codigo_c) from the structure array.
      msgbox( "Value is larger than the maximum allowed" ) // Displays a message box indicating that the generated value is too large.
   ELSE // If the generated value is within the allowed length:
      test->codigo_n := NextNumber( "test", "codigo_n", 1 ) // Assigns the next available number (incremented by 1) to the 'codigo_n' field in the current record of the "test" table.
      test->codigo_c := NextNumber( "test", "codigo_c", 2 ) // Assigns the next available number (incremented by 2) to the 'codigo_c' field in the current record of the "test" table.
   ENDIF

   _BrowseEnd( "browse_1", "test" ) // Refreshes the browse control 'browse_1' to reflect the changes in the "test" table.  This is a MiniGUI function.
   test.browse_1.Setfocus // Sets the focus to the browse control 'browse_1'.

RETURN
