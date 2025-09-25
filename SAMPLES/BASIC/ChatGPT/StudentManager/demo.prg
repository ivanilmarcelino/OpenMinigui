#include "hmg.ch"  // Include MiniGUI definitions and constants

REQUEST DBFCDX     // Request DBFCDX driver for database operations

//------------------------------------------------------------------------------ 
// FUNCTION: Main
// Purpose: Application entry point. Initializes main window, menu, grid, and buttons.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
FUNCTION Main()
   // Define main window with properties and initialization
   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 420 ;
         MAIN ;
         TITLE "MiniGUI Student Manager - Live CRUD" ;
         ON INIT OpenDB()

      // Define main menu with File and Record options
      DEFINE MAIN MENU
         POPUP "&File"
            ITEM "&Exit" ACTION Win_1.RELEASE
         END POPUP
         POPUP "&Record"
            ITEM "&Add"    ACTION AddStudent()
            ITEM "&Edit"   ACTION EditStudent()
            ITEM "&Delete" ACTION DeleteStudent()
            ITEM "&Reload" ACTION LoadStudents()
         END POPUP
      END MENU

      // Define grid for displaying student records
      @ 10, 10 GRID grdStudents ;
         WIDTH 600 HEIGHT 300 ;
         HEADERS { "ID", "Name", "Class", "Score" } ;
         WIDTHS { 50, 200, 100, 100 } ;
         JUSTIFY { GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_LEFT, GRID_JTFY_RIGHT } ;
         EDIT ;
         INPLACE { ;
         { 'TEXTBOX', 'NUMERIC' }, ;
         { 'TEXTBOX', 'CHARACTER' }, ;
         { 'TEXTBOX', 'CHARACTER' }, ;
         { 'TEXTBOX', 'NUMERIC', '99.99' } ;
         } ;
         COLUMNVALID { {|| UpdateStudent( 1 ) }, {|| UpdateStudent( 2 ) }, {|| UpdateStudent( 3 ) }, {|| UpdateStudent( 4 ) } }

      // Define action buttons for CRUD operations
      @ 320, 10  BUTTON btnAdd  CAPTION "Add"    WIDTH 90 ACTION AddStudent()
      @ 320, 110 BUTTON btnEdit CAPTION "Edit"   WIDTH 90 ACTION EditStudent()
      @ 320, 210 BUTTON btnDel  CAPTION "Delete" WIDTH 90 ACTION DeleteStudent()
   END WINDOW

   // Center and activate the main window
   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1
RETURN NIL

//------------------------------------------------------------------------------
// PROCEDURE: OpenDB
// Purpose: Opens or creates students.dbf and loads initial data into grid.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE OpenDB()
   // Create DBF if it doesn't exist
   IF ! File( "students.dbf" )
      CreateDBF()
   ENDIF
   // Load student records into grid
   LoadStudents()
   // Select first row in grid
   Win_1.grdStudents.VALUE := 1
RETURN

//------------------------------------------------------------------------------
// PROCEDURE: LoadStudents
// Purpose: Loads student records from DBF into the grid.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE LoadStudents()
   LOCAL aData := {}
   // Open DBF in shared mode
   USE students NEW VIA "DBFCDX" ALIAS STUDENTS SHARED
   IF Used()
      GO TOP
      // Read records into array
      DO WHILE ! Eof()
         AAdd( aData, { STUDENTS->ID, STUDENTS->NAME, STUDENTS->CLASS, STUDENTS->SCORE } )
         SKIP
      ENDDO
      USE
   ENDIF
   // Set grid data from array
   Win_1.grdStudents.SetArray( aData )
RETURN

//------------------------------------------------------------------------------
// PROCEDURE: AddStudent
// Purpose: Adds a new student record to the DBF and refreshes grid.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE AddStudent()
   LOCAL aNew := { "", "", "", "" }
   // Set default ID based on last record
   aNew[ 1 ] := hb_ntos( Win_1.grdStudents.Item( Win_1.grdStudents.ItemCount )[ 1 ] + 1 )
   // Prompt user for new student data
   IF InputBoxArray( "Enter new student data", { "ID", "Name", "Class", "Score" }, @aNew )
      // Append new record to DBF
      USE students EXCLUSIVE
      APPEND BLANK
      REPLACE ID    WITH Val( aNew[ 1 ] )
      REPLACE NAME  WITH aNew[ 2 ]
      REPLACE CLASS WITH aNew[ 3 ]
      REPLACE SCORE WITH Val( aNew[ 4 ] )
      USE
      // Refresh grid with updated data
      LoadStudents()
      Win_1.grdStudents.VALUE := Win_1.grdStudents.ItemCount
   ENDIF
RETURN

//------------------------------------------------------------------------------
// PROCEDURE: EditStudent
// Purpose: Edits selected student record in the DBF and refreshes grid.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE EditStudent()
   LOCAL nRow := Win_1.grdStudents.VALUE
   LOCAL aRow
   // Check if a row is selected
   IF nRow > 0
      aRow := Win_1.grdStudents.Item( nRow )
      aRow[ 1 ] := hb_ntos( aRow[ 1 ] )
      aRow[ 4 ] := hb_ntos( aRow[ 4 ] )
      // Prompt user to edit student data
      IF InputBoxArray( "Edit student data", { "ID", "Name", "Class", "Score" }, @aRow )
         // Update record in DBF
         USE students EXCLUSIVE
         GO nRow
         REPLACE ID    WITH Val( aRow[ 1 ] )
         REPLACE NAME  WITH aRow[ 2 ]
         REPLACE CLASS WITH aRow[ 3 ]
         REPLACE SCORE WITH Val( aRow[ 4 ] )
         USE
         // Refresh grid
         LoadStudents()
      ENDIF
   ELSE
      MsgStop( "Please select a row to edit." )
   ENDIF
RETURN

//------------------------------------------------------------------------------
// PROCEDURE: DeleteStudent
// Purpose: Deletes selected student record from DBF and refreshes grid.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE DeleteStudent()
   LOCAL nRow := Win_1.grdStudents.VALUE
   // Check if a row is selected
   IF nRow > 0
      // Confirm deletion with user
      IF MsgYesNo( "Delete selected student?" )
         // Delete record and pack DBF
         USE students EXCLUSIVE
         GO nRow
         DELETE
         PACK
         USE
         // Adjust grid selection
         Win_1.grdStudents.VALUE := iif( nRow == Win_1.grdStudents.ItemCount, nRow - 1, nRow )
         // Refresh grid
         LoadStudents()
      ENDIF
   ELSE
      MsgStop( "Please select a row to delete." )
   ENDIF
RETURN

//------------------------------------------------------------------------------
// FUNCTION: UpdateStudent
// Purpose: Updates a single student field in the DBF during in-place grid editing.
// Parameters: nCol (Numeric) - Column index being edited
// Returns: Logical (.T.) - Indicates successful update
//------------------------------------------------------------------------------
FUNCTION UpdateStudent( nCol )
   LOCAL nRow := Win_1.grdStudents.VALUE
   LOCAL aRow
   // Check if a row is selected
   IF nRow > 0
      aRow := Win_1.grdStudents.Item( nRow )
      aRow[ nCol ] := This.CellValue
      // Update record in DBF
      USE students EXCLUSIVE
      GO nRow
      REPLACE ID    WITH aRow[ 1 ]
      REPLACE NAME  WITH aRow[ 2 ]
      REPLACE CLASS WITH aRow[ 3 ]
      REPLACE SCORE WITH aRow[ 4 ]
      USE
   ENDIF
RETURN .T.

//------------------------------------------------------------------------------
// FUNCTION: InputBoxArray
// Purpose: Displays a multi-field input dialog for adding/editing student data.
// Parameters:
//   cTitle   (Character) - Dialog title
//   aPrompts (Array)     - Field labels
//   aValues  (Array*)    - Values updated by reference
//   aFormats (Array)     - Optional column widths
// Returns: Logical - .T. if successful, .F. if canceled
//------------------------------------------------------------------------------
FUNCTION InputBoxArray( cTitle, aPrompts, aValues, aFormats )
   LOCAL i, aResults
   // Set default column widths if not provided
   DEFAULT aFormats := { 3, 25, 10, 5 }
   aResults := InputWindow( cTitle, aPrompts, aValues, aFormats )
   // Update values if input is valid
   FOR i := 1 TO Len( aResults )
      IF aResults[ i ] == NIL
         RETURN .F.
      ENDIF
      aValues[ i ] := aResults[ i ]
   NEXT
RETURN .T.

//------------------------------------------------------------------------------
// PROCEDURE: CreateDBF
// Purpose: Creates students.dbf with predefined structure and sample records.
// Parameters: None
// Returns: NIL
//------------------------------------------------------------------------------
PROCEDURE CreateDBF()
   LOCAL aStruct := {}
   LOCAL aSample, i
   // Define DBF structure
   AAdd( aStruct, { "ID",    "N", 3, 0 } )
   AAdd( aStruct, { "NAME",  "C", 25, 0 } )
   AAdd( aStruct, { "CLASS", "C", 10, 0 } )
   AAdd( aStruct, { "SCORE", "N", 5, 2 } )
   // Create DBF if it doesn't exist
   IF ! File( "students.dbf" )
      dbCreate( "students.dbf", aStruct, "DBFCDX" )
      MsgInfo( "File students.dbf created!" )
   ENDIF
   // Insert sample data
   USE students NEW VIA "DBFCDX"
   aSample := { ;
      { 1, "Alice Smith",   "Class A", 89.50 }, ;
      { 2, "Bob Johnson",   "Class B", 76.25 }, ;
      { 3, "Charlie Brown", "Class A", 92.00 }, ;
      { 4, "Diana Ross",    "Class C", 68.75 }, ;
      { 5, "Ethan Clark",   "Class B", 81.10 } }
   FOR i := 1 TO Len( aSample )
      APPEND BLANK
      REPLACE ID    WITH aSample[ i, 1 ]
      REPLACE NAME  WITH aSample[ i, 2 ]
      REPLACE CLASS WITH aSample[ i, 3 ]
      REPLACE SCORE WITH aSample[ i, 4 ]
   NEXT
   MsgInfo( "Sample data inserted!" )
   USE
RETURN