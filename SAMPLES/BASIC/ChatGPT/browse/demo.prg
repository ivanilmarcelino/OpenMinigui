#include "minigui.ch"

FUNCTION Main()

   SET BROWSESYNC ON
   SET NAVIGATION EXTENDED

   // Open DBF file (creates it if it doesn't exist)
   IF ! File( "data.dbf" )
      CreateDatabase()
   ENDIF
   USE DATA NEW ALIAS DataDB SHARED

   // Define Main Window
   DEFINE WINDOW oMainWin ;
      WIDTH 600 ;
      HEIGHT 400 ;
      TITLE "Main Window with Database Browse" ;
      MAIN ;
      ON RELEASE {|| CloseDatabase() }

      // Define Browse Control in the Main Window
      @ 10, 10 BROWSE oBrowse ;
         WIDTH 560 HEIGHT 300 ;
         HEADERS { "Name", "Age", "City" } ;
         WIDTHS { 200, 100, 200 } ;
         WORKAREA DataDB ;
         FIELDS { "Name", "Age", "City" } ;
         ON DBLCLICK {|| ShowDetailWindow() }

      @ 320, 10 LABEL NUL VALUE "Double click to edit the browse record" WIDTH 560 CENTERALIGN
   END WINDOW

   CENTER WINDOW oMainWin
   ACTIVATE WINDOW oMainWin

RETURN NIL

// Function to create a new DBF file
FUNCTION CreateDatabase()

   LOCAL aStruct

   // Define DBF structure
   aStruct := { ;
      { "Name", "C", 50, 0 }, ;
      { "Age", "N", 3, 0 }, ;
      { "City", "C", 50, 0 } }

   // Create the DBF file
   dbCreate( "data.dbf", aStruct )
   USE DATA NEW ALIAS DataDB EXCLUSIVE
   APPEND BLANK
   REPLACE NAME WITH "John Doe", Age WITH 25, City WITH "New York"
   APPEND BLANK
   REPLACE NAME WITH "Jane Smith", Age WITH 30, City WITH "Los Angeles"
   APPEND BLANK
   REPLACE NAME WITH "Mike Brown", Age WITH 35, City WITH "Chicago"
   CLOSE DataDB

RETURN NIL

// Function to close the database
FUNCTION CloseDatabase()
   IF Used( "DataDB" )
      CLOSE DataDB
   ENDIF

RETURN NIL

// Function to display a floating window for editing a record
FUNCTION ShowDetailWindow()

   LOCAL cName, nAge, cCity

   // Ensure a record is selected
   IF Eof()
      MsgStop( "No record selected!", "Error" )
      RETURN NIL
   ENDIF

   // Retrieve the selected record data
   cName := Trim( FieldGet( 1 ) )
   nAge := FieldGet( 2 )
   cCity := Trim( FieldGet( 3 ) )

   // Define Floating Child Window
   DEFINE WINDOW oChildWin ;
      WIDTH 300 ;
      HEIGHT 200 ;
      TITLE "Edit Record Details" ;
      MODAL

      // Editable Fields
      @ 10, 10 LABEL NUL VALUE "Name:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 10, 100 TEXTBOX oEditName ;
         VALUE cName ;
         WIDTH 150

      @ 40, 10 LABEL NUL VALUE "Age:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 40, 100 TEXTBOX oEditAge ;
         VALUE nAge ;
         WIDTH 150 NUMERIC RIGHTALIGN

      @ 70, 10 LABEL NUL VALUE "City:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 70, 100 TEXTBOX oEditCity ;
         VALUE cCity ;
         WIDTH 150

      // Save Button
      @ 120, 100 BUTTON oSaveButton ;
         CAPTION "Save" ;
         ACTION {|| SaveRecord( 'oChildWin' ), ;
            _ReleaseWindow( 'oChildWin' ) }

   END WINDOW

   CENTER WINDOW oChildWin
   ACTIVATE WINDOW oChildWin

RETURN NIL

// Function to save the record back to the DBF file
FUNCTION SaveRecord( cChildWin )

   LOCAL cName, nAge, cCity

   // Get edited values using the parent window name
   cName := GetProperty( cChildWin, 'oEditName', 'Value' )
   nAge := GetProperty( cChildWin, 'oEditAge', 'Value' )
   cCity := GetProperty( cChildWin, 'oEditCity', 'Value' )

   // Validate inputs
   IF Empty( cName ) .OR. nAge <= 0 .OR. Empty( cCity )
      MsgStop( "Invalid input! Please check the fields." )
      RETURN NIL
   ENDIF

   // Lock the record for updating
   IF RLock()
      // Update the record in the database
      REPLACE NAME WITH cName, Age WITH nAge, City WITH cCity

      // Commit the transaction
      dbCommit()
      dbUnlock()
      MsgInfo( "Record updated!" )

      // Refresh the browse control
      oMainWin.oBrowse.Refresh
   ELSE
      MsgStop( "Failed to lock the record. Please try again." )
   ENDIF

RETURN NIL
