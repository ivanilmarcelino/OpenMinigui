/*
 * MiniGUI Demo Program
 * Demonstrates a simple database browse with editing functionality using
 * Harbour MiniGUI.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
*/

#include "minigui.ch"

// Main function: Entry point of the application.
// Sets up the database, defines the main window with a browse control,
// and handles window activation and release.
FUNCTION Main()

   // Enable browse synchronization and extended navigation mode for better user interaction.
   SET BROWSESYNC ON
   SET NAVIGATION EXTENDED

   // Check if the database file exists; create it if not.
   IF ! File( "data.dbf" )
      CreateDatabase()
   ENDIF
   // Open the database file for shared access.
   USE DATA NEW ALIAS DataDB SHARED

   // Define the main window properties.
   DEFINE WINDOW oMainWin ;
      WIDTH 600 ;
      HEIGHT 400 ;
      TITLE "Main Window with Database Browse" ;
      MAIN ;
      ON RELEASE {|| CloseDatabase() }  // Close database on window release.

      // Define the browse control to display database records.
      @ 10, 10 BROWSE oBrowse ;
         WIDTH 560 HEIGHT 300 ;
         HEADERS { "Name", "Age", "City" } ;  // Column headers.
         WIDTHS { 200, 100, 200 } ;  // Column widths.
         WORKAREA DataDB ;  // Alias of the database.
         FIELDS { "Name", "Age", "City" } ;  // Fields to display.
         ON DBLCLICK {|| ShowDetailWindow() }  // Open edit window on double-click.

      // Add a label with instructions for the user.
      @ 320, 10 LABEL NUL VALUE "Double click to edit the browse record" WIDTH 560 CENTERALIGN
   END WINDOW

   // Center and activate the main window.
   CENTER WINDOW oMainWin
   ACTIVATE WINDOW oMainWin
RETURN NIL

// Function to create a new database file with sample data.
// Defines the structure and populates initial records.
FUNCTION CreateDatabase()
   LOCAL aStruct

   // Define the database structure: fields, types, lengths, decimals.
   aStruct := { ;
      { "Name", "C", 50, 0 }, ;
      { "Age", "N", 3, 0 }, ;
      { "City", "C", 50, 0 } }

   // Create the DBF file using the defined structure.
   dbCreate( "data.dbf", aStruct )

   // Open the database exclusively to add records.
   USE DATA NEW ALIAS DataDB EXCLUSIVE
   // Add sample records.
   APPEND BLANK
   REPLACE NAME WITH "John Doe", Age WITH 25, City WITH "New York"
   APPEND BLANK
   REPLACE NAME WITH "Jane Smith", Age WITH 30, City WITH "Los Angeles"
   APPEND BLANK
   REPLACE NAME WITH "Mike Brown", Age WITH 35, City WITH "Chicago"
   APPEND BLANK
   REPLACE NAME WITH "Alice Johnson", Age WITH 28, City WITH "San Francisco"
   APPEND BLANK
   REPLACE NAME WITH "Bob White", Age WITH 42, City WITH "Miami"
   APPEND BLANK
   REPLACE NAME WITH "Charlie Black", Age WITH 31, City WITH "Seattle"
   APPEND BLANK
   REPLACE NAME WITH "David Green", Age WITH 27, City WITH "Boston"
   APPEND BLANK
   REPLACE NAME WITH "Eve Blue", Age WITH 45, City WITH "Denver"
   APPEND BLANK
   REPLACE NAME WITH "Frank Red", Age WITH 33, City WITH "Austin"
   APPEND BLANK
   REPLACE NAME WITH "Grace Yellow", Age WITH 29, City WITH "Portland"
   APPEND BLANK
   REPLACE NAME WITH "Henry Purple", Age WITH 38, City WITH "Atlanta"
   APPEND BLANK
   REPLACE NAME WITH "Ivy Orange", Age WITH 26, City WITH "Phoenix"
   APPEND BLANK
   REPLACE NAME WITH "Jack Pink", Age WITH 40, City WITH "Orlando"
   APPEND BLANK
   REPLACE NAME WITH "Kelly Brown", Age WITH 32, City WITH "Las Vegas"
   APPEND BLANK
   REPLACE NAME WITH "Leo Gray", Age WITH 37, City WITH "Nashville"
   // Close the database after population.
   CLOSE DataDB
RETURN NIL

// Function to close the database if it's in use.
// Ensures proper cleanup on application exit.
FUNCTION CloseDatabase()
   IF Used( "DataDB" )
      CLOSE DataDB
   ENDIF
RETURN NIL

// Function to show a modal window for editing the selected record.
// Retrieves current record data and displays editable fields.
FUNCTION ShowDetailWindow()
   LOCAL cName, nAge, cCity

   // Check if a valid record is selected.
   IF Eof()
      MsgStop( "No record selected!", "Error" )
      RETURN NIL
   ENDIF

   // Retrieve field values from the current record.
   cName := Trim( FieldGet( 1 ) )
   nAge := FieldGet( 2 )
   cCity := Trim( FieldGet( 3 ) )

   // Define the modal child window for editing.
   DEFINE WINDOW oChildWin ;
      WIDTH 300 ;
      HEIGHT 200 ;
      TITLE "Edit Record Details" ;
      MODAL

      // Labels and textboxes for editing fields.
      @ 10, 10 LABEL NUL VALUE "Name:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 10, 100 TEXTBOX oEditName ;
         VALUE cName ;
         WIDTH 150

      @ 40, 10 LABEL NUL VALUE "Age:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 40, 100 TEXTBOX oEditAge ;
         VALUE nAge ;
         WIDTH 150 NUMERIC RIGHTALIGN  // Numeric input with right alignment.

      @ 70, 10 LABEL NUL VALUE "City:" WIDTH 80 RIGHTALIGN VCENTERALIGN
      @ 70, 100 TEXTBOX oEditCity ;
         VALUE cCity ;
         WIDTH 150

      // Button to save changes and close the window.
      @ 120, 100 BUTTON oSaveButton ;
         CAPTION "Save" ;
         ACTION {|| SaveRecord( 'oChildWin' ), ;
         _ReleaseWindow( 'oChildWin' ) }  // Save and release window.

   END WINDOW

   // Center and activate the child window.
   CENTER WINDOW oChildWin
   ACTIVATE WINDOW oChildWin
RETURN NIL

// Function to save edited record back to the database.
// Validates input, locks record, updates fields, and refreshes browse.
FUNCTION SaveRecord( cChildWin )
   LOCAL cName, nAge, cCity

   // Retrieve edited values from the child window controls.
   cName := GetProperty( cChildWin, 'oEditName', 'Value' )
   nAge := GetProperty( cChildWin, 'oEditAge', 'Value' )
   cCity := GetProperty( cChildWin, 'oEditCity', 'Value' )

   // Validate the input fields.
   IF Empty( cName ) .OR. nAge <= 0 .OR. Empty( cCity )
      MsgStop( "Invalid input! Please check the fields." )
      RETURN NIL
   ENDIF

   // Attempt to lock the record for update.
   IF RLock()
      // Update the database fields.
      REPLACE NAME WITH cName, Age WITH nAge, City WITH cCity

      // Commit changes and unlock.
      dbCommit()
      dbUnlock()
      MsgInfo( "Record updated!" )

      // Refresh the browse control in the main window.
      oMainWin.oBrowse.Refresh
   ELSE
      MsgStop( "Failed to lock the record. Please try again." )
   ENDIF
RETURN NIL
