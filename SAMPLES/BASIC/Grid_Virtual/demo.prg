/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "hmg.ch"

MEMVAR aHeaders, aWidths, aJust  // Memory variables to store grid column headers, widths, and justification.
MEMVAR aSalaries                 // Memory variable to store the salary data (array of arrays).
MEMVAR aValid                    // Memory variable to store validation functions for each column.
MEMVAR aWhen                     // Memory variable to store WHEN conditions for each column (when editing is allowed).
MEMVAR aControls                 // Memory variable to store control types for each column (e.g., TEXTBOX, NUMERIC).
MEMVAR aRows                     // Memory variable to store data for the second grid.

// ------------
FUNCTION Main()
/*
 *  Main function of the application.
 *
 *  This function defines and activates the main window of the application,
 *  which contains an editable virtual grid. It initializes the grid's data,
 *  column properties, and event handlers.
 */

   PRIVATE aHeaders, aWidths, aJust  // Private variables to hold grid column properties.
   PRIVATE aSalaries := {}           // Private variable to store salary data (initialized as an empty array).
   PRIVATE aValid := {}              // Private variable to store validation functions (initialized as an empty array).
   PRIVATE aWhen := {}               // Private variable to store WHEN conditions (initialized as an empty array).
   PRIVATE aControls := {}           // Private variable to store control types (initialized as an empty array).

   SET NAVIGATION EXTENDED  // Enables extended navigation features within the application.

   aHeaders := { "Name", "Salary" }  // Defines the column headers for the grid.
   aWidths := { 200, 100 }           // Defines the column widths for the grid.
   aJust := { 0, 1 }                 // Defines the column justification (0=Left, 1=Right).

   aValid := { {|| DataValidation() }, {|| DataValidation() } }  // Defines validation functions for each column.
   aWhen := { {|| .T. }, {|| .T. } }                             // Defines WHEN conditions for each column (always true in this case).
   aControls := { { "TEXTBOX", "CHARACTER" }, { "TEXTBOX", "NUMERIC", "9999.99" } }  // Defines control types for each column.

   AAdd( aSalaries, { "Simpson", 65.00 } )   // Adds initial salary data to the array.
   AAdd( aSalaries, { "Mulder", 41.00 } )    // Adds initial salary data to the array.
   AAdd( aSalaries, { "Smart Max", 25.00 } ) // Adds initial salary data to the array.

   DEFINE WINDOW Form_1 ;  // Defines the main window.
         AT 100, 100 ;      // Sets the window position.
         WIDTH 500 ;        // Sets the window width.
         HEIGHT 550 ;       // Sets the window height.
         TITLE 'Editable Virtual Grid Test' ;  // Sets the window title.
         MAIN               // Designates this window as the main window.

      @ 10, 10 GRID Grid_1 ;  // Defines a grid control within the window.
         WIDTH 320 ;         // Sets the grid width.
         HEIGHT 340 ;        // Sets the grid height.
         HEADERS aHeaders ;    // Assigns the column headers.
         WIDTHS aWidths ;      // Assigns the column widths.
         VALUE { 1, 1 } ;      // Sets the initial selected cell.
         TOOLTIP 'Editable Grid Control' ;  // Sets the tooltip text.
         EDIT ;                  // Enables editing of the grid cells.
         COLUMNCONTROLS aControls ;  // Assigns the column control types.
         COLUMNVALID aValid ;      // Assigns the column validation functions.
         COLUMNWHEN aWhen ;        // Assigns the column WHEN conditions.
         VIRTUAL ;               // Enables virtual mode (data is retrieved on demand).
         ITEMCOUNT Len( aSalaries ) ;  // Sets the number of rows in the grid.
         ON QUERYDATA OnQuery( aSalaries ) ;  // Assigns the function to retrieve data for the grid.
         JUSTIFY aJust ;           // Assigns the column justification.
         CELLNAVIGATION ;        // Enables cell navigation using arrow keys.
         ON GOTFOCUS SetTotal()   // Calls SetTotal() when the grid receives focus.

      @ 360, 10 LABEL L_1 ;  // Defines a label control.
         VALUE "Subtotal:" ;  // Sets the label text.
         WIDTH 200 ;        // Sets the label width.
         HEIGHT 30 SIZE 11 BOLD  // Sets the label height, size and font style.

      @ 360, 220 TEXTBOX T_1 ;  // Defines a textbox control.
         VALUE 0.00 ;          // Sets the initial value.
         WIDTH 100 ;           // Sets the textbox width.
         HEIGHT 21 ;           // Sets the textbox height.
         NUMERIC INPUTMASK "9999.99"  // Specifies that the textbox accepts numeric input with a specific format.

      @ 400, 10 BUTTON B_1 ;  // Defines a button control.
         CAPTION "F6 - Add records from list" ;  // Sets the button caption.
         ACTION AddRecordsFromList() ;  // Assigns the action to be performed when the button is clicked.
         WIDTH 200 ;           // Sets the button width.
         HEIGHT 30            // Sets the button height.

   END WINDOW

   ON KEY F6 OF FORM_1 ACTION AddRecordsFromList()  // Assigns the AddRecordsFromList() function to the F6 key press within Form_1.

   ACTIVATE WINDOW Form_1  // Activates the main window.

RETURN NIL
// ----------------
PROCEDURE SetTotal()
/*
 *  Calculates and displays the total salary from the grid.
 *
 *  This procedure iterates through the salary column of the grid, sums the values,
 *  and displays the total in a textbox.
 */

   LOCAL nValue := 0, I  // Local variables to store the total value and loop counter.

   FOR I=1 TO Form_1.Grid_1.ItemCount  // Loops through each row in the grid.
      nValue += Form_1.Grid_1.Cell(I, 2)  // Adds the salary value from the second column to the total.
   NEXT

   FORM_1.T_1.Value := nValue  // Displays the total salary in the textbox.

RETURN
// ----------------
FUNCTION OnQuery( aSource )
/*
 *  Retrieves data for the virtual grid.
 *
 *  This function is called by the grid control to retrieve the data for a specific cell.
 *  It uses the row and column indices to access the data from the provided source array.
 *
 *  Parameters:
 *      aSource - The data source (array of arrays) for the grid.
 *
 *  Return:
 *      .T. (Always returns true to indicate success).
 */

   LOCAL nRow, nCol  // Local variables to store the row and column indices.

   nRow := This.QueryRowIndex  // Gets the row index of the cell being queried.
   nCol := This.QueryColIndex  // Gets the column index of the cell being queried.
   IF nRow > 0 .AND. nCol > 0  // Checks if the row and column indices are valid.
      This.QueryData := aSource[ nRow, nCol ]  // Assigns the data from the source array to the QueryData property.
   ENDIF

RETURN .T.
// -----------------------
FUNCTION DataValidation
/*
 *  Validates data entered into the grid cells.
 *
 *  This function is called when the user attempts to save a value in a grid cell.
 *  It checks if the cell is empty and updates the salary data array.
 *
 *  Parameters:
 *      None
 *
 *  Return:
 *      .T. if the data is valid, .F. otherwise.
 */

   LOCAL nRow, nCol  // Local variables to store the row and column indices.

   nRow := This.Value[ 1 ]  // Gets the row index of the cell being validated.
   nCol := This.Value[ 2 ]  // Gets the column index of the cell being validated.

   IF Empty( This.CellValue )  // Checks if the cell value is empty.
      RETURN .F.  // Returns .F. if the cell is empty (invalid).
   ENDIF
   aSalaries[ nRow, nCol ] := This.CellValue  // Updates the salary data array with the new cell value.

RETURN .T.  // Returns .T. to indicate that the data is valid.
// ------------------------
PROCEDURE AddRecordsFromList
/*
 *  Displays a modal window with a grid to select records from a list.
 *
 *  This procedure defines and activates a modal window containing a grid populated with
 *  a list of names and preferred salaries. The user can select a record from this grid
 *  to add it to the main salary grid.
 */

   PRIVATE aHeaders := { "Name", "Preferred salary" }  // Private variable to store the column headers for the second grid.
   PRIVATE aWidths := { 300, 200 }                     // Private variable to store the column widths for the second grid.
   PRIVATE aJust := { 0, 1 }                           // Private variable to store the column justification for the second grid.

   PRIVATE aRows := {}                                 // Private variable to store the data for the second grid.

   AAdd( aRows, { "Marek", 100.20 } )  // Adds initial data to the array.
   AAdd( aRows, { "Iza", 123.22 } )    // Adds initial data to the array.
   AAdd( aRows, { "Szymon", 321.23 } ) // Adds initial data to the array.
   AAdd( aRows, { "Javier", 143.24 } ) // Adds initial data to the array.
   AAdd( aRows, { "Nico", 154.25 } )   // Adds initial data to the array.
   AAdd( aRows, { "Maxim", 132.26 } )  // Adds initial data to the array.
   AAdd( aRows, { "Reno", 199.77 } )   // Adds initial data to the array.

   DEFINE WINDOW Form_2 ;   // Defines the modal window.
         AT 300, 300 ;      // Sets the window position.
         WIDTH 580 ;        // Sets the window width.
         HEIGHT 400 ;       // Sets the window height.
         TITLE 'Salariers' ;// Sets the window title.
         MODAL              // Specifies that this window is modal (blocks interaction with other windows).

      @ 10, 10 GRID Grid_2 ; // Defines a grid control within the window.
         WIDTH 520 ;         // Sets the grid width.
         HEIGHT 240 ;        // Sets the grid height.
         HEADERS aHeaders ;  // Assigns the column headers.
         WIDTHS aWidths ;    // Assigns the column widths.
         VALUE { 1, 1 } ;    // Sets the initial selected cell.
         TOOLTIP 'Press F2 or double click to select name' ;  // Sets the tooltip text.
         COLUMNCONTROLS aControls ; // Assigns the column control types.
         COLUMNVALID aValid ;       // Assigns the column validation functions.
         COLUMNWHEN aWhen ;         // Assigns the column WHEN conditions.
         VIRTUAL ;                  // Enables virtual mode (data is retrieved on demand).
         ITEMCOUNT Len( aRows ) ;   // Sets the number of rows in the grid.
         ON QUERYDATA OnQuery( aRows ) ;  // Assigns the function to retrieve data for the grid.
         ON DBLCLICK SelectRecordFromList() ;  // Assigns the function to be called on double-click.
         JUSTIFY aJust ;           // Assigns the column justification.
         CELLNAVIGATION            // Enables cell navigation using arrow keys.

      @ 270, 10 BUTTON B_2 ;  // Defines a button control.
         CAPTION "F2 -select" ;  // Sets the button caption.
         ACTION SelectRecordFromList() ;  // Assigns the action to be performed when the button is clicked.
         WIDTH 200 ;           // Sets the button width.
         HEIGHT 30            // Sets the button height.

   END WINDOW

   ON KEY ESCAPE OF FORM_2 ACTION FORM_2.Release()  // Assigns the action to be performed when the ESCAPE key is pressed.
   ON KEY F2 OF FORM_2 ACTION SelectRecordFromList()  // Assigns the action to be performed when the F2 key is pressed.

   ACTIVATE WINDOW Form_2  // Activates the modal window.

RETURN
// ---------------------
PROCEDURE SelectRecordFromList
/*
 *  Displays a modal window to change the salary of a selected record.
 *
 *  This procedure defines and activates a modal window that allows the user to
 *  modify the salary of a record selected from the list in Form_2.
 */

   LOCAL i  // Local variable to store the selected row index.

   i := Form_2.Grid_2.Value[ 1 ]  // Gets the selected row index from the grid.

   IF i == 0  // Checks if a row is selected.
      RETURN  // Returns if no row is selected.
   ENDIF

   DEFINE WINDOW Form_3 ;   // Defines the modal window.
         AT 350, 200 ;      // Sets the window position.
         WIDTH 220 ;        // Sets the window width.
         HEIGHT 160 ;       // Sets the window height.
         TITLE 'Change salary' ;  // Sets the window title.
         MODAL              // Specifies that this window is modal.

      @ 10, 10 LABEL L_1 ;  // Defines a label control.
         WIDTH 60 ;         // Sets the label width.
         HEIGHT 18 ;        // Sets the label height.
         VALUE "Name" ;     // Sets the label text.
         RIGHTALIGN         // Aligns the text to the right.

      @ 10, 80 TEXTBOX T_1 ; // Defines a textbox control.
         HEIGHT 24 ;         // Sets the textbox height.
         WIDTH 120 ;         // Sets the textbox width.
         VALUE aRows[ i, 1 ] ;  // Sets the initial value to the name from the selected row.
         READONLY ;          // Makes the textbox read-only.
         NOTABSTOP           // Prevents the textbox from receiving focus when tabbing.

      @ 40, 10 LABEL L_2 ;   // Defines a label control.
         WIDTH 60 ;          // Sets the label width.
         HEIGHT 18 ;         // Sets the label height.
         VALUE "Salary" ;    // Sets the label text.
         RIGHTALIGN          // Aligns the text to the right.

      @ 40, 80 TEXTBOX T_2 ; // Defines a textbox control.
         HEIGHT 24 ;         // Sets the textbox height.
         WIDTH 120 ;         // Sets the textbox width.
         VALUE aRows[ i, 2 ] ;  // Sets the initial value to the salary from the selected row.
         NUMERIC ;           // Specifies that the textbox accepts numeric input.
         INPUTMASK "9999.99" ;  // Sets the input mask for the numeric input.
         RIGHTALIGN          // Aligns the text to the right.

      @ 80, 10 BUTTON B_Save ; // Defines a button control.
         CAPTION "F2 -Save" ;  // Sets the button caption.
         ACTION SaveRecord() ; // Assigns the action to be performed when the button is clicked.
         WIDTH 120 ;         // Sets the button width.
         HEIGHT 30           // Sets the button height.

   END WINDOW

   ON KEY ESCAPE OF Form_3 ACTION Form_3.Release()  // Assigns the action to be performed when the ESCAPE key is pressed.
   ON KEY F2 OF Form_3 ACTION SaveRecord()  // Assigns the action to be performed when the F2 key is pressed.

   ACTIVATE WINDOW Form_3  // Activates the modal window.

RETURN
// -----------
PROCEDURE SaveRecord
/*
 *  Saves the record to the main salary grid and refreshes the display.
 *
 *  This procedure adds the name and salary from Form_3 to the aSalaries array,
 *  updates the item count of the main grid (Form_1.Grid_1), and refreshes the grid's display.
 *  It also calls SetTotal() to recalculate and display the total salary.
 */

   RELEASE KEY F2 OF Form_3  // Releases the F2 key assignment for Form_3.

   IF .NOT. IsWindowDefined( "Form_3" )  // Checks if the window is still defined.
      RETURN  // Returns if the window is not defined.
   ENDIF

   AAdd( aSalaries, { Form_3.T_1.Value, Form_3.T_2.Value } )  // Adds the name and salary from Form_3 to the aSalaries array.

   Form_3.RELEASE  // Closes the Form_3 window.

   // refresh grid in main windows
   Form_1.Grid_1.ItemCount := 0  // Resets the item count of the main grid.
   Form_1.Grid_1.ItemCount := Len( aSalaries )  // Sets the item count to the new length of the aSalaries array.
   Form_1.Grid_1.Value := { Len( aSalaries ), 2 }  // Sets the selected cell to the last row and second column.

   SetTotal()  // Recalculates and displays the total salary.

RETURN
