/*
 * MiniGUI Virtual Column Grid Demo
 *
 * Author: Grigory Filatov
*/

#include "minigui.ch"

/*
 * PROCEDURE Main
 *
 * Initializes the application, defines the main window with a virtual column grid, and sets up the UI elements.
 *
 * Purpose:
 *   This is the entry point of the application. It performs the following tasks:
 *     1. Defines the main window with a virtual column grid control.
 *     2. Defines a main menu with options to save data, exit, append a row, insert a row, and delete a row.
 *     3. Loads data from 'data.ini' if it exists, otherwise initializes the aRows array with sample data.
 *     4. Sorts the aRows array alphabetically by the second column (Last Name).
 *     5. Configures the virtual column grid with headers, widths, data source (aRows), and edit capabilities.
 *     6. Adjusts the form's height and width based on the grid's dimensions and screen size.
 *     7. Centers the main window on the screen.
 *     8. Activates the main window, making it visible to the user.
 *   The virtual column grid is used to display and edit a large dataset efficiently, as it only loads the data that is currently visible.
 *   The data.ini file provides persistence, allowing the application to remember the data between sessions.
 *
 * Notes:
 *   The virtual column grid uses the QueryTest() function to retrieve data for each cell.
 *   The SETVIRTUALITEM() function is used to update the aRows array when a cell is edited.
 *   The data.ini file stores the contents of the aRows array for persistence between application sessions.
 *   The grid uses in-place editing with different textbox types for each column, allowing users to directly edit the data within the grid.
 */
PROCEDURE Main

   LOCAL aRows[ 20 ][ 6 ] // Array to hold the data for the grid.  Each row represents a record, and each column represents a field.

   DEFINE WINDOW Form_1 ;                // Define the main window
      AT 0, 0 ;                          // Position of the window (top-left corner)
      WIDTH 640 ;                        // Width of the window
      HEIGHT 400 ;                       // Height of the window
      TITLE 'Virtual Column Grid Test' ; // Title of the window
      MAIN                               // Designates this window as the main window

   DEFINE MAIN MENU                      // Define the main menu for the window

   DEFINE POPUP 'File'                   // Define a popup menu named 'File'
      MENUITEM 'Save Data' ACTION SaveData( aRows ) // Define a menu item 'Save Data' that calls the SaveData() procedure when clicked
      SEPARATOR                          // Add a separator line to the menu
      MENUITEM 'Exit'  ACTION ThisWindow.Release // Define a menu item 'Exit' that closes the window when clicked
   END POPUP
   DEFINE POPUP 'Actions'                // Define a popup menu named 'Actions'
      MENUITEM 'Append Row' ACTION AppendRow( aRows ) // Define a menu item 'Append Row' that calls the AppendRow() procedure when clicked
      MENUITEM 'Insert Row' ACTION InsertRow( aRows ) // Define a menu item 'Insert Row' that calls the InsertRow() procedure when clicked
      MENUITEM 'Delete Row' ACTION DeleteRow( aRows ) // Define a menu item 'Delete Row' that calls the DeleteRow() procedure when clicked
   END POPUP

   END MENU

   IF File( 'data.ini' ) // Check if the 'data.ini' file exists
      BEGIN INI FILE "data.ini" // If the file exists, open it as an INI file
         GET aRows SECTION "Data" ENTRY "Array" // Read the 'Array' entry from the 'Data' section of the INI file into the aRows array
      END INI
      ASort( aRows, , , {| x, y| x[ 1 ] < y[ 1 ] } ) // Sort the aRows array alphabetically by the second column (Last Name)
   ELSE
      // If the 'data.ini' file does not exist, initialize the aRows array with sample data
      aRows[ 1 ] := { '01', 'Simpson', 'Homer', 5, 10, 5 * 10 }
      aRows[ 2 ] := { '02', 'Mulder', 'Fox', 24, 32, 24 * 32 }
      aRows[ 3 ] := { '03', 'Smart', 'Max', 43, 58, 43 * 58 }
      aRows[ 4 ] := { '04', 'Grillo', 'Pepe', 89, 23, 89 * 23 }
      aRows[ 5 ] := { '05', 'Kirk', 'James', 34, 73, 34 * 73 }
      aRows[ 6 ] := { '06', 'Barriga', 'Carlos', 39, 54, 39 * 54 }
      aRows[ 7 ] := { '07', 'Flanders', 'Ned', 43, 11, 43 * 11 }
      aRows[ 8 ] := { '08', 'Smith', 'John', 12, 34, 12 * 34 }
      aRows[ 9 ] := { '09', 'Pedemonti', 'Flavio', 10, 100, 10 * 100 }
      aRows[10 ] := { '10', 'Gomez', 'Juan', 58, 32, 58 * 32 }
      aRows[11 ] := { '11', 'Fernandez', 'Raul', 32, 43, 32 * 43 }
      aRows[12 ] := { '12', 'Borges', 'Javier', 26, 30, 26 * 30 }
      aRows[13 ] := { '13', 'Alvarez', 'Alberto', 54, 98, 54 * 98 }
      aRows[14 ] := { '14', 'Gonzalez', 'Ambo', 43, 73, 43 * 73 }
      aRows[15 ] := { '15', 'Batistuta', 'Gol', 48, 28, 48 * 28 }
      aRows[16 ] := { '16', 'Vinazzi', 'Amigo', 39, 83, 39 * 83 }
      aRows[17 ] := { '17', 'Pedemonti', 'Flavio', 53, 84, 53 * 84 }
      aRows[18 ] := { '18', 'Samarbide', 'Armando', 54, 73, 54 * 73 }
      aRows[19 ] := { '19', 'Pradon', 'Alejandra', 12, 45, 12 * 45 }
      aRows[20 ] := { '20', 'Reyes', 'Monica', 32, 36, 32 * 36 }
   ENDIF

   @ 10, 10 GRID Grid_1 ;               // Define a grid control
      AUTOSIZEWIDTH -1 ;                // Automatically size the width of the grid to fit the window
      AUTOSIZEHEIGHT -1 ;               // Automatically size the height of the grid to fit the window
      HEADERS { 'Code', 'Last Name', 'First Name', 'Quantity', 'Price', 'Cost' } ; // Set the headers for the grid columns
      WIDTHS { 60, 100, 100, 80, 80, 100 } ; // Set the widths for the grid columns
      VIRTUAL ;                         // Enable virtual mode for the grid (data is retrieved on demand)
      ITEMCOUNT Len( aRows ) ;          // Set the number of rows in the grid to the length of the aRows array
      ON QUERYDATA QueryTest( aRows ) ; // Set the event handler for retrieving data for each cell to the QueryTest() procedure
      CELLNAVIGATION ; // Enable cell navigation using arrow keys
      VALUE { 1, 1 } ; // Set the initial selected cell to the first row and first column
      EDIT INPLACE { ; // Enable in-place editing for the grid cells
         { 'TEXTBOX', 'CHARACTER', '999' }, ; // Define a textbox for the 'Code' column (character, max length 3)
         { 'TEXTBOX', 'CHARACTER', }, ; // Define a textbox for the 'Last Name' column (character, no max length)
         { 'TEXTBOX', 'CHARACTER', }, ; // Define a textbox for the 'First Name' column (character, no max length)
         { 'TEXTBOX', 'NUMERIC', '9,999' }, ; // Define a numeric textbox for the 'Quantity' column (numeric, max length 5)
         { 'TEXTBOX', 'NUMERIC', '999.99' }, ; // Define a numeric textbox for the 'Price' column (numeric, max length 6, 2 decimal places)
         { 'TEXTBOX', 'NUMERIC', '9,999,999.99' } ; // Define a numeric textbox for the 'Cost' column (numeric, max length 10, 2 decimal places)
      };
      COLUMNWHEN { ; // Define conditions for when a column is editable
         {|| Empty ( This.CellValue ) }, ; // 'Code' column is editable when the cell is empty
         {|| This.CellValue >= 'M' }, ; // 'Last Name' column is editable when the cell value is greater than or equal to 'M'
         {|| This.CellValue >= 'C' }, ; // 'First Name' column is editable when the cell value is greater than or equal to 'C'
         {|| ! Empty ( This.CellValue ) }, ; // 'Quantity' column is editable when the cell is not empty
         {|| ! Empty ( This.CellValue ) }, ; // 'Price' column is editable when the cell is not empty
         {|| Empty ( This.CellValue ) } ; // 'Cost' column is editable when the cell is empty
      } ;
      COLUMNVALID { {|| SETVIRTUALITEM( aRows ) }, ; // Set the validation function for the 'Code' column to SETVIRTUALITEM()
                    {|| SETVIRTUALITEM( aRows ) }, ; // Set the validation function for the 'Last Name' column to SETVIRTUALITEM()
                    {|| SETVIRTUALITEM( aRows ) }, ; // Set the validation function for the 'First Name' column to SETVIRTUALITEM()
                    {|| SETVIRTUALITEM( aRows ) }, ; // Set the validation function for the 'Quantity' column to SETVIRTUALITEM()
                    {|| SETVIRTUALITEM( aRows ) }, ; // Set the validation function for the 'Price' column to SETVIRTUALITEM()
                  } ;
      JUSTIFY { GRID_JTFY_LEFT, ;  // Justify the 'Code' column to the left
                GRID_JTFY_RIGHT, ; // Justify the 'Last Name' column to the right
                GRID_JTFY_RIGHT, ; // Justify the 'First Name' column to the right
                GRID_JTFY_RIGHT, ; // Justify the 'Quantity' column to the right
                GRID_JTFY_RIGHT, ; // Justify the 'Price' column to the right
                GRID_JTFY_RIGHT }; // Justify the 'Cost' column to the right
      PAINTDOUBLEBUFFER            // Enable double buffering for smoother rendering

   END WINDOW

   // Adjust the form's height to fit the grid and window decorations, but not exceed the screen height
   Form_1.Height := Min( Sys.ClientHeight, Form_1.Grid_1.Height + GetTitleHeight() + GetMenubarHeight() + 2 * GetBorderHeight() + 2 * Form_1.Grid_1.Row )
   IF Form_1.Grid_1.Height > Sys.ClientHeight - ( GetTitleHeight() + GetMenubarHeight() + 2 * GetBorderHeight() + 2 * Form_1.Grid_1.Row )
      Form_1.Grid_1.Height := Sys.ClientHeight - ( GetTitleHeight() + GetMenubarHeight() + 2 * GetBorderHeight() + 2 * Form_1.Grid_1.Row )
   ENDIF
   Form_1.Width := Form_1.Grid_1.Width + 2 * GetBorderWidth() + 2 * Form_1.Grid_1.Col // Adjust the form's width to fit the grid and window decorations

   CENTER WINDOW Form_1 // Center the main window on the screen

   ACTIVATE WINDOW Form_1 // Activate the main window, making it visible to the user

RETURN


/*
 * PROCEDURE QueryTest( aArr )
 *
 * Retrieves the data for a specific cell in the virtual column grid.
 *
 * Parameters:
 *   aArr - ARRAY - The array containing the data for the grid. This array holds all the data displayed in the grid.
 *
 * Return Value:
 *   None
 *
 * Purpose:
 *   This procedure is called by the grid control's ON QUERYDATA event. It's the core of the virtual grid implementation.
 *   When the grid needs to display a cell, it calls this procedure to retrieve the data for that specific cell.
 *   The procedure determines the row and column indices of the requested cell using This.QueryRowIndex and This.QueryColIndex.
 *   It then retrieves the corresponding value from the aArr array and assigns it to This.QueryData.
 *   The grid control then uses the value in This.QueryData to display the cell's content.
 *   This approach allows the grid to display large datasets efficiently, as it only retrieves the data that is currently visible.
 *
 * Notes:
 *   This.QueryRowIndex and This.QueryColIndex are automatically set by the grid control before calling this procedure.
 *   The aArr array must be properly populated with the data to be displayed in the grid.
 *   If the row or column index is out of bounds, the procedure may result in an error.
 */
PROCEDURE QueryTest( aArr )
   IF This.QueryRowIndex > 0
      This.QueryData := aArr[ This.QueryRowIndex ][ This.QueryColIndex ] // Retrieve the data from the aArr array based on the current row and column indices
   ENDIF

RETURN


/*
 * FUNCTION SETVIRTUALITEM( aArr )
 *
 * Updates the data in the aArr array when a cell in the virtual column grid is edited.
 *
 * Parameters:
 *   aArr - ARRAY - The array containing the data for the grid. This array holds all the data displayed in the grid.
 *
 * Return Value:
 *   lRet - LOGICAL - .T. if the update was successful, .F. otherwise.  Returns .F. if the value is empty for the first three columns.
 *
 * Purpose:
 *   This function is called by the grid control's COLUMNVALID event. It's triggered when the user finishes editing a cell in the grid.
 *   The function retrieves the new value entered by the user from This.CellValue.
 *   It also retrieves the row and column indices of the edited cell from This.CellRowIndex and This.CellColIndex.
 *   It then updates the corresponding element in the aArr array with the new value.
 *   If the edited column is 'Quantity' or 'Price', it recalculates the 'Cost' column.
 *   The function returns .T. if the update was successful, and .F. if the value is empty for the first three columns ('Code', 'Last Name', 'First Name').
 *   This validation ensures that these required fields are not left empty.
 *
 * Notes:
 *   This.CellValue, This.CellRowIndex, and This.CellColIndex are automatically set by the grid control before calling this function.
 *   The aArr array is modified directly by this function.
 *   The function assumes that the 'Cost' column is always the 6th column in the array.
 */
FUNCTION SETVIRTUALITEM( aArr )

   LOCAL nVal := This.CellValue    // Get the new value entered by the user
   LOCAL nCol := This.CellColIndex // Get the column index of the edited cell
   LOCAL nRow := This.CellRowIndex // Get the row index of the edited cell
   LOCAL lRet := .T.               // Initialize the return value to .T. (success)

   aArr[ nRow ][ nCol ] := nVal    // Update the corresponding element in the aArr array with the new value

   // If the edited column is 'Quantity' or 'Price', recalculate the 'Cost' column
   IF nCol > 3
      aArr[ nRow ][ 6 ] := aArr[ nRow ][ iif( nCol == 5, 4, 5 ) ] * nVal // Calculate the 'Cost' by multiplying 'Quantity' and 'Price'
   ELSE
      lRet := !Empty( nVal )       // If the edited column is not 'Quantity' or 'Price', check if the value is empty
   ENDIF

RETURN lRet // Return .T. if the update was successful, .F. otherwise


/*
 * PROCEDURE SaveData( aArr )
 *
 * Saves the data from the aArr array to the 'data.ini' file.
 *
 * Parameters:
 *   aArr - ARRAY - The array containing the data to be saved.
 *
 * Return Value:
 *   None
 *
 * Purpose:
 *   This procedure is called when the user selects the 'Save Data' menu item.
 *   It persists the data currently displayed in the grid to a file named 'data.ini'.
 *   This allows the application to remember the data between sessions.
 *   The data is saved in the INI file format, which is a simple text-based format.
 *   The entire aArr array is saved as a single entry named "Array" within a section named "Data" in the INI file.
 *
 * Notes:
 *   The 'data.ini' file will be created if it does not already exist.
 *   If the file already exists, it will be overwritten with the new data.
 *   The INI file format is relatively simple and may not be suitable for storing very large datasets.
 */
PROCEDURE SaveData( aArr )

   BEGIN INI FILE "data.ini"                   // Open the 'data.ini' file as an INI file
      SET SECTION "Data" ENTRY "Array" TO aArr // Save the aArr array to the 'Array' entry in the 'Data' section
   END INI

RETURN

/*
 * PROCEDURE AppendRow( aArr )
 *
 * Appends a new row to the aArr array and updates the grid.
 *
 * Parameters:
 *   aArr - ARRAY - The array to which the new row will be appended. This array holds all the data displayed in the grid.
 *
 * Return Value:
 *   None
 *
 * Purpose:
 *   This procedure is called when the user selects the 'Append Row' menu item.
 *   It adds a new, empty row to the end of the aArr array.
 *   It then updates the grid control to reflect the new number of rows.
 *   Finally, it sets the grid's value to the last row, so the user can immediately start editing the new row.
 *   This provides a convenient way for the user to add new records to the dataset.
 *
 * Notes:
 *   The new row is initialized with default values.
 *   The grid's ItemCount property must be updated after appending the row to ensure that the grid displays the correct number of rows.
 */
PROCEDURE AppendRow( aArr )

   AAdd( aArr, { '', 'Reyes', 'Monica', 1, 1, 1 } ) // Append a new row with default values to the aArr array
   Form_1.Grid_1.ItemCount := Len( aArr )           // Update the grid's item count to reflect the new number of rows
   Form_1.Grid_1.Value := Len( aArr )               // Set the grid's value to the last row

RETURN

/*
 * PROCEDURE InsertRow( aArr )
 *
 * Inserts a new row into the aArr array at the currently selected row and updates the grid.
 *
 * Parameters:
 *   aArr - ARRAY - The array into which the new row will be inserted. This array holds all the data displayed in the grid.
 *
 * Return Value:
 *   None
 *
 * Purpose:
 *   This procedure is called when the user selects the 'Insert Row' menu item.
 *   It inserts a new, empty row into the aArr array at the position of the currently selected row in the grid.
 *   It then updates the grid control to reflect the new number of rows.
 *   This allows the user to insert a new record at a specific location within the dataset.
 *
 * Notes:
 *   The new row is initialized with default values.
 *   The grid's ItemCount property must be updated after inserting the row to ensure that the grid displays the correct number of rows.
 */
PROCEDURE InsertRow( aArr )

   AIns( aArr, Form_1.Grid_1.Value [1], { '', 'Reyes', 'Monica', 1, 1, 1 }, .T. ) // Insert a new row with default values into the aArr array at the selected row
   Form_1.Grid_1.ItemCount := Len( aArr ) // Update the grid's item count to reflect the new number of rows

RETURN

/*
 * PROCEDURE DeleteRow( aArr )
 *
 * Deletes the currently selected row from the aArr array and updates the grid.
 *
 * Parameters:
 *   aArr - ARRAY - The array from which the row will be deleted. This array holds all the data displayed in the grid.
 *
 * Return Value:
 *   None
 *
 * Purpose:
 *   This procedure is called when the user selects the 'Delete Row' menu item.
 *   It removes the row that is currently selected in the grid from the aArr array.
 *   It then updates the grid control to reflect the new number of rows.
 *   If the deleted row was the last row, it adjusts the grid's value to point to the new last row.
 *   This provides a way for the user to remove unwanted records from the dataset.
 *
 * Notes:
 *   The grid's ItemCount property must be updated after deleting the row to ensure that the grid displays the correct number of rows.
 *   If the deleted row was the last row, the grid's Value property must be adjusted to prevent an out-of-bounds error.
 */
PROCEDURE DeleteRow( aArr )

   LOCAL nRow := Form_1.Grid_1.Value      // Get the index of the currently selected row

   ADel( aArr, nRow[ 1 ] )                // Delete the selected row from the aArr array
   ASize( aArr, Len( aArr ) -1 )          // Resize the aArr array to remove the empty element after deletion
   Form_1.Grid_1.ItemCount := Len( aArr ) // Update the grid's item count to reflect the new number of rows
   IF nRow[ 1 ] > Len( aArr )             // If the deleted row was the last row
      Form_1.Grid_1.Value := Len( aArr )  // Set the grid's value to the new last row
   ENDIF

RETURN
