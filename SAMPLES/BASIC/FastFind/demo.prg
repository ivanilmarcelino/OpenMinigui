/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-06 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Based on sample provided by Honorio and modified by Jacek Kubica
 * Adapted by MigSoft for Harbour MiniGUI IDE
*/

#include <minigui.ch>

/*
 * PROCEDURE Main()
 *
 * Initializes the application, opens the "Cuentas" database, creates an index, loads the main window,
 * centers it on the screen, sets focus to the first text box, and activates the window.
 *
 * Purpose:
 *   This is the entry point of the application. It performs the following tasks:
 *     1. Disables multiple warnings.
 *     2. Opens the "Cuentas" database table.
 *     3. Creates an index on the "nombre" field of the "Cuentas" table. This index is used for faster searching.
 *     4. Loads the main window (Win_1) from a resource file (likely a .FMG file).
 *     5. Centers the main window on the screen.
 *     6. Sets the focus to the "Text_1" control within the main window. This is likely a text box where the user enters search criteria.
 *     7. Activates the main window, making it visible and responsive to user input.
 */
PROCEDURE Main()

   SET MULTIPLE OFF WARNING

   USE Cuentas
   INDEX ON FIELD->nombre TO cuentas

   LOAD WINDOW Win_1
   CENTER WINDOW Win_1
   Win_1.Text_1.Setfocus
   ACTIVATE WINDOW Win_1

RETURN

/*
 * FUNCTION Captura()
 *
 * Filters and displays records from the "Cuentas" database in a grid based on user input.
 *
 * Purpose:
 *   This function implements the search functionality of the application. It performs the following steps:
 *     1. Retrieves the user's input from the "Text_1" control in the "Win_1" window.
 *     2. Converts the input to uppercase and trims any leading or trailing spaces.
 *     3. Determines the length of the input string.
 *     4. Initializes variables to track the number of processed records and the maximum number of records to display in the grid.
 *     5. Sets the "cCampo" memory variable to "NOMBRE". This variable determines which field in the "Cuentas" table is used for filtering.
 *     6. Selects the "Cuentas" database area.
 *     7. Seeks to the first record in the database that matches the user's input (using the index created in the Main() procedure).
 *     8. Disables updates to the grid control to improve performance while adding items.
 *     9. Deletes all existing items from the grid.
 *     10. Iterates through the database records, adding records to the grid that match the user's input.
 *     11. The loop continues until the end of the file is reached, the maximum number of records is displayed, or a record is found that does not match the user's input.
 *     12. Enables updates to the grid control.
 */
FUNCTION Captura()

   LOCAL cCapt := Upper( AllTrim( win_1.Text_1.value ) )
   LOCAL nTaman := Len( cCapt )
   LOCAL nRegProc := 0
   LOCAL nMaxRegGrid := 70
   MEMVAR cCampo
   PRIVATE cCampo := "NOMBRE"

   dbSelectArea( "Cuentas" )
   dbSeek( cCapt )

   win_1.Grid_1.DisableUpdate
   DELETE ITEM ALL FROM Grid_1 OF Win_1

   DO While ! Eof()
      IF SubStr( FIELD->&cCampo, 1, nTaman ) == cCapt
         nRegProc += 1
         IF nRegProc > nMaxRegGrid
            EXIT
         ENDIF
         ADD ITEM { TRANSF( Cuentas->Imputacion, "9999999" ), ;
            Cuentas->Nombre } TO Grid_1 OF Win_1
      ELSEIF SubStr( FIELD->&cCampo, 1, nTaman ) > cCapt
         EXIT
      ENDIF
      dbSkip()
   ENDDO
   win_1.Grid_1.EnableUpdate

RETURN NIL

/*
 * PROCEDURE VerItem()
 *
 * Displays the values of the selected row in the grid in a message box.
 *
 * Purpose:
 *   This function provides a way for the user to view the details of a selected record in the grid.
 *   It retrieves the values from the first and second columns of the selected row and displays them in a message box.
 */
PROCEDURE VerItem()

   MsgInfo( 'Col 1: ' + GetColValue( "Grid_1", "Win_1", 1 ) + '  ' ;
      + 'Col 2: ' + GetColValue( "Grid_1", "Win_1", 2 ) )

RETURN

/*
 * FUNCTION GetColValue(xObj, xForm, nCol)
 *
 * Retrieves the value of a specific column in a grid row.
 *
 * Parameters:
 *   xObj  (STRING): The name of the grid object (e.g., "Grid_1").
 *   xForm (STRING): The name of the form containing the grid (e.g., "Win_1").
 *   nCol  (NUMERIC): The column number to retrieve (1-based index).
 *
 * Returns:
 *   The value of the specified column in the selected grid row. The data type of the return value depends on the data type of the column.
 *
 * Purpose:
 *   This function provides a generic way to access the values in a grid control.
 *   It retrieves the currently selected row in the grid and then returns the value of the specified column in that row.
 */
FUNCTION GetColValue( xObj, xForm, nCol )

   LOCAL nPos := GetProperty( xForm, xObj, 'Value' )
   LOCAL aRet := GetProperty( xForm, xObj, 'Item', nPos )

RETURN aRet[ nCol ]

/*
 * FUNCTION SetColValue(xObj, xForm, nCol, xValue)
 *
 * Sets the value of a specific column in a grid row.
 *
 * Parameters:
 *   xObj   (STRING): The name of the grid object (e.g., "Grid_1").
 *   xForm  (STRING): The name of the form containing the grid (e.g., "Win_1").
 *   nCol   (NUMERIC): The column number to set (1-based index).
 *   xValue (ANY): The new value to set for the specified column. The data type should match the column's expected data type.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function allows modifying the values displayed in a grid control.
 *   It retrieves the currently selected row in the grid, updates the value of the specified column in that row, and then updates the grid control with the modified row data.
 */
FUNCTION SetColValue( xObj, xForm, nCol, xValue )

   LOCAL nPos := GetProperty( xForm, xObj, 'Value' )
   LOCAL aRet := GetProperty( xForm, xObj, 'Item', nPos )
   aRet[ nCol ] := xValue
   SetProperty( xForm, xObj, 'Item', nPos, aRet )

RETURN NIL
