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
 *  Procedure Main()
 *
 *  Description:
 *      This is the main procedure of the application. It initializes the application,
 *      opens the "Cuentas" database, creates an index, loads and activates the main window.
 */
PROCEDURE Main()

   SET MULTIPLE OFF WARNING

   USE Cuentas // Opens the "Cuentas" database table.  Assumes the table exists in the current directory.
   INDEX ON FIELD->nombre TO cuentas // Creates an index file named "cuentas.ntx" based on the "nombre" field.  This speeds up searching.

   LOAD WINDOW Win_1 // Loads the window definition from the resource file (.hrb).  "Win_1" is the name of the window in the resource.
   CENTER WINDOW Win_1 // Centers the window on the screen.
   Win_1.Text_1.Setfocus // Sets the focus to the "Text_1" control within the "Win_1" window.  This allows the user to start typing immediately.
   ACTIVATE WINDOW Win_1 // Makes the window visible and active.

RETURN

/*
 *  Function Captura()
 *
 *  Description:
 *      This function captures user input from the "Text_1" control in the "Win_1" window,
 *      searches the "Cuentas" database for matching records, and populates the "Grid_1" control
 *      with the results.  It limits the number of displayed records to avoid performance issues.
 */
FUNCTION Captura()

   LOCAL cCapt := Upper( AllTrim( win_1.Text_1.value ) ) // Gets the text from the "Text_1" control, converts it to uppercase, and removes leading/trailing spaces.
   LOCAL nTaman := Len( cCapt ) // Gets the length of the captured text.
   LOCAL nRegProc := 0 // Initializes a counter for the number of processed records.
   LOCAL nMaxRegGrid := 70 // Sets the maximum number of records to display in the grid.  This prevents the grid from becoming too large and slow.
   MEMVAR cCampo // Declares a memory variable named "cCampo".  Memory variables are global in scope.
   PRIVATE cCampo := "NOMBRE" // Assigns the value "NOMBRE" to the private memory variable "cCampo". This determines which field is searched.

   dbSelectArea( "Cuentas" ) // Selects the "Cuentas" database work area.
   dbSeek( cCapt ) // Seeks the database index for a record matching the captured text.

   win_1.Grid_1.DisableUpdate // Disables updates to the grid control to improve performance while adding items.
   DELETE ITEM ALL FROM Grid_1 OF Win_1 // Clears all existing items from the grid control.

   DO While ! Eof() // Loops through the database records until the end of the file is reached.
      IF SubStr( FIELD->&cCampo, 1, nTaman ) == cCapt // Checks if the substring of the "cCampo" field (which is "NOMBRE") matches the captured text.  The FIELD->&cCampo syntax uses macro substitution to access the field dynamically.
         nRegProc += 1 // Increments the record counter.
         IF nRegProc > nMaxRegGrid // Checks if the maximum number of records has been reached.
            EXIT // Exits the loop if the maximum number of records has been reached.
         ENDIF
         ADD ITEM { TRANSFORM( Cuentas->Imputacion, "9999999" ), ; // Adds a new row to the grid with the "Imputacion" and "Nombre" fields from the current database record. TRANSF formats the numeric value.
            Cuentas->Nombre } TO Grid_1 OF Win_1
      ELSEIF SubStr( FIELD->&cCampo, 1, nTaman ) > cCapt // If the current record's field is greater than the search term, it means no further matches are possible (due to the index).
         EXIT  // Exits the loop because no more matching records will be found.
      ENDIF
      dbSkip() // Moves to the next record in the database.
   ENDDO
   win_1.Grid_1.EnableUpdate // Re-enables updates to the grid control.

RETURN NIL

/*
 *  Procedure VerItem()
 *
 *  Description:
 *      This procedure displays a message box showing the values of the first two columns
 *      of the currently selected row in the "Grid_1" control of the "Win_1" window.
 */
PROCEDURE VerItem()
   MsgInfo( 'Col 1: ' + GetColValue( "Grid_1", "Win_1", 1 ) + '  ' ; // Displays a message box with the values of the first two columns of the selected grid row.
   + 'Col 2: ' + GetColValue( "Grid_1", "Win_1", 2 ) )

RETURN

/*
 *  Function GetColValue( xObj, xForm, nCol )
 *
 *  Description:
 *      This function retrieves the value of a specific column in the currently selected row
 *      of a grid control.
 *
 *  Parameters:
 *      xObj  (STRING): The name of the grid object (e.g., "Grid_1").
 *      xForm (STRING): The name of the form containing the grid (e.g., "Win_1").
 *      nCol  (NUMERIC): The column number to retrieve (1-based).
 *
 *  Return Value:
 *      The value of the specified column in the selected row of the grid.
 */
FUNCTION GetColValue( xObj, xForm, nCol )

   LOCAL nPos := GetProperty( xForm, xObj, 'Value' ) // Gets the index of the currently selected row in the grid.
   LOCAL aRet := GetProperty( xForm, xObj, 'Item', nPos ) // Gets the array representing the selected row's data.

RETURN aRet[ nCol ] // Returns the value of the specified column from the array.

/*
 *  Function SetColValue( xObj, xForm, nCol, xValue )
 *
 *  Description:
 *      This function sets the value of a specific column in the currently selected row
 *      of a grid control.
 *
 *  Parameters:
 *      xObj   (STRING): The name of the grid object (e.g., "Grid_1").
 *      xForm  (STRING): The name of the form containing the grid (e.g., "Win_1").
 *      nCol   (NUMERIC): The column number to set (1-based).
 *      xValue (ANY): The new value to set for the column.
 *
 *  Return Value:
 *      NIL
 */
FUNCTION SetColValue( xObj, xForm, nCol, xValue )

   LOCAL nPos := GetProperty( xForm, xObj, 'Value' ) // Gets the index of the currently selected row in the grid.
   LOCAL aRet := GetProperty( xForm, xObj, 'Item', nPos ) // Gets the array representing the selected row's data.
   aRet[ nCol ] := xValue // Sets the value of the specified column in the array.
   SetProperty( xForm, xObj, 'Item', nPos, aRet ) // Updates the grid with the modified row data.

RETURN NIL
