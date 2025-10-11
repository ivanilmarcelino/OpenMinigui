/*
 * HMG - Harbour Win32 GUI library Demo
 * Virtual Grid with Load/Save and Row Movement
 *
 * Purpose: Demonstrates a virtual grid in HMG Extended that displays a list of contacts,
 * allowing users to load/save data to a file and move rows up or down. The grid uses a
 * virtual data source for efficient rendering of large datasets.
 *
 * Author: Edward
 * Date: 18/Jun/2021
 * License: Public domain (as typical for HMG demos)
 */

#include "hmg.ch"

// Constant for the filename used to store serialized data
#define ARRAY_FILENAME   'Items.Array'

/*
 * FUNCTION Main()
 *
 * Initializes the application, sets up the main window, and configures a virtual grid
 * with menu options and buttons for managing contact data.
 *
 * Purpose:
 *   - Checks for and creates a default data file if it doesn't exist.
 *   - Defines a main window with a virtual grid, menu options (Load, Save, Clear),
 *     and buttons (Move Up, Move Down) for interacting with the data.
 *   - Configures the grid to display contact data (Last Name, First Name, Phone) from
 *     a dynamic array (aItems) using a virtual data source.
 *
 * Parameters: None
 * Returns: NIL
 *
 * Notes:
 *   - Uses a virtual grid to optimize performance by loading only visible rows.
 *   - Data is serialized to/deserialized from a file using hb_Serialize/hb_Deserialize.
 *   - Menu actions update the grid's item count and refresh the display.
 */
FUNCTION MAIN

   LOCAL aItems := {}

   // Create default data file if it doesn't exist
   IF ! File( ARRAY_FILENAME )
      StrFile( hb_Serialize( { ;
         { 'Simpson', 'Homer', '555-5555' }, ;
         { 'Mulder', 'Fox', '324-6432' }, ;
         { 'Smart', 'Max', '432-5892' }, ;
         { 'Grillo', 'Pepe', '894-2332' }, ;
         { 'Kirk', 'James', '346-9873' }, ;
         { 'Barriga', 'Carlos', '394-9654' }, ;
         { 'Flanders', 'Ned', '435-3211' }, ;
         { 'Smith', 'John', '123-1234' }, ;
         { 'Pedemonti', 'Flavio', '000-0000' }, ;
         { 'Gomez', 'Juan', '583-4832' }, ;
         { 'Fernandez', 'Raul', '321-4332' }, ;
         { 'Borges', 'Javier', '326-9430' }, ;
         { 'Alvarez', 'Alberto', '543-7898' }, ;
         { 'Gonzalez', 'Ambo', '437-8473' }, ;
         { 'Batistuta', 'Gol', '485-2843' }, ;
         { 'Vinazzi', 'Amigo', '394-5983' }, ;
         { 'Pedemonti', 'Flavio', '534-7984' }, ;
         { 'Samarbide', 'Armando', '854-7873' }, ;
         { 'Pradon', 'Alejandra', '???-????' }, ;
         { 'Reyes', 'Monica', '432-5836' } } ), ARRAY_FILENAME )
   ENDIF

   // Define main window
   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 550 ;
         HEIGHT 410 ;
         TITLE 'Virtual Grid with Load/Save' ;
         MAIN

      // Define main menu with file operations
      DEFINE MAIN MENU
         DEFINE POPUP 'File'
            MENUITEM 'Load Items' ACTION ( aItems := hb_Deserialize( FileStr( ARRAY_FILENAME ) ), Form_1.Grid_1.ItemCount := Len( aItems ), Form_1.Grid_1.Refresh, MsgInfo( "Items Loaded" ) )
            MENUITEM 'Save Items' ACTION ( StrFile( hb_Serialize( aItems ), ARRAY_FILENAME ), MsgInfo( "Items Saved" ) )
            MENUITEM 'Clear Items' ACTION ( aItems := {}, Form_1.Grid_1.ItemCount := Len( aItems ), Form_1.Grid_1.Refresh )
         END POPUP
      END MENU

      // Define virtual grid for contact data
      @ 10, 10 GRID Grid_1 ;
         WIDTH 400 ;
         HEIGHT 330 ;
         HEADERS { 'Last Name', 'First Name', 'Phone' } ;
         WIDTHS { 140, 140, 90 } ;
         VIRTUAL ;
         ITEMCOUNT Len( aItems ) ;
         ON QUERYDATA QueryTest( aItems ) ;
         CELLNAVIGATION ;
         VALUE { 1, 1 }

      // Define buttons for moving rows
      @ 10, 440 BUTTON bUp CAPTION "Move Up" ACTION aItems := moveUp( aItems ) WIDTH 80
      @ 40, 440 BUTTON bDown CAPTION "Move Down" ACTION aItems := moveDown( aItems ) WIDTH 80
   END WINDOW

   // Center and activate the window
   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * PROCEDURE QueryTest( aItems )
 *
 * Retrieves data for a specific cell in the virtual grid during rendering.
 *
 * Purpose:
 *   - Called by the grid to populate a cell with data from the aItems array.
 *   - Uses the current row and column indices to access the appropriate data element.
 *
 * Parameters:
 *   aItems (ARRAY): Array containing contact data { Last Name, First Name, Phone }.
 *
 * Returns: None (sets data via This.QueryData)
 *
 * Notes:
 *   - Essential for virtual grids, as it provides data on-demand for visible cells.
 *   - Assumes aItems is properly structured and indices are valid.
 */
PROCEDURE QueryTest( aItems )

   LOCAL i := This.QueryRowIndex
   LOCAL j := This.QueryColIndex

   This.QueryData := aItems[ i ][ j ]

RETURN

/*
 * FUNCTION moveUp( aItems )
 *
 * Moves the selected grid row up one position in the aItems array.
 *
 * Purpose:
 *   - Retrieves the currently selected row index from the grid.
 *   - Swaps the selected row with the one above it, if not already at the top.
 *   - Updates the grid display and selection to reflect the new row position.
 *
 * Parameters:
 *   aItems (ARRAY): Array containing contact data.
 *
 * Returns:
 *   aItems (ARRAY): Modified array with the row moved up.
 *
 * Notes:
 *   - Prevents moving the first row up to avoid invalid indexing.
 *   - Preserves the selected column during the move.
 *   - Refreshes the grid to reflect the updated array.
 */
FUNCTION moveUp( aItems )

   LOCAL nPos := Form_1.Grid_1.VALUE[ 1 ]
   LOCAL nCol := Form_1.Grid_1.VALUE[ 2 ]
   LOCAL aRow
   IF Len( aItems ) > 0 .AND. nPos > 1
      aRow := aItems[ nPos ]
      hb_ADel( aItems, nPos, .T. )
      hb_AIns( aItems, nPos - 1, aRow, .T. )
      Form_1.Grid_1.Refresh
      Form_1.Grid_1.VALUE := { nPos - 1, nCol }
   ENDIF

RETURN aItems

/*
 * FUNCTION moveDown( aItems )
 *
 * Moves the selected grid row down one position in the aItems array.
 *
 * Purpose:
 *   - Retrieves the currently selected row index from the grid.
 *   - Swaps the selected row with the one below it, if not already at the bottom.
 *   - Updates the grid display and selection to reflect the new row position.
 *
 * Parameters:
 *   aItems (ARRAY): Array containing contact data.
 *
 * Returns:
 *   aItems (ARRAY): Modified array with the row moved down.
 *
 * Notes:
 *   - Prevents moving the last row down to avoid invalid indexing.
 *   - Preserves the selected column during the move.
 *   - Refreshes the grid to reflect the updated array.
 */
FUNCTION moveDown( aItems )

   LOCAL nPos := Form_1.Grid_1.VALUE[ 1 ]
   LOCAL nCol := Form_1.Grid_1.VALUE[ 2 ]
   LOCAL aRow
   IF Len( aItems ) > 0 .AND. nPos < Len( aItems )
      aRow := aItems[ nPos ]
      hb_ADel( aItems, nPos, .T. )
      hb_AIns( aItems, nPos + 1, aRow, .T. )
      Form_1.Grid_1.Refresh
      Form_1.Grid_1.VALUE := { nPos + 1, nCol }
   ENDIF

RETURN aItems