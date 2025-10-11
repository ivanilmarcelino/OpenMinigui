/*
 * HMG - Harbour Win32 GUI library Demo
 * Virtual Grid with Grouping Functionality
 *
 * Purpose: Demonstrates a virtual grid in HMG Extended with group headers and items,
 * allowing users to expand/collapse groups, move items between groups via double-click,
 * and display a list of confirmed attendees. The grid uses a virtual data source for
 * efficient rendering of large datasets.
 *
 * Author: Grigory Filatov
 * Date: 09/30/2025
 * License: Public domain
 */

#include "hmg.ch"

// Static variables to store group identifiers and data arrays
STATIC GROUP1_ID  // Label for the "Confirmed" group
STATIC GROUP2_ID  // Label for the "Unconfirmed" group
STATIC aData      // Master array: { type, name, surname/group_name, group_id, expanded (for groups) }
STATIC aVisible   // Filtered array of visible rows for the grid based on group expansion

/*
 * FUNCTION Main()
 *
 * Initializes the application, sets up the main window, and configures a virtual grid
 * to display grouped data with interactive features.
 *
 * Purpose:
 *   - Defines group identifiers and sample data for two groups: Confirmed and Unconfirmed attendees.
 *   - Populates the master data array (aData) with group headers and items.
 *   - Creates the visible rows array (aVisible) based on group expansion state.
 *   - Defines a main window with a virtual grid and a button to list confirmed attendees.
 *   - Configures grid properties for dynamic data display, user interaction, and visual styling.
 *
 * Parameters: None
 * Returns: NIL
 *
 * Notes:
 *   - Uses a virtual grid to optimize performance for large datasets by loading only visible rows.
 *   - The grid supports double-click to toggle group expansion or move items between groups.
 *   - The second column displays a combo box for group status, but it is read-only in this version.
 */
FUNCTION Main

   LOCAL aRows

   // Initialize group identifiers
   GROUP1_ID := "List of persons CONFIRMED their attendance of the event"
   GROUP2_ID := "List of persons NOT CONFIRMED their attendance of the event"

   // Initialize sample data array with 25 person records
   aRows := Array(25)
   aRows[1] := { 'Simpson', 'Homer', 1 }
   aRows[2] := { 'Mulder', 'Fox', 1 }
   aRows[3] := { 'Smart', 'Max', 1 }
   aRows[4] := { 'Grillo', 'Pepe', 1 }
   aRows[5] := { 'Kirk', 'James', 1 }
   aRows[6] := { 'Barriga', 'Carlos', 1 }
   aRows[7] := { 'Flanders', 'Ned', 1 }
   aRows[8] := { 'Smith', 'John', 1 }
   aRows[9] := { 'Pedemonti', 'Flavio', 1 }
   aRows[10] := { 'Gomez', 'Juan', 1 }
   aRows[11] := { 'Fernandez', 'Raul', 1 }
   aRows[12] := { 'Borges', 'Javier', 1 }
   aRows[13] := { 'Alvarez', 'Alberto', 1 }
   aRows[14] := { 'Gonzalez', 'Ambo', 1 }
   aRows[15] := { 'Gracie', 'Helio', 1 }
   aRows[16] := { 'Vinazzi', 'Amigo', 1 }
   aRows[17] := { 'Gracie', 'Royce', 1 }
   aRows[18] := { 'Samarbide', 'Armando', 1 }
   aRows[19] := { 'Pradon', 'Alejandra', 1 }
   aRows[20] := { 'Reyes', 'Monica', 1 }
   aRows[21] := { 'Silva', 'Anderson', 1 }
   aRows[22] := { 'Machida', 'Lyoto', 1 }
   aRows[23] := { 'Nogueira', 'Rodrigo', 1 }
   aRows[24] := { 'Belford', 'Victor', 1 }
   aRows[25] := { 'Werdum', 'Fabricio', 1 }

   // Add "ITEM" type and set group IDs (move items 13-25 to group 2)
   AEval( aRows, {|a| AIns( a, 1, "ITEM", .T. ) } )
   AEval( aRows, {|a| a[4] := 2 }, 13 )

   // Insert group headers for Confirmed (group 1) and Unconfirmed (group 2)
   AIns( aRows, 1, { "GROUP", GROUP1_ID, 1, .T. }, .T. )
   AIns( aRows, 14, { "GROUP", GROUP2_ID, 2, .T. }, .T. )

   // Clone rows to master data array and build visible rows
   aData := AClone( aRows )
   aVisible := BuildVisibleRows( aData )

   // Set default font for the application
   SET FONT TO "Tahoma", 12
   DEFINE FONT DlgFont FONTNAME "Tahoma" SIZE 12

   // Define main window
   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 800 ;
      HEIGHT 600 ;
      TITLE "Demo: VIRTUAL GRID Group" ;
      MAIN

      // Define virtual grid
      @ 50, 10 GRID Grid_1 ;
         WIDTH 750 ;
         HEIGHT 430 ;
         HEADERS { "Name of person", "Assistance" } ;
         WIDTHS { 580, 140 } ;
         VIRTUAL ;
         ITEMCOUNT Len( aVisible ) ;
         ON QUERYDATA Grid1_OnQueryData() ;
         ON DBLCLICK Grid1_OnDblClick() ;
         COLUMNCONTROLS { NIL, { 'COMBOBOX', { 'Confirmed', 'Unconfirmed' } } } ;
         VALUE { 1, 1 } ;
         CELLNAVIGATION ;
         DYNAMICFORECOLOR { {|x, nRow| Grid1_ForeColor( x, nRow ) }, NIL } ;
         DYNAMICBACKCOLOR { {|x, nRow| Grid1_BackColor( x, nRow ) }, NIL }

      // Define button to list confirmed attendees
      @ 500, 560 BUTTON Button_8 CAPTION "List Confirmed" ACTION GetListConfirmed() ;
         WIDTH 150 ;
         HEIGHT 32

   END WINDOW

   // Center and activate the window
   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * PROCEDURE GetListConfirmed()
 *
 * Displays a list of all persons in the Confirmed group in a message box.
 *
 * Purpose:
 *   - Iterates through the aData array to find the Confirmed group header (GROUP1_ID).
 *   - Collects names of all items in the Confirmed group (group_id = 1).
 *   - Displays the list in a message box using AlertInfo.
 *
 * Parameters: None
 * Returns: None
 *
 * Notes:
 *   - Assumes aData is properly structured with group headers followed by items.
 *   - Uses CRLF to format the list for readability in the message box.
 */
PROCEDURE GetListConfirmed
   LOCAL cList := ""
   LOCAL i := AScan( aData, {|r| r[1] == "GROUP" .AND. r[2] == GROUP1_ID } )

   IF i > 0
      i++
      DO WHILE i <= Len( aData ) .AND. aData[i][1] == "ITEM" .AND. aData[i][4] == 1
         cList += aData[i][2] + ", " + aData[i][3] + CRLF
         i++
      ENDDO
   ENDIF

   AlertInfo( cList, "Confirmed List" )

RETURN

/*
 * FUNCTION BuildVisibleRows( aData )
 *
 * Creates an array of visible rows for the grid based on group expansion state.
 *
 * Purpose:
 *   - Iterates through aData to build aVisible, including group headers and items.
 *   - Includes group headers regardless of expansion state.
 *   - Includes items only if their group is expanded (aData[i][4] = .T.).
 *   - Maintains group and item order as in aData.
 *
 * Parameters:
 *   aData (ARRAY): Master data array with group headers and items.
 *
 * Returns:
 *   aVisible (ARRAY): Array of visible rows for the grid.
 *
 * Notes:
 *   - Group rows in aVisible: { "GROUP", group_name, group_id, expanded }
 *   - Item rows in aVisible: { "ITEM", full_name, group_id }
 *   - Uses group_id (aData[i][3] for groups, aData[j][4] for items) to match items to groups.
 */
FUNCTION BuildVisibleRows( aData )
   LOCAL aVisible := {}
   LOCAL i, j

   FOR i := 1 TO Len( aData )
      IF aData[i][1] == "GROUP"
         AAdd( aVisible, { "GROUP", aData[i][2], aData[i][3], aData[i][4] } )
         IF aData[i][4] // Expanded
            j := i + 1
            DO WHILE j <= Len( aData ) .AND. aData[j][1] == "ITEM" .AND. aData[j][4] == aData[i][3]
               AAdd( aVisible, { "ITEM", aData[j][2] + " " + aData[j][3], aData[j][4] } )
               j++
            ENDDO
         ENDIF
      ENDIF
   NEXT

RETURN aVisible

/*
 * FUNCTION Grid1_OnQueryData()
 *
 * Provides data for the virtual grid cells during rendering.
 *
 * Purpose:
 *   - Retrieves the row and column indices being queried by the grid.
 *   - Returns appropriate data from aVisible for display, formatting group headers
 *     with expansion indicators and item counts, and items with indented names and status.
 *
 * Parameters: None (uses This.QueryRowIndex and This.QueryColIndex)
 * Returns: "" (empty string, as data is set via This.QueryData)
 *
 * Notes:
 *   - Group headers show "[ - ]" or "[ + ]" based on expansion state (aRow[4]).
 *   - Item rows are indented with 4 spaces in the first column.
 *   - Second column shows "Confirmed" or "Unconfirmed" for items, empty for groups.
 */
FUNCTION Grid1_OnQueryData()
   LOCAL nRow := This.QueryRowIndex
   LOCAL nCol := This.QueryColIndex
   LOCAL aRow := aVisible[nRow]
   LOCAL cText := ""

   IF aRow[1] == "GROUP"
      DO CASE
      CASE nCol == 1
         cText := iif( aRow[4], "[ - ] ", "[ + ] " ) + aRow[2] + iif( aRow[4], "", " (" + LTrim( Str( CountGroupItems( aRow[2] ) ) ) + ")" )
         This.QueryData := cText
      CASE nCol == 2
         This.QueryData := ""
      ENDCASE
   ELSEIF aRow[1] == "ITEM"
      DO CASE
      CASE nCol == 1
         This.QueryData := Space(4) + aRow[2]
      CASE nCol == 2
         This.QueryData := iif( aRow[3] == 1, "Confirmed", "Unconfirmed" )
      ENDCASE
   ENDIF

RETURN ""

/*
 * FUNCTION CountGroupItems( cGroupName )
 *
 * Counts the number of items in a specified group.
 *
 * Purpose:
 *   - Finds the group header in aData by name (cGroupName).
 *   - Counts subsequent "ITEM" rows with matching group_id.
 *   - Used to display item counts in collapsed group headers.
 *
 * Parameters:
 *   cGroupName (STRING): Name of the group (GROUP1_ID or GROUP2_ID).
 *
 * Returns:
 *   nCount (NUMERIC): Number of items in the specified group.
 *
 * Notes:
 *   - Uses group_id (1 or 2) to ensure accurate item counting.
 *   - Stops counting when a non-item row or different group_id is encountered.
 */
FUNCTION CountGroupItems( cGroupName )
   LOCAL nCount := 0
   LOCAL i := AScan( aData, {|r| r[1] == "GROUP" .AND. r[2] == cGroupName } )

   IF i > 0
      i++
      DO WHILE i <= Len( aData ) .AND. aData[i][1] == "ITEM" .AND. aData[i][4] == iif( cGroupName == GROUP1_ID, 1, 2 )
         nCount++
         i++
      ENDDO
   ENDIF

RETURN nCount

/*
 * FUNCTION Grid1_OnDblClick()
 *
 * Handles double-click events on the grid to toggle group expansion or move items.
 *
 * Purpose:
 *   - For group rows: Toggles the expansion state (expanded/collapsed) and refreshes the grid.
 *   - For item rows: Moves the item to the opposite group by calling ChangeGroup.
 *   - Updates aVisible and the grid's item count after changes.
 *
 * Parameters: None (uses This.VALUE for row index)
 * Returns: NIL
 *
 * Notes:
 *   - Uses aVisible to identify the clicked row and aData to update the master data.
 *   - Ensures grid refresh to reflect changes in group expansion or item movement.
 */
FUNCTION Grid1_OnDblClick()
   LOCAL nRow := This.VALUE[1]
   LOCAL aRow, i

   IF nRow > 0
      aRow := aVisible[nRow]
      IF aRow[1] == "GROUP"
         i := AScan( aData, {|r| r[1] == "GROUP" .AND. r[2] == aRow[2] } )
         IF i > 0
            aData[i][4] := ! aData[i][4] // Toggle expanded/collapsed
            aVisible := BuildVisibleRows( aData )
            This.Grid_1.ItemCount := Len( aVisible )
            This.Refresh
         ENDIF
      ELSE
         ChangeGroup( nRow )
      ENDIF
   ENDIF

RETURN NIL

/*
 * FUNCTION ChangeGroup( nRow )
 *
 * Moves an item to the opposite group when double-clicked.
 *
 * Purpose:
 *   - Identifies the item in aVisible by row index and determines its current and target groups.
 *   - Calls MoveItem to update aData and refresh the grid.
 *
 * Parameters:
 *   nRow (NUMERIC): Row index in aVisible.
 *
 * Returns:
 *   .T. (LOGICAL): Indicates successful execution.
 *
 * Notes:
 *   - Uses aVisible[nRow][3] to determine the current group_id (1 or 2).
 *   - Assumes two groups (GROUP1_ID and GROUP2_ID) for toggling.
 */
FUNCTION ChangeGroup( nRow )
   LOCAL cItemName := aVisible[nRow][2]
   LOCAL cFromGroup := iif( aVisible[nRow][3] == 1, GROUP1_ID, GROUP2_ID )
   LOCAL cToGroup := iif( aVisible[nRow][3] == 1, GROUP2_ID, GROUP1_ID )

   MoveItem( cItemName, cFromGroup, cToGroup )

RETURN .T.

/*
 * FUNCTION Grid1_ForeColor( x, nRow )
 *
 * Sets the text color for grid rows based on row type.
 *
 * Purpose:
 *   - Returns black for group headers and dark blue for items to visually distinguish them.
 *
 * Parameters:
 *   x (ANY): Unused (column index, not needed here).
 *   nRow (NUMERIC): Row index in aVisible.
 *
 * Returns:
 *   ARRAY: RGB color values {R, G, B}.
 *
 * Notes:
 *   - Applied dynamically via DYNAMICFORECOLOR grid property.
 */
FUNCTION Grid1_ForeColor( x, nRow )
   LOCAL aRow := aVisible[nRow]

   IF aRow[1] == "GROUP"
      RETURN { 0, 0, 0 } // Black text for group headers
   ELSE
      RETURN { 0, 0, 128 } // Dark blue for items
   ENDIF

RETURN NIL

/*
 * FUNCTION Grid1_BackColor( x, nRow )
 *
 * Sets the background color for grid rows based on row type.
 *
 * Purpose:
 *   - Returns light gray for group headers and white for items to enhance visual hierarchy.
 *
 * Parameters:
 *   x (ANY): Unused (column index, not needed here).
 *   nRow (NUMERIC): Row index in aVisible.
 *
 * Returns:
 *   ARRAY: RGB color values {R, G, B}.
 *
 * Notes:
 *   - Applied dynamically via DYNAMICBACKCOLOR grid property.
 */
FUNCTION Grid1_BackColor( x, nRow )
   LOCAL aRow := aVisible[nRow]

   IF aRow[1] == "GROUP"
      RETURN { 220, 220, 220 } // Light gray background
   ELSE
      RETURN { 255, 255, 255 } // White background
   ENDIF

RETURN NIL

/*
 * FUNCTION MoveItem( cItemName, cFromGroup, cToGroup )
 *
 * Moves an item from one group to another in aData and updates the grid.
 *
 * Purpose:
 *   - Finds the item in aData by name and current group.
 *   - Updates its group_id and repositions it in aData to maintain group and item order.
 *   - Rebuilds aVisible and refreshes the grid to reflect the change.
 *
 * Parameters:
 *   cItemName (STRING): Full name of the item (e.g., "Simpson Homer").
 *   cFromGroup (STRING): Current group name (GROUP1_ID or GROUP2_ID).
 *   cToGroup (STRING): Target group name (GROUP1_ID or GROUP2_ID).
 *
 * Returns:
 *   .T. (LOGICAL): If the move was successful.
 *   .F. (LOGICAL): If the item or target group was not found.
 *
 * Notes:
 *   - Maintains group order (Confirmed, then Unconfirmed) and inserts items at the end of the target group.
 *   - Updates aVisible and grid item count to ensure consistent display.
 */
FUNCTION MoveItem( cItemName, cFromGroup, cToGroup )
   LOCAL nFromIndex, nToIndex, aItem
   LOCAL nFromGroupId := iif( cFromGroup == GROUP1_ID, 1, 2 )
   LOCAL nToGroupId := iif( cToGroup == GROUP2_ID, 2, 1 )

   nFromIndex := AScan( aData, {|r| r[1] == "ITEM" .AND. r[2] + " " + r[3] == cItemName .AND. r[4] == nFromGroupId } )
   IF nFromIndex == 0
      RETURN .F.
   ENDIF

   aItem := AClone( aData[nFromIndex] )
   aItem[4] := nToGroupId

   ADel( aData, nFromIndex )
   ASize( aData, Len( aData ) - 1 )

   nToIndex := AScan( aData, {|r| r[1] == "GROUP" .AND. r[2] == cToGroup } )
   IF nToIndex == 0
      RETURN .F.
   ENDIF

   DO WHILE nToIndex < Len( aData ) .AND. aData[nToIndex + 1][1] == "ITEM" .AND. aData[nToIndex + 1][4] == nToGroupId
      nToIndex++
   ENDDO

   AIns( aData, nToIndex + 1, aItem, .T. )

   aVisible := BuildVisibleRows( aData )
   Form_1.Grid_1.ItemCount := Len( aVisible )
   Form_1.Grid_1.Refresh

RETURN .T.