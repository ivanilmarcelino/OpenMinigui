/*
 * HMG - Harbour Win32 GUI library Demo
 * Virtual Grid with Nested Groups
 *
 * Purpose: Demonstrates a virtual grid in HMG Extended with nested group headers and items,
 * allowing users to expand/collapse groups interactively. The grid displays a hierarchical
 * dataset with indentation to reflect group levels, optimized for efficient rendering.
 *
 * Author: Grigory Filatov
 * Date: 10/02/2025
 * License: Public domain
 */

#include "minigui.ch"

// Static variables to store hierarchical data and visible rows
STATIC aData      // Master array: { type, name, expanded (for groups)/visible (for items), level }
STATIC aVisible   // Filtered array of visible rows for the grid based on group expansion

/*
 * PROCEDURE Main()
 *
 * Initializes the application, sets up the main window, and configures a virtual grid
 * to display a hierarchical dataset with nested groups and items.
 *
 * Purpose:
 *   - Defines a hierarchical dataset (aData) with top-level groups, subgroups, and items.
 *   - Builds the visible rows array (aVisible) based on group expansion states.
 *   - Creates a main window with a virtual grid to display the data.
 *   - Configures grid properties for dynamic data display, user interaction (expand/collapse),
 *     and visual styling (colors and indentation).
 *
 * Parameters: None
 * Returns: None
 *
 * Notes:
 *   - Uses a virtual grid to optimize performance by loading only visible rows.
 *   - Supports nested groups with varying levels, indicated by indentation in the grid.
 *   - Double-clicking a group header toggles its expansion state.
 */
PROCEDURE MAIN

   // Initialize hierarchical dataset with groups and items
   aData := { ;
      { "GROUP", "Fruits", .T., 0 }, ;        // Top-level group, expanded, level 0
         { "GROUP", "Citrus", .F., 1 }, ;     // Subgroup, not expanded, level 1
            { "ITEM", "Orange", .T., 2 }, ;   // Item, visible, level 2
            { "ITEM", "Lemon", .T., 2 }, ;    // Item, visible, level 2
         { "GROUP", "Tropical", .T., 1 }, ;   // Subgroup, expanded, level 1
            { "ITEM", "Banana", .T., 2 }, ;   // Item, visible, level 2
            { "ITEM", "Mango", .T., 2 }, ;    // Item, visible, level 2
      { "GROUP", "Vegetables", .F., 0 }, ;    // Top-level group, not expanded, level 0
         { "ITEM", "Carrot", .T., 1 }, ;      // Item, visible, level 1
         { "ITEM", "Potato", .T., 1 }, ;      // Item, visible, level 1
         { "ITEM", "Tomato", .T., 1 } ;       // Item, visible, level 1
      }

   // Build visible rows based on group expansion
   aVisible := BuildVisibleRows( aData )

   // Define main window
   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 600 ;
         HEIGHT 400 ;
         MAIN ;
         TITLE "Virtual Grid with Nested Groups"

      // Define virtual grid
      @ 50, 10 GRID Grid_1 ;
         WIDTH 500 HEIGHT 280 ;
         HEADERS { "Name" } ;
         WIDTHS { 450 } ;
         VALUE { 1, 1 } ;
         VIRTUAL ;
         ITEMCOUNT Len( aVisible ) ;
         ON QUERYDATA Grid1_OnQueryData() ;
         ON DBLCLICK Grid1_OnClick() ;
         CELLNAVIGATION ;
         DYNAMICFORECOLOR { {| x, nRow | Grid1_ForeColor( x, nRow ) } } ;
         DYNAMICBACKCOLOR { {| x, nRow | Grid1_BackColor( x, nRow ) } }

   END WINDOW

   // Center and activate the window
   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

/*
 * FUNCTION BuildVisibleRows( aData )
 *
 * Creates an array of visible rows for the grid based on group expansion states.
 *
 * Purpose:
 *   - Processes the hierarchical aData array to build aVisible, including only groups
 *     and their items if the parent group is expanded.
 *   - Uses a single-pass approach with ProcessNode to handle nested groups efficiently.
 *
 * Parameters:
 *   aData (ARRAY): Master hierarchical data array with groups and items.
 *
 * Returns:
 *   aVisible (ARRAY): Array of visible rows { type, name, expanded (for groups), level }.
 *
 * Notes:
 *   - Groups are always included; items are included only if their parent group is expanded.
 *   - Maintains hierarchical structure using level values for indentation.
 *   - Uses ProcessNode to recursively process nested groups and items.
 */
FUNCTION BuildVisibleRows( aData )

   LOCAL aVisible := {}
   LOCAL n := Len( aData ), i := 1, last

   // Process each node in aData, advancing index based on ProcessNode's return
   DO WHILE i <= n
      last := ProcessNode( aData, i, aVisible )
      i := last + 1
   ENDDO

RETURN aVisible

/*
 * FUNCTION ProcessNode( aData, idx, aOut )
 *
 * Processes a single node (group or item) in aData and adds it to aOut if visible.
 *
 * Purpose:
 *   - Handles a group or item at the given index, adding it to aOut.
 *   - For groups: Adds the group and, if expanded, recursively processes its children.
 *   - For items: Adds the item directly.
 *   - Returns the last index processed to skip child nodes correctly.
 *
 * Parameters:
 *   aData (ARRAY): Master data array.
 *   idx (NUMERIC): Index of the current node in aData.
 *   aOut (ARRAY): Output array (aVisible) to store visible rows.
 *
 * Returns:
 *   last (NUMERIC): Last index processed in aData (for caller to skip children).
 *
 * Notes:
 *   - Group rows in aOut: { "GROUP", name, expanded, level }
 *   - Item rows in aOut: { "ITEM", name, NIL, level }
 *   - Skips children of collapsed groups to optimize aVisible construction.
 */
FUNCTION ProcessNode( aData, idx, aOut )

   LOCAL n := Len( aData )
   LOCAL rec := aData[ idx ]
   LOCAL level := rec[ 4 ]
   LOCAL last := idx
   LOCAL j

   IF rec[ 1 ] == "GROUP"
      // Add group row
      AAdd( aOut, { "GROUP", rec[ 2 ], rec[ 3 ], level } )

      // Process children only if group is expanded
      IF rec[ 3 ]
         j := idx + 1
         DO WHILE j <= n .AND. aData[ j ][ 4 ] > level
            last := ProcessNode( aData, j, aOut )
            j := last + 1
         ENDDO
      ELSE
         // Skip children of collapsed group
         j := idx + 1
         DO WHILE j <= n .AND. aData[ j ][ 4 ] > level
            j++
         ENDDO
         last := j - 1
      ENDIF

   ELSEIF rec[ 1 ] == "ITEM"
      // Add item row
      AAdd( aOut, { "ITEM", rec[ 2 ], NIL, level } )
      last := idx
   ENDIF

RETURN last

/*
 * FUNCTION _CountNestedItemsAndLast( aData, idx )
 *
 * Counts items in a group’s subtree and returns the last index processed.
 *
 * Purpose:
 *   - Recursively counts all items under a group (including in nested subgroups).
 *   - Tracks the last index processed to support efficient tree traversal.
 *   - Used by CountGroupItems to display item counts in collapsed group headers.
 *
 * Parameters:
 *   aData (ARRAY): Master data array.
 *   idx (NUMERIC): Index of the group node in aData.
 *
 * Returns:
 *   ARRAY: { item_count (NUMERIC), last_index (NUMERIC) }
 *
 * Notes:
 *   - Handles nested groups by recursively calling itself for subgroups.
 *   - Counts only "ITEM" rows, ignoring other group rows in the count.
 */
FUNCTION _CountNestedItemsAndLast( aData, idx )

   LOCAL n := Len( aData ), level := aData[ idx ][ 4 ]
   LOCAL j := idx + 1, cnt := 0, last := idx
   LOCAL res

   DO WHILE j <= n .AND. aData[ j ][ 4 ] > level
      IF aData[ j ][ 1 ] == "ITEM"
         cnt++
         last := j
         j++
      ELSEIF aData[ j ][ 1 ] == "GROUP"
         res := _CountNestedItemsAndLast( aData, j )
         cnt += res[ 1 ]
         last := res[ 2 ]
         j := res[ 2 ] + 1
      ELSE
         j++
      ENDIF
   ENDDO

RETURN { cnt, last }

/*
 * FUNCTION CountGroupItems( cGroupName )
 *
 * Counts the total number of items in a group’s subtree, including nested groups.
 *
 * Purpose:
 *   - Finds the group in aData by name and uses _CountNestedItemsAndLast to count items.
 *   - Used to display item counts in collapsed group headers in the grid.
 *
 * Parameters:
 *   cGroupName (STRING): Name of the group to count items for.
 *
 * Returns:
 *   nCount (NUMERIC): Number of items in the group’s subtree.
 *
 * Notes:
 *   - Returns 0 if the group is not found.
 *   - Efficiently handles nested groups via recursive helper function.
 */
FUNCTION CountGroupItems( cGroupName )

   LOCAL res
   LOCAL i := AScan( aData, {| r | r[ 1 ] == "GROUP" .AND. r[ 2 ] == cGroupName } )
   IF i == 0
      RETURN 0
   ENDIF

   res := _CountNestedItemsAndLast( aData, i )

RETURN res[ 1 ]

/*
 * FUNCTION Grid1_OnQueryData()
 *
 * Provides data for virtual grid cells during rendering.
 *
 * Purpose:
 *   - Retrieves the row index being queried and formats data from aVisible.
 *   - Displays group headers with expansion indicators and item counts, and items with indentation.
 *
 * Parameters: None (uses This.QueryRowIndex and This.QueryColIndex)
 * Returns: "" (empty string, as data is set via This.QueryData)
 *
 * Notes:
 *   - Indents rows based on level (aRow[4] * 4 spaces).
 *   - Group headers show "[ - ]" or "[ + ]" and item counts when collapsed.
 *   - Only one column is used, so QueryColIndex is not checked.
 */
FUNCTION Grid1_OnQueryData()

   LOCAL nRow := This.QueryRowIndex
   LOCAL aRow := aVisible[ nRow ]
   LOCAL cIndent := Space( aRow[ 4 ] * 4 )
   LOCAL cText := ""

   IF aRow[ 1 ] == "GROUP"
      IF aRow[ 3 ]
         cText := cIndent + "[ - ] " + aRow[ 2 ]
      ELSE
         cText := cIndent + "[ + ] " + aRow[ 2 ] + " (" + LTrim( Str( CountGroupItems( aRow[ 2 ] ) ) ) + ")"
      ENDIF
      This.QueryData := cText
   ELSEIF aRow[ 1 ] == "ITEM"
      This.QueryData := cIndent + aRow[ 2 ]
   ENDIF

RETURN ""

/*
 * FUNCTION Grid1_OnClick()
 *
 * Handles double-click events on the grid to toggle group expansion.
 *
 * Purpose:
 *   - For group rows: Toggles the expansion state and rebuilds aVisible to update the grid.
 *   - Ignores double-clicks on item rows.
 *   - Refreshes the grid to reflect changes in visible rows.
 *
 * Parameters: None (uses This.CellRowIndex)
 * Returns: NIL
 *
 * Notes:
 *   - Matches group rows by name and level to ensure correct identification in aData.
 *   - Updates aVisible and grid item count after toggling expansion.
 */
FUNCTION Grid1_OnClick()

   LOCAL nRow := This.CellRowIndex
   LOCAL aRow := aVisible[ nRow ]
   LOCAL i

   IF aRow[ 1 ] == "GROUP"
      i := AScan( aData, {| r | r[ 1 ] == "GROUP" .AND. r[ 2 ] == aRow[ 2 ] .AND. r[ 4 ] == aRow[ 4 ] } )
      IF i > 0
         aData[ i ][ 3 ] := ! aData[ i ][ 3 ] // toggle expanded
         aVisible := BuildVisibleRows( aData )
         This.Grid_1.ItemCount := Len( aVisible )
         This.Refresh
      ENDIF
   ENDIF

RETURN NIL

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

   LOCAL aRow := aVisible[ nRow ]
   IF aRow[ 1 ] == "GROUP"
      RETURN { 0, 0, 0 } // black for groups
   ELSE
      RETURN { 0, 0, 128 } // dark blue for items
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

   LOCAL aRow := aVisible[ nRow ]
   IF aRow[ 1 ] == "GROUP"
      RETURN { 220, 220, 220 } // gray background for groups
   ELSE
      RETURN { 255, 255, 255 } // white for items
   ENDIF

RETURN NIL