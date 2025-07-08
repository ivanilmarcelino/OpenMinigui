/*
 * ListView with Extended styles
 */

#include "minigui.ch"
#include "i_winuser.ch"

// Define arrays for storing image references, column headers, and column widths
STATIC a_image[ 2 ], a_cab[ 4 ], a_width[ 4 ]

FUNCTION Main

   LOCAL i, aRows := {}
   LOCAL aStyles := {}, aStylesTip := {}, aStylesEx := {}, aStylesExTip := {}

   // Initialize images for grid control
   a_image[ 1 ] = 'BMP_NO' // Image for "no"
   a_image[ 2 ] = 'BMP_OK' // Image for "ok"

   // Populate rows with sample data
   FOR i := 1 TO 100
      AAdd( aRows, { i % 2, 'Item ' + hb_ntos( i ), hb_ntos( i * 10 ), 'Created: ' + hb_TSToStr( hb_DateTime() ) } )
   NEXT

   // Define column headers and their widths
   a_cab[ 1 ] = '' ; a_width[ 1 ] = 20
   a_cab[ 2 ] = 'Item Name' ; a_width[ 2 ] = 180
   a_cab[ 3 ] = 'Value' ; a_width[ 3 ] = 80
   a_cab[ 4 ] = 'Time' ; a_width[ 4 ] = 80

   // Populate basic styles and their descriptions
   AAdd( aStyles, "ALIGNLEFT" )
   AAdd( aStyles, "ALIGNTOP" )
   AAdd( aStyles, "AUTOARRANGE" )
   AAdd( aStyles, "NOCOLUMNHEADER" )
   AAdd( aStyles, "NOLABELWRAP" )
   AAdd( aStyles, "NOSCROLL" )
   AAdd( aStyles, "NOSORTHEADER" )
   AAdd( aStyles, "SHOWSELALWAYS" )
   AAdd( aStyles, "SINGLESEL" )
   AAdd( aStyles, "SORTASCENDING" )
   AAdd( aStyles, "SORTDESCENDING" )

   AAdd( aStylesTip, "Specifies that items are left-aligned in icon and small icon view." )
   AAdd( aStylesTip, "Specifies that items are aligned with the top of the control in icon and small icon view." )
   AAdd( aStylesTip, "Specifies that icons are automatically kept arranged in icon view and small icon view." )
   AAdd( aStylesTip, "Specifies that a column header is not displayed in report view." )
   AAdd( aStylesTip, "Displays item text on a single line in icon view." )
   AAdd( aStylesTip, "Disables scrolling. All items must be within the client area." )
   AAdd( aStylesTip, "Specifies that column headers do not work like buttons." )
   AAdd( aStylesTip, "Always show the selection, if any, even if the control does not have the focus." )
   AAdd( aStylesTip, "Allows only one item at a time to be selected." )
   AAdd( aStylesTip, "Sorts items based on item text in ascending order." )
   AAdd( aStylesTip, "Sorts items based on item text in descending order." )

   // Populate extended styles and their descriptions
   AAdd( aStylesEx, "CHECKBOXES" )
   AAdd( aStylesEx, "FLATSB" )
   AAdd( aStylesEx, "DOUBLEBUFFER" )
   AAdd( aStylesEx, "GRIDLINES" )
   AAdd( aStylesEx, "HEADERDRAGDROP" )
   AAdd( aStylesEx, "INFOTIP" )
   AAdd( aStylesEx, "ONECLICKACTIVATE" )
   AAdd( aStylesEx, "TRACKSELECT" )
   AAdd( aStylesEx, "TWOCLICKACTIVATE" )
   AAdd( aStylesEx, "UNDERLINECOLD" )
   AAdd( aStylesEx, "UNDERLINEHOT" )

   AAdd( aStylesExTip, "Enables check boxes for items in a list view control." )
   AAdd( aStylesExTip, "Enables flat scroll bars in the list view." )
   AAdd( aStylesExTip, "Paints via double-buffering, which reduces flickering." )
   AAdd( aStylesExTip, "Displays gridlines around items and subitems." )
   AAdd( aStylesExTip, "Enables drag-and-drop reordering of columns in a list view control." )
   AAdd( aStylesExTip, "Enables a LVN_GETINFOTIP notification message to the parent window before displaying an item's tooltip." )
   AAdd( aStylesExTip, "The list view control sends an LVN_ITEMACTIVATE notification message to the parent window when the user clicks an item." )
   AAdd( aStylesExTip, "Enables hot-track selection in a list view control.  Requires either ONECLICKACTIVATE or TWOCLICKACTIVATE." )
   AAdd( aStylesExTip, "The list view control sends an LVN_ITEMACTIVATE notification message to the parent window when the user double-clicks an item." )
   AAdd( aStylesExTip, "Causes non-hot items that are activatable to be displayed with underlined text. This style requires that TWOCLICKACTIVATE also be set." )
   AAdd( aStylesExTip, "Causes hot items that are activatable to be displayed with underlined text. This style requires that ONECLICKACTIVATE or TWOCLICKACTIVATE also be set." )

   // Define main application window
   DEFINE WINDOW Form_1 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'Grid control with Extended styles' ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         ON INIT GridOnChange()

      // Define a status bar to display selection information
      DEFINE STATUSBAR
         STATUSITEM "" FONTCOLOR BLACK CENTERALIGN
      END STATUSBAR

      // Add a grid to the window
      @ 20, 10 GRID Grid_1 ;
         WIDTH 360 ;
         HEIGHT 389 ;
         HEADERS a_cab ;
         WIDTHS a_width ;
         ITEMS aRows ;
         VALUE { 1 } ;
         IMAGE a_image ;
         ON CHANGE GridOnChange() ;
         NOLINES ;
         MULTISELECT ;
         PAINTDOUBLEBUFFER

      // Add a tab control for configuring styles
      DEFINE TAB Tab_1 ;
            AT 20, 389 ;
            WIDTH 230 ;
            HEIGHT 389 ;
            VALUE 1

         // Page for basic styles
         PAGE 'Styles'

            @ 35, 15 FRAME Frame_1 WIDTH 200 HEIGHT 300

            // Generate checkboxes for each style
            FOR i := 1 TO Len( aStyles )
               @ 30 + i * 24, 30 CheckBox ( "Check1_" + hb_ntos( i ) ) CAPTION aStyles[ i ] WIDTH 160 HEIGHT 18 TOOLTIP aStylesTip[ i ]
            NEXT

            // Apply button for styles
            @ 350, 115 BUTTON Button_1 CAPTION "Apply" WIDTH 100 HEIGHT 26 ACTION ApplyStyles()

         END PAGE

         // Page for extended styles
         PAGE 'Extended Styles'

            @ 35, 15 FRAME Frame_2 WIDTH 200 HEIGHT 300

            // Generate checkboxes for each extended style
            FOR i := 1 TO Len( aStylesEx )
               @ 30 + i * 24, 30 CheckBox ( "Check2_" + hb_ntos( i ) ) CAPTION aStylesEx[ i ] WIDTH 160 HEIGHT 18 TOOLTIP aStylesExTip[ i ]
            NEXT

            // Apply button for extended styles
            @ 350, 115 BUTTON Button_2 CAPTION "Apply" WIDTH 100 HEIGHT 26 ACTION ApplyStylesEx()

         END PAGE

      END TAB

      // Set initial checkbox states for styles
      Form_1.Check1_1.Enabled := .F.
      Form_1.Check1_2.Enabled := .F.
      Form_1.Check1_3.Enabled := .F.
      Form_1.Check1_5.Enabled := .F.
      Form_1.Check1_8.VALUE := .T.
      Form_1.Check1_8.Enabled := .F.
      Form_1.Check1_10.VALUE := .T.

      Form_1.Check2_3.VALUE := .T.
      Form_1.Check2_5.VALUE := .T.
      Form_1.Check2_6.VALUE := .T.

   END WINDOW

   Form_1.Center
   Form_1.Activate

RETURN NIL

*.......................................................*

// Handle grid value changes and update the status bar
PROCEDURE GridOnChange()

   LOCAL v := form_1.grid_1.VALUE

   form_1.statusbar.item( 1 ) := ;
      'Selected: ' + iif( ValType( v ) == 'N', hb_ntos( v ), hb_ValToExp( v ) ) + '/' + ;
      hb_ntos( form_1.grid_1.ItemCount )

RETURN

*.......................................................*

// Apply basic styles to the grid
PROCEDURE ApplyStyles()

   LOCAL i, aRows := {}, v

   v := Form_1.Grid_1.VALUE
   // Redefine the grid with new style settings
   Form_1.Grid_1.RELEASE

   // Update checkbox values based on conditions
   IF Form_1.Check1_11.VALUE
      FOR i := 100 TO 1 STEP -1
         AAdd( aRows, { i % 2, 'Item ' + hb_ntos( i ), hb_ntos( i * 10 ), 'Created: ' + hb_TSToStr( hb_DateTime() ) } )
      NEXT
      Form_1.Check1_10.VALUE := .F.
   ELSE
      FOR i := 1 TO 100
         AAdd( aRows, { i % 2, 'Item ' + hb_ntos( i ), hb_ntos( i * 10 ), 'Created: ' + hb_TSToStr( hb_DateTime() ) } )
      NEXT
      Form_1.Check1_10.VALUE := .T.
   ENDIF

   form_1.statusbar.item( 1 ) := ""

   // Reinitialize the grid with updated rows
   DEFINE GRID Grid_1
      PARENT Form_1
      ROW 20
      COL 10
      WIDTH 358
      HEIGHT 388
      HEADERS a_cab
      WIDTHS a_width
      ITEMS aRows
      VALUE iif( Form_1.Check1_9.VALUE, iif( ISARRAY( v ) .AND. Len( v ) > 0, v[ 1 ], 1 ), { v } )
      IMAGE a_image
      ONCHANGE GridOnChange()
      SHOWHEADERS ! Form_1.Check1_4.VALUE
      NOSORTHEADERS Form_1.Check1_7.VALUE
      MULTISELECT ! Form_1.Check1_9.VALUE
   END GRID

   SetWindowStyle ( Form_1.Grid_1.Handle, LVS_NOSCROLL, Form_1.Check1_6.Value )

   ApplyStylesEx()

   Form_1.Grid_1.SetFocus

RETURN

*.......................................................*

// Apply extended styles to the grid
PROCEDURE ApplyStylesEx()

   LOCAL h := GetControlHandle( "Grid_1", "Form_1" )
   LOCAL iStyle

   // Use system calls to set or unset extended styles
   // Based on the values of extended style checkboxes
   iStyle := LVS_EX_CHECKBOXES
   IF Form_1.Check2_1.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_FLATSB
   IF Form_1.Check2_2.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_DOUBLEBUFFER
   IF Form_1.Check2_3.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_GRIDLINES
   IF Form_1.Check2_4.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_HEADERDRAGDROP
   IF Form_1.Check2_5.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_INFOTIP
   IF Form_1.Check2_6.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_ONECLICKACTIVATE
   IF Form_1.Check2_7.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_TRACKSELECT
   IF Form_1.Check2_8.VALUE .AND. ( Form_1.Check2_7.VALUE .OR. Form_1.Check2_9.Value )
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_TWOCLICKACTIVATE
   IF Form_1.Check2_9.VALUE
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_UNDERLINECOLD
   IF Form_1.Check2_9.VALUE .AND. Form_1.Check2_10.VALUE .AND. Form_1.Check2_11.VALUE == .F.
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

   iStyle := LVS_EX_UNDERLINEHOT
   IF Form_1.Check2_11.VALUE .AND. Form_1.Check2_10.VALUE == .F. .AND. ( Form_1.Check2_7.VALUE .OR. Form_1.Check2_9.Value )
      ListView_ChangeExtendedStyle ( h, iStyle, NIL )
   ELSE
      ListView_ChangeExtendedStyle ( h, NIL, iStyle )
   ENDIF

RETURN
