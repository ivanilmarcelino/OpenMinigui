/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by KDJ
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include "hmg.ch"
#include "i_winuser.ch"

#define LABEL_NAME 1
#define LABEL_HWND 2

/*-----------------------------------------------------------------------------*
FUNCTION Main()
*------------------------------------------------------------------------------*
*
*  Description:
*     This is the main function of the HMG Extended demo application. It creates a window with labels that act like buttons.
*
*  Purpose:
*     This function demonstrates how to use labels as interactive elements (similar to buttons) in an HMG Extended application.
*     It creates a main window containing several labels, each of which responds to mouse hover, mouse leave, and click events.
*     The labels are also made focusable and navigable using the TAB key. This showcases how to extend the functionality of standard
*     HMG controls and create custom UI elements. The demo illustrates the use of HMG_ChangeWindowStyle to modify the appearance
*     and behavior of the labels, making them appear as interactive buttons.
*
*  Notes:
*     The demo uses the WS_TABSTOP and WS_EX_STATICEDGE window styles to make the labels focusable and give them a 3D border.
*     The HMG_GetUniqueName function is used to generate unique names for the labels, preventing naming conflicts.
*
*/
FUNCTION Main()

   LOCAL aLabel := Array( 3, 2 )  // Array to store label information: name and HWND (window handle)
   LOCAL n

   SET DIALOGBOX CENTER OF PARENT // Centers the dialog box relative to its parent window

   DEFINE WINDOW MainForm ;
         WIDTH 300 ;
         HEIGHT 260 ;
         TITLE "Labels as buttons" ;
         MAIN ;
         FONT "Arial" SIZE 9

      // these labels can get focus and process keyboard/mouse messages
      FOR n := 1 TO Len( aLabel )
         aLabel[ n ][ LABEL_NAME ] := HMG_GetUniqueName( "LABEL_" )             // Generate a unique name for the label

         DEFINE LABEL ( aLabel[ n ][ LABEL_NAME ] )
            ROW 10 + 55 * ( n - 1 )
            COL 10
            WIDTH 140
            HEIGHT 45
            VALUE "This is " + aLabel[ n ][ LABEL_NAME ]
            ALIGNMENT CENTER
            FONTCOLOR BLACK
            ONMOUSEHOVER OnLblHover( aLabel, This.Name )                        // Call OnLblHover when the mouse hovers over the label
            ONMOUSELEAVE OnLblLeave( This.Name )                                // Call OnLblLeave when the mouse leaves the label
            ACTION MsgBox( GetProperty( ThisWindow.NAME, This.NAME, "VALUE" ) ) // Display a message box when the label is clicked
         END LABEL

         aLabel[ n ][ LABEL_HWND ] := GetProperty( "MainForm", aLabel[ n ][ LABEL_NAME ], "HANDLE" ) // Get the window handle of the label

         HMG_ChangeWindowStyle( aLabel[ n ][ LABEL_HWND ], 0x00010200 /*WS_TABSTOP|SS_CENTERIMAGE*/, NIL, .F., .F. ) // Remove WS_TABSTOP and SS_CENTERIMAGE styles.  SS_CENTERIMAGE is not needed and WS_TABSTOP is added back later.
         HMG_ChangeWindowStyle( aLabel[ n ][ LABEL_HWND ], WS_EX_STATICEDGE, NIL, .T., .T. ) // Add WS_EX_STATICEDGE style to give the label a 3D border
      NEXT

      DEFINE LABEL NUL                                                // A standard label for comparison
         ROW 190
         COL 120
         AUTOSIZE .T.
         VALUE "This is a standard LABEL"
      END LABEL

      DEFINE BUTTON CloseButton
         ROW 190
         COL 10
         WIDTH 80
         HEIGHT 23
         CAPTION "Close"
         ACTION MainForm.RELEASE                                      // Close the main window when the button is clicked
         ONGOTFOCUS LabelSetBorder( aLabel, 0 )                       // Remove border from labels when the close button gets focus
      END BUTTON

   END WINDOW

   SetFocus( aLabel[ 1 ][ LABEL_HWND ] )                              // Set focus to the first label
   LabelSetBorder( aLabel, aLabel[ 1 ][ LABEL_HWND ] )                // Add border to the first label

   ON KEY TAB OF MainForm ACTION OnNextTabItem( aLabel, .F. )         // Call OnNextTabItem when the TAB key is pressed
   ON KEY SHIFT + TAB OF MainForm ACTION OnNextTabItem( aLabel, .T. ) // Call OnNextTabItem when SHIFT + TAB is pressed

   ON KEY DOWN OF MainForm ACTION OnNextTabItem( aLabel, .F. )        // Call OnNextTabItem when the DOWN arrow key is pressed
   ON KEY UP OF MainForm ACTION OnNextTabItem( aLabel, .T. )          // Call OnNextTabItem when the UP arrow key is pressed

   ON KEY RETURN OF MainForm ACTION OnEnter( aLabel )                 // Call OnEnter when the RETURN key is pressed
   ON KEY SPACE OF MainForm ACTION OnEnter( aLabel )                  // Call OnEnter when the SPACE key is pressed

   MainForm.CENTER
   MainForm.ACTIVATE

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION OnNextTabItem( aLabel, lPrev )
*------------------------------------------------------------------------------*
*
*  Description:
*     This function handles the navigation between labels using the TAB key (or arrow keys).
*
*  Parameters:
*     aLabel - An array containing information about the labels (name and HWND).
*     lPrev  - A logical value indicating whether to move to the previous label (.T.) or the next label (.F.).
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function is called when the user presses the TAB key (or arrow keys) to navigate between the labels.
*     It finds the currently focused label (the one with the border), removes the border from it, and adds the border to the next (or previous) label.
*     This provides a visual indication of which label has focus. The function uses GetNextDlgTabItem to determine the next control in the tab order.
*
*  Notes:
*     The function uses HMG_IsWindowStyle to check if a label has the WS_BORDER style.
*     The GetNextDlgTabItem function is used to get the handle of the next control in the tab order.
*
*/
FUNCTION OnNextTabItem( aLabel, lPrev )

   LOCAL nPos

   nPos := GetFocusedLabelIndex( aLabel )                          // Find the index of the label with the border
   IF lPrev .AND. nPos > 1 .OR. ! lPrev .AND. nPos < Len( aLabel ) // Check if there is a previous or next label
      LabelSetBorder( aLabel, GetNextDlgTabItem( MainForm.HANDLE, MainForm.&( MainForm.FocusedControl ).HANDLE, lPrev ) ) // Set the border to the next/previous label
   ENDIF

   nPos := GetFocusedLabelIndex( aLabel )      // Find the index of the label with the border
   IF nPos > 0
      SetFocus( aLabel[ nPos ][ LABEL_HWND ] ) // Set focus to the label with the border
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION OnEnter( aLabel )
*------------------------------------------------------------------------------*
*
*  Description:
*     This function handles the event when the user presses the ENTER or SPACE key while a label is focused.
*
*  Parameters:
*     aLabel - An array containing information about the labels (name and HWND).
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function is called when the user presses the ENTER or SPACE key while a label has focus.
*     It simulates a click on the focused label by displaying a message box with the label's value.
*     If no label is focused (e.g., the Close button has focus), it triggers the Close button's action.
*
*  Notes:
*     The function uses AScan to find the label with the WS_BORDER style, indicating that it has focus.
*     The Eval function is used to execute the Close button's action.
*
*/
FUNCTION OnEnter( aLabel )

   LOCAL nPos := GetFocusedLabelIndex( aLabel ) // Find the index of the label with the border

   IF nPos > 0
      MsgBox( GetProperty( ThisWindow.NAME, aLabel[ nPos ][ LABEL_NAME ], "VALUE" ) ) // Display a message box with the label's value
   ELSE
      Eval( MainForm.CloseButton.ACTION ) // Execute the Close button's action
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION OnLblHover( aLabel, cControl )
*------------------------------------------------------------------------------*
*
*  Description:
*     This function handles the mouse hover event for a label.
*
*  Parameters:
*     aLabel   - An array containing information about the labels (name and HWND).
*     cControl - The name of the label that the mouse is hovering over.
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function is called when the mouse cursor hovers over a label.
*     It changes the cursor to a "finger" cursor, sets focus to the label, adds a border to the label, and changes the label's font color to red and makes it bold.
*     This provides visual feedback to the user that the label is interactive.
*
*  Notes:
*     The function uses RC_CURSOR to change the cursor.
*     The function uses SetFocus to set focus to the label.
*     The function uses LabelSetBorder to add a border to the label.
*     The function uses SetProperty to change the label's font color and bold state.
*
*/
FUNCTION OnLblHover( aLabel, cControl )

   LOCAL nPos := AScan( aLabel, {| a1 | cControl == a1[ LABEL_NAME ] } ) // Find the index of the label with the given name
   LOCAL cForm := ThisWindow.NAME

   RC_CURSOR( "MINIGUI_FINGER" )                          // Change the cursor to a "finger" cursor

   SetFocus( aLabel[ nPos ][ LABEL_HWND ] )               // Set focus to the label
   LabelSetBorder( aLabel, aLabel[ nPos ][ LABEL_HWND ] ) // Add a border to the label

   SetProperty( cForm, cControl, "FONTCOLOR", RED )       // Change the font color to red
   SetProperty( cForm, cControl, "FONTBOLD", .T. )        // Make the font bold

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION OnLblLeave( cControl )
*------------------------------------------------------------------------------*
*
*  Description:
*     This function handles the mouse leave event for a label.
*
*  Parameters:
*     cControl - The name of the label that the mouse is leaving.
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function is called when the mouse cursor leaves a label.
*     It resets the label's font color to black and removes the bold style.
*     This restores the label's appearance to its default state.
*
*  Notes:
*     The function uses SetProperty to change the label's font color and bold state.
*
*/
FUNCTION OnLblLeave( cControl )

   LOCAL cForm := ThisWindow.NAME

   SetProperty( cForm, cControl, "FONTCOLOR", BLACK ) // Change the font color to black
   SetProperty( cForm, cControl, "FONTBOLD", .F. )    // Remove the bold style

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION LabelSetBorder( aLabel, nHWnd )
*------------------------------------------------------------------------------*
*
*  Description:
*     This function sets a border around a specified label and removes the border from any other label that currently has it.
*
*  Parameters:
*     aLabel - An array containing information about the labels (name and HWND).
*     nHWnd  - The window handle of the label to set the border on.  A value of 0 removes the border from all labels.
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function is used to visually indicate which label has focus. It adds a border to the currently focused label and removes the border from any previously focused label.
*     This provides a clear visual cue to the user about which label is currently active.
*
*  Notes:
*     The function uses HMG_ChangeWindowStyle to add and remove the WS_BORDER and WS_EX_STATICEDGE styles.
*     The function uses AScan to find the index of the label that currently has the border.
*
*/
FUNCTION LabelSetBorder( aLabel, nHWnd )

   LOCAL nPosDel := GetFocusedLabelIndex( aLabel )                       // Find the index of the label with the border
   LOCAL nPosSet := AScan( aLabel, {| a1 | nHWnd == a1[ LABEL_HWND ] } ) // Find the index of the label with the given HWND

   IF nPosDel != nPosSet // Check if the label to remove the border from is different from the label to add the border to
      IF nPosDel > 0 // Check if there is a label with the border
         HMG_ChangeWindowStyle( aLabel[ nPosDel ][ LABEL_HWND ], NIL, WS_BORDER, .F., .F. )        // Remove the WS_BORDER style
         HMG_ChangeWindowStyle( aLabel[ nPosDel ][ LABEL_HWND ], WS_EX_STATICEDGE, NIL, .T., .T. ) // Add the WS_EX_STATICEDGE style
      ENDIF

      IF nPosSet > 0 // Check if there is a label with the given HWND
         HMG_ChangeWindowStyle( aLabel[ nPosSet ][ LABEL_HWND ], WS_BORDER, NIL, .F., .F. )        // Add the WS_BORDER style
         HMG_ChangeWindowStyle( aLabel[ nPosSet ][ LABEL_HWND ], NIL, WS_EX_STATICEDGE, .T., .T. ) // Remove the WS_EX_STATICEDGE style
      ENDIF
   ENDIF

RETURN NIL

FUNCTION GetFocusedLabelIndex( aLabel )
RETURN AScan( aLabel, {| a1 | HMG_IsWindowStyle( a1[ LABEL_HWND ], WS_BORDER ) } )
