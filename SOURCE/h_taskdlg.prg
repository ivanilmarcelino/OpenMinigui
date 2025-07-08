/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 */

#if ! defined( __XHARBOUR__ ) .AND. ( __HARBOUR__ - 0 > 0x030000 )

#include "hbclass.ch"
#include "TaskDlgs.ch"
#include "i_var.ch"

/*-----------------------------------------------------------------------------*
CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Defines the TSimpleTaskDialog class, which provides a simplified interface for displaying Task Dialogs.
*     This class is designed for basic use cases where only a title, instruction, content,
*     common buttons, and a main icon are needed.
*
*  Purpose:
*     This class simplifies the use of Task Dialogs for common scenarios, reducing the amount of code required
*     to display a basic dialog with a title, instruction, content, and standard buttons/icons.  It provides
*     a more user-friendly interface compared to directly using the underlying Windows API.
*
*  Notes:
*     This class is suitable for simple dialogs. For more complex scenarios with custom buttons, radio buttons,
*     or expanded information, use the TTaskDialog class.
*
*/
CREATE CLASS TSimpleTaskDialog FUNCTION SimpleTaskDialog

   EXPORTED:
   VAR    Cargo
   VAR    lError                       READONLY   INIT .T.
   VAR    nButtonResult                READONLY   INIT NIL
   VAR    nResult                      READONLY   INIT E_FAIL

   METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon )
   METHOD Execute()

   METHOD Title( cTitle )              SETGET
   METHOD Instruction( cInstruction )  SETGET
   METHOD Content( cContent )          SETGET
   METHOD CommonButtons( nCBs )        SETGET
   METHOD MainIcon( nIcon )            SETGET

   PROTECTED:
   VAR    cTitle                       INIT       NIL
   VAR    cInstruction                 INIT       NIL
   VAR    cContent                     INIT       NIL
   VAR    nCommonButtons               INIT       TDCBF_OK_BUTTON
   VAR    nMainIcon                    INIT       TD_NO_ICON

ENDCLASS

/*-----------------------------------------------------------------------------*
METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Constructor for the TSimpleTaskDialog class. Initializes the dialog's properties.
*
*  Parameters:
*     cTitle:       The title of the Task Dialog (String or Numeric).
*     cInstruction: The main instruction text (String or Numeric).
*     cContent:     The content text (String or Numeric).
*     nCommonButtons: A flag indicating which common buttons to display (Numeric, e.g., TDCBF_OK_BUTTON).
*     nMainIcon:    A flag indicating which main icon to display (Numeric, e.g., TD_INFORMATION_ICON).
*
*  Return Value:
*     Self (the object instance).
*
*  Purpose:
*     This constructor initializes the TSimpleTaskDialog object with the provided title, instruction, content,
*     common buttons, and main icon. It handles different data types for the title, instruction, and content,
*     and sets the common buttons and main icon flags if numeric values are provided.
*
*  Notes:
*     The constructor uses the iif() function to handle different data types for the title, instruction, and content.
*     If a non-string or non-numeric value is provided, it is set to NIL.
*
*/
METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog

   // Assign the title, instruction, and content, handling different data types and NULL values.
   ::cTitle       := iif( HB_ISNUMERIC( cTitle ), cTitle, iif( ! HB_ISSTRING( cTitle ), NIL, iif( HB_ISNULL( cTitle ), NIL, cTitle ) ) )
   ::cInstruction := iif( HB_ISNUMERIC( cInstruction ), cInstruction, iif( ! HB_ISSTRING( cInstruction ), NIL, iif( HB_ISNULL( cInstruction ), NIL, cInstruction ) ) )
   ::cContent     := iif( HB_ISNUMERIC( cContent ), cContent, iif( ! HB_ISSTRING( cContent ), NIL, iif( HB_ISNULL( cContent ), NIL, cContent ) ) )

   // If a numeric value is provided for common buttons, assign it.
   IF HB_ISNUMERIC( nCommonButtons )
      ::nCommonButtons := nCommonButtons
   ENDIF

   // If a numeric value is provided for the main icon, assign it.
   IF HB_ISNUMERIC( nMainIcon )
      ::nMainIcon := nMainIcon
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------*
METHOD Execute() CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Executes the Task Dialog, displaying it to the user.
*
*  Parameters:
*     None
*
*  Return Value:
*     Logical (.T. if the dialog executed successfully, .F. otherwise).
*
*  Purpose:
*     This method displays the Task Dialog to the user and sets the object's properties based on the result of the execution.
*     It checks if the operating system is Windows Vista or later and calls the appropriate API function.
*     If the operating system is older than Windows Vista, it sets the result to E_NOTIMPL (Not Implemented).
*
*  Notes:
*     The win_TaskDialog0 function is used to display the Task Dialog.
*     The @nButton parameter passes the variable by reference so the function can set the value.
*
*/
METHOD Execute() CLASS TSimpleTaskDialog

   LOCAL nResult
   LOCAL nButton := NIL

   // Initialize error flag and result variables.
   ::lError        := .T.
   ::nButtonResult := NIL
   ::nResult       := E_FAIL

   // Check if the operating system is Windows Vista or later.
   IF os_IsWinVista_Or_Later()
      // Call the win_TaskDialog0 function to display the Task Dialog.
      // The @nButton passes the variable by reference so the function can set the value.
      nResult := win_TaskDialog0( ,, ::cTitle, ::cInstruction, ::cContent, ::nCommonButtons, ::nMainIcon, @nButton )
   ELSE
      // If the operating system is older than Windows Vista, set the result to E_NOTIMPL (Not Implemented).
      nResult := E_NOTIMPL // Not implemented yet
   ENDIF

   // Update the object's properties based on the result of the Task Dialog execution.
   ::lError        := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nResult       := nResult

RETURN ( ! ::lError )

/*-----------------------------------------------------------------------------*
METHOD Title( cTitle ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the dialog's title.
*
*  Parameters:
*     cTitle: The new title for the dialog (String or Numeric).
*
*  Return Value:
*     The old title value.
*
*  Purpose:
*     This method allows you to get or set the title of the Task Dialog.
*     When setting the title, it handles different data types and NULL values.
*
*  Notes:
*     If the new title is a NULL string, the title is set to NIL.
*
*/
METHOD Title( cTitle ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cTitle

   // If the new title is a string or numeric value, assign it.
   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      // If the new title is a NULL string, set the title to NIL.
      ::cTitle := iif( HB_ISSTRING( cTitle ) .AND. HB_ISNULL( cTitle ), NIL, cTitle )
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD Instruction( cInstruction ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the dialog's main instruction text.
*
*  Parameters:
*     cInstruction: The new instruction text (String or Numeric).
*
*  Return Value:
*     The old instruction text value.
*
*  Purpose:
*     This method allows you to get or set the main instruction text of the Task Dialog.
*     When setting the instruction text, it handles different data types and NULL values.
*
*  Notes:
*     If the new instruction text is a NULL string, the instruction text is set to NIL.
*
*/
METHOD Instruction( cInstruction ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cInstruction

   // If the new instruction is a string or numeric value, assign it.
   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      // If the new instruction is a NULL string, set the instruction to NIL.
      ::cInstruction := iif( HB_ISSTRING( cInstruction ) .AND. HB_ISNULL( cInstruction ), NIL, cInstruction )
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD Content( cContent ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the dialog's content text.
*
*  Parameters:
*     cContent: The new content text (String or Numeric).
*
*  Return Value:
*     The old content text value.
*
*  Purpose:
*     This method allows you to get or set the content text of the Task Dialog.
*     When setting the content text, it handles different data types and NULL values.
*
*  Notes:
*     If the new content text is a NULL string, the content text is set to NIL.
*
*/
METHOD Content( cContent ) CLASS TSimpleTaskDialog

   LOCAL cOldVal := ::cContent

   // If the new content is a string or numeric value, assign it.
   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      // If the new content is a NULL string, set the content to NIL.
      ::cContent := iif( HB_ISSTRING( cContent ) .AND. HB_ISNULL( cContent ), NIL, cContent )
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD CommonButtons( nCBs ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the common buttons flag.
*
*  Parameters:
*     nCBs: The new common buttons flag (Numeric).
*
*  Return Value:
*     The old common buttons flag value.
*
*  Purpose:
*     This method allows you to get or set the common buttons flag of the Task Dialog.
*     The common buttons flag indicates which common buttons to display (e.g., TDCBF_OK_BUTTON, TDCBF_CANCEL_BUTTON).
*
*  Notes:
*     The new common buttons flag must be a numeric value.
*
*/
METHOD CommonButtons( nCBs ) CLASS TSimpleTaskDialog

   LOCAL nOldVal := ::nCommonButtons

   // If the new common buttons flag is a numeric value, assign it.
   IF HB_ISNUMERIC( nCBs )
      ::nCommonButtons := nCBs
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD MainIcon( nIcon ) CLASS TSimpleTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the main icon flag.
*
*  Parameters:
*     nIcon: The new main icon flag (Numeric).
*
*  Return Value:
*     The old main icon flag value.
*
*  Purpose:
*     This method allows you to get or set the main icon flag of the Task Dialog.
*     The main icon flag indicates which main icon to display (e.g., TD_INFORMATION_ICON, TD_WARNING_ICON).
*
*  Notes:
*     The new main icon flag must be a numeric value.
*
*/
METHOD MainIcon( nIcon ) CLASS TSimpleTaskDialog

   LOCAL nOldVal := ::nMainIcon

   // If the new main icon flag is a numeric value, assign it.
   IF HB_ISNUMERIC( nIcon )
      ::nMainIcon := nIcon
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Defines the TTaskDialog class, which provides a comprehensive interface for displaying Task Dialogs with advanced features.
*     This class allows for customization of various aspects of the dialog, including custom buttons,
*     radio buttons, verification checkboxes, and expanded information.
*
*  Purpose:
*     This class provides a flexible and customizable way to display Task Dialogs with advanced features.
*     It allows you to create complex dialogs with custom buttons, radio buttons, verification checkboxes,
*     and expanded information.
*
*  Notes:
*     This class is suitable for complex dialogs. For simple dialogs, use the TSimpleTaskDialog class.
*
*/
CREATE CLASS TTaskDialog FUNCTION TaskDialog

   EXPORTED:
   VAR    Cargo
   VAR    lActive               READONLY   INIT .F.
   VAR    lError                READONLY   INIT .T.
   VAR    nButtonResult         READONLY   INIT NIL
   VAR    nRadioButtonResult    READONLY   INIT NIL
   VAR    nResult               READONLY   INIT E_FAIL
   VAR    lVerifyResult         READONLY   INIT .F.

   METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon )
   METHOD Execute() INLINE ::ShowDialog()
   METHOD ShowDialog()
   METHOD DialogHandle()
   METHOD Showing( lState )
   METHOD OnCreated( hWnd, nNotify, nWParam, nLParam )
   METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam )
   METHOD Listener( hWnd, nNotify, nWParam, nLParam )
   METHOD CommonButtons( nCBs )                SETGET
   METHOD WindowTitle( cTitle )                SETGET
   METHOD Title( cTitle )                      SETGET
   METHOD MainIcon( nIcon )                    SETGET
   METHOD MainInstruction( cInstruction )      SETGET
   METHOD Instruction( cInstruction )          SETGET
   METHOD Content( cContent )                  SETGET
   METHOD CustomButtons( aCustButton )         SETGET
   METHOD DefaultButton( nDefaultButton )      SETGET
   METHOD CustomRadioButtons( aCustButton )    SETGET
   METHOD DefaultRadioButton( nDefaultButton ) SETGET
   METHOD VerificationText( cText )            SETGET
   METHOD ExpandedInfo( cText )                SETGET
   METHOD ExpandedControlText( cText )         SETGET
   METHOD ExpandedCtrlText( cText )            SETGET
   METHOD CollapsedControlText( cText )        SETGET
   METHOD CollapsedCtrlText( cText )           SETGET
   METHOD FooterIcon( nIcon )                  SETGET
   METHOD Footer( cFooter )                    SETGET
   METHOD Width( nWidth )                      SETGET
   METHOD Parent( cFormName )                  SETGET
   METHOD ParentHandle( nHandle )              SETGET
   METHOD CallBackBlock( bCode )               SETGET
   METHOD Flags( nFlags )                      SETGET
   METHOD AllowDialogCancellation( lNewVal )   SETGET
   METHOD CanBeMinimized( lNewVal )            SETGET
   METHOD EnableHyperlinks( lNewVal )          SETGET
   METHOD ExpandedByDefault( lNewVal )         SETGET
   METHOD ExpandFooterArea( lNewVal )          SETGET
   METHOD NoDefaultRadioButton( lNewVal )      SETGET
   METHOD PositionRelativeToWindow( lNewVal )  SETGET
   METHOD RightToLeftLayout( lNewVal )         SETGET
   METHOD VerificationEnabled( lNewVal )       SETGET
   METHOD timeoutMS( nMS )                     SETGET
   METHOD TimedOut( lOut )                     SETGET
   // NOTE: Next method returns valid (non NIL) result if a the dialog has been shown
   // The ID of the clicked button
   METHOD SelectedButton()      INLINE ::nButtonResult
   // The ID of the selected radio button
   METHOD SelectedRadioButton() INLINE ::nRadioButtonResult
   // The state of the verification checkbox (read only)
   METHOD VerificationChecked() INLINE ::lVerifyResult

   PROTECTED:
   VAR aConfig                  INIT Array( TDC_CONFIG )
   VAR HWND            READONLY INIT NIL
   VAR lTimeOut        READONLY INIT .F.
   VAR nTimeOutMS      READONLY INIT 0

ENDCLASS

/*-----------------------------------------------------------------------------*
METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Constructor for the TTaskDialog class.  Initializes the Task Dialog with the specified parameters.
*
*  Parameters:
*     cTitle - The title of the Task Dialog (String or Numeric).
*     cInstruction - The main instruction text (String or Numeric).
*     cContent - The main content text (String or Numeric).
*     cFooter - The footer text (String or Numeric).
*     nCommonButtons - A numeric value representing the common buttons to display (e.g., OK, Cancel, Yes, No).  See TaskDlgs.ch for possible values.
*     nMainIcon - A numeric value representing the main icon to display. See TaskDlgs.ch for possible values.
*
*  Return Value:
*     The newly created TTaskDialog object (Self).
*
*  Purpose:
*     This method is responsible for creating a new instance of the TTaskDialog class and initializing its configuration parameters.
*     It takes the title, instruction, content, footer, common buttons, and main icon as input and stores them in the aConfig array.
*     This array is used later when the ShowDialog method is called to create and display the Task Dialog.
*
*  Notes:
*     The parameters can be either strings or numeric values.  If a parameter is not a string or numeric, it will be treated as NIL.
*     The HB_ISNUMERIC, HB_ISSTRING, and HB_ISNULL functions are used to check the data type of the parameters.
*
*/
METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon ) CLASS TTaskDialog

   ::aConfig[ TDC_WINDOWTITLE ]     := iif( HB_ISNUMERIC( cTitle ), cTitle, iif( ! HB_ISSTRING( cTitle ), NIL, iif( HB_ISNULL( cTitle ), NIL, cTitle ) ) )
   ::aConfig[ TDC_MAININSTRUCTION ] := iif( HB_ISNUMERIC( cInstruction ), cInstruction, iif( ! HB_ISSTRING( cInstruction ), NIL, iif( HB_ISNULL( cInstruction ), NIL, cInstruction ) ) )
   ::aConfig[ TDC_CONTENT ] := iif( HB_ISNUMERIC( cContent ), cContent, iif( ! HB_ISSTRING( cContent ), NIL, iif( HB_ISNULL( cContent ), NIL, cContent ) ) )
   ::aConfig[ TDC_FOOTER ]  := iif( HB_ISNUMERIC( cFooter ), cFooter, iif( ! HB_ISSTRING( cFooter ), NIL, iif( HB_ISNULL( cFooter ), NIL, cFooter ) ) )

   IF HB_ISNUMERIC( nCommonButtons )
      ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCommonButtons
   ENDIF

   IF HB_ISNUMERIC( nMainIcon )
      ::aConfig[ TDC_MAINICON ] := nMainIcon
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------*
METHOD ShowDialog() CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Shows the Task Dialog.
*
*  Parameters:
*     None
*
*  Return Value:
*     .T. if the dialog was created and displayed successfully, .F. otherwise.
*
*  Purpose:
*     This method is responsible for creating and displaying the Task Dialog using the Windows TaskDialogIndirect API.
*     It retrieves the configuration parameters from the aConfig array and passes them to the win_TaskDialogIndirect0 function.
*     It also handles error checking and sets the nButtonResult, nRadioButtonResult, and lVerifyResult properties based on the user's interaction with the dialog.
*
*  Notes:
*     Requires Windows Vista or newer.  Older operating systems will result in an error.
*     The win_TaskDialogIndirect0 function is a wrapper around the Windows TaskDialogIndirect API.
*     The os_IsWinVista_Or_Later function is used to check the operating system version.
*     The E_NOTIMPL constant represents an error code indicating that the function is not implemented.
*     The NOERROR constant represents a success code.
*     If a timeout is specified or an ONTIMER method exists, the TDF_CALLBACK_TIMER flag is set.
*
*/
METHOD ShowDialog() CLASS TTaskDialog

   LOCAL nResult
   LOCAL nButton      := NIL
   LOCAL nRadioButton := NIL
   LOCAL lVerificationFlagChecked := .F.

   IF ! ::lActive
      ::lError             := .T.
      ::nButtonResult      := NIL
      ::nRadioButtonResult := NIL
      ::nResult            := E_FAIL
      ::TimedOut           := .F.

      IF ::timeoutMS() > 0 .OR. __objHasMethod( Self, "ONTIMER" )
         ::Flags := hb_bitOr( ::Flags, TDF_CALLBACK_TIMER )
      ENDIF

      IF ::timeoutMS() > 0
         ::AllowDialogCancellation := .T.
      ENDIF

      IF os_IsWinVista_Or_Later()
         ::aConfig[ 23 ] := self
         nResult := win_TaskDialogIndirect0( ::aConfig, @nButton, @nRadioButton, @lVerificationFlagChecked )
      ELSE
         nResult := E_NOTIMPL // Not implemented yet
      ENDIF

      ::lError             := !( nResult == NOERROR )
      ::nButtonResult      := nButton
      ::nRadioButtonResult := nRadioButton
      ::lVerifyResult      := lVerificationFlagChecked
      ::nResult            := nResult
   ENDIF

RETURN ( ! ::lError )

/*-----------------------------------------------------------------------------*
METHOD DialogHandle() CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Returns the handle of the dialog.
*
*  Parameters:
*     None
*
*  Return Value:
*     The Windows handle (HWND) of the Task Dialog (Numeric). Returns NIL if the dialog is not currently showing.
*
*  Purpose:
*     This method provides access to the Windows handle (HWND) of the Task Dialog.
*     This handle can be used to interact with the Task Dialog directly using Windows API functions.
*     For example, you can use the handle to change the dialog's title, move it to a different location, or send it messages.
*
*  Notes:
*     This handle is only valid (and non NIL) while the dialog is visible.
*
*/
METHOD DialogHandle() CLASS TTaskDialog
RETURN ::HWND

/*-----------------------------------------------------------------------------*
METHOD Showing( lState ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Getter/Setter for the flag indicating whether the dialog is currently showing.
*
*  Parameters:
*     lState - A logical value indicating whether the dialog should be shown (.T.) or not (.F.).  This parameter is optional.  If omitted, the method returns the current state.
*
*  Return Value:
*     A logical value indicating whether the dialog is currently showing (.T.) or not (.F.).
*
*  Purpose:
*     This method provides a way to check whether the Task Dialog is currently being displayed and to show the dialog if it is not already visible.
*     If the lState parameter is set to .T. and the dialog is not currently active, the ShowDialog method is called to display the dialog.
*     This method can be used to ensure that the dialog is only displayed once and that it is properly managed.
*
*/
METHOD Showing( lState ) CLASS TTaskDialog

   hb_default( @lState, .F. )

   IF lState .AND. ! ::lActive
      ::ShowDialog()
   ENDIF

RETURN ::lActive

/*-----------------------------------------------------------------------------*
METHOD OnCreated( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Indicates that the Task Dialog has been created.
*
*  Parameters:
*     hWnd - The handle of the Task Dialog window (Numeric).
*     nNotify - The notification code (Numeric).  This should be TDN_CREATED.
*     nWParam - Unused parameter (Numeric).
*     nLParam - Unused parameter (Numeric).
*
*  Return Value:
*     .F. (always).
*
*  Purpose:
*     This method is called when the Task Dialog has been successfully created.
*     It sets the lActive flag to .T. to indicate that the dialog is active and stores the window handle in the HWND property.
*     This method is typically called by the Windows Task Dialog API as a callback function.
*
*/
METHOD OnCreated( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )

   IF nNotify == TDN_CREATED
      ::lActive := .T.
      ::HWND := hWnd
   ENDIF

RETURN .F.

/*-----------------------------------------------------------------------------*
METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Indicates that the Task Dialog has been destroyed.
*
*  Parameters:
*     hWnd - The handle of the Task Dialog window (Numeric).
*     nNotify - The notification code (Numeric).  This should be TDN_DESTROYED.
*     nWParam - Unused parameter (Numeric).
*     nLParam - Unused parameter (Numeric).
*
*  Return Value:
*     .F. (always).
*
*  Purpose:
*     This method is called when the Task Dialog has been destroyed.
*     It sets the lActive flag to .F. to indicate that the dialog is no longer active and sets the HWND property to NIL.
*     This method is typically called by the Windows Task Dialog API as a callback function.
*
*/
METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )

   IF nNotify == TDN_DESTROYED
      ::lActive := .F.
      ::HWND := Nil
   ENDIF

RETURN .F.

/*-----------------------------------------------------------------------------*
METHOD Listener( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     The default event listener for the Task Dialog.  It calls the user-defined callback function (if any) to handle Task Dialog events.
*
*  Parameters:
*     hWnd    - The handle of the Task Dialog window.
*     nNotify - The notification code (e.g., button click, radio button selection).
*     nWParam - Additional information specific to the notification.
*     nLParam - Additional information specific to the notification.
*
*  Return Value:
*     .T. (True) if the callback function is not defined or if the callback function returns .T., otherwise the return value of the callback function.
*
*  Purpose:
*     This method acts as a central point for handling events generated by the Task Dialog. It checks if a user-defined callback
*     function has been specified in the ::aConfig[ TDC_CALLBACK ] array. If a callback function exists, it is executed with
*     the event parameters. This allows developers to customize the behavior of the Task Dialog in response to various events,
*     such as button clicks, radio button selections, and hyperlink clicks.
*
*  Notes:
*     The callback function should be defined as an Eval item (code block) that accepts the following parameters:
*       - The TTaskDialog object itself (self).
*       - The notification code (nNotify).
*       - The WPARAM value (nWParam).
*       - The LPARAM value (nLParam).
*
*/
METHOD Listener( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( hWnd )

   IF HB_ISEVALITEM( ::aConfig[ TDC_CALLBACK ] )
      RETURN ::aConfig[ TDC_CALLBACK ]:Eval( self, nNotify, nWParam, nLParam )
   ENDIF

RETURN .T.

/*-----------------------------------------------------------------------------*
METHOD CommonButtons( nCBs ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the common buttons displayed in the Task Dialog.
*
*  Parameters:
*     nCBs - (Optional) A numeric value representing the common button flags (e.g., IDYES, IDNO, IDOK, IDCANCEL). If omitted, the current value is returned.
*
*  Return Value:
*     The previous value of the common button flags.
*
*  Purpose:
*     This method allows developers to specify which standard buttons (e.g., Yes, No, OK, Cancel) should be displayed in the Task Dialog.
*     It provides a convenient way to control the basic interaction options available to the user.
*
*  Notes:
*     The nCBs parameter should be a combination of the predefined ID constants for common buttons (e.g., IDYES + IDNO).
*     If no common buttons are specified and no custom buttons are specified, the Task Dialog will contain the OK button by default.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD CommonButtons( nCBs ) CLASS TTaskDialog

   LOCAL nOldCBS := ::aConfig[ TDC_COMMON_BUTTON_FLAGS ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nCBs )
         ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCBs
      ENDIF
   ENDIF

RETURN nOldCBS

/*-----------------------------------------------------------------------------*
METHOD WindowTitle( cTitle ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the title of the Task Dialog window.
*
*  Parameters:
*     cTitle - (Optional) A string or numeric value representing the new title for the Task Dialog. If omitted, the current title is returned.
*
*  Return Value:
*     The previous title of the Task Dialog window.
*
*  Purpose:
*     This method allows developers to customize the title bar text of the Task Dialog.  A clear and informative title helps the user
*     understand the purpose of the dialog and the context of the information being presented.
*
*  Notes:
*     If the Task Dialog is currently active (::lActive is .T.), the window title is updated immediately using the _SetWindowTitle function.
*     If cTitle is NIL, it is treated as an empty string.
*
*/
METHOD WindowTitle( cTitle ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_WINDOWTITLE ]

   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      ::aConfig[ TDC_WINDOWTITLE ] := iif( HB_ISSTRING( cTitle ) .AND. HB_ISNULL( cTitle ), NIL, cTitle )
      IF ::lActive
         _SetWindowTitle( ::HWND, ::aConfig[ TDC_WINDOWTITLE ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD Title( cTitle ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     An alias for the WindowTitle method. Sets or retrieves the title of the Task Dialog window.
*
*  Parameters:
*     cTitle - (Optional) A string or numeric value representing the new title for the Task Dialog. If omitted, the current title is returned.
*
*  Return Value:
*     The previous title of the Task Dialog window.
*
*  Purpose:
*     Provides a more concise and readable way to set or retrieve the window title.  This is purely for convenience and code readability.
*
*/
METHOD Title( cTitle ) CLASS TTaskDialog
RETURN ::WindowTitle( cTitle )

/*-----------------------------------------------------------------------------*
METHOD MainIcon( nIcon ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the main icon displayed in the Task Dialog.
*
*  Parameters:
*     nIcon - (Optional) A numeric value representing the icon resource ID. If omitted, the current icon is returned.
*
*  Return Value:
*     The previous icon resource ID.
*
*  Purpose:
*     This method allows developers to customize the main icon displayed in the Task Dialog.  Using appropriate icons can visually
*     communicate the type of message being presented (e.g., information, warning, error).
*
*  Notes:
*     The nIcon parameter should be a valid icon resource ID.
*     If the Task Dialog is currently active (::lActive is .T.), the icon is updated immediately using the _UpdateMainIcon function.
*
*/
METHOD MainIcon( nIcon ) CLASS TTaskDialog

   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_MAINICON ] := nIcon
      IF ::lActive
         _UpdateMainIcon( ::HWND, ::aConfig[ TDC_MAINICON ] )
      ENDIF
   ENDIF

RETURN ::aConfig[ TDC_MAINICON ]

/*-----------------------------------------------------------------------------*
METHOD MainInstruction( cInstruction ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the main instruction text displayed in the Task Dialog.
*
*  Parameters:
*     cInstruction - (Optional) A string or numeric value representing the main instruction text. If omitted, the current instruction text is returned.
*
*  Return Value:
*     The previous main instruction text.
*
*  Purpose:
*     This method allows developers to set the main instruction text, which is typically displayed prominently at the top of the Task Dialog.
*     The main instruction should provide a concise and clear statement of the user's task or the purpose of the dialog.
*
*  Notes:
*     If the Task Dialog is currently active (::lActive is .T.), the instruction text is updated immediately using the _SetMainInstruction function.
*     If cInstruction is NIL, it is treated as an empty string.
*
*/
METHOD MainInstruction( cInstruction ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_MAININSTRUCTION ]

   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      ::aConfig[ TDC_MAININSTRUCTION ] := iif( HB_ISSTRING( cInstruction ) .AND. HB_ISNULL( cInstruction ), NIL, cInstruction )
      IF ::lActive
         _SetMainInstruction( ::HWND, ::aConfig[ TDC_MAININSTRUCTION ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD Instruction( cInstruction ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     An alias for the MainInstruction method. Sets or retrieves the main instruction text displayed in the Task Dialog.
*
*  Parameters:
*     cInstruction - (Optional) A string or numeric value representing the main instruction text. If omitted, the current instruction text is returned.
*
*  Return Value:
*     The previous main instruction text.
*
*  Purpose:
*     Provides a more concise and readable way to set or retrieve the main instruction. This is purely for convenience and code readability.
*
*/
METHOD Instruction( cInstruction ) CLASS TTaskDialog
RETURN ::MainInstruction( cInstruction )

/*-----------------------------------------------------------------------------*
METHOD Content( cContent ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the main content text displayed in the Task Dialog.
*
*  Parameters:
*     cContent - (Optional) A string or numeric value representing the main content text. If omitted, the current content text is returned.
*
*  Return Value:
*     The previous main content text.
*
*  Purpose:
*     This method allows developers to set the main content text, which provides detailed information or instructions to the user.
*     The content should be clear, concise, and relevant to the main instruction.
*
*  Notes:
*     If the Task Dialog is currently active (::lActive is .T.), the content text is updated immediately using the _SetContent function.
*     If cContent is NIL, it is treated as an empty string.
*
*/
METHOD Content( cContent ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_CONTENT ]

   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      ::aConfig[ TDC_CONTENT ] := iif( HB_ISSTRING( cContent ) .AND. HB_ISNULL( cContent ), NIL, cContent )
      IF ::lActive
         _SetContent( ::HWND, ::aConfig[ TDC_CONTENT ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD CustomButtons( aCustButton ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the array of custom buttons displayed in the Task Dialog.
*
*  Parameters:
*     aCustButton - (Optional) An array of custom button definitions. Each element in the array should be an array containing the button ID and the button text. If omitted, the current array of custom buttons is returned.
*
*  Return Value:
*     The previous array of custom button definitions.
*
*  Purpose:
*     This method allows developers to define custom buttons with specific IDs and labels, providing more flexible interaction options
*     than the standard common buttons.
*
*  Notes:
*     The aCustButton parameter should be an array where each element is an array of the form { nButtonID, cButtonText }.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD CustomButtons( aCustButton ) CLASS TTaskDialog

   LOCAL aOldVal := ::aConfig[ TDC_TASKDIALOG_BUTTON ]

   IF ! ::lActive
      IF HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
         ::aConfig[ TDC_BUTTON ] := Len( aCustButton )
         ::aConfig[ TDC_TASKDIALOG_BUTTON ] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*-----------------------------------------------------------------------------*
METHOD DefaultButton( nDefaultButton ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the ID of the default button in the Task Dialog.
*
*  Parameters:
*     nDefaultButton - (Optional) A numeric value representing the ID of the default button. If omitted, the current default button ID is returned.
*
*  Return Value:
*     The previous default button ID.
*
*  Purpose:
*     This method allows developers to specify which button should be the default (i.e., the button that is activated when the user presses Enter).
*     Setting a default button improves usability by providing a clear and predictable action for the user.
*
*  Notes:
*     The nDefaultButton parameter should be the ID of one of the buttons in the Task Dialog (either a common button or a custom button).
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD DefaultButton( nDefaultButton ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTBUTTON ] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD CustomRadioButtons( aCustButton ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the array of custom radio buttons displayed in the Task Dialog.
*
*  Parameters:
*     aCustButton - (Optional) An array of custom radio button definitions. Each element in the array should be an array containing the button ID and the button text. If omitted, the current array of custom radio buttons is returned.
*
*  Return Value:
*     The previous array of custom radio button definitions.
*
*  Purpose:
*     This method allows developers to define custom radio buttons with specific IDs and labels, providing a way for the user to select one option from a set of mutually exclusive choices.
*
*  Notes:
*     The aCustButton parameter should be an array where each element is an array of the form { nButtonID, cButtonText }.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD CustomRadioButtons( aCustButton ) CLASS TTaskDialog

   LOCAL aOldVal := ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ]

   IF ! ::lActive
      IF HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
         ::aConfig[ TDC_RADIOBUTTON ] := Len( aCustButton )
         ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ] := aCustButton
      ENDIF
   ENDIF

RETURN aOldVal

/*-----------------------------------------------------------------------------*
METHOD DefaultRadioButton( nDefaultButton ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the ID of the radio button that is selected by default in the Task Dialog.
*
*  Parameters:
*     nDefaultButton - (Optional) A numeric value representing the ID of the default radio button. If omitted, the current default radio button ID is returned.
*
*  Return Value:
*     The previous default radio button ID.
*
*  Purpose:
*     This method allows developers to specify which radio button should be selected by default when the Task Dialog is displayed.
*     Setting a default radio button provides a convenient starting point for the user and can guide them towards the most common or recommended option.
*
*  Notes:
*     The nDefaultButton parameter should be the ID of one of the radio buttons in the Task Dialog.
*     If this value does not correspond to a button ID, the first button in the array is selected by default.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD DefaultRadioButton( nDefaultButton ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTRADIOBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTRADIOBUTTON ] := nDefaultButton
      ENDIF
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD VerificationText( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the text displayed next to the verification checkbox in the Task Dialog.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the verification text. If omitted, the current verification text is returned.
*
*  Return Value:
*     The previous verification text.
*
*  Purpose:
*     This method allows developers to customize the text displayed next to the verification checkbox. The verification checkbox is typically used
*     to allow the user to confirm that they understand the implications of their actions or that they want to perform a specific action in addition to the main task.
*
*  Notes:
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD VerificationText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_VERIFICATIONTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_VERIFICATIONTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD ExpandedInfo( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the expanded information text displayed in the Task Dialog.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the expanded information text. If omitted, the current expanded information text is returned.
*
*  Return Value:
*     The previous expanded information text.
*
*  Purpose:
*     This method allows developers to provide additional, less critical information that is initially hidden but can be revealed by the user
*     by clicking an expand/collapse button. This is useful for presenting detailed explanations, technical details, or optional instructions
*     without cluttering the main dialog content.
*
*  Notes:
*     If the Task Dialog is currently active (::lActive is .T.), the expanded information text is updated immediately using the _SetExpandedInformation function.
*     The expanded information is displayed either immediately below the content or below the footer text depending on whether the ExpandFooterArea flag is true.
*     If the EnableHyperlinks flag is true, then this string may contain hyperlinks in the form: <A HREF="executablestring">Hyperlink Text</A>.
*     Enabling hyperlinks when using content from an unsafe source may cause security vulnerabilities.
*
*/
METHOD ExpandedInfo( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_EXPANDEDINFORMATION ]

   IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
      ::aConfig[ TDC_EXPANDEDINFORMATION ] := cText
      IF ::lActive
         _SetExpandedInformation( ::HWND, ::aConfig[ TDC_EXPANDEDINFORMATION ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD ExpandedControlText( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the text used to label the button for collapsing the expandable information.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the collapse button text. If omitted, the current collapse button text is returned.
*
*  Return Value:
*     The previous collapse button text.
*
*  Purpose:
*     This method allows developers to customize the text displayed on the button that collapses the expanded information area.
*     A clear and descriptive label helps the user understand the purpose of the button.
*
*  Notes:
*     This member is ignored when the ExpandedInformation member is empty.
*     If this member is empty and the CollapsedControlText is specified, then the CollapsedControlText value will be used for this member as well.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD ExpandedControlText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_EXPANDEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_EXPANDEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD ExpandedCtrlText( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     An alias for the ExpandedControlText method. Sets or retrieves the text used to label the button for collapsing the expandable information.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the collapse button text. If omitted, the current collapse button text is returned.
*
*  Return Value:
*     The previous collapse button text.
*
*  Purpose:
*     Provides a more concise and readable way to set or retrieve the collapse button text. This is purely for convenience and code readability.
*
*/
METHOD ExpandedCtrlText( cText ) CLASS TTaskDialog
RETURN ::ExpandedControlText( cText )

/*-----------------------------------------------------------------------------*
METHOD CollapsedControlText( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the text used to label the button for expanding the expandable information.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the expand button text. If omitted, the current expand button text is returned.
*
*  Return Value:
*     The previous expand button text.
*
*  Purpose:
*     This method allows developers to customize the text displayed on the button that expands the hidden information area.
*     A clear and descriptive label helps the user understand the purpose of the button.
*
*  Notes:
*     This member is ignored when the ExpandedInformation member is empty.
*     If this member is empty and the ExpandedControlText is specified, then the ExpandedControlText value will be used for this member as well.
*     This method only modifies the button configuration if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD CollapsedControlText( cText ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD CollapsedCtrlText( cText ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     An alias for the CollapsedControlText method. Sets or retrieves the text used to label the button for expanding the expandable information.
*
*  Parameters:
*     cText - (Optional) A string or numeric value representing the expand button text. If omitted, the current expand button text is returned.
*
*  Return Value:
*     The previous expand button text.
*
*  Purpose:
*     Provides a more concise and readable way to set or retrieve the expand button text. This is purely for convenience and code readability.
*
*/
METHOD CollapsedCtrlText( cText ) CLASS TTaskDialog
RETURN ::CollapsedControlText( cText )

/*-----------------------------------------------------------------------------*
METHOD FooterIcon( nIcon ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the footer icon displayed in the Task Dialog.
*
*  Parameters:
*     nIcon - (Optional) A numeric value representing the icon resource ID. If omitted, the current footer icon is returned.
*
*  Return Value:
*     The previous footer icon resource ID.
*
*  Purpose:
*     This method allows developers to customize the icon displayed in the footer area of the Task Dialog.  Using appropriate icons can visually
*     communicate the type of message being presented (e.g., information, warning, error).  The footer icon is often used to indicate the
*     severity or importance of the footer text.
*
*  Notes:
*     The nIcon parameter should be a valid icon resource ID.
*     If the Task Dialog is currently active (::lActive is .T.), the icon is updated immediately using the _UpdateFooterIcon function.
*
*/
METHOD FooterIcon( nIcon ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_FOOTERICON ]

   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_FOOTERICON ] := nIcon
      IF ::lActive
         _UpdateFooterIcon( ::HWND, ::aConfig[ TDC_FOOTERICON ] )
      ENDIF
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD Footer( cFooter ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the footer text displayed in the Task Dialog.
*
*  Parameters:
*     cFooter - (Optional) A string or numeric value representing the footer text. If omitted, the current footer text is returned.
*
*  Return Value:
*     The previous footer text.
*
*  Purpose:
*     This method allows developers to set the footer text, which is typically displayed at the bottom of the Task Dialog.
*     The footer can be used to provide additional information, legal disclaimers, or copyright notices.
*
*  Notes:
*     If the Task Dialog is currently active (::lActive is .T.), the footer text is updated immediately using the _SetFooter function.
*     If EnableHyperlinks is true, this can show clickable links.
*
*/
METHOD Footer( cFooter ) CLASS TTaskDialog

   LOCAL cOldVal := ::aConfig[ TDC_FOOTER ]

   IF HB_ISSTRING( cFooter ) .OR. HB_ISNUMERIC( cFooter )
      ::aConfig[ TDC_FOOTER ] := cFooter
      IF ::lActive
         _SetFooter( ::HWND, ::aConfig[ TDC_FOOTER ] )
      ENDIF
   ENDIF

RETURN cOldVal

/*-----------------------------------------------------------------------------*
METHOD Width( nWidth ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the width of the Task Dialog's client area, in dialog units.
*
*  Parameters:
*     nWidth - (Optional) A numeric value representing the width in dialog units. If omitted, the current width is returned.
*
*  Return Value:
*     The previous width of the Task Dialog's client area.
*
*  Purpose:
*     This method allows developers to control the width of the Task Dialog.  Setting an appropriate width ensures that the content is displayed
*     correctly and that the dialog fits well within the user's screen.
*
*  Notes:
*     If nWidth is 0, the Task Dialog manager will calculate the ideal width.
*     This method only modifies the width if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD Width( nWidth ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_WIDTH ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nWidth )
      ::aConfig[ TDC_WIDTH ] := nWidth
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD ParentHandle( nHandle ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the handle of the parent window for the Task Dialog.
*
*  Parameters:
*     nHandle - (Optional) A numeric value representing the window handle of the parent window. If omitted, the current parent window handle is returned.
*
*  Return Value:
*     The previous parent window handle.
*
*  Purpose:
*     This method allows developers to specify the parent window for the Task Dialog.  Setting a parent window ensures that the Task Dialog is displayed
*     on top of the parent window and that it is properly associated with the parent window's application.
*
*  Notes:
*     The nHandle parameter should be a valid window handle.
*     This method only modifies the parent window handle if the Task Dialog is not currently active (::lActive is .F.) and the provided handle is a valid window handle.
*
*/
METHOD ParentHandle( nHandle ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_HWND ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nHandle ) .AND. IsWindowHandle( nHandle )
      ::aConfig[ TDC_HWND ] := nHandle
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD Parent( cFormName ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the name of the parent form for the Task Dialog.
*
*  Parameters:
*     cFormName - (Optional) A string representing the name of the parent form. If omitted, the current parent form name is returned.
*
*  Return Value:
*     The previous parent form name.
*
*  Purpose:
*     This method allows developers to specify the parent form for the Task Dialog by its name.  It retrieves the window handle of the form
*     using GetFormHandle and then sets the parent window handle using ::ParentHandle.  This provides a more convenient way to specify
*     the parent window than directly using the window handle.
*
*  Notes:
*     This method relies on the _HMG_aFormNames and _HMG_aFormHandles arrays, which are part of the HMG Extended framework.
*
*/
METHOD Parent( cFormName ) CLASS TTaskDialog
RETURN _HMG_aFormNames[ AScan ( _HMG_aFormHandles, ::ParentHandle( GetFormHandle( cFormName ) ) ) ]

/*-----------------------------------------------------------------------------*
METHOD CallBackBlock( bCode ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the callback code block for the Task Dialog.
*
*  Parameters:
*     bCode - (Optional) A code block to be executed when Task Dialog events occur. If omitted, the current callback code block is returned.
*
*  Return Value:
*     The previous callback code block.
*
*  Purpose:
*     This method allows developers to define a custom code block that will be executed in response to various events generated by the Task Dialog.
*     This provides a flexible way to customize the behavior of the Task Dialog and handle user interactions.
*
*  Notes:
*     The callback code block should accept the following parameters:
*       - The TTaskDialog object itself (self).
*       - The notification code (nNotify).
*       - The WPARAM value (nWParam).
*       - The LPARAM value (nLParam).
*     This method only modifies the callback code block if the Task Dialog is not currently active (::lActive is .F.).
*     This method will be deleted in a future version.
*
*/
METHOD CallBackBlock( bCode ) CLASS TTaskDialog

   IF ! ::lActive
      IF HB_ISEVALITEM( bCode )
         ::aConfig[ TDC_CALLBACK ] := bCode
      ENDIF
   ENDIF

RETURN ::aConfig[ TDC_CALLBACK ]

/*-----------------------------------------------------------------------------*
METHOD Flags( nFlags ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the raw flags for the Task Dialog.  This method provides direct access to the underlying Task Dialog flags.
*
*  Parameters:
*     nFlags - (Optional) A numeric value representing the Task Dialog flags.  If provided, the flags are set.
*              If omitted, the current flags are returned.
*
*  Return Value:
*     The previous value of the Task Dialog flags (numeric).
*
*  Purpose:
*     This method allows developers to directly manipulate the Task Dialog flags, providing fine-grained control over the dialog's behavior.
*     It's primarily used for advanced scenarios where the individual flag properties don't offer sufficient control.
*
*  Notes:
*     Directly manipulating the flags can be complex and requires a thorough understanding of the Task Dialog API.
*     It's generally recommended to use the individual property methods (e.g., AllowDialogCancellation, CanBeMinimized) for setting flags.
*     This method only sets the flags if the Task Dialog is not currently active (::lActive is .F.).
*
*/
METHOD Flags( nFlags ) CLASS TTaskDialog

   LOCAL nOldVal := ::aConfig[ TDC_TASKDIALOG_FLAGS ]
   IF ! ::lActive
      IF HB_ISNUMERIC( nFlags )
         ::aConfig[ TDC_TASKDIALOG_FLAGS ] := nFlags
      ENDIF
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD AllowDialogCancellation( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the Task Dialog can be cancelled using Alt-F4, Escape, or the title bar's close button.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to allow cancellation.  .T. enables cancellation, .F. disables it.
*               If omitted, the current cancellation state is returned.
*
*  Return Value:
*     The previous cancellation state (logical).  .T. if cancellation was previously allowed, .F. otherwise.
*
*  Purpose:
*     This method provides a convenient way to control whether the user can close the Task Dialog without explicitly clicking a button.
*     It's useful for scenarios where you want to prevent the user from prematurely dismissing the dialog.
*
*  Notes:
*     This method only modifies the cancellation state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_ALLOW_DIALOG_CANCELLATION flag within the overall Task Dialog flags.
*
*/
METHOD AllowDialogCancellation( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_ALLOW_DIALOG_CANCELLATION )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_ALLOW_DIALOG_CANCELLATION ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD CanBeMinimized( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the Task Dialog can be minimized.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether the dialog can be minimized. .T. enables minimization, .F. disables it.
*               If omitted, the current minimization state is returned.
*
*  Return Value:
*     The previous minimization state (logical). .T. if minimization was previously allowed, .F. otherwise.
*
*  Purpose:
*     This method allows developers to control whether the Task Dialog can be minimized, providing flexibility in managing the user's workflow.
*     It's useful for scenarios where you want to allow the user to temporarily hide the dialog without closing it.
*
*  Notes:
*     This method only modifies the minimization state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_CAN_BE_MINIMIZED flag within the overall Task Dialog flags.
*
*/
METHOD CanBeMinimized( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_CAN_BE_MINIMIZED ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_CAN_BE_MINIMIZED )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_CAN_BE_MINIMIZED ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD EnableHyperlinks( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether hyperlinks are enabled in the Task Dialog's content, expanded information, and footer.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to enable hyperlinks. .T. enables hyperlinks, .F. disables them.
*               If omitted, the current hyperlink state is returned.
*
*  Return Value:
*     The previous hyperlink state (logical). .T. if hyperlinks were previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to include clickable hyperlinks within the Task Dialog's text.
*     It's useful for providing users with quick access to related information or external resources.
*
*  Notes:
*     This method only modifies the hyperlink state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_ENABLE_HYPERLINKS flag within the overall Task Dialog flags.
*     Hyperlink execution must be handled in the OnHyperlinkClicked event.  The Task Dialog itself does not execute the hyperlinks.
*     Enabling hyperlinks with content from untrusted sources can pose security risks.
*
*/
METHOD EnableHyperlinks( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_ENABLE_HYPERLINKS ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_ENABLE_HYPERLINKS )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_ENABLE_HYPERLINKS ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD ExpandedByDefault( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the expanded information section of the Task Dialog is displayed by default.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to expand the information by default. .T. expands by default, .F. does not.
*               If omitted, the current expanded-by-default state is returned.
*
*  Return Value:
*     The previous expanded-by-default state (logical). .T. if expanded by default was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to control whether the expanded information section is initially visible to the user.
*     It's useful for providing additional details that are not essential but may be helpful to some users.
*
*  Notes:
*     This method only modifies the expanded-by-default state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_EXPANDED_BY_DEFAULT flag within the overall Task Dialog flags.
*     This flag is ignored if the ExpandedInformation member is empty.
*
*/
METHOD ExpandedByDefault( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_EXPANDED_BY_DEFAULT ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_EXPANDED_BY_DEFAULT )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_EXPANDED_BY_DEFAULT ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD ExpandFooterArea( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the expanded information is displayed in the footer area of the Task Dialog.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to expand the footer area. .T. expands the footer area, .F. does not.
*               If omitted, the current expand-footer-area state is returned.
*
*  Return Value:
*     The previous expand-footer-area state (logical). .T. if expanding the footer area was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to control where the expanded information is displayed within the Task Dialog.
*     It's useful for organizing the dialog's content and providing a consistent user experience.
*
*  Notes:
*     This method only modifies the expand-footer-area state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_EXPAND_FOOTER_AREA flag within the overall Task Dialog flags.
*     This flag is ignored if the ExpandedInformation member is empty.
*
*/
METHOD ExpandFooterArea( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_EXPAND_FOOTER_AREA ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_EXPAND_FOOTER_AREA )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_EXPAND_FOOTER_AREA ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD NoDefaultRadioButton( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether no radio button is selected by default in the Task Dialog.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to have no default radio button. .T. disables the default selection, .F. enables it.
*               If omitted, the current no-default-radio-button state is returned.
*
*  Return Value:
*     The previous no-default-radio-button state (logical). .T. if no default radio button was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to force the user to explicitly select a radio button before proceeding.
*     It's useful for scenarios where a default selection might lead to unintended consequences.
*
*  Notes:
*     This method only modifies the no-default-radio-button state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_NO_DEFAULT_RADIO_BUTTON flag within the overall Task Dialog flags.
*
*/
METHOD NoDefaultRadioButton( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_NO_DEFAULT_RADIO_BUTTON )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_NO_DEFAULT_RADIO_BUTTON ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD PositionRelativeToWindow( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the Task Dialog is positioned relative to its parent window.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to position relative to the parent window. .T. positions relative to the parent, .F. positions relative to the monitor.
*               If omitted, the current position-relative-to-window state is returned.
*
*  Return Value:
*     The previous position-relative-to-window state (logical). .T. if positioning relative to the window was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to control the positioning of the Task Dialog on the screen.
*     It's useful for ensuring that the dialog is displayed in a contextually relevant location.
*
*  Notes:
*     This method only modifies the position-relative-to-window state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_POSITION_RELATIVE_TO_WINDOW flag within the overall Task Dialog flags.
*     If this flag is not set (or no parent window is specified), the dialog is centered on the monitor.
*
*/
METHOD PositionRelativeToWindow( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_POSITION_RELATIVE_TO_WINDOW )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_POSITION_RELATIVE_TO_WINDOW ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD RightToLeftLayout( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the Task Dialog's layout is right-to-left.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether to use right-to-left layout. .T. enables right-to-left, .F. uses left-to-right.
*               If omitted, the current right-to-left-layout state is returned.
*
*  Return Value:
*     The previous right-to-left-layout state (logical). .T. if right-to-left layout was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to support right-to-left languages in their applications.
*     It's useful for ensuring that the Task Dialog is displayed correctly in cultures that use right-to-left text direction.
*
*  Notes:
*     This method only modifies the right-to-left-layout state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_RTL_LAYOUT flag within the overall Task Dialog flags.
*
*/
METHOD RightToLeftLayout( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_RTL_LAYOUT ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_RTL_LAYOUT )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_RTL_LAYOUT ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD VerificationEnabled( lNewVal ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the enable state of the verification checkbox in the Task Dialog.
*
*  Parameters:
*     lNewVal - (Optional) A logical value indicating whether the verification checkbox is enabled. .T. enables the checkbox, .F. disables it.
*               If omitted, the current verification-enabled state is returned.
*
*  Return Value:
*     The previous verification-enabled state (logical). .T. if the verification checkbox was previously enabled, .F. otherwise.
*
*  Purpose:
*     This method allows developers to include a verification checkbox in the Task Dialog, allowing the user to confirm an action or setting.
*     It's useful for scenarios where you want to provide an extra layer of confirmation before proceeding.
*
*  Notes:
*     This method only modifies the verification-enabled state if the Task Dialog is not currently active (::lActive is .F.).
*     It uses bitwise operations to manipulate the TDF_VERIFICATION_FLAG_CHECKED flag within the overall Task Dialog flags.
*
*/
METHOD VerificationEnabled( lNewVal ) CLASS TTaskDialog

   LOCAL nCurFlags := ::Flags(), lOldVal
   LOCAL nNewFlags

   hb_default( @nCurFlags, 0 )
   lOldVal := ( hb_bitAnd( nCurFlags, TDF_VERIFICATION_FLAG_CHECKED ) != 0 )

   IF ! ::lActive .AND. HB_ISLOGICAL( lNewVal )
      IF ( ( ! lOldVal ) .AND. lNewVal )
         nNewFlags := hb_bitOr( nCurFlags, TDF_VERIFICATION_FLAG_CHECKED )
      ELSEIF ( lOldVal .AND. ( ! lNewVal ) )
         nNewFlags := hb_bitAnd( nCurFlags, hb_bitNot( TDF_VERIFICATION_FLAG_CHECKED ) )
      ENDIF
      ::Flags( nNewFlags )
   ENDIF

RETURN lOldVal

/*-----------------------------------------------------------------------------*
METHOD timeoutMS ( nMS ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the timeout value for the Task Dialog, in milliseconds.
*
*  Parameters:
*     nMS - (Optional) A numeric value representing the timeout in milliseconds. If provided, the timeout is set.
*           If omitted, the current timeout value is returned.
*
*  Return Value:
*     The previous timeout value in milliseconds (numeric).
*
*  Purpose:
*     This method allows developers to automatically close the Task Dialog after a specified period of time.
*     It's useful for scenarios where you want to display a temporary message or notification without requiring user interaction.
*
*  Notes:
*     This method only modifies the timeout value if the Task Dialog is not currently active (::lActive is .F.).
*     The Task Dialog will close automatically after the specified timeout, regardless of user interaction.
*
*/
METHOD timeoutMS ( nMS ) CLASS TTaskDialog

   LOCAL nOldVal := ::nTimeOutMS

   IF ! ::lActive .AND. HB_ISNUMERIC( nMS )
      ::nTimeOutMS := nMS
   ENDIF

RETURN nOldVal

/*-----------------------------------------------------------------------------*
METHOD TimedOut( lOut ) CLASS TTaskDialog
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves whether the Task Dialog has timed out.
*
*  Parameters:
*     lOut - (Optional) A logical value indicating whether the dialog has timed out. If provided, the timeout state is set.
*           If omitted, the current timeout state is returned.
*
*  Return Value:
*     The current timeout state (logical). .T. if the dialog has timed out, .F. otherwise.
*
*  Purpose:
*     This method allows developers to determine whether the Task Dialog was closed due to a timeout.
*     It's useful for handling different scenarios based on whether the user interacted with the dialog or it closed automatically.
*
*  Notes:
*     This method can only set the timeout state if the Task Dialog is currently active (::lActive is .T.).
*     The intention is that this method will be read-only in the future.
*
*/
METHOD TimedOut( lOut ) CLASS TTaskDialog

   IF ::lActive .AND. HB_ISLOGICAL( lOut )
      ::lTimeOut := lOut
   ENDIF

RETURN ::lTimeOut

#endif /* __XHARBOUR__ */
