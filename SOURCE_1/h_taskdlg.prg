/*
 * HMG Extended - Task Dialog Classes
 *
 * Object-oriented wrappers around the Windows Task Dialog API.
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 */

#if ! defined( __XHARBOUR__ ) .AND. ( __HARBOUR__ - 0 > 0x030000 )

#include "hbclass.ch"
#include "TaskDlgs.ch"
#include "i_var.ch"

/*
 * TDC_CALLBACK_INSTANCE defines the specific index within the TASKDIALOGCONFIG
 * structure array where the Harbour object reference (Self) is stored.
 * This allows the C-level callback function to route Windows messages
 * back to the correct class instance.
 */
#define TDC_CALLBACK_INSTANCE 23

/*
 * CLASS: TSimpleTaskDialog
 * Purpose: Provides a simplified interface for the Windows Task Dialog API.
 * This class is intended for basic notifications with standard icons and buttons.
 */
CREATE CLASS TSimpleTaskDialog FUNCTION SimpleTaskDialog
   EXPORTED:
   VAR Cargo                           // User-defined data storage.
   VAR lError READONLY INIT .T.        // Indicates if the last execution failed.
   VAR nButtonResult READONLY INIT NIL // Stores the ID of the button clicked by the user.
   VAR nResult READONLY INIT E_FAIL    // Stores the HRESULT from the Windows API call.

   METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon )
   METHOD Execute()

   // Property accessors using SETGET for fluent configuration
   METHOD Title( cTitle ) SETGET
   METHOD Instruction( cInstruction ) SETGET
   METHOD Content( cContent ) SETGET
   METHOD CommonButtons( nCBs ) SETGET
   METHOD MainIcon( nIcon ) SETGET

   PROTECTED:
   VAR cTitle INIT NIL                     // Window title text.
   VAR cInstruction INIT NIL               // Main instruction (large blue text).
   VAR cContent INIT NIL                   // Body content text.
   VAR nCommonButtons INIT TDCBF_OK_BUTTON // Bitmask of standard buttons (OK, Cancel, etc.).
   VAR nMainIcon INIT TD_NO_ICON           // Resource ID or constant for the main icon.

   METHOD validateText( vVal )
ENDCLASS

/*
 * METHOD: New
 * Purpose: Constructor for TSimpleTaskDialog.
 * Parameters:
 *   - cTitle: String/Numeric title of the dialog window.
 *   - cInstruction: Main heading text.
 *   - cContent: Detailed body text.
 *   - nCommonButtons: Numeric bitmask (e.g., TDCBF_YES_BUTTON | TDCBF_NO_BUTTON).
 *   - nMainIcon: Numeric constant for the system icon (e.g., TD_INFORMATION_ICON).
 */
METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog
   ::cTitle := ::validateText( cTitle )
   ::cInstruction := ::validateText( cInstruction )
   ::cContent := ::validateText( cContent )

   // Only update if valid numeric types are provided to prevent runtime errors
   IF HB_ISNUMERIC( nCommonButtons )
      ::nCommonButtons := nCommonButtons
   ENDIF
   IF HB_ISNUMERIC( nMainIcon )
      ::nMainIcon := nMainIcon
   ENDIF
RETURN Self

// Internal helper to ensure text parameters are valid for the API
METHOD validateText( vVal ) CLASS TSimpleTaskDialog
RETURN ValidateText( vVal )

/*
 * METHOD: Execute
 * Purpose: Displays the dialog to the user.
 * Logic: Checks for Windows Vista+ compatibility as Task Dialogs are not available on XP.
 * Returns: Logical .T. if the dialog was displayed and closed successfully, .F. otherwise.
 */
METHOD Execute() CLASS TSimpleTaskDialog
   LOCAL nResult, nButton := NIL
   ::lError := .T.
   ::nButtonResult := NIL
   ::nResult := E_FAIL

   // Task Dialog API requires Windows Vista or later.
   IF os_IsWinVista_Or_Later()
      // win_TaskDialog0 is a low-level wrapper for the TaskDialog function.
      nResult := win_TaskDialog0( ,, ::cTitle, ::cInstruction, ::cContent, ::nCommonButtons, ::nMainIcon, @nButton )
   ELSE
      nResult := E_NOTIMPL
   ENDIF

   ::lError := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nResult := nResult
RETURN ( ! ::lError )

// Setter/Getter for the Window Title
METHOD Title( cTitle ) CLASS TSimpleTaskDialog
   LOCAL cOld := ::cTitle
   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      ::cTitle := ::validateText( cTitle )
   ENDIF
RETURN cOld

// Setter/Getter for the Main Instruction (Heading)
METHOD Instruction( cInstruction ) CLASS TSimpleTaskDialog
   LOCAL cOld := ::cInstruction
   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      ::cInstruction := ::validateText( cInstruction )
   ENDIF
RETURN cOld

// Setter/Getter for the Content (Body text)
METHOD Content( cContent ) CLASS TSimpleTaskDialog
   LOCAL cOld := ::cContent
   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      ::cContent := ::validateText( cContent )
   ENDIF
RETURN cOld

// Setter/Getter for Common Button flags
METHOD CommonButtons( nCBs ) CLASS TSimpleTaskDialog
   LOCAL nOld := ::nCommonButtons
   IF HB_ISNUMERIC( nCBs )
      ::nCommonButtons := nCBs
   ENDIF
RETURN nOld

// Setter/Getter for the Main Icon constant
METHOD MainIcon( nIcon ) CLASS TSimpleTaskDialog
   LOCAL nOld := ::nMainIcon
   IF HB_ISNUMERIC( nIcon )
      ::nMainIcon := nIcon
   ENDIF
RETURN nOld

/*
 * CLASS: TTaskDialog
 * Purpose: Advanced wrapper for TaskDialogIndirect, supporting custom buttons,
 * radio buttons, footers, verification checkboxes, and progress bars.
 */
CREATE CLASS TTaskDialog FUNCTION TaskDialog
   EXPORTED:
   VAR Cargo
   VAR lActive READONLY INIT .F.            // True if the dialog is currently visible.
   VAR lError READONLY INIT .T.
   VAR nButtonResult READONLY INIT NIL      // ID of the clicked button.
   VAR nRadioButtonResult READONLY INIT NIL // ID of the selected radio button.
   VAR nResult READONLY INIT E_FAIL
   VAR lVerifyResult READONLY INIT .F.      // State of the verification checkbox.

   METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon )
   METHOD Execute() INLINE ::ShowDialog()
   METHOD ShowDialog()
   METHOD DialogHandle()
   METHOD Showing( lState )

   // Event Handlers (Callbacks)
   METHOD OnCreated( hWnd, nNotify, nWParam, nLParam )
   METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam )
   METHOD Listener( hWnd, nNotify, nWParam, nLParam )

   // Configuration Methods
   METHOD CommonButtons( nCBs ) SETGET
   METHOD WindowTitle( cTitle ) SETGET
   METHOD Title( cTitle ) SETGET
   METHOD MainIcon( nIcon ) SETGET
   METHOD MainInstruction( cInstruction ) SETGET
   METHOD Instruction( cInstruction ) SETGET
   METHOD Content( cContent ) SETGET
   METHOD CustomButtons( aCustButton ) SETGET
   METHOD DefaultButton( nDefaultButton ) SETGET
   METHOD CustomRadioButtons( aCustButton ) SETGET
   METHOD DefaultRadioButton( nDefaultButton ) SETGET
   METHOD VerificationText( cText ) SETGET
   METHOD ExpandedInfo( cText ) SETGET
   METHOD ExpandedControlText( cText ) SETGET
   METHOD ExpandedCtrlText( cText ) SETGET
   METHOD CollapsedControlText( cText ) SETGET
   METHOD CollapsedCtrlText( cText ) SETGET
   METHOD FooterIcon( nIcon ) SETGET
   METHOD Footer( cFooter ) SETGET
   METHOD Width( nWidth ) SETGET
   METHOD ParentHandle( nHandle ) SETGET
   METHOD CallBackBlock( bCode ) SETGET
   METHOD Flags( nFlags ) SETGET

   // Flag Helpers (Boolean toggles for TDF_* flags)
   METHOD AllowDialogCancellation( lNewVal ) SETGET
   METHOD CanBeMinimized( lNewVal ) SETGET
   METHOD EnableHyperlinks( lNewVal ) SETGET
   METHOD ExpandedByDefault( lNewVal ) SETGET
   METHOD ExpandFooterArea( lNewVal ) SETGET
   METHOD NoDefaultRadioButton( lNewVal ) SETGET
   METHOD PositionRelativeToWindow( lNewVal ) SETGET
   METHOD RightToLeftLayout( lNewVal ) SETGET
   METHOD VerificationEnabled( lNewVal ) SETGET

   // Timeout logic
   METHOD timeoutMS( nMS ) SETGET
   METHOD TimedOut( lOut ) SETGET

   // Result Accessors
   METHOD SelectedButton() INLINE ::nButtonResult
   METHOD SelectedRadioButton() INLINE ::nRadioButtonResult
   METHOD VerificationChecked() INLINE ::lVerifyResult

   PROTECTED:
   VAR aConfig INIT Array( TDC_CONFIG ) // Internal array mapping to TASKDIALOGCONFIG structure.
   VAR HWND READONLY INIT NIL           // Handle to the dialog window once created.
   VAR lTimeOut READONLY INIT .F.       // Flag set if the dialog closed due to timeout.
   VAR nTimeOutMS READONLY INIT 0       // Timeout duration in milliseconds.

   METHOD setFlag( nBit, lEnable )
   METHOD validateText( vVal )
   METHOD _SetTextProp( nIdx, cVal, cUpdateFunc )
   METHOD _SetNumProp( nIdx, nVal )
ENDCLASS

/*
 * METHOD: New
 * Purpose: Initializes the advanced Task Dialog with primary text and button settings.
 */
METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon ) CLASS TTaskDialog
   ::aConfig[ TDC_WINDOWTITLE ] := ::validateText( cTitle )
   ::aConfig[ TDC_MAININSTRUCTION ] := ::validateText( cInstruction )
   ::aConfig[ TDC_CONTENT ] := ::validateText( cContent )
   ::aConfig[ TDC_FOOTER ] := ::validateText( cFooter )

   IF HB_ISNUMERIC( nCommonButtons )
      ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCommonButtons
   ENDIF
   IF HB_ISNUMERIC( nMainIcon )
      ::aConfig[ TDC_MAINICON ] := nMainIcon
   ENDIF
RETURN Self

METHOD validateText( vVal ) CLASS TTaskDialog
RETURN ValidateText( vVal )

/*
 * METHOD: _SetTextProp
 * Purpose: Internal helper to set text properties.
 * Logic: If the dialog is active (visible), it attempts to update the UI
 * in real-time using the provided update function.
 */
METHOD _SetTextProp( nIdx, cVal, cUpdateFunc ) CLASS TTaskDialog
   LOCAL cOld := ::aConfig[ nIdx ]
   IF HB_ISSTRING( cVal ) .OR. HB_ISNUMERIC( cVal )
      ::aConfig[ nIdx ] := ::validateText( cVal )
      // Dynamic UI update if the dialog is already on screen
      IF ::lActive .AND. HB_ISSTRING( cUpdateFunc ) .AND. ! Empty( cUpdateFunc )
         &( cUpdateFunc )( ::HWND, ::aConfig[ nIdx ] )
      ENDIF
   ENDIF
RETURN cOld

/*
 * METHOD: _SetNumProp
 * Purpose: Internal helper to set numeric properties.
 * Logic: Most numeric properties (like button counts) cannot be changed
 * once the dialog is active.
 */
METHOD _SetNumProp( nIdx, nVal ) CLASS TTaskDialog
   LOCAL nOld := ::aConfig[ nIdx ]
   IF ! ::lActive .AND. HB_ISNUMERIC( nVal )
      ::aConfig[ nIdx ] := nVal
   ENDIF
RETURN nOld

/*
 * METHOD: ShowDialog
 * Purpose: Prepares and displays the complex Task Dialog.
 * Logic:
 *   1. Prevents multiple instances of the same dialog object.
 *   2. Configures timer flags if a timeout or ONTIMER method is defined.
 *   3. Passes the 'Self' object to the callback structure so the C-level
 *      callback can route events back to this instance.
 *   4. Calls win_TaskDialogIndirect0.
 */
METHOD ShowDialog() CLASS TTaskDialog
   LOCAL nResult, nButton := NIL, nRadioButton := NIL, lVerificationFlagChecked := .F.

   IF ::lActive
      RETURN .F.
   ENDIF

   ::lError := .T.
   ::nButtonResult := NIL
   ::nRadioButtonResult := NIL
   ::nResult := E_FAIL
   ::TimedOut := .F.

   // Enable timer notifications if requested.
   IF ::timeoutMS() > 0 .OR. __objHasMethod( Self, "ONTIMER" )
      ::Flags := hb_bitOr( ::Flags, TDF_CALLBACK_TIMER )
   ENDIF

   // Ensure the dialog can be closed if a timeout is set.
   IF ::timeoutMS() > 0
      ::AllowDialogCancellation := .T.
   ENDIF

   IF os_IsWinVista_Or_Later()
      // Store the object reference for the callback.
      ::aConfig[ TDC_CALLBACK_INSTANCE ] := Self
      nResult := win_TaskDialogIndirect0( ::aConfig, @nButton, @nRadioButton, @lVerificationFlagChecked )
   ELSE
      nResult := E_NOTIMPL
   ENDIF

   ::lError := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nRadioButtonResult := nRadioButton
   ::lVerifyResult := lVerificationFlagChecked
   ::nResult := nResult
RETURN ( ! ::lError )

// Returns the Windows Window Handle (HWND) of the dialog.
METHOD DialogHandle() CLASS TTaskDialog
RETURN ::HWND

/*
 * METHOD: Showing
 * Purpose: Checks visibility or forces the dialog to show.
 * Parameter: lState (Optional) - If .T., attempts to show the dialog.
 */
METHOD Showing( lState ) CLASS TTaskDialog
   hb_default( @lState, .F. )
   IF lState .AND. ! ::lActive
      ::ShowDialog()
   ENDIF
RETURN ::lActive

/*
 * METHOD: OnCreated
 * Purpose: Internal callback handler for the TDN_CREATED notification.
 * Side Effects: Sets the lActive flag and stores the window handle.
 */
METHOD OnCreated( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )
   IF nNotify == TDN_CREATED
      ::lActive := .T.
      ::HWND := hWnd
   ENDIF
RETURN .F.

/*
 * METHOD: OnDestroyed
 * Purpose: Internal callback handler for the TDN_DESTROYED notification.
 * Side Effects: Clears the lActive flag and window handle.
 */
METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
   HB_SYMBOL_UNUSED( hWnd )
   HB_SYMBOL_UNUSED( nWParam )
   HB_SYMBOL_UNUSED( nLParam )
   IF nNotify == TDN_DESTROYED
      ::lActive := .F.
      ::HWND := NIL
   ENDIF
RETURN .F.

/*
 * METHOD: Listener
 * Purpose: Routes generic Task Dialog notifications to a user-defined codeblock.
 * Returns: The result of the codeblock evaluation, or .T. by default.
 */
METHOD Listener( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog
   HB_SYMBOL_UNUSED( hWnd )
   IF HB_ISEVALITEM( ::aConfig[ TDC_CALLBACK ] )
      RETURN ::aConfig[ TDC_CALLBACK ]:Eval( Self, nNotify, nWParam, nLParam )
   ENDIF
RETURN .T.

// Setter/Getter for standard button bitmask.
METHOD CommonButtons( nCBs ) CLASS TTaskDialog
   LOCAL nOld := ::aConfig[ TDC_COMMON_BUTTON_FLAGS ]
   IF ! ::lActive .AND. HB_ISNUMERIC( nCBs )
      ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCBs
   ENDIF
RETURN nOld

METHOD WindowTitle( cTitle ) CLASS TTaskDialog
RETURN ::_SetTextProp( TDC_WINDOWTITLE, cTitle, "_SetWindowTitle" )

METHOD Title( cTitle ) CLASS TTaskDialog
RETURN ::WindowTitle( cTitle )

// Setter/Getter for the Main Icon. Supports dynamic updates while the dialog is visible.
METHOD MainIcon( nIcon ) CLASS TTaskDialog
   LOCAL nOld := ::aConfig[ TDC_MAINICON ]
   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_MAINICON ] := nIcon
      IF ::lActive
         _UpdateMainIcon( ::HWND, ::aConfig[ TDC_MAINICON ] )
      ENDIF
   ENDIF
RETURN nOld

METHOD MainInstruction( cInstruction ) CLASS TTaskDialog
RETURN ::_SetTextProp( TDC_MAININSTRUCTION, cInstruction, "_SetMainInstruction" )

METHOD Instruction( cInstruction ) CLASS TTaskDialog
RETURN ::MainInstruction( cInstruction )

METHOD Content( cContent ) CLASS TTaskDialog
RETURN ::_SetTextProp( TDC_CONTENT, cContent, "_SetContent" )

/*
 * METHOD: CustomButtons
 * Purpose: Defines an array of custom buttons.
 * Parameter: aCustButton - Array of pairs { nButtonID, cButtonCaption }.
 */
METHOD CustomButtons( aCustButton ) CLASS TTaskDialog
   LOCAL aOld := ::aConfig[ TDC_TASKDIALOG_BUTTON ]
   IF ! ::lActive .AND. HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
      ::aConfig[ TDC_BUTTON ] := Len( aCustButton )
      ::aConfig[ TDC_TASKDIALOG_BUTTON ] := aCustButton
   ENDIF
RETURN aOld

METHOD DefaultButton( nDefaultButton ) CLASS TTaskDialog
RETURN ::_SetNumProp( TDC_DEFAULTBUTTON, nDefaultButton )

/*
 * METHOD: CustomRadioButtons
 * Purpose: Defines an array of radio buttons.
 * Parameter: aCustButton - Array of pairs { nButtonID, cButtonCaption }.
 */
METHOD CustomRadioButtons( aCustButton ) CLASS TTaskDialog
   LOCAL aOld := ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ]
   IF ! ::lActive .AND. HB_ISARRAY( aCustButton ) .AND. Len( aCustButton ) > 0
      ::aConfig[ TDC_RADIOBUTTON ] := Len( aCustButton )
      ::aConfig[ TDC_TASKDIALOG_RADIOBUTTON ] := aCustButton
   ENDIF
RETURN aOld

METHOD DefaultRadioButton( nDefaultButton ) CLASS TTaskDialog
RETURN ::_SetNumProp( TDC_DEFAULTRADIOBUTTON, nDefaultButton )

METHOD VerificationText( cText ) CLASS TTaskDialog
   LOCAL cOld := ::aConfig[ TDC_VERIFICATIONTEXT ]
   IF ! ::lActive .AND. ( HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText ) )
      ::aConfig[ TDC_VERIFICATIONTEXT ] := ::validateText( cText )
   ENDIF
RETURN cOld

METHOD ExpandedInfo( cText ) CLASS TTaskDialog
RETURN ::_SetTextProp( TDC_EXPANDEDINFORMATION, cText, "_SetExpandedInformation" )

METHOD ExpandedControlText( cText ) CLASS TTaskDialog
   LOCAL cOld := ::aConfig[ TDC_EXPANDEDCONTROLTEXT ]
   IF ! ::lActive .AND. ( HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText ) )
      ::aConfig[ TDC_EXPANDEDCONTROLTEXT ] := ::validateText( cText )
   ENDIF
RETURN cOld

METHOD ExpandedCtrlText( cText ) CLASS TTaskDialog
RETURN ::ExpandedControlText( cText )

METHOD CollapsedControlText( cText ) CLASS TTaskDialog
   LOCAL cOld := ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ]
   IF ! ::lActive .AND. ( HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText ) )
      ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ] := ::validateText( cText )
   ENDIF
RETURN cOld

METHOD CollapsedCtrlText( cText ) CLASS TTaskDialog
RETURN ::CollapsedControlText( cText )

// Setter/Getter for the Footer Icon. Supports dynamic updates.
METHOD FooterIcon( nIcon ) CLASS TTaskDialog
   LOCAL nOld := ::aConfig[ TDC_FOOTERICON ]
   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_FOOTERICON ] := nIcon
      IF ::lActive
         _UpdateFooterIcon( ::HWND, ::aConfig[ TDC_FOOTERICON ] )
      ENDIF
   ENDIF
RETURN nOld

METHOD Footer( cFooter ) CLASS TTaskDialog
RETURN ::_SetTextProp( TDC_FOOTER, cFooter, "_SetFooter" )

METHOD Width( nWidth ) CLASS TTaskDialog
RETURN ::_SetNumProp( TDC_WIDTH, nWidth )

// Sets the parent window handle. Must be a valid window handle.
METHOD ParentHandle( nHandle ) CLASS TTaskDialog
   LOCAL nOld := ::aConfig[ TDC_HWND ]
   IF ! ::lActive .AND. HB_ISNUMERIC( nHandle ) .AND. IsWindowHandle( nHandle )
      ::aConfig[ TDC_HWND ] := nHandle
   ENDIF
RETURN nOld

// Assigns a codeblock to handle dialog events.
METHOD CallBackBlock( bCode ) CLASS TTaskDialog
   IF ! ::lActive .AND. HB_ISEVALITEM( bCode )
      ::aConfig[ TDC_CALLBACK ] := bCode
   ENDIF
RETURN ::aConfig[ TDC_CALLBACK ]

METHOD Flags( nFlags ) CLASS TTaskDialog
RETURN ::_SetNumProp( TDC_TASKDIALOG_FLAGS, nFlags )

/*
 * METHOD: setFlag
 * Purpose: Internal helper to toggle specific bits in the TDF_* flags bitmask.
 * Parameters:
 *    nBit: The bitmask constant to toggle.
 *    lEnable: .T. to set the bit, .F. to clear it.
 */
METHOD setFlag( nBit, lEnable ) CLASS TTaskDialog
   LOCAL nCur := ::Flags()
   hb_default( @nCur, 0 )
   IF ! ::lActive .AND. HB_ISLOGICAL( lEnable )
      ::Flags( iif( lEnable, hb_bitOr( nCur, nBit ), hb_bitAnd( nCur, hb_bitNot( nBit ) ) ) )
   ENDIF
RETURN NIL

// Toggles the ability to close the dialog via Alt+F4 or the 'X' button.
METHOD AllowDialogCancellation( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_ALLOW_DIALOG_CANCELLATION ) != 0 )
   ::setFlag( TDF_ALLOW_DIALOG_CANCELLATION, lNewVal )
RETURN lOld

// Toggles whether the dialog can be minimized to the taskbar.
METHOD CanBeMinimized( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_CAN_BE_MINIMIZED ) != 0 )
   ::setFlag( TDF_CAN_BE_MINIMIZED, lNewVal )
RETURN lOld

// Enables processing of <A HREF="..."> tags in text fields.
METHOD EnableHyperlinks( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_ENABLE_HYPERLINKS ) != 0 )
   ::setFlag( TDF_ENABLE_HYPERLINKS, lNewVal )
RETURN lOld

// Determines if the expanded information area is visible when the dialog opens.
METHOD ExpandedByDefault( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_EXPANDED_BY_DEFAULT ) != 0 )
   ::setFlag( TDF_EXPANDED_BY_DEFAULT, lNewVal )
RETURN lOld

// Determines if expanded info is shown in the footer area instead of the main body.
METHOD ExpandFooterArea( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_EXPAND_FOOTER_AREA ) != 0 )
   ::setFlag( TDF_EXPAND_FOOTER_AREA, lNewVal )
RETURN lOld

// If .T., no radio button is selected by default.
METHOD NoDefaultRadioButton( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_NO_DEFAULT_RADIO_BUTTON ) != 0 )
   ::setFlag( TDF_NO_DEFAULT_RADIO_BUTTON, lNewVal )
RETURN lOld

// Centers the dialog relative to the parent window instead of the screen.
METHOD PositionRelativeToWindow( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_POSITION_RELATIVE_TO_WINDOW ) != 0 )
   ::setFlag( TDF_POSITION_RELATIVE_TO_WINDOW, lNewVal )
RETURN lOld

// Enables Right-To-Left layout for specific locales.
METHOD RightToLeftLayout( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_RTL_LAYOUT ) != 0 )
   ::setFlag( TDF_RTL_LAYOUT, lNewVal )
RETURN lOld

// Sets the initial state of the verification checkbox.
METHOD VerificationEnabled( lNewVal ) CLASS TTaskDialog
   LOCAL lOld := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_VERIFICATION_FLAG_CHECKED ) != 0 )
   ::setFlag( TDF_VERIFICATION_FLAG_CHECKED, lNewVal )
RETURN lOld

// Configures the auto-close timeout in milliseconds.
METHOD timeoutMS( nMS ) CLASS TTaskDialog
   LOCAL nOld := ::nTimeOutMS
   IF ! ::lActive .AND. HB_ISNUMERIC( nMS )
      ::nTimeOutMS := nMS
   ENDIF
RETURN nOld

// Internal flag used by the timer callback to signal a timeout closure.
METHOD TimedOut( lOut ) CLASS TTaskDialog
   IF ::lActive .AND. HB_ISLOGICAL( lOut )
      ::lTimeOut := lOut
   ENDIF
RETURN ::lTimeOut

/*
 * FUNCTION: ValidateText
 * Purpose: Static helper to sanitize input for Task Dialog text fields.
 * Logic: Task Dialogs accept either a string or a numeric resource ID.
 */
STATIC FUNCTION ValidateText( vVal )
RETURN iif( HB_ISNUMERIC( vVal ) .OR. ( HB_ISSTRING( vVal ) .AND. ! HB_ISNULL( vVal ) ), vVal, NIL )

#endif
