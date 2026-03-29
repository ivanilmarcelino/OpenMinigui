/*
 * HMG Extended - Task Dialog Classes
 *
 * This module provides Object-Oriented wrappers for the Windows Task Dialog API, 
 * introduced in Windows Vista. It abstracts the complexity of the Win32 API 
 * into manageable Harbour classes, allowing for modern, professional-looking 
 * message boxes and complex interaction dialogs.
 *
 * The module contains two primary classes:
 *   1. TSimpleTaskDialog: A wrapper for the basic TaskDialog function.
 *   2. TTaskDialog: A comprehensive wrapper for TaskDialogIndirect, supporting 
 *      advanced features like progress bars, radio buttons, and footers.
 *
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 * Enhanced documentation for HMG Extended Edition standards.
 */

#if ! defined( __XHARBOUR__ ) .AND. ( __HARBOUR__ - 0 > 0x030000 )

#include "hbclass.ch"
#include "TaskDlgs.ch"
#include "i_var.ch"

// Internal constant used to store the Harbour object instance pointer within the 
// TASKDIALOGCONFIG structure. This is critical for the C-level callback function 
// to route Windows notifications (TDN_*) back to the specific Harbour class instance.
#define TDC_CALLBACK_INSTANCE 23

/*-----------------------------------------------------------------------------*
 * CLASS: TSimpleTaskDialog
 * 
 * Purpose:
 *    Provides a lightweight interface for the standard TaskDialog API.
 *    This is intended for simple notifications that require more visual 
 *    hierarchy than MsgInfo() but do not need complex event handling.
 * 
 * Implementation:
 *    Uses win_TaskDialog0, which maps to the direct TaskDialog() Windows function.
 *    This approach is memory-efficient as it avoids the overhead of the 
 *    full TASKDIALOGCONFIG structure.
 *-----------------------------------------------------------------------------*/
CREATE CLASS TSimpleTaskDialog FUNCTION SimpleTaskDialog

   EXPORTED:
   VAR Cargo                      // User-defined data storage for context-specific information.
   VAR lError READONLY INIT .T.   // Boolean flag indicating if the last Execute() call failed.
   VAR nButtonResult READONLY INIT NIL // Stores the ID of the button clicked by the user (e.g., IDOK).
   VAR nResult READONLY INIT E_FAIL    // Stores the raw HRESULT returned by the Windows API.

   METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon )
   METHOD Execute()

   // Property Accessors (Setters/Getters)
   METHOD Title( cTitle ) SETGET
   METHOD Instruction( cInstruction ) SETGET
   METHOD Content( cContent ) SETGET
   METHOD CommonButtons( nCBs ) SETGET
   METHOD MainIcon( nIcon ) SETGET

   PROTECTED:
   VAR cTitle INIT NIL            // The text appearing in the window title bar.
   VAR cInstruction INIT NIL      // The primary instruction (large blue text).
   VAR cContent INIT NIL          // The secondary content (standard body text).
   VAR nCommonButtons INIT TDCBF_OK_BUTTON // Bitmask of standard buttons (OK, Yes, No, etc).
   VAR nMainIcon INIT TD_NO_ICON  // Resource ID or constant for the main icon.

   METHOD validateText( vVal )    // Internal helper to sanitize text/resource inputs.
ENDCLASS

/*
 * METHOD: New
 * Purpose: Constructor for the simple task dialog.
 * Parameters:
 *    cTitle         : String/Numeric - Window title.
 *    cInstruction   : String/Numeric - Main heading.
 *    cContent       : String/Numeric - Body text.
 *    nCommonButtons : Numeric - Bitmask (e.g., TDCBF_YES_BUTTON | TDCBF_NO_BUTTON).
 *    nMainIcon      : Numeric - Icon constant (e.g., TD_INFORMATION_ICON).
 * Returns: Self
 * Reasoning: We allow Numeric values for text fields because the Windows API 
 *            can accept Integer Resource IDs for localized strings.
 */
METHOD New( cTitle, cInstruction, cContent, nCommonButtons, nMainIcon ) CLASS TSimpleTaskDialog

   ::cTitle := ::validateText( cTitle )
   ::cInstruction := ::validateText( cInstruction )
   ::cContent := ::validateText( cContent )

   // Validate numeric parameters to prevent API-level crashes or unexpected behavior.
   IF HB_ISNUMERIC( nCommonButtons )
      ::nCommonButtons := nCommonButtons
   ENDIF

   IF HB_ISNUMERIC( nMainIcon )
      ::nMainIcon := nMainIcon
   ENDIF
   
RETURN Self

/*
 * METHOD: validateText
 * Purpose: Sanitizes input for text fields.
 * Logic: Task Dialogs accept Strings or Numerics (for Resource IDs). 
 *        NIL or empty strings are handled to avoid passing invalid pointers to the C layer.
 */
METHOD validateText( vVal ) CLASS TSimpleTaskDialog
RETURN ValidateText( vVal )

/*
 * METHOD: Execute
 * Purpose: Displays the modal dialog and waits for user interaction.
 * Returns: Logical .T. if the dialog was shown and closed successfully.
 * Side Effects: 
 *    - Updates ::nButtonResult with the ID of the button that closed the dialog.
 *    - Updates ::lError and ::nResult based on the API return code.
 */
METHOD Execute() CLASS TSimpleTaskDialog
   LOCAL nResult
   LOCAL nButton := NIL

   // Reset state before execution to ensure results from previous calls are cleared.
   ::lError := .T.
   ::nButtonResult := NIL
   ::nResult := E_FAIL

   // Task Dialogs are only available on Vista and newer. 
   // On older systems, we return E_NOTIMPL to allow the caller to handle fallbacks.
   IF os_IsWinVista_Or_Later()
      // win_TaskDialog0 is the Harbour wrapper for the TaskDialog API.
      // The @nButton parameter is passed by reference to capture the user's choice.
      nResult := win_TaskDialog0( ,, ::cTitle, ::cInstruction, ::cContent, ::nCommonButtons, ::nMainIcon, @nButton )
   ELSE
      nResult := E_NOTIMPL 
   ENDIF

   // S_OK (0) indicates success in the Windows API.
   ::lError := !( nResult == NOERROR )
   ::nButtonResult := nButton
   ::nResult := nResult
   
RETURN ( ! ::lError )

/*
 * METHOD: Title (Setter/Getter)
 * Purpose: Manages the window title.
 * Logic: Returns current value. If a new value is provided, it updates the internal state.
 */
METHOD Title( cTitle ) CLASS TSimpleTaskDialog
   LOCAL cOldVal := ::cTitle

   IF HB_ISSTRING( cTitle ) .OR. HB_ISNUMERIC( cTitle )
      ::cTitle := iif( HB_ISSTRING( cTitle ) .AND. HB_ISNULL( cTitle ), NIL, cTitle )
   ENDIF
RETURN cOldVal

/*
 * METHOD: Instruction (Setter/Getter)
 * Purpose: Manages the primary instruction text (the large blue heading).
 */
METHOD Instruction( cInstruction ) CLASS TSimpleTaskDialog
   LOCAL cOldVal := ::cInstruction

   IF HB_ISSTRING( cInstruction ) .OR. HB_ISNUMERIC( cInstruction )
      ::cInstruction := iif( HB_ISSTRING( cInstruction ) .AND. HB_ISNULL( cInstruction ), NIL, cInstruction )
   ENDIF
RETURN cOldVal

/*
 * METHOD: Content (Setter/Getter)
 * Purpose: Manages the secondary body text.
 */
METHOD Content( cContent ) CLASS TSimpleTaskDialog
   LOCAL cOldVal := ::cContent

   IF HB_ISSTRING( cContent ) .OR. HB_ISNUMERIC( cContent )
      ::cContent := iif( HB_ISSTRING( cContent ) .AND. HB_ISNULL( cContent ), NIL, cContent )
   ENDIF
RETURN cOldVal

/*
 * METHOD: CommonButtons (Setter/Getter)
 * Purpose: Manages the bitmask for standard buttons (OK, Cancel, Yes, No, etc).
 */
METHOD CommonButtons( nCBs ) CLASS TSimpleTaskDialog
   LOCAL nOldVal := ::nCommonButtons

   IF HB_ISNUMERIC( nCBs )
      ::nCommonButtons := nCBs
   ENDIF
RETURN nOldVal

/*
 * METHOD: MainIcon (Setter/Getter)
 * Purpose: Manages the main icon displayed in the dialog.
 */
METHOD MainIcon( nIcon ) CLASS TSimpleTaskDialog
   LOCAL nOldVal := ::nMainIcon

   IF HB_ISNUMERIC( nIcon )
      ::nMainIcon := nIcon
   ENDIF
RETURN nOldVal

/*-----------------------------------------------------------------------------*
 * CLASS: TTaskDialog
 * 
 * Purpose:
 *    A high-level, object-oriented wrapper for the Windows TaskDialogIndirect API.
 *    This class supports the full range of Task Dialog features, including 
 *    custom buttons, radio buttons, progress bars, footers, and event callbacks.
 * 
 * Design Decision:
 *    The class maintains an internal array (aConfig) that maps directly to the 
 *    C-level TASKDIALOGCONFIG structure. This ensures that the Harbour object 
 *    remains synchronized with the requirements of the underlying Windows API.
 * 
 * HMG Extended Integration:
 *    This class integrates with HMG's internal window management. It uses 
 *    HMG-specific internal functions (prefixed with _) to update the UI of 
 *    an active dialog in real-time without needing to recreate the window.
 *-----------------------------------------------------------------------------*/
CREATE CLASS TTaskDialog FUNCTION TaskDialog

   EXPORTED:
   VAR Cargo                                // User-defined data storage.
   VAR lActive READONLY INIT .F.            // Indicates if the dialog is currently visible on screen.
   VAR lError READONLY INIT .T.             // Indicates if the last API execution failed.
   VAR nButtonResult READONLY INIT NIL      // ID of the button clicked by the user.
   VAR nRadioButtonResult READONLY INIT NIL // ID of the selected radio button.
   VAR nResult READONLY INIT E_FAIL         // The HRESULT returned by the Windows API call.
   VAR lVerifyResult READONLY INIT .F.      // Final state of the verification checkbox (e.g., "Don't show again").

   METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon )
   METHOD Execute() INLINE ::ShowDialog()   // Standard alias for ShowDialog.
   METHOD ShowDialog()
   METHOD DialogHandle()
   METHOD Showing( lState )
   METHOD OnCreated( hWnd, nNotify, nWParam, nLParam )
   METHOD OnDestroyed( hWnd, nNotify, nWParam, nLParam )
   METHOD Listener( hWnd, nNotify, nWParam, nLParam )
   
   // Property Accessors (SETGET)
   // These methods allow reading and writing properties. Many will update the 
   // UI immediately if the dialog is currently active (lActive == .T.).
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
   
   // Flag Management Methods
   // These methods toggle specific bits within the TDF_* bitmask to control behavior.
   METHOD AllowDialogCancellation( lNewVal ) SETGET
   METHOD CanBeMinimized( lNewVal ) SETGET
   METHOD EnableHyperlinks( lNewVal ) SETGET
   METHOD ExpandedByDefault( lNewVal ) SETGET
   METHOD ExpandFooterArea( lNewVal ) SETGET
   METHOD NoDefaultRadioButton( lNewVal ) SETGET
   METHOD PositionRelativeToWindow( lNewVal ) SETGET
   METHOD RightToLeftLayout( lNewVal ) SETGET
   METHOD VerificationEnabled( lNewVal ) SETGET
   METHOD timeoutMS( nMS ) SETGET
   METHOD TimedOut( lOut ) SETGET

   // Result Retrieval Helpers
   METHOD SelectedButton() INLINE ::nButtonResult
   METHOD SelectedRadioButton() INLINE ::nRadioButtonResult
   METHOD VerificationChecked() INLINE ::lVerifyResult
   
   PROTECTED:
   VAR aConfig INIT Array( TDC_CONFIG )     // Internal array representing the C structure.
   VAR HWND READONLY INIT NIL               // The Windows handle of the active dialog.
   VAR lTimeOut READONLY INIT .F.           // Internal flag indicating if a timeout occurred.
   VAR nTimeOutMS READONLY INIT 0           // Timeout duration in milliseconds.

   METHOD setFlag( nBit, lEnable )          // Helper to toggle specific bits within the Flags bitmask.
   METHOD validateText( vVal )              // Ensures input is either a valid string or a numeric resource ID.
ENDCLASS

/*
 * METHOD: New
 * Purpose: Constructor for the TTaskDialog class.
 * Parameters:
 *    cTitle          : Window title text.
 *    cInstruction    : Primary instruction text.
 *    cContent        : Detailed body text.
 *    cFooter         : Text for the footer area.
 *    nCommonButtons  : Bitmask of standard buttons.
 *    nMainIcon       : Resource ID or constant for the main icon.
 * Returns: Self
 */
METHOD New( cTitle, cInstruction, cContent, cFooter, nCommonButtons, nMainIcon ) CLASS TTaskDialog

   // Initialize the internal configuration array.
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

/*
 * METHOD: validateText
 * Purpose: Ensures that input values are compatible with the TaskDialog API.
 * Logic: The API accepts either a string or a numeric resource ID.
 */
METHOD validateText( vVal ) CLASS TTaskDialog
RETURN ValidateText( vVal )

/*
 * METHOD: ShowDialog
 * Purpose: Prepares the environment and executes the TaskDialogIndirect API call.
 * Logic:
 *    1. Resets result variables to ensure a clean state.
 *    2. Automatically enables the TDF_CALLBACK_TIMER flag if a timeout is set.
 *    3. Sets the callback instance to 'self' to enable event routing back to this object.
 *    4. Calls the C-level wrapper win_TaskDialogIndirect0.
 * Returns: Logical .T. if the dialog was displayed and closed successfully.
 */
METHOD ShowDialog() CLASS TTaskDialog
   LOCAL nResult
   LOCAL nButton := NIL
   LOCAL nRadioButton := NIL
   LOCAL lVerificationFlagChecked := .F.

   // Prevent re-entrant calls if the dialog is already visible.
   IF ! ::lActive
      ::lError := .T.
      ::nButtonResult := NIL
      ::nRadioButtonResult := NIL
      ::nResult := E_FAIL
      ::TimedOut := .F.

      // Windows requires the TDF_CALLBACK_TIMER flag to send TDN_TIMER notifications.
      // We check for a timeout value or the existence of an ONTIMER method.
      IF ::timeoutMS() > 0 .OR. __objHasMethod( Self, "ONTIMER" )
         ::Flags := hb_bitOr( ::Flags, TDF_CALLBACK_TIMER )
      ENDIF

      // If a timeout is active, the dialog must be cancellable to allow the timer 
      // logic to close the window programmatically via the API.
      IF ::timeoutMS() > 0
         ::AllowDialogCancellation := .T.
      ENDIF

      // TaskDialogIndirect is only available on Windows Vista and later.
      IF os_IsWinVista_Or_Later()
         // Store the object reference so the C callback knows which Harbour object to notify.
         ::aConfig[ TDC_CALLBACK_INSTANCE ] := self
         nResult := win_TaskDialogIndirect0( ::aConfig, @nBUTTON, @nRadioButton, @lVerificationFlagChecked )
      ELSE
         nResult := E_NOTIMPL
      ENDIF

      // Process results after the modal loop finishes.
      ::lError := !( nResult == NOERROR )
      ::nButtonResult := nButton
      ::nRadioButtonResult := nRadioButton
      ::lVerifyResult := lVerificationFlagChecked
      ::nResult := nResult
   ENDIF
   
RETURN ( ! ::lError )

/*
 * METHOD: DialogHandle
 * Purpose: Returns the Windows HWND of the dialog.
 * Returns: Numeric handle or NIL if the dialog is not active.
 */
METHOD DialogHandle() CLASS TTaskDialog
RETURN ::HWND

/*
 * METHOD: Showing
 * Purpose: Checks the active state or forces the dialog to show.
 * Parameters: lState - If .T., attempts to show the dialog.
 * Returns: Current active state.
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
 * Side Effects: Sets the lActive flag and stores the window handle for UI updates.
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
 * Side Effects: Clears the active state and window handle.
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
 * Purpose: Central dispatcher for all Task Dialog notifications.
 * Logic: Routes events to a user-defined code block if one is assigned.
 * Returns: Result of the code block evaluation (usually .T. to continue).
 */
METHOD Listener( hWnd, nNotify, nWParam, nLParam ) CLASS TTaskDialog

   HB_SYMBOL_UNUSED( hWnd )

   // If the developer provided a CallBackBlock, evaluate it with event details.
   IF HB_ISEVALITEM( ::aConfig[ TDC_CALLBACK ] )
      RETURN ::aConfig[ TDC_CALLBACK ]:Eval( self, nNotify, nWParam, nLParam )
   ENDIF
   
RETURN .T.

/*
 * METHOD: CommonButtons
 * Purpose: Sets or gets the standard button bitmask.
 * Note: Cannot be modified while the dialog is active as the API structure is fixed at creation.
 */
METHOD CommonButtons( nCBs ) CLASS TTaskDialog
   LOCAL nOldCBS := ::aConfig[ TDC_COMMON_BUTTON_FLAGS ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nCBs )
         ::aConfig[ TDC_COMMON_BUTTON_FLAGS ] := nCBs
      ENDIF
   ENDIF
RETURN nOldCBS

/*
 * METHOD: WindowTitle
 * Purpose: Sets or gets the dialog's title bar text.
 * Side Effect: Uses HMG's _SetWindowTitle to update the UI immediately if the dialog is visible.
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

METHOD Title( cTitle ) CLASS TTaskDialog
RETURN ::WindowTitle( cTitle )

/*
 * METHOD: MainIcon
 * Purpose: Sets or gets the primary icon.
 * Side Effect: Uses HMG's _UpdateMainIcon for real-time updates.
 */
METHOD MainIcon( nIcon ) CLASS TTaskDialog

   IF HB_ISNUMERIC( nIcon )
      ::aConfig[ TDC_MAINICON ] := nIcon
      IF ::lActive
         _UpdateMainIcon( ::HWND, ::aConfig[ TDC_MAINICON ] )
      ENDIF
   ENDIF
RETURN ::aConfig[ TDC_MAINICON ]

/*
 * METHOD: MainInstruction
 * Purpose: Sets or gets the primary instruction text.
 * Side Effect: Uses HMG's _SetMainInstruction for real-time updates.
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

METHOD Instruction( cInstruction ) CLASS TTaskDialog
RETURN ::MainInstruction( cInstruction )

/*
 * METHOD: Content
 * Purpose: Sets or gets the body content text.
 * Side Effect: Uses HMG's _SetContent for real-time updates.
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

/*
 * METHOD: CustomButtons
 * Purpose: Defines an array of custom buttons.
 * Parameters: aCustButton - Array of { nID, cText } pairs.
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

/*
 * METHOD: DefaultButton
 * Purpose: Sets the ID of the button that has focus by default.
 */
METHOD DefaultButton( nDefaultButton ) CLASS TTaskDialog
   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTBUTTON ] := nDefaultButton
      ENDIF
   ENDIF
RETURN nOldVal

/*
 * METHOD: CustomRadioButtons
 * Purpose: Defines an array of radio buttons.
 * Parameters: aCustButton - Array of { nID, cText } pairs.
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

/*
 * METHOD: DefaultRadioButton
 * Purpose: Sets the ID of the radio button selected by default.
 */
METHOD DefaultRadioButton( nDefaultButton ) CLASS TTaskDialog
   LOCAL nOldVal := ::aConfig[ TDC_DEFAULTRADIOBUTTON ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nDefaultButton )
         ::aConfig[ TDC_DEFAULTRADIOBUTTON ] := nDefaultButton
      ENDIF
   ENDIF
RETURN nOldVal

/*
 * METHOD: VerificationText
 * Purpose: Sets the label for the verification checkbox (e.g., "Don't show this again").
 */
METHOD VerificationText( cText ) CLASS TTaskDialog
   LOCAL cOldVal := ::aConfig[ TDC_VERIFICATIONTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_VERIFICATIONTEXT ] := cText
      ENDIF
   ENDIF
RETURN cOldVal

/*
 * METHOD: ExpandedInfo
 * Purpose: Sets or gets the text in the expandable info area.
 * Side Effect: Uses HMG's _SetExpandedInformation for real-time updates.
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

/*
 * METHOD: ExpandedControlText
 * Purpose: Sets the label for the expander button when the info is visible.
 */
METHOD ExpandedControlText( cText ) CLASS TTaskDialog
   LOCAL cOldVal := ::aConfig[ TDC_EXPANDEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_EXPANDEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF
RETURN cOldVal

METHOD ExpandedCtrlText( cText ) CLASS TTaskDialog
RETURN ::ExpandedControlText( cText )

/*
 * METHOD: CollapsedControlText
 * Purpose: Sets the label for the expander button when the info is hidden.
 */
METHOD CollapsedControlText( cText ) CLASS TTaskDialog
   LOCAL cOldVal := ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ]

   IF ! ::lActive
      IF HB_ISSTRING( cText ) .OR. HB_ISNUMERIC( cText )
         ::aConfig[ TDC_COLLAPSEDCONTROLTEXT ] := cText
      ENDIF
   ENDIF
RETURN cOldVal

METHOD CollapsedCtrlText( cText ) CLASS TTaskDialog
RETURN ::CollapsedControlText( cText )

/*
 * METHOD: FooterIcon
 * Purpose: Sets or gets the icon displayed in the footer.
 * Side Effect: Uses HMG's _UpdateFooterIcon for real-time updates.
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

/*
 * METHOD: Footer
 * Purpose: Sets or gets the footer text.
 * Side Effect: Uses HMG's _SetFooter for real-time updates.
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

/*
 * METHOD: Width
 * Purpose: Sets the width of the dialog in dialog units.
 */
METHOD Width( nWidth ) CLASS TTaskDialog
   LOCAL nOldVal := ::aConfig[ TDC_WIDTH ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nWidth )
      ::aConfig[ TDC_WIDTH ] := nWidth
   ENDIF
RETURN nOldVal

/*
 * METHOD: ParentHandle
 * Purpose: Sets the numeric handle (HWND) of the owner window.
 */
METHOD ParentHandle( nHandle ) CLASS TTaskDialog
   LOCAL nOldVal := ::aConfig[ TDC_HWND ]

   IF ! ::lActive .AND. HB_ISNUMERIC( nHandle ) .AND. IsWindowHandle( nHandle )
      ::aConfig[ TDC_HWND ] := nHandle
   ENDIF
RETURN nOldVal

/*
 * METHOD: CallBackBlock
 * Purpose: Assigns a Harbour code block to handle dialog events.
 * Parameters: bCode - Code block receiving ( self, nNotify, nWParam, nLParam ).
 */
METHOD CallBackBlock( bCode ) CLASS TTaskDialog

   IF ! ::lActive
      IF HB_ISEVALITEM( bCode )
         ::aConfig[ TDC_CALLBACK ] := bCode
      ENDIF
   ENDIF
RETURN ::aConfig[ TDC_CALLBACK ]

/*
 * METHOD: Flags
 * Purpose: Direct access to the TDF_* bitmask flags.
 */
METHOD Flags( nFlags ) CLASS TTaskDialog
   LOCAL nOldVal := ::aConfig[ TDC_TASKDIALOG_FLAGS ]

   IF ! ::lActive
      IF HB_ISNUMERIC( nFlags )
         ::aConfig[ TDC_TASKDIALOG_FLAGS ] := nFlags
      ENDIF
   ENDIF
RETURN nOldVal

/*
 * METHOD: setFlag
 * Purpose: Internal helper to toggle specific bits within the Flags bitmask.
 * Parameters:
 *    nBit    : The bit constant to modify.
 *    lEnable : .T. to set the bit, .F. to clear it.
 */
METHOD setFlag( nBit, lEnable ) CLASS TTaskDialog
   LOCAL nCurFlags := ::Flags()

   hb_default( @nCurFlags, 0 )
   IF ! ::lActive .AND. HB_ISLOGICAL( lEnable )
      ::Flags( iif( lEnable, hb_bitOr( nCurFlags, nBit ), hb_bitAnd( nCurFlags, hb_bitNot( nBit ) ) ) )
   ENDIF
RETURN NIL

/*
 * METHOD: AllowDialogCancellation
 * Flag: TDF_ALLOW_DIALOG_CANCELLATION
 * Purpose: Enables closing the dialog via ESC, Alt+F4, or the 'X' button.
 */
METHOD AllowDialogCancellation( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_ALLOW_DIALOG_CANCELLATION ) != 0 )
   ::setFlag( TDF_ALLOW_DIALOG_CANCELLATION, lNewVal )
RETURN lOldVal

/*
 * METHOD: CanBeMinimized
 * Flag: TDF_CAN_BE_MINIMIZED
 * Purpose: Enables the minimize button on the dialog.
 */
METHOD CanBeMinimized( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_CAN_BE_MINIMIZED ) != 0 )
   ::setFlag( TDF_CAN_BE_MINIMIZED, lNewVal )
RETURN lOldVal

/*
 * METHOD: EnableHyperlinks
 * Flag: TDF_ENABLE_HYPERLINKS
 * Purpose: Allows the dialog to process <A HREF> tags in text fields.
 */
METHOD EnableHyperlinks( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_ENABLE_HYPERLINKS ) != 0 )
   ::setFlag( TDF_ENABLE_HYPERLINKS, lNewVal )
RETURN lOldVal

/*
 * METHOD: ExpandedByDefault
 * Flag: TDF_EXPANDED_BY_DEFAULT
 * Purpose: If .T., the expanded info section is shown immediately upon opening.
 */
METHOD ExpandedByDefault( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_EXPANDED_BY_DEFAULT ) != 0 )
   ::setFlag( TDF_EXPANDED_BY_DEFAULT, lNewVal )
RETURN lOldVal

/*
 * METHOD: ExpandFooterArea
 * Flag: TDF_EXPAND_FOOTER_AREA
 * Purpose: If .T., the expanded info is displayed in the footer area instead of the content area.
 */
METHOD ExpandFooterArea( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_EXPAND_FOOTER_AREA ) != 0 )
   ::setFlag( TDF_EXPAND_FOOTER_AREA, lNewVal )
RETURN lOldVal

/*
 * METHOD: NoDefaultRadioButton
 * Flag: TDF_NO_DEFAULT_RADIO_BUTTON
 * Purpose: If .T., no radio button is selected by default.
 */
METHOD NoDefaultRadioButton( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_NO_DEFAULT_RADIO_BUTTON ) != 0 )
   ::setFlag( TDF_NO_DEFAULT_RADIO_BUTTON, lNewVal )
RETURN lOldVal

/*
 * METHOD: PositionRelativeToWindow
 * Flag: TDF_POSITION_RELATIVE_TO_WINDOW
 * Purpose: If .T., the dialog centers itself relative to the parent window instead of the screen.
 */
METHOD PositionRelativeToWindow( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_POSITION_RELATIVE_TO_WINDOW ) != 0 )
   ::setFlag( TDF_POSITION_RELATIVE_TO_WINDOW, lNewVal )
RETURN lOldVal

/*
 * METHOD: RightToLeftLayout
 * Flag: TDF_RTL_LAYOUT
 * Purpose: Enables right-to-left layout for localized languages (e.g., Arabic, Hebrew).
 */
METHOD RightToLeftLayout( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_RTL_LAYOUT ) != 0 )
   ::setFlag( TDF_RTL_LAYOUT, lNewVal )
RETURN lOldVal

/*
 * METHOD: VerificationEnabled
 * Flag: TDF_VERIFICATION_FLAG_CHECKED
 * Purpose: Sets the initial state of the verification checkbox.
 */
METHOD VerificationEnabled( lNewVal ) CLASS TTaskDialog
   LOCAL lOldVal := ( hb_bitAnd( hb_defaultValue( ::Flags(), 0 ), TDF_VERIFICATION_FLAG_CHECKED ) != 0 )
   ::setFlag( TDF_VERIFICATION_FLAG_CHECKED, lNewVal )
RETURN lOldVal

/*
 * METHOD: timeoutMS
 * Purpose: Sets or gets the automatic closure timeout in milliseconds.
 * Logic: This value is used by the internal timer logic to close the dialog.
 */
METHOD timeoutMS ( nMS ) CLASS TTaskDialog
   LOCAL nOldVal := ::nTimeOutMS
   IF ! ::lActive .AND. HB_ISNUMERIC( nMS )
      ::nTimeOutMS := nMS
   ENDIF
RETURN nOldVal

/*
 * METHOD: TimedOut
 * Purpose: Indicates if the dialog was closed due to the timeout expiring.
 * Logic: This flag is typically set by the timer event handler.
 */
METHOD TimedOut( lOut ) CLASS TTaskDialog
   IF ::lActive .AND. HB_ISLOGICAL( lOut )
      ::lTimeOut := lOut
   ENDIF
RETURN ::lTimeOut

// Static helper function to ensure text values are either valid strings or numeric resource IDs.
STATIC FUNCTION ValidateText( vVal )
RETURN iif( HB_ISNUMERIC( vVal ) .OR. ( HB_ISSTRING( vVal ) .AND. ! HB_ISNULL( vVal ) ), vVal, NIL )

#endif /* !__XHARBOUR__ && __HARBOUR__ > 3.0 */
