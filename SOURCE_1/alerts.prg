/*
 * Harbour MiniGUI Extended Edition
 * Alert and Message Dialog Utility Functions
 *
 * High-level abstraction for HMG Extended messaging system.
 * Provides wrappers for common dialog patterns.
 *
 * Copyright 2019-2026 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"
#include "i_winuser.ch"

// Standardized return values for ternary-state alert dialogs.
// Using these constants instead of literal integers improves code maintainability 
// and prevents "magic number" confusion when evaluating user input.
#define ALERT_YES     1
#define ALERT_NO      0
#define ALERT_CANCEL -1

/*
 * STATIC FUNCTION _GetTitleAndOptions( vInput, aDefaultOptions )
 *
 * Purpose:
 *  Parses the polymorphic 'Title' parameter. In HMG Extended, alert functions 
 *  often overload the title parameter to allow passing a simple string, 
 *  a numeric timeout, or a configuration array.
 *
 * Parameters:
 *  - vInput: Mixed. Can be a String (Title), Numeric (Timeout), or Array {Title, Options}.
 *  - aDefaultOptions: Array. Fallback button labels or configuration.
 *
 * Returns:
 *  - { cTitle, xOptions }: A standardized array for the internal _Alert engine.
 */
STATIC FUNCTION _GetTitleAndOptions( vInput, aDefaultOptions )
   LOCAL cTitle
   LOCAL xOptions := aDefaultOptions 

   DO CASE
      // Logic: If the user passes a number, they are likely specifying a 
      // timeout for an auto-closing alert rather than a window title.
      CASE ISNUMERIC( vInput )
         xOptions := vInput  

      // Logic: If an array is passed, the developer is providing both a 
      // custom title and specific button labels or a timeout value.
      CASE ISARRAY( vInput ) .AND. Len( vInput ) >= 1
         cTitle := vInput[ 1 ]
         IF Len( vInput ) >= 2
            xOptions := vInput[ 2 ]
         ENDIF

      // Logic: Default to treating the input as a standard window caption string.
      OTHERWISE
         cTitle := vInput
   ENDCASE

RETURN { cTitle, xOptions }

/*
 * FUNCTION AlertYesNo( cMessage, Title, lRevertDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Purpose:
 *  Displays a confirmation dialog with "Yes" and "No" buttons.
 *
 * Parameters:
 *  - cMessage: The text content of the dialog.
 *  - Title: Window caption or configuration array.
 *  - lRevertDefault: If .T., the "No" button receives initial focus.
 *  - xIcon: Custom icon resource.
 *  - nSize: Font size for the message text.
 *  - aColors: Array {ForeColor, BackColor, WindowColor}.
 *  - lAlwaysOnTop: If .T., the dialog stays above all other windows.
 *  - bInit: Optional code block to execute on dialog initialization.
 *
 * Returns:
 *  - Logical: .T. if "Yes" (IDOK) was selected, .F. otherwise.
 */
FUNCTION AlertYesNo( cMessage, Title, lRevertDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   // Retrieve localized labels for "Yes" and "No" from the HMG internal language system.
   // _HMG_aABMLangLabel[20] and [21] ensure the UI matches the user's locale.
   LOCAL aTitleData := _GetTitleAndOptions( Title, { '&' + _HMG_aABMLangLabel[ 20 ], '&' + _HMG_aABMLangLabel[ 21 ] } )
   LOCAL cTitle := aTitleData[ 1 ]
   LOCAL aOptions := aTitleData[ 2 ]

   // The result is compared against IDOK (Win32 standard for successful confirmation).
   // hb_defaultValue handles the focus logic: 2 for "No", 1 for "Yes".
RETURN ( _Alert( cMessage, aOptions, cTitle, , iif( hb_defaultValue( lRevertDefault, .F. ), 2, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit ) == IDOK )

/*
 * FUNCTION AlertYesNoCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Purpose:
 *  Displays a three-button dialog (Yes, No, Cancel) for complex decision points.
 *
 * Returns:
 *  - Numeric: ALERT_YES (1), ALERT_NO (0), or ALERT_CANCEL (-1).
 */
FUNCTION AlertYesNoCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   // Accesses internal language arrays for "Yes", "No", and "Cancel".
   LOCAL aTitleData := _GetTitleAndOptions( Title, { '&' + _HMG_aABMLangLabel[ 20 ], '&' + _HMG_aABMLangLabel[ 21 ], '&' + _HMG_aABMLangButton[ 13 ] } )
   LOCAL cTitle := aTitleData[ 1 ]
   LOCAL aOptions := aTitleData[ 2 ]

   // The final parameter .T. enables the 'X' close button on the window frame.
   SWITCH _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. )
      CASE 1 ; RETURN ( ALERT_YES )
      CASE 2 ; RETURN ( ALERT_NO )
   END SWITCH

   // If the user closes the window or clicks Cancel, we return the cancel constant.
RETURN ( ALERT_CANCEL )

/*
 * FUNCTION AlertRetryCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Purpose:
 *  Standard dialog for error recovery, allowing the user to attempt an action again.
 *
 * Returns:
 *  - Logical: .T. if "Retry" (IDOK) is clicked, .F. for "Cancel".
 */
FUNCTION AlertRetryCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   // Localization: _HMG_aLangButton[13] = Retry, [7] = Cancel.
   LOCAL aTitleData := _GetTitleAndOptions( Title, { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] } ) 
   LOCAL cTitle := aTitleData[ 1 ]
   LOCAL aOptions := aTitleData[ 2 ]

RETURN ( _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. ) == IDOK )

/*
 * FUNCTION AlertOkCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Purpose:
 *  Standard confirmation dialog for non-destructive operations.
 */
FUNCTION AlertOkCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   // Localization: _HMG_BRWLangButton[4] = OK, [3] = Cancel.
   LOCAL aTitleData := _GetTitleAndOptions( Title, { _HMG_BRWLangButton[ 4 ], _HMG_BRWLangButton[ 3 ] } )
   LOCAL cTitle := aTitleData[ 1 ]
   LOCAL aOptions := aTitleData[ 2 ]

RETURN ( _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. ) == IDOK )

/*
 * FUNCTION AlertExclamation( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Purpose:
 *  Displays a warning alert with an exclamation icon and triggers the system warning sound.
 */
FUNCTION AlertExclamation( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleData := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleData[ 2 ]
   LOCAL cTitle := aTitleData[ 1 ]

   // Play the Windows System Exclamation sound unless the developer explicitly silences it.
   IF ! hb_defaultValue( lNoSound, .F. )
      PlayExclamation()
   ENDIF

   // _HMG_MESSAGE[10] provides the localized "Warning" string.
RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 10 ] ), , , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*
 * FUNCTION AlertInfo( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Purpose:
 *  Displays an informational alert with the standard 'i' icon and system asterisk sound.
 */
FUNCTION AlertInfo( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleData := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleData[ 2 ]
   LOCAL cTitle := aTitleData[ 1 ]

   // Play the Windows System Asterisk (Information) sound.
   IF ! hb_defaultValue( lNoSound, .F. )
      PlayAsterisk()
   ENDIF

   // ICON_INFORMATION is a Win32 constant for the standard info icon.
RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 11 ] ), ICON_INFORMATION, , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*
 * FUNCTION AlertStop( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Purpose:
 *  Displays a critical error alert with the red 'X' icon and system hand sound.
 */
FUNCTION AlertStop( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleData := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleData[ 2 ]
   LOCAL cTitle := aTitleData[ 1 ]

   // Play the Windows System Hand (Critical Stop) sound.
   IF ! hb_defaultValue( lNoSound, .F. )
      PlayHand()
   ENDIF

   // ICON_STOP is a Win32 constant for the standard error icon.
RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 12 ] ), ICON_STOP, , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*
 * STATIC FUNCTION _Alert( cMessage, aOptions, cTitle, nIconType, nDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lClosable )
 *
 * Purpose:
 *  The core internal engine for all alert wrappers. It manages global HMG state 
 *  variables and prepares the environment for the low-level HMG_Alert call.
 *
 * Side Effects:
 *  - Updates _HMG_ModalDialogReturn to control initial button focus.
 *  - Updates _HMG_MainWindowFirst to handle focus logic in apps without an active main form.
 *  - Wraps the bInit code block to inject 'TopMost' window behavior.
 */
STATIC FUNCTION _Alert( cMessage, aOptions, cTitle, nIconType, nDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lClosable )
   LOCAL bOldInit

   // Sanitize inputs to prevent runtime errors in the underlying engine.
   __defaultNIL( @cMessage, "" )
   hb_default( @nDefault, 0 )

   // _HMG_ModalDialogReturn is a global state variable used by HMG to 
   // determine which control index receives focus upon window activation.
   IF nDefault > 0
      _HMG_ModalDialogReturn := nDefault
   ENDIF

   // Logic: If AlwaysOnTop is requested, we must ensure the window property 
   // is set during the ON INIT event. We wrap any existing bInit block 
   // provided by the user to preserve their logic while adding TopMost support.
   IF hb_defaultValue( lAlwaysOnTop, .T. )
      bOldInit := bInit
      bInit := {|| iif( HB_ISBLOCK( bOldInit ), Eval( bOldInit ), NIL ), This.TopMost := .T. }
   ENDIF

   // Logic: HMG Extended needs to know if a main window is active. If no form 
   // is currently 'Active' (type 'A'), we set _HMG_MainWindowFirst to .F. 
   // to ensure the alert dialog handles its own window parenting correctly.
   IF AScan( _HMG_aFormType, 'A' ) == 0
      _HMG_MainWindowFirst := .F.
   ENDIF

   // Final execution of the core HMG Extended alert engine.
RETURN HMG_Alert( cMessage, aOptions, cTitle, nIconType, xIcon, nSize, aColors, bInit, lClosable )
