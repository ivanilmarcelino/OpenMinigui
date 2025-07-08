/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2019-2025 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"
#include "i_winuser.ch"

// Define constants for button return value
#define ALERT_YES     1
#define ALERT_NO      0
#define ALERT_CANCEL -1

/*-----------------------------------------------------------------------------
 * STATIC FUNCTION _GetTitleAndOptions( vInput, aDefaultOptions )
 *
 * Description:
 *  This function extracts the title and options from the input parameter vInput.
 *  It handles different input types for vInput:
 *   - Numeric:  vInput is treated as the wait time for the alert.
 *   - Array:    vInput[1] is the title, and vInput[2] is either the wait time (numeric) or an options array (e.g., button labels).
 *   - Other:    vInput is treated as the title.
 *  If vInput[2] is not an array, aDefaultOptions is used as the options array.
 *
 * Parameters:
 *  - vInput:          Mixed type. Can be a string (title), a number (wait time), or an array (title and options).
 *  - aDefaultOptions: Array. The default options array to use if vInput[2] is not an array.
 *
 * Returns:
 *  - Array: An array containing the extracted title (string) and options (array or number).
 *           The array structure is { cTitle, xOptions }.
 *
 * Purpose:
 *  This function simplifies the handling of different parameter formats for alert dialogs,
 *  allowing the user to specify the title and options in various ways.
 *-----------------------------------------------------------------------------*/
STATIC FUNCTION _GetTitleAndOptions( vInput, aDefaultOptions )
   LOCAL cTitle
   LOCAL xOptions := aDefaultOptions // Can be a number (wait time) or an array (options)

   DO CASE
      CASE ISNUMERIC( vInput )
         xOptions := vInput  // Wait time
      CASE ISARRAY( vInput ) .AND. Len( vInput ) >= 1
         cTitle := vInput[ 1 ]
         IF Len( vInput ) >= 2
            xOptions := vInput[ 2 ]
         ENDIF
      OTHERWISE
         cTitle := vInput
   ENDCASE

RETURN { cTitle, xOptions }

/*-----------------------------------------------------------------------------
 * FUNCTION AlertYesNo( cMessage, Title, lRevertDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Description:
 *  Displays a Yes/No alert dialog to the user.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - lRevertDefault:  Logical. If .T., the "No" button is the default button. Otherwise, "Yes" is the default.
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *
 * Returns:
 *  - Logical: .T. if the user clicked "Yes", .F. if the user clicked "No".
 *
 * Purpose:
 *  Provides a simple way to display a Yes/No confirmation dialog to the user.
 *  The lRevertDefault parameter allows for easy customization of the default button.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertYesNo( cMessage, Title, lRevertDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title, { '&' + _HMG_aABMLangLabel[ 20 ], '&' + _HMG_aABMLangLabel[ 21 ] } )
   LOCAL cTitle := aTitleInfo[ 1 ]
   LOCAL aOptions := aTitleInfo[ 2 ]

RETURN ( _Alert( cMessage, aOptions, cTitle, , iif( hb_defaultValue( lRevertDefault, .F. ), 2, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit ) == IDOK )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertYesNoCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Description:
 *  Displays a Yes/No/Cancel alert dialog to the user.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - nDefaultButton:  Numeric. Specifies which button is the default button (1=Yes, 2=No, other=Cancel).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *
 * Returns:
 *  - Numeric: ALERT_YES (1) if the user clicked "Yes", ALERT_NO (0) if the user clicked "No", ALERT_CANCEL (-1) if the user clicked "Cancel".
 *
 * Purpose:
 *  Provides a way to display a Yes/No/Cancel confirmation dialog to the user, allowing for more complex decision-making.
 *  The nDefaultButton parameter allows specifying the default button.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertYesNoCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title, { '&' + _HMG_aABMLangLabel[ 20 ], '&' + _HMG_aABMLangLabel[ 21 ], '&' + _HMG_aABMLangButton[ 13 ] } )
   LOCAL cTitle := aTitleInfo[ 1 ]
   LOCAL aOptions := aTitleInfo[ 2 ]

   SWITCH _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. )

   CASE 1

      RETURN ( ALERT_YES )

   CASE 2

      RETURN ( ALERT_NO )

   END SWITCH

RETURN ( ALERT_CANCEL )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertRetryCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Description:
 *  Displays a Retry/Cancel alert dialog to the user.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - nDefaultButton:  Numeric. Specifies which button is the default button (1=Retry, other=Cancel).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *
 * Returns:
 *  - Logical: .T. if the user clicked "Retry", .F. if the user clicked "Cancel".
 *
 * Purpose:
 *  Provides a way to display a Retry/Cancel confirmation dialog to the user, typically used when an operation has failed and the user can retry it.
 *  The nDefaultButton parameter allows specifying the default button.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertRetryCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title, { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] } ) // P.D. July 3, 2021
   LOCAL cTitle := aTitleInfo[ 1 ]
   LOCAL aOptions := aTitleInfo[ 2 ]

RETURN ( _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. ) == IDOK )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertOkCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
 *
 * Description:
 *  Displays an OK/Cancel alert dialog to the user.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - nDefaultButton:  Numeric. Specifies which button is the default button (1=OK, other=Cancel).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *
 * Returns:
 *  - Logical: .T. if the user clicked "OK", .F. if the user clicked "Cancel".
 *
 * Purpose:
 *  Provides a way to display an OK/Cancel confirmation dialog to the user, typically used when the user needs to confirm an action or dismiss a message.
 *  The nDefaultButton parameter allows specifying the default button.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertOkCancel( cMessage, Title, nDefaultButton, xIcon, nSize, aColors, lAlwaysOnTop, bInit )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title, { _HMG_BRWLangButton[ 4 ], _HMG_BRWLangButton[ 3 ] } )
   LOCAL cTitle := aTitleInfo[ 1 ]
   LOCAL aOptions := aTitleInfo[ 2 ]

RETURN ( _Alert( cMessage, aOptions, cTitle, , hb_defaultValue( nDefaultButton, 1 ), xIcon, nSize, aColors, lAlwaysOnTop, bInit, .T. ) == IDOK )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertExclamation( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Description:
 *  Displays an exclamation alert dialog to the user.  This type of alert typically indicates a warning or potential problem.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *  - lNoSound:        Logical. If .T., no sound will be played when the alert dialog is displayed.
 *
 * Returns:
 *  - Logical: .T. if the alert was displayed successfully.
 *
 * Purpose:
 *  Provides a way to display an exclamation alert dialog to the user, indicating a warning or potential problem.
 *  The lNoSound parameter allows disabling the default exclamation sound.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertExclamation( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleInfo[ 2 ]
   LOCAL cTitle := aTitleInfo[ 1 ]

   IF Empty( lNoSound )
      PlayExclamation()
   ENDIF

RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 10 ] ), , , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertInfo( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Description:
 *  Displays an information alert dialog to the user. This type of alert typically provides general information or status updates.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *  - lNoSound:        Logical. If .T., no sound will be played when the alert dialog is displayed.
 *
 * Returns:
 *  - Logical: .T. if the alert was displayed successfully.
 *
 * Purpose:
 *  Provides a way to display an information alert dialog to the user, providing general information or status updates.
 *  The lNoSound parameter allows disabling the default information sound.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertInfo( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleInfo[ 2 ]
   LOCAL cTitle := aTitleInfo[ 1 ]

   IF Empty( lNoSound )
      PlayAsterisk()
   ENDIF

RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 11 ] ), ICON_INFORMATION, , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*-----------------------------------------------------------------------------
 * FUNCTION AlertStop( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
 *
 * Description:
 *  Displays a stop alert dialog to the user. This type of alert typically indicates a critical error or a situation that requires immediate attention.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - Title:           Mixed. The title of the alert dialog. Can be a string, a number (wait time), or an array (title and options).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *  - lNoSound:        Logical. If .T., no sound will be played when the alert dialog is displayed.
 *
 * Returns:
 *  - Logical: .T. if the alert was displayed successfully.
 *
 * Purpose:
 *  Provides a way to display a stop alert dialog to the user, indicating a critical error or a situation that requires immediate attention.
 *  The lNoSound parameter allows disabling the default stop sound.
 *-----------------------------------------------------------------------------*/
FUNCTION AlertStop( cMessage, Title, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lNoSound )
   LOCAL aTitleInfo := _GetTitleAndOptions( Title )
   LOCAL nWaitSec := aTitleInfo[ 2 ]
   LOCAL cTitle := aTitleInfo[ 1 ]

   IF Empty( lNoSound )
      PlayHand()
   ENDIF

RETURN _Alert( cMessage, nWaitSec, hb_defaultValue( cTitle, _HMG_MESSAGE[ 12 ] ), ICON_STOP, , xIcon, nSize, aColors, lAlwaysOnTop, bInit )

/*-----------------------------------------------------------------------------
 * STATIC FUNCTION _Alert( cMessage, aOptions, cTitle, nIconType, nDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lClosable )
 *
 * Description:
 *  Core function to display the alert dialog. This function handles the actual creation and display of the alert dialog.
 *
 * Parameters:
 *  - cMessage:        String. The message to display in the alert dialog.
 *  - aOptions:        Array. An array of button labels to display in the alert dialog.
 *  - cTitle:          String. The title of the alert dialog.
 *  - nIconType:       Numeric. Specifies the icon type to display in the alert dialog (e.g., ICON_INFORMATION, ICON_STOP).
 *  - nDefault:        Numeric. Specifies which button is the default button (1-based index).
 *  - xIcon:           Mixed. Specifies the icon to display in the alert dialog. Can be a numeric icon constant or a bitmap filename.
 *  - nSize:           Numeric. The size of the alert dialog.
 *  - aColors:         Array. An array of colors to use for the alert dialog.
 *  - lAlwaysOnTop:    Logical. If .T., the alert dialog will always be on top of other windows.
 *  - bInit:           Block. A code block to execute when the alert dialog is initialized.
 *  - lClosable:       Logical. If .T., the alert dialog can be closed by the user (e.g., using the close button).
 *
 * Returns:
 *  - Numeric: The ID of the button that was clicked by the user.
 *
 * Purpose:
 *  This function is the core implementation for displaying alert dialogs. It takes all the necessary parameters and calls the HMG_Alert function to create and display the dialog.
 *-----------------------------------------------------------------------------*/
STATIC FUNCTION _Alert( cMessage, aOptions, cTitle, nIconType, nDefault, xIcon, nSize, aColors, lAlwaysOnTop, bInit, lClosable )
   __defaultNIL( @cMessage, "" )
   hb_default( @nDefault, 0 )

   IF ! Empty( nDefault )
      _HMG_ModalDialogReturn := nDefault
   ENDIF

   IF hb_defaultValue( lAlwaysOnTop, .T. ) .AND. Empty( bInit )
      bInit := {|| This.TopMost := .T. }
   ENDIF

   IF AScan( _HMG_aFormType, 'A' ) == 0
      _HMG_MainWindowFirst := .F.
   ENDIF

RETURN HMG_Alert( cMessage, aOptions, cTitle, nIconType, xIcon, nSize, aColors, bInit, lClosable )
