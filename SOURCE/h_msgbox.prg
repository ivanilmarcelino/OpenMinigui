/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

MsgBox code rewrotten by Jacek Kubica <kubica@wssk.wroc.pl>
(c) 2006 HMG Experimental Build 16g

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "minigui.ch"
#include "i_winuser.ch"

* Constants for default button values
#define DEFAULT_BUTTON_1 1
#define DEFAULT_BUTTON_2 2
#define DEFAULT_BUTTON_3 3

* Constants for button-click return values
#define MESSAGE_YES      1
#define MESSAGE_NO       0
#define MESSAGE_CANCEL  -1

/*-----------------------------------------------------------------------------*
* STATIC FUNCTION _MsgBox( cMessage, cTitle, nStyle, nIcon, lSysModal, lTopMost )
* 
* Description:
*   This is a helper function that encapsulates the actual call to the Windows
*   MessageBoxIndirect API. It handles argument defaulting, type conversion,
*   and modal behavior selection.  It is declared as STATIC, meaning it is only
*   accessible within this source file.
* 
* Parameters:
*   cMessage  (STRING): The message to display in the message box.  If it's not a string,
*                       it will be converted to a string. If it's an array, all elements
*                       will be concatenated into a single string.
*   cTitle    (STRING): The title of the message box.
*   nStyle    (NUMERIC): The style flags for the message box (e.g., MB_YESNO, MB_OKCANCEL).
*   nIcon     (NUMERIC): The icon to display in the message box (e.g., MB_ICONQUESTION).
*   lSysModal (LOGICAL):  If .T. (default), the message box is system modal (disables all
*                       windows). If .F., it's application modal (disables only the
*                       application's windows).
*   lTopMost  (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: The result of the MessageBoxIndirect call (e.g., IDOK, IDCANCEL, IDYES, IDNO).
*-----------------------------------------------------------------------------*/
STATIC FUNCTION _MsgBox( cMessage, cTitle, nStyle, nIcon, lSysModal, lTopMost )
   LOCAL cText

   __defaultNIL( @cMessage, "" )
   __defaultNIL( @cTitle, "" )

   IF ! ISCHARACTER( cMessage )
      IF ISARRAY( cMessage )
         cText := ""
         AEval( cMessage, {| x | cText += hb_ValToStr( x ) } )
         cMessage := cText
      ELSE
         cMessage := hb_ValToStr( cMessage )
      ENDIF
   ENDIF

   nStyle += iif( hb_defaultValue( lSysModal, .T. ), MB_SYSTEMMODAL, MB_APPLMODAL )

   IF hb_defaultValue( lTopMost, .T. )
      nStyle += MB_TOPMOST
   ENDIF

RETURN MessageBoxIndirect( NIL, cMessage, cTitle, nStyle, nIcon )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgYesNo ( Message, Title, RevertDefault, nIcon, lSysModal, lTopMost )
* 
* Description:
*   Displays a message box with "Yes" and "No" buttons.
* 
* Parameters:
*   Message       (STRING): The message to display.
*   Title         (STRING): The title of the message box.
*   RevertDefault (LOGICAL): If .T., the "No" button is the default. Otherwise, "Yes" is the default.
*   nIcon         (NUMERIC): The icon to display (e.g., MB_ICONQUESTION). If NIL or 0, a question mark icon is used.
*   lSysModal     (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   lTopMost      (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   LOGICAL: .T. if the user clicked "Yes", .F. if the user clicked "No".
*-----------------------------------------------------------------------------*/
FUNCTION MsgYesNo ( Message, Title, RevertDefault, nIcon, lSysModal, lTopMost )
   LOCAL nStyle := MB_YESNO

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONQUESTION, MB_USERICON )

   IF hb_defaultValue( RevertDefault, .F. )
      nStyle += MB_DEFBUTTON2
   ENDIF

RETURN ( _MsgBox( Message, Title, nStyle, nIcon, lSysModal, lTopMost ) == IDYES )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgYesNoCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
* 
* Description:
*   Displays a message box with "Yes", "No", and "Cancel" buttons.
* 
* Parameters:
*   Message        (STRING): The message to display.
*   Title          (STRING): The title of the message box.
*   nIcon          (NUMERIC): The icon to display (e.g., MB_ICONQUESTION). If NIL or 0, a question mark icon is used.
*   lSysModal      (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   nDefaultButton (NUMERIC): Specifies which button is the default:
*                             DEFAULT_BUTTON_1 (1): "Yes" (default).
*                             DEFAULT_BUTTON_2 (2): "No".
*                             DEFAULT_BUTTON_3 (3): "Cancel".
*   lTopMost       (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: MESSAGE_YES (1) if the user clicked "Yes".
*            MESSAGE_NO (0) if the user clicked "No".
*            MESSAGE_CANCEL (-1) if the user clicked "Cancel".
*-----------------------------------------------------------------------------*/
FUNCTION MsgYesNoCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
   LOCAL nStyle := MB_YESNOCANCEL

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONQUESTION, MB_USERICON )

   SWITCH hb_defaultValue( nDefaultButton, DEFAULT_BUTTON_1 )

   CASE DEFAULT_BUTTON_2
      nStyle += MB_DEFBUTTON2
      EXIT
   CASE DEFAULT_BUTTON_3
      nStyle += MB_DEFBUTTON3

   END SWITCH

   SWITCH _MsgBox( Message, Title, nStyle, nIcon, lSysModal, lTopMost )

   CASE IDYES
      RETURN ( MESSAGE_YES )
   CASE IDNO
      RETURN ( MESSAGE_NO )

   END SWITCH

RETURN ( MESSAGE_CANCEL )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgRetryCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
* 
* Description:
*   Displays a message box with "Retry" and "Cancel" buttons.
* 
* Parameters:
*   Message        (STRING): The message to display.
*   Title          (STRING): The title of the message box.
*   nIcon          (NUMERIC): The icon to display (e.g., MB_ICONQUESTION). If NIL or 0, a question mark icon is used.
*   lSysModal      (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   nDefaultButton (NUMERIC): Specifies which button is the default:
*                             DEFAULT_BUTTON_1 (1): "Retry" (default).
*                             DEFAULT_BUTTON_2 (2): "Cancel".
*   lTopMost       (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   LOGICAL: .T. if the user clicked "Retry", .F. if the user clicked "Cancel".
*-----------------------------------------------------------------------------*/
FUNCTION MsgRetryCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
   LOCAL nStyle := MB_RETRYCANCEL

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONQUESTION, MB_USERICON )

   IF hb_defaultValue( nDefaultButton, DEFAULT_BUTTON_1 ) == DEFAULT_BUTTON_2
      nStyle += MB_DEFBUTTON2
   ENDIF

RETURN ( _MsgBox( Message, Title, nStyle, nIcon, lSysModal, lTopMost ) == IDRETRY )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgOkCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
* 
* Description:
*   Displays a message box with "OK" and "Cancel" buttons.
* 
* Parameters:
*   Message        (STRING): The message to display.
*   Title          (STRING): The title of the message box.
*   nIcon          (NUMERIC): The icon to display (e.g., MB_ICONQUESTION). If NIL or 0, a question mark icon is used.
*   lSysModal      (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   nDefaultButton (NUMERIC): Specifies which button is the default:
*                             DEFAULT_BUTTON_1 (1): "OK" (default).
*                             DEFAULT_BUTTON_2 (2): "Cancel".
*   lTopMost       (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   LOGICAL: .T. if the user clicked "OK", .F. if the user clicked "Cancel".
*-----------------------------------------------------------------------------*/
FUNCTION MsgOkCancel ( Message, Title, nIcon, lSysModal, nDefaultButton, lTopMost )
   LOCAL nStyle := MB_OKCANCEL

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONQUESTION, MB_USERICON )

   IF hb_defaultValue( nDefaultButton, DEFAULT_BUTTON_1 ) == DEFAULT_BUTTON_2
      nStyle += MB_DEFBUTTON2
   ENDIF

RETURN ( _MsgBox( Message, Title, nStyle, nIcon, lSysModal, lTopMost ) == IDOK )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgExclamation ( Message, Title, nIcon, lSysModal, lTopMost )
* 
* Description:
*   Displays a message box with an exclamation icon and an "OK" button.
* 
* Parameters:
*   Message   (STRING): The message to display.
*   Title     (STRING): The title of the message box. If NIL, a default title from _HMG_MESSAGE[10] is used.
*   nIcon     (NUMERIC): The icon to display. If NIL or 0, an exclamation icon is used.
*   lSysModal (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   lTopMost  (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: The result of the MessageBoxIndirect call (IDOK).
*-----------------------------------------------------------------------------*/
FUNCTION MsgExclamation ( Message, Title, nIcon, lSysModal, lTopMost )
   LOCAL nStyle := MB_OK

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONEXCLAMATION, MB_USERICON )

RETURN _MsgBox( Message, hb_defaultValue( Title, _HMG_MESSAGE [10] ), nStyle, nIcon, lSysModal, lTopMost )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgInfo ( Message, Title, nIcon, lSysModal, lTopMost )
* 
* Description:
*   Displays a message box with an information icon and an "OK" button.
* 
* Parameters:
*   Message   (STRING): The message to display.
*   Title     (STRING): The title of the message box. If NIL, a default title from _HMG_MESSAGE[11] is used.
*   nIcon     (NUMERIC): The icon to display. If NIL or 0, an information icon is used.
*   lSysModal (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   lTopMost  (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: The result of the MessageBoxIndirect call (IDOK).
*-----------------------------------------------------------------------------*/
FUNCTION MsgInfo ( Message, Title, nIcon, lSysModal, lTopMost )
   LOCAL nStyle := MB_OK

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONINFORMATION, MB_USERICON )

RETURN _MsgBox( Message, hb_defaultValue( Title, _HMG_MESSAGE [11] ), nStyle, nIcon, lSysModal, lTopMost )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgStop ( Message, Title, nIcon, lSysModal, lTopMost )
* 
* Description:
*   Displays a message box with a stop (error) icon and an "OK" button.
* 
* Parameters:
*   Message   (STRING): The message to display.
*   Title     (STRING): The title of the message box. If NIL, a default title from _HMG_MESSAGE[12] is used.
*   nIcon     (NUMERIC): The icon to display. If NIL or 0, a stop icon is used.
*   lSysModal (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   lTopMost  (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: The result of the MessageBoxIndirect call (IDOK).
*-----------------------------------------------------------------------------*/
FUNCTION MsgStop ( Message, Title, nIcon, lSysModal, lTopMost )
   LOCAL nStyle := MB_OK

   nStyle += iif( Empty( hb_defaultValue( nIcon, 0 ) ), MB_ICONSTOP, MB_USERICON )

RETURN _MsgBox( Message, hb_defaultValue( Title, _HMG_MESSAGE [12] ), nStyle, nIcon, lSysModal, lTopMost )

/*-----------------------------------------------------------------------------*
* FUNCTION MsgBox ( Message, Title, lSysModal, lTopMost )
* 
* Description:
*   Displays a simple message box with an "OK" button. This is the most basic
*   message box function.
* 
* Parameters:
*   Message   (STRING): The message to display.
*   Title     (STRING): The title of the message box.
*   lSysModal (LOGICAL): If .T. (default), the message box is system modal. If .F., it's application modal.
*   lTopMost  (LOGICAL): If .T. (default), the message box is displayed as a top-most window.
* 
* Return Value:
*   NUMERIC: The result of the MessageBoxIndirect call (IDOK).
*-----------------------------------------------------------------------------*/
FUNCTION MsgBox ( Message, Title, lSysModal, lTopMost )
RETURN _MsgBox( Message, Title, MB_OK, NIL, lSysModal, lTopMost )
