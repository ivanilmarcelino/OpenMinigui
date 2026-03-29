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
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "minigui.ch"
#include "i_winuser.ch"

/* 
 * Configuration Constants 
 */

// Default icon identifier used when no specific user icon is provided.
#define MB_DEFAULT_ICON   0

// Standardized return values for ternary-state message boxes (Yes/No/Cancel).
// These provide a consistent numeric interface regardless of the underlying Win32 IDs.
#define MESSAGE_YES       1
#define MESSAGE_NO        0
#define MESSAGE_CANCEL   -1

/*
 * Function: _NormalizeMsg
 *
 * Purpose: Sanitizes and converts input data into a displayable string format.
 *
 * Parameters:
 *    cMsg - The message content. Supports String, Array, or other types via hb_ValToStr.
 *
 * Returns: A character string ready for display.
 *
 * Logic: 
 *    If an array is passed, the function iterates through it and concatenates 
 *    all elements. This allows developers to pass multi-line messages or 
 *    mixed-type data without manual conversion.
 */
STATIC FUNCTION _NormalizeMsg( cMsg )
   LOCAL cText := "", xVal

   // Ensure cMsg is at least an empty string if NIL is passed.
   __defaultNIL( @cMsg, "" )

   IF ISARRAY( cMsg )
      // Concatenate array elements into a single string block.
      FOR EACH xVal IN cMsg
         cText += hb_ValToStr( xVal )
      NEXT
      RETURN cText
   ENDIF

   // Convert non-string types (numbers, dates, etc.) to string.
RETURN hb_ValToStr( cMsg )

/*
 * Function: _ApplyBoxFlags
 *
 * Purpose: Combines base style flags with modality and Z-order settings.
 *
 * Parameters:
 *    nStyle - The initial Win32 MessageBox style bitmask.
 *    lSys   - Logical; .T. for System Modal (blocks all apps), .F. for Application Modal.
 *    lTop   - Logical; .T. to set the MB_TOPMOST flag.
 *
 * Returns: Updated numeric style bitmask.
 *
 * Logic:
 *    Uses bitwise OR (hb_bitOr) to merge flags. System modality is the default 
 *    behavior in this implementation to ensure user attention.
 */
STATIC FUNCTION _ApplyBoxFlags( nStyle, lSys, lTop )
   LOCAL lSystem  := hb_defaultValue( lSys, .T. )
   LOCAL lTopMost := hb_defaultValue( lTop, .T. )

   // Apply modality: System modal stays on top of all windows; App modal only blocks the current app.
   nStyle := hb_bitOr( nStyle, ;
                       iif( lSystem, MB_SYSTEMMODAL, MB_APPLMODAL ) )

   // Force the window to the foreground if requested.
   IF lTopMost
      nStyle := hb_bitOr( nStyle, MB_TOPMOST )
   ENDIF

RETURN nStyle

/*
 * Function: _BuildStyle
 *
 * Purpose: Constructs the complete Win32 style bitmask for the message box.
 *
 * Parameters:
 *    nButtons - The button configuration constant (e.g., MB_YESNO).
 *    nIcon    - Handle or ID for a custom icon.
 *    nDefIcon - The fallback system icon style (e.g., MB_ICONQUESTION).
 *    nDefBtn  - The 1-based index of the button that should have initial focus.
 * Returns: A combined numeric style bitmask.
 *
 * Logic:
 *    If nIcon is 0, it uses the system default icon. Otherwise, it flags the 
 *    style as MB_USERICON. The default button is calculated by shifting bits:
 *    Win32 defines MB_DEFBUTTON2 as 256 (0x100) and MB_DEFBUTTON3 as 512 (0x200).
 */
STATIC FUNCTION _BuildStyle( nButtons, nIcon, nDefIcon, nDefBtn )
   LOCAL nStyle
   LOCAL nBtn := hb_defaultValue( nDefBtn, 1 )
   LOCAL nIconStyle

   // Determine if we use a standard system icon or a user-defined resource.
   nIconStyle := iif( hb_defaultValue( nIcon, 0 ) == MB_DEFAULT_ICON, ;
                      nDefIcon, ;
                      MB_USERICON )

   nStyle := hb_bitOr( nButtons, nIconStyle )

   // Calculate default button flag: (Index - 1) * 256 maps 2->256, 3->512, etc.
   IF ISNUMERIC( nBtn ) .AND. nBtn > 1
      nStyle := hb_bitOr( nStyle, ( nBtn - 1 ) * 256 )
   ENDIF

RETURN nStyle

/*
 * Function: _MsgBox
 *
 * Purpose: The core internal engine for displaying message boxes.
 *
 * Parameters:
 *    cMsg   - The message text.
 *    cTitle - The window title.
 *    nStyle - Combined Win32 style flags.
 *    nIcon  - Icon resource identifier.
 *    lSys   - Modality flag.
 *    lTop   - Topmost flag.
 *
 * Returns: Numeric Win32 ID of the button pressed (e.g., IDOK, IDYES).
 *
 * Side Effects: Suspends program execution until the user closes the dialog.
 *
 * Logic:
 *    Uses MessageBoxIndirect for maximum flexibility, allowing custom icons 
 *    and precise control over window behavior.
 */
STATIC FUNCTION _MsgBox( cMsg, cTitle, nStyle, nIcon, lSys, lTop )
   cMsg   := _NormalizeMsg( cMsg )
   nStyle := _ApplyBoxFlags( nStyle, lSys, lTop )

RETURN MessageBoxIndirect( NIL, ;
                           cMsg, ;
                           hb_defaultValue( cTitle, "" ), ;
                           nStyle, ;
                           nIcon )

/*
 * Function: MsgYesNo
 *
 * Purpose: Displays a standard Yes/No confirmation dialog.
 *
 * Parameters:
 *    lRevDef - Logical; if .T., 'No' is the default button (prevents accidental 'Yes').
 *
 * Returns: .T. if the user clicked 'Yes', .F. otherwise.
 */
FUNCTION MsgYesNo ( cMsg, cTitle, lRevDef, nIcon, lSys, lTop )
   LOCAL nStyle

   // Build style with Question icon and optional default button reversal.
   nStyle := _BuildStyle( MB_YESNO, ;
                          nIcon, ;
                          MB_ICONQUESTION, ;
                          iif( hb_defaultValue( lRevDef, .F. ), 2, 1 ) )

RETURN ( _MsgBox( cMsg, cTitle, nStyle, nIcon, lSys, lTop ) == IDYES )

/*
 * Function: MsgYesNoCancel
 *
 * Purpose: Displays a dialog with 'Yes', 'No', and 'Cancel' buttons.
 *
 * Parameters:
 *    nDefBtn - Index (1-3) of the default focused button.
 *
 * Returns: MESSAGE_YES (1), MESSAGE_NO (0), or MESSAGE_CANCEL (-1).
 */
FUNCTION MsgYesNoCancel ( cMsg, cTitle, nIcon, lSys, nDefBtn, lTop )
   LOCAL nRes

   nRes := _MsgBox( cMsg, cTitle, ;
                    _BuildStyle( MB_YESNOCANCEL, nIcon, MB_ICONQUESTION, nDefBtn ), ;
                    nIcon, lSys, lTop )

   IF nRes == IDYES
      RETURN MESSAGE_YES
   ELSEIF nRes == IDNO
      RETURN MESSAGE_NO
   ENDIF

RETURN MESSAGE_CANCEL

/*
 * Function: MsgRetryCancel
 *
 * Purpose: Displays a dialog for retrying a failed operation.
 *
 * Returns: .T. if 'Retry' is clicked, .F. if 'Cancel' is clicked.
 */
FUNCTION MsgRetryCancel ( cMsg, cTitle, nIcon, lSys, nDefBtn, lTop )
RETURN ( _MsgBox( cMsg, cTitle, ;
                  _BuildStyle( MB_RETRYCANCEL, nIcon, MB_ICONQUESTION, nDefBtn ), ;
                  nIcon, lSys, lTop ) == IDRETRY )

/*
 * Function: MsgOkCancel
 *
 * Purpose: Displays a dialog for confirming an action with an option to abort.
 *
 * Returns: .T. if 'OK' is clicked, .F. if 'Cancel' is clicked.
 */
FUNCTION MsgOkCancel ( cMsg, cTitle, nIcon, lSys, nDefBtn, lTop )
RETURN ( _MsgBox( cMsg, cTitle, ;
                  _BuildStyle( MB_OKCANCEL, nIcon, MB_ICONQUESTION, nDefBtn ), ;
                  nIcon, lSys, lTop ) == IDOK )

/*
 * Function: MsgExclamation
 *
 * Purpose: Displays a warning message with an exclamation icon.
 *
 * Logic: Uses the localized "Warning" title from the HMG internal message array.
 *
 * Returns: IDOK (numeric).
 */
FUNCTION MsgExclamation ( cMsg, cTitle, nIcon, lSys, lTop )
RETURN _MsgBox( cMsg, ;
                hb_defaultValue( cTitle, _HMG_MESSAGE[ 10 ] ), ;
                _BuildStyle( MB_OK, nIcon, MB_ICONEXCLAMATION, 1 ), ;
                nIcon, lSys, lTop )

/*
 * Function: MsgInfo
 *
 * Purpose: Displays an informational message with an 'i' icon.
 *
 * Logic: Uses the localized "Information" title from the HMG internal message array.
 *
 * Returns: IDOK (numeric).
 */
FUNCTION MsgInfo ( cMsg, cTitle, nIcon, lSys, lTop )
RETURN _MsgBox( cMsg, ;
                hb_defaultValue( cTitle, _HMG_MESSAGE[ 11 ] ), ;
                _BuildStyle( MB_OK, nIcon, MB_ICONINFORMATION, 1 ), ;
                nIcon, lSys, lTop )

/*
 * Function: MsgStop
 *
 * Purpose: Displays a critical error or stop message with a red 'X' icon.
 *
 * Logic: Uses the localized "Stop" title from the HMG internal message array.
 *
 * Returns: IDOK (numeric).
 */
FUNCTION MsgStop ( cMsg, cTitle, nIcon, lSys, lTop )
RETURN _MsgBox( cMsg, ;
                hb_defaultValue( cTitle, _HMG_MESSAGE[ 12 ] ), ;
                _BuildStyle( MB_OK, nIcon, MB_ICONSTOP, 1 ), ;
                nIcon, lSys, lTop )

/*
 * Function: MsgBox
 *
 * Purpose: The simplest message box implementation, showing only an OK button.
 *
 * Parameters:
 *    cMsg   - The message to display.
 *    cTitle - Optional window title.
 *
 * Returns: IDOK (numeric).
 */
FUNCTION MsgBox ( cMsg, cTitle, lSys, lTop )
RETURN _MsgBox( cMsg, cTitle, MB_OK, NIL, lSys, lTop )
