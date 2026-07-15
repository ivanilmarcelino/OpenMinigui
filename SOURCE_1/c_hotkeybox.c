/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   HOTKEYBOX Control Source Code
   Copyright 2006 Grigory Filatov <gfilatov@gmail.com>

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
#include <mgdefs.h>
#include <commctrl.h>

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
LPSTR       WideToAnsi( LPWSTR );
#endif
HINSTANCE   GetInstance( void );

/*
 * Function: InterpretHotKey
 * -------------------------
 * Translates a numeric hotkey bitmask into a localized, human-readable string.
 *
 * Parameters:
 *    - setting: A UINT containing the modifier flags in the high byte and 
 *               the virtual key code in the low byte.
 *    - szKeyName: A pointer to a TCHAR buffer that receives the formatted string.
 *
 * Logic:
 *    The function extracts modifier flags (Ctrl, Alt, Shift) and uses the 
 *    Windows API GetKeyNameText to retrieve the localized name of the primary key.
 */
void InterpretHotKey( UINT setting, TCHAR *szKeyName )
{
   BOOL  Ctrl, Alt, Shift;
   UINT  scanCode, uCode, uVKey, WorkKey;
   int   len;

   // Extract the modifier flags from the high byte (bits 8-15)
   uCode = ( setting & 0x0000FF00 ) >> 8;

   // Extract the virtual key code from the low byte (bits 0-7)
   uVKey = setting & 255;
   *szKeyName = 0;

   // Determine which modifier keys are active based on HotKey control flags
   Ctrl = uCode & HOTKEYF_CONTROL;
   Alt = uCode & HOTKEYF_ALT;
   Shift = uCode & HOTKEYF_SHIFT;

   // Build the prefix string for the hotkey combination
   lstrcat( szKeyName, Ctrl ? TEXT( "Ctrl + " ) : TEXT( "" ) );
   lstrcat( szKeyName, Shift ? TEXT( "Shift + " ) : TEXT( "" ) );
   lstrcat( szKeyName, Alt ? TEXT( "Alt + " ) : TEXT( "" ) );

   // Convert the virtual key code to a hardware-dependent scan code for GetKeyNameText
   scanCode = MapVirtualKey( uVKey, 0 );

   // Construct the lParam for GetKeyNameText:
   // Bits 16-23: Scan code
   // Bit 24: Extended key flag (e.g., right-side Alt or Ctrl)
   WorkKey = ( scanCode << 16 ) | ( ( uCode & HOTKEYF_EXT ) ? 0x01000000 : 0 );

   // Append the localized name of the key (e.g., "A", "F1", "Home") to the modifiers
   len = lstrlen( szKeyName );
   GetKeyNameText( WorkKey, szKeyName + len, 100 - len );
}

/*
 * Function: C_GETHOTKEYNAME
 * -------------------------
 * Harbour wrapper to retrieve the display name of the current hotkey in a control.
 *
 * Parameters:
 *    - hWnd (via hb_par): Handle to the HotKeyBox control.
 *
 * Returns:
 *    - Character: The human-readable string (e.g., "Ctrl + Alt + K").
 */
HB_FUNC( C_GETHOTKEYNAME )
{
   WORD  wHotKey;
   TCHAR szKeyName[100];

#ifdef UNICODE
   LPSTR pStr;
#endif

   // Request the current hotkey combination from the Win32 control
   wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 );

   // Convert the raw WORD value into a readable string
   InterpretHotKey( wHotKey, szKeyName );

#ifndef UNICODE
   hb_retclen( szKeyName, 100 );
#else
   // Handle Unicode to ANSI conversion for Harbour's internal string management if necessary
   pStr = WideToAnsi( szKeyName );
   hb_retclen( pStr, 100 );
   hb_xfree( pStr );
#endif
}

/*
 * Function: INITHOTKEYBOX
 * -----------------------
 * Initializes and creates the physical Win32 HotKey control.
 *
 * Parameters:
 *    - 1: Parent Window Handle
 *    - 2, 3: Col, Row (x, y)
 *    - 4, 5: Width, Height
 *    - 8: Invisible flag (Logical)
 *    - 9: NoTabStop flag (Logical)
 *
 * Returns:
 *    - HWND: The handle of the newly created control.
 */
HB_FUNC( INITHOTKEYBOX )
{
   DWORD Style = WS_CHILD;

   // HMG logic: If the 'Invisible' parameter is false, add WS_VISIBLE style
   if( !hb_parl( 8 ) )
   {
      Style |= WS_VISIBLE;
   }

   // HMG logic: If the 'NoTabStop' parameter is false, add WS_TABSTOP style
   if( !hb_parl( 9 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the control using the standard Windows HotKey class
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            HOTKEY_CLASS,  // Predefined Common Control class
            TEXT( "" ),
            Style,
            hb_parni( 2 ),
            hb_parni( 3 ),
            hb_parni( 4 ),
            hb_parni( 5 ),
            hmg_par_raw_HWND( 1 ),
            NULL,
            GetInstance(),
            NULL
         )
   );
}

/*
 * Function: SETHOTKEYVALUE
 * ------------------------
 * Programmatically sets the key combination for the HotKeyBox.
 *
 * Parameters:
 *    - 1: Control Handle (HWND)
 *    - 2: Hotkey value (WORD)
 *
 * Side Effects:
 *    - Updates the UI of the control to show the new key.
 *    - Defines rules to prevent invalid combinations (like Alt-only).
 */
HB_FUNC( SETHOTKEYVALUE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   WORD  wHotKey = hmg_par_WORD( 2 );

   // Apply the hotkey value if it is non-zero
   if( wHotKey != 0 )
   {
      SendMessage( hWnd, HKM_SETHOTKEY, wHotKey, 0 );
   }

   // HKM_SETRULES defines invalid combinations.
   // Here we prevent 'None' (HKCOMB_NONE) and 'Shift-only' (HKCOMB_S)
   // by forcing them to use Alt (HOTKEYF_ALT) instead.
   SendMessage( hWnd, HKM_SETRULES, ( WPARAM ) HKCOMB_NONE | HKCOMB_S, MAKELPARAM( HOTKEYF_ALT, 0 ) );
}

/*
 * Function: C_GETHOTKEYVALUE
 * --------------------------
 * Retrieves the hotkey components as a Harbour array.
 *
 * Parameters:
 *    - 1: Control Handle (HWND)
 *
 * Returns:
 *    - Array: { nVirtualKeyCode, nModifierFlags }
 *      Note: Modifier flags are mapped to MOD_* constants used by RegisterHotKey.
 */
HB_FUNC( C_GETHOTKEYVALUE )
{
   WORD  wHotKey;
   UINT  uVirtualKeyCode;
   UINT  uModifiers;
   UINT  iModifierKeys;

   // Get the raw WORD from the control
   wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 );

   // Extract components: Low byte is Virtual Key, High byte is Modifiers
   uVirtualKeyCode = LOBYTE( LOWORD( wHotKey ) );
   uModifiers = HIBYTE( LOWORD( wHotKey ) );

   // Map HotKey control flags (HOTKEYF_*) to standard Windows Modifier flags (MOD_*)
   // This ensures compatibility with the RegisterHotKey API.
   iModifierKeys = ( ( uModifiers & HOTKEYF_CONTROL ) ? MOD_CONTROL : 0 ) | ( ( uModifiers & HOTKEYF_ALT ) ? MOD_ALT : 0 ) | ( ( uModifiers & HOTKEYF_SHIFT ) ? MOD_SHIFT : 0 );

   // Return a 2-element array to the Harbour application
   hb_reta( 2 );
   HB_STORNI( ( UINT ) uVirtualKeyCode, -1, 1 );
   HB_STORNI( ( UINT ) iModifierKeys, -1, 2 );
}

/*
 * Function: C_GETHOTKEY
 * ---------------------
 * Retrieves the raw WORD value of the hotkey.
 *
 * Parameters:
 *    - 1: Control Handle (HWND)
 *
 * Returns:
 *    - Numeric: The raw WORD value (Modifiers + VKey).
 */
HB_FUNC( C_GETHOTKEY )
{
   WORD  wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 );

   // Return the raw value directly for low-level manipulation
   hmg_ret_WORD( wHotKey );
}
