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
    Copyright 1999-2025, https://harbour.github.io/

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

/**
 * Function: InterpretHotKey
 * Converts a hotkey setting (modifier keys and virtual key) to a human-readable string.
 * Parameters:
 *   - setting: Encoded hotkey information, containing modifiers and virtual key code.
 *   - szKeyName: Output string where the formatted key combination will be stored.
 */
void InterpretHotKey( UINT setting, TCHAR *szKeyName )
{
   BOOL     Ctrl, Alt, Shift;
   UINT     WorkKey, uCode, uVKey;

#ifndef UNICODE
   LPSTR    lpString;
#else
   LPWSTR   lpString;
#endif
   uCode = ( setting & 0x0000FF00 ) >> 8; // Extract modifier codes
   uVKey = setting & 255;                 // Extract virtual key code
   *szKeyName = 0;

   // Check modifier states
   Ctrl = uCode & HOTKEYF_CONTROL;
   Alt = uCode & HOTKEYF_ALT;
   Shift = uCode & HOTKEYF_SHIFT;

   // Append modifier text to output
   lstrcat( szKeyName, Ctrl ? TEXT( "Ctrl + " ) : TEXT( "" ) );
   lstrcat( szKeyName, Shift ? TEXT( "Shift + " ) : TEXT( "" ) );
   lstrcat( szKeyName, Alt ? TEXT( "Alt + " ) : TEXT( "" ) );

#ifndef UNICODE
   lpString = szKeyName;
#else
   lpString = AnsiToWide( ( char * ) szKeyName );
#endif

   // Map virtual key to key name
   WorkKey = MapVirtualKey( uVKey, 0 );
   if( uCode & 0x00000008 )               // Check if extended key
   {
      WorkKey = 0x03000000 | ( WorkKey << 16 );
   }
   else
   {
      WorkKey = 0x02000000 | ( WorkKey << 16 );
   }

   // Get the key name text and append it to the existing string
   GetKeyNameText( WorkKey, lpString + lstrlen( lpString ), 100 );

#ifdef UNICODE
   hb_xfree( lpString );      // Free memory if Unicode conversion was used
#endif
}

/**
 * Function: C_GETHOTKEYNAME
 * Retrieves and returns the name of the currently set hotkey in a specified control.
 * Parameters:
 *   - Control's handle (parameter 1).
 * Returns:
 *   - A string with the human-readable name of the hotkey.
 */
HB_FUNC( C_GETHOTKEYNAME )
{
   WORD  wHotKey;
   TCHAR szKeyName[100];

#ifdef UNICODE
   LPSTR pStr;
#endif
   wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 ); // Get current hotkey setting
   InterpretHotKey( wHotKey, szKeyName ); // Convert hotkey setting to readable text
#ifndef UNICODE
   hb_retclen( szKeyName, 100 );          // Return result as ANSI string
#else
   pStr = WideToAnsi( szKeyName );
   hb_retclen( pStr, 100 );
   hb_xfree( pStr );
#endif
}

/**
 * Function: INITHOTKEYBOX
 * Initializes a hotkey control (edit box to capture hotkey input) with specified styles.
 * Parameters:
 *   - Parent window handle (parameter 1).
 *   - X, Y coordinates, width, and height of the control (parameters 2-5).
 *   - Visibility and tab-stop options (parameters 8 and 9).
 * Returns:
 *   - Handle to the created hotkey control.
 */
HB_FUNC( INITHOTKEYBOX )
{
   DWORD Style = WS_CHILD;

   // Apply visibility style if specified
   if( !hb_parl( 8 ) )
   {
      Style |= WS_VISIBLE;
   }

   // Apply tab-stop style if specified
   if( !hb_parl( 9 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the hotkey control
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            HOTKEY_CLASS,
            TEXT( "" ),
            Style,
            hb_parni( 2 ),                // x-coordinate
            hb_parni( 3 ),                // y-coordinate
            hb_parni( 4 ),                // width
            hb_parni( 5 ),                // height
            hmg_par_raw_HWND( 1 ),        // parent window handle
            NULL,
            GetInstance(),
            NULL
         )
   );
}

/**
 * Function: SETHOTKEYVALUE
 * Sets the hotkey value for a specified hotkey control.
 * Parameters:
 *   - hWnd: Handle to the hotkey control.
 *   - wHotKey: WORD value representing the hotkey to set.
 */
HB_FUNC( SETHOTKEYVALUE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   WORD  wHotKey = hmg_par_WORD( 2 );

   // Set hotkey if valid value is provided
   if( wHotKey != 0 )
   {
      SendMessage( hWnd, HKM_SETHOTKEY, wHotKey, 0 );
   }

   // Set invalid key combinations and restrict ALT-only combinations
   SendMessage( hWnd, HKM_SETRULES, ( WPARAM ) HKCOMB_NONE | HKCOMB_S, MAKELPARAM( HOTKEYF_ALT, 0 ) );
}

/**
 * Function: C_GETHOTKEYVALUE
 * Retrieves the current hotkey setting (key code and modifier keys) in a hotkey control.
 * Parameters:
 *   - hWnd: Handle to the hotkey control.
 * Returns:
 *   - Array with two elements: virtual key code and modifier keys.
 */
HB_FUNC( C_GETHOTKEYVALUE )
{
   WORD  wHotKey;
   UINT  uVirtualKeyCode;
   UINT  uModifiers;
   UINT  iModifierKeys;

   wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 );

   // Extract virtual key code and modifiers
   uVirtualKeyCode = LOBYTE( LOWORD( wHotKey ) );
   uModifiers = HIBYTE( LOWORD( wHotKey ) );

   // Map modifiers to respective MOD_* values
   iModifierKeys = ( ( uModifiers & HOTKEYF_CONTROL ) ? MOD_CONTROL : 0 ) | ( ( uModifiers & HOTKEYF_ALT ) ? MOD_ALT : 0 ) | ( ( uModifiers & HOTKEYF_SHIFT ) ? MOD_SHIFT : 0 );

   // Return array with virtual key code and modifiers
   hb_reta( 2 );
   HB_STORNI( ( UINT ) uVirtualKeyCode, -1, 1 );
   HB_STORNI( ( UINT ) iModifierKeys, -1, 2 );
}

/**
 * Function: C_GETHOTKEY
 * Retrieves the raw hotkey value from a specified hotkey control.
 * Parameters:
 *   - hWnd: Handle to the hotkey control.
 * Returns:
 *   - Raw WORD hotkey value.
 */
HB_FUNC( C_GETHOTKEY )
{
   WORD  wHotKey = ( WORD ) SendMessage( hmg_par_raw_HWND( 1 ), HKM_GETHOTKEY, 0, 0 );

   hmg_ret_WORD( wHotKey );               // Return raw hotkey value
}
