/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

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

/* 
 * External Win32 API declarations not always present in standard headers
 * or requiring specific dynamic linking.
 */
int WINAPI        MessageBoxTimeout( HWND, LPCTSTR, LPCTSTR, UINT, WORD, DWORD );
HINSTANCE         GetInstance( void );
extern HB_PTRUINT wapi_GetProcAddress( HMODULE, LPCSTR );

/* ------------------------------------------------------------------------ */
/* Helper Macros                                                            */
/* These macros simplify the extraction of parameters from the Harbour      */
/* virtual machine stack, providing default values when parameters are      */
/* missing or of the wrong type.                                            */
/* ------------------------------------------------------------------------ */

// Safely retrieves a Window Handle (HWND) from a Harbour parameter.
#define _GET_HWND( n, def )   ( HB_ISNUM( n ) ? hmg_par_raw_HWND( n ) : ( def ) )

// Safely retrieves an Instance Handle (HINSTANCE) from a Harbour parameter.
#define _GET_HINST( n, def )  ( HB_ISNUM( n ) ? hmg_par_raw_HINSTANCE( n ) : ( def ) )

// Safely retrieves a 32-bit unsigned integer (DWORD) from a Harbour parameter.
#define _GET_DWORD( n, def )  ( HB_ISNUM( n ) ? hmg_par_DWORD( n ) : ( def ) )

/* 
 * Text / Resource Resolver Macro
 * Handles the dual nature of Win32 API arguments that can be either a
 * string pointer or a resource ID (MAKEINTRESOURCE).
 * It also manages Unicode/ANSI encoding based on the build configuration.
 */
#ifdef UNICODE
#define _STR( n ) ( HB_ISCHAR( n ) ? ( LPCWSTR ) hb_osStrU16Encode( hb_parc( n ) ) : ( HB_ISNUM( n ) ? MAKEINTRESOURCE( hb_parni( n ) ) : NULL ) )
#else
#define _STR( n ) ( HB_ISCHAR( n ) ? hb_parc( n ) : ( HB_ISNUM( n ) ? MAKEINTRESOURCE( hb_parni( n ) ) : NULL ) )
#endif

/*
 * FUNCTION: MESSAGEBOXINDIRECT()
 * 
 * Purpose:
 * Wraps the Win32 MessageBoxIndirect API, allowing for complex message boxes
 * that can include custom icons, help contexts, and specific language IDs.
 *
 * Parameters (from Harbour):
 * 1: hWndOwner (Numeric) - Handle to the owner window. Defaults to Active Window.
 * 2: lpText    (String)  - The message body text or resource ID.
 * 3: lpCaption (String)  - The title bar text or resource ID.
 * 4: dwStyle   (Numeric) - Flags determining buttons and behavior (e.g., MB_YESNO).
 * 5: lpIcon    (String)  - Resource name or ID for a custom icon.
 * 6: hInstance (Numeric) - Module handle for resource loading.
 * 7: dwContextHelpId (Numeric) - Help context identifier.
 * 9: dwLanguageId (Numeric) - Language identifier for button text.
 *
 * Returns:
 * Numeric - The ID of the button pressed by the user (e.g., IDOK, IDCANCEL).
 */
HB_FUNC( MESSAGEBOXINDIRECT )
{
   MSGBOXPARAMS   mbp;
   
   // Initialize structure to zero to ensure unused members don't contain garbage.
   memset( &mbp, 0, sizeof( mbp ) );

   mbp.cbSize = sizeof( mbp );
   
   // Determine the owner window; if not provided, use the current active window 
   // to ensure the dialog is modal to the application.
   mbp.hwndOwner = _GET_HWND( 1, GetActiveWindow() );
   
   // Determine the instance handle for loading resources (icons/strings).
   mbp.hInstance = _GET_HINST( 6, GetInstance() );

   // Resolve text, caption, and icon resources using the helper macro.
   mbp.lpszText = _STR( 2 );
   mbp.lpszCaption = _STR( 3 ) ? _STR( 3 ) : TEXT( "" );
   mbp.lpszIcon = _STR( 5 );

   // Extract bitwise style flags.
   mbp.dwStyle = hmg_par_DWORD( 4 );
   
   // Help and Language settings.
   mbp.dwContextHelpId = _GET_DWORD( 7, 0 );
   mbp.lpfnMsgBoxCallback = NULL; // Callbacks are not currently bridged to Harbour.
   mbp.dwLanguageId = _GET_DWORD( 9, MAKELANGID( LANG_NEUTRAL, SUBLANG_NEUTRAL ) );

   // Execute the API and return the result to the Harbour environment.
   hmg_ret_NINT( MessageBoxIndirect( &mbp ) );
}

/*
 * FUNCTION: MESSAGEBOXTIMEOUT()
 * 
 * Purpose:
 * Displays a message box that automatically closes after a specified duration.
 * This uses an undocumented but widely available function in user32.dll.
 *
 * Parameters (from Harbour):
 * 1: lpText    (String)  - The message body text.
 * 2: lpCaption (String)  - The title bar text.
 * 3: uType     (Numeric) - Flags for buttons/icons. Defaults to MB_OK.
 * 4: dwTimeout (Numeric) - Time in milliseconds before auto-closing.
 *
 * Returns:
 * Numeric - The button ID pressed, or 32000 (MB_TIMEDOUT) if the timer expired.
 */
HB_FUNC( MESSAGEBOXTIMEOUT )
{
   HWND        hWnd = GetActiveWindow();

   // Handle string encoding based on Unicode/ANSI build settings.
#ifdef UNICODE
   TCHAR       *lpText = ( TCHAR * ) hb_osStrU16Encode( hb_parc( 1 ) );
   TCHAR       *lpCaption = ( TCHAR * ) hb_osStrU16Encode( hb_parc( 2 ) );
#else
   const char  *lpText = hb_parc( 1 );
   const char  *lpCaption = hb_parc( 2 );
#endif

   UINT        uType = hmg_par_UINT_def( 3, MB_OK );
   WORD        wLang = MAKELANGID( LANG_NEUTRAL, SUBLANG_NEUTRAL );
   
   // Default to a very large value (effectively no timeout) if not specified.
   DWORD       dwTimeout = _GET_DWORD( 4, 0xFFFFFFFF );

   hmg_ret_NINT( MessageBoxTimeout( hWnd, lpText, lpCaption, uType, wLang, dwTimeout ) );
}

/*
 * INTERNAL FUNCTION: MessageBoxTimeout (Dynamic Loader)
 * 
 * Reasoning:
 * MessageBoxTimeout is an exported function in user32.dll but is not 
 * officially documented in standard Windows SDK headers. To ensure 
 * compatibility across different compilers and Windows versions, we 
 * load it dynamically at runtime.
 */
int WINAPI MessageBoxTimeout( HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds )
{
   // Define the function signature for the pointer.
   typedef int ( WINAPI *PFN_MBT ) ( HWND, LPCTSTR, LPCTSTR, UINT, WORD, DWORD );

   // Use a static variable to cache the function pointer after the first lookup.
   static PFN_MBT pFunc = NULL;

   if( pFunc == NULL )
   {
      // Attempt to load the User32 library.
      HMODULE  hLib = LoadLibrary( TEXT( "user32.dll" ) );

      if( hLib )
      {
         // Resolve the address of the function, choosing the A or W variant 
         // based on the compilation environment.
#ifdef UNICODE
         pFunc = ( PFN_MBT ) wapi_GetProcAddress( hLib, "MessageBoxTimeoutW" );
#else
         pFunc = ( PFN_MBT ) wapi_GetProcAddress( hLib, "MessageBoxTimeoutA" );
#endif
      }
   }

   // If the function was found, call it; otherwise, return 0 (failure).
   return pFunc ? pFunc( hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds ) : 0;
}