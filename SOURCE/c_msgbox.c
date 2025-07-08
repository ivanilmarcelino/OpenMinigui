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
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/
#include <mgdefs.h>                 // Include Minigui framework definitions

/* Undocumented Windows API function prototype */
// MessageBoxTimeout is a Windows function that shows a message box with a timeout parameter
int WINAPI        MessageBoxTimeout( HWND, LPCTSTR, LPCTSTR, UINT, WORD, DWORD );

// Declaration of external functions to retrieve the HINSTANCE and function addresses dynamically
HINSTANCE         GetInstance( void );
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Function: MESSAGEBOXINDIRECT
// Purpose: This function provides a flexible way to display message boxes with various customization options. It allows specifying the owner window, text, caption, icon, style, help context ID, and language ID. It leverages the Windows API function MessageBoxIndirect.
// Parameters:
//   - hWnd: Handle to the owner window. If not provided or invalid, the active window is used.
//   - cText: Text to display in the message box. Can be a string or a resource ID.
//   - cCaption: Caption of the message box. Can be a string or a resource ID.
//   - nStyle: Style of the message box (e.g., MB_OK, MB_YESNO).
//   - xIcon: Icon to display in the message box. Can be a string or a resource ID.
//   - hInst: Instance handle of the application. If not provided, the default instance is used.
//   - nHelpId: Help context ID for the message box.
//   - nProc: Not used. Reserved for future use.
//   - nLang: Language ID for the message box.
// Return Value:
//   - Returns the ID of the button pressed by the user (e.g., IDOK, IDCANCEL, IDYES, IDNO).
// Contributed by Andy Wos <andywos@unwired.com.au>
HB_FUNC( MESSAGEBOXINDIRECT )
{
   // Initialize the MSGBOXPARAMS structure with default values
   MSGBOXPARAMS   mbp = { 0 };
   mbp.cbSize = sizeof( MSGBOXPARAMS );

   // Determine the owner window or use the active window by default
   mbp.hwndOwner = HB_ISNUM( 1 ) ? hmg_par_raw_HWND( 1 ) : GetActiveWindow();
   mbp.hInstance = HB_ISNUM( 6 ) ? hmg_par_raw_HINSTANCE( 6 ) : GetInstance();

#ifndef UNICODE
   // For non-Unicode builds, assign text, caption, and icon directly or as resource identifiers
   mbp.lpszText = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : ( HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hb_parni( 2 ) ) : NULL );
   mbp.lpszCaption = HB_ISCHAR( 3 ) ? hb_parc( 3 ) : ( HB_ISNUM( 3 ) ? MAKEINTRESOURCE( hb_parni( 3 ) ) : "" );
   mbp.lpszIcon = HB_ISCHAR( 5 ) ? hb_parc( 5 ) : ( HB_ISNUM( 5 ) ? MAKEINTRESOURCE( hb_parni( 5 ) ) : NULL );
#else
   // For Unicode builds, convert text, caption, and icon to wide strings if provided
   mbp.lpszText = ( LPCWSTR ) ( HB_ISCHAR( 2 ) ? hb_osStrU16Encode( hb_parc( 2 ) ) : ( HB_ISNUM( 2 ) ? MAKEINTRESOURCE( hb_parni( 2 ) ) : NULL ) );
   mbp.lpszCaption = ( LPCWSTR ) ( HB_ISCHAR( 3 ) ? hb_osStrU16Encode( hb_parc( 3 ) ) : ( HB_ISNUM( 3 ) ? MAKEINTRESOURCE( hb_parni( 3 ) ) : TEXT( "" ) ) );
   mbp.lpszIcon = ( LPCWSTR ) ( HB_ISCHAR( 5 ) ? hb_osStrU16Encode( hb_parc( 5 ) ) : ( HB_ISNUM( 5 ) ? MAKEINTRESOURCE( hb_parni( 5 ) ) : NULL ) );
#endif

   // Assign style, help context ID, and language ID for the message box
   mbp.dwStyle = hmg_par_DWORD( 4 );
   mbp.dwContextHelpId = HB_ISNUM( 7 ) ? hmg_par_DWORD( 7 ) : 0;
   mbp.lpfnMsgBoxCallback = NULL;   // No callback function
   mbp.dwLanguageId = HB_ISNUM( 9 ) ? hmg_par_DWORD( 9 ) : MAKELANGID( LANG_NEUTRAL, SUBLANG_NEUTRAL );

   // Display the message box and return the button ID selected by the user
   hmg_ret_NINT( MessageBoxIndirect( &mbp ) );
}

// Function: MESSAGEBOXTIMEOUT
// Purpose: This function displays a message box with a specified timeout. If the user doesn't interact with the message box within the timeout period, the function returns. It uses the Windows API function MessageBoxTimeout.
// Parameters:
//   - Text: Text to display in the message box.
//   - Caption: Caption of the message box.
//   - nTypeButton: Type of buttons to display in the message box (e.g., MB_OK, MB_YESNO). Defaults to MB_OK.
//   - nMilliseconds: Timeout duration in milliseconds. If not provided, the message box will not timeout.
// Return Value:
//   - Returns the ID of the button pressed by the user (e.g., IDOK, IDCANCEL, IDYES, IDNO). If the message box times out, it may return a value indicating timeout (typically 0).
HB_FUNC( MESSAGEBOXTIMEOUT )
{
   HWND        hWnd = GetActiveWindow();                    // Use the active window as the message box owner
#ifndef UNICODE
   const char  *lpText = hb_parc( 1 );                      // Text of the message box (ANSI string)
   const char  *lpCaption = hb_parc( 2 );                   // Caption of the message box (ANSI string)
#else
   TCHAR       *lpText = hb_osStrU16Encode( hb_parc( 1 ) ); // Convert text to Unicode if needed
   TCHAR       *lpCaption = hb_osStrU16Encode( hb_parc( 2 ) ); // Convert caption to Unicode if needed
#endif

   // Message box type and timeout duration (default to MB_OK button and no timeout if not specified)
   UINT        uType = hmg_par_UINT_def( 3, MB_OK );
   WORD        wLanguageId = MAKELANGID( LANG_NEUTRAL, SUBLANG_NEUTRAL );
   DWORD       dwMilliseconds = HB_ISNUM( 4 ) ? hmg_par_DWORD( 4 ) : ( DWORD ) 0xFFFFFFFF; // Timeout duration

   // Display the message box with a timeout and return the selected button ID
   hmg_ret_NINT( MessageBoxTimeout( hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds ) );
}

// Function: MessageBoxTimeout
// Purpose: This function is a wrapper around the Windows API function MessageBoxTimeout. It dynamically loads the MessageBoxTimeout function from User32.dll at runtime. This allows the application to run on systems where MessageBoxTimeout is not available (older Windows versions) without causing a crash. If the function is not found, it returns 0.
// Parameters:
//   - hWnd (HWND): Handle to the owner window.
//   - lpText (LPCTSTR): Text to display in the message box.
//   - lpCaption (LPCTSTR): Caption of the message box.
//   - uType (UINT): Type of buttons to display in the message box (e.g., MB_OK, MB_YESNO).
//   - wLanguageId (WORD): Language ID for the message box.
//   - dwMilliseconds (DWORD): Timeout duration in milliseconds.
// Return Value:
//   - Returns the ID of the button pressed by the user (e.g., IDOK, IDCANCEL, IDYES, IDNO). If the message box times out or the function is not found, it returns 0.
int WINAPI MessageBoxTimeout( HWND hWnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType, WORD wLanguageId, DWORD dwMilliseconds )
{
   // Define a pointer to the MessageBoxTimeout function type
   typedef int ( WINAPI *PMessageBoxTimeout ) ( HWND, LPCTSTR, LPCTSTR, UINT, WORD, DWORD );

   // Static function pointer to avoid reloading the function
   static PMessageBoxTimeout  pMessageBoxTimeout = NULL;

   if( pMessageBoxTimeout == NULL ) // Load the function only if not already loaded
   {
      // Load the User32.dll library and retrieve the MessageBoxTimeout function address
      HMODULE  hLib = LoadLibrary( TEXT( "User32.dll" ) );

#ifdef UNICODE
      // Load the Unicode version of MessageBoxTimeout if available
      pMessageBoxTimeout = ( PMessageBoxTimeout ) wapi_GetProcAddress( hLib, "MessageBoxTimeoutW" );
#else
      // Load the ANSI version of MessageBoxTimeout if available
      pMessageBoxTimeout = ( PMessageBoxTimeout ) wapi_GetProcAddress( hLib, "MessageBoxTimeoutA" );
#endif
   }

   // Call the dynamically loaded MessageBoxTimeout function if available, or return 0 on failure
   return pMessageBoxTimeout == NULL ? 0 : pMessageBoxTimeout( hWnd, lpText, lpCaption, uType, wLanguageId, dwMilliseconds );
}
