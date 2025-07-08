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

#include <mgdefs.h>                    // Include Minigui definitions
#include <commctrl.h>                  // Include for common control functionalities

// Disable specific warning (4201) for Microsoft compilers when including Rich Edit definitions
#if defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4201 )
#endif
#include <richedit.h>                  // Include for Rich Edit control functionality
#if defined( _MSC_VER )
#pragma warning( pop )
#endif

// Define specific flags and macros for MinGW and Watcom compilers to ensure compatibility
#if defined( __MINGW32__ ) && defined( __MINGW32_VERSION )
#define IMF_AUTOFONT 0x0002            // Enable auto-font adjustment in Rich Edit control for MinGW
#endif
#if defined( __WATCOMC__ )
#define ENM_DRAGDROPDONE   0x00000010  // Event mask for drag-drop done for Watcom
#define SF_USECODEPAGE     0x0020      // Use specific code page in formatting
#endif

// Define Rich Edit class name for Windows if not already defined
#if defined( MSFTEDIT_CLASS )
#undef MSFTEDIT_CLASS
#endif
#define MSFTEDIT_CLASS  TEXT( "RICHEDIT50W" )   // Specify Rich Edit control class

// Function prototypes and global variables
static BOOL       IsWinxpSp1Min( void );        // Check if OS is Windows XP SP1 or newer
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );         // Convert ANSI string to Wide string (Unicode mode)
LPSTR             WideToAnsi( LPWSTR );         // Convert Wide string to ANSI string (Unicode mode)
#endif
HINSTANCE         GetInstance( void );          // Retrieve application instance handle

// Global handle for Rich Edit library
static HINSTANCE  hRELib = NULL;

// Initialize a Rich Edit box control with various styles based on parameters
HB_FUNC( INITRICHEDITBOX )
{
   HWND  hRE = NULL;          // Handle for Rich Edit control
   TCHAR *lpClassName;        // Pointer to class name for Rich Edit

   // Set initial styles for multi-line, enter-key return, scroll behavior
   DWORD Style = ES_MULTILINE | ES_WANTRETURN | WS_CHILD | ES_NOHIDESEL | ( hb_parl( 14 ) ? ES_AUTOVSCROLL : WS_VSCROLL );

   // Add additional styles based on parameters
   if( hb_parl( 10 ) )
   {
      Style |= ES_READONLY;   // Make control read-only if specified
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;    // Make control visible if specified
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;    // Enable tab stop if specified
   }

   if( !hb_parl( 13 ) )
   {
      Style |= WS_HSCROLL;    // Enable horizontal scroll if specified
   }

   // Check if the system is Windows XP SP1 or newer to determine Rich Edit version
   if( IsWinxpSp1Min() )
   {
      if( !hRELib )
      {
         hRELib = LoadLibrary( TEXT( "Msftedit.dll" ) ); // Load newer Rich Edit library
      }

      lpClassName = MSFTEDIT_CLASS; // Use Rich Edit class for newer systems
   }
   else
   {
      if( !hRELib )
      {
         hRELib = LoadLibrary( TEXT( "RichEd20.dll" ) ); // Load older Rich Edit library
      }

      lpClassName = RICHEDIT_CLASS;       // Use older Rich Edit class for older systems
   }

   // If library loaded successfully, create Rich Edit control with specified styles and dimensions
   if( hRELib )
   {
      hRE = CreateWindowEx
         (
            WS_EX_CLIENTEDGE,             // Extended style with client edge
            lpClassName,                  // Rich Edit class name based on OS version
            TEXT( "" ),                   // No initial text
            Style,                        // Combined style for Rich Edit control
            hb_parni( 3 ),                // X position
            hb_parni( 4 ),                // Y position
            hb_parni( 5 ),                // Width
            hb_parni( 6 ),                // Height
            hmg_par_raw_HWND( 1 ),        // Parent window handle
            hmg_par_raw_HMENU( 2 ),       // Menu handle (ID of the control)
            GetInstance(),                // Application instance handle
            NULL                          // No additional data
         );

      // Set text limit for the Rich Edit control
      SendMessage( hRE, EM_EXLIMITTEXT, ( WPARAM ) hb_parni( 9 ), ( LPARAM ) 0 );

      // Set event mask to monitor selection changes, drag-drop completion, text changes, and scrolling
      SendMessage( hRE, EM_SETEVENTMASK, ( WPARAM ) 0, ( LPARAM ) ENM_SELCHANGE | ENM_DRAGDROPDONE | ENM_CHANGE | ENM_SCROLL );
   }

   // Return the handle to the created Rich Edit control
   hmg_ret_raw_HWND( hRE );
}

// Unload the Rich Edit library when it is no longer needed
HB_FUNC( UNLOADRICHEDITLIB )
{
   if( hRELib )
   {
      FreeLibrary( hRELib );              // Release the loaded Rich Edit library
      hRELib = NULL;                      // Reset library handle to NULL
   }
}

// Callback function for reading data from a file into a Rich Edit control stream
DWORD CALLBACK EditStreamCallbackR( DWORD_PTR dwCookie, LPBYTE lpbBuff, LONG cb, LONG FAR *pcb )
{
   HANDLE   hFile = ( HANDLE ) dwCookie;  // File handle passed as cookie

   // Attempt to read the specified number of bytes from the file into buffer
   if( !ReadFile( hFile, ( LPVOID ) lpbBuff, cb, ( LPDWORD ) pcb, NULL ) )
   {
      return( DWORD ) - 1;                // Return error code if read operation fails
   }

   return 0;   // Return success code if read operation is successful
}

// Callback function for writing data from a Rich Edit control stream to a file
DWORD CALLBACK EditStreamCallbackW( DWORD_PTR dwCookie, LPBYTE lpbBuff, LONG cb, LONG FAR *pcb )
{
   HANDLE   hFile = ( HANDLE ) dwCookie;  // File handle passed as cookie

   // Attempt to write the specified number of bytes from buffer to file
   if( !WriteFile( hFile, ( LPVOID ) lpbBuff, cb, ( LPDWORD ) pcb, NULL ) )
   {
      return( DWORD ) - 1;                // Return error code if write operation fails
   }

   return 0;   // Return success code if write operation is successful
}

HB_FUNC( STREAMIN )        //StreamIn(HWND hwndCtrl, LPCTSTR lpszPath, int typ )
{
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   HANDLE      hFile;

#ifndef UNICODE
   LPCSTR      cFileName = ( char * ) hb_parc( 2 );
#else
   LPCWSTR     cFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   EDITSTREAM  es;
   long        Flag, Mode;

   switch( hb_parni( 3 ) )
   {
      case 1:
         Flag = SF_TEXT;
         Mode = TM_PLAINTEXT;
         break;

      case 2:
         Flag = SF_RTF;
         Mode = TM_RICHTEXT;
         break;

      case 3:
         Flag = SF_TEXT | SF_UNICODE;
         Mode = TM_PLAINTEXT;
         break;

      case 4:
         Flag = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         Mode = TM_PLAINTEXT;
         break;

      case 5:
         Flag = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_RTF;
         Mode = TM_RICHTEXT;
         break;

      case 6:
         Flag = ( CP_UTF7 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         Mode = TM_PLAINTEXT;
         break;

      default:
         Flag = SF_TEXT;
         Mode = TM_PLAINTEXT;
   }

   // open the source file.
   if( ( hFile = CreateFile( cFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL ) ) == INVALID_HANDLE_VALUE )
   {
      hb_retl( FALSE );
      return;
   }

#ifdef UNICODE
   else
   {
      hb_xfree( ( TCHAR * ) cFileName );
   }
#endif
   es.pfnCallback = EditStreamCallbackR;
   es.dwCookie = ( DWORD_PTR ) hFile;
   es.dwError = 0;

   // send EM_STREAMIN message to the Rich Edit Control.
   SendMessage( hwnd, EM_STREAMIN, ( WPARAM ) Flag, ( LPARAM ) & es );
   SendMessage( hwnd, EM_SETTEXTMODE, ( WPARAM ) Mode, 0 );

   CloseHandle( hFile );

   hmg_ret_L( !( es.dwError ) );
}

HB_FUNC( STREAMOUT )       //StreamOut(HWND hwndCtrl, LPCTSTR lpszPath, int Typ )
{
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   HANDLE      hFile;

#ifndef UNICODE
   LPCSTR      cFileName = ( char * ) hb_parc( 2 );
#else
   LPCWSTR     cFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   EDITSTREAM  es;
   long        Flag;

   switch( hb_parni( 3 ) )
   {
      case 1:
         Flag = SF_TEXT;
         break;

      case 2:
         Flag = SF_RTF;
         break;

      case 3:
         Flag = SF_TEXT | SF_UNICODE;
         break;

      case 4:
         Flag = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         break;

      case 5:
         Flag = ( CP_UTF8 << 16 ) | SF_USECODEPAGE | SF_RTF;
         break;

      case 6:
         Flag = ( CP_UTF7 << 16 ) | SF_USECODEPAGE | SF_TEXT;
         break;

      default:
         Flag = SF_TEXT;
   }

   // open the destination file.
   if( ( hFile = CreateFile( cFileName, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL ) ) == INVALID_HANDLE_VALUE )
   {
      hb_retl( FALSE );
      return;
   }

#ifdef UNICODE
   else
   {
      hb_xfree( ( TCHAR * ) cFileName );
   }
#endif
   es.pfnCallback = EditStreamCallbackW;
   es.dwCookie = ( DWORD_PTR ) hFile;
   es.dwError = 0;

   // send EM_STREAMOUT message to the Rich Edit Control.
   SendMessage( hwnd, EM_STREAMOUT, ( WPARAM ) Flag, ( LPARAM ) & es );

   CloseHandle( hFile );

   hmg_ret_L( !( es.dwError ) );
}

// Function to get the "Auto Font" setting of a Rich Edit control
HB_FUNC( GETAUTOFONTRTF )  // GetAutoFont(HWND hwnd)
{
   LRESULT  lAuto;

   // Send message to retrieve language options from the Rich Edit control and check the AutoFont flag
   lAuto = SendMessage( hmg_par_raw_HWND( 1 ), EM_GETLANGOPTIONS, 0, 0 ) & IMF_AUTOFONT;

   // Return the result indicating whether AutoFont is enabled or not
   hmg_ret_L( lAuto );
}

// Function to set the "Auto Font" setting of a Rich Edit control
HB_FUNC( SETAUTOFONTRTF )  // SetAutoFont(HWND hwnd, lAutoFont)
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   LRESULT  lOpt, lResult;

   // Retrieve the current language options for the Rich Edit control
   lOpt = SendMessage( hwnd, EM_GETLANGOPTIONS, 0, 0 );

   // Modify the language options to enable or disable AutoFont based on the parameter
   if( hb_parl( 2 ) )
   {
      lOpt |= IMF_AUTOFONT;   // Enable AutoFont if parameter is true
   }
   else
   {
      lOpt &= ~IMF_AUTOFONT;  // Disable AutoFont if parameter is false
   }

   // Send message to set the new language options
   lResult = SendMessage( hwnd, EM_SETLANGOPTIONS, 0, lOpt );

   // Return the result of setting the language options
   hmg_ret_L( lResult );
}

// Function to set the background color of a Rich Edit control
HB_FUNC( SETBKGNDCOLOR )   // SetBkgndColor(HWND hwnd, lSyscol, nRed, nGreen, nBlue)
{
   LRESULT  lResult;
   INT      syscol = hb_parl( 2 ) ? 0 : 1;   // Determine if system color or custom color
   COLORREF bkgcolor = RGB( hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); // Define custom color

   // Send message to set the background color of the Rich Edit control
   lResult = SendMessage( hmg_par_raw_HWND( 1 ), EM_SETBKGNDCOLOR, ( WPARAM ) syscol, ( LPARAM ) bkgcolor );

   // Return the result of the background color setting
   hmg_ret_L( lResult );
}

// Function to retrieve the font properties of text in a Rich Edit control
HB_FUNC( GETFONTRTF )
{
   CHARFORMAT  cF;
   long        PointSize;
   int         bold;
   int         Italic;
   int         Underline;
   int         StrikeOut;
   int         SelText;

#ifdef UNICODE
   LPSTR       pStr;
#endif

   // Initialize CHARFORMAT structure to retrieve specific font attributes
   cF.cbSize = sizeof( CHARFORMAT );
   cF.dwMask = CFM_BOLD | CFM_ITALIC | CFM_UNDERLINE | CFM_SIZE;

   // Set selection type based on input parameter (default or selected text)
   SelText = hb_parni( 2 ) > 0 ? SCF_SELECTION : SCF_DEFAULT;

   // Retrieve the current font settings from the Rich Edit control
   SendMessage( hmg_par_raw_HWND( 1 ), EM_GETCHARFORMAT, ( WPARAM ) SelText, ( LPARAM ) & cF );

   // Convert font height to point size
   PointSize = cF.yHeight / 20;

   // Determine font effects based on the CHARFORMAT structure
   bold = ( cF.dwEffects & CFE_BOLD ) ? 1 : 0;
   Italic = ( cF.dwEffects & CFE_ITALIC ) ? 1 : 0;
   Underline = ( cF.dwEffects & CFE_UNDERLINE ) ? 1 : 0;
   StrikeOut = ( cF.dwEffects & CFE_STRIKEOUT ) ? 1 : 0;

   // Store the font properties in an array to return
   hb_reta( 8 );
#ifndef UNICODE
   HB_STORC( cF.szFaceName, -1, 1 );         // Store font face name
#else
   pStr = WideToAnsi( cF.szFaceName );       // Convert font face name if in Unicode mode
   HB_STORC( pStr, -1, 1 );
   hb_xfree( pStr );
#endif
   HB_STORVNL( ( LONG ) PointSize, -1, 2 );  // Store point size
   HB_STORL( bold, -1, 3 );                  // Store bold status
   HB_STORL( Italic, -1, 4 );                // Store italic status
   HB_STORVNL( cF.crTextColor, -1, 5 );      // Store text color
   HB_STORL( Underline, -1, 6 );             // Store underline status
   HB_STORL( StrikeOut, -1, 7 );             // Store strikeout status
   HB_STORNI( cF.bCharSet, -1, 8 );          // Store character set
}

// Function to set the font properties of text in a Rich Edit control
HB_FUNC( SETFONTRTF )
{
   LRESULT     lResult;
   CHARFORMAT  cF;
   DWORD       Mask;
   DWORD       Effects = 0;
   int         SelText = SCF_SELECTION;

#ifndef UNICODE
   TCHAR       *szFaceName = ( TCHAR * ) hb_parc( 3 );
#else
   TCHAR       *szFaceName = ( TCHAR * ) hb_osStrU16Encode( ( char * ) hb_parc( 3 ) );
#endif
   cF.cbSize = sizeof( CHARFORMAT );

   // Retrieve existing character format and optionally override mask
   Mask = ( DWORD ) SendMessage( hmg_par_raw_HWND( 1 ), EM_GETCHARFORMAT, ( WPARAM ) SelText, ( LPARAM ) & cF );
   if( hb_parni( 10 ) > 0 )
   {
      Mask = hb_parni( 10 );
   }

   // Set the selection mode based on parameter (apply to selection or all text)
   SelText = hb_parni( 2 ) < 0 ? SCF_ALL : ( hb_parni( 2 ) > 0 ? SCF_SELECTION | SCF_WORD : SelText );

   // Set the font effects based on parameters
   if( hb_parl( 5 ) )
   {
      Effects |= CFE_BOLD;
   }

   if( hb_parl( 6 ) )
   {
      Effects |= CFE_ITALIC;
   }

   if( hb_parl( 8 ) )
   {
      Effects |= CFE_UNDERLINE;
   }

   if( hb_parl( 9 ) )
   {
      Effects |= CFE_STRIKEOUT;
   }

   // Update CHARFORMAT structure with selected mask, effects, and other attributes
   cF.dwMask = Mask;
   cF.dwEffects = Effects;
   if( hb_parnl( 4 ) )
   {
      cF.yHeight = hb_parnl( 4 ) * 20;       // Set font size
   }

   cF.crTextColor = hmg_par_COLORREF( 7 );   // Set font color

   // Set font face name if provided
   if( hb_parclen( 3 ) > 0 )
   {
      lstrcpy( cF.szFaceName, szFaceName );
   }

   // Apply the updated character format to the Rich Edit control
   lResult = SendMessage( hmg_par_raw_HWND( 1 ), EM_SETCHARFORMAT, ( WPARAM ) SelText, ( LPARAM ) & cF );

   // Return result of setting character format
   hmg_ret_L( lResult );
}

// Disable deprecated function warning for GetVersionEx on Microsoft compilers
#if defined( _MSC_VER )
#pragma warning( disable : 4996 )
#endif

// Check if the operating system is Windows XP Service Pack 1 or higher
static BOOL IsWinxpSp1Min( void )
{
#ifndef UNICODE
   LPCSTR         pch;
#else
   LPCWSTR        pch;
#endif
   OSVERSIONINFO  osvi;

   // Initialize OS version info structure and retrieve version information
   osvi.dwOSVersionInfoSize = sizeof( osvi );
   if( !GetVersionEx( &osvi ) )
   {
      return FALSE;     // Return false if version information cannot be retrieved
   }

   // Check for Windows XP SP1 or higher
   if( osvi.dwMajorVersion >= 5 )
   {
      if( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 )
      {
         return FALSE;  // Windows 2000, not XP
      }
      else if( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 )
      {
#ifndef UNICODE
         pch = strstr( osvi.szCSDVersion, "Service Pack" );
#else
         pch = _tcsstr( osvi.szCSDVersion, TEXT( "Service Pack" ) );
#endif
         return( pch && lstrcmpi( pch, TEXT( "Service Pack 1" ) ) >= 0 );
      }

      return TRUE;      // OS version is above Windows XP SP1
   }

   return FALSE;
}
