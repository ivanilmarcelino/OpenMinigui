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

// Define minimum Internet Explorer version as 5.01
#define _WIN32_IE 0x0501

// Include custom MiniGUI definitions
#include <mgdefs.h>

// Ensure compatibility for specific compilers (MINGW or XCC) with minimum Windows version NT 5.0
#if ( defined( __MINGW32__ ) || defined( __XCC__ ) ) && ( _WIN32_WINNT < 0x0500 )
#define _WIN32_WINNT 0x0500
#endif

// Include Windows common controls for GUI elements
#include <commctrl.h>

// Manage compiler-specific warnings for Microsoft Visual C++ compiler
#if defined( _MSC_VER )
#pragma warning( push )
#pragma warning( disable : 4201 )   // Suppress warning 4201 (nonstandard extension used: nameless struct/union)
#endif

// Include support for rich text formatting
#include <richedit.h>

#if defined( _MSC_VER )
#pragma warning( pop )              // Restore previous warning settings
#endif

// Include support for shell operations
#include <shellapi.h>

// Include Harbour API functions
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

// Define additional compatibility settings for older Borland compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
#define ES_AWAYMODE_REQUIRED  ( ( DWORD ) 0x00000040 )   // Enable away mode (sleep with display off) setting
#endif

// Custom message identifier for a taskbar-related operation
#define WM_TASKBAR   WM_USER + 1043

// External function declarations
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );                  // Convert ANSI string to wide string
LPSTR             WideToAnsi( LPWSTR );                  // Convert wide string to ANSI
#endif

// Retrieve application resources
HINSTANCE         GetResources( void );

// Function to get the address of a specified procedure within a DLL
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Function to handle errors, displaying a message and optionally exiting the application
extern void       hmg_ErrorExit( LPCTSTR lpMessage, DWORD dwError, BOOL bExit );

// Load an image from a specified file and return an HBITMAP handle
extern HBITMAP    HMG_LoadImage( const char *FileName );

// MiniGUI resource control for registering a resource with a specified type
void              RegisterResource( HANDLE hResource, LPCSTR szType );

// Convert a bitmap image into a region based on transparent color and tolerance
HRGN              BitmapToRegion( HBITMAP hBmp, COLORREF cTransparentColor, COLORREF cTolerance );

// Global variables for managing main window and keyboard accelerator table
HWND              g_hWndMain = NULL;                     // Handle to the main application window
HACCEL            g_hAccel = NULL;  // Handle to the keyboard accelerator table

// Static variable for modeless dialog window handle, initially NULL
static HWND       hDlgModeless = NULL;

// Set the main window and accelerator table handles for the application
BOOL SetAcceleratorTable( HWND hWnd, HACCEL hHaccel )
{
   g_hWndMain = hWnd;               // Assign the main window handle
   g_hAccel = hHaccel;              // Assign the accelerator table handle
   return TRUE;                     // Return TRUE indicating successful assignment
}

// Main application message loop for processing messages
HB_FUNC( DOMESSAGELOOP )
{
   MSG   Msg;              // Structure to store message information
   int   status;           // Status of the message retrieval

   // Continue retrieving messages until GetMessage returns 0 or an error occurs
   while( ( status = GetMessage( &Msg, NULL, 0, 0 ) ) != 0 )
   {
      if( status == -1 )   // If there's an error retrieving the message
      {
         // If specified, handle the error and optionally exit the application
         if( hb_parldef( 1, HB_TRUE ) )
         {
            hmg_ErrorExit( TEXT( "DOMESSAGELOOP" ), 0, TRUE ); // Error handling function
         }
      }
      else
      {
         // Retrieve the current active window handle (for modeless dialogs)
         hDlgModeless = GetActiveWindow();

         // Process the message if it's not an accelerator key or dialog message
         if( hDlgModeless == ( HWND ) NULL || !TranslateAccelerator( g_hWndMain, g_hAccel, &Msg ) )
         {
            if( !IsDialogMessage( hDlgModeless, &Msg ) )       // If not a dialog message, translate and dispatch it
            {
               TranslateMessage( &Msg );                 // Prepare message for handling
               DispatchMessage( &Msg );                  // Dispatch message to the window procedure
            }
         }
      }
   }
}

/*
 * DoEvents is a statement that yields execution of the current
 * thread so that the operating system can process other events.
 * This function cleans out the message loop and executes any other pending
 * business.
 */
HB_FUNC( DOEVENTS )
{
   MSG   Msg;

   while( PeekMessage( ( LPMSG ) & Msg, 0, 0, 0, PM_REMOVE ) )
   {
      hDlgModeless = GetActiveWindow();

      if( hDlgModeless == NULL || !IsDialogMessage( hDlgModeless, &Msg ) )
      {
         TranslateMessage( &Msg );
         DispatchMessage( &Msg );
      }
   }
}

HB_FUNC( EXITPROCESS )
{
   ExitProcess( HB_ISNUM( 1 ) ? hb_parni( 1 ) : 0 );
}

HB_FUNC( SHOWWINDOW )
{
   hb_retl( ShowWindow( hmg_par_raw_HWND( 1 ), HB_ISNUM( 2 ) ? hb_parni( 2 ) : SW_SHOW ) );
}

HB_FUNC( GETACTIVEWINDOW )
{
   hmg_ret_raw_HWND( GetActiveWindow() );
}

HB_FUNC( SETACTIVEWINDOW )
{
   hmg_ret_raw_HWND( SetActiveWindow( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( POSTQUITMESSAGE )
{
   PostQuitMessage( hb_parni( 1 ) );
}

HB_FUNC( DESTROYWINDOW )
{
   hb_retl( DestroyWindow( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( ISWINDOWVISIBLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsWindowVisible( hwnd ) : FALSE );
}

HB_FUNC( ISWINDOWENABLED )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsWindowEnabled( hwnd ) : FALSE );
}

HB_FUNC( ENABLEWINDOW )
{
   hb_retl( EnableWindow( hmg_par_raw_HWND( 1 ), TRUE ) );
}

HB_FUNC( DISABLEWINDOW )
{
   hb_retl( EnableWindow( hmg_par_raw_HWND( 1 ), FALSE ) );
}

HB_FUNC( SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( GETFOREGROUNDWINDOW )
{
   hmg_ret_raw_HWND( GetForegroundWindow() );
}

HB_FUNC( SETWINDOWTEXT )
{
#ifndef UNICODE
   LPCSTR   lpString = ( LPCSTR ) hb_parc( 2 );
   hb_retl( SetWindowText( hmg_par_raw_HWND( 1 ), lpString ) );
#else
   LPCWSTR  lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );
   hb_retl( SetWindowText( hmg_par_raw_HWND( 1 ), lpString ) );
   hb_xfree( ( TCHAR * ) lpString );
#endif
}

HB_FUNC( SETWINDOWTEXTW )
{
   hb_retl( SetWindowTextW( hmg_par_raw_HWND( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

HB_FUNC( SETWINDOWPOS )
{
   hb_retl( SetWindowPos( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hmg_par_UINT( 7 ) ) );
}

HB_FUNC( ANIMATEWINDOW )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   DWORD dwTime = hmg_par_DWORD( 2 );
   DWORD dwFlags = hmg_par_DWORD( 3 );

   hb_retl( AnimateWindow( hWnd, dwTime, dwFlags ) );
}

HB_FUNC( FLASHWINDOWEX )
{
   FLASHWINFO  FlashWinInfo;

   FlashWinInfo.cbSize = sizeof( FLASHWINFO );
   FlashWinInfo.hwnd = hmg_par_raw_HWND( 1 );
   FlashWinInfo.dwFlags = hmg_par_DWORD( 2 );
   FlashWinInfo.uCount = hmg_par_UINT( 3 );
   FlashWinInfo.dwTimeout = hmg_par_DWORD( 4 );

   hb_retl( FlashWindowEx( &FlashWinInfo ) );
}

HB_FUNC( SETLAYEREDWINDOWATTRIBUTES )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      HMODULE  hDll = GetModuleHandle( TEXT( "user32.dll" ) );

      hb_retl( HB_FALSE );

      if( NULL != hDll )
      {
         typedef BOOL ( __stdcall *SetLayeredWindowAttributes_ptr ) ( HWND, COLORREF, BYTE, DWORD );

         SetLayeredWindowAttributes_ptr   fn_SetLayeredWindowAttributes = ( SetLayeredWindowAttributes_ptr ) wapi_GetProcAddress
            (
               hDll,
               "SetLayeredWindowAttributes"
            );

         if( NULL != fn_SetLayeredWindowAttributes )
         {
            COLORREF crKey = hmg_par_COLORREF( 2 );
            BYTE     bAlpha = hmg_par_BYTE( 3 );
            DWORD    dwFlags = hmg_par_DWORD( 4 );

            if( !( GetWindowLongPtr( hWnd, GWL_EXSTYLE ) & WS_EX_LAYERED ) )
            {
               SetWindowLongPtr( hWnd, GWL_EXSTYLE, GetWindowLongPtr( hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );
            }

            hmg_ret_L( fn_SetLayeredWindowAttributes( hWnd, crKey, bAlpha, dwFlags ) );
         }
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to load user32.dll", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "Invalid window handle", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

static BOOL CenterIntoParent( HWND hwnd )
{
   HWND  hwndParent;
   RECT  rect, rectP;
   int   width, height;
   int   screenwidth, screenheight;
   int   x, y;

   // make the window relative to its parent
   hwndParent = GetParent( hwnd );

   GetWindowRect( hwnd, &rect );
   GetWindowRect( hwndParent, &rectP );

   width = rect.right - rect.left;
   height = rect.bottom - rect.top;

   x = ( ( rectP.right - rectP.left ) - width ) / 2 + rectP.left;
   y = ( ( rectP.bottom - rectP.top ) - height ) / 2 + rectP.top;

   screenwidth = GetSystemMetrics( SM_CXSCREEN );
   screenheight = GetSystemMetrics( SM_CYSCREEN );

   // make sure that the child window never moves outside of the screen
   if( x < 0 )
   {
      x = 0;
   }

   if( y < 0 )
   {
      y = 0;
   }

   if( x + width > screenwidth )
   {
      x = screenwidth - width;
   }

   if( y + height > screenheight )
   {
      y = screenheight - height;
   }

   MoveWindow( hwnd, x, y, width, height, FALSE );

   return TRUE;
}

HB_FUNC( C_CENTER )
{
   HWND  hwnd;
   RECT  rect;
   int   w, h, x, y;

   hwnd = hmg_par_raw_HWND( 1 );
   if( !IsWindow( hwnd ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "Invalid window handle", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( hb_parl( 2 ) )
   {
      hb_retl( CenterIntoParent( hwnd ) );
   }
   else
   {
      GetWindowRect( hwnd, &rect );
      w = rect.right - rect.left;
      h = rect.bottom - rect.top;
      x = GetSystemMetrics( SM_CXSCREEN );
      SystemParametersInfo( SPI_GETWORKAREA, 1, &rect, 0 );
      y = rect.bottom - rect.top;

      hb_retl( SetWindowPos( hwnd, HWND_TOP, ( x - w ) / 2, ( y - h ) / 2, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE ) );
   }
}

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
void SwitchToWindow( HWND hwnd, BOOL fRestore )
{
   HB_SYMBOL_UNUSED( fRestore );
   SetFocus( hwnd );
}

#else
void SwitchToWindow( HWND hwnd, BOOL fRestore )
{
   typedef long long int ( WINAPI *TSwitchToThisWindow ) ( HWND, BOOL );

   static TSwitchToThisWindow pSwitchToThisWindow = NULL;

   SetForegroundWindow( hwnd );
   Sleep( 20 );

   if( !pSwitchToThisWindow )
   {
      pSwitchToThisWindow = ( TSwitchToThisWindow ) wapi_GetProcAddress( GetModuleHandle( TEXT( "user32.dll" ) ), "SwitchToThisWindow" );
   }

   if( pSwitchToThisWindow )
   {
      HWND  hwndLastActive = GetLastActivePopup( hwnd );

      if( IsWindowVisible( hwndLastActive ) )
      {
         hwnd = hwndLastActive;
      }

      if( pSwitchToThisWindow )
      {
         SwitchToThisWindow( hwnd, fRestore );
      }
   }
}
#endif

HB_FUNC( SWITCHTOTHISWINDOW )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   if( !IsWindow( hwnd ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "Invalid window handle", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   SwitchToWindow( hwnd, hb_parldef( 2, HB_TRUE ) );

   hb_retl( GetForegroundWindow() == hwnd );
}

HB_FUNC( GETWINDOWTEXT )
{
#ifdef UNICODE
   LPSTR    pStr;
#endif
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   int      iLen = GetWindowTextLength( hWnd );
   LPTSTR   szText = ( TCHAR * ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) );

#ifndef UNICODE
   iLen = GetWindowText( hWnd, szText, iLen + 1 );
   hb_retclen( szText, iLen );
#else
   GetWindowText( hWnd, szText, iLen + 1 );
   pStr = WideToAnsi( szText );
   hb_retc( pStr );
   hb_xfree( pStr );
#endif
   hb_xfree( szText );
}

HB_FUNC( SENDMESSAGE )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   UINT     msg = hmg_par_UINT( 2 );
   WPARAM   wParam = hmg_par_WPARAM( 3 );
   LPARAM   lParam = hmg_par_LPARAM( 4 );
   LRESULT  result;

   // Validate inputs
   if( !IsWindow( hwnd ) || !msg )
   {
      hb_errRT_BASE_SubstR
      (
         EG_ARG,
         !IsWindow( hwnd ) ? 5001 : 5002,
         !IsWindow( hwnd ) ? "Invalid window handle" : "Invalid message ID",
         HB_ERR_FUNCNAME,
         HB_ERR_ARGS_BASEPARAMS
      );
      return;
   }

   // Send the message and return the result
   result = SendMessage( hwnd, msg, wParam, lParam );
   hmg_ret_LRESULT( result );
}

HB_FUNC( SENDMESSAGESTRING )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   UINT     msg = hmg_par_UINT( 2 );
   WPARAM   wParam = hmg_par_WPARAM( 3 );
   LRESULT  result;

#ifndef UNICODE
   LPARAM   lParam = ( LPARAM ) ( LPSTR ) hb_parc( 4 );
#else
   LPWSTR   lpString = hb_parc( 4 ) ? AnsiToWide( ( char * ) hb_parc( 4 ) ) : NULL;
   LPARAM   lParam = ( LPARAM ) lpString;
#endif
   if( !IsWindow( hwnd ) || !msg )
   {
      hb_errRT_BASE_SubstR
      (
         EG_ARG,
         !IsWindow( hwnd ) ? 5001 : 5002,
         !IsWindow( hwnd ) ? "Invalid window handle" : "Invalid message ID",
         HB_ERR_FUNCNAME,
         HB_ERR_ARGS_BASEPARAMS
      );
      return;
   }

   result = SendMessage( hwnd, msg, wParam, lParam );
#ifdef UNICODE
   if( lpString )
   {
      hb_xfree( lpString );
   }
#endif
   hmg_ret_LRESULT( result );
}

HB_FUNC( GETNOTIFYCODE )
{
   LPARAM   lParam = hmg_par_raw_LPARAM( 1 );
   NMHDR    *nmhdr = ( NMHDR * ) lParam;
   if( !nmhdr )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid notification data", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hb_retni( nmhdr->code );
}

HB_FUNC( GETNOTIFYLINK )
{
   LPARAM   lParam = hmg_par_raw_LPARAM( 1 );
   ENLINK   *pENLink = ( ENLINK * ) lParam;

   hb_retnl( ( LONG ) pENLink->msg );
   HB_STORNL( ( LONG_PTR ) pENLink->wParam, 2 );
   HB_STORNL( ( LONG_PTR ) pENLink->lParam, 3 );
   hb_stornl( ( LONG ) pENLink->chrg.cpMin, 4 );
   hb_stornl( ( LONG ) pENLink->chrg.cpMax, 5 );
}

//JP 107a
HB_FUNC( GETNOTIFYID )
{
   LPARAM   lParam = hmg_par_raw_LPARAM( 1 );
   NMHDR    *nmhdr = ( NMHDR * ) lParam;
   if( !nmhdr )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid notification data", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hmg_ret_raw_HWND( nmhdr->idFrom );
}

HB_FUNC( GETHWNDFROM )
{
   LPARAM   lParam = hmg_par_raw_LPARAM( 1 );
   NMHDR    *nmhdr = ( NMHDR * ) lParam;
   if( !nmhdr )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Invalid notification data", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hmg_ret_raw_HWND( nmhdr->hwndFrom );
}

HB_FUNC( GETDRAWITEMHANDLE )
{
   hmg_ret_raw_HWND( ( ( ( DRAWITEMSTRUCT FAR * ) HB_PARNL( 1 ) )->hwndItem ) );
}

HB_FUNC( GETFOCUS )
{
   hmg_ret_raw_HWND( GetFocus() );
}

HB_FUNC( GETGRIDCOLUMN )
{
   hmg_ret_NINT( ( LPARAM ) ( ( ( NM_LISTVIEW * ) HB_PARNL( 1 ) )->iSubItem ) );
}

HB_FUNC( GETGRIDVKEY )
{
   hmg_ret_WORD( ( WORD ) ( ( ( LV_KEYDOWN * ) HB_PARNL( 1 ) )->wVKey ) );
}

HB_FUNC( MOVEWINDOW )
{
   hb_retl( MoveWindow( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNIL( 6 ) ? TRUE : hb_parl( 6 ) ) ) );
}

HB_FUNC( GETSYSTEMMETRICS )
{
   hmg_ret_NINT( GetSystemMetrics( hb_parni( 1 ) ) );
}

HB_FUNC( GETWINDOWRECT )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   RECT  rect;

   if( !IsWindow( hwnd ) || !GetWindowRect( hwnd, &rect ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to get window rect", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( HB_ISNUM( 2 ) )
   {
      switch( hb_parni( 2 ) )
      {
         case 1:
            hmg_ret_LONG( rect.top );
            break;

         case 2:
            hmg_ret_LONG( rect.left );
            break;

         case 3:
            hmg_ret_LONG( rect.right - rect.left );
            break;

         case 4:
            hmg_ret_LONG( rect.bottom - rect.top );
      }
   }
   else if( HB_ISARRAY( 2 ) )
   {
      HB_STORVNL( rect.left, 2, 1 );
      HB_STORVNL( rect.top, 2, 2 );
      HB_STORVNL( rect.right, 2, 3 );
      HB_STORVNL( rect.bottom, 2, 4 );
   }
}

HB_FUNC( GETCLIENTRECT )
{
   RECT  rect;

   hb_retl( GetClientRect( hmg_par_raw_HWND( 1 ), &rect ) );

   HB_STORVNL( rect.left, 2, 1 );
   HB_STORVNL( rect.top, 2, 2 );
   HB_STORVNL( rect.right, 2, 3 );
   HB_STORVNL( rect.bottom, 2, 4 );
}

HB_FUNC( GETDESKTOPAREA )
{
   RECT  rect;

   SystemParametersInfo( SPI_GETWORKAREA, 1, &rect, 0 );

   hb_reta( 4 );
   HB_STORVNL( rect.left, -1, 1 );
   HB_STORVNL( rect.top, -1, 2 );
   HB_STORVNL( rect.right, -1, 3 );
   HB_STORVNL( rect.bottom, -1, 4 );
}

HB_FUNC( GETTASKBARHEIGHT )
{
   HWND  hwnd = FindWindow( TEXT( "Shell_TrayWnd" ), NULL );
   if( hwnd )
   {
      RECT  rect;
      GetWindowRect( hwnd, &rect );
      hmg_ret_LONG( rect.bottom - rect.top );
   }
   else
   {
      hb_retni( 0 );
   }
}

static BOOL ShowNotifyIcon( HWND hWnd, BOOL bAdd, HICON hIcon, TCHAR *szText )
{
   NOTIFYICONDATA nid;

   ZeroMemory( &nid, sizeof( nid ) );

   nid.cbSize = sizeof( NOTIFYICONDATA );
   nid.hIcon = hIcon;
   nid.hWnd = hWnd;
   nid.uID = 0;
   nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
   nid.uCallbackMessage = WM_TASKBAR;
   lstrcpy( nid.szTip, szText );

   return Shell_NotifyIcon( bAdd ? NIM_ADD : NIM_DELETE, &nid );
}

HB_FUNC( SHOWNOTIFYICON )
{
#ifndef UNICODE
   char  *szText = ( char * ) hb_parc( 4 );
#else
   TCHAR *szText = ( TCHAR * ) AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif
   hb_retl( ShowNotifyIcon( hmg_par_raw_HWND( 1 ), hmg_par_BOOL( 2 ), hmg_par_raw_HICON( 3 ), ( TCHAR * ) szText ) );

#ifdef UNICODE
   hb_xfree( szText );
#endif
}

HB_FUNC( GETCURSORPOS )
{
   POINT pt;

   if( !GetCursorPos( &pt ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to get cursor position", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( hb_pcount() == 1 )
   {
      ScreenToClient( hmg_par_raw_HWND( 1 ), &pt );
   }

   hb_reta( 2 );
   HB_STORNI( hb_pcount() == 0 ? pt.y : pt.x, -1, 1 );
   HB_STORNI( hb_pcount() == 0 ? pt.x : pt.y, -1, 2 );
}

HB_FUNC( SCREENTOCLIENT )
{
   LONG  x = hmg_par_LONG( 2 );
   LONG  y = hmg_par_LONG( 3 );
   POINT pt;

   pt.x = x;
   pt.y = y;

   ScreenToClient( hmg_par_raw_HWND( 1 ), &pt );

   hb_reta( 2 );
   HB_STORNI( pt.x, -1, 1 );
   HB_STORNI( pt.y, -1, 2 );
}

HB_FUNC( CLIENTTOSCREEN )
{
   LONG  x = hmg_par_LONG( 2 );
   LONG  y = hmg_par_LONG( 3 );
   POINT pt;

   pt.x = x;
   pt.y = y;

   hb_retl( ClientToScreen( hmg_par_raw_HWND( 1 ), &pt ) );

   if( HB_ISBYREF( 2 ) )
   {
      hb_storni( pt.x, 2 );
   }

   if( HB_ISBYREF( 3 ) )
   {
      hb_storni( pt.y, 3 );
   }
}

HB_FUNC( LOADTRAYICON )
{
   HICON       hIcon;
   HINSTANCE   hInstance = hmg_par_raw_HINSTANCE( 1 );   // handle to application instance
#ifndef UNICODE
   LPCTSTR     lpIconName = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : MAKEINTRESOURCE( hb_parni( 2 ) );   // name string or resource identifier
#else
   LPCWSTR     lpIconName = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : ( LPCWSTR ) MAKEINTRESOURCE( hb_parni( 2 ) );
#endif
   int         cxDesired = HB_ISNUM( 3 ) ? hb_parni( 3 ) : GetSystemMetrics( SM_CXSMICON );
   int         cyDesired = HB_ISNUM( 4 ) ? hb_parni( 4 ) : GetSystemMetrics( SM_CYSMICON );

   hIcon = ( HICON ) LoadImage( hInstance, lpIconName, IMAGE_ICON, cxDesired, cyDesired, LR_DEFAULTCOLOR );

   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage( hInstance, lpIconName, IMAGE_ICON, cxDesired, cyDesired, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   RegisterResource( hIcon, "ICON" );
   hmg_ret_raw_HANDLE( hIcon );

#ifdef UNICODE
   if( HB_ISCHAR( 2 ) )
   {
      hb_xfree( ( TCHAR * ) lpIconName );
   }
#endif
}

static BOOL ChangeNotifyIcon( HWND hWnd, HICON hIcon, TCHAR *szText )
{
   NOTIFYICONDATA nid;

   ZeroMemory( &nid, sizeof( nid ) );

   nid.cbSize = sizeof( NOTIFYICONDATA );
   nid.hIcon = hIcon;
   nid.hWnd = hWnd;
   nid.uID = 0;
   nid.uFlags = NIF_ICON | NIF_TIP;
   lstrcpy( nid.szTip, szText );

   return Shell_NotifyIcon( NIM_MODIFY, &nid );
}

HB_FUNC( CHANGENOTIFYICON )
{
#ifndef UNICODE
   char  *szText = ( char * ) hb_parc( 3 );
#else
   TCHAR *szText = ( TCHAR * ) AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   hb_retl( ChangeNotifyIcon( hmg_par_raw_HWND( 1 ), hmg_par_raw_HICON( 2 ), ( TCHAR * ) szText ) );

#ifdef UNICODE
   hb_xfree( szText );
#endif
}

HB_FUNC( GETITEMPOS )
{
   hmg_ret_raw_HWND( ( ( NMMOUSE FAR * ) HB_PARNL( 1 ) )->dwItemSpec );
}

HB_FUNC( SETSCROLLRANGE )
{
   hb_retl( SetScrollRange( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parl( 5 ) ) );
}

HB_FUNC( GETSCROLLPOS )
{
   hmg_ret_NINT( GetScrollPos( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( GETWINDOWSTATE )
{
   HWND              hwnd = hmg_par_raw_HWND( 1 );
   WINDOWPLACEMENT   wp;

   wp.length = sizeof( WINDOWPLACEMENT );
   if( !IsWindow( hwnd ) || !GetWindowPlacement( hwnd, &wp ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to get window placement", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hmg_ret_UINT( wp.showCmd );
}

HB_FUNC( GETPARENT )
{
   hmg_ret_raw_HWND( GetParent( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( SETPARENT )
{
   HWND  hWnd = SetParent( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ) );
   hmg_ret_raw_HWND( hWnd );
}

HB_FUNC( GETDESKTOPWINDOW )
{
   hmg_ret_raw_HWND( GetDesktopWindow() );
}

static BOOL CALLBACK EnumWindowsProc( HWND hWnd, LPARAM pArray )
{
   PHB_ITEM pHWnd = hb_itemPutNInt( NULL, ( LONG_PTR ) hWnd );

   hb_arrayAddForward( ( PHB_ITEM ) pArray, pHWnd );
   hb_itemRelease( pHWnd );

   return TRUE;
}

HB_FUNC( ENUMWINDOWS )
{
   PHB_ITEM pArray = hb_itemArrayNew( 0 );

   EnumWindows( ( WNDENUMPROC ) EnumWindowsProc, ( LPARAM ) pArray );

   hb_itemReturnRelease( pArray );
}

static BOOL CALLBACK EnumChildProc( HWND hWnd, LPARAM lParam )
{
   PHB_ITEM pCodeBlock = ( PHB_ITEM ) lParam;
   PHB_ITEM pHWnd = hb_itemPutNInt( NULL, ( LONG_PTR ) hWnd );

   if( pCodeBlock )
   {
      hb_evalBlock1( pCodeBlock, pHWnd );
   }

   hb_itemRelease( pHWnd );

   return( BOOL ) hb_parl( -1 );
}

HB_FUNC( C_ENUMCHILDWINDOWS )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   PHB_ITEM pCodeBlock = hb_param( 2, HB_IT_BLOCK );

   if( IsWindow( hWnd ) && pCodeBlock )
   {
      hmg_ret_L( EnumChildWindows( hWnd, EnumChildProc, ( LPARAM ) pCodeBlock ) );
   }
}

//        IsWow64Process ( [ nProcessID ] ) --> return lBoolean
HB_FUNC( ISWOW64PROCESS )
{
   typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

   static LPFN_ISWOW64PROCESS fnIsWow64Process = NULL;

   BOOL                       IsWow64 = FALSE;

   if( fnIsWow64Process == NULL )
   {
      fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( GetModuleHandle( TEXT( "kernel32" ) ), "IsWow64Process" );
   }

   if( fnIsWow64Process != NULL )
   {
      if( !HB_ISNUM( 1 ) )
      {
         fnIsWow64Process( GetCurrentProcess(), &IsWow64 );
      }
      else
      {
         DWORD    ProcessID = hmg_par_DWORD( 1 );
         HANDLE   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
         if( hProcess )
         {
            fnIsWow64Process( hProcess, &IsWow64 );
            CloseHandle( hProcess );
         }
      }
   }

   hb_retl( IsWow64 );
}

//        GetCurrentProcessId() --> return nProcessID
HB_FUNC( GETCURRENTPROCESSID )
{
   hmg_ret_NINT( GetCurrentProcessId() );
}

//        EnumProcessesID () ---> return array { nProcessID1, nProcessID2, ... }
HB_FUNC( ENUMPROCESSESID )
{
   typedef BOOL ( WINAPI *Func_EnumProcesses ) ( DWORD *, DWORD, DWORD * );

   static Func_EnumProcesses  pEnumProcesses = NULL;

   DWORD                      aProcessesID[1024], cbNeeded, nProcesses;
   unsigned int               i;

   PHB_ITEM                   pArray = hb_itemArrayNew( 0 );

   if( pEnumProcesses == NULL )
   {
      HMODULE  hLib = LoadLibrary( TEXT( "Psapi.dll" ) );
      pEnumProcesses = ( Func_EnumProcesses ) wapi_GetProcAddress( hLib, "EnumProcesses" );
   }

   if( pEnumProcesses == NULL )
   {
      return;
   }

   // Get the list of process identifiers.
   if( pEnumProcesses( aProcessesID, sizeof( aProcessesID ), &cbNeeded ) == FALSE )
   {
      return;
   }

   // Calculate how many process identifiers were returned.
   nProcesses = cbNeeded / sizeof( DWORD );

   for( i = 0; i < nProcesses; i++ )
   {
      if( aProcessesID[i] != 0 )
      {
         PHB_ITEM pItem = hb_itemPutNL( NULL, ( LONG ) aProcessesID[i] );
         hb_arrayAddForward( pArray, pItem );
         hb_itemRelease( pItem );
      }
   }

   hb_itemReturnRelease( pArray );
}

//        GetWindowThreadProcessId (hWnd, @nThread, @nProcessID)
HB_FUNC( GETWINDOWTHREADPROCESSID )
{
   DWORD nThread, nProcessID;

   nThread = GetWindowThreadProcessId( hmg_par_raw_HWND( 1 ), &nProcessID );

   if( HB_ISBYREF( 2 ) )
   {
      hb_storni( nThread, 2 );
   }

   if( HB_ISBYREF( 3 ) )
   {
      hb_storni( nProcessID, 3 );
   }
}

//        GetProcessName ( [ nProcessID ] ) --> return cProcessName
HB_FUNC( GETPROCESSNAME )
{
   typedef BOOL ( WINAPI *Func_EnumProcessModules ) ( HANDLE, HMODULE *, DWORD, LPDWORD );

   static Func_EnumProcessModules   pEnumProcessModules = NULL;

   typedef DWORD ( WINAPI *Func_GetModuleBaseName ) ( HANDLE, HMODULE, LPTSTR, DWORD );

   static Func_GetModuleBaseName pGetModuleBaseName = NULL;

#ifdef UNICODE
   LPSTR                         pStr;
#endif
   DWORD                         ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();
   TCHAR                         cProcessName[MAX_PATH] = _TEXT( "" );
   HANDLE                        hProcess;

   if( pEnumProcessModules == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );
      pEnumProcessModules = ( Func_EnumProcessModules ) wapi_GetProcAddress( hLib, "EnumProcessModules" );
   }

   if( pEnumProcessModules == NULL )
   {
      return;
   }

   if( pGetModuleBaseName == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );

#ifdef UNICODE
      pGetModuleBaseName = ( Func_GetModuleBaseName ) wapi_GetProcAddress( hLib, "GetModuleBaseNameW" );
#else
      pGetModuleBaseName = ( Func_GetModuleBaseName ) wapi_GetProcAddress( hLib, "GetModuleBaseNameA" );
#endif
   }

   if( pGetModuleBaseName == NULL )
   {
      return;
   }

   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
   if( hProcess )
   {
      HMODULE  hMod;
      DWORD    cbNeeded;
      if( pEnumProcessModules( hProcess, &hMod, sizeof( hMod ), &cbNeeded ) )
      {
         pGetModuleBaseName( hProcess, hMod, cProcessName, sizeof( cProcessName ) / sizeof( TCHAR ) );
      }

      CloseHandle( hProcess );
#ifndef UNICODE
      hb_retc( cProcessName );
#else
      pStr = WideToAnsi( cProcessName );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

//        GetProcessFullName ( [ nProcessID ] ) --> return cProcessFullName
HB_FUNC( GETPROCESSFULLNAME )
{
   typedef BOOL ( WINAPI *Func_EnumProcessModules ) ( HANDLE, HMODULE *, DWORD, LPDWORD );

   static Func_EnumProcessModules   pEnumProcessModules = NULL;

   typedef DWORD ( WINAPI *Func_GetModuleFileNameEx ) ( HANDLE, HMODULE, LPTSTR, DWORD );

   static Func_GetModuleFileNameEx  pGetModuleFileNameEx = NULL;

#ifdef UNICODE
   LPSTR                            pStr;
#endif
   DWORD                            ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();
   TCHAR                            cProcessFullName[MAX_PATH] = _TEXT( "" );
   HANDLE                           hProcess;

   if( pEnumProcessModules == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );
      pEnumProcessModules = ( Func_EnumProcessModules ) wapi_GetProcAddress( hLib, "EnumProcessModules" );
   }

   if( pEnumProcessModules == NULL )
   {
      return;
   }

   if( pGetModuleFileNameEx == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );

#ifdef UNICODE
      pGetModuleFileNameEx = ( Func_GetModuleFileNameEx ) wapi_GetProcAddress( hLib, "GetModuleFileNameExW" );
#else
      pGetModuleFileNameEx = ( Func_GetModuleFileNameEx ) wapi_GetProcAddress( hLib, "GetModuleFileNameExA" );
#endif
   }

   if( pGetModuleFileNameEx == NULL )
   {
      return;
   }

   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
   if( hProcess )
   {
      HMODULE  hMod;
      DWORD    cbNeeded;
      if( pEnumProcessModules( hProcess, &hMod, sizeof( hMod ), &cbNeeded ) )
      {
         pGetModuleFileNameEx( hProcess, hMod, cProcessFullName, sizeof( cProcessFullName ) / sizeof( TCHAR ) );
      }

      CloseHandle( hProcess );
#ifndef UNICODE
      hb_retc( cProcessFullName );
#else
      pStr = WideToAnsi( cProcessFullName );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

//        TerminateProcess ( [ nProcessID ] , [ nExitCode ] )
HB_FUNC( TERMINATEPROCESS )
{
   DWORD    ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();
   UINT     uExitCode = hmg_par_UINT( 2 );
   HANDLE   hProcess = OpenProcess( PROCESS_TERMINATE, FALSE, ProcessID );

   if( hProcess )
   {
      TerminateProcess( hProcess, uExitCode );
      CloseHandle( hProcess );
   }

   hb_retl( hProcess != NULL );
}

HB_FUNC( REDRAWWINDOWCONTROLRECT )
{
   RECT  r;

   r.top = hb_parni( 2 );
   r.left = hb_parni( 3 );
   r.bottom = hb_parni( 4 );
   r.right = hb_parni( 5 );

   hb_retl( RedrawWindow( hmg_par_raw_HWND( 1 ), &r, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW ) );
}

HB_FUNC( ADDSPLITBOXITEM )
{
   HWND           hwnd = hmg_par_raw_HWND( 1 );
   UINT           Style = RBBS_CHILDEDGE | RBBS_GRIPPERALWAYS | RBBS_USECHEVRON;
   REBARBANDINFO  rbBand;
   RECT           rc;

#ifndef UNICODE
   LPSTR          lpText = ( LPSTR ) hb_parc( 5 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 5 ) );
#endif
   if( hb_parl( 4 ) )
   {
      Style |= RBBS_BREAK;
   }

   GetWindowRect( hwnd, &rc );

   rbBand.cbSize = sizeof( REBARBANDINFO );
   rbBand.fMask = RBBIM_TEXT | RBBIM_STYLE | RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_SIZE;
   rbBand.fStyle = Style;
   rbBand.hbmBack = 0;

   rbBand.lpText = lpText;
   rbBand.hwndChild = hwnd;

   if( hb_parni( 9 ) )
   {
      rbBand.fMask = rbBand.fMask | RBBIM_IDEALSIZE;
   }

   if( !hb_parl( 8 ) )
   {
      // Not Horizontal
      rbBand.cxMinChild = hb_parni( 6 ) ? hb_parni( 6 ) : 0;
      rbBand.cyMinChild = hb_parni( 7 ) ? hb_parni( 7 ) : rc.bottom - rc.top;
      rbBand.cx = hb_parni( 3 );
      if( hb_parni( 9 ) )
      {
         rbBand.cxIdeal = hb_parni( 6 ) ? hb_parni( 6 ) : 0;
         rbBand.cxMinChild = hb_parni( 9 );
      }
      else
      {
         rbBand.cxMinChild = hb_parni( 6 ) ? hb_parni( 6 ) : 0;
      }
   }
   else
   {
      // Horizontal
      if( hb_parni( 6 ) == 0 && hb_parni( 7 ) == 0 )
      {
         // Not ToolBar
         rbBand.cxMinChild = 0;
         rbBand.cyMinChild = rc.right - rc.left;
         rbBand.cx = rc.bottom - rc.top;
      }
      else
      {
         // ToolBar
         rbBand.cyMinChild = hb_parni( 6 ) ? hb_parni( 6 ) : 0;
         rbBand.cx = hb_parni( 7 ) ? hb_parni( 7 ) : rc.bottom - rc.top;
         if( hb_parni( 9 ) )
         {
            rbBand.cxIdeal = hb_parni( 7 ) ? hb_parni( 7 ) : rc.bottom - rc.top;
            rbBand.cxMinChild = hb_parni( 9 );
         }
         else
         {
            rbBand.cxMinChild = hb_parni( 7 ) ? hb_parni( 7 ) : rc.bottom - rc.top;
         }
      }
   }

   hmg_ret_LRESULT( SendMessage( hmg_par_raw_HWND( 2 ), RB_INSERTBAND, ( WPARAM ) - 1, ( LPARAM ) & rbBand ) );

#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

HB_FUNC( C_SETWINDOWRGN )
{
   HRGN     hRgn = NULL;
   HBITMAP  hbmp;

   if( hb_parni( 6 ) == 0 )
   {
      hb_retl( SetWindowRgn( GetActiveWindow(), NULL, TRUE ) );
   }
   else
   {
      switch( hb_parni( 6 ) )
      {
         case 1:
            hRgn = CreateRectRgn( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
            break;

         case 2:
            hRgn = CreateEllipticRgn( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
            break;

         case 3:
            hRgn = CreateRoundRectRgn( 0, 0, hb_parni( 4 ), hb_parni( 5 ), hb_parni( 2 ), hb_parni( 3 ) );
            break;

         case 4:
            hbmp = ( HBITMAP ) LoadImage( GetResources(), ( TCHAR * ) hb_parc( 2 ), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );
            if( hbmp == NULL )
            {
               hbmp = ( HBITMAP ) LoadImage( NULL, ( TCHAR * ) hb_parc( 2 ), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
            }

            hRgn = BitmapToRegion( hbmp, RGB( HB_PARNI( 3, 1 ), HB_PARNI( 3, 2 ), HB_PARNI( 3, 3 ) ), 0x101010 );
            DeleteObject( hbmp );
            break;

         default:
            break;
      }

      if( hRgn )
      {
         SetWindowRgn( hmg_par_raw_HWND( 1 ), hRgn, TRUE );
         RegisterResource( hRgn, "REGION" );
         hmg_ret_raw_HANDLE( hRgn );
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to create region", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
   }
}

HB_FUNC( C_SETPOLYWINDOWRGN )
{
   HRGN  hRgn;
   POINT lppt[512];
   int   i, fnPolyFillMode;
   int   nPoints = ( int ) hb_parinfa( 2, 0 );

   if( hb_parni( 4 ) == 1 )
   {
      fnPolyFillMode = WINDING;
   }
   else
   {
      fnPolyFillMode = ALTERNATE;
   }

   for( i = 0; i <= nPoints - 1; i++ )
   {
      lppt[i].x = HB_PARNI( 2, i + 1 );
      lppt[i].y = HB_PARNI( 3, i + 1 );
   }

   hRgn = CreatePolygonRgn( lppt, nPoints, fnPolyFillMode );

   if( hRgn )
   {
      SetWindowRgn( GetActiveWindow(), hRgn, TRUE );

      RegisterResource( hRgn, "REGION" );
      hmg_ret_raw_HANDLE( hRgn );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to create region", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( GETHELPDATA )
{
   hmg_ret_raw_HWND( ( ( ( HELPINFO FAR * ) HB_PARNL( 1 ) )->hItemHandle ) );
}

HB_FUNC( GETMSKTEXTMESSAGE )
{
   hmg_ret_raw_HWND( ( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->msg ) );
}

HB_FUNC( GETMSKTEXTWPARAM )
{
   hmg_ret_raw_HWND( ( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->wParam ) );
}

HB_FUNC( GETMSKTEXTLPARAM )
{
   hmg_ret_raw_HWND( ( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->lParam ) );
}

HB_FUNC( GETWINDOW )
{
   hmg_ret_raw_HWND( GetWindow( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( GETGRIDOLDSTATE )
{
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   NM_LISTVIEW *NMLV = ( NM_LISTVIEW * ) lParam;

   hmg_ret_UINT( NMLV->uOldState );
}

HB_FUNC( GETGRIDNEWSTATE )
{
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   NM_LISTVIEW *NMLV = ( NM_LISTVIEW * ) lParam;

   hmg_ret_UINT( NMLV->uNewState );
}

HB_FUNC( GETGRIDDISPINFOINDEX )
{
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   LV_DISPINFO *pDispInfo = ( LV_DISPINFO * ) lParam;

   int         iItem = pDispInfo->item.iItem;
   int         iSubItem = pDispInfo->item.iSubItem;

   hb_reta( 2 );
   HB_STORNI( iItem + 1, -1, 1 );
   HB_STORNI( iSubItem + 1, -1, 2 );
}

HB_FUNC( SETGRIDQUERYDATA )
{
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   LV_DISPINFO *pDispInfo = ( LV_DISPINFO * ) lParam;

   // Copy the text to the LV_ITEM structure
   // Maximum number of characters is in pDispInfo->Item.cchTextMax
#ifdef UNICODE
   LPWSTR      lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
   lstrcpyn( pDispInfo->item.pszText, lpText, pDispInfo->item.cchTextMax );
   hb_xfree( lpText );
#else
   lstrcpyn( pDispInfo->item.pszText, ( char * ) hb_parc( 2 ), pDispInfo->item.cchTextMax );
#endif
}

HB_FUNC( SETGRIDQUERYIMAGE )
{
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   LV_DISPINFO *pDispInfo = ( LV_DISPINFO * ) lParam;

   pDispInfo->item.iImage = hb_parni( 2 );
}

HB_FUNC( FINDWINDOWEX )
{
#ifndef UNICODE
   LPCSTR   lpszClass = ( char * ) hb_parc( 3 );
   LPCSTR   lpszWindow = ( char * ) hb_parc( 4 );
#else
   LPWSTR   lpszClass = ( hb_parc( 3 ) != NULL ) ? hb_osStrU16Encode( hb_parc( 3 ) ) : NULL;
   LPWSTR   lpszWindow = ( hb_parc( 4 ) != NULL ) ? hb_osStrU16Encode( hb_parc( 4 ) ) : NULL;
#endif
   hmg_ret_raw_HWND( FindWindowEx( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ), lpszClass, lpszWindow ) );

#ifdef UNICODE
   if( lpszClass != NULL )
   {
      hb_xfree( lpszClass );
   }

   if( lpszWindow != NULL )
   {
      hb_xfree( lpszWindow );
   }
#endif
}

HB_FUNC( GETDS )
{
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   if( lplvcd->nmcd.dwDrawStage == CDDS_PREPAINT )
   {
      hb_retni( CDRF_NOTIFYITEMDRAW );
   }
   else if( lplvcd->nmcd.dwDrawStage == CDDS_ITEMPREPAINT )
   {
      if( hb_pcount() > 1 )
      {
         if( ListView_GetNextItem( hmg_par_raw_HWND( 2 ), -1, LVNI_ALL | LVNI_SELECTED ) == hb_parni( 3 ) )
         {
            ListView_SetItemState( hmg_par_raw_HWND( 2 ), hb_parni( 3 ), 0, LVIS_SELECTED );
         }
      }

      hb_retni( CDRF_NOTIFYSUBITEMDRAW );
   }
   else if( lplvcd->nmcd.dwDrawStage == ( CDDS_SUBITEM | CDDS_ITEMPREPAINT ) )
   {
      hb_retni( -1 );
   }
   else
   {
      hb_retni( CDRF_DODEFAULT );
   }
}

HB_FUNC( GETRC )           // Get ListView CustomDraw Row and Column
{
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   hb_reta( 2 );
   HB_STORVNL( ( LONG ) lplvcd->nmcd.dwItemSpec + 1, -1, 1 );
   HB_STORNI( ( INT ) lplvcd->iSubItem + 1, -1, 2 );
}

HB_FUNC( SETBCFC )         // Set Dynamic BackColor and ForeColor
{
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   lplvcd->clrTextBk = hb_parni( 2 );
   lplvcd->clrText = hb_parni( 3 );

   hb_retni( CDRF_NEWFONT );
}

HB_FUNC( SETBRCCD )        // Set Default BackColor and ForeColor
{
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   lplvcd->clrText = RGB( 0, 0, 0 );
   lplvcd->clrTextBk = RGB( 255, 255, 255 );

   hb_retni( CDRF_NEWFONT );
}

HB_FUNC( GETTABBEDCONTROLBRUSH )
{
   RECT  rc;
   HDC   hDC = hmg_par_raw_HDC( 1 );

   SetBkMode( hDC, TRANSPARENT );
   GetWindowRect( hmg_par_raw_HWND( 2 ), &rc );
   MapWindowPoints( NULL, hmg_par_raw_HWND( 3 ), ( LPPOINT ) ( &rc ), 2 );
   SetBrushOrgEx( hDC, -rc.left, -rc.top, NULL );

   hmg_ret_raw_HBRUSH( hmg_par_raw_HBRUSH( 4 ) );
}

HB_FUNC( GETTABBRUSH )
{
   HBRUSH   hBrush;
   RECT     rc;
   HDC      hDC;
   HDC      hDCMem;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;
   HWND     hWnd = hmg_par_raw_HWND( 1 );

   GetWindowRect( hWnd, &rc );
   hDC = GetDC( hWnd );
   hDCMem = CreateCompatibleDC( hDC );

   hBmp = CreateCompatibleBitmap( hDC, rc.right - rc.left, rc.bottom - rc.top );

   hOldBmp = ( HBITMAP ) SelectObject( hDCMem, hBmp );

   SendMessage( hWnd, WM_PRINTCLIENT, ( WPARAM ) hDCMem, ( LPARAM ) PRF_ERASEBKGND | PRF_CLIENT | PRF_NONCLIENT );

   hBrush = CreatePatternBrush( hBmp );
   if( hBrush )
   {
      RegisterResource( hBrush, "BRUSH" );
      hmg_ret_raw_HBRUSH( hBrush );
   }

   SelectObject( hDCMem, hOldBmp );

   DeleteObject( hBmp );
   DeleteDC( hDCMem );
   ReleaseDC( hWnd, hDC );
}

HB_FUNC( INITMINMAXINFO )  // ( hWnd ) --> aMinMaxInfo
{
   LONG  x, y, mx, my;

   if( GetWindowLong( hmg_par_raw_HWND( 1 ), GWL_STYLE ) & WS_SIZEBOX )
   {
      x = -GetSystemMetrics( SM_CXFRAME );
      y = -GetSystemMetrics( SM_CYFRAME );
   }
   else
   {
      x = -GetSystemMetrics( SM_CXBORDER );
      y = -GetSystemMetrics( SM_CYBORDER );
   }

   mx = GetSystemMetrics( SM_CXSCREEN ) - 2 * x;
   my = GetSystemMetrics( SM_CYSCREEN ) - 2 * y;

   hb_reta( 8 );
   HB_STORVNL( ( LONG ) mx, -1, 1 );
   HB_STORVNL( ( LONG ) my, -1, 2 );
   HB_STORVNL( ( LONG ) x, -1, 3 );
   HB_STORVNL( ( LONG ) y, -1, 4 );
   HB_STORVNL( ( LONG ) 0, -1, 5 );
   HB_STORVNL( ( LONG ) 0, -1, 6 );
   HB_STORVNL( ( LONG ) mx, -1, 7 );
   HB_STORVNL( ( LONG ) my, -1, 8 );
}

HB_FUNC( SETMINMAXINFO )   // ( pMinMaxInfo, aMinMaxInfo ) --> 0
{
   MINMAXINFO  *pMinMaxInfo = ( MINMAXINFO * ) HB_PARNL( 1 );

   pMinMaxInfo->ptMaxSize.x = HB_PARNI( 2, 1 );
   pMinMaxInfo->ptMaxSize.y = HB_PARNI( 2, 2 );
   pMinMaxInfo->ptMaxPosition.x = HB_PARNI( 2, 3 );
   pMinMaxInfo->ptMaxPosition.y = HB_PARNI( 2, 4 );
   pMinMaxInfo->ptMinTrackSize.x = HB_PARNI( 2, 5 );
   pMinMaxInfo->ptMinTrackSize.y = HB_PARNI( 2, 6 );
   pMinMaxInfo->ptMaxTrackSize.x = HB_PARNI( 2, 7 );
   pMinMaxInfo->ptMaxTrackSize.y = HB_PARNI( 2, 8 );

   hb_retni( 0 );
}

HB_FUNC( LOCKWINDOWUPDATE )
{
   hmg_ret_L( LockWindowUpdate( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( SETSTANDBY )
{
   EXECUTION_STATE   esFlags = HB_ISLOG( 1 ) && hb_parl( 1 ) ? ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_DISPLAY_REQUIRED | ES_AWAYMODE_REQUIRED : ES_CONTINUOUS;

   hmg_ret_DWORD( SetThreadExecutionState( esFlags ) );
}

HB_FUNC( ISWINDOWHANDLE )
{
   hmg_ret_L( IsWindow( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( ISICONIC )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsIconic( hwnd ) : FALSE );
}

HB_FUNC( ISZOOMED )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsZoomed( hwnd ) : FALSE );
}

HB_FUNC( SETWINDOWHICON )
{
   hmg_ret_LONG_PTR( SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HICON, hmg_par_raw_LONG_PTR( 2 ) ) );
}

HB_FUNC( GETWINDOWBRUSH )
{
   hmg_ret_LONG_PTR( GetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HBRBACKGROUND ) );
}

HB_FUNC( SETWINDOWBRUSH )
{
   hmg_ret_LONG_PTR( SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HBRBACKGROUND, hmg_par_raw_LONG_PTR( 2 ) ) );
}

HB_FUNC( CREATEHATCHBRUSH )
{
   HBRUSH   hBrush = CreateHatchBrush( hb_parni( 1 ), hmg_par_COLORREF( 2 ) );

   RegisterResource( hBrush, "BRUSH" );
   hmg_ret_raw_HBRUSH( hBrush );
}

/* Modified by P.Ch. 16.10. */
HB_FUNC( CREATEPATTERNBRUSH )
{
   HBRUSH   hBrush;
   HBITMAP  hImage;

#ifndef UNICODE
   LPCTSTR  lpImageName = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : ( HB_ISNUM( 1 ) ? MAKEINTRESOURCE( hb_parni( 1 ) ) : NULL );
#else
   LPCWSTR  lpImageName = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : ( HB_ISNUM( 1 ) ? ( LPCWSTR ) MAKEINTRESOURCE( hb_parni( 1 ) ) : NULL );
#endif
   hImage = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );

   if( hImage == NULL && HB_ISCHAR( 1 ) )
   {
      hImage = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   }

   if( hImage == NULL )
   {
      hImage = ( HBITMAP ) HMG_LoadImage( hb_parc( 1 ) );
   }

   if( hImage == NULL )
   {
      hb_ret();
   }
   else
   {
      hBrush = CreatePatternBrush( hImage );

      RegisterResource( hBrush, "BRUSH" );
      hmg_ret_raw_HBRUSH( hBrush );
   }

#ifdef UNICODE
   if( HB_ISCHAR( 1 ) )
   {
      hb_xfree( ( TCHAR * ) lpImageName );
   }
#endif
}

/*
   BitmapToRegion: Create a region from the "non-transparent" pixels of a bitmap
   Author        : Jean-Edouard Lachand-Robert
   (http://www.geocities.com/Paris/LeftBank/1160/resume.htm), June 1998.

   hBmp :              Source bitmap
   cTransparentColor : Color base for the "transparent" pixels
                       (default is black)
   cTolerance :        Color tolerance for the "transparent" pixels.

   A pixel is assumed to be transparent if the value of each of its 3
   components (blue, green and red) is
   greater or equal to the corresponding value in cTransparentColor and is
   lower or equal to the corresponding value in cTransparentColor + cTolerance.
 */
#define ALLOC_UNIT   100

HRGN BitmapToRegion( HBITMAP hBmp, COLORREF cTransparentColor, COLORREF cTolerance )
{
   HRGN  hRgn = NULL;
   VOID  *pbits32;
   DWORD maxRects = ALLOC_UNIT;

   if( hBmp )
   {
      // Create a memory DC inside which we will scan the bitmap content
      HDC   hMemDC = CreateCompatibleDC( NULL );
      if( hMemDC )
      {
         BITMAP            bm;
         BITMAPINFOHEADER  RGB32BITSBITMAPINFO;
         HBITMAP           hbm32;

         // Get bitmap size
         GetObject( hBmp, sizeof( bm ), &bm );

         // Create a 32 bits depth bitmap and select it into the memory DC
         RGB32BITSBITMAPINFO.biSize = sizeof( BITMAPINFOHEADER );
         RGB32BITSBITMAPINFO.biWidth = bm.bmWidth;
         RGB32BITSBITMAPINFO.biHeight = bm.bmHeight;
         RGB32BITSBITMAPINFO.biPlanes = 1;
         RGB32BITSBITMAPINFO.biBitCount = 32;
         RGB32BITSBITMAPINFO.biCompression = BI_RGB;
         RGB32BITSBITMAPINFO.biSizeImage = 0;
         RGB32BITSBITMAPINFO.biXPelsPerMeter = 0;
         RGB32BITSBITMAPINFO.biYPelsPerMeter = 0;
         RGB32BITSBITMAPINFO.biClrUsed = 0;
         RGB32BITSBITMAPINFO.biClrImportant = 0;

         hbm32 = CreateDIBSection( hMemDC, ( BITMAPINFO * ) &RGB32BITSBITMAPINFO, DIB_RGB_COLORS, &pbits32, NULL, 0 );
         if( hbm32 )
         {
            HBITMAP  holdBmp = ( HBITMAP ) SelectObject( hMemDC, hbm32 );

            // Create a DC just to copy the bitmap into the memory DC
            HDC      hDC = CreateCompatibleDC( hMemDC );
            if( hDC )
            {
               // Get how many bytes per row we have for the bitmap bits (rounded up to 32 bits)
               BITMAP   bm32;
               HANDLE   hData;
               RGNDATA  *pData;
               BYTE     *p32;
               BYTE     lr, lg, lb, hr, hg, hb;
               INT      y, x;
               HRGN     h;

               GetObject( hbm32, sizeof( bm32 ), &bm32 );
               while( bm32.bmWidthBytes % 4 )
               {
                  bm32.bmWidthBytes++;
               }

               // Copy the bitmap into the memory DC
               holdBmp = ( HBITMAP ) SelectObject( hDC, hBmp );
               BitBlt( hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hDC, 0, 0, SRCCOPY );

               // For better performances, we will use the  ExtCreateRegion() function to create the  region.
               // This function take a RGNDATA structure on  entry.
               // We will add rectangles by amount of ALLOC_UNIT number in this structure.
               hData = GlobalAlloc( GMEM_MOVEABLE, sizeof( RGNDATAHEADER ) + ( sizeof( RECT ) * maxRects ) );

               pData = ( RGNDATA * ) GlobalLock( hData );
               pData->rdh.dwSize = sizeof( RGNDATAHEADER );
               pData->rdh.iType = RDH_RECTANGLES;
               pData->rdh.nCount = pData->rdh.nRgnSize = 0;
               SetRect( &pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0 );

               // Keep on hand highest and lowest values for the  "transparent" pixels
               lr = GetRValue( cTransparentColor );
               lg = GetGValue( cTransparentColor );
               lb = GetBValue( cTransparentColor );
               hr = ( BYTE ) HB_MIN( 0xff, lr + GetRValue( cTolerance ) );
               hg = ( BYTE ) HB_MIN( 0xff, lg + GetGValue( cTolerance ) );
               hb = ( BYTE ) HB_MIN( 0xff, lb + GetBValue( cTolerance ) );

               // Scan each bitmap row from bottom to top (the bitmap is  inverted vertically)
               p32 = ( BYTE * ) bm32.bmBits + ( bm32.bmHeight - 1 ) * bm32.bmWidthBytes;
               for( y = 0; y < bm.bmHeight; y++ )     // Scan each bitmap pixel from left to right
               {
                  for( x = 0; x < bm.bmWidth; x++ )   // Search for a continuous range of "non transparent pixels"
                  {
                     int   x0 = x;
                     LONG  *p = ( LONG * ) p32 + x;
                     while( x < bm.bmWidth )
                     {
                        BYTE  b = GetRValue( *p );
                        if( b >= lr && b <= hr )
                        {
                           b = GetGValue( *p );
                           if( b >= lg && b <= hg )
                           {
                              b = GetBValue( *p );
                              if( b >= lb && b <= hb )
                              {
                                 break;               // This pixel is "transparent"
                              }
                           }
                        }

                        p++;
                        x++;
                     }

                     if( x > x0 )                     // Add the pixels (x0, y) to (x, y+1) as a new rectangle in the region
                     {
                        RECT  *pr;
                        if( pData->rdh.nCount >= maxRects )
                        {
                           GlobalUnlock( hData );
                           maxRects += ALLOC_UNIT;
                           hData = GlobalReAlloc( hData, sizeof( RGNDATAHEADER ) + ( sizeof( RECT ) * maxRects ), GMEM_MOVEABLE );
                           pData = ( RGNDATA * ) GlobalLock( hData );
                        }

                        pr = ( RECT * ) &pData->Buffer;
                        SetRect( &pr[pData->rdh.nCount], x0, y, x, y + 1 );
                        if( x0 < pData->rdh.rcBound.left )
                        {
                           pData->rdh.rcBound.left = x0;
                        }

                        if( y < pData->rdh.rcBound.top )
                        {
                           pData->rdh.rcBound.top = y;
                        }

                        if( x > pData->rdh.rcBound.right )
                        {
                           pData->rdh.rcBound.right = x;
                        }

                        if( y + 1 > pData->rdh.rcBound.bottom )
                        {
                           pData->rdh.rcBound.bottom = y + 1;
                        }

                        pData->rdh.nCount++;

                        // On Windows98, ExtCreateRegion() may fail if  the number of rectangles is too
                        // large (ie: > 4000).
                        // Therefore, we have to create the region by multiple steps.
                        if( pData->rdh.nCount == 2000 )
                        {
                           h = ExtCreateRegion( NULL, sizeof( RGNDATAHEADER ) + ( sizeof( RECT ) * maxRects ), pData );
                           if( hRgn )
                           {
                              CombineRgn( hRgn, hRgn, h, RGN_OR );
                              DeleteObject( h );
                           }
                           else
                           {
                              hRgn = h;
                           }

                           pData->rdh.nCount = 0;
                           SetRect( &pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0 );
                        }
                     }
                  }

                  // Go to next row (remember, the bitmap is inverted vertically)
                  p32 -= bm32.bmWidthBytes;
               }

               // Create or extend the region with the remaining  rectangles
               h = ExtCreateRegion( NULL, sizeof( RGNDATAHEADER ) + ( sizeof( RECT ) * maxRects ), pData );
               if( hRgn )
               {
                  CombineRgn( hRgn, hRgn, h, RGN_OR );
                  DeleteObject( h );
               }
               else
               {
                  hRgn = h;
               }

               // Clean up
               GlobalFree( hData );
               SelectObject( hDC, holdBmp );
               DeleteDC( hDC );
            }

            DeleteObject( SelectObject( hMemDC, holdBmp ) );
         }

         DeleteDC( hMemDC );
      }
   }

   return hRgn;
}
