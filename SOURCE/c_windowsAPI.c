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

/*
 * FUNCTION: SetAcceleratorTable
 *
 * Sets the main window and accelerator table handles for the application.
 *
 * Parameters:
 *   hWnd: HWND - Handle to the main window.
 *   hHaccel: HACCEL - Handle to the accelerator table.
 *
 * Returns:
 *   BOOL - TRUE indicating successful assignment.
 *
 * Purpose:
 *   Assigns the main window handle and accelerator table handle for the application.
 */
BOOL SetAcceleratorTable( HWND hWnd, HACCEL hHaccel )
{
   g_hWndMain = hWnd;               // Assign the main window handle
   g_hAccel = hHaccel;              // Assign the accelerator table handle
   return TRUE;                     // Return TRUE indicating successful assignment
}

/*
 * FUNCTION: DOMESSAGELOOP
 *
 * Main application message loop for processing messages.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Continuously retrieves and processes messages until the application exits.
 */
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
 * FUNCTION: DOEVENTS
 *
 * Processes all pending messages in the message queue.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Cleans out the message loop and executes any other pending business.
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

/*
 * FUNCTION: EXITPROCESS
 *
 * Exits the current process and all its threads.
 *
 * Parameters:
 *   1: INT - Exit code for the process.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Terminates the application with the specified exit code.
 */
HB_FUNC( EXITPROCESS )
{
   ExitProcess( HB_ISNUM( 1 ) ? hb_parni( 1 ) : 0 );
}

/*
 * FUNCTION: SHOWWINDOW
 *
 * Shows or hides a window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: INT - Command to show or hide the window.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was previously visible, FALSE otherwise.
 *
 * Purpose:
 *   Sets the visibility state of the specified window.
 */
HB_FUNC( SHOWWINDOW )
{
   hb_retl( ShowWindow( hmg_par_raw_HWND( 1 ), HB_ISNUM( 2 ) ? hb_parni( 2 ) : SW_SHOW ) );
}

/*
 * FUNCTION: GETACTIVEWINDOW
 *
 * Retrieves the handle to the active window.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HWND - Handle to the active window.
 *
 * Purpose:
 *   Gets the handle of the currently active window.
 */
HB_FUNC( GETACTIVEWINDOW )
{
   hmg_ret_raw_HWND( GetActiveWindow() );
}

/*
 * FUNCTION: SETACTIVEWINDOW
 *
 * Activates a window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to activate.
 *
 * Returns:
 *   HWND - Handle to the previously active window.
 *
 * Purpose:
 *   Sets the specified window as the active window.
 */
HB_FUNC( SETACTIVEWINDOW )
{
   hmg_ret_raw_HWND( SetActiveWindow( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: POSTQUITMESSAGE
 *
 * Posts a quit message to the message queue.
 *
 * Parameters:
 *   1: INT - Exit code for the application.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Indicates to the system that the application has made a request to terminate.
 */
HB_FUNC( POSTQUITMESSAGE )
{
   PostQuitMessage( hb_parni( 1 ) );
}

/*
 * FUNCTION: DESTROYWINDOW
 *
 * Destroys the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to destroy.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully destroyed, FALSE otherwise.
 *
 * Purpose:
 *   Destroys the specified window, removing it from the screen and freeing memory.
 */
HB_FUNC( DESTROYWINDOW )
{
   hb_retl( DestroyWindow( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: ISWINDOWVISIBLE
 *
 * Determines whether the specified window is visible.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   LOGICAL - TRUE if the window is visible, FALSE otherwise.
 *
 * Purpose:
 *   Checks the visibility state of the specified window.
 */
HB_FUNC( ISWINDOWVISIBLE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsWindowVisible( hwnd ) : FALSE );
}

/*
 * FUNCTION: ISWINDOWENABLED
 *
 * Determines whether the specified window is enabled.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   LOGICAL - TRUE if the window is enabled, FALSE otherwise.
 *
 * Purpose:
 *   Checks the enabled state of the specified window.
 */
HB_FUNC( ISWINDOWENABLED )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsWindowEnabled( hwnd ) : FALSE );
}

/*
 * FUNCTION: ENABLEWINDOW
 *
 * Enables the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to enable.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was previously disabled, FALSE otherwise.
 *
 * Purpose:
 *   Enables the specified window so that it can receive input.
 */
HB_FUNC( ENABLEWINDOW )
{
   hb_retl( EnableWindow( hmg_par_raw_HWND( 1 ), TRUE ) );
}

/*
 * FUNCTION: DISABLEWINDOW
 *
 * Disables the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to disable.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was previously enabled, FALSE otherwise.
 *
 * Purpose:
 *   Disables the specified window so that it cannot receive input.
 */
HB_FUNC( DISABLEWINDOW )
{
   hb_retl( EnableWindow( hmg_par_raw_HWND( 1 ), FALSE ) );
}

/*
 * FUNCTION: SETFOREGROUNDWINDOW
 *
 * Brings the specified window to the foreground and activates it.
 *
 * Parameters:
 *   1: HWND - Handle to the window to bring to the foreground.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully brought to the foreground, FALSE otherwise.
 *
 * Purpose:
 *   Activates and brings the specified window to the top of the Z order.
 */
HB_FUNC( SETFOREGROUNDWINDOW )
{
   hb_retl( SetForegroundWindow( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: BRINGWINDOWTOTOP
 *
 * Brings the specified window to the top of the Z order.
 *
 * Parameters:
 *   1: HWND - Handle to the window to bring to the top.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully brought to the top, FALSE otherwise.
 *
 * Purpose:
 *   Brings the specified window to the top of the Z order without activating it.
 */
HB_FUNC( BRINGWINDOWTOTOP )
{
   hb_retl( BringWindowToTop( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: GETFOREGROUNDWINDOW
 *
 * Retrieves the handle to the foreground window.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HWND - Handle to the foreground window.
 *
 * Purpose:
 *   Gets the handle of the window that is currently in the foreground.
 */
HB_FUNC( GETFOREGROUNDWINDOW )
{
   hmg_ret_raw_HWND( GetForegroundWindow() );
}

/*
 * FUNCTION: SETWINDOWTEXT
 *
 * Sets the text of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: LPCSTR/LPCWSTR - Text to set.
 *
 * Returns:
 *   LOGICAL - TRUE if the text was successfully set, FALSE otherwise.
 *
 * Purpose:
 *   Changes the title or text of the specified window.
 */
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

/*
 * FUNCTION: SETWINDOWTEXTW
 *
 * Sets the text of the specified window using wide characters.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: LPCWSTR - Wide character text to set.
 *
 * Returns:
 *   LOGICAL - TRUE if the text was successfully set, FALSE otherwise.
 *
 * Purpose:
 *   Changes the title or text of the specified window using wide characters.
 */
HB_FUNC( SETWINDOWTEXTW )
{
   hb_retl( SetWindowTextW( hmg_par_raw_HWND( 1 ), ( LPCWSTR ) hb_parc( 2 ) ) );
}

/*
 * FUNCTION: SETWINDOWPOS
 *
 * Sets the position and size of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: HWND - Handle to the window to precede the positioned window in the Z order.
 *   3: INT - X coordinate of the window.
 *   4: INT - Y coordinate of the window.
 *   5: INT - Width of the window.
 *   6: INT - Height of the window.
 *   7: UINT - Window positioning flags.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully positioned, FALSE otherwise.
 *
 * Purpose:
 *   Changes the size, position, and Z order of the specified window.
 */
HB_FUNC( SETWINDOWPOS )
{
   hb_retl( SetWindowPos( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hmg_par_UINT( 7 ) ) );
}

/*
 * FUNCTION: ANIMATEWINDOW
 *
 * Animates the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to animate.
 *   2: DWORD - Time to animate the window in milliseconds.
 *   3: DWORD - Animation type.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully animated, FALSE otherwise.
 *
 * Purpose:
 *   Animates the specified window with the given animation type and duration.
 */
HB_FUNC( ANIMATEWINDOW )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   DWORD dwTime = hmg_par_DWORD( 2 );
   DWORD dwFlags = hmg_par_DWORD( 3 );

   hb_retl( AnimateWindow( hWnd, dwTime, dwFlags ) );
}

/*
 * FUNCTION: FLASHWINDOWEX
 *
 * Flashes the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to flash.
 *   2: DWORD - Flash status.
 *   3: UINT - Number of times to flash the window.
 *   4: DWORD - Flash timeout in milliseconds.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully flashed, FALSE otherwise.
 *
 * Purpose:
 *   Flashes the specified window to attract the user's attention.
 */
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

/*
 * FUNCTION: SETLAYEREDWINDOWATTRIBUTES
 *
 * Sets the opacity and transparency color key of a layered window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: COLORREF - Transparency color key.
 *   3: BYTE - Alpha value for the window.
 *   4: DWORD - Layered window attributes.
 *
 * Returns:
 *   LOGICAL - TRUE if the attributes were successfully set, FALSE otherwise.
 *
 * Purpose:
 *   Sets the opacity and transparency color key for a layered window.
 */
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

/*
 * FUNCTION: CenterIntoParent
 *
 * Centers a window relative to its parent window.
 *
 * Parameters:
 *   hwnd: HWND - Handle to the window to center.
 *
 * Returns:
 *   BOOL - TRUE if the window was successfully centered, FALSE otherwise.
 *
 * Purpose:
 *   Centers the specified window relative to its parent window, ensuring it stays within screen boundaries.
 */
static BOOL CenterIntoParent( HWND hwnd )
{
   HWND  hwndParent;
   RECT  rect, rectP;
   int   width, height;
   int   screenwidth, screenheight;
   int   x, y;

   // Make the window relative to its parent
   hwndParent = GetParent( hwnd );

   GetWindowRect( hwnd, &rect );
   GetWindowRect( hwndParent, &rectP );

   width = rect.right - rect.left;
   height = rect.bottom - rect.top;

   x = ( ( rectP.right - rectP.left ) - width ) / 2 + rectP.left;
   y = ( ( rectP.bottom - rectP.top ) - height ) / 2 + rectP.top;

   screenwidth = GetSystemMetrics( SM_CXSCREEN );
   screenheight = GetSystemMetrics( SM_CYSCREEN );

   // Make sure that the child window never moves outside of the screen
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

/*
 * FUNCTION: C_CENTER
 *
 * Centers a window on the screen or relative to its parent.
 *
 * Parameters:
 *   1: HWND - Handle to the window to center.
 *   2: LOGICAL - Flag to determine if the window should be centered relative to its parent.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully centered, FALSE otherwise.
 *
 * Purpose:
 *   Centers the specified window on the screen or relative to its parent window.
 */
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

/*
 * FUNCTION: SwitchToWindow (for older Borland compilers)
 *
 * Switches focus to the specified window.
 *
 * Parameters:
 *   hwnd: HWND - Handle to the window to switch to.
 *   fRestore: BOOL - Flag to restore the window if minimized.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Sets focus to the specified window.
 */
void SwitchToWindow( HWND hwnd, BOOL fRestore )
{
   HB_SYMBOL_UNUSED( fRestore );
   SetFocus( hwnd );
}

#else

/*
 * FUNCTION: SwitchToWindow
 *
 * Switches focus to the specified window.
 *
 * Parameters:
 *   hwnd: HWND - Handle to the window to switch to.
 *   fRestore: BOOL - Flag to restore the window if minimized.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Sets focus to the specified window and optionally restores it if minimized.
 */
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

/*
 * FUNCTION: SWITCHTOTHISWINDOW
 *
 * Switches focus to the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window to switch to.
 *   2: LOGICAL - Flag to restore the window if minimized.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully brought to the foreground, FALSE otherwise.
 *
 * Purpose:
 *   Sets focus to the specified window and optionally restores it if minimized.
 */
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

/*
 * FUNCTION: GETWINDOWTEXT
 *
 * Retrieves the text of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   LPCSTR/LPCWSTR - Text of the window.
 *
 * Purpose:
 *   Gets the title or text of the specified window.
 */
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

/*
 * FUNCTION: SENDMESSAGE
 *
 * Sends a message to the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: UINT - Message to send.
 *   3: WPARAM - Additional message-specific information.
 *   4: LPARAM - Additional message-specific information.
 *
 * Returns:
 *   LRESULT - Result of the message processing.
 *
 * Purpose:
 *   Sends a specified message to a window or windows and does not return until the message is processed.
 */
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

/*
 * FUNCTION: SENDMESSAGESTRING
 *
 * Sends a string message to the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: UINT - Message to send.
 *   3: WPARAM - Additional message-specific information.
 *   4: LPCSTR/LPCWSTR - String message to send.
 *
 * Returns:
 *   LRESULT - Result of the message processing.
 *
 * Purpose:
 *   Sends a specified string message to a window or windows and does not return until the message is processed.
 */
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

/*
 * FUNCTION: GETNOTIFYCODE
 *
 * Retrieves the notification code from a notification message.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the notification message data.
 *
 * Returns:
 *   INT - Notification code.
 *
 * Purpose:
 *   Gets the notification code from a notification message.
 */
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

/*
 * FUNCTION: GETNOTIFYLINK
 *
 * Retrieves link information from a notification message.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the notification message data.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Gets the link information from a notification message.
 */
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

/*
 * FUNCTION: GETNOTIFYID
 *
 * Retrieves the identifier of the control sending the notification.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the notification message data.
 *
 * Returns:
 *   HWND - Handle to the control sending the notification.
 *
 * Purpose:
 *   Gets the identifier of the control sending the notification.
 */
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

/*
 * FUNCTION: GETHWNDFROM
 *
 * Retrieves the handle to the window sending the notification.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the notification message data.
 *
 * Returns:
 *   HWND - Handle to the window sending the notification.
 *
 * Purpose:
 *   Gets the handle to the window sending the notification.
 */
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

/*
 * FUNCTION: GETDRAWITEMHANDLE
 *
 * Retrieves the handle to the item from a DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   HWND - Handle to the item.
 *
 * Purpose:
 *   Gets the handle to the item from a DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETDRAWITEMHANDLE )
{
   hmg_ret_raw_HWND( ( ( DRAWITEMSTRUCT FAR * ) HB_PARNL( 1 ) )->hwndItem );
}

/*
 * FUNCTION: GETFOCUS
 *
 * Retrieves the handle to the window that has the keyboard focus.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HWND - Handle to the window with the keyboard focus.
 *
 * Purpose:
 *   Gets the handle to the window that currently has the keyboard focus.
 */
HB_FUNC( GETFOCUS )
{
   hmg_ret_raw_HWND( GetFocus() );
}

/*
 * FUNCTION: GETGRIDCOLUMN
 *
 * Retrieves the column index from a grid notification message.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the grid notification message data.
 *
 * Returns:
 *   INT - Column index.
 *
 * Purpose:
 *   Gets the column index from a grid notification message.
 */
HB_FUNC( GETGRIDCOLUMN )
{
   hmg_ret_NINT( ( LPARAM ) ( ( NM_LISTVIEW * ) HB_PARNL( 1 ) )->iSubItem );
}

/*
 * FUNCTION: GETGRIDVKEY
 *
 * Retrieves the virtual key code from a grid key down notification message.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the grid key down notification message data.
 *
 * Returns:
 *   WORD - Virtual key code.
 *
 * Purpose:
 *   Gets the virtual key code from a grid key down notification message.
 */
HB_FUNC( GETGRIDVKEY )
{
   hmg_ret_WORD( ( WORD ) ( ( LV_KEYDOWN * ) HB_PARNL( 1 ) )->wVKey );
}

/*
 * FUNCTION: MOVEWINDOW
 *
 * Moves the specified window to a new position and optionally changes its size.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: INT - X coordinate of the new position.
 *   3: INT - Y coordinate of the new position.
 *   4: INT - New width of the window.
 *   5: INT - New height of the window.
 *   6: LOGICAL - Flag to repaint the window.
 *
 * Returns:
 *   LOGICAL - TRUE if the window was successfully moved, FALSE otherwise.
 *
 * Purpose:
 *   Changes the position and optionally the size of the specified window.
 */
HB_FUNC( MOVEWINDOW )
{
   hb_retl( MoveWindow( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNIL( 6 ) ? TRUE : hb_parl( 6 ) ) ) );
}

/*
 * FUNCTION: GETSYSTEMMETRICS
 *
 * Retrieves the specified system metric or system configuration setting.
 *
 * Parameters:
 *   1: INT - System metric or configuration setting to retrieve.
 *
 * Returns:
 *   INT - The requested system metric or configuration setting.
 *
 * Purpose:
 *   Gets the specified system metric or configuration setting.
 */
HB_FUNC( GETSYSTEMMETRICS )
{
   hmg_ret_NINT( GetSystemMetrics( hb_parni( 1 ) ) );
}

/*
 * FUNCTION: GETWINDOWRECT
 *
 * Retrieves the dimensions of the bounding rectangle of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: INT - Optional parameter to specify which dimension to retrieve.
 *
 * Returns:
 *   INT - The requested dimension or an array of dimensions.
 *
 * Purpose:
 *   Gets the dimensions of the bounding rectangle of the specified window.
 */
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

/*
 * FUNCTION: GETCLIENTRECT
 *
 * Retrieves the dimensions of the client area of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   LOGICAL - TRUE if the dimensions were successfully retrieved, FALSE otherwise.
 *
 * Purpose:
 *   Gets the dimensions of the client area of the specified window.
 */
HB_FUNC( GETCLIENTRECT )
{
   RECT  rect;

   hb_retl( GetClientRect( hmg_par_raw_HWND( 1 ), &rect ) );

   HB_STORVNL( rect.left, 2, 1 );
   HB_STORVNL( rect.top, 2, 2 );
   HB_STORVNL( rect.right, 2, 3 );
   HB_STORVNL( rect.bottom, 2, 4 );
}

/*
 * FUNCTION: GETDESKTOPAREA
 *
 * Retrieves the dimensions of the desktop work area.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   Array - Array containing the dimensions of the desktop work area.
 *
 * Purpose:
 *   Gets the dimensions of the desktop work area.
 */
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

/*
 * FUNCTION: GETTASKBARHEIGHT
 *
 * Retrieves the height of the taskbar.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   INT - Height of the taskbar.
 *
 * Purpose:
 *   Gets the height of the taskbar.
 */
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

/*
 * FUNCTION: ShowNotifyIcon
 *
 * Shows or hides a notification icon in the taskbar status area.
 *
 * Parameters:
 *   hWnd: HWND - Handle to the window that owns the icon.
 *   bAdd: BOOL - Flag to add or remove the icon.
 *   hIcon: HICON - Handle to the icon.
 *   szText: TCHAR* - Text to display when the mouse hovers over the icon.
 *
 * Returns:
 *   BOOL - TRUE if the icon was successfully shown or hidden, FALSE otherwise.
 *
 * Purpose:
 *   Adds or removes a notification icon in the taskbar status area.
 */
static BOOL ShowNotifyIcon( HWND hWnd, BOOL bAdd, HICON hIcon, TCHAR *szText )
{
   NOTIFYICONDATA nid;

   ZeroMemory( &nid, sizeof( NOTIFYICONDATA ) );

   nid.cbSize = sizeof( NOTIFYICONDATA );
   nid.hIcon = hIcon;
   nid.hWnd = hWnd;
   nid.uID = 0;
   nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
   nid.uCallbackMessage = WM_TASKBAR;
   lstrcpy( nid.szTip, szText );

   return Shell_NotifyIcon( bAdd ? NIM_ADD : NIM_DELETE, &nid );
}

/*
 * FUNCTION: SHOWNOTIFYICON
 *
 * Shows or hides a notification icon in the taskbar status area.
 *
 * Parameters:
 *   1: HWND - Handle to the window that owns the icon.
 *   2: LOGICAL - Flag to add or remove the icon.
 *   3: HICON - Handle to the icon.
 *   4: LPCSTR/LPCWSTR - Text to display when the mouse hovers over the icon.
 *
 * Returns:
 *   LOGICAL - TRUE if the icon was successfully shown or hidden, FALSE otherwise.
 *
 * Purpose:
 *   Adds or removes a notification icon in the taskbar status area.
 */
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

/*
 * FUNCTION: GETCURSORPOS
 *
 * Retrieves the position of the cursor.
 *
 * Parameters:
 *   1: HWND - Optional handle to the window to convert the cursor position to client coordinates.
 *
 * Returns:
 *   Array - Array containing the X and Y coordinates of the cursor.
 *
 * Purpose:
 *   Gets the position of the cursor in screen or client coordinates.
 */
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

/*
 * FUNCTION: SCREENTOCLIENT
 *
 * Converts the screen coordinates of a specified point to client coordinates.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: INT - X coordinate of the point in screen coordinates.
 *   3: INT - Y coordinate of the point in screen coordinates.
 *
 * Returns:
 *   Array - Array containing the X and Y coordinates of the point in client coordinates.
 *
 * Purpose:
 *   Converts the screen coordinates of a specified point to client coordinates.
 */
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

/*
 * FUNCTION: CLIENTTOSCREEN
 *
 * Converts the client coordinates of a specified point to screen coordinates.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *   2: INT - X coordinate of the point in client coordinates.
 *   3: INT - Y coordinate of the point in client coordinates.
 *
 * Returns:
 *   LOGICAL - TRUE if the conversion was successful, FALSE otherwise.
 *
 * Purpose:
 *   Converts the client coordinates of a specified point to screen coordinates.
 */
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

/*
 * FUNCTION: LOADTRAYICON
 *
 * Loads an icon for use in the system tray.
 *
 * Parameters:
 *   1: HINSTANCE - Handle to the application instance.
 *   2: LPCSTR/LPCWSTR - Name string or resource identifier of the icon.
 *   3: INT - Optional width of the icon.
 *   4: INT - Optional height of the icon.
 *
 * Returns:
 *   HICON - Handle to the loaded icon.
 *
 * Purpose:
 *   Loads an icon from a resource or file for use in the system tray.
 */
HB_FUNC( LOADTRAYICON )
{
   HICON       hIcon;
   HINSTANCE   hInstance = hmg_par_raw_HINSTANCE( 1 );   // Handle to application instance
#ifndef UNICODE
   LPCTSTR     lpIconName = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : MAKEINTRESOURCE( hb_parni( 2 ) );   // Name string or resource identifier
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

/*
 * FUNCTION: ChangeNotifyIcon
 *
 * Changes the icon, tooltip, or both in the notification area.
 *
 * Parameters:
 *   hWnd: HWND - Handle to the window that owns the icon.
 *   hIcon: HICON - Handle to the new icon.
 *   szText: TCHAR* - New tooltip text.
 *
 * Returns:
 *   BOOL - TRUE if the icon was successfully changed, FALSE otherwise.
 *
 * Purpose:
 *   Modifies the icon or tooltip text of an existing notification icon.
 */
static BOOL ChangeNotifyIcon( HWND hWnd, HICON hIcon, TCHAR *szText )
{
   NOTIFYICONDATA nid;

   ZeroMemory( &nid, sizeof( NOTIFYICONDATA ) );

   nid.cbSize = sizeof( NOTIFYICONDATA );
   nid.hIcon = hIcon;
   nid.hWnd = hWnd;
   nid.uID = 0;
   nid.uFlags = NIF_ICON | NIF_TIP;
   lstrcpy( nid.szTip, szText );

   return Shell_NotifyIcon( NIM_MODIFY, &nid );
}

/*
 * FUNCTION: CHANGENOTIFYICON
 *
 * Changes the icon, tooltip, or both in the notification area.
 *
 * Parameters:
 *   1: HWND - Handle to the window that owns the icon.
 *   2: HICON - Handle to the new icon.
 *   3: LPCSTR/LPCWSTR - New tooltip text.
 *
 * Returns:
 *   LOGICAL - TRUE if the icon was successfully changed, FALSE otherwise.
 *
 * Purpose:
 *   Modifies the icon or tooltip text of an existing notification icon.
 */
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

/*
 * FUNCTION: GETITEMPOS
 *
 * Retrieves the item position from a notification message.
 *
 * Parameters:
 *   1: LPARAM - Pointer to the notification message data.
 *
 * Returns:
 *   HWND - Handle to the item.
 *
 * Purpose:
 *   Gets the item position from a notification message.
 */
HB_FUNC( GETITEMPOS )
{
   hmg_ret_raw_HWND( ( ( NMMOUSE FAR * ) HB_PARNL( 1 ) )->dwItemSpec );
}

/*
 * FUNCTION: SETSCROLLRANGE
 *
 * Sets the scroll range for a scroll bar.
 *
 * Parameters:
 *   1: HWND - Handle to the window with the scroll bar.
 *   2: INT - Scroll bar type.
 *   3: INT - Minimum scrolling position.
 *   4: INT - Maximum scrolling position.
 *   5: LOGICAL - Flag to redraw the scroll bar.
 *
 * Returns:
 *   LOGICAL - TRUE if the scroll range was successfully set, FALSE otherwise.
 *
 * Purpose:
 *   Sets the minimum and maximum scrolling positions for the specified scroll bar.
 */
HB_FUNC( SETSCROLLRANGE )
{
   hb_retl( SetScrollRange( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parl( 5 ) ) );
}

/*
 * FUNCTION: GETSCROLLPOS
 *
 * Retrieves the current position of the scroll box in the specified scroll bar.
 *
 * Parameters:
 *   1: HWND - Handle to the window with the scroll bar.
 *   2: INT - Scroll bar type.
 *
 * Returns:
 *   INT - Current position of the scroll box.
 *
 * Purpose:
 *   Gets the current position of the scroll box in the specified scroll bar.
 */
HB_FUNC( GETSCROLLPOS )
{
   hmg_ret_NINT( GetScrollPos( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

/*
 * FUNCTION: GETWINDOWSTATE
 *
 * Retrieves the show state and the restored, minimized, and maximized positions of a window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   UINT - The show state of the window.
 *
 * Purpose:
 *   Gets the show state and the restored, minimized, and maximized positions of the specified window.
 */
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

/*
 * FUNCTION: GETPARENT
 *
 * Retrieves the handle to the parent of the specified window.
 *
 * Parameters:
 *   1: HWND - Handle to the window.
 *
 * Returns:
 *   HWND - Handle to the parent window.
 *
 * Purpose:
 *   Gets the handle to the parent of the specified window.
 */
HB_FUNC( GETPARENT )
{
   hmg_ret_raw_HWND( GetParent( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: SETPARENT
 *
 * Changes the parent window of the specified child window.
 *
 * Parameters:
 *   1: HWND - Handle to the child window.
 *   2: HWND - Handle to the new parent window.
 *
 * Returns:
 *   HWND - Handle to the previous parent window.
 *
 * Purpose:
 *   Changes the parent of the specified child window to the new parent window.
 */
HB_FUNC( SETPARENT )
{
   HWND  hWnd = SetParent( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ) );
   hmg_ret_raw_HWND( hWnd );
}

/*
 * FUNCTION: GETDESKTOPWINDOW
 *
 * Retrieves the handle to the desktop window.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HWND - Handle to the desktop window.
 *
 * Purpose:
 *   Gets the handle to the desktop window.
 */
HB_FUNC( GETDESKTOPWINDOW )
{
   hmg_ret_raw_HWND( GetDesktopWindow() );
}

/*
 * FUNCTION: EnumWindowsProc
 *
 * Callback function for enumerating top-level windows.
 *
 * Parameters:
 *   hWnd: HWND - Handle to the window.
 *   pArray: LPARAM - Pointer to the array to store window handles.
 *
 * Returns:
 *   BOOL - TRUE to continue enumeration, FALSE to stop.
 *
 * Purpose:
 *   Enumerates all top-level windows and stores their handles in an array.
 */
static BOOL CALLBACK EnumWindowsProc( HWND hWnd, LPARAM pArray )
{
   PHB_ITEM pHWnd = hb_itemPutNInt( NULL, ( LONG_PTR ) hWnd );

   hb_arrayAddForward( ( PHB_ITEM ) pArray, pHWnd );
   hb_itemRelease( pHWnd );

   return TRUE;
}

/*
 * FUNCTION: ENUMWINDOWS
 *
 * Enumerates all top-level windows.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   Array - Array of window handles.
 *
 * Purpose:
 *   Enumerates all top-level windows and returns an array of their handles.
 */
HB_FUNC( ENUMWINDOWS )
{
   PHB_ITEM pArray = hb_itemArrayNew( 0 );

   EnumWindows( ( WNDENUMPROC ) EnumWindowsProc, ( LPARAM ) pArray );

   hb_itemReturnRelease( pArray );
}

/*
 * FUNCTION: EnumChildProc
 *
 * Callback function for enumerating child windows.
 *
 * Parameters:
 *   hWnd: HWND - Handle to the child window.
 *   lParam: LPARAM - Pointer to the code block to execute for each child window.
 *
 * Returns:
 *   BOOL - TRUE to continue enumeration, FALSE to stop.
 *
 * Purpose:
 *   Enumerates all child windows of a specified parent window and executes a code block for each child window.
 */
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

/*
 * FUNCTION: C_ENUMCHILDWINDOWS
 *
 * Enumerates all child windows of a specified parent window.
 *
 * Parameters:
 *   1: HWND - Handle to the parent window.
 *   2: Block - Code block to execute for each child window.
 */
HB_FUNC( C_ENUMCHILDWINDOWS )
{
   // Retrieve the handle to the parent window from the first parameter
   HWND     hWnd = hmg_par_raw_HWND( 1 );

   // Retrieve the code block from the second parameter, defaulting to HB_IT_BLOCK if not provided
   PHB_ITEM pCodeBlock = hb_param( 2, HB_IT_BLOCK );

   // Check if the window handle is valid and if the code block is provided
   if( IsWindow( hWnd ) && pCodeBlock )
   {
      // Call EnumChildWindows with the provided window handle, the callback function, and the code block as a parameter
      // Return the result of EnumChildWindows as a logical value
      hmg_ret_L( EnumChildWindows( hWnd, EnumChildProc, ( LPARAM ) pCodeBlock ) );
   }
}

// Function: IsWow64Process
// Determines whether the specified process is running under WOW64.
// Parameters:
// [nProcessID] - Optional process ID. If not provided, the current process is used.
// Returns: lBoolean - TRUE if the process is running under WOW64, FALSE otherwise.
HB_FUNC( ISWOW64PROCESS )
{
   // Define a function pointer type for the IsWow64Process function
   typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

   // Static variable to hold the function pointer
   static LPFN_ISWOW64PROCESS fnIsWow64Process = NULL;

   // Variable to store the result
   BOOL                       IsWow64 = FALSE;

   // If the function pointer is NULL, retrieve it using wapi_GetProcAddress
   if( fnIsWow64Process == NULL )
   {
      fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( GetModuleHandle( TEXT( "kernel32" ) ), "IsWow64Process" );
   }

   // If the function pointer is not NULL, proceed to call the function
   if( fnIsWow64Process != NULL )
   {
      // If the first parameter is not a number, use the current process
      if( !HB_ISNUM( 1 ) )
      {
         fnIsWow64Process( GetCurrentProcess(), &IsWow64 );
      }
      else
      {
         // Otherwise, open the specified process and call the function
         DWORD    ProcessID = hmg_par_DWORD( 1 );
         HANDLE   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
         if( hProcess )
         {
            fnIsWow64Process( hProcess, &IsWow64 );
            CloseHandle( hProcess );
         }
      }
   }

   // Return the result
   hb_retl( IsWow64 );
}

// Function: GetCurrentProcessId
// Retrieves the process identifier of the calling process.
// Returns: nProcessID - The process identifier.
HB_FUNC( GETCURRENTPROCESSID )
{
   // Retrieve and return the current process ID
   hmg_ret_NINT( GetCurrentProcessId() );
}

// Function: EnumProcessesID
// Enumerates all processes and returns an array of process identifiers.
// Returns: Array { nProcessID1, nProcessID2, ... } - An array of process identifiers.
HB_FUNC( ENUMPROCESSESID )
{
   // Define a function pointer type for the EnumProcesses function
   typedef BOOL ( WINAPI *Func_EnumProcesses ) ( DWORD *, DWORD, DWORD * );

   // Static variable to hold the function pointer
   static Func_EnumProcesses  pEnumProcesses = NULL;

   // Array to store process IDs and variables for needed size and number of processes
   DWORD                      aProcessesID[1024], cbNeeded, nProcesses;
   unsigned int               i;

   // Create a new array to store the process IDs
   PHB_ITEM                   pArray = hb_itemArrayNew( 0 );

   // If the function pointer is NULL, retrieve it using wapi_GetProcAddress
   if( pEnumProcesses == NULL )
   {
      HMODULE  hLib = LoadLibrary( TEXT( "Psapi.dll" ) );
      pEnumProcesses = ( Func_EnumProcesses ) wapi_GetProcAddress( hLib, "EnumProcesses" );
   }

   // If the function pointer is still NULL, return without doing anything
   if( pEnumProcesses == NULL )
   {
      return;
   }

   // Get the list of process identifiers
   if( pEnumProcesses( aProcessesID, sizeof( aProcessesID ), &cbNeeded ) == FALSE )
   {
      return;
   }

   // Calculate how many process identifiers were returned
   nProcesses = cbNeeded / sizeof( DWORD );

   // Iterate through the process IDs and add them to the array
   for( i = 0; i < nProcesses; i++ )
   {
      if( aProcessesID[i] != 0 )
      {
         PHB_ITEM pItem = hb_itemPutNL( NULL, ( LONG ) aProcessesID[i] );
         hb_arrayAddForward( pArray, pItem );
         hb_itemRelease( pItem );
      }
   }

   // Return the array of process IDs
   hb_itemReturnRelease( pArray );
}

// Function: GetWindowThreadProcessId
// Retrieves the identifier of the thread that created the specified window and, optionally, the identifier of the process that created the window.
// Parameters:
// hWnd - Handle to the window.
// @nThread - Reference to store the thread identifier.
// @nProcessID - Reference to store the process identifier.
HB_FUNC( GETWINDOWTHREADPROCESSID )
{
   // Variables to store the thread and process identifiers
   DWORD nThread, nProcessID;

   // Retrieve the thread identifier and optionally the process identifier
   nThread = GetWindowThreadProcessId( hmg_par_raw_HWND( 1 ), &nProcessID );

   // Store the thread identifier if the second parameter is a reference
   if( HB_ISBYREF( 2 ) )
   {
      hb_storni( nThread, 2 );
   }

   // Store the process identifier if the third parameter is a reference
   if( HB_ISBYREF( 3 ) )
   {
      hb_storni( nProcessID, 3 );
   }
}

// Function: GetProcessName
// Retrieves the name of the executable file for the specified process.
// Parameters:
// [nProcessID] - Optional process ID. If not provided, the current process is used.
// Returns: cProcessName - The name of the executable file.
HB_FUNC( GETPROCESSNAME )
{
   // Define function pointer types for EnumProcessModules and GetModuleBaseName
   typedef BOOL ( WINAPI *Func_EnumProcessModules ) ( HANDLE, HMODULE *, DWORD, LPDWORD );

   typedef DWORD ( WINAPI *Func_GetModuleBaseName ) ( HANDLE, HMODULE, LPTSTR, DWORD );

   // Static variables to hold the function pointers
   static Func_EnumProcessModules   pEnumProcessModules = NULL;
   static Func_GetModuleBaseName    pGetModuleBaseName = NULL;

#ifdef UNICODE
   LPSTR                            pStr;
#endif

   // Retrieve the process ID from the first parameter or use the current process ID
   DWORD                            ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();
   TCHAR                            cProcessName[MAX_PATH] = _TEXT( "" );
   HANDLE                           hProcess;

   // If the function pointer for EnumProcessModules is NULL, retrieve it
   if( pEnumProcessModules == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );
      pEnumProcessModules = ( Func_EnumProcessModules ) wapi_GetProcAddress( hLib, "EnumProcessModules" );
   }

   // If the function pointer is still NULL, return without doing anything
   if( pEnumProcessModules == NULL )
   {
      return;
   }

   // If the function pointer for GetModuleBaseName is NULL, retrieve it
   if( pGetModuleBaseName == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );

#ifdef UNICODE
      pGetModuleBaseName = ( Func_GetModuleBaseName ) wapi_GetProcAddress( hLib, "GetModuleBaseNameW" );
#else
      pGetModuleBaseName = ( Func_GetModuleBaseName ) wapi_GetProcAddress( hLib, "GetModuleBaseNameA" );
#endif
   }

   // If the function pointer is still NULL, return without doing anything
   if( pGetModuleBaseName == NULL )
   {
      return;
   }

   // Open the process with the specified ID
   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
   if( hProcess )
   {
      HMODULE  hMod;
      DWORD    cbNeeded;

      // Enumerate the modules in the process and retrieve the base name of the first module
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

// Function: GetProcessFullName
// Retrieves the full path of the executable file for the specified process.
// Parameters:
// [nProcessID] - Optional process ID. If not provided, the current process is used.
// Returns: cProcessFullName - The full path of the executable file.
HB_FUNC( GETPROCESSFULLNAME )
{
   // Define function pointer types for EnumProcessModules and GetModuleFileNameEx
   typedef BOOL ( WINAPI *Func_EnumProcessModules ) ( HANDLE, HMODULE *, DWORD, LPDWORD );

   typedef DWORD ( WINAPI *Func_GetModuleFileNameEx ) ( HANDLE, HMODULE, LPTSTR, DWORD );

   // Static variables to hold the function pointers
   static Func_EnumProcessModules   pEnumProcessModules = NULL;
   static Func_GetModuleFileNameEx  pGetModuleFileNameEx = NULL;

#ifdef UNICODE
   LPSTR                            pStr;
#endif

   // Retrieve the process ID from the first parameter or use the current process ID
   DWORD                            ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();
   TCHAR                            cProcessFullName[MAX_PATH] = _TEXT( "" );
   HANDLE                           hProcess;

   // If the function pointer for EnumProcessModules is NULL, retrieve it
   if( pEnumProcessModules == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );
      pEnumProcessModules = ( Func_EnumProcessModules ) wapi_GetProcAddress( hLib, "EnumProcessModules" );
   }

   // If the function pointer is still NULL, return without doing anything
   if( pEnumProcessModules == NULL )
   {
      return;
   }

   // If the function pointer for GetModuleFileNameEx is NULL, retrieve it
   if( pGetModuleFileNameEx == NULL )
   {
      HMODULE  hLib = LoadLibrary( _TEXT( "Psapi.dll" ) );

#ifdef UNICODE
      pGetModuleFileNameEx = ( Func_GetModuleFileNameEx ) wapi_GetProcAddress( hLib, "GetModuleFileNameExW" );
#else
      pGetModuleFileNameEx = ( Func_GetModuleFileNameEx ) wapi_GetProcAddress( hLib, "GetModuleFileNameExA" );
#endif
   }

   // If the function pointer is still NULL, return without doing anything
   if( pGetModuleFileNameEx == NULL )
   {
      return;
   }

   // Open the process with the specified ID
   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID );
   if( hProcess )
   {
      HMODULE  hMod;
      DWORD    cbNeeded;

      // Enumerate the modules in the process and retrieve the full path of the first module
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

// Function: TerminateProcess
// Terminates the specified process and all its threads.
// Parameters:
// [nProcessID] - Optional process ID. If not provided, the current process is used.
// [nExitCode] - Optional exit code. If not provided, defaults to 0.
// Returns: lBoolean - TRUE if the process was terminated successfully, FALSE otherwise.
HB_FUNC( TERMINATEPROCESS )
{
   // Retrieve the process ID from the first parameter or use the current process ID
   DWORD    ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();

   // Retrieve the exit code from the second parameter
   UINT     uExitCode = hmg_par_UINT( 2 );

   // Open the process with terminate access
   HANDLE   hProcess = OpenProcess( PROCESS_TERMINATE, FALSE, ProcessID );

   if( hProcess )
   {
      // Terminate the process with the specified exit code
      TerminateProcess( hProcess, uExitCode );
      CloseHandle( hProcess );
   }

   // Return TRUE if the process was opened successfully, FALSE otherwise
   hb_retl( hProcess != NULL );
}

// Function: RedrawWindowControlRect
// Updates the client area of the specified window.
// Parameters:
// hWnd - Handle to the window.
// top - Top coordinate of the rectangle.
// left - Left coordinate of the rectangle.
// bottom - Bottom coordinate of the rectangle.
// right - Right coordinate of the rectangle.
// Returns: lBoolean - TRUE if the window was successfully redrawn, FALSE otherwise.
HB_FUNC( REDRAWWINDOWCONTROLRECT )
{
   // Define a rectangle structure and set its coordinates
   RECT  r;
   r.top = hb_parni( 2 );
   r.left = hb_parni( 3 );
   r.bottom = hb_parni( 4 );
   r.right = hb_parni( 5 );

   // Redraw the specified window with the defined rectangle
   hb_retl( RedrawWindow( hmg_par_raw_HWND( 1 ), &r, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW ) );
}

// Function: AddSplitBoxItem
// Adds a band to a rebar control.
// Parameters:
// hwnd - Handle to the child window.
// Style - Style of the band.
// lpText - Text to display in the band.
// Returns: LRESULT - Result of the message sent to the rebar control.
HB_FUNC( ADDSPLITBOXITEM )
{
   // Retrieve the handle to the child window and set the initial style
   HWND           hwnd = hmg_par_raw_HWND( 1 );
   UINT           Style = RBBS_CHILDEDGE | RBBS_GRIPPERALWAYS | RBBS_USECHEVRON;

   // Define a REBARBANDINFO structure and a RECT structure
   REBARBANDINFO  rbBand;
   RECT           rc;

#ifndef UNICODE
   // Retrieve the text for the band
   LPSTR    lpText = ( LPSTR ) hb_parc( 5 );
#else
   // Convert the text to wide characters if necessary
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 5 ) );
#endif

   // Add the RBBS_BREAK style if specified
   if( hb_parl( 4 ) )
   {
      Style |= RBBS_BREAK;
   }

   // Retrieve the dimensions of the child window
   GetWindowRect( hwnd, &rc );

   // Set the size and mask for the REBARBANDINFO structure
   rbBand.cbSize = sizeof( REBARBANDINFO );
   rbBand.fMask = RBBIM_TEXT | RBBIM_STYLE | RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_SIZE;
   rbBand.fStyle = Style;
   rbBand.hbmBack = 0;

   // Set the text and child window handle for the band
   rbBand.lpText = lpText;
   rbBand.hwndChild = hwnd;

   // Add the RBBIM_IDEALSIZE mask if specified
   if( hb_parni( 9 ) )
   {
      rbBand.fMask = rbBand.fMask | RBBIM_IDEALSIZE;
   }

   // Set the dimensions for the band based on whether it is horizontal or not
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

   // Send the RB_INSERTBAND message to the rebar control and return the result
   hmg_ret_LRESULT( SendMessage( hmg_par_raw_HWND( 2 ), RB_INSERTBAND, ( WPARAM ) - 1, ( LPARAM ) & rbBand ) );

#ifdef UNICODE
   // Free the wide character string if necessary
   hb_xfree( lpText );
#endif
}

// Function: C_SETWINDOWRGN
// Sets the window region of a window to a specified shape.
// Parameters:
// hWnd - Handle to the window.
// nType - Type of region to create (0 for none, 1 for rectangle, 2 for ellipse, 3 for rounded rectangle, 4 for bitmap).
// Returns: lBoolean - TRUE if the region was successfully set, FALSE otherwise.
HB_FUNC( C_SETWINDOWRGN )
{
   // Initialize the region handle and bitmap handle
   HRGN     hRgn = NULL;
   HBITMAP  hbmp;

   // If the region type is 0, set the window region to NULL and return
   if( hb_parni( 6 ) == 0 )
   {
      hb_retl( SetWindowRgn( GetActiveWindow(), NULL, TRUE ) );
   }
   else
   {
      // Create the region based on the specified type
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
            // Load the bitmap from resources or file and create a region from it
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

      // If the region was created successfully, set it as the window region and register it
      if( hRgn )
      {
         SetWindowRgn( hmg_par_raw_HWND( 1 ), hRgn, TRUE );
         RegisterResource( hRgn, "REGION" );
         hmg_ret_raw_HANDLE( hRgn );
      }
      else
      {
         // If the region was not created successfully, return an error
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to create region", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
   }
}

// Function: C_SETPOLYWINDOWRGN
// Sets the window region of a window to a polygonal shape.
// Parameters:
// aPoints - Array of points defining the polygon.
// nFillMode - Fill mode for the polygon (1 for WINDING, otherwise ALTERNATE).
// Returns: HRGN - Handle to the created region.
HB_FUNC( C_SETPOLYWINDOWRGN )
{
   // Initialize the region handle and an array of points
   HRGN  hRgn;
   POINT lppt[512];
   int   i, fnPolyFillMode;
   int   nPoints = ( int ) hb_parinfa( 2, 0 );

   // Set the fill mode based on the specified parameter
   if( hb_parni( 4 ) == 1 )
   {
      fnPolyFillMode = WINDING;
   }
   else
   {
      fnPolyFillMode = ALTERNATE;
   }

   // Retrieve the points from the array and store them in the POINT array
   for( i = 0; i <= nPoints - 1; i++ )
   {
      lppt[i].x = HB_PARNI( 2, i + 1 );
      lppt[i].y = HB_PARNI( 3, i + 1 );
   }

   // Create the polygonal region
   hRgn = CreatePolygonRgn( lppt, nPoints, fnPolyFillMode );

   // If the region was created successfully, set it as the window region and register it
   if( hRgn )
   {
      SetWindowRgn( GetActiveWindow(), hRgn, TRUE );

      RegisterResource( hRgn, "REGION" );
      hmg_ret_raw_HANDLE( hRgn );
   }
   else
   {
      // If the region was not created successfully, return an error
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to create region", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

// Function: GETHELPDATA
// Retrieves the handle to the item associated with a help event.
// Parameters:
// lParam - Pointer to the help event data.
// Returns: HWND - Handle to the item.
HB_FUNC( GETHELPDATA )
{
   // Retrieve the handle to the item from the help event data
   hmg_ret_raw_HWND( ( ( HELPINFO FAR * ) HB_PARNL( 1 ) )->hItemHandle );
}

// Function: GETMSKTEXTMESSAGE
// Retrieves the message associated with a message filter event.
// Parameters:
// lParam - Pointer to the message filter event data.
// Returns: HWND - The message.
HB_FUNC( GETMSKTEXTMESSAGE )
{
   // Retrieve the message from the message filter event data
   hmg_ret_raw_HWND( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->msg );
}

// Function: GETMSKTEXTWPARAM
// Retrieves the wParam associated with a message filter event.
// Parameters:
// lParam - Pointer to the message filter event data.
// Returns: HWND - The wParam.
HB_FUNC( GETMSKTEXTWPARAM )
{
   // Retrieve the wParam from the message filter event data
   hmg_ret_raw_HWND( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->wParam );
}

// Function: GETMSKTEXTLPARAM
// Retrieves the lParam associated with a message filter event.
// Parameters:
// lParam - Pointer to the message filter event data.
// Returns: HWND - The lParam.
HB_FUNC( GETMSKTEXTLPARAM )
{
   // Retrieve the lParam from the message filter event data
   hmg_ret_raw_HWND( ( ( MSGFILTER FAR * ) HB_PARNL( 1 ) )->lParam );
}

// Function: GETWINDOW
// Retrieves the handle to a window that has the specified relationship to the specified window.
// Parameters:
// hWnd - Handle to the window.
// nCmd - Command specifying the relationship.
// Returns: HWND - Handle to the window.
HB_FUNC( GETWINDOW )
{
   // Retrieve and return the handle to the window with the specified relationship
   hmg_ret_raw_HWND( GetWindow( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

// Function: GETGRIDOLDSTATE
// Retrieves the old state of an item in a list view control.
// Parameters:
// lParam - Pointer to the list view notification data.
// Returns: UINT - The old state of the item.
HB_FUNC( GETGRIDOLDSTATE )
{
   // Retrieve the old state from the list view notification data
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   NM_LISTVIEW *NMLV = ( NM_LISTVIEW * ) lParam;

   hmg_ret_UINT( NMLV->uOldState );
}

// Function: GETGRIDNEWSTATE
// Retrieves the new state of an item in a list view control.
// Parameters:
// lParam - Pointer to the list view notification data.
// Returns: UINT - The new state of the item.
HB_FUNC( GETGRIDNEWSTATE )
{
   // Retrieve the new state from the list view notification data
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   NM_LISTVIEW *NMLV = ( NM_LISTVIEW * ) lParam;

   hmg_ret_UINT( NMLV->uNewState );
}

// Function: GETGRIDDISPINFOINDEX
// Retrieves the item and subitem indices from a list view dispinfo notification.
// Parameters:
// lParam - Pointer to the list view dispinfo notification data.
// Returns: Array - An array containing the item and subitem indices.
HB_FUNC( GETGRIDDISPINFOINDEX )
{
   // Retrieve the item and subitem indices from the list view dispinfo notification data
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   LV_DISPINFO *pDispInfo = ( LV_DISPINFO * ) lParam;

   int         iItem = pDispInfo->item.iItem;
   int         iSubItem = pDispInfo->item.iSubItem;

   // Create and return an array with the item and subitem indices
   hb_reta( 2 );
   HB_STORNI( iItem + 1, -1, 1 );
   HB_STORNI( iSubItem + 1, -1, 2 );
}

// Function: SETGRIDQUERYDATA
// Sets the text for an item in a list view control.
// Parameters:
// lParam - Pointer to the list view dispinfo notification data.
// cText - Text to set for the item.
HB_FUNC( SETGRIDQUERYDATA )
{
   // Retrieve the list view dispinfo notification data and the text to set
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

// Function: SETGRIDQUERYIMAGE
// Sets the image index for an item in a list view control.
// Parameters:
// lParam - Pointer to the list view dispinfo notification data.
// nImage - Image index to set for the item.
HB_FUNC( SETGRIDQUERYIMAGE )
{
   // Retrieve the list view dispinfo notification data and set the image index
   LPARAM      lParam = hmg_par_raw_LPARAM( 1 );
   LV_DISPINFO *pDispInfo = ( LV_DISPINFO * ) lParam;

   pDispInfo->item.iImage = hb_parni( 2 );
}

// Function: FINDWINDOWEX
// Retrieves the handle to a window whose class name and window name match the specified strings.
// Parameters:
// hWndParent - Handle to the parent window.
// hWndChildAfter - Handle to the child window to start the search after.
// lpszClass - Class name of the window to find.
// lpszWindow - Window name of the window to find.
// Returns: HWND - Handle to the window.
HB_FUNC( FINDWINDOWEX )
{
#ifndef UNICODE
   // Retrieve the class and window names
   LPCSTR   lpszClass = ( char * ) hb_parc( 3 );
   LPCSTR   lpszWindow = ( char * ) hb_parc( 4 );
#else
   // Convert the class and window names to wide characters if necessary
   LPWSTR   lpszClass = ( hb_parc( 3 ) != NULL ) ? hb_osStrU16Encode( hb_parc( 3 ) ) : NULL;
   LPWSTR   lpszWindow = ( hb_parc( 4 ) != NULL ) ? hb_osStrU16Encode( hb_parc( 4 ) ) : NULL;
#endif

   // Retrieve and return the handle to the window with the specified class and window names
   hmg_ret_raw_HWND( FindWindowEx( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ), lpszClass, lpszWindow ) );

#ifdef UNICODE
   // Free the wide character strings if necessary
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

// Function: GETDS
// Handles custom drawing for a list view control.
// Parameters:
// lParam - Pointer to the list view custom draw data.
// Returns: INT - Result of the custom draw operation.
HB_FUNC( GETDS )
{
   // Retrieve the list view custom draw data
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   // Handle different draw stages
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

// Function: GETRC
// Retrieves the row and column from a list view custom draw notification.
// Parameters:
// lParam - Pointer to the list view custom draw data.
// Returns: Array - An array containing the row and column.
HB_FUNC( GETRC )           // Get ListView CustomDraw Row and Column
{
   // Retrieve the list view custom draw data
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   // Create and return an array with the row and column
   hb_reta( 2 );
   HB_STORVNL( ( LONG ) lplvcd->nmcd.dwItemSpec + 1, -1, 1 );
   HB_STORNI( ( INT ) lplvcd->iSubItem + 1, -1, 2 );
}

// Function: SETBCFC
// Sets the background and foreground colors for a list view custom draw.
// Parameters:
// lParam - Pointer to the list view custom draw data.
// nBackColor - Background color to set.
// nForeColor - Foreground color to set.
// Returns: INT - Result of the custom draw operation.
HB_FUNC( SETBCFC )         // Set Dynamic BackColor and ForeColor
{
   // Retrieve the list view custom draw data and set the background and foreground colors
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   lplvcd->clrTextBk = hb_parni( 2 );
   lplvcd->clrText = hb_parni( 3 );

   hb_retni( CDRF_NEWFONT );
}

// Function: SETBRCCD
// Sets the default background and foreground colors for a list view custom draw.
// Parameters:
// lParam - Pointer to the list view custom draw data.
// Returns: INT - Result of the custom draw operation.
HB_FUNC( SETBRCCD )        // Set Default BackColor and ForeColor
{
   // Retrieve the list view custom draw data and set the default background and foreground colors
   LPARAM            lParam = hmg_par_raw_LPARAM( 1 );
   LPNMLVCUSTOMDRAW  lplvcd = ( LPNMLVCUSTOMDRAW ) lParam;

   lplvcd->clrText = RGB( 0, 0, 0 );
   lplvcd->clrTextBk = RGB( 255, 255, 255 );

   hb_retni( CDRF_NEWFONT );
}

// Function: GETTABBEDCONTROLBRUSH
// Retrieves a brush for a tabbed control.
// Parameters:
// hDC - Handle to the device context.
// hWnd - Handle to the window.
// hWndTab - Handle to the tab control.
// hBrush - Handle to the brush.
// Returns: HBRUSH - Handle to the brush.
HB_FUNC( GETTABBEDCONTROLBRUSH )
{
   // Define a rectangle structure and retrieve the device context
   RECT  rc;
   HDC   hDC = hmg_par_raw_HDC( 1 );

   // Set the background mode to transparent and retrieve the dimensions of the window
   SetBkMode( hDC, TRANSPARENT );
   GetWindowRect( hmg_par_raw_HWND( 2 ), &rc );
   MapWindowPoints( NULL, hmg_par_raw_HWND( 3 ), ( LPPOINT ) ( &rc ), 2 );
   SetBrushOrgEx( hDC, -rc.left, -rc.top, NULL );

   // Return the handle to the brush
   hmg_ret_raw_HBRUSH( hmg_par_raw_HBRUSH( 4 ) );
}

// Function: GETTABBRUSH
// Retrieves a brush for a tab control.
// Parameters:
// hWnd - Handle to the window.
// Returns: HBRUSH - Handle to the brush.
HB_FUNC( GETTABBRUSH )
{
   // Define variables for the brush, rectangle, device contexts, and bitmaps
   HBRUSH   hBrush;
   RECT     rc;
   HDC      hDC;
   HDC      hDCMem;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;
   HWND     hWnd = hmg_par_raw_HWND( 1 );

   // Retrieve the dimensions of the window and create compatible device contexts and bitmaps
   GetWindowRect( hWnd, &rc );
   hDC = GetDC( hWnd );
   hDCMem = CreateCompatibleDC( hDC );

   hBmp = CreateCompatibleBitmap( hDC, rc.right - rc.left, rc.bottom - rc.top );

   hOldBmp = ( HBITMAP ) SelectObject( hDCMem, hBmp );

   // Send a WM_PRINTCLIENT message to the window to draw its client area
   SendMessage( hWnd, WM_PRINTCLIENT, ( WPARAM ) hDCMem, ( LPARAM ) PRF_ERASEBKGND | PRF_CLIENT | PRF_NONCLIENT );

   // Create a pattern brush from the bitmap and register it as a resource
   hBrush = CreatePatternBrush( hBmp );
   if( hBrush )
   {
      RegisterResource( hBrush, "BRUSH" );
      hmg_ret_raw_HBRUSH( hBrush );
   }

   // Clean up the device contexts and bitmaps
   SelectObject( hDCMem, hOldBmp );

   DeleteObject( hBmp );
   DeleteDC( hDCMem );
   ReleaseDC( hWnd, hDC );
}

// Function: INITMINMAXINFO
// Initializes a MINMAXINFO structure with default values.
// Parameters:
// hWnd - Handle to the window.
// Returns: Array - An array containing the default values for the MINMAXINFO structure.
HB_FUNC( INITMINMAXINFO )  // ( hWnd ) --> aMinMaxInfo
{
   // Define variables for the dimensions
   LONG  x, y, mx, my;

   // Retrieve the window style and set the dimensions based on whether the window is sizable
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

   // Calculate the maximum dimensions based on the screen size
   mx = GetSystemMetrics( SM_CXSCREEN ) - 2 * x;
   my = GetSystemMetrics( SM_CYSCREEN ) - 2 * y;

   // Create and return an array with the default values for the MINMAXINFO structure
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

// Function: SETMINMAXINFO
// Sets the values of a MINMAXINFO structure.
// Parameters:
// pMinMaxInfo - Pointer to the MINMAXINFO structure.
// aMinMaxInfo - Array containing the values for the MINMAXINFO structure.
HB_FUNC( SETMINMAXINFO )   // ( pMinMaxInfo, aMinMaxInfo ) --> 0
{
   // Retrieve the pointer to the MINMAXINFO structure and set its values from the array
   MINMAXINFO  *pMinMaxInfo = ( MINMAXINFO * ) HB_PARNL( 1 );

   pMinMaxInfo->ptMaxSize.x = HB_PARNI( 2, 1 );
   pMinMaxInfo->ptMaxSize.y = HB_PARNI( 2, 2 );
   pMinMaxInfo->ptMaxPosition.x = HB_PARNI( 2, 3 );
   pMinMaxInfo->ptMaxPosition.y = HB_PARNI( 2, 4 );
   pMinMaxInfo->ptMinTrackSize.x = HB_PARNI( 2, 5 );
   pMinMaxInfo->ptMinTrackSize.y = HB_PARNI( 2, 6 );
   pMinMaxInfo->ptMaxTrackSize.x = HB_PARNI( 2, 7 );
   pMinMaxInfo->ptMaxTrackSize.y = HB_PARNI( 2, 8 );

   // Return 0 to indicate success
   hb_retni( 0 );
}

// Function: LOCKWINDOWUPDATE
// Locks updates for the specified window.
// Parameters:
// hWnd - Handle to the window.
// Returns: lBoolean - TRUE if the window was successfully locked, FALSE otherwise.
HB_FUNC( LOCKWINDOWUPDATE )
{
   // Lock updates for the specified window and return the result
   hmg_ret_L( LockWindowUpdate( hmg_par_raw_HWND( 1 ) ) );
}

// Function: SETSTANDBY
// Sets the execution state for the current thread.
// Parameters:
// lStandby - Logical value indicating whether to set standby mode.
// Returns: DWORD - The previous execution state.
HB_FUNC( SETSTANDBY )
{
   // Define the execution state flags based on the logical parameter
   EXECUTION_STATE   esFlags = HB_ISLOG( 1 ) && hb_parl( 1 ) ? ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_DISPLAY_REQUIRED | ES_AWAYMODE_REQUIRED : ES_CONTINUOUS;

   // Set the execution state and return the previous state
   hmg_ret_DWORD( SetThreadExecutionState( esFlags ) );
}

// Function: ISWINDOWHANDLE
// Determines whether the specified handle is a valid window handle.
// Parameters:
// hWnd - Handle to the window.
// Returns: lBoolean - TRUE if the handle is valid, FALSE otherwise.
HB_FUNC( ISWINDOWHANDLE )
{
   // Check if the handle is a valid window handle and return the result
   hmg_ret_L( IsWindow( hmg_par_raw_HWND( 1 ) ) );
}

// Function: ISICONIC
// Determines whether the specified window is minimized.
// Parameters:
// hWnd - Handle to the window.
// Returns: lBoolean - TRUE if the window is minimized, FALSE otherwise.
HB_FUNC( ISICONIC )
{
   // Retrieve the window handle and check if it is minimized
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsIconic( hwnd ) : FALSE );
}

// Function: ISZOOMED
// Determines whether the specified window is maximized.
// Parameters:
// hWnd - Handle to the window.
// Returns: lBoolean - TRUE if the window is maximized, FALSE otherwise.
HB_FUNC( ISZOOMED )
{
   // Retrieve the window handle and check if it is maximized
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   hb_retl( IsWindow( hwnd ) ? IsZoomed( hwnd ) : FALSE );
}

// Function: SETWINDOWHICON
// Sets the icon for the specified window.
// Parameters:
// hWnd - Handle to the window.
// hIcon - Handle to the icon.
// Returns: LONG_PTR - The previous icon handle.
HB_FUNC( SETWINDOWHICON )
{
   // Set the icon for the specified window and return the previous icon handle
   hmg_ret_LONG_PTR( SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HICON, hmg_par_raw_LONG_PTR( 2 ) ) );
}

// Function: GETWINDOWBRUSH
// Retrieves the background brush for the specified window.
// Parameters:
// hWnd - Handle to the window.
// Returns: LONG_PTR - The handle to the background brush.
HB_FUNC( GETWINDOWBRUSH )
{
   // Retrieve and return the handle to the background brush for the specified window
   hmg_ret_LONG_PTR( GetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HBRBACKGROUND ) );
}

// Function: SETWINDOWBRUSH
// Sets the background brush for the specified window.
// Parameters:
// hWnd - Handle to the window.
// hBrush - Handle to the brush.
// Returns: LONG_PTR - The previous brush handle.
HB_FUNC( SETWINDOWBRUSH )
{
   // Set the background brush for the specified window and return the previous brush handle
   hmg_ret_LONG_PTR( SetClassLongPtr( hmg_par_raw_HWND( 1 ), GCLP_HBRBACKGROUND, hmg_par_raw_LONG_PTR( 2 ) ) );
}

// Function: CREATEHATCHBRUSH
// Creates a hatch brush with the specified hatch style and color.
// Parameters:
// nStyle - Hatch style.
// crColor - Color for the hatch brush.
// Returns: HBRUSH - Handle to the hatch brush.
HB_FUNC( CREATEHATCHBRUSH )
{
   // Create the hatch brush with the specified style and color
   HBRUSH   hBrush = CreateHatchBrush( hb_parni( 1 ), hmg_par_COLORREF( 2 ) );

   // Register the brush as a resource and return its handle
   RegisterResource( hBrush, "BRUSH" );
   hmg_ret_raw_HBRUSH( hBrush );
}

/* Modified by P.Ch. 16.10. */

// Function: CREATEPATTERNBRUSH
// Creates a pattern brush with the specified bitmap.
// Parameters:
// cImage - Name or resource identifier of the bitmap.
// Returns: HBRUSH - Handle to the pattern brush.
HB_FUNC( CREATEPATTERNBRUSH )
{
   // Define variables for the brush and bitmap
   HBRUSH   hBrush;
   HBITMAP  hImage;

#ifndef UNICODE
   // Retrieve the image name or resource identifier
   LPCTSTR  lpImageName = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : ( HB_ISNUM( 1 ) ? MAKEINTRESOURCE( hb_parni( 1 ) ) : NULL );
#else
   // Convert the image name to wide characters if necessary
   LPCWSTR  lpImageName = HB_ISCHAR( 1 ) ? AnsiToWide( ( char * ) hb_parc( 1 ) ) : ( HB_ISNUM( 1 ) ? ( LPCWSTR ) MAKEINTRESOURCE( hb_parni( 1 ) ) : NULL );
#endif

   // Load the bitmap from resources or file
   hImage = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );

   if( hImage == NULL && HB_ISCHAR( 1 ) )
   {
      hImage = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   }

   if( hImage == NULL )
   {
      hImage = ( HBITMAP ) HMG_LoadImage( hb_parc( 1 ) );
   }

   // If the bitmap was loaded successfully, create a pattern brush and register it
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
   // Free the wide character string if necessary
   if( HB_ISCHAR( 1 ) )
   {
      hb_xfree( ( TCHAR * ) lpImageName );
   }
#endif
}

/*
   BitmapToRegion: Create a region from the "non-transparent" pixels of a bitmap
   Author: Jean-Edouard Lachand-Robert
   (http://www.geocities.com/Paris/LeftBank/1160/resume.htm), June 1998.

   hBmp: Source bitmap
   cTransparentColor: Color base for the "transparent" pixels
   (default is black)
   cTolerance: Color tolerance for the "transparent" pixels.

   A pixel is assumed to be transparent if the value of each of its 3
   components (blue, green and red) is
   greater or equal to the corresponding value in cTransparentColor and is
   lower or equal to the corresponding value in cTransparentColor + cTolerance.
*/
#define ALLOC_UNIT   100

// Function: BitmapToRegion
// Creates a region from the non-transparent pixels of a bitmap.
// Parameters:
// hBmp - Handle to the bitmap.
// cTransparentColor - Color to treat as transparent.
// cTolerance - Color tolerance for transparency.
// Returns: HRGN - Handle to the created region.
HRGN BitmapToRegion( HBITMAP hBmp, COLORREF cTransparentColor, COLORREF cTolerance )
{
   // Initialize the region handle
   HRGN  hRgn = NULL;
   VOID  *pbits32;
   DWORD maxRects = ALLOC_UNIT;

   if( hBmp )
   {
      // Create a memory DC inside which we will scan the bitmap content
      HDC   hMemDC = CreateCompatibleDC( NULL );
      if( hMemDC )
      {
         // Define variables for the bitmap and bitmap info header
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

         // Create a DIB section and select it into the memory DC
         hbm32 = CreateDIBSection( hMemDC, ( BITMAPINFO * ) &RGB32BITSBITMAPINFO, DIB_RGB_COLORS, &pbits32, NULL, 0 );
         if( hbm32 )
         {
            HBITMAP  holdBmp = ( HBITMAP ) SelectObject( hMemDC, hbm32 );

            // Create a DC just to copy the bitmap into the memory DC
            HDC      hDC = CreateCompatibleDC( hMemDC );
            if( hDC )
            {
               // Define variables for the bitmap, data handle, and region data
               BITMAP   bm32;
               HANDLE   hData;
               RGNDATA  *pData;
               BYTE     *p32;
               BYTE     lr, lg, lb, hr, hg, hb;
               INT      y, x;
               HRGN     h;

               // Get the bitmap info and ensure the width is a multiple of 4
               GetObject( hbm32, sizeof( bm32 ), &bm32 );
               while( bm32.bmWidthBytes % 4 )
               {
                  bm32.bmWidthBytes++;
               }

               // Copy the bitmap into the memory DC
               holdBmp = ( HBITMAP ) SelectObject( hDC, hBmp );
               BitBlt( hMemDC, 0, 0, bm.bmWidth, bm.bmHeight, hDC, 0, 0, SRCCOPY );

               // Allocate memory for the region data
               hData = GlobalAlloc( GMEM_MOVEABLE, sizeof( RGNDATAHEADER ) + ( sizeof( RECT ) * maxRects ) );

               // Lock the memory and initialize the region data
               pData = ( RGNDATA * ) GlobalLock( hData );
               pData->rdh.dwSize = sizeof( RGNDATAHEADER );
               pData->rdh.iType = RDH_RECTANGLES;
               pData->rdh.nCount = pData->rdh.nRgnSize = 0;
               SetRect( &pData->rdh.rcBound, MAXLONG, MAXLONG, 0, 0 );

               // Set the color tolerance for transparency
               lr = GetRValue( cTransparentColor );
               lg = GetGValue( cTransparentColor );
               lb = GetBValue( cTransparentColor );
               hr = ( BYTE ) HB_MIN( 0xff, lr + GetRValue( cTolerance ) );
               hg = ( BYTE ) HB_MIN( 0xff, lg + GetGValue( cTolerance ) );
               hb = ( BYTE ) HB_MIN( 0xff, lb + GetBValue( cTolerance ) );

               // Scan each bitmap row from bottom to top
               p32 = ( BYTE * ) bm32.bmBits + ( bm32.bmHeight - 1 ) * bm32.bmWidthBytes;
               for( y = 0; y < bm.bmHeight; y++ )
               {
                  // Scan each bitmap pixel from left to right
                  for( x = 0; x < bm.bmWidth; x++ )
                  {
                     int   x0 = x;
                     LONG  *p = ( LONG * ) p32 + x;

                     // Search for a continuous range of "non transparent pixels"
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
                                 break;   // This pixel is "transparent"
                              }
                           }
                        }

                        p++;
                        x++;
                     }

                     // Add the pixels (x0, y) to (x, y+1) as a new rectangle in the region
                     if( x > x0 )
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

                        // On Windows98, ExtCreateRegion() may fail if the number of rectangles is too large
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

               // Create or extend the region with the remaining rectangles
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
