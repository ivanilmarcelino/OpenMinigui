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
#define _WIN32_IE 0x0501

#ifdef __XCC__
#define _WIN32_WINDOWS  0x0410
#endif
#include <mgdefs.h>
#include <commctrl.h>

// Compatibility definitions for older Borland C++ compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_SCROLLBAR "ScrollBar"
#define WC_STATIC    "Static"
#endif

// Function prototype for the window procedure used in subclassing
LRESULT APIENTRY  SubClassFunc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );
static WNDPROC    lpfnOldWndProc;

// Function prototype to retrieve the instance handle of the application
HINSTANCE         GetInstance( void );

/*
 * FUNCTION: INITBROWSE
 *
 * Initializes a ListView control with custom styles.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: HMENU - Menu handle (used as control ID).
 *   3: INT - X coordinate of the control.
 *   4: INT - Y coordinate of the control.
 *   5: INT - Width of the control.
 *   6: INT - Height of the control.
 *   7: LOGICAL - WS_TABSTOP style (TRUE or FALSE).
 *
 * Returns:
 *   HWND - Handle of the created ListView control.
 *
 * Purpose:
 *   Creates a ListView control with specified styles and dimensions,
 *   and subclasses it to intercept messages like WM_MOUSEWHEEL for scrolling.
 *   The WS_TABSTOP style is added based on the value of hb_parl(7).
 */
HB_FUNC( INITBROWSE )
{
   HWND                 hListView;
   DWORD                style = LVS_SINGLESEL | LVS_SHOWSELALWAYS | WS_CHILD | WS_VISIBLE | LVS_REPORT;

   // Initialize common controls for ListView
   INITCOMMONCONTROLSEX icex;
   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC  = ICC_LISTVIEW_CLASSES;
   InitCommonControlsEx( &icex );

   // Add WS_TABSTOP if hb_parl(7) is false
   if( !hb_parl( 7 ) )
   {
      style |= WS_TABSTOP;
   }

   // Create the ListView control
   hListView = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,                // Extended style with client edge
         WC_LISTVIEW,                     // Class name for ListView
         TEXT( "" ),                      // No text needed
         style,                           // Combined style flags
         hb_parni( 3 ),                   // X position
         hb_parni( 4 ),                   // Y position
         hb_parni( 5 ),                   // Width
         hb_parni( 6 ),                   // Height
         hmg_par_raw_HWND( 1 ),           // Parent window handle
         hmg_par_raw_HMENU( 2 ),          // Menu handle
         GetInstance(),                   // Module instance handle
         NULL                             // Additional parameters
      );

   // Subclass to intercept messages like WM_MOUSEWHEEL
   lpfnOldWndProc = SubclassWindow1( hListView, SubClassFunc );
   hmg_ret_raw_HWND( hListView );
}

/*
 * FUNCTION: SubClassFunc
 *
 * Message handling function to process WM_MOUSEWHEEL events.
 *
 * Parameters:
 *   hWnd: HWND - Handle of the window receiving the message.
 *   msg: UINT - The message identifier.
 *   wParam: WPARAM - Additional message-specific information.
 *   lParam: LPARAM - Additional message-specific information.
 *
 * Returns:
 *   LRESULT - Result of the message processing.
 *
 * Purpose:
 *   Intercepts WM_MOUSEWHEEL messages to simulate up/down arrow key presses
 *   for scrolling the ListView control when the mouse wheel is used.
 *   Other messages are passed to the original window procedure.
 */
LRESULT APIENTRY SubClassFunc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   if( msg == WM_MOUSEWHEEL )
   {
      // Scroll up or down based on the wheel delta
      if( GET_WHEEL_DELTA_WPARAM( wParam ) > 0 )
      {
         keybd_event( VK_UP, 0, 0, 0 );   // Simulate UP arrow key press
      }
      else
      {
         keybd_event( VK_DOWN, 0, 0, 0 ); // Simulate DOWN arrow key press
      }

      return CallWindowProc( lpfnOldWndProc, hWnd, 0, 0, 0 );
   }

   return CallWindowProc( lpfnOldWndProc, hWnd, msg, wParam, lParam );
}

/*
 * FUNCTION: INITVSCROLLBAR
 *
 * Initializes a vertical scrollbar control.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: INT - X coordinate of the scrollbar.
 *   3: INT - Y coordinate of the scrollbar.
 *   4: INT - Width of the scrollbar.
 *   5: INT - Height of the scrollbar.
 *
 * Returns:
 *   HWND - Handle of the created scrollbar control.
 *
 * Purpose:
 *   Creates a vertical scrollbar control with specified dimensions and sets its scroll range.
 */
HB_FUNC( INITVSCROLLBAR )
{
   HWND  hScrollbar;

   hScrollbar = CreateWindowEx
      (
         0,             // No extended styles
         WC_SCROLLBAR,  // Scrollbar control class
         TEXT( "" ),    // No window text
         WS_CHILD | WS_VISIBLE | SBS_VERT,   // Style for visible vertical scrollbar
         hb_parni( 2 ),                         // X position
         hb_parni( 3 ),                         // Y position
         hb_parni( 4 ),                         // Width
         hb_parni( 5 ),                         // Height
         hmg_par_raw_HWND( 1 ),                 // Parent window handle
         ( HMENU ) NULL,                        // No menu handle
         GetInstance(),                         // Module instance handle
         NULL                                   // Additional parameters
      );

   // Set scroll range from 1 to 100, and allow immediate redraw
   SetScrollRange( hScrollbar, SB_CTL, 1, 100, TRUE );
   hmg_ret_raw_HWND( hScrollbar );
}

/*
 * FUNCTION: GETSCROLLRANGEMAX
 *
 * Retrieves the maximum scroll range for a scrollbar.
 *
 * Parameters:
 *   1: HWND - Handle of the scrollbar control.
 *   2: INT - Scrollbar type (e.g., SB_CTL for a control scrollbar).
 *
 * Returns:
 *   INT - The maximum scroll range value.
 *
 * Purpose:
 *   Gets the maximum scroll position of a scrollbar control.
 */
HB_FUNC( GETSCROLLRANGEMAX )
{
   int   minPos, maxPos;

   // Retrieve the range limits for the specified scrollbar
   GetScrollRange( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), &minPos, &maxPos );
   hmg_ret_NINT( maxPos );
}

/*
 * FUNCTION: INITVSCROLLBARBUTTON
 *
 * Creates a static control styled as a button, typically to represent a scroll button.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: INT - X coordinate of the button.
 *   3: INT - Y coordinate of the button.
 *   4: INT - Width of the button.
 *   5: INT - Height of the button.
 *
 * Returns:
 *   HWND - Handle of the created static control (button).
 *
 * Purpose:
 *   Creates a static control that visually resembles a button, often used as part of a custom scrollbar implementation.
 */
HB_FUNC( INITVSCROLLBARBUTTON )
{
   hmg_ret_raw_HWND
   (
      CreateWindow
         (
            WC_STATIC,                          // Static control class
            TEXT( "" ),                         // No text
            WS_CHILD | WS_VISIBLE | SS_SUNKEN,  // Styles to display sunken look
            hb_parni( 2 ),          // X position
            hb_parni( 3 ),          // Y position
            hb_parni( 4 ),          // Width
            hb_parni( 5 ),          // Height
            hmg_par_raw_HWND( 1 ),  // Parent window handle
            ( HMENU ) NULL,         // No menu handle
            GetInstance(),          // Module instance handle
            NULL                    // Additional parameters
         )
   );
}

/*
 * FUNCTION: SETSCROLLINFO
 *
 * Sets the scroll info for a scrollbar, including page size, position, and range.
 *
 * Parameters:
 *   1: HWND - Handle of the scrollbar control.
 *   2: INT - Maximum scroll range.
 *   3: INT - Current scroll position.
 *   4: INT - Page size (visible area).
 *
 * Returns:
 *   INT - Nonzero if successful; otherwise, zero.
 *
 * Purpose:
 *   Configures the scrollbar's properties, such as the total range, current position, and the size of the visible portion.
 *   This is essential for properly displaying and controlling the scrollbar's behavior.
 */
HB_FUNC( SETSCROLLINFO )
{
   SCROLLINFO  si;
   ZeroMemory( &si, sizeof( si ) );

   // Configure scroll information
   si.cbSize = sizeof( SCROLLINFO );
   si.fMask = SIF_PAGE | SIF_POS | SIF_RANGE;
   si.nMin = 1;
   si.nMax = hb_parni( 2 );         // Maximum scroll range
   si.nPage = hb_parni( 4 );        // Page size
   si.nPos = hb_parni( 3 );         // Scroll position

   // Apply scroll info and return success/failure
   hb_retl( SetScrollInfo( hmg_par_raw_HWND( 1 ), SB_CTL, &si, TRUE ) );
}
