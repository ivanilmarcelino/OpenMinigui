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
#define _WIN32_IE 0x0501

#ifdef __XCC__
#define _WIN32_WINDOWS  0x0410
#endif
#include <mgdefs.h>
#include <commctrl.h>

/* 
 * Compatibility layer for legacy Borland C++ compilers.
 */
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_SCROLLBAR "ScrollBar"
#define WC_STATIC    "Static"
#endif

/* Internal function prototypes for window procedure subclassing and instance retrieval */
LRESULT APIENTRY  SubClassFunc( HWND, UINT, WPARAM, LPARAM );
HINSTANCE         GetInstance( void );

/* 
 * Global pointer to the original window procedure of the ListView.
 * Used to chain messages back to the default handler after custom processing.
 */
static WNDPROC    s_lpOldWndProc = NULL;

/*
 * Function: INITBROWSE
 * Purpose: Initializes and creates a Win32 ListView control configured as an HMG Browse.
 * Parameters:
 *    1: HWND  - Parent window handle.
 *    2: HMENU - Control ID (passed as HMENU in Win32 API for child windows).
 *    3: INT   - X coordinate (horizontal position).
 *    4: INT   - Y coordinate (vertical position).
 *    5: INT   - Width of the control.
 *    6: INT   - Height of the control.
 *    7: LOGIC - TabStop flag (True to disable tab stop, False to enable).
 * Returns: HWND - Handle to the newly created ListView control.
 */
HB_FUNC( INITBROWSE )
{
   HWND                 hListView;
   HWND                 hParent;
   HMENU                hMenu;
   DWORD                dwStyle;
   INITCOMMONCONTROLSEX icex;
   int                  nX, nY, nW, nH;
   BOOL                 lTabStop;

   // Extract parameters from the Harbour virtual machine stack
   hParent = hmg_par_raw_HWND( 1 );
   hMenu = hmg_par_raw_HMENU( 2 );
   nX = hb_parni( 3 );
   nY = hb_parni( 4 );
   nW = hb_parni( 5 );
   nH = hb_parni( 6 );
   lTabStop = !hb_parl( 7 ); // Logic is inverted based on HMG's internal parameter passing

   // Ensure the ListView common control class is initialized within the process
   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC = ICC_LISTVIEW_CLASSES;
   InitCommonControlsEx( &icex );

   // Define base styles: 
   // LVS_REPORT: Required for grid-like multi-column display.
   // LVS_SINGLESEL: Restricts selection to one row at a time.
   // LVS_SHOWSELALWAYS: Selection remains visible even when control loses focus.
   dwStyle = WS_CHILD | WS_VISIBLE | LVS_REPORT | LVS_SINGLESEL | LVS_SHOWSELALWAYS;

   if( lTabStop )
   {
      dwStyle |= WS_TABSTOP;
   }

   // Create the ListView control using the extended client edge style for a 3D border effect
   hListView = CreateWindowEx( WS_EX_CLIENTEDGE, WC_LISTVIEW, TEXT( "" ), dwStyle, nX, nY, nW, nH, hParent, hMenu, GetInstance(), NULL );

   // Subclass the control to intercept specific messages (like Mouse Wheel)
   if( hListView != NULL )
   {
      s_lpOldWndProc = SubclassWindow1( hListView, SubClassFunc );
   }

   // Return the handle to the Harbour application
   hmg_ret_raw_HWND( hListView );
}

/*
 * Function: SubClassFunc
 * Purpose: Custom Window Procedure for the Browse control.
 * Logic: Intercepts WM_MOUSEWHEEL to provide consistent row-by-row navigation.
 * Why: Standard ListView wheel scrolling can be erratic in certain report modes; 
 *      translating it to keyboard events ensures the selection moves predictably.
 */
LRESULT APIENTRY SubClassFunc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
   if( uMsg == WM_MOUSEWHEEL )
   {
      // Determine rotation direction: Positive is away from user (Up), Negative is toward user (Down)
      if( GET_WHEEL_DELTA_WPARAM( wParam ) > 0 )
      {
         // Simulate a physical Up Arrow key press to move selection up
         keybd_event( VK_UP, 0, 0, 0 );
      }
      else
      {
         // Simulate a physical Down Arrow key press to move selection down
         keybd_event( VK_DOWN, 0, 0, 0 );
      }
   }

   // Pass all other messages (and the wheel message) to the original handler
   return CallWindowProc( s_lpOldWndProc, hWnd, uMsg, wParam, lParam );
}

/*
 * Function: INITVSCROLLBAR
 * Purpose: Creates a standalone vertical scrollbar control.
 * Parameters:
 *    1: HWND - Parent window handle.
 *    2: INT  - X position.
 *    3: INT  - Y position.
 *    4: INT  - Width.
 *    5: INT  - Height.
 * Returns: HWND - Handle to the scrollbar.
 * Side Effects: Sets an initial default range of 1 to 100.
 */
HB_FUNC( INITVSCROLLBAR )
{
   HWND  hScrollbar;
   HWND  hParent;
   int   nX, nY, nW, nH;

   hParent = hmg_par_raw_HWND( 1 );
   nX = hb_parni( 2 );
   nY = hb_parni( 3 );
   nW = hb_parni( 4 );
   nH = hb_parni( 5 );

   // Create the scrollbar with SBS_VERT style for vertical orientation
   hScrollbar = CreateWindowEx( 0, WC_SCROLLBAR, TEXT( "" ), WS_CHILD | WS_VISIBLE | SBS_VERT, nX, nY, nW, nH, hParent, ( HMENU ) NULL, GetInstance(), NULL );

   // Initialize with a standard range to prevent division-by-zero or UI glitches before first update
   if( hScrollbar != NULL )
   {
      SetScrollRange( hScrollbar, SB_CTL, 1, 100, TRUE );
   }

   hmg_ret_raw_HWND( hScrollbar );
}

/*
 * Function: GETSCROLLRANGEMAX
 * Purpose: Retrieves the maximum scrolling position of a specified scrollbar.
 * Parameters:
 *    1: HWND - Handle to the control or window containing the scrollbar.
 *    2: INT  - Scrollbar type (SB_CTL for control, SB_HORZ/SB_VERT for window bars).
 * Returns: INT - The maximum value of the scroll range.
 */
HB_FUNC( GETSCROLLRANGEMAX )
{
   HWND  hWnd;
   int   nType;
   int   nMin, nMax;

   hWnd = hmg_par_raw_HWND( 1 );
   nType = hb_parni( 2 );

   nMin = 0;
   nMax = 0;

   if( hWnd != NULL )
   {
      // Win32 API fills nMin and nMax with the current range limits
      GetScrollRange( hWnd, nType, &nMin, &nMax );
   }

   hmg_ret_NINT( nMax );
}

/*
 * Function: INITVSCROLLBARBUTTON
 * Purpose: Creates a static placeholder control, typically used as a visual 
 *          anchor or "filler" button in custom scrollbar implementations.
 * Parameters:
 *    1: HWND - Parent window handle.
 *    2: INT  - X, Y, W, H coordinates.
 * Returns: HWND - Handle to the static control.
 * Why: Used to fill the small square area where horizontal and vertical 
 *      scrollbars meet, or to act as a custom scroll button container.
 */
HB_FUNC( INITVSCROLLBARBUTTON )
{
   HWND  hWnd;
   HWND  hParent;
   int   nX, nY, nW, nH;

   hParent = hmg_par_raw_HWND( 1 );
   nX = hb_parni( 2 );
   nY = hb_parni( 3 );
   nW = hb_parni( 4 );
   nH = hb_parni( 5 );

   // SS_SUNKEN provides a recessed 3D look consistent with classic Windows scrollbars
   hWnd = CreateWindow( WC_STATIC, TEXT( "" ), WS_CHILD | WS_VISIBLE | SS_SUNKEN, nX, nY, nW, nH, hParent, ( HMENU ) NULL, GetInstance(), NULL );

   hmg_ret_raw_HWND( hWnd );
}

/*
 * Function: SETSCROLLINFO
 * Purpose: Configures scrollbar parameters using the modern 32-bit SCROLLINFO structure.
 * Parameters:
 *    1: HWND - Handle to the scrollbar control.
 *    2: INT  - Maximum range value.
 *    3: INT  - Current thumb position.
 *    4: INT  - Page size (determines the proportional size of the scroll thumb).
 * Returns: LOGIC - Success or failure of the operation.
 */
HB_FUNC( SETSCROLLINFO )
{
   HWND        hWnd;
   SCROLLINFO  si;
   int         nMax, nPos, nPage;

   hWnd = hmg_par_raw_HWND( 1 );
   nMax = hb_parni( 2 );
   nPos = hb_parni( 3 );
   nPage = hb_parni( 4 );

   // Clear structure to avoid garbage data in optional fields
   ZeroMemory( &si, sizeof( si ) );

   si.cbSize = sizeof( SCROLLINFO );
   // SIF_PAGE: Set thumb size, SIF_POS: Set position, SIF_RANGE: Set min/max
   si.fMask = SIF_PAGE | SIF_POS | SIF_RANGE;
   si.nMin = 1;
   si.nMax = nMax;
   si.nPage = nPage;
   si.nPos = nPos;

   // Apply settings to the control (SB_CTL indicates a standalone scrollbar control)
   hb_retl( SetScrollInfo( hWnd, SB_CTL, &si, TRUE ) );
}