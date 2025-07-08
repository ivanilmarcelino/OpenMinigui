/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

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

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

// Include headers for MiniGUI definitions and Harbour API error and item handling.
#include <mgdefs.h>
#include "hbapierr.h"
#include "hbapiitm.h"

// Function declarations and macros for working with monitors and points.
extern HB_EXPORT BOOL   Array2Point( PHB_ITEM aPoint, POINT *pt );
#ifndef __XHARBOUR__
HB_EXPORT PHB_ITEM      Rect2Hash( RECT *rc );              // Converts a RECT struct to a Harbour hash.
BOOL CALLBACK           _MonitorEnumProc0( HMONITOR hMonitor, HDC hdcMonitor, LPRECT lprcMonitor, LPARAM dwData );
#endif
static void             ClipOrCenterRectToMonitor( LPRECT prc, HMONITOR hMonitor, UINT flags );

// Return the count of monitors detected on the system.
HB_FUNC( COUNTMONITORS )
{
   hb_retni( GetSystemMetrics( SM_CMONITORS ) );
}

// Check if all displays have the same color format (boolean result).
HB_FUNC( ISSAMEDISPLAYFORMAT )
{
   hmg_ret_L( GetSystemMetrics( SM_SAMEDISPLAYFORMAT ) );
}

#ifndef __XHARBOUR__

/*
   Enumerates all display monitors. Each monitor's handle and rectangle are added to a Harbour array.
   The EnumDisplayMonitors function is called with a callback (_MonitorEnumProc0).
*/
HB_FUNC( ENUMDISPLAYMONITORS )
{
   PHB_ITEM pMonitorEnum = hb_itemArrayNew( 0 );            // Initialize an empty array.
   EnumDisplayMonitors( NULL, NULL, _MonitorEnumProc0, ( LPARAM ) pMonitorEnum );

   hb_itemReturnRelease( pMonitorEnum );                    // Return the array to Harbour.
}

// Callback function for ENUMDISPLAYMONITORS, called once per monitor.
BOOL CALLBACK _MonitorEnumProc0( HMONITOR hMonitor, HDC hdcMonitor, LPRECT lprcMonitor, LPARAM dwData )
{
   PHB_ITEM pMonitor = hb_itemArrayNew( 2 );                // Create array for monitor data.
   PHB_ITEM pRect = Rect2Hash( lprcMonitor );               // Convert RECT to a hash.
   HB_SYMBOL_UNUSED( hdcMonitor );
   hb_arraySetNInt( pMonitor, 1, ( LONG_PTR ) hMonitor );   // Set monitor handle.
   hb_itemArrayPut( pMonitor, 2, pRect );                   // Set monitor rectangle.
   hb_arrayAddForward( ( PHB_ITEM ) dwData, pMonitor );     // Add monitor data to main array.

   hb_itemRelease( pMonitor );                              // Release temporary items.
   hb_itemRelease( pRect );

   return TRUE;
}

// Retrieve monitor information (like rectangle bounds) as a Harbour array.
HB_FUNC( GETMONITORINFO )
{
   MONITORINFO mi;

   mi.cbSize = sizeof( MONITORINFO );

   if( GetMonitorInfo( hmg_par_raw_HMONITOR( 1 ), &mi ) )
   {
      PHB_ITEM pMonInfo = hb_itemArrayNew( 3 );                // Create array for monitor info.
      PHB_ITEM pMonitor = Rect2Hash( &mi.rcMonitor );          // Convert monitor rectangle to hash.
      PHB_ITEM pWork = Rect2Hash( &mi.rcWork );                // Convert work area rectangle to hash.
      hb_itemArrayPut( pMonInfo, 1, pMonitor );                // Set monitor rectangle.
      hb_itemArrayPut( pMonInfo, 2, pWork );                   // Set work area rectangle.
      hb_arraySetNInt( pMonInfo, 3, ( LONG_PTR ) mi.dwFlags ); // Set monitor flags.
      hb_itemReturnRelease( pMonInfo );                        // Return monitor info array.
      hb_itemRelease( pMonitor );
      hb_itemRelease( pWork );
   }
   else
   {
      hb_ret();                     // Return null if monitor info retrieval failed.
   }
}
#endif

// Returns the monitor associated with a specified point.
HB_FUNC( MONITORFROMPOINT )
{
   POINT pt;

   // Check if the input is an array of points.
   if( HB_ISARRAY( 1 ) )
   {
      if( !Array2Point( hb_param( 1, HB_IT_ARRAY ), &pt ) )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 5000, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      else
      {
         hmg_ret_raw_HANDLE( MonitorFromPoint( pt, hb_parnldef( 2, MONITOR_DEFAULTTONULL ) ) );
      }
   }
   else if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      // Input is specified as separate x and y coordinates.
      pt.x = hb_parnl( 1 );
      pt.y = hb_parnl( 2 );

      hmg_ret_raw_HANDLE( MonitorFromPoint( pt, hb_parnldef( 3, MONITOR_DEFAULTTONULL ) ) );
   }
   else
   {
      // Error for invalid input parameters.
      hb_errRT_BASE_SubstR( EG_ARG, 5000, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

// Returns the monitor associated with a specified window.
HB_FUNC( MONITORFROMWINDOW )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      hmg_ret_raw_HANDLE( MonitorFromWindow( hwnd, hb_parnldef( 2, MONITOR_DEFAULTTONULL ) ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/*
   Adjusts a window to be centered or clipped to its monitor bounds, using the multi-monitor API.
*/
#define MONITOR_CENTER     0x0001   // Center the rect on monitor
#define MONITOR_CLIP       0x0000   // Clip the rect to monitor
#define MONITOR_WORKAREA   0x0002   // Use the monitor's work area
#define MONITOR_AREA       0x0000   // Use the monitor's entire area

HB_FUNC( WINDOWTOMONITOR )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      HMONITOR hMonitor = HB_ISNUM( 2 ) ? hmg_par_raw_HMONITOR( 2 ) : NULL;
      UINT     flags = 0 | ( hmg_par_UINT_def( 3, ( MONITOR_CENTER | MONITOR_WORKAREA ) ) );
      RECT     rc;

      GetWindowRect( hwnd, &rc );
      ClipOrCenterRectToMonitor( &rc, hMonitor, flags );

      SetWindowPos( hwnd, NULL, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 5001, "MiniGUI Error", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

// Helper function to clip or center a rectangle within a monitor's bounds.
static void ClipOrCenterRectToMonitor( LPRECT prc, HMONITOR hMonitor, UINT flags )
{
   MONITORINFO mi;
   RECT        rc;
   int         w = prc->right - prc->left;
   int         h = prc->bottom - prc->top;

   // Get the nearest monitor if none is specified.
   if( NULL == hMonitor )
   {
      hMonitor = MonitorFromRect( prc, MONITOR_DEFAULTTONEAREST );
   }

   // Get the monitor work area or full area based on flags.
   mi.cbSize = sizeof( mi );
   GetMonitorInfo( hMonitor, &mi );

   rc = ( flags & MONITOR_WORKAREA ) ? mi.rcWork : mi.rcMonitor;

   // Center or clip the rectangle to the monitor area.
   if( flags & MONITOR_CENTER )
   {
      prc->left = rc.left + ( rc.right - rc.left - w ) / 2;
      prc->top = rc.top + ( rc.bottom - rc.top - h ) / 2;
      prc->right = prc->left + w;
      prc->bottom = prc->top + h;
   }
   else
   {
      prc->left = HB_MAX( rc.left, HB_MIN( rc.right - w, prc->left ) );
      prc->top = HB_MAX( rc.top, HB_MIN( rc.bottom - h, prc->top ) );
      prc->right = prc->left + w;
      prc->bottom = prc->top + h;
   }
}

#ifndef __XHARBOUR__

// Converts a RECT structure to a Harbour hash containing 'left', 'top', 'right', 'bottom' keys.
HB_EXPORT PHB_ITEM Rect2Hash( RECT *rc )
{
   PHB_ITEM phRect = hb_hashNew( NULL );
   PHB_ITEM pKey = hb_itemPutCConst( NULL, "left" );
   PHB_ITEM pValue = hb_itemPutNL( NULL, rc->left );

   hb_hashAddNew( phRect, pKey, pValue );

   hb_itemPutCConst( pKey, "top" );
   hb_itemPutNL( pValue, rc->top );
   hb_hashAddNew( phRect, pKey, pValue );

   hb_itemPutCConst( pKey, "right" );
   hb_itemPutNL( pValue, rc->right );
   hb_hashAddNew( phRect, pKey, pValue );

   hb_itemPutCConst( pKey, "bottom" );
   hb_itemPutNL( pValue, rc->bottom );
   hb_hashAddNew( phRect, pKey, pValue );

   hb_itemRelease( pKey );
   hb_itemRelease( pValue );

   return phRect;
}
#endif
