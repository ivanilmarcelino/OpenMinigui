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
#include <commctrl.h>

/* 
   Compatibility fix for older Borland C++ compilers that might not 
   have the WC_STATIC constant defined for the Static control class.
*/
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_STATIC "Static"
#endif
#include "hbvm.h"

// Stores the original Window Procedure of the Label control when subclassing is active.
// This is required to pass unprocessed messages back to the default Windows handler.
static WNDPROC    LabelOldWndProc;

/* Forward declarations for internal functions and subclassing logic */
LRESULT APIENTRY  LabelSubClassFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam );
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
#endif
HINSTANCE         GetInstance( void );

/*
 * HB_FUNC( INITLABEL )
 * --------------------
 * Purpose: 
 *    Initializes and creates a Win32 Static control (Label) within the HMG framework.
 * 
 * Parameters (from Harbour stack):
 *    1: hParent (HWND)      - Handle to the parent window.
 *    2: lpText (String)     - The initial text/caption of the label.
 *    3: hMenu (HMENU/ID)    - The control identifier.
 *    4-7: nX, nY, nW, nH    - Position and dimensions.
 *    9: lNotify (BOOL)      - Enable SS_NOTIFY (allows parent to receive click events).
 *    10: lSubClass (BOOL)   - Enable custom subclassing for mouse tracking.
 *    11: lBorder (BOOL)     - Draw a simple border.
 *    12: lClientEdge (BOOL) - Draw a sunken 3D border.
 *    13: lHScroll (BOOL)    - Enable horizontal scrollbar.
 *    14: lVScroll (BOOL)    - Enable vertical scrollbar.
 *    15: lTransparent (BOOL)- Enable WS_EX_TRANSPARENT for background blending.
 *    16: lVisible (BOOL)    - Initial visibility state.
 *    17: lRight (BOOL)      - Right-align text.
 *    18: lCenter (BOOL)     - Center-align text.
 *    19: lCenterImage (BOOL)- Vertically center text/image.
 *    20: lNoPrefix (BOOL)   - Disable '&' character shortcut processing.
 *
 * Returns:
 *    HWND of the created Label control.
 */
HB_FUNC( INITLABEL )
{
   HWND     hWnd;

   /* 
      Extract raw handles and primitive types from the Harbour Virtual Machine.
   */
   HWND     hParent = hmg_par_raw_HWND( 1 );
   HMENU    hMenu = hmg_par_raw_HMENU( 3 );
   int      nX = hb_parni( 4 );
   int      nY = hb_parni( 5 );
   int      nW = hb_parni( 6 );
   int      nH = hb_parni( 7 );

   BOOL     lNotify = hb_parl( 9 );
   BOOL     lSubClass = hb_parl( 10 );
   BOOL     lBorder = hb_parl( 11 );
   BOOL     lClientEdge = hb_parl( 12 );
   BOOL     lHScroll = hb_parl( 13 );
   BOOL     lVScroll = hb_parl( 14 );
   BOOL     lTransparent = hb_parl( 15 );
   BOOL     lVisible = hb_parl( 16 );
   BOOL     lRight = hb_parl( 17 );
   BOOL     lCenter = hb_parl( 18 );
   BOOL     lCenterImage = hb_parl( 19 );
   BOOL     lNoPrefix = hb_parl( 20 );

   /* Handle string conversion for Unicode builds to ensure correct Win32 API mapping */
#ifndef UNICODE
   LPCSTR   lpText = hb_parc( 2 );
#else
   LPCWSTR  lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif

   /* 
      Initialize Window Styles. 
      WS_CHILD is mandatory as labels are always contained within a window.
      WS_EX_TRANSPARENT is used to allow the parent's background to show through.
   */
   DWORD    Style = WS_CHILD;
   DWORD    ExStyle = lTransparent ? WS_EX_TRANSPARENT : 0;

   /* 
      SS_NOTIFY is critical: without it, Static controls are 'transparent' to mouse 
      clicks, meaning the parent won't receive STN_CLICKED notifications.
   */
   if( lNotify || lSubClass )
   {
      Style |= SS_NOTIFY;
   }

   if( lBorder )
   {
      Style |= WS_BORDER;
   }

   if( lHScroll )
   {
      Style |= WS_HSCROLL;
   }

   if( lVScroll )
   {
      Style |= WS_VSCROLL;
   }

   /* Note: Logic inversion check - if NOT visible flag is set, we apply WS_VISIBLE */
   if( !lVisible )
   {
      Style |= WS_VISIBLE;
   }

   /* Text alignment styles */
   if( lRight )
   {
      Style |= ES_RIGHT;
   }
   else if( lCenter )
   {
      Style |= ES_CENTER;
   }

   if( lCenterImage )
   {
      Style |= SS_CENTERIMAGE;
   }

   /* SS_NOPREFIX prevents the control from interpreting '&' as an accelerator key */
   if( lNoPrefix )
   {
      Style |= SS_NOPREFIX;
   }

   if( lClientEdge )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   /* Create the actual Win32 window using the 'Static' class */
   hWnd = CreateWindowEx( ExStyle, WC_STATIC, lpText, Style, nX, nY, nW, nH, hParent, hMenu, GetInstance(), NULL );

   /* 
      If subclassing is requested, we swap the window procedure.
      This allows to intercept mouse movement and other low-level events 
      that the standard Static control doesn't expose via standard notifications.
   */
   if( lSubClass )
   {
      LabelOldWndProc = SubclassWindow1( hWnd, LabelSubClassFunc );
   }

   /* Return the window handle back to the Harbour application */
   hmg_ret_raw_HWND( hWnd );

#ifdef UNICODE
   /* Clean up the temporary wide-string buffer allocated for Unicode conversion */
   hb_xfree( ( TCHAR * ) lpText );
#endif
}

/*
 * LabelSubClassFunc
 * ----------------
 * Purpose: 
 *    A custom Window Procedure used to intercept messages for Label controls.
 *    Primarily used to implement 'On Mouse Over' and 'On Mouse Leave' events.
 * 
 * Reasoning:
 *    Standard Win32 Static controls do not natively provide a 'Mouse Leave' event.
 *    By subclassing, we can use TrackMouseEvent to detect when the cursor leaves 
 *    the control's area and notify the Harbour-level event handler.
 */
LRESULT APIENTRY LabelSubClassFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   TRACKMOUSEEVENT   tme;
   static PHB_SYMB   pSymbol = NULL;
   static BOOL       bTracking = FALSE;

   LRESULT           r = 0;

   /* 
      When the mouse moves over the control, we start tracking the mouse 
      to catch the moment it leaves (WM_MOUSELEAVE).
   */
   if( Msg == WM_MOUSEMOVE )
   {
      if( !bTracking )
      {
         tme.cbSize = sizeof( TRACKMOUSEEVENT );
         tme.dwFlags = TME_LEAVE;
         tme.hwndTrack = hWnd;
         tme.dwHoverTime = HOVER_DEFAULT;

         if( _TrackMouseEvent( &tme ) )
         {
            bTracking = TRUE;
         }
      }
   }
   else if( Msg == WM_MOUSELEAVE )
   {
      /* Reset tracking state so it can be re-armed on the next move */
      bTracking = FALSE;
   }

   /* 
      Bridge the C-level Windows message to the Harbour-level event system.
      We look for a Harbour function named 'OLABELEVENTS' which acts as a 
      dispatcher for label-specific events in the HMG library.
   */
   if( Msg == WM_MOUSEMOVE || Msg == WM_MOUSELEAVE )
   {
      if( !pSymbol )
      {
         pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OLABELEVENTS" ) );
      }

      /* 
         Execute the Harbour callback. 
         We pass the Window Handle, Message ID, and parameters.
      */
      if( pSymbol && hb_vmRequestReenter() )
      {
         hb_vmPushSymbol( pSymbol );
         hb_vmPushNil();
         hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );
         hb_vmPushLong( Msg );
         hb_vmPushNumInt( wParam );
         hb_vmPushNumInt( lParam );
         hb_vmDo( 4 );

         /* Check if the Harbour code returned a specific result to override default behavior */
         r = hmg_par_LRESULT( -1 );
         hb_vmRequestRestore();
      }

      /* If the event handler returned a non-zero value, we stop further processing */
      if( r != 0 )
      {
         return r;
      }

      /* Otherwise, continue with the original window procedure */
      return CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );
   }

   /* Default behavior: pass all other messages to the original procedure */
   bTracking = FALSE;
   return CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );
}
