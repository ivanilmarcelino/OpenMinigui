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
#include <mgdefs.h>                       // MiniGUI definitions for window elements and styles
#include <commctrl.h>                     // Common Windows controls, necessary for GUI elements

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
   // Define static class name for older Borland compilers
   #define WC_STATIC "Static"
#endif
#include "hbvm.h"                         // Harbour Virtual Machine (VM) definitions for handling VM functions

// Function prototypes
LRESULT APIENTRY  LabelSubClassFunc( HWND hwnd, UINT Msg, WPARAM wParam, LPARAM lParam );
static WNDPROC    LabelOldWndProc;        // Stores the original window procedure for subclassing
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );   // Converts ANSI strings to Wide strings for Unicode
#endif

// Retrieves the application instance handle
HINSTANCE         GetInstance( void );

// Function: INITLABEL
// Initializes a static label with various customizable styles and options.
HB_FUNC( INITLABEL )
{
   HWND     hWnd;
   DWORD    Style = WS_CHILD;             // Base style for a child window
   DWORD    ExStyle = hb_parl( 15 ) ? WS_EX_TRANSPARENT : 0;   // Optional transparency
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Label text in ANSI mode
#else
   LPCWSTR  lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Label text in Unicode mode
#endif

   // Apply styles based on parameters passed to the function
   if( hb_parl( 9 ) || hb_parl( 10 ) )
   {
      Style |= SS_NOTIFY;           // Enable mouse notifications if needed
   }

   if( hb_parl( 11 ) )
   {
      Style |= WS_BORDER;           // Add a border if specified
   }

   if( hb_parl( 13 ) )
   {
      Style |= WS_HSCROLL;          // Enable horizontal scroll if specified
   }

   if( hb_parl( 14 ) )
   {
      Style |= WS_VSCROLL;          // Enable vertical scroll if specified
   }

   if( !hb_parl( 16 ) )
   {
      Style |= WS_VISIBLE;          // Set the label as visible by default
   }

   if( hb_parl( 17 ) )
   {
      Style |= ES_RIGHT;            // Align text to the right if specified
   }

   if( hb_parl( 18 ) )
   {
      Style |= ES_CENTER;           // Center-align the text if specified
   }

   if( hb_parl( 19 ) )
   {
      Style |= SS_CENTERIMAGE;      // Center image vertically if specified
   }

   if( hb_parl( 20 ) )
   {
      Style |= SS_NOPREFIX;         // Suppress & for mnemonic if specified
   }

   if( hb_parl( 12 ) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;  // Add client edge style if specified
   }

   // Create the static label window with the specified styles
   hWnd = CreateWindowEx
      (
         ExStyle,                   // Extended window style
         WC_STATIC,                 // Class name for static control
         lpWindowName,              // Text displayed by the static control
         Style,                     // Style for the static control
         hb_parni( 4 ),             // X position
         hb_parni( 5 ),             // Y position
         hb_parni( 6 ),             // Width
         hb_parni( 7 ),             // Height
         hmg_par_raw_HWND( 1 ),     // Parent window handle
         hmg_par_raw_HMENU( 3 ),    // Menu or child-window identifier
         GetInstance(),             // Instance handle
         NULL                       // Additional application data
      );

   // Subclass the label for custom event handling if required
   if( hb_parl( 10 ) )
   {
      LabelOldWndProc = SubclassWindow1( hWnd, LabelSubClassFunc );
   }

   hmg_ret_raw_HWND( hWnd );  // Return the handle of the created label
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );              // Free converted string in Unicode
#endif
}

// Define _OLD_STYLE if old event calling style is required
#define _OLD_STYLE   0

// Subclass procedure for handling mouse events on the label
LRESULT APIENTRY LabelSubClassFunc( HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   TRACKMOUSEEVENT   tme;                             // Struct for tracking mouse events
   static PHB_SYMB   pSymbol = NULL;                  // Pointer to user-defined function (UDF) symbol
   static BOOL       bMouseTracking = FALSE;          // Flag for tracking mouse state
   LRESULT           r = 0;                           // Result to return
#if _OLD_STYLE
   BOOL              bCallUDF = FALSE;                // Flag to check if UDF should be called
#endif

   // Handle mouse movement and leave messages
   if( Msg == WM_MOUSEMOVE || Msg == WM_MOUSELEAVE )
   {
      if( Msg == WM_MOUSEMOVE )
      {
         // Begin tracking mouse leave event if mouse has entered
         if( !bMouseTracking )
         {
            tme.cbSize = sizeof( TRACKMOUSEEVENT );   // Set size of TRACKMOUSEEVENT
            tme.dwFlags = TME_LEAVE;                  // Track mouse leaving event
            tme.hwndTrack = hWnd;                     // Track this window
            tme.dwHoverTime = HOVER_DEFAULT;

            if( _TrackMouseEvent( &tme ) )            // Start tracking
            {
#if _OLD_STYLE
               bCallUDF = TRUE;        // Set to call UDF if _OLD_STYLE is enabled
#endif
               bMouseTracking = TRUE;  // Mark as tracking
            }
#if _OLD_STYLE
         }
         else
         {
            bCallUDF = FALSE;
#endif
         }
      }
      else
      {
#if _OLD_STYLE
         bCallUDF = TRUE;                 // Enable UDF call if _OLD_STYLE
#endif
         bMouseTracking = FALSE;          // Stop tracking on mouse leave
      }

      // Call user-defined function (UDF) if specified
#if _OLD_STYLE
      if( bCallUDF )
      {
#endif
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OLABELEVENTS" ) );   // Get UDF symbol
         }

         if( pSymbol && hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( pSymbol );               // Push UDF symbol onto VM stack
            hb_vmPushNil();                           // Push nil for SELF
            hb_vmPushNumInt( ( HB_PTRUINT ) hWnd );   // Push window handle
            hb_vmPushLong( Msg );                     // Push message
            hb_vmPushNumInt( wParam );                // Push WPARAM (event-specific data)
            hb_vmPushNumInt( lParam );                // Push LPARAM (event-specific data)
            hb_vmDo( 4 );                             // Call UDF with 4 parameters

            r = hmg_par_LRESULT( -1 );                // Get the result from the UDF
            hb_vmRequestRestore();                    // Restore VM state
         }

#if _OLD_STYLE
      }
#endif
      return( r != 0 ) ? r : CallWindowProc( LabelOldWndProc, hWnd, 0, 0, 0 );  // Return UDF result or pass to original proc
   }

   // Reset tracking if not a mouse message
   bMouseTracking = FALSE;
   return CallWindowProc( LabelOldWndProc, hWnd, Msg, wParam, lParam );   // Pass other messages to original proc
}

#undef _OLD_STYLE   // Undefine _OLD_STYLE to prevent accidental use elsewhere
