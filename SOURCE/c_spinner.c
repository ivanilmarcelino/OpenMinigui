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
#define _WIN32_IE 0x0501   // Set minimum required version of Internet Explorer (needed for some common controls)

#include <mgdefs.h>
#include <commctrl.h>

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
   // Define edit control class name for older versions of Borland C++
   #define WC_EDIT   "Edit"
#endif
#include "hbvm.h"

// Ensure support for the ICC_STANDARD_CLASSES flag in older compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) || defined( __XCC__ )
   #define ICC_STANDARD_CLASSES  0x00004000
#endif

// Forward declarations
LRESULT CALLBACK  OwnSpinProc( HWND hedit, UINT Msg, WPARAM wParam, LPARAM lParam );
HINSTANCE         GetInstance( void );

/**
 * Function: INITSPINNER
 * Initializes a spinner control with an associated "buddy" edit control.
 * The spinner control allows users to increment or decrement a numeric value.
 * Parameters:
 *   - hwnd (parent window handle)
 *   - Control styles, visibility, range, read-only setting, and alignment options.
 * Returns:
 *   - An array containing the handles to the edit and spinner controls.
 */
HB_FUNC( INITSPINNER )
{
   HWND                 hedit, hupdown;
   HWND                 hwnd = hmg_par_raw_HWND( 1 );                   // Parent window handle
   DWORD                Style1 = ES_NUMBER | WS_CHILD | ES_AUTOHSCROLL; // Edit control styles
   DWORD                Style2 = WS_CHILD | WS_BORDER | UDS_ARROWKEYS | UDS_ALIGNRIGHT | UDS_SETBUDDYINT | UDS_NOTHOUSANDS;   // Spinner control styles
   INITCOMMONCONTROLSEX i;

   i.dwSize = sizeof( INITCOMMONCONTROLSEX );

   // Set visibility and tab-stop options
   if( !hb_parl( 11 ) )
   {
      Style1 |= WS_VISIBLE;
      Style2 |= WS_VISIBLE;
   }

   if( !hb_parl( 12 ) )
   {
      Style1 |= WS_TABSTOP;
   }

   if( hb_parl( 13 ) )
   {
      Style2 |= UDS_WRAP;                    // Enable wrap-around behavior for spinner
   }

   if( hb_parl( 14 ) )
   {
      Style1 |= ES_READONLY;                 // Set edit control to read-only if specified
   }

   if( hb_parl( 15 ) )
   {
      Style2 |= UDS_HORZ | UDS_ALIGNRIGHT;   // Align spinner horizontally if specified
   }

   // Initialize standard controls
   i.dwICC = ICC_STANDARD_CLASSES;
   InitCommonControlsEx( &i );

   // Create the "buddy" edit control for numeric input
   hedit = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,       // Extended style for edit control
         WC_EDIT,                // Edit control class name
         NULL,                   // No initial text
         Style1,                 // Defined styles for the edit control
         hb_parni( 3 ),          // X-coordinate
         hb_parni( 4 ),          // Y-coordinate
         hb_parni( 5 ),          // Width
         hb_parni( 10 ),         // Height
         hwnd,                   // Parent window handle
         hmg_par_raw_HMENU( 2 ), // Control ID or menu handle
         GetInstance(),          // Application instance
         NULL                    // No additional parameters
      );

   // Initialize up-down (spinner) controls
   i.dwICC = ICC_UPDOWN_CLASS;
   InitCommonControlsEx( &i );

   // Create the up-down control
   hupdown = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,       // Extended style for up-down control
         UPDOWN_CLASS,           // Up-down control class name
         NULL,                   // No initial text
         Style2,                 // Defined styles for the up-down control
         0,                      // X-coordinate (auto-aligns with buddy)
         0,                      // Y-coordinate
         0,                      // Width (auto-sizes to fit buddy)
         0,                      // Height
         hwnd,                   // Parent window handle
         ( HMENU ) NULL,         // No menu handle
         GetInstance(),          // Application instance
         NULL                    // No additional parameters
      );

   // Associate the edit control with the up-down control (buddy relationship)
   SendMessage( hupdown, UDM_SETBUDDY, ( WPARAM ) hedit, ( LPARAM ) NULL );

   // Set the range of the spinner
   SendMessage( hupdown, UDM_SETRANGE32, ( WPARAM ) hb_parni( 8 ), ( LPARAM ) hb_parni( 9 ) );

   // Store the old window procedure, then subclass the edit control to handle custom messages
   SetProp( hedit, TEXT( "oldspinproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hedit, GWLP_WNDPROC ) );
   SubclassWindow2( hedit, OwnSpinProc );

   // Return handles of the edit and spinner controls
   hb_reta( 2 );
   hmg_storvnl_HANDLE( hedit, -1, 1 );
   hmg_storvnl_HANDLE( hupdown, -1, 2 );
}

/**
 * Function: SETSPINNERINCREMENT
 * Sets the increment for the spinner control (number of units to increment/decrement).
 * Parameters:
 *   - hWnd: Handle to the spinner control.
 *   - increment: Integer value specifying the increment step.
 */
HB_FUNC( SETSPINNERINCREMENT )
{
   UDACCEL  inc;
   inc.nSec = 0;
   inc.nInc = hb_parni( 2 );  // Set increment value
   SendMessage( hmg_par_raw_HWND( 1 ), UDM_SETACCEL, ( WPARAM ) 1, ( LPARAM ) & inc );
}

/**
 * Function: OwnSpinProc
 * Custom window procedure to handle specific messages for the buddy edit control.
 * Handles context menu events, destroys subclassing on WM_DESTROY, and other custom events.
 * Parameters:
 *   - hedit: Handle to the edit control.
 *   - Msg: Message ID.
 *   - wParam, lParam: Additional message-specific parameters.
 * Returns:
 *   - LRESULT: Result of message handling.
 */
LRESULT CALLBACK OwnSpinProc( HWND hedit, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   LRESULT           r;
   WNDPROC           OldWndProc;

   OldWndProc = ( WNDPROC ) ( LONG_PTR ) GetProp( hedit, TEXT( "oldspinproc" ) );

   switch( Msg )
   {
      case WM_DESTROY:
         // Restore original window procedure and remove property on destruction
         SubclassWindow2( hedit, OldWndProc );
         RemoveProp( hedit, TEXT( "oldspinproc" ) );
         break;

      case WM_CONTEXTMENU:
      case WM_GETDLGCODE:
         // Handle context menu and other custom dialog messages
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OSPINEVENTS" ) );
         }

         if( pSymbol )
         {
            // Push parameters onto the VM stack and call the event handler
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hedit );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }

         r = hmg_par_LRESULT( -1 ); // Get return result

         // Return result if non-zero, otherwise call default window procedure
         return( r != 0 ) ? r : CallWindowProc( OldWndProc, hedit, Msg, wParam, lParam );
   }

   // Call the default window procedure for unhandled messages
   return CallWindowProc( OldWndProc, hedit, Msg, wParam, lParam );
}
