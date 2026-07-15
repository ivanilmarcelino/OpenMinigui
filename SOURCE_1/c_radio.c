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

// Compatibility fix for older Borland C++ compilers that may not define the standard Button class name macro.
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_BUTTON "Button"
#endif

// Forward declaration for Unicode string conversion utility.
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Retrieves the instance handle of the current application module.
HINSTANCE   GetInstance( void );

// Standard height for Radio Button controls to ensure UI consistency.
#define RADIO_HEIGHT 28

/*
 * _GetWindowText
 * Internal helper to handle string encoding transitions.
 * Converts a standard C string to a Wide string if UNICODE is defined, 
 * otherwise returns the pointer as-is for ANSI builds.
 */
static LPVOID _GetWindowText( const char *cText )
{
#ifndef UNICODE
   return( LPVOID ) cText;
#else
   return( LPVOID ) AnsiToWide( cText );
#endif
}

/*
 * _FreeWindowText
 * Internal helper to release memory allocated during string conversion.
 * Only performs an action in UNICODE mode to prevent memory leaks from AnsiToWide.
 */
static void _FreeWindowText( LPVOID pText )
{
#ifdef UNICODE
   hb_xfree( pText );
#else
   HB_SYMBOL_UNUSED( pText );
#endif
}

/*
 * _CreateRadio
 * Low-level wrapper for the Windows API CreateWindow function.
 * Specifically configured to create "Button" class controls with Radio Button behavior.
 */
static HWND _CreateRadio( HWND hParent, LPVOID lpText, HMENU hMenu, int x, int y, int width, DWORD style )
{
   return CreateWindow( WC_BUTTON, ( LPCTSTR ) lpText, style, x, y, width, RADIO_HEIGHT, hParent, hMenu, GetInstance(), NULL );
}

/*
 * HB_FUNC( INITRADIOGROUP )
 * Harbour-level function to initialize the FIRST radio button in a group.
 * 
 * Parameters:
 * 1: HWND   - Parent window handle.
 * 2: String - Caption text.
 * 3: HMENU  - Control Identifier (ID).
 * 4: Int    - Row (Y) position.
 * 5: Int    - Col (X) position.
 * 8: Int    - Width.
 * 9: Logic  - Invisible flag (.T. = Hidden).
 * 10: Logic - NoTabStop flag (.T. = Skip in Tab order).
 * 11: Logic - LeftText flag (.T. = Text on the left side of the button).
 *
 * Note: This function applies the WS_GROUP style, which signals Windows that 
 * this control starts a new logical group of radio buttons.
 */
HB_FUNC( INITRADIOGROUP )
{
   LPVOID   lpWindowName;
   DWORD    style;

   // Prepare the caption text based on build encoding (ANSI/Unicode).
   lpWindowName = _GetWindowText( hb_parc( 2 ) );

   // BS_AUTORADIOBUTTON: Automatically unchecks other buttons in the group when selected.
   // WS_GROUP: Defines the start of a group for keyboard navigation (arrow keys).
   style = BS_NOTIFY | WS_CHILD | BS_AUTORADIOBUTTON | WS_GROUP;

   // Parameter 9 is 'Invisible'. If false, we add WS_VISIBLE.
   if( !hb_parl( 9 ) )
   {
      style |= WS_VISIBLE;
   }

   // Parameter 10 is 'NoTabStop'. If false, we add WS_TABSTOP.
   if( !hb_parl( 10 ) )
   {
      style |= WS_TABSTOP;
   }

   // BS_LEFTTEXT: Places the selection circle to the right of the text.
   if( hb_parl( 11 ) )
   {
      style |= BS_LEFTTEXT;
   }

   // Create the control and return the handle to the Harbour virtual machine.
   hmg_ret_raw_HWND( _CreateRadio( hmg_par_raw_HWND( 1 ), lpWindowName, hmg_par_raw_HMENU( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 8 ), style ) );

   _FreeWindowText( lpWindowName );
}

/*
 * HB_FUNC( INITRADIOBUTTON )
 * Harbour-level function to initialize SUBSEQUENT radio buttons in an existing group.
 * 
 * Parameters:
 * Identical to INITRADIOGROUP, but logic differs regarding group boundaries.
 *
 * Note: This function does NOT apply WS_GROUP. In Windows, all radio buttons 
 * created after a WS_GROUP control (and before the next WS_GROUP or non-button control) 
 * belong to the same mutually exclusive set.
 */
HB_FUNC( INITRADIOBUTTON )
{
   LPVOID   lpWindowName;
   DWORD    style;

   lpWindowName = _GetWindowText( hb_parc( 2 ) );

   // Standard styles for a child radio button.
   style = BS_NOTIFY | WS_CHILD | BS_AUTORADIOBUTTON;

   // Visibility and alignment logic consistent with HMG standards.
   if( !hb_parl( 9 ) )
   {
      style |= WS_VISIBLE;
   }

   if( hb_parl( 10 ) )
   {
      style |= BS_LEFTTEXT;
   }

   // Create the control and return the handle.
   hmg_ret_raw_HWND( _CreateRadio( hmg_par_raw_HWND( 1 ), lpWindowName, hmg_par_raw_HMENU( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 8 ), style ) );

   _FreeWindowText( lpWindowName );
}
