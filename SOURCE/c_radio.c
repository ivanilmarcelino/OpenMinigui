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

#include <mgdefs.h>     // Include custom definitions for MiniGUI framework
#include <commctrl.h>   // Include common control library for GUI elements

// Compatibility for older Borland compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_BUTTON "Button"
#endif

// Function declaration to convert ANSI to wide characters for Unicode support
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Function declaration to get the instance handle of the application
HINSTANCE   GetInstance( void );

/*
 * Function: INITRADIOGROUP
 * Initializes a group of radio buttons with specified styles and attributes.
 * Parameters:
 *   - hwnd: Handle to the parent window.
 *   - text: Text label for the radio button group.
 *   - hmenu: Unique ID or menu handle for the radio group.
 *   - x, y: Coordinates for the radio group's position.
 *   - width: Width of the radio button group.
 *   - visible: Boolean to control visibility of the radio group.
 *   - tabStop: Boolean to set if the radio group is part of the tab order.
 *   - leftText: Boolean to display text on the left side of the button.
 * Returns:
 *   - Handle to the created radio group control.
 */
HB_FUNC( INITRADIOGROUP )
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Retrieve text label in ANSI format
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );          // Convert text label to wide characters for Unicode
#endif
   DWORD    Style = BS_NOTIFY | WS_CHILD | BS_AUTORADIOBUTTON | WS_GROUP;  // Base styles for radio group

   // Set visibility style if specified
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;
   }

   // Set tab stop style if specified
   if( !hb_parl( 10 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Set text alignment to the left if specified
   if( hb_parl( 11 ) )
   {
      Style |= BS_LEFTTEXT;
   }

   // Create the radio group control and return its handle
   hmg_ret_raw_HWND
   (
      CreateWindow
         (
            WC_BUTTON,                    // Class name for the button control
            lpWindowName,                 // Text label for the radio group
            Style,                        // Styles set above
            hb_parni( 4 ),                // X-coordinate
            hb_parni( 5 ),                // Y-coordinate
            hb_parni( 8 ),                // Width
            28,                           // Default height of the button group
            hmg_par_raw_HWND( 1 ),        // Handle to parent window
            hmg_par_raw_HMENU( 3 ),       // Menu handle or unique control ID
            GetInstance(),                // Application instance handle
            NULL                          // No additional parameters
         )
   );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );  // Free allocated memory if Unicode conversion was used
#endif
}

/*
 * Function: INITRADIOBUTTON
 * Initializes a single radio button with specified styles and attributes.
 * Parameters:
 *   - hwnd: Handle to the parent window.
 *   - text: Text label for the radio button.
 *   - hmenu: Unique ID or menu handle for the radio button.
 *   - x, y: Coordinates for the radio button's position.
 *   - width: Width of the radio button.
 *   - visible: Boolean to control visibility of the radio button.
 *   - leftText: Boolean to display text on the left side of the button.
 * Returns:
 *   - Handle to the created radio button control.
 */
HB_FUNC( INITRADIOBUTTON )
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Retrieve text label in ANSI format
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert text label to wide characters for Unicode
#endif
   DWORD    Style = BS_NOTIFY | WS_CHILD | BS_AUTORADIOBUTTON;    // Base styles for individual radio button

   // Set visibility style if specified
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;
   }

   // Set text alignment to the left if specified
   if( hb_parl( 10 ) )
   {
      Style |= BS_LEFTTEXT;
   }

   // Create the radio button control and return its handle
   hmg_ret_raw_HWND
   (
      CreateWindow
         (
            WC_BUTTON,                    // Class name for the button control
            lpWindowName,                 // Text label for the radio button
            Style,                        // Styles set above
            hb_parni( 4 ),                // X-coordinate
            hb_parni( 5 ),                // Y-coordinate
            hb_parni( 8 ),                // Width
            28,                           // Default height of the button
            hmg_par_raw_HWND( 1 ),        // Handle to parent window
            hmg_par_raw_HMENU( 3 ),       // Menu handle or unique control ID
            GetInstance(),                // Application instance handle
            NULL                          // No additional parameters
         )
   );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );  // Free allocated memory if Unicode conversion was used
#endif
}
