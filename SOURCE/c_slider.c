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

#include <mgdefs.h>
#include <commctrl.h>

// Function to retrieve the instance handle of the application
HINSTANCE   GetInstance( void );

// Initialize a slider (trackbar) control with optional customization for orientation, tick marks, and selection range
HB_FUNC( INITSLIDER )
{
   HWND                 hTrackBar;
   DWORD                style = WS_CHILD | ( hb_parl( 10 ) ? TBS_NOTICKS : TBS_AUTOTICKS );  // Base style with or without ticks
   DWORD                selMin = 0, selMax = 0; // Default selection range values

   // Initialize common controls for the trackbar
   INITCOMMONCONTROLSEX icex;
   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC = ICC_BAR_CLASSES;
   InitCommonControlsEx( &icex );

   // Adjust styles based on function parameters
   if( hb_parl( 9 ) )
   {
      style |= TBS_VERT;            // Vertical orientation if parameter 9 is true
   }

   if( hb_parl( 11 ) )
   {
      style |= TBS_BOTH;            // Tick marks on both sides if parameter 11 is true
   }

   if( hb_parl( 12 ) )
   {
      style |= TBS_TOP;             // Top-aligned tick marks if parameter 12 is true
   }

   if( hb_parl( 13 ) )
   {
      style |= TBS_LEFT;            // Left-aligned tick marks if parameter 13 is true
   }

   if( !hb_parl( 14 ) )
   {
      style |= WS_VISIBLE;          // Set visible if parameter 14 is false
   }

   if( !hb_parl( 15 ) )
   {
      style |= WS_TABSTOP;          // Tab stop enabled if parameter 15 is false
   }

   // Enable selection range if parameter 16 is true
   if( hb_parl( 16 ) )
   {
      style |= TBS_ENABLESELRANGE;  // Enable selection range
      selMin = HB_MIN( hb_parnidef( 17, 0 ), hb_parnidef( 18, 0 ) ); // Set minimum of range
      selMax = HB_MAX( hb_parnidef( 17, 0 ), hb_parnidef( 18, 0 ) ); // Set maximum of range
   }

   // Create the trackbar (slider) control
   hTrackBar = CreateWindow
      (
         TRACKBAR_CLASS,         // Trackbar class name
         NULL,                   // No title text
         style,                  // Combined styles
         hb_parni( 3 ),          // X position
         hb_parni( 4 ),          // Y position
         hb_parni( 5 ),          // Width
         hb_parni( 6 ),          // Height
         hmg_par_raw_HWND( 1 ),  // Parent window handle
         hmg_par_raw_HMENU( 2 ), // Menu handle (used for control IDs)
         GetInstance(),          // Application instance handle
         NULL                    // Additional application data
      );

   // Set the range for the trackbar
   SendMessage( hTrackBar, TBM_SETRANGE, ( WPARAM ) TRUE, MAKELONG( hb_parni( 7 ), hb_parni( 8 ) ) ); // Minimum and maximum range

   // Set the selection range if enabled and valid
   if( hb_parl( 16 ) && ( selMin != selMax ) )
   {
      SendMessage( hTrackBar, TBM_SETSEL, ( WPARAM ) TRUE, MAKELONG( selMin, selMax ) );              // Set selection range
   }

   // Return the handle of the created trackbar
   hmg_ret_raw_HWND( hTrackBar );
}
