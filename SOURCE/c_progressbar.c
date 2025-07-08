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

// Function to get the current instance handle
HINSTANCE   GetInstance( void );

// Function: INITPROGRESSBAR
// Initializes and creates a progress bar with optional styles and configurations.
// Parameters:
//   1. HWND parentHandle - The handle of the parent window.
//   2. HMENU menuHandle - The handle of the menu or identifier for the progress bar.
//   3-6. int x, y, width, height - Coordinates and dimensions for positioning the progress bar.
//   7-8. int minRange, maxRange - The range of values for the progress bar.
//   9. bool vertical - TRUE to make the progress bar vertical.
//   10. bool smooth - TRUE to make the progress bar smooth.
//   11. bool visible - FALSE to hide the progress bar initially.
//   12. int initialPosition - The initial position value for the progress bar.
// Returns:
//   A handle to the created progress bar window.
HB_FUNC( INITPROGRESSBAR )
{
   HWND                 hbutton;
   DWORD                Style = WS_CHILD; // Basic style for the progress bar
   INITCOMMONCONTROLSEX i;                // Struct for initializing common controls

   // Initialize the common controls for progress bars
   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC = ICC_PROGRESS_CLASS;
   InitCommonControlsEx( &i );

   // Set styles based on input parameters
   if( hb_parl( 9 ) )            // If the 9th parameter is TRUE, make it vertical
   {
      Style |= PBS_VERTICAL;
   }

   if( hb_parl( 10 ) )           // If the 10th parameter is TRUE, make it smooth
   {
      Style |= PBS_SMOOTH;
   }

   if( !hb_parl( 11 ) )          // If the 11th parameter is FALSE, make it visible by default
   {
      Style |= WS_VISIBLE;
   }

   // Create the progress bar window with the specified styles and parameters
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,       // Extended window style with a client edge
         PROGRESS_CLASS,         // Class name for a progress bar
         0,                      // No window title
         Style,                  // Window styles combined from above
         hb_parni( 3 ),          // X-coordinate
         hb_parni( 4 ),          // Y-coordinate
         hb_parni( 5 ),          // Width
         hb_parni( 6 ),          // Height
         hmg_par_raw_HWND( 1 ),  // Handle to parent window
         hmg_par_raw_HMENU( 2 ), // Menu handle or identifier
         GetInstance(),          // Instance handle
         NULL                    // No additional application data
      );

   // Set the range of values for the progress bar
   SendMessage( hbutton, PBM_SETRANGE, 0, MAKELONG( hb_parni( 7 ), hb_parni( 8 ) ) );

   // Set the initial position of the progress bar
   SendMessage( hbutton, PBM_SETPOS, ( WPARAM ) hb_parni( 12 ), 0 );

   // Return the handle to the created progress bar
   hmg_ret_raw_HWND( hbutton );
}
