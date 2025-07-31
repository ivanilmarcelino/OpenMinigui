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

/*
 * FUNCTION INITPROGRESSBAR( parentHandle, menuHandle, x, y, width, height, minRange, maxRange, vertical, smooth, visible, initialPosition )
 *
 * Creates and initializes a progress bar control.
 *
 * Parameters:
 *   parentHandle: HWND - Handle to the parent window where the progress bar will be created.
 *   menuHandle: HMENU - Handle to a menu or a control identifier.  Used as the control ID for the progress bar.
 *   x: INT - The x-coordinate of the upper-left corner of the progress bar, relative to the parent window.
 *   y: INT - The y-coordinate of the upper-left corner of the progress bar, relative to the parent window.
 *   width: INT - The width of the progress bar control.
 *   height: INT - The height of the progress bar control.
 *   minRange: INT - The minimum value for the progress bar's range.
 *   maxRange: INT - The maximum value for the progress bar's range.
 *   vertical: LOGICAL - .T. to create a vertical progress bar, .F. for a horizontal progress bar.
 *   smooth: LOGICAL - .T. to create a smooth progress bar (PBS_SMOOTH style), .F. for a segmented progress bar.
 *   visible: LOGICAL - .T. to make the progress bar visible upon creation, .F. to create it hidden.
 *   initialPosition: INT - The initial position of the progress bar.
 *
 * Returns:
 *   HWND - The handle to the newly created progress bar control.
 *
 * Purpose:
 *   This function provides a convenient way to create and configure a progress bar control within an HMG application.
 *   It encapsulates the Windows API calls necessary to create the control, set its range, initial position, and style.
 *   The progress bar can be used to visually indicate the progress of a long-running operation to the user.
 */
HB_FUNC( INITPROGRESSBAR )
{
   HWND                 hbutton;
   DWORD                Style = WS_CHILD; // Basic style for the progress bar
   INITCOMMONCONTROLSEX i;                // Struct for initializing common controls

   // Initialize the common controls for progress bars.
   // This ensures that the progress bar control class is registered and available.
   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC = ICC_PROGRESS_CLASS;
   InitCommonControlsEx( &i );

   // Set styles based on input parameters.
   if( hb_parl( 9 ) )            // If the 9th parameter (vertical) is TRUE, make it vertical.
   {
      Style |= PBS_VERTICAL;
   }

   if( hb_parl( 10 ) )           // If the 10th parameter (smooth) is TRUE, make it smooth.
   {
      Style |= PBS_SMOOTH;
   }

   if( !hb_parl( 11 ) )          // If the 11th parameter (visible) is FALSE, make it visible by default.
   {
      Style |= WS_VISIBLE;
   }

   // Create the progress bar window with the specified styles and parameters.
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,       // Extended window style with a client edge
         PROGRESS_CLASS,         // Class name for a progress bar
         0,                      // No window title
         Style,                  // Window styles combined from above (WS_CHILD, PBS_VERTICAL, PBS_SMOOTH, WS_VISIBLE).
         hb_parni( 3 ),          // X-coordinate
         hb_parni( 4 ),          // Y-coordinate
         hb_parni( 5 ),          // Width
         hb_parni( 6 ),          // Height
         hmg_par_raw_HWND( 1 ),  // Handle to parent window
         hmg_par_raw_HMENU( 2 ), // Menu handle or identifier
         GetInstance(),          // Instance handle
         NULL                    // No additional application data
      );

   // Set the range of values for the progress bar.
   // MAKELONG combines the minimum and maximum range values into a single LPARAM.
   SendMessage( hbutton, PBM_SETRANGE, 0, MAKELONG( hb_parni( 7 ), hb_parni( 8 ) ) );

   // Set the initial position of the progress bar.
   SendMessage( hbutton, PBM_SETPOS, ( WPARAM ) hb_parni( 12 ), 0 );

   // Return the handle to the created progress bar.
   hmg_ret_raw_HWND( hbutton );
}
