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
#include <mgdefs.h>

// Function: INITHOTKEY
// Registers a system-wide hotkey that sends a notification to the specified window when triggered.
// Parameters:
//   1. hwnd (window handle) - The handle of the window that will receive the hotkey notifications.
//   2. fsModifiers (integer) - Modifier keys for the hotkey (e.g., ALT, CTRL, SHIFT).
//   3. vk (integer) - Virtual key code for the main key in the hotkey combination.
//   4. id (integer) - Unique identifier for this hotkey registration (used for management and lookup).
// Returns:
//   A logical (TRUE or FALSE) indicating if the hotkey was successfully registered.
HB_FUNC( INITHOTKEY )
{
   hb_retl
   (
      RegisterHotKey
         (
            hmg_par_raw_HWND( 1 ),  // Window handle to receive hot-key notification
            hb_parni( 4 ),          // Unique identifier for this hotkey
            hb_parni( 2 ),          // Modifier flags (ALT, CTRL, SHIFT, etc.)
            hb_parni( 3 )           // Virtual-key code
         )
   );
}

// Function: RELEASEHOTKEY
// Unregisters a previously registered hotkey for the specified window and identifier.
// Parameters:
//   1. hwnd (window handle) - The handle of the window associated with the hotkey.
//   2. id (integer) - The unique identifier of the hotkey to unregister.
// Returns:
//   A logical (TRUE or FALSE) indicating if the hotkey was successfully unregistered.
HB_FUNC( RELEASEHOTKEY )
{
   hb_retl( 
      UnregisterHotKey
      (
         hmg_par_raw_HWND( 1 ),     // Window handle associated with the hotkey
         hb_parni( 2 )              // Unique identifier of the hotkey
      )
   );
}
