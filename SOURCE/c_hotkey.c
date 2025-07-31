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

/*
 * FUNCTION INITHOTKEY( hwnd, fsModifiers, vk, id )
 *
 * Registers a system-wide hotkey. When the hotkey is pressed, a WM_HOTKEY message
 * is sent to the specified window.
 *
 * Parameters:
 *   hwnd        : HWND - The handle of the window that will receive the WM_HOTKEY message.
 *   fsModifiers : NUMERIC - Modifier flags specifying which modifier keys (e.g., ALT, CTRL, SHIFT)
 *                 must be pressed in combination with the virtual key.  Valid values are defined
 *                 by the Windows API (e.g., MOD_ALT, MOD_CONTROL, MOD_SHIFT).
 *   vk          : NUMERIC - The virtual-key code of the key that must be pressed to trigger the hotkey.
 *                 See the Windows API documentation for a list of valid virtual-key codes (e.g., VK_F1, VK_A).
 *   id          : NUMERIC - A unique identifier for the hotkey. This identifier is included in the WM_HOTKEY
 *                 message, allowing the window to distinguish between different hotkeys.
 *
 * Returns:
 *   LOGICAL - .T. if the hotkey was successfully registered; otherwise, .F..
 *
 * Purpose:
 *   This function allows an application to define global hotkeys that trigger actions even when the application
 *   is not in the foreground. This is useful for implementing features like global shortcuts or system-wide
 *   commands. For example, a media player might use a global hotkey to start or stop playback, regardless of
 *   which application currently has focus.
 *
 * Notes:
 *   - Hotkeys are system-wide resources, so it's important to unregister them when they are no longer needed
 *     to avoid conflicts with other applications.
 *   - The 'id' parameter must be unique within the application.
 *   - If another application has already registered the same hotkey, this function will fail.
 *   - The function relies on the Windows API function RegisterHotKey.
 */
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

/*
 * FUNCTION RELEASEHOTKEY( hwnd, id )
 *
 * Unregisters a previously registered hotkey.
 *
 * Parameters:
 *   hwnd : HWND - The handle of the window that registered the hotkey.
 *   id   : NUMERIC - The unique identifier of the hotkey to unregister.  This must match the 'id'
 *          used when the hotkey was initially registered with INITHOTKEY().
 *
 * Returns:
 *   LOGICAL - .T. if the hotkey was successfully unregistered; otherwise, .F..
 *
 * Purpose:
 *   This function releases a hotkey that was previously registered using INITHOTKEY(). It is crucial to
 *   unregister hotkeys when they are no longer needed to prevent conflicts with other applications and
 *   to free up system resources.  Failing to unregister hotkeys can lead to unexpected behavior and
 *   resource leaks.
 *
 * Notes:
 *   - The function relies on the Windows API function UnregisterHotKey.
 *   - If the specified hotkey was not previously registered, this function will return .F..
 */
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
