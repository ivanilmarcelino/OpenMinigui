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

/*-----------------------------------------------------------------------------*
FUNCTION INITTIMER ( hwnd, nIDEvent, uElapse )
*------------------------------------------------------------------------------*
*
*  Description:
*     Initializes a Windows timer associated with a specific window.
*
*  Parameters:
*     hwnd    - The handle of the window to which the timer will be associated.
*     nIDEvent - A non-zero timer identifier.  This is used to distinguish between multiple timers associated with the same window.
*     uElapse - The timeout value, in milliseconds.  The system sends a WM_TIMER message to the application's message queue when this interval elapses.
*
*  Return Value:
*     .T. (TRUE) if the timer was successfully created; otherwise, .F. (FALSE).
*
*  Purpose:
*     This function provides a way to execute code at regular intervals within a window.
*     It wraps the Windows API SetTimer to create a timer that sends WM_TIMER messages to the specified window.
*     This is useful for tasks such as updating a clock display, polling for data changes, or triggering animations.
*
*  Notes:
*     The nIDEvent must be unique for each timer associated with the same window.
*     The timer continues to send messages until it is explicitly destroyed using KILLTIMER.
*     The resolution of Windows timers is limited, and the actual interval may be slightly longer than the specified uElapse.
*
*/
HB_FUNC( INITTIMER )
{
   hb_retl
   (
      ( UINT ) SetTimer
         (
            hmg_par_raw_HWND( 1 ),  // Window handle associated with the timer
            hmg_par_UINT( 2 ),      // Timer ID
            hmg_par_UINT( 3 ),      // Timer interval in milliseconds
            ( TIMERPROC ) NULL      // No callback function; uses default WM_TIMER message
         ) != 0
   );
}

/*-----------------------------------------------------------------------------*
FUNCTION KILLTIMER ( hwnd, nIDEvent )
*------------------------------------------------------------------------------*
*
*  Description:
*     Destroys a Windows timer associated with a specific window.
*
*  Parameters:
*     hwnd    - The handle of the window whose timer is to be destroyed.
*     nIDEvent - The identifier of the timer to be destroyed.
*
*  Return Value:
*     .T. (TRUE) if the timer was successfully destroyed; otherwise, .F. (FALSE).
*
*  Purpose:
*     This function stops and removes a timer that was previously created using INITTIMER.
*     It wraps the Windows API KillTimer to prevent the timer from sending further WM_TIMER messages.
*     It is crucial to call this function when a timer is no longer needed to avoid resource leaks and unexpected behavior.
*
*  Notes:
*     If the specified timer does not exist, the function returns .F. (FALSE).
*
*/
HB_FUNC( KILLTIMER )
{
   hb_retl(
      KillTimer(
         hmg_par_raw_HWND( 1 ),  // Window handle associated with the timer
         hmg_par_UINT( 2 )       // Timer ID to destroy
      ) != 0
   );
}
