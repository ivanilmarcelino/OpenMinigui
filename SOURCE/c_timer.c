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

#include "hbapiitm.h"
#include "hbapierr.h"

#define MAX_TIMERS   128

typedef struct
{
   HWND  hwnd;
   UINT  timerID;
   UINT  interval;
   BOOL  inUse;
} TimerRecord;

static TimerRecord   s_timerRegistry[MAX_TIMERS];

/*
 * FUNCTION AddTimerToRegistry( hwnd, id, interval )
 *
 * Adds a timer to the internal timer registry.
 *
 * Parameters:
 *   hwnd     : The handle of the window associated with the timer (HWND).
 *   id       : The timer identifier (UINT).
 *   interval : The timer interval in milliseconds (UINT).
 *
 * Returns:
 *   A logical value indicating whether the timer was successfully added to the registry.
 *   Returns .T. (TRUE) if the timer was added, .F. (FALSE) if the registry is full.
 *
 * Purpose:
 *   This function is a helper function used to maintain a record of all active timers
 *   within the application. This allows for proper management and cleanup of timers
 *   when they are no longer needed. The registry prevents resource leaks by ensuring
 *   that timers are properly tracked and can be stopped when the associated window is closed.
 *
 * Notes:
 *   The registry has a fixed size (MAX_TIMERS). If the registry is full, the function will return .F.
 */
static BOOL AddTimerToRegistry( HWND hwnd, UINT id, UINT interval )
{
   int   i;
   for( i = 0; i < MAX_TIMERS; ++i )
   {
      if( !s_timerRegistry[i].inUse )
      {
         s_timerRegistry[i].hwnd = hwnd;
         s_timerRegistry[i].timerID = id;
         s_timerRegistry[i].interval = interval;
         s_timerRegistry[i].inUse = TRUE;
         return TRUE;
      }
   }

   return FALSE;  // No space
}

/*
 * FUNCTION RemoveTimerFromRegistry( hwnd, id )
 *
 * Removes a timer from the internal timer registry.
 *
 * Parameters:
 *   hwnd : The handle of the window associated with the timer (HWND).
 *   id   : The timer identifier (UINT).
 *
 * Returns:
 *   A logical value indicating whether the timer was successfully removed from the registry.
 *   Returns .T. (TRUE) if the timer was removed, .F. (FALSE) if the timer was not found.
 *
 * Purpose:
 *   This function is a helper function used to remove a timer from the internal registry
 *   when the timer is stopped. This ensures that the registry remains accurate and prevents
 *   the application from attempting to stop a timer that no longer exists.
 */
static BOOL RemoveTimerFromRegistry( HWND hwnd, UINT id )
{
   int   i;
   for( i = 0; i < MAX_TIMERS; ++i )
   {
      if( s_timerRegistry[i].inUse && s_timerRegistry[i].hwnd == hwnd && s_timerRegistry[i].timerID == id )
      {
         s_timerRegistry[i].inUse = FALSE;
         return TRUE;
      }
   }

   return FALSE;
}

/*
 * FUNCTION HMG_LISTTIMERS()
 *
 * Creates and returns an array containing information about all active timers in the registry.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   A Harbour array (PHB_ITEM) where each element represents an active timer. Each timer element
 *   is itself an array containing the following information:
 *     - Element 1: The window handle (HWND) associated with the timer (as a LONG_PTR).
 *     - Element 2: The timer identifier (UINT).
 *     - Element 3: The timer interval in milliseconds (UINT).
 *   If no timers are active, an empty array is returned.
 *
 * Purpose:
 *   This function is primarily intended for debugging and introspection. It allows developers
 *   to inspect the state of the timer registry and verify that timers are being created and
 *   destroyed correctly. This can be useful for identifying and resolving timer-related issues.
 *
 * Notes:
 *   The returned array is a newly allocated Harbour item and must be released using hb_itemRelease()
 *   when it is no longer needed to prevent memory leaks.
 */
HB_FUNC( HMG_LISTTIMERS )
{
   PHB_ITEM pArray = hb_itemArrayNew( 0 );
   int      i;

   for( i = 0; i < MAX_TIMERS; ++i )
   {
      if( s_timerRegistry[i].inUse )
      {
         PHB_ITEM pItem = hb_itemArrayNew( 3 );
         hb_arraySetNInt( pItem, 1, ( HB_PTRUINT ) s_timerRegistry[i].hwnd );
         hb_arraySetNI( pItem, 2, s_timerRegistry[i].timerID );
         hb_arraySetNI( pItem, 3, s_timerRegistry[i].interval );

         hb_arrayAddForward( pArray, pItem );
         hb_itemRelease( pItem );
      }
   }

   hb_itemReturnRelease( pArray );
}

/*
 * FUNCTION INITTIMER( hwnd, nIDEvent, uElapse )
 *
 * Initializes a Windows timer that sends WM_TIMER messages to the specified window at regular intervals.
 *
 * Parameters:
 *   hwnd     : The handle of the window that will receive the WM_TIMER messages (HWND).
 *   nIDEvent : The timer identifier (UINT). This value must be unique for each timer associated with the window.
 *   uElapse  : The timer interval in milliseconds (UINT). This specifies how often the WM_TIMER message is sent.
 *
 * Returns:
 *   A logical value indicating whether the timer was successfully created.
 *   Returns .T. (TRUE) if the timer was created, .F. (FALSE) if the timer creation failed.
 *
 * Purpose:
 *   This function allows developers to create timers that can be used to perform tasks at regular intervals.
 *   For example, a timer could be used to update a display, check for new data, or perform other periodic actions.
 *   The timer sends WM_TIMER messages to the specified window, which can then process the message and perform the
 *   desired action. The timer ID is used to identify the specific timer when the WM_TIMER message is received.
 *
 * Notes:
 *   The timer identifier (nIDEvent) must be unique for each timer associated with the same window. If a timer with
 *   the same identifier already exists, the existing timer will be replaced.
 *   The timer interval (uElapse) is specified in milliseconds. A value of 0 will cause the timer to fire as quickly
 *   as possible, but this can consume a significant amount of CPU time.
 *   The function also adds the timer to an internal registry for tracking purposes.
 */
HB_FUNC( INITTIMER )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   UINT  nIDEvent = hmg_par_UINT( 2 );
   UINT  uElapse = hmg_par_UINT( 3 );

   if( hwnd && nIDEvent != 0 && uElapse > 0 )
   {
      UINT_PTR  result = SetTimer( hwnd, nIDEvent, uElapse, ( TIMERPROC ) NULL );

      if( result )
      {
         AddTimerToRegistry( hwnd, nIDEvent, uElapse );
      }

      hb_retl( result != 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );  // Raise error if arguments are invalid
   }
}

/*
 * FUNCTION KILLTIMER( hwnd, nIDEvent )
 *
 * Destroys a Windows timer previously created using INITTIMER.
 *
 * Parameters:
 *   hwnd     : The handle of the window associated with the timer (HWND).
 *   nIDEvent : The timer identifier (UINT) of the timer to be destroyed.
 *
 * Returns:
 *   A logical value indicating whether the timer was successfully destroyed.
 *   Returns .T. (TRUE) if the timer was destroyed, .F. (FALSE) if the timer could not be destroyed
 *   (e.g., if the timer does not exist or the window handle is invalid).
 *
 * Purpose:
 *   This function is used to stop a timer that was previously started using INITTIMER. It is important
 *   to destroy timers when they are no longer needed to prevent resource leaks and ensure that the
 *   application behaves correctly.
 *
 * Notes:
 *   If the specified timer does not exist, the function will return .F. (FALSE).
 *   The function also removes the timer from the internal registry.
 */
HB_FUNC( KILLTIMER )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   UINT  nIDEvent = hmg_par_UINT( 2 );

   if( hwnd && nIDEvent != 0 )
   {
      BOOL  result = KillTimer( hwnd, nIDEvent );

      if( result )
      {
         RemoveTimerFromRegistry( hwnd, nIDEvent );
      }

      hb_retl( result != 0 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1071, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );  // Raise error if arguments are invalid
   }
}
