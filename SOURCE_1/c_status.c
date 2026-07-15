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

#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>
#include "hbapierr.h"

// Maximum number of segments (parts) allowed in a single status bar.
// This limit prevents stack overflow and aligns with common UI design constraints.
#define MAX_PARTS 40

// Standard width reserved for the sizing grip (the triangle at the bottom-right).
// Used to ensure the last part doesn't overlap the resize handle.
#define SIZE_GRIP_WIDTH 21

// Internal padding used when calculating part distributions to ensure visual spacing.
#define PART_PADDING 8

/* 
 * Unicode/ANSI Compatibility Layer
 * -------------------------------
 * These macros and function prototypes facilitate the conversion between Harbour's 
 * internal string representation and the Windows API requirements.
 * In UNICODE builds, Harbour strings (usually UTF-8 or ANSI) are converted to 
 * Wide characters (LPWSTR) before being passed to the Win32 API.
 */
#ifdef UNICODE
   LPWSTR   AnsiToWide( LPCSTR );
   LPSTR    WideToAnsi( LPWSTR );
   #define HB_TEXT( x ) ( ( x ) ? AnsiToWide( x ) : NULL )
   #define HB_TEXT_FREE( x )  if( x ) hb_xfree( x )
#else
   #define HB_TEXT( x ) ( x )
   #define HB_TEXT_FREE( x )
#endif

HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

/* 
 * LoadStatusIcon
 * Internal helper function to load an icon for a status bar segment.
 * 
 * Parameters:
 *    - name: The resource name or file path of the icon.
 *    - cx, cy: Desired dimensions (usually matched to status bar height).
 * 
 * Logic:
 *    1. Attempts to load from the application's compiled resources.
 *    2. If not found, attempts to load from an external .ico file.
 */
static HICON LoadStatusIcon( LPCTSTR name, int cx, int cy )
{
   HICON hIcon = NULL;
   if( name && *name )
   {
      // Try loading from internal resources first (standard HMG behavior)
      hIcon = ( HICON ) LoadImage( GetResources(), name, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR );
      if( !hIcon )
      {
         // Fallback to external file if resource loading fails
         hIcon = ( HICON ) LoadImage( NULL, name, IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
      }
   }

   return hIcon;
}

/* 
 * GetStatusParts
 * Retrieves the current right-edge coordinates of all segments in the status bar.
 * 
 * Returns: The number of parts currently existing.
 */
static int GetStatusParts( HWND hWndSB, int *parts )
{
   return ( int ) SendMessage( hWndSB, SB_GETPARTS, MAX_PARTS, ( LPARAM ) parts );
}

/* 
 * HasSizeGrip
 * Checks if the parent window has the WS_SIZEBOX style.
 * 
 * Reasoning: If the parent is resizable, the status bar automatically displays 
 * a sizing grip. We need to know this to adjust the width of the last segment.
 */
static BOOL HasSizeGrip( HWND hWndParent )
{
   return hWndParent && ( GetWindowLong( hWndParent, GWL_STYLE ) & WS_SIZEBOX );
}

/* 
 * HB_FUNC( INITMESSAGEBAR )
 * Purpose: Initializes the main Status Bar container for a window.
 * 
 * Parameters:
 *    1: HWND - Handle of the parent window.
 *    2: INT  - Control ID for the status bar.
 * 
 * Returns: HWND of the created status bar.
 */
HB_FUNC( INITMESSAGEBAR )
{
   // Create the status window. SBT_TOOLTIPS is included to support per-part tooltips.
   HWND  hWndSB = CreateStatusWindow( WS_CHILD | WS_VISIBLE | SBT_TOOLTIPS, NULL, hmg_par_raw_HWND( 1 ), hb_parni( 2 ) );

   if( hWndSB )
   {
      // Initialize with a single part spanning the full width (-1).
      int   parts[1] = { -1 };
      SendMessage( hWndSB, SB_SETPARTS, 1, ( LPARAM ) parts );
   }

   hmg_ret_raw_HWND( hWndSB );
}

/* 
 * HB_FUNC( INITITEMBAR )
 * Purpose: Adds or configures a specific segment (part) within the status bar.
 * 
 * Parameters:
 *    1: HWND    - Status Bar handle.
 *    2: STRING  - Text to display in the part.
 *    3: INT     - Width (legacy/unused in some contexts).
 *    4: INT     - Space/Width for the new part.
 *    5: LOGICAL - Append mode (.T. to add to existing, .F. to reset/overwrite).
 *    6: STRING  - Icon name or path.
 *    7: STRING  - Tooltip text.
 *    8: INT     - Style flags (1: Raised/Popout, 2: Flat/No Borders).
 * 
 * Returns: INT - The total number of parts now in the bar.
 */
HB_FUNC( INITITEMBAR )
{
   HWND     hWndSB = hmg_par_raw_HWND( 1 );
   HWND     hWndParent = GetParent( hWndSB );
   int      nSpace = hb_parni( 4 );
   BOOL     lAppend = hb_parnl( 5 );
   int      parts[MAX_PARTS];
   int      nParts;
   int      i;
#ifndef UNICODE
   LPCSTR   text, iconName, tip;
#else
   LPWSTR   text, iconName, tip;
#endif
   RECT     rc;
   WORD     flags = 0;

   // Initialize parts array to zero.
   for( i = 0; i < MAX_PARTS; i++ )
   {
      parts[i] = 0;
   }

   // If appending, we retrieve existing part boundaries to calculate new offsets.
   nParts = lAppend ? GetStatusParts( hWndSB, parts ) : 0;

   // Convert Harbour strings to appropriate C strings (ANSI or Wide).
   text = HB_TEXT( hb_parc( 2 ) );
   iconName = HB_TEXT( hb_parc( 6 ) );
   tip = HB_TEXT( hb_parc( 7 ) );

   GetClientRect( hWndSB, &rc );

   if( lAppend )
   {
      SendMessage( hWndSB, SB_GETPARTS, 40, ( LPARAM ) parts );
   }

   nParts++;

   if( !lAppend )
   {
      // Single part mode: The part spans the entire width of the control.
      parts[nParts - 1] = rc.right;
   }
   else
   {
      /* 
       * Logic for Multi-Part Bars:
       * When adding a new part, we shift existing parts to the left to make room.
       * This implementation assumes parts are added from right-to-left or 
       * that the new part takes precedence in the layout.
       */
      for( i = 0; i < nParts - 1; i++ )
      {
         parts[i] -= nSpace - PART_PADDING;
      }

      // Adjust for the size grip if this is the second part being added.
      if( HasSizeGrip( hWndParent ) && nParts == 2 )
      {
         parts[0] -= SIZE_GRIP_WIDTH;
      }

      // The last part's right edge is anchored to the window edge,
      // minus the height of the bar to avoid the sizing grip area.
      parts[nParts - 1] = HasSizeGrip( hWndParent ) ? rc.right - rc.bottom - rc.top + 2 : rc.right;
   }

   // Apply the new partitioning to the control.
   SendMessage( hWndSB, SB_SETPARTS, nParts, ( LPARAM ) parts );

   // Determine visual style based on the 8th parameter.
   switch( hb_parni( 8 ) )
   {
      case 1:
         flags = SBT_POPOUT;           // Raised appearance
         break;

      case 2:
         flags = SBT_NOBORDERS;        // Flat appearance
         break;
   }

   // Set the text for the newly created part.
   if( text )
   {
      SendMessage( hWndSB, SB_SETTEXT, ( nParts - 1 ) | flags, ( LPARAM ) text );
   }

   // Load and set the icon if provided.
   if( iconName && *iconName )
   {
      // Calculate icon size based on status bar height (minus small padding).
      int   cy = rc.bottom - rc.top - 4;
      HICON hIcon = LoadStatusIcon( iconName, cy, cy );
      if( hIcon )
      {
         SendMessage( hWndSB, SB_SETICON, nParts - 1, ( LPARAM ) hIcon );
      }
   }

   // Set the tooltip for the specific part.
   if( tip )
   {
      SendMessage( hWndSB, SB_SETTIPTEXT, nParts - 1, ( LPARAM ) tip );
   }

   // Clean up allocated memory for Unicode strings.
   HB_TEXT_FREE( text );
   HB_TEXT_FREE( iconName );
   HB_TEXT_FREE( tip );

   hb_retni( nParts );
}

/* 
 * HB_FUNC( SETITEMBAR )
 * Purpose: Updates the text of an existing status bar segment.
 * 
 * Parameters:
 *    1: HWND   - Status Bar handle.
 *    2: STRING - New text.
 *    3: INT    - Part index (1-based).
 */
HB_FUNC( SETITEMBAR )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   int      iPos = hb_parni( 3 ) - 1;  // Convert Harbour 1-based index to C 0-based.
#ifndef UNICODE
   LPCSTR   lpText = HB_TEXT( hb_parc( 2 ) );
#else
   LPWSTR   lpText = HB_TEXT( hb_parc( 2 ) );
#endif

   /* 
    * Design Decision:
    * We retrieve the existing flags (like SBT_POPOUT) from the current text 
    * to ensure that updating the text doesn't reset the visual style of the part.
    */
   WORD     flags = HIWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, iPos, 0 ) );

   SendMessage( hWnd, SB_SETTEXT, iPos | flags, ( LPARAM ) lpText );
   HB_TEXT_FREE( lpText );
}

/* 
 * HB_FUNC( GETITEMBAR )
 * Purpose: Retrieves the text currently displayed in a status bar segment.
 * 
 * Parameters:
 *    1: HWND - Status Bar handle.
 *    2: INT  - Part index (1-based).
 * 
 * Returns: STRING - The text content of the segment.
 */
HB_FUNC( GETITEMBAR )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   int   iPos = hb_parni( 2 ) - 1;

   // First, determine the length of the text to allocate a buffer.
   int   nLen = LOWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, iPos, 0 ) ) + 1;
   TCHAR *buffer = ( TCHAR * ) hb_xgrab( nLen * sizeof( TCHAR ) );

   SendMessage( hWnd, SB_GETTEXT, iPos, ( LPARAM ) buffer );

#ifdef UNICODE
   // Convert Wide string back to ANSI for Harbour if necessary.
   {
      LPSTR ansi = WideToAnsi( buffer );
      hb_retc( ansi );
      hb_xfree( ansi );
   }

#else
   hb_retc( buffer );
#endif
   hb_xfree( buffer );
}

/* 
 * HB_FUNC( REFRESHITEMBAR )
 * Purpose: Recalculates part widths when the parent window is resized.
 * 
 * Parameters:
 *    1: HWND - Status Bar handle.
 *    2: INT  - Minimum size for the first part (prevents it from disappearing).
 * 
 * Logic:
 *    This function calculates a 'delta' (the change in window width) and 
 *    applies it to the segments. It ensures the rightmost part stays 
 *    anchored to the right edge while respecting the sizing grip.
 */
HB_FUNC( REFRESHITEMBAR )
{
   HWND  hWndSB = hmg_par_raw_HWND( 1 );
   HWND  hWndParent = GetParent( hWndSB );
   int   size = hb_parni( 2 );
   int   parts[MAX_PARTS];
   int   nParts = GetStatusParts( hWndSB, parts );
   RECT  rc;
   int   delta;
   BOOL  propagate = TRUE;
   int   i;

   GetClientRect( hWndSB, &rc );

   /* 
    * Calculate Delta:
    * If the window is maximized or has no size grip, delta is simply the 
    * difference between the client area and the last part's edge.
    * Otherwise, we subtract the size grip area from the calculation.
    */
   delta = ( nParts == 1 || IsZoomed( hWndParent ) || !HasSizeGrip( hWndParent ) ) ? rc.right - parts[nParts - 1] : rc.right - parts[nParts - 1] - rc.bottom - rc.top + 2;

   if( rc.right > 0 )
   {
      for( i = 0; i < nParts; i++ )
      {
         if( i == 0 )
         {
            // Ensure the first part doesn't shrink below the user-defined minimum.
            if( size >= parts[i] && delta < 0 )
            {
               propagate = FALSE;
            }
            else
            {
               if( parts[i] + delta < size )
               {
                  delta = size - parts[i];
               }

               parts[i] += delta;
            }
         }
         else if( propagate )
         {
            // Shift subsequent parts by the same delta to maintain relative spacing.
            parts[i] += delta;
         }
      }
   }

   SendMessage( hWndSB, SB_SETPARTS, nParts, ( LPARAM ) parts );
   hb_retni( nParts );
}

/* 
 * HB_FUNC( KEYTOGGLE )
 * Purpose: Toggles the state of keyboard keys (Caps Lock, Num Lock, Scroll Lock).
 * 
 * Parameters:
 *    1: WORD - Virtual Key code (e.g., VK_CAPITAL).
 * 
 * Side Effects: Updates the system keyboard state buffer.
 */
HB_FUNC( KEYTOGGLE )
{
   BYTE  pBuffer[256];
   WORD  wKey = hmg_par_WORD( 1 );

   GetKeyboardState( pBuffer );

   // Toggle the low-order bit (0x01) which represents the toggle state.
   if( pBuffer[wKey] & 0x01 )
   {
      pBuffer[wKey] &= 0xFE;
   }
   else
   {
      pBuffer[wKey] |= 0x01;
   }

   SetKeyboardState( pBuffer );
}

/* 
 * HB_FUNC( KEYTOGGLENT )
 * Purpose: Toggles keyboard keys using simulated hardware events.
 * 
 * Reasoning: On Windows NT/2000/XP and later, simulating a key press 
 * via keybd_event is often more reliable for updating system-wide 
 * indicator lights than modifying the keyboard state buffer.
 */
HB_FUNC( KEYTOGGLENT )
{
   BYTE  wKey = hmg_par_BYTE( 1 );

   // Simulate Key Down
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY, 0 );

   // Simulate Key Up
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0 );
}

/* 
 * HB_FUNC( SETSTATUSITEMICON )
 * Purpose: Changes the icon of a specific status bar segment.
 * 
 * Parameters:
 *    1: HWND   - Status Bar handle.
 *    2: INT    - Part index (1-based).
 *    3: STRING - Icon name or path.
 *    4: HICON  - Optional raw handle to an existing icon.
 */
HB_FUNC( SETSTATUSITEMICON )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   int      nPart = hb_parni( 2 ) - 1;
   HICON    hIcon = HB_ISNUM( 4 ) ? hmg_par_raw_HICON( 4 ) : NULL;
   HICON    oldIcon;
   RECT     rc;
   int      cy;
#ifndef UNICODE
   LPCSTR   name;
#else
   LPWSTR   name;
#endif

   /* 
    * Memory Management:
    * Before setting a new icon, we retrieve and destroy the old one 
    * to prevent GDI resource leaks.
    */
   oldIcon = ( HICON ) SendMessage( hWnd, SB_GETICON, nPart, 0 );
   if( oldIcon )
   {
      DestroyIcon( oldIcon );
   }

   // If no raw handle was provided, load the icon from the name string.
   if( !hIcon )
   {
      GetClientRect( hWnd, &rc );
      cy = rc.bottom - rc.top - 4;
      name = HB_TEXT( hb_parc( 3 ) );
      hIcon = LoadStatusIcon( name, cy, cy );
      HB_TEXT_FREE( name );
   }

   if( hIcon )
   {
      SendMessage( hWnd, SB_SETICON, nPart, ( LPARAM ) hIcon );
   }
}

/* 
 * HB_FUNC( SETSTATUSBARSIZE )
 * Purpose: Manually sets the widths of all parts in the status bar using an array.
 * 
 * Parameters:
 *    1: HWND  - Status Bar handle.
 *    2: ARRAY - Array of integers representing the width of each part.
 * 
 * Logic:
 *    The Win32 SB_SETPARTS message requires an array of absolute right-edge 
 *    coordinates. This function converts a Harbour array of relative widths 
 *    into these absolute coordinates by accumulating the values.
 */
HB_FUNC( SETSTATUSBARSIZE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   int   nParts = ( int ) hb_parinfa( 2, 0 );
   int   *parts = ( int * ) hb_xgrab( sizeof( int ) * nParts );
   int   acc = 0;
   int   i;

   for( i = 0; i < nParts; i++ )
   {
      acc += HB_PARNI( 2, i + 1 );
      parts[i] = acc;
   }

   SendMessage( hWnd, SB_SETPARTS, nParts, ( LPARAM ) parts );

   // Force a window move/resize with 0 dimensions to trigger an internal
   // repaint and layout update of the status bar.
   MoveWindow( hWnd, 0, 0, 0, 0, TRUE );
   hb_xfree( parts );
}

/* 
 * HB_FUNC( REFRESHPROGRESSITEM )
 * Purpose: Synchronizes the position of a progress bar embedded in a status bar.
 * 
 * Parameters:
 *    1: HWND - Status Bar handle.
 *    2: INT  - Part index (1-based) where the progress bar is located.
 *    3: HWND - Progress Bar handle.
 * 
 * Logic:
 *    Retrieves the bounding rectangle of the status bar segment and 
 *    moves the progress bar control to perfectly overlay that area.
 */
HB_FUNC( REFRESHPROGRESSITEM )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   RECT  rc;

   SendMessage( hWnd, SB_GETRECT, hb_parni( 2 ) - 1, ( LPARAM ) & rc );

   SetWindowPos( hmg_par_raw_HWND( 3 ), 0, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE );
}

/* 
 * HB_FUNC( CREATEPROGRESSBARITEM )
 * Purpose: Creates a Progress Bar control as a child of the Status Bar.
 * 
 * Parameters:
 *    1: HWND    - Status Bar handle (Parent).
 *    2: INT     - Part index (1-based) to occupy.
 *    3: INT     - Initial position value.
 *    4: INT     - Range Minimum.
 *    5: INT     - Range Maximum.
 * 
 * Returns: HWND of the created Progress Bar.
 */
HB_FUNC( CREATEPROGRESSBARITEM )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   RECT  rc;
   DWORD style = WS_CHILD | PBS_SMOOTH;
   HWND  hProg;

   // Get the coordinates of the target segment to size the progress bar.
   SendMessage( hWnd, SB_GETRECT, hb_parni( 2 ) - 1, ( LPARAM ) & rc );

   // If an initial position is provided, make the control visible immediately.
   if( hb_parni( 3 ) )
   {
      style |= WS_VISIBLE;
   }

   // Create the progress bar. Height is reduced by 1 pixel to prevent
   // overlapping the status bar's bottom border.
   hProg = CreateWindowEx( 0, PROGRESS_CLASS, NULL, style, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top - 1, hWnd, NULL, GetInstance(), NULL );

   if( hProg )
   {
      SendMessage( hProg, PBM_SETRANGE, 0, MAKELONG( hb_parni( 4 ), hb_parni( 5 ) ) );
      SendMessage( hProg, PBM_SETPOS, hb_parni( 3 ), 0 );
      hmg_ret_raw_HWND( hProg );
   }
   else
   {
      hb_ret();
   }
}

/* 
 * HB_FUNC( SETPOSPROGRESSBARITEM )
 * Purpose: Updates the value of an embedded progress bar and manages visibility.
 * 
 * Parameters:
 *    1: HWND - Progress Bar handle.
 *    2: INT  - New position value.
 * 
 * Logic:
 *    In HMG, a progress bar in a status bar is often hidden when its 
 *    value is 0 to allow the underlying status text to be visible.
 */
HB_FUNC( SETPOSPROGRESSBARITEM )
{
   HWND  hProg = hmg_par_raw_HWND( 1 );
   int   nPos = hb_parni( 2 );

   // Toggle visibility: Show if position > 0, Hide if 0.
   ShowWindow( hProg, nPos ? SW_SHOW : SW_HIDE );
   SendMessage( hProg, PBM_SETPOS, nPos, 0 );
}
