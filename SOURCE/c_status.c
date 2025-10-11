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
#define _WIN32_IE 0x0501                  // Sets the minimum required version of Internet Explorer to 5.01 for compatibility.
#include <mgdefs.h>                       // Includes necessary definitions and macros.
#include <commctrl.h>                     // Includes common controls header for status bar and tooltips.
#include "hbapierr.h"                     // Include Harbour error handling API
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );         // Converts ANSI string to Unicode.
LPSTR       WideToAnsi( LPWSTR );         // Converts Unicode string to ANSI.
#endif

// Function prototypes for obtaining instances and resources.
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

/*
 * FUNCTION:  INITMESSAGEBAR
 *
 *  Description:
 *      Initializes a status bar with a single section.  This function creates
 *      a status bar window and sets it up with one part, effectively making
 *      it a simple message bar.
 *
 *  Parameters:
 *      1: Parent window handle (HWND). The status bar will be created as a child of this window.
 *      2: Status bar ID (numeric).  An identifier for the status bar control.
 *
 *  Returns:
 *      HWND: The handle to the created status bar window.  Returns NULL if the
 *            status bar creation fails.
 *
 *  Purpose:
 *      This function provides a basic status bar for displaying simple messages.
 *      It's a starting point for more complex status bar configurations.
 */
HB_FUNC( INITMESSAGEBAR )
{
   HWND  hWndSB;                          // Handle for the status bar window.

   // Creates a visible status bar window as a child of the specified parent.
   hWndSB = CreateStatusWindow( WS_CHILD | WS_VISIBLE | SBT_TOOLTIPS, NULL, hmg_par_raw_HWND( 1 ), hb_parni( 2 ) );

   if( hWndSB )
   {
      int   ptArray[1] = { -1 };

      // Sets the status bar to one section.
      SendMessage( hWndSB, SB_SETPARTS, 1, ( LPARAM ) ptArray );
   }

   hmg_ret_raw_HWND( hWndSB );            // Returns the status bar handle.
}

/*
 * FUNCTION: INITITEMBAR
 *
 *  Description:
 *      Initializes a customized item bar (status bar) with icons and text.
 *      This function allows for creating a status bar with multiple sections,
 *      each potentially containing an icon, text, and tooltip.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to initialize.
 *      2: Text for the item (character). The text to display in the status bar section.
 *      3: (Unused)
 *      4: Space between items (numeric).  The amount of space to leave between sections.
 *      5: Initialize from existing parts (logical).  If .T., the function will add a new part to the existing status bar. If .F., it will create a new status bar.
 *      6: Icon name (character). The name of the icon resource or the path to an icon file.
 *      7: Tooltip text (character). The text to display as a tooltip when the mouse hovers over the item.
 *      8: Display flags (numeric).  Flags to control the appearance of the item (e.g., SBT_POPOUT, SBT_NOBORDERS).
 *
 *  Returns:
 *      Numeric: The updated number of parts in the status bar.
 *
 *  Purpose:
 *      This function provides a way to create a more sophisticated status bar
 *      with multiple interactive elements, such as icons and tooltips.  It's
 *      useful for providing detailed status information to the user.
 */
HB_FUNC( INITITEMBAR )
{
   HWND     hWndSB = hmg_par_raw_HWND( 1 );
   HWND     hWndParent = GetParent( hWndSB );
   DWORD    Style = hWndParent ? GetWindowLong( hWndParent, GWL_STYLE ) : 0;
   int      n, nSpace = hb_parni( 4 );
   int      nrOfParts = hb_parnl( 5 ) ? ( int ) SendMessage( hWndSB, SB_GETPARTS, 40, 0 ) : 0;
   int      ptArray[40] = { 0 };
#ifndef UNICODE
   LPCSTR   lpText = hb_parc( 2 ), lpIconName = hb_parc( 6 ), lpTipText = hb_parc( 7 );
#else
   LPWSTR   lpText = AnsiToWide( hb_parc( 2 ) ), lpIconName = AnsiToWide( hb_parc( 6 ) ), lpTipText = AnsiToWide( hb_parc( 7 ) );
#endif
   RECT     rect;
   WORD     displayFlags;

   if( hb_parnl( 5 ) )
   {
      SendMessage( hWndSB, SB_GETPARTS, 40, ( LPARAM ) ptArray );
   }

   nrOfParts++;

   GetClientRect( hWndSB, &rect );

   if( !hb_parnl( 5 ) )
   {
      ptArray[nrOfParts - 1] = rect.right;
   }
   else
   {
      for( n = 0; n < nrOfParts - 1; n++ )
      {
         ptArray[n] -= nSpace - 8;
      }

      // Adjusts the first section for the size grip if present.
      if( Style & WS_SIZEBOX )
      {
         if( nrOfParts == 2 )
         {
            ptArray[0] -= 21;
         }
      }

      ptArray[nrOfParts - 1] = ( Style & WS_SIZEBOX ) ? rect.right - rect.bottom - rect.top + 2 : rect.right;
   }

   SendMessage( hWndSB, SB_SETPARTS, nrOfParts, ( LPARAM ) ptArray );

   if( lpIconName && *lpIconName )
   {
      int   cx = rect.bottom - rect.top - 4, cy = cx;
      HICON hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR );
      if( !hIcon )
      {
         hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
      }

      if( hIcon )
      {
         SendMessage( hWndSB, SB_SETICON, nrOfParts - 1, ( LPARAM ) hIcon );
      }
   }

   displayFlags = ( WORD ) ( hb_parni( 8 ) == 1 ? SBT_POPOUT : ( hb_parni( 8 ) == 2 ? SBT_NOBORDERS : 0 ) );

   if( lpText )
   {
      SendMessage( hWndSB, SB_SETTEXT, ( nrOfParts - 1 ) | displayFlags, ( LPARAM ) lpText );
   }

   if( lpTipText )
   {
      SendMessage( hWndSB, SB_SETTIPTEXT, nrOfParts - 1, ( LPARAM ) lpTipText );
   }

#ifdef UNICODE
   hb_xfree( lpText );
   hb_xfree( lpIconName );
   hb_xfree( lpTipText );
#endif
   hb_retni( nrOfParts );
}

/*
 * FUNCTION: SETITEMBAR
 *
 *  Description:
 *      Updates the text in a specified part of the item bar (status bar).
 *      This function allows you to change the text displayed in a specific
 *      section of the status bar.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to update.
 *      2: Text to set (character). The new text to display in the specified section.
 *      3: Item position (numeric). The index of the section to update (1-based).
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is useful for dynamically updating the status bar with
 *      information that changes during the application's execution.
 */
HB_FUNC( SETITEMBAR )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 ); // Handle for the status bar.
   int      iPos = hb_parni( 3 ) - 1;     // Position of the item to update.
   WORD     nFlags;                 // Current display flags for the text.
#ifndef UNICODE
   LPCSTR   lpText = hb_parc( 2 );  // ANSI text to set.
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );             // Convert to Unicode text if needed.
#endif

   // Retrieve the current text length and style flags for the part.
   nFlags = HIWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, iPos, 0 ) );
   SendMessage( hWnd, SB_SETTEXT, iPos | nFlags, ( LPARAM ) lpText );   // Update the text.
#ifdef UNICODE
   hb_xfree( lpText );                 // Free allocated memory if using Unicode.
#endif
}

/*
 * FUNCTION: GETITEMBAR
 *
 *  Description:
 *      Retrieves and returns the text of a specified part of the item bar (status bar).
 *      This function allows you to read the text displayed in a specific
 *      section of the status bar.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to query.
 *      2: Item position (numeric). The index of the section to retrieve text from (1-based).
 *
 *  Returns:
 *      Character: The text currently displayed in the specified section of the status bar.
 *
 *  Purpose:
 *      This function is useful for retrieving the current status information
 *      displayed in the status bar, allowing other parts of the application
 *      to react to changes in the status.
 */
HB_FUNC( GETITEMBAR )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   HWND  hWnd = hmg_par_raw_HWND( 1 ); // Handle for the status bar.
   int   iPos = hb_parni( 2 ) - 1;     // Position of the item to retrieve.
   int   nLen = LOWORD( SendMessage( hWnd, SB_GETTEXTLENGTH, iPos, 0 ) ) + 1;

   // Allocate memory based on text length and retrieve the text.
   TCHAR *cString = ( TCHAR * ) hb_xgrab( nLen * sizeof( TCHAR ) );
   SendMessage( hWnd, SB_GETTEXT, iPos, ( LPARAM ) cString );

#ifndef UNICODE
   hb_retc( cString );                 // Return the ANSI string directly.
#else
   pStr = WideToAnsi( cString );       // Convert to ANSI and return.
   hb_retc( pStr );
   hb_xfree( pStr );                   // Free memory after conversion.
#endif
   hb_xfree( cString );                // Free the initial memory allocation.
}

/*
 * FUNCTION: REFRESHITEMBAR
 *
 *  Description:
 *      Refreshes the layout and dimensions of parts in the item bar (status bar).
 *      This function recalculates and updates the size and position of each
 *      section in the status bar, ensuring that they are correctly displayed,
 *      especially after resizing or other layout changes.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to refresh.
 *      2: Size (numeric). The new size for the first part of the status bar.
 *
 *  Returns:
 *      Numeric: The updated number of parts in the status bar.
 *
 *  Purpose:
 *      This function is essential for maintaining the correct appearance of
 *      the status bar when the application window is resized or when the
 *      content of the status bar changes dynamically.
 *      It ensures that the status bar sections are properly aligned and
 *      that the text and icons are displayed correctly.
 */
HB_FUNC( REFRESHITEMBAR )
{
   HWND  hWndSB = hmg_par_raw_HWND( 1 );
   int   n, size = hb_parni( 2 );
   int   nrOfParts = ( int ) SendMessage( hWndSB, SB_GETPARTS, 40, 0 );
   int   ptArray[40];
   RECT  rect;
   HWND  hWndParent = GetParent( hWndSB );
   DWORD Style = hWndParent ? GetWindowLong( hWndParent, GWL_STYLE ) : 0;
   int   nDev;
   BOOL  s = TRUE;                     // Indicates if adjustment is feasible.
   SendMessage( hWndSB, SB_GETPARTS, 40, ( LPARAM ) ptArray );

   GetClientRect( hWndSB, &rect );

   // Calculates required adjustment based on window state and size.
   if( ( nrOfParts == 1 ) || ( IsZoomed( hWndParent ) || !( Style & WS_SIZEBOX ) ) )
   {
      nDev = rect.right - ptArray[nrOfParts - 1];
   }
   else
   {
      nDev = rect.right - ptArray[nrOfParts - 1] - rect.bottom - rect.top + 2;
   }

   if( rect.right > 0 )
   {
      for( n = 0; n <= nrOfParts - 1; n++ )
      {
         if( n == 0 )
         {
            if( size >= ptArray[n] && nDev < 0 )
            {
               s = FALSE;
            }
            else
            {
               if( ptArray[n] + nDev < size )
               {
                  nDev = size - ptArray[n];
               }

               ptArray[n] += nDev;     // Apply adjustment.
            }
         }
         else if( s )
         {
            ptArray[n] += nDev;        // Apply adjustment if s remains true.
         }
      }
   }

   SendMessage( hWndSB, SB_SETPARTS, nrOfParts, ( LPARAM ) ptArray );   // Update parts with new layout.
   hb_retni( nrOfParts );                          // Return the updated part count.
}

/*
 * FUNCTION: KEYTOGGLE
 *
 *  Description:
 *      Toggles the state of a specific key on the keyboard (e.g., Caps Lock, Num Lock).
 *      This function simulates pressing and releasing a key, effectively
 *      changing its state.
 *
 *  Parameters:
 *      1: Key code (WORD). The virtual key code of the key to toggle.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is useful for programmatically controlling keyboard
 *      settings, such as enabling or disabling Caps Lock or Num Lock.
 */
HB_FUNC( KEYTOGGLE )
{
   BYTE  pBuffer[256];                             // Buffer to hold the keyboard state for each key.
   WORD  wKey = hmg_par_WORD( 1 );                 // Key code to toggle, passed as a parameter.
   GetKeyboardState( pBuffer );                    // Retrieves the current state of each key.

   // Checks if the specified key is currently "on" (1) or "off" (0).
   if( pBuffer[wKey] & 0x01 )
   {
      pBuffer[wKey] &= 0xFE;                       // Turns the key "off" by clearing the least significant bit.
   }
   else
   {
      pBuffer[wKey] |= 0x01;                       // Turns the key "on" by setting the least significant bit.
   }

   SetKeyboardState( pBuffer );                    // Updates the keyboard state to reflect the toggle.
}

/*
 * FUNCTION: KEYTOGGLENT
 *
 *  Description:
 *      Simulates a key press and release for a specified key using keybd_event.
 *      This function provides a more direct way to simulate keyboard input.
 *
 *  Parameters:
 *      1: Key code (BYTE). The virtual key code of the key to simulate.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is useful for sending specific key presses to the
 *      operating system, which can be used to trigger actions in other
 *      applications or within the current application.
 */
HB_FUNC( KEYTOGGLENT )
{
   BYTE  wKey = hmg_par_BYTE( 1 );                 // Key code to simulate, passed as a parameter.

   // Simulates key press with extended key flag.
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY | 0, 0 );

   // Simulates key release with key-up flag.
   keybd_event( wKey, 0x45, KEYEVENTF_EXTENDEDKEY | KEYEVENTF_KEYUP, 0 );
}

/*
 * FUNCTION: SETSTATUSITEMICON
 *
 *  Description:
 *      Sets an icon on a specific item in the status bar.
 *      This function allows you to associate an icon with a particular
 *      section of the status bar, providing a visual indicator of the
 *      status or function of that section.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to modify.
 *      2: Item position (numeric). The index of the section to set the icon for (1-based).
 *      3: Icon name (character). The name of the icon resource or the path to an icon file.
 *      4: Icon handle (HICON).  Optional. If provided, uses this handle directly instead of loading by name.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is useful for visually enhancing the status bar and
 *      providing users with a clear understanding of the current state of
 *      different aspects of the application.
 *      For example, it can be used to display an icon indicating the
 *      status of a network connection, the battery level, or the presence
 *      of new messages.
 *
 *  Notes:
 *      The function attempts to load the icon from resources first, and if
 *      that fails, it attempts to load it from a file.
 *      If an icon handle is provided in parameter 4, it is used directly,
 *      bypassing the loading process.
 */
HB_FUNC( SETSTATUSITEMICON )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   int      nPart = hb_parni( 2 ) - 1;             /* zero-based part index */
   HICON    hIcon = HB_ISNUM( 4 ) ? hmg_par_raw_HICON( 4 ) : NULL;

#ifndef UNICODE
   LPCSTR   lpIconName = hb_parc( 3 );
#else
   LPWSTR   lpIconName = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   HICON    hOldIcon = ( HICON ) SendMessage( hwnd, SB_GETICON, nPart, ( LPARAM ) 0 );

   // Removes the current icon from the specified status bar item.
   if( hOldIcon )
   {
      DestroyIcon( hOldIcon );
   }

   /* if caller passed an HICON handle in parameter 4, use it */
   if( !hIcon && lpIconName && *lpIconName )
   {
      /* load icon from resources then fallback to file */
      RECT  rc;
      int   cx, cy;                                // Dimensions for the icon.
      GetClientRect( hwnd, &rc );

      cy = ( rc.bottom - rc.top ) - 4;
      cx = cy;

      hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR );
      if( !hIcon )
      {
         hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
      }
   }

   if( hIcon )
   {
      /* set icon for the specified part (zero-based index) */
      SendMessage( hwnd, SB_SETICON, nPart, ( LPARAM ) hIcon );
   }

#ifdef UNICODE
   hb_xfree( lpIconName );
#endif
}

/*
 * FUNCTION: SETSTATUSBARSIZE
 *
 *  Description:
 *      Configures the width of each part/section in the status bar.
 *      This function allows you to define the size of each section in the
 *      status bar, providing precise control over the layout of the status
 *      bar elements.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to configure.
 *      2: Array of widths (array). An array containing the width of each section in the status bar.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is useful for creating a status bar with sections of
 *      varying sizes, allowing you to optimize the display of different
 *      types of information.
 */
HB_FUNC( SETSTATUSBARSIZE )
{
   HWND     hwndStatus = hmg_par_raw_HWND( 1 );    // Status bar handle.
   int      nParts = ( int ) hb_parinfa( 2, 0 );   // Number of parts defined in the array.

   // Allocates memory to hold the widths of each status bar part.
   HLOCAL   hloc = LocalAlloc( LHND, sizeof( int ) * nParts );
   LPINT    lpParts = LocalLock( hloc );

   int      nWidth = 0;                // Cumulative width for each part.
   int      i;

   // Assigns each part width based on the provided array.
   for( i = 0; i < nParts; i++ )
   {
      nWidth += HB_PARNI( 2, i + 1 );  // Adds width for current part.
      lpParts[i] = nWidth;
   }

   SendMessage( hwndStatus, SB_SETPARTS, nParts, ( LPARAM ) lpParts );  // Sets the parts on the status bar.
   MoveWindow( hwndStatus, 0, 0, 0, 0, TRUE );  // Redraws the window to apply changes.
   LocalUnlock( hloc ); // Releases the allocated memory.
   LocalFree( hloc );
}

/*
 * FUNCTION: REFRESHPROGRESSITEM
 *
 *  Description:
 *      Updates the position of a progress bar in a status bar item to match the item's current position.
 *      This function ensures that the progress bar remains correctly aligned
 *      within its designated section of the status bar, even when the status
 *      bar is resized or repositioned.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar containing the progress bar.
 *      2: Item position (numeric). The index of the section containing the progress bar (1-based).
 *      3: Progress bar window handle (HWND). The handle of the progress bar control.
 *
 *  Returns:
 *      None.
 *
 *  Purpose:
 *      This function is essential for maintaining the correct visual
 *      relationship between the progress bar and its containing status bar
 *      section, ensuring a consistent and user-friendly display.
 *      It is typically called after the status bar has been resized or
 *      repositioned, or when the progress bar's position needs to be
 *      updated for any other reason.
 */
HB_FUNC( REFRESHPROGRESSITEM )
{
   HWND  hwndStatus = hmg_par_raw_HWND( 1 ); // Handle to the status bar.
   RECT  rc;

   // Gets the rectangle for the specified status bar item.
   SendMessage( hwndStatus, SB_GETRECT, hb_parni( 2 ) - 1, ( LPARAM ) & rc );

   // Adjusts the position of the progress bar within the item.
   SetWindowPos( hmg_par_raw_HWND( 3 ), 0, rc.left, rc.top, 0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE );
}

/*
 * FUNCTION: CREATEPROGRESSBARITEM
 *
 *  Description:
 *      Creates a progress bar control within a specified status bar item.
 *      This function dynamically creates a progress bar control and places it
 *      within a designated section of the status bar, allowing you to visually
 *      indicate the progress of a task.
 *
 *  Parameters:
 *      1: Status bar window handle (HWND). The handle of the status bar to add the progress bar to.
 *      2: Item position (numeric). The index of the section to place the progress bar in (1-based).
 *      3: Visible (numeric).  1 to make the progress bar visible, 0 to hide it.
 *      4: Minimum range (numeric). The minimum value for the progress bar range.
 *      5: Maximum range (numeric). The maximum value for the progress bar range.
 *
 *  Returns:
 *      HWND: The handle to the created progress bar window.  Returns NULL if the
 *            progress bar creation fails.
 *
 *  Purpose:
 *      This function is useful for providing users with a visual representation
 *      of the progress of long-running tasks, improving the user experience.
 */
HB_FUNC( CREATEPROGRESSBARITEM )
{
   HWND  hwndStatus = hmg_par_raw_HWND( 1 ); // Handle to the status bar.
   HWND  hwndProgressBar;
   RECT  rc;
   DWORD Style = WS_CHILD | PBS_SMOOTH;      // Style for a smooth child progress bar.

   // Retrieves the rectangle for the specified item in the status bar.
   SendMessage( hwndStatus, SB_GETRECT, hb_parni( 2 ) - 1, ( LPARAM ) & rc );
   if( hb_parni( 3 ) )                 // If visible flag is set, add WS_VISIBLE style.
   {
      Style |= WS_VISIBLE;
   }

   // Creates the progress bar control within the item rectangle.
   if
   (
      (
         hwndProgressBar = CreateWindowEx
            (
               0,                      // No extended styles.
               PROGRESS_CLASS,         // Class name for progress bars.
               ( LPCTSTR ) NULL,       // No title text.
               Style,                  // Defined style.
               rc.left,                // Left position.
               rc.top,                 // Top position.
               rc.right - rc.left,     // Width.
               rc.bottom - rc.top - 1, // Height.
               hwndStatus,             // Parent window is the status bar.
               ( HMENU ) NULL,         // No menu.
               GetInstance(),          // Instance handle.
               ( LPVOID ) NULL         // No additional data.
            )
      ) != NULL
   )
   {
      // Sets the range and initial position of the progress bar.
      SendMessage( hwndProgressBar, PBM_SETRANGE, 0, MAKELONG( hb_parni( 4 ), hb_parni( 5 ) ) );
      SendMessage( hwndProgressBar, PBM_SETPOS, hb_parni( 3 ), 0 );
      hmg_ret_raw_HWND( hwndProgressBar );   // Returns the handle to the new progress bar.
   }
   else
   {
      hb_ret();   // Returns NULL if creation failed.
   }
}

/*
 * FUNCTION: SETPOSPROGRESSBARITEM
 *
 *  Description:
 *      Sets the position and visibility of a progress bar in a status bar item.
 *      This function allows you to control the current position of the
 *      progress bar, as well as its visibility, providing dynamic feedback
 *
 *  Purpose:
 *      This function provides a way to dynamically update the progress bar
 *      within a status bar item.  It's used to visually represent the
 *      progress of a long-running operation to the user.  The visibility
 *      control allows the progress bar to be shown only when needed,
 *      avoiding unnecessary screen clutter.
 */
HB_FUNC( SETPOSPROGRESSBARITEM )
{
   HWND  hwndProgressBar = hmg_par_raw_HWND( 1 );  // Handle to the progress bar.
   int   nPos = hb_parni( 2 );

   // Shows or hides the progress bar based on nPos (0 to hide, non-0 to show).
   ShowWindow( hwndProgressBar, nPos ? SW_SHOW : SW_HIDE );

   // Sets the current position of the progress bar.
   SendMessage( hwndProgressBar, PBM_SETPOS, nPos, 0 );
}
