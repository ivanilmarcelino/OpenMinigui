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

// Define the Windows Internet Explorer version for compatibility with control features.
#define _WIN32_IE 0x0501

#include <mgdefs.h>     // MiniGUI definitions.
#include <commctrl.h>   // Common control library for Windows.
extern BOOL Array2Point( PHB_ITEM aPoint, POINT *pt );   // External function to convert array to POINT.

// Loads the first image in an image list with specified properties.
HIMAGELIST  HMG_ImageListLoadFirst( const char *FileName, int cGrow, int Transparent, int *nWidth, int *nHeight );

// Adds an image to an existing image list with transparency options.
void        HMG_ImageListAdd( HIMAGELIST himl, char *FileName, int Transparent );

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );   // Converts ANSI string to wide string in Unicode environments.
#endif
HINSTANCE   GetInstance( void );    // Retrieves the current application instance.
HINSTANCE   GetResources( void );   // Retrieves application resource instance.

// MiniGUI resource management function to register resource with a given type.
void        RegisterResource( HANDLE hResource, LPCSTR szType );

/*
 * HB_FUNC( INITTABCONTROL )
 *
 * Initializes a tab control window with specified properties and tab captions.
 *
 * Parameters:
 *   - 1: Parent window handle (HWND).
 *   - 2: Tab control ID (HMENU).
 *   - 3: X position of the tab control.
 *   - 4: Y position of the tab control.
 *   - 5: Width of the tab control.
 *   - 6: Height of the tab control.
 *   - 7: Array containing tab captions (PHB_ITEM).
 *   - 8: Initial selected tab index (1-based).
 *   - 9: Reserved (unused).
 *   - 10: Reserved (unused).
 *   - 11: Optional: TRUE to enable TCS_BUTTONS style.
 *   - 12: Optional: TRUE to enable TCS_FLATBUTTONS style.
 *   - 13: Optional: TRUE to enable TCS_HOTTRACK style.
 *   - 14: Optional: TRUE to enable TCS_VERTICAL style.
 *   - 15: Optional: TRUE to enable TCS_BOTTOM style.
 *   - 16: Optional: TRUE to enable TCS_MULTILINE style.
 *   - 17: Optional: TRUE to enable TCS_OWNERDRAWFIXED style.
 *   - 18: Optional: FALSE to disable WS_TABSTOP style.
 *
 * Returns:
 *   - Handle to the created tab control window (HWND).
 *
 * This function creates a tab control, sets its styles based on the provided parameters,
 * and populates it with tab items from the given array of captions. It handles both
 * ANSI and Unicode environments by converting the captions appropriately.
 */
HB_FUNC( INITTABCONTROL )
{
   PHB_ITEM hArray;                 // Array to hold tab captions.
   HWND     hbutton;                // Handle for the tab control.
   TC_ITEM  tie;                    // Tab control item structure.
   int      l;       // Number of tabs.
   int      i;       // Loop index for tab items.
#ifndef UNICODE
   LPSTR    lpText;  // Text buffer for ANSI mode.
#else
   LPWSTR   lpText;  // Text buffer for Unicode mode.
#endif
   DWORD    Style = WS_CHILD | WS_VISIBLE | TCS_TOOLTIPS;   // Initial tab control style with tooltips.

   // Apply additional styles based on optional parameters.
   if( hb_parl( 11 ) )
   {
      Style |= TCS_BUTTONS;
   }

   if( hb_parl( 12 ) )
   {
      Style |= TCS_FLATBUTTONS;
   }

   if( hb_parl( 13 ) )
   {
      Style |= TCS_HOTTRACK;
   }

   if( hb_parl( 14 ) )
   {
      Style |= TCS_VERTICAL;
   }

   if( hb_parl( 15 ) )
   {
      Style |= TCS_BOTTOM;
   }

   if( hb_parl( 16 ) )
   {
      Style |= TCS_MULTILINE;
   }

   if( hb_parl( 17 ) )
   {
      Style |= TCS_OWNERDRAWFIXED;
   }

   if( !hb_parl( 18 ) )
   {
      Style |= WS_TABSTOP;
   }

   l = ( int ) hb_parinfa( 7, 0 ) - 1;    // Get tab count from input array.
   hArray = hb_param( 7, HB_IT_ARRAY );   // Retrieve tab captions array.

   // Create tab control window with specified styles and position.
   hbutton = CreateWindow
      (
         WC_TABCONTROL,          // Class name for the tab control.
         NULL,                   // No initial text.
         Style,                  // Combined styles.
         hb_parni( 3 ),          // X position.
         hb_parni( 4 ),          // Y position.
         hb_parni( 5 ),          // Width.
         hb_parni( 6 ),          // Height.
         hmg_par_raw_HWND( 1 ),  // Parent window handle.
         hmg_par_raw_HMENU( 2 ), // Tab control ID.
         GetInstance(),          // Application instance handle.
         NULL                    // No additional creation parameters.
      );

   tie.mask = TCIF_TEXT;   // Only text is used in each tab item.
   tie.iImage = -1;        // No images for tab items by default.

   // Loop through each tab item in reverse to insert into tab control.
   for( i = l; i >= 0; i = i - 1 )
   {
#ifndef UNICODE
      lpText = ( char * ) hb_arrayGetCPtr( hArray, i + 1 );                // Get text for each tab (ANSI).
#else
      lpText = AnsiToWide( ( char * ) hb_arrayGetCPtr( hArray, i + 1 ) );  // Convert text to wide (Unicode).
#endif
      tie.pszText = lpText;

      TabCtrl_InsertItem( hbutton, 0, &tie );         // Insert tab item at position 0.
#ifdef UNICODE
      hb_xfree( ( TCHAR * ) lpText );                 // Free allocated memory for Unicode strings.
#endif
   }

   TabCtrl_SetCurSel( hbutton, hb_parni( 8 ) - 1 );   // Set the initial selected tab.

   hmg_ret_raw_HWND( hbutton );              // Return the tab control handle.
}

/*
 * HB_FUNC( TABCTRL_SETCURSEL )
 *
 * Sets the currently selected tab in a tab control.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *   - 2: Tab index to select (1-based).
 *
 * Returns:
 *   - The index of the previously selected tab, or -1 if there was no previously selected tab.
 *
 * This function allows you to programmatically change the active tab in a tab control.
 */
HB_FUNC( TABCTRL_SETCURSEL )
{
   hmg_ret_NINT( TabCtrl_SetCurSel( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1 ) );
}

/*
 * HB_FUNC( TABCTRL_GETCURSEL )
 *
 * Retrieves the index of the currently selected tab in a tab control.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *
 * Returns:
 *   - The index of the currently selected tab (1-based). Returns 1 if no tab is selected.
 *
 * This function is useful for determining which tab is currently active in the tab control.
 */
HB_FUNC( TABCTRL_GETCURSEL )
{
   hmg_ret_NINT( TabCtrl_GetCurSel( hmg_par_raw_HWND( 1 ) ) + 1 );
}

/*
 * HB_FUNC( TABCTRL_INSERTITEM )
 *
 * Inserts a new tab item into a tab control at a specified position.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *   - 2: Position to insert the new tab item (0-based).
 *   - 3: Text of the new tab item (LPSTR).
 *
 * Returns:
 *   - None.
 *
 * This function adds a new tab to the tab control with the specified text.
 * The new tab is inserted at the given position, shifting existing tabs if necessary.
 */
HB_FUNC( TABCTRL_INSERTITEM )
{
   TC_ITEM  tie;                             // Tab item structure.
#ifndef UNICODE
   LPSTR    lpText = ( LPSTR ) hb_parc( 3 ); // ANSI text parameter.
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 3 ) );          // Unicode conversion for text.
#endif
   tie.mask = TCIF_TEXT;
   tie.iImage = -1;
   tie.pszText = lpText;

   TabCtrl_InsertItem( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), &tie ); // Insert item.

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );           // Free memory for Unicode strings.
#endif
}

/*
 * HB_FUNC( TABCTRL_DELETEITEM )
 *
 * Deletes a tab item from a tab control at a specified position.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *   - 2: Tab index to delete (0-based).
 *
 * Returns:
 *   - None.
 *
 * This function removes a tab from the tab control at the given position.
 * Subsequent tabs are shifted to fill the gap.
 */
HB_FUNC( TABCTRL_DELETEITEM )
{
   TabCtrl_DeleteItem( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) );
}

/*
 * HB_FUNC( SETTABCAPTION )
 *
 * Sets or updates the caption (text) of a specific tab in a tab control.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *   - 2: Tab index (1-based).
 *   - 3: New text for the tab caption (LPSTR).
 *
 * Returns:
 *   - None.
 *
 * This function allows you to change the text displayed on a tab in the tab control.
 */
HB_FUNC( SETTABCAPTION )
{
#ifndef UNICODE
   LPSTR    lpText = ( LPSTR ) hb_parc( 3 ); // ANSI text parameter.
#else
   LPWSTR   lpText = AnsiToWide( ( char * ) hb_parc( 3 ) );             // Unicode conversion for text.
#endif
   TC_ITEM  tie;

   tie.mask = TCIF_TEXT;
   tie.pszText = lpText;

   TabCtrl_SetItem( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1, &tie );   // Set tab caption.

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpText );              // Free memory for Unicode strings.
#endif
}

/*
 * HB_FUNC( ADDTABBITMAP )
 *
 * Adds images to the tab control for each tab, using an image list.
 *
 * Parameters:
 *   - 1: Tab control handle (HWND).
 *   - 2: Array of image filenames (PHB_ITEM).
 *   - 3: Transparency setting (TRUE for transparent, FALSE for opaque).
 *   - 4: Optional: Image width. If not provided, it will be determined from the first image.
 *   - 5: Optional: Image height. If not provided, it will be determined from the first image.
 *
 * Returns:
 *   - Handle to the created image list (HIMAGELIST).
 *
 * This function loads images from the specified files, creates an image list,
 * associates the image list with the tab control, and assigns an image to each tab.
 */
HB_FUNC( ADDTABBITMAP )
{
   HWND        hbutton = hmg_par_raw_HWND( 1 ); // Tab control handle.
   TC_ITEM     tie;  // Tab item structure for image.
   HIMAGELIST  himl = ( HIMAGELIST ) NULL;   // Image list handle.
   PHB_ITEM    hArray;                    // Array of image filenames.
   char        *FileName;
   int         nCount, i;
   int         cx = HB_ISNUM( 4 ) ? hb_parni( 4 ) : -1;
   int         cy = HB_ISNUM( 5 ) ? hb_parni( 5 ) : -1;

   nCount = ( int ) hb_parinfa( 2, 0 );   // Number of images in the array.
   if( nCount > 0 )  // Check if there are images to add.
   {
      int   Transparent = hb_parl( 3 ) ? 0 : 1; // Determine transparency based on input.
      hArray = hb_param( 2, HB_IT_ARRAY );      // Retrieve image filename array.

      // Loop through each image filename in the array.
      for( i = 1; i <= nCount; i++ )
      {
         FileName = ( char * ) hb_arrayGetCPtr( hArray, i );

         if( himl == NULL )
         {
            himl = HMG_ImageListLoadFirst( FileName, nCount, Transparent, &cx, &cy );  // Load first image.
         }
         else
         {
            HMG_ImageListAdd( himl, FileName, Transparent );   // Add subsequent images.
         }
      }

      if( himl != NULL )
      {
         SendMessage( hbutton, TCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) himl );   // Set image list for tab control.
         RegisterResource( himl, "IMAGELIST" );          // Register the image list resource.
      }

      for( i = 0; i < nCount; i++ )
      {
         tie.mask = TCIF_IMAGE;                          // Set image flag.
         tie.iImage = i;
         TabCtrl_SetItem( ( HWND ) hbutton, i, &tie );   // Assign images to each tab.
      }
   }

   hmg_ret_raw_HANDLE( himl );   // Return the image list handle.
}

/*
 * HB_FUNC( WINDOWFROMPOINT )
 *
 * Retrieves the window handle at a specified screen point.
 *
 * Parameters:
 *   - 1: Array containing X and Y coordinates of the screen point (PHB_ITEM).
 *
 * Returns:
 *   - Handle to the window at the specified point (HWND).
 *
 * This function is useful for determining which window is located at a particular
 * screen coordinate.
 */
HB_FUNC( WINDOWFROMPOINT )
{
   POINT Point;

   Array2Point( hb_param( 1, HB_IT_ARRAY ), &Point ); // Convert array to POINT structure.
   hmg_ret_raw_HWND( WindowFromPoint( Point ) );      // Retrieve window handle at specified point.
}

/*
 * HB_FUNC( GETMESSAGEPOS )
 *
 * Gets the cursor position for the last message retrieved by the calling thread.
 *
 * Parameters:
 *   - None.
 *
 * Returns:
 *   - A DWORD value containing the X and Y coordinates of the cursor. The X coordinate is the low-order word,
 *     and the Y coordinate is the high-order word.
 *
 * This function is useful for determining the cursor position when handling window messages.
 */
HB_FUNC( GETMESSAGEPOS )
{
   hmg_ret_DWORD( GetMessagePos() );
}
