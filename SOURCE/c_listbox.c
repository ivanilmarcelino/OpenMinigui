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
#include <mgdefs.h>                             // Include for Harbour GUI definitions
#include <windowsx.h>                           // Include for extended Windows API functions
#include <commctrl.h>                           // Include for common controls like list boxes and buttons

// Compatibility macro for Borland C++ 5.5 compiler
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
// Defining listbox class name for compatibility
#define WC_LISTBOX   "ListBox"
#endif
#define TOTAL_TABS   10                         // Define maximum number of tab stops in list box controls

// Function prototypes for string conversion (ANSI <-> Unicode) when compiling in Unicode mode
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
LPSTR       WideToAnsi( LPWSTR );
#endif

// Function prototype to retrieve the instance handle of the application
HINSTANCE   GetInstance( void );

// Function to initialize a list box control with various optional styles
HB_FUNC( INITLISTBOX )
{
   HWND  hbutton;
   DWORD Style = WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_NOINTEGRALHEIGHT;

   // Conditional styles based on passed parameters (visibility, tab stop, sorting, tab stops, multi-column)
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_TABSTOP;
   }

   if( hb_parl( 11 ) )
   {
      Style |= LBS_SORT;
   }

   if( hb_parl( 13 ) )
   {
      Style |= LBS_USETABSTOPS;
   }

   if( hb_parl( 14 ) )
   {
      Style |= LBS_MULTICOLUMN | WS_HSCROLL;
   }

   // Create the list box control with the specified styles and dimensions
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,                      // Extended style with a client edge
         WC_LISTBOX,                            // Window class name for list boxes
         TEXT( "" ),                            // No text
         Style,                                 // Combined style
         hb_parni( 3 ),                         // X position
         hb_parni( 4 ),                         // Y position
         hb_parni( 5 ),                         // Width
         hb_parni( 6 ),                         // Height
         hmg_par_raw_HWND( 1 ),                 // Parent window handle
         hmg_par_raw_HMENU( 2 ),                // Menu handle (ID of the control)
         GetInstance(),                         // Application instance handle
         NULL                                   // No additional data
      );

   // Enable drag-and-drop if specified
   if( hb_parl( 12 ) )
   {
      MakeDragList( hbutton );
   }

   // Set column width for multi-column list box
   if( hb_parl( 14 ) )
   {
      SendMessage( hbutton, LB_SETCOLUMNWIDTH, ( WPARAM ) hb_parni( 5 ) - 20, 0 );
   }

   // Return the handle to the created list box control
   hmg_ret_raw_HWND( hbutton );
}

// Function to add a string to a list box
HB_FUNC( LISTBOXADDSTRING )
{
#ifndef UNICODE
   LPTSTR   lpString = ( LPTSTR ) hb_parc( 2 ); // Direct pointer for ANSI mode
#else
   LPWSTR   lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );  // Convert to Unicode for Unicode mode
#endif

   // Send message to add string to list box
   SendMessage( hmg_par_raw_HWND( 1 ), LB_ADDSTRING, 0, ( LPARAM ) lpString );

#ifdef UNICODE
   hb_xfree( lpString );   // Free allocated memory in Unicode mode
#endif
}

// Function to insert a string at a specific position in a list box
HB_FUNC( LISTBOXINSERTSTRING )
{
#ifndef UNICODE
   LPTSTR   lpString = ( LPTSTR ) hb_parc( 2 );                // Direct pointer for ANSI mode
#else
   LPWSTR   lpString = AnsiToWide( ( char * ) hb_parc( 2 ) );  // Convert to Unicode for Unicode mode
#endif

   // Send message to insert string at specified index
   SendMessage( hmg_par_raw_HWND( 1 ), LB_INSERTSTRING, ( WPARAM ) hb_parni( 3 ) - 1, ( LPARAM ) lpString );

#ifdef UNICODE
   hb_xfree( lpString );                  // Free allocated memory in Unicode mode
#endif
}

/* Modified by P.Ch. 16.10. */

// Function to get a string from a specific position in a list box
HB_FUNC( LISTBOXGETSTRING )
{
#ifdef UNICODE
   LPSTR lpString;                        // Buffer for converted string in Unicode mode
#endif
   int   iLen = ( int ) SendMessage( hmg_par_raw_HWND( 1 ), LB_GETTEXTLEN, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) 0 );
   TCHAR *cString;

   // Allocate memory for string if valid length
   if( iLen > 0 && NULL != ( cString = ( TCHAR * ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) ) ) )
   {
      SendMessage( hmg_par_raw_HWND( 1 ), LB_GETTEXT, ( WPARAM ) hb_parni( 2 ) - 1, ( LPARAM ) cString );
#ifdef UNICODE
      lpString = WideToAnsi( cString );   // Convert to ANSI
      hb_retc( lpString );                // Return string
      hb_xfree( lpString );               // Free converted string
#else
      hb_retclen_buffer( cString, iLen ); // Direct return in ANSI mode
#endif
   }
   else
   {
      hb_retc_null();                     // Return NULL if invalid length
   }
}

// Function to initialize a multi-selection list box
HB_FUNC( INITMULTILISTBOX )
{
   HWND  hbutton;
   DWORD Style = LBS_EXTENDEDSEL | WS_CHILD | WS_VSCROLL | LBS_DISABLENOSCROLL | LBS_NOTIFY | LBS_MULTIPLESEL | LBS_NOINTEGRALHEIGHT;

   // Conditional styles based on visibility and tab stop parameters
   if( !hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_TABSTOP;
   }

   if( hb_parl( 11 ) )
   {
      Style |= LBS_SORT;
   }

   if( hb_parl( 13 ) )
   {
      Style |= LBS_USETABSTOPS;
   }

   if( hb_parl( 14 ) )
   {
      Style |= LBS_MULTICOLUMN;
   }

   // Create the list box control with the specified styles and dimensions
   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,                // Extended style
         WC_LISTBOX,                   // List box class name
         TEXT( "" ),                   // No initial text
         Style,                        // Combined style
         hb_parni( 3 ),                // X position
         hb_parni( 4 ),                // Y position
         hb_parni( 5 ),                // Width
         hb_parni( 6 ),                // Height
         hmg_par_raw_HWND( 1 ),        // Parent window handle
         hmg_par_raw_HMENU( 2 ),       // Menu handle (ID of the control)
         GetInstance(),                // Application instance handle
         NULL                          // No additional data
      );

   // Enable drag-and-drop if specified
   if( hb_parl( 12 ) )
   {
      MakeDragList( hbutton );
   }

   hmg_ret_raw_HWND( hbutton );        // Return the handle of created list box
}

// Function to get multiple selections from a list box
HB_FUNC( LISTBOXGETMULTISEL )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 ); // List box handle
   int   i;
   int   buffer[32768];                // Buffer for selected item indexes
   int   n;

   n = ( int ) SendMessage( hwnd, LB_GETSELCOUNT, 0, 0 );                  // Get count of selected items
   SendMessage( hwnd, LB_GETSELITEMS, ( WPARAM ) n, ( LPARAM ) buffer );   // Retrieve selected items

   hb_reta( n );  // Return an array of selected items
   for( i = 0; i < n; i++ )
   {
      HB_STORNI( buffer[i] + 1, -1, i + 1 );             // Store each item index in the return array
   }
}

// Function to set multiple selections in a list box
HB_FUNC( LISTBOXSETMULTISEL )
{
   PHB_ITEM wArray;
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   int      i, n, l;

   wArray = hb_param( 2, HB_IT_ARRAY );                  // Get array of items to select
   l = ( int ) hb_parinfa( 2, 0 ) - 1;                   // Length of the array
   n = ( int ) SendMessage( hwnd, LB_GETCOUNT, 0, 0 );   // Get count of items in list box

   // Clear current selections
   for( i = 0; i < n; i++ )
   {
      SendMessage( hwnd, LB_SETSEL, ( WPARAM ) 0, ( LPARAM ) i );
   }

   // Set new selections from array
   for( i = 0; i <= l; i++ )
   {
      SendMessage( hwnd, LB_SETSEL, ( WPARAM ) 1, ( LPARAM ) hb_arrayGetNI( wArray, i + 1 ) - 1 );
   }
}

// Function to set tab stops for a multi-tab list box
HB_FUNC( LISTBOXSETMULTITAB )
{
   PHB_ITEM wArray;
   int      nTabStops[TOTAL_TABS];
   int      l, i;
   DWORD    dwDlgBase = GetDialogBaseUnits();
   int      baseunitX = LOWORD( dwDlgBase );

   wArray = hb_param( 2, HB_IT_ARRAY );
   l = ( int ) hb_parinfa( 2, 0 ) - 1;

   // Set each tab stop from array, converted based on dialog base units
   for( i = 0; i <= l; i++ )
   {
      nTabStops[i] = MulDiv( hb_arrayGetNI( wArray, i + 1 ), 4, baseunitX );
   }

   hb_retl( ListBox_SetTabStops( hmg_par_raw_HWND( 1 ), l, nTabStops ) );  // Apply tab stops
}

// Register a unique message identifier for drag-and-drop operations
HB_FUNC( _GETDDLMESSAGE )
{
   UINT  g_dDLMessage;
   g_dDLMessage = RegisterWindowMessage( DRAGLISTMSGSTRING );

   hmg_ret_UINT( g_dDLMessage );          // Return the registered message identifier
}

// Get the notification code for a drag-and-drop operation in list box
HB_FUNC( GET_DRAG_LIST_NOTIFICATION_CODE )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   hmg_ret_UINT( lpdli->uNotification );  // Return the notification code
}

// Get the index of the item being dragged in list box
HB_FUNC( GET_DRAG_LIST_DRAGITEM )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   hmg_ret_NINT( LBItemFromPt( lpdli->hWnd, lpdli->ptCursor, TRUE ) );     // Return item index under cursor
}

// Draw insertion indicator for drag-and-drop operation in list box
HB_FUNC( DRAG_LIST_DRAWINSERT )
{
   HWND           hwnd = hmg_par_raw_HWND( 1 );
   LPARAM         lParam = hmg_par_raw_LPARAM( 2 );
   int            nItem = hb_parni( 3 );
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   int            nItemCount = ( int ) SendMessage( ( HWND ) lpdli->hWnd, LB_GETCOUNT, 0, 0 );

   DrawInsert( hwnd, lpdli->hWnd, ( nItem < nItemCount ) ? nItem : -1 );   // Draw insert marker at position
}

// Function to move items within a list box during a drag-and-drop operation
HB_FUNC( DRAG_LIST_MOVE_ITEMS )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPDRAGLISTINFO lpdli = ( LPDRAGLISTINFO ) lParam;

   char           string[1024];
   int            result;

   // Get the text of the item to move
   result = ListBox_GetText( lpdli->hWnd, hb_parni( 2 ), string );
   if( result != LB_ERR )
   {
      result = ListBox_DeleteString( lpdli->hWnd, hb_parni( 2 ) );         // Delete the original item
   }

   // Insert the item at the new position
   if( result != LB_ERR )
   {
      result = ListBox_InsertString( lpdli->hWnd, hb_parni( 3 ), string );
   }

   // Set the inserted item as the selected item
   if( result != LB_ERR )
   {
      result = ListBox_SetCurSel( lpdli->hWnd, hb_parni( 3 ) );
   }

   hmg_ret_L( result != LB_ERR );   // Return success status of operation
}
