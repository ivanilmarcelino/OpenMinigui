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
#include <commctrl.h>

// Edit Class Name for compatibility with older Borland compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_EDIT   "Edit"
#endif

extern LRESULT CALLBACK OwnEditProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

HINSTANCE               GetInstance( void );

/*
 * FUNCTION INITEDITBOX( hwnd, hmenu, x, y, width, height, placeholder, edgeStyle, maxChars, readOnly, visible, tabStop, vScroll, hScroll )
 *
 * Creates and initializes an edit box control with specified styles and properties.
 *
 * Parameters:
 *   hwnd      : HWND - Handle of the parent window for the edit box.
 *   hmenu     : HMENU - Menu handle or unique control ID for the edit box.  This is used to identify the control.
 *   x         : NUMERIC - X-coordinate of the top-left corner of the edit box, relative to the parent window.
 *   y         : NUMERIC - Y-coordinate of the top-left corner of the edit box, relative to the parent window.
 *   width     : NUMERIC - Width of the edit box in pixels.
 *   height    : NUMERIC - Height of the edit box in pixels.
 *   placeholder: CHARACTER - Placeholder text (NOT USED in this implementation).
 *   edgeStyle : LOGICAL - Determines whether the edit box has a client edge (sunken border). .T. for no edge, .F. for edge.
 *   maxChars  : NUMERIC - Maximum number of characters that can be entered into the edit box.
 *   readOnly  : LOGICAL - Determines whether the edit box is read-only. .T. for read-only, .F. for editable.
 *   visible   : LOGICAL - Determines whether the edit box is initially visible. .T. for visible, .F. for hidden.
 *   tabStop   : LOGICAL - Determines whether the edit box can be reached using the Tab key. .T. for tab stop, .F. for no tab stop.
 *   vScroll   : LOGICAL - Determines whether vertical scrolling is enabled. .T. for no vertical scroll, .F. for vertical scroll.
 *   hScroll   : LOGICAL - Determines whether horizontal scrolling is enabled. .T. for no horizontal scroll, .F. for horizontal scroll.
 *
 * Returns:
 *   HWND - Handle to the newly created edit box control.  This handle is used to interact with the control later.
 *
 * Purpose:
 *   This function provides a convenient way to create and configure an edit box control within a Harbour MiniGUI application.
 *   It encapsulates the Windows API calls required to create the control and set its initial properties, simplifying the process
 *   for the developer.  It allows for customization of the edit box's appearance and behavior through various parameters.
 *   For example, it can be used to create a multi-line text editor with scrollbars, or a single-line input field with a character limit.
 *
 * Notes:
 *   The function subclasses the edit box's window procedure with a custom procedure (OwnEditProc) to handle specific events.
 */
HB_FUNC( INITEDITBOX )
{
   HWND  hbutton;
   DWORD Style = ES_MULTILINE | ES_WANTRETURN | WS_CHILD;   // Default styles: multi-line, allow return key, child window
   DWORD ExStyle = hb_parl( 8 ) ? 0 : WS_EX_CLIENTEDGE;     // Client edge based on edgeStyle parameter

   if( hb_parl( 10 ) )
   {
      Style |= ES_READONLY;      // Set read-only style if specified
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;       // Set visibility style if specified
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;       // Add to tab order if specified
   }

   if( !hb_parl( 13 ) )
   {
      Style |= WS_VSCROLL;       // Enable vertical scroll bar
   }
   else
   {
      Style |= ES_AUTOVSCROLL;   // Enable auto vertical scrolling for long text
   }

   if( !hb_parl( 14 ) )
   {
      Style |= WS_HSCROLL;       // Enable horizontal scroll bar
   }

   // Create the edit box with the specified styles and parent/owner window
   hbutton = CreateWindowEx
      (
         ExStyle,
         WC_EDIT,
         TEXT( "" ),
         Style,
         hb_parni( 3 ),          // x-coordinate
         hb_parni( 4 ),          // y-coordinate
         hb_parni( 5 ),          // width
         hb_parni( 6 ),          // height
         hmg_par_raw_HWND( 1 ),  // parent window handle
         hmg_par_raw_HMENU( 2 ), // menu handle/control ID
         GetInstance(),
         NULL
      );

   // Set the maximum number of characters allowed in the edit box
   SendMessage( hbutton, EM_LIMITTEXT, hmg_par_WPARAM( 9 ), ( LPARAM ) 0 );

   // Set up a custom window procedure for the edit box
   SetProp( hbutton, TEXT( "oldeditproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hbutton, GWLP_WNDPROC ) );
   SubclassWindow2( hbutton, OwnEditProc );

   // Return the handle to the edit box control
   hmg_ret_raw_HWND( hbutton );
}
