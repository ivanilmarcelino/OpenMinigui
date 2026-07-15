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
#include <mgdefs.h>
#include <commctrl.h>

/* 
   Compatibility fix: Older Borland compilers may not have WC_EDIT defined 
   in their headers. We manually define it as the standard Windows "Edit" class.
*/
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_EDIT   "Edit"
#endif

// External reference to the custom window procedure used for EditBox subclassing.
extern LRESULT CALLBACK OwnEditProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

// Helper to retrieve the current application instance handle.
HINSTANCE               GetInstance( void );

/*
 * HB_FUNC( INITEDITBOX )
 * ----------------------
 * Purpose:
 *    Internal HMG function to create and initialize a Win32 Edit control 
 *    configured as a multi-line EditBox.
 *
 * Parameters (from Harbour stack):
 *    1: HWND    - Parent window handle.
 *    2: HMENU   - Control ID (passed as a menu handle for child windows).
 *    3: nX      - Horizontal position (pixels).
 *    4: nY      - Vertical position (pixels).
 *    5: nWidth  - Control width.
 *    6: nHeight - Control height.
 *    7: NIL     - Placeholder (unused in current implementation).
 *    8: lNoEdge - Logical: If .T., suppresses the WS_EX_CLIENTEDGE style.
 *    9: nMax    - Numeric: Maximum character limit.
 *   10: lRead   - Logical: If .T., sets the control to Read-Only mode.
 *   11: lHide   - Logical: If .T., the control is created without WS_VISIBLE.
 *   12: lNoTab  - Logical: If .T., the control is excluded from Tab navigation.
 *   13: lNoVScr - Logical: If .T., disables the vertical scrollbar.
 *   14: lNoHScr - Logical: If .T., disables the horizontal scrollbar.
 *
 * Returns:
 *    HWND - The handle of the created Edit control, or NULL if creation fails.
 *
 * Side Effects:
 *    - Creates a new window in the Win32 subsystem.
 *    - Subclasses the control by attaching 'OwnEditProc'.
 *    - Stores the original window procedure in a window property named "oldeditproc".
 */
HB_FUNC( INITEDITBOX )
{
   // Extract parameters from the Harbour Virtual Machine stack into C variables.
   HWND     hWndParent  = hmg_par_raw_HWND( 1 );
   HMENU    hMenu       = hmg_par_raw_HMENU( 2 );
   int      nX          = hb_parni( 3 );
   int      nY          = hb_parni( 4 );
   int      nWidth      = hb_parni( 5 );
   int      nHeight     = hb_parni( 6 );
   BOOL     bNoEdge     = hb_parl( 8 );
   WPARAM   nMaxChars   = ( WPARAM ) hb_parni( 9 );
   BOOL     bReadOnly   = hb_parl( 10 );
   BOOL     bNotVisible = hb_parl( 11 );  
   BOOL     bNoTabStop  = hb_parl( 12 );   
   BOOL     bNoVScroll  = hb_parl( 13 );   
   BOOL     bNoHScroll  = hb_parl( 14 );   
   HWND     hWndEdit;

   /* 
      Define base styles:
      - WS_CHILD: Required for controls embedded in a window.
      - ES_MULTILINE: HMG EditBox is designed for multi-line text.
      - ES_WANTRETURN: Ensures the 'Enter' key creates a new line rather than 
        triggering the default button of the parent dialog.
   */
   DWORD    dwStyle = WS_CHILD | ES_MULTILINE | ES_WANTRETURN;
   
   /* 
      Extended Style: WS_EX_CLIENTEDGE provides the standard 3D sunken border.
      We only apply it if the user hasn't explicitly requested 'NoEdge'.
   */
   DWORD    dwExStyle = bNoEdge ? 0 : WS_EX_CLIENTEDGE;

   // Apply Read-Only attribute if requested.
   if( bReadOnly )
   {
      dwStyle |= ES_READONLY;
   }

   // Visibility logic: use an inverted flag (bNotVisible) in this internal call.
   if( !bNotVisible )
   {
      dwStyle |= WS_VISIBLE;
   }

   // TabStop logic: Allows the user to navigate to this control using the Tab key.
   if( !bNoTabStop )
   {
      dwStyle |= WS_TABSTOP;
   }

   /* 
      Vertical Scroll Logic:
      If vertical scroll is enabled, we add WS_VSCROLL (the bar).
      If disabled, we use ES_AUTOVSCROLL to allow the text to scroll internally 
      without showing a physical scrollbar.
   */
   if( !bNoVScroll )
   {
      dwStyle |= WS_VSCROLL;
   }
   else
   {
      dwStyle |= ES_AUTOVSCROLL;
   }

   // Horizontal Scroll Logic: Adds a horizontal scrollbar if requested.
   if( !bNoHScroll )
   {
      dwStyle |= WS_HSCROLL;
   }

   /* 
      Create the Win32 Window using CreateWindowEx to support the extended border style.
   */
   hWndEdit = CreateWindowEx( 
      dwExStyle, 
      WC_EDIT, 
      TEXT( "" ), 
      dwStyle, 
      nX, nY, nWidth, nHeight, 
      hWndParent, 
      hMenu, 
      GetInstance(), 
      NULL 
   );

   if( hWndEdit )
   {
      /* 
         Set the maximum text length. 
         EM_LIMITTEXT is the standard message to restrict user input size.
      */
      SendMessage( hWndEdit, EM_LIMITTEXT, nMaxChars, 0 );

      /* 
         Subclassing Mechanism:
         1. Retrieve the original Windows Edit procedure using GetWindowLongPtr.
         2. Store it in a Window Property ("oldeditproc"). This is safer than 
            global variables as it allows multiple EditBoxes to coexist.
         3. Replace the procedure with HMG's 'OwnEditProc' to handle custom 
            events (like specialized key handling or focus management).
      */
      SetProp( hWndEdit, TEXT( "oldeditproc" ), ( HANDLE ) ( LONG_PTR ) GetWindowLongPtr( hWndEdit, GWLP_WNDPROC ) );
      SubclassWindow2( hWndEdit, OwnEditProc );
   }

   // Return the handle of the created control back to the Harbour application.
   hmg_ret_raw_HWND( hWndEdit );
}