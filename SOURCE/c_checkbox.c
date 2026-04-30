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
   Compatibility definitions for older Borland C++ compilers to ensure 
   modern Windows Common Control constants are available.
*/
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_BUTTON                      "Button"
#define BUTTON_IMAGELIST_ALIGN_CENTER  4
#endif

/* 
   External HMG helper functions for image processing and button management.
   HMG_LoadPicture: Handles multi-format image loading (BMP, JPG, PNG, etc.)
   HMG_SetButtonImageList: Associates an ImageList with a button for themed icons.
*/
HBITMAP     HMG_LoadPicture
            (
               const char  *FileName,
               int         New_Width,
               int         New_Height,
               HWND        hWnd,
               int         ScaleStretch,
               int         Transparent,
               long        BackgroundColor,
               int         AdjustImage,
               HB_BOOL     bAlphaFormat,
               int         iAlphaConstant
            );
HIMAGELIST  HMG_SetButtonImageList( HWND hButton, const char *FileName, int Transparent, UINT uAlign );

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif
HINSTANCE   GetInstance( void );

/*
 * HB_FUNC( INITCHECKBOX )
 * -----------------------
 * Purpose: 
 *    Initializes and creates a standard Windows CheckBox control.
 *
 * Parameters:
 *    1: HWND    - Parent window handle.
 *    2: String  - Control caption/text.
 *    3: Integer - Control ID (HMENU).
 *    4: Integer - Row (Y position).
 *    5: Integer - Column (X position).
 *    6: Logical - Multiline support.
 *    7: Logical - 3-State mode (Checked, Unchecked, Indeterminate).
 *    8: Integer - Width.
 *    9: Integer - Height.
 *    10: Logical - Invisible flag (True = Hidden initially).
 *    11: Logical - NoTabStop flag (True = Cannot be tabbed into).
 *    12: Logical - RightAlign flag (Text on the left, box on the right).
 *    13: Logical - Transparent flag (Extended style for background transparency).
 *
 * Returns: 
 *    HWND of the created control.
 */
HB_FUNC( INITCHECKBOX )
{
#ifdef UNICODE
   // Convert ANSI string from Harbour to WideChar for Unicode builds
   LPWSTR   lpWindowName = AnsiToWide( hb_parc( 2 ) );
#else
   LPCSTR   lpWindowName = hb_parc( 2 );
#endif

   // BS_NOTIFY is essential for the parent to receive BN_CLICKED and other notifications.
   // BS_AUTO3STATE or BS_AUTOCHECKBOX allows the OS to handle the toggle logic automatically.
   DWORD    Style = BS_NOTIFY | WS_CHILD | ( hb_parl( 7 ) ? BS_AUTO3STATE : BS_AUTOCHECKBOX );

   // Visibility logic: HMG defaults to visible unless explicitly hidden.
   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   // TabStop logic: Controls whether the user can navigate to this control using the Tab key.
   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;
   }

   // BS_LEFTTEXT places the checkbox to the right of the text.
   if( hb_parl( 12 ) )
   {
      Style |= BS_LEFTTEXT;
   }

   // BS_MULTILINE allows the caption to wrap if it exceeds the control width.
   if( hb_parl( 6 ) )
   {
      Style |= BS_MULTILINE;
   }

   // CreateWindowEx is used here to support WS_EX_TRANSPARENT for better UI integration.
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            hb_parl( 13 ) ? WS_EX_TRANSPARENT : 0,
            WC_BUTTON,
            lpWindowName,
            Style,
            hb_parni( 4 ),
            hb_parni( 5 ),
            hb_parni( 8 ),
            hb_parni( 9 ),
            hmg_par_raw_HWND( 1 ),
            hmg_par_raw_HMENU( 3 ),
            GetInstance(),
            NULL
         )
   );

#ifdef UNICODE
   hb_xfree( lpWindowName );
#endif
}

/*
 * HB_FUNC( INITCHECKBUTTON )
 * --------------------------
 * Purpose: 
 *    Creates a CheckBox that visually behaves like a PushButton (Toggle Button).
 *
 * Parameters:
 *    1: HWND    - Parent window handle.
 *    2: String  - Button caption.
 *    3: Integer - Control ID.
 *    4: Integer - Row.
 *    5: Integer - Column.
 *    8: Integer - Width.
 *    9: Integer - Height.
 *    10: Logical - Invisible flag.
 *    11: Logical - NoTabStop flag.
 *
 * Returns: 
 *    HWND of the created control.
 */
HB_FUNC( INITCHECKBUTTON )
{
#ifdef UNICODE
   LPWSTR   lpWindowName = AnsiToWide( hb_parc( 2 ) );
#else
   LPCSTR   lpWindowName = hb_parc( 2 );
#endif

   // BS_PUSHLIKE is the key style that transforms the checkbox into a toggle button.
   DWORD    Style = BS_NOTIFY | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;
   }

   hmg_ret_raw_HWND( CreateWindow( WC_BUTTON, lpWindowName, Style, hb_parni( 4 ), hb_parni( 5 ), hb_parni( 8 ), hb_parni( 9 ), hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 3 ), GetInstance(), NULL ) );

#ifdef UNICODE
   hb_xfree( lpWindowName );
#endif
}

/*
 * HB_FUNC( INITIMAGECHECKBUTTON )
 * -------------------------------
 * Purpose: 
 *    Creates a Push-style CheckBox that displays an image (Bitmap or ImageList).
 *
 * Parameters:
 *    1: HWND    - Parent window handle.
 *    2: String  - Caption (usually empty when using images).
 *    3: Integer - Control ID.
 *    4: Integer - Row.
 *    5: Integer - Column.
 *    7: Logical - Transparent image flag.
 *    8: String  - Path to the image file.
 *    9: Integer - Width.
 *    10: Integer - Height.
 *    11: Logical - Invisible flag.
 *    12: Logical - NoTabStop flag.
 *    13: Logical - Use ImageList flag (True = Use modern ImageList, False = Standard Bitmap).
 *
 * Returns: 
 *    Array: { Control_HWND, Image_Handle }
 *    The image handle is returned so the HMG framework can manage its lifecycle and prevent leaks.
 */
HB_FUNC( INITIMAGECHECKBUTTON )
{
   HWND     hbutton;
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );
#else
   LPWSTR   lpWindowName = AnsiToWide( hb_parc( 2 ) );
#endif

   // BS_BITMAP style is required for the button to accept and display graphical content.
   DWORD    Style = BS_NOTIFY | BS_BITMAP | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;
   }

   hbutton = CreateWindow( WC_BUTTON, lpWindowName, Style, hb_parni( 4 ), hb_parni( 5 ), hb_parni( 9 ), hb_parni( 10 ), hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 3 ), GetInstance(), NULL );

   // Logic branch: Standard Bitmap vs. ImageList
   if( !hb_parl( 13 ) )
   {
      // Load a standard bitmap using HMG's internal engine.
      // Parameters -1 for width/height indicate original image dimensions should be used.
      HBITMAP  himage = HMG_LoadPicture( hb_parc( 8 ), -1, -1, hmg_par_raw_HWND( 1 ), 0, hb_parl( 7 ) ? 0 : 1, -1, 0, HB_FALSE, 255 );
      
      // Assign the bitmap to the button via Win32 API message.
      SendMessage( hbutton, BM_SETIMAGE, IMAGE_BITMAP, ( LPARAM ) himage );

      // Return both handles to Harbour for resource tracking.
      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( himage, -1, 2 );
   }
   else
   {
      // Use ImageList approach, which is better for themed applications and high-DPI.
      HIMAGELIST  himl = HMG_SetButtonImageList( hbutton, hb_parc( 8 ), hb_parl( 7 ) ? 0 : 1, BUTTON_IMAGELIST_ALIGN_CENTER );

      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( himl, -1, 2 );
   }

#ifdef UNICODE
   hb_xfree( lpWindowName );
#endif
}