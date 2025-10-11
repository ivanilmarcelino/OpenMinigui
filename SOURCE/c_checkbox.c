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

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
   // Define button control constants for older versions of Borland C++
   #define WC_BUTTON                      "Button"
   #define BUTTON_IMAGELIST_ALIGN_CENTER  4
#endif

// Function declarations for loading images and handling instances
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
HINSTANCE   GetResources( void );

/**
 * Function: INITCHECKBOX
 * Initializes a checkbox control with various style options.
 * Parameters:
 *   - lpWindowName: The label text for the checkbox.
 *   - Style settings including visibility, tab-stop, multiline, left-text alignment, etc.
 * Returns:
 *   - The handle to the created checkbox control.
 */
HB_FUNC( INITCHECKBOX )
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   DWORD    Style = BS_NOTIFY | WS_CHILD | ( hb_parl( 7 ) ? BS_AUTO3STATE : BS_AUTOCHECKBOX );
   DWORD    ExStyle = hb_parl( 13 ) ? WS_EX_TRANSPARENT : 0;

   // Adjust styles based on parameters
   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;
   }

   if( hb_parl( 12 ) )
   {
      Style |= BS_LEFTTEXT;
   }

   if( hb_parl( 6 ) )
   {
      Style |= BS_MULTILINE;
   }

   // Create the checkbox control
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            ExStyle,
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

/**
 * Function: INITCHECKBUTTON
 * Initializes a check button (checkbox styled like a button).
 * Parameters:
 *   - lpWindowName: The label text for the button.
 *   - Style settings including visibility, tab-stop, etc.
 * Returns:
 *   - The handle to the created check button.
 */
HB_FUNC( INITCHECKBUTTON )
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   DWORD    Style = BS_NOTIFY | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   // Adjust styles based on parameters
   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the check button
   hmg_ret_raw_HWND
   (
      CreateWindow
         (
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

/**
 * Function: INITIMAGECHECKBUTTON
 * Initializes a check button with an image, optionally using an image list.
 * Parameters:
 *   - lpWindowName: The label text for the button.
 *   - Image file for the button.
 *   - Various style settings, including visibility, tab-stop, transparency, etc.
 * Returns:
 *   - Handles to the created check button and its associated image.
 */
HB_FUNC( INITIMAGECHECKBUTTON )
{
   HWND        hbutton;
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   HBITMAP     himage;
   HIMAGELIST  himl;

#ifndef UNICODE
   LPCSTR      lpWindowName = hb_parc( 2 );
#else
   LPWSTR      lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   DWORD       Style = BS_NOTIFY | BS_BITMAP | WS_CHILD | BS_AUTOCHECKBOX | BS_PUSHLIKE;

   // Adjust styles based on parameters
   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the button with bitmap style to accommodate an image
   hbutton = CreateWindow
      (
         WC_BUTTON,
         lpWindowName,
         Style,
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 9 ),
         hb_parni( 10 ),
         hwnd,
         hmg_par_raw_HMENU( 3 ),
         GetInstance(),
         NULL
      );

   // Load image either as a bitmap or image list based on parameter
   if( !hb_parl( 13 ) )
   {
      himage = HMG_LoadPicture( hb_parc( 8 ), -1, -1, hwnd, 0, hb_parl( 7 ) ? 0 : 1, -1, 0, HB_FALSE, 255 );
      SendMessage( hbutton, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himage );
      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( himage, -1, 2 );
   }
   else
   {
      himl = HMG_SetButtonImageList( hbutton, hb_parc( 8 ), hb_parl( 7 ) ? 0 : 1, BUTTON_IMAGELIST_ALIGN_CENTER );
      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( himl, -1, 2 );
   }

#ifdef UNICODE
   hb_xfree( lpWindowName );
#endif
}
