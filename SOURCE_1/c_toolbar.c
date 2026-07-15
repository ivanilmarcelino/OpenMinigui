/*
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

   TOOLBAREX and TOOLBUTTONEX controls source code
   (C)2005 Janusz Pora <januszpora@onet.eu>
*/

#define _WIN32_IE 0x0501
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0502

#include <mgdefs.h>

// Suppress warnings for older Win32 API functions replaced by secure versions
#if defined( _MSC_VER )
#pragma warning( disable : 4996 )
#endif
#include <commctrl.h>

// Maximum number of buttons processed in a single batch operation
#define NUM_TOOLBAR_BUTTONS   10

/*
 * External HMG Image Loader
 * Used to handle various image formats (BMP, JPG, PNG) and convert them 
 * to Win32 HBITMAPs compatible with toolbar image lists.
 */
extern HBITMAP    HMG_LoadPicture
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
                     int         iAlpfaConstant
                  );

#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
#endif

HINSTANCE         GetInstance( void );
HINSTANCE         GetResources( void );

void              RegisterResource( HANDLE hResource, LPCSTR szType );
void pascal       DelResource( HANDLE hResource );

/* 
 * Static state variables for Toolbar Customization
 * These persist during the 'Customize' dialog session to allow 'Reset' functionality.
 */
static LPTBBUTTON lpSaveButtons;                   // Stores original button state for restoration
static int        nResetCount, buttonCount;        // Counters for button tracking
static int        isInSizeMsg = 0;                 // Recursion guard for resizing logic

/*
 * FUNCTION: INITTOOLBAR
 * Purpose: Initializes a standard Win32 Toolbar control.
 * 
 * Parameters:
 *   1: HWND   - Parent window handle.
 *   3: HMENU  - Control ID/Menu handle.
 *   6: INT    - Button Width.
 *   7: INT    - Button Height.
 *   10: BOOL  - Flat style (Office-like look).
 *   11: BOOL  - Bottom alignment (CCS_BOTTOM).
 *   12: BOOL  - List style (Text to the right of icons).
 *   13: BOOL  - No Parent Align (Manual positioning).
 *   14: BOOL  - Client Edge (3D border).
 *   15: BOOL  - Wrapable (Multi-line support).
 *   16: BOOL  - Adjustable (Allows user customization via Shift+Double Click).
 *
 * Returns: HWND of the created toolbar.
 */
HB_FUNC( INITTOOLBAR )
{
   HWND  hwndTB;

   // Default styles: Child window, visible, supports tooltips
   DWORD Style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;
   DWORD ExStyle = 0;

   // Enable dropdown arrows by default for buttons that support them
   DWORD TbExStyle = TBSTYLE_EX_DRAWDDARROWS;

   if( hb_parl( 14 ) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }

   if( hb_parl( 10 ) )
   {
      Style |= TBSTYLE_FLAT;
   }

   if( hb_parl( 11 ) )
   {
      Style |= CCS_BOTTOM;
   }

   if( hb_parl( 12 ) )
   {
      Style |= TBSTYLE_LIST;
   }

   if( hb_parl( 13 ) )
   {
      // CCS_NOPARENTALIGN prevents the toolbar from snapping to the top/bottom of the parent
      Style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl( 15 ) )
   {
      Style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl( 16 ) )
   {
      Style |= CCS_ADJUSTABLE;
   }

   hwndTB = CreateWindowEx( ExStyle, TOOLBARCLASSNAME, NULL, Style, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 3 ), GetInstance(), NULL );

   // Explicitly set sizes if provided, otherwise Windows defaults are used
   if( hb_parni( 6 ) && hb_parni( 7 ) )
   {
      SendMessage( hwndTB, TB_SETBUTTONSIZE, hb_parni( 6 ), hb_parni( 7 ) );
      SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( hb_parni( 6 ), hb_parni( 7 ) ) );
   }

   SendMessage( hwndTB, TB_SETEXTENDEDSTYLE, 0, ( LPARAM ) TbExStyle );

   hmg_ret_raw_HWND( hwndTB );
}

/*
 * FUNCTION: INITTOOLBUTTON
 * Purpose: Adds a button to an existing toolbar.
 * 
 * Logic:
 *   1. Loads the image via HMG_LoadPicture.
 *   2. Adds the bitmap to the toolbar's internal image list.
 *   3. Configures the TBBUTTON structure with styles (Check, Group, Dropdown).
 *   4. Appends the button to the control.
 */
HB_FUNC( INITTOOLBUTTON )
{
   HWND        hwndTB = hmg_par_raw_HWND( 1 );
   HBITMAP     himage = NULL;
   TBADDBITMAP tbab;
   TBBUTTON    tbb[NUM_TOOLBAR_BUTTONS];
   int         nBtn = 0, nPoz;
   int         Style = TBSTYLE_BUTTON;
#ifndef UNICODE
   LPCSTR      lpText;
#else
   LPWSTR      lpText;
#endif
   memset( tbb, 0, sizeof( tbb ) );

   // Handle image loading and scaling
   if( hb_parclen( 8 ) > 0 )
   {
      // Calculate internal padding to ensure the image fits within the button borders
      DWORD tSize = ( DWORD ) SendMessage( hwndTB, TB_GETPADDING, 0, 0 );
      int   px = LOWORD( tSize ), py = HIWORD( tSize );
      int   ix = hb_parl( 16 ) ? hb_parni( 6 ) - px : -1;
      int   iy = hb_parl( 16 ) ? hb_parni( 7 ) - py : -1;

      himage = ( HBITMAP ) HMG_LoadPicture( hb_parc( 8 ), ix, iy, hwndTB, 1, hb_parl( 9 ) ? 0 : 1, -1, hb_parl( 16 ) ? 1 : 0, HB_FALSE, 255 );
   }

   if( hb_parl( 11 ) )
   {
      Style |= TBSTYLE_AUTOSIZE;
   }

   tbab.hInst = NULL;
   tbab.nID = ( UINT_PTR ) himage;

   // TB_ADDBITMAP returns the index of the first new image in the internal list
   nPoz = ( int ) SendMessage( hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) & tbab );

   if( hb_parclen( 2 ) > 0 )
   {
#ifndef UNICODE
      lpText = hb_parc( 2 );
#else
      lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif

      // TB_ADDSTRING adds the caption to the toolbar's string pool
      tbb[nBtn].iString = ( int ) SendMessage( hwndTB, TB_ADDSTRING, 0, ( LPARAM ) lpText );
#ifdef UNICODE
      hb_xfree( lpText );
#endif
   }

   // Map HMG logical flags to Win32 Button Styles
   if( hb_parl( 12 ) )
   {
      Style |= BTNS_CHECK;
   }

   if( hb_parl( 13 ) )
   {
      Style |= BTNS_GROUP;
   }

   if( hb_parl( 14 ) )
   {
      Style |= BTNS_DROPDOWN;
   }

   if( hb_parl( 15 ) )
   {
      Style |= BTNS_WHOLEDROPDOWN;
   }

   tbb[nBtn].iBitmap = nPoz;
   tbb[nBtn].idCommand = hb_parni( 3 );
   tbb[nBtn].fsState = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle = ( BYTE ) Style;
   nBtn++;

   // Handle optional separator following the button
   if( hb_parl( 10 ) )
   {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage( hwndTB, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );
   SendMessage( hwndTB, TB_ADDBUTTONS, nBtn, ( LPARAM ) tbb );
   SendMessage( hwndTB, TB_AUTOSIZE, 0, 0 );

   hmg_ret_raw_HANDLE( himage );
}

/*
 * FUNCTION: WidestBtn
 * Purpose: Internal helper to measure text dimensions using the system icon font.
 * Used to calculate auto-sizing for buttons with text.
 */
LONG WidestBtn( LPCTSTR pszStr, HWND hwnd )
{
   SIZE     sz;
   LOGFONT  lf;
   HFONT    hFont;
   HDC      hdc;
#ifndef UNICODE
   LPCSTR   lpString = pszStr;
#else
   LPCWSTR  lpString = AnsiToWide( ( char * ) pszStr );
#endif

   // Retrieve the standard font used for icon titles in Windows
   SystemParametersInfo( SPI_GETICONTITLELOGFONT, sizeof( LOGFONT ), &lf, 0 );
   hdc = GetDC( hwnd );
   hFont = CreateFontIndirect( &lf );
   SelectObject( hdc, hFont );
   GetTextExtentPoint32( hdc, lpString, ( int ) lstrlen( lpString ), &sz );
   ReleaseDC( hwnd, hdc );
   DeleteObject( hFont );
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpString );
#endif
   return MAKELONG( sz.cx, sz.cy );
}

/*
 * FUNCTION: INITTOOLBAREX
 * Purpose: Extended Toolbar initialization.
 * Supports TBSTYLE_EX_MIXEDBUTTONS which allows some buttons to have text 
 * while others only show icons, optimizing space.
 */
HB_FUNC( INITTOOLBAREX )
{
   HWND                 hwndTB;
   DWORD                Style = WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS;
   DWORD                ExStyle = 0;
   DWORD                TbExStyle = TBSTYLE_EX_DRAWDDARROWS;
   INITCOMMONCONTROLSEX icex;

   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icex.dwICC = ICC_BAR_CLASSES;
   InitCommonControlsEx( &icex );

   if( hb_parl( 14 ) )
   {
      ExStyle |= WS_EX_CLIENTEDGE;
   }
   else
   {
      // Hide buttons that don't fit in the current width
      TbExStyle |= TBSTYLE_EX_HIDECLIPPEDBUTTONS;
   }

   if( hb_parl( 10 ) )
   {
      Style |= TBSTYLE_FLAT;
   }

   if( hb_parl( 11 ) )
   {
      Style |= CCS_BOTTOM;
   }

   if( hb_parl( 12 ) )
   {
      Style |= TBSTYLE_LIST;
   }

   if( hb_parl( 13 ) )
   {
      Style |= CCS_NOPARENTALIGN | CCS_NODIVIDER | CCS_NORESIZE;
   }

   if( hb_parl( 15 ) )
   {
      // Mixed buttons allow text/no-text variation
      TbExStyle |= TBSTYLE_EX_MIXEDBUTTONS;
   }

   if( hb_parl( 16 ) )
   {
      Style |= TBSTYLE_WRAPABLE;
   }

   if( hb_parl( 17 ) )
   {
      Style |= CCS_ADJUSTABLE;
   }

   hwndTB = CreateWindowEx( ExStyle, TOOLBARCLASSNAME, NULL, Style, 0, 0, 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 3 ), GetInstance(), NULL );

   if( hb_parni( 6 ) && hb_parni( 7 ) )
   {
      DWORD nPadd = ( DWORD ) SendMessage( hwndTB, TB_GETPADDING, 0, 0 );

      SendMessage( hwndTB, TB_SETBUTTONSIZE, hb_parni( 6 ), hb_parni( 7 ) );

      // Adjust bitmap size to account for padding
      SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( hb_parni( 6 ) - LOWORD( nPadd ), hb_parni( 7 ) - HIWORD( nPadd ) ) );
   }

   SendMessage( hwndTB, TB_SETBUTTONWIDTH, 0, ( LPARAM ) MAKELONG( hb_parni( 6 ), hb_parni( 6 ) ) );
   SendMessage( hwndTB, TB_SETEXTENDEDSTYLE, 0, ( LPARAM ) TbExStyle );
   hmg_ret_raw_HWND( hwndTB );
}

/*
 * FUNCTION: INITTOOLBUTTONEX
 * Purpose: Extended button initialization with support for system image lists 
 * and advanced text alignment.
 */
HB_FUNC( INITTOOLBUTTONEX )
{
   HWND           hwndTB = hmg_par_raw_HWND( 1 );
   HBITMAP        himage = NULL;
   BITMAP         bm;
   TBADDBITMAP    tbab;
   TBBUTTON       tbb[NUM_TOOLBAR_BUTTONS];
   TBBUTTON       lpBtn;
   DWORD          tSize, TbStyle;
   int            nBtn = 0, nPoz, xBtn, Style = TBSTYLE_BUTTON;
   int            ix = 0, iy = 0, px, py, tmax = 0, i;
   TCHAR          cBuff[255] = { 0 };
#ifndef UNICODE
   LPCSTR         lpText;
#else
   LPWSTR         lpText;
#endif
   OSVERSIONINFO  osvi;

   memset( tbb, 0, sizeof( tbb ) );

   TbStyle = ( DWORD ) SendMessage( hwndTB, TB_GETSTYLE, 0, 0 );
   xBtn = ( int ) SendMessage( hwndTB, TB_BUTTONCOUNT, 0, 0 );

   // Handle text and calculate vertical space for multi-line or text-under-icon styles
   if( hb_parclen( 2 ) > 0 )
   {
#ifndef UNICODE
      lpText = hb_parc( 2 );
#else
      lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
      tbb[nBtn].iString = ( int ) SendMessage( hwndTB, TB_ADDSTRING, 0, ( LPARAM ) lpText );
      Style |= BTNS_SHOWTEXT;
#ifdef UNICODE
      hb_xfree( lpText );
#endif
      tSize = WidestBtn( ( LPCTSTR ) hb_parc( 2 ), hwndTB );
      tmax = HIWORD( tSize );

      // Iterate existing buttons to find the tallest text to maintain uniformity
      for( i = 0; i < xBtn; i++ )
      {
         SendMessage( hwndTB, TB_GETBUTTON, i, ( LPARAM ) & lpBtn );
         SendMessage( hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, ( LPARAM ) cBuff );
         tSize = WidestBtn( cBuff, hwndTB );
         if( tmax < HIWORD( tSize ) )
         {
            tmax = HIWORD( tSize );
         }
      }
   }

   tSize = ( DWORD ) SendMessage( hwndTB, TB_GETPADDING, 0, 0 );
   px = LOWORD( tSize );
   py = HIWORD( tSize );

   if( hb_parl( 16 ) )
   {
      ix = hb_parni( 6 ) - px;
      iy = hb_parni( 7 ) - py;
   }

   if( HB_ISCHAR( 8 ) )
   {
      himage = ( HBITMAP ) HMG_LoadPicture( hb_parc( 8 ), hb_parl( 16 ) ? ix : -1, hb_parl( 16 ) ? iy : -1, hwndTB, 1, hb_parl( 9 ) ? 0 : 1, -1, hb_parl( 16 ) ? 1 : 0, HB_FALSE, 255 );
   }

   // Adjust padding based on OS version and button style for visual consistency
   if( himage != NULL && GetObject( himage, sizeof( BITMAP ), &bm ) != 0 )
   {
      ix = bm.bmWidth;
      iy = bm.bmHeight;
      if( TbStyle & TBSTYLE_LIST )
      {
         tmax = 0;
      }

      if( ( ix + px ) > hb_parni( 6 ) )
      {
         ix = hb_parni( 6 ) - px;
      }
      else
      {
         px = hb_parni( 6 ) - ix;
      }

      if( ( iy + tmax + py ) > hb_parni( 7 ) )
      {
         iy = hb_parni( 7 ) - tmax - py;
      }
      else
      {
         py = hb_parni( 7 ) - tmax - iy;
      }

      osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
      GetVersionEx( &osvi );

      // Legacy Windows NT4 handling for padding
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 )
      {
         if( !( TbStyle & TBSTYLE_LIST ) )
         {
            SendMessage( hwndTB, TB_SETPADDING, 0, MAKELPARAM( px, py ) );
         }
      }
      else if( !( Style & BTNS_SHOWTEXT ) )
      {
         SendMessage( hwndTB, TB_SETPADDING, 0, MAKELPARAM( px, py ) );
      }

      SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( ix, iy ) );
   }

   if( hb_parl( 11 ) )
   {
      Style |= TBSTYLE_AUTOSIZE;
   }

   // Handle System Image Lists (e.g., standard Shell icons)
   if( hb_parni( 17 ) > -1 )
   {
      if( xBtn == 0 )
      {
         if( hb_parni( 18 ) > IDB_HIST_LARGE_COLOR )
         {
            SendMessage( hwndTB, TB_SETIMAGELIST, 0, ( LPARAM ) hmg_par_raw_HIMAGELIST( 18 ) );
            if( hb_parni( 19 ) )
            {
               SendMessage( hwndTB, TB_SETHOTIMAGELIST, 0, ( LPARAM ) hmg_par_raw_HIMAGELIST( 19 ) );
            }

            tbab.nID = hb_parni( 18 );
         }
         else
         {
            tbab.hInst = HINST_COMMCTRL;
            tbab.nID = hb_parni( 18 );
            SendMessage( hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) & tbab );
         }
      }

      nPoz = hb_parni( 17 );
   }
   else
   {
      tbab.hInst = NULL;
      tbab.nID = ( UINT_PTR ) himage;
      nPoz = ( int ) SendMessage( hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) & tbab );
   }

   if( hb_parl( 12 ) )
   {
      Style |= BTNS_CHECK;
   }

   if( hb_parl( 13 ) )
   {
      Style |= BTNS_GROUP;
   }

   if( hb_parl( 14 ) )
   {
      Style |= BTNS_DROPDOWN;
   }

   if( hb_parl( 15 ) )
   {
      Style |= BTNS_WHOLEDROPDOWN;
   }

   SendMessage( hwndTB, TB_AUTOSIZE, 0, 0 );

   tbb[nBtn].iBitmap = nPoz;
   tbb[nBtn].idCommand = hb_parni( 3 );
   tbb[nBtn].fsState = TBSTATE_ENABLED;
   tbb[nBtn].fsStyle = ( BYTE ) Style;
   nBtn++;

   if( hb_parl( 10 ) )
   {
      tbb[nBtn].fsState = 0;
      tbb[nBtn].fsStyle = TBSTYLE_SEP;
      nBtn++;
   }

   SendMessage( hwndTB, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );
   SendMessage( hwndTB, TB_ADDBUTTONS, nBtn, ( LPARAM ) tbb );
   hmg_ret_raw_HANDLE( himage );
}

/*
 * FUNCTION: GETSIZETOOLBAR
 * Purpose: Calculates the total bounding size of the toolbar.
 * Logic: Uses TB_GETMAXSIZE and adjusts for separators and dropdown arrows 
 * which might not be fully accounted for in older Windows versions.
 */
HB_FUNC( GETSIZETOOLBAR )
{
   SIZE           lpSize;
   TBBUTTON       lpBtn;
   HWND           hwndTB = hmg_par_raw_HWND( 1 );
   int            i, nBtn;
   OSVERSIONINFO  osvi;

   SendMessage( hwndTB, TB_GETMAXSIZE, 0, ( LPARAM ) & lpSize );

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );
   nBtn = ( int ) SendMessage( hwndTB, TB_BUTTONCOUNT, 0, 0 );

   for( i = 0; i < nBtn; i++ )
   {
      SendMessage( hwndTB, TB_GETBUTTON, i, ( LPARAM ) & lpBtn );

      // NT4 fix for separator width
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 )
      {
         if( lpBtn.fsStyle & TBSTYLE_SEP )
         {
            lpSize.cx += 3;
         }
      }

      // Add space for the dropdown arrow glyph
      if( lpBtn.fsStyle & BTNS_DROPDOWN )
      {
         lpSize.cx += 16;
      }
   }

   hb_retnl( MAKELONG( lpSize.cy, lpSize.cx ) );
}

/*
 * FUNCTION: MAXTEXTBTNTOOLBAR
 * Purpose: Uniformly sizes all buttons based on the widest text label.
 */
HB_FUNC( MAXTEXTBTNTOOLBAR )
{
   TCHAR cString[255] = { 0 };
   HWND  hwndTB = hmg_par_raw_HWND( 1 );
   int   i, nBtn = ( int ) SendMessage( hwndTB, TB_BUTTONCOUNT, 0, 0 );
   int   tmax = 0, ty = 0;
   DWORD tSize, Style;

   for( i = 0; i < nBtn; i++ )
   {
      TBBUTTON lpBtn;
      SendMessage( hwndTB, TB_GETBUTTON, i, ( LPARAM ) & lpBtn );
      SendMessage( hwndTB, TB_GETBUTTONTEXT, lpBtn.idCommand, ( LPARAM ) cString );
      tSize = WidestBtn( cString, hwndTB );
      ty = HIWORD( tSize );
      if( tmax < ( int ) LOWORD( tSize ) )
      {
         tmax = LOWORD( tSize );
      }
   }

   if( tmax == 0 )
   {
      SendMessage( hwndTB, TB_SETBUTTONSIZE, hb_parni( 2 ), hb_parni( 3 ) );
      SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( hb_parni( 2 ), hb_parni( 3 ) ) );
   }
   else
   {
      Style = ( DWORD ) SendMessage( hwndTB, TB_GETSTYLE, 0, 0 );
      if( Style & TBSTYLE_LIST )
      {
         SendMessage( hwndTB, TB_SETBUTTONSIZE, hb_parni( 2 ), hb_parni( 3 ) + 2 );
         SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( hb_parni( 3 ), hb_parni( 3 ) ) );
      }
      else
      {
         SendMessage( hwndTB, TB_SETBUTTONSIZE, hb_parni( 2 ), hb_parni( 3 ) - ty + 2 );
         SendMessage( hwndTB, TB_SETBITMAPSIZE, 0, ( LPARAM ) MAKELONG( hb_parni( 3 ) - ty, hb_parni( 3 ) - ty ) );
      }

      SendMessage( hwndTB, TB_SETBUTTONWIDTH, 0, ( LPARAM ) MAKELONG( hb_parni( 2 ), hb_parni( 2 ) + 2 ) );
   }

   SendMessage( hwndTB, TB_AUTOSIZE, 0, 0 );
}

/*
 * FUNCTION: ISBUTTONBARCHECKED
 *
 * Checks if a toolbar button is checked.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button to check.
 *
 * Returns:
 *   TRUE if the button is checked, FALSE otherwise.
 */
HB_FUNC( ISBUTTONBARCHECKED )
{
   TBBUTTON lpBtn;
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETBUTTON, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & lpBtn );
   hb_retl( ( int ) SendMessage( hmg_par_raw_HWND( 1 ), TB_ISBUTTONCHECKED, lpBtn.idCommand, 0 ) );
}

/*
 * FUNCTION: CHECKBUTTONBAR
 *
 * Checks or unchecks a toolbar button.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button to check or uncheck.
 *   Check state: TRUE to check, FALSE to uncheck.
 *
 * Returns:
 *   None.
 */
HB_FUNC( CHECKBUTTONBAR )
{
   TBBUTTON lpBtn;
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETBUTTON, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & lpBtn );
   SendMessage( hmg_par_raw_HWND( 1 ), TB_CHECKBUTTON, lpBtn.idCommand, hb_parl( 3 ) );
}

/*
 * FUNCTION: ISBUTTONENABLED
 *
 * Checks if a toolbar button is enabled.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button to check.
 *
 * Returns:
 *   TRUE if the button is enabled, FALSE otherwise.
 */
HB_FUNC( ISBUTTONENABLED )
{
   TBBUTTON lpBtn;
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETBUTTON, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & lpBtn );
   hb_retl( ( int ) SendMessage( hmg_par_raw_HWND( 1 ), TB_ISBUTTONENABLED, lpBtn.idCommand, 0 ) );
}

/*
 * FUNCTION: GETBUTTONBARRECT
 *
 * Gets the rectangle of a toolbar button.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button.
 *
 * Returns:
 *   A LONG value containing the left and bottom coordinates of the button rectangle.
 */
HB_FUNC( GETBUTTONBARRECT )
{
   RECT  rc;
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETITEMRECT, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rc );
   hmg_ret_LONG( MAKELONG( rc.left, rc.bottom ) );
}

/*
 * FUNCTION: GETBUTTONPOS
 *
 * Gets the position of a toolbar button.
 *
 * Parameters:
 *   lParam: The LPARAM value containing the button position.
 *
 * Returns:
 *   The position of the button.
 */
HB_FUNC( GETBUTTONPOS )
{
   hmg_ret_NINT( ( ( NMTOOLBAR FAR * ) HB_PARNL( 1 ) )->iItem );
}

/*
 * FUNCTION: SETBUTTONTIP
 *
 * Sets the tooltip text for a toolbar button.
 *
 * Parameters:
 *   lParam: The LPARAM value containing the tooltip information.
 *   Text: The tooltip text.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETBUTTONTIP )
{
#ifndef UNICODE
   LPSTR          lpText = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   LPTOOLTIPTEXT  lpttt;

   lpttt = ( LPTOOLTIPTEXT ) HB_PARNL( 1 );
   lpttt->hinst = GetModuleHandle( NULL );
   lpttt->lpszText = lpText;
}

/*
 * FUNCTION: SETTOOLBUTTONCAPTION
 *
 * Sets the caption for a toolbar button.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button.
 *   Text: The caption text.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETTOOLBUTTONCAPTION )
{
#ifndef UNICODE
   LPSTR          lpText = ( LPSTR ) hb_parc( 3 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   TBBUTTONINFO   tbinfo;

   tbinfo.cbSize = sizeof( tbinfo );
   tbinfo.dwMask = TBIF_TEXT;
   tbinfo.pszText = lpText;

   SendMessage( hmg_par_raw_HWND( 1 ), TB_SETBUTTONINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & tbinfo );

#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION: SETTOOLBUTTONIMAGE
 *
 * Sets the image for a toolbar button.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Button index: The index of the button.
 *   Image index: The index of the image.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETTOOLBUTTONIMAGE )
{
   TBBUTTONINFO   tbinfo;

   tbinfo.cbSize = sizeof( tbinfo );
   tbinfo.dwMask = TBIF_IMAGE;
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETBUTTONINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & tbinfo );

   tbinfo.iImage = hb_parni( 3 );
   SendMessage( hmg_par_raw_HWND( 1 ), TB_SETBUTTONINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & tbinfo );
}

/*
 * FUNCTION: REPLACETOOLBUTTONIMAGE
 *
 * Replaces the image of a toolbar button.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Old bitmap handle: The handle to the old bitmap.
 *   Image file: The file name of the new image.
 *   Use callback: TRUE to use a callback for the image.
 *   Button index: The index of the button.
 *
 * Returns:
 *   The handle to the new bitmap.
 */
HB_FUNC( REPLACETOOLBUTTONIMAGE )
{
   HWND     hwndTB = hmg_par_raw_HWND( 1 );
   HBITMAP  hBitmapOld = hmg_par_raw_HBITMAP( 2 );
   int      iImageIdx = hb_parl( 4 ) ? I_IMAGECALLBACK : I_IMAGENONE;
   int      nButtonID = hmg_par_INT( 5 );
   HBITMAP  hBitmapNew;

   hBitmapNew = ( HBITMAP ) HMG_LoadPicture( hb_parc( 3 ), -1, -1, hwndTB, 1, 1, -1, 0, HB_FALSE, 255 );

   if( ( hBitmapOld != NULL ) && ( hBitmapNew != NULL ) )
   {
      TBREPLACEBITMAP   tbrb;
      tbrb.hInstOld = NULL;
      tbrb.nIDOld = ( UINT_PTR ) hBitmapOld;
      tbrb.hInstNew = NULL;
      tbrb.nIDNew = ( UINT_PTR ) hBitmapNew;
      tbrb.nButtons = 1;
      SendMessage( hwndTB, TB_REPLACEBITMAP, 0, ( LPARAM ) & tbrb );
   }
   else
   {
      TBBUTTONINFO   tbinfo;
      int            iBitMapIndex;

      if( hBitmapNew != NULL )
      {
         TBADDBITMAP tbab;
         tbab.hInst = NULL;
         tbab.nID = ( UINT_PTR ) hBitmapNew;
         iBitMapIndex = ( int ) SendMessage( hwndTB, TB_ADDBITMAP, 1, ( LPARAM ) & tbab );
      }
      else
      {
         iBitMapIndex = iImageIdx;
      }

      tbinfo.cbSize = sizeof( tbinfo );
      tbinfo.dwMask = TBIF_IMAGE;
      SendMessage( hwndTB, TB_GETBUTTONINFO, ( WPARAM ) nButtonID, ( LPARAM ) & tbinfo );

      tbinfo.iImage = iBitMapIndex;
      SendMessage( hwndTB, TB_SETBUTTONINFO, ( WPARAM ) nButtonID, ( LPARAM ) & tbinfo );
   }

   RegisterResource( hBitmapNew, "BMP" );
   hmg_ret_raw_HWND( hBitmapNew );
}

/*
 * FUNCTION: SETROWSBUTTON
 *
 * Sets the number of rows for a toolbar.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Rows: The number of rows.
 *   Vertical: TRUE if the toolbar is vertical.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETROWSBUTTON )
{
   RECT  rc;

   SendMessage( hmg_par_raw_HWND( 1 ), TB_SETROWS, ( WPARAM ) MAKEWPARAM( hb_parni( 2 ), hb_parl( 3 ) ), ( LPARAM ) & rc );

   hb_reta( 2 );
   HB_STORVNL( rc.right - rc.left, -1, 1 );
   HB_STORVNL( rc.bottom - rc.top, -1, 2 );
}

/*
 * FUNCTION: RESIZESPLITBOXITEM
 *
 * Resizes a split box item.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Index: The index of the item.
 *   Min width: The minimum width.
 *   Min height: The minimum height.
 *   Ideal width: The ideal width.
 *
 * Returns:
 *   None.
 */
HB_FUNC( RESIZESPLITBOXITEM )
{
   REBARBANDINFO  rbBand;

   rbBand.cbSize = sizeof( REBARBANDINFO );
   rbBand.fMask = RBBIM_CHILDSIZE | RBBIM_IDEALSIZE;

   SendMessage( hmg_par_raw_HWND( 1 ), RB_GETBANDINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rbBand );

   rbBand.fStyle = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxMinChild = hb_parni( 3 );
   rbBand.cyMinChild = hb_parni( 4 );
   rbBand.cxIdeal = hb_parni( 5 );
   rbBand.cx = hb_parni( 5 );

   SendMessage( hmg_par_raw_HWND( 1 ), RB_SETBANDINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rbBand );
}

/*
 * FUNCTION: SETCHEVRONSTYLESPLITBOXITEM
 *
 * Sets the chevron style for a split box item.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Index: The index of the item.
 *   Ideal width: The ideal width.
 *
 * Returns:
 *   TRUE if successful, FALSE otherwise.
 */
HB_FUNC( SETCHEVRONSTYLESPLITBOXITEM )
{
   REBARBANDINFO  rbBand;

   rbBand.cbSize = sizeof( REBARBANDINFO );
   rbBand.fMask = RBBIM_STYLE | RBBIM_IDEALSIZE;

   SendMessage( hmg_par_raw_HWND( 1 ), RB_GETBANDINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rbBand );

   rbBand.fStyle = rbBand.fStyle | RBBS_USECHEVRON;
   rbBand.cxIdeal = hb_parni( 3 ) + 50;

   hb_retl( ( int ) SendMessage( hmg_par_raw_HWND( 1 ), RB_SETBANDINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rbBand ) );
}

/*
 * FUNCTION: SETCAPTIONSPLITBOXITEM
 *
 * Sets the caption for a split box item.
 *
 * Parameters:
 *   hwndTB: The handle to the toolbar window.
 *   Index: The index of the item.
 *   Text: The caption text.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETCAPTIONSPLITBOXITEM )
{
#ifndef UNICODE
   LPSTR          lpText = ( LPSTR ) hb_parc( 3 );
#else
   LPWSTR         lpText = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   REBARBANDINFO  rbBand;

   rbBand.cbSize = sizeof( REBARBANDINFO );
   rbBand.fMask = RBBIM_TEXT;
   rbBand.lpText = lpText;

   SendMessage( hmg_par_raw_HWND( 1 ), RB_SETBANDINFO, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & rbBand );

#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION: TestHidenBtn
 *
 * Tests if a toolbar button is hidden.
 *
 * Parameters:
 *   tbHwnd: The handle to the toolbar window.
 *   rcRb: The rectangle of the rebar.
 *   dv: The delta value.
 *   nBtn: The number of buttons.
 *
 * Returns:
 *   The number of hidden buttons.
 */
int TestHidenBtn( HWND tbHwnd, RECT rcRb, INT dv, INT nBtn )
{
   RECT  rcDst, rcBt;
   int   nBtnV = 0;
   int   i;

   for( i = 0; i < nBtn; i++ )
   {
      SendMessage( ( HWND ) tbHwnd, TB_GETITEMRECT, ( WPARAM ) ( UINT ) i, ( LPARAM ) & rcBt );

      rcBt.left += dv;
      rcBt.top += rcRb.top;
      rcBt.right += dv;
      rcBt.bottom += rcRb.top;

      IntersectRect( &rcDst, &rcRb, &rcBt );
      if( EqualRect( &rcDst, &rcBt ) )
      {
         nBtnV++;
      }
   }

   return nBtnV;
}

/*
 * FUNCTION: CREATEPOPUPCHEVRON
 *
 * Creates a popup chevron for a rebar control.
 *
 * This function calculates the position and dimensions of a popup chevron for a rebar control.
 * It retrieves information about the specified band in the rebar control and determines
 * the number of hidden buttons in the toolbar within that band. The function then returns
 * an array containing the coordinates of the chevron and other relevant information.
 *
 * Parameters:
 *   hwnd: Handle to the rebar control.
 *   lParam: LPARAM value containing a pointer to an NMREBARCHEVRON structure with chevron information.
 *
 * Returns:
 *   An array containing the following elements:
 *   1. Left coordinate of the chevron rectangle.
 *   2. Top coordinate of the chevron rectangle.
 *   3. Right coordinate of the chevron rectangle.
 *   4. Bottom coordinate of the chevron rectangle.
 *   5. Handle to the toolbar window within the specified rebar band.
 *   6. Number of hidden buttons in the toolbar.
 *   7. Total number of buttons in the toolbar.
 */
HB_FUNC( CREATEPOPUPCHEVRON )
{
   HWND              hwnd = hmg_par_raw_HWND( 1 ); // Handle to the rebar control
   HWND              tbHwnd;              // Handle to the toolbar window
   RECT              rcRb;                // Rectangle structure for the rebar band
   RECT              rcTB;                // Rectangle structure for the toolbar
   RECT              rcRR;                // Rectangle structure for the rebar control
   RECT              rcCvr;               // Rectangle structure for the chevron
   int               uBand;               // Index of the rebar band
   int               tx;                  // Number of hidden buttons in the toolbar
   int               dv;                  // Horizontal offset for window coordinates
   int               nBtn;                // Total number of buttons in the toolbar
   LPNMREBARCHEVRON  lpRB;                // Pointer to the NMREBARCHEVRON structure
   REBARBANDINFO     rbbi;                // Structure for rebar band information

   // Get the window rectangle of the rebar control
   GetWindowRect( hwnd, &rcRR );

   // Retrieve the NMREBARCHEVRON structure from the lParam
   lpRB = ( LPNMREBARCHEVRON ) HB_PARNL( 2 );
   uBand = lpRB->uBand;                   // Get the band index
   rcCvr = lpRB->rc;                      // Get the chevron rectangle

   // Retrieve the rectangle of the specified rebar band
   SendMessage( hwnd, RB_GETRECT, ( WPARAM ) uBand, ( LPARAM ) & rcRb );

   // Adjust the right edge of the band rectangle to exclude the chevron area
   rcRb.right -= ( rcCvr.right - rcCvr.left );

   // Initialize the REBARBANDINFO structure to get information about the band
   rbbi.cbSize = sizeof( REBARBANDINFO );
   rbbi.fMask = RBBIM_SIZE | RBBIM_CHILD | RBBIM_CHILDSIZE;

   // Retrieve information about the specified rebar band
   SendMessage( hwnd, RB_GETBANDINFO, ( WPARAM ) uBand, ( LPARAM ) ( LPREBARBANDINFO ) & rbbi );

   // Get the handle to the toolbar window within the rebar band
   tbHwnd = ( HWND ) rbbi.hwndChild;

   // Get the window rectangle of the toolbar
   GetWindowRect( ( HWND ) tbHwnd, &rcTB );

   // Calculate the horizontal offset for converting between screen and client coordinates
   dv = rcTB.left - rcRR.left + 1;

   // Get the total number of buttons in the toolbar
   nBtn = ( INT ) SendMessage( ( HWND ) tbHwnd, TB_BUTTONCOUNT, 0, 0 );

   // Determine the number of hidden buttons in the toolbar
   tx = TestHidenBtn( ( HWND ) tbHwnd, rcRb, dv, nBtn );

   // Prepare to return an array with 7 elements
   hb_reta( 7 );

   // Store the coordinates of the chevron rectangle and other information in the array
   HB_STORVNL( rcCvr.left, -1, 1 );       // Left coordinate of the chevron rectangle
   HB_STORVNL( rcCvr.top, -1, 2 );        // Top coordinate of the chevron rectangle
   HB_STORVNL( rcCvr.right, -1, 3 );      // Right coordinate of the chevron rectangle
   HB_STORVNL( rcCvr.bottom, -1, 4 );     // Bottom coordinate of the chevron rectangle
   hmg_storvnl_HANDLE( tbHwnd, -1, 5 );   // Handle to the toolbar window
   HB_STORNI( tx, -1, 6 );                // Number of hidden buttons in the toolbar
   HB_STORNI( nBtn, -1, 7 );              // Total number of buttons in the toolbar
}

/*
 * FUNCTION: GETBUTTONBAR
 *
 * Retrieves information about a specific button in a toolbar.
 *
 * This function gets the details of a button in a toolbar, such as its bitmap index,
 * command ID, and whether it is a separator or enabled. The information is returned
 * in an array.
 *
 * Parameters:
 *   hwndTB: Handle to the toolbar.
 *   index: Index of the button to retrieve information about.
 *
 * Returns:
 *   An array containing the following elements:
 *   1. The bitmap index of the button.
 *   2. The command ID of the button.
 *   3. A logical value indicating if the button is a separator.
 *   4. A logical value indicating if the button is enabled.
 */
HB_FUNC( GETBUTTONBAR )
{
   TBBUTTON lpBtn;                     // Structure to receive button information
   BOOL     lSep;                      // Flag indicating if the button is a separator
   BOOL     lEnable;                   // Flag indicating if the button is enabled

   // Retrieve information about the specified button
   SendMessage( hmg_par_raw_HWND( 1 ), TB_GETBUTTON, ( WPARAM ) hb_parni( 2 ), ( LPARAM ) & lpBtn );

   // Determine if the button is a separator
   lSep = ( lpBtn.fsStyle & TBSTYLE_SEP ) ? TRUE : FALSE;

   // Determine if the button is enabled
   lEnable = ( lpBtn.fsState & TBSTATE_ENABLED ) ? TRUE : FALSE;

   // Prepare to return an array with 4 elements
   hb_reta( 4 );

   // Store the button information in the array
   HB_STORNI( lpBtn.iBitmap, -1, 1 );  // Bitmap index of the button
   HB_STORNI( lpBtn.idCommand, -1, 2 );   // Command ID of the button
   HB_STORL( lSep, -1, 3 );               // Logical value indicating if the button is a separator
   HB_STORL( lEnable, -1, 4 );            // Logical value indicating if the button is enabled
}

/*
 * FUNCTION: GETIMAGELIST
 *
 * Retrieves an image from an image list associated with a toolbar.
 *
 * This function gets a specific image from the image list of a toolbar and registers
 * the image as a resource. The handle to the image is then returned.
 *
 * Parameters:
 *   hwndTB: Handle to the toolbar.
 *   index: Index of the image to retrieve.
 *
 * Returns:
 *   The handle to the retrieved image.
 */
HB_FUNC( GETIMAGELIST )
{
   HIMAGELIST  himl;       // Handle to the image list
   HBITMAP     himage;     // Handle to the image
   IMAGEINFO   ImageInfo;  // Structure to receive image information

   // Get the handle to the image list associated with the toolbar
   himl = ( HIMAGELIST ) SendMessage( hmg_par_raw_HWND( 1 ), TB_GETIMAGELIST, 0, 0 );

   // Retrieve information about the specified image
   ImageList_GetImageInfo( himl, hmg_par_INT( 2 ), &ImageInfo );

   // Get the handle to the image
   himage = ImageInfo.hbmImage;

   // Register the image as a resource
   RegisterResource( himage, "BMP" );

   // Return the handle to the image
   hmg_ret_raw_HANDLE( himage );
}

/*
 * FUNCTION: SETCHEVRONIMAGE
 *
 * Sets the bitmaps for a menu item in a menu.
 *
 * This function sets both the normal and selected bitmaps for a specified menu item.
 *
 * Parameters:
 *   hMenu: Handle to the menu.
 *   item: Identifier of the menu item.
 *   himage: Handle to the bitmap to set.
 *
 * Returns:
 *   None.
 */
HB_FUNC( SETCHEVRONIMAGE )
{
   HBITMAP  himage = hmg_par_raw_HBITMAP( 3 );  // Handle to the bitmap

   // Set the bitmaps for the specified menu item
   SetMenuItemBitmaps( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND, himage, himage );
}

/*
 * FUNCTION: DESTROYMENU
 *
 * Destroys a specified menu and frees any memory that the menu occupies.
 *
 * Parameters:
 *   hMenu: Handle to the menu to destroy.
 *
 * Returns:
 *   None.
 */
HB_FUNC( DESTROYMENU )
{
   // Destroy the specified menu
   DestroyMenu( hmg_par_raw_HMENU( 1 ) );
}

/*
 * FUNCTION: ADJUSTFLOATTOOLBAR
 *
 * Adjusts the size and position of a floating toolbar.
 *
 * This function calculates the appropriate size and position for a floating toolbar
 * and moves it to the calculated position.
 *
 * Parameters:
 *   hwndParent: Handle to the parent window.
 *   hwndTB: Handle to the toolbar window.
 *   hwndFloatTB: Handle to the floating toolbar window.
 *
 * Returns:
 *   None.
 */
HB_FUNC( ADJUSTFLOATTOOLBAR )
{
   HWND  hwndTB;                                   // Handle to the toolbar window
   RECT  rc;                                       // Rectangle structure for button dimensions
   int   nbuttons;                                 // Number of buttons in the toolbar
   int   height;                                   // Calculated height for the toolbar window
   int   width;                                    // Calculated width for the toolbar window
   POINT pt;                                       // Point structure for initial position

   // Get the handle to the toolbar window
   hwndTB = hmg_par_raw_HWND( 3 );

   // Retrieve the dimensions of a button in the toolbar
   SendMessage( hwndTB, TB_GETITEMRECT, 0, ( LPARAM ) & rc );

   // Get the number of buttons in the toolbar
   nbuttons = ( int ) SendMessage( hwndTB, TB_BUTTONCOUNT, 0, 0 );

   // Calculate the height of the toolbar window
   height = rc.bottom + GetSystemMetrics( SM_CYCAPTION ) + GetSystemMetrics( SM_CYFRAME ) + 2 * GetSystemMetrics( SM_CYDLGFRAME );
   height += 2 * GetSystemMetrics( SM_CYBORDER );

   // Calculate the width of the toolbar window
   width = ( nbuttons ) * rc.right;
   width += 2 * GetSystemMetrics( SM_CXDLGFRAME );

   // Set the initial position for the toolbar window
   pt.x = pt.y = 50;

   // Convert the initial position from parent window coordinates to screen coordinates
   MapWindowPoints( hmg_par_raw_HWND( 1 ), HWND_DESKTOP, ( LPPOINT ) & pt, 1 );

   // Move the toolbar window to the calculated position and size
   MoveWindow( hmg_par_raw_HWND( 2 ), pt.x, pt.y, width, height, TRUE );
}

/*
 * FUNCTION: ResizeToolbar
 *
 * Resizes a toolbar to fit within a specified width.
 *
 * This function calculates the number of rows needed for the toolbar buttons to fit
 * within the specified width and adjusts the toolbar accordingly.
 *
 * Parameters:
 *   hwndTB: Handle to the toolbar window.
 *   widthTb: The width within which the toolbar should fit.
 *
 * Returns:
 *   1 if the toolbar was successfully resized, 0 otherwise.
 */
int ResizeToolbar( HWND hwndTB, int widthTb )
{
   RECT  rcb, rc;                                  // Rectangle structures for window dimensions
   int   n, width, height, nrow;                   // Variables for calculations
   HWND  hwndParent;                               // Handle to the parent window
   DWORD style;                                    // Window style
   int   nButtons, bwidth, nBtnRow;                // Variables for button calculations
   int   heightTB;                                 // Current height of the toolbar

   // Retrieve the handle to the toolbar window and the target width
   hwndTB = hmg_par_raw_HWND( 1 );
   widthTb = hb_parni( 2 );

   // Get the dimensions of a button in the toolbar
   SendMessage( hwndTB, TB_GETITEMRECT, 0, ( LPARAM ) & rc );
   bwidth = rc.right;

   // Check if the target width is less than the button width
   if( widthTb < bwidth )
   {
      return 0;
   }

   // Get the current window rectangle of the toolbar
   GetWindowRect( hwndTB, &rc );
   heightTB = rc.bottom - rc.top;

   // Get the number of buttons in the toolbar
   nButtons = ( int ) SendMessage( hwndTB, TB_BUTTONCOUNT, 0, 0 );

   // Initialize the rectangle structure for calculations
   memset( &rcb, 0, sizeof( RECT ) );

   // Calculate the number of buttons that can fit in a single row
   if( bwidth > 0 )
   {
      n = widthTb / ( bwidth );
   }
   else
   {
      return 0;
   }

   // Calculate the number of rows needed
   if( nButtons % n == 0 )
   {
      nrow = nButtons / n;
   }
   else
   {
      nrow = nButtons / n + 1;
   }

   // Set the number of rows and adjust the toolbar size
   SendMessage( hwndTB, TB_SETROWS, ( WPARAM ) MAKEWPARAM( nrow, TRUE ), ( LPARAM ) & rcb );
   SendMessage( hwndTB, TB_AUTOSIZE, 0, 0 );

   // Get the handle to the parent window and its style
   hwndParent = GetParent( hwndTB );
   style = GetWindowLong( hwndParent, GWL_STYLE );

   // Adjust the window rectangle for the parent window style
   AdjustWindowRect( &rcb, style, 0 );

   // Convert the rectangle coordinates from parent window coordinates to screen coordinates
   MapWindowPoints( hwndParent, HWND_DESKTOP, ( LPPOINT ) & rcb, 2 );

   // Calculate the number of buttons per row
   nBtnRow = nButtons / ( nrow );
   if( nrow > 1 )
   {
      nBtnRow += nButtons & 1;
   }

   // Calculate the new width and height for the parent window
   width = nBtnRow * bwidth;
   width += 2 * GetSystemMetrics( SM_CXDLGFRAME );
   width += 2 * GetSystemMetrics( SM_CXBORDER );
   height = rcb.bottom - rcb.top;
   height += 2 * GetSystemMetrics( SM_CYBORDER );

   // Move the parent window if the size has changed
   if( !( width == widthTb ) || !( height == heightTB ) )
   {
      MoveWindow( hwndParent, rcb.left, rcb.top, width, height, TRUE );
   }

   return 1;
}

/*
 * FUNCTION: RESIZEFLOATTOOLBAR
 *
 * Resizes a floating toolbar if it is not already in the process of being resized.
 *
 * This function checks if the toolbar is already being resized to avoid recursive calls
 * and then calls the ResizeToolbar function to resize the toolbar.
 *
 * Parameters:
 *   hwndTB: Handle to the toolbar window.
 *   widthTb: The width within which the toolbar should fit.
 *
 * Returns:
 *   HB_TRUE if the toolbar was successfully resized, HB_FALSE otherwise.
 */
HB_FUNC( RESIZEFLOATTOOLBAR )
{
   HWND  hwndTB = hmg_par_raw_HWND( 1 );           // Handle to the toolbar window
   int   widthTb = hb_parni( 2 );                  // Target width for the toolbar

   // Check if the toolbar is already being resized
   if( isInSizeMsg )
   {
      hb_retl( HB_FALSE );
      return;
   }

   // Set the flag to indicate that the toolbar is being resized
   isInSizeMsg = 1;

   // Resize the toolbar if the handle is valid
   if( hwndTB )
   {
      ResizeToolbar( hwndTB, widthTb );
   }

   // Reset the flag to indicate that the toolbar is no longer being resized
   isInSizeMsg = 0;

   // Return success status
   hb_retl( HB_TRUE );
}

/*
 * FUNCTION: TOOLBAREXCUSTFUNC
 *
 * Handles customization notifications for a toolbar.
 *
 * This function processes various notifications related to toolbar customization,
 * such as beginning and ending adjustments, getting button information, and resetting the toolbar.
 *
 * Parameters:
 *   Msg: The message being processed.
 *   lParam: LPARAM value containing additional message information.
 *
 * Returns:
 *   HB_TRUE if the notification was handled successfully, HB_FALSE otherwise.
 */
HB_FUNC( TOOLBAREXCUSTFUNC )
{
   TBBUTTON    lpBtn;                              // Structure for button information
   UINT        Msg = hmg_par_UINT( 2 );            // Message being processed
   LPARAM      lParam = hmg_par_raw_LPARAM( 4 );   // Additional message information
   LPTBNOTIFY  lpTB = ( LPTBNOTIFY ) lParam;       // Pointer to the TBNOTIFY structure
   int         i; // Loop counter

   // Process the message based on its type
   switch( Msg )
   {
      case WM_NOTIFY:
         // Handle different notification codes
         switch( ( ( LPNMHDR ) lParam )->code )
         {
            case TBN_BEGINADJUST:
               // Start customizing the toolbar: save the current button count and button information
               nResetCount = ( int ) SendMessage( lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0 );
               buttonCount = nResetCount;
               lpSaveButtons = ( LPTBBUTTON ) GlobalAlloc( GPTR, sizeof( TBBUTTON ) * nResetCount );

               // Save information for each button
               for( i = 0; i < nResetCount; i++ )
               {
                  SendMessage( lpTB->hdr.hwndFrom, TB_GETBUTTON, i, ( LPARAM ) ( lpSaveButtons + i ) );
               }

               hb_retl( HB_TRUE );
               break;

            case TBN_GETBUTTONINFO:
               // Retrieve information for a specific button
               {
                  LPTBNOTIFY  lpTbNotify = ( LPTBNOTIFY ) lParam;

                  if( lpTbNotify->iItem >= buttonCount || lpTbNotify->iItem < 0 )
                  {
                     hb_retl( HB_FALSE );
                  }
                  else
                  {
                     SendMessage( lpTB->hdr.hwndFrom, TB_GETBUTTON, lpTbNotify->iItem, ( LPARAM ) & lpBtn );
                     lpTbNotify->tbButton = lpSaveButtons[lpTbNotify->iItem];
                     hb_retl( HB_TRUE );
                  }
               }
               break;

            case TBN_RESET:
               // Reset the toolbar to its original state
               {
                  int   nCount;

                  nCount = ( int ) SendMessage( lpTB->hdr.hwndFrom, TB_BUTTONCOUNT, 0, 0 );

                  // Remove all buttons
                  for( i = nCount - 1; i >= 0; i-- )
                  {
                     SendMessage( lpTB->hdr.hwndFrom, TB_DELETEBUTTON, i, 0 );
                  }

                  // Restore the original buttons
                  SendMessage( lpTB->hdr.hwndFrom, TB_ADDBUTTONS, ( WPARAM ) nResetCount, ( LPARAM ) lpSaveButtons );
                  hb_retl( HB_TRUE );
               }
               break;

            case TBN_ENDADJUST:
               // End customizing the toolbar: free the allocated memory for saved buttons
               GlobalFree( ( HGLOBAL ) lpSaveButtons );
               hb_retl( HB_TRUE );
               break;

            default:
               // Notification code not handled
               hb_retl( HB_FALSE );
         }
   }
}
