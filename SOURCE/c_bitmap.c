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

   Parts of this code are contributed for MiniGUI Project
   used here under permission of authors :

   Copyright 2005 (C) Andy Wos <andywos@unwired.com.au>
 + DrawGlyph()
   Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 + GetBitmapSize(), GetIconSize(), DrawGlyphMask()
   Copyright 2009 (C) Andi Jahja <harbour@cbn.net.id>
 + GetImageSize()
 ---------------------------------------------------------------------------*/

#include <mgdefs.h>
#include "hbapiitm.h"
#include "hbapifs.h"

#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Function to convert a bitmap to a DIB (Device Independent Bitmap)
HANDLE      DibFromBitmap( HBITMAP, HPALETTE );

// Function to get the number of colors in a DIB
WORD        GetDIBColors( LPSTR );

// Function to get the instance handle of the application resources
HINSTANCE   GetResources( void );

// Function to register a resource in the Minigui resource management system
void        RegisterResource( HANDLE hResource, LPCSTR szType );

/*
 * FUNCTION SaveWindowToBitmap( HWND hWnd, LPCTSTR lpFile, const RECT *rc )
 *
 * Captures the content of a window or a portion of it and saves it to a bitmap file.
 *
 * Parameters:
 *   hWnd: HWND - Handle of the window to capture.
 *   lpFile: LPCTSTR - Path and filename where the bitmap will be saved. Can be ANSI or Unicode depending on the build.
 *   rc: const RECT* - Pointer to a RECT structure defining the area to capture in client coordinates.
 *
 * Returns:
 *   BOOL - TRUE if the capture and save were successful, FALSE otherwise.
 *
 * Purpose:
 *   This function allows saving a snapshot of a window's content to a bitmap file. It's useful for creating screenshots
 *   or capturing specific regions of a window for further processing or storage. The function handles the necessary
 *   device context creation, bitmap manipulation, and file writing operations.
 *
 * Notes:
 *   The RECT structure 'rc' must contain coordinates relative to the client area of the window.
 *   The function creates a new file or overwrites an existing one at the specified path.
 *   Error handling is implemented to ensure resources are released even if the process fails.
 */
static BOOL SaveWindowToBitmap( HWND hWnd, LPCTSTR lpFile, const RECT *rc )
{
   HDC                  hDC, hMemDC = NULL;
   HBITMAP              hBitmap = NULL, hOldBmp;
   HANDLE               hDIB = NULL;
   HANDLE               filehandle = INVALID_HANDLE_VALUE;
   LPBITMAPINFOHEADER   lpBI = NULL;
   BITMAPFILEHEADER     bmfHdr;
   DWORD                dwDIBSize, dwWritten, dwBmBitsSize;
   BOOL                 result = FALSE;
   HPALETTE             hPal = NULL;

   int                  width = rc->right - rc->left;
   int                  height = rc->bottom - rc->top;

   if( width <= 0 || height <= 0 )
   {
      return FALSE;
   }

   // Get DC for window (client area)
   hDC = GetDC( hWnd );
   if( !hDC )
   {
      goto cleanup;
   }

   // Create memory DC + compatible bitmap
   hMemDC = CreateCompatibleDC( hDC );
   if( !hMemDC )
   {
      goto cleanup;
   }

   hBitmap = CreateCompatibleBitmap( hDC, width, height );
   if( !hBitmap )
   {
      goto cleanup;
   }

   hOldBmp = ( HBITMAP ) SelectObject( hMemDC, hBitmap );
   if( !hOldBmp )
   {
      goto cleanup;
   }

   // Copy window area to memory DC
   if( !BitBlt( hMemDC, 0, 0, width, height, hDC, rc->left, rc->top, SRCCOPY ) )
   {
      goto cleanup;
   }

   // Restore previous object BEFORE we delete hBitmap later
   SelectObject( hMemDC, hOldBmp );

   // Convert bitmap to DIB
   hDIB = DibFromBitmap( hBitmap, hPal );
   if( !hDIB )
   {
      goto cleanup;
   }

   filehandle = CreateFile( lpFile, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL );
   if( filehandle == INVALID_HANDLE_VALUE )
   {
      goto cleanup;
   }

   lpBI = ( LPBITMAPINFOHEADER ) GlobalLock( hDIB );
   if( !lpBI || lpBI->biSize != sizeof( BITMAPINFOHEADER ) )
   {
      goto cleanup;
   }

   // Setup bitmap file header
   bmfHdr.bfType = 0x4D42;          // "BM"
   dwDIBSize = lpBI->biSize + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBQUAD ) );
   dwBmBitsSize = ( ( ( lpBI->biWidth * ( DWORD ) lpBI->biBitCount ) + 31 ) / 32 * 4 ) * lpBI->biHeight;
   dwDIBSize += dwBmBitsSize;
   lpBI->biSizeImage = dwBmBitsSize;

   bmfHdr.bfSize = dwDIBSize + sizeof( BITMAPFILEHEADER );
   bmfHdr.bfReserved1 = 0;
   bmfHdr.bfReserved2 = 0;
   bmfHdr.bfOffBits = sizeof( BITMAPFILEHEADER ) + lpBI->biSize + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBQUAD ) );

   // Write headers + DIB data
   if( !WriteFile( filehandle, &bmfHdr, sizeof( BITMAPFILEHEADER ), &dwWritten, NULL ) || dwWritten != sizeof( BITMAPFILEHEADER ) )
   {
      goto cleanup;
   }

   if( !WriteFile( filehandle, lpBI, dwDIBSize, &dwWritten, NULL ) || dwWritten != dwDIBSize )
   {
      goto cleanup;
   }

   result = TRUE;

cleanup:
   if( lpBI )
   {
      GlobalUnlock( hDIB );
   }

   if( filehandle != INVALID_HANDLE_VALUE )
   {
      CloseHandle( filehandle );
   }

   if( hDIB )
   {
      GlobalFree( hDIB );
   }

   if( hBitmap )
   {
      DeleteObject( hBitmap );
   }

   if( hMemDC )
   {
      DeleteDC( hMemDC );
   }

   if( hDC )
   {
      ReleaseDC( hWnd, hDC );
   }

   return result;
}

/*
 * FUNCTION SaveClientAreaToBitmap( HWND hWnd, LPCTSTR lpFile )
 *
 * Captures the client area of a window and saves it to a bitmap file.
 *
 * Parameters:
 *   hWnd: HWND - Handle of the window to capture.
 *   lpFile: LPCTSTR - Path and filename where the bitmap will be saved. Can be ANSI or Unicode depending on the build.
 *
 * Returns:
 *   BOOL - TRUE if the capture and save were successful, FALSE otherwise.
 *
 * Purpose:
 *   This function simplifies capturing the entire client area of a window by automatically retrieving the client rectangle
 *   and then calling SaveWindowToBitmap to perform the actual capture and save. It's a convenience function for capturing
 *   the visible content of a window without including the title bar or borders.
 */
static BOOL SaveClientAreaToBitmap( HWND hWnd, LPCTSTR lpFile )
{
   RECT  rc;
   if( !GetClientRect( hWnd, &rc ) )
   {
      return FALSE;
   }

   return SaveWindowToBitmap( hWnd, lpFile, &rc );
}

/*
 * FUNCTION SaveWindowAreaToBitmap( HWND hWnd, LPCTSTR lpFile )
 *
 * Captures the entire window area (including borders and title bar) and saves it to a bitmap file.
 *
 * Parameters:
 *   hWnd: HWND - Handle of the window to capture.
 *   lpFile: LPCTSTR - Path and filename where the bitmap will be saved. Can be ANSI or Unicode depending on the build.
 *
 * Returns:
 *   BOOL - TRUE if the capture and save were successful, FALSE otherwise.
 *
 * Purpose:
 *   This function captures the entire window, including the non-client area (title bar, borders, etc.). It retrieves the
 *   window rectangle, converts it to client coordinates, and then calls SaveWindowToBitmap to perform the actual capture
 *   and save. This is useful for capturing the complete visual representation of a window as seen on the screen.
 */
static BOOL SaveWindowAreaToBitmap( HWND hWnd, LPCTSTR lpFile )
{
   POINT ptTL;
   POINT ptBR;
   RECT  rcClient;
   RECT  rc;
   if( !GetWindowRect( hWnd, &rc ) )
   {
      return FALSE;
   }

   // Convert screen coords to client coords
   ptTL.x = rc.left;
   ptTL.y = rc.top;
   ptBR.x = rc.right;
   ptBR.y = rc.bottom;
   if( !ScreenToClient( hWnd, &ptTL ) || !ScreenToClient( hWnd, &ptBR ) )
   {
      return FALSE;
   }

   rcClient.left = ptTL.x;
   rcClient.top = ptTL.y;
   rcClient.right = ptBR.x;
   rcClient.bottom = ptBR.y;

   return SaveWindowToBitmap( hWnd, lpFile, &rcClient );
}

/*
 * FUNCTION SAVEWINDOWBYHANDLE( hWnd, lpFileName, top, left, bottom, right )
 *
 * Saves the client area or a specified portion of a window to a bitmap file.
 *
 * Parameters:
 *   1: hWnd: HWND - Handle of the window to capture.
 *   2: lpFileName: STRING - Path and filename where the bitmap will be saved.
 *   3: top: NUMERIC - Top coordinate of the capture area (optional, -1 for full client area).
 *   4: left: NUMERIC - Left coordinate of the capture area (optional, -1 for full client area).
 *   5: bottom: NUMERIC - Bottom coordinate of the capture area (optional, -1 for full client area).
 *   6: right: NUMERIC - Right coordinate of the capture area (optional, -1 for full client area).
 *
 * Returns:
 *   LOGICAL - .T. if the capture and save were successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a Harbour-callable interface to save a window's content to a bitmap file. It allows specifying
 *   a rectangular region within the client area to capture, or capturing the entire client area if no coordinates are provided.
 *   It uses the SaveWindowToBitmap and SaveClientAreaToBitmap functions to perform the actual capture and save operations.
 *
 * Notes:
 *   If top, left, bottom, and right are all -1, the entire client area is captured.
 *   Otherwise, the specified rectangular region is captured.
 */
HB_FUNC( SAVEWINDOWBYHANDLE )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
#ifndef UNICODE
   LPCSTR   lpFileName = hb_parc( 2 );
#else
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   RECT     rc;
   int      top = hb_parni( 3 ), left = hb_parni( 4 ), bottom = hb_parni( 5 ), right = hb_parni( 6 );

   BOOL     ok;
   if( top != -1 && left != -1 && bottom != -1 && right != -1 )
   {
      rc.top = top;
      rc.left = left;
      rc.bottom = bottom;
      rc.right = right;
      ok = SaveWindowToBitmap( hWnd, lpFileName, &rc );
   }
   else
   {
      ok = SaveClientAreaToBitmap( hWnd, lpFileName );
   }

#ifdef UNICODE
   hb_xfree( lpFileName );
#endif
   hb_retl( ok );
}

/*
 * FUNCTION WNDCOPY( hWnd, bRect, lpFileName )
 *
 * Saves either the entire window (including borders and title bar) or the client area to a bitmap file.
 *
 * Parameters:
 *   1: hWnd: HWND - Handle of the window to capture.
 *   2: bRect: LOGICAL - .T. to capture the entire window, .F. to capture only the client area.
 *   3: lpFileName: STRING - Path and filename where the bitmap will be saved.
 *
 * Returns:
 *   LOGICAL - .T. if the capture and save were successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a Harbour-callable interface to save either the entire window or just the client area to a bitmap file.
 *   It uses the SaveWindowAreaToBitmap and SaveClientAreaToBitmap functions to perform the actual capture and save operations,
 *   based on the value of the bRect parameter.
 */
HB_FUNC( WNDCOPY )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   BOOL     bRect = hb_parl( 2 );   // TRUE=full window, FALSE=client area
#ifndef UNICODE
   LPCSTR   lpFileName = hb_parc( 3 );
#else
   LPWSTR   lpFileName = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   BOOL     ok = bRect ? SaveWindowAreaToBitmap( hWnd, lpFileName ) : SaveClientAreaToBitmap( hWnd, lpFileName );

#ifdef UNICODE
   hb_xfree( lpFileName );
#endif
   hb_retl( ok );
}

/*
 * FUNCTION DibNumColors( pv )
 *
 * Determines the number of colors in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   pv: VOID FAR* - Pointer to the DIB data.
 *
 * Returns:
 *   WORD - The number of colors in the DIB.
 *
 * Purpose:
 *   This function analyzes the DIB header to determine the number of colors used in the bitmap.
 *   It checks the biClrUsed field (for BITMAPINFO headers) or calculates the number of colors based on the bits per pixel (for BITMAPCORE headers).
 *   This information is crucial for correctly interpreting the color palette of the DIB.
 */
WORD DibNumColors( VOID FAR *pv )
{
   int                  bits;
   LPBITMAPINFOHEADER   lpbi;
   LPBITMAPCOREHEADER   lpbc;

   lpbi = ( ( LPBITMAPINFOHEADER ) pv );
   lpbc = ( ( LPBITMAPCOREHEADER ) pv );

   /* With the BITMAPINFO format headers, the size of the palette
    * is in biClrUsed, whereas in the BITMAPCORE - style headers, it
    * is dependent on the bits per pixel (= 2 raised to the power of
    * bits/pixel).
    */
   if( lpbi->biSize != sizeof( BITMAPCOREHEADER ) )
   {
      if( lpbi->biClrUsed != 0 )
      {
         return ( WORD ) lpbi->biClrUsed;
      }

      bits = lpbi->biBitCount;
   }
   else
   {
      bits = lpbc->bcBitCount;
   }

   switch( bits )
   {
      case 1:
         return 2;

      case 4:
         return 16;

      case 8:
         return 256;

      default:
         /* A 24 bitcount DIB has no color table */
         return 0;
   }
}

/*
 * FUNCTION PaletteSize( pv )
 *
 * Determines the size, in bytes, of the color palette in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   pv: VOID FAR* - Pointer to the DIB data.
 *
 * Returns:
 *   WORD - The size of the palette in bytes.
 *
 * Purpose:
 *   This function calculates the size of the color palette based on the DIB header information.
 *   It uses the DibNumColors function to determine the number of colors and then multiplies it by the size of each color entry
 *   (RGBTRIPLE for BITMAPCORE headers, RGBQUAD for BITMAPINFO headers). This is essential for correctly allocating memory
 *   and accessing the color palette data.
 */
static WORD PaletteSize( VOID FAR *pv )
{
   LPBITMAPINFOHEADER   lpbi;
   WORD                 NumColors;

   lpbi = ( LPBITMAPINFOHEADER ) pv;

   NumColors = DibNumColors( lpbi );

   if( lpbi->biSize == sizeof( BITMAPCOREHEADER ) )
   {
      return ( WORD ) ( NumColors * sizeof( RGBTRIPLE ) );
   }
   else
   {
      return ( WORD ) ( NumColors * sizeof( RGBQUAD ) );
   }
}

/*
 * Macro WIDTHBYTES( i )
 *
 * Calculates the number of bytes required to store a scan line of a bitmap, rounded up to the nearest DWORD boundary.
 *
 * Parameters:
 *   i: int - The width of the scan line in bits.
 *
 * Returns:
 *   int - The number of bytes required to store the scan line.
 *
 * Purpose:
 *   This macro ensures that each scan line of a bitmap is aligned to a 4-byte boundary, which is a common requirement for
 *   bitmap storage and processing. It's used to calculate the size of the bitmap data buffer.
 */
#define WIDTHBYTES( i ) ( ( ( i ) + 31 ) / 32 * 4 )

/*
 * FUNCTION DibFromBitmap( hbm, hpal )
 *
 * Converts a bitmap (HBITMAP) to a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   hbm: HBITMAP - Handle to the bitmap to convert.
 *   hpal: HPALETTE - Handle to the palette to use for the DIB. If NULL, the default palette is used.
 *
 * Returns:
 *   HANDLE - Handle to the newly created DIB. Returns NULL on failure.
 *
 * Purpose:
 *   This function converts a device-dependent bitmap (HBITMAP) into a device-independent bitmap (DIB).
 *   DIBs are more portable and can be used in various contexts, such as saving to a file or displaying on different devices.
 *   The function allocates memory for the DIB, copies the bitmap data, and sets the appropriate header information.
 *
 * Notes:
 *   The function allocates memory using GlobalAlloc, which must be freed using GlobalFree when the DIB is no longer needed.
 *   The function uses GetDIBits to retrieve the bitmap data, which requires a compatible device context.
 */
HANDLE DibFromBitmap( HBITMAP hbm, HPALETTE hpal )
{
   BITMAP               bm;
   BITMAPINFOHEADER     bi;
   BITMAPINFOHEADER FAR *lpbi;
   DWORD                dwLen;
   WORD                 biBits;
   HANDLE               hdib;
   HANDLE               h;
   HDC                  hdc;
   HPALETTE             hOldPal;

   if( !hbm )
   {
      return NULL;
   }

   if( hpal == NULL )
   {
      hpal = ( HPALETTE ) GetStockObject( DEFAULT_PALETTE );
   }

   GetObject( hbm, sizeof( bm ), ( LPSTR ) &bm );

   biBits = ( WORD ) ( bm.bmPlanes * bm.bmBitsPixel );

   bi.biSize = sizeof( BITMAPINFOHEADER );
   bi.biWidth = bm.bmWidth;
   bi.biHeight = bm.bmHeight;
   bi.biPlanes = 1;
   bi.biBitCount = biBits;
   bi.biCompression = BI_RGB;
   bi.biSizeImage = 0;
   bi.biXPelsPerMeter = 0;
   bi.biYPelsPerMeter = 0;
   bi.biClrUsed = 0;
   bi.biClrImportant = 0;

   dwLen = bi.biSize + PaletteSize( &bi );

   hdc = GetDC( NULL );
   hOldPal = SelectPalette( hdc, hpal, FALSE );
   RealizePalette( hdc );

   hdib = GlobalAlloc( GHND, dwLen );

   if( !hdib )
   {
      SelectPalette( hdc, hOldPal, FALSE );
      ReleaseDC( NULL, hdc );
      return NULL;
   }

   lpbi = ( LPBITMAPINFOHEADER ) GlobalLock( hdib );

   memcpy( ( char * ) lpbi, ( char * ) &bi, sizeof( bi ) );

   /* Call GetDIBits with a NULL lpBits param, so it will calculate the
    * biSizeImage field for us
    */
   GetDIBits( hdc, hbm, 0L, ( DWORD ) bi.biHeight, ( LPBYTE ) NULL, ( LPBITMAPINFO ) lpbi, ( DWORD ) DIB_RGB_COLORS );

   memcpy( ( char * ) &bi, ( char * ) lpbi, sizeof( bi ) );
   GlobalUnlock( hdib );

   /* If the driver did not fill in the biSizeImage field, make one up */
   if( bi.biSizeImage == 0 )
   {
      bi.biSizeImage = WIDTHBYTES( ( DWORD ) bm.bmWidth * biBits ) * bm.bmHeight;
   }

   /* Realloc the buffer big enough to hold all the bits */
   dwLen = bi.biSize + PaletteSize( &bi ) + bi.biSizeImage;

   h = GlobalReAlloc( hdib, dwLen, 0 );
   if( h )
   {
      hdib = h;
   }
   else
   {
      GlobalFree( hdib );

      SelectPalette( hdc, hOldPal, FALSE );
      ReleaseDC( NULL, hdc );
      return NULL;
   }

   /* Call GetDIBits with a NON-NULL lpBits param, and actually get the
    * bits this time
    */
   lpbi = ( LPBITMAPINFOHEADER ) GlobalLock( hdib );

   if
   (
      GetDIBits
         (
            hdc,
            hbm,
            0L,
            ( DWORD ) bi.biHeight,
            ( LPBYTE ) lpbi + ( WORD ) lpbi->biSize + PaletteSize( lpbi ),
            ( LPBITMAPINFO ) lpbi,
            ( DWORD ) DIB_RGB_COLORS
         ) == 0
   )
   {
      GlobalUnlock( hdib );

      SelectPalette( hdc, hpal, FALSE );
      ReleaseDC( NULL, hdc );
      return NULL;
   }

   GlobalUnlock( hdib );
   SelectPalette( hdc, hpal, FALSE );
   ReleaseDC( NULL, hdc );

   return hdib;
}

/*
 * FUNCTION GetDIBColors( lpDIB )
 *
 * Gets the number of colors in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   lpDIB: LPSTR - Pointer to the DIB data.
 *
 * Returns:
 *   WORD - The number of colors in the DIB.
 *
 * Purpose:
 *   This function retrieves the number of colors used in a DIB by accessing the bcBitCount field of the BITMAPCOREHEADER structure.
 *   It's a simplified version of DibNumColors, specifically designed for BITMAPCOREHEADER format.
 */
WORD GetDIBColors( LPSTR lpDIB )
{
   WORD  wBitCount = ( ( LPBITMAPCOREHEADER ) lpDIB )->bcBitCount;

   return wBitCount;
}

/*
 * FUNCTION C_HASALPHA( hBitmap )
 *
 * Checks if a bitmap has an alpha channel (transparency).
 *
 * Parameters:
 *   1: hBitmap: HBITMAP - Handle to the bitmap to check.
 *
 * Returns:
 *   LOGICAL - .T. if the bitmap has an alpha channel, .F. otherwise.
 *
 * Purpose:
 *   This function determines if a given bitmap contains an alpha channel, indicating the presence of transparency.
 *   It converts the bitmap to a DIB, then iterates through the pixel data, checking the alpha component of each pixel.
 *   If any pixel has a non-zero alpha value, the function returns .T., indicating that the bitmap has an alpha channel.
 *
 * Notes:
 *   The function checks the device capabilities to ensure that the display supports 32-bit color depth (required for alpha channels).
 *   The function allocates memory for the DIB, which must be freed using GlobalFree when the DIB is no longer needed.
 */
HB_FUNC( C_HASALPHA )
{
   HANDLE   hDib;
   BOOL     bAlphaChannel = FALSE;
   HDC      hDC = GetDC( GetDesktopWindow() );

   if( GetDeviceCaps( hDC, BITSPIXEL ) < 32 )
   {
      ReleaseDC( GetDesktopWindow(), hDC );
      hb_retl( FALSE );
      return;
   }

   ReleaseDC( GetDesktopWindow(), hDC );

   hDib = DibFromBitmap( hmg_par_raw_HBITMAP( 1 ), ( HPALETTE ) NULL );

   if( hDib )
   {
      LPBITMAPINFO   lpbmi = ( LPBITMAPINFO ) GlobalLock( hDib );
      unsigned char  *uc = ( LPBYTE ) lpbmi + ( WORD ) lpbmi->bmiHeader.biSize + PaletteSize( lpbmi );
      unsigned long  ul;

      for( ul = 0; ul < lpbmi->bmiHeader.biSizeImage && !bAlphaChannel; ul += 4 )
      {
         if( uc[ul + 3] != 0 )
         {
            bAlphaChannel = TRUE;
         }
      }

      GlobalUnlock( hDib );
      GlobalFree( hDib );
   }

   hb_retl( bAlphaChannel );
}

/*
 * FUNCTION Icon2Bmp( hIcon )
 *
 * Converts an icon (HICON) to a bitmap (HBITMAP).
 *
 * Parameters:
 *   hIcon: HICON - Handle to the icon to convert.
 *
 * Returns:
 *   HBITMAP - Handle to the newly created bitmap.
 *
 * Purpose:
 *   This function converts an icon into a bitmap by drawing the icon onto a compatible bitmap.
 *   This is useful when you need to manipulate an icon as a bitmap, for example, to apply effects or save it to a file.
 *
 * Notes:
 *   The function creates a compatible device context and bitmap, draws the icon onto the bitmap, and then cleans up the resources.
 */
HBITMAP Icon2Bmp( HICON hIcon )
{
   HDC      hDC = GetDC( NULL );
   HDC      hMemDC = CreateCompatibleDC( hDC );
   ICONINFO icon;
   BITMAP   bitmap;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;

   GetIconInfo( hIcon, &icon );
   GetObject( icon.hbmColor, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   hBmp = CreateCompatibleBitmap( hDC, bitmap.bmWidth, bitmap.bmHeight );
   hOldBmp = ( HBITMAP ) SelectObject( hMemDC, hBmp );

   PatBlt( hMemDC, 0, 0, bitmap.bmWidth, bitmap.bmHeight, PATCOPY );
   DrawIconEx( hMemDC, 0, 0, hIcon, bitmap.bmWidth, bitmap.bmHeight, 0, NULL, DI_NORMAL );

   SelectObject( hMemDC, hOldBmp );
   DeleteDC( hMemDC );
   DeleteObject( icon.hbmMask );
   DeleteObject( icon.hbmColor );

   ReleaseDC( NULL, hDC );

   return hBmp;
}

/*
 * FUNCTION IconMask2Bmp( hIcon )
 *
 * Converts an icon's mask (HICON) to a bitmap (HBITMAP).
 *
 * Parameters:
 *   hIcon: HICON - Handle to the icon whose mask is to be converted.
 *
 * Returns:
 *   HBITMAP - Handle to the newly created bitmap representing the icon's mask.
 *
 * Purpose:
 *   This function extracts the mask from an icon and converts it into a bitmap. The mask defines the transparent areas of the icon.
 *   This is useful for drawing icons with transparency or for creating custom drawing routines that require the icon's mask.
 *
 * Notes:
 *   The function creates a compatible device context and bitmap, draws the icon's mask onto the bitmap, and then cleans up the resources.
 *   The resulting bitmap is a monochrome bitmap where white represents the transparent areas of the icon.
 */
HBITMAP IconMask2Bmp( HICON hIcon )
{
   HDC      hDC = GetDC( 0 );
   HDC      hMemDC = CreateCompatibleDC( hDC );
   ICONINFO icon;
   BITMAP   bitmap;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;

   GetIconInfo( hIcon, &icon );
   GetObject( icon.hbmColor, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   hBmp = CreateCompatibleBitmap( hDC, bitmap.bmWidth, bitmap.bmHeight );
   hOldBmp = ( HBITMAP ) SelectObject( hMemDC, hBmp );

   PatBlt( hMemDC, 0, 0, bitmap.bmWidth, bitmap.bmHeight, WHITENESS );
   DrawIconEx( hMemDC, 0, 0, hIcon, bitmap.bmWidth, bitmap.bmHeight, 0, NULL, DI_MASK );

   SelectObject( hMemDC, hOldBmp );
   DeleteDC( hMemDC );
   DeleteObject( icon.hbmMask );
   DeleteObject( icon.hbmColor );
   ReleaseDC( 0, hDC );

   return hBmp;
}

/*
 * FUNCTION DRAWGLYPH
 *
 * Draws a glyph (bitmap or icon) with optional transparency, disabled effects, and stretching.
 *
 * Parameters:
 *   1: hDC: HDC - Handle to the device context where the glyph will be drawn.
 *   2: x: INT - X coordinate of the top-left corner of the glyph.
 *   3: y: INT - Y coordinate of the top-left corner of the glyph.
 *   4: dx: INT - Width of the glyph. If 0, the original width of the bitmap/icon is used.
 *   5: dy: INT - Height of the glyph. If 0, the original height of the bitmap/icon is used.
 *   6: hBmp: HBITMAP - Handle to the bitmap or icon to draw.
 *   7: rgbTransparent: COLORREF - Color to treat as transparent. If NIL, the color of the top-left pixel is used.
 *   8: disabled: LOGICAL - .T. to draw the glyph with a disabled (grayed-out) effect, .F. otherwise.
 *   9: stretched: LOGICAL - .T. to stretch the glyph to fit the specified width and height, .F. otherwise.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   This function provides a flexible way to draw bitmaps and icons with various options, including transparency, disabled effects, and stretching.
 *   It handles both bitmaps and icons, automatically converting icons to bitmaps if necessary. It uses a mask to achieve transparency,
 *   allowing for drawing glyphs with irregular shapes. The disabled effect is achieved by drawing a shadow and highlight, creating a 3D effect.
 *
 * Notes:
 *   The function creates several temporary device contexts and bitmaps, which are deleted at the end of the function to avoid memory leaks.
 *   The function uses BitBlt and StretchBlt to perform the drawing operations, which are optimized for performance.
 */
HB_FUNC( DRAWGLYPH )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );
   int      x = hb_parni( 2 );
   int      y = hb_parni( 3 );
   int      dx = hb_parni( 4 );
   int      dy = hb_parni( 5 );
   HBITMAP  hBmp = hmg_par_raw_HBITMAP( 6 );
   COLORREF rgbTransparent = RGB( 255, 255, 255 );
   BOOL     disabled = hb_parl( 8 );
   BOOL     stretched = HB_ISNIL( 9 ) ? FALSE : hb_parl( 9 );
   BOOL     bHasBkColor = !HB_ISNIL( 7 );

   HDC      hDCMem;
   HDC      hDCMask;
   HDC      hDCStretch;
   HDC      hDCNoBlink;

   HBITMAP  hBmpDefault;
   HBITMAP  hBmpTransMask;
   HBITMAP  hBmpStretch = NULL;
   HBITMAP  hBmpIcon = NULL;
   HBITMAP  hBmpNoBlink, hBmpNoBlinkOld;
   BITMAP   bitmap;
   ICONINFO icon;
   HBRUSH   hBr;
   HBRUSH   hOld;

   if( bHasBkColor )
   {
      rgbTransparent = hmg_par_COLORREF( 7 );
   }

   // Check if the handle is a bitmap
   if( ( UINT ) GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap ) != sizeof( BITMAP ) )
   {
      // Check if it is an icon
      if( !GetIconInfo( ( HICON ) hBmp, &icon ) )
      {
         return;
      }

      DeleteObject( icon.hbmMask );
      DeleteObject( icon.hbmColor );

      if( !icon.fIcon )
      {
         return;
      }

      if( !disabled && !stretched )
      {
         // Just draw the icon directly
         DrawIconEx( hDC, x, y, ( HICON ) hBmp, dx, dy, 0, NULL, DI_NORMAL );
         return;
      }
      else
      {
         if( !stretched )
         {
            // Convert icon to bitmap mask
            hBmp = IconMask2Bmp( ( HICON ) hBmp );
         }
         else
         {
            // Convert icon to bitmap
            hBmp = Icon2Bmp( ( HICON ) hBmp );
         }

         hBmpIcon = hBmp;

         // Ignore the user-provided color
         rgbTransparent = RGB( 255, 255, 255 );
         bHasBkColor = TRUE;
         GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap );
      }
   }

   hDCMem = CreateCompatibleDC( hDC );

   if( stretched )
   {
      dx = ( dx > 0 ? dx : bitmap.bmWidth );
      dy = ( dy > 0 ? dy : bitmap.bmHeight );
      hBmpStretch = CreateCompatibleBitmap( hDC, dx, dy );
      SelectObject( hDCMem, hBmpStretch );
      hDCStretch = CreateCompatibleDC( hDC );
      hBmpDefault = ( HBITMAP ) SelectObject( hDCStretch, hBmp );

      StretchBlt( hDCMem, 0, 0, dx, dy, hDCStretch, 0, 0, bitmap.bmWidth, bitmap.bmHeight, SRCCOPY );

      SelectObject( hDCStretch, hBmpDefault );
      DeleteDC( hDCStretch );
   }
   else
   {
      dx = ( dx > 0 ? HB_MIN( dx, bitmap.bmWidth ) : bitmap.bmWidth );
      dy = ( dy > 0 ? HB_MIN( dy, bitmap.bmHeight ) : bitmap.bmHeight );
      hBmpDefault = ( HBITMAP ) SelectObject( hDCMem, hBmp );
   }

   // Prime the "no blink" device context
   hDCNoBlink = CreateCompatibleDC( hDC );
   hBmpNoBlink = CreateCompatibleBitmap( hDC, dx, dy );
   hBmpNoBlinkOld = ( HBITMAP ) SelectObject( hDCNoBlink, hBmpNoBlink );
   BitBlt( hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY );
   SetBkColor( hDCNoBlink, RGB( 255, 255, 255 ) );                // White
   SetTextColor( hDCNoBlink, RGB( 0, 0, 0 ) );                    // Black

   // Was background color given?
   // No? Get the color automatically
   if( !bHasBkColor )
   {
      rgbTransparent = GetPixel( hDCMem, 0, 0 );
   }

   // Build mask based on transparent color
   hDCMask = CreateCompatibleDC( hDCNoBlink );
   hBmpTransMask = CreateBitmap( dx, dy, 1, 1, NULL );
   SelectObject( hDCMask, hBmpTransMask );
   SetBkColor( hDCMem, rgbTransparent );
   BitBlt( hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY );

   if( disabled )
   {
      hBr = CreateSolidBrush( GetSysColor( COLOR_BTNHIGHLIGHT ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );

      hBr = CreateSolidBrush( GetSysColor( COLOR_BTNSHADOW ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 0, 0, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );
   }
   else
   {
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT );
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMask, 0, 0, SRCAND );
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT );
   }

   BitBlt( hDC, x, y, dx, dy, hDCNoBlink, 0, 0, SRCCOPY );

   // Clean up
   SelectObject( hDCMem, hBmpDefault );
   SelectObject( hDCMask, hBmpDefault );
   SelectObject( hDCNoBlink, hBmpNoBlinkOld );
   DeleteDC( hDCMem );
   DeleteDC( hDCMask );
   DeleteDC( hDCNoBlink );
   DeleteObject( hBmpTransMask );
   DeleteObject( hBmpNoBlink );

   if( stretched )
   {
      DeleteObject( hBmpStretch );
   }

   if( hBmpIcon )
   {
      DeleteObject( hBmpIcon );
   }
}

/*
 * FUNCTION: DRAWGLYPHMASK
 *
 * Creates and draws a bitmap mask with the first pixel treated as the transparent color.
 *
 * Parameters:
 *   1: HDC - Device context handle.
 *   2: INT - X coordinate.
 *   3: INT - Y coordinate.
 *   4: INT - Width.
 *   5: INT - Height.
 *   6: HBITMAP - Handle to the bitmap.
 *   7: COLORREF - Transparent color.
 *   10: HWND - Handle to the window.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Creates and draws a bitmap mask with the first pixel treated as the transparent color.
 */
HB_FUNC( DRAWGLYPHMASK )
{
   HDC      hDC = hmg_par_raw_HDC( 1 );
   int      dx = hb_parni( 4 );
   int      dy = hb_parni( 5 );
   HBITMAP  hBmp = hmg_par_raw_HBITMAP( 6 );
   COLORREF rgbTransparent;
   HWND     hwnd = hmg_par_raw_HWND( 10 );

   HDC      hDCMem;
   HDC      hDCMask;

   HBITMAP  hBmpDefault;
   HBITMAP  hBmpTransMask;

   BITMAP   bitmap;

   GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap );

   SetBkColor( hDC, RGB( 255, 255, 255 ) );                       // White
   SetTextColor( hDC, RGB( 0, 0, 0 ) );                           // Black
   hDCMem = CreateCompatibleDC( hDC );

   dx = ( dx > 0 ? HB_MIN( dx, bitmap.bmWidth ) : bitmap.bmWidth );
   dy = ( dy > 0 ? HB_MIN( dy, bitmap.bmHeight ) : bitmap.bmHeight );
   hBmpDefault = ( HBITMAP ) SelectObject( hDCMem, hBmp );
   rgbTransparent = GetPixel( hDCMem, 0, 0 );

   // Build mask based on transparent color
   hDCMask = CreateCompatibleDC( hDC );
   hBmpTransMask = CreateBitmap( dx, dy, 1, 1, NULL );

   SelectObject( hDCMask, hBmpTransMask );
   SetBkColor( hDCMem, rgbTransparent );
   BitBlt( hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY );

   // Handle to bitmaped button mask
   if( hwnd != NULL )
   {
      SendMessage( hwnd, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) hBmpTransMask );
   }

   SelectObject( hDCMem, hBmpDefault );
   SelectObject( hDCMask, hBmpDefault );
   DeleteDC( hDCMem );
   DeleteDC( hDCMask );
}

/*
 * FUNCTION: LOADBITMAP
 *
 * Loads a bitmap from a resource or file.
 *
 * Parameters:
 *   1: LPCSTR/LPCWSTR - Name of the bitmap resource or file.
 *
 * Returns:
 *   HBITMAP - Handle to the loaded bitmap.
 *
 * Purpose:
 *   Loads a bitmap from a resource or file and registers it as a resource.
 */
HB_FUNC( LOADBITMAP )
{
#ifndef UNICODE
   LPCSTR   lpImageName = hb_parc( 1 );                           // Bitmap name (ANSI)
#else
   LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 1 ) );  // Bitmap name (Unicode)
#endif
   HBITMAP  hBitmap;

   // Load the bitmap from resources
   hBitmap = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );

   // If not found in resources, load from file
   if( hBitmap == NULL )
   {
      hBitmap = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   RegisterResource( hBitmap, "BMP" );             // Register the bitmap resource
   hmg_ret_raw_HANDLE( hBitmap );                  // Return the handle to the loaded bitmap
#ifdef UNICODE
   hb_xfree( lpImageName );                        // Free the allocated memory for the Unicode bitmap name
#endif
}

/*
 * FUNCTION: DrawGlyph
 *
 * Draws a glyph (bitmap or icon) with optional transparency and disabled effects at the C level.
 *
 * Parameters:
 *   hDC: HDC - Device context handle.
 *   x: INT - X coordinate.
 *   y: INT - Y coordinate.
 *   dx: INT - Width.
 *   dy: INT - Height.
 *   hBmp: HBITMAP - Handle to the bitmap.
 *   rgbTransparent: COLORREF - Transparent color.
 *   disabled: BOOL - Flag to indicate if the glyph is disabled.
 *   stretched: BOOL - Flag to indicate if the glyph should be stretched.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Draws a glyph (bitmap or icon) with optional transparency and disabled effects at the C level.
 */
VOID DrawGlyph( HDC hDC, int x, int y, int dx, int dy, HBITMAP hBmp, COLORREF rgbTransparent, BOOL disabled, BOOL stretched )
{
   HDC      hDCMem, hDCMask, hDCStretch, hDCNoBlink;
   HBITMAP  hBmpDefault, hBmpTransMask, hBmpStretch = NULL;
   HBITMAP  hBmpIcon = NULL;
   HBITMAP  hBmpNoBlink, hBmpNoBlinkOld;
   BITMAP   bitmap;
   ICONINFO icon;
   HBRUSH   hBr, hOld;
   BOOL     bHasBkColor = !HB_ISNIL( 7 );

   // Check if the handle is a bitmap
   if( ( UINT ) GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap ) != sizeof( BITMAP ) )
   {
      // Check if it is an icon
      if( !GetIconInfo( ( HICON ) hBmp, &icon ) )
      {
         return;
      }

      DeleteObject( icon.hbmMask );
      DeleteObject( icon.hbmColor );

      if( !icon.fIcon )
      {
         return;
      }

      if( !disabled && !stretched )
      {
         // Just draw the icon directly
         DrawIconEx( hDC, x, y, ( HICON ) hBmp, dx, dy, 0, NULL, DI_NORMAL );
         return;
      }
      else
      {
         if( !stretched )
         {
            // Convert icon to bitmap mask
            hBmp = IconMask2Bmp( ( HICON ) hBmp );
         }
         else
         {
            // Convert icon to bitmap
            hBmp = Icon2Bmp( ( HICON ) hBmp );
         }

         hBmpIcon = hBmp;

         // Ignore the user-provided color
         rgbTransparent = RGB( 255, 255, 255 );
         bHasBkColor = TRUE;
         GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap );
      }
   }

   hDCMem = CreateCompatibleDC( hDC );

   if( stretched )
   {
      dx = ( dx > 0 ? dx : bitmap.bmWidth );
      dy = ( dy > 0 ? dy : bitmap.bmHeight );

      hBmpStretch = CreateCompatibleBitmap( hDC, dx, dy );
      SelectObject( hDCMem, hBmpStretch );
      hDCStretch = CreateCompatibleDC( hDC );
      hBmpDefault = ( HBITMAP ) SelectObject( hDCStretch, hBmp );

      StretchBlt( hDCMem, 0, 0, dx, dy, hDCStretch, 0, 0, bitmap.bmWidth, bitmap.bmHeight, SRCCOPY );

      SelectObject( hDCStretch, hBmpDefault );
      DeleteDC( hDCStretch );
   }
   else
   {
      dx = ( dx > 0 ? HB_MIN( dx, bitmap.bmWidth ) : bitmap.bmWidth );
      dy = ( dy > 0 ? HB_MIN( dy, bitmap.bmHeight ) : bitmap.bmHeight );
      hBmpDefault = ( HBITMAP ) SelectObject( hDCMem, hBmp );
   }

   // Prime the "no blink" device context
   hDCNoBlink = CreateCompatibleDC( hDC );
   hBmpNoBlink = CreateCompatibleBitmap( hDC, dx, dy );
   hBmpNoBlinkOld = ( HBITMAP ) SelectObject( hDCNoBlink, hBmpNoBlink );
   BitBlt( hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY );
   SetBkColor( hDCNoBlink, RGB( 255, 255, 255 ) ); // White
   SetTextColor( hDCNoBlink, RGB( 0, 0, 0 ) );     // Black

   // Was background color given?
   // No? Get the color automatically
   if( !bHasBkColor )
   {
      rgbTransparent = GetPixel( hDCMem, 0, 0 );
   }

   // Build mask based on transparent color
   hDCMask = CreateCompatibleDC( hDCNoBlink );
   hBmpTransMask = CreateBitmap( dx, dy, 1, 1, NULL );
   SelectObject( hDCMask, hBmpTransMask );
   SetBkColor( hDCMem, rgbTransparent );
   BitBlt( hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY );

   if( disabled )
   {
      hBr = CreateSolidBrush( GetSysColor( COLOR_BTNHIGHLIGHT ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );

      hBr = CreateSolidBrush( GetSysColor( COLOR_BTNSHADOW ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 0, 0, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );
   }
   else
   {
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT );
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMask, 0, 0, SRCAND );
      BitBlt( hDCNoBlink, 0, 0, dx, dy, hDCMem, 0, 0, SRCINVERT );
   }

   BitBlt( hDC, x, y, dx, dy, hDCNoBlink, 0, 0, SRCCOPY );

   // Clean up
   SelectObject( hDCMem, hBmpDefault );
   SelectObject( hDCMask, hBmpDefault );
   SelectObject( hDCNoBlink, hBmpNoBlinkOld );
   DeleteDC( hDCMem );
   DeleteDC( hDCMask );
   DeleteDC( hDCNoBlink );
   DeleteObject( hBmpTransMask );
   DeleteObject( hBmpNoBlink );

   if( stretched )
   {
      DeleteObject( hBmpStretch );
   }

   if( hBmpIcon )
   {
      DeleteObject( hBmpIcon );
   }
}

/*
 * FUNCTION: GetImageSize
 *
 * Purpose: Retrieves dimensions of an image file (JPG, GIF, PNG).
 *
 * Parameters:
 *   fn: const char* - File name of the image.
 *   x: int* - Pointer to store width.
 *   y: int* - Pointer to store height.
 *
 * Returns: BOOL - TRUE if successful, FALSE otherwise.
 */
BOOL GetImageSize( const char *fn, int *x, int *y )
{
   unsigned char  buf[24];
   FILE           *f;

   *x = *y = 0;

   if( !fn || !*fn )
   {
      return FALSE;
   }

   // Open file in binary read mode
   f = fopen( fn, "rb" );
   if( !f )
   {
      return FALSE;
   }

   // Read first 24 bytes
   if( fread( buf, 1, 24, f ) != 24 )
   {
      fclose( f );
      return FALSE;
   }

   // JPEG: Check signature (FFD8)
   if( buf[0] == 0xFF && buf[1] == 0xD8 )
   {
      unsigned char  marker[4];
      int            segLength;

      // Move past SOI
      fseek( f, 2, SEEK_SET );

      while( fread( marker, 1, 4, f ) == 4 )
      {
         if( marker[0] != 0xFF )
         {
            break;   // invalid marker
         }

         segLength = ( marker[2] << 8 ) | marker[3];
         if( segLength < 2 )
         {
            break;   // corrupt segment
         }

         // SOF markers
         if( marker[1] == 0xC0 || marker[1] == 0xC1 || marker[1] == 0xC2 || marker[1] == 0xC3 || marker[1] == 0xC9 || marker[1] == 0xCA || marker[1] == 0xCB )
         {
            unsigned char  dims[5];
            if( fread( dims, 1, 5, f ) == 5 )
            {
               *y = ( dims[1] << 8 ) | dims[2];
               *x = ( dims[3] << 8 ) | dims[4];
               fclose( f );
               return TRUE;
            }
            break;
         }
         else
         {
            // Skip rest of segment
            fseek( f, segLength - 2, SEEK_CUR );
         }
      }

      fclose( f );
      return FALSE;  // no SOF found
   }

   // GIF: Check "GIF" signature
   if( buf[0] == 'G' && buf[1] == 'I' && buf[2] == 'F' )
   {
      *x = buf[6] + ( buf[7] << 8 );
      *y = buf[8] + ( buf[9] << 8 );
      fclose( f );
      return TRUE;
   }

   // PNG: Check PNG signature and IHDR chunk
   if
   (
      buf[0] == 0x89
   && buf[1] == 'P'
   && buf[2] == 'N'
   && buf[3] == 'G'
   && buf[4] == 0x0D
   && buf[5] == 0x0A
   && buf[6] == 0x1A
   && buf[7] == 0x0A
   && buf[12] == 'I'
   && buf[13] == 'H'
   && buf[14] == 'D'
   && buf[15] == 'R'
   )
   {
      *x = ( buf[16] << 24 ) + ( buf[17] << 16 ) + ( buf[18] << 8 ) + buf[19];
      *y = ( buf[20] << 24 ) + ( buf[21] << 16 ) + ( buf[22] << 8 ) + buf[23];
      fclose( f );
      return TRUE;
   }

   fclose( f );
   return FALSE;
}

/*
 * FUNCTION: HB_GETIMAGESIZE
 *
 * Purpose: Harbour wrapper for GetImageSize.
 *
 * Parameters: 1: LPCSTR - Image file name.
 *
 * Returns: Array - {width, height} or {0, 0} on failure.
 */
HB_FUNC( HB_GETIMAGESIZE )
{
   int   x = 0, y = 0;
   if( hb_parclen( 1 ) > 0 )
   {
      GetImageSize( hb_parcx( 1 ), &x, &y );
   }

   hb_reta( 2 );
   HB_STORNI( x, -1, 1 );
   HB_STORNI( y, -1, 2 );
}

/*
 * FUNCTION: _arraySet
 *
 * Helper function to set values in an array.
 *
 * Parameters:
 *   pArray: PHB_ITEM - Pointer to the array.
 *   Width: int - Width value.
 *   Height: int - Height value.
 *   BitsPixel: int - Bits per pixel value.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Sets values in an array for width, height, and bits per pixel.
 */
static void _arraySet( PHB_ITEM pArray, int Width, int Height, int BitsPixel )
{
   HB_arraySetNL( pArray, 1, Width );
   HB_arraySetNL( pArray, 2, Height );
   HB_arraySetNL( pArray, 3, BitsPixel );
}

/*
 * FUNCTION: GETBITMAPSIZE
 *
 * Gets the size of a bitmap.
 *
 * Parameters:
 *   1: LPCSTR/LPCWSTR - Name of the bitmap file or resource, or handle to the bitmap.
 *
 * Returns:
 *   Array - Array containing the width, height, and bits per pixel of the bitmap.
 *
 * Purpose:
 *   Retrieves the width, height, and bits per pixel of a bitmap from a file, resource, or handle.
 */
HB_FUNC( GETBITMAPSIZE )
{
   PHB_ITEM pResult = hb_itemArrayNew( 3 );
   HBITMAP  hBitmap = NULL;
   BOOL     bDelete = TRUE;

   if( hb_parclen( 1 ) > 0 )
   {
#ifndef UNICODE
      LPCSTR   lpImageName = hb_parc( 1 );
#else
      LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
      hBitmap = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );

      if( hBitmap == NULL )
      {
         hBitmap = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );
      }

#ifdef UNICODE
      hb_xfree( lpImageName );
#endif
   }
   else
   {
      if( GetObjectType( hmg_par_raw_HGDIOBJ( 1 ) ) == OBJ_BITMAP )
      {
         hBitmap = hmg_par_raw_HBITMAP( 1 );
         bDelete = FALSE;
      }
   }

   _arraySet( pResult, 0, 0, 4 );

   if( hBitmap != NULL )
   {
      BITMAP   bm;

      if( GetObject( hBitmap, sizeof( BITMAP ), &bm ) )
      {
         _arraySet( pResult, bm.bmWidth, bm.bmHeight, bm.bmBitsPixel );
      }

      if( bDelete )
      {
         DeleteObject( hBitmap );
      }
   }

   hb_itemReturnRelease( pResult );
}

/*
 * FUNCTION: GETICONSIZE
 *
 * Gets the size of an icon.
 *
 * Parameters:
 *   1: HICON - Handle to the icon.
 *
 * Returns:
 *   Array - Array containing the width, height, and bits per pixel of the icon.
 *
 * Purpose:
 *   Retrieves the width, height, and bits per pixel of an icon.
 */
HB_FUNC( GETICONSIZE )
{
   PHB_ITEM pResult = hb_itemArrayNew( 3 );
   HICON    hIcon = hmg_par_raw_HICON( 1 );

   _arraySet( pResult, 0, 0, 4 );

   if( hIcon )
   {
      ICONINFO sIconInfo;

      if( GetIconInfo( hIcon, &sIconInfo ) )
      {
         BITMAP   bm;

         if( GetObject( sIconInfo.hbmColor, sizeof( BITMAP ), &bm ) )
         {
            _arraySet( pResult, bm.bmWidth, bm.bmHeight, bm.bmBitsPixel );
         }

         if( sIconInfo.hbmMask )
         {
            DeleteObject( sIconInfo.hbmMask );
         }

         if( sIconInfo.hbmColor )
         {
            DeleteObject( sIconInfo.hbmColor );
         }
      }
   }

   hb_itemReturnRelease( pResult );
}

/*
 * FUNCTION: GETPIXELCOLOR
 *
 * Gets the color of a pixel.
 *
 * Parameters:
 *   1: HDC - Device context handle.
 *   2: INT - X coordinate of the pixel.
 *   3: INT - Y coordinate of the pixel.
 *
 * Returns:
 *   Array - Array containing the red, green, and blue components of the pixel color.
 *
 * Purpose:
 *   Retrieves the red, green, and blue components of the color of a pixel at the specified coordinates.
 */
HB_FUNC( GETPIXELCOLOR )
{
   COLORREF pixel = GetPixel( hmg_par_raw_HDC( 1 ), hb_parni( 2 ), hb_parni( 3 ) );

   hmg_ret_L( pixel != CLR_INVALID );

   HB_STORNI( ( UINT ) GetRValue( pixel ), 4, 1 );
   HB_STORNI( ( UINT ) GetGValue( pixel ), 4, 2 );
   HB_STORNI( ( UINT ) GetBValue( pixel ), 4, 3 );
}
