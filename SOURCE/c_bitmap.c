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
 + GetBitmapSize(),GetIconSize(),DrawGlyphMask()
   Copyright 2009 (C) Andi Jahja <harbour@cbn.net.id>
 + GetImageSize()
 ---------------------------------------------------------------------------*/

#include <mgdefs.h>
#include "hbapiitm.h"
#include "hbapifs.h"

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif

// Function to convert a bitmap to a DIB (Device Independent Bitmap)
HANDLE DibFromBitmap( HBITMAP, HPALETTE );

// Function to get the number of colors in a DIB
WORD GetDIBColors( LPSTR );

// Function to get the instance handle of the application resources
HINSTANCE GetResources( void );

// Function to register a resource in the Minigui resource management system
void RegisterResource( HANDLE hResource, LPCSTR szType );

/*
 * FUNCTION: SAVEWINDOWBYHANDLE
 *
 * Saves a window or a portion of a window to a bitmap file.
 *
 * Parameters:
 *   1: HWND - Handle to the window to be saved.
 *   2: LPCSTR/LPCWSTR - File name to save the bitmap.
 *   3: INT - Top coordinate of the rectangle to save.
 *   4: INT - Left coordinate of the rectangle to save.
 *   5: INT - Bottom coordinate of the rectangle to save.
 *   6: INT - Right coordinate of the rectangle to save.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Captures the content of a window or a specified portion of a window and saves it to a bitmap file.
 */
HB_FUNC( SAVEWINDOWBYHANDLE )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 ); // Handle to the window to be saved
   HDC      hDC  = GetDC( hWnd );         // Get the device context of the window
   HDC      hMemDC;                       // Memory device context for creating the bitmap
   RECT     rc;                           // Rectangle structure to define the area to capture
   HBITMAP  hBitmap;                      // Handle to the bitmap
   HBITMAP  hOldBmp;                      // Handle to the old bitmap
   HPALETTE hPal = 0;                     // Handle to the palette
   HANDLE   hDIB;                         // Handle to the DIB (Device Independent Bitmap)

#ifndef UNICODE
   LPCSTR lpFileName = ( char * ) hb_parc( 2 );                // File name to save the bitmap (ANSI)
#else
   LPCWSTR lpFileName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // File name to save the bitmap (Unicode)
#endif

   int top    = hb_parni( 3 ); // Top coordinate of the rectangle to save
   int left   = hb_parni( 4 ); // Left coordinate of the rectangle to save
   int bottom = hb_parni( 5 ); // Bottom coordinate of the rectangle to save
   int right  = hb_parni( 6 ); // Right coordinate of the rectangle to save

   BITMAPFILEHEADER   bmfHdr;  // Bitmap file header
   LPBITMAPINFOHEADER lpBI;    // Pointer to the bitmap info header
   HANDLE filehandle;          // Handle to the file
   DWORD  dwDIBSize;           // Size of the DIB
   DWORD  dwWritten;           // Number of bytes written to the file
   DWORD  dwBmBitsSize;        // Size of the bitmap bits

   // Set the rectangle coordinates based on the provided parameters or the entire client area
   if( top != -1 && left != -1 && bottom != -1 && right != -1 )
   {
      rc.top    = top;
      rc.left   = left;
      rc.bottom = bottom;
      rc.right  = right;
   }
   else
   {
      GetClientRect( hWnd, &rc );
   }

   // Create a compatible memory DC and bitmap for capturing the window content
   hMemDC  = CreateCompatibleDC( hDC );
   hBitmap = CreateCompatibleBitmap( hDC, rc.right - rc.left, rc.bottom - rc.top );
   hOldBmp = ( HBITMAP ) SelectObject( hMemDC, hBitmap );

   // Copy the content of the window to the bitmap
   BitBlt( hMemDC, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, rc.top, rc.left, SRCCOPY );

   // Restore the old bitmap and convert the captured bitmap to a DIB
   SelectObject( hMemDC, hOldBmp );
   hDIB = DibFromBitmap( hBitmap, hPal );

   // Create a file to save the bitmap
   filehandle = CreateFile( lpFileName, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL );

   // Lock the DIB and write the bitmap file header and DIB to the file
   lpBI = ( LPBITMAPINFOHEADER ) GlobalLock( hDIB );
   if( lpBI && lpBI->biSize == sizeof( BITMAPINFOHEADER ) )
   {
      bmfHdr.bfType = ( ( WORD ) ( 'M' << 8 ) | 'B' );

      dwDIBSize = *( LPDWORD ) lpBI + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBTRIPLE ) );

      dwBmBitsSize      = ( ( ( ( lpBI->biWidth ) * ( ( DWORD ) lpBI->biBitCount ) ) + 31 ) / 32 * 4 ) * lpBI->biHeight;
      dwDIBSize        += dwBmBitsSize;
      lpBI->biSizeImage = dwBmBitsSize;

      bmfHdr.bfSize      = dwDIBSize + sizeof( BITMAPFILEHEADER );
      bmfHdr.bfReserved1 = 0;
      bmfHdr.bfReserved2 = 0;

      bmfHdr.bfOffBits = ( DWORD ) sizeof( BITMAPFILEHEADER ) + lpBI->biSize + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBTRIPLE ) );

      WriteFile( filehandle, ( LPSTR ) &bmfHdr, sizeof( BITMAPFILEHEADER ), &dwWritten, NULL );
      WriteFile( filehandle, ( LPSTR ) lpBI, dwDIBSize, &dwWritten, NULL );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpFileName ); // Free the allocated memory for the Unicode file name
#endif

   // Clean up resources
   GlobalUnlock( hDIB );
   CloseHandle( filehandle );
   DeleteObject( hBitmap );
   DeleteDC( hMemDC );
   GlobalFree( hDIB );
   ReleaseDC( hWnd, hDC );
}

/*
 * FUNCTION: WNDCOPY
 *
 * Saves a window or a portion of a window to a bitmap file.
 *
 * Parameters:
 *   1: HWND - Handle to the window to be saved.
 *   2: LOGICAL - Flag to determine if the entire window or client area should be saved.
 *   3: LPCSTR/LPCWSTR - File name to save the bitmap.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Captures the content of a window or a specified portion of a window and saves it to a bitmap file.
 */
HB_FUNC( WNDCOPY )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 ); // Handle to the window to be saved
   HDC      hDC  = GetDC( hWnd );         // Get the device context of the window
   HDC      hMemDC;                       // Memory device context for creating the bitmap
   RECT     rc;                           // Rectangle structure to define the area to capture
   HBITMAP  hBitmap;                      // Handle to the bitmap
   HBITMAP  hOldBmp;                      // Handle to the old bitmap
   HPALETTE hPal  = 0;                    // Handle to the palette
   BOOL     bRect = hb_parl( 2 );         // Flag to determine if the entire window or client area should be saved

#ifndef UNICODE
   LPCSTR lpFileName = ( char * ) hb_parc( 3 );                // File name to save the bitmap (ANSI)
#else
   LPCWSTR lpFileName = AnsiToWide( ( char * ) hb_parc( 3 ) ); // File name to save the bitmap (Unicode)
#endif

   HANDLE hDIB;               // Handle to the DIB (Device Independent Bitmap)
   BITMAPFILEHEADER   bmfHdr; // Bitmap file header
   LPBITMAPINFOHEADER lpBI;   // Pointer to the bitmap info header
   HANDLE filehandle;         // Handle to the file
   DWORD  dwDIBSize;          // Size of the DIB
   DWORD  dwWritten;          // Number of bytes written to the file
   DWORD  dwBmBitsSize;       // Size of the bitmap bits

   // Set the rectangle coordinates based on the provided parameters or the entire window area
   if( bRect )
   {
      GetWindowRect( hWnd, &rc );
   }
   else
   {
      GetClientRect( hWnd, &rc );
   }

   // Create a compatible memory DC and bitmap for capturing the window content
   hMemDC  = CreateCompatibleDC( hDC );
   hBitmap = CreateCompatibleBitmap( hDC, rc.right - rc.left, rc.bottom - rc.top );
   hOldBmp = ( HBITMAP ) SelectObject( hMemDC, hBitmap );

   // Copy the content of the window to the bitmap
   BitBlt( hMemDC, 0, 0, rc.right - rc.left, rc.bottom - rc.top, hDC, 0, 0, SRCCOPY );

   // Restore the old bitmap and convert the captured bitmap to a DIB
   SelectObject( hMemDC, hOldBmp );
   hDIB = DibFromBitmap( hBitmap, hPal );

   // Create a file to save the bitmap
   filehandle = CreateFile( lpFileName, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL );

   // Lock the DIB and write the bitmap file header and DIB to the file
   lpBI = ( LPBITMAPINFOHEADER ) GlobalLock( hDIB );
   if( lpBI && lpBI->biSize == sizeof( BITMAPINFOHEADER ) )
   {
      bmfHdr.bfType = ( ( WORD ) ( 'M' << 8 ) | 'B' );

      dwDIBSize = *( LPDWORD ) lpBI + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBTRIPLE ) );

      dwBmBitsSize      = ( ( ( ( lpBI->biWidth ) * ( ( DWORD ) lpBI->biBitCount ) ) + 31 ) / 32 * 4 ) * lpBI->biHeight;
      dwDIBSize        += dwBmBitsSize;
      lpBI->biSizeImage = dwBmBitsSize;

      bmfHdr.bfSize      = dwDIBSize + sizeof( BITMAPFILEHEADER );
      bmfHdr.bfReserved1 = 0;
      bmfHdr.bfReserved2 = 0;

      bmfHdr.bfOffBits = ( DWORD ) sizeof( BITMAPFILEHEADER ) + lpBI->biSize + ( GetDIBColors( ( LPSTR ) lpBI ) * sizeof( RGBTRIPLE ) );

      WriteFile( filehandle, ( LPSTR ) &bmfHdr, sizeof( BITMAPFILEHEADER ), &dwWritten, NULL );
      WriteFile( filehandle, ( LPSTR ) lpBI, dwDIBSize, &dwWritten, NULL );
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpFileName ); // Free the allocated memory for the Unicode file name
#endif

   // Clean up resources
   GlobalUnlock( hDIB );
   CloseHandle( filehandle );
   DeleteDC( hMemDC );
   GlobalFree( hDIB );
   ReleaseDC( hWnd, hDC );
}

/*
 * FUNCTION: DibNumColors
 *
 * Determines the number of colors in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   pv: VOID FAR * - Pointer to the DIB.
 *
 * Returns:
 *   WORD - Number of colors in the DIB.
 *
 * Purpose:
 *   Calculates the number of colors in a DIB based on its bit count.
 */
WORD DibNumColors( VOID FAR * pv )
{
   int bits;
   LPBITMAPINFOHEADER lpbi;
   LPBITMAPCOREHEADER lpbc;

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
 * FUNCTION: PaletteSize
 *
 * Determines the size of the palette in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   pv: VOID FAR * - Pointer to the DIB.
 *
 * Returns:
 *   WORD - Size of the palette in bytes.
 *
 * Purpose:
 *   Calculates the size of the palette in a DIB based on the number of colors.
 */
static WORD PaletteSize( VOID FAR * pv )
{
   LPBITMAPINFOHEADER lpbi;
   WORD NumColors;

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
 * Macro: WIDTHBYTES
 *
 * Calculates the number of bytes required for a given number of bits.
 *
 * Parameters:
 *   i: int - Number of bits.
 *
 * Returns:
 *   int - Number of bytes.
 *
 * Purpose:
 *   Calculates the number of bytes required to store a given number of bits, rounded up to the nearest DWORD boundary.
 */
#define WIDTHBYTES( i )  ( ( ( i ) + 31 ) / 32 * 4 )

/*
 * FUNCTION: DibFromBitmap
 *
 * Converts a bitmap to a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   hbm: HBITMAP - Handle to the bitmap.
 *   hpal: HPALETTE - Handle to the palette.
 *
 * Returns:
 *   HANDLE - Handle to the DIB.
 *
 * Purpose:
 *   Converts a bitmap to a DIB, which can be used for various operations such as saving to a file.
 */
HANDLE DibFromBitmap( HBITMAP hbm, HPALETTE hpal )
{
   BITMAP bm;
   BITMAPINFOHEADER       bi;
   BITMAPINFOHEADER FAR * lpbi;
   DWORD  dwLen;
   WORD   biBits;
   HANDLE hdib;
   HANDLE h;
   HDC    hdc;

   if( ! hbm )
   {
      return NULL;
   }

   if( hpal == NULL )
   {
      hpal = ( HPALETTE ) GetStockObject( DEFAULT_PALETTE );
   }

   GetObject( hbm, sizeof( bm ), ( LPSTR ) &bm );

   biBits = ( WORD ) ( bm.bmPlanes * bm.bmBitsPixel );

   bi.biSize          = sizeof( BITMAPINFOHEADER );
   bi.biWidth         = bm.bmWidth;
   bi.biHeight        = bm.bmHeight;
   bi.biPlanes        = 1;
   bi.biBitCount      = biBits;
   bi.biCompression   = BI_RGB;
   bi.biSizeImage     = 0;
   bi.biXPelsPerMeter = 0;
   bi.biYPelsPerMeter = 0;
   bi.biClrUsed       = 0;
   bi.biClrImportant  = 0;

   dwLen = bi.biSize + PaletteSize( &bi );

   hdc  = GetDC( NULL );
   hpal = SelectPalette( hdc, hpal, FALSE );
   RealizePalette( hdc );

   hdib = GlobalAlloc( GHND, dwLen );

   if( ! hdib )
   {
      SelectPalette( hdc, hpal, FALSE );
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

      SelectPalette( hdc, hpal, FALSE );
      ReleaseDC( NULL, hdc );
      return NULL;
   }

   /* Call GetDIBits with a NON-NULL lpBits param, and actually get the
    * bits this time
    */
   lpbi = ( LPBITMAPINFOHEADER ) GlobalLock( hdib );

   if( GetDIBits( hdc, hbm, 0L, ( DWORD ) bi.biHeight, ( LPBYTE ) lpbi + ( WORD ) lpbi->biSize + PaletteSize( lpbi ), ( LPBITMAPINFO ) lpbi, ( DWORD ) DIB_RGB_COLORS ) == 0 )
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
 * FUNCTION: GetDIBColors
 *
 * Gets the number of colors in a DIB (Device Independent Bitmap).
 *
 * Parameters:
 *   lpDIB: LPSTR - Pointer to the DIB.
 *
 * Returns:
 *   WORD - Number of colors in the DIB.
 *
 * Purpose:
 *   Retrieves the number of colors in a DIB based on its bit count.
 */
WORD GetDIBColors( LPSTR lpDIB )
{
   WORD wBitCount = ( ( LPBITMAPCOREHEADER ) lpDIB )->bcBitCount;

   return wBitCount;
}

/*
 * FUNCTION: C_HASALPHA
 *
 * Checks if a bitmap has an alpha channel.
 *
 * Parameters:
 *   1: HBITMAP - Handle to the bitmap.
 *
 * Returns:
 *   LOGICAL - TRUE if the bitmap has an alpha channel, FALSE otherwise.
 *
 * Purpose:
 *   Determines if a bitmap has an alpha channel by examining its pixel data.
 */
HB_FUNC( C_HASALPHA )
{
   HANDLE hDib;
   BOOL   bAlphaChannel = FALSE;
   HDC    hDC = GetDC( GetDesktopWindow() );

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
      LPBITMAPINFO    lpbmi = ( LPBITMAPINFO ) GlobalLock( hDib );
      unsigned char * uc    = ( LPBYTE ) lpbmi + ( WORD ) lpbmi->bmiHeader.biSize + PaletteSize( lpbmi );
      unsigned long   ul;

      for( ul = 0; ul < lpbmi->bmiHeader.biSizeImage && ! bAlphaChannel; ul += 4 )
      {
         if( uc[ ul + 3 ] != 0 )
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
 * FUNCTION: Icon2Bmp
 *
 * Converts an icon to a bitmap.
 *
 * Parameters:
 *   hIcon: HICON - Handle to the icon.
 *
 * Returns:
 *   HBITMAP - Handle to the converted bitmap.
 *
 * Purpose:
 *   Converts an icon to a bitmap by drawing the icon onto a compatible bitmap.
 */
HBITMAP Icon2Bmp( HICON hIcon )
{
   HDC      hDC    = GetDC( NULL );
   HDC      hMemDC = CreateCompatibleDC( hDC );
   ICONINFO icon;
   BITMAP   bitmap;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;

   GetIconInfo( hIcon, &icon );
   GetObject( icon.hbmColor, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   hBmp    = CreateCompatibleBitmap( hDC, bitmap.bmWidth, bitmap.bmHeight );
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
 * FUNCTION: IconMask2Bmp
 *
 * Converts an icon mask to a bitmap.
 *
 * Parameters:
 *   hIcon: HICON - Handle to the icon.
 *
 * Returns:
 *   HBITMAP - Handle to the converted bitmap.
 *
 * Purpose:
 *   Converts an icon mask to a bitmap by drawing the icon mask onto a compatible bitmap.
 */
HBITMAP IconMask2Bmp( HICON hIcon )
{
   HDC      hDC    = GetDC( 0 );
   HDC      hMemDC = CreateCompatibleDC( hDC );
   ICONINFO icon;
   BITMAP   bitmap;
   HBITMAP  hBmp;
   HBITMAP  hOldBmp;

   GetIconInfo( hIcon, &icon );
   GetObject( icon.hbmColor, sizeof( BITMAP ), ( LPVOID ) &bitmap );
   hBmp    = CreateCompatibleBitmap( hDC, bitmap.bmWidth, bitmap.bmHeight );
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
 * FUNCTION: DRAWGLYPH
 *
 * Draws a glyph (bitmap or icon) with optional transparency and disabled effects.
 *
 * Parameters:
 *   1: HDC - Device context handle.
 *   2: INT - X coordinate.
 *   3: INT - Y coordinate.
 *   4: INT - Width.
 *   5: INT - Height.
 *   6: HBITMAP - Handle to the bitmap.
 *   7: COLORREF - Transparent color.
 *   8: LOGICAL - Flag to indicate if the glyph is disabled.
 *   9: LOGICAL - Flag to indicate if the glyph should be stretched.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Draws a glyph (bitmap or icon) with optional transparency and disabled effects.
 */
HB_FUNC( DRAWGLYPH )
{
   HDC      hDC  = hmg_par_raw_HDC( 1 );
   int      x    = hb_parni( 2 );
   int      y    = hb_parni( 3 );
   int      dx   = hb_parni( 4 );
   int      dy   = hb_parni( 5 );
   HBITMAP  hBmp = hmg_par_raw_HBITMAP( 6 );
   COLORREF rgbTransparent = RGB( 255, 255, 255 );
   BOOL     disabled       = hb_parl( 8 );
   BOOL     stretched      = HB_ISNIL( 9 ) ? FALSE : hb_parl( 9 );
   BOOL     bHasBkColor    = ! HB_ISNIL( 7 );

   HDC hDCMem;
   HDC hDCMask;
   HDC hDCStretch;
   HDC hDCNoBlink;

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
      if( ! GetIconInfo( ( HICON ) hBmp, &icon ) )
      {
         return;
      }

      DeleteObject( icon.hbmMask );
      DeleteObject( icon.hbmColor );

      if( ! icon.fIcon )
      {
         return;
      }

      if( ! disabled && ! stretched )
      {
         // Just draw the icon directly
         DrawIconEx( hDC, x, y, ( HICON ) hBmp, dx, dy, 0, NULL, DI_NORMAL );
         return;
      }
      else
      {
         if( ! stretched )
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
         bHasBkColor    = TRUE;
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
      hDCStretch  = CreateCompatibleDC( hDC );
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
   hDCNoBlink     = CreateCompatibleDC( hDC );
   hBmpNoBlink    = CreateCompatibleBitmap( hDC, dx, dy );
   hBmpNoBlinkOld = ( HBITMAP ) SelectObject( hDCNoBlink, hBmpNoBlink );
   BitBlt( hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY );
   SetBkColor( hDCNoBlink, RGB( 255, 255, 255 ) ); // White
   SetTextColor( hDCNoBlink, RGB( 0, 0, 0 ) );     // Black

   // Was background color given?
   // No? Get the color automatically
   if( ! bHasBkColor )
   {
      rgbTransparent = GetPixel( hDCMem, 0, 0 );
   }

   // Build mask based on transparent color
   hDCMask       = CreateCompatibleDC( hDCNoBlink );
   hBmpTransMask = CreateBitmap( dx, dy, 1, 1, NULL );
   SelectObject( hDCMask, hBmpTransMask );
   SetBkColor( hDCMem, rgbTransparent );
   BitBlt( hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY );

   if( disabled )
   {
      hBr  = CreateSolidBrush( GetSysColor( COLOR_BTNHIGHLIGHT ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );

      hBr  = CreateSolidBrush( GetSysColor( COLOR_BTNSHADOW ) );
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
   HDC      hDC  = hmg_par_raw_HDC( 1 );
   int      dx   = hb_parni( 4 );
   int      dy   = hb_parni( 5 );
   HBITMAP  hBmp = hmg_par_raw_HBITMAP( 6 );
   COLORREF rgbTransparent;
   HWND     hwnd = hmg_par_raw_HWND( 10 );

   HDC hDCMem;
   HDC hDCMask;

   HBITMAP hBmpDefault;
   HBITMAP hBmpTransMask;

   BITMAP bitmap;

   GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap );

   SetBkColor( hDC, RGB( 255, 255, 255 ) ); // White
   SetTextColor( hDC, RGB( 0, 0, 0 ) );     // Black
   hDCMem = CreateCompatibleDC( hDC );

   dx = ( dx > 0 ? HB_MIN( dx, bitmap.bmWidth ) : bitmap.bmWidth );
   dy = ( dy > 0 ? HB_MIN( dy, bitmap.bmHeight ) : bitmap.bmHeight );
   hBmpDefault    = ( HBITMAP ) SelectObject( hDCMem, hBmp );
   rgbTransparent = GetPixel( hDCMem, 0, 0 );

   // Build mask based on transparent color
   hDCMask       = CreateCompatibleDC( hDC );
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
   LPCSTR lpImageName = hb_parc( 1 );                          // Bitmap name (ANSI)
#else
   LPWSTR lpImageName = AnsiToWide( ( char * ) hb_parc( 1 ) ); // Bitmap name (Unicode)
#endif
   HBITMAP hBitmap;

   // Load the bitmap from resources
   hBitmap = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );

   // If not found in resources, load from file
   if( hBitmap == NULL )
   {
      hBitmap = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   RegisterResource( hBitmap, "BMP" ); // Register the bitmap resource
   hmg_ret_raw_HANDLE( hBitmap );      // Return the handle to the loaded bitmap

#ifdef UNICODE
   hb_xfree( lpImageName ); // Free the allocated memory for the Unicode bitmap name
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
   BOOL     bHasBkColor = ( rgbTransparent != CLR_INVALID );

   // Check if the handle is a bitmap
   if( ( UINT ) GetObject( hBmp, sizeof( BITMAP ), ( LPVOID ) &bitmap ) != sizeof( BITMAP ) )
   {
      // Check if it is an icon
      if( ! GetIconInfo( ( HICON ) hBmp, &icon ) )
      {
         return;
      }

      DeleteObject( icon.hbmMask );
      DeleteObject( icon.hbmColor );

      if( ! icon.fIcon )
      {
         return;
      }

      if( ! disabled && ! stretched )
      {
         // Just draw the icon directly
         DrawIconEx( hDC, x, y, ( HICON ) hBmp, dx, dy, 0, NULL, DI_NORMAL );
         return;
      }
      else
      {
         if( ! stretched )
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
         bHasBkColor    = TRUE;
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
      hDCStretch  = CreateCompatibleDC( hDC );
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
   hDCNoBlink     = CreateCompatibleDC( hDC );
   hBmpNoBlink    = CreateCompatibleBitmap( hDC, dx, dy );
   hBmpNoBlinkOld = ( HBITMAP ) SelectObject( hDCNoBlink, hBmpNoBlink );
   BitBlt( hDCNoBlink, 0, 0, dx, dy, hDC, x, y, SRCCOPY );
   SetBkColor( hDCNoBlink, RGB( 255, 255, 255 ) ); // White
   SetTextColor( hDCNoBlink, RGB( 0, 0, 0 ) );     // Black

   // Was background color given?
   // No? Get the color automatically
   if( ! bHasBkColor )
   {
      rgbTransparent = GetPixel( hDCMem, 0, 0 );
   }

   // Build mask based on transparent color
   hDCMask       = CreateCompatibleDC( hDCNoBlink );
   hBmpTransMask = CreateBitmap( dx, dy, 1, 1, NULL );
   SelectObject( hDCMask, hBmpTransMask );
   SetBkColor( hDCMem, rgbTransparent );
   BitBlt( hDCMask, 0, 0, dx, dy, hDCMem, 0, 0, SRCCOPY );

   if( disabled )
   {
      hBr  = CreateSolidBrush( GetSysColor( COLOR_BTNHIGHLIGHT ) );
      hOld = ( HBRUSH ) SelectObject( hDCNoBlink, hBr );
      BitBlt( hDCNoBlink, 1, 1, dx - 0, dy - 0, hDCMask, 0, 0, 12060490 );
      SelectObject( hDCNoBlink, hOld );
      DeleteObject( hBr );

      hBr  = CreateSolidBrush( GetSysColor( COLOR_BTNSHADOW ) );
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
 * Gets the dimensions of an image file.
 *
 * Parameters:
 *   fn: const char* - File name of the image.
 *   x: int* - Pointer to store the width.
 *   y: int* - Pointer to store the height.
 *
 * Returns:
 *   BOOL - TRUE if successful, FALSE otherwise.
 *
 * Purpose:
 *   Retrieves the width and height of an image file (JPG, GIF, PNG).
 */
BOOL GetImageSize( const char * fn, int * x, int * y )
{
   unsigned char buf[ 24 ];
   long          len;

   FILE * f = hb_fopen( fn, "rb" );

   if( ! f )
   {
      return FALSE;
   }

   fseek( f, 0, SEEK_END );

   len = ftell( f );

   fseek( f, 0, SEEK_SET );

   if( len < 24 )
   {
      fclose( f );
      return FALSE;
   }

   // Strategy:
   // reading GIF dimensions requires the first 10 bytes of the file
   // reading PNG dimensions requires the first 24 bytes of the file
   // reading JPEG dimensions requires scanning through jpeg chunks
   // In all formats, the file is at least 24 bytes big, so we'll read
   // that always
   fread( buf, 1, 24, f );

   // For JPEGs, we need to read the first 12 bytes of each chunk.
   // We'll read those 12 bytes at buf+2...buf+14, i.e. overwriting
   // the existing buf.
   if( buf[ 0 ] == 0xFF && buf[ 1 ] == 0xD8 && buf[ 2 ] == 0xFF && buf[ 3 ] == 0xE0 && buf[ 6 ] == 'J' && buf[ 7 ] == 'F' && buf[ 8 ] == 'I' && buf[ 9 ] == 'F' )
   {
      long pos = 2;
      while( buf[ 2 ] == 0xFF )
      {
         if( buf[ 3 ] == 0xC0 || buf[ 3 ] == 0xC1 || buf[ 3 ] == 0xC2 || buf[ 3 ] == 0xC3 || buf[ 3 ] == 0xC9 || buf[ 3 ] == 0xCA || buf[ 3 ] == 0xCB )
         {
            break;
         }

         pos += 2 + ( buf[ 4 ] << 8 ) + buf[ 5 ];
         if( pos + 12 > len )
         {
            break;
         }

         fseek( f, pos, SEEK_SET );
         fread( buf + 2, 1, 12, f );
      }
   }

#if ! ( defined( __POCC__ ) && __POCC__ >= 1100 )
   fclose( f );
#endif /* __POCC__ */

   // JPEG: (first two bytes of buf are first two bytes of the jpeg
   // file; rest of buf is the DCT frame
   if( buf[ 0 ] == 0xFF && buf[ 1 ] == 0xD8 && buf[ 2 ] == 0xFF )
   {
      *y = ( buf[ 7 ] << 8 ) + buf[ 8 ];
      *x = ( buf[ 9 ] << 8 ) + buf[ 10 ];
      return TRUE;
   }

   // GIF: first three bytes say "GIF", next three give version
   // number. Then dimensions
   if( buf[ 0 ] == 'G' && buf[ 1 ] == 'I' && buf[ 2 ] == 'F' )
   {
      *x = buf[ 6 ] + ( buf[ 7 ] << 8 );
      *y = buf[ 8 ] + ( buf[ 9 ] << 8 );
      return TRUE;
   }

   // PNG: the first frame is by definition an IHDR frame, which gives
   // dimensions
   if( buf[ 0 ] == 0x89 && buf[ 1 ] == 'P' && buf[ 2 ] == 'N' && buf[ 3 ] == 'G' && buf[ 4 ] == 0x0D && buf[ 5 ] == 0x0A && buf[ 6 ] == 0x1A && buf[ 7 ] == 0x0A && buf[ 12 ] == 'I' && buf[ 13 ] == 'H' && buf[ 14 ] == 'D' && buf[ 15 ] == 'R' )
   {
      *x = ( buf[ 16 ] << 24 ) + ( buf[ 17 ] << 16 ) + ( buf[ 18 ] << 8 ) + ( buf[ 19 ] << 0 );
      *y = ( buf[ 20 ] << 24 ) + ( buf[ 21 ] << 16 ) + ( buf[ 22 ] << 8 ) + ( buf[ 23 ] << 0 );
      return TRUE;
   }

   return FALSE;
}

/*
 * FUNCTION: HB_GETIMAGESIZE
 *
 * Gets the dimensions of an image file.
 *
 * Parameters:
 *   1: LPCSTR - File name of the image.
 *
 * Returns:
 *   Array - Array containing the width and height of the image.
 *
 * Purpose:
 *   Retrieves the width and height of an image file (JPG, GIF, PNG).
 */
HB_FUNC( HB_GETIMAGESIZE )
{
   int x = 0, y = 0;

   GetImageSize( hb_parcx( 1 ), &x, &y );

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
      LPCSTR lpImageName = hb_parc( 1 );
#else
      LPWSTR lpImageName = AnsiToWide( ( char * ) hb_parc( 1 ) );
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
      BITMAP bm;

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
   HICON    hIcon   = hmg_par_raw_HICON( 1 );

   _arraySet( pResult, 0, 0, 4 );

   if( hIcon )
   {
      ICONINFO sIconInfo;

      if( GetIconInfo( hIcon, &sIconInfo ) )
      {
         BITMAP bm;

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
