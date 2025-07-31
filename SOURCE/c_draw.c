/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

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

   Parts of this code are contributed and used here under permission of the
   author: Copyright 2017 (C) P.Chornyj <myorg63@mail.ru>
 */

#include <mgdefs.h>
#include "hbapiitm.h"  // Harbour API for item manipulation

// Define GdiFlush for certain compilers that may not have it
#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ <= 1410 ) ) || defined( __WATCOMC__ )
WINGDIAPI BOOL WINAPI      GdiFlush( void );
#endif

// External functions for handling color references and rectangles
extern HB_EXPORT BOOL      Array2ColorRef( PHB_ITEM aCRef, COLORREF *cr ); // Converts array to COLORREF
extern HB_EXPORT BOOL      Array2Rect( PHB_ITEM aRect, RECT *rc );         // Converts array to RECT
extern HB_EXPORT PHB_ITEM  Rect2Array( RECT *rc );                         // Converts RECT to array

// Begins the paint process for a window, returning a device context handle.
HB_FUNC( BEGINPAINT )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );             // Retrieve the window handle from the function parameter
   if( IsWindow( hWnd ) )
   {
      PAINTSTRUCT ps;                              // Structure to hold paint information
      hmg_ret_raw_HDC( BeginPaint( hWnd, &ps ) );  // Begin paint, return the handle to the HDC
      hb_storclen( ( const char * ) &ps, sizeof( PAINTSTRUCT ), 2 ); // Store PAINTSTRUCT in function param
   }
   else
   {
      hmg_ret_raw_HANDLE( NULL );                     // Return null if window is invalid
   }
}

// Completes the paint process for a window, releasing the device context.
HB_FUNC( ENDPAINT )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );          // Retrieve window handle
   PAINTSTRUCT *pps = ( PAINTSTRUCT * ) hb_parc( 2 ); // Retrieve PAINTSTRUCT pointer
   if( IsWindow( hWnd ) && pps )
   {
      hmg_ret_L( EndPaint( hWnd, pps ) );             // End painting and return success
   }
   else
   {
      hb_retl( HB_FALSE ); // Return false if window or PAINTSTRUCT is invalid
   }
}

// Draws a focus rectangle inside a given item rectangle.
HB_FUNC( DRAWFOCUSRECT )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );   // Get DRAWITEMSTRUCT from parameter
   if( pps )
   {
      InflateRect( &pps->rcItem, -3, -3 );               // Shrink the rectangle
      DrawFocusRect( pps->hDC, &pps->rcItem );           // Draw focus rectangle on device context
      InflateRect( &pps->rcItem, +3, +3 );               // Restore original rectangle size
   }
}

// Draws a state on a specified device context using a brush and optional flags.
HB_FUNC( DRAWSTATE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );                   // Get the window handle
   HDC   hDC;
   BOOL  bDC = FALSE;

   if( IsWindow( hWnd ) )
   {
      hDC = GetDC( hWnd );          // Obtain device context if valid window
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_raw_HDC( 1 );   // Use existing HDC if window is invalid
   }

   if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )         // Check if valid device context
   {
      HBRUSH   hBrush = ( HBRUSH ) NULL;
      COLORREF crBrush;
      LPARAM   lpData;
      WPARAM   wData = ( WPARAM ) hb_parclen( 4 );
      HB_ISIZ  fuFlags = hb_parns( 10 );

      // Create a brush if color array is provided
      if( Array2ColorRef( hb_param( 2, HB_IT_ANY ), &crBrush ) )
      {
         hBrush = CreateSolidBrush( crBrush );
      }

      lpData = ( wData > 0 ) ? ( LPARAM ) hb_parc( 4 ) : ( LPARAM ) hmg_par_raw_LONG_PTR( 4 );

      // Draw state with specified parameters
      hmg_ret_L( DrawState( hDC, hBrush, NULL, lpData, wData, hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ), ( UINT ) fuFlags ) );

      // Release device context if acquired within function
      if( bDC )
      {
         ReleaseDC( hWnd, hDC );
      }

      // Delete object if requested by parameter
      if( hb_parl( 11 ) )
      {
         if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_BITMAP )
         {
            DeleteObject( ( HBITMAP ) lpData );
         }
         else
         {
            DestroyIcon( ( HICON ) lpData );
         }
      }
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Retrieves the update rectangle for a window, returning it as an array if specified.
HB_FUNC( GETUPDATERECT )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      if( HB_ISNIL( 2 ) )
      {
         hmg_ret_L( GetUpdateRect( hWnd, NULL, hb_parl( 3 ) ) );
      }
      else
      {
         RECT  rc;

         hmg_ret_L( GetUpdateRect( hWnd, &rc, hb_parl( 3 ) ) );
#ifndef __XHARBOUR__
         hb_itemParamStoreRelease( 2, Rect2Array( &rc ) );  // Convert RECT to array if Harbour
#endif
      }
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Forces all pending GDI operations to complete.
HB_FUNC( GDIFLUSH )
{
   hmg_ret_L( GdiFlush() );
}

// Draws grayed-out text, typically for disabled elements.
HB_FUNC( GRAYSTRING )
{
   int   nCount = hb_parni( 5 );          // Number of characters to draw
   int   nLen = ( int ) hb_parclen( 4 );  // Length of string
   nCount = ( nCount > 0 ) ? HB_MIN( nCount, nLen ) : nLen;

   if( nLen > 0 )
   {
      HWND  hWnd = hmg_par_raw_HWND( 1 );
      HDC   hDC;
      BOOL  bDC = FALSE;

      if( IsWindow( hWnd ) )
      {
         hDC = GetDC( hWnd );
         bDC = TRUE;
      }
      else
      {
         hDC = hmg_par_raw_HDC( 1 );
      }

      if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )
      {
         HBRUSH      hBrush = ( HBRUSH ) NULL;
         COLORREF    crBrush;
         const char  *lpData = hb_parc( 4 );

         if( Array2ColorRef( hb_param( 2, HB_IT_ANY ), &crBrush ) )
         {
            hBrush = CreateSolidBrush( crBrush );
         }

         hmg_ret_L( GrayString( hDC, hBrush, NULL, ( LPARAM ) lpData, nCount, hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ) ) );

         if( bDC )
         {
            ReleaseDC( hWnd, hDC );
         }
      }
      else
      {
         hb_retl( HB_FALSE );
      }
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Marks a specific area of a window as needing to be redrawn.
HB_FUNC( INVALIDATERECT )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      BOOL  bRect = FALSE;
      RECT  rc;

      if( ( hb_pcount() > 2 ) && ( !HB_ISNIL( 3 ) ) )
      {
         bRect = Array2Rect( hb_param( 3, HB_IT_ANY ), &rc );

         if( !bRect )
         {
            rc.left = hb_parni( 3 );
            rc.top = hb_parni( 4 );
            rc.right = hb_parni( 5 );
            rc.bottom = hb_parni( 6 );
            bRect = TRUE;
         }
      }

      hmg_ret_L( InvalidateRect( hWnd, bRect ? &rc : NULL, hb_parni( 2 ) ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Forces a window to redraw its content.
HB_FUNC( REDRAWWINDOW )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      UINT  uiFlags = RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW;

      if( HB_TRUE == hb_parl( 2 ) )
      {
         uiFlags |= RDW_INTERNALPAINT;
      }

      hmg_ret_L( RedrawWindow( hWnd, NULL, NULL, uiFlags ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Sets the background color for a specified device context.
HB_FUNC( C_SETBACKCOLOR )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   HDC   hDC;
   BOOL  bDC = FALSE;

   if( IsWindow( hWnd ) )
   {
      hDC = GetDC( hWnd );
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_raw_HDC( 1 );
   }

   if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )
   {
      COLORREF cr;

      if( !Array2ColorRef( hb_param( 2, HB_IT_ANY ), &cr ) )
      {
         cr = RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
      }

      hb_retns( ( HB_ISIZ ) SetBkColor( hDC, cr ) );

      if( bDC )
      {
         ReleaseDC( hWnd, hDC );
      }
   }
   else
   {
      hb_retns( ( HB_ISIZ ) CLR_INVALID );
   }
}

// Sets the background drawing mode for text output (opaque or transparent).
HB_FUNC( SETBKMODE )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   HDC   hDC;
   BOOL  bDC = FALSE;

   if( IsWindow( hWnd ) )
   {
      hDC = GetDC( hWnd );
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_raw_HDC( 1 );
   }

   if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )
   {
      hb_retni( SetBkMode( hDC, hb_parnidef( 2, OPAQUE ) ) );

      if( bDC )
      {
         ReleaseDC( hWnd, hDC );
      }
   }
   else
   {
      hb_retni( 0 );
   }
}

// Forces the window to repaint its client area.
HB_FUNC( UPDATEWINDOW )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      hmg_ret_L( UpdateWindow( hWnd ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Validates a specified rectangle, marking it as processed for painting.
HB_FUNC( VALIDATERECT )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      BOOL  bRect = FALSE;
      RECT  rc;

      if( ( hb_pcount() > 1 ) && ( !HB_ISNIL( 2 ) ) )
      {
         bRect = Array2Rect( hb_param( 2, HB_IT_ANY ), &rc );

         if( !bRect )
         {
            rc.left = hb_parni( 2 );
            rc.top = hb_parni( 3 );
            rc.right = hb_parni( 4 );
            rc.bottom = hb_parni( 5 );
            bRect = TRUE;
         }
      }

      hmg_ret_L( ValidateRect( hWnd, bRect ? &rc : NULL ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

// Retrieves the window handle associated with a specified device context.
HB_FUNC( WINDOWFROMDC )
{
   hmg_ret_raw_HWND( WindowFromDC( hmg_par_raw_HDC( 1 ) ) );
}
