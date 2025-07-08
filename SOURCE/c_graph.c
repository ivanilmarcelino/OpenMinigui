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

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
#endif
#ifdef __cplusplus
extern "C"
{
#endif
extern BOOL    Array2ColorRef( PHB_ITEM aCRef, COLORREF *cr );
extern HFONT   PrepareFont( TCHAR *, int, int, int, int, int, int, int );
#ifdef __cplusplus
}
#endif

/*
 *  HB_FUNC( TEXTDRAW )
 *
 *  Description:
 *     Draws text on a window or device context (DC) using specified parameters like font, color, and rectangle.
 *
 *  Parameters:
 *     1: hWnd (HWND) or hDC (HDC): Handle to the window or device context on which to draw the text.
 *     2: Y (int): The y-coordinate of the upper-left corner of the rectangle.
 *     3: X (int): The x-coordinate of the upper-left corner of the rectangle.
 *     4: String (string): The text string to be drawn.
 *     5: Bottom (int): The y-coordinate of the lower-right corner of the rectangle.
 *     6: Right (int): The x-coordinate of the lower-right corner of the rectangle.
 *     7: Foreground Color (PHB_ITEM): Harbour array representing the RGB color for the text.
 *     8: Background Color (PHB_ITEM): Harbour array representing the RGB color for the background.
 *     9: Font Name (string): The name of the font to use.
 *    10: Font Size (int): The size of the font.
 *    11: Bold (logical):  .T. for bold font, .F. otherwise.
 *    12: Italic (logical): .T. for italic font, .F. otherwise.
 *    13: Underline (logical): .T. for underlined font, .F. otherwise.
 *    14: Strikeout (logical): .T. for strikeout font, .F. otherwise.
 *    15: Transparent (logical): .T. for transparent background, .F. otherwise.
 *    16: Angle (numeric): Angle for the font.
 *
 *  Return Value:
 *     (logical): .T. if the text was drawn successfully, .F. otherwise.
 *
 *  Purpose:
 *     This function provides a way to draw text with custom formatting on a window or device context.
 *     It handles font selection, color settings, and background modes before drawing the text using ExtTextOut.
 */
HB_FUNC( TEXTDRAW )
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
      int      bold = hb_parl( 11 ) ? FW_BOLD : FW_NORMAL;
      DWORD    italic = ( DWORD ) hb_parl( 12 );
      DWORD    underline = ( DWORD ) hb_parl( 13 );
      DWORD    strikeout = ( DWORD ) hb_parl( 14 );
      DWORD    angle = hb_parnl( 16 );
#ifndef UNICODE
      LPCSTR   lpString = hb_parc( 4 );
#else
      LPCWSTR  lpString = AnsiToWide( ( char * ) hb_parc( 4 ) );
      LPWSTR   pStr;
#endif
      HFONT    font;
      HGDIOBJ  hgdiobj;
      int      iBkMode;
      COLORREF crBkColor = CLR_INVALID;
      COLORREF crFgColor = CLR_INVALID;
      RECT     rect;

#ifdef UNICODE
      pStr = AnsiToWide( hb_parc( 9 ) );
      font = PrepareFont( ( TCHAR * ) pStr, hb_parni( 10 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
      hb_xfree( pStr );
#else
      font = PrepareFont( ( TCHAR * ) hb_parc( 9 ), hb_parni( 10 ), bold, italic, underline, strikeout, angle, DEFAULT_CHARSET );
#endif
      hgdiobj = SelectObject( hDC, font );

      if( hb_parl( 15 ) )
      {
         iBkMode = SetBkMode( hDC, TRANSPARENT );
      }
      else
      {
         iBkMode = SetBkMode( hDC, OPAQUE );

         if( Array2ColorRef( hb_param( 8, HB_IT_ANY ), &crBkColor ) )
         {
            crBkColor = SetBkColor( hDC, crBkColor );
         }
      }

      if( Array2ColorRef( hb_param( 7, HB_IT_ANY ), &crFgColor ) )
      {
         SetTextColor( hDC, crFgColor );
      }

      SetRect( &rect, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 6 ), hb_parni( 5 ) );

      hmg_ret_L( ExtTextOut( hDC, hb_parni( 3 ), hb_parni( 2 ), ETO_OPAQUE, &rect, lpString, ( int ) lstrlen( lpString ), NULL ) );

#ifdef UNICODE
      hb_xfree( ( TCHAR * ) lpString );
#endif
      SelectObject( hDC, hgdiobj );

      if( 0 != iBkMode )
      {
         SetBkMode( hDC, iBkMode );
      }

      if( CLR_INVALID != crBkColor )
      {
         SetBkColor( hDC, crBkColor );
      }

      if( CLR_INVALID != crFgColor )
      {
         SetTextColor( hDC, crFgColor );
      }

      DeleteObject( font );

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

/*
 *  HB_FUNC( LINEDRAW )
 *
 *  Description:
 *     Draws a line on a window using the specified coordinates and color.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the line.
 *     2: Y1 (int): The y-coordinate of the starting point of the line.
 *     3: X1 (int): The x-coordinate of the starting point of the line.
 *     4: Y2 (int): The y-coordinate of the ending point of the line.
 *     5: X2 (int): The x-coordinate of the ending point of the line.
 *     6: Color (PHB_ITEM): Harbour array representing the RGB color of the line.
 *     7: Pen Width (int): The width of the pen used to draw the line.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a simple way to draw a line on a window with a specified color and width.
 *     It creates a pen with the given color and width, selects it into the device context, draws the line,
 *     and then cleans up by deselecting the pen and releasing the device context.
 */
HB_FUNC( LINEDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1;
   HPEN     hpen;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 7 ), RGB( HB_PARNI( 6, 1 ), HB_PARNI( 6, 2 ), HB_PARNI( 6, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   MoveToEx( hdc1, hb_parni( 3 ), hb_parni( 2 ), NULL );
   LineTo( hdc1, hb_parni( 5 ), hb_parni( 4 ) );
   SelectObject( hdc1, hgdiobj1 );
   DeleteObject( hpen );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( RECTDRAW )
 *
 *  Description:
 *     Draws a rectangle on a window using the specified coordinates, border color, fill color, and fill option.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the rectangle.
 *     2: Top (int): The y-coordinate of the upper-left corner of the rectangle.
 *     3: Left (int): The x-coordinate of the upper-left corner of the rectangle.
 *     4: Bottom (int): The y-coordinate of the lower-right corner of the rectangle.
 *     5: Right (int): The x-coordinate of the lower-right corner of the rectangle.
 *     6: Border Color (PHB_ITEM): Harbour array representing the RGB color of the rectangle's border.
 *     7: Pen Width (int): The width of the pen used to draw the rectangle's border.
 *     8: Fill Color (PHB_ITEM): Harbour array representing the RGB color to fill the rectangle with.
 *     9: Fill (logical): .T. to fill the rectangle with the specified color, .F. for a hollow rectangle.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw a rectangle on a window with customizable border color, width,
 *     fill color, and fill option. It creates a pen for the border, a brush for the fill (solid or hollow),
 *     selects them into the device context, draws the rectangle, and then cleans up by deselecting the objects
 *     and releasing the device context.
 */
HB_FUNC( RECTDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1, hgdiobj2;
   HPEN     hpen;
   HBRUSH   hbrush;
   LOGBRUSH br;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 7 ), RGB( HB_PARNI( 6, 1 ), HB_PARNI( 6, 2 ), HB_PARNI( 6, 3 ) ) );

   hgdiobj1 = SelectObject( hdc1, hpen );
   if( hb_parl( 9 ) )
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 8, 1 ), HB_PARNI( 8, 2 ), HB_PARNI( 8, 3 ) ) );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }
   else
   {
      br.lbStyle = BS_HOLLOW;
      hbrush = CreateBrushIndirect( &br );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }

   Rectangle( hdc1, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 5 ), hb_parni( 4 ) );
   SelectObject( hdc1, hgdiobj1 );
   SelectObject( hdc1, hgdiobj2 );
   DeleteObject( hpen );
   DeleteObject( hbrush );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( ROUNDRECTDRAW )
 *
 *  Description:
 *     Draws a rounded rectangle on a window using the specified coordinates, corner ellipse dimensions,
 *     border color, fill color, and fill option.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the rounded rectangle.
 *     2: Top (int): The y-coordinate of the upper-left corner of the rectangle.
 *     3: Left (int): The x-coordinate of the upper-left corner of the rectangle.
 *     4: Bottom (int): The y-coordinate of the lower-right corner of the rectangle.
 *     5: Right (int): The x-coordinate of the lower-right corner of the rectangle.
 *     6: Ellipse Width (int): The width of the ellipse used to round the corners.
 *     7: Ellipse Height (int): The height of the ellipse used to round the corners.
 *     8: Border Color (PHB_ITEM): Harbour array representing the RGB color of the rectangle's border.
 *     9: Pen Width (int): The width of the pen used to draw the rectangle's border.
 *    10: Fill Color (PHB_ITEM): Harbour array representing the RGB color to fill the rectangle with.
 *    11: Fill (logical): .T. to fill the rectangle with the specified color, .F. for a hollow rectangle.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw a rounded rectangle on a window with customizable corner roundness,
 *     border color, width, fill color, and fill option. It creates a pen for the border, a brush for the fill
 *     (solid or hollow), selects them into the device context, draws the rounded rectangle using RoundRect,
 *     and then cleans up by deselecting the objects and releasing the device context.
 */
HB_FUNC( ROUNDRECTDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1, hgdiobj2;
   HPEN     hpen;
   HBRUSH   hbrush;
   LOGBRUSH br;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 9 ), RGB( HB_PARNI( 8, 1 ), HB_PARNI( 8, 2 ), HB_PARNI( 8, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   if( hb_parl( 11 ) )
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 10, 1 ), HB_PARNI( 10, 2 ), HB_PARNI( 10, 3 ) ) );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }
   else
   {
      br.lbStyle = BS_HOLLOW;
      hbrush = CreateBrushIndirect( &br );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }

   RoundRect( hdc1, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 5 ), hb_parni( 4 ), hb_parni( 6 ), hb_parni( 7 ) );
   SelectObject( hdc1, hgdiobj1 );
   SelectObject( hdc1, hgdiobj2 );
   DeleteObject( hpen );
   DeleteObject( hbrush );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( ELLIPSEDRAW )
 *
 *  Description:
 *     Draws an ellipse on a window using the specified bounding rectangle, border color, fill color, and fill option.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the ellipse.
 *     2: Top (int): The y-coordinate of the upper-left corner of the bounding rectangle.
 *     3: Left (int): The x-coordinate of the upper-left corner of the bounding rectangle.
 *     4: Bottom (int): The y-coordinate of the lower-right corner of the bounding rectangle.
 *     5: Right (int): The x-coordinate of the lower-right corner of the bounding rectangle.
 *     6: Border Color (PHB_ITEM): Harbour array representing the RGB color of the ellipse's border.
 *     7: Pen Width (int): The width of the pen used to draw the ellipse's border.
 *     8: Fill Color (PHB_ITEM): Harbour array representing the RGB color to fill the ellipse with.
 *     9: Fill (logical): .T. to fill the ellipse with the specified color, .F. for a hollow ellipse.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw an ellipse on a window with customizable border color, width,
 *     fill color, and fill option. It creates a pen for the border, a brush for the fill (solid or hollow),
 *     selects them into the device context, draws the ellipse using Ellipse, and then cleans up by deselecting
 *     the objects and releasing the device context.
 */
HB_FUNC( ELLIPSEDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1, hgdiobj2;
   HPEN     hpen;
   HBRUSH   hbrush;
   LOGBRUSH br;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 7 ), RGB( HB_PARNI( 6, 1 ), HB_PARNI( 6, 2 ), HB_PARNI( 6, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   if( hb_parl( 9 ) )
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 8, 1 ), HB_PARNI( 8, 2 ), HB_PARNI( 8, 3 ) ) );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }
   else
   {
      br.lbStyle = BS_HOLLOW;
      hbrush = CreateBrushIndirect( &br );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }

   Ellipse( hdc1, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 5 ), hb_parni( 4 ) );
   SelectObject( hdc1, hgdiobj1 );
   SelectObject( hdc1, hgdiobj2 );
   DeleteObject( hpen );
   DeleteObject( hbrush );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( ARCDRAW )
 *
 *  Description:
 *     Draws an arc on a window using the specified bounding rectangle, start and end points, and border color.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the arc.
 *     2: Top (int): The y-coordinate of the upper-left corner of the bounding rectangle.
 *     3: Left (int): The x-coordinate of the upper-left corner of the bounding rectangle.
 *     4: Bottom (int): The y-coordinate of the lower-right corner of the bounding rectangle.
 *     5: Right (int): The x-coordinate of the lower-right corner of the bounding rectangle.
 *     6: Y Start (int): The y-coordinate of the arc's starting point.
 *     7: X Start (int): The x-coordinate of the arc's starting point.
 *     8: Y End (int): The y-coordinate of the arc's ending point.
 *     9: X End (int): The x-coordinate of the arc's ending point.
 *    10: Border Color (PHB_ITEM): Harbour array representing the RGB color of the arc's border.
 *    11: Pen Width (int): The width of the pen used to draw the arc's border.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw an arc on a window with customizable border color and width.
 *     It creates a pen for the border, selects it into the device context, draws the arc using Arc,
 *     and then cleans up by deselecting the pen and releasing the device context.
 */
HB_FUNC( ARCDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1;
   HPEN     hpen;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 11 ), RGB( HB_PARNI( 10, 1 ), HB_PARNI( 10, 2 ), HB_PARNI( 10, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   Arc( hdc1, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 5 ), hb_parni( 4 ), hb_parni( 7 ), hb_parni( 6 ), hb_parni( 9 ), hb_parni( 8 ) );
   SelectObject( hdc1, hgdiobj1 );
   DeleteObject( hpen );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( PIEDRAW )
 *
 *  Description:
 *     Draws a pie slice on a window using the specified bounding rectangle, start and end points, border color,
 *     fill color, and fill option.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the pie slice.
 *     2: Top (int): The y-coordinate of the upper-left corner of the bounding rectangle.
 *     3: Left (int): The x-coordinate of the upper-left corner of the bounding rectangle.
 *     4: Bottom (int): The y-coordinate of the lower-right corner of the bounding rectangle.
 *     5: Right (int): The x-coordinate of the lower-right corner of the bounding rectangle.
 *     6: Y Start (int): The y-coordinate of the pie's starting point.
 *     7: X Start (int): The x-coordinate of the pie's starting point.
 *     8: Y End (int): The y-coordinate of the pie's ending point.
 *     9: X End (int): The x-coordinate of the pie's ending point.
 *    10: Border Color (PHB_ITEM): Harbour array representing the RGB color of the pie's border.
 *    11: Pen Width (int): The width of the pen used to draw the pie's border.
 *    12: Fill Color (PHB_ITEM): Harbour array representing the RGB color to fill the pie slice with.
 *    13: Fill (logical): .T. to fill the pie slice with the specified color, .F. for a hollow pie slice.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw a pie slice on a window with customizable border color, width,
 *     fill color, and fill option. It creates a pen for the border, a brush for the fill (solid or hollow),
 *     selects them into the device context, draws the pie slice using Pie, and then cleans up by deselecting
 *     the objects and releasing the device context.
 */
HB_FUNC( PIEDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1, hgdiobj2;
   HPEN     hpen;
   HBRUSH   hbrush;
   LOGBRUSH br;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 11 ), RGB( HB_PARNI( 10, 1 ), HB_PARNI( 10, 2 ), HB_PARNI( 10, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   if( hb_parl( 13 ) )
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 12, 1 ), HB_PARNI( 12, 2 ), HB_PARNI( 12, 3 ) ) );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }
   else
   {
      br.lbStyle = BS_HOLLOW;
      hbrush = CreateBrushIndirect( &br );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }

   Pie( hdc1, hb_parni( 3 ), hb_parni( 2 ), hb_parni( 5 ), hb_parni( 4 ), hb_parni( 7 ), hb_parni( 6 ), hb_parni( 9 ), hb_parni( 8 ) );
   SelectObject( hdc1, hgdiobj1 );
   SelectObject( hdc1, hgdiobj2 );
   DeleteObject( hpen );
   DeleteObject( hbrush );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( POLYGONDRAW )
 *
 *  Description:
 *     Draws a polygon on a window using the specified array of points, border color, fill color, and fill option.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the polygon.
 *     2: X Coordinates (PHB_ITEM): Harbour array containing the x-coordinates of the polygon's vertices. The first element of the array must be the number of points.
 *     3: Y Coordinates (PHB_ITEM): Harbour array containing the y-coordinates of the polygon's vertices. The first element of the array must be the number of points.
 *     4: Border Color (PHB_ITEM): Harbour array representing the RGB color of the polygon's border.
 *     5: Pen Width (int): The width of the pen used to draw the polygon's border.
 *     6: Fill Color (PHB_ITEM): Harbour array representing the RGB color to fill the polygon with.
 *     7: Fill (logical): .T. to fill the polygon with the specified color, .F. for a hollow polygon.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw a polygon on a window with customizable vertices, border color, width,
 *     fill color, and fill option. It creates a pen for the border, a brush for the fill (solid or hollow),
 *     selects them into the device context, constructs the POINT array from the Harbour arrays, draws the polygon
 *     using Polygon, and then cleans up by deselecting the objects and releasing the device context.
 */
HB_FUNC( POLYGONDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1, hgdiobj2;
   HPEN     hpen;
   HBRUSH   hbrush;
   LOGBRUSH br;
   POINT    apoints[1024];
   int      number = ( int ) hb_parinfa( 2, 0 );
   int      i;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 5 ), RGB( HB_PARNI( 4, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   if( hb_parl( 7 ) )
   {
      hbrush = CreateSolidBrush( RGB( HB_PARNI( 6, 1 ), HB_PARNI( 6, 2 ), HB_PARNI( 6, 3 ) ) );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }
   else
   {
      br.lbStyle = BS_HOLLOW;
      hbrush = CreateBrushIndirect( &br );
      hgdiobj2 = SelectObject( hdc1, hbrush );
   }

   for( i = 0; i <= number - 1; i++ )
   {
      apoints[i].x = HB_PARNI( 2, i + 1 );
      apoints[i].y = HB_PARNI( 3, i + 1 );
   }

   Polygon( hdc1, apoints, number );
   SelectObject( hdc1, hgdiobj1 );
   SelectObject( hdc1, hgdiobj2 );
   DeleteObject( hpen );
   DeleteObject( hbrush );
   ReleaseDC( hWnd1, hdc1 );
}

/*
 *  HB_FUNC( POLYBEZIERDRAW )
 *
 *  Description:
 *     Draws a series of Bezier curves on a window using the specified array of points and border color.
 *
 *  Parameters:
 *     1: hWnd (HWND): Handle to the window on which to draw the Bezier curves.
 *     2: X Coordinates (PHB_ITEM): Harbour array containing the x-coordinates of the Bezier curve's control points. The first element of the array must be the number of points.
 *     3: Y Coordinates (PHB_ITEM): Harbour array containing the y-coordinates of the Bezier curve's control points. The first element of the array must be the number of points.
 *     4: Border Color (PHB_ITEM): Harbour array representing the RGB color of the Bezier curve's border.
 *     5: Pen Width (int): The width of the pen used to draw the Bezier curve's border.
 *
 *  Return Value:
 *     None.
 *
 *  Purpose:
 *     This function provides a way to draw a series of Bezier curves on a window with customizable control points
 *     and border color. It creates a pen for the border, selects it into the device context, constructs the POINT
 *     array from the Harbour arrays, draws the Bezier curves using PolyBezier, and then cleans up by deselecting
 *     the pen and releasing the device context.
 */
HB_FUNC( POLYBEZIERDRAW )
{
   HWND     hWnd1;
   HDC      hdc1;
   HGDIOBJ  hgdiobj1;
   HPEN     hpen;
   POINT    apoints[1024];
   DWORD    number = ( DWORD ) hb_parinfa( 2, 0 );
   DWORD    i;

   hWnd1 = hmg_par_raw_HWND( 1 );
   hdc1 = GetDC( hWnd1 );
   hpen = CreatePen( PS_SOLID, hb_parni( 5 ), RGB( HB_PARNI( 4, 1 ), HB_PARNI( 4, 2 ), HB_PARNI( 4, 3 ) ) );
   hgdiobj1 = SelectObject( hdc1, hpen );
   for( i = 0; i <= number - 1; i++ )
   {
      apoints[i].x = HB_PARNI( 2, i + 1 );
      apoints[i].y = HB_PARNI( 3, i + 1 );
   }

   PolyBezier( hdc1, apoints, number );
   SelectObject( hdc1, hgdiobj1 );
   DeleteObject( hpen );
   ReleaseDC( hWnd1, hdc1 );
}

/*
   WndDrawBox

   Draws a box using two different pens to create a 3D effect.

   Parameters:
       hDC       : Handle to the device context on which to draw.
       rct       : Pointer to a RECT structure defining the rectangle to draw.
       hPUpLeft  : Handle to the pen used for the top and left sides of the box.  This pen typically represents a highlight color.
       hPBotRit  : Handle to the pen used for the bottom and right sides of the box. This pen typically represents a shadow color.

   Return Value:
       None

   Purpose:
       This function draws a rectangle using two different pens to simulate a 3D border effect.  It's used to give the appearance of depth or elevation to UI elements. The function selects the first pen, draws the top and left sides, then selects the second pen and draws the bottom and right sides.  The original pen is restored at the end. The -1 in LineTo( hDC, rct->right, rct->top - 1 ); is to avoid overlapping lines.
*/
void WndDrawBox( HDC hDC, RECT *rct, HPEN hPUpLeft, HPEN hPBotRit )
{
   HPEN  hOldPen = ( HPEN ) SelectObject( hDC, hPUpLeft );
   POINT pt;

   MoveToEx( hDC, rct->left, rct->bottom, &pt );

   LineTo( hDC, rct->left, rct->top );
   LineTo( hDC, rct->right, rct->top );
   SelectObject( hDC, hPBotRit );

   MoveToEx( hDC, rct->left, rct->bottom, &pt );

   LineTo( hDC, rct->right, rct->bottom );
   LineTo( hDC, rct->right, rct->top - 1 );

   SelectObject( hDC, hOldPen );
}

/*
   WindowBoxIn

   Draws a sunken box around a rectangle.

   Parameters:
       hDC   : Handle to the device context on which to draw.
       pRect : Pointer to a RECT structure defining the rectangle to draw around.

   Return Value:
       None

   Purpose:
       This function draws a rectangle that appears to be sunken into the background. It achieves this effect by using a gray pen for the top and left sides (simulating shadow) and a white pen for the bottom and right sides (simulating highlight).  It calls WndDrawBox to perform the actual drawing. The pens are created and then deleted to avoid resource leaks.
*/
void WindowBoxIn( HDC hDC, RECT *pRect )
{
   HPEN  hWhite = CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNHIGHLIGHT ) );
   HPEN  hGray = CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNSHADOW ) );

   WndDrawBox( hDC, pRect, hGray, hWhite );

   DeleteObject( hGray );
   DeleteObject( hWhite );
}

/*
   WindowRaised

   Draws a raised box around a rectangle.

   Parameters:
       hDC   : Handle to the device context on which to draw.
       pRect : Pointer to a RECT structure defining the rectangle to draw around.

   Return Value:
       None

   Purpose:
       This function draws a rectangle that appears to be raised above the background. It achieves this effect by using a white pen for the top and left sides (simulating highlight) and a gray pen for the bottom and right sides (simulating shadow). It calls WndDrawBox to perform the actual drawing. The pens are created and then deleted to avoid resource leaks.
*/
void WindowRaised( HDC hDC, RECT *pRect )
{
   HPEN  hGray = CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNSHADOW ) );
   HPEN  hWhite = CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNHIGHLIGHT ) );

   WndDrawBox( hDC, pRect, hWhite, hGray );

   DeleteObject( hGray );
   DeleteObject( hWhite );
}

/*
   WNDBOXIN

   Harbour function to draw a sunken box around a rectangle.

   Parameters:
       Parameter 1: HDC - Handle to the device context on which to draw.  Passed from Harbour as a raw HDC value.
       Parameter 2: Top coordinate of the rectangle.
       Parameter 3: Left coordinate of the rectangle.
       Parameter 4: Bottom coordinate of the rectangle.
       Parameter 5: Right coordinate of the rectangle.

   Return Value:
       None

   Purpose:
       This function is a Harbour callable function that draws a sunken box. It retrieves the rectangle coordinates from the Harbour parameters, constructs a RECT structure, and then calls the WindowBoxIn function to perform the actual drawing.  hmg_par_raw_HDC is a helper function (likely from the HMG Extended library) to safely retrieve the HDC from the Harbour parameter stack. hb_parni is a Harbour function to retrieve numeric parameters.
*/
HB_FUNC( WNDBOXIN )
{
   RECT  rct;

   rct.top = hb_parni( 2 );
   rct.left = hb_parni( 3 );
   rct.bottom = hb_parni( 4 );
   rct.right = hb_parni( 5 );

   WindowBoxIn( hmg_par_raw_HDC( 1 ), &rct );
}

/*
   WNDBOXRAISED

   Harbour function to draw a raised box around a rectangle.

   Parameters:
       Parameter 1: HDC - Handle to the device context on which to draw. Passed from Harbour as a raw HDC value.
       Parameter 2: Top coordinate of the rectangle.
       Parameter 3: Left coordinate of the rectangle.
       Parameter 4: Bottom coordinate of the rectangle.
       Parameter 5: Right coordinate of the rectangle.

   Return Value:
       None

   Purpose:
       This function is a Harbour callable function that draws a raised box. It retrieves the rectangle coordinates from the Harbour parameters, constructs a RECT structure, and then calls the WindowRaised function to perform the actual drawing. hmg_par_raw_HDC is a helper function (likely from the HMG Extended library) to safely retrieve the HDC from the Harbour parameter stack. hb_parni is a Harbour function to retrieve numeric parameters.
*/
HB_FUNC( WNDBOXRAISED )
{
   RECT  rct;

   rct.top = hb_parni( 2 );
   rct.left = hb_parni( 3 );
   rct.bottom = hb_parni( 4 );
   rct.right = hb_parni( 5 );

   WindowRaised( hmg_par_raw_HDC( 1 ), &rct );
}
