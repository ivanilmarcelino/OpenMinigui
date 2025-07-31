/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2008 Walter Formigoni <walter.formigoni@uol.com.br>

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

#include "i_winuser.ch"
#include "minigui.ch"

#define DT_CENTER 1

/*
 * FUNCTION OwnTabPaint(lParam)
 *
 * Custom painting function for tab controls.
 *
 * Parameters:
 *   lParam (NUMERIC): A parameter that contains information about the control to be painted.
 *
 * Return Value:
 *   NUMERIC: Returns 0 if successful, otherwise returns 1.
 *
 * Purpose:
 *   This function is responsible for custom painting of tab controls. It handles the drawing of the tab buttons,
 *   including their text and images, and manages the visual state of the tabs (e.g., selected, inactive).
 *
 * Notes:
 *   This function uses various Windows API functions to achieve custom drawing of the tab controls.
 *   It handles different visual states and styles of the tabs, such as selected, inactive, and hot-tracked tabs.
 */
FUNCTION OwnTabPaint( lParam )

   LOCAL hDC, hBrush, hOldFont, hImage
   LOCAL aBkColor, aForeColor, aInactiveColor, aBmp, aMetr, aBtnRc
   LOCAL oldTextColor, oldBkMode, nTextColor, bkColor
   LOCAL i, nItemId, x1, y1, x2, y2, xp1, yp1, xp2, yp2
   LOCAL lSelected, lBigFsize, lBigFsize2, lBigFsize3

   // Get the device context for the control
   hDC := GETOWNBTNDC( lParam )

   // Find the index of the control in the global array of control handles
   i := AScan( _HMG_aControlHandles, GETOWNBTNHANDLE( lParam ) )

   // Check if the device context is empty or the control index is invalid
   IF Empty( hDC ) .OR. i == 0
      RETURN( 1 )
   ENDIF

   // Get the item ID of the tab
   nItemId := GETOWNBTNITEMID( lParam ) + 1

   // Get the rectangle of the tab button
   aBtnRc := GETOWNBTNRECT( lParam )

   // Check if the tab is selected
   lSelected := ( AND( GETOWNBTNSTATE( lParam ), ODS_SELECTED ) == ODS_SELECTED )

   // Check the font size to adjust text positioning
   lBigFsize := ( _HMG_aControlFontSize[ i ] >= 12 )
   lBigFsize2 := ( _HMG_aControlFontSize[ i ] >= 18 )
   lBigFsize3 := ( _HMG_aControlFontSize[ i ] >= 24 )

   // Store the height of the tab
   _HMG_aControlMiscData1[ i ][ 1 ] := aBtnRc[ 4 ] - aBtnRc[ 2 ]

   // Select the font for the control
   hOldFont := SelectObject( hDC, _HMG_aControlFontHandle[ i ] )

   // Get text metrics for the selected font
   aMetr := GetTextMetric( hDC )

   // Set the background mode to transparent
   oldBkMode := SetBkMode( hDC, TRANSPARENT )

   // Get the system color for button text
   nTextColor := GetSysColor( COLOR_BTNTEXT )

   // Set the text color
   oldTextColor := SetTextColor( hDC, GetRed( nTextColor ), GetGreen( nTextColor ), GetBlue( nTextColor ) )

   // Determine the background color for the tab
   IF ISARRAY( _HMG_aControlMiscData2[ i ] ) .AND. nItemId <= Len( _HMG_aControlMiscData2[ i ] ) .AND. IsArrayRGB( _HMG_aControlMiscData2[ i ][ nItemId ] )
      aBkColor := _HMG_aControlMiscData2[ i ][ nItemId ]
   ELSE
      aBkColor := _HMG_aControlBkColor[ i ]
   ENDIF

   // Convert the background color to RGB
   bkColor := RGB( aBkColor[ 1 ], aBkColor[ 2 ], aBkColor[ 3 ] )

   // Set the background color
   SetBkColor( hDC, bkColor )

   // Create a solid brush for the background
   hBrush := CreateSolidBrush( aBkColor[ 1 ], aBkColor[ 2 ], aBkColor[ 3 ] )

   // Fill the tab rectangle with the background color
   FillRect( hDC, aBtnRc[ 1 ], aBtnRc[ 2 ], aBtnRc[ 3 ], aBtnRc[ 4 ], hBrush )

   // Delete the brush to free resources
   DeleteObject( hBrush )

   // Calculate text position
   x1 := aBtnRc[ 1 ]
   y1 := Round( aBtnRc[ 4 ] / 2, 0 ) - ( aMetr[ 1 ] - 10 )
   x2 := aBtnRc[ 3 ] - 2
   y2 := y1 + aMetr[ 1 ]

   // Check if the tab has an image
   IF _HMG_aControlMiscData1[ i ][ 2 ] // ImageFlag
      // Ensure the item ID is within the bounds of the picture array
      nItemId := Min( nItemId, Len( _HMG_aControlPicture[ i ] ) )

      // Load the bitmap for the tab
      hImage := LoadBitmap( _HMG_aControlPicture[ i ][ nItemId ] )

      // If the bitmap is not loaded, try to load it as an image
      IF Empty( hImage )
         hImage := LoadImage( _HMG_aControlPicture[ i ][ nItemId ], , , , , , bkColor )
      ENDIF

      // Get the size of the bitmap
      aBmp := GetBitmapSize( hImage )

      // Calculate the position for the image
      xp1 := 4
      xp2 := aBmp[ 1 ]
      yp2 := aBmp[ 2 ]
      yp1 := Round( aBtnRc[ 4 ] / 2 - yp2 / 2, 0 )
      x1 += 2 * xp1 + xp2

      // Draw the image on the tab
      IF _HMG_aControlMiscData1[ i ][ 4 ] // Bottom Tab
         IF lSelected
            DrawGlyph( hDC, aBtnRc[ 1 ] + 2 * xp1, 2 * yp1 - iif( lBigFsize, 8, 5 ), xp2, 2 * yp2 - iif( lBigFsize, 8, 5 ), hImage, bkColor, .F., .F. )
         ELSE
            DrawGlyph( hDC, aBtnRc[ 1 ] + xp1, 2 * yp1 - iif( lBigFsize, 8, 5 ), xp2, 2 * yp2 - iif( lBigFsize, 8, 5 ), hImage, bkColor, .F., .F. )
         ENDIF
      ELSE
         IF lSelected
            DrawGlyph( hDC, aBtnRc[ 1 ] + 2 * xp1, yp1 - 2, xp2, yp2, hImage, bkColor, .F., .F. )
         ELSE
            DrawGlyph( hDC, aBtnRc[ 1 ] + xp1, yp1 + 2, xp2, yp2, hImage, bkColor, .F., .F. )
         ENDIF
      ENDIF

      // Delete the image to free resources
      DeleteObject( hImage )
   ENDIF

   // Set the text color based on the tab state
   IF lSelected
      IF _HMG_aControlMiscData1[ i ][ 5 ] // HotTrack
         IF IsArrayRGB( aForeColor := _HMG_aControlMiscData1[ i ][ 6 ] )
            SetTextColor( hDC, aForeColor[ 1 ], aForeColor[ 2 ], aForeColor[ 3 ] )
         ELSEIF bkColor == GetSysColor( COLOR_BTNFACE )
            SetTextColor( hDC, 0, 0, 128 )
         ELSE
            SetTextColor( hDC, 255, 255, 255 )
         ENDIF
      ENDIF
   ELSE
      IF IsArrayRGB( aInactiveColor := _HMG_aControlMiscData1[ i ][ 7 ] )
         SetTextColor( hDC, aInactiveColor[ 1 ], aInactiveColor[ 2 ], aInactiveColor[ 3 ] )
      ENDIF
   ENDIF

   // Draw the text on the tab
   IF _HMG_aControlMiscData1[ i ][ 4 ] // Bottom Tab
      IF lSelected
         DrawText( hDC, _HMG_aControlCaption[ i ][ nItemId ], x1, 2 * y1 - iif( lBigFsize3, -12, iif( lBigFsize2, -3, iif(lBigFsize, 6, 12 ) ) ), x2, 2 * y2 - iif( lBigFsize3, -12, iif( lBigFsize2, -3, iif(lBigFsize, 6, 12 ) ) ), DT_CENTER )
      ELSE
         DrawText( hDC, _HMG_aControlCaption[ i ][ nItemId ], x1, 2 * y1 - iif( lBigFsize3, -18, iif( lBigFsize2, -8, iif(lBigFsize, 0, 8 ) ) ), x2, 2 * y2 - iif( lBigFsize3, -18, iif( lBigFsize2, -8, iif(lBigFsize, 0, 8 ) ) ), DT_CENTER )
      ENDIF
   ELSE
      IF lSelected
         DrawText( hDC, _HMG_aControlCaption[ i ][ nItemId ], x1, y1 - iif( lBigFsize3, -9, iif( lBigFsize2, -5, iif(lBigFsize, 0, 4 ) ) ), x2, y2 - iif( lBigFsize3, -9, iif( lBigFsize2, -5, iif(lBigFsize, 0, 4 ) ) ), DT_CENTER )
      ELSE
         DrawText( hDC, _HMG_aControlCaption[ i ][ nItemId ], x1, y1 + iif( lBigFsize3, 14, iif( lBigFsize2, 8, iif(lBigFsize, 4, 0 ) ) ), x2, y2 + iif( lBigFsize3, 14, iif( lBigFsize2, 8, iif(lBigFsize, 4, 0 ) ) ), DT_CENTER )
      ENDIF
   ENDIF

   // Restore the original font and background mode
   SelectObject( hDC, hOldFont )
   SetBkMode( hDC, oldBkMode )
   SetTextColor( hDC, oldTextColor )

RETURN( 0 )
