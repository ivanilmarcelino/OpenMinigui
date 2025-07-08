/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2012 Rossine <qiinfo@ig.com.br>
*/

#include "minigui.ch"

#define SRCCOPY  0x00CC0020 // Defines a raster operation code (ROP code) used in BitBlt and StretchBlt functions.
                            // SRCCOPY means that the source is copied directly to the destination.

PROCEDURE Main
  /*
   *  PROCEDURE Main
   *  -------------------------------------------------------------------------------------------------
   *  Purpose: This is the main procedure of the application. It defines and activates the main window,
   *           including its controls (labels) and menu. It also sets up a timer to periodically zoom the image
   *           under the cursor.
   */
   DEFINE WINDOW Form_1 ;
      MAIN ;
      CLIENTAREA 550, 350 ;
      TITLE 'Zoom' ;
      NOMAXIMIZE NOSIZE

      DEFINE LABEL Label_1
         ROW 5
         COL 5
         WIDTH 120
         HEIGHT 40
         VALUE ""
         CLIENTEDGE .T.
      END LABEL

      DEFINE LABEL Label_2
         ROW 50
         COL 5
         WIDTH 120
         HEIGHT 30
         VALUE ""
         FONTNAME "Courier New"
         FONTSIZE 12
         FONTBOLD .T.
         FONTCOLOR { 255, 255, 255 }
         BACKCOLOR { 000, 105, 000 }
         CENTERALIGN .T.
         CLIENTEDGE .T.
      END LABEL

      DEFINE MAIN MENU

         POPUP 'Zoom'

         ITEM 'Factor 1' NAME Factor1 ACTION check_menu( 1 )
         ITEM 'Factor 2' NAME Factor2 ACTION check_menu( 2 )
         ITEM 'Factor 3' NAME Factor3 ACTION check_menu( 3 ) CHECKED
         ITEM 'Factor 4' NAME Factor4 ACTION check_menu( 4 )
         ITEM 'Factor 5' NAME Factor5 ACTION check_menu( 5 )
         SEPARATOR
         ITEM 'Exit' ACTION Form_1.Release

         END POPUP

      END MENU

      ON KEY ESCAPE ACTION ThisWindow.Release()

      DEFINE TIMER oTimer INTERVAL 40 ACTION ZoomImage()

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN


STATIC FUNCTION check_menu( nFactor )
  /*
   *  STATIC FUNCTION check_menu( nFactor )
   *  -------------------------------------------------------------------------------------------------
   *  Purpose: This function is called when a zoom factor menu item is selected. It updates the checked
   *           state of all zoom factor menu items to ensure that only one is checked at a time, providing
   *           a mutually exclusive selection of zoom factors.
   *
   *  Parameters:
   *      nFactor - The zoom factor that was selected (1 to 5).
   *
   *  Return Value: NIL
   */
   LOCAL n

   FOR n := 1 TO 5
      SetProperty( "Form_1", "Factor" + Str( n, 1 ), "Checked", ( n == nFactor ) )
   NEXT

RETURN NIL


STATIC FUNCTION GetZoom()
  /*
   *  STATIC FUNCTION GetZoom()
   *  -------------------------------------------------------------------------------------------------
   *  Purpose: This function determines the currently selected zoom factor based on which menu item is checked.
   *           It iterates through the zoom factor menu items and returns the corresponding zoom level.
   *           If no zoom factor is explicitly selected, it defaults to a zoom factor of 3.
   *
   *  Parameters: None
   *
   *  Return Value:
   *      nZoom - The currently selected zoom factor (1 to 5).  Defaults to 3 if none are checked.
   */
   LOCAL n
   LOCAL nZoom := 3

   FOR n := 1 TO 5
      IF GetProperty( "Form_1", "Factor" + Str( n, 1 ), "Checked" ) == .T.
         nZoom := n
         EXIT
      ENDIF
   NEXT

RETURN nZoom


STATIC FUNCTION ZoomImage()
  /*
   *  STATIC FUNCTION ZoomImage()
   *  -------------------------------------------------------------------------------------------------
   *  Purpose: This function is called by the timer to periodically zoom the image under the cursor.
   *           It retrieves the current zoom factor, captures a portion of the screen around the cursor,
   *           and displays it in a zoomed-in view within the main window.  It also displays the color
   *           of the pixel under the cursor in two labels. The function uses Windows API calls to capture
   *           the screen content and draw the zoomed image.
   *
   *  Parameters: None
   *
   *  Return Value: NIL
   */
   LOCAL nZoom
   LOCAL hDeskTop
   LOCAL aPos
   LOCAL hWnd
   LOCAL hDC
   LOCAL hPen
   LOCAL hOldPen, aColor := { 0, 0, 0 }
   LOCAL nTop, nLeft, nWidth, nHeight

   nZoom := GetZoom()

   hDeskTop := GetDC( 0 ) // Gets the device context for the entire screen (desktop).  The device context is a handle to a data structure that Windows uses internally.

   aPos := GetCursorPos() // Gets the current cursor position in screen coordinates.

   IF GetPixelColor( hDeskTop, aPos[ 2 ], aPos[ 1 ], aColor ) // Gets the color of the pixel at the cursor position.
      Form_1.Label_1.Backcolor := aColor // Sets the background color of Label_1 to the color of the pixel.
      Form_1.Label_2.Value := StrZero( aColor[ 1 ], 3 ) + "," + StrZero( aColor[ 2 ], 3 ) + "," + StrZero( aColor[ 3 ], 3 ) // Sets the value of Label_2 to the RGB values of the pixel.
   ENDIF

   hWnd := ThisWindow.Handle // Gets the handle of the main window.
   hDC := GetDC( hWnd ) // Gets the device context for the main window.
   hPen := CreatePen( 0, 1, 255 ) // Creates a pen for drawing the crosshairs.  The parameters are pen style, width, and color.

   nTop := 10
   nLeft := 130
   nWidth := 400
   nHeight := 300

   Moveto( hDC, nLeft - 1, nTop - 1 )
   Lineto( hDC, nLeft + nWidth, nTop - 1 )
   Lineto( hDC, nLeft + nWidth, nTop + nHeight )
   Lineto( hDC, nLeft - 1, nTop + nHeight )
   Lineto( hDC, nLeft - 1, nTop - 1 )

   StretchBlt( hDC, nLeft, nTop, nWidth, nHeight, hDeskTop, aPos[ 2 ] - nWidth / ( 2 * nZoom ), aPos[ 1 ] - nHeight / ( 2 * nZoom ), nWidth / nZoom, nHeight / nZoom, SRCCOPY ) // Stretches a portion of the screen around the cursor into the main window.

   hOldPen := SelectObject( hDC, hPen )

   Moveto( hDC, nLeft + nWidth / 2 + 1, nTop - 1 )
   Lineto( hDC, nLeft + nWidth / 2 + 1, nTop + nHeight + 1 )

   Moveto( hDC, nLeft + 1, nTop + nHeight / 2 + 1 )
   Lineto( hDC, nLeft + nWidth + 1, nTop + nHeight / 2 + 1 )

   SelectObject( hDC, hOldPen )
   DeleteObject( hPen )

   ReleaseDC( hWnd, hDC ) // Releases the device context for the main window.

   ReleaseDC( 0, hDeskTop ) // Releases the device context for the desktop.

RETURN NIL


#pragma BEGINDUMP

#include "mgdefs.h"

HB_FUNC( STRETCHBLT )
{
   /*
    * HB_FUNC( STRETCHBLT )
    * -------------------------------------------------------------------------------------------------
    * Purpose: This function is a Harbour wrapper for the Windows API StretchBlt function. It allows
    *          Harbour code to call the StretchBlt function, which performs a bit-block transfer (bitblt)
    *          from a source device context to a destination device context, stretching or compressing
    *          the bitmap to fit the destination rectangle, if necessary. This is essential for zooming
    *          the image captured from the screen.
    *
    * Parameters:
    *   1: hDC - Handle to the destination device context.
    *   2: nX - X-coordinate of the upper-left corner of the destination rectangle.
    *   3: nY - Y-coordinate of the upper-left corner of the destination rectangle.
    *   4: nWidth - Width of the destination rectangle.
    *   5: nHeight - Height of the destination rectangle.
    *   6: hDC - Handle to the source device context.
    *   7: nX1 - X-coordinate of the upper-left corner of the source rectangle.
    *   8: nY1 - Y-coordinate of the upper-left corner of the source rectangle.
    *   9: nWidth1 - Width of the source rectangle.
    *  10: nHeight1 - Height of the source rectangle.
    *  11: nROP - Raster operation code (ROP code).  Specifies how the source color data is to be combined with the destination color data.
    *
    * Return Value:
    *   Returns .T. (TRUE) if the function succeeds, .F. (FALSE) otherwise.
    */
   hb_retl( StretchBlt( ( HDC ) HB_PARNL( 1 ) ,
                        hb_parni( 2 ) ,
                        hb_parni( 3 ) ,
                        hb_parni( 4 ) ,
                        hb_parni( 5 ) ,
                        ( HDC ) HB_PARNL( 6 ) ,
                        hb_parni( 7 ) ,
                        hb_parni( 8 ) ,
                        hb_parni( 9 ) ,
                        hb_parni( 10 ) ,
                        ( DWORD ) hb_parnl( 11 )
                        ) );
}

#pragma ENDDUMP
