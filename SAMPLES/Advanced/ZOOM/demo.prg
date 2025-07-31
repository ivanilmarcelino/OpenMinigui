/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2012 Rossine <qiinfo@ig.com.br>
*/

#include "minigui.ch"

#define SRCCOPY  0x00CC0020 // Defines a raster operation code (ROP code) used in BitBlt and StretchBlt functions.
                            // SRCCOPY means that the source is copied directly to the destination.

/*
 * PROCEDURE Main
 *
 * Initializes the main window, defines its controls (labels), menu, and a timer for image zooming.
 *
 * Purpose:
 *   This is the entry point of the application. It performs the following tasks:
 *     1. Defines the main window with its properties (title, size, etc.).
 *     2. Creates two labels: one to display the color under the cursor and another to display the RGB values.
 *     3. Defines a main menu with zoom factor options and an exit option.
 *     4. Sets up a timer that calls the ZoomImage() function periodically to update the zoomed image.
 *     5. Centers the main window on the screen.
 *     6. Activates the main window, making it visible to the user.
 *
 *   The application demonstrates how to capture a portion of the screen around the cursor, zoom it, and display it in a window.
 *   It also shows how to use a timer to periodically update the display.
 */
PROCEDURE Main

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

/*
 * STATIC FUNCTION check_menu( nFactor )
 *
 * Updates the checked state of zoom factor menu items to ensure only one is selected at a time.
 *
 * Parameters:
 *   nFactor - The zoom factor that was selected (1 to 5).
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This function is called when a zoom factor menu item is selected. It iterates through all zoom factor menu items
 *   and sets their "Checked" property to .T. only for the selected factor (nFactor), and .F. for all others.
 *   This ensures that only one zoom factor is active at any given time, providing a mutually exclusive selection.
 */
STATIC FUNCTION check_menu( nFactor )
   LOCAL n

   FOR n := 1 TO 5
      SetProperty( "Form_1", "Factor" + Str( n, 1 ), "Checked", ( n == nFactor ) )
   NEXT

RETURN NIL

/*
 * STATIC FUNCTION GetZoom()
 *
 * Determines the currently selected zoom factor based on which menu item is checked.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   nZoom - The currently selected zoom factor (1 to 5). Defaults to 3 if none are checked.
 *
 * Purpose:
 *   This function iterates through the zoom factor menu items and checks their "Checked" property.
 *   It returns the corresponding zoom factor if a menu item is checked. If no zoom factor is explicitly selected,
 *   it defaults to a zoom factor of 3. This function is used by ZoomImage() to determine the zoom level.
 */
STATIC FUNCTION GetZoom()
   LOCAL n
   LOCAL nZoom := 3

   FOR n := 1 TO 5
      IF GetProperty( "Form_1", "Factor" + Str( n, 1 ), "Checked" ) == .T.
         nZoom := n
         EXIT
      ENDIF
   NEXT

RETURN nZoom

/*
 * STATIC FUNCTION ZoomImage()
 *
 * Captures a portion of the screen around the cursor, zooms it, and displays it in the main window.
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This function is called by the timer to periodically update the zoomed image. It performs the following tasks:
 *     1. Gets the current zoom factor using GetZoom().
 *     2. Gets the device context for the entire screen (desktop).
 *     3. Gets the current cursor position.
 *     4. Gets the color of the pixel under the cursor and displays it in Label_1 and Label_2.
 *     5. Gets the device context for the main window.
 *     6. Creates a pen for drawing the crosshairs.
 *     7. Calculates the position and size of the zoomed image in the main window.
 *     8. Uses StretchBlt() to stretch a portion of the screen around the cursor into the main window.
 *     9. Draws crosshairs in the center of the zoomed image.
 *     10. Releases the device contexts and deletes the pen.
 *
 *   This function demonstrates how to capture screen content, zoom it, and display it in a window using Windows API calls.
 */
STATIC FUNCTION ZoomImage()
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

/*
 * HB_FUNC( STRETCHBLT )
 *
 * Harbour wrapper for the Windows API StretchBlt function.
 *
 * Parameters:
 *   1: hDC - Handle to the destination device context (HDC).
 *   2: nX - X-coordinate of the upper-left corner of the destination rectangle.
 *   3: nY - Y-coordinate of the upper-left corner of the destination rectangle.
 *   4: nWidth - Width of the destination rectangle.
 *   5: nHeight - Height of the destination rectangle.
 *   6: hDC - Handle to the source device context (HDC).
 *   7: nX1 - X-coordinate of the upper-left corner of the source rectangle.
 *   8: nY1 - Y-coordinate of the upper-left corner of the source rectangle.
 *   9: nWidth1 - Width of the source rectangle.
 *  10: nHeight1 - Height of the source rectangle.
 *  11: nROP - Raster operation code (ROP code) specifying how the source color data is to be combined with the destination color data.
 *
 * Return Value:
 *   .T. (TRUE) if the function succeeds, .F. (FALSE) otherwise.
 *
 * Purpose:
 *   This function provides a way to call the Windows API StretchBlt function from Harbour code.
 *   StretchBlt performs a bit-block transfer (bitblt) from a source device context to a destination device context,
 *   stretching or compressing the bitmap to fit the destination rectangle, if necessary. This is essential for zooming
 *   the image captured from the screen.
 */
HB_FUNC( STRETCHBLT )
{
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
