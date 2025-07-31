/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * (c) 2021 Grigory Filatov <gfilatov@inbox.ru>
 */

#include <hmg.ch>

/*
 * FUNCTION Main()
 *
 * This is the main function of the application. It defines and activates the main window,
 * including an image control that is centered within the window.
 *
 * Purpose:
 *   This function serves as the entry point for the application. It creates the main window,
 *   defines an image control within it, and sets up event handlers for window resizing and key presses.
 *   The image is centered within the window using the Img_center procedure. The F2 key allows the user to
 *   adjust the window size to fit the image perfectly. The ESCAPE key closes the window.
 *
 * Notes:
 *   The image is loaded from a resource named 'OLGA'. The HMG_GetFormControls function is used to
 *   retrieve the image control object. The window's ON SIZE and ON MAXIMIZE events are handled by the
 *   Img_center procedure to ensure the image remains centered when the window is resized or maximized.
 */
FUNCTION Main()
   LOCAL cImgCtl

   DEFINE WINDOW win_1 ;
         MAIN ;
         CLIENTAREA 1.45 * 352, 1.3 * 450 ;
         TITLE "Center Image From Resource (press F2 for form adjusting)" ;
         BACKCOLOR TEAL ;
         ON INIT Img_center( cImgCtl ) ;
         ON SIZE Img_center( cImgCtl ) ;
         ON MAXIMIZE Img_center( cImgCtl )

      ON KEY F2 ACTION ;
         ( win_1.width := win_1.(cImgCtl).width + 2 * getborderwidth(), ;
         win_1.height := win_1.(cImgCtl).height + gettitleheight() + 2 * getborderheight(), ;
         win_1.Center )

      ON KEY ESCAPE ACTION win_1.Release()

      DEFINE IMAGE NUL
         PICTURE 'OLGA'
      END IMAGE

      cImgCtl := HMG_GetFormControls( This.Name, "IMAGE" )[ 1 ]

   END WINDOW

   win_1.Center()
   win_1.Activate()

RETURN NIL

/*
 * PROCEDURE Img_center( cImage )
 *
 * Centers an image control within its parent window.
 *
 * Parameters:
 *   cImage : The name of the image control to center (string).
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure is designed to dynamically center an image control within its parent window,
 *   regardless of the window's size. It calculates the new row and column positions for the image
 *   based on the window's client area dimensions and the image's dimensions. This ensures that the
 *   image remains centered even when the window is resized or maximized.
 *
 * Notes:
 *   The procedure first hides the image control to prevent flickering during the repositioning process.
 *   It then calculates the new row and column positions based on the difference between the window's
 *   client area dimensions and the image's dimensions, divided by 2. Finally, it shows the image control
 *   at its new centered position. The 'this' keyword refers to the parent window object.
 */
PROCEDURE Img_center( cImage )

   this.( cImage ).Hide
   this.( cImage ).row := ( thiswindow.clientheight - this.( cImage ).height ) / 2
   this.( cImage ).col := ( thiswindow.clientwidth - this.( cImage ).width ) / 2
   this.( cImage ).Show

RETURN
