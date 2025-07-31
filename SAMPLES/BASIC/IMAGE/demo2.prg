#include "hmg.ch"

STATIC hBitmap1, hBitmap2
STATIC hBitmap
STATIC cImageFile := "overlay.png"

/*
 * FUNCTION Main()
 *
 * Initializes the application, loads bitmaps, defines the main window,
 * and activates the window to start the application's event loop.
 *
 * Purpose:
 *   This is the entry point of the application. It sets up the graphical
 *   user interface by loading the initial bitmaps, defining the main window
 *   with its properties and event handlers, and then activating the window
 *   to make it visible and responsive to user interactions.  It orchestrates
 *   the initial setup of the application's visual components.
 *
 * Notes:
 *   The function assumes that the bitmap files "olga1.jpg" and "calendar.bmp"
 *   exist in the same directory as the executable.  The cImageFile variable
 *   defines the name of the output image file that will be created by the
 *   MakeBitmap procedure.
 */
FUNCTION Main()

   hBitmap1 := BT_BitmapLoadFile ("olga1.jpg")
   hBitmap2 := BT_BitmapLoadFile ("calendar.bmp")

   MakeBitmap()

   DEFINE WINDOW Form_1 ;
      AT 90,90 ;
      CLIENTAREA BT_BitmapWidth(hBitmap1), BT_BitmapHeight(hBitmap1) ;
      TITLE "Image Overlay" ;
      MAIN ;
      ON INIT     Proc_ON_INIT() ;
      ON RELEASE  Proc_ON_RELEASE() ;
      ON PAINT    Proc_ON_PAINT()

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * PROCEDURE Proc_ON_INIT()
 *
 * Handles the initialization event of the main window.  Releases the initial bitmaps
 * and loads the overlay image.
 *
 * Purpose:
 *   This procedure is called when the main window is initialized. It releases the
 *   bitmaps loaded in the Main function and loads the overlay image created by
 *   the MakeBitmap procedure. This ensures that the overlay image is available
 *   for display when the window is first shown.
 *
 * Notes:
 *   This procedure assumes that the MakeBitmap procedure has already been called
 *   and that the overlay image file exists.
 */
PROCEDURE Proc_ON_INIT

   BT_BitmapRelease (hBitmap1)
   BT_BitmapRelease (hBitmap2)

   hBitmap := BT_BitmapLoadFile (cImageFile)

RETURN

/*
 * PROCEDURE Proc_ON_RELEASE()
 *
 * Handles the release event of the main window.  Releases the overlay bitmap
 * and deletes the overlay image file.
 *
 * Purpose:
 *   This procedure is called when the main window is released (closed). It releases
 *   the overlay bitmap to free up memory and deletes the overlay image file to clean
 *   up the file system. This ensures that resources are properly released when the
 *   application is closed.
 *
 * Notes:
 *   The FErase function is used to delete the overlay image file.  Error handling
 *   should be added to handle cases where the file cannot be deleted (e.g., if it is
 *   in use by another process).
 */
PROCEDURE Proc_ON_RELEASE

   BT_BitmapRelease (hBitmap)
   FErase (cImageFile)

RETURN

/*
 * PROCEDURE Proc_ON_PAINT()
 *
 * Handles the paint event of the main window.  Draws the overlay bitmap onto the
 * window's client area.
 *
 * Purpose:
 *   This procedure is called when the main window needs to be repainted. It retrieves
 *   the dimensions of the overlay bitmap, creates a device context (DC) for the window's
 *   client area, draws the bitmap onto the DC, and then deletes the DC. This ensures
 *   that the overlay image is displayed correctly in the window.
 *
 * Notes:
 *   The BT_STRETCH flag is used to stretch the bitmap to fit the window's client area.
 *   If the bitmap's aspect ratio does not match the window's aspect ratio, the image
 *   may be distorted.
 */
PROCEDURE Proc_ON_PAINT

   LOCAL hDC, BTstruct
   LOCAL w, h

   w := BT_BitmapWidth(hBitmap)
   h := BT_BitmapHeight(hBitmap)

   hDC := BT_CreateDC ("Form_1", BT_HDC_INVALIDCLIENTAREA, @BTstruct)
      BT_DrawBitmap (hDC, 0, 0, w, h, BT_STRETCH, hBitmap)
   BT_DeleteDC (BTstruct)

RETURN

/*
 * PROCEDURE MakeBitmap()
 *
 * Creates a new bitmap by cloning the first bitmap, pasting the second bitmap
 * onto it with transparency, saving the resulting bitmap to a file, and then
 * releasing the cloned bitmap.
 *
 * Purpose:
 *   This procedure creates an overlay image by combining two bitmaps. It clones
 *   the first bitmap, pastes the second bitmap onto the clone with transparency,
 *   saves the resulting bitmap to a file, and then releases the cloned bitmap to
 *   free up memory. This allows creating a composite image that can be displayed
 *   in the application.
 *
 * Notes:
 *   The procedure uses the BT_BitmapPasteTransparent function to paste the second
 *   bitmap onto the clone with transparency. The BT_FILEFORMAT_PNG flag is used
 *   to save the bitmap in PNG format, which supports transparency. The position of
 *   the second bitmap is calculated based on the dimensions of the two bitmaps.
 */
PROCEDURE MakeBitmap

   LOCAL hBitmap := BT_BitmapClone (hBitmap1)
   LOCAL t, l, w1, h1, w2, h2

   w1 := BT_BitmapWidth(hBitmap1)
   h1 := BT_BitmapHeight(hBitmap1)
   w2 := BT_BitmapWidth(hBitmap2)
   h2 := BT_BitmapHeight(hBitmap2)

   t := h1 - h2 - 20
   l := w1 - w2 - 20

   BT_BitmapPasteTransparent (hBitmap, t, l, w2, h2, BT_STRETCH, hBitmap2, NIL) 
   BT_BitmapSaveFile (hBitmap, cImageFile, BT_FILEFORMAT_PNG) 
   BT_BitmapRelease (hBitmap)

RETURN
