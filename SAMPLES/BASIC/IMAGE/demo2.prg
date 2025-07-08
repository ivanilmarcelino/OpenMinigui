#include "hmg.ch"

STATIC hBitmap1, hBitmap2
STATIC hBitmap
STATIC cImageFile := "overlay.png"

FUNCTION Main()

   hBitmap1 := BT_BitmapLoadFile ("olga1.jpg")
   hBitmap2 := BT_BitmapLoadFile ("calendar.bmp")

   MakeBitmap()

   DEFINE WINDOW Form_1 ;
      AT 90,90 ;
      CLIENTAREA BT_BitmapWidth(hBitmap1),BT_BitmapHeight(hBitmap1) ;
      TITLE "Image Overlay" ;
      MAIN ;
      ON INIT     Proc_ON_INIT() ;
      ON RELEASE  Proc_ON_RELEASE() ;
      ON PAINT    Proc_ON_PAINT()

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE Proc_ON_INIT

   BT_BitmapRelease (hBitmap1)
   BT_BitmapRelease (hBitmap2)

   hBitmap := BT_BitmapLoadFile (cImageFile)

RETURN


PROCEDURE Proc_ON_RELEASE

   BT_BitmapRelease (hBitmap)
   FErase (cImageFile)

RETURN


PROCEDURE Proc_ON_PAINT

   LOCAL hDC, BTstruct
   LOCAL w, h

   w := BT_BitmapWidth(hBitmap)
   h := BT_BitmapHeight(hBitmap)

   hDC := BT_CreateDC ("Form_1", BT_HDC_INVALIDCLIENTAREA, @BTstruct)
      BT_DrawBitmap (hDC, 0, 0, w, h, BT_STRETCH, hBitmap)
   BT_DeleteDC (BTstruct)

RETURN


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
