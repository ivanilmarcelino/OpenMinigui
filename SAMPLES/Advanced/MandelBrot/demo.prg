/*
  MINIGUI - Harbour Win32 GUI library Demo

  Author: Siri Rathinagiri <srgiri@dataone.in>

  Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include <hmg.ch>

STATIC lStop := .F.
STATIC nWidth := 800
STATIC nHeight := 600
STATIC hBitmap

FUNCTION MAIN

   DEFINE WINDOW main ;
      CLIENTAREA nWidth + 20, nHeight + 62 ;
      TITLE 'MandelBrot' ;
      MAIN ;
      NOMAXIMIZE NOSIZE ;
      ON PAINT Proc_ON_PAINT() ;
      ON RELEASE BT_BitmapRelease( hBitmap )

   DEFINE BUTTON b1
      row 10
      col 10
      WIDTH 80
      CAPTION "Draw"
      ACTION startcalc()
   END BUTTON

   DEFINE BUTTON stop
      ROW 10
      COL 110
      CAPTION 'Stop'
      ACTION lStop := .T.
   END BUTTON

   END WINDOW

   main.Center
   main.Activate

RETURN NIL


PROCEDURE Proc_ON_PAINT

   LOCAL hDC, BTstruct

   hDC := BT_CreateDC( "main", BT_HDC_INVALIDCLIENTAREA, @BTstruct )
   BT_DrawBitmap( hDC, 50, 10, nWidth, nHeight + 2, BT_COPY, hBitmap )
   BT_DeleteDC( BTstruct )

RETURN


FUNCTION startcalc

   LOCAL nWidth2 := nWidth / 2
   LOCAL nHeight2 := nHeight / 2
   LOCAL hDC, BTstruct, hDC2, BTstruct2
   LOCAL aColors := { ;
      { 66, 30, 15 }, ;
      { 25, 7, 26 }, ;
      { 9, 1, 47 }, ;
      { 4, 4, 73 }, ;
      { 0, 7, 100 }, ;
      { 12, 44, 138 }, ;
      { 24, 82, 177 }, ;
      { 57, 125, 209 }, ;
      { 134, 181, 229 }, ;
      { 211, 236, 248 }, ;
      { 241, 233, 191 }, ;
      { 248, 201, 95 }, ;
      { 255, 170, 0 }, ;
      { 204, 128, 0 }, ;
      { 153, 87, 0 }, ;
      { 106, 52, 3 } ;
      }, aColor
   LOCAL cx
   LOCAL cy
   LOCAL scale
   LOCAL limit
   LOCAL x, y
   LOCAL ax
   LOCAL ay
   LOCAL a1, a2
   LOCAL b1, b2
   LOCAL lp
   LOCAL nRow
   LOCAL nCol

   lStop := .F.
   IF hBitmap <> NIL
      BT_BitmapRelease( hBitmap )
   ENDIF

   BT_ClientAreaInvalidateAll ( "main" )
   hDC := BT_CreateDC ( "main", BT_HDC_INVALIDCLIENTAREA, @BTstruct )

   hBitmap := BT_BitmapCreateNew ( nWidth, nHeight + 2, WHITE )
   hDC2 := BT_CreateDC ( hBitmap, BT_HDC_BITMAP, @BTstruct2 )

   BT_DrawFillRectangle ( hDC, 50, 10, nWidth, nHeight + 2, { 255, 255, 255 }, { 0, 0, 0 }, 1 )
   BT_DrawFillRectangle ( hDC2, 0, 0, nWidth, nHeight + 2, { 255, 255, 255 }, { 0, 0, 0 }, 1 )

   cx := 0.05
   cy := 0.05
   scale := 0.005
   limit := 40

   FOR x := -nWidth2 TO nWidth2

      FOR y := -nHeight2 TO nHeight2

         ax := cx + ( x * scale )
         ay := cy + ( y * scale )
         a1 := ax
         b1 := ay

         FOR lp := 1 TO 255

            a2 := ( a1 * a1 ) - ( b1 * b1 ) + ax
            b2 := 2 * a1 * b1 + ay
            a1 := a2
            b1 := b2

            IF ( a1 * a1 ) + ( b1 * b1 ) > limit
               EXIT
            ENDIF

         NEXT lp

         IF lp < 255
            aColor := aColors[ iif( Int( Mod( lp, 16 ) ) > 0, Int( Mod( lp, 16 ) ), 1 ) ]
         ELSE
            aColor := { 0, 0, 0 }
         ENDIF

         nRow := 50 + nHeight2 + y
         nCol := 10 + nWidth2 + x

         BT_DrawSetPixel( hDC, nRow, nCol, aColor )
         BT_DrawSetPixel( hDC2, nRow - 50 , nCol - 10, aColor )

         DO EVENTS

         IF lStop
            EXIT
         ENDIF

      NEXT y

      IF lStop
         EXIT
      ENDIF

   NEXT x

   BT_DeleteDC( BTstruct )
   BT_DeleteDC( BTstruct2 )

RETURN NIL
