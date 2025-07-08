/*
 * Author: P.Chornyj <myorg63@mail.ru>
 */

ANNOUNCE RDDSYS

#include "minigui.ch"


PROCEDURE Main()

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 320 HEIGHT 240 ;
         TITLE 'fn:BmpSize() Demo' ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      DEFINE MAIN MENU
         DEFINE POPUP "&File"
            MENUITEM '&Open' ACTION ( ;
               Form_Main.Image_1.Picture := ;
               GetFile( { { 'Bmp Files', '*.bmp' } }, 'Open a File', GetCurrentFolder(), .F., .T. ) ;
               )
            SEPARATOR
            MENUITEM "E&xit" ACTION ThisWindow.RELEASE
         END POPUP
      END MENU

      @ 20, 20 IMAGE Image_1 ;
         PICTURE 'DEMO' ;
         ACTION Image_1_OnClick( Form_Main.Image_1.Picture ) ;
         TOOLTIP 'Click Me'

   END WINDOW

   Form_Main.Center()
   Form_Main.Activate()

RETURN


STATIC PROCEDURE Image_1_OnClick( cName )

   LOCAL aPictInfo := BmpSize( cName )
   LOCAL cMsg

   cMsg := "Picture name:" + Chr( 9 ) + cFileNoPath( cName ) + CRLF
   cMsg += "Image Width:" + Chr( 9 ) + hb_ntos( aPictInfo[ 1 ] ) + CRLF
   cMsg += "Image Height:" + Chr( 9 ) + hb_ntos( aPictInfo[ 2 ] ) + CRLF
   cMsg += "BitsPerPixel:" + Chr( 9 ) + hb_ntos( aPictInfo[ 3 ] ) + CRLF

   cMsg += "Image has Alpha:" + Chr( 9 ) + If( HasAlpha( cName ), 'TRUE', 'FALSE' )

   MsgInfo( cMsg, 'Bitmap Info' )

RETURN
