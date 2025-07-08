/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * (c) 2021 Grigory Filatov <gfilatov@inbox.ru>
 */

#include <hmg.ch>

*----------------------------------------------------------------------------*
FUNCTION Main()
*----------------------------------------------------------------------------*
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

*----------------------------------------------------------------------------*
PROCEDURE Img_center( cImage )
*----------------------------------------------------------------------------*

   this.( cImage ).Hide
   this.( cImage ).row := ( thiswindow.clientheight - this.( cImage ).height ) / 2
   this.( cImage ).col := ( thiswindow.clientwidth - this.( cImage ).width ) / 2
   this.( cImage ).Show

RETURN
