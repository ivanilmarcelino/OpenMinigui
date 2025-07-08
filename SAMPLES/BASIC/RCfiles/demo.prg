/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */


#include "minigui.ch" 

PROCEDURE Main()

   DEFINE WINDOW Form_Main ;
      CLIENTAREA 364, 243 ;
      TITLE 'Test for loading images from RC file' ;
      ICON "APPICON" ;
      MAIN

      @ 1, 1 IMAGE Image_1 ;
         PICTURE "DEMO1" ;
         ADJUSTIMAGE ;
         TOOLTIP "BITMAP"

      @ 1, 122 IMAGE Image_2 ;
         PICTURE "DEMO2" ;
         ADJUSTIMAGE ;
         TOOLTIP "GIF"

      @ 1, 243 IMAGE Image_3 ;
         PICTURE "DEMO3" ;
         ADJUSTIMAGE ;
         TOOLTIP "JPG"

      @ 121, 1 IMAGE Image_4 ;
         PICTURE "DEMO4" ;
         ADJUSTIMAGE ;
         TOOLTIP "PNG"

      @ 121, 122 IMAGE Image_5 ;
         PICTURE "DEMO5" ;
         ADJUSTIMAGE ;
         TOOLTIP "TIF"

      @ 121, 243 IMAGE Image_6 ;
         PICTURE "DEMO6" ;
         ADJUSTIMAGE ;
         TOOLTIP "ICO"

      ON KEY ESCAPE ACTION ThisWindow.Release
   END WINDOW

   CENTER WINDOW Form_Main
   ACTIVATE WINDOW Form_Main

RETURN
