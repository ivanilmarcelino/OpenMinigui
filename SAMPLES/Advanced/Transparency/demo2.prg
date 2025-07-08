/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

FUNCTION Main()

   LOCAL nTra := 128

   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 300 ;
         HEIGHT 300 ;
         TITLE 'Transparent window' ;
         MAIN ;
         NOSIZE NOMAXIMIZE ;
         ON INIT ( ThisWindow.AlphaBlendTransparent := nTra )

      @ 200, 100 BUTTON But1 ;
         CAPTION "Click Me" ;
         HEIGHT 35 WIDTH 100 ;
         ACTION ( nTra := iif( nTra == 128, 255, 128 ), Win_1.AlphaBlendTransparent := nTra )

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

RETURN NIL
