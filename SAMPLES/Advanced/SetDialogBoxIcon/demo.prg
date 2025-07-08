/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"

FUNCTION Main()

   SET DEFAULT ICON TO "DEFAULTICON"

   SET DIALOGBOX POSITION CENTER OF PARENT

   SET CENTERWINDOW RELATIVE PARENT

   DEFINE WINDOW Form_1 ;
         WIDTH 640 ;
         HEIGHT 480 ;
         TITLE "Set Default Icon To Dialog Box" ;
         MAIN

      @ 50, 100 BUTTON Button_1 ;
         CAPTION "Click Me" ;
         ACTION MsgInfo( "Ok, thanks!", "Button pressed" )

      @ 50, 250 BUTTON Button_2 ;
         CAPTION "Click Me too" ;
         ACTION MsgStop( "Ok, thanks!", "Button pressed" )

      @ 90, 100 BUTTON Button_3 ;
         CAPTION "Alert Info" ;
         ACTION AlertInfo( "Ok, thanks!", "Button pressed" )

      @ 90, 250 BUTTON Button_4 ;
         CAPTION "Alert Stop" ;
         ACTION AlertStop( "Ok, thanks!", "Button pressed" )

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL
