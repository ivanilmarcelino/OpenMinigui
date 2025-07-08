/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "hmg.ch"

FUNCTION MAIN

   LOCAL nW := 34, nX := 10, nS := 10

   SET OOP ON

   DEFINE WINDOW button_test ;
      AT 0, 0 ;
      WIDTH 510 ;
      HEIGHT 300 ;
      MAIN ;
      TITLE "OOP Button Test"

      @ 100, nX BUTTONEX button_1 ;
         CAPTION " 1 " ;
         ACTION _wPost( 10, This.button_1.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 1

      nX += nW + nS
      @ 100, nX BUTTONEX button_2 ;
         CAPTION " 2 " ;
         ACTION _wPost( 10, This.button_2.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 2

      nX += nW + nS
      @ 100, nX BUTTONEX button_3 ;
         CAPTION " 3 " ;
         ACTION _wPost( 10, This.button_3.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 3

      nX += nW + nS
      @ 100, nX BUTTONEX button_4 ;
         CAPTION " 4 " ;
         ACTION _wPost( 10, This.button_4.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 4

      nX += nW + nS
      @ 100, nX BUTTONEX button_5 ;
         CAPTION " 5 " ;
         ACTION _wPost( 10, This.button_5.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 5

      nX += nW + nS
      @ 100, nX BUTTONEX button_6 ;
         CAPTION " 6 " ;
         ACTION _wPost( 10, This.button_6.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 6

      nX += nW + nS
      @ 100, nX BUTTONEX button_7 ;
         CAPTION " 7 " ;
         ACTION _wPost( 10, This.button_7.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 7

      nX += nW + nS
      @ 100, nX BUTTONEX button_8 ;
         CAPTION " 8 " ;
         ACTION _wPost( 10, This.button_8.Index ) ;
         WIDTH nW ;
         HEIGHT 28 ;
         HOTKEY 8

      nX += nW + nS
      DEFINE BUTTONEX button_9
         ROW 100
         COL nX
         CAPTION " 9 "
         ACTION _wPost( 10, This.button_9.Index )
         WIDTH nW
         HEIGHT 28
         HOTKEY 9
      END BUTTONEX

      nX += nW + nS * 5

      @ 100, nX BUTTONEX button_F ;
         CAPTION " &F " ;
         ACTION _wPost( 10, This.Index ) ;
         WIDTH nW ;
         HEIGHT 28

      WITH OBJECT This.Object
         :Event( 10, {| ob | This.&( ob:Name ).SetFocus, DoEvents(), MsgInfo( This.CAPTION + ' ' + ob:Name, ThisWindow.Name ) } )
         :Event( 99, {| ow | ow:Release() } )
      END WITH

      ON KEY ESCAPE ACTION _wPost( 99 )
      ON KEY RETURN ACTION _wPost( 10, This.&( This.FocusedControl ).Index )
      ON KEY ALT + F ACTION _wPost( 10, This.button_F.Index )

   END WINDOW

   CENTER WINDOW button_test

   ACTIVATE WINDOW button_test

RETURN NIL
