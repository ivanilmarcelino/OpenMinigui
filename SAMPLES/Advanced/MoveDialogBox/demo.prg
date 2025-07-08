/*
 * HMG Demo: Move Dialog Box
 * (c) 2014, by Dr. Claudio Soto <srvet@adinet.com.uy> , http://srvet.blogspot.com
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 *
 * Revised by Petr Chornyj <myorg63@mail.ru>
*/

#include "hmg.ch"

FUNCTION MAIN

   DEFINE WINDOW Form_1 ;
      AT 100,100 ;
      WIDTH 700 HEIGHT 500 ;
      TITLE 'Move Dialog Box' ;
      MAIN

      @  50,350 BUTTON Button_1 CAPTION "Dlg Move 1"  ACTION ( SET DIALOGBOX CENTER OF This.Handle,; 
                                                               MsgInfo ("Hello", "Dlg Move 1") )

      @ 125,100 BUTTON Button_2 CAPTION "Dlg Move 2"  ACTION ( SET DIALOGBOX CENTER OF PARENT,; 
                                                               MsgInfo ("Hello", "Dlg Move 2") )

      @ 200,100 BUTTON Button_3 CAPTION "Dlg Move 3"  ACTION ( SET DIALOGBOX ROW 50 COL 30,; 
                                                               MsgInfo ("Hello", "Dlg Move 3") )

      @ 275,100 BUTTON Button_4 CAPTION "Dlg Move 4"  ACTION ( SET DIALOGBOX CENTER OF ThisWindow.Handle,; 
                                                               GetFolder () )

      @ 350,100 BUTTON Button_5 CAPTION "Dlg No Move" ACTION ( SET DIALOGBOX POSITION DISABLE,;
                                                               MsgInfo( GetClassName( Form_1.Handle ) ) )

   END WINDOW

   ACTIVATE WINDOW Form_1

RETURN NIL
