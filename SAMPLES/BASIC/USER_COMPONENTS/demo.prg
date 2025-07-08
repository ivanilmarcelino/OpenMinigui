/*
 * MiniGUI User Components Demo
 * (c) 2006 Roberto Lopez
*/

#include "minigui.ch"

PROCEDURE Main

   SET FONT TO _GetSysFont(), 10

   DEFINE WINDOW Win1 ;
         AT 0, 0 ;
         WIDTH 547 ;
         HEIGHT 450 ;
         TITLE 'Custom Component Demo' ;
         MAIN

      DEFINE MAIN MENU
         DEFINE POPUP 'Test'
            MENUITEM 'Custom Method: SetFocus' ACTION Win1.Test.SetFocus
            MENUITEM 'Custom Method: Disable' ACTION Win1.Test.Disable
            MENUITEM 'Custom Method: Enable' ACTION Win1.Test.Enable
            MENUITEM 'Custom Property: Handle (Get)' ACTION MsgInfo ( Win1.Test.Handle )
            MENUITEM 'Custom Property: Handle (Set)' ACTION Win1.Test.Handle := 1
            MENUITEM 'Custom Property: Caption (Get)' ACTION MsgInfo ( Win1.Test.Caption )
            MENUITEM 'Custom Property: Caption (Set)' ACTION Win1.Test.Caption := 'New Caption'
            MENUITEM 'Custom Property: Picture (Get)' ACTION MsgInfo ( Win1.Test.Picture )
            MENUITEM 'Custom Property: Picture (Set)' ACTION Win1.Test.Picture := 'button.png'
         END POPUP
      END MENU

      @ 10, 10 HMGBUTTON test ;
         CAPTION 'Left Align Button' ;
         PICTURE "hmg.png" ;
         WIDTH 280 ;
         HEIGHT 180 ;
         TOOLTIP "Button 1" ;
         ACTION MsgInfo( 'Click! 1' ) LEFT

      @ 200, 10 HMGBUTTON test2 ;
         CAPTION 'Right Align Button' ;
         PICTURE "hmg.png" ;
         WIDTH 280 ;
         HEIGHT 180 ;
         TOOLTIP "Button 2" ;
         ACTION MsgInfo( 'Click! 2' ) RIGHT

      @ 10, 300 HMGBUTTON test3 ;
         CAPTION 'Top Picture Align Button' ;
         PICTURE "hmg.png" ;
         WIDTH 220 ;
         HEIGHT 180 ;
         TOOLTIP "Button 3" ;
         ACTION MsgInfo( 'Click! 3' ) TOP
/*
      @ 200, 300 HMGBUTTON test4 ;
         CAPTION 'Bottom Picture Align Button' ;
         PICTURE "hmg.png" ;
         WIDTH 220 ;
         HEIGHT 180 ;
         TOOLTIP "Button 4" ;
         ACTION MsgInfo( 'Click! 4' ) BOTTOM
*/
      DEFINE BUTTON test4
         ROW 200
         COL 300
         CAPTION 'Bottom Picture Align Button'
         PICTURE "hmg.png"
         WIDTH 220
         HEIGHT 180
         TOOLTIP "Button 4"
         ACTION MsgInfo( 'Click! 4' )
         PICTALIGNMENT BOTTOM
      END HMGBUTTON

   END WINDOW

   Win1.Test.SetFocus

   CENTER WINDOW Win1

   ACTIVATE WINDOW Win1

RETURN

#include "HMGButton.prg"
