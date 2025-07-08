/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * ButtonEx Icon Demo
 */

#include "hmg.ch"

PROCEDURE Main

   SET FONT TO _GetSysFont(), 10

   DEFINE WINDOW Win1 ;
         AT 0, 0 ;
         WIDTH 550 ;
         HEIGHT 450 ;
         TITLE 'ButtonEx Control Demo' ;
         MAIN

      DEFINE MAIN MENU
         DEFINE POPUP 'Test'
            MENUITEM 'Custom Method: SetFocus' ACTION Win1.Test.SetFocus
            MENUITEM 'Custom Method: Disable' ACTION Win1.Test.Enabled := .F.
            MENUITEM 'Custom Method: Enable' ACTION Win1.Test.Enabled := .T.
            MENUITEM 'Custom Property: Handle (Get)' ACTION MsgInfo ( Win1.Test.Handle )
            MENUITEM 'Custom Property: Caption (Get)' ACTION MsgInfo ( Win1.Test.Caption )
            MENUITEM 'Custom Property: Caption (Set)' ACTION Win1.Test.Caption := 'New Caption'
            MENUITEM 'Custom Property: ICON (Get)' ACTION MsgInfo ( Win1.Test.ICON )
            MENUITEM 'Custom Property: ICON (Set)' ACTION Win1.Test.ICON := 'button.ico'
         END POPUP
      END MENU

      @ 10, 10 BUTTONEX test ;
         CAPTION 'Left Align Button' ;
         ICON "hmg.ico" ;
         IMAGESIZE 110,100 ;
         WIDTH 280 ;
         HEIGHT 180 ;
         TOOLTIP "Button 1" ;
         ACTION MsgInfo( 'Click! 1' )

      @ 200, 10 BUTTONEX test2 ;
         CAPTION 'Right Align Button' ;
         ICON "hmg.ico" ;
         IMAGESIZE 100,110 ;
         WIDTH 280 ;
         HEIGHT 180 ;
         TOOLTIP "Button 2" ;
         ACTION MsgInfo( 'Click! 2' ) LEFTTEXT

      DEFINE BUTTON test3
         ROW 10
         COL 300
         CAPTION 'Top ICON Align Button'
         ICON "hmg.ico"
         WIDTH 220
         HEIGHT 180
         TOOLTIP "Button 3"
         ACTION MsgInfo( 'Click! 3' )
         PICTALIGNMENT TOP
      END BUTTON

      DEFINE BUTTON test4
         ROW 200
         COL 300
         CAPTION 'Bottom ICON Align Button'
         ICON "hmg.ico"
         WIDTH 220
         HEIGHT 180
         TOOLTIP "Button 4"
         ACTION MsgInfo( 'Click! 4' )
         PICTALIGNMENT BOTTOM
      END BUTTON

   END WINDOW

   CENTER WINDOW Win1

   ACTIVATE WINDOW Win1

RETURN
