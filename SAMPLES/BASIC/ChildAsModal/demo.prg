#include "hmg.ch"

PROCEDURE main

   LOCAL cActiveFormName := 'Form_2'

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'Main Window';
      MAIN

   DEFINE MAIN MENU
      POPUP 'Child Window'
         ITEM 'Open Win 2' ACTION {|| DoWindow2() }
      END POPUP
   END MENU

   DEFINE BUTTON B_OK
      ROW    20
      COL    30
      WIDTH  100
      HEIGHT 28
      ACTION Msgbox( 'OK' )
      CAPTION "OK"
   END BUTTON

   DEFINE TIMER T_1 INTERVAL 250 ACTION SetProperty( ThisWindow.Name, "Enabled", ( _IsWindowActive( cActiveFormName ) == .F. ) )

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

*****************************************************************************
PROCEDURE DoWindow2()

   LOCAL cActiveFormName := 'Form_3'
   LOCAL cOwnerFormName := ThisWindow.Name, cOwnerFormTitle := ThisWindow.Title

   DEFINE WINDOW Form_2 ;
      AT App.Row + 30, App.Col + 30 ;
      WIDTH 600 HEIGHT 400 ;
      TITLE 'Win 2' ;
      CHILD ;
      ON INIT ( SetProperty( cOwnerFormName, "Title", cOwnerFormTitle + " - Disabled" ) ) ;
      ON RELEASE ( SetProperty( cOwnerFormName, "Title", cOwnerFormTitle ) ) ;

      @ 20, 40 BUTTON Button_2 caption 'Child Win 3' WIDTH 100 HEIGHT 28 ACTION {|| DoWindow3() }

   DEFINE TIMER T_1 INTERVAL 250 ACTION SetProperty( ThisWindow.Name, "Enabled", ( _IsWindowActive( cActiveFormName ) == .F. ) )

   END WINDOW

   ACTIVATE WINDOW Form_2

RETURN

*****************************************************************************
PROCEDURE DoWindow3()

   LOCAL cOwnerFormName := ThisWindow.Name, cOwnerFormTitle := ThisWindow.Title

   DEFINE WINDOW Form_3 ;
      AT App.Row + 60, App.Col + 60 ;
      WIDTH 600 HEIGHT 400 ;
      TITLE 'Win 3' ;
      CHILD ;
      ON INIT ( SetProperty( cOwnerFormName, "Title", cOwnerFormTitle + " - Disabled" ) ) ;
      ON RELEASE ( SetProperty( cOwnerFormName, "Enabled", .T. ), SetProperty( cOwnerFormName, "Title", cOwnerFormTitle ), ;
         DoMethod( cOwnerFormName, "setFocus" ) )

   @ 50, 100 BUTTON Button_2 caption 'OK' WIDTH 100 HEIGHT 28 ACTION {|| MsgBox( 'OK' ) }

   END WINDOW

   ACTIVATE WINDOW Form_3

RETURN
