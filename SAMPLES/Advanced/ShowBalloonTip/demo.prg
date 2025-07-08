/*
 * MiniGUI Show Balloon Tip Demo
*/

/*
  // constant types of icons available for the balloon tip
  // see: https://docs.microsoft.com/en-us/windows/desktop/api/commctrl/ns-commctrl-_tageditballoontip
  #define TTI_NONE              0
  #define TTI_INFO              1
  #define TTI_WARNING           2
  #define TTI_ERROR             3
  #define TTI_INFO_LARGE        4
  #define TTI_WARNING_LARGE     5
  #define TTI_ERROR_LARGE       6
*/

#include "hmg.ch"

FUNCTION Main

   SET TOOLTIPSTYLE BALLOON

   DEFINE WINDOW Form_1 ;
      WIDTH 460 HEIGHT 360 ;
      TITLE 'Harbour MiniGUI ShowBalloonTip Demo' ;
      ICON 'demo.ico' ;
      MAIN ;
      ON INIT ( Form_1.Edit_1.Value := Form_1.StatusBar.Item(2) ) ;
      FONT 'Arial' SIZE 10

      DEFINE STATUSBAR
         STATUSITEM 'HMG Power Ready!' WIDTH 100
         STATUSITEM 'ShowBalloonTip Demo' WIDTH 300
      END STATUSBAR
      
      DEFINE BUTTON Button_1
         ROW 30
         COL 10
         CAPTION 'Edit'
         ACTION ( Form_1.Edit_1.SetFocus )
      END BUTTON
      
      @ 80, 10 EDITBOX Edit_1 ;
         WIDTH 410 ;
         HEIGHT 140 ;
         VALUE '' ;
         TOOLTIP 'EditBox' ;
         MAXLENGTH 255 ;
         ON GOTFOCUS  iif( this.VALUE == 'ShowBalloonTip Demo', ;
                      ShowBalloonTip( this.HANDLE, "Entering editbox zone...", "Warning!", TTI_WARNING_LARGE ), ) ;
         ON LOSTFOCUS HideBalloonTip( this.HANDLE )

      @ 230, 10 TEXTBOX Text_1 ;
         WIDTH 410 ;
         HEIGHT 24 ;
         VALUE '' ;
         TOOLTIP 'Textbox : type some value' ;
         MAXLENGTH 255 ;
         CUEBANNER "Name" ;
         ON GOTFOCUS  iif( Empty( this.VALUE ), ShowBalloonTip( this.HANDLE, "Please, type your name here.", "Hi!", TTI_INFO ), ) ;
         ON LOSTFOCUS HideBalloonTip( this.HANDLE )

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN NIL
