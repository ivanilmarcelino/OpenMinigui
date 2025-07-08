/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2012 Janusz Pora <januszpora@onet.eu>
*/

#include "minigui.ch"

FUNCTION Main()

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'MiniGUI Check Label Demo' ;
         MAIN BACKCOLOR YELLOW

      @ 50, 50 BUTTON Btn1 ;
         CAPTION "Check Label_2" ;
         WIDTH 120 DEFAULT ;
         ACTION Form_Main.Label_2.Checked := .T.

      @ 90, 50 BUTTON Btn2 ;
         CAPTION "Uncheck Label_2" ;
         WIDTH 120 ;
         ACTION Form_Main.Label_2.Checked := .F.

      @ 200, 30 CHECKLABEL Label_1 ;
         WIDTH 200 HEIGHT 21 ;
         VALUE 'Check Label_1 standard' ;
         CHECKED ;
         FONT 'Arial' SIZE 9 BACKCOLOR YELLOW ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" ) ;
         ONCLICK Form_Main.Label_1.Checked := ! this.Checked

      DEFINE CHECKLABEL Label_2
         ROW 300
         COL 30
         WIDTH 200
         HEIGHT 18
         VALUE 'Left Check Label_2 standard'
         ONMOUSEHOVER Form_Main.Label_2.FontItalic := .T.
         ONMOUSELEAVE Form_Main.Label_2.FontItalic := .F.
         LEFTCHECK .T.
         TRANSPARENT .T.
         VCENTERALIGN .T.
      END CHECKLABEL

      @ 150, 330 CHECKLABEL Label_3 ;
         WIDTH 250 HEIGHT 32 ;
         VALUE 'Check Label with 1 Image ' ;
         FONT 'Arial' SIZE 12 ;
         CHECKED ;
         IMAGE 'thumbup.bmp' ;
         BACKCOLOR YELLOW ;
         VCENTERALIGN ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" ) ;
         ONCLICK Form_Main.Label_3.Checked := ! this.Checked

      @ 250, 330 CHECKLABEL Label_4 ;
         WIDTH 250 HEIGHT 32 ;
         VALUE 'Check Label with 2 Images' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'thumbup.bmp', 'thumbdown.bmp' } ;
         BACKCOLOR YELLOW ;
         VCENTERALIGN ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" ) ;
         ONCLICK Form_Main.Label_4.Checked := ! this.Checked

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL
