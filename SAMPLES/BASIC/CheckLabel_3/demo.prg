/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

#define DGREEN		{ 0 , 120 , 0 }
#define DGRAY		{ 95 , 95 , 95 }

FUNCTION Main

   LOCAL lChecked

   DEFINE WINDOW Form_Main ;
      AT 0,0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'MiniGUI Check Label Demo' ;
      MAIN

      @ 10,50 BUTTON Btn1 ;
         CAPTION "Disable Label_1" ;
         WIDTH 120 ;
         ACTION SetProperty( 'Form_Main', 'Label_1', "Enabled", .F. )

      @ 40,50 BUTTON Btn2 ;
         CAPTION "Enable Label_1" ;
         WIDTH 120 ;
         ACTION SetProperty( 'Form_Main', 'Label_1', "Enabled", .T. )

      @ 80,50 BUTTON Btn3 ;
         CAPTION "Check Label_1" ;
         WIDTH 120 ;
         ACTION Form_Main.Label_1.Checked := .T.

      @ 110,50 BUTTON Btn4 ;
         CAPTION "Uncheck Label_1" ;
         WIDTH 120 ;
         ACTION Form_Main.Label_1.Checked := .F.

      @ 150,30 CHECKLABEL Label_1 ;
         HEIGHT 46 ;
         VALUE 'On / Off switcher' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ 190,30 SWITCHER Label_2 ;
         HEIGHT 46 ;
         VALUE 'On' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         ON MOUSEHOVER ( Form_Main.Label_2.FontColor := BLUE ) ;
         ON MOUSELEAVE ( Form_Main.Label_2.FontColor := BLACK ) ;
         ONCLICK ( lChecked := Form_Main.Label_2.Checked, Form_Main.Label_2.Value := iif(lChecked, 'Off', 'On' ), ;
            Form_Main.Label_2.Checked := !lChecked )

      @ 150,330 CHECKLABEL Label_3 ;
         HEIGHT 36 ;
         VALUE 'On / Off switcher' ;
         FONT 'Arial' SIZE 10 ;
         IMAGE { 'switch_on.bmp', 'switch_off.bmp' } ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ 190,330 CHECKLABEL Label_4 ;
         HEIGHT 36 ;
         VALUE 'On' ;
         FONT 'Arial' SIZE 10 ;
         IMAGE { 'switch_on.bmp', 'switch_off.bmp' } ;
         AUTOSIZE ;
         CHECKED ;
         FONTCOLOR DGREEN ;
         ONCLICK ( lChecked := Form_Main.Label_4.Checked, Form_Main.Label_4.Value := iif(lChecked, 'Off', 'On' ), ;
            Form_Main.Label_4.FontColor := iif( lChecked, DGRAY, DGREEN ), ;
            Form_Main.Label_4.Checked := !lChecked )

      @ 290,30 SWITCHER Label_5 ;
         HEIGHT 44 ;
         VALUE ' On / Off switcher' ;
         FONT 'Arial' SIZE 11 ;
         IMAGE { 'switch_orange.bmp', 'switch_gray.bmp' } ;
         LEFTCHECK ;
         FONTCOLOR DGRAY ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" ) ;
         ONCLICK ( lChecked := Form_Main.Label_5.Checked, ;
            Form_Main.Label_5.FontColor := iif( lChecked, DGRAY, ORANGE ), ;
            Form_Main.Label_5.Checked := !lChecked )

      @ 330,30 SWITCHER Label_6 ;
         HEIGHT 44 ;
         VALUE ' On' ;
         FONT 'Arial' SIZE 11 ;
         IMAGE { 'switch_orange.bmp', 'switch_gray.bmp' } ;
         LEFTCHECK ;
         CHECKED ;
         FONTCOLOR ORANGE ;
         ONCLICK ( lChecked := Form_Main.Label_6.Checked, Form_Main.Label_6.Value := iif(lChecked, ' Off', ' On' ), ;
            Form_Main.Label_6.FontColor := iif( lChecked, DGRAY, ORANGE ), Form_Main.Label_6.Checked := !lChecked )

      @ 260,330 CHECKLABEL Label_7 ;
         WIDTH 160 HEIGHT 44 ;
         VALUE 'On / Off switcher' ;
         FONT 'Arial' SIZE 11 ;
         IMAGE { 'vswitch_orange.bmp', 'vswitch_gray.bmp' } ;
         VCENTERALIGN ;
         FONTCOLOR DGRAY ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" ) ;
         ONCLICK ( lChecked := Form_Main.Label_7.Checked, ;
            Form_Main.Label_7.FontColor := iif( lChecked, DGRAY, ORANGE ), ;
            Form_Main.Label_7.Checked := !lChecked )

      @ 330,330 CHECKLABEL Label_8 ;
         WIDTH 80 HEIGHT 44 ;
         VALUE 'On' ;
         FONT 'Arial' SIZE 11 ;
         IMAGE { 'vswitch_orange.bmp', 'vswitch_gray.bmp' } ;
         VCENTERALIGN ;
         LEFTCHECK ;
         CHECKED ;
         FONTCOLOR ORANGE ;
         ONCLICK ( lChecked := Form_Main.Label_8.Checked, Form_Main.Label_8.Value := iif(lChecked, 'Off', 'On' ), ;
            Form_Main.Label_8.FontColor := iif( lChecked, DGRAY, ORANGE ), Form_Main.Label_8.Checked := !lChecked )

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL
