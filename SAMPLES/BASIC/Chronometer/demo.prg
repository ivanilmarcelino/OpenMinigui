/*
 * MiniGUI Chronometer Demo
 */

#include "minigui.ch"

DECLARE WINDOW Form_2

*------------------------------------------------------------------------------*
Function Main()
*------------------------------------------------------------------------------*

   local nSecsLapsed

   DEFINE WINDOW Form_1 ;
      CLIENTAREA 400, 300 ;
      TITLE 'Chronometer Demo' ;
      ICON "clock.ico" ;
      MAIN ;
      NOMAXIMIZE NOSIZE

      @ 50, 50 BUTTON Button_1 CAPTION "Test" ;
        ACTION ( nSecsLapsed := ChronoMeter(), ;
        MsgDebug( nSecsLapsed, SECTOTIME( nSecsLapsed, .t. ) ) )

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

Return Nil

*------------------------------------------------------------------------------*
Function ChronoMeter()
*------------------------------------------------------------------------------*

   local nStartSec, nSecsLapsed := 0
   local bValue := { || SECTOTIME( nSecsLapsed, .t. ) }
   local bExec := {||
      nSecsLapsed := SECONDS() - nStartSec
      Form_2.oSay.Value := bValue
      return nil
      }
   local bInit := {||
      DEFINE TIMER oTimer OF Form_2 INTERVAL 100 ACTION If( nStartSec == nil, , Eval( bExec ) )
      return nil
      }

   DEFINE FONT oFont FONTNAME "Arial" SIZE 14 DEFAULT
   DEFINE FONT oBold FONTNAME "Arial" SIZE 30

   DEFINE WINDOW Form_2 ;
      CLIENTAREA 550, 120 ;
      NOSIZE ;
      TITLE "Chronometer Modal Form" ;
      ICON "clock.ico" ;
      MODAL ;
      ON INIT Eval( bInit )

   @ 40, 40 BUTTON oBtnStart CAPTION "START" WIDTH 100 HEIGHT 40 ;
      ACTION iif( nStartSec == nil, ;
     ( nStartSec := SECONDS(), Form_2.oBtnStop.SetFocus() ), ) FLAT

   @ 40,170 LABEL oSay VALUE bValue ;
      WIDTH 210 HEIGHT 40 FONT "oBold" CENTERALIGN VCENTERALIGN ;
      FONTCOLOR nRGB2Arr( CLR_HGREEN ) BACKCOLOR nRGB2Arr( CLR_BLACK )

   @ 40,410 BUTTON oBtnStop CAPTION "STOP"  WIDTH 100 HEIGHT 40 ;
      ACTION iif( nStartSec != nil .or. nSecsLapsed > 0, ;
      ( If( nStartSec == nil, ;
      If( nSecsLapsed > 0, nSecsLapsed := 0, nil ), ;
      ( nSecsLapsed := SECONDS() - nStartSec, nStartSec := nil ) ), ;
      Form_2.oBtnStop.Caption := If( nStartSec == nil .and. nSecsLapsed > 0, "CLEAR", "STOP" ), ;
      Form_2.oSay.Value := bValue, Form_2.oBtnStart.SetFocus() ), ) FLAT

   END WINDOW

   CENTER WINDOW Form_2
   ACTIVATE WINDOW Form_2

   RELEASE FONT oFont, oBold

Return nSecsLapsed
