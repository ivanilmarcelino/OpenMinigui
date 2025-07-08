/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 26/Apr/2022
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
*/

#include "hmg.ch"

#define aStdColor     { 240, 240, 240 }
#define aActiveColor  { 020, 210, 040 }

FUNCTION Main()

   SET PROCEED EACH RADIOBUTTON EVENT OFF

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'HMG RadioGroup Demo' ;
         MAIN ;
         FONT 'Arial' SIZE 10 ;
         BACKCOLOR aStdColor

      DEFINE RADIOGROUP Radio_1
         OPTIONS { 'One', 'Two', 'Three', 'Four' }
         VALUE 1
         WIDTH 100
         TOOLTIP 'RadioGroup'
         ONCHANGE MsgInfo( "On Change event Radio 1" )
         READONLY { .F., .T., .F., .T. }
         ROW 10
         COL 10
         BACKCOLOR aStdColor
         ON GOTFOCUS Form_1.Radio_1.BACKCOLOR := aActiveColor
         ON LOSTFOCUS Form_1.Radio_1.BACKCOLOR := aStdColor
      END RADIOGROUP

      DEFINE RADIOGROUP Radio_2
         ROW 10
         COL 150
         OPTIONS { 'A', 'B', 'C', 'D' }
         VALUE 1
         WIDTH 100
         TOOLTIP 'RadioGroup'
         READONLY { .F., .F., .F., .F. }
         BACKCOLOR aStdColor
      END RADIOGROUP

      Form_1.Radio_2.OnGotFocus := {|| Form_1.Radio_2.BACKCOLOR := aActiveColor }
      Form_1.Radio_2.OnLostFocus := {|| Form_1.Radio_2.BACKCOLOR := aStdColor }
      Form_1.Radio_2.OnChange := {|| MsgInfo( "On Change event Radio 2" ) }

      @ 150, 10 TEXTBOX Text_1 ;
         VALUE "Some text" ;
         BACKCOLOR aStdColor ;
         ON GOTFOCUS This.BACKCOLOR := aActiveColor ;
         ON LOSTFOCUS This.BACKCOLOR := aStdColor

   END WINDOW

   Form_1.Text_1.SetFocus()

   Form_1.Center()

   Form_1.Activate()

RETURN NIL
