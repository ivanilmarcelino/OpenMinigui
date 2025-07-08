/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"
#include "i_winuser.ch"

STATIC IsRunning := .T.

*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*

   SET MULTIPLE OFF WARNING

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 320 HEIGHT 240 ;
         TITLE 'MiniGUI Wheel Demo' ;
         MAIN ;
         ON INIT CircleProgressAnimation()

      DEFINE LABEL Label_1
         ROW 20
         COL 20
         VALUE ' Animated Label '
         AUTOSIZE .T.
         FONTNAME 'Times New Roman'
         FONTSIZE 10
         FONTCOLOR { 0, 70, 213 }
      END LABEL

      DEFINE BUTTON Button_1
         ROW 18
         COL 130
         CAPTION 'Start'
         ONCLICK iif( ! IsRunning, ( IsRunning := ! IsRunning, Form_Main.Timer_1.Enabled := .T. ), )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 50
         COL 130
         CAPTION 'Stop'
         ONCLICK iif( IsRunning, ( IsRunning := ! IsRunning, Form_Main.Timer_1.Enabled := .F. ), )
      END BUTTON

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   CENTER WINDOW Form_Main

   Form_Main.Row := ( Form_Main.Row ) - 245

   ACTIVATE WINDOW Form_Main

RETURN NIL

*--------------------------------------------------------*
FUNCTION CircleProgressAnimation()
*--------------------------------------------------------*
   LOCAL r, c

   DEFINE PROGRESSWHEEL PW_1
      PARENT Form_Main
      ROW    20 + Form_Main.Label_1.HEIGHT
      COL    50
      WIDTH  30
      HEIGHT 30
      VALUE  0
      COLORDONEMAX BLACK
      COLORREMAIN { 228, 230, 228 }
      COLORINNER nRGB2Arr( GetSysColor( COLOR_BTNFACE ) )
      INNERSIZE 64
      SHOWTEXT .F.
   END PROGRESSWHEEL

   r := Form_Main.Label_1.ROW
   c := Form_Main.Label_1.COL

   DRAW RECTANGLE IN WINDOW Form_Main ;
      AT r - 2, c - 2 TO r + Form_Main.Label_1.HEIGHT + Form_Main.PW_1.HEIGHT + 4, ;
      c + Form_Main.Label_1.WIDTH + 2 ;
      PENCOLOR { 100, 100, 100 }

   DEFINE TIMER Timer_1 PARENT Form_Main INTERVAL 30 ACTION OnTimer( This.Index, This.Cargo )
   Form_Main.Timer_1.Cargo := "PW_1"

RETURN NIL

*--------------------------------------------------------*
PROCEDURE OnTimer( i, ControlName )
*--------------------------------------------------------*
   LOCAL Max := 100
   LOCAL ColorRemain, ColorDoneMax
   LOCAL FormName := GetParentFormName( i )
   STATIC Position := 0

   Position += 4
   IF Position <= Max
      PW_SetPosition( ControlName, FormName, Position )
   ELSE
      ColorRemain := GetProperty( FormName, ControlName, "ColorRemain" )
      ColorDoneMax := GetProperty( FormName, ControlName, "ColorDoneMax" )
      PW_SetColorRemain( ControlName, FormName, ColorDoneMax, .F. )
      PW_SetColorDoneMax( ControlName, FormName, ColorRemain, .F. )
      Position := 0
   ENDIF

RETURN
