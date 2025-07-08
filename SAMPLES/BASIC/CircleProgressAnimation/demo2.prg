/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"

STATIC IsRunning := .T.

*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*
   LOCAL cFile := "circle.avi"

   SET MULTIPLE OFF WARNING

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 320 HEIGHT 240 ;
         TITLE 'MiniGUI Animate Demo' ;
         MAIN ;
         ON INIT CircleProgressAnimation( cFile )

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
         ONCLICK iif( ! IsRunning, ( IsRunning := ! IsRunning, Form_Main.Avi_1.Open( cFile ) ), )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 50
         COL 130
         CAPTION 'Stop'
         ONCLICK iif( IsRunning, ( IsRunning := ! IsRunning, Form_Main.Avi_1.Close() ), )
      END BUTTON

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   CENTER WINDOW Form_Main

   Form_Main.ROW := ( Form_Main.Row ) + 245

   ACTIVATE WINDOW Form_Main

RETURN NIL

*--------------------------------------------------------*
FUNCTION CircleProgressAnimation( cAvifile )
*--------------------------------------------------------*
   LOCAL aPictInfo, r, c

   aPictInfo := GetAviFileSize( cAvifile )

   @ 20 + Form_Main.Label_1.HEIGHT, 50 ANIMATEBOX Avi_1 ;
      PARENT Form_Main ;
      WIDTH aPictInfo[ 1 ] HEIGHT aPictInfo[ 2 ] ;
      FILE cAvifile AUTOPLAY TRANSPARENT NOBORDER

   r := Form_Main.Label_1.ROW
   c := Form_Main.Label_1.COL

   DRAW RECTANGLE IN WINDOW Form_Main ;
      AT r - 2, c - 2 TO r + Form_Main.Label_1.HEIGHT + Form_Main.Avi_1.HEIGHT + 2, ;
      c + Form_Main.Label_1.WIDTH + 2 ;
      PENCOLOR { 100, 100, 100 }

RETURN NIL
