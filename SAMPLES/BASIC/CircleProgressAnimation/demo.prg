/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"

STATIC oGif

*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*
   LOCAL cFile := "telegraaf.gif"

   SET MULTIPLE OFF WARNING

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 320 HEIGHT 240 ;
         TITLE 'MiniGUI Label Demo' ;
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
         ONCLICK iif( ! oGif:IsRunning(), oGif:Play(), )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 50
         COL 130
         CAPTION 'Stop'
         ONCLICK iif( oGif:IsRunning(), oGif:Stop(), )
      END BUTTON

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL

*--------------------------------------------------------*
FUNCTION CircleProgressAnimation( cGIFfile )
*--------------------------------------------------------*
   LOCAL aPictInfo, r, c

   aPictInfo := hb_GetImageSize( cGIFfile )

   @ 20 + Form_Main.Label_1.HEIGHT, 50 ANIGIF Gif_1 ;
      OBJ oGif ;
      PARENT Form_Main ;
      PICTURE cGIFfile ;
      WIDTH aPictInfo[ 1 ] ;
      HEIGHT aPictInfo[ 2 ]

   r := Form_Main.Label_1.ROW
   c := Form_Main.Label_1.COL

   DRAW RECTANGLE IN WINDOW Form_Main ;
      AT r - 2, c - 2 TO r + Form_Main.Label_1.HEIGHT + Form_Main.Gif_1.HEIGHT + 2, ;
      c + Form_Main.Label_1.WIDTH + 2 ;
      PENCOLOR { 100, 100, 100 }

RETURN NIL
