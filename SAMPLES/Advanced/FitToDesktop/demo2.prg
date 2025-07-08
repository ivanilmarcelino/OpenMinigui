/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Christian T. Kurowski <xharbour@wp.pl>
 *
*/

#include "hmg.ch"

STATIC nCorWidth
STATIC nCorHeight

PROCEDURE Main()

   LOCAL nWidth := 800
   LOCAL nHeight := 600

   nCorWidth := GetDesktopRealWidth() / nWidth
   nCorHeight := GetDesktopRealHeight() / nHeight

   DEFINE WINDOW oMain ;
         AT ;
         GetDesktopRealTop(), ;
         GetDesktopRealLeft() ;
         WIDTH nWidth * nCorWidth ;
         HEIGHT nHeight * nCorHeight ;
         TITLE 'MiniGUI Resolution Adjustment Demo' ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE ;
         NOSYSMENU

      ON KEY F2 ;
         ACTION {|| oMain.ROW := GetDesktopRealTop(), ;
         oMain.COL := GetDesktopRealLeft(), ;
         oMain.WIDTH := nWidth * nCorWidth, ;
         oMain.HEIGHT := nHeight * nCorHeight }

      DEFINE BUTTONEX ButtonEX_Exit
         ROW 5 * nCorHeight
         COL 10 * nCorWidth
         WIDTH 90 * nCorWidth
         HEIGHT 55 * nCorHeight
         CAPTION "Exit"
         ICON NIL
         ACTION {|| ThisWindow.Release }
         FONTSIZE 10 * nCorHeight
         VERTICAL .T.
         TOOLTIP "Exit"
      END BUTTONEX

      DEFINE FRAME Frame_1
         ROW 70 * nCorHeight
         COL 10 * nCorWidth
         WIDTH 780 * nCorWidth
         HEIGHT 490 * nCorHeight
         FONTSIZE 14 * nCorHeight
         OPAQUE .T.
         CAPTION 'MiniGUI Resolution Adjustment Demo'
      END FRAME

      DEFINE LABEL Label_1
         ROW 100 * nCorHeight
         COL 20 * nCorWidth
         VALUE 'Move window and press F2'
         WIDTH 380 * nCorWidth
         HEIGHT 30 * nCorHeight
         FONTSIZE 14 * nCorHeight
      END LABEL

      DEFINE STATUSBAR FONT "ARIAL" SIZE 9 * nCorHeight
         STATUSITEM 'MiniGUI Resolution Adjustment Demo'
         DATE WIDTH 85 * nCorWidth
         CLOCK WIDTH 70 * nCorWidth
      END STATUSBAR

   END WINDOW

   ACTIVATE WINDOW oMain

RETURN
