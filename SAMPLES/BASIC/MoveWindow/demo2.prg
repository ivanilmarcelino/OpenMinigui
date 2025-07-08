/*
 * MiniGUI Demo
 */

#include "minigui.ch"

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 450 HEIGHT 400 ;
         TITLE 'Main Window' ;
         MAIN ;
         ON MOUSECLICK MoveActiveWindow( This.Handle ) ;
         ON INIT CreaChild() ;
         ON MOVE MoveTest() ;
         ON SIZE MoveTest() ;
         NOMAXIMIZE NOMINIMIZE

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

      DEFINE STATUSBAR
         STATUSITEM "Click on Form and holding mouse's button for moving this window" FONTCOLOR BLACK CENTERALIGN
      END STATUSBAR

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION CreaChild()

   LOCAL xPos := Form_1.COL
   LOCAL yPos := Form_1.ROW
   LOCAL nWidth := Form_1.WIDTH

   DEFINE WINDOW Form_2 ;
         AT yPos, xPos + nWidth ;
         WIDTH 350 HEIGHT 200 ;
         TITLE 'Child Window' ;
         CHILD NOSYSMENU ;
         ON MOUSECLICK Form_2.HEIGHT := ( Form_1.Height )

      DEFINE STATUSBAR
         STATUSITEM "This window CHILD will be 'anchored' to the MAIN" FONTCOLOR BLACK CENTERALIGN
      END STATUSBAR

      DEFINE TIMER t_1 INTERVAL 250 ACTION Form_1.SetFocus ONCE

   END WINDOW

   ACTIVATE WINDOW Form_2

RETURN NIL


#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

FUNCTION MoveActiveWindow( hWnd )

   PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )

RETURN NIL


FUNCTION MoveTest()

   LOCAL xPos := hb_ntos( _HMG_MouseRow - GetTitleHeight() - GetBorderWidth() )
   LOCAL yPos := hb_ntos( _HMG_MouseCol - GetBorderWidth() )
   LOCAL nWidth := Form_1.WIDTH

   Form_1.StatusBar.Item( 1 ) := "Row: " + yPos + " / Col: " + xPos + " | " + hb_ntos( nWidth )

   ChgChild()

RETURN NIL


FUNCTION ChgChild()

   LOCAL xPos := Form_1.COL
   LOCAL yPos := Form_1.ROW
   LOCAL nWidth := Form_1.WIDTH

   Form_2.ROW := yPos
   Form_2.COL := xPos + nWidth

RETURN NIL
