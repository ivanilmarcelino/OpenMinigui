/*
 * DrawBoxGradient.prg
 *
 * Author: P.Chornyj <myorg63@mail.ru>
*/

ANNOUNCE RDDSYS

#include "minigui.ch"

#define NONE      0
#define BOX       2
#define PANEL     3

PROCEDURE Main ()

   LOCAL aColor := HMG_n2RGB( GetSysColor( 15 ) )

   SET FONT TO "Arial", 12

   DEFINE WINDOW x ;
         WIDTH 640 ;
         HEIGHT 400 ;
         TITLE "Draw Box and Panel Gradient Sample" ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         BACKCOLOR aColor ;
         ON INIT SwitchBoxPanel()

      @ 95, 110 LABEL Label_1 VALUE "" AUTOSIZE TRANSPARENT FONTCOLOR YELLOW

      @ 95, 410 LABEL Label_2 VALUE "" AUTOSIZE TRANSPARENT FONTCOLOR YELLOW

      @ 255, 110 LABEL Label_3 VALUE "" AUTOSIZE TRANSPARENT

      @ 255, 410 LABEL Label_4 VALUE "" AUTOSIZE TRANSPARENT

      @ 330, 240 BUTTON Button_1 ;
         CAPTION "&Switch" ;
         ACTION SwitchBoxPanel() ;
         WIDTH 150 HEIGHT 26

   END WINDOW

   CENTER WINDOW x

   ACTIVATE WINDOW x

RETURN

PROCEDURE SwitchBoxPanel()
   STATIC lPanelFirst := .F.

   ERASE WINDOW x

   IF lPanelFirst
      // Panel first, Box second
      DRAW GRADIENT IN WINDOW x AT 20, 20 TO 200, 300 VERTICAL BORDER PANEL
      x.Label_1.VALUE := "Gradient Panel"

      DRAW GRADIENT IN WINDOW x AT 20, 320 TO 200, 610 BORDER BOX
      x.Label_2.VALUE := "Gradient Box In"

      DRAW GRADIENT IN WINDOW x AT 250, 20 TO 278, 300 VERTICAL BORDER PANEL ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 200, 200, 216 }
      x.Label_3.VALUE := "Gradient Panel"

      DRAW GRADIENT IN WINDOW x AT 250, 320 TO 278, 610 VERTICAL BORDER BOX ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 220, 220, 220 }
      x.Label_4.VALUE := "Gradient Box In"
   ELSE
      // Box first, Panel second (original order)
      DRAW GRADIENT IN WINDOW x AT 20, 20 TO 200, 300 BORDER BOX
      x.Label_1.VALUE := "Gradient Box In"

      DRAW GRADIENT IN WINDOW x AT 20, 320 TO 200, 610 VERTICAL BORDER PANEL
      x.Label_2.VALUE := "Gradient Panel"

      DRAW GRADIENT IN WINDOW x AT 250, 20 TO 278, 300 VERTICAL BORDER BOX ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 220, 220, 220 }
      x.Label_3.VALUE := "Gradient Box In"

      DRAW GRADIENT IN WINDOW x AT 250, 320 TO 278, 610 VERTICAL BORDER PANEL ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 200, 200, 216 }
      x.Label_4.VALUE := "Gradient Panel"
   ENDIF

   // Always paint the vertical gradients on the right
   DRAW GRADIENT IN WINDOW x AT 300, 20 TO 302, 316 BORDER NONE ;
      BEGINCOLOR { 250, 0, 0 } ;
      ENDCOLOR { 130, 0, 0 }

   DRAW GRADIENT IN WINDOW x AT 300, 316 TO 302, 610 BORDER NONE ;
      BEGINCOLOR { 130, 0, 0 } ;
      ENDCOLOR { 250, 0, 0 }

   InvalidateRect( x.HANDLE, 0 )

   lPanelFirst := !lPanelFirst  // Toggle state for next click
RETURN
