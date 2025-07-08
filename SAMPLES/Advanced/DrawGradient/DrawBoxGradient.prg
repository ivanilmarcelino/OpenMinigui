/*
 * DrawBoxGradient.prg
 *
 * Author: P.Chornyj <myorg63@mail.ru>
 *
 * This program demonstrates the DRAW GRADIENT functionality of HMG Extended.
 * It showcases different border styles, gradient directions, and color combinations.
*/

ANNOUNCE RDDSYS

#include "minigui.ch"

#define NONE      0
#define BOX       2
#define PANEL     3

PROCEDURE Main

   LOCAL aColor := HMG_n2RGB( GetSysColor( 15 ) )

   SET FONT TO "Arial", 12

   DEFINE WINDOW x ;
         WIDTH 640 ;
         HEIGHT 420 ;
         TITLE "Draw Box and Panel Gradient Sample" ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         BACKCOLOR aColor

      /*
       * This command draws a gradient-filled box within the window 'x'.
       * It starts at coordinates (20, 20) and extends to (200, 300).
       * The 'BORDER BOX' option specifies that the box should have a standard box border.
       * The default gradient direction (horizontal) and colors are used.
       */
      DRAW GRADIENT IN WINDOW x AT 20, 20 TO 200, 300 BORDER BOX

      @ 95, 110 LABEL Label_1 VALUE "Gradient Box In" AUTOSIZE TRANSPARENT FONTCOLOR YELLOW

      /*
       * This command draws a gradient-filled panel within the window 'x'.
       * It starts at coordinates (20, 320) and extends to (200, 610).
       * 'VERTICAL' specifies that the gradient should be drawn vertically.
       * 'BORDER PANEL' specifies that the panel should have a panel-style border.
       * The default gradient colors are used.
       */
      DRAW GRADIENT IN WINDOW x AT 20, 320 TO 200, 610 ;
         VERTICAL BORDER PANEL

      @ 95, 410 LABEL Label_2 VALUE "Gradient Panel" AUTOSIZE TRANSPARENT FONTCOLOR YELLOW

      /*
       * This command draws a gradient-filled box within the window 'x'.
       * It starts at coordinates (250, 20) and extends to (278, 300).
       * 'VERTICAL' specifies that the gradient should be drawn vertically.
       * 'BORDER BOX' specifies that the box should have a standard box border.
       * 'BEGINCOLOR' sets the starting color of the gradient to white ({255, 255, 255}).
       * 'ENDCOLOR' sets the ending color of the gradient to light gray ({220, 220, 220}).
       */
      DRAW GRADIENT IN WINDOW x AT 250, 20 TO 278, 300 ;
         VERTICAL BORDER BOX ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 220, 220, 220 }

      @ 220, 110 LABEL Label_3 VALUE "Gradient Box In" AUTOSIZE TRANSPARENT

      /*
       * This command draws a gradient-filled panel within the window 'x'.
       * It starts at coordinates (250, 320) and extends to (278, 610).
       * 'VERTICAL' specifies that the gradient should be drawn vertically.
       * 'BORDER PANEL' specifies that the panel should have a panel-style border.
       * 'BEGINCOLOR' sets the starting color of the gradient to white ({255, 255, 255}).
       * 'ENDCOLOR' sets the ending color of the gradient to a light bluish-gray ({200, 200, 216}).
       */
      DRAW GRADIENT IN WINDOW x AT 250, 320 TO 278, 610 ;
         VERTICAL BORDER PANEL ;
         BEGINCOLOR { 255, 255, 255 } ;
         ENDCOLOR { 200, 200, 216 }

      @ 220, 410 LABEL Label_4 VALUE "Gradient Panel" AUTOSIZE TRANSPARENT

      /*
       * This command draws a gradient-filled area within the window 'x'.
       * It starts at coordinates (300, 20) and extends to (302, 316).
       * 'BORDER NONE' specifies that no border should be drawn around the area.
       * 'BEGINCOLOR' sets the starting color of the gradient to bright red ({250, 0, 0}).
       * 'ENDCOLOR' sets the ending color of the gradient to a darker red ({130, 0, 0}).
       */
      DRAW GRADIENT IN WINDOW x AT 300, 20 TO 302, 316 ;
         BORDER NONE ;
         BEGINCOLOR { 250, 0, 0 } ;
         ENDCOLOR { 130, 0, 0 }

      /*
       * This command draws a gradient-filled area within the window 'x'.
       * It starts at coordinates (300, 316) and extends to (302, 610).
       * 'BORDER NONE' specifies that no border should be drawn around the area.
       * 'BEGINCOLOR' sets the starting color of the gradient to a darker red ({130, 0, 0}).
       * 'ENDCOLOR' sets the ending color of the gradient to bright red ({250, 0, 0}).
       * This creates a continuous gradient effect with the previous DRAW GRADIENT command.
       */
      DRAW GRADIENT IN WINDOW x AT 300, 316 TO 302, 610 ;
         BORDER NONE ;
         BEGINCOLOR { 130, 0, 0 } ;
         ENDCOLOR { 250, 0, 0 }

      @ 330, 240 BUTTON Button_1 ;
         CAPTION "&Close" ;
         ACTION ThisWindow.RELEASE ;
         WIDTH 150 HEIGHT 26

   END WINDOW

   CENTER WINDOW x

   ACTIVATE WINDOW x

RETURN
