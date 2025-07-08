/*
 * HMG Embedded Child Window Demo
 * (c) 2002-2010 Roberto Lopez
*/

#include "minigui.ch"

FUNCTION Main

   DEFINE WINDOW Win_0 ;
         ROW 0 ;
         COL 0 ;
         WIDTH 400 ;
         HEIGHT 400 ;
         TITLE 'Panel Window Demo 3' ;
         WINDOWTYPE MAIN

      DEFINE BUTTON Button_1
         ROW 160
         COL 90
         WIDTH 200
         CAPTION 'Click Me!'
         ACTION Test()
      END BUTTON

   END WINDOW

   CENTER WINDOW Win_0

   ACTIVATE WINDOW Win_0

RETURN NIL

PROCEDURE Test

   DEFINE WINDOW Win_1 ;
         ROW 0 ;
         COL 0 ;
         WIDTH 500 ;
         HEIGHT 300 ;
         TITLE 'Panel in Modal Window' ;
         WINDOWTYPE MODAL

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

      DEFINE WINDOW Win_2 ;
            ROW 30 ;
            COL 30 ;
            WIDTH 300 ;
            HEIGHT 200 ;
            VIRTUAL WIDTH 400 ;
            VIRTUAL HEIGHT 400 ;
            WINDOWTYPE PANEL

         DEFINE LABEL Label_1
            ROW 10
            COL 10
            VALUE 'Panel window...'
            WIDTH 300
         END LABEL

         DEFINE BUTTON Button_1
            ROW 40
            COL 10
            CAPTION 'Click Me!'
            ACTION MsgInfo( 'Clicked!' )
         END BUTTON

         DEFINE LABEL Label_2
            ROW 90
            COL 10
            VALUE "Can do this!"
            WIDTH 300
         END LABEL

         DEFINE TEXTBOX Text_1
            ROW 120
            COL 10
            VALUE 'Test'
         END TEXTBOX

         DEFINE TEXTBOX Text_2
            ROW 150
            COL 10
            VALUE 'Test'
         END TEXTBOX

         DEFINE TEXTBOX Text_3
            ROW 180
            COL 10
            VALUE 'Test'
         END TEXTBOX

         DEFINE TEXTBOX Text_4
            ROW 210
            COL 10
            VALUE 'Test'
         END TEXTBOX

         DEFINE TEXTBOX Text_5
            ROW 240
            COL 10
            VALUE 'Test'
         END TEXTBOX

      END WINDOW

      DEFINE TEXTBOX Text_1
         ROW 300
         COL 10
         VALUE 'Test'
      END TEXTBOX

   END WINDOW

   CENTER WINDOW Win_1

   // Panel windows are automatically activated through its parent
   // so, only Win_1 must be activated.

   ACTIVATE WINDOW Win_1

RETURN
