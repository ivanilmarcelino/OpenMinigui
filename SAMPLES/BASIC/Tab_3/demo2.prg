/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"

FUNCTION Main()

   DEFINE WINDOW tabsample AT 0, 0 WIDTH 400 HEIGHT 300 TITLE 'Add control test' MAIN

      DEFINE TAB tab1 AT 10, 10 WIDTH 370 HEIGHT 200

         DEFINE PAGE 'Page1'

            DEFINE BUTTON b1
               ROW 30
               COL 10
               CAPTION 'Press here to add a control'
               WIDTH 180
               ACTION addnewcontrols( { 'lbl1', 'text1' } )
            END BUTTON

         END PAGE

         DEFINE PAGE 'Page2'

            DEFINE BUTTON b2
               ROW 30
               COL 10
               CAPTION 'Press here to add a control'
               WIDTH 180
               ACTION addnewcontrol2( 'btn1' )
            END BUTTON

         END PAGE

      END TAB

      ON KEY ESCAPE ACTION thiswindow.release()

   END WINDOW

   tabsample.center()
   tabsample.activate()

RETURN NIL


FUNCTION addnewcontrols( actrl )

   LOCAL c1, c2

   c1 := actrl[ 1 ]
   c2 := actrl[ 2 ]

   IF iscontroldefined( &c1, tabsample )
      tabsample.&( c1 ).release()
   ENDIF

   DEFINE LABEL &c1
      parent tabsample
      ROW 50
      COL 10
      WIDTH 40
      VALUE 'label'
   END LABEL

   IF iscontroldefined( &c2, tabsample )
      tabsample.&( c2 ).RELEASE
   ENDIF

   DEFINE TEXTBOX &c2
      parent tabsample
      ROW 50
      COL 50
      WIDTH 100
   END TEXTBOX

   tabsample.tab1.addcontrol( c1, 1, 84, 10 )
   tabsample.tab1.addcontrol( c2, 1, 80, 50 )

RETURN NIL


FUNCTION addnewcontrol2( ctrl )

   IF iscontroldefined( &ctrl, tabsample )
      tabsample.&( ctrl ).release()
   ENDIF

   DEFINE BUTTONEX &ctrl
      parent tabsample
      ROW 10
      COL 10
      WIDTH 180
      CAPTION 'Click me'
      ACTION MsgBox( 'Button action', 'Result' )
   END BUTTONEX

   tabsample.tab1.addcontrol( ctrl, 2, 80, 10 )

RETURN NIL
