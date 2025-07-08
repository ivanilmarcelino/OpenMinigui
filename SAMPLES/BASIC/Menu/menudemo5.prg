/*
* MiniGUI Menu Demo
*/

#include "hmg.ch"


Procedure Main

   DEFINE WINDOW Win_1 ;
      AT 0,0 ;
      WIDTH 400 ;
      HEIGHT 400 ;
      TITLE 'Dynamic Context Menu Demo ' ;
      MAIN

      DEFINE MAIN MENU
         POPUP "&File"

            MENUITEM  'Exit'  ACTION ThisWindow.Release()

         END POPUP

         POPUP "&Options"

            MENUITEM  'Dynamic Context Menu at Cursor '  ACTION DynamicContextMenu(1)
            MENUITEM  'Dynamic Context Menu at Position '  ACTION DynamicContextMenu(2)

         END POPUP
      END MENU

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

Return


Procedure MenuProc()

   If This.Name == '01'
      MsgInfo ('Action 01')
   ElseIf This.Name == '02'
      MsgInfo ('Action 02')
   ElseIf This.Name == '03'
      MsgInfo ('Action 03')
   EndIf

RETURN


FUNCTION DynamicContextMenu(typ)
   LOCAL N
   LOCAL m_char

   DEFINE CONTEXT MENU OF Win_1

      FOR N = 1 TO 3
         m_char := strzero(n,2)
         MENUITEM  'Context Item ' + m_char ACTION MenuProc() NAME &m_char
      NEXT

   END MENU

   DO CASE
      CASE typ == 1
         SHOW CONTEXTMENU PARENT Win_1

      CASE typ == 2
         SetCursorPos( GetDesktopWidth()/2, GetDesktopHeight()/2 - 20 )
         SHOW CONTEXTMENU OF Win_1 AT GetDesktopHeight()/2 - 50, GetDesktopWidth()/2 - 80
   ENDCASE

   RELEASE CONTEXT MENU OF Win_1

RETURN Nil
