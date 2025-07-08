/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Mouse & Image Test
 * (C) 2021 Krzysztof Stankiewicz
*/

#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 300, 300 ;
         MAIN ;
         TITLE "Mouse & Image Test 5" ;
         ON MOUSECLICK ReForm() ;
         ON MOUSEDRAG ReForm() ;
         ON INIT ReForm()

      ON KEY ESCAPE ACTION Form_1.RELEASE

   END WINDOW

   DEFINE IMAGELIST IL1 ;
      OF Form_1 ;
      BUTTONSIZE 1, 300 ;
      IMAGE { '002.jpg' } ;
      IMAGECOUNT 300

   DEFINE IMAGELIST IL2 ;
      OF Form_1 ;
      BUTTONSIZE 1, 300 ;
      IMAGE { '001.jpg' } ;
      IMAGECOUNT 300

   CENTER WINDOW Form_1
   ACTIVATE WINDOW form_1

RETURN

//----------------------------------------------------------------------------//

PROCEDURE ReForm()

   LOCAL nx := GetCursorCol() - Form_1.COL - GetBorderWidth()
   LOCAL i

   IF nx <= 0
      nx := 150
   ENDIF
   FOR i := nx TO 300
      DRAW IMAGELIST Il2 OF Form_1 AT 1, i IMAGEINDEX ( i )
   NEXT

   FOR i := 1 TO nx
      DRAW IMAGELIST Il1 OF Form_1 AT 1, i IMAGEINDEX ( i )
   NEXT

RETURN
