/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

FUNCTION Main()

   SET StationName TO 'John_Station'
   SET CommPath TO GetTempFolder() + '\'

   DEFINE WINDOW Form_0 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'MiniGUI Communications Demo' ;
         ICON 'demo.ico' ;
         MAIN ;
         FONT 'Arial' SIZE 10

      DEFINE MAIN MENU
         POPUP 'File'
            ITEM 'GetData' ACTION GetTest()
            ITEM 'Exit' ACTION Form_0.Release()
         END POPUP
         POPUP 'Help'
            ITEM 'About' ACTION MsgInfo ( "MiniGUI Communications Demo" )
         END POPUP
      END MENU

   END WINDOW

   CENTER WINDOW Form_0

   ACTIVATE WINDOW Form_0

RETURN NIL

PROCEDURE GetTest()

   LOCAL r
   LOCAL i

   REPEAT

      r := GetData()

      IF ValType ( r ) == 'A'

         IF ValType ( r[ 1 ] ) != 'A'

            AEval ( r, {| i | MsgInfo( i ) } )

         ELSE

            FOR EACH i IN r

               AEval ( i, {| j | MsgInfo( j ) } )

            NEXT

         ENDIF

      ELSEIF r # NIL

         MsgInfo( r )

      ENDIF

   UNTIL r # NIL

RETURN
