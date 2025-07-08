/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

FUNCTION Main()

   SET StationName TO 'Roberto_Station'
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
            ITEM 'SendData' ACTION SendTest()
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

PROCEDURE SendTest

   LOCAL SetStation := 'John_Station'
   LOCAL aData := {}
   LOCAL aMultiData := {}

   AAdd ( aData, 'Juan' )
   AAdd ( aData, 'Carlos' )
   AAdd ( aData, 'Roberto' )

   AAdd ( aMultiData, { 'John', 'Smith', '555-5555' } )
   AAdd ( aMultiData, { 'Peter', 'Gomez', '543-8372' } )
   AAdd ( aMultiData, { 'Albert', 'Anderson', '854-8273' } )

   SendData ( SetStation, 100 )

   SendData ( SetStation, Date() )

   SendData ( SetStation, Time() )

   SendData ( SetStation, .T. )

   SendData ( SetStation, aData )

   SendData ( SetStation, aMultiData )

RETURN
