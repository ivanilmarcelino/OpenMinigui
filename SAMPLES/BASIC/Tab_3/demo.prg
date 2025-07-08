/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2013-2017 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

FUNCTION Main

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'Harbour MiniGUI Demo' ;
         MAIN ;
         ON SIZE SizeTest()

      DEFINE MAIN MENU
         DEFINE POPUP 'Test'
            MENUITEM 'Disable Page 1' ACTION Form_1.Tab_1.Enabled( 1 ) := .F.
            MENUITEM 'Enable Page 1' ACTION Form_1.Tab_1.Enabled( 1 ) := .T.
            SEPARATOR
            MENUITEM 'Disable Page 2' ACTION Form_1.Tab_1.Enabled( 2 ) := .F.
            MENUITEM 'Enable Page 2' ACTION Form_1.Tab_1.Enabled( 2 ) := .T.
            SEPARATOR
            MENUITEM 'Disable Page 3' ACTION Form_1.Tab_1.Enabled( 3 ) := .F.
            MENUITEM 'Enable Page 3' ACTION Form_1.Tab_1.Enabled( 3 ) := .T.
            SEPARATOR
            MENUITEM "E&xit" ACTION Form_1.Release()
         END POPUP
      END MENU

      DEFINE TAB Tab_1 ;
            AT 10, 10 ;
            WIDTH 600 ;
            HEIGHT 400 ;
            VALUE 1 ;
            TOOLTIP 'Tab Control'

         PAGE 'Page &1'

            @ 40, 05 FRAME Frame_1 WIDTH 150 HEIGHT 120 CAPTION "Page 1" FONTCOLOR BLACK

            @ 60, 20 TEXTBOX txt_1 VALUE '1-Uno'
            @ 90, 20 TEXTBOX txt_2 VALUE '2-Dos'
            @ 120, 20 TEXTBOX txt_3 VALUE '3-Tres'

         END PAGE

         PAGE 'Page &2'

            @ 40, 45 FRAME Frame_2 WIDTH 150 HEIGHT 120 CAPTION "Page 2" FONTCOLOR BLACK

            @ 60, 60 TEXTBOX txt_a VALUE 'A-Uno'
            @ 90, 60 TEXTBOX txt_b VALUE 'B-Dos'

            @ 120, 60 COMBOBOX combo_1 ITEMS { '1-Uno', '2-Dos', '3-Tres' } VALUE 1

         END PAGE

         PAGE 'Page &3'

            @ 60, 100 TEXTBOX txt_c VALUE 'C-Uno'
            @ 90, 100 TEXTBOX txt_d VALUE 'D-Dos'

            @ 120, 100 SPINNER spinner_1 RANGE 0, 10 VALUE 5

            @ 150, 100 FRAME Frame_3 WIDTH 120 HEIGHT 110 CAPTION "Page 3" FONTCOLOR BLACK

            DEFINE RADIOGROUP R1
               ROW 170
               COL 120
               OPTIONS { 'Uno', 'Dos', 'Tres' }
               VALUE 1
               WIDTH 80
            END RADIOGROUP

         END PAGE

      END TAB

   END WINDOW

   SetProperty( 'Form_1', 'Tab_1', 'Enabled', 1, .F. )

   Form_1.CENTER

   Form_1.ACTIVATE

RETURN NIL


PROCEDURE SizeTest()

   Form_1.Tab_1.WIDTH := Form_1.WIDTH - 40
   Form_1.Tab_1.HEIGHT := Form_1.HEIGHT - 80

RETURN
