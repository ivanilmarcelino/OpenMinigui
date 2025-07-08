/*
 * HMG - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"

FUNCTION Main

   DEFINE WINDOW Form_1 ;
         WIDTH 240 HEIGHT 480 ;
         TITLE "Demo" ;
         ICON "demo.ico" ;
         MAIN

      @ 10, 10 BUTTONEX Control_1 ;
         CAPTION "HMG" ;
         ICON "demo.ico" ;
         FONT "Arial" SIZE 24 ;
         WIDTH 200 HEIGHT 80

      @ 100, 10 BUTTON Button_1 ;
         CAPTION "Get Values" ;
         ACTION GetValues()

      @ 150, 10 BUTTON Button_2 ;
         CAPTION "Arial" ;
         ACTION Form_1.Control_1.FontName := "Arial"

      @ 150, 110 BUTTON Button_3 ;
         CAPTION "Courier New" ;
         ACTION Form_1.Control_1.FontName := "Courier New"

      @ 200, 10 BUTTON Button_4 ;
         CAPTION "Size 24" ;
         ACTION Form_1.Control_1.FontSize := 24

      @ 200, 110 BUTTON Button_5 ;
         CAPTION "Size 36" ;
         ACTION Form_1.Control_1.FontSize := 36

      @ 250, 10 BUTTON Button_6 ;
         CAPTION "Bold ON" ;
         ACTION Form_1.Control_1.FontBold := .T.

      @ 250, 110 BUTTON Button_7 ;
         CAPTION "Bold OFF" ;
         ACTION Form_1.Control_1.FontBold := .F.

      @ 300, 10 BUTTON Button_8 ;
         CAPTION "Italic ON" ;
         ACTION Form_1.Control_1.FontItalic := .T.

      @ 300, 110 BUTTON Button_9 ;
         CAPTION "Italic OFF" ;
         ACTION Form_1.Control_1.FontItalic := .F.

      @ 350, 10 BUTTON Button_10 ;
         CAPTION "Underline ON" ;
         ACTION Form_1.Control_1.FontUnderline := .T.

      @ 350, 110 BUTTON Button_11 ;
         CAPTION "Underline OFF" ;
         ACTION Form_1.Control_1.FontUnderline := .F.

      @ 400, 10 BUTTON Button_12 ;
         CAPTION "Strikeout ON" ;
         ACTION Form_1.Control_1.FontStrikeout := .T.

      @ 400, 110 BUTTON Button_13 ;
         CAPTION "Strikeout OFF" ;
         ACTION Form_1.Control_1.FontStrikeout := .F.

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION GetValues

   LOCAL m := ""

   m += "Name: " + Form_1.Control_1.FontName + hb_eol()
   m += "Size: " + StrZero( Form_1.Control_1.FontSize, 2 ) + hb_eol()
   m += "Bold: " + if( Form_1.Control_1.FontBold, "True", "False" ) + hb_eol()
   m += "Italic: " + if( Form_1.Control_1.FontItalic, "True", "False" ) + hb_eol()
   m += "Underline: " + if( Form_1.Control_1.FontUnderline, "True", "False" ) + hb_eol()
   m += "Strikeout: " + if( Form_1.Control_1.FontStrikeout, "True", "False" ) + hb_eol()

   MsgInfo( m )

RETURN NIL
