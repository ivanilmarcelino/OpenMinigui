/*
  MINIGUI - Harbour Win32 GUI library Demo/Sample

  Copyright 2002-08 Roberto Lopez <harbourminigui@gmail.com>
  http://harbourminigui.googlepages.com

  'Draw Border' is Open Source/Freeware HMG Demo / Sample.

  All bug reports and suggestions are welcome.

  Developed with MiniGUI  - Harbour Win32 GUI library (HMG),
  Compiled and Linked with Harbour Compiler and MinGW.

  Thanks to "Le Roy" Roberto Lopez.

  Copyright 2013 © Bicahi Esgici <esgici @ gmail.com>

*/

#include <hmg.ch>
#include "DrawBorder.ch"

PROCEDURE Main()

   LOAD WINDOW TestBorder AS frmTestBorder

   ON KEY ESCAPE OF frmTestBorder ACTION ThisWindow.RELEASE

   frmTestBorder.CENTER
   frmTestBorder.ACTIVATE

RETURN // Main()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE Draw_All()

   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Label_1"
   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Label_2" PENWIDTH 3 UPCOLOR 1 DOWNCOLOR 0
   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Label_3" PENWIDTH 3 UPCOLOR 0 DOWNCOLOR 1
   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Label_4" PENWIDTH 3 UPCOLOR { 190, 210, 230 } DOWNCOLOR { 100, 149, 237 }

   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Button_1"
   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Button_2" PENWIDTH 3 UPCOLOR 1 DOWNCOLOR 0
   DRAW BORDER WINDOW "frmTestBorder" CONTROL "Button_3" PENWIDTH 3 UPCOLOR 0 DOWNCOLOR 1

   DrawWideBorder( "frmTestBorder", "Image_1", { 164, 260, 155 } )

RETURN // Draw_All()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE DrawWideBorder( ;
      cWindowName, ;
      cControlName, ;
      aColor, ;
      nSpace, ;
      nStep )

   LOCAL nWidness, aCurColor

   hb_default( @nSpace, 10 )
   hb_default( @nSTEP, 1 )

   FOR nWidness := 1 TO nSpace STEP nStep

      aCurColor := AClone( aColor )

      DRAW BORDER WINDOW cWindowName CONTROL cControlName UPCOLOR aCurColor DOWNCOLOR aCurColor SPACE nWidness

      aColor[ 1 ] -= 10
      aColor[ 2 ] -= 10
      aColor[ 3 ] += 10

   NEXT nWidness

RETURN // DrawWideBorder()
