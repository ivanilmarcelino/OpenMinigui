/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2020-2024 Grigory Filatov
*/

#include "minigui.ch"
#include "winprint.ch"


FUNCTION Main()

   LOCAL aSymbol, aSymbol2, aSymbol3, y, x, cSymbol

   aSymbol := { 'WHITE', 'BLUE', 'GREEN', 'FUCHSIA', 'RED', 'PURPLE', 'YELLOW', 'OLIVE', 'SILVER', 'GRAY' }
   aSymbol2 := { 'PINK', 'BROWN', 'greenyellow', 'ORANGE', 'MAROON', 'AQUA', 'NAVY', 'TEAL', 'thistle', 'tomato' }
   aSymbol3 := { 'antiquewhite', 'blueviolet', 'goldenrod', 'royalblue', 'rosybrown', 'aquamarine', 'blanchedalmond', 'burlywood', 'honeydew', 'powderblue' }

   DEFINE WINDOW Form_1 ;
         WIDTH 400 ;
         HEIGHT 380 ;
         TITLE 'Character Colors Demo' ;
         MAIN

      y := 20
      x := 20

      FOR EACH cSymbol IN aSymbol
         @ y, x LABEL NUL WIDTH 100 HEIGHT 24 ;
            VALUE cSymbol CENTERALIGN VCENTERALIGN BACKCOLOR cColorToRGB( cSymbol )

         y += 30
      NEXT

      y := 20
      x += 120

      FOR EACH cSymbol IN aSymbol2
         @ y, x LABEL NUL WIDTH 100 HEIGHT 24 ;
            VALUE cSymbol CENTERALIGN VCENTERALIGN BACKCOLOR cColorToRGB( cSymbol )

         y += 30
      NEXT

      y := 20
      x += 120

      FOR EACH cSymbol IN aSymbol3
         @ y, x LABEL NUL WIDTH 100 HEIGHT 24 ;
            VALUE cSymbol CENTERALIGN VCENTERALIGN BACKCOLOR cColorToRGB( cSymbol )

         y += 30
      NEXT

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION cColorToRGB( gr )

   LOCAL cColor, hexNumber

   INIT PRINTSYS
   cColor := HBPRNCOLOR( gr )
   RELEASE PRINTSYS

   hexNumber := DECTOHEXA( cColor )

RETURN RGB( HEXATODEC( SubStr( HexNumber, -2 ) ), HEXATODEC( SubStr( HexNumber, 5, 2 ) ), HEXATODEC( SubStr( HexNumber, 3, 2 ) ) )
