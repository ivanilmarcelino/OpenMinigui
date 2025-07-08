/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "minigui.ch"

FUNCTION Main

   LOCAL aData := {}, aMonths := {}, aColors
   LOCAL i

   FOR i := 1 TO 12
      AAdd( aData, { 10 * i } )
   NEXT

   FOR i := 1 TO 12
      AAdd( aMonths, cMonth( CtoD( StrZero( i, 2 ) + '/01/' + hb_ntos( Year( Date() ) ) ) ) )
   NEXT

   aColors := { RED, BLUE, OLIVE, GREEN, ORANGE, PURPLE, FUCHSIA, PINK, MAROON, GRAY, SILVER, TEAL }

   DEFINE WINDOW Graph ;
      CLIENTAREA 560, 560 ;
      NOMAXIMIZE ;
      BACKCOLOR { 179, 217, 255 } ;
      TITLE 'Draw a graph and print it (use CTRL+P for print)' ;
      MAIN

      DRAW GRAPH ;
         IN WINDOW Graph ;
         AT 10, 10 ;
         TO 550, 550 ;
         TITLE "Monthly Data" ;
         TYPE BARS ;
         SERIES aData ;
         YVALUES { "2019" } ;
         DEPTH 15 ;
         BARWIDTH 15 ;
         HVALUES 10 ;
         SERIENAMES aMonths ;
         COLORS aColors ;
         3DVIEW ;
         SHOWXGRID ;
         SHOWXVALUES ;
         SHOWYVALUES ;
         SHOWLEGENDS ;
         NOBORDER

      ON KEY CONTROL+P ACTION GraphPrint( aData, aMonths, aColors )
      ON KEY ESCAPE ACTION Graph.Release
   END WINDOW

   CENTER WINDOW Graph
   ACTIVATE WINDOW Graph

RETURN NIL


PROCEDURE GraphPrint( aData, aMonths, aColors )

      PRINT GRAPH ;
         IN WINDOW Graph ;
         AT 40, 40 ;
         TO 510, 510 ;
         TITLE "Monthly Data" ;
         TYPE BARS ;
         SERIES aData ;
         YVALUES { "2019" } ;
         DEPTH 15 ;
         BARWIDTH 15 ;
         HVALUES 10 ;
         SERIENAMES aMonths ;
         COLORS aColors ;
         3DVIEW ;
         SHOWXGRID ;
         SHOWXVALUES ;
         SHOWYVALUES ;
         SHOWLEGENDS

RETURN
