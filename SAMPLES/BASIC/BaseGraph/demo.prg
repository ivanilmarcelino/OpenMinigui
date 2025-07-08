/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2007 MigSoft <fugaz_cl@yahoo.es>
*/

#include "minigui.ch"

#define AZUL     {   0 , 128 , 192  }
#define CELESTE  {   0 , 128 , 255  }
#define VERDE    {   0 , 128 , 128  }
#define CAFE     { 128 , 64  ,   0  }

STATIC aYvalAll := { "Ene", "Feb", "Mar", "Abr", "May", "Jun", ;
                     "Jul", "Ago", "Sep", "Oct", "Nov", "Dic" }
STATIC aYval1er := { "Ene", "Feb", "Mar", "Abr", "May", "Jun" }
STATIC aYval2do := { "Jul", "Ago", "Sep", "Oct", "Nov", "Dic" }

STATIC aSerieNames

PROCEDURE Main

   LOCAL aClr := { RED, YELLOW, AZUL, ORANGE, VERDE, FUCHSIA, GREEN, CAFE, ;
                   BLUE, BROWN, PINK, PURPLE, BLACK, WHITE, GRAY }
   LOCAL n := 1, cNombre, m
   LOCAL nReg, aColor, aColor1, aSer, aSer1, aSer2

   USE SALDOMES

   nReg := RecCount()
   aColor := Array( nReg )
   aColor1 := Array( nReg )
   aSerieNames := Array( nReg )
   aSer := Array( nReg, 12 )
   aSer1 := Array( nReg, 6 )
   aSer2 := Array( nReg, 6 )

   DO WHILE ! saldomes->( Eof() )
      cNombre := Lower( saldomes->Banco )
      aSerieNames[ n ] := cNombre
      aSer[ n, 1 ] := saldomes->enero ; aSer[ n, 2 ] := saldomes->febrero
      aSer[ n, 3 ] := saldomes->marzo ; aSer[ n, 4 ] := saldomes->abril
      aSer[ n, 5 ] := saldomes->mayo ; aSer[ n, 6 ] := saldomes->junio
      aSer[ n, 7 ] := saldomes->julio ; aSer[ n, 8 ] := saldomes->agosto
      aSer[ n, 9 ] := saldomes->septiembre ; aSer[ n, 10 ] := saldomes->octubre
      aSer[ n, 11 ] := saldomes->noviembre ; aSer[ n, 12 ] := saldomes->diciembre
      aColor[ n ] := aClr[ n ]
      SKIP
      n++
   ENDDO

   FOR n = 1 TO nReg
      FOR m = 1 TO 6
         aSer1[ n, m ] := aSer[ n, m ]
         aSer2[ n, m ] := aSer[ n, m + 6 ]
      NEXT
      aColor1[ n ] := aClr[ n ]
   NEXT

   DEFINE WINDOW GraphTest ;
         AT 0, 0 ;
         WIDTH 720 ;
         HEIGHT 480 ;
         TITLE "Graph Demo" ;
         MAIN ;
         ICON "Graph.ico" ;
         NOMAXIMIZE NOSIZE ;
         ON INIT DrawBarGraph( aSer, aYvalAll, aColor )

      DEFINE BUTTON Button_1
         ROW 405
         COL 40
         CAPTION '1er Semestre'
         ACTION DrawBarGraph( aSer1, aYval1er, aColor1 )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 405
         COL 180
         CAPTION '2do Semestre'
         ACTION DrawBarGraph( aSer2, aYval2do, aColor1 )
      END BUTTON

      DEFINE BUTTON Button_3
         ROW 405
         COL 320
         CAPTION 'Lineas'
         ACTION DrawLinesGraph( aSer, aYvalAll, aColor )
      END BUTTON

      DEFINE BUTTON Button_4
         ROW 405
         COL 460
         CAPTION 'Puntos'
         ACTION DrawPointsGraph( aSer, aYvalAll, aColor )
      END BUTTON

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   GraphTest.Center
   ACTIVATE WINDOW GraphTest

RETURN

PROCEDURE DrawBarGraph( paSer, paYval, paCol )

   ERASE WINDOW GraphTest

   DRAW GRAPH IN WINDOW GraphTest ;
      AT 20, 20 ;
      TO 400, 700 ;
      TITLE "Saldo por Banco" ;
      TYPE BARS ;
      SERIES paSer ;
      YVALUES paYval ;
      DEPTH 15 ;
      BARWIDTH 15 ;
      HVALUES 5 ;
      SERIENAMES aSerieNames ;
      COLORS paCol ;
      3DVIEW ;
      SHOWGRID ;
      SHOWXVALUES ;
      SHOWYVALUES ;
      SHOWLEGENDS ;
      DATAMASK "$99,999"

RETURN

PROCEDURE DrawLinesGraph( paSer, paYval, paCol )

   ERASE WINDOW GraphTest

   DRAW GRAPH IN WINDOW GraphTest ;
      AT 20, 20 ;
      TO 400, 700 ;
      TITLE "Saldo por Banco" ;
      TYPE LINES ;
      SERIES paSer ;
      YVALUES paYval ;
      DEPTH 15 ;
      BARWIDTH 15 ;
      HVALUES 5 ;
      SERIENAMES aSerieNames ;
      COLORS paCol ;
      3DVIEW ;
      SHOWGRID ;
      SHOWXVALUES ;
      SHOWYVALUES ;
      SHOWLEGENDS ;
      DATAMASK "$99,999"

RETURN

PROCEDURE DrawPointsGraph( paSer, paYval, paCol )

   ERASE WINDOW GraphTest

   DRAW GRAPH IN WINDOW GraphTest ;
      AT 20, 20 ;
      TO 400, 700 ;
      TITLE "Saldo por Banco" ;
      TYPE POINTS ;
      SERIES paSer ;
      YVALUES paYval ;
      DEPTH 15 ;
      BARWIDTH 15 ;
      HVALUES 5 ;
      SERIENAMES aSerieNames ;
      COLORS paCol ;
      3DVIEW ;
      SHOWGRID ;
      SHOWXVALUES ;
      SHOWYVALUES ;
      SHOWLEGENDS ;
      DATAMASK "$99,999"

RETURN
