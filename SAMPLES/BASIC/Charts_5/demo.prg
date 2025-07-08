/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021-2022 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

ANNOUNCE RDDSYS

STATIC aSer1, aSer2, aSer3
STATIC aSerName1, aSerName2, aSerName3
STATIC aSerVal1, aSerVal3
STATIC aClrs, nGraphType

PROCEDURE Main()

   aClrs := { RED, ;
      LGREEN, ;
      YELLOW, ;
      BLUE, ;
      WHITE, ;
      GRAY, ;
      FUCHSIA, ;
      TEAL, ;
      NAVY, ;
      MAROON, ;
      GREEN, ;
      OLIVE, ;
      PURPLE, ;
      SILVER, ;
      AQUA, ;
      BLACK, ;
      RED, ;
      LGREEN, ;
      YELLOW, ;
      BLUE }

   DEFINE WINDOW GraphTest ;
         AT 0, 0 ;
         WIDTH 640 ;
         HEIGHT 580 ;
         TITLE "Charts DBF Demo by Grigory Filatov" ;
         MAIN ;
         ICON "Chart.ico" ;
         NOMAXIMIZE NOSIZE ;
         BACKCOLOR iif( ISVISTAORLATER(), { 220, 220, 220 }, Nil ) ;
         FONT "Tahoma" SIZE 9 ;
         ON INIT OpenTable()

      DEFINE BUTTON Button_1
         ROW 510
         COL 30
         CAPTION 'Chart &1'
         ACTION drawchart_1( aser1 )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 510
         COL 150
         CAPTION 'Chart &2'
         ACTION drawchart_2( aser2 )
      END BUTTON

      DEFINE BUTTON Button_3
         ROW 510
         COL 270
         CAPTION 'Chart &3'
         ACTION drawchart_3( aser3 )
      END BUTTON

      DEFINE BUTTON Button_4
         ROW 510
         COL 390
         CAPTION '&Print'
         ACTION PrintGraph( nGraphType )
      END BUTTON

      DEFINE BUTTON Button_5
         ROW 510
         COL 510
         CAPTION 'E&xit'
         ACTION GraphTest.Release()
      END BUTTON

   END WINDOW

   GraphTest.Center()

   ACTIVATE WINDOW GraphTest

RETURN

PROCEDURE DrawChart_1 ( aSer )

   nGraphType := 1

   ERASE WINDOW GraphTest

   DRAW GRAPH ;
      IN WINDOW GraphTest ;
      AT 20, 20 ;
      TO 500, 610 ;
      TITLE "Population (top 10 values)" ;
      TYPE BARS ;
      SERIES aSer ;
      YVALUES aSerVal1 ;
      DEPTH 12 ;
      BARWIDTH 12 ;
      HVALUES 10 ;
      SERIENAMES aSerName1 ;
      COLORS aClrs ;
      3DVIEW ;
      SHOWXGRID ;
      SHOWXVALUES ;
      SHOWLEGENDS LEGENDSWIDTH 70 DATAMASK "9,999,999"

   GraphTest.Button_1.SetFocus

RETURN

PROCEDURE DrawChart_2 ( aSer )

   nGraphType := 2

   ERASE WINDOW GraphTest

   DRAW GRAPH ;
      IN WINDOW GraphTest ;
      AT 20, 130 ;
      TO 490, 500 ;
      TITLE "Area size (top 10 values)" ;
      TYPE PIE ;
      SERIES aSer ;
      DEPTH 15 ;
      SERIENAMES aSerName2 ;
      COLORS aClrs ;
      3DVIEW ;
      SHOWXVALUES ;
      SHOWLEGENDS

   GraphTest.Button_2.SetFocus

RETURN

PROCEDURE DrawChart_3 ( aSer )

   nGraphType := 3

   ERASE WINDOW GraphTest

   DRAW GRAPH ;
      IN WINDOW GraphTest ;
      AT 20, 0 ;
      TO 500, 590 ;
      TITLE "Population density (top 20 values)" ;
      TYPE BARS ;
      SERIES aSer ;
      YVALUES aSerVal3 ;
      DEPTH 4 ;
      BARWIDTH 8 ;
      HVALUES 5 ;
      SERIENAMES aSerName3 ;
      COLORS aClrs ;
      3DVIEW ;
      SHOWXGRID ;
      SHOWXVALUES ;
      SHOWLEGENDS LEGENDSWIDTH 105 DATAMASK "9 999"

   GraphTest.Button_3.SetFocus

RETURN

PROCEDURE PrintGraph()

   GraphTest.Button_4.SetFocus

   SWITCH nGraphType
   CASE 1
      PRINT GRAPH ;
         IN WINDOW GraphTest ;
         AT 20, 20 ;
         TO 500, 610 ;
         TITLE "Population (top 10 values)" ;
         TYPE BARS ;
         SERIES aSer1 ;
         YVALUES aSerVal1 ;
         DEPTH 12 ;
         BARWIDTH 12 ;
         HVALUES 10 ;
         SERIENAMES aSerName1 ;
         COLORS aClrs ;
         3DVIEW ;
         SHOWXGRID ;
         SHOWXVALUES ;
         SHOWLEGENDS LEGENDSWIDTH 70 DATAMASK "9,999,999" ;
         LIBRARY HBPRINT
      EXIT
   CASE 2
      PRINT GRAPH ;
         IN WINDOW GraphTest ;
         AT 20, 130 ;
         TO 490, 500 ;
         TITLE "Area size (top 10 values)" ;
         TYPE PIE ;
         SERIES aSer2 ;
         DEPTH 15 ;
         SERIENAMES aSerName2 ;
         COLORS aClrs ;
         3DVIEW ;
         SHOWXVALUES ;
         SHOWLEGENDS ;
         LIBRARY HBPRINT
      EXIT
   CASE 3
      PRINT GRAPH ;
         IN WINDOW GraphTest ;
         AT 20, 0 ;
         TO 500, 590 ;
         TITLE "Population density (top 20 values)" ;
         TYPE BARS ;
         SERIES aSer3 ;
         YVALUES aSerVal3 ;
         DEPTH 4 ;
         BARWIDTH 8 ;
         HVALUES 5 ;
         SERIENAMES aSerName3 ;
         COLORS aClrs ;
         3DVIEW ;
         SHOWXGRID ;
         SHOWXVALUES ;
         SHOWLEGENDS LEGENDSWIDTH 105 DATAMASK "9 999" ;
         LIBRARY HBPRINT
   END

RETURN

PROCEDURE OpenTable

   LOCAL oServer
   LOCAL oQuery
   LOCAL oRow
   LOCAL n, r
   LOCAL Sql

   IF ! ConnectTo( @oServer )
      MsgStop( "Unable connect to the server!", "Error" )
      RETURN
   ENDIF

   // Request data for Chart 1
   //
   sql := "SELECT * FROM Country ORDER BY Population DESC LIMIT 10" // top 10 values

   oQuery := oServer:Query( sql )
   IF oQuery:lError
      MsgAlert( oQuery:Error(), "MySQL Error" )
      RETURN
   ENDIF

   // One serie data
   aSer1 := Array( 10, 1 )
   aSerVal1 := Array( 10 )
   aSerName1 := Array( 10 )

   n := 0
   oQuery:gotop()
   WHILE ! oQuery:eof()
      oRow := oQuery:GetRow( ++n )
      aSer1[ n, 1 ] := oRow:fieldGet( "Population" ) / 1000
      aSerVal1[ n ] := oRow:fieldGet( "Name" )
      aSerName1[ n ] := aSerVal1[ n ]
      oQuery:Skip( 1 )
   ENDDO
   oQuery:Destroy()

   // Request data for Chart 2
   //
   sql := "SELECT Name, SurfaceArea FROM Country ORDER BY SurfaceArea DESC LIMIT 10" // top 10 values

   oQuery := oServer:Query( sql )
   IF oQuery:lError
      MsgAlert( oQuery:Error(), "MySQL Error" )
      RETURN
   ENDIF

   // One serie data
   aSer2 := Array( 10 )
   aSerName2 := Array( 10 )

   n := 0
   oQuery:gotop()
   WHILE ! oQuery:eof()
      oRow := oQuery:GetRow( ++n )
      aSer2[ n ] := Round( oRow:fieldGet( "SurfaceArea" ) / 1000, 2 )
      aSerName2[ n ] := Trim( oRow:fieldGet( "Name" ) )
      oQuery:Skip( 1 )
   ENDDO
   oQuery:Destroy()

   // Request data for Chart 3
   //
   sql := "SELECT Name, Population / SurfaceArea as off FROM Country WHERE SurfaceArea > 250 ORDER BY off DESC LIMIT 40"

   oQuery := oServer:Query( sql )
   IF oQuery:lError
      MsgAlert( oQuery:Error(), "MySQL Error" )
      RETURN
   ENDIF

   // One serie data
   aSer3 := Array( 20, 1 )
   aSerVal3 := Array( 20 )
   aSerName3 := Array( 20 )

   n := 0
   r := 0
   oQuery:gotop()
   WHILE ! oQuery:eof()
      oRow := oQuery:GetRow( ++r )
      IF oRow:fieldGet( "off" ) > 5000
         oQuery:Skip( 1 )
         LOOP
      ENDIF
      IF n > 19
         EXIT
      ENDIF
      aSer3[ ++n, 1 ] := Round( oRow:fieldGet( "off" ), 3 )
      aSerVal3[ n ] := oRow:fieldGet( "Name" )
      aSerName3[ n ] := Transform( aSer3[ n, 1 ], "9 999.999" ) + ' ' + aSerVal3[ n ]
      oQuery:Skip( 1 )
   ENDDO
   oQuery:Destroy()

   // First chart drawing
   DrawChart_1( aser1 )

RETURN

#include "connto.prg"
