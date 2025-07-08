/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"

REQUEST DBFCDX

FUNCTION MAIN

   CreateDBF()

   USE GRAFDATA NEW VIA "DBFCDX"
   SET ORDER TO TAG MAIN
   GO TOP

   SET FONT TO _GetSysFont(), 10

   DEFINE WINDOW GraphTest ;
         CLIENTAREA 1140, 500 ;
         TITLE "Bar graph from database" ;
         MAIN ;
         NOMAXIMIZE Nominimize ;
         NOSIZE ;
         ON Init ( DrawBarGraph( "NORTH", 20, 575 ), DrawBarGraph( "SOUTH", 580, 1410 ) ) ;
         ON Release ( dbCloseAll(), FileDelete( "GRAFDATA.*" ) )

   END WINDOW

   CENTER WINDOW GraphTest
   ACTIVATE WINDOW GraphTest

RETURN NIL


PROCEDURE DrawBarGraph ( cRegion, nLeft, nRight )

   LOCAL aProd := HMG_DbfToArray( "JAN,FEB,MAR,APR,MAY", {|| FIELD->REGION = cRegion } )
   LOCAL aSName := HMG_DbfToArray( "PRODUCT", {|| FIELD->REGION = cRegion } )
   LOCAL aClrs := {}
   LOCAL aSerie := {}

   AAdd( aSerie, aProd[ 1 ] )
   AAdd( aSerie, aProd[ 2 ] )
   AAdd( aSerie, aProd[ 3 ] )
   AAdd( aClrs, HMG_n2RGB( METRO_AMBER ) )
   AAdd( aClrs, HMG_n2RGB( METRO_OLIVE ) )
   AAdd( aClrs, HMG_n2RGB( CLR_HMAGENTA ) )

   DRAW GRAPH ;
      IN WINDOW GraphTest ;
      AT 20, nLeft ;
      TO 490, nRight ;
      TITLE "ABC COMPANY, " + cRegion + " REGION" ;
      TYPE BARS ;
      SERIES aSerie ;
      YVALUES { "Jan", "Feb", "Mar", "Apr", "May" } ;
      DEPTH 1 ;
      BARWIDTH 10 ;
      HVALUES 4 ;
      SERIENAMES aSName ;
      COLORS aClrs ;
      SHOWXVALUES ;
      SHOWYVALUES ;
      SHOWGRID ;
      SHOWLEGENDS

RETURN


STATIC FUNCTION CreateDBF()

   FIELD REGION, PRODUCT

   LOCAL aData := ;
      { { "NORTH", "DeskTop", 14280, 20420, 12870, 25347, 7640 } ;
      , { "NORTH", "Lap-Top", 8350, 10315, 15870, 5347, 12340 } ;
      , { "NORTH", "Printer", 12345, 8945, 10560, 15600, 17610 } ;
      , { "SOUTH", "DeskTop", 12345, 8945, 10560, 15600, 17610 } ;
      , { "SOUTH", "Lap-Top", 14280, 20420, 12870, 25347, 7640 } ;
      , { "SOUTH", "Printer", 8350, 10315, 15870, 5347, 12340 } ;
      }

   dbCreate( "GRAFDATA", { { "REGION", "C", 5, 0 }, { "PRODUCT", "C", 7, 0 }, ;
      { "JAN", "N", 5, 0 }, { "FEB", "N", 5, 0 }, { "MAR", "N", 5, 0 }, ;
      { "APR", "N", 5, 0 }, { "MAY", "N", 5, 0 } }, "DBFCDX", .T., "DATA" )

   HMG_ArrayToDBF( aData )

   GO TOP
   INDEX ON REGION + PRODUCT TAG MAIN
   CLOSE DATA

RETURN NIL
