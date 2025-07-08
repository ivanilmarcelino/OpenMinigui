/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "minigui.ch"

REQUEST DBFCDX

FIELD COUNTRY,REGION,CODE

//----------------------------------------------------------------------------//

FUNCTION Main()

   CreateTables()

   USE REGION NEW VIA "DBFCDX"
   SET ORDER TO TAG CODE
   GO TOP

   USE COUNTRY NEW VIA "DBFCDX"
   SET ORDER TO TAG CODE
   SET RELATION TO REGION INTO REGION
   GO TOP

   USE CITY NEW VIA "DBFCDX"
   SET ORDER TO TAG COUNTRY
   SET RELATION TO COUNTRY INTO COUNTRY
   GO TOP

   SELECT CITY

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 460 HEIGHT 400 ;
         TITLE 'MiniGUI Browse Demo' ;
         MAIN NOMAXIMIZE ;
         ON RELEASE ( dbCloseAll(), FileDelete( "*.cdx" ), FileDelete( "*.dbf" ) )

      @ 10, 10 BROWSE Brw_1 ;
         WIDTH 420 ;
         HEIGHT 340 ;
         HEADERS { 'REGION', 'CONTRY', 'CITY', 'AIRPORT' } ;
         WIDTHS { 100, 100, 100, 95 } ;
         WORKAREA CITY ;
         FIELDS { 'REGION->REGION', 'COUNTRY->COUNTRY', 'CITY', 'CODE' }

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

//----------------------------------------------------------------------------//

STATIC FUNCTION CreateTables()

   LOCAL aRegion  := { { "EU", "Europe" }, { "NA", "North America" } }
   LOCAL aCountry := { { "SP", "EU", "Spain" }, { "IT", "EU", "Italy" }, ;
                       { "US", "NA", "USA" }, { "CA", "NA", "Canada" } }

   LOCAL aCity    := { { "MAD", "SP", "Madrid" }, { "AGP", "SP", "Malaga" }, ;
                       { "TRN", "IT", "Turin"  }, { "VCE", "IT", "Venice" }, ;
                       { "JFK", "US", "New York"},{ "BOS", "US", "Boston" }, ;
                       { "YOW", "CA", "Ottawa" }, { "YUL", "CA", "Montriel" } }


   DBCREATE( "REGION.DBF", {{"CODE","C",2,0},{"REGION","C",15,0}}, "DBFCDX", .T., "TMP" )
   HMG_ArrayToDbf( aRegion )
   INDEX ON CODE TAG CODE
   CLOSE TMP

   DBCREATE( "COUNTRY.DBF", {{"CODE","C",2,0},{"REGION","C",2,0},{"COUNTRY","C",15,0}}, "DBFCDX", .T., "TMP" )
   HMG_ArrayToDbf( aCountry )
   INDEX ON CODE TAG CODE
   CLOSE TMP

   DBCREATE( "CITY.DBF", {{"CODE","C",3,0},{"COUNTRY","C",2,0},{"CITY","C",15,0}}, "DBFCDX", .T., "TMP" )
   HMG_ArrayToDbf( aCity )
   INDEX ON COUNTRY TAG COUNTRY
   CLOSE TMP

RETURN NIL
