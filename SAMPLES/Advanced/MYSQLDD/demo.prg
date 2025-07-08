/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2008 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2008 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Based on RDDSQL sample included in Harbour distribution
*/

#include "minigui.ch"
#include "dbinfo.ch"
#include "error.ch"

ANNOUNCE RDDSYS
REQUEST SDDMY, SQLMIX

MEMVAR memvarcountryname
MEMVAR memvarcountryresidents
*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*

   rddSetDefault( "SQLMIX" )

   IF rddInfo( RDDI_CONNECT, { "MYSQL", "localhost", "root",, "test" } ) == 0
      MsgStop( "Unable connect to the server!", "Error" )
      RETURN NIL
   ENDIF

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'MiniGUI SQL Database Driver Demo' ;
         MAIN NOMAXIMIZE ;
         ON INIT OpenTable() ;
         ON RELEASE CloseTable()

      DEFINE MAIN MENU

         DEFINE POPUP 'Test'
            MENUITEM 'Add record' ACTION AddRecord( 'ARG', 'Argentina', 38740000 )
            SEPARATOR
            ITEM "Exit" ACTION ThisWindow.Release()
         END POPUP

      END MENU

      @ 10, 10 BROWSE Browse_1 ;
         WIDTH 610 ;
         HEIGHT 390 ;
         HEADERS { 'Code', 'Name', 'Residents' } ;
         WIDTHS { 50, 160, 100 } ;
         WORKAREA country ;
         FIELDS { 'country->Code', 'country->Name', 'country->Residents' } ;
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_RIGHT } ;
         EDIT ;
         VALID {, {|| sqlupdate( 2 ) }, {|| sqlupdate( 3 ) } } ;
         READONLY { .T., .F., .F. }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL

*--------------------------------------------------------*
PROCEDURE OpenTable
*--------------------------------------------------------*

   IF CreateTable()

      dbUseArea( .T.,, "SELECT * FROM country", "country" )

      INDEX ON FIELD->RESIDENTS TAG residents TO country

      GO TOP

   ELSE

      Form_1.Release()

   ENDIF

RETURN

*--------------------------------------------------------*
PROCEDURE CloseTable
*--------------------------------------------------------*

   dbCloseAll()

RETURN

*--------------------------------------------------------*
PROCEDURE AddRecord( cCode, cName, nResidents )
*--------------------------------------------------------*

   IF rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('" + cCode + "', '" + cName + "', " + LTrim( Str( nResidents ) ) + ")" )

      // MsgInfo( RDDINFO(RDDI_AFFECTEDROWS), "Count of Affected Rows" )

      APPEND BLANK

      REPLACE CODE WITH cCode, ;
         NAME WITH cName, ;
         RESIDENTS WITH nResidents

      Form_1.Browse_1.VALUE := country->( RecNo() )
      Form_1.Browse_1.Refresh

   ELSE

      MsgStop( "Can't append record to table Country!", "Error" )

   ENDIF

RETURN

*--------------------------------------------------------*
FUNCTION sqlupdate( nColumn )
*--------------------------------------------------------*
   LOCAL nValue := Form_1.Browse_1.VALUE
   LOCAL cCode, cField, cNewValue

   IF nColumn == 2

      cField := "Name"
      cNewValue := "'" + MEMVAR.Country.NAME + "'"

   ELSEIF nColumn == 3

      cField := "Residents"
      cNewValue := LTrim( Str( MEMVAR.Country.Residents ) )

   ENDIF

   GO nValue
   cCode := "'" + country->CODE + "'"

   If ! rddInfo( RDDI_EXECUTE, "UPDATE country SET " + cField + " = " + cNewValue + " WHERE CODE = " + cCode )

      MsgStop( "Can't update record in table Country!", "Error" )
      RETURN .F.

   ENDIF

RETURN .T.

*--------------------------------------------------------*
FUNCTION CreateTable
*--------------------------------------------------------*
   LOCAL ret := .T.

   rddInfo( RDDI_EXECUTE, "DROP TABLE country" )

   IF rddInfo( RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int(11))" )

      If ! rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('LTU', 'Lithuania', 3369600), ('USA', 'United States of America', 305397000), ('POR', 'Portugal', 10617600), ('POL', 'Poland', 38115967), ('AUS', 'Australia', 21446187), ('FRA', 'France', 64473140), ('RUS', 'Russia', 141900000)" )

         MsgStop( "Can't fill table Country!", "Error" )
         ret := .F.

      ENDIF

   ELSE

      MsgStop( "Can't create table Country!", "Error" )
      ret := .F.

   ENDIF

RETURN ret
