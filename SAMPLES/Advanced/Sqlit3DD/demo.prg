/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2010-2019 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Based on RDDSQL sample included in Harbour distribution
 */

#include "minigui.ch"
#include "dbinfo.ch"

ANNOUNCE RDDSYS
REQUEST SDDSQLITE3, SQLMIX

MEMVAR memvarcountryname
MEMVAR memvarcountryresidents
MEMVAR memvarcountryupdated
*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*

   rddSetDefault( "SQLMIX" )

   IF rddInfo( RDDI_CONNECT, { "SQLITE3", hb_DirBase() + "test.sq3" } ) == 0
      MsgStop( "Unable connect to the server!", "Error" )
      RETURN NIL
   ENDIF

   /* ISO 8601 Calendar dates: YYYY-MM-DD */
   SET DATE FORMAT "yyyy-mm-dd"

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'SQLITE3 Database Driver Demo' ;
      MAIN NOMAXIMIZE ;
      ON INIT OpenTable() ;
      ON RELEASE CloseTable()

      DEFINE MAIN MENU

      DEFINE POPUP 'Test'
         MENUITEM 'Add record' ACTION AddRecord( 'Argentina', 38740000 )
         SEPARATOR
         MENUITEM "Exit" ACTION ThisWindow.Release()
      END POPUP

      END MENU

   @ 10, 10 BROWSE Browse_1 ;
      WIDTH 610 ;
      HEIGHT 390 ;
      HEADERS { 'Code', 'Name', 'Residents', 'Updated' } ;
      WIDTHS { 50, 160, 100, 100 } ;
      WORKAREA country ;
      FIELDS { 'country->Code', 'country->Name', 'country->Residents', 'country->Updated' } ;
      JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_RIGHT, BROWSE_JTFY_CENTER } ;
      EDIT ;
      VALID { , {|| sqlupdate( 2 ) }, {|| sqlupdate( 3 ) }, {|| sqlupdate( 4 ) } } ;
      READONLY { .T., .F., .F., .F. }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL

*--------------------------------------------------------*
PROCEDURE OpenTable
*--------------------------------------------------------*

   IF IsExistTable( "country" ) .OR. CreateTable()

      dbUseArea( .T.,, "SELECT * FROM country", "country" )

      INDEX ON FIELD->RESIDENTS TAG residents TO country

      GO TOP

   ELSE

      QUIT

   ENDIF

RETURN

*--------------------------------------------------------*
PROCEDURE CloseTable
*--------------------------------------------------------*

   dbCloseAll()

RETURN

*--------------------------------------------------------*
PROCEDURE AddRecord( cName, nResidents )
*--------------------------------------------------------*
   LOCAL cCode := Upper( Left( cName, 3 ) )

   IF rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('" + cCode + "', '" + cName + "', " + hb_ntos( nResidents ) + ", '" + dtoc( Date() ) + "')" )

      APPEND BLANK

      REPLACE CODE WITH cCode, ;
         RESIDENTS WITH nResidents, ;
         NAME WITH cName, ;
         Updated WITH Date()

      Form_1.Browse_1.Value := country->( RecNo() )
      Form_1.Browse_1.Refresh

   ELSE

      MsgStop( "Can't append record to table Country!", "Error" )

   ENDIF

RETURN

*--------------------------------------------------------*
FUNCTION sqlupdate( nColumn )
*--------------------------------------------------------*
   LOCAL nValue := Form_1.Browse_1.Value
   LOCAL cCode, cField, cNewValue

   cField := FieldName( nColumn )

   IF nColumn == 2

      cNewValue := "'" + MEMVAR.Country.Name + "'"

   ELSEIF nColumn == 3

      cNewValue := hb_ntos( MEMVAR.Country.Residents )

   ELSEIF nColumn == 4

      cNewValue := "'" + dtoc( MEMVAR.Country.Updated ) + "'"

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
   LOCAL ret := .T., cupd := "'" + dtoc( Date() ) + "'"

   IF IsExistTable( "country" )
      rddInfo( RDDI_EXECUTE, "DROP TABLE country" )
   ENDIF

   IF rddInfo( RDDI_EXECUTE, "CREATE TABLE country (CODE char(3), NAME char(50), RESIDENTS int(11), Updated date(10))" )

      If ! rddInfo( RDDI_EXECUTE, "INSERT INTO country values ('LTU', 'Lithuania', 3369600, " + cupd + "), ('USA', 'United States of America', 305397000, " + cupd + "), ('POR', 'Portugal', 10617600, " + cupd + "), ('POL', 'Poland', 38115967, " + cupd + "), ('AUS', 'Australia', 21446187, " + cupd + "), ('FRA', 'France', 64473140, " + cupd + "), ('RUS', 'Russia', 141900000, " + cupd + ")" )

         MsgStop( "Can't fill table Country!", "Error" )
         ret := .F.

      ENDIF

   ELSE

      MsgStop( "Can't create table Country!", "Error" )
      ret := .F.

   ENDIF

RETURN ret

*--------------------------------------------------------*
FUNCTION IsExistTable( cTable )
*--------------------------------------------------------*
   LOCAL ret

   dbUseArea( .T.,, "SELECT name FROM sqlite_master WHERE type='table' AND name='" + cTable + "'", "tables" )

   ret := ( tables->( LastRec() ) > 0 )

   dbCloseArea()

RETURN ret
