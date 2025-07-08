/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2010-2021 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Based on RDDSQL sample included in Harbour distribution
 */

#include "minigui.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

ANNOUNCE RDDSYS
REQUEST SDDSQLITE3, SQLMIX

MEMVAR oBrw_1
*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*

   rddSetDefault( "SQLMIX" )

   IF rddInfo( RDDI_CONNECT, { "SQLITE3", hb_DirBase() + "test.sq3" } ) == 0
      MsgStop( "Unable connect to the server!", "Error" )
      RETURN NIL
   ENDIF

   SET DATE FORMAT "yyyy-mm-dd"

   OpenTable()

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'SQLITE3 Database Driver Demo' ;
      MAIN NOMAXIMIZE ;
      ON RELEASE CloseTable()

      DEFINE MAIN MENU

      DEFINE POPUP 'Test'
         MENUITEM 'Add record' ACTION AddRecord( 'Argentina', 38740000 )
         SEPARATOR
         MENUITEM "Exit" ACTION ThisWindow.Release()
      END POPUP

      END MENU

   CreateBrowse( "oBrw_1", 'Form_1', 10, 10, Form_1.Width - 35, ;
      Form_1.Height - GetTitleHeight() - iif( IsThemed(), 1, 2 ) * GetBorderHeight() - 45, "country" )

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION CreateBrowse( cBrw, cParent, nRow, nCol, nWidth, nHeight, cAlias )

   LOCAL i

   PUBLIC &cBrw

   DEFINE TBROWSE &cBrw ;
      AT nRow, nCol ;
      ALIAS cAlias ;
      OF &cParent ;
      WIDTH  nWidth ;
      HEIGHT nHeight ;
      COLORS { CLR_BLACK, CLR_BLUE } ;
      FONT "MS Sans Serif" ;
      SIZE 12

      :SetAppendMode( .F. )
      :SetDeleteMode( .F. )

      :lNoHScroll := .T.
      :lCellBrw := .T.
      :nSelWidth := 16

      // loading the ALL database fields
      LoadFields( cBrw, cParent )

      :nHeightCell += 4
      :nHeightHead += 10
      :nWheelLines := 1

      :lNoChangeOrd := TRUE
      :hBrush := CreateSolidBrush( 230, 240, 255 )

      :SetColor( { 16 },  {        RGB(  43, 149, 168 ) } )                             //  SyperHeader backcolor
      :SetColor( {  3 },  {        RGB( 255, 255, 255 ) } )                             //  Header font color
      :SetColor( {  4 },  { {|| { RGB(  43, 149, 168 ), RGB(   0,  54,  94 ) } } } )    //  Header backcolor
      :SetColor( { 17 },  {        RGB( 255, 255, 255 ) } )                             //  Font color in SyperHeader
      :SetColor( {  6 },  { {|| { RGB( 255, 255,  74 ), RGB( 240, 240,   0 ) } } } )    //  Cursor backcolor
      :SetColor( { 12 },  { {|| { RGB( 128, 128, 128 ), RGB( 250, 250, 250 ) } } } )    //  Inactive cursor backcolor
      :SetColor( {  2 },  { {||   RGB( 230, 240, 255 ) } } )                            //  Grid backcolor
      :SetColor( {  1 },  { {||   RGB(   0,   0,   0 ) } } )                            //  Text color in grid
      :SetColor( {  5 },  { {||   RGB(   0,   0, 255 ) } } )                            //  Text color of cursor in grid
      :SetColor( { 11 },  { {||   RGB(   0,   0,   0 ) } } )                            //  Text color of inactive cursor in grid

      :nClrLine := COLOR_GRID

      // modify the default settings
      :aColumns[ 1 ]:cHeading  := "Id"
      :aColumns[ 1 ]:nAlign    := DT_CENTER
      :SetColSize( 1, 60 )
      :SetColSize( 2, 300 )
      :aColumns[ 3 ]:nAlign    := DT_RIGHT
      :aColumns[ 3 ]:cPicture  := Replicate("9", 11)
      // editing
      FOR i := 2 TO country->( Fcount() )
         :aColumns[ i ]:lEdit  := TRUE
         :aColumns[ i ]:bPostEdit := { |Val, Brw| sqlupdate(Val, Brw) }
      NEXT

      :aColumns[ 4 ]:nAlign    := DT_CENTER
      :aColumns[ 4 ]:nEditMove := DT_DONT_MOVE
      :SetColSize( 4, 125 )

      :ResetVScroll()

   END TBROWSE

RETURN NIL

*--------------------------------------------------------*
PROCEDURE OpenTable
*--------------------------------------------------------*

   IF IsExistTable( "country" ) .OR. CreateTable()

      dbUseArea( .T.,, "SELECT * FROM country", "country" )

      INDEX ON FIELD->CODE TAG CODE TO country

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
         NAME WITH cName, ;
         Updated WITH Date(), ;
         RESIDENTS WITH nResidents

      oBrw_1:GoToRec(country->( RecNo()), .T.)
      oBrw_1:Refresh(.F.)

   ELSE

      MsgStop( "Can't append record to table Country!", "Error" )

   ENDIF

RETURN

*--------------------------------------------------------*
FUNCTION sqlupdate( uVal, oBrw )
*--------------------------------------------------------*
   LOCAL cCode, cField, cNewValue
   LOCAL nColumn, oCol

   WITH OBJECT oBrw
      nColumn := :nCell
      oCol := :aColumns[ nColumn ]
      cField := oCol:cName
   END WITH

   IF !( uVal == oCol:Cargo )

      IF nColumn == 2

         cNewValue := "'" + uVal + "'"

      ELSEIF nColumn == 3

         cNewValue := hb_ntos( uVal )

      ELSEIF nColumn == 4

         cNewValue := "'" + dtoc( uVal ) + "'"

      ENDIF

      cCode := "'" + Eval( oBrw:aColumns[ 1 ]:bData ) + "'"

      IF ! rddInfo( RDDI_EXECUTE, "UPDATE country SET " + cField + " = " + cNewValue + " WHERE CODE = " + cCode )

         MsgStop( "Can't update record in the table " + oBrw:cAlias + "!", "Error" )
         RETURN .F.

      ENDIF

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
