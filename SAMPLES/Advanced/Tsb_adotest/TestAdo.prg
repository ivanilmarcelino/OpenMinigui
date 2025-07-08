/*
 *
 * Testado.prg
 *
 * Test program sample for ADO Browse.
 *
 * Copyright 2020 Itamar M. Lins Jr. Junior and Jose Quintas
 *
 */

#include "minigui.ch"
#include "tsbrowse.ch"

STATIC Font_1, Font_2

FUNCTION Main()

   DEFINE FONT Font_1 FONTNAME "Courier New" SIZE 12
   DEFINE FONT Font_2 FONTNAME "Courier New" SIZE 12

   DEFINE WINDOW MainWindow ;
         AT 200, 0 CLIENTAREA 400, 150 ;
         TITLE "ADO Example" ;
         MAIN

   END WINDOW

   DEFINE MAIN MENU OF MainWindow
      DEFINE POPUP "&Tests"
         MENUITEM "&Browse ADO" ACTION DlgADO()
         MENUITEM "&Browse DBF" ACTION DlgDBF()
         SEPARATOR
         MENUITEM "&Exit" ACTION MainWindow.Release()
      END POPUP
   END MENU

   CENTER WINDOW MainWindow
   ACTIVATE WINDOW MainWindow

RETURN NIL


FUNCTION DlgADO()

   LOCAL oBrw, cnSQL

   cnSQL := RecordsetADO()

   SET INTERACTIVECLOSE OFF

   DEFINE WINDOW ModDlg ;
         AT 0, 0 CLIENTAREA 730, 600 ;
         TITLE "ADO BROWSE" ;
         MODAL ;
         ON RELEASE iif( HB_ISOBJECT( cnSQL ), cnSQL:Close(), )

      @ 20, 10 TBROWSE oBrw RECORDSET cnSQL OF ModDlg AUTOCOLS WIDTH 710 HEIGHT 500 FONT "Font_1" ;
         HEADERS "Name", "Adress" ;
         SIZES 300, 300

      @ 540, 540 BUTTONEX BtnEnd CAPTION "Close" ON CLICK {|| ModDlg.Release() } WIDTH 180 HEIGHT 36 FLAT FONT "Font_1"

      oBrw:nHeightCell := ( oBrw:nHeight / 18 )
      oBrw:nHeightHead := ( oBrw:nHeight / 18 )
      oBrw:lNoHScroll := .T.
      oBrw:lNoResetPos := .T.
      oBrw:nLineStyle := 0
      oBrw:AdjColumns()

      ON KEY F2 ACTION ;
         Msginfo( "Records:  " + hb_ntos( oBrw:nLen ) + hb_eol() + ;
         "Total:    " + hb_ntos( oBrw:oRSet:RecordCount() ) + hb_eol() + ;
         "Recno:    " + hb_ntos( Int( Eval( oBrw:bRecNo ) ) ) + hb_eol() + ;
         "AbsPos:   " + hb_ntos( oBrw:oRSet:AbsolutePosition ) )

   END WINDOW

   CENTER WINDOW ModDlg
   ACTIVATE WINDOW ModDlg

   SET INTERACTIVECLOSE ON

RETURN NIL

// --- Recordset ADO ---

#define AD_VARCHAR     200

FUNCTION RecordsetADO()

   LOCAL nCont, cChar := "A"
   LOCAL cnSQL := CreateObject( "ADODB.Recordset" )

   WITH OBJECT cnSQL
      :Fields:Append( "NAME", AD_VARCHAR, 30 )
      :Fields:Append( "ADRESS", AD_VARCHAR, 30 )
      :Open()
      FOR nCont = 1 TO 1000
         :AddNew()
         :Fields( "NAME" ):Value := "ADO_NAME_" + Replicate( cChar, 10 ) + Str( nCont, 8 )
         :Fields( "ADRESS" ):Value := "ADO_ANDRESS_" + Replicate( cChar, 10 ) + Str( nCont, 8 )
         :Update()
         cChar := iif( cChar == "Z", "A", Chr( Asc( cChar ) + 1 ) )
      NEXT
      :MoveFirst()
   ENDWITH

RETURN cnSQL


FUNCTION DlgDBF()

   LOCAL oBrw

   CreateDBF( "test" )
   USE test

   DEFINE WINDOW ModDlg ;
         AT 0, 0 CLIENTAREA 730, 600 ;
         TITLE "DBF BROWSE" ;
         MODAL ;
         ON RELEASE ( dbCloseArea() )

      @ 20, 10 TBROWSE oBrw OF ModDlg WIDTH 710 HEIGHT 500 FONT "Font_2"

      @ 540, 540 BUTTONEX BtnEnd CAPTION "Close" ON CLICK {|| ModDlg.Release() } WIDTH 180 HEIGHT 36 FLAT FONT "Font_2"

      ADD COLUMN TO oBrw HEADER 'Name' ;
         SIZE 300 ;
         DATA FieldWBlock( "NAME", Select() ) ;
         ALIGN DT_LEFT, nMakeLong( DT_CENTER, DT_CENTER )

      ADD COLUMN TO oBrw HEADER 'Adress' ;
         SIZE 300 ;
         DATA FieldWBlock( "ADRESS", Select() ) ;
         ALIGN DT_LEFT, nMakeLong( DT_CENTER, DT_CENTER )

      oBrw:nHeightCell := ( oBrw:nHeight / 18 )
      oBrw:nHeightHead := ( oBrw:nHeight / 18 )
      oBrw:lNoHScroll := .T.
      oBrw:lNoResetPos := .T.
      oBrw:nLineStyle := 0
      oBrw:AdjColumns()

      ON KEY F2 ACTION ;
         Msginfo( "Records:  " + hb_ntos( oBrw:nLen ) + hb_eol() + ;
         "Total:    " + hb_ntos( ( oBrw:cAlias )->( LastRec() ) ) + hb_eol() + ;
         "Recno:    " + hb_ntos( ( oBrw:cAlias )->( RecNo() ) ) + hb_eol() + ;
         "AbsPos:   " + hb_ntos( oBrw:nRowPos() ) )

   END WINDOW

   CENTER WINDOW ModDlg
   ACTIVATE WINDOW ModDlg

RETURN NIL

// --- DBF ---

FUNCTION CreateDbf( cName )

   LOCAL nCont, cChar := "A"

   IF hb_vfExists( cName + ".dbf" )
      RETURN NIL
   ENDIF

   dbCreate( cName, { ;
      { "NAME", "C", 30, 0 }, ;
      { "ADRESS", "C", 30, 0 } } )

   USE ( cName ) ALIAS base

   FOR nCont = 1 TO 1000
      APPEND BLANK
      REPLACE base->NAME WITH "DBF_NAME_" + Replicate( cChar, 10 ) + Str( nCont, 8 ), ;
         base->ADRESS WITH "DBF_ADRESS_" + Replicate( cChar, 10 ) + Str( nCont, 8 )
      cChar := iif( cChar == "Z", "A", Chr( Asc( cChar ) + 1 ) )
   NEXT
   USE

RETURN NIL

// ==================== EOF of Testado.prg =======================
