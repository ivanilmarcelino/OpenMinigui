/*

  MySqlCli - MySQLClient written in Harbour/HMG

       (c) 2004 Cristobal Molla <cemese@terra.es>
       (c) 2005 Mitja Podgornik <yamamoto@rocketmail.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  This application use code from:
  -    Harbour (c) The Harbour project team (https://harbour.github.io)
  -    HMG Harbour Minigui (c) Roberto Lopez (https://sourceforge.net/projects/harbourminigui)

  March 2005: version 2.0 by Mitja Podgornik

   ! rewritten in english
   ! optimized for new Harbour tmysql changes
   + added export/import to/from Dbf
   + dynamic grid fill (only first 1000 records in old version)
   + cosmetic modifications

  November 2023: add port option by Pierpaolo Martinello

*/

#include "hmg.ch"

#define PROGRAM "MySqlCli"
#define BSL chr(92)

#xtranslate nTrim( <nValue> ) => hb_ntos( <nValue> )

MEMVAR lConnected, oServer, cServer, nPort, cUser, cPaswd, cDBase, cCryptKey
MEMVAR aDatabases, aTables, lTableSelect, lBaseSelect, cBase, cTable
MEMVAR nScope, nFrom

SET PROCEDURE TO GridQuery

*------------------------------------------------------------------------------*
PROCEDURE Main()
*------------------------------------------------------------------------------*

   // Initialize settings
   InitConfig()

   DEFINE WINDOW F_Main ;
         AT 0, 0 ;
         WIDTH 800 ;
         HEIGHT 600 ;
         TITLE PROGRAM ;
         ICON "A_ICO_MAIN" ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE ;
         ON INIT {|| UpdateMain() } ;
         ON RELEASE Disconnect()

      DEFINE MAIN MENU

         DEFINE POPUP " &File "
            MENUITEM "  &Connect  " ;
               ACTION {|| Connect() } ;
               NAME mnuConnect
            MENUITEM "  &Disconnect  " ;
               ACTION {|| Disconnect() } ;
               NAME mnuDisconnect
            SEPARATOR
            MENUITEM "  &Export to DBF  " ;
               ACTION {|| SQL2Dbf() } ;
               NAME mnusql2dbf
            MENUITEM "  &Import from DBF  " ;
               ACTION {|| Dbf2Sql( .T. ) } ;
               NAME mnudbf2sql
            SEPARATOR
            MENUITEM "  &Exit  " ;
               ACTION F_Main.RELEASE ;
               NAME mnuEnd
         END POPUP

         DEFINE POPUP " &Help "
            MENUITEM "About" ;
               ACTION ShellAbout( "", PROGRAM + " v.2.0 (client version " + mysql_get_client_info() + ")" + CRLF + "Cristobal Molla, Mitja Podgornik" )
         END POPUP

      END MENU

      DEFINE SPLITBOX
         DEFINE TOOLBAR Tbar_1 ;
               BUTTONSIZE 40, 32 ;
               FLAT ;
               RIGHTTEXT

            BUTTON tbbConnect ;
               CAPTION "Connect" ;
               PICTURE "BMP_32_CONNECT" ;
               ACTION {|| Connect() }

            BUTTON tbbDisconnect ;
               CAPTION "Disconnect" ;
               PICTURE "BMP_32_DISCONNECT" ;
               ACTION {|| Disconnect() }

            BUTTON tbbsql2dbf ;
               CAPTION "Export to DBF" ;
               PICTURE "BMP_32_BASE" ;
               ACTION {|| SQL2Dbf() }

            BUTTON tbbdbf2sql ;
               CAPTION "Import from DBF" ;
               PICTURE "BMP_32_TABLE" ;
               ACTION {|| Dbf2Sql( .T. ) }

         END TOOLBAR
      END SPLITBOX

      DEFINE STATUSBAR
         STATUSITEM ""
         STATUSITEM "" WIDTH 200
         STATUSITEM "" ICON "ICO_LEDOFF" WIDTH 035
      END STATUSBAR

      DEFINE TREE TREE_1 ;
            AT 60, 10 ;
            WIDTH 200 ;
            HEIGHT 460 ;
            ON CHANGE {|| TreeChange() } ;
            NODEIMAGES { "BMP_16_SERVER" } ;
            ITEMIMAGES { "BMP_16_ITEMOFF", "BMP_16_ITEMON" } ;
            ITEMIDS

      END TREE

      DEFINE TAB TABInfo ;
            AT 60, 220 ;
            WIDTH 565 ;
            HEIGHT 460 ;
            VALUE 1

         PAGE "&Structure"

            @ 30, 15 FRAME frmTable ;
               CAPTION "Structure" ;
               WIDTH 535 ;
               HEIGHT 415 ;
               BOLD

            @ 50, 30 GRID GRD_Table ;
               WIDTH 505 ;
               HEIGHT 380 ;
               HEADERS { "Name", "Type", ;
               "Null", "Key", ;
               "Default value", ;
               "Extra" } ;
               WIDTHS { 125, 125, 50, 50, 125, 150 } ;
               ON GOTFOCUS {|| setControl( .T. ) } ;
               ON LOSTFOCUS {|| setControl( .F. ) }

         END PAGE

         PAGE "&Data"

            @ 30, 15 FRAME frmData ;
               CAPTION "Data" ;
               WIDTH 535 ;
               HEIGHT 415 ;
               BOLD

            @ 50, 30 GRID GRD_Data ;
               WIDTH 505 ;
               HEIGHT 380 ;
               HEADERS { "" } ;
               WIDTHS { 100 } ;
               ITEMS { { "" } } ;
               VALUE 1 ;
               ON GOTFOCUS {|| setControl( .T. ) } ;
               ON CHANGE {|| AddData( F_Main.GRD_Data.Value ), setMessage( "Record: " + ntrim( F_Main.GRD_Data.Value ), 2 ) } ;
               ON LOSTFOCUS {|| setControl( .F. ) } ;
               COLUMNSORT {}

         END PAGE

      END TAB

      @ 64, 340 LABEL NUL ;
         VALUE "F2 - Export Structure or Data to CSV   F7 - Make a data query" ;
         WIDTH 340 HEIGHT 21 FONTCOLOR NAVY CENTERALIGN

      ON KEY F2 ACTION iif( GetControlType( this.FocusedControl, thiswindow.name ) == "GRID", ExportGridToCSV( thiswindow.name, this.FocusedControl ), )
      ON KEY F7 Action iif( GetControlType( this.FocusedControl, thiswindow.name ) == "GRID", GridQuery( thiswindow.name, this.FocusedControl, DoMethod ( thiswindow.name, this.FocusedControl, "GetArray" ) ), )

   END WINDOW

   setMessage()
   F_main.tabInfo.Enabled := .F.

   CENTER WINDOW F_Main
   ACTIVATE WINDOW F_Main

RETURN


*------------------------------------------------------------------------------*
PROCEDURE InitConfig()
*------------------------------------------------------------------------------*
   PUBLIC lConnected := .F.
   PUBLIC oServer := NIL
   PUBLIC cServer := "localhost"
   PUBLIC nPort := 3306
   PUBLIC cUser := "root"
   PUBLIC cPaswd := ""
   PUBLIC cDBase := "test"
   PUBLIC cCryptKey := GetEnv( "MY_CRYPT_KEY" )

   IF Empty( cCryptKey )
      cCryptKey := "topsecret" // Replace with dynamic retrieval if needed
   ENDIF

   PUBLIC aDatabases := {}
   PUBLIC aTables := {}
   PUBLIC lTableSelect := .F.
   PUBLIC lBaseSelect := .F.
   PUBLIC cBase := ""
   PUBLIC cTable := ""

   PUBLIC nScope := 500 // max records for select scope (limit)
   PUBLIC nFrom := 0    // starting pointer for select scope - Don’t touch!

   SET DATE GERMAN
   SET CENTURY ON
   SET MULTIPLE OFF WARNING
   SET CODEPAGE TO UNICODE

   IF File("MySqlCli.ini")
      BEGIN INI FILE "MySqlCli.ini"
         GET cServer SECTION "Login" ENTRY "server"
         GET nPort SECTION "Login" ENTRY "port"
         GET cUser SECTION "Login" ENTRY "user"
         GET cPaswd SECTION "Login" ENTRY "pass"
         GET cDBase SECTION "Login" ENTRY "database"
      END INI
      cPaswd := hb_decrypt(cPaswd, cCryptKey)
   ELSE
      BEGIN INI FILE "MySqlCli.ini"
         SET SECTION "Login" ENTRY "server" TO cServer
         SET SECTION "Login" ENTRY "port" TO nPort
         SET SECTION "Login" ENTRY "user" TO cUser
         SET SECTION "Login" ENTRY "pass" TO hb_crypt(cPaswd, cCryptKey)
         SET SECTION "Login" ENTRY "database" TO cDBase
      END INI
   ENDIF

RETURN


*------------------------------------------------------------------------------*
FUNCTION Connect()
*------------------------------------------------------------------------------*
   LOCAL nRow, nCol
   LOCAL ndb := 0 //30

   IF lConnected
      RETURN NIL
   ENDIF

   nRow := GetProperty( "F_Main", "Row" ) + 200
   nCol := GetProperty( "F_Main", "Col" ) + 230

   DEFINE WINDOW F_Connect ;
         AT nRow, nCol ;
         WIDTH 315 ;
         HEIGHT 265 - ndb ;
         TITLE " Connect to MySQL server" ;
         MODAL ;
         NOSIZE ;
         nosysmenu

      ON KEY ESCAPE ACTION f_connect.RELEASE

      @ 10, 10 FRAME frm_Data ;
         CAPTION "" ;
         WIDTH 285 ;
         HEIGHT 180 - Ndb ;
         BOLD

      @ 34, 20 LABEL lblServer ;
         VALUE "Server" ;
         WIDTH 50 ;
         HEIGHT 21

      @ 64, 20 LABEL lport ;
         VALUE "Port" ;
         WIDTH 55 ;
         HEIGHT 21

      @ 94, 20 LABEL lblUser ;
         VALUE "User" ;
         WIDTH 50 ;
         HEIGHT 21

      @ 124, 20 LABEL lblPaswd ;
         VALUE "Password" ;
         WIDTH 55 ;
         HEIGHT 21

      IF ndb < 1
         @ 154, 20 LABEL lblDBase ;
            VALUE "Database" ;
            WIDTH 55 ;
            HEIGHT 21
      ENDIF

      @ 30, 85 TEXTBOX txtServer ;
         HEIGHT 21 ;
         VALUE cServer ;
         WIDTH 200 ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) } ;
         MAXLENGTH 60

      @ 60, 85 TEXTBOX txtPort ;
         HEIGHT 21 ;
         VALUE nPort ;
         WIDTH 100 ;
         NUMERIC ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) } ;
         ON ENTER F_Connect.btnConnect.ACTION ;
         MAXLENGTH 5

      @ 90, 85 TEXTBOX txtUser ;
         HEIGHT 21 ;
         VALUE cUser ;
         WIDTH 100 ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) } ;
         MAXLENGTH 40

      @ 120, 85 TEXTBOX txtPaswd ;
         HEIGHT 21 ;
         VALUE cPaswd ;
         WIDTH 100 ;
         PASSWORD ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) } ;
         MAXLENGTH 40

      IF ndb < 1
         @ 150, 85 TEXTBOX txtDBase ;
            HEIGHT 21 ;
            VALUE cDBase ;
            WIDTH 100 ;
            ON GOTFOCUS {|| setControl( .T. ) } ;
            ON LOSTFOCUS {|| setControl( .F. ) } ;
            ON ENTER F_Connect.btnConnect.ACTION ;
            MAXLENGTH 40
      ENDIF

      @ 200 - ndb, 145 BUTTON btnAbort ;
         CAPTION "&Abort" ;
         ACTION {|| f_connect.release } ;
         WIDTH 70 ;
         HEIGHT 25

      @ 200 - ndb, 225 BUTTON btnConnect ;
         CAPTION "&Connect" ;
         ACTION {|| Login() } ;
         WIDTH 70 ;
         HEIGHT 25

   END WINDOW

   ACTIVATE WINDOW F_Connect

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION Login()
*------------------------------------------------------------------------------*
   cServer := AllTrim( F_Connect.txtServer.Value )
   cUser := AllTrim( F_Connect.txtUser.Value )
   cPaswd := AllTrim( F_Connect.txtPaswd.Value )
   cDBase := AllTrim( F_Connect.txtDBase.Value )
   nPort := F_Connect.txtPort.VALUE

   IF ! Empty( cServer ) .AND. ! Empty( cUser )
      setMessage( "Connecting to MySQL server...", 1 )
      oServer := TMySQLServer():New( cServer, cUser, cPaswd, nport )
      IF !( oServer:NetErr() )
         lConnected := .T.
         F_main.tabInfo.Enabled := .T.
         UpdateTree()
         IF ! Empty( cDBase )
            oServer:SelectDb( cDBase )
            IF oServer:NetErr()
               MsgExclamation( "Error connecting to database: " + oServer:Error() )
            ENDIF
         ENDIF
      ENDIF
      setMessage()
      UpdateMain()
      BEGIN INI FILE "MySqlCli.ini"
         SET SECTION "Login" ENTRY "server" TO cServer
         SET SECTION "Login" ENTRY "port" TO nPort
         SET SECTION "Login" ENTRY "user" TO cUser
         SET SECTION "Login" ENTRY "pass" TO hb_crypt( cPaswd, cCryptKey )
         SET SECTION "Login" ENTRY "database" TO cDBase
      END INI
   ENDIF
   F_connect.RELEASE

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION Disconnect()
*------------------------------------------------------------------------------*
   IF lConnected
      oServer:Destroy()
      lConnected := .F.
      F_main.tabinfo.VALUE := 1
      UpdateTree()
      UpdateMain()
      setMessage()
   ENDIF
   F_main.tabInfo.Enabled := .F.
   lBaseSelect := .F.
   lTableSelect := .F.
   F_Main.GRD_Table.DeleteAllItems
   F_Main.GRD_Data.DeleteAllItems

RETURN NIL


*------------------------------------------------------------------------------*
PROCEDURE UpdateTree()
*------------------------------------------------------------------------------*
/*
     Node number format: SBBTT
     where:
     S     node Server         1 - 9
     BB    node DataBases     01 - 99
     TT    item Tables        01 - 999
*/

   LOCAL i
   LOCAL j
   LOCAL nNodeBase
   LOCAL nNodeTable
   LOCAL nNodeSelect

   IF lConnected

      F_Main.TREE_1.DeleteAllItems
      F_Main.TREE_1.AddItem( oServer:cUser + "@" + cServer, 0, 1 )

      aDatabases := oServer:ListDBs()
      IF oServer:NetErr()
         MsgExclamation( "Error verifying database list: " + oServer:Error() )
         RETURN
      ENDIF

      FOR i := 1 TO Len( aDatabases )
         nNodeBase := Val( "1" + PadL( i, 2, "0" ) )
         F_Main.TREE_1.AddItem( aDatabases[ i ], 1, nNodeBase )

         oServer:SelectDb( aDatabases[ i ] )
         IF oServer:NetErr()
            MsgExclamation( "Error connecting to database: " + oServer:Error() )
            RETURN
         ENDIF
         IF ! Empty( cDBase )
            IF Lower( cDBase ) == aDatabases[ i ]
               nNodeSelect := nNodeBase
            ENDIF
         ENDIF

         aTables := oServer:ListTables()
         IF oServer:NetErr()
            MsgExclamation( "Error verifying TABles list: " + oServer:Error() )
            RETURN
         ENDIF

         FOR j := 1 TO Len( aTables )
            nNodeTable := Val( nTrim( nNodeBase ) + PadL( j, 3, "0" ) )
            F_Main.TREE_1.AddItem( aTables[ j ], nNodeBase, nNodeTable )
         NEXT

      NEXT

      F_Main.TREE_1.Expand( 1 )
      IF nNodeSelect <> NIL
         F_Main.TREE_1.VALUE := nNodeSelect
         F_Main.TREE_1.Expand( nNodeSelect )
      ENDIF
   ELSE
      DoMethod( "F_Main", "TREE_1", "DeleteAllItems" )
   ENDIF

   cBase := ""
   cTable := ""
   lBaseSelect := .F.
   lTableSelect := .F.

RETURN


*------------------------------------------------------------------------------*
PROCEDURE UpdateTable()
*------------------------------------------------------------------------------*
   LOCAL oQuery AS OBJECT
   LOCAL i
   LOCAL aFile

   F_Main.GRD_Table.DeleteAllItems

   IF lConnected
      setMessage( "SQL Query: DESCRIBE " + Upper( cTable ) + "...", 1 )
      oQuery := oServer:Query( "describe " + cTable )
      IF !( oQuery:NetErr() )
         oQuery:Gotop()
         F_Main.GRD_Data.DisableUpdate
         FOR i := 1 TO oQuery:LastRec()
            oQuery:GetRow( i )
            aFile := oQuery:aRow
            IF ! Empty( aFile[ 1 ] )
               F_Main.GRD_Table.addItem( aFile )
            ENDIF
         NEXT
         F_Main.GRD_Data.EnableUpdate
      ENDIF
      oQuery:Destroy()
      setMessage()
   ENDIF

RETURN


*------------------------------------------------------------------------------*
PROCEDURE UpdateData()
*------------------------------------------------------------------------------*
   LOCAL oQuery AS OBJECT
   LOCAL i AS NUMERIC
   LOCAL nColumns
   LOCAL aFields
   LOCAL aWidths
   LOCAL aTypes
   LOCAL aLine

   STATIC lBusy := .F.

   IF lBusy
      DO EVENTS
      RETURN
   ELSE
      lBusy := .T.

      setMessage( "SQL Query: SELECT * FROM " + Upper( cTable ), 1 )
      oQuery := oServer:Query( "select * from " + cTable + " limit " + ntrim( nFrom ) + "," + ntrim( nScope ) )
      IF !( oQuery:NetErr() )
         nColumns := oQuery:FCount()
         aFields := Array( nColumns )
         aWidths := Array( nColumns )
         aTypes := Array( nColumns )
         FOR i := 1 TO nColumns
            aFields[ i ] := oQuery:FieldName( i )
            aTypes[ i ] := oQuery:FieldType( i )
            aWidths[ i ] := iif( oQuery:FieldLen( i ) > Len( oQuery:FieldName( i ) ), ;
               oQuery:FieldLen( i ) * 12, Len( oQuery:FieldName( i ) ) * 12 )
            aWidths[ i ] := Min( 250, aWidths[ i ] )
         NEXT
      ELSE
         RETURN
      ENDIF

      // Old columns cleaning
      nColumns := GetProperty( "F_Main", "GRD_Data", "ColumnCount" )
      DO WHILE nColumns != 0
         F_Main.GRD_Data.DeleteColumn( nColumns )
         nColumns--
      ENDDO

      // Add columns from query result
      FOR i := 1 TO oQuery:FCount()
         F_Main.GRD_Data.AddColumn( i, aFields[ i ], aWidths[ i ], iif( aTypes[ i ] == "C", 0, 1 ) )
      NEXT

      nColumns := GetProperty( "F_Main", "GRD_Data", "ColumnCount" )
      i := GetProperty( "F_Main", "GRD_Data", "Index" )
      _HMG_aControlHeadClick[ i ] := Array( nColumns )
      _HMG_aControlRangeMax[ i ] := Array( nColumns )

      // Add column's sorting
      AEval( Array( nColumns ), ;
         {| x, k | _HMG_aControlHeadClick[ i ][ k ] := {| n | HMG_SortColumn( n ) }, ;
         _HMG_aControlRangeMax[ i ][ k ] := 1, HB_SYMBOL_UNUSED( x ) } )

      oQuery:GoTop()

      F_Main.GRD_Data.DisableUpdate

      FOR i := 1 TO oQuery:LastRec()
         IF ( i % 100 ) == 0
            DO EVENTS
            setmessage( "Record: " + ntrim( i ), 2 )
         ENDIF

         oQuery:GetRow( i )

         aLine := oQuery:aRow
         IF ! Empty( aLine[ 1 ] )
            F_Main.GRD_Data.addItem( ItemChar( aLine, aTypes ) )
         ENDIF
      NEXT

      F_Main.GRD_Data.EnableUpdate

      oQuery:Destroy()
      setMessage()

      lBusy := .F.
   ENDIF

RETURN


*------------------------------------------------------------------------------*
FUNCTION ItemChar( aLine, aType )
*------------------------------------------------------------------------------*
   LOCAL aRet, i, l := Len( aLine )

   aRet := Array( l )
   FOR i := 1 TO l
      DO CASE
      CASE aType[ i ] == "N"
         aRet[ i ] := ntrim( aLine[ i ] )
      CASE aType[ i ] == "D"
         aRet[ i ] := DToC( aLine[ i ] )
      CASE aType[ i ] == "L"
         aRet[ i ] := iif( iif( ISLOGICAL( aLine[ i ] ), aLine[ i ], Asc( Left( aLine[ i ], 1 ) ) != 0 ), ".T.", ".F." )
      OTHERWISE
         aRet[ i ] := aLine[ i ]
      ENDCASE
   NEXT

RETURN aRet


*------------------------------------------------------------------------------*
PROCEDURE UpdateMain()
*------------------------------------------------------------------------------*
   IF lConnected
      SetProperty( "F_Main", "TREE_1", "Enabled", .T. )
      SetProperty( "F_Main", "tabInfo", "Enabled", .T. )
      SetProperty( "F_Main", "tbbConnect", "Enabled", .F. )
      SetProperty( "F_Main", "tbbDisconnect", "Enabled", .T. )
      SetProperty( "F_Main", "tbbSQL2Dbf", "Enabled", .T. )
      SetProperty( "F_Main", "tbbDbf2SQL", "Enabled", .T. )
      SetProperty( "F_Main", "StatusBar", "Icon", 3, "ICO_LEDON" )
      SetProperty( "F_Main", "mnuConnect", "Enabled", .F. )
      SetProperty( "F_Main", "mnuDisconnect", "Enabled", .T. )
      SetProperty( "F_Main", "mnusql2dbf", "Enabled", .T. )
      SetProperty( "F_Main", "mnudbf2sql", "Enabled", .T. )
   ELSE
      SetProperty( "F_Main", "TREE_1", "Enabled", .F. )
      SetProperty( "F_Main", "tabInfo", "Enabled", .F. )
      SetProperty( "F_Main", "tbbConnect", "Enabled", .T. )
      SetProperty( "F_Main", "tbbDisconnect", "Enabled", .F. )
      SetProperty( "F_Main", "tbbSQL2Dbf", "Enabled", .F. )
      SetProperty( "F_Main", "tbbDbf2Sql", "Enabled", .F. )
      SetProperty( "F_Main", "StatusBar", "Icon", 3, "ICO_LEDOFF" )
      SetProperty( "F_Main", "mnuConnect", "Enabled", .T. )
      SetProperty( "F_Main", "mnuDisconnect", "Enabled", .F. )
      SetProperty( "F_Main", "mnusql2dbf", "Enabled", .F. )
      SetProperty( "F_Main", "mnudbf2sql", "Enabled", .F. )
   ENDIF

RETURN


*------------------------------------------------------------------------------*
PROCEDURE TreeChange()
*------------------------------------------------------------------------------*

   LOCAL aRecords
   LOCAL nItem
   LOCAL oQuery AS OBJECT

   lTableSelect := .F.
   lBaseSelect := .F.
   nItem := GetProperty( "F_Main", "TREE_1", "Value" )

   DO CASE
   CASE nItem >= 1 .AND. nItem <= 9
      setMessage( "Databases: " + nTrim( Len( aDatabases ) ), 1 )

   CASE nItem >= 100 .AND. nItem <= 999
      cBase := GetProperty( "F_Main", "TREE_1", "Item", nItem )
      oServer:SelectDb( cBase )
      aTables := oServer:ListTables()
      setMessage( "Tables in database " + Upper( cBase ) + ": " + nTrim( Len( aTables ) ), 1 )
      lBaseSelect := .T.

   CASE nItem >= 10000 .AND. nItem <= 999999
      cTable := GetProperty( "F_Main", "TREE_1", "Item", nItem )
      nItem := Val( SubStr( nTrim( nItem ), 1, 3 ) )
      cBase := GetProperty( "F_Main", "TREE_1", "Item", nItem )
      oServer:SelectDB( cBase )
      aTables := oServer:ListTables()
      lBaseSelect := .T.
      lTableSelect := .T.

      nFrom := 0

      UpdateTable()
      UpdateData()

      oQuery := oServer:Query( "select count(*) from " + cTable )
      IF !( oQuery:NetErr() )
         oQuery:Gotop()
         aRecords := oQuery:aRow()
         setMessage( "Records in table " + Upper( cTable ) + ": " + nTrim( aRecords[ 1 ] ), 1 )
         oQuery:Destroy()
      ELSE
         RETURN
      ENDIF
   ENDCASE

RETURN


*------------------------------------------------------------------------------*
FUNCTION setControl( lValue )
*------------------------------------------------------------------------------*
   IF hb_defaultValue( lValue, .F. )
      SetProperty( thisWindow.NAME, this.NAME, "BackColor", { 255, 255, 200 } )
   ELSE
      SetProperty( thisWindow.NAME, this.NAME, "BackColor", { 255, 255, 255 } )
   ENDIF

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION setMessage( cMessage, nItem )
*------------------------------------------------------------------------------*
   IF cMessage == NIL
      SetProperty( "F_Main", "StatusBar", "Item", 1, " " )
      SetProperty( "F_Main", "StatusBar", "Item", 2, " " )
   ELSE
      SetProperty( "F_Main", "StatusBar", "Item", nItem, " " + cMessage )
   ENDIF

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION SQL2Dbf()
*------------------------------------------------------------------------------*
   LOCAL nRow
   LOCAL nCol
   LOCAL cFileName := cTable

   IF ! lTableSelect
      msginfo( "No Table selected...", PROGRAM )
      RETURN NIL
   ENDIF

   nRow := GetProperty( "F_Main", "Row" ) + 200
   nCol := GetProperty( "F_Main", "Col" ) + 230

   DEFINE WINDOW F_2Dbf ;
         AT nRow, nCol ;
         WIDTH 360 ;
         HEIGHT 220 ;
         TITLE " Export to DBF" ;
         MODAL ;
         NOSIZE

      @ 10, 10 FRAME frm_2Dbf ;
         CAPTION "" ;
         WIDTH 335 ;
         HEIGHT 100 ;
         BOLD

      @ 34, 20 LABEL lblFile ;
         VALUE "Table name:" ;
         WIDTH 70 ;
         HEIGHT 21

      @ 30, 100 TEXTBOX txtFile ;
         HEIGHT 21 ;
         VALUE cFileName ;
         WIDTH 200 ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) } ;
         MAXLENGTH 60

      @ 64, 20 LABEL lblMap ;
         VALUE "Folder:" ;
         WIDTH 70 ;
         HEIGHT 21

      @ 60, 100 TEXTBOX txtMap ;
         HEIGHT 21 ;
         VALUE BSL + Lower( CurDir() ) + BSL ;
         WIDTH 200 ;
         ON GOTFOCUS {|| setControl( .T. ) } ;
         ON LOSTFOCUS {|| setControl( .F. ) }

      @ 60, 305 BUTTON btnFolder ;
         CAPTION "..." ;
         ACTION {|| ( F_2Dbf.txtMap.VALUE := GetFolder( "Select folder:" ) ), F_2Dbf.txtMap.SetFocus } ;
         WIDTH 30 ;
         HEIGHT 21

      @ 120, 275 BUTTON btn2Dbf ;
         CAPTION "&Export" ;
         ACTION {|| copy2dbf( F_2Dbf.txtMap.VALUE, F_2Dbf.txtFile.Value ), F_2Dbf.Release } ;
         WIDTH 70 ;
         HEIGHT 25

      @ 155, 275 BUTTON btn2DbfCancel ;
         CAPTION "&Cancel" ;
         ACTION {|| F_2Dbf.Release } ;
         WIDTH 70 ;
         HEIGHT 25

   END WINDOW

   ACTIVATE WINDOW F_2Dbf

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION copy2dbf( cFolder, cFile )
*------------------------------------------------------------------------------*
   LOCAL cMap := AllTrim( cFolder )
   LOCAL cFilename := AllTrim( cFile )

   LOCAL oQuery, oRow AS OBJECT
   LOCAL i, j AS NUMERIC
   LOCAL aStruct
   LOCAL aOKType
   LOCAL cField

   IF lConnected

      IF Right( cMap, 1 ) != BSL .AND. ! Empty( cMap )
         cMap += BSL
      ENDIF
      cFileName := cMap + cFileName
      IF !( "." $ cFileName )
         cFileName := cFileName + ".dbf"
      ENDIF

      IF File( cFileName )
         IF ! msgyesno( "File " + cFileName + " already exists! Overwrite?", PROGRAM )
            RETURN NIL
         ENDIF
      ENDIF

      IF ( i := FCreate( cFileName ) ) > 0
         FClose( i )
      ELSE
         msgexclamation( "Incorrect Table name...", PROGRAM )
         RETURN NIL
      ENDIF

      setMessage( "Exporting Table " + cTable + " in " + cFileName + "...", 1 )
      oQuery := oServer:Query( "select * from " + cTable )
      IF !( oQuery:NetErr() )

         aStruct := Array( oQuery:FCount() )
         aOKType := Array( oQuery:FCount() )

         FOR i := 1 TO oQuery:FCount()
            IF ( oQuery:FieldType( i ) ) $ "CNDLM"
               aOKType[ i ] := .T.
               aStruct[ i ] := { oQuery:FieldName( i ), ;
                  oQuery:FieldType( i ), ;
                  oQuery:FieldLen( i ), ;
                  oQuery:FieldDec( i, .T. ) }
               IF aStruct[ i ][ 4 ] > 0
                  aStruct[ i ][ 3 ] += 1
               ENDIF
            ELSE
               aOKType[ i ] := .F.
               aStruct[ i ] := { oQuery:FieldName( i ), "C", 1, 0 }
            ENDIF
         NEXT

         dbCreate( cFileName, aStruct )

         USE ( cFileName ) ALIAS dbf_ex NEW

         oQuery:GoTop()
         FOR i := 1 TO oQuery:LastRec()
            IF ( i % 100 ) == 0
               DO EVENTS
               setmessage( "Record: " + ntrim( i ), 2 )
            ENDIF
            oRow := oQuery:GetRow( i )
            APPEND BLANK

            FOR j := 1 TO Len( aStruct )
               cField := RTrim( Left( aStruct[ j ][ 1 ], 10 ) )
               IF aOKType[ j ]
                  dbf_ex->&cField := oRow:FieldGet( j )
               ELSE
                  dbf_ex->&cField := "?"
               ENDIF
            NEXT
         NEXT

         USE
         oQuery:Destroy()
         setMessage( " Table: " + cFileName, 1 )
         setmessage( "Records: " + ntrim( i - 1 ), 2 )
         msginfo( "Export finished!", PROGRAM )
      ELSE
         msgExclamation( "Error exporting file...", PROGRAM )
      ENDIF
      setMessage()
   ELSE
      msgExclamation( "Not connected...", PROGRAM )
   ENDIF

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION AddData( nPos )
*------------------------------------------------------------------------------*
   LOCAL oQuery AS OBJECT
   LOCAL i
   LOCAL aTypes
   LOCAL aLine

   IF lConnected .AND. lTableSelect .AND. nPos == F_Main.GRD_Data.ItemCount

      nFrom += nScope

      setMessage( "SQL Query: SELECT * FROM " + Upper( cTable ), 1 )
      oQuery := oServer:Query( "select * from " + cTable + " limit " + ntrim( nFrom ) + "," + ntrim( nScope ) )
      IF !( oQuery:NetErr() )
         aTypes := Array( oQuery:FCount() )
         FOR i := 1 TO oQuery:FCount()
            aTypes[ i ] := oQuery:FieldType( i )
         NEXT
      ELSE
         nFrom := 0
         RETURN NIL
      ENDIF

      oQuery:GoTop()

      F_Main.GRD_Data.DisableUpdate

      FOR i := 1 TO oQuery:LastRec()
         IF ( i % 100 ) == 0
            setmessage( "Record: " + ntrim( i ), 2 )
         ENDIF

         oQuery:GetRow( i )

         aLine := oQuery:aRow
         IF ! Empty( aLine[ 1 ] )
            F_Main.GRD_Data.addItem( ItemChar( aLine, aTypes ) )
         ENDIF
      NEXT

      F_Main.GRD_Data.EnableUpdate

      oQuery:Destroy()
      setMessage()

      F_Main.GRD_Data.VALUE := nPos

   ENDIF

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION wgwin( s )
*------------------------------------------------------------------------------*

   // 437
   s := StrTran( s, "^", Chr( 200 ) )
   s := StrTran( s, "~", Chr( 232 ) )
   s := StrTran( s, "[", Chr( 138 ) )
   s := StrTran( s, "{", Chr( 154 ) )
   s := StrTran( s, "@", Chr( 142 ) )
   s := StrTran( s, "`", Chr( 158 ) )

   // 852 ð
   s := StrTran( s, Chr( 208 ), Chr( 240 ) )
   // 437 ð
   s := StrTran( s, "|", Chr( 240 ) )
   // 437 Ð
   s := StrTran( s, BSL, Chr( 208 ) )

   // 852 Š
   s := StrTran( s, Chr( 230 ), Chr( 138 ) )
   // 437 æ
   s := StrTran( s, "}", Chr( 230 ) )
   // 852 æ
   s := StrTran( s, Chr( 134 ), Chr( 230 ) )

   // 852 Æ
   s := StrTran( s, Chr( 143 ), Chr( 198 ) )
   // 437 Æ
   s := StrTran( s, "]", Chr( 198 ) )

   // 852
   s := StrTran( s, Chr( 172 ), Chr( 200 ) )
   s := StrTran( s, Chr( 159 ), Chr( 232 ) )
   s := StrTran( s, Chr( 231 ), Chr( 154 ) )
   s := StrTran( s, Chr( 166 ), Chr( 142 ) )
   s := StrTran( s, Chr( 167 ), Chr( 158 ) )
   s := StrTran( s, Chr( 209 ), Chr( 208 ) )

   // 8859-2
   s := StrTran( s, Chr( 169 ), Chr( 138 ) )
   s := StrTran( s, Chr( 185 ), Chr( 154 ) )
   s := StrTran( s, Chr( 174 ), Chr( 142 ) )
   s := StrTran( s, Chr( 190 ), Chr( 158 ) )

   // navednica (MySql) z dvojnim narekovajem
   s := StrTran( s, "'", '"' )

RETURN s


*------------------------------------------------------------------------------*
FUNCTION dbf2sql( lCreateTable )
*------------------------------------------------------------------------------*
   LOCAL l, f
   LOCAL fh, cInsert, cQuery, i, cFileName, aDbfFiles, aDbfStruct, lError := .F., cRec
   LOCAL xField, cField
   LOCAL nRef := 0, nWrite := 500

   IF ! lBaseSelect
      msginfo( "No dababase selected...", PROGRAM )
      RETURN NIL
   ENDIF

   IF lConnected

      aDbfFiles := getfile( { { "*.dbf", "*.DBF" } }, "Select files to copy", "", .T., .F. )

      IF Len( aDbfFiles ) == 1 .AND. Empty( aDbfFiles[ 1 ] )
         RETURN NIL
      ENDIF

      FOR f := 1 TO Len( aDbfFiles )

         IF ! Empty( Alias() )
            dbffile->( dbCloseArea() )
         ENDIF

         l := Len( aDbfFiles[ f ] )
         FOR i := 1 TO l - 1
            IF SubStr( aDbfFiles[ f ], l - i, 1 ) == BSL
               EXIT
            ENDIF
         NEXT

         cFileName := StrTran( Lower( Right( aDbfFiles[ f ], i ) ), ".dbf", "" )

         IF AScan( aTables, cFileName ) > 0
            IF ! msgyesno( "Table " + cFileName + " already exists! Overwrite?", PROGRAM )
               lError := .T.
               EXIT
            ENDIF
         ENDIF

         dbUseArea( .T., , aDbfFiles[ f ], "dbffile", , .T. )
         aDbfStruct := dbffile->( dbStruct() )
         FOR i := 1 TO Len( aDbfStruct )
            IF aDbfStruct[ i ][ 4 ] > 0
               aDbfStruct[ i ][ 3 ] -= 1
            ENDIF
         NEXT

         IF lCreateTable
            IF AScan( aTables, cFileName ) > 0
               oServer:DeleteTable( cFileName )
               IF oServer:NetErr()
                  lError := .T.
                  MsgExclamation( oServer:Error(), "-1" )
                  EXIT
               ENDIF
            ENDIF

            oServer:CreateTable( cFileName, aDbfStruct )
            IF oServer:NetErr()
               lError := .T.
               MsgExclamation( oServer:Error(), "0" )
               EXIT
            ENDIF
         ENDIF

         cRec := "/" + AllTrim( Str( dbffile->( LastRec() ) ) )
         fh := FCreate( "dump.sql" )

         IF fh < 0
            lError := .T.
            msgexclamation( "Handle create?" )
            EXIT
         ENDIF

         cInsert := "INSERT INTO " + cFileName + " VALUES " + CRLF
         FWrite( fh, cInsert )

         DO WHILE ! dbffile->( Eof() )

            cQuery := "("

            FOR i := 1 TO dbffile->( FCount() )
               xField := NIL
               xField := dbffile->( FieldGet( i ) )
               DO CASE
               CASE ValType( xField ) == "D"
                  cField := "'" + d2c( xField ) + "'"
               CASE ValType( xField ) == "C"
                  cField := AllTrim( xField )
                  cField := wgwin( cField )
                  cField := "'" + cField + "'"
               CASE ValType( xField ) == "M"
                  cField := MemoTran( AllTrim( xField ), "", "" )
                  cField := wgwin( cField )
                  cField := "'" + cField + "'"
               CASE ValType( xField ) == "N"
                  cField := SQLTrim( xField, aDbfStruct[ i, 4 ] )
               CASE ValType( xField ) == "L"
                  cField := l2c( xField )
               ENDCASE
               cQuery := cQuery + cField + ","

            NEXT
            // remove last ","
            cQuery := Left( cQuery, Len( cQuery ) - 1 ) + ")"

            FWrite( fh, cQuery )

            nRef++

            dbffile->( dbSkip() )

            // write after each nWrite records!
            IF nRef == nWrite
               DO EVENTS
               setmessage( cBase + "." + cFileName + ": " + LTrim( Str( dbffile->( RecNo() ) ) ) + cRec, 1 )
               nRef := 0
               IF ! MySQL_Dump( fh )
                  lError := .T.
                  EXIT
               ENDIF
               FClose( fh )
               ERASE dump.sql
               fh := FCreate( "Dump.sql" )
               IF fh < 1
                  MsgExclamation( "Handle create?" )
                  lError := .T.
                  EXIT
               ENDIF
               FWrite( fh, cInsert )
            ELSE
               IF ! Eof()
                  FWrite( fh, ", " + CRLF )
               ENDIF
            ENDIF

         ENDDO

         IF nRef > 0
            DO EVENTS
            setmessage( cBase + "." + cFileName + ": " + LTrim( Str( dbffile->( RecNo() ) ) ) + cRec, 1 )
            MySql_Dump( fh )
         ENDIF

         dbffile->( dbCloseArea() )

         FClose( fh )
         ERASE dump.sql

      NEXT

      IF ! Empty( Alias() )
         dbffile->( dbCloseArea() )
      ENDIF

      IF ! lError
         UpdateTree()
         msginfo( "Transfer completed.", PROGRAM )
      ENDIF

      setmessage()

   ENDIF

   DO EVENTS

RETURN NIL


*------------------------------------------------------------------------------*
FUNCTION MySql_Dump( _fh )
*------------------------------------------------------------------------------*
   LOCAL flen, oQuery, lret := .T.

   // eof, bof
   fLen := FSeek( _fh, 0, 2 )
   FSeek( _fh, 0 )
   oQuery := oServer:Query( FReadStr( _fh, flen ) )
   IF oServer:NetErr()
      msginfo( oServer:Error() )
      lret := .F.
   ELSE
      oQuery:Destroy()
   ENDIF

RETURN lret


*------------------------------------------------------------------------------*
FUNCTION d2c( _date )
*------------------------------------------------------------------------------*
   LOCAL cret := '0000-00-00'

   IF ! Empty( _date )
      cret := hb_DToC( _date, "yyyy-mm-dd" )
   ENDIF

RETURN cret


*------------------------------------------------------------------------------*
FUNCTION SQLtrim( _value, _dec )
*------------------------------------------------------------------------------*
   LOCAL cret := "0"

   IF _value <> 0
      cret := LTrim( Str( _value, 30, iif( _dec == NIL, 0, _dec ) ) )
   ENDIF

RETURN cret


*------------------------------------------------------------------------------*
FUNCTION l2c( _bool )
*------------------------------------------------------------------------------*

RETURN iif( _bool, "1", "0" )


*------------------------------------------------------------------------------*
PROCEDURE ExportGridToCSV(windowname, gridname)
*------------------------------------------------------------------------------*
   LOCAL cFileName := "Data_" + gridname + ".csv"
   LOCAL aLines := {}, aHeaders := {}, nColumns, nItems, count1, cLine, oError

   // Validate inputs
   IF Empty(windowname) .OR. Empty(gridname)
      MsgStop("Invalid window or grid name", "Error")
      RETURN
   ENDIF

   IF File( cFileName )
      IF ! msgyesno( "File " + cFileName + " already exists! Overwrite?", PROGRAM )
         RETURN
      ENDIF
   ENDIF

   // Get column count and headers
   nColumns := LEN(GETPROPERTY(windowname, gridname, "item", 1))
   FOR count1 := 1 TO nColumns
      AAdd(aHeaders, GETPROPERTY(windowname, gridname, "header", count1))
   NEXT

   // Add headers to CSV
   AAdd(aLines, ArrayToCSV(aHeaders))

   // Get item count
   nItems := GETPROPERTY(windowname, gridname, "itemcount")
   FOR count1 := 1 TO nItems
      cLine := ArrayToCSV(GETPROPERTY(windowname, gridname, "item", count1))
      AAdd(aLines, cLine)
   NEXT

   // Write to file
   BEGIN SEQUENCE
      hb_MemoWrit(cFileName, CRLFJOIN(aLines))
      MsgInfo("Data exported to CSV file: " + cFileName)
   RECOVER USING oError
      MsgStop("Error exporting data: " + oError:Description, "Error")
   END SEQUENCE

RETURN

*------------------------------------------------------------------------------*
// Converts an array to a CSV string, escaping special characters
STATIC FUNCTION ArrayToCSV(aArray)
*------------------------------------------------------------------------------*
   LOCAL nLen := LEN(aArray), cResult := ""
   LOCAL count1

   FOR count1 := 1 TO nLen
      cResult += EscapeCSV(CValToChar(aArray[count1]))
      IF count1 < nLen
         cResult += ","
      ENDIF
   NEXT

RETURN cResult

*------------------------------------------------------------------------------*
STATIC FUNCTION EscapeCSV(cValue)
*------------------------------------------------------------------------------*
RETURN '"' + STRTRAN(cValue, '"', '""') + '"'

*------------------------------------------------------------------------------*
STATIC FUNCTION CRLFJOIN(aArray)
*------------------------------------------------------------------------------*
   LOCAL cResult := "", count1

   FOR count1 := 1 TO LEN(aArray)
      cResult += aArray[count1] + CRLF
   NEXT

RETURN hb_StrShrink( cResult, 2 )  // Remove the last CRLF
