/*
 * MiniGui - Microsoft SQL sample
 *
 * Copyright 2011 Alen Uzelac <alen@bbm.hr>
 *
 */

#include "minigui.ch"
#include "dbinfo.ch"

REQUEST SQLMIX, SDDODBC

STATIC nSQLConnection

*------------------------
FUNCTION Main()
*------------------------

   nSQLConnection := 0

   SET DATE TO GERMAN
   SET DELETED ON

   DEFINE WINDOW Win1 ;
         AT 0, 0 WIDTH 600 HEIGHT 250 ;
         TITLE "Microsoft SQL demo" ;
         NOSIZE NOMAXIMIZE ;
         MAIN

      @ 32, 30 LABEL labLabel1 VALUE "Server:" WIDTH 60
      @ 30, 100 TEXTBOX txtServer VALUE "SERVER\SQLEXPRESS" WIDTH 250
      @ 62, 30 LABEL labLabel2 VALUE "Database:" WIDTH 60
      @ 60, 100 TEXTBOX txtDatabase VALUE "test"
      @ 92, 30 LABEL labLabel3 VALUE "Table:" WIDTH 60
      @ 90, 100 TEXTBOX txtTable VALUE "demo"
      @ 122, 30 LABEL labLabel4 VALUE "Username:" WIDTH 65
      @ 120, 100 TEXTBOX txtUser VALUE "sa"
      @ 152, 30 LABEL labLabel5 VALUE "Password:" WIDTH 60
      @ 150, 100 TEXTBOX txtPass VALUE "pass"

      @ 30, 400 BUTTON btnButton1 CAPTION "Connect to database" WIDTH 150 ACTION ConnectDatabase()
      @ 60, 400 BUTTON btnButton2 CAPTION "Create table" WIDTH 150 ACTION Table_Create()
      @ 90, 400 BUTTON btnButton3 CAPTION "Fill with data" WIDTH 150 ACTION Table_Fill()
      @ 120, 400 BUTTON btnButton4 CAPTION "Browse data" WIDTH 150 ACTION Table_Browse()
      @ 150, 400 BUTTON btnButton5 CAPTION "Disconnect" WIDTH 150 ACTION DisconnectDatabase()

   END WINDOW

   ShowButtons( .F. )
   Win1.Center()
   Win1.Activate()

RETURN NIL

PROCEDURE ShowButtons( lShow )

   Win1.btnButton1.enabled := ! lShow
   Win1.btnButton2.enabled := lShow
   Win1.btnButton3.enabled := lShow
   Win1.btnButton4.enabled := lShow
   Win1.btnButton5.enabled := lShow
   Win1.txtServer.Enabled := ! lShow
   Win1.txtDatabase.Enabled := ! lShow
   Win1.txtTable.Enabled := ! lShow
   Win1.txtUser.Enabled := ! lShow
   Win1.txtPass.Enabled := ! lShow

RETURN

PROCEDURE ConnectDatabase()

   nSQLConnection := SQL_ConnectDatabase( Win1.txtServer.VALUE, Win1.txtDatabase.VALUE, Win1.txtUser.VALUE, Win1.txtPass.Value )

   IF nSQLConnection = 0
      msgstop( "Connection to server '" + win1.txtServer.VALUE +"' database '" + win1.txtDatabase.VALUE +"' failed." )
   ELSE
      Win1.btnButton1.CAPTION := "Connected"
      ShowButtons( .T. )
   ENDIF

RETURN

PROCEDURE DisconnectDatabase()

   IF SQL_Disconnect( nSQLConnection ) > 0
      Win1.btnButton1.CAPTION := "Connect to database"
      ShowButtons( .F. )
   ENDIF

RETURN

PROCEDURE Table_Create()

   IF SQL_CreateTable( win1.txtTable.value )
      MsgInfo( "Table '" + win1.txtTable.VALUE +"' created." )
   ELSE
      MsgStop( "Table '" + win1.txtTable.VALUE +"' not created." )
   ENDIF

RETURN

PROCEDURE Table_Fill()

   LOCAL nI, nRecords := 100, cStr := "", cDate := DToC( Date() )

   FOR nI := 1 TO nRecords
      cStr += "('20" + Right( cDate, 2 ) + "-" + SubStr( cDate, 4, 2 ) + "-" + Left( cDate, 2 ) + "',"
      cStr += "'" + Time() + "',"
      cStr += "'Demo text " + AllTrim( Str( nI ) ) + "',"
      cStr += "'Table_fill1'),"
   NEXT nI

   cStr := Left( cStr, Len( cStr ) - 1 ) // deleting last ,

   IF rddInfo( RDDI_EXECUTE, "INSERT INTO " + Win1.txtTable.VALUE +" values " + cStr, "SQLMIX" )
      MsgInfo( "Added " + AllTrim( Str( nRecords ) ) + " records." )
   ELSE
      MsgStop( "Error executing SQL command." + CRLF + "INSERT INTO " + Win1.txtTable.VALUE +" values " + cStr )
   ENDIF

RETURN

PROCEDURE Table_Browse()

   TRY
      dbUseArea( .T., "SQLMIX", "SELECT * FROM " + Win1.txtTable.VALUE, "table",,,, nSQLConnection )
   CATCH
      MsgInfo( "Cannot open table '" + Win1.txtTable.VALUE +"' on server '" + Win1.txtServer.VALUE +"'." )
      RETURN
   END

   DEFINE WINDOW Win2 ;
         AT Win1.ROW + 100, Win1.COL + 10 ;
         WIDTH 500 HEIGHT 400 ;
         TITLE 'Browse MSSQL data' ;
         MODAL ;
         FONT 'Arial' SIZE 10

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

      @ 20, 20 BROWSE brwBrowse1 ;
         WIDTH 460 ;
         HEIGHT 330 ;
         HEADERS { 'Date', 'Time', 'Text', 'Type' } ;
         WIDTHS { 80, 80, 100, 80 } ;
         WORKAREA table ;
         FIELDS { 'table->date', 'table->time', 'table->text', 'table->type' } ;
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT } ;
         FONT "MS Sans serif" SIZE 09

   END WINDOW
   ACTIVATE WINDOW Win2

   dbCloseArea()

RETURN

FUNCTION SQL_ConnectDatabase( cServer, cDatabase, cUser, cPassword )
RETURN rddInfo( RDDI_CONNECT, { "ODBC", "Driver={SQL Server};Server=" + cServer + ";Database=" + cDatabase + ";Uid=" + cUser + ";Pwd=" + cPassword + ";" }, 'SQLMIX' )

FUNCTION SQL_Disconnect( nConnection )
RETURN rddInfo( RDDI_DISCONNECT, nConnection )

FUNCTION SQL_CreateTable( cTable )
RETURN rddInfo( RDDI_EXECUTE, "CREATE TABLE [dbo].[" + cTable + "] ( [DATE] date NULL,  [TIME] varchar(8) NULL,  [TEXT] varchar(30) NULL,  [TYPE] varchar(30) NULL)", "SQLMIX" )
