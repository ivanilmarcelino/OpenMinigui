/*
 *    PROGRAMM : Simple NET I/O Server (+ Client conn.), based upon excellent
 *               NETIO lib for Harbour, by Przemyslaw Czerpak.
 *    Author:    Pete D. <pete_westg@yahoo.gr> - March 2010
 *    LICENCE:   Free (GPL) for any use (like in 'free speach', but always at your own risk!)
*/

// #include 'funcs.prg'

#include <minigui.ch>


#define NET_ADDRESS Main.Text_IP.Value
#define NET_PORT    Main.Text_Port.Value
#define NET_PASSWD  ALLTRIM( IIF(Main.Check_ENC.Value, Main.Text_Pass.Value, "") )
#define NET_DIR     Main.Text_DIR.Value
#define NET_RPC     Main.Check_RPC.Value
#define NET_ENC     Main.Check_ENC.Value
#define NET_PATH    "net:" + NET_ADDRESS + ":" + HB_NToS( NET_PORT ) + ;
                    IF( ! Empty( NET_PASSWD ), ":" + NET_PASSWD, "" ) + ":"


REQUEST DBFCDX

STATIC pListenSocket := NIL
STATIC lIsConnection := .F.
STATIC lClientMode := .F.

FUNCTION Main()

   SET EXCLUSIVE OFF
   rddSetDefault( "DBFCDX" )

   LOAD WINDOW MAIN

   IF lClientMode
      MAIN.Button_StartServer.Enabled := .F.
      MAIN.Button_StopServer.Enabled := .F.
      MAIN.M_STARTS.Enabled := .F.
      MAIN.M_STOPS.Enabled := .F.
      MAIN.Tab_1.VALUE := 2
   ENDIF

   MAIN.CENTER

   MAIN.ACTIVATE

RETURN NIL

***************************
PROCEDURE NotifyIconClick()

   LOCAL hWnd := GetFormHandle( 'Main' )
   IF IsWIndowDefined( Main )
      IF IsWindowVisible( hWnd )
         MAIN.Hide
      ELSE
         MAIN.Show
         IF IsIconic( hWnd )
            MAIN.RESTORE
         ENDIF
      ENDIF
   ENDIF

RETURN

**********************
FUNCTION StartServer()

   LOCAL cPassword := NET_PASSWD
   DECLARE WINDOW MAIN

   pListenSocket := netio_mtserver( NET_PORT, NET_ADDRESS, ;
      NET_DIR, NET_RPC, ;
      @ cPassword )
   IF ! Empty( pListenSocket )
      MAIN.Image_ServerStatus.Hide
      *Main.Image_ServerStatus.Picture := "NetFolder.jpg"
      MAIN.Image_ServerStatus.PICTURE := "NETFLD_PIC"
      MAIN.Image_ServerStatus.Refresh
      MAIN.Image_ServerStatus.Show
      MsgInfo( "Good news! server started and is listening at Port: " + hb_ntos( NET_PORT ), "START SERVER" )
   ELSE

      MsgExclamation( "Sadly enough, I was unable to start server!" + CRLF + ;
         "(Check Port and/or IPAddress)", "START SERVER" )
      MAIN.Image_ServerStatus.PICTURE := "NETFLD_PIC"
   ENDIF

RETURN NIL

*********************
FUNCTION StopServer()

   IF ! Empty( pListenSocket )
      MAIN.Image_ServerStatus.Hide
      netio_serverstop( pListenSocket, .T. )
      pListenSocket := NIL

      MAIN.Image_ServerStatus.PICTURE := ""
      MAIN.Image_ServerStatus.Refresh
      MAIN.Image_ServerStatus.Show

      MsgInfo( "Server stopped!", "STOP SERVER" )
   ELSE
      MsgExclamation( "Hey! cannot stop a non-started Server!", "STOP SERVER" )
   ENDIF

RETURN NIL

*********************
PROCEDURE ServerSpecs

   LOCAL cSpecs
   IF ! Empty( pListenSocket )
      cSpecs := CRLF + ;
         " NETIO_SERVER  IP : " + NET_ADDRESS + CRLF + ;
         " LISTENING PORT   : " + hb_ntos( NET_PORT ) + CRLF + ;
         " RPC ACTIVE       : " + IF( NET_RPC, "Yes", "No" ) + CRLF + ;
         " ENCRYPTION       : " + IF( NET_ENC, "Yes", "No" ) + CRLF + ;
         " PASSWORD         : " + NET_PASSWD + CRLF + ;
         " NET FULLPATH     : " + NET_PATH + CRLF
      BrowseText( cSpecs, "SERVER SPECS" )
   ELSE
      MsgExclamation( "No Server seems to been started, on this machine!", "Stop Server" )
   ENDIF

RETURN

******************
FUNCTION QuitApp()

   DECLARE WINDOW MAIN
   IF ! Empty( pListenSocket )
      StopServer()
   ENDIF
   MAIN.RELEASE

RETURN NIL

****************
FUNCTION About()
   AlertInfo( "Simple NET I/O Server (+ Client conn.),; based upon the excellent " + ;
      "NETIO lib for Harbour,; by Przemyslaw Czerpak;;" + ;
      "Author Pete D. <pete_westg@yahoo.gr>;March 2010;" + ;
      "Free (GPL) for any use.", ;
      "Simple NET I/O Server" )

RETURN NIL

*********************
FUNCTION ManagePass()

   DECLARE WINDOW MAIN
   IF NET_ENC
      MAIN.Text_Pass.Enabled := .T.
   ELSE
      MAIN.Text_Pass.VALUE := ""
      MAIN.Text_Pass.Enabled := .F.
   ENDIF

RETURN NIL

#include "Directry.ch"

**********************************
FUNCTION ClientConnect( lConnect )

   LOCAL aDirectory := {}
   LOCAL aGrid := {}
   LOCAL nI
   DEFAULT lConnect TO ( MAIN.Button_Connect.CAPTION == "Connect to Server" )
   IF lConnect
      lIsConnection := NetIO_Connect( NET_ADDRESS, NET_PORT, , NET_PASSWD )
      IF lIsConnection

         MAIN.Image_ConnectionStatus.Hide
         MAIN.Image_ConnectionStatus.PICTURE := "CONECT_PIC"
         MAIN.Image_ConnectionStatus.Show
         MsgInfo( "Connection established!", "CONNECT" )

         IF NET_RPC
            MAIN.Grid_DBF.DeleteAllItems()
            aDirectory := netio_funcexec( "Directory", "*.dbf" )
            FOR nI := 1 TO Len( aDirectory )
               MAIN.Grid_DBF.AddItem( MakeItem( aDirectory[ nI ] ) )
            NEXT
         ENDIF
         MAIN.Button_Connect.CAPTION := "Disconnect.."
      ELSE
         MsgExclamation( "Bad news! Could not connect...", "CONNECT" )
      ENDIF
   ELSE
      IF lIsConnection
         lConnect := netio_disconnect( NET_ADDRESS, NET_PORT )
         IF lConnect
            MAIN.Image_ConnectionStatus.Hide
            MAIN.Image_ConnectionStatus.PICTURE := ""
            MAIN.Image_ConnectionStatus.Refresh
            MAIN.Image_ConnectionStatus.Show
            MsgInfo( "Disconected from Server!", "DISCONNECT" )

            lIsConnection := ! lConnect
            MAIN.Grid_DBF.DeleteAllItems()
            MAIN.Button_Connect.CAPTION := "Connect to Server"
         ELSE
            MsgExclamation( "Strange, but could not disconnect!", "DISCONNECT" )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

********************************
STATIC FUNCTION MakeItem( aEle )
RETURN { PadR( aEle[ F_NAME ], 15 ), hb_ntos( aEle[ F_SIZE ] ), ;
      DToC( aEle[ F_DATE ] ), aEle[ F_TIME ] }

************************
FUNCTION BrowseThisDBF()

   LOCAL cDBF := MAIN.Grid_DBF.Item( MAIN.Grid_DBF.Value )[ 1 ]
   IF ! Empty( MAIN.Grid_DBF.Value ) .AND. ! Empty( cDbf )
      cDBF := NET_PATH + MAIN.Grid_DBF.Item( MAIN.Grid_DBF.Value )[ 1 ]
      USE ( cDBF )
      EDIT EXTENDED
   ELSE
      msgStop( "No file Selected for browse!", "BROWSE DBF" )
   ENDIF

RETURN NIL


****************
FUNCTION Usage()

   BrowseText( 'Readme.rtf', 'NETIO LIB DOCUMENTATION', .T. )

RETURN NIL

*******************
FUNCTION ExecPROC()

   LOCAL cParams
   LOCAL lResult := .F.
   LOCAL Title := "Execute PROCEDURE"
   LOCAL aL := { "PROCNAME", "PROCPARAM_1", "PROCPARAM_2" }
   LOCAL aIV := { "", "", "" }
   LOCAL aF := { 20, 20, 20 }
   LOCAL aProc

   aProc := InputWindow( Title, aL, aIV, aF )
   IF ISNIL( aProc[ 1 ] )
      RETURN lResult
   ENDIF
   cParams := AllTrim( aProc[ 2 ] ) + if( Empty( aProc[ 3 ] ), "", "," + AllTrim( aProc[ 3 ] ) )

   IF ( lResult := NETIO_PROCEXECW( aProc[ 1 ], cParams ) )
      msgInfo( "Given PROC " + aProc[ 1 ] + " executed on server side" )
   ELSE
      msgInfo( "Given PROC " + aProc[ 1 ] + " not executed on server side" )
   ENDIF

RETURN lResult
