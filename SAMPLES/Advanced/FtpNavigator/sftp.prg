#include "hmg.ch"
#include "hbcurl.ch"
#include "fileio.ch"

#if 0
#define HB_CURL_CURLSSH_AUTH_ANY              hb_bitNot( 0 )      /* all types supported by the server */
#define HB_CURL_CURLSSH_AUTH_NONE             0                   /* none allowed, silly but complete */
#define HB_CURL_CURLSSH_AUTH_PUBLICKEY        1                   /* Memvar/private key files */
#define HB_CURL_CURLSSH_AUTH_PASSWORD         2                   /* password */
#define HB_CURL_CURLSSH_AUTH_HOST             4                   /* host key files */
#define HB_CURL_CURLSSH_AUTH_KEYBOARD         8                   /* keyboard interactive */
#define HB_CURL_CURLSSH_AUTH_DEFAULT          HB_CURL_CURLSSH_AUTH_ANY
#endif

MEMVAR cUrl
MEMVAR UserLogin
MEMVAR UserPass
MEMVAR nPort

MEMVAR cPrivateKey
MEMVAR cPublicKey
MEMVAR cPKeyMD5

MEMVAR curlHandle
MEMVAR cRemoteDir
MEMVAR cLocalDir

MEMVAR aRemoteDir
MEMVAR aLocalDir

MEMVAR cTitle

FUNCTION Main()
***************
   LOCAL i, aDrives := {}

   SET FONT TO "MS Shell Dlg", 9

   SET CENTURY ON
   SET DATE TO ANSI

#pragma TEXTHIDDEN(1)

   // Public Online SFTP server
   PUBLIC cUrl := "sftp://demo.wftpserver.com/"
   PUBLIC UserLogin := "demo-user"
   PUBLIC UserPass := "demo-user"
   PUBLIC nPort := 2222

   PUBLIC cPrivateKey := "" // GetCurrentFolder()+"\PrvKey.ppk"
   PUBLIC cPublicKey := "" // GetCurrentFolder()+"\PubKey.pub"
   PUBLIC cPKeyMD5 := "" // "cf:8d:17:f0:db:ef:84:7b:bb:62:c3:d4:3f:4a:93:13"

#pragma TEXTHIDDEN(0)

   PUBLIC curlHandle := NIL
   PUBLIC cRemoteDir := ""
   PUBLIC cLocalDir := GetCurrentFolder() + "\"

   PUBLIC aRemoteDir := {}
   PUBLIC aLocalDir := {}

   FOR i = 65 TO 90
      IF IsDisk ( Chr( i ) )
         AAdd( aDrives, Chr( i ) + ':' )
      ENDIF
   NEXT

   PUBLIC cTitle := "FTP Navigator"

   DEFINE WINDOW ftp AT 0, 0 WIDTH 805 HEIGHT 600 TITLE cTitle MAIN NOSIZE NOMAXIMIZE ;
         ON RELEASE Disconnect() ;
         ON INTERACTIVECLOSE MsgYesNo ( "Exit FTP Navigator?", "Confirm", .T. )

      DEFINE MAIN MENU
         DEFINE POPUP "File"
            MENUITEM "Connect to the server" ACTION Connect()
            MENUITEM "Disconnect from the server" ACTION Disconnect()
            SEPARATOR
            MENUITEM "Exit" ACTION ReleaseFTP()
         END POPUP

         @ 525, 20 LABEL LabelF1 VALUE "F1 - Connect" ACTION {|| IF ( Empty( curlHandle ), Connect(), Disconnect() ) } AUTOSIZE
         @ 525, 145 LABEL LabelF2 VALUE "F2 - Drive" ACTION DriveList() AUTOSIZE
         @ 525, 240 LABEL LabelF5 VALUE "F5 - Copy (upload)" ACTION {|| IF ( ftp.FocusedControl = 'LocalGrid', OnLocalKey( VK_F5 ), IF ( ftp.FocusedControl = 'RemoteGrid', OnRemoteKey( VK_F5 ), Nil ) ) } AUTOSIZE
         @ 525, 380 LABEL LabelF6 VALUE "F6 - Rename" ACTION {|| IF ( ftp.FocusedControl = 'LocalGrid', OnLocalKey( VK_F6 ), IF ( ftp.FocusedControl = 'RemoteGrid', OnRemoteKey( VK_F6 ), Nil ) ) } AUTOSIZE
         @ 525, 490 LABEL LabelF7 VALUE "F7 - Make Folder" ACTION {|| IF ( ftp.FocusedControl = 'LocalGrid', OnLocalKey( VK_F7 ), IF ( ftp.FocusedControl = 'RemoteGrid', OnRemoteKey( VK_F7 ), Nil ) ) } AUTOSIZE
         @ 525, 610 LABEL LabelF8 VALUE "F8 - Delete" ACTION {|| IF ( ftp.FocusedControl = 'LocalGrid', OnLocalKey( VK_F8 ), IF ( ftp.FocusedControl = 'RemoteGrid', OnRemoteKey( VK_F8 ), Nil ) ) } AUTOSIZE
         @ 525, 700 LABEL LabelESC VALUE "Escape - Exit" ACTION ReleaseFTP() AUTOSIZE

         ON KEY ESCAPE ACTION ReleaseFTP()
         ON KEY F1 ACTION {|| IF ( Empty( curlHandle ), Connect(), Disconnect() ) }
         ON KEY F2 ACTION {|| DriveList() }
         ON KEY F5 ACTION ( ftp.LabelF5.Action )
         ON KEY F6 ACTION ( ftp.LabelF6.Action )
         ON KEY F7 ACTION ( ftp.LabelF7.Action )
         ON KEY F8 ACTION ( ftp.LabelF8.Action )
         ON KEY F9 ACTION {|| IF ( ftp.FocusedControl = 'LocalGrid', OnLocalKey( VK_F9 ), IF ( ftp.FocusedControl = 'RemoteGrid', OnRemoteKey( VK_F9 ), Nil ) ) }

         @ 0, 0 COMBOBOX ComboDrive ;
            ITEMS aDrives ;
            VALUE AScan( aDrives, GetDrvLetter() ) ;
            WIDTH 35 ;
            ON GOTFOCUS NIL ;
            ON CHANGE IF( aDrives[ This.Value ] # GetDrvLetter(), ( cLocalDir := aDrives[ This.Value ] + "\", RefreshLocalGrid ( LocalDir(Directory(cLocalDir, "HSD" ) ) ) ), Nil ) ;
            ON LOSTFOCUS NIL ;
            ON ENTER NIL ;
            ON DISPLAYCHANGE NIL ;
            ON DROPDOWN NIL ;
            ON CLOSEUP NIL ;
            NOTABSTOP

         @ 3, 40 LABEL LabelLocal VALUE cLocalDir WIDTH 360 HEIGHT 18 // ENDELLIPSES
         @ 22, 0 GRID LocalGrid ;
            WIDTH 399 ;
            HEIGHT 490 ;
            HEADERS { '', 'File Name', 'Size', 'Date', 'Time', 'Attr' } ;
            WIDTHS { 0, 140, 65, 75, 60, 23 } ;
            IMAGE { "FILE", "FOLDER" } ;
            ITEMS aLocalDir ;
            VALUE 1 ;
            JUSTIFY {, GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT, GRID_JTFY_LEFT } ;
            ON GOTFOCUS ( ftp.LabelF5.VALUE := "F5 - Copy (upload)" ) ;
            ON DBLCLICK OnLocalKey( VK_RETURN )

         ftp.LocalGrid.PaintDoubleBuffer := .T.
/*
  FOR i = 1 TO ftp.LocalGrid.ColumnCOUNT
   ftp.LocalGrid.HeaderDYNAMICFORECOLOR (i) := {|| BLUE  } //Headers color
   ftp.LocalGrid.HeaderDYNAMICBACKCOLOR (i) := {|| COLOR_AntiqueWhite } //Headers background
  NEXT i
*/
         RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ) )

         @ 3, 400 LABEL LabelRemote VALUE cRemoteDir WIDTH 400 HEIGHT 18 // ENDELLIPSES

         @ 22, 400 GRID RemoteGrid ;
            WIDTH 399 ;
            HEIGHT 490 ;
            HEADERS { '', 'File Name', 'Size', 'Date', 'Time', 'Attr' } ;
            WIDTHS { 0, 130, 65, 75, 45, 63 } ;
            IMAGE { "FILE", "FOLDER" } ;
            ITEMS aRemoteDir ;
            JUSTIFY {, GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT, GRID_JTFY_LEFT } ;
            ON GOTFOCUS ( ftp.LabelF5.VALUE := "F5 - Copy (download)" ) ;
            ON DBLCLICK OnRemoteKey( VK_RETURN )

         ftp.RemoteGrid.PaintDoubleBuffer := .T.
/*
  FOR i = 1 TO ftp.RemoteGrid.ColumnCOUNT
   ftp.RemoteGrid.HeaderDYNAMICFORECOLOR (i) := {|| BLUE  } //Headers color
   ftp.RemoteGrid.HeaderDYNAMICBACKCOLOR (i) := {|| COLOR_AntiqueWhite } //Headers background
  NEXT i
*/
         ftp.LocalGrid.SetFocus

      END MENU
   END WINDOW

   CENTER WINDOW ftp
   ACTIVATE WINDOW ftp

RETURN NIL

FUNCTION RefreshRemoteGrid ( aRemoteDir, xPos )
***********************************************
   LOCAL aItem

   ftp.RemoteGrid.DisableUpdate()
   ftp.RemoteGrid.DeleteAllItems

   FOR EACH aItem IN aRemoteDir
      ASize( aItem, Len( aItem ) + 1 )
      AIns( aItem, 1 ) ; aItem[ 1 ] := 0
      aItem[ 1 ] := if( 'D' $ Upper( aItem[ 6 ] ), 1, 0 )
      ftp.RemoteGrid.AddItem( aItem )
   NEXT

   DO CASE
   CASE HB_ISNUMERIC( xPos )
      xPos := Min( xPos, ftp.RemoteGrid.ItemCount )
   CASE HB_ISCHAR ( xPos )
      xPos := Max ( 1, AScan ( aRemoteDir, {| x | x[ 2 ] == xPos } ) )
      OTHER
      xPos := 1
   ENDCASE

   ftp.RemoteGrid.VALUE := xPos
   ftp.RemoteGrid.EnableUpdate()
   ftp.LabelRemote.VALUE := "/" + cRemoteDir

RETURN NIL

FUNCTION RefreshLocalGrid ( aLocalDir, xPos )
*********************************************
   LOCAL aItem

   ftp.LocalGrid.DisableUpdate()
   ftp.LocalGrid.DeleteAllItems

   FOR EACH aItem IN aLocalDir
      ASize( aItem, Len( aItem ) + 1 )
      AIns( aItem, 1 ) ; aItem[ 1 ] := 1
      aItem[ 1 ] := if( 'D' $ Upper( aItem[ 6 ] ), 1, 0 )
      ftp.LocalGrid.AddItem( aItem )
   NEXT

   DO CASE
   CASE HB_ISNUMERIC( xPos )
      xPos := Min( xPos, ftp.LocalGrid.ItemCount )
   CASE HB_ISCHAR ( xPos )
      xPos := Max ( 1, AScan ( aLocalDir, {| x | x[ 2 ] == xPos } ) )
      OTHER
      xPos := 1
   ENDCASE

   ftp.LocalGrid.VALUE := xPos
   ftp.LocalGrid.EnableUpdate()
   ftp.LabelLocal.VALUE := cLocalDir

RETURN NIL

FUNCTION Connect()
******************
   LOCAL cErr, curlErr

   cRemoteDir := ""
   aRemoteDir := {}

   IF ! Empty( curlHandle )
      Disconnect()
   ENDIF

   IF ! ConfirmToConnect()
      RETURN NIL
   ENDIF

   // Init
   curlHandle := curl_easy_init()
   IF Empty( curlHandle )
      Info_( "Error while init cURL lib.", .T. )
      Disconnect()
   ELSE
      cURL += IF ( Right ( cURL, 1 ) # '/', '/', '' ) // add slash to cURL
      info_( 'Connecting to ' + cURL )
      curl_easy_reset( curlHandle )
      Login_FTP( cURL )
      curlErr := curl_easy_perform( curlHandle )
      IF curlErr # 0
         MsgStop ( curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')', 'Error' )
         Disconnect()
      ELSE
         cErr := Dir_FTP( cRemoteDir, @aRemoteDir )
         IF ! Empty ( cErr )
            MsgStop( cErr, 'Error' )
         ELSE
            RefreshRemoteGrid ( aRemoteDir )
            ftp.TITLE := cTitle + " " + cUrl + cRemoteDir
            ftp.LabelF1.VALUE := "F1 - Disconnect"
         ENDIF
      ENDIF
   ENDIF

   WAIT CLEAR

RETURN NIL

PROCEDURE Disconnect()
**********************
   cRemoteDir := ""
   IF ! Empty( curlHandle )
      curl_global_cleanup( curlHandle )
   ENDIF

   curlHandle := NIL
   ftp.TITLE := cTitle
   ftp.RemoteGrid.DeleteAllItems
   ftp.LabelRemote.VALUE := ""
   ftp.LabelF1.VALUE := "F1 - Connect"

RETURN

FUNCTION Dir_FTP( cFolder, aDirectory )
***************************************
   LOCAL cRet := "", cSftpURL, cDirList, cList := "", aDirList
   LOCAL aMonth := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }
   LOCAL curlErr, a1List, a1Dir, cfName

   DEFAULT cFolder := ""
   DEFAULT aDirectory := {}

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   info_( 'Directory listing ...' )

   cSftpURL := cUrl + curl_easy_escape( curlHandle, cFolder )

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   // curl_easy_setopt(curlHandle, HB_CURLOPT_DIRLISTONLY )  //list file name only
   curl_easy_setopt( curlHandle, HB_CURLOPT_DL_BUFF_SETUP, .T. )
   curl_easy_setopt( curlHandle, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | info_( 'Directory listing ... ' + Str( ( nPos / nLen ) * 100, 6, 2 ) + '%' ) } )
   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, .F. )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   curl_easy_setopt( curlHandle, HB_CURLOPT_DL_BUFF_GET, @cDirList )
   aDirList := hb_ATokens( cDirList, Chr( 10 ) )
   ASize( aDirList, Len ( aDirList ) - 1 )

   // get name, size, date, time, atrrib
   FOR EACH cList IN aDirList
      a1List := hb_ATokens( cList )
      IF Len( a1List ) < 7
         LOOP
      ENDIF
      a1Dir := Array( 5 )
      a1Dir[ 5 ] := a1List[ 1 ] // attributes
      a1Dir[ 2 ] := a1List[ 5 ] // size
      // aFile[ F_DATE ] := hb_SToD( cYear + cMonth + cDay )
      IF At( ":", a1List[ 8 ] ) > 0 // time insteed year
         // a1Dir[3] := strzero(AScan(aMonth,{|x|x==a1List[6]})) + "." + a1List [7] //date (w/o year)
         a1Dir[ 3 ] := StrZero( Year( Date() ), 4 ) + '.'
         a1Dir[ 4 ] := a1List[ 8 ] // time
      ELSE
         a1Dir[ 3 ] := a1List[ 8 ] + '.' // date with year
         a1Dir[ 4 ] := "" // no time
      ENDIF
      a1Dir[ 3 ] += StrZero( AScan( aMonth, {| x | x == a1List[ 6 ] } ), 2, 0 ) + '.' + StrZero( Val( a1List[ 7 ] ), 2, 0 )
      cfName := LTrim( cList )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 1 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 2 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 3 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 4 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 5 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 6 ] ) + 1 ) )
      cfName := LTrim( SubStr( cfName, Len( a1List[ 7 ] ) + 1 ) )
      cfName := AllTrim( StrTran ( StrTran ( LTrim(SubStr( cfName, Len( a1List[ 8 ] ) + 1 ) ), Chr(13 ), "" ), Chr( 10 ), "" ) )

      IF 'D' $ Upper( a1List[ 1 ] )
         a1Dir[ 1 ] := "[ " + cfName + " ]" // dir name
         a1Dir[ 2 ] := "<DIR>"
      ELSE
         a1Dir[ 1 ] := cfName // file name
      ENDIF

      IF 'D' $ Upper( a1List[ 1 ] ) .AND. cFName == "." // Skip "." folder
         LOOP
      ENDIF

      AAdd( aDirectory, a1Dir )
   NEXT

   IF ! Empty( cFolder ) // no root
      IF Empty( aDirectory ) .OR. AScan( aDirectory, {| x | x[ 1 ] == "[ .. ]" } ) = 0 // no parrent directory
         AAdd( aDirectory, { "[ .. ]", "<DIR>", "", "", "d" } ) // add parent directory
      ENDIF
   ENDIF

   aDirectory := ASort ( aDirectory, , , {| x, y | IF( 'D' $ Upper( x[ 5 ] ), '0', '1' ) + Lower( x[ 1 ] ) < IF( 'D' $ Upper( y[ 5 ] ), '0', '1' ) + Lower( y[ 1 ] ) } )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   WAIT CLEAR

RETURN cRet

FUNCTION LocalDir ( aDir )
**************************
   LOCAL a1List, aDirectory := {}, a1Dir
   FOR EACH a1List IN aDir
      IF 'D' $ Upper( a1List[ 5 ] ) .AND. a1List[ 1 ] == "." // Skip "." folder
         LOOP
      ENDIF
      a1Dir := Array( 5 )
      IF 'D' $ Upper ( a1List[ 5 ] ) // dir name
         a1Dir[ 1 ] := "[ " + a1List[ 1 ] + " ]"
         a1Dir[ 2 ] := "<DIR>"
      ELSE // file name
         a1Dir[ 1 ] := a1List[ 1 ]
         a1Dir[ 2 ] := AllTrim( Str( a1List[ 2 ] ) )
      ENDIF
      a1Dir[ 3 ] := a1List[ 3 ]
      a1Dir[ 4 ] := a1List[ 4 ]
      a1Dir[ 5 ] := a1List[ 5 ]
      AAdd( aDirectory, a1Dir )
   NEXT

   // soft list
   aDirectory := ASort ( aDirectory, , , {| x, y | IF( 'D' $ Upper( x[ 5 ] ), '0', '1' ) + Lower( x[ 1 ] ) < IF( 'D' $ Upper( y[ 5 ] ), '0', '1' ) + Lower( y[ 1 ] ) } )

RETURN aDirectory

FUNCTION Send_FTP( cFile, cSFile )
**********************************
   LOCAL cRet := "", cSftpURL, nFSize, cRemoteFile
   LOCAL curlErr

   DEFAULT cSFile := cFile

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   IF Empty( cFile ) .OR. ! File ( cFile )
      RETURN "No File " + cFile
   ENDIF

   cRemoteFile := hb_FNameNameExt( cSFile )
   cSftpURL := cUrl + cRemoteDir + cRemoteFile
   nFSize := hb_FSize( cFile )

   info_( 'Uploading ...' )

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   // curl_easy_setopt(curlHandle, HB_CURLOPT_URL, cSftpURL)
   curl_easy_setopt( curlHandle, HB_CURLOPT_UPLOAD, 1 )
   curl_easy_setopt( curlHandle, HB_CURLOPT_UL_FILE_SETUP, cFile )
   curl_easy_setopt( curlHandle, HB_CURLOPT_INFILESIZE_LARGE, nFSize )
   curl_easy_setopt( curlHandle, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | info_( 'Uploading ... ' + Str( ( nPos / nLen ) * 100, 6, 2 ) + '%' ) } )
   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, 0 )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   WAIT CLEAR

RETURN cRet

FUNCTION Recv_FTP( cRemoteFile, cLocalFile )
********************************************
   LOCAL cRet := "", cSftpURL
   LOCAL curlErr

   DEFAULT cLocalFile := cRemoteFile

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   cSftpURL := cUrl + cRemoteDir + cRemoteFile

   info_( 'Downloading -> ' + cSftpURL )

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   // curl_easy_setopt(curlHandle, HB_CURLOPT_URL, cSftpURL)
   curl_easy_setopt( curlHandle, HB_CURLOPT_DOWNLOAD )
   curl_easy_setopt( curlHandle, HB_CURLOPT_DL_FILE_SETUP, cLocalFile )
   curl_easy_setopt( curlHandle, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | info_( 'Downloading ... ' + Str( ( nPos / nLen ) * 100, 6, 2 ) + '%' ) } )
   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, 0 )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   curl_easy_reset( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   WAIT CLEAR

RETURN cRet

FUNCTION Delete_FTP( cRemoteFile )
**********************************
   LOCAL cRet := "", cSftpURL, cCmd, cProtocol, aQuote := {}
   LOCAL curlErr

   DEFAULT cRemoteFile := ""

   IF Empty( cRemoteFile )
      RETURN "No file selected"
   ENDIF

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   info_( 'Deleting file ...' )

   cSftpURL := cUrl + curl_easy_escape( curlHandle, cRemoteDir + cRemoteFile )

   cProtocol := GetProtocol ( cSftpURL )

   SWITCH cProtocol
   CASE "SFTP"
      cCmd := "rm"
      aQuote := { cCmd + ' "' + cRemoteDir + cRemoteFile + '"' }
      EXIT
   CASE "FTP"
      cCmd := "DELE"
      aQuote := { cCmd + ' ' + cRemoteFile }
      EXIT
   ENDSWITCH

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   curl_easy_setopt( curlHandle, HB_CURLOPT_UPLOAD )
   curl_easy_setopt( curlHandle, HB_CURLOPT_UL_NULL_SETUP )
   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, .T. )
   curl_easy_setopt( curlHandle, HB_CURLOPT_POSTQUOTE, aQuote )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   WAIT CLEAR

RETURN cRet

FUNCTION DeleteFolder_FTP( cRemoteFolder )
******************************************
   LOCAL cRet := "", cSftpURL, cCmd, cProtocol, aQuote := {}
   LOCAL curlErr

   DEFAULT cRemoteFolder := ""

   IF Empty( cRemoteFolder )
      RETURN "No folder selected"
   ENDIF

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   info_( 'Deleting folder ...' )

   cSftpURL := cUrl + curl_easy_escape( curlHandle, cRemoteDir )

   cProtocol := GetProtocol ( cSftpURL )

   SWITCH cProtocol
   CASE "SFTP"
      cCmd := "rmdir"
      aQuote := { cCmd + ' "' + cRemoteDir + cRemoteFolder + '"' }
      EXIT
   CASE "FTP"
      cCmd := "RMD"
      aQuote := { cCmd + ' ' + cRemoteFolder }
      EXIT
   ENDSWITCH

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, .T. )
   curl_easy_setopt( curlHandle, HB_CURLOPT_POSTQUOTE, aQuote )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   WAIT CLEAR

RETURN cRet

FUNCTION MakeFolder_FTP( cRemoteFolder )
****************************************
   LOCAL cRet := "", cSftpURL, cCmd, cProtocol, aQuote := {}
   LOCAL curlErr

   DEFAULT cRemoteFolder := ""

   IF Empty( cRemoteFolder )
      RETURN "No folder selected"
   ENDIF

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   info_( 'Making folder ...' )

   cSftpURL := cUrl + curl_easy_escape( curlHandle, cRemoteDir )

   cProtocol := GetProtocol ( cSftpURL )

   SWITCH cProtocol
   CASE "SFTP"
      cCmd := "mkdir"
      aQuote := { cCmd + ' "' + cRemoteDir + cRemoteFolder + '"' }
      EXIT
   CASE "FTP"
      cCmd := "MKD"
      aQuote := { cCmd + ' ' + cRemoteFolder }
      EXIT
   ENDSWITCH

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, .T. )
   curl_easy_setopt( curlHandle, HB_CURLOPT_POSTQUOTE, aQuote )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF
   WAIT CLEAR

RETURN cRet

FUNCTION Rename_FTP( cRemoteFileOld, cRemoteFileNew )
*****************************************************
   LOCAL cRet := "", cSftpURL, cCmd, cProtocol, aQuote := {}
   LOCAL curlErr

   DEFAULT cRemoteFileOld := "", cRemoteFileNew := ""

   IF Empty( curlHandle )
      RETURN "First connect to the server."
   ENDIF

   IF Empty( cRemoteFileOld )
      RETURN "No file selected."
   ENDIF

   IF Empty( cRemoteFileNew )
      RETURN "A new file name has not been set."
   ENDIF

   info_( 'Renaming ...' )

   cSftpURL := cUrl + curl_easy_escape( curlHandle, cRemoteDir )

   cProtocol := GetProtocol ( cSftpURL )

   SWITCH cProtocol
   CASE "SFTP"
      cCmd := "rename"
      aQuote := { cCmd + ' "' + cRemoteDir + cRemoteFileOld + '" "' + cRemoteDir + cRemoteFileNew + '"' }
      EXIT
   CASE "FTP"
      cCmd := "RNFR"
      AAdd( aQuote, cCmd + ' ' + cRemoteFileOld )
      cCmd := "RNTO"
      AAdd( aQuote, cCmd + ' ' + cRemoteFileNew )
      EXIT
   ENDSWITCH

   curl_easy_reset( curlHandle )

   Login_FTP( cSftpURL )

   curl_easy_setopt( curlHandle, HB_CURLOPT_NOPROGRESS, .T. )
   curl_easy_setopt( curlHandle, HB_CURLOPT_POSTQUOTE, aQuote )

   /* Do everything */
   curlErr := curl_easy_perform( curlHandle )

   /* Report any errors */
   IF curlErr # 0
      cRet := curl_easy_strerror( curlErr ) + ' (' + AllTrim( Str( curlErr ) ) + ')'
   ENDIF

   curl_easy_reset( curlHandle )

   WAIT CLEAR

RETURN cRet

FUNCTION GetProtocol( cUrl )
****************************
   DEFAULT cUrl := ""

RETURN Upper( SubStr( cUrl, 1, At( ":", cUrl ) - 1 ) )

PROCEDURE Login_FTP( cSftpURL )
*******************************
   LOCAL AUTH := HB_CURL_CURLSSH_AUTH_NONE
   LOCAL cProtocol := GetProtocol ( cSftpURL )

   curl_easy_setopt( curlHandle, HB_CURLOPT_URL, StrTran ( cSftpURL, "%2F", "/" ) )

   IF ! Empty( UserLogin )
      curl_easy_setopt( curlHandle, HB_CURLOPT_USERNAME, UserLogin )
   ENDIF
   IF ! Empty( UserPass )
      curl_easy_setopt( curlHandle, HB_CURLOPT_PASSWORD, UserPass )
      AUTH += HB_CURL_CURLSSH_AUTH_PASSWORD
   ENDIF

   curl_easy_setopt( curlHandle, HB_CURLOPT_PORT, nPort )

   SWITCH cProtocol
   CASE 'SFTP'
      curl_easy_setopt( curlHandle, HB_CURLOPT_PROTOCOLS, HB_CURLPROTO_FTP )
      IF ( ! Empty( cPrivateKey ) .AND. File ( cPrivateKey ) ) .OR. ( ! Empty( cPublicKey ) .AND. File ( cPublicKey ) )
         AUTH += HB_CURL_CURLSSH_AUTH_PUBLICKEY

         IF ! Empty( cPrivateKey )
            curl_easy_setopt( curlHandle, HB_CURLOPT_SSH_PRIVATE_KEYFILE, cPrivateKey )
         ENDIF
         IF ! Empty( cPublicKey )
            curl_easy_setopt( curlHandle, HB_CURLOPT_SSH_PUBLIC_KEYFILE, cPublicKey )
         ENDIF

         curl_easy_setopt( curlHandle, HB_CURLOPT_KEYPASSWD, "" )
      ENDIF

      curl_easy_setopt( curlHandle, HB_CURLOPT_SSH_AUTH_TYPES, AUTH )

      IF ! Empty ( cPKeyMD5 )
         curl_easy_setopt( curlHandle, HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, cPKeyMD5 )
      ENDIF
      EXIT

   CASE 'FTP'
      curl_easy_setopt( curlHandle, HB_CURLOPT_PROTOCOLS, HB_CURLPROTO_FTP )
      EXIT

   END SWITCH

   curl_easy_setopt( curlHandle, HB_CURLOPT_VERBOSE, .T. )

RETURN

FUNCTION info_( cMess, lWait )
******************************
   LOCAL nTimeIni := hb_MilliSeconds()
   DEFAULT lWait := .F.
   DO EVENTS
   WAIT WINDOW cMess NoWait
   IF lWait
      WHILE( ( hb_MilliSeconds() - nTimeIni ) < 3 * 1000 )
         DO EVENTS
         hb_ReleaseCPU()
      ENDDO
   ENDIF

RETURN NIL

PROCEDURE RemoteListFiles( xPos )
*********************************
   LOCAL cErr
   aRemoteDir := {}
   cErr := Dir_FTP( cRemoteDir, @aRemoteDir )
   IF ! Empty ( cErr )
      MsgStop( cErr )
   ELSE
      RefreshRemoteGrid ( aRemoteDir, xPos )
   ENDIF

RETURN

FUNCTION OnRemoteKey( vKey )
****************************
   LOCAL cFName
   LOCAL xErr, cFNew, xNewPos
   LOCAL lIsDir

   IF ftp.RemoteGrid.VALUE == 0
      RETURN NIL
   ENDIF
   cFName := ftp.RemoteGrid.Cell( ftp.RemoteGrid.VALUE/*CellRowFocused*/, 2 )
   lIsDir := 'D' $ Upper ( ftp.RemoteGrid.Cell( ftp.RemoteGrid.VALUE/*CellRowFocused*/, 6 ) )

   IF lIsDir // extract real folder name
      cFName := SubStr( cFName, 3, Len( cFName ) - 4 )
   ENDIF

   DO CASE
   CASE vKey == VK_RETURN // change folder
      IF lIsDir
         IF cFName = ".." // level up
            xNewPos := hb_ATokens ( cRemoteDir, "/" )
            xNewPos := "[ " + xNewPos[ Len ( xNewPos ) - 1 ] + " ]"
         ENDIF
         cRemoteDir := StrTran( SubStr( TrueName( "/" + cRemoteDir + cFName + "/" ), Len ( TrueName("/" ) ) + 1 ), "\", "/" )
         RemoteListFiles( xNewPos )
      ENDIF

   CASE vKey == VK_F9 // level up
      xNewPos := hb_ATokens ( cRemoteDir, "/" )
      IF Len ( xNewPos ) = 1
         RETURN NIL
      ENDIF
      xNewPos := "[ " + xNewPos[ Len ( xNewPos ) - 1 ] + " ]"
      cRemoteDir := StrTran( SubStr( TrueName( "/" + cRemoteDir + ".." + "/" ), Len ( TrueName("/" ) ) + 1 ), "\", "/" )
      RemoteListFiles( xNewPos )

   CASE vKey == VK_F5 // receive file
      IF ! lIsDir // files only
         cFNew := MyInputBox ( "Copy (download) as:", "Copy (download)", cFName )
         IF ! Empty( cFNew )
            xErr := Recv_FTP ( cFName, cLocalDir + cFNew )
            IF ! Empty ( xErr )
               MsgStop( xErr, 'Error' )
            ELSE
               RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), ftp.LocalGrid.Cell( ftp.LocalGrid.VALUE/*CellRowFocused*/, 1 ) )
            ENDIF
         ENDIF
      ENDIF

   CASE vKey == VK_F6 // rename
      cFNew := MyInputBox ( "Enter new name", "Rename", cFName )
      IF ! Empty( cFNew ) .AND. !( cfNew == cfName )
         xErr := Rename_FTP( cFName, cFNew )
         IF ! Empty ( xErr )
            MsgStop( xErr, 'Error' )
         ELSE
            RemoteListFiles( IF( lIsDir, "[ " + cFNew + " ]", cFNew ) )
         ENDIF
      ENDIF

   CASE vKey == VK_F7 // make remote folder
      cFNew := MyInputBox ( "Name new folder", "New remote folder", "" )
      IF ! Empty( cFNew )
         xErr := MakeFolder_FTP ( cFNew )
         IF ! Empty ( xErr )
            MsgStop( xErr, 'Error' )
         ELSE
            RemoteListFiles( "[ " + cFNew + " ]" )
         ENDIF
      ENDIF

   CASE vKey == VK_F8 .AND. MsgYesNo( 'Are you sure you want to delete "' + cFName + '" ?', "Confirm", .T. ) // delete
      IF ! lIsDir // file
         xErr := Delete_FTP( cFName )
         IF ! Empty ( xErr )
            MsgStop( xErr, 'Error' )
         ELSE
            RemoteListFiles( ftp.RemoteGrid.VALUE/*CellRowFocused*/ )
         ENDIF
      ELSE // remove remote folder
         xErr := DeleteFolder_FTP( cFName )
         IF ! Empty ( xErr )
            MsgStop( xErr, 'Error' )
         ELSE
            RemoteListFiles( ftp.RemoteGrid.VALUE/*CellRowFocused*/ )
         ENDIF
      ENDIF
   ENDCASE

RETURN NIL

FUNCTION OnLocalKey( vKey )
***************************
   LOCAL cFName := ftp.LocalGrid.Cell( ftp.LocalGrid.VALUE/*CellRowFocused*/, 2 )
   LOCAL lIsDir := 'D' $ Upper ( ftp.LocalGrid.Cell( ftp.LocalGrid.VALUE/*CellRowFocused*/, 6 ) )
   LOCAL xErr, cFNew, xNewPos := 1

   IF lIsDir // extract real folder name
      cFName := SubStr( cFName, 3, Len( cFName ) - 4 )
   ENDIF

   DO CASE
   CASE vKey == VK_RETURN // change folder
      IF lIsDir
         IF cFName = ".." // level up
            xNewPos := hb_ATokens ( cLocalDir, "\" )
            xNewPos := "[ " + xNewPos[ Len ( xNewPos ) - 1 ] + " ]"
         ENDIF
         cLocalDir := hb_PathNormalize( cLocalDir + cFName + "\" ) // folder name
         RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), xNewPos )
      ENDIF

   CASE vKey == VK_F9 // level up
      xNewPos := hb_ATokens ( cLocalDir, "\" )
      IF Len ( xNewPos ) = 2
         RETURN NIL
      ENDIF
      xNewPos := "[ " + xNewPos[ Len ( xNewPos ) - 1 ] + " ]"
      cLocalDir := hb_PathNormalize( cLocalDir + ".." + "\" ) // folder ..
      RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), xNewPos )

   CASE vKey == VK_F5 // send file
      IF ! lIsDir // files only
         cFNew := MyInputBox ( "Copy (upload) as:", "Copy (upload)", cFName )
         IF ! Empty( cFNew )
            xErr := Send_FTP ( cLocalDir + cFName, cFNew )
            IF ! Empty ( xErr )
               MsgStop( xErr, 'Error' )
            ELSE
               RemoteListFiles( ftp.RemoteGrid.Cell( ftp.RemoteGrid.VALUE/*CellRowFocused*/, 1 ) )
            ENDIF
         ENDIF
      ENDIF

   CASE vKey == VK_F6 // rename
      cFNew := MyInputBox ( "Enter new name", "Rename", cFName )
      IF ! Empty( cFNew ) .AND. !( cfNew == cfName )
         xErr := hb_vfRename( cLocalDir + cFname, cLocalDir + cFNew )
         IF xErr # 0
            MsgStop( "DOS error " + hb_ValToStr( xErr ), 'Error' )
         ELSE
            RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), IF( lIsDir, "[ " + cFNew + " ]", cFNew ) )
         ENDIF
      ENDIF

   CASE vKey == VK_F7 // Make directory
      cFNew := MyInputBox ( "Name new folder", "New folder", "" )
      IF ! Empty( cFNew )
         xErr := hb_DirCreate( cLocalDir + cFNew )
         IF xErr # 0
            MsgStop( "DOS error " + hb_ValToStr( xErr ), 'Error' )
         ELSE
            RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), "[ " + cFNew + " ]" )
         ENDIF
      ENDIF


   CASE vKey == VK_F8 .AND. MsgYesNo( 'Are you sure you want to delete "' + cFName + '" ?', "Confirm", .T. ) // delete
      IF lIsDir // folder
         xErr := hb_DirDelete( cLocalDir + cFName )
         IF xErr # 0
            MsgStop( "DOS error " + hb_ValToStr( xErr ), 'Error' )
         ELSE
            RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), ftp.LocalGrid.VALUE/*CellRowFocused*/ )
         ENDIF
      ELSE
         IF ! hb_FileDelete( cLocalDir + cFName )
            MsgStop( "DOS error " + hb_ValToStr( FError() ), 'Error' )
         ELSE
            RefreshLocalGrid ( LocalDir( Directory( cLocalDir, "HSD" ) ), ftp.LocalGrid.VALUE/*CellRowFocused*/ )
         ENDIF

      ENDIF

   ENDCASE

RETURN NIL

FUNCTION ReleaseFTP()
*********************
   IF ftp.FocusedControl == 'ComboDrive'
      ComboBoxDropdownClose( GetControlHandle ( 'ComboDrive', 'ftp' ) )
      _PushKey ( VK_TAB )
      RETURN NIL
   ENDIF
   IF MsgYesNo ( "Exit FTP Navigator?", "Confirm", .T. )
      ThisWindow.RELEASE
   ENDIF

RETURN NIL

FUNCTION GetDrvLetter()
***********************

RETURN UPPER ( Left ( cLocalDir, 2 ) )

FUNCTION DriveList()
********************
   ComboBoxDropdown( GetControlHandle ( 'ComboDrive', 'ftp' ) )
   ftp.ComboDrive.SetFocus

RETURN NIL

FUNCTION ComboBoxDropdown( nHandle )
************************************
   DEFAULT nHandle := GetControlHandle ( this.NAME, thiswindow.name )
   ComboBoxShowDropDown( nHandle )

RETURN NIL

FUNCTION ComboBoxDropdownClose( nHandle )
*****************************************
   DEFAULT nHandle := GetControlHandle ( this.NAME, thiswindow.name )
   ComboBoxShowDropDownClose( nHandle )

RETURN NIL

FUNCTION ConfirmToConnect()
***************************
   LOCAL lConnect := .F., cFile

   DEFINE WINDOW ConnectParams AT 197, 394 WIDTH 445 HEIGHT 335 TITLE "Settings for connection to the server" MODAL

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

      @ 20, 20 LABEL LabelURL VALUE "Host URL:" AUTOSIZE
      @ 50, 20 LABEL LabelPort VALUE "Port:" AUTOSIZE
      @ 80, 20 LABEL LabelUser VALUE "User name:" AUTOSIZE
      @ 110, 20 LABEL LabelPass VALUE "Password:" AUTOSIZE
      @ 140, 20 LABEL LabelMD5 VALUE "Host Public Key MD5:" AUTOSIZE
      @ 170, 20 LABEL LabelPubK VALUE "Host Public Key file:" AUTOSIZE
      @ 200, 20 LABEL LabelPrvK VALUE "Host Private Key file:" AUTOSIZE

      @ 20, 130 TEXTBOX TextUrl WIDTH 230 VALUE cUrl
      @ 50, 130 TEXTBOX TextPort WIDTH 50 VALUE nPort NUMERIC
      @ 80, 130 TEXTBOX TextUser WIDTH 230 VALUE UserLogin
      @ 110, 130 TEXTBOX TextPass WIDTH 230 VALUE UserPass PASSWORD
      @ 140, 130 TEXTBOX TextMD5 WIDTH 230 VALUE cPKeyMD5
      @ 170, 130 TEXTBOX TextPubK WIDTH 230 VALUE cPublicKey
      @ 200, 130 TEXTBOX TextPrvK WIDTH 230 VALUE cPrivateKey

      @ 170, 370 BUTTON ButtonPubK CAPTION "..." ;
         ACTION ( cFile := Getfile ( { { 'All Files', '*.*' } }, 'Select Public Key File', GetCurrentFolder() + "\", .F., .T. ), ;
         IF( ! Empty( cFile ), ConnectParams.TextPubK.VALUE := cFile, Nil ) ) ;
         WIDTH 30 HEIGHT 20
      @ 200, 370 BUTTON ButtonPrvK CAPTION "..." ;
         ACTION ( cFile := Getfile ( { { 'All Files', '*.*' } }, 'Select Private Key File', GetCurrentFolder() + "\", .F., .T. ), ;
         IF( ! Empty( cFile ), ConnectParams.TextPrvK.VALUE := cFile, Nil ) ) ;
         WIDTH 30 HEIGHT 20

      @ 250, 90 BUTTON ButtonOk CAPTION "Connect" ;
         ACTION ( lConnect := .T., ;
         cUrl := ConnectParams.TextUrl.VALUE, ;
         nPort := ConnectParams.TextPort.VALUE, ;
         UserLogin := ConnectParams.TextUser.VALUE, ;
         UserPass := ConnectParams.TextPass.VALUE, ;
         cPKeyMD5 := ConnectParams.TextMD5.VALUE, ;
         cPublicKey := ConnectParams.TextPubK.VALUE, ;
         cPrivateKey := ConnectParams.TextPrvK.VALUE, ;
         ThisWindow.Release ) ;
         WIDTH 100
      @ 250, 250 BUTTON ButtonCancel CAPTION "Cancel" ACTION ThisWindow.RELEASE WIDTH 100

      ConnectParams.ButtonOk.SetFocus

   END WINDOW

   CENTER WINDOW ConnectParams
   ACTIVATE WINDOW ConnectParams

RETURN lConnect

FUNCTION MyInputBox( cMess, cTitle, cValue )
********************************************
   LOCAL cRet := ""
   DEFAULT cMess := "", cTitle := ""

   IF HB_ISNIL( cValue )
      RETURN cRet
   ENDIF

   DEFINE WINDOW MyInpBox AT 0, 0 WIDTH 400 HEIGHT 110 TITLE cTitle MODAL NOSIZE

      ON KEY ESCAPE ACTION ReleaseMyInpBox()

      @ 5, 20 LABEL Label1 VALUE cMess AUTOSIZE
      @ 25, 20 TEXTBOX Text1 WIDTH 350 HEIGHT 18 VALUE cValue ;
         ON ENTER ( cRet := MyInpBox.Text1.VALUE, ReleaseMyInpBox() )

      @ 50, 190 BUTTON ButtonOk CAPTION "&OK" ;
         ACTION ( cRet := MyInpBox.Text1.VALUE, ReleaseMyInpBox() ) ;
         WIDTH 80 HEIGHT 24
      @ 50, 290 BUTTON ButtonCancel CAPTION "&Cancel" ACTION ReleaseMyInpBox() WIDTH 80 HEIGHT 24

      MyInpBox.Text1.SetFocus

   END WINDOW

   CENTER WINDOW MyInpBox
   ACTIVATE WINDOW MyInpBox

RETURN cRet

FUNCTION ReleaseMyInpBox()
**************************
   MyInpBox.RELEASE

RETURN NIL

******************************************

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"

HB_FUNC ( COMBOBOXSHOWDROPDOWN )

{
    SendMessage((HWND) hb_parnl (1), CB_SHOWDROPDOWN, (WPARAM)(int) 1, (LPARAM)(int) 0);
}

HB_FUNC ( COMBOBOXSHOWDROPDOWNCLOSE )

{
    SendMessage((HWND) hb_parnl (1), CB_SHOWDROPDOWN, (WPARAM)(int) 0, (LPARAM)(int) 0);
}

#pragma ENDDUMP
