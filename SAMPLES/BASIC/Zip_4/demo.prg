/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Based on the built-in ZIP Support in Windows.
 * The usage was explained by Jimmy on HMG forum 04/Apr/2020
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include <minigui.ch>
#include "directry.ch"
#include "fileio.ch"


#command COMPRESS [ FILES ] <afiles> ;
      TO <zipfile> ;
      BLOCK <block> ;
      [ <ovr: OVERWRITE> ] ;
      => ;
      COMPRESSFILES ( <zipfile>, <afiles>, <block>, <.ovr.> )


#command UNCOMPRESS [ FILE ] <zipfile> ;
      EXTRACTPATH <extractpath> ;
      [ BLOCK <block> ] ;
      [ <createdir: CREATEDIR> ] ;
      => ;
      UNCOMPRESSFILES ( <zipfile>, <block>, <extractpath>, <.createdir.> )


*------------------------------------------------------------------------------*
PROCEDURE Main()
*------------------------------------------------------------------------------*

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 400 HEIGHT 215 ;
         TITLE "Backup" ;
         ICON "demo.ico" ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         FONT "Arial" SIZE 9

      DEFINE BUTTON Button_1
         ROW 140
         COL 45
         WIDTH 150
         HEIGHT 30
         CAPTION "&Create Backup"
         ACTION CreateZip()
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 140
         COL 205
         WIDTH 150
         HEIGHT 28
         CAPTION "&Recover Backup"
         ACTION UnZip()
      END BUTTON

      DEFINE PROGRESSBAR ProgressBar_1
         ROW 60
         COL 45
         WIDTH 310
         HEIGHT 30
         RANGEMIN 0
         RANGEMAX 10
         VALUE 0
      END PROGRESSBAR

      DEFINE LABEL Label_1
         ROW 100
         COL 25
         WIDTH 350
         HEIGHT 20
         VALUE ""
         FONTNAME "Arial"
         FONTSIZE 10
         TOOLTIP ""
         FONTBOLD .T.
         TRANSPARENT .T.
         CENTERALIGN .T.
      END LABEL

      ON KEY ESCAPE ACTION Form_1.RELEASE

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

*------------------------------------------------------------------------------*
FUNCTION CreateZip()
*------------------------------------------------------------------------------*

   LOCAL aDir := Directory( "*.*" ), aFiles := {}, nLen
   LOCAL cPath := GetStartUpFolder() + "\"
   LOCAL cFolder, cTarget, cBackup
   LOCAL NameZip := "Backup.zip"

   cFolder := GetFolder( "Select the destination folder of the save", cPath,, .F. )

   IF Empty( cFolder )
      RETURN NIL
   ELSE
      cBackup := cFolder + "\BackUp"
      cTarget := hb_DirSepAdd( cBackup )
      cTarget += NameZip
   ENDIF

   IF ! hb_DirExists( cBackup )
      hb_DirBuild( cBackup )
   ENDIF

   FillFiles( aFiles, aDir, cPath )

   IF ( nLen := Len( aFiles ) ) > 0

      Form_1.ProgressBar_1.RANGEMIN := 0
      Form_1.ProgressBar_1.RANGEMAX := nLen

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      COMPRESS aFiles ;
         TO cTarget ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         OVERWRITE

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }

      IF File( cTarget )
         Form_1.Label_1.Value := 'Backup is finished'
      ELSE
         MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 255, 0, 0 }
         Form_1.Label_1.Value := 'An error has been encountered! Backup Failed'
      ENDIF

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ProgressUpdate( nPos, cFile, lShowFileName )
*------------------------------------------------------------------------------*

   DEFAULT lShowFileName := .F.

   Form_1.ProgressBar_1.Value := nPos
   Form_1.Label_1.Value := cFileNoPath( cFile )

   IF lShowFileName
      InkeyGUI( 250 )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION UnZip()
*------------------------------------------------------------------------------*

   LOCAL cCurDir := GetCurrentFolder(), cArchive

   cArchive := Getfile ( { { 'ZIP Files', '*.zip' } }, 'Open File', cCurDir, .F., .T. )

   IF ! Empty( cArchive )

      Form_1.ProgressBar_1.RANGEMIN := 0
      Form_1.ProgressBar_1.RANGEMAX := GetZipFileCount( cArchive )

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }
      Form_1.Label_1.Value := ''

      UNCOMPRESS cArchive ;
         EXTRACTPATH cCurDir + "\BackUp" ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         CREATEDIR

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }
      Form_1.Label_1.Value := 'Restoration of Backup is finished'

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION FillFiles( aFiles, cDir, cPath )
*------------------------------------------------------------------------------*

   LOCAL cItem

   FOR cItem := 1 TO Len( cDir )
      IF cDir[ cItem ][ F_ATTR ] <> "D"
         AAdd( aFiles, cPath + cDir[ cItem ][ F_NAME ] )
      ENDIF
   NEXT

RETURN aFiles

*------------------------------------------------------------------------------*
FUNCTION GetZipFileCount ( zipfile )
*------------------------------------------------------------------------------*

RETURN GetZipObject( zipfile ):items():Count()

*------------------------------------------------------------------------------*
STATIC FUNCTION GetZipObject( zipfile )
*------------------------------------------------------------------------------*

   IF _SetGetGlobal( "oShell" ) == NIL

      STATIC oShell AS GLOBAL VALUE CreateObject( "Shell.Application" )
      STATIC oWinZip AS GLOBAL VALUE _SetGetGlobal( "oShell" ):NameSpace( ZipFile )

   ENDIF

RETURN _SetGetGlobal( "oWinZip" )

#define FOF_MULTIDESTFILES         0x0001
#define FOF_CONFIRMMOUSE           0x0002
#define FOF_SILENT                 0x0004  // don't create progress/report
#define FOF_RENAMEONCOLLISION      0x0008
#define FOF_NOCONFIRMATION         0x0010  // Don't prompt the user
#define FOF_ALLOWUNDO              0x0040
#define FOF_FILESONLY              0x0080  // on *.*, do only files
#define FOF_SIMPLEPROGRESS         0x0100  // means don't show names of files
#define FOF_NOCONFIRMMKDIR         0x0200  // don't confirm making any needed dirs
#define FOF_NOERRORUI              0x0400  // don't put up error UI
#define FOF_NOCOPYSECURITYATTRIBS  0x0800  // don't copy NT file Security Attributes
#define FOF_NORECURSION            0x1000  // don't recurse into directories

*------------------------------------------------------------------------------*
PROCEDURE UNCOMPRESSFILES ( zipfile, block, extractpath, createdir )
*------------------------------------------------------------------------------*

   LOCAL oZip
   LOCAL oNameDest
   LOCAL aFiles, cFile
   LOCAL i

   IF createdir .AND. ! hb_DirExists( ExtractPath )
      hb_DirBuild( ExtractPath )
   ENDIF

   DO EVENTS

   oZip := GetZipObject( zipfile )

   oNameDest := _SetGetGlobal( "oShell" ):NameSpace( ExtractPath )

   oNameDest:CopyHere( oZip:items(), hb_BitOr( FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_SILENT ) )

   IF ValType( block ) == 'B'

      aFiles := Directory( ExtractPath + "\*.*" )

      FOR i := 1 TO Len( aFiles )

         cFile := cFileName( aFiles[ i ][ F_NAME ] )

         Eval( block, cFile, i )

      NEXT i

   ENDIF

   ASSIGN GLOBAL oZip := NIL
   ASSIGN GLOBAL oShell := NIL

RETURN

#define ZIP_HEADER_ARRAY     { 80, 75, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define ZIP_HEADER_ARRAY_LEN 24

*------------------------------------------------------------------------------*
PROCEDURE COMPRESSFILES ( zipfile, afiles, block, ovr )
*------------------------------------------------------------------------------*

   LOCAL oZip
   LOCAL oFolder
   LOCAL cFile, cDir, cCurDir
   LOCAL i
   LOCAL nCount
   LOCAL nFiles
   LOCAL nHandle

   IF ovr == .T.

      IF File( zipfile )
         DELETE FILE ( zipfile )
      ENDIF

   ENDIF

   // create empty ZIP and write Header
   //
   nHandle := FCreate( ZipFile, FC_NORMAL )

   IF nHandle == F_ERROR
      RETURN
   ENDIF

   FOR i := 1 TO ZIP_HEADER_ARRAY_LEN
      FWrite( nHandle, Chr( ZIP_HEADER_ARRAY[ i ] ) )
   NEXT

   FClose( nHandle )

   // create COM Object
   //
   oZip := GetZipObject( zipfile )

   IF ValType( oZip ) != "O"
      RETURN
   ENDIF

   cCurDir := ""

   FOR i := 1 TO Len( aFiles )

      cFile := cFileName( aFiles[ i ] )

      Eval( block, cFile, i )

      cDir := cFilePath( aFiles[ i ] )

      IF cCurDir <> cDir

         oFolder := _SetGetGlobal( "oShell" ):NameSpace( cDir )
         cCurDir := cDir

         oZip:CopyHere( oFolder:items(), hb_BitOr( FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR, FOF_FILESONLY, FOF_SIMPLEPROGRESS ) )

      ENDIF

      // wait until all files are written
      //
      nCount := 0

      DO WHILE .T.

         nFiles := oZip:items():Count()
         IF nFiles >= i
            EXIT
         ENDIF

         DO EVENTS

         IF ++nCount > 50
            EXIT
         ENDIF

      ENDDO

   NEXT i

   ASSIGN GLOBAL oZip := NIL
   ASSIGN GLOBAL oShell := NIL

RETURN
