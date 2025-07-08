/*
 Made By Sylvain Robert 2004-01-23

 Modified for MiniGUI by Grigory Filatov 2007-11-03

 This Example show you how to ZIP the all files from the current Directory
 it also ZIP all files in subdirectory.
*/

#include <minigui.ch>

*------------------------------------------------------------------------------*
PROCEDURE MAIN
*------------------------------------------------------------------------------*

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 400 HEIGHT 215 ;
         TITLE "Backup demo with using of a password" ;
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
         ACTION CreateZip( "abc" )
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 140
         COL 205
         WIDTH 150
         HEIGHT 28
         CAPTION "&Recover Backup"
         ACTION UnZip()
      END BUTTON

      DEFINE PROGRESSBAR ProgressBar_0
         ROW 20
         COL 45
         WIDTH 310
         HEIGHT 30
         RANGEMIN 0
         RANGEMAX 100
         VALUE 0
         FORECOLOR { 0, 130, 0 }
         BACKCOLOR { 201, 201, 201 }
      END PROGRESSBAR

      DEFINE PROGRESSBAR ProgressBar_1
         ROW 60
         COL 45
         WIDTH 310
         HEIGHT 30
         RANGEMIN 0
         RANGEMAX 10
         VALUE 0
         FORECOLOR { 0, 130, 0 }
         BACKCOLOR { 201, 201, 201 }
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
FUNCTION CreateZip( cPass )
*------------------------------------------------------------------------------*
   LOCAL aDir := Directory( "*.*", "D" ), aFiles := {}, nLen, lSuccess
   LOCAL cPath := CurDrive() + ":\" + CurDir() + "\"
   LOCAL cTarget, NameZip := "Backup.Zip"

   cTarget := GetFolder( "Select the destination folder of the save",,, .F. )

   IF Empty( cTarget )
      RETURN NIL
   ELSE
      cTarget := hb_DirSepAdd( cTarget )
      cTarget += NameZip
   ENDIF

   FillFiles( aFiles, aDir, cPath )

   IF ( nLen := Len( aFiles ) ) > 0

      Form_1.ProgressBar_0.VALUE := 0
      Form_1.ProgressBar_1.RANGEMAX := nLen - lton( File( 'Backup.Zip' ) )

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      IF Empty( cPass )

         COMPRESS aFiles ;
            TO cTarget ;
            BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
            LEVEL 9 ;
            OVERWRITE ;
            STOREPATH ;
            FILEPROGRESS {| nPos, nTotal | ProgressFile( nPos, nTotal, .T. ) } ;
            RESULT lSuccess

      ELSE

         COMPRESS aFiles ;
            TO cTarget ;
            BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
            LEVEL 9 ;
            PASSWORD cPass ;
            OVERWRITE ;
            STOREPATH ;
            FILEPROGRESS {| nPos, nTotal | ProgressFile( nPos, nTotal, .T. ) } ;
            RESULT lSuccess

      ENDIF

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }

      Form_1.Label_1.VALUE := iif( lSuccess, 'Backup is finished', 'Backup Error' )

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ProgressUpdate( nPos, cFile, lShowFileName )
*------------------------------------------------------------------------------*

   DEFAULT lShowFileName := .F.

   Form_1.ProgressBar_1.VALUE := nPos
   Form_1.Label_1.VALUE := cFileNoPath( cFile )

   IF lShowFileName
      InkeyGUI()
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ProgressFile( nPos, nTotal, lShowFileProgress )
*------------------------------------------------------------------------------*

   DEFAULT lShowFileProgress := .F.

   Form_1.ProgressBar_0.VALUE := ( nPos / nTotal ) * 100

   IF lShowFileProgress
      InkeyGUI( 50 )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION UnZip()
*------------------------------------------------------------------------------*
   LOCAL cCurDir := GetCurrentFolder(), cArchive, lSuccess

   cArchive := Getfile ( { { 'Zip Files', '*.ZIP' } }, 'Open File', cCurDir, .F., .T. )

   IF ! Empty( cArchive )

      Form_1.ProgressBar_0.VALUE := 0

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      IF hb_ZipWithPassword( cArchive )
         Form_1.ProgressBar_1.RANGEMAX := 2

         UNCOMPRESS cArchive ;
            EXTRACTPATH cCurDir + "\BackUp" ;
            FILEMASK "demo.*" ;
            BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
            PASSWORD "abc" ;
            RESULT lSuccess ;
            CREATEDIR
      ELSE
         Form_1.ProgressBar_1.RANGEMAX := hb_GetFileCount( cArchive )

         UNCOMPRESS cArchive ;
            EXTRACTPATH cCurDir + "\BackUp" ;
            BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
            FILEPROGRESS {| nPos, nTotal | ProgressFile( nPos, nTotal, .T. ) } ;
            RESULT lSuccess ;
            CREATEDIR
      ENDIF

      InkeyGUI( 50 )

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }

      Form_1.Label_1.VALUE := iif( lSuccess, 'Restoration of Backup is finished', 'Restoration Error' )

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION FillFiles( aFiles, cDir, cPath )
*------------------------------------------------------------------------------*
   LOCAL aSubDir, cItem

   FOR cItem := 1 TO Len( cDir )

      IF cDir[ cItem ][ 5 ] <> "D"

         AAdd( aFiles, cPath + cDir[ cItem ][ 1 ] )

      ELSEIF cDir[ cItem ][ 1 ] <> "." .AND. cDir[ cItem ][ 1 ] <> ".."

         aSubDir := Directory( cPath + cDir[ cItem ][ 1 ] + "\*.*", "D" )
         aFiles := FillFiles( aFiles, aSubdir, cPath + cDir[ cItem ][ 1 ] + "\" )

      ENDIF

   NEXT

RETURN aFiles
