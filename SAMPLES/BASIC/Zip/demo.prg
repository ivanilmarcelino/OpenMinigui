/*

  Made By Sylvain Robert 2004-01-23

  Modified for MiniGUI by Grigory Filatov 2007-10-26

  This Example show you how to ZIP the all files from the current Directory
  it also ZIP all files in subdirectory.

*/

#include <minigui.ch>

*------------------------------------------------------------------------------*
PROCEDURE Main
*------------------------------------------------------------------------------*

   IF File( "BACKUP.ZIP" )
      DELETE FILE BACKUP.ZIP
   ENDIF

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 400 HEIGHT 215 ;
      TITLE "Backup demo by hbziparc library" ;
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

   ON KEY ESCAPE ACTION Form_1.Release

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

*------------------------------------------------------------------------------*
FUNCTION CreateZip()
*------------------------------------------------------------------------------*
   LOCAL aDir := Directory( "*.*", "D" ), aFiles := {}, nLen
   LOCAL cPath := CurDrive() + ":\" + CurDir() + "\"

   FillFiles( aFiles, aDir, cPath )

   IF ( nLen := Len( aFiles ) ) > 0

      Form_1.ProgressBar_1.RangeMin := 1
      Form_1.ProgressBar_1.RangeMax := nLen

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      COMPRESS aFiles ;
         TO 'BACKUP.ZIP' ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         LEVEL 9 ;
         OVERWRITE ;
         ;// STOREPATH

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }

      Form_1.Label_1.Value := 'Backup is finished'

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ProgressUpdate( nPos, cFile, lShowFileName )
*------------------------------------------------------------------------------*

   DEFAULT lShowFileName := .F.

   Form_1.ProgressBar_1.Value := nPos
   Form_1.Label_1.Value := cFileNoPath( cFile )

   IF lShowFileName
      InkeyGUI( 50 )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION UnZip()
*------------------------------------------------------------------------------*
   LOCAL cCurDir := GetCurrentFolder(), cArchive

   cArchive := Getfile ( { { 'Zip Files', '*.ZIP' } }, 'Open File', cCurDir, .F., .T. )

   IF !Empty( cArchive ) .AND. hb_IsZipFile( cArchive )

      IF hb_ZipWithPassword( cArchive )
         MsgStop( 'A password is required for restoration' )
         RETURN NIL
      ENDIF

      Form_1.ProgressBar_1.RangeMin := 0
      Form_1.ProgressBar_1.RangeMax := Len( hb_GetFilesInZip( cArchive ) )

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

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
   LOCAL aSubDir, cItem

   FOR cItem := 1 TO Len( cDir )

      IF cDir[ cItem ][ 5 ] <> "D"

         IF hb_FNameExt( cDir[ cItem ][ 1 ] ) <> ".ZIP"
            AAdd( aFiles, cPath + cDir[ cItem ][ 1 ] )
         ENDIF

      ELSEIF cDir[ cItem ][ 1 ] <> "." .AND. cDir[ cItem ][ 1 ] <> ".."

         aSubDir := Directory( cPath + cDir[ cItem ][ 1 ] + "\*.*", "D" )
         aFiles := FillFiles( aFiles, aSubdir, cPath + cDir[ cItem ][ 1 ] + "\" )

      ENDIF

   NEXT

RETURN aFiles
