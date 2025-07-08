/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2008 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Based on HBMZIP Harbour contribution library samples
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov - 2008-2012
*/

#include <hmg.ch>

*------------------------------------------------------------------------------*
PROCEDURE Main
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
         FORECOLOR { 0, 130, 0 }
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
   LOCAL aDir := Directory( "f*.txt", "D" ), aFiles := {}, nLen
   LOCAL cPath := CurDrive() + ":\" + CurDir() + "\"
   LOCAL cTarget, NameZip := "ZipTest.Zip"

   cTarget := GetFolder( "Select the destination folder of the save",,, .F. )

   IF Empty( cTarget )
      RETURN NIL
   ELSE
      cTarget := hb_DirSepAdd( cTarget )
      cTarget += NameZip
   ENDIF

   FillFiles( aFiles, aDir, cPath )

   IF ( nLen := Len( aFiles ) ) > 0
      Form_1.ProgressBar_1.RANGEMIN := 0
      Form_1.ProgressBar_1.RANGEMAX := nLen

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      COMPRESS aFiles ;
         TO cTarget ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         PASSWORD "mypass" ;
         STOREPATH ;
         OVERWRITE

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }

      IF File( cTarget )
         Form_1.Label_1.VALUE := 'Backup is finished'
      ELSE
         MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 255, 0, 0 }
         Form_1.Label_1.VALUE := 'An error has been encountered! Backup Failed'
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
      InkeyGUI( 100 )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION UnZip()
*------------------------------------------------------------------------------*
   LOCAL cCurDir := GetCurrentFolder(), cArchive

   cArchive := Getfile ( { { 'Zip Files', '*.ZIP' } }, 'Open File', cCurDir, .F., .T. )

   IF ! Empty( cArchive )
      Form_1.ProgressBar_1.RANGEMIN := 0
      Form_1.ProgressBar_1.RANGEMAX := hb_GetFileCount( cArchive )
      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      UNCOMPRESS cArchive ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         PASSWORD "mypass"

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }
      Form_1.Label_1.VALUE := 'Restoration of Backup is finished'
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
