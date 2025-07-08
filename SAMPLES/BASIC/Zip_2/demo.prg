/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2008 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Based on freeware Zip Component by Belus Technology
*/

#include <minigui.ch>

#command COMPRESS [ FILES ] <afiles> ;
      TO <zipfile> ;
      BLOCK <block> ;
      [ LEVEL <level> ] ;
      [ <ovr: OVERWRITE> ] ;
      [ <spt: STOREPATH> ] ;
      => ;
      COMPRESSFILES ( <zipfile>, <afiles>, <level>, <block>, <.ovr.>, <.spt.> )


#command UNCOMPRESS [ FILE ] <zipfile> ;
      EXTRACTPATH <extractpath> ;
      [ BLOCK <block> ] ;
      [ <createdir: CREATEDIR> ] ;
      [ PASSWORD <password> ] ;
      => ;
      UNCOMPRESSFILES ( <zipfile>, <block>, <extractpath> )


*------------------------------------------------------------------------------*
PROCEDURE Main()
*------------------------------------------------------------------------------*

   IF IsWinNT() .AND. ! wapi_IsUserAnAdmin()
      MsgStop( 'This Program Runs In An Admin Mode Only!', 'Stop' )
      RETURN
   ENDIF

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 400 HEIGHT 215 ;
         TITLE "Backup" ;
         ICON "demo.ico" ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         ON INIT RegActiveX() ;
         ON RELEASE UnRegActiveX() ;
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

      ON KEY ESCAPE ACTION Form_1.RELEASE

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

*------------------------------------------------------------------------------*
FUNCTION CreateZip()
*------------------------------------------------------------------------------*

   LOCAL aDir := Directory( "xzip.*", "D" ), aFiles := {}, nLen
   LOCAL cPath := CurDrive() + ":\" + CurDir() + "\"

   FillFiles( aFiles, aDir, cPath )

   IF ( nLen := Len( aFiles ) ) > 0
      Form_1.ProgressBar_1.RANGEMIN := 1
      Form_1.ProgressBar_1.RANGEMAX := nLen
      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      COMPRESS aFiles ;
         TO 'Backup.Zip' ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         LEVEL 9 ;
         OVERWRITE

      InkeyGUI( 250 )
      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 255 }
      Form_1.Label_1.VALUE := 'Backup is finished'
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ProgressUpdate( nPos, cFile, lShowFileName )
*------------------------------------------------------------------------------*

DEFAULT lShowFileName := .F.

   Form_1.ProgressBar_1.VALUE := nPos
   Form_1.Label_1.VALUE := cFileNoPath( cFile )

   IF lShowFileName
      InkeyGUI( 250 )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION UnZip()
*------------------------------------------------------------------------------*

   LOCAL cCurDir := GetCurrentFolder(), cArchive

   cArchive := Getfile ( { { 'Zip Files', '*.ZIP' } }, 'Open File', cCurDir, .F., .T. )

   IF ! Empty( cArchive )
      Form_1.ProgressBar_1.RANGEMIN := 0
      Form_1.ProgressBar_1.RANGEMAX := GetFilesCountInZip( cArchive )
      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 0, 0, 0 }

      UNCOMPRESS cArchive ;
         EXTRACTPATH cCurDir + "\BackUp" ;
         BLOCK {| cFile, nPos | ProgressUpdate( nPos, cFile, .T. ) } ;
         CREATEDIR

      InkeyGUI( 250 )
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

*------------------------------------------------------------------------------*
FUNCTION GetFilesCountInZip ( zipfile )
*------------------------------------------------------------------------------*

RETURN GetZipObject():Contents( zipfile ):Count

*------------------------------------------------------------------------------*
STATIC FUNCTION GetZipObject()
*------------------------------------------------------------------------------*

   IF _SetGetGlobal( "ObjZip" ) == NIL
      STATIC ObjZip AS GLOBAL VALUE CreateObject( "XStandard.Zip" )
   ENDIF

RETURN _SetGetGlobal( "ObjZip" )

*------------------------------------------------------------------------------*
PROCEDURE UNCOMPRESSFILES ( zipfile, block, extractpath )
*------------------------------------------------------------------------------*

   LOCAL oZip
   LOCAL nCount
   LOCAL objItem
   LOCAL i

   IF ! IsZipFile( zipfile )
      MsgStop( "The file " + zipfile + " is not a valid ZIP file!", , , .F. )

      MODIFY CONTROL Label_1 OF Form_1 FONTCOLOR { 255, 0, 0 }

      Form_1.Label_1.Value := 'Backup Error'
      InkeyGUI( 1000 )

      RETURN
   ENDIF

   oZip := GetZipObject ()

   nCount := oZip:Contents( zipfile ):Count

   FOR i := 1 TO nCount

      objItem := oZip:Contents( zipfile ):Item( i )

      IF ValType ( block ) = 'B'
         Eval ( block, objItem:Name, i )
      ENDIF

      oZip:UnPack( zipfile, extractpath, objItem:Name )

   NEXT i

RETURN

*------------------------------------------------------------------------------*
PROCEDURE COMPRESSFILES ( zipfile, afiles, level, block, ovr, lStorePath )
*------------------------------------------------------------------------------*

   LOCAL oZip
   LOCAL nCount
   LOCAL i

   oZip := GetZipObject ()

   IF ovr == .T.

      IF File ( zipfile )
         DELETE FILE ( zipfile )
      ENDIF

   ENDIF

   nCount := Len ( afiles )

   FOR i := 1 TO nCount

      Eval ( block, afiles[ i ], i )

      oZip:Pack( afiles[ i ], zipfile, lStorePath, , level )

   NEXT i

RETURN

*------------------------------------------------------------------------------*
PROCEDURE RegActiveX()
*------------------------------------------------------------------------------*

   IF File ( GetStartUpFolder() + '\xzip.dll' )
      EXECUTE FILE "regsvr32" PARAMETERS "/s XZip.dll" HIDE
   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE UnRegActiveX()
*------------------------------------------------------------------------------*

   IF File ( GetStartUpFolder() + '\xzip.dll' )
      EXECUTE FILE "regsvr32" PARAMETERS "/u /s XZip.dll" HIDE
   ENDIF

RETURN

#include "fileio.ch"
*--------------------------------------------------------*
FUNCTION IsZipFile( cFilename )
*--------------------------------------------------------*
   LOCAL nHandle, cBuffer := Space(4)

   IF ( nHandle := FOPEN( cFilename, FO_READ + FO_SHARED ) ) <> F_ERROR
      FSEEK( nHandle, 0, FS_SET )
      IF FREAD( nHandle, @cBuffer, 4 ) == 4
         IF Left( cBuffer, 1 ) == 'P' .AND. Substr( cBuffer, 2, 1 ) == 'K' .AND. ;
               Substr( cBuffer, 3, 1 ) == Chr(3) .AND. Right( cBuffer, 1 ) == Chr(4)
            FCLOSE( nHandle )
            RETURN .T.
         ENDIF
      ENDIF
      FCLOSE( nHandle )
   ENDIF

RETURN .F.
