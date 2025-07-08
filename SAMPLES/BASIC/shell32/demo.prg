/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-06 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2004-07 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"
#include "shell32.ch"

#define MsgInfo( c ) MsgInfo( c, "Information", , .f. )
#define MsgAlert( c ) MsgExclamation( c, "Attention", , .f. )

PROCEDURE Main( nOper, cFile, cCopy )

   LOCAL aFile := {}, aCopy := {}
   LOCAL cPath := GetStartupFolder() + "\"

   DEFAULT cFile := "compile.bat"
   DEFAULT cCopy := cPath + "Test Directory\compile.bat"
   DEFAULT nOper := FO_COPY

   IF ValType( nOper ) == "C"
      nOper := Val( nOper )
   ENDIF

   AAdd( aFile, cFile )
   AAdd( aFile, "demo.prg" )
   AAdd( aCopy, cPath + "Test Directory" )

   DEFINE WINDOW Form_Main ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'Shell Files Operation sample by Grigory Filatov' ;
         MAIN ;
         NOSIZE NOMAXIMIZE

      DEFINE BUTTON x
         ROW 10
         COL 10
         CAPTION "Test 1"
         DEFAULT .T.
         ACTION test( aFile, aCopy, nOper, cCopy )
      END BUTTON

      DEFINE BUTTON x2
         ROW 40
         COL 10
         CAPTION "Test 2"
         DEFAULT .T.
         ACTION test2( aFile, aCopy, nOper )
      END BUTTON

      DEFINE BUTTON y
         ROW 70
         COL 10
         CAPTION "Exit"
         ACTION ThisWindow.RELEASE
      END BUTTON

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN


PROCEDURE test( aFile, aCopy, nOper, cCopy )

   IF ( ShellFiles( Application.Handle, aFile, aCopy, nOper, FOF_NOCONFIRMMKDIR ) == 0 )

      MsgInfo( "The Test Directory is maked and the files are copied." )

      IF ShFileDelete( , cCopy, .T. )

         MsgInfo( "The file " + aFile[ 1 ] + " is deleted from the Test Directory." )

      ELSE

         MsgAlert( "Error of deleting!" )

      ENDIF

   ENDIF

RETURN


PROCEDURE test2( aFile, aCopy, nOper )

   LOCAL cDirPath

   IF ( ShellFiles( Application.Handle, aFile, aCopy, nOper, FOF_NOCONFIRMMKDIR ) == 0 )

      MsgInfo( "The Test Directory is maked and the files are copied." )

      cDirPath := aCopy[ 1 ]

      IF ShFolderDelete( , cDirPath, .F. )

         IF hb_DirExists( cDirPath )

            MsgInfo( "The folder " + cDirPath + " is NOT erased." )

         ELSE

            MsgInfo( "The folder " + cDirPath + " is erased." )

         ENDIF

      ELSE

         MsgAlert( "Error of deleting!" )

      ENDIF

   ENDIF

RETURN
