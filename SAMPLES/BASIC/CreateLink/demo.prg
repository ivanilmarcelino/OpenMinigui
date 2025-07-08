/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

PROCEDURE Main()

   LOCAL cLinkName := "Calculator"

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 334 ;
         HEIGHT 276 ;
         TITLE 'Create Desktop Shortcut' ;
         MAIN

      DEFINE MAIN MENU

         DEFINE POPUP "Test"
            MENUITEM 'Create Desktop Shortcut' ACTION CreateShortcut( cLinkName )
            MENUITEM 'Remove Desktop Shortcut' ACTION DeleteShortcut( cLinkName )
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.Release()
         END POPUP

      END MENU

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN

*------------------------------------------------------------------------------*
PROCEDURE CreateShortcut( cLink )
*------------------------------------------------------------------------------*

   LOCAL cLinkName := hb_DirSepAdd( GetDesktopFolder() ) + cLink + ".lnk"
   LOCAL cExeName := "Calc.exe"
   LOCAL cFileName := hb_DirSepAdd( GetWindowsFolder() ) + cExeName
   LOCAL nSuccess

   IF ! File( cFileName )
      cFileName := hb_DirSepAdd( GetSystemFolder() ) + cExeName
   ENDIF

   CREATE LINK FILE cLinkName ;
      TARGETFILE cFileName ;
      DESCRIPTION "Classic arithmetic tasks with an on-screen calculator." ;
      WORKING DIRECTORY cFilePath( cFileName ) ;
      ICON LOCATION cFileName ;
      HOTKEY CTRL+ALT+Z ;
      RESULT nSuccess

   IF nSuccess == S_OK
      MsgInfo( "The shortcut has been successfully created on the desktop.", "Result" )
   ELSE
      MsgStop( "Create Link Error!", "ERROR", , .F. )
   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE DeleteShortcut( cLink )
*------------------------------------------------------------------------------*

   LOCAL WshShell := CreateObject( "WScript.Shell" )
   LOCAL DesktopFolder := WshShell:SpecialFolders:Item( "Desktop" )
   LOCAL cLinkName := hb_DirSepAdd( DesktopFolder ) + cLink + ".lnk"
   LOCAL FSO
   LOCAL lError := .F.

   FSO := CreateObject( "Scripting.fileSystemObject" )

   IF FSO:FileExists( cLinkName )
      FSO:DeleteFile( cLinkName )
   ELSE
      lError := .T.
      MsgAlert( "Shortcut <" + cLink + "> not found on desktop.", "Result" )
   ENDIF

   IF ! lError
      MsgInfo( "The shortcut has been removed from the desktop.", "Result" )
   ENDIF

   FSO := NIL
   WshShell := NIL

RETURN
