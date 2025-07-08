/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"

PROCEDURE Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 334 ;
         HEIGHT 276 ;
         TITLE 'Windows Script Host Demo' ;
         MAIN

      DEFINE MAIN MENU

         DEFINE POPUP "Test"
            MENUITEM 'Create Desktop Shortcuts' ACTION CreateShortcuts()
            MENUITEM 'Remove Desktop Shortcuts' ACTION DeleteShortcuts( ;
               { "Shutdown", "Log Off", "Restart" } )
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.Release()
         END POPUP

      END MENU

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN

*------------------------------------------------------------------------------*
PROCEDURE CreateShortcuts()
*------------------------------------------------------------------------------*

   LOCAL WshShell := CreateObject( "WScript.Shell" )
   LOCAL DesktopFolder := WshShell:SpecialFolders:Item( "Desktop" )

   MakeShortCut( WshShell, DesktopFolder + "\Shutdown.lnk", ;
      "%systemroot%\System32\shutdown.exe", "-s -t 0", ;
      "%systemroot%\System32\shell32.dll,27", ;
      "Shutdown Computer (Power Off)", "%systemroot%\System32\" )

   MakeShortCut( WshShell, DesktopFolder + "\Log Off.lnk", ;
      "%systemroot%\System32\shutdown.exe", "-l", ;
      "%systemroot%\System32\shell32.dll,44", ;
      "Log Off (Switch User)", "%systemroot%\System32\" )

   MakeShortCut( WshShell, DesktopFolder + "\Restart.lnk", ;
      "%systemroot%\System32\shutdown.exe", "-r -t 0", ;
      "%systemroot%\System32\shell32.dll,176", ;
      "Restart Computer (Reboot)", "%systemroot%\System32\" )

   MsgInfo( "Created <Shutdown>, <Restart> and <Log Off> shortcuts", "Result" )

   WshShell := NIL

RETURN

*------------------------------------------------------------------------------*
PROCEDURE MakeShortCut( WshShell, LinkName, ;
      TargetPath, Arguments, IconLocation, Description, WorkingDirectory )
*------------------------------------------------------------------------------*

   LOCAL FileShortcut

   FileShortcut := WshShell:CreateShortcut( LinkName )

   WITH OBJECT FileShortcut
      :TargetPath := TargetPath
      :Arguments := Arguments
      :WindowStyle := 1
      :IconLocation := IconLocation
      :Description := Description
      :WorkingDirectory := WorkingDirectory
      :Save()
   END WITH

   FileShortcut := NIL

RETURN

*------------------------------------------------------------------------------*
PROCEDURE DeleteShortcuts( aLinks )
*------------------------------------------------------------------------------*

   LOCAL WshShell := CreateObject( "WScript.Shell" )
   LOCAL DesktopFolder := WshShell:SpecialFolders:Item( "Desktop" )
   LOCAL FSO := CreateObject( "Scripting.fileSystemObject" )
   LOCAL cLink, cLinkName, lError := .F.

   FOR EACH cLink IN aLinks
      cLinkName := DesktopFolder + "\" + cLink + ".lnk"
      IF FSO:FileExists( cLinkName )
         FSO:DeleteFile( cLinkName )
      ELSE
         lError := .T.
         MsgInfo( "Shortcut <" + cLink + "> not found on desktop" )
      ENDIF
   NEXT

   IF ! lError
      MsgInfo( "Shortcuts were removed from desktop", "Result" )
   ENDIF

   FSO := NIL
   WshShell := NIL

RETURN
