/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

/*
 * PROCEDURE Main()
 *
 * Defines and activates the main window of the application.
 *
 * Purpose:
 *   This is the entry point of the application. It defines the main window,
 *   including its title, size, and menu. It also centers the window on the
 *   screen and activates it, making it visible to the user.
 *   The main window contains a menu with options to create or delete a desktop
 *   shortcut to the Calculator application, and to exit the application.
 */
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

/*
 * PROCEDURE CreateShortcut( cLink )
 *
 * Creates a desktop shortcut to the Calculator application.
 *
 * Parameters:
 *   cLink : The name of the shortcut to be created (e.g., "Calculator").
 *           This name will be used as the base name for the .lnk file.
 *           Data type: Character.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure automates the creation of a desktop shortcut to the
 *   Calculator application. It constructs the full path to the shortcut file,
 *   the Calculator executable, and then uses the CREATE LINK FILE command
 *   to create the shortcut.  It then displays a message to the user indicating
 *   whether the shortcut was successfully created.
 *
 * Notes:
 *   The procedure assumes that the Calculator executable (Calc.exe) is located
 *   either in the Windows folder or the System folder. It uses the GetWindowsFolder()
 *   and GetSystemFolder() functions to determine the correct location.
 *   The CREATE LINK FILE command is specific to HMG Extended and provides
 *   a simplified way to create shortcuts.
 *   The procedure uses the hb_DirSepAdd() function to ensure that directory
 *   separators are correctly added to the file paths.
 *   The hotkey is hardcoded to CTRL+ALT+Z.
 */
PROCEDURE CreateShortcut( cLink )

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

/*
 * PROCEDURE DeleteShortcut( cLink )
 *
 * Deletes a desktop shortcut with the specified name.
 *
 * Parameters:
 *   cLink : The name of the shortcut to be deleted (e.g., "Calculator").
 *           This name will be used to construct the full path to the .lnk file.
 *           Data type: Character.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This procedure removes a desktop shortcut. It uses the WScript.Shell
 *   object to access the desktop folder and the Scripting.FileSystemObject
 *   to delete the shortcut file. It displays a message to the user indicating
 *   whether the shortcut was successfully deleted or if it was not found.
 *
 * Notes:
 *   The procedure relies on the Windows Script Host (WSH) and the
 *   Scripting.FileSystemObject (FSO) COM objects to interact with the
 *   file system. These objects must be available on the system for the
 *   procedure to work correctly.
 *   The procedure uses late binding to create and access the COM objects,
 *   which means that the object types are not known at compile time.
 *   Error handling is included to check if the shortcut file exists before
 *   attempting to delete it.
 *   The procedure sets the FSO and WshShell objects to NIL at the end to
 *   release the COM objects and free up resources.
 */
PROCEDURE DeleteShortcut( cLink )

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
