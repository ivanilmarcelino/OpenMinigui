/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/

#ifdef __XHARBOUR__
#define __SYSDATA__
#endif

#include 'minigui.ch'

/*-----------------------------------------------------------------------------*
FUNCTION WindowsVersion()
*------------------------------------------------------------------------------*
*
*  Description:
*     Retrieves the operating system version information.  Specifically, it attempts to retrieve
*     detailed version information for Windows 10 and later, and falls back to the standard
*     WinVersion() function for older operating systems.
*
*  Parameters:
*     None
*
*  Return Value:
*     An array containing the following elements:
*       - [1]: The product name (e.g., "Windows 10 Pro", "Windows 11 Home").
*       - [2]: The release ID or display version (e.g., "20H2", "22H2" or "22000.1234").
*       - [3]: The build number (e.g., "Build 19042.985").
*
*  Purpose:
*     This function provides a more robust way to determine the Windows version, especially for
*     Windows 10 and later, where the traditional WinVersion() function may not return accurate
*     information.  It reads version information directly from the Windows Registry. This is
*     crucial for applications that need to adapt their behavior based on the specific OS version.
*
*  Notes:
*     - The function relies on the IsWin10OrLater() and hb_osisWin11() functions to determine the OS version.
*     - It uses GetRegistryValue() to read values from the Windows Registry.
*     - The UBR (Update Build Revision) value is appended to the build number for more precise versioning.
*     - If the OS is not Windows 10 or later, it falls back to the WinVersion() function.
*
*/
FUNCTION WindowsVersion()
   LOCAL cKey
   LOCAL aRetVal := Array( 4 )

   IF IsWin10OrLater()
      cKey := "SOFTWARE\Microsoft\Windows NT\CurrentVersion"
      aRetVal [1] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "ProductName" )
      IF hb_osisWin11()
         aRetVal [1] := StrTran( aRetVal [1], "10", "11" )
         aRetVal [2] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "DisplayVersion" )
      ELSE
         aRetVal [2] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "ReleaseId" )
      ENDIF
      aRetVal [3] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "CurrentBuild" ) + "." + ;
         hb_ntos( GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "UBR", "N" ) )
      aRetVal [4] := ""
   ELSE
      aRetVal := WinVersion()
   ENDIF

RETURN { aRetVal [1] + aRetVal [4] , aRetVal [2] , 'Build ' + aRetVal [3] }

/*-----------------------------------------------------------------------------*
FUNCTION _Execute( hWnd , cOperation , cFile , cParameters , cDirectory , nState )
*------------------------------------------------------------------------------*
*
*  Description:
*     Executes a file or performs an operation on a file using the Windows ShellExecute API.
*
*  Parameters:
*     hWnd        - The handle of the parent window.  If NIL, the active window is used.
*                   Data type: HWND (Handle to a Window). Optional.
*     cOperation  - The operation to perform (e.g., "open", "print", "edit", "explore", "find").
*                   Data type: STRING.
*     cFile       - The file to execute or operate on.
*                   Data type: STRING. Optional.
*     cParameters - Parameters to pass to the executable file.
*                   Data type: STRING. Optional.
*     cDirectory  - The working directory for the executable.
*                   Data type: STRING.
*     nState      - The window state (e.g., SW_SHOWNORMAL, SW_MAXIMIZE, SW_MINIMIZE).
*                   Data type: NUMERIC. Optional. Defaults to SW_SHOWNORMAL.
*
*  Return Value:
*     The return value of the ShellExecute API function.  A value greater than 32 indicates success.
*     A value less than or equal to 32 indicates an error.
*
*  Purpose:
*     This function provides a convenient wrapper around the Windows ShellExecute API, allowing
*     applications to launch files, open documents, print files, and perform other shell operations.
*     It simplifies the process of interacting with the operating system's shell.
*     Example Usage:
*       _Execute( hWnd, "open", "C:\MyDocument.txt", "", "C:\", SW_SHOWNORMAL )  // Opens the text file.
*
*  Notes:
*     - The function uses hb_defaultValue() to provide default values for optional parameters.
*     - The possible values for cOperation are 'edit', 'explore', 'find', 'open', 'print'.
*     - The nState parameter controls how the application window is displayed.
*
*/
FUNCTION _Execute( hWnd , cOperation , cFile , cParameters , cDirectory , nState )
RETURN ShellExecute( hb_defaultValue( hWnd, GetActiveWindow() ), cOperation, ;
   hb_defaultValue( cFile, "" ), cParameters, cDirectory, hb_defaultValue( nState, SW_SHOWNORMAL ) )

/*-----------------------------------------------------------------------------*
PROCEDURE ShellAbout( cTitle , cMsg , hIcon )
*------------------------------------------------------------------------------*
*
*  Description:
*     Displays a standard "About" dialog box using the C_ShellAbout function.  This function ensures
*     that only one "About" dialog box is displayed at a time, even if the function is called multiple times.
*
*  Parameters:
*     cTitle - The title of the "About" dialog box.
*              Data type: STRING.
*     cMsg   - The message to display in the "About" dialog box.
*              Data type: STRING.
*     hIcon  - The handle of the icon to display in the "About" dialog box.  If NIL, the default icon is used.
*              Data type: HICON (Handle to an Icon). Optional.
*
*  Return Value:
*     None.
*
*  Purpose:
*     This procedure simplifies the creation of "About" dialog boxes in HMG applications.  It prevents
*     multiple "About" dialogs from being opened simultaneously by using a global variable to track
*     whether an "About" dialog is already active.  This ensures a consistent and user-friendly experience.
*
*  Notes:
*     - The function uses a static global variable _HMG_ShellAbout to track whether an "About" dialog is already open.
*     - The C_ShellAbout function (presumably a C function linked into the HMG application) is responsible for
*       actually displaying the dialog box.
*     - If an icon is provided (hIcon), the function destroys the icon after the dialog box is closed to prevent memory leaks.
*
*/
PROCEDURE ShellAbout( cTitle , cMsg , hIcon )
   LOCAL nCount

   IF _SetGetGlobal( "_HMG_ShellAbout" ) == NIL
      STATIC _HMG_ShellAbout AS GLOBAL VALUE 0
   ENDIF

   IF ( nCount := _SetGetGlobal( "_HMG_ShellAbout" ) ) == 0

      ASSIGN GLOBAL _HMG_ShellAbout := ++nCount

      IF C_ShellAbout( GetActiveWindow() , cTitle , cMsg , hIcon )
         IF hIcon != NIL .AND. IsHIcon( hIcon )  // Release Icon
            DestroyIcon ( hIcon )
         ENDIF
         ASSIGN GLOBAL _HMG_ShellAbout := --nCount
      ENDIF

   ENDIF

RETURN
