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
   Copyright 1999-2026, https://harbour.github.io/

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
   
   Purpose:
      Retrieves detailed Operating System version information, specifically 
      tailored to handle the nuances of Windows 10 and Windows 11.

   Parameters:
      None.

   Returns:
      An array { cProductName, cVersion, cBuildString }
      - [1] Product Name (e.g., "Windows 11 Pro")
      - [2] Release/Version ID (e.g., "22H2")
      - [3] Formatted Build String (e.g., "Build 22621.1702")

   Implementation Logic:
      Standard Windows APIs often report incorrect versions for Windows 10+ 
      unless the application is specifically manifested. This function bypasses 
      API limitations by querying the Windows Registry directly for "CurrentVersion" 
      data, providing the most accurate identification for modern environments.
 *------------------------------------------------------------------------------*/
FUNCTION WindowsVersion()

   // Path to the registry key containing NT-based Windows versioning data
   LOCAL cKey := "SOFTWARE\Microsoft\Windows NT\CurrentVersion"
   LOCAL cProd, cVer, cBuild, cExtra := ""
   LOCAL aWin

   // Check if the OS is Windows 10 or newer using HMG internal detection
   IF IsWin10OrLater()

      // Retrieve the descriptive product name from the registry
      cProd  := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "ProductName" )

      /* 
         Logic for Windows 11:
         Early Windows 11 builds often still report "Windows 10" in the ProductName registry key.
         We perform a string replacement if hb_osisWin11() confirms the OS is actually Windows 11.
         We also switch from 'ReleaseId' to 'DisplayVersion' as the standard for version naming.
      */
      IF hb_osisWin11()
         cProd := StrTran( cProd, "10", "11" )
         cVer  := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "DisplayVersion" )
      ELSE
         // For standard Windows 10, ReleaseId (e.g., 1909, 20H2) is the primary identifier
         cVer  := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "ReleaseId" )
      ENDIF

      /* 
         Build Number Construction:
         Combines the base 'CurrentBuild' with the 'UBR' (Update Build Revision).
         The UBR is stored as a Numeric (DWORD) in the registry, hence the "N" parameter.
      */
      cBuild := ;
         GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "CurrentBuild" ) + "." + ;
         hb_ntos( GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "UBR", "N" ) )

   ELSE

      /* 
         Fallback for Legacy OS:
         If the OS is older than Windows 10, we rely on the standard WinVersion() 
         array which is generally accurate for Windows 7, 8, and XP.
      */
      aWin := WinVersion()

      cProd  := aWin[1]
      cVer   := aWin[2]
      cBuild := aWin[3]

      // Check for service pack or extra version info if available in the array
      IF Len( aWin ) >= 4
         cExtra := aWin[4]
      ENDIF

   ENDIF

   // Return the formatted results as a 3-element array
   RETURN { cProd + cExtra, cVer, "Build " + cBuild }

/*-----------------------------------------------------------------------------*
   FUNCTION _Execute( hWnd, cOperation, cFile, cParameters, cDirectory, nState )
   
   Purpose:
      A high-level wrapper for the Windows ShellExecute API.

   Parameters:
      - hWnd: Handle to the parent window. If NIL, defaults to the current active window.
      - cOperation: The action to perform ("open", "print", "explore", "runas", etc.).
      - cFile: The file, executable, or URL to be processed.
      - cParameters: Command-line arguments (if cFile is an executable).
      - cDirectory: The working directory for the process.
      - nState: Window display state (e.g., SW_SHOWNORMAL, SW_HIDE).

   Returns:
      Numeric: A value > 32 indicates success. Values <= 32 represent Shell error codes.

   Design Decision:
      Uses hb_defaultValue() to ensure the function is robust even when called with 
      minimal arguments, defaulting to the active HMG window and standard visibility.
 *------------------------------------------------------------------------------*/
FUNCTION _Execute( hWnd , cOperation , cFile , cParameters , cDirectory , nState )
   RETURN ShellExecute( hb_defaultValue( hWnd, GetActiveWindow() ), cOperation, ;
      hb_defaultValue( cFile, "" ), cParameters, cDirectory, hb_defaultValue( nState, SW_SHOWNORMAL ) )

/*-----------------------------------------------------------------------------*
   PROCEDURE ShellAbout( cTitle, cMsg, hIcon )
   
   Purpose:
      Displays the standard Windows "About" dialog box.

   Parameters:
      - cTitle: The application name/title to display in the dialog.
      - cMsg: Additional text (usually copyright or version info).
      - hIcon: Handle to a custom icon. If provided, it will be displayed in the dialog.

   Side Effects:
      - UI: Freezes the parent window while the modal dialog is open.
      - Resource Management: Automatically destroys the hIcon handle after use to prevent GDI leaks.
      - Global State: Uses a global counter to prevent multiple instances of the About box.

   Logic:
      The procedure implements a "Singleton" pattern for the dialog using a 
      Global variable. This prevents the user from triggering multiple About 
      dialogs simultaneously via rapid clicking.
 *------------------------------------------------------------------------------*/
PROCEDURE ShellAbout( cTitle , cMsg , hIcon )
   LOCAL nCount

   // Initialize the global tracking variable if it doesn't exist in the HMG environment
   IF _SetGetGlobal( "_HMG_ShellAbout" ) == NIL
      STATIC _HMG_ShellAbout AS GLOBAL VALUE 0
   ENDIF

   // Only proceed if no other ShellAbout dialog is currently active
   IF ( nCount := _SetGetGlobal( "_HMG_ShellAbout" ) ) == 0

      // Increment the global counter to "lock" the dialog
      ASSIGN GLOBAL _HMG_ShellAbout := ++nCount

      // Call the C-level wrapper for the Windows ShellAbout API
      IF C_ShellAbout( GetActiveWindow() , cTitle , cMsg , hIcon )
         
         // If a custom icon handle was passed, we must release it from memory
         IF hIcon != NIL .AND. IsHIcon( hIcon )
            DestroyIcon ( hIcon )
         ENDIF
         
         // Decrement the global counter to "unlock" and allow future calls
         ASSIGN GLOBAL _HMG_ShellAbout := --nCount
      ENDIF

   ENDIF

RETURN
