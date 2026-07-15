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
   // Ensure __SYSDATA__ is defined for xHarbour compatibility layers
   #define __SYSDATA__
#endif

#include "minigui.ch"
#include "error.ch"
#include "hbver.ch"

/*
 * PROCEDURE: ClipInit()
 * Purpose:   Automatic initialization routine executed before the main application entry point.
 * Logic:     Validates the operating system environment to ensure compatibility with HMG Extended.
 * Side Effects: Terminates the process if the OS is a legacy Windows 9x version.
 */
INIT PROCEDURE ClipInit()
#ifndef __XHARBOUR__
   // Harbour-specific check for Windows 95/98/Me. 
   // HMG Extended requires NT-based kernels for modern API support.
   IF hb_osIsWin9x()
#else
   // xHarbour-specific check for legacy Windows versions.
   IF os_isWin95() .OR. os_isWin98()
#endif
      // Display a modal exclamation message using HMG's standard UI messaging.
      // Parameters: Message, Title, Icon, SystemModal, TopMost.
      MsgExclamation( "The " + hb_ArgV( 0 ) + " file" + CRLF + ;
         "expects a newer version of Windows." + CRLF + ;
         "Upgrade your Windows version.", "Error Starting Program", , .F., .T. )
      
      // Force immediate termination of the process with exit code 1.
      ExitProcess( 1 )
   ENDIF
   
   // Call the internal HMG initialization routine to set up global structures.
   Init()
RETURN

/*
 * PROCEDURE: ClipExit()
 * Purpose:   Cleanup routine executed automatically upon application termination.
 * Logic:     Ensures the Windows process is properly closed.
 */
EXIT PROCEDURE ClipExit()
   ExitProcess()
RETURN

#ifndef __XHARBOUR__
/*
 * PROCEDURE: hb_GTSYS()
 * Purpose:   Configures the Harbour Graphics Terminal (GT) system.
 * Logic:     Requests the GUI driver instead of the default console driver.
 * Why:       This prevents a console window from flashing or persisting when 
 *            launching a GUI-based HMG application.
 */
PROCEDURE hb_GTSYS
   REQUEST HB_GT_GUI_DEFAULT
RETURN
#endif

/*
 * FUNCTION: MsgMiniGuiError( cErrorMessage, lAddText )
 * Purpose:   Standardized error reporting for the HMG framework.
 * Parameters:
 *    - cErrorMessage (String): The specific error description.
 *    - lAddText (Logical): If true, appends a "Program terminated" suffix.
 * Returns:   The result of the current ErrorBlock evaluation.
 * Side Effects: Triggers the Harbour error handling system.
 */
FUNCTION MsgMiniGuiError( cErrorMessage, lAddText )
   // Default lAddText to .T. if not provided to ensure consistent user feedback.
   IF hb_defaultValue( lAddText, .T. )
      cErrorMessage += " Program terminated."
   ENDIF
   
   // Generate a custom error object and pass it to the global error handler.
   // This allows developers to intercept HMG errors via ErrorBlock().
RETURN Eval( ErrorBlock(), HMG_GenError( cErrorMessage ) )

/*
 * STATIC FUNCTION: HMG_GenError( cMsg )
 * Purpose:   Creates and populates a Harbour Error object.
 * Parameters:
 *    - cMsg (String): The error message to be encapsulated.
 * Returns:   An Error object configured for the HMG subsystem.
 */
STATIC FUNCTION HMG_GenError( cMsg )
   LOCAL oError := ErrorNew()
   
   // Define the subsystem as "MGERROR" to distinguish it from standard RTL or RDD errors.
   oError:SubSystem   := "MGERROR"
   oError:SubCode     := 0
   oError:Severity    := ES_CATASTROPHIC
   oError:Description := cMsg
   oError:Operation   := NIL
   
RETURN oError

/*
 * FUNCTION: MiniGuiVersion( nVersion )
 * Purpose:   Retrieves the current version string of the HMG Extended library.
 * Parameters:
 *    - nVersion (Numeric): Format selector (0=Full, 1=Short, 2=Minimal).
 * Returns:   A string containing version, architecture, and charset info.
 */
FUNCTION MiniGuiVersion( nVersion )
   LOCAL cVersion
   
   // Define the base version string.
   #define MG_VERSION "Harbour MiniGUI Extended Edition 26.07.0 ("
   
#ifndef __XHARBOUR__
   // Use Harbour's native version function to detect 32-bit vs 64-bit builds.
   cVersion := MG_VERSION + hb_ntos( hb_Version( HB_VERSION_BITWIDTH ) ) + "-bit) "
#else
   // Use xHarbour compatibility check for executable architecture.
   cVersion := MG_VERSION + iif( IsExe64(), "64", "32" ) + "-bit) "
#endif
   
   // Append the active character set (e.g., ANSI, UTF8) for debugging localization issues.
   cVersion += HMG_CharsetName()
   
   // If the application is compiled with debug flags, append a suffix.
   IF Set( _SET_DEBUG )
      cVersion += " (DEBUG)"
   ENDIF
   
   // Ensure nVersion is within valid bounds [0-2].
   hb_default( @nVersion, 0 )
   nVersion := Max( 0, Min( nVersion, 2 ) )
   
   // Return the version string truncated based on the requested level of detail.
   SWITCH nVersion
      CASE 1 
         RETURN Left( cVersion, 40 )
      CASE 2 
         RETURN Left( cVersion, 15 )
   ENDSWITCH
   
RETURN cVersion