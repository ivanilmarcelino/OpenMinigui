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
#include "minigui.ch"
#include "error.ch"
#include "hbver.ch"

//#define TRACE
/*-----------------------------------------------------------------------------*
* PROCEDURE ClipInit()
*
* Description:
*   This procedure is the initialization routine for the application.
*   It checks the Windows version and displays an error message if it's running on
*   Windows 95 or 98, as these versions are not supported. If the Windows version
*   is acceptable, it calls the Init() function to perform further initialization.
*-----------------------------------------------------------------------------*/
INIT PROCEDURE ClipInit()

   IF os_isWin95() .OR. os_isWin98()
      MsgExclamation( "The " + hb_ArgV( 0 ) + " file" + CRLF + ;
         "expects a newer version of Windows." + CRLF + ;
         "Upgrade your Windows version.", "Error Starting Program", , .F., .T. )

      ExitProcess( 1 )

   ENDIF

#ifdef TRACE
   __TRACEPRGCALLS( .T. )
   HB_TRACESTATE( .T. )
#endif
   Init()

RETURN

/*-----------------------------------------------------------------------------*
* PROCEDURE ClipExit()
*
* Description:
*   This procedure is the exit routine for the application.
*   It terminates the application process.
*-----------------------------------------------------------------------------*/
EXIT PROCEDURE ClipExit()

   ExitProcess()

RETURN

#ifndef __XHARBOUR__
/*
* PROCEDURE hb_GTSYS
*
* Description:
*   This procedure requests the default GUI graphics terminal system (GTSYS) for Harbour.
*   It ensures that the GUI is initialized correctly.
*/
PROCEDURE hb_GTSYS

   REQUEST HB_GT_GUI_DEFAULT

RETURN

#endif

/*-----------------------------------------------------------------------------*
* FUNCTION MsgMiniGuiError( cErrorMessage, lAddText )
*
* Description:
*   This function displays an error message using the MiniGUI framework.
*   It takes an error message string as input and optionally appends a default
*   termination message. It then evaluates the ErrorBlock() with a generated
*   HMG error object, effectively triggering the error handling mechanism.
*
*   This function provides a standardized way to display error messages within
*   MiniGUI applications.  It leverages the Harbour error handling system to
*   ensure that errors are reported consistently and can be handled gracefully.
*
* Parameters:
*   cErrorMessage: The error message to display.
*   lAddText: Optional. If .T. (default), appends " Program terminated." to the message.
*
* Return Value:
*   The return value depends on the ErrorBlock() implementation.
*-----------------------------------------------------------------------------*/
FUNCTION MsgMiniGuiError( cErrorMessage, lAddText )

   IF hb_defaultValue( lAddText, .T. )
      cErrorMessage += " Program terminated."
   ENDIF

RETURN Eval( ErrorBlock(), HMG_GenError( cErrorMessage ) )

/*-----------------------------------------------------------------------------*
* STATIC FUNCTION HMG_GenError( cMsg )
*
* Description:
*   This function generates a Harbour error object with specific MiniGUI-related
*   information. It sets the subsystem, subcode, severity, description, and operation
*   properties of the error object.
*
*   This function is a helper function for MsgMiniGuiError.  It creates a
*   standardized error object that can be used by the Harbour error handling
*   system.  The error object contains information about the error, such as its
*   severity and description.
*
* Parameters:
*   cMsg: The error message to be stored in the error object's description.
*
* Return Value:
*   oError: A Harbour error object populated with MiniGUI-specific error details.
*-----------------------------------------------------------------------------*/
STATIC FUNCTION HMG_GenError( cMsg )

   LOCAL oError := ErrorNew()

   oError:SubSystem   := "MGERROR"
   oError:SubCode     := 0
   oError:Severity    := ES_CATASTROPHIC
   oError:Description := cMsg
   oError:Operation   := NIL

RETURN oError

#define MG_VERSION "Harbour MiniGUI Extended Edition 25.08 ("

/*-----------------------------------------------------------------------------*
* FUNCTION MiniGuiVersion( nVer )
*
* Description:
*   This function returns the version string of the Harbour MiniGUI Extended Edition.
*   It constructs the version string based on the Harbour version and the character set.
*   It also includes a "DEBUG" suffix if the debug mode is enabled. The function allows
*   for different levels of version information to be returned based on the nVer parameter.
*
*   This function provides a way to retrieve the version of the MiniGUI library.
*   This can be useful for debugging, logging, or displaying the version information
*   to the user. The different levels of version information allow for flexibility
*   in how the version is displayed.
*
* Parameters:
*   nVer: Optional. Specifies the level of version information to return.
*                   0 (default): Returns the full version string.
*                   1: Returns a shorter version string (38 characters).
*                   2: Returns an even shorter version string (15 characters).
*
* Return Value:
*   cVer: The version string of the Harbour MiniGUI Extended Edition, truncated based on nVer.
*-----------------------------------------------------------------------------*/
FUNCTION MiniGuiVersion( nVer )
#ifndef __XHARBOUR__
   LOCAL cVer := MG_VERSION + hb_ntos( hb_Version( HB_VERSION_BITWIDTH ) ) + "-bit) "
#else
   LOCAL cVer := MG_VERSION + iif( IsExe64(), "64", "32" ) + "-bit) "
#endif
   LOCAL anOfs
   LOCAL nIndex

   hb_default( @nVer, 0 )

   cVer += HMG_CharsetName()

   IF Set( _SET_DEBUG )
      cVer += " (DEBUG)"
   ENDIF

   anOfs := { Len( cVer ), 38, 15 }
   nIndex := Max( 0, Min( nVer, 2 ) ) + 1

RETURN Left( cVer, anOfs[ nIndex ] )
