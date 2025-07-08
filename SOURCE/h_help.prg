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

#include "minigui.ch"
#include "fileio.ch"

/*-----------------------------------------------------------------------------*
PROCEDURE SetHelpFile( cFile )
*------------------------------------------------------------------------------*
*
*  Purpose:
*     Sets the active help file for the application. This procedure checks if the specified
*     help file exists and attempts to open it. If successful, it sets the global variable
*     _HMG_ActiveHelpFile to the file's path. If not, it displays an error message.
*
*  Parameters:
*     cFile - The path to the help file (string).
*
*  Return Value:
*     None.
*
*  Side Effects:
*     Sets the global variable _HMG_ActiveHelpFile.
*     Displays a message box if the file cannot be opened or is not found.
*
*  Note:
*     This procedure assumes that the global variable _HMG_ActiveHelpFile is defined elsewhere.
*     It uses the Harbour file I/O functions FOpen, FError, and FClose.
*     It uses the HMG MsgAlert function to display error messages.
*/
PROCEDURE SetHelpFile( cFile )
   LOCAL hFile

   IF File( cFile )

      hFile := FOpen( cFile, FO_READ + FO_SHARED )

      _HMG_ActiveHelpFile := iif( FError() == 0, cFile, "" )

      IF Empty( _HMG_ActiveHelpFile )
         MsgAlert( "Error opening of help file. Error: " + Str( FError(), 2, 0 ), "Alert" )
      ENDIF

      FClose( hFile )

   ELSE

      MsgAlert( "Help file " + cFile + " is not found!", "Warning" )

   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
PROCEDURE DisplayHelpTopic( xTopic , nMet )
*------------------------------------------------------------------------------*
*
*  Purpose:
*     Displays a specific help topic from the active help file. This procedure determines
*     the type of help file (CHM or other) and uses the appropriate method to display the topic.
*     For CHM files, it uses the "hh.exe" (HTML Help executable) to display the topic.
*     For other help files, it uses the WinHelp API function.
*
*  Parameters:
*     xTopic - The topic to display. This can be a numeric ID, a string representing a topic name, or NIL.
*     nMet   - The method to use for displaying the help topic (numeric).  This parameter is used with WinHelp API.
*              It's defaulted to 0 if not provided.
*
*  Return Value:
*     None.
*
*  Side Effects:
*     Displays a help window.
*     Sets the global variables _HMG_nTopic and _HMG_nMet.
*
*  Note:
*     This procedure assumes that the global variables _HMG_ActiveHelpFile and _HMG_MainHandle are defined elsewhere.
*     It uses the Harbour functions ValType, hb_ntos, AllTrim, Upper, and _Execute.
*     It uses the Windows API function WinHelp.
*     The _Execute function is assumed to be a custom function for executing external programs.
*/
PROCEDURE DisplayHelpTopic( xTopic , nMet )
   LOCAL cParam := ""

   IF Empty( _HMG_ActiveHelpFile )
      RETURN
   ENDIF

   _HMG_nTopic := xTopic
   _HMG_nMet   := nMet

   __defaultNIL( @nMet, 0 )

   IF Right( AllTrim( Upper( _HMG_ActiveHelpFile ) ) , 4 ) == '.CHM'

      SWITCH ValType( xTopic )
      CASE 'N'
         cParam := '-mapid ' + hb_ntos( xTopic ) + ' ' + _HMG_ActiveHelpFile
         EXIT
      CASE 'C'
         cParam := '"' + _HMG_ActiveHelpFile + '::/' + AllTrim( xTopic ) + '"'
         EXIT
      CASE 'U'
         cParam := '"' + _HMG_ActiveHelpFile + '"'
      ENDSWITCH

      _Execute( _HMG_MainHandle , "open" , "hh.exe" , cParam , , SW_SHOW )

   ELSE

      __defaultNIL( @xTopic, 0 )

      WinHelp( _HMG_MainHandle , _HMG_ActiveHelpFile , nMet , xTopic )

   ENDIF

RETURN
