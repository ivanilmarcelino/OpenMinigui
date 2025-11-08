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

 ----------------------------------------------------------------------------*/
#include "minigui.ch"
#include "error.ch"
#include "fileio.ch"
#include "hbmemvar.ch"
#include "hbver.ch"

#ifdef __XHARBOUR__
   REQUEST Select, Alias, RecNo, DbFilter, DbRelation, IndexOrd, IndexKey
#endif

#ifdef _TSBROWSE_
   MEMVAR _TSB_aControlhWnd
#endif
/*-----------------------------------------------------------------------------*
PROCEDURE ErrorSys
*------------------------------------------------------------------------------*
*
*  Description:
*     Initializes the error handling system by setting the error block and configuring the error log file.
*
*  Parameters:
*     None
*
*  Return Value:
*     None
*
*  Purpose:
*     This procedure is called at the start of the application to set up the global error handling mechanism.
*     It defines the ErrorBlock, which is a code block that will be executed whenever an error occurs.
*     The ErrorBlock calls the DefError function to handle the error.
*     It also sets the output log file for Harbour's output logging (_SET_HBOUTLOG) and includes version information.
*     This ensures that all errors are caught and logged for debugging purposes.
*
*  Notes:
*     The #ifndef __XHARBOUR__ block is used to conditionally compile the code based on the Harbour compiler being used.
*     This is necessary because xHarbour may have different requirements for error logging.
*
*/
PROCEDURE ErrorSys
   ErrorBlock( { | oError | DefError( oError ) } )
#ifndef __XHARBOUR__
   Set( _SET_HBOUTLOG, GetStartUpFolder() + hb_ps() + "error.log" )
   Set( _SET_HBOUTLOGINFO, MiniGUIVersion() )
#endif

RETURN

/*-----------------------------------------------------------------------------*
STATIC FUNCTION DefError( oError )
*------------------------------------------------------------------------------*
*
*  Description:
*     Handles a specific error object, logging details and displaying an error message to the user.
*
*  Parameters:
*     oError - An error object containing information about the error that occurred.
*
*  Return Value:
*     .F. -  Indicates that the error was handled and the application should exit.  In some specific cases (division by zero, lock error, database open error, append lock error), it may return 0 or .T. to allow the program to continue.
*
*  Purpose:
*     This function is the core error handler for the application. It receives an error object, extracts relevant information,
*     logs the error details to an HTML file, and displays a user-friendly error message.  It also handles specific error conditions
*     like division by zero, database lock errors, and file open errors, providing appropriate responses.
*     The function aims to provide comprehensive error reporting to aid in debugging and maintenance.
*
*  Notes:
*     The function uses HTML formatting to create a readable error log file.  It also retrieves system information and the call stack to provide
*     context for the error.  The _lShowDetailError() function controls whether a detailed error message is displayed to the user.
*     The function calls ExitProcess() to terminate the application after handling the error.
*
*/
STATIC FUNCTION DefError( oError )
   LOCAL lOldSetState := ( Len( DToC( Date() ) ) == 10 )
   LOCAL cText
   LOCAL HtmArch
   LOCAL HtmText
   LOCAL n

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV .AND. ;
         oError:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure
   IF oError:genCode == EG_LOCK .AND. ;
         oError:canRetry
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
         oError:osCode == 32 .AND. ;
         oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
         oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF ! lOldSetState
      SET CENTURY ON
   ENDIF

   HtmArch := Html_ErrorLog()
   cText := ErrorMessage( oError )

   Html_RawText( HtmArch, '<div class="record">' )
   Html_RawText( HtmArch, '<p class="updated">' )
   Html_LineText( HtmArch, 'Date: <span class="date">' + DToC( Date() ) + '</span> ' + 'Time: <span class="time">' + Time() + '</span>' )
   Html_LineText( HtmArch, 'Application: ' + GetExeFileName() )
   Html_LineText( HtmArch, 'User: ' + NetName() + " / " + GetUserName() )
   Html_LineText( HtmArch, 'Time from start: ' + TimeFromStart() )
   Html_RawText( HtmArch, '<span class="error">' + cText + '</span>' )
   Html_RawText( HtmArch, '</p>' )
   cText += CRLF + CRLF

   HTML_RawText( HtmArch, "<details><summary>" )
   HTML_RawText( HtmArch, PadC( " Stack Trace ", 79, "-" ) )
   HTML_RawText( HtmArch, '<br/></summary><span class="stacktrace">' )

   n := 1
   WHILE ! Empty( ProcName( ++n ) )
      HtmText := "Called from " + ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) + ")" + ;
         iif( ProcLine( n ) > 0, " in module: " + ProcFile( n ), "" ) + CRLF
      cText += HtmText
      Html_LineText( HtmArch, HtmText )
   ENDDO

   Html_RawText( HtmArch, "</span></details>" )

   SET CENTURY ( lOldSetState )

   IF _lShowDetailError()
      ErrorLog( HtmArch, oError )
   ENDIF

   Html_Line( HtmArch )
   Html_RawText( HtmArch, '</div>' )
   Html_End( HtmArch )

   ShowError( cText, oError )

   ExitProcess()

RETURN .F.

/*-----------------------------------------------------------------------------*
STATIC FUNCTION ErrorMessage( oError )
*------------------------------------------------------------------------------*
*
*  Description:
*     Formats an error message string from an error object.
*
*  Parameters:
*     oError - An error object containing information about the error.
*
*  Return Value:
*     A character string containing the formatted error message.
*
*  Purpose:
*     This function takes an error object and constructs a human-readable error message string.
*     It includes the error severity, subsystem, error code, description, filename or operation, and OS error code (if available).
*     The function is used to create a concise and informative error message for display to the user or logging to a file.
*
*  Notes:
*     The function uses a DO CASE statement to determine whether to include the filename or operation in the error message.
*     It also iterates through the error arguments (if any) and includes them in the message.
*
*/
STATIC FUNCTION ErrorMessage( oError )
   // start error message
   LOCAL cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "
   LOCAL n

   // add subsystem name if available
   IF ISCHARACTER( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUMBER( oError:subCode )
      cMessage += "/" + hb_ntos( oError:subCode )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += " " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE !Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   // add OS error code if available
   IF !Empty( oError:osCode )
      cMessage += CRLF
      cMessage += "OS Error: " + GetOSErrorDescription( oError:osCode )
   ENDIF

   IF ValType( oError:args ) == "A"
      cMessage += CRLF
      cMessage += "   Args:" + CRLF
      FOR n := 1 TO Len( oError:args )
         cMessage += ;
            "     [" + hb_ntos( n, 2 ) + "] = " + ValType( oError:args[ n ] ) + ;
            "   " + cValToChar( oError:args[ n ] ) + ;
            iif( ValType( oError:args[ n ] ) == "A", " length: " + ;
            hb_ntos( Len( oError:args[ n ] ) ), "" ) + iif( n < Len( oError:args ), CRLF, "" )
      NEXT
   ENDIF

RETURN cMessage

/*-----------------------------------------------------------------------------*
STATIC PROCEDURE ShowError( cErrorMessage, oError )
*------------------------------------------------------------------------------*
*
*  Description:
*     Displays an error message to the user, either as a detailed alert or a simple message box.
*
*  Parameters:
*     cErrorMessage - The full error message string to display.
*     oError - The error object (used for less detailed error messages).
*
*  Return Value:
*     None
*
*  Purpose:
*     This procedure is responsible for presenting the error information to the user.
*     It checks a global variable (_HMG_ShowError) to determine if error display is enabled.
*     It also checks the _lShowDetailError() function to determine whether to display the full error message or a simplified version.
*     The error is displayed either using an AlertStop (for detailed errors) or a MsgStop (for simple errors).
*     It also allows for custom initialization and exit code blocks to be executed before and after the error display.
*
*  Notes:
*     The procedure uses global variables to control the error display behavior.  It also uses conditional compilation (#ifdef _TSBROWSE_)
*     to handle specific cases related to the TSBrowse control.  The procedure sets the ErrorLevel to 1 to indicate that an error has occurred.
*
*/
STATIC PROCEDURE ShowError( cErrorMessage, oError )
   LOCAL cMsg := ""
   LOCAL bInit

   IF _SetGetGlobal( "_HMG_ShowError" ) == NIL
      STATIC _HMG_ShowError AS GLOBAL VALUE .T.
   ENDIF

   IF _SetGetGlobal( "_HMG_ShowError" )

      ASSIGN GLOBAL _HMG_ShowError := .F.
#ifdef _TSBROWSE_
      _TSB_aControlhWnd := {}
#endif
      IF ISBLOCK( _HMG_bOnErrorInit )
         cMsg := Eval( _HMG_bOnErrorInit, cMsg, oError, ErrorMessage( oError ), cErrorMessage )
      ENDIF

      cMsg += iif( _lShowDetailError(), cErrorMessage, ErrorMessage( oError ) )

      IF ISLOGICAL( _HMG_lOnErrorStop ) .AND. _HMG_lOnErrorStop

         bInit := {|| 
            LOCAL n := 2, ;
               aBackColor := hb_defaultValue( _HMG_aErrorBackColor, MAROON ), ;
               aFontColor := hb_defaultValue( _HMG_aErrorFontColor, WHITE ), ;
               aTopFontColor := hb_defaultValue( _HMG_aTopStringFontColor, YELLOW )
            EraseWindow( "oDlg" )
            SetProperty( "oDlg", "BackColor", aBackColor )
            DRAW ICON IN WINDOW oDlg AT 37, 16 PICTURE "ZZZ_B_STOP64" WIDTH 64 HEIGHT 64 COLOR aBackColor
            IF GetControlType( "Say_01", "oDlg" ) == "EDIT"
               SetProperty( "oDlg", "Say_01", "FontColor", aFontColor )
               SetProperty( "oDlg", "Say_01", "BackColor", aBackColor )
            ELSE
               SetProperty( "oDlg", "Say_01", "FontColor", aTopFontColor )
               SetProperty( "oDlg", "Say_01", "BackColor", aBackColor )
               SetProperty( "oDlg", "Say_01", "Alignment", "CENTER" )
               SetProperty( "oDlg", "Say_02", "FontColor", aTopFontColor )
               SetProperty( "oDlg", "Say_02", "BackColor", aBackColor )
               SetProperty( "oDlg", "Say_02", "Alignment", "CENTER" )
               WHILE _IsControlDefined( "Say_" + StrZero( ++n, 2 ), "oDlg" )
                  SetProperty( "oDlg", "Say_" + StrZero( n, 2 ), "FontColor", aFontColor )
                  SetProperty( "oDlg", "Say_" + StrZero( n, 2 ), "BackColor", aBackColor )
               END
            ENDIF
            RETURN NIL
            }

         IF AScan( _HMG_aFormType, 'A' ) == 0
            _HMG_MainWindowFirst := .F.
         ENDIF

         IF GetFontHandle( "DlgFont" ) == 0
            DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 14
         ENDIF

         HMG_Alert_MaxLines( iif( _lShowDetailError(), 35, 25 ) )

         AlertStop( cMsg, "Program Error: " + Upper( GetExeFileName() ), "ZZZ_B_STOP64", 64, { hb_defaultValue( _HMG_aButtonBackColor, { 217, 67, 67 } ) }, .T., bInit )

      ELSE

         MsgStop( StrTran( cMsg, ";", CRLF ), 'Program Error', NIL, .F. )

      ENDIF

      ErrorLevel( 1 )

      IF ISBLOCK( _HMG_bOnErrorExit )
         Eval( _HMG_bOnErrorExit )
      ENDIF

      ReleaseAllWindows()

   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
STATIC PROCEDURE ErrorLog( nHandle, oErr )
*------------------------------------------------------------------------------*
*
*  Description:
*     Logs detailed system and error information to an HTML file.
*
*  Parameters:
*     nHandle - The file handle of the HTML file to write to.
*     oErr - The error object containing information about the error.
*
*  Return Value:
*     None
*
*  Purpose:
*     This procedure writes detailed system and error information to an HTML file.
*     It includes information about the workstation, user, CPU, memory, disk space, operating system,
*     Harbour and MiniGUI versions, compiler, and various SET settings.  It also includes information about the current work area
*     and the error object itself.  The function is used to create a comprehensive error log for debugging purposes.
*
*  Notes:
*     The procedure uses conditional compilation (#ifdef __XHARBOUR__) to handle differences between Harbour and xHarbour.
*     It also uses the hb_WAEval() function to iterate through all work areas and retrieve information about each one.
*     The procedure includes information about available memory variables to aid in debugging memory-related issues.
*
*/
STATIC PROCEDURE ErrorLog( nHandle, oErr )
   STATIC _lAddError := .T.
#ifndef __XHARBOUR__
   LOCAL nScope, tmp, cName
#endif
   LOCAL nCount, xValue

   IF _lAddError

      _lAddError := .F.

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " System Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "Workstation name...: " + NetName() )
      Html_LineText( nHandle, "Active user name...: " + GetUserName() )
      xValue := GetCPUInfo()
      Html_LineText( nHandle, "CPU type...........: " + xValue[ 1 ] + " [~" + strvalue( xValue[ 2 ] ) + " MHz]" )
      Html_LineText( nHandle, "Hardware memory....: " + strvalue( MemoryStatus( 1 ) ) + " MB" )
      Html_LineText( nHandle, "Available memory...: " + strvalue( MemoryStatus( 2 ) ) + " MB" )
      Html_LineText( nHandle, "Current disk.......: " + DiskName() )
      Html_LineText( nHandle, "Current directory..: " + CurDir() )
#ifdef __XHARBOUR__
      Html_LineText( nHandle, "Free disk space....: " + strvalue( Round( DiskSpace() / ( 1024 * 1024 ), 0 ) ) + " MB" )
#else
      Html_LineText( nHandle, "Free disk space....: " + strvalue( Round( hb_DiskSpace( hb_DirBase() ) / ( 1024 * 1024 ), 0 ) ) + " MB" )
#endif
      Html_LineText( nHandle, "" )
      Html_LineText( nHandle, "Operating system...: " + OS() )
      Html_LineText( nHandle, "MiniGUI version....: " + MiniGUIVersion() )
      Html_LineText( nHandle, "Harbour version....: " + Version() )
#if ( __HARBOUR__ - 0 > 0x030200 )
      Html_LineText( nHandle, "Harbour built on...: " + hb_Version( HB_VERSION_BUILD_DATE_STR ) )
#else
      Html_LineText( nHandle, "Harbour built on...: " + hb_BuildDate() )
#endif
      Html_LineText( nHandle, "C/C++ compiler.....: " + hb_Compiler() )
#ifdef __XHARBOUR__
      Html_LineText( nHandle, "Multi Threading....: " + iif( Hb_MultiThread(), "YES", "NO" ) )
      Html_LineText( nHandle, "VM Optimization....: " + strvalue( hb_VMMode() ) )

      IF Type( "Select()" ) == "UI" .OR. Type( "Select()" ) == "N"
         Html_LineText( nHandle, "" )
         Html_LineText( nHandle, "Current Work Area..: " + strvalue( &("Select()") ) )
#else
      Html_LineText( nHandle, "Multi Threading....: " + iif( hb_mtvm(), "YES", "NO" ) )
      Html_LineText( nHandle, "VM Optimization....: " + iif( hb_VMMode() == 1, "YES", "NO" ) )

      IF hb_IsFunction( "Select" )
         Html_LineText( nHandle, "" )
         Html_LineText( nHandle, "Current Work Area..: " + strvalue( Eval( hb_macroBlock( "Select()" ) ) ) )
#endif
      ENDIF

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Environmental Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "SET ALTERNATE......: " + strvalue( Set( _SET_ALTERNATE ), .T. ) )
      Html_LineText( nHandle, "SET ALTFILE........: " + strvalue( Set( _SET_ALTFILE ) ) )
      Html_LineText( nHandle, "SET AUTOPEN........: " + strvalue( Set( _SET_AUTOPEN ), .T. ) )
      Html_LineText( nHandle, "SET AUTORDER.......: " + strvalue( Set( _SET_AUTORDER ) ) )
      Html_LineText( nHandle, "SET AUTOSHARE......: " + strvalue( Set( _SET_AUTOSHARE ) ) )

#ifdef __XHARBOUR__
      Html_LineText( nHandle, "SET BACKGROUNDTASKS: " + strvalue( Set( _SET_BACKGROUNDTASKS ), .T. ) )
      Html_LineText( nHandle, "SET BACKGROUNDTICK.: " + strvalue( Set( _SET_BACKGROUNDTICK ), .T. ) )
#endif
      Html_LineText( nHandle, "SET CENTURY........: " + strvalue( __SetCentury(), .T. ) )
      Html_LineText( nHandle, "SET COUNT..........: " + strvalue( Set( _SET_COUNT ) ) )

      Html_LineText( nHandle, "SET DATE FORMAT....: " + strvalue( Set( _SET_DATEFORMAT ) ) )
      Html_LineText( nHandle, "SET DBFLOCKSCHEME..: " + strvalue( Set( _SET_DBFLOCKSCHEME ) ) )
      Html_LineText( nHandle, "SET DEBUG..........: " + strvalue( Set( _SET_DEBUG ), .T. ) )
      Html_LineText( nHandle, "SET DECIMALS.......: " + strvalue( Set( _SET_DECIMALS ) ) )
      Html_LineText( nHandle, "SET DEFAULT........: " + strvalue( Set( _SET_DEFAULT ) ) )
      Html_LineText( nHandle, "SET DEFEXTENSIONS..: " + strvalue( Set( _SET_DEFEXTENSIONS ), .T. ) )
      Html_LineText( nHandle, "SET DELETED........: " + strvalue( Set( _SET_DELETED ), .T. ) )
      Html_LineText( nHandle, "SET DELIMCHARS.....: " + strvalue( Set( _SET_DELIMCHARS ) ) )
      Html_LineText( nHandle, "SET DELIMETERS.....: " + strvalue( Set( _SET_DELIMITERS ), .T. ) )
      Html_LineText( nHandle, "SET DIRCASE........: " + strvalue( Set( _SET_DIRCASE ) ) )
      Html_LineText( nHandle, "SET DIRSEPARATOR...: " + strvalue( Set( _SET_DIRSEPARATOR ) ) )

      Html_LineText( nHandle, "SET EOL............: " + strvalue( Asc( Set( _SET_EOL ) ) ) )
      Html_LineText( nHandle, "SET EPOCH..........: " + strvalue( Set( _SET_EPOCH ) ) )
      Html_LineText( nHandle, "SET ERRORLOG.......: " + strvalue( _GetErrorlogFile() ) )
#ifdef __XHARBOUR__
      Html_LineText( nHandle, "SET ERRORLOOP......: " + strvalue( Set( _SET_ERRORLOOP ) ) )
#endif
      Html_LineText( nHandle, "SET EXACT..........: " + strvalue( Set( _SET_EXACT ), .T. ) )
      Html_LineText( nHandle, "SET EXCLUSIVE......: " + strvalue( Set( _SET_EXCLUSIVE ), .T. ) )
      Html_LineText( nHandle, "SET EXTRA..........: " + strvalue( Set( _SET_EXTRA ), .T. ) )
      Html_LineText( nHandle, "SET EXTRAFILE......: " + strvalue( Set( _SET_EXTRAFILE ) ) )

      Html_LineText( nHandle, "SET FILECASE.......: " + strvalue( Set( _SET_FILECASE ) ) )
      Html_LineText( nHandle, "SET FIXED..........: " + strvalue( Set( _SET_FIXED ), .T. ) )
      Html_LineText( nHandle, "SET FORCEOPT.......: " + strvalue( Set( _SET_FORCEOPT ), .T. ) )

      Html_LineText( nHandle, "SET HARDCOMMIT.....: " + strvalue( Set( _SET_HARDCOMMIT ), .T. ) )

      Html_LineText( nHandle, "SET IDLEREPEAT.....: " + strvalue( Set( _SET_IDLEREPEAT ), .T. ) )

      Html_LineText( nHandle, "SET LANGUAGE.......: " + strvalue( Set( _SET_LANGUAGE ) ) )

      Html_LineText( nHandle, "SET MARGIN.........: " + strvalue( Set( _SET_MARGIN ) ) )
      Html_LineText( nHandle, "SET MBLOCKSIZE.....: " + strvalue( Set( _SET_MBLOCKSIZE ) ) )
      Html_LineText( nHandle, "SET MFILEEXT.......: " + strvalue( Set( _SET_MFILEEXT ) ) )

      Html_LineText( nHandle, "SET OPTIMIZE.......: " + strvalue( Set( _SET_OPTIMIZE ), .T. ) )
#ifdef __XHARBOUR__
      Html_LineText( nHandle, "SET OUTPUTSAFETY...: " + strvalue( Set( _SET_OUTPUTSAFETY ), .T. ) )
#endif

      Html_LineText( nHandle, "SET PATH...........: " + strvalue( Set( _SET_PATH ) ) )
      Html_LineText( nHandle, "SET PRINTER........: " + strvalue( Set( _SET_PRINTER ), .T. ) )
#ifdef __XHARBOUR__
      Html_LineText( nHandle, "SET PRINTERJOB.....: " + strvalue( Set( _SET_PRINTERJOB ) ) )
#endif
      Html_LineText( nHandle, "SET PRINTFILE......: " + strvalue( Set( _SET_PRINTFILE ) ) )

      Html_LineText( nHandle, "SET SOFTSEEK.......: " + strvalue( Set( _SET_SOFTSEEK ), .T. ) )

#ifdef __XHARBOUR__
      Html_LineText( nHandle, "SET TRACE..........: " + strvalue( Set( _SET_TRACE ), .T. ) )
      Html_LineText( nHandle, "SET TRACEFILE......: " + strvalue( Set( _SET_TRACEFILE ) ) )
      Html_LineText( nHandle, "SET TRACESTACK.....: " + strvalue( Set( _SET_TRACESTACK ) ) )
#endif
      Html_LineText( nHandle, "SET TRIMFILENAME...: " + strvalue( Set( _SET_TRIMFILENAME ) ) )

      Html_LineText( nHandle, "SET UNIQUE.........: " + strvalue( Set( _SET_UNIQUE ), .T. ) )

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Detailed Work Area Items ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

#ifdef __XHARBOUR__
      IF Type( "Select()" ) == "UI" .OR. Type( "Select()" ) == "N"
         FOR nCount := 1 TO 250
            IF !Empty( ( nCount )->( &("Alias()" ) ) )
               ( nCount )->( Html_LineText( nHandle, "Work Area No ......: " + strvalue( &("Select()" ) ) ) )
               ( nCount )->( Html_LineText( nHandle, "Alias .............: " + &("Alias()" ) ) )
               ( nCount )->( Html_LineText( nHandle, "Current Recno .....: " + strvalue( &("RecNo()" ) ) ) )
               ( nCount )->( Html_LineText( nHandle, "Current Filter ....: " + &("DbFilter()" ) ) )
               ( nCount )->( Html_LineText( nHandle, "Relation Exp. .....: " + &("DbRelation()" ) ) )
               ( nCount )->( Html_LineText( nHandle, "Index Order .......: " + strvalue( &("IndexOrd(0)" ) ) ) )
               ( nCount )->( Html_LineText( nHandle, "Active Key ........: " + strvalue( &("IndexKey(0)" ) ) ) )
               Html_LineText( nHandle, "" )
            ENDIF
         NEXT
      ENDIF
#else
      hb_WAEval( {||
         IF hb_IsFunction( "Select" )
            Html_LineText( nHandle, "Work Area No ......: " + strvalue( Do( "Select" ) ) )
         ENDIF
         IF hb_IsFunction( "Alias" )
            Html_LineText( nHandle, "Alias .............: " + Do( "Alias" ) )
         ENDIF
         IF hb_IsFunction( "RecNo" )
            Html_LineText( nHandle, "Current Recno .....: " + strvalue( Do( "RecNo" ) ) )
         ENDIF
         IF hb_IsFunction( "dbFilter" )
            Html_LineText( nHandle, "Current Filter ....: " + Do( "dbFilter" ) )
         ENDIF
         IF hb_IsFunction( "dbRelation" )
            Html_LineText( nHandle, "Relation Exp. .....: " + Do( "dbRelation" ) )
         ENDIF
         IF hb_IsFunction( "IndexOrd" )
            Html_LineText( nHandle, "Index Order .......: " + strvalue( Do( "IndexOrd" ) ) )
         ENDIF
         IF hb_IsFunction( "IndexKey" )
            Html_LineText( nHandle, "Active Key ........: " + strvalue( Eval( hb_macroBlock( "IndexKey( 0 )" ) ) ) )
         ENDIF
         Html_LineText( nHandle, "" )
         RETURN .T.
         } )
#endif

      HTML_RawText( nHandle, "</details>" )

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Internal Error Handling Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "Subsystem Call ....: " + oErr:subsystem )
      Html_LineText( nHandle, "System Code .......: " + strvalue( oErr:subcode ) )
      Html_LineText( nHandle, "Default Status ....: " + strvalue( oErr:candefault ) )
      Html_LineText( nHandle, "Description .......: " + oErr:description )
      IF !Empty( oErr:filename )
         Html_LineText( nHandle, "Involved File .....: " + oErr:filename )
      ENDIF
      IF !Empty( oErr:operation )
         Html_LineText( nHandle, "Operation .........: " + oErr:operation )
      ENDIF
      IF !Empty( oErr:oscode )
         Html_LineText( nHandle, "OS Error Code .....: " + GetOSErrorDescription( oErr:oscode ) )
      ENDIF

#ifdef __XHARBOUR__
   #ifdef HB_THREAD_SUPPORT
      Html_LineText( nHandle, "Running threads ...: " + strvalue( oErr:RunningThreads() ) )
      Html_LineText( nHandle, "VM thread ID ......: " + strvalue( oErr:VmThreadId() ) )
      Html_LineText( nHandle, "OS thread ID ......: " + strvalue( oErr:OsThreadId() ) )
   #endif
      HTML_RawText( nHandle, "</details>" )
#else
      HTML_RawText( nHandle, "</details>" )

      /* NOTE: Adapted from hb_mvSave() source in Harbour RTL. */
      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " Available Memory Variables ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      FOR EACH nScope IN { HB_MV_PUBLIC, HB_MV_PRIVATE }

         nCount := __mvDbgInfo( nScope )
         FOR tmp := 1 TO nCount

            xValue := __mvDbgInfo( nScope, tmp, @cName )
            IF ValType( xValue ) $ "CNDTL" .AND. Left( cName, 1 ) <> "_"
               Html_LineText( nHandle, "     " + cName + " TYPE " + ValType( xValue ) + " [" + hb_CStr( xValue ) + "]" )
            ENDIF

         NEXT

      NEXT

      IF nCount > 0
         Html_LineText( nHandle, "" )
      ENDIF

      HTML_RawText( nHandle, "</details>" )
#endif
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
FUNCTION strvalue( c, l )
*------------------------------------------------------------------------------*
*  Description:
*     Converts a value of various data types to a string representation.
*
*  Parameters:
*     c - The value to convert. Can be of type Character, Memo, Numeric, Date, or Logical.
*     l - (Optional) A logical value used only when 'c' is a Logical type.  If .T., uses "ON"/"OFF" for true/false. If .F. or omitted, uses ".T."/".F.".
*
*  Return Value:
*     A string representation of the input value 'c'. Returns an empty string if the data type of 'c' is not handled.
*
*  Purpose:
*     This function provides a unified way to convert different data types into strings for display or storage.
*     It handles Character, Memo, Numeric, Date, and Logical types, providing specific formatting for each.
*     The 'l' parameter allows for customizing the string representation of logical values.
*
*  Notes:
*     The function uses a SWITCH statement to determine the data type of the input value.
*     The hb_defaultValue() function is used to provide a default value for the optional 'l' parameter.
*/
STATIC FUNCTION strvalue( c, l )

   SWITCH ValType( c )
   CASE "C"
   CASE "M" ; RETURN c
   CASE "N" ; RETURN hb_ntos( c )
   CASE "D" ; RETURN DToC( c )
   CASE "L" ; RETURN iif( hb_defaultValue( l, .F. ), iif( c, "ON", "OFF" ), iif( c, ".T.", ".F." ) )
   ENDSWITCH

RETURN ""

/*-----------------------------------------------------------------------------*
FUNCTION _lShowDetailError( lNewValue )
*------------------------------------------------------------------------------*
*  Description:
*     Enables or disables the display of detailed error information.
*
*  Parameters:
*     lNewValue - A logical value indicating whether to show detailed error information.
*                   .T. enables detailed error display, .F. disables it.
*
*  Return Value:
*     The previous logical value indicating whether detailed error information was being displayed.
*
*  Purpose:
*     This function controls the level of detail shown in error messages.  It allows the application to
*     switch between displaying concise error messages (for end-users) and detailed error messages
*     (for debugging purposes).  It uses a global variable to store the current setting.
*
*  Side Effects:
*     Modifies a global variable named "_HMG" + ProcName() to store the error detail setting.
*
*  Notes:
*     The function uses _AddNewGlobal to create the global variable if it doesn't exist.
*     The ProcName() function is used to dynamically generate the name of the global variable,
*     making it specific to this function.
*/
FUNCTION _lShowDetailError( lNewValue )
   LOCAL cVarName := "_HMG" + ProcName()
   LOCAL lOldValue := _AddNewGlobal( cVarName, .T. )

   IF ISLOGICAL( lNewValue )
      _SetGetGlobal( cVarName, lNewValue )
   ENDIF

RETURN lOldValue

/*-----------------------------------------------------------------------------*
FUNCTION HTML_ERRORLOG()
*------------------------------------------------------------------------------*
*  Description:
*     Creates or opens an HTML file for logging errors.
*
*  Parameters:
*     None
*
*  Return Value:
*     A file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) to the opened HTML error log file, or -1/NIL if the file could not be opened or error logging is disabled.
*
*  Purpose:
*     This function manages the creation and opening of an HTML file used for logging application errors.
*     It checks if error logging is enabled and either creates a new file or opens an existing one for appending error information.
*     The HTML format allows for easy viewing of the error log in a web browser.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*     The __HTML_INSERT_OFFSET() function is used to determine the position to insert new error information before the closing </BODY></HTML> tags.
*/
FUNCTION HTML_ERRORLOG()
   LOCAL HtmArch
   LOCAL cErrorLogFile := _GetErrorlogFile()

   IF IsErrorLogActive()
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      IF .NOT. File( cErrorLogFile )
         HtmArch := Html_Ini( cErrorLogFile, "Harbour MiniGUI Errorlog File" )
         IF HtmArch > 0
#else
      IF .NOT. hb_vfExists( cErrorLogFile )
         HtmArch := Html_Ini( cErrorLogFile, "Harbour MiniGUI Errorlog File" )
         IF HtmArch != NIL
#endif
            Html_Line( HtmArch )
         ENDIF
      ELSE
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
         HtmArch := FOpen( cErrorLogFile, FO_READWRITE )
         IF HtmArch > 0
            FSeek( HtmArch, __HTML_INSERT_OFFSET(), FS_END )
#else
         HtmArch := hb_vfOpen( cErrorLogFile, FO_WRITE )
         IF HtmArch != NIL
            hb_vfSeek( HtmArch, __HTML_INSERT_OFFSET(), FS_END )
#endif
         ENDIF
      ENDIF
   ENDIF

RETURN ( HtmArch )

/*-----------------------------------------------------------------------------*
FUNCTION HTML_INI( ARCH, TITLE )
*------------------------------------------------------------------------------*
*  Description:
*     Initializes an HTML file for error logging by creating the file and writing the initial HTML structure.
*
*  Parameters:
*     ARCH  - The file name (including path) of the HTML file to create.
*     TITLE - The title to be displayed in the HTML page's title bar and heading.
*
*  Return Value:
*     A file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) to the newly created HTML file, or -1/NIL if the file could not be created or error logging is disabled.
*
*  Purpose:
*     This function creates a new HTML file and writes the basic HTML structure (DOCTYPE, HTML, HEAD, BODY tags)
*     along with the specified title.  It's used to initialize the error log file before writing error messages.
*     The function also handles character encoding (UTF-8) if the system code page is set to UTF-8.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*     The __HTML_BODY_TEMPLATE() function returns a base64-encoded string containing the basic HTML structure.
*     The StrTran() function is used to replace placeholders in the HTML template with the provided title and character encoding.
*/
FUNCTION HTML_INI( ARCH, TITLE )
   LOCAL HtmArch := -1, cTemplate

   IF IsErrorLogActive()
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      HtmArch := FCreate( ARCH )
      IF FError() != 0
#else
      HtmArch := hb_vfOpen( ARCH, FO_CREAT + FO_TRUNC + FO_WRITE )
      IF HtmArch == NIL
#endif
         MsgStop( "Can`t open errorlog file " + ARCH, "Error" )
      ELSE
         cTemplate := __HTML_BODY_TEMPLATE()
         cTemplate := StrTran( cTemplate, "{{TITLE}}", TITLE )
         IF Set( _SET_CODEPAGE ) == "UTF8"
            cTemplate := StrTran( cTemplate, ["windows-1251"], ["utf-8"] )
         ENDIF
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
         FWrite( HtmArch, cTemplate )
#else
         hb_vfWrite( HtmArch, cTemplate )
#endif
      ENDIF
   ENDIF

RETURN ( HtmArch )

/*-----------------------------------------------------------------------------*
PROCEDURE HTML_RAWTEXT( HTMARCH, LINEA )
*------------------------------------------------------------------------------*
*  Description:
*     Writes a line of raw text to an HTML file without any HTML formatting.
*
*  Parameters:
*     HTMARCH - The file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) of the opened HTML file.
*     LINEA   - The string to write to the file.
*
*  Purpose:
*     This procedure writes a given string to the specified HTML file. It's used for writing plain text content
*     to the error log without any HTML tags or formatting.  It appends a carriage return and line feed (CRLF)
*     to the end of the line.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*     The RTrim() function is used to remove trailing spaces from the input string.
*/
PROCEDURE HTML_RAWTEXT( HTMARCH, LINEA )
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + CRLF )
#endif
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINETEXT( HTMARCH, LINEA )
*------------------------------------------------------------------------------*
*  Description:
*     Writes a line of text to an HTML file, adding a line break (<BR>) tag.
*
*  Parameters:
*     HTMARCH - The file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) of the opened HTML file.
*     LINEA   - The string to write to the file.
*
*  Purpose:
*     This procedure writes a given string to the specified HTML file, adding an HTML line break tag (<BR>)
*     at the end of the line. This ensures that each line of text is displayed on a new line in the HTML output.
*     It's used for writing formatted text content to the error log.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*     The RTrim() function is used to remove trailing spaces from the input string.
*/
PROCEDURE HTML_LINETEXT( HTMARCH, LINEA )
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + CRLF )
#endif
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINE( HTMARCH )
*------------------------------------------------------------------------------*
*  Description:
*     Writes a horizontal rule (<HR>) tag to an HTML file.
*
*  Parameters:
*     HTMARCH - The file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) of the opened HTML file.
*
*  Purpose:
*     This procedure writes an HTML horizontal rule tag (<HR>) to the specified HTML file.
*     This tag creates a horizontal line in the HTML output, used to visually separate sections of the error log.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*/
PROCEDURE HTML_LINE( HTMARCH )
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, "<HR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, "<HR>" + CRLF )
#endif
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
PROCEDURE HTML_END( HTMARCH )
*------------------------------------------------------------------------------*
*  Description:
*     Closes an HTML file, writing the closing HTML tags (</BODY></HTML>).
*
*  Parameters:
*     HTMARCH - The file handle (numeric in xHarbour/older Harbour, NIL in newer Harbour) of the opened HTML file.
*
*  Purpose:
*     This procedure writes the closing HTML tags (</BODY></HTML>) to the specified HTML file and then closes the file.
*     This completes the HTML structure and ensures that the error log file is properly saved.
*
*  Notes:
*     The function uses preprocessor directives (#if defined...) to handle differences between xHarbour/older Harbour and newer Harbour versions regarding file handling functions.
*/
PROCEDURE HTML_END( HTMARCH )
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, "</BODY></HTML>" )
      FClose( HTMARCH )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, "</BODY></HTML>" )
      hb_vfClose( HTMARCH )
#endif
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_INSERT_OFFSET()
*------------------------------------------------------------------------------*
*  Description:
*     Calculates the offset from the end of the file to insert new HTML content before the closing tags.
*
*  Parameters:
*     None
*
*  Return Value:
*     A negative number representing the offset from the end of the file where new HTML content should be inserted.
*
*  Purpose:
*     This function calculates the offset needed to insert new error log entries before the closing </BODY></HTML> tags
*     in the HTML error log file. This allows the error log to be appended to without overwriting the closing tags.
*
*  Notes:
*     The function returns a negative value because it represents an offset from the end of the file.
*/
STATIC FUNCTION __HTML_INSERT_OFFSET()
RETURN ( -1 ) * Len( "</BODY></HTML>" )

/*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_BODY_TEMPLATE()
*------------------------------------------------------------------------------*
*  Description:
*     Returns a base64-decoded string containing the HTML body template for the error log file.
*
*  Parameters:
*     None
*
*  Return Value:
*     A string containing the HTML body template.
*
*  Purpose:
*     This function provides the basic HTML structure for the error log file. The HTML structure is stored as a base64-encoded string
*     to avoid issues with character encoding and to make it easier to include in the source code.
*     The template includes the DOCTYPE declaration, HTML, HEAD, and BODY tags, as well as some basic CSS styling and JavaScript for filtering.
*
*  Notes:
*     The base64-encoded string contains placeholders (e.g., "{{TITLE}}") that are replaced with actual values at runtime.
*     The JavaScript code included in the template provides basic filtering functionality for the error log.
*/
STATIC FUNCTION __HTML_BODY_TEMPLATE()
RETURN hb_base64Decode( "PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PG1ldGEgY2hhcnNldD0id2luZG93cy0xMjUxIj48dGl0bGU+e3tUSVRMRX19PC90aXRsZT48c3R5bGU+Ym9keXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2JhY2tncm91bmQtY29sb3I6I2ZmZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO3BhZGRpbmc6MTVweH0uc3VtbWFyeSxkZXRhaWxzIHN1bW1hcnl7Y29sb3I6IzA2OTtiYWNrZ3JvdW5kOiNmZmM7Ym9yZGVyOjFweCBzb2xpZCAjOWFmO3BhZGRpbmc6NXB4O21hcmdpbjoxMHB4IDVweDtjdXJzb3I6cG9pbnRlcn0ubGlua3tkaXNwbGF5OmJsb2NrO2JhY2tncm91bmQ6I2NmYzt0ZXh0LWRlY29yYXRpb246bm9uZX1oMXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2ZvbnQtc2l6ZToxNTAlO2NvbG9yOiMwMGM7Zm9udC13ZWlnaHQ6NzAwO2JhY2tncm91bmQtY29sb3I6I2YwZjBmMH0udXBkYXRlZHtmb250LWZhbWlseTpzYW5zLXNlcmlmO2NvbG9yOiNjMDA7Zm9udC1zaXplOjExMCV9Lm5vcm1hbHRleHR7Zm9udC1mYW1pbHk6c2Fucy1zZXJpZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO2ZvbnQtd2VpZ2h0OjQwMDt0ZXh0LXRyYW5zZm9ybTpub25lO3RleHQtZGVjb3JhdGlvbjpub25lfS5sYXJnZS1zZWxlY3R7Zm9udC1zaXplOjEyNSU7cGFkZGluZzo4cHg7bWFyZ2luOjVweDtiYWNrZ3JvdW5kOiNjZGZ9PC9zdHlsZT48c2NyaXB0PmNvbnN0IGZpbHRlckJ5PShyLGUsbCxuPW51bGwpPT57bGV0IHQ9ci5tYXAobCk7dD10LnJlZHVjZSgoZSx0KT0+KHQgaW4gZXx8KGVbdF09MCksZVt0XSsrLGUpLHt9KSx0PU9iamVjdC5lbnRyaWVzKHQpLnJlZHVjZSgoZSxbdCxyXSk9PihlW3RdPVt0LHIsYFske3J9XSAke3R9YF0sZSkse30pO2NvbnN0IGM9ZG9jdW1lbnQucXVlcnlTZWxlY3RvcihlKTtPYmplY3QudmFsdWVzKHQpLnNvcnQoKGUsdCk9PnRbMV0tZVsxXSkuZm9yRWFjaCgoW2UsLHRdKT0+e2NvbnN0IHI9ZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgib3B0aW9uIik7ci52YWx1ZT1lLHIuaW5uZXJUZXh0PXQsYy5hcHBlbmRDaGlsZChyKX0pO2MuYWRkRXZlbnRMaXN0ZW5lcigiY2hhbmdlIixlPT57biYmbihjKTtjb25zdCB0PWUudGFyZ2V0LnZhbHVlO3IuZm9yRWFjaChlPT4oKGUsdCk9Pnt2YXIgcjsibnVsbCIhPT10PyhyPWwoZSksZS5zdHlsZS5kaXNwbGF5PXI9PT10P251bGw6Im5vbmUiLGNvbnNvbGUubG9nKGUsZS5zdHlsZS5kaXNwbGF5KSk6ZS5zdHlsZS5kaXNwbGF5PW51bGx9KShlLHQpKX0pfTtkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKCJET01Db250ZW50TG9hZGVkIixmdW5jdGlvbihlKXt2YXIgdD1bLi4uZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbCgiLnJlY29yZCIpXSxyPXQ9PmRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoIiNleHRyYS1wYW5lbCIpLnF1ZXJ5U2VsZWN0b3JBbGwoInNlbGVjdCIpLmZvckVhY2goZT0+dCE9PWUmJihlLnNlbGVjdGVkSW5kZXg9MCkpO2ZpbHRlckJ5KHQsIiNmaWx0ZXJCeURhdGUiLGU9PmUucXVlcnlTZWxlY3RvcigiLmRhdGUiKS5pbm5lclRleHQsciksZmlsdGVyQnkodCwiI2ZpbHRlckJ5U3RhY2t0cmFjZSIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuc3RhY2t0cmFjZSIpPy5jaGlsZE5vZGVzWzBdPy5kYXRhLnRyaW0oKSxyKSxmaWx0ZXJCeSh0LCIjZmlsdGVyQnlFcnJvciIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuZXJyb3IiKS5pbm5lclRleHQucmVwbGFjZSgvKFxyXG58XG58XHIpL2dtLCIiKS5yZXBsYWNlKC9cc3syLH0vZywiICIpLnRyaW0oKSxyKSxkb2N1bWVudC5xdWVyeVNlbGVjdG9yKCIjZXh0cmEtcGFuZWwiKS5zdHlsZS5kaXNwbGF5PW51bGwsY29uc29sZS5sb2coIkRPTUNvbnRlbnRMb2FkZWQiKX0pPC9zY3JpcHQ+PC9oZWFkPjxib2R5PjxoMSBzdHlsZT0idGV4dC1hbGlnbjpjZW50ZXIiPnt7VElUTEV9fTwvaDE+PGRpdiBpZD0iZXh0cmEtcGFuZWwiIHN0eWxlPSJkaXNwbGF5Om5vbmUiPjxzZWxlY3QgYXV0b2NvbXBsZXRlPSJvZmYiIGlkPSJmaWx0ZXJCeURhdGUiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRGF0ZTwvb3B0aW9uPjxvcHRpb24gdmFsdWU9Im51bGwiPkFsbCBkYXRlczwvb3B0aW9uPjwvc2VsZWN0PiA8c2VsZWN0IGF1dG9jb21wbGV0ZT0ib2ZmIiBpZD0iZmlsdGVyQnlTdGFja3RyYWNlIiBjbGFzcz0ibGFyZ2Utc2VsZWN0Ij48b3B0aW9uIHNlbGVjdGVkPSJzZWxlY3RlZCIgZGlzYWJsZWQ9ImRpc2FibGVkIiB2YWx1ZT0ibnVsbCI+RmlsdGVyIGJ5IFN0YWNrVHJhY2U8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgc3RhY2t0cmFjZXM8L29wdGlvbj48L3NlbGVjdD4gPHNlbGVjdCBhdXRvY29tcGxldGU9Im9mZiIgaWQ9ImZpbHRlckJ5RXJyb3IiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRXJyb3I8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgZXJyb3JzPC9vcHRpb24+PC9zZWxlY3Q+PC9kaXY+PC9ib2R5PjwvaHRtbD4=" )

/*-----------------------------------------------------------------------------*
FUNCTION GetCPUInfo()
*------------------------------------------------------------------------------*
*  Description:
*     Retrieves CPU information from the Windows Registry.
*
*  Parameters:
*     None
*
*  Return Value:
*     An array containing two elements:
*       [1] - The processor name string (e.g., "Intel(R) Core(TM) i7-8700K CPU @ 3.70GHz").
*       [2] - The processor speed in MHz (as a number).
*
*  Purpose:
*     This function retrieves the CPU name and speed from the Windows Registry.
*     This information can be used for system information display or for diagnostic purposes.
*
*  Notes:
*     The function uses the GetRegistryValue() function to read the values from the registry.
*     The registry key and value names are hardcoded in the function.
*     If the registry values are not found, the corresponding array elements will be empty.
*/
FUNCTION GetCPUInfo()
   LOCAL aRetVal := Array( 2 )
   LOCAL cKey := "HARDWARE\DESCRIPTION\System\CentralProcessor\0"

   aRetVal[ 1 ] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "ProcessorNameString" )
   aRetVal[ 2 ] := GetRegistryValue( HKEY_LOCAL_MACHINE, cKey, "~MHz", "N" )

RETURN aRetVal

/*-----------------------------------------------------------------------------*
FUNCTION GetOSErrorDescription( nError )
*------------------------------------------------------------------------------*
*  Description:
*     Returns a textual description for a given operating system error code.
*
*  Parameters:
*     nError - The operating system error code (a numeric value).
*
*  Return Value:
*     A string containing the error code and its description (e.g., "2=File not found").
*     If the error code is not found in the internal list, returns "nError=Unknown error".
*
*  Purpose:
*     This function provides a user-friendly description for operating system error codes.
*     It maintains an internal array of error codes and their corresponding descriptions.
*     This allows the application to display meaningful error messages to the user instead of just numeric error codes.
*
*  Notes:
*     The function uses a static array to store the error codes and descriptions.
*     The array is initialized only once when the function is first called.
*     The AScan() function is used to search for the error code in the array.
*/
FUNCTION GetOSErrorDescription( nError )
   LOCAL nPos
   STATIC aError := {}

   IF Len( aError ) == 0
      AAdd( aError, {  1, "Invalid function number                  " } )
      AAdd( aError, {  2, "File not found                           " } )
      AAdd( aError, {  3, "Path not found                           " } )
      AAdd( aError, {  4, "Too many open files (no handles left)    " } )
      AAdd( aError, {  5, "Access denied                            " } )
      AAdd( aError, {  6, "Invalid handle                           " } )
      AAdd( aError, {  7, "Memory control blocks destroyed          " } )
      AAdd( aError, {  8, "Insufficient memory                      " } )
      AAdd( aError, {  9, "Invalid memory block address             " } )
      AAdd( aError, { 10, "Invalid environment                      " } )
      AAdd( aError, { 11, "Invalid format                           " } )
      AAdd( aError, { 12, "Invalid access code                      " } )
      AAdd( aError, { 13, "Invalid data                             " } )
      AAdd( aError, { 15, "Invalid drive was specified              " } )
      AAdd( aError, { 16, "Attempt to remove the current directory  " } )
      AAdd( aError, { 17, "Not same device                          " } )
      AAdd( aError, { 18, "No more files                            " } )
      AAdd( aError, { 19, "Attempt to write to write-protected media" } )
      AAdd( aError, { 20, "Unknown unit                             " } )
      AAdd( aError, { 21, "Drive not ready                          " } )
      AAdd( aError, { 22, "Unknown command                          " } )
      AAdd( aError, { 23, "Data CRC error                           " } )
      AAdd( aError, { 24, "Bad request structure length             " } )
      AAdd( aError, { 25, "Seek error                               " } )
      AAdd( aError, { 26, "Unknown media type                       " } )
      AAdd( aError, { 27, "Sector not found                         " } )
      AAdd( aError, { 28, "Printer out of paper                     " } )
      AAdd( aError, { 29, "Write fault                              " } )
      AAdd( aError, { 30, "Read fault                               " } )
      AAdd( aError, { 31, "General failure                          " } )
      AAdd( aError, { 32, "Sharing violation                        " } )
      AAdd( aError, { 33, "Lock violation                           " } )
      AAdd( aError, { 34, "Invalid disk change                      " } )
      AAdd( aError, { 35, "FCB unavailable                          " } )
      AAdd( aError, { 36, "Sharing buffer overflow                  " } )
      AAdd( aError, { 38, "Unable to complete the operation         " } )
      AAdd( aError, { 50, "Network request not supported            " } )
      AAdd( aError, { 51, "Remote computer not listening            " } )
      AAdd( aError, { 52, "Duplicate name on network                " } )
      AAdd( aError, { 53, "Network path not found                   " } )
      AAdd( aError, { 54, "Network busy                             " } )
      AAdd( aError, { 55, "Network device no longer exists          " } )
      AAdd( aError, { 56, "NETBIOS command limit exceeded           " } )
      AAdd( aError, { 57, "System error, NETBIOS error              " } )
      AAdd( aError, { 58, "Incorrect response from network          " } )
      AAdd( aError, { 59, "Unexpected network error                 " } )
      AAdd( aError, { 60, "Incompatible remote adapter              " } )
      AAdd( aError, { 61, "Print queue full                         " } )
      AAdd( aError, { 62, "Not enough space for print file          " } )
      AAdd( aError, { 63, "Print file was cancelled                 " } )
      AAdd( aError, { 64, "Network name was denied                  " } )
      AAdd( aError, { 65, "Access denied                            " } )
      AAdd( aError, { 66, "Network device type incorrect            " } )
      AAdd( aError, { 67, "Network name not found                   " } )
      AAdd( aError, { 68, "Network name limit exceeded              " } )
      AAdd( aError, { 69, "NETBIOS session limit exceeded           " } )
      AAdd( aError, { 70, "Sharing temporarily paused               " } )
      AAdd( aError, { 71, "Network request not accepted             " } )
      AAdd( aError, { 72, "Print or disk redirection is paused      " } )
      AAdd( aError, { 80, "File exists                              " } )
      AAdd( aError, { 82, "Cannot make directory entry              " } )
      AAdd( aError, { 83, "Fail on INT 24                           " } )
      AAdd( aError, { 84, "Too many redirections                    " } )
      AAdd( aError, { 85, "Duplicate redirection                    " } )
      AAdd( aError, { 86, "Invalid password                         " } )
      AAdd( aError, { 87, "Invalid parameter                        " } )
      AAdd( aError, { 88, "Network data fault                       " } )
      AAdd( aError, { 89, "Function not supported by network        " } )
      AAdd( aError, { 90, "Required system component not installed  " } )
   ENDIF

   IF ( nPos := AScan( aError, {| x | x[ 1 ] == nError } ) ) > 0
      RETURN hb_ntos( aError[ nPos ][ 1 ] ) + "=" + Trim( aError[ nPos ][ 2 ] )
   ENDIF

RETURN hb_ntos( nError ) + "=Unknown error"

// (JK) HMG 1.0 Build 6
*-----------------------------------------------------------------------------*
PROCEDURE _SetErrorLogFile( cFile )
*-----------------------------------------------------------------------------*
   _HMG_ErrorLogFile := IFEMPTY( cFile, GetStartUpFolder() + hb_ps() + "ErrorLog.htm", cFile )

RETURN
