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
   Copyright 1999-2021, https://harbour.github.io/

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
*-----------------------------------------------------------------------------*
PROCEDURE ErrorSys
*-----------------------------------------------------------------------------*
   ErrorBlock( { | oError | DefError( oError ) } )
#ifndef __XHARBOUR__
   Set( _SET_HBOUTLOG, GetStartUpFolder() + "\error.log" )
   Set( _SET_HBOUTLOGINFO, MiniGUIVersion() )
#endif

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION DefError( oError )
*-----------------------------------------------------------------------------*
   LOCAL lOldSetState := ( Len( DToC( Date() ) ) == 10 )
   LOCAL cText
   LOCAL HtmArch
   LOCAL HtmText
   LOCAL n
   LOCAL cFileScr, cLang

   //? "##### " + ProcNL()
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

   IF TYPE('cPubGlobLang') != "U"
      cLang := Upper( Left( Set ( _SET_LANGUAGE ), 2 ) )
      IF cLang # M->cPubGlobLang    // смотреть main.prg
         // установить язык вывода, который был при старте программы
         hb_CdpSelect(M->cPubLangCdp)
      ENDIF
   ENDIF

   HtmArch := Html_ErrorLog()
   cText   := ErrorMessage( oError )

   Html_RawText( HtmArch, '<div class="record">' )
   Html_RawText( HtmArch, '<p class="updated">' )
   Html_LineText( HtmArch, 'Date: <span class="date">' + DToC( Date() ) + '</span> ' + 'Time: <span class="time">' + Time() + '</span>' )
   // эти строки по желанию
   Html_LineText( HtmArch, 'Application: ' + GetExeFileName() + "    " + M->cPubVersProg )
   Html_LineText( HtmArch, 'User: ' + NetName()+"/"+hb_UserName()+"/"+M->cOperator )
   IF SELECT() > 0
      IF INDEXORD() > 0
        Html_LineText( HtmArch, 'DbInfo: Alias - '+ ALIAS() + ', Ord - ' + OrdSetFocus() + ;
                               ', Recno - ' + HB_NtoS(RecNo()) + '/' + HB_NtoS(LastRec()) )
      ELSE
        Html_LineText( HtmArch, 'DbInfo: Alias - '+ ALIAS() + ;
                               ', Recno - ' + HB_NtoS(RecNo()) + '/' + HB_NtoS(LastRec()) )
      ENDIF
   ELSE
   Html_LineText( HtmArch, 'DbInfo: Alias - '+ ALIAS() )
   ENDIF

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

   // запись экрана ошибки / record error screen
   cFileScr := _SaveScreenError(oError)
   Html_RawText( HtmArch, '<a target="blank" class="summary link" href="' + cFileScr + '">Screenshot: ' + cFileScr + '</a>' )

   Html_Line( HtmArch )
   Html_RawText( HtmArch, '</div>' )
   Html_End( HtmArch )

   ShowError( cText, oError )

   ExitProcess()

RETURN .F.

*-----------------------------------------------------------------------------*
STATIC FUNCTION ErrorMessage( oError )
*-----------------------------------------------------------------------------*
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
      cMessage += "  " + oError:description
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
      cMessage += " (DOS Error " + hb_ntos( oError:osCode ) + ")"
   ENDIF

   IF ValType( oError:args ) == "A"
      cMessage += CRLF
      cMessage += "   Args:" + CRLF
      FOR n := 1 TO Len( oError:args )
         cMessage += ;
            "     [" + hb_ntos( n, 2 ) + "] = " + ValType( oError:args[ n ] ) + ;
            "   " + cValToChar( cValToChar( oError:args[ n ] ) ) + ;
            iif( ValType( oError:args[ n ] ) == "A", " length: " + ;
            hb_ntos( Len( oError:args[ n ] ) ), "" ) + iif( n < Len( oError:args ), CRLF, "" )
      NEXT
   ENDIF

RETURN cMessage

*-----------------------------------------------------------------------------*
STATIC PROCEDURE ShowError( cErrorMessage, oError )
*-----------------------------------------------------------------------------*
   LOCAL cMsg := ""

   STATIC _lShowError := .T.

   IF _lShowError

      _lShowError := .F.
#ifdef _TSBROWSE_
      _TSB_aControlhWnd := {}
#endif
      IF ISBLOCK( _HMG_bOnErrorInit )
         cMsg := Eval( _HMG_bOnErrorInit, cMsg )
      ENDIF

      cMsg += iif( _lShowDetailError(), cErrorMessage, ErrorMessage( oError ) )

      IF ISLOGICAL( _HMG_lOnErrorStop ) .AND. _HMG_lOnErrorStop == .F.

         MsgStop( StrTran( cMsg, ";", CRLF ), 'Program Error', NIL, .F. )

      ELSE

         ErrorWindows(cMsg)

      ENDIF

      ErrorLevel( 1 )

      IF ISBLOCK( _HMG_bOnErrorExit )
         Eval( _HMG_bOnErrorExit )
      ENDIF

      myExitError(cMsg)  // вызов своей функции при ошибке завершения программы
                             // call your function on program termination error
      //ReleaseAllWindows()

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE ErrorLog( nHandle, oErr )
*-----------------------------------------------------------------------------*
   STATIC _lAddError := .T.
#ifdef __XHARBOUR__
   LOCAL nCount
#else
   LOCAL nScope, nCount, tmp, cName, xValue
#endif

   IF _lAddError

      _lAddError := .F.

      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " System Information ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )

      Html_LineText( nHandle, "Workstation name...: " + NetName() )
      Html_LineText( nHandle, "Active user name...: " + GetUserName() )
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
      Html_LineText( nHandle, "C/C++ compiler.....: " + hb_CCompiler() )
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
               ( nCount )->( Html_LineText( nHandle, "" ) )
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

      Html_LineText( nHandle, "Subsystem Call ....: " + oErr:subsystem() )
      Html_LineText( nHandle, "System Code .......: " + strvalue( oErr:subcode() ) )
      Html_LineText( nHandle, "Default Status ....: " + strvalue( oErr:candefault() ) )
      Html_LineText( nHandle, "Description .......: " + oErr:description() )
      Html_LineText( nHandle, "Operation .........: " + oErr:operation() )
      Html_LineText( nHandle, "Involved File .....: " + oErr:filename() )
      Html_LineText( nHandle, "Dos Error Code ....: " + strvalue( oErr:oscode() ) )

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
               Html_LineText( nHandle, "      " + cName + " TYPE " + ValType( xValue ) + " [" + hb_CStr( xValue ) + "]" )
            ENDIF

         NEXT

      NEXT

      IF nCount > 0
         Html_LineText( nHandle, "" )
      ENDIF

      HTML_RawText( nHandle, "</details>" )

      /* NOTE: 29.07.2023 VAG */
      HTML_RawText( nHandle, "<details><summary>" )
      HTML_RawText( nHandle, PadC( " List of open windows ", 79, "-" ) )
      HTML_RawText( nHandle, "<br/></summary>" )
      HTML_RawText( nHandle, ListGetForms() )
      HTML_RawText( nHandle, "</details>" )

#endif
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION strvalue( c, l )
*-----------------------------------------------------------------------------*

   SWITCH ValType( c )
   CASE "C"
   CASE "M" ; RETURN c
   CASE "N" ; RETURN hb_ntos( c )
   CASE "D" ; RETURN DToC( c )
   CASE "L" ; RETURN iif( hb_defaultValue( l, .F. ), iif( c, "ON", "OFF" ), iif( c, ".T.", ".F." ) )
   ENDSWITCH

RETURN ""

/* Date Created: 14/11/2005
   Author: Antonio Novo <antonionovo@gmail.com>
   Enable/Disable Error Detail */
*-----------------------------------------------------------------------------*
FUNCTION _lShowDetailError( lNewValue )
*-----------------------------------------------------------------------------*
   STATIC _lShowDetailError := .T.

   LOCAL lOldValue := _lShowDetailError

   IF ISLOGICAL( lNewValue )
      _lShowDetailError := lNewValue
   ENDIF

RETURN lOldValue

*-01-01-2003
*-Author: Antonio Novo
*-Create/Open the ErrorLog.Htm file
*-----------------------------------------------------------------------------*
FUNCTION HTML_ERRORLOG
*-----------------------------------------------------------------------------*
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

*-30-12-2002
*-Author: Antonio Novo
*-HTML Page Head
*-----------------------------------------------------------------------------*
FUNCTION HTML_INI( ARCH, TITLE )
*-----------------------------------------------------------------------------*
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

*-----------------------------------------------------------------------------*
PROCEDURE HTML_RAWTEXT( HTMARCH, LINEA )
*-----------------------------------------------------------------------------*
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + CRLF )
#endif
   ENDIF

RETURN

*-30-12-2002
*-Author: Antonio Novo
*-HTM Page Line
*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINETEXT( HTMARCH, LINEA )
*-----------------------------------------------------------------------------*
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, RTrim( LINEA ) + "<BR>" + CRLF )
#endif
   ENDIF

RETURN

*-30-12-2002
*-Author: Antonio Novo
*-HTM Line
*-----------------------------------------------------------------------------*
PROCEDURE HTML_LINE( HTMARCH )
*-----------------------------------------------------------------------------*
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   IF HTMARCH > 0 .AND. IsErrorLogActive()
      FWrite( HTMARCH, "<HR>" + Chr( 13 ) + Chr( 10 ) )
#else
   IF HTMARCH != NIL .AND. IsErrorLogActive()
      hb_vfWrite( HTMARCH, "<HR>" + CRLF )
#endif
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE HTML_END( HTMARCH )
*-----------------------------------------------------------------------------*
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

// (JK) HMG 1.0 Build 6
*-----------------------------------------------------------------------------*
PROCEDURE _SetErrorLogFile( cFile )
*-----------------------------------------------------------------------------*
   _HMG_ErrorLogFile := IFEMPTY( cFile, GetStartUpFolder() + "\ErrorLog.htm", cFile )

RETURN

// Functions to insert HTML into error code - 24.05.21 Artyom Verchenko
*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_INSERT_OFFSET()
*-----------------------------------------------------------------------------*

RETURN ( -1 * Len( "</BODY></HTML>" ) )

*-----------------------------------------------------------------------------*
STATIC FUNCTION __HTML_BODY_TEMPLATE()
*-----------------------------------------------------------------------------*

RETURN hb_base64Decode("PCFET0NUWVBFIGh0bWw+PGh0bWw+PGhlYWQ+PG1ldGEgY2hhcnNldD0id2luZG93cy0xMjUxIj48dGl0bGU+e3tUSVRMRX19PC90aXRsZT48c3R5bGU+Ym9keXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2JhY2tncm91bmQtY29sb3I6I2ZmZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO3BhZGRpbmc6MTVweH0uc3VtbWFyeSxkZXRhaWxzIHN1bW1hcnl7Y29sb3I6IzA2OTtiYWNrZ3JvdW5kOiNmZmM7Ym9yZGVyOjFweCBzb2xpZCAjOWFmO3BhZGRpbmc6NXB4O21hcmdpbjoxMHB4IDVweDtjdXJzb3I6cG9pbnRlcn0ubGlua3tkaXNwbGF5OmJsb2NrO2JhY2tncm91bmQ6I2NmYzt0ZXh0LWRlY29yYXRpb246bm9uZX1oMXtmb250LWZhbWlseTpzYW5zLXNlcmlmO2ZvbnQtc2l6ZToxNTAlO2NvbG9yOiMwMGM7Zm9udC13ZWlnaHQ6NzAwO2JhY2tncm91bmQtY29sb3I6I2YwZjBmMH0udXBkYXRlZHtmb250LWZhbWlseTpzYW5zLXNlcmlmO2NvbG9yOiNjMDA7Zm9udC1zaXplOjExMCV9Lm5vcm1hbHRleHR7Zm9udC1mYW1pbHk6c2Fucy1zZXJpZjtmb250LXNpemU6MTAwJTtjb2xvcjojMDAwO2ZvbnQtd2VpZ2h0OjQwMDt0ZXh0LXRyYW5zZm9ybTpub25lO3RleHQtZGVjb3JhdGlvbjpub25lfS5sYXJnZS1zZWxlY3R7Zm9udC1zaXplOjEyNSU7cGFkZGluZzo4cHg7bWFyZ2luOjVweDtiYWNrZ3JvdW5kOiNjZGZ9PC9zdHlsZT48c2NyaXB0PmNvbnN0IGZpbHRlckJ5PShyLGUsbCxuPW51bGwpPT57bGV0IHQ9ci5tYXAobCk7dD10LnJlZHVjZSgoZSx0KT0+KHQgaW4gZXx8KGVbdF09MCksZVt0XSsrLGUpLHt9KSx0PU9iamVjdC5lbnRyaWVzKHQpLnJlZHVjZSgoZSxbdCxyXSk9PihlW3RdPVt0LHIsYFske3J9XSAke3R9YF0sZSkse30pO2NvbnN0IGM9ZG9jdW1lbnQucXVlcnlTZWxlY3RvcihlKTtPYmplY3QudmFsdWVzKHQpLnNvcnQoKGUsdCk9PnRbMV0tZVsxXSkuZm9yRWFjaCgoW2UsLHRdKT0+e2NvbnN0IHI9ZG9jdW1lbnQuY3JlYXRlRWxlbWVudCgib3B0aW9uIik7ci52YWx1ZT1lLHIuaW5uZXJUZXh0PXQsYy5hcHBlbmRDaGlsZChyKX0pO2MuYWRkRXZlbnRMaXN0ZW5lcigiY2hhbmdlIixlPT57biYmbihjKTtjb25zdCB0PWUudGFyZ2V0LnZhbHVlO3IuZm9yRWFjaChlPT4oKGUsdCk9Pnt2YXIgcjsibnVsbCIhPT10PyhyPWwoZSksZS5zdHlsZS5kaXNwbGF5PXI9PT10P251bGw6Im5vbmUiLGNvbnNvbGUubG9nKGUsZS5zdHlsZS5kaXNwbGF5KSk6ZS5zdHlsZS5kaXNwbGF5PW51bGx9KShlLHQpKX0pfTtkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKCJET01Db250ZW50TG9hZGVkIixmdW5jdGlvbihlKXt2YXIgdD1bLi4uZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbCgiLnJlY29yZCIpXSxyPXQ9PmRvY3VtZW50LnF1ZXJ5U2VsZWN0b3IoIiNleHRyYS1wYW5lbCIpLnF1ZXJ5U2VsZWN0b3JBbGwoInNlbGVjdCIpLmZvckVhY2goZT0+dCE9PWUmJihlLnNlbGVjdGVkSW5kZXg9MCkpO2ZpbHRlckJ5KHQsIiNmaWx0ZXJCeURhdGUiLGU9PmUucXVlcnlTZWxlY3RvcigiLmRhdGUiKS5pbm5lclRleHQsciksZmlsdGVyQnkodCwiI2ZpbHRlckJ5U3RhY2t0cmFjZSIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuc3RhY2t0cmFjZSIpPy5jaGlsZE5vZGVzWzBdPy5kYXRhLnRyaW0oKSxyKSxmaWx0ZXJCeSh0LCIjZmlsdGVyQnlFcnJvciIsZT0+ZS5xdWVyeVNlbGVjdG9yKCIuZXJyb3IiKS5pbm5lclRleHQucmVwbGFjZSgvKFxyXG58XG58XHIpL2dtLCIiKS5yZXBsYWNlKC9cc3syLH0vZywiICIpLnRyaW0oKSxyKSxkb2N1bWVudC5xdWVyeVNlbGVjdG9yKCIjZXh0cmEtcGFuZWwiKS5zdHlsZS5kaXNwbGF5PW51bGwsY29uc29sZS5sb2coIkRPTUNvbnRlbnRMb2FkZWQiKX0pPC9zY3JpcHQ+PC9oZWFkPjxib2R5PjxoMSBzdHlsZT0idGV4dC1hbGlnbjpjZW50ZXIiPnt7VElUTEV9fTwvaDE+PGRpdiBpZD0iZXh0cmEtcGFuZWwiIHN0eWxlPSJkaXNwbGF5Om5vbmUiPjxzZWxlY3QgYXV0b2NvbXBsZXRlPSJvZmYiIGlkPSJmaWx0ZXJCeURhdGUiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRGF0ZTwvb3B0aW9uPjxvcHRpb24gdmFsdWU9Im51bGwiPkFsbCBkYXRlczwvb3B0aW9uPjwvc2VsZWN0PiA8c2VsZWN0IGF1dG9jb21wbGV0ZT0ib2ZmIiBpZD0iZmlsdGVyQnlTdGFja3RyYWNlIiBjbGFzcz0ibGFyZ2Utc2VsZWN0Ij48b3B0aW9uIHNlbGVjdGVkPSJzZWxlY3RlZCIgZGlzYWJsZWQ9ImRpc2FibGVkIiB2YWx1ZT0ibnVsbCI+RmlsdGVyIGJ5IFN0YWNrVHJhY2U8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgc3RhY2t0cmFjZXM8L29wdGlvbj48L3NlbGVjdD4gPHNlbGVjdCBhdXRvY29tcGxldGU9Im9mZiIgaWQ9ImZpbHRlckJ5RXJyb3IiIGNsYXNzPSJsYXJnZS1zZWxlY3QiPjxvcHRpb24gc2VsZWN0ZWQ9InNlbGVjdGVkIiBkaXNhYmxlZD0iZGlzYWJsZWQiIHZhbHVlPSJudWxsIj5GaWx0ZXIgYnkgRXJyb3I8L29wdGlvbj48b3B0aW9uIHZhbHVlPSJudWxsIj5BbGwgZXJyb3JzPC9vcHRpb24+PC9zZWxlY3Q+PC9kaXY+PC9ib2R5PjwvaHRtbD4=")

*-------------------------- List of open windows -----------------------------*
FUNCTION ListGetForms
*-----------------------------------------------------------------------------*
   LOCAL nI, cForm, aFrm, cMsg := ""

   aFrm := HMG_GetForms()
   cMsg += "Number of open windows: " + HB_NtoS(LEN(aFrm)) + CRLF

   FOR nI := 1 TO LEN(aFrm)
      cForm := UPPER(aFrm[nI])
      cMsg += HB_NtoS(nI) + ") "
      cMsg += ' Form: ' + cForm + ', Type: "'+_HMG_aFormType[nI]+'" '
      cMsg += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cMsg += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cMsg += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cMsg += ', Title: ' + GetProperty( cForm, "Title" ) + CRLF
   NEXT
   cMsg := STRTRAN(cMsg, CRLF, "<br/>")

RETURN cMsg

//////////////////////////////////////////////////////////////////////////////
// Function to record the screen of a form - 24.05.21 Andrey Verchenko
*-----------------------------------------------------------------------------*
FUNCTION _SaveScreenError( oError )
*-----------------------------------------------------------------------------*
   LOCAL lSave, nWin, cPathScr, cFile, cMaska, cFileBmp, cForm, cMsg

   lSave := .T. //  SET ERRORSCREEN TO WINDOWSFORM/SCREENDESKTOP

   IF !lSave  // SET ERRORSCREEN TO WINDOWSFORM/SCREENDESKTOP
      cMaska := "ErrorWin"    // текущее окно программы
   ELSE
      cMaska := "ErrorScr"    // весь экран рабочего стола
   ENDIF

   cPathScr := cFilePath( _HMG_ErrorLogFile ) + "\"
   cFile    := cPathScr + cMaska + "" + DTOS( DATE() )
   cFile    += "-" + CharRepl( ":", TIME(), "-" )
   cFile    += ".png"
   cFileBmp := ChangeFileExt( cFile , ".bmp" )

   IF !lSave

      cForm := _HMG_ThisFormName

      IF !Empty(cForm) .and. _IsWindowDefined(cForm)

         // Запись текущего окна программы  / Save the current program window
         nWin := Ascan( _HMG_aFormHandles, iif( _HMG_BeginWindowMDIActive, GetActiveMdiHandle(), GetActiveWindow() ) )
         IF nWin > 0
            oError:cargo := _HMG_aFormNames[ nWin ]
            DoMethod( oError:cargo, "SaveAs", cFileBmp )
         ELSE
            SAVEWINDOWBYHANDLE( GetFormHandle( cForm ), cFileBmp, -1, -1, -1, -1 )
         ENDIF

         HMG_SaveImage( cFileBmp, cFile, "PNG" )  // Save to PNG

         FErase ( cFileBmp )

      ELSE

         cMsg  := "Не смог определить ОКНО с ошибкой ! cForm= [" + cValtoChar(cForm) + "]"
         cFile := ChangeFileExt( cFile, '.log' )
         HB_MemoWrit( "tsb_Dog_card.txt", cMsg )

      ENDIF

   ELSE

      // Запись всего экрана рабочего стола / Save entire desktop screen
      BT_BitmapSaveFile(BT_BitmapCaptureDesktop(), cFile, 4)

   ENDIF

   cFile := cFileNoPath( cFile )

RETURN cFile

////////////////////////////////////////////////////////////////////////////
FUNCTION ErrorWindows(cErr)
   LOCAL cFont, nFontSize, nI, nY, nX, nW, nH, nG, cN, nHMemo, cTitle
   LOCAL aBtn, nBtnH, nBtnW, aBackColor, aMsg[3], aBtnBClr, cMsg, lCrtHandle
   DEFAULT cErr := "No error line !"

   nY := nX := nG := 20 ; nW := 850 ; nH := 600 ; nBtnH := 75
   aBackColor := MAROON
   aBtnBClr   := { { 35,179, 15}, ORANGE, {217,67,67} }
   cFont      := "Verdana"
   nFontSize  := 18

   //IF Hb_LangSelect() == "ru.RU1251"
   IF M->nProgLang == 1
      aBtn    := { '&Отправить'+CRLF+'ошибки на сайт', '&Просмотр'+CRLF+'ошибки', '&Выход' }
      aMsg[1] := "Произошла ошибка в программе"
      aMsg[2] := "НЕОБХОДИМО СВЯЗАТЬСЯ С ПРОГРАММИСТОМ"
      aMsg[3] := "Попробуйте перезапустить программу !"
      cTitle  := "Ошибка в программе"
   ELSEIF M->nProgLang == 3               // Hb_LangSelect() == "UA1125" // украинский язык
       aBtn    := { '&Надіслати'+CRLF+'помилки на сайт', '&Перегляд'+CRLF+'помилки', '&Вихід' }
       aMsg[1] := "Сталася помилка в програмі"
       aMsg[2] := "НЕОБХІДНО КОНТАКТИ З програмістів"
       aMsg[3] := "Спробуйте перезапустити програму!"
       cTitle  := "Помилка в програмі"
   ELSE
      aBtn    := { '&Send'+CRLF+'errors to site', '&View'+CRLF+'errors', '&Exit' }
      aMsg[1] := "A Run-Time Error Has Occured"
      aMsg[2] := "IT IS NECESSARY TO CONTACT THE PROGRAMMER"
      aMsg[3] := "Try restarting the program!"
      cTitle  := "Error in the program"
   ENDIF

   IF !Empty( _HMG_MainHandle )  // если нет MAIN окна
     SET WINDOW MAIN OFF
   ENDIF

   DEFINE WINDOW Form_Err AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE cTitle ICON "1MAIN_ICO"                    ;
      MODAL NOSIZE                                     ;
      FONT cFont SIZE nFontSize                        ;
      BACKCOLOR aBackColor                             ;
      ON INIT {|| This.Topmost := .T. , DoEvents() }

      // похоже окно DEFINE WINDOW Form_Err не создалось
      // Т.е. нет handle окна, причина, наверно память разрушена или еще что, но нет
      // создания, команда в DEFINE WINDOW ...
      // Formhandle := InitWindow( Caption, x, y, w, h, nominimize, nomaximize, nosize, nosysmenu, nocaption, topmost, ClassName, ParentHandle, vscroll, hscroll, helpbutton, palette, panel )
      // ^^^^^^^^^^ => Nil
      // и нет работы всех др. операторов, возможно, надо проверять
      lCrtHandle := .F.
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         nW := This.ClientWidth                    // проверка
         lCrtHandle := .T.
      END SEQUENCE

      IF lCrtHandle
         nW := This.ClientWidth
         nH := This.ClientHeight
      ELSE
         cMsg := "Ошибка создания окна DEFINE WINDOW Form_Err !" + CRLF
         cMsg += "Нехватка или ошибка ПАМЯТИ компьютера !" + CRLF
         cMsg += ProcNL() + CRLF + ProcNL(1) + CRLF + ProcNL(2)
         MsgStop(cMsg, "ERROR")         // именно эта ф-я !!!  убрал AlertStop - 15.08.23
         nW := 850
         nH := 600
      ENDIF

      nY += nG/2

      DRAW ICON IN WINDOW Form_Err AT nY, nX PICTURE "ZZZ_B_STOP64" ;
            WIDTH 64 HEIGHT 64 COLOR aBackColor

      nX += 64 + nG
      FOR nI := 1 TO Len(aMsg)

         cN := 'Lbl_' + StrZero(nI, 2)
         @ nY, nX LABEL &cN PARENT Form_Err WIDTH nW - nX - nG HEIGHT 36 ;
           VALUE aMsg[nI] FONTCOLOR YELLOW TRANSPARENT CENTERALIGN VCENTERALIGN

         nY += nFontSize + nG
      NEXT
      nY += nG

      cMsg := CHARREM( '[]', M->cPubVersProg )
      cMsg := ALLTRIM( cMsg )
      @ nY - nFontSize, nG LABEL Label_Ver PARENT Form_Err WIDTH 200 HEIGHT 20 ;
         VALUE cMsg SIZE 10 FONTCOLOR WHITE TRANSPARENT

      nHMemo  := nH - nY - ( nBtnH + nG * 2 )
      @ nY, nG EDITBOX Edit_Memo PARENT Form_Err WIDTH nW - nG*2 HEIGHT nHMemo ;
        VALUE cErr  READONLY NOHSCROLL SIZE nFontSize - 6 ;
        BACKCOLOR aBackColor FONTCOLOR WHITE

      nY    := nH - nBtnH - nG
      nBtnW := (nW - nG*4) / 3
      nX    := nG
      @ nY, nX BUTTONEX Btn_Inet PARENT Form_Err          ;
        WIDTH nBtnW HEIGHT nBtnH CAPTION aBtn[1] ICON Nil ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR                   ;
        FONTCOLOR WHITE  BACKCOLOR aBtnBClr[1]            ;
        ON MOUSEHOVER ( This.Backcolor := BLACK      , This.Fontcolor := YELLOW ) ;
        ON MOUSELEAVE ( This.Backcolor := aBtnBClr[1], This.Fontcolor := WHITE  ) ;
        ACTION {|| SubmittingErrorToSite(cTitle,cErr) , Form_Err.Release }
      nX += nBtnW + nG

      @ nY, nX BUTTONEX Btn_Log PARENT Form_Err           ;
        WIDTH nBtnW HEIGHT nBtnH CAPTION aBtn[2] ICON Nil ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR                   ;
        FONTCOLOR WHITE  BACKCOLOR aBtnBClr[2]            ;
        ON MOUSEHOVER ( This.Backcolor := BLACK      , This.Fontcolor := YELLOW ) ;
        ON MOUSELEAVE ( This.Backcolor := aBtnBClr[2], This.Fontcolor := WHITE  ) ;
        ACTION {|| ShellExecute( , 'open', _HMG_ErrorLogFile,,, SW_SHOWNORMAL) ,;
                   Form_Err.Release }
      nX += nBtnW + nG

      @ nY, nX BUTTONEX Btn_Exit PARENT Form_Err          ;
        WIDTH nBtnW HEIGHT nBtnH CAPTION aBtn[3] ICON Nil ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR                   ;
        FONTCOLOR WHITE  BACKCOLOR aBtnBClr[3]            ;
        ON MOUSEHOVER ( This.Backcolor := BLACK      , This.Fontcolor := YELLOW ) ;
        ON MOUSELEAVE ( This.Backcolor := aBtnBClr[3], This.Fontcolor := WHITE  ) ;
        ACTION {|| Form_Err.Release }

   END WINDOW

   CENTER   WINDOW Form_Err
   ACTIVATE WINDOW Form_Err ON INIT {|| This.Minimize, DoEvents(), This.Restore }

RETURN NIL

////////////////////////////////////////////////////////////////////////////
FUNCTION SubmittingErrorToSite(cTitle,cErr)
   LOCAL cMsg, cTime := HB_TSTOSTR( HB_DATETIME() )
   LOCAL cParam, cExeRun, cFileTxt

   cMsg := "Здесь можно вызывать отдельную программу;отправки ошибок на сайт разработчика;"
   cMsg += "Here you can call a separate program; send errors to the developer's site;;"
   cMsg += cTitle + " !  "  + cTime + ";;"
   cMsg += cErr
   // здесь будет отдельная программа, которая делает архив папки ErrorsLog
   // и отправляет этот архив на сайт по заданному адресу
   // there will be a separate program that makes an archive of the ErrorsLog folder
   // and sends this archive to the site at the given address
   cExeRun := App.Cargo:cExeSite    // SmallAlert.exe
   IF FILE(cExeRun)
      cParam  := '-info "' + cMsg + '"'
      ShellExecute( , 'open', cExeRun, cParam, , SW_SHOWNORMAL)
   ELSE
      cFileTxt := ChangeFileExt( ExeName(), '-err.txt' )
      cMsg := AtRepl( ";", cMsg, CRLF )
      HB_MemoWrit( cFileTxt , cMsg + CRLF)
      wApi_Sleep(100)
      ShellExecute(0,"Open",cFileTxt,,,SW_SHOWNORMAL)
   ENDIF
   wApi_Sleep(100)

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
Function myExitError(cMsg)   // вызов моей функции при ошибке завершения программы

   ? "."
   ? REPL("=",20) + "> ОШИБКА при завершении программы ! myExitError() <====="
   ? cMsg
   ? REPL("=",20) + "> ERROR at the end of the program ! myExitError() <====="

   // закрыть/close LetoDB
   // закрыть/close PostgreSql
   DbCloseAll()    // закрыть все базы / close all bases

   ? CRLF + ">>> STOP <<<  " + HMG_TimeMS( App.Cargo:tStart )

   // Записываем события программы в журнал событий
   // Write program events to the event log
   cMsg := HB_TSTOSTR( HB_DATETIME() ) + " | "
   cMsg += PADR("* Error ! program closure " + cFileNoPath(App.ExeName),50)
   cMsg += " | " + ProcNL() + CRLF
   STRFILE( cMsg, App.Cargo:cLogEvents, .T. )

   ReleaseAllWindows()

RETURN Nil

