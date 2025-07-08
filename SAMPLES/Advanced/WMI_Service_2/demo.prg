/*
 * Proyecto: Serivios
 * Descripción: Muestra información relativa a los servicios de Windows
 * Autor: Rafa Carmona
 * Fecha: 07/22/08
 *
 * Adapted by Grigory Filatov <gfilatov@inbox.ru>
 *
 * Revised by Evangelos Tsakalidis <tsakal@otenet.gr>
 *
 * 20/10/2019 Revised by Pierpaolo Martinello <assistenza[at]pcmodula.it>
 */

#define _NO_BTN_PICTURE_

#include "SET_COMPILE_HMG_UNICODE.ch"

#include "hmg.ch"

*-----------------------------
#define _MYTITLE_ ".:: System information about services ::."
*-----------------------------

STATIC oWS

*-----------------------------
PROCEDURE MAIN
*-----------------------------

   SET MULTIPLE OFF
   SET AUTOADJUST ON

#ifndef UNICODE
   SET CODEPAGE TO UNICODE

#endif

   oWS := CreateObject( "WScript.Shell" )

   LOAD WINDOW Demo AS Form_1

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN

*-----------------------------
FUNCTION AddText( t )
*-----------------------------
   LOCAL a := Form_1.RichEdit_1.VALUE

   a += t + CRLF
   Form_1.RichEdit_1.VALUE := a

RETURN NIL

*-----------------------------
FUNCTION UnicodeToAnsi( cText )
*-----------------------------

#ifndef UNICODE
   cText := HMG_UNICODE_TO_ANSI( cText )

#endif

Return( cText )

*-----------------------------
STATIC FUNCTION xToString( xValue )
*-----------------------------
   LOCAL cType := ValType( xValue )
   LOCAL cValue := ""

   DO CASE
   CASE cType $ "CM" ;  cValue := AllTrim( xValue )
   CASE cType == "N" ;  cValue := hb_ntos( xValue )
   CASE cType == "D" ;  cValue := DToC( xValue )
   CASE cType == "T" ;  cValue := hb_TSToStr( xValue, .T. )
   CASE cType == "L" ;  cValue := iif( xValue, "True", "False" )
   CASE cType == "A" ;  cValue := AToC( xValue )
   CASE cType $ "UE" ;  cValue := "NIL"
   CASE cType == "B" ;  cValue := "{|| ... }"
   CASE cType == "O" ;  cValue := "{" + xValue:className + "}"
   ENDCASE

RETURN cValue

*-----------------------------
STATIC FUNCTION WMIService()
*-----------------------------
   STATIC oWMI

   LOCAL oLocator

   IF oWMI == NIL

      oLocator := CreateObject( "wbemScripting.SwbemLocator" )
      oWMI := oLocator:ConnectServer()

   ENDIF

RETURN oWMI

*-----------------------------
FUNCTION Btn1Click()
*-----------------------------
   LOCAL oWmi, oService
   LOCAL tBegin := hb_DateTime(), tEnd

   Form_1.RichEdit_1.VALUE := ""

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( "Select * From Win32_Service" )

      AddText( UnicodeToAnsi( oService:DisplayName ) + " : " + oService:State )

   NEXT
   tEnd := hb_DateTime()
   AddText( timeMsg( tBegin, tEnd ) )
   oWsSendKeys( "^{END}" )

RETURN NIL

*-----------------------------
FUNCTION Btn2Click()
*-----------------------------
   LOCAL oWmi, oService
   LOCAL tBegin := hb_DateTime(), tEnd

   Form_1.RichEdit_1.VALUE := ""

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( "Select * From Win32_Service" )

      AddText( "System Name : " + oService:SystemName )
      AddText( "Service Name : " + oService:Name )
      AddText( "Service Type : " + oService:ServiceType )
      AddText( "Service State : " + oService:State )
      AddText( "Code : " + xToString( oService:ExitCode ) )
      AddText( "Process ID : " + xToString( oService:ProcessID ) )
      AddText( "Can Be Paused : " + xToString( oService:AcceptPause ) )
      AddText( "Can Be Stopped : " + xToString( oService:AcceptStop ) )
      AddText( "Caption : " + UnicodeToAnsi( oService:Caption ) )
      AddText( "Description : " + UnicodeToAnsi( xToString( oService:Description ) ) )
      AddText( "Can Interact with Desktop : " + xToString( oService:DesktopInteract ) )
      AddText( "Display Name : " + UnicodeToAnsi( oService:DisplayName ) )
      AddText( "Error Control : " + oService:ErrorControl )
      AddText( "Executable Path Name : " + xToString( oService:PathName ) )
      AddText( "Service Started : " + xToString( oService:Started ) )
      AddText( "Start Mode : " + xToString( oService:StartMode ) )
      AddText( "Start Name : " + xToString( oService:StartName ) )
      AddText( Replicate( "-", 145 ) )

   NEXT
   tEnd := hb_DateTime()
   AddText( timeMsg( tBegin, tEnd ) )
   oWsSendKeys( "^{END}" )

RETURN NIL

*-----------------------------
FUNCTION Btn3Click()
*-----------------------------
   LOCAL oWmi, oService
   LOCAL tBegin := hb_DateTime(), tEnd

   Form_1.RichEdit_1.VALUE := ""

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( [ Select * FROM Win32_Service Where State <> 'Running' ] )

      AddText( UnicodeToAnsi( oService:DisplayName ) + " : " + oService:State )

   NEXT
   tEnd := hb_DateTime()
   AddText( timeMsg( tBegin, tEnd ) )
   oWsSendKeys( "^{END}" )

RETURN NIL

*-----------------------------
FUNCTION Btn4Click()
*-----------------------------
   LOCAL oWmi, oService
   LOCAL tBegin := hb_DateTime(), tEnd

   Form_1.RichEdit_1.VALUE := ""

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( [ Select * FROM Win32_Service Where PathName = 'C:\\WINDOWS\\system32\\services.exe' ] )

      AddText( UnicodeToAnsi( oService:DisplayName ) )

   NEXT
   tEnd := hb_DateTime()
   AddText( timeMsg( tBegin, tEnd ) )
   oWsSendKeys( "^{END}" )

RETURN NIL

// List Service Status Changes as recorded in the System Event Log
*-----------------------------
FUNCTION Btn5Click()
*-----------------------------
   LOCAL dtmConvertedDate := CreateObject( "WbemScripting.SWbemDateTime" )
   LOCAL objWMIService := CreateObject( "wbemScripting.SwbemLocator" )
   LOCAL strEvent
   LOCAL oSrv := objWMIService:ConnectServer()
   LOCAL tBegin := hb_DateTime(), tEnd

   Form_1.RichEdit_1.VALUE := ""

   FOR EACH strEvent IN oSrv:ExecQuery( [ Select * FROM Win32_NTLogEvent Where Logfile = 'System' and EventCode = '7036' ] )

      dtmConvertedDate:Value = strEvent:TimeWritten
      AddText( xToString( dtmConvertedDate:GetVarDate ) + Chr( 9 ) + UnicodeToAnsi( strEvent:Message ) )

   NEXT
   tEnd := hb_DateTime()
   AddText( timeMsg( tBegin, tEnd ) )
   oWsSendKeys( "^{END}" )

RETURN NIL

*-----------------------------
PROCEDURE oWsSendKeys( sKeys )
*-----------------------------
   IF oWS:AppActivate( _MYTITLE_ )
      Form_1.RichEdit_1.SetFocus()
      oWS:SendKeys( sKeys, .F. )
      // oWS:SendKeys("%+") // Alt+Shift for change keyboard languages
   ENDIF

RETURN

*-----------------------------
FUNCTION Btn_1Click()
*-----------------------------
   LOCAL oWmi, oService

   LOCAL tBegin := hb_DateTime(), tEnd
   LOCAL x := stringBuffer():New()
   Form_1.RichEdit_1.VALUE := Replicate( "Please Wait... Please Wait... Please Wait... Please Wait... Please Wait..." + CRLF, 34 )

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( "Select * From Win32_Service" )

      x:setStr( UnicodeToAnsi( oService:DisplayName ) + ' : ' + oService:State )

   NEXT
   tEnd := hb_DateTime()
   Form_1.RichEdit_1.VALUE := timeMsg( tBegin, tEnd ) + ;
      CRLF + ;
      x:getStr( CRLF )
   Form_1.RichEdit_1.SetFocus()

RETURN NIL

*-----------------------------
FUNCTION Btn_2Click()
*-----------------------------
   LOCAL oWmi, oService

   LOCAL tBegin := hb_DateTime(), tEnd
   LOCAL x := stringBuffer():New()
   Form_1.RichEdit_1.VALUE := Replicate( "Please Wait... Please Wait... Please Wait... Please Wait... Please Wait..." + CRLF, 34 )

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( "Select * From Win32_Service" )
      x:setStr( "System Name : " + oService:SystemName )
      x:setStr( "Service Name : " + oService:Name )
      x:setStr( "Service Type : " + oService:ServiceType )
      x:setStr( "Service State : " + oService:State )
      x:setStr( "Code : " + xToString( oService:ExitCode ) )
      x:setStr( "Process ID : " + xToString( oService:ProcessID ) )
      x:setStr( "Can Be Paused : " + xToString( oService:AcceptPause ) )
      x:setStr( "Can Be Stopped : " + xToString( oService:AcceptStop ) )
      x:setStr( "Caption : " + UnicodeToAnsi( oService:Caption ) )
      x:setStr( "Description : " + UnicodeToAnsi( xToString( oService:Description ) ) )
      x:setStr( "Can Interact with Desktop : " + xToString( oService:DesktopInteract ) )
      x:setStr( "Display Name : " + UnicodeToAnsi( oService:DisplayName ) )
      x:setStr( "Error Control : " + oService:ErrorControl )
      x:setStr( "Executable Path Name : " + xToString( oService:PathName ) )
      x:setStr( "Service Started : " + xToString( oService:Started ) )
      x:setStr( "Start Mode : " + oService:StartMode )
      x:setStr( "Start Name : " + xToString( oService:StartName ) )
      x:setStr( Replicate( "-", 145 ) )
   NEXT
   tEnd := hb_DateTime()
   Form_1.RichEdit_1.VALUE := timeMsg( tBegin, tEnd ) + ;
      CRLF + ;
      x:getStr( CRLF )
   Form_1.RichEdit_1.SetFocus()

RETURN NIL

*-----------------------------
FUNCTION Btn_3Click()
*-----------------------------
   LOCAL oWmi, oService

   LOCAL tBegin := hb_DateTime(), tEnd
   LOCAL x := stringBuffer():New()
   Form_1.RichEdit_1.VALUE := Replicate( "Please Wait... Please Wait... Please Wait... Please Wait... Please Wait..." + CRLF, 34 )

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( [ Select * FROM Win32_Service Where State <> 'Running' ] )

      x:setStr( UnicodeToAnsi( oService:DisplayName ) + ' : ' + oService:State )

   NEXT
   tEnd := hb_DateTime()
   Form_1.RichEdit_1.VALUE := timeMsg( tBegin, tEnd ) + ;
      CRLF + ;
      x:getStr( CRLF )
   Form_1.RichEdit_1.SetFocus()

RETURN NIL

*-----------------------------
FUNCTION Btn_4Click()
*-----------------------------
   LOCAL oWmi, oService

   LOCAL tBegin := hb_DateTime(), tEnd
   LOCAL x := stringBuffer():New()
   Form_1.RichEdit_1.VALUE := Replicate( "Please Wait... Please Wait... Please Wait... Please Wait... Please Wait..." + CRLF, 34 )

   oWmi := WmiService()

   FOR EACH oService IN oWmi:ExecQuery( [ Select * FROM Win32_Service Where PathName = 'C:\\WINDOWS\\system32\\services.exe' ] )

      x:setStr( UnicodeToAnsi( oService:DisplayName ) )

   NEXT
   tEnd := hb_DateTime()
   Form_1.RichEdit_1.VALUE := timeMsg( tBegin, tEnd ) + ;
      CRLF + ;
      x:getStr( CRLF )
   Form_1.RichEdit_1.SetFocus()

RETURN NIL

// List Service Status Changes as recorded in the System Event Log
*-----------------------------
FUNCTION Btn_5Click()
*-----------------------------
   LOCAL dtmConvertedDate := CreateObject( "WbemScripting.SWbemDateTime" )
   LOCAL objWMIService := CreateObject( "wbemScripting.SwbemLocator" )
   LOCAL strEvent
   LOCAL oSrv := objWMIService:ConnectServer()

   LOCAL tBegin := hb_DateTime(), tEnd
   LOCAL x := stringBuffer():New()
   Form_1.RichEdit_1.VALUE := Replicate( "Please Wait... Please Wait... Please Wait... Please Wait... Please Wait..." + CRLF, 34 )

   FOR EACH strEvent IN oSrv:ExecQuery( [ Select * FROM Win32_NTLogEvent Where Logfile = 'System' and EventCode = '7036' ] )

      dtmConvertedDate:Value = strEvent:TimeWritten
      x:setStr( xToString( dtmConvertedDate:GetVarDate ) + Chr( 9 ) + UnicodeToAnsi( strEvent:Message ) + CRLF )

   NEXT
   tEnd := hb_DateTime()
   Form_1.RichEdit_1.VALUE := timeMsg( tBegin, tEnd ) + ;
      CRLF + ;
      x:getStr( '' )
   Form_1.RichEdit_1.SetFocus()

RETURN NIL

*-----------------------------
FUNCTION timeMsg( tBegin, tEnd )
*-----------------------------
   LOCAL sRet := Replicate( "=", 81 ) + ;
      CRLF + ;
      'Begin : ' + xToString( tBegin ) + ;
      ' # End : ' + xToString( tEnd ) + ;
      ' # Process Time : ' + xToString( ( tEnd - tBegin ) * 86400 ) + ' Seconds' + ;
      CRLF + ;
      Replicate( "=", 81 )

return( sRet )
*-----------------------------
#include "sBufCLS.prg"
*-----------------------------

#pragma BEGINDUMP

#include "mgdefs.h"

HB_FUNC( GETWINDOWTEXT )
{

#ifdef UNICODE
   LPSTR  pStr;

#endif
   HWND   hWnd   = ( HWND ) HB_PARNL( 1 );
   int    iLen   = GetWindowTextLength( hWnd );
   LPTSTR szText = ( TCHAR * ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) );

   iLen = GetWindowText( hWnd, szText, iLen + 1 );

#ifndef UNICODE
   iLen = GetWindowText( hWnd, szText, iLen + 1 );

   hb_retclen( szText, iLen );

#else
   GetWindowText( hWnd, szText, iLen + 1 );

   pStr = hb_osStrU16Decode( szText );
   hb_retc( pStr );
   hb_xfree( pStr );

#endif
   hb_xfree( szText );
}

HB_FUNC( SETWINDOWTEXT )
{

#ifndef UNICODE
   LPCSTR lpString = ( LPCSTR ) hb_parc( 2 );

#else
   LPCWSTR lpString = hb_osStrU16Encode( hb_parc( 2 ) );

#endif
   SetWindowText( ( HWND ) HB_PARNL( 1 ), lpString );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpString );

#endif
}

#pragma ENDDUMP
