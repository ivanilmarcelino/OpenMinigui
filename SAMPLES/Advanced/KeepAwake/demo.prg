/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2022 Grigory Filatov <gfilatov@gmail.com>
 *
*/

ANNOUNCE RDDSYS

#include "minigui.ch"

#define APP_TITLE "Keep Your Computer Awake"
#define IDI_MAIN 1001

FUNCTION Main()

   LOCAL nTokens, i, cLabel, cText
   LOCAL cMsg := "The computer can not" + CRLF + "go to sleep while this" + CRLF + "window is open."

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 296, 174 ;
         TITLE APP_TITLE ;
         ICON IDI_MAIN ;
         MAIN ;
         ON MINIMIZE Minimize_Click() ;
         NOTIFYTOOLTIP APP_TITLE ;
         ON NOTIFYCLICK Notify_Click() ;
         ON INIT SetKeepAwake( .T. ) ;
         ON RELEASE SetKeepAwake()

      nTokens := NumToken( cMsg, CRLF )

      FOR i := 1 TO nTokens
         cLabel := "Label_" + hb_ntos( i )
         cText := Token( cMsg, CRLF, i )
         @ 50 + ( i - 1 ) * 20, 20 LABEL ( cLabel ) ;
            VALUE cText ;
            WIDTH 260 ;
            FONT "Arial" SIZE 12 BOLD ;
            FONTCOLOR RED CENTERALIGN
      NEXT

   END WINDOW

   ACTIVATE WINDOW Form_1 ;
      ON INIT ( _HMG_aFormNotifyIconName[ GetFormIndex( "Form_1" ) ] := IDI_MAIN, This.Center() )

RETURN NIL


PROCEDURE Notify_Click

   LOCAL FormHandle := GetFormHandle( "Form_1" )

   Form_1.Restore()

   SetForegroundWindow( FormHandle )

   ShowNotifyIcon( FormHandle, .F., NIL, NIL )

RETURN


PROCEDURE Minimize_Click

   LOCAL i := GetFormIndex( "Form_1" )

   IF _HMG_aFormMiscData1[ i ][ 1 ] == NIL
      _HMG_aFormMiscData1[ i ][ 1 ] := LoadTrayIcon( GetInstance(), _HMG_aFormNotifyIconName[ i ] )
   ENDIF

   ShowNotifyIcon( _HMG_aFormhandles[ i ], .T., ;
      _HMG_aFormMiscData1[ i ][ 1 ], _HMG_aFormNotifyIconToolTip[ i ] )

   Form_1.Hide()

RETURN


FUNCTION SetKeepAwake( lOnOff )

   DEFAULT lOnOff TO .F.

   IF ! SetThreadExecutionState( lOnOff )
      AlertStop( "Call to SetThreadExecutionState failed unexpectedly." )
   ENDIF

RETURN NIL


#pragma BEGINDUMP

#include "hbapi.h"
#include "windows.h"
#include <winbase.h>

HB_FUNC( SETTHREADEXECUTIONSTATE )
{
   HB_UINT result;

   if( hb_parl( 1 ) )
      result = SetThreadExecutionState( ES_CONTINUOUS | ES_SYSTEM_REQUIRED | ES_DISPLAY_REQUIRED );
   else
      result = SetThreadExecutionState( ES_CONTINUOUS );

   if ( result == NULL )
   {
      // Function failed...
      hb_retl( HB_FALSE );
   }

   hb_retl( HB_TRUE );
}

#pragma ENDDUMP
