/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Передача сообщений между приложениями/процессами при помощи сообщения WM_COPYDATA
 * Passing messages between applications/processes using the WM_COPYDATA message
 *
*/

ANNOUNCE RDDSYS

#include "hmg.ch"
#define APP_TITLE   "Template of the finished program on MiniGui"
#define APP_ID      555
//////////////////////////////////////////////////////////////////
PROCEDURE MAIN
   LOCAL hWnd, cMsg, nI, cVal, aSay, cWWin, nTime, cSay, cStr

   SET WINDOW MAIN OFF

   SET MSGALERT FONTCOLOR TO BLACK
   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }   // светло-жёлтый
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 16

   hWnd := FindWindowEx( ,,, APP_TITLE )
   IF hWnd == 0
      cMsg := "Didn't find the program !;[" + APP_TITLE + "]; Exit..."
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      QUIT
   ENDIF

   nTime := Seconds()
   cMsg  := 'Sending messages to the program: "' + APP_TITLE + '"'
   aSay  := { '1/5' + SPACE(30) + '00:00:00', cMsg, "From: " + cFileNoPath(GetExeFileName()) }
   cWWin := WaitWindow( aSay, .T., 980, 18, NIL, BLACK, { 238, 249, 142 } )

   DoMethod( cWWin, "Minimize" )         ; DO EVENTS
   DoMethod( cWWin, "Restore"  )         ; DO EVENTS
   SetProperty( cWWin, "Topmost", .T. )  ; DO EVENTS
   //SetProperty( cWWin, "Message", "Height", 32 )
   cMsg += ";;"

   FOR nI := 1 TO 5
      cVal := cFileNoPath( GetExeFileName() ) + " | "
      cVal += HB_TSTOSTR( HB_DATETIME() )
      cVal += " - Message sent: " + HB_NtoS(nI)

      // Transfer data to window -> APP_TITLE
      SendMessageData( hWnd, cVal, APP_ID )
      cMsg += cVal + ";"

      cVal := HB_NtoS( nI ) + "/5" + SPACE(30) + SECTOTIME( Seconds() - nTime )
      //cVal := aSay[1] + CRLF + aSay[2] + CRLF + cSay
      SetProperty( cWWin, "Message", "Value", cVal )
      wApi_Sleep(100)
      DO EVENTS
      wApi_Sleep(1200)
   NEXT
   wApi_Sleep(1200)

   cVal := "Notepad.exe | "
   cVal += HB_TSTOSTR( HB_DATETIME() )
   cVal += " - Message sent: Open log !"
   cStr := SUBSTR(cVal, 1 , 12)
   SetProperty( cWWin, "Message", "Value", cStr )
   // Transfer data to window -> APP_TITLE
   SendMessageData( hWnd, cVal, APP_ID )
   cMsg += ";" + cVal + ";"

   wApi_Sleep(1200)

   cVal := "Demo_timer.exe | "
   cVal += HB_TSTOSTR( HB_DATETIME() )
   cVal += " - Message sent: RUN->Button_Sample3 !"
   cStr := SUBSTR(cVal, 1 , 12)
   SetProperty( cWWin, "Message", "Value", cStr )
   // Transfer data to window -> APP_TITLE
   SendMessageData( hWnd, cVal, APP_ID )
   cMsg += ";" + cVal + ";"

   WaitWindow()

   AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )

RETURN

///////////////////////////////////////////////////////////////////
#pragma BEGINDUMP

#include <mgdefs.h>
#include <hbapi.h>
#include <shlobj.h>
#include <time.h>
#include <windows.h>
#include "hbwinuni.h"

#ifndef __XHARBOUR__
   #define ISBYREF( n )          HB_ISBYREF( n )
#endif

HB_FUNC( SENDMESSAGEDATA )
{
   HWND hwnd = ( HWND ) HB_PARNL( 1 );

   if( IsWindow( hwnd ) )
   {
      COPYDATASTRUCT cds;

      cds.dwData = ( ULONG_PTR ) hb_parni( 3 ) ;
      cds.cbData = hb_parclen( 2 );
      cds.lpData = ( char * ) hb_parc( 2 );

      SendMessage( hwnd, WM_COPYDATA, 0, ( LPARAM ) &cds );
   }
}

HB_FUNC( GETMESSAGEDATA )
{
   PCOPYDATASTRUCT pcds = ( PCOPYDATASTRUCT ) HB_PARNL( 1 );

   hb_retc_null();

   if( pcds )
   {
      if( pcds->lpData )
      {
         hb_retclen(  pcds->lpData, pcds->cbData );
      }

      if( HB_ISBYREF( 2 ) )
      {
         hb_stornl( pcds->dwData, 2 );
      }
   }
}

