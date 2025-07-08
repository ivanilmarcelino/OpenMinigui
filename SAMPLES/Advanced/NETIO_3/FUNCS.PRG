/*
 * FUNCTION BrowseText( cTXT, cTitle, lRead )
 * Reads a text file (*.txt or *.rtf ) and browse
 * (c) Pete D. 14/04/2010 <pete_westg@yahoo.gr>
 * GPL
*/

#include <minigui.ch>

******************************************
FUNCTION BrowseText( cTXT, cTitle, lRead )
******************************************
   LOCAL cBuffer, cErrMess
   DEFAULT lRead TO .F.
   cBuffer := iif( lRead, MemoRead( cTXT ), cTXT )
   IF Empty( cBuffer )
      cErrMess := iif( lRead, "File " + cTXT + " could no be found/read!", "Cannot browse empty text!" )
      msgStop( cErrMess, "TEXT BROWSE ERROR" )
      RETURN NIL
   ENDIF
   DEFAULT cTitle TO "BROWSE TEXT"
   LOAD WINDOW BrowseText AS wBW
   wBW.Title := cTitle
   wBW.RichEdit_1.VALUE := cBuffer
   CENTER WINDOW wBW
   ACTIVATE WINDOW wBW

RETURN NIL

******************************************
PROCEDURE BrowseTextCopy2Clipboard()

   DECLARE WINDOW wBW
   System.Clipboard := wBW.RichEdit_1.VALUE

RETURN

******************************************************************
FUNCTION GetHostIP()
// 'Borrowed' code found into
// MiniGUI\SAMPLES\Advanced\SysInfo\SysInfo.prg by Grigory Filatov
******************************************************************
   LOCAL cIP := "000.000.000.000"
   LOCAL cRetIP := ""
   LOCAL nDot
   IF WSAStartUp() == 0
      cIP := GetHostByName( GetHostName() )
      WSACleanUp()
      WHILE ( nDot := At( ".", cIP ) ) > 0
         cRetIP += PadL( Left( cIp, nDot - 1 ), 3, "0" ) + "."
         cIP := SubStr( cIP, nDot + 1 )
      END
      cRetIP += PadL( cIP, 3, "0" )
   ENDIF

RETURN cRetIP

// ----------------------------------------------------------------------------

#pragma BEGINDUMP

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"

HB_FUNC ( GETHOSTNAME )
{
   BYTE Name[ 255 ];
   gethostname( ( char * ) Name, 255 );
   hb_retc( ( char * ) Name );
}


HB_FUNC ( GETHOSTBYNAME )
{
   struct hostent * pHost;
   BYTE addr[ 20 ];
   strcpy( ( char * ) addr, "0.0.0.0" );
   pHost = gethostbyname( ( char * ) hb_parc( 1 ) ) ;
   if( pHost )
   {
      wsprintf( ( char * ) addr, "%i.%i.%i.%i" ,
                ( BYTE ) pHost->h_addr[ 0 ], ( BYTE ) pHost->h_addr[ 1 ],
                ( BYTE ) pHost->h_addr[ 2 ], ( BYTE ) pHost->h_addr[ 3 ] );
   }
   hb_retc( ( char * ) addr );
}


HB_FUNC ( WSASTARTUP )
{
   WSADATA wsa;
   hb_retni( WSAStartup( 0x101, &wsa ) );
}


HB_FUNC ( WSACLEANUP )
{
   hb_retni( WSACleanup() );
}

#pragma ENDDUMP
