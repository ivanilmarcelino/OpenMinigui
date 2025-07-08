/*
 * MiniGUI Volume Info Demo
 */

#include "hmg.ch"

#define TAB Chr(9)
#define MsgInfo( c ) MsgInfo( c, , , .f. )

PROCEDURE Main

   LOCAL cVolName
   LOCAL nSerNum
   LOCAL nMaxName
   LOCAL nFlags
   LOCAL cSysName
   LOCAL cDiskSerial
   LOCAL cInfo := ""

   SET DIALOGBOX POSITION CENTER OF PARENT

   /* wapi_GetVolumeInformation( <cRootPath>, @<cVolumeName>, @<nSerial>,
    *                            @<nMaxComponentLength>, @<nFileSystemFlags>,
    *                            @<cFileSystemName> ) --> <lSuccess>
    */
   IF wapi_GetVolumeInformation( "c:\", @cVolName, @nSerNum, @nMaxName, @nFlags, @cSysName )

      cDiskSerial := I2Hex( HiWord( nSerNum ) ) + "-" + I2Hex( nSerNum )

      cInfo += "Drive Name :" + TAB + "C:" + CRLF
      cInfo += "Volume Name :" + TAB + cVolName + CRLF
      cInfo += "Serial Number :" + TAB + cDiskSerial + CRLF
      cInfo += "File System :" + TAB + cSysName
   ENDIF

   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 300 ;
         TITLE 'Volume Information' ;
         ICON "HD.ICO" ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      @ 10, 10 BUTTON Button1 CAPTION "Get Info" ACTION iif( Empty( cInfo ), MsgStop( "Error!" ), MsgInfo( cInfo ) )

      @ 50, 10 BUTTON Button2 CAPTION "Close" ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

RETURN

#pragma BEGINDUMP

#include <mgdefs.h>

static char * u2Hex( WORD wWord )
{
   static far char szHex[ 5 ];
   WORD i = 3;

   do
   {
      szHex[ i ] = ( char ) ( 48 + ( wWord & 0x000F ) );
      if( szHex[ i ] > 57 )
         szHex[ i ] += ( char ) 7;
      wWord >>= 4;
   }
   while( i-- > 0 );

   szHex[ 4 ] = 0;

   return szHex;
}

HB_FUNC( I2HEX )
{
   hb_retc( u2Hex( hmg_par_WORD( 1 ) ) );
}

#pragma ENDDUMP
