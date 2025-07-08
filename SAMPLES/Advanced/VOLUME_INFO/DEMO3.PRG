/*
 *
 * MiniGUI DLL Demo
 *
*/

#include "hmg.ch"
#include "hbdyn.ch"

#define TAB Chr(9)
#define MsgInfo( c ) MsgInfo( c, , , .f. )

PROCEDURE Main

   LOCAL cInfo := ""
   LOCAL aInfo := GetVolumeInfo( "c:\" )

   SET DIALOGBOX POSITION CENTER OF PARENT

   IF ! Empty( aInfo[ 2 ] )
      cInfo += "Drive Name :" + TAB + "C:" + CRLF
      cInfo += "Volume Name :" + TAB + aInfo[ 1 ] + CRLF
      cInfo += "Serial Number :" + TAB + aInfo[ 2 ] + CRLF
      cInfo += "File System :" + TAB + aInfo[ 3 ]
   ENDIF

   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 300 ;
         TITLE 'Volume Information via HMG_CallDLL()' ;
         ICON "HD.ICO" ;
         MAIN NOMAXIMIZE NOSIZE

      @ 10, 10 BUTTON Button1 CAPTION "Get Info" ACTION iif( Empty( cInfo ), MsgStop( "Error!" ), MsgInfo( cInfo ) )

      @ 50, 10 BUTTON Button2 CAPTION "Close" ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN

/*
*/
FUNCTION GetVolumeInfo( cDrive )

   LOCAL VolName := Space( 201 ), nVol := 200, Serial := Space( 4 ), MAXLEN := 0, Flags := 0, SysName := Space( 241 ), nSys := 240
   LOCAL aInfo := Array( 3 ), cTemp := "", i

   IF HMG_CallDLL( "KERNEL32.DLL", HB_DYN_CTYPE_BOOL, "GetVolumeInformation", cDrive, @VolName, nVol, @Serial, @MaxLen, Flags, @SysName, nSys )

      aInfo[ 1 ] := Trim( VolName )

      FOR i := 4 TO 1 STEP -1

         cTemp += PadLeft( DecToHexA( Asc( SubStr(Serial, i, 1 ) ) ), 2, "0" )

      NEXT

      aInfo[ 2 ] := PosIns( cTemp, "-", 5 )

      aInfo[ 3 ] := Trim( SysName )

   ENDIF

RETURN aInfo
