/*
 * USB Stick Arrive / Remove Demo
 *
 * (c) 2019 Auge & Ohr
 */

#include "hmg.ch"

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         WIDTH 400 ;
         HEIGHT 200 ;
         TITLE 'USB Stick Demo' ;
         MAIN ;
         NOMAXIMIZE ;
         ON INTERACTIVECLOSE MsgYesNo( "Are you sure?", "Exit" )

      @ 10, 10 BUTTON Button_1 ;
         CAPTION 'Exit' ;
         ACTION iif ( MsgYesNo( "Are you sure?", "Exit", .T. ), ThisWindow.RELEASE, Nil )

      @ 100, 10 LABEL Label_1 ;
         VALUE "insert / remove USB Stick" ;
         HEIGHT 40 ;
         WIDTH 360 ;
         FONT "Arial" SIZE 22 ;
         CENTERALIGN ;
         BORDER

   END WINDOW

   CREATE EVENT PROCNAME USB_Detect()

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


#define WM_DEVICECHANGE             0x0219
#define DBT_DEVICEARRIVAL           0x8000
#define DBT_DEVICEQUERYREMOVE       0x8001
#define DBT_DEVICEQUERYREMOVEFAILED 0x8002
#define DBT_DEVICEREMOVEPENDING     0x8003
#define DBT_DEVICEREMOVECOMPLETE    0x8004
#define DBT_DEVTYP_VOLUME           2


FUNCTION USB_Detect( nHWnd, nMsg, nWParam, nLParam )

   LOCAL nMask, cDevice := ""

   HB_SYMBOL_UNUSED( nHWnd )

   DO CASE

   CASE nMsg == WM_DEVICECHANGE

      DO CASE
      CASE nWParam == DBT_DEVICEARRIVAL
         nMask := DeviceChangeInfo( nLParam )
         IF ! Empty( nMask ) .AND. nMask > 0
            cDevice := GetDrive( nMask )
         ENDIF

         MSGINFO ( "Inserted drive " + cDevice + CRLF + WMI_Info( cDevice ) )

      CASE nWParam == DBT_DEVICEREMOVECOMPLETE
         nMask := DeviceChangeInfo( nLParam )
         IF ! Empty( nMask ) .AND. nMask > 0
            cDevice := GetDrive( nMask )
         ENDIF

         MSGINFO ( "Removed drive " + cDevice )

      END CASE

   END CASE

RETURN NIL


STATIC FUNCTION GetDrive( nMask )

   LOCAL cBin := NToC( nMask, 2 )
   LOCAL nBit := Len( cBin ) - hb_At( '1', cBin )

RETURN Chr( nBit + 65 ) + ":"


STATIC FUNCTION WMI_Info( cDrive )

   LOCAL oProcesses
   LOCAL oDrive
   LOCAL oWMI, lSuccess := .F.
   LOCAL cInfo := ""

   oWMI := WMIService()

   IF ! Empty( oWMI )
      DO WHILE ! lSuccess
         TRY
            oProcesses := oWMI:ExecQuery( "SELECT * FROM Win32_LogicalDisk" )
            lSuccess := .T.

         CATCH
            DO EVENTS
         END
      ENDDO

      IF oProcesses:Count > 0
         FOR EACH oDrive IN oProcesses
            IF oDrive:DeviceID = cDrive
               cInfo += "Name: " + oDrive:Description + CRLF
               cInfo += "Type: " + iif( oDrive:DriveType == 2, "Removable", "N/A" ) + CRLF
               cInfo += "File System: " + cValToChar( oDrive:FileSystem ) + CRLF
               cInfo += "Size: " + cValToChar( Val( oDrive:Size ) / ( 1024 * 1024 * 1024 ) ) + "GB" + CRLF
               cInfo += "Free Space: " + cValToChar( Val( oDrive:FreeSpace ) / ( 1024 * 1024 * 1024 ) ) + "GB" + CRLF
               cInfo += "Serial Number: " + cValToChar( oDrive:VolumeSerialNumber )
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

RETURN cInfo


STATIC FUNCTION WMIService()

   STATIC oWMI
   LOCAL oLocator, lSuccess := .F.

   IF oWMI == NIL
      oLocator := CreateObject( "wbemScripting.SwbemLocator" )
      IF Empty( oLocator )
         MsgInfo( "Can not create wbemScripting.SwbemLocator" )
      ELSE

         DO WHILE ! lSuccess
            TRY
               oWMI := oLocator:ConnectServer()
               lSuccess := .T.

            CATCH
               DO EVENTS
            END
         ENDDO

      ENDIF
   ENDIF

RETURN oWMI


/*
   C-Code
 */

#pragma BEGINDUMP

#include <mgdefs.h>
#include "dbt.h"

HB_FUNC( DEVICECHANGEINFO )
{
   PDEV_BROADCAST_HDR lpdb = (PDEV_BROADCAST_HDR) HB_PARNL( 1 );

   if ( lpdb->dbch_devicetype == DBT_DEVTYP_VOLUME )
   {
      PDEV_BROADCAST_VOLUME lpdbv = (PDEV_BROADCAST_VOLUME) lpdb;
      hb_retnl( lpdbv->dbcv_unitmask );
   }
   else
     hb_retnl( 0 );
}

#pragma ENDDUMP
