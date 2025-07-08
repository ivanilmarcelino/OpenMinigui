/*
 * MiniGUI WMI Service Demo
 *
 * (c) 2008-2009 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

PROCEDURE Main

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 200 + iif( IsVistaOrLater(), GetBorderHeight(), 0 ) ;
         TITLE 'WMI Services Demo' ;
         MAIN

      DEFINE BUTTON Button_1
         ROW 10
         COL 10
         WIDTH 120
         CAPTION 'Processor Info'
         ACTION ProcessorInfo()
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 40
         COL 10
         WIDTH 120
         CAPTION 'Disk Drive Info'
         ACTION DiskDriveInfo()
      END BUTTON

      DEFINE BUTTON Button_3
         ROW 70
         COL 10
         WIDTH 120
         CAPTION 'Logical Disk Info'
         ACTION LogicalDiskInfo()
      END BUTTON

      DEFINE BUTTON Button_4
         ROW 100
         COL 10
         WIDTH 120
         CAPTION 'Physical Media Info'
         ACTION PhysicalMediaInfo()
      END BUTTON

      DEFINE BUTTON Button_5
         ROW 130
         COL 10
         WIDTH 120
         CAPTION 'Account Info'
         ACTION SIDInfo( GetUserName() )
      END BUTTON

   END WINDOW

   Form_1.Button_4.Enabled := IsWinNT()
   Form_1.Button_5.Enabled := IsWinNT()

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

*-----------------------------------------------------------------------------*
#translate IFNOTCHAR( <exp1>,<exp2> ) ;
      = > ;
      IF( ValType( < exp1 > ) != "C", < exp2 >, < exp1 > )

#define IS_DATE(x)                   (VALTYPE(x) == "D")
#define IS_LOGICAL(x)                (VALTYPE(x) == "L")
#define IS_NUMERIC(x)                (VALTYPE(x) == "N")
#define CASE_AT(x,y,z)               z[AT(x,y)+1]
#define TRIM_NUMBER(x)               LTRIM(STR(x))
#define NULL                         ""

#define XTOC(x)              CASE_AT(VALTYPE(x), "CNDLM", ;
      { NULL, ;
      x, ;
      IF( IS_NUMERIC( x ), ;
      TRIM_NUMBER( x ), ;
      NULL ), ;
      IF( IS_DATE( x ), DToC( x ), NULL ), ;
      IF( IS_LOGICAL( x ), ;
      IF( x, ".T.", ".F." ), ;
      NULL ), ;
      x } )
*-----------------------------------------------------------------------------*


FUNCTION ProcessorInfo()

   LOCAL oWmi, oProc
   LOCAL cInfo := ""

   oWmi := WmiService()

   FOR EACH oProc IN oWmi:ExecQuery( "SELECT * FROM Win32_Processor" )

      cInfo := ""
      cInfo += "Manufacturer: " + oProc:Manufacturer + CRLF
      cInfo += "Model: " + oProc:Name + CRLF
      cInfo += "Description: " + oProc:Description + CRLF
      cInfo += "ID: " + oProc:ProcessorID + CRLF + CRLF

      IF IsWinNT() .AND. ! IsWinXPHome()

         cInfo += "Cores: " + TRIM_NUMBER( oProc:NumberOfCores ) + CRLF
         cInfo += "Logical Processors: " + TRIM_NUMBER( oProc:NumberOfLogicalProcessors ) + CRLF
         IF "Intel" $ oProc:Manufacturer
            cInfo += "Hyper-Threading: " + iif( oProc:NumberOfCores < oProc:NumberOfLogicalProcessors, "Enabled", "Disabled" ) + CRLF
         ENDIF
         cInfo += CRLF

      ENDIF

      cInfo += "Address Width: " + TRIM_NUMBER( oProc:AddressWidth ) + " bits" + CRLF
      cInfo += "Data Width: " + TRIM_NUMBER( oProc:DataWidth ) + " bits" + CRLF + CRLF

      cInfo += "Current Speed: " + TRIM_NUMBER( oProc:CurrentClockSpeed ) + " MHz" + CRLF
      cInfo += "Max Speed: " + TRIM_NUMBER( oProc:MaxClockSpeed ) + " MHz" + CRLF
      cInfo += "Ext Clock: " + TRIM_NUMBER( oProc:ExtClock ) + " MHz"

      MsgInfo( cInfo, oProc:DeviceID )

   NEXT

RETURN NIL


FUNCTION DiskDriveInfo()

   LOCAL oWmi, oDrive
   LOCAL cInfo := ""

   oWmi := WmiService()

   FOR EACH oDrive IN oWmi:ExecQuery( "SELECT * FROM Win32_DiskDrive" )

      cInfo += "Model: " + IFEMPTY( oDrive:Model, "N/A", oDrive:Model ) + CRLF
      cInfo += "Name: " + StrTran( oDrive:Name, "\\.\", "" ) + CRLF
      cInfo += "Type: " + IFEMPTY( oDrive:InterfaceType, "N/A", oDrive:InterfaceType ) + CRLF

      IF IsWinNT()

         cInfo += "Signature: " + IF( IS_NUMERIC( oDrive:Signature ), LTrim( Str( Abs( oDrive:Signature ), 20, 0 ) ), "N/A" ) + CRLF

      ENDIF

      cInfo += CRLF

   NEXT

   MsgInfo( cInfo, "Result" )

RETURN NIL


FUNCTION LogicalDiskInfo()

   LOCAL oWmi, oDrive, cSerialNumber
   LOCAL cInfo := ""

   oWmi := WmiService()

   FOR EACH oDrive IN oWmi:ExecQuery( "SELECT * FROM Win32_LogicalDisk" )

      cSerialNumber := XTOC( oDrive:VolumeSerialNumber )
      cInfo += "Volume: " + oDrive:Name + "   Serial Number: " + IFEMPTY( cSerialNumber, "N/A", Stuff( cSerialNumber, 5, 0, "-" ) ) + CRLF

   NEXT

   MsgInfo( cInfo, "Result" )

RETURN NIL


FUNCTION PhysicalMediaInfo()

   LOCAL oWmi, oDrive
   LOCAL cInfo := ""

   oWmi := WmiService()

   FOR EACH oDrive IN oWmi:ExecQuery( "SELECT * FROM Win32_PhysicalMedia" )

      cInfo := "Serial: " + IfNotChar( oDrive:SerialNumber, "N/A" )

      MsgInfo( cInfo, IfNotChar( StrTran( oDrive:Tag, "\\.\", "" ), "N/A" ) )

   NEXT

RETURN NIL


FUNCTION SIDInfo( cUserAccount )

   LOCAL oWmi, oProc
   LOCAL cInfo
   LOCAL cFullName := Myuser(.T.)

   oWmi := WmiService()

   FOR EACH oProc IN oWmi:ExecQuery( "SELECT * FROM Win32_Account WHERE Name = '" + cUserAccount + "'" )
      cInfo := ""
      cInfo += "UserName: " + oProc:Name + CRLF
      if !empty( cFullname )
         cInfo += "Full UserName: "+  cFullName + CRLF
      Endif
      cInfo += "SID: " + oProc:SID + CRLF
      cInfo += "SIDtype: " + TRIM_NUMBER( oProc:SIDtype ) + CRLF
      cInfo += "Domain: " + oProc:Domain + CRLF
      cInfo += "Status: " + oProc:Status + CRLF
      cInfo += "Caption: " + oProc:Caption + CRLF
      cInfo += "Description: " + oProc:Description

      MsgInfo( cInfo, "Account Properties" )

   NEXT

RETURN NIL


*-----------------------------------------------------------------------------*
STATIC FUNCTION WMIService()
*-----------------------------------------------------------------------------*
   STATIC oWMI

   LOCAL oLocator

   IF oWMI == NIL

      oLocator := CreateObject( "wbemScripting.SwbemLocator" )
      oWMI := oLocator:ConnectServer()

   ENDIF

RETURN oWMI

*-----------------------------------------------------------------------------*
STATIC FUNCTION IsWinXPHome()
*-----------------------------------------------------------------------------*
   LOCAL AVER := WinVersion()

RETURN "Home" $ AVER[ 4 ]

*-----------------------------------------------------------------------------*
FUNCTION MyUser( lOnlyfull )
*-----------------------------------------------------------------------------*
   LOCAL hResult, cResult, npos
   LOCAL cUserName := GetUserName()
   LOCAL cCommand := "wmic.exe useraccount where name='" + cUserName + "' get fullname"

   DEFAULT lOnlyfull to .F.

   hResult := SysCmd( cCommand, @cResult )

   IF hResult != -1
      npos := At( " ", cResult ) + 1
      IF npos > 1     // the full name is there
         cUsername := SubStr( cResult, npos )
      Else
         cUsername := iif( lOnlyFull, "", cUserName )
      Endif
   Endif

RETURN cUsername

*-----------------------------------------------------------------------------*
STATIC FUNCTION SysCmd( cCommand, /*@*/ cResult )
*-----------------------------------------------------------------------------*
   LOCAL hProcess
   LOCAL hStdOut, hStderr, nState, nBytes
   LOCAL cBuff := Space( 1024 )

   // hProcess := HB_PROCESSOPEN( <cCommand>, NIL, @hStdOut, @hStderr, lDetach )
   hProcess := hb_processOpen( cCommand, NIL, @hStdOut, @hStdErr, .T. )

   IF hProcess != -1

      // nState := hb_ProcessValue( hProcess, lWait )
      nState := hb_processValue( hProcess, .T. )

      WHILE nState <> -1

         nBytes := FRead( hStdOut, @cBuff, 1024 /* cBuff length */ )

         IF nBytes == 0
            EXIT
         ENDIF

         nState := hb_processValue( hProcess, .T. )

      END

      cBuff := StrTran( cBuff, Chr( 13 ) )
      cBuff := StrTran( cBuff, Chr( 10 ) )
      cResult := AllTrim( StrUnspace( cBuff ) )

      hb_processClose( hProcess )

   ENDIF

RETURN hProcess

/* Converts multiple spaces to just one. | source: c:\harbour\config\lang.hb */
*-----------------------------------------------------------------------------*
STATIC FUNCTION StrUnspace( cString )
*-----------------------------------------------------------------------------*
   LOCAL cResult := ""
   LOCAL cChar, cCharPrev
   LOCAL tmp

   FOR tmp := 1 TO Len( cString )

      cChar := SubStr( cString, tmp, 1 )

      IF !( cChar == " " ) .OR. !( cCharPrev == " " )
         cResult += cChar
      ENDIF

      cCharPrev := cChar

   NEXT

RETURN cResult
