
#include "minigui.ch"
#include "error.ch"
#include "fileio.ch"
#include "hbmemvar.ch"
#include "hbver.ch"

STATIC cJson := "ErrorLog.json"
STATIC cDateTime := ""

*-----------------------------------------------------------------------------*
MEMVAR hError, br_Log

*-----------------------------------------------------------------------------*
FUNCTION DefErrorJson( oError )
*-----------------------------------------------------------------------------*
   LOCAL aKeys := {}, aLogEntry := {}

   IF hb_FileExists( cJson )
      hb_jsonDecode( FileStr( cJson ), @hError )
   END

   ErrorLogjSON( oError )
   doEvents()

   hb_jsonDecode( FileStr( cJson ), @hError )

   aKeys := hb_HKeys( hError )
   AEval( aKeys, {| e | AAdd( aLogEntry, { e, hError[ e ][ "Message" ], hError[ e ][ "User" ], hError[ e ][ "Time From Start" ] } ) } )
   br_Log:SetArray( aLogEntry )
   br_log:Reset( .T. )

RETURN .F.

*-----------------------------------------------------------------------------*
FUNCTION ErrorMessageJson( oError )
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
   CASE ! Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE ! Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   // add OS error code if available
   IF ! Empty( oError:osCode )
      cMessage += " (DOS Error " + hb_ntos( oError:osCode ) + ")"
   ENDIF

RETURN cMessage

*-----------------------------------------------------------------------------*
PROCEDURE ErrorLogJson( oErr )
*-----------------------------------------------------------------------------*
   LOCAL nScope, nCount, tmp, xValue, cName, N

   cDateTime := hb_TToC( hb_DateTime() )
   IF ! HB_ISHASH( hError )
      hError := hb_Hash()
   END
   hError[ cDateTime ] := hb_Hash()

   hError[ cDateTime ][ "Message" ] := ErrorMessagejSON( oErr )
   hError[ cDateTime ][ "Application" ] := GetExeFileName()
   hError[ cDateTime ][ "User" ] := NetName() + " / " + GetUserName()
   hError[ cDateTime ][ "Time From Start" ] := TimeFromStart()

   hError[ cDateTime ][ "StackTrace" ] := hb_Hash()

   n := 1
   WHILE ! Empty( ProcName( ++n ) )
      hError[ cDateTime ][ "StackTrace" ][ hb_ntoc( n - 1 ) ] := ProcName( n ) + "(" + hb_ntos( ProcLine( n ) ) + ")" + iif( ProcLine( n ) > 0, " in module: " + ProcFile( n ), "" )
   ENDDO

   hError[ cDateTime ][ "SystemInformation" ] := hb_Hash()
   hError[ cDateTime ][ "SystemInformation" ][ "Workstation Name" ] := NetName()
   hError[ cDateTime ][ "SystemInformation" ][ "Active user name" ] := GetUserName()
   hError[ cDateTime ][ "SystemInformation" ][ "Available memory" ] := x2c( MemoryStatus( 2 ) ) + " MB"
   hError[ cDateTime ][ "SystemInformation" ][ "Current disk" ] := DiskName()
   hError[ cDateTime ][ "SystemInformation" ][ "Current directory" ] := CurDir()
   hError[ cDateTime ][ "SystemInformation" ][ "Free disk space" ] := X2C( Round( hb_DiskSpace( hb_DirBase() ) / ( 1024 * 1024 ), 0 ) ) + " MB"
   hError[ cDateTime ][ "SystemInformation" ][ "Operating system" ] := OS()
   hError[ cDateTime ][ "SystemInformation" ][ "MiniGUI version" ] := MiniGUIVersion()
   hError[ cDateTime ][ "SystemInformation" ][ "Harbour version" ] := Version()
   hError[ cDateTime ][ "SystemInformation" ][ "Harbour built on" ] := hb_BuildDate()
   hError[ cDateTime ][ "SystemInformation" ][ "C/C++ compiler" ] := hb_Compiler()
   hError[ cDateTime ][ "SystemInformation" ][ "Multi Threading" ] := iif( hb_mtvm(), "YES", "NO" )
   hError[ cDateTime ][ "SystemInformation" ][ "VM Optimization" ] := iif( hb_VMMode() == 1, "YES", "NO" )

   IF hb_IsFunction( "Select" )
      hError[ cDateTime ][ "SystemInformation" ][ "Current Work Area" ] := X2C( Eval( hb_macroBlock( "Select()" ) ) )
   ENDIF
   hError[ cDateTime ][ "EnvironmentalInformation" ] := hb_Hash()
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET ALTERNATE" ] := X2C( Set( _SET_ALTERNATE ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET ALTFILE" ] := X2C( Set( _SET_ALTFILE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET AUTOPEN" ] := X2C( Set( _SET_AUTOPEN ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET AUTORDER" ] := X2C( Set( _SET_AUTORDER ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET AUTOSHARE" ] := X2C( Set( _SET_AUTOSHARE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET CENTURY" ] := X2C( __SetCentury(), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET COUNT" ] := X2C( Set( _SET_COUNT ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DATE FORMAT" ] := X2C( Set( _SET_DATEFORMAT ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DBFLOCKSCHEME" ] := X2C( Set( _SET_DBFLOCKSCHEME ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DEBUG" ] := X2C( Set( _SET_DEBUG ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DECIMALS" ] := X2C( Set( _SET_DECIMALS ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DEFAULT" ] := X2C( Set( _SET_DEFAULT ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DEFEXTENSIONS" ] := X2C( Set( _SET_DEFEXTENSIONS ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DELETED" ] := X2C( Set( _SET_DELETED ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DELIMCHARS" ] := X2C( Set( _SET_DELIMCHARS ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DELIMETERS" ] := X2C( Set( _SET_DELIMITERS ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DIRCASE" ] := X2C( Set( _SET_DIRCASE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET DIRSEPARATOR" ] := X2C( Set( _SET_DIRSEPARATOR ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EOL" ] := X2C( Asc( Set( _SET_EOL ) ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EPOCH" ] := X2C( Set( _SET_EPOCH ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET ERRORLOG" ] := X2C( _GetErrorlogFile() )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EXACT" ] := X2C( Set( _SET_EXACT ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EXCLUSIVE" ] := X2C( Set( _SET_EXCLUSIVE ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EXTRA" ] := X2C( Set( _SET_EXTRA ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET EXTRAFILE" ] := X2C( Set( _SET_EXTRAFILE ) )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET FILECASE" ] := X2C( Set( _SET_FILECASE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET FIXED" ] := X2C( Set( _SET_FIXED ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET FORCEOPT" ] := X2C( Set( _SET_FORCEOPT ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET HARDCOMMIT" ] := X2C( Set( _SET_HARDCOMMIT ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET IDLEREPEAT" ] := X2C( Set( _SET_IDLEREPEAT ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET LANGUAGE" ] := X2C( Set( _SET_LANGUAGE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET MARGIN" ] := X2C( Set( _SET_MARGIN ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET MBLOCKSIZE" ] := X2C( Set( _SET_MBLOCKSIZE ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET MFILEEXT" ] := X2C( Set( _SET_MFILEEXT ) )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET OPTIMIZE" ] := X2C( Set( _SET_OPTIMIZE ), .T. )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET PATH" ] := X2C( Set( _SET_PATH ) )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET PRINTER" ] := X2C( Set( _SET_PRINTER ), .T. )
   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET PRINTFILE" ] := X2C( Set( _SET_PRINTFILE ) )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET SOFTSEEK" ] := X2C( Set( _SET_SOFTSEEK ), .T. )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET TRIMFILENAME" ] := X2C( Set( _SET_TRIMFILENAME ) )

   hError[ cDateTime ][ "EnvironmentalInformation" ][ "SET UNIQUE" ] := X2C( Set( _SET_UNIQUE ), .T. )

   hError[ cDateTime ][ "WorkAreaInformation" ] := hb_Hash()

   hb_WAEval( {||
   IF hb_IsFunction( "Select" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ] := hb_Hash()
   ENDIF
   IF hb_IsFunction( "Alias" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "Alias" ] := Do( "Alias" )
   ENDIF
   IF hb_IsFunction( "RecNo" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "Current Recno" ] := X2C( Do( "RecNo" ) )
   ENDIF
   IF hb_IsFunction( "dbFilter" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "Current Filter" ] := X2C( Do( "dbFilter" ) )
   ENDIF
   IF hb_IsFunction( "dbRelation" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "dbRelation" ] := X2C( Do( "dbRelation" ) )
   ENDIF
   IF hb_IsFunction( "IndexOrd" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "IndexOrd" ] := X2C( Do( "IndexOrd" ) )
   ENDIF
   IF hb_IsFunction( "IndexKey" )
      hError[ cDateTime ][ "WorkAreaInformation" ][ X2C( Do( "Select" ) ) ][ "IndexKey" ] := X2C( Do( "IndexKey" ) )
   ENDIF

   RETURN .T.
   } )

   hError[ cDateTime ][ "Internal Error Handling Information" ] := hb_Hash()
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Subsystem Call" ] := oErr:subsystem()
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "System Code" ] := X2C( oErr:subcode() )
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Default Status" ] := X2C( oErr:candefault() )
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Description" ] := oErr:description()
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Operation" ] := oErr:operation()
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Involved File" ] := oErr:filename()
   hError[ cDateTime ][ "Internal Error Handling Information" ][ "Dos Error Code" ] := X2C( oErr:oscode() )

   hError[ cDateTime ][ "Available Memory Variables" ] := hb_Hash()


   FOR EACH nScope IN { 1, 2, 3, 4, 5, 6 }

      nCount := __mvDbgInfo( nScope )
      FOR tmp := 1 TO nCount

         xValue := __mvDbgInfo( nScope, tmp, @cName )
// IF ValType( xValue ) $ "CNDTL" .AND. Left( cName, 1 ) <> "_"
         hError[ cDateTime ][ "Available Memory Variables" ][ hb_ntoc( tmp ) ] := hb_Hash()
         hError[ cDateTime ][ "Available Memory Variables" ][ hb_ntoc( tmp ) ][ "Name" ] := cName
         hError[ cDateTime ][ "Available Memory Variables" ][ hb_ntoc( tmp ) ][ "Type" ] := ValType( xValue )
         hError[ cDateTime ][ "Available Memory Variables" ][ hb_ntoc( tmp ) ][ "Value" ] := hb_CStr( xValue )
// ENDIF

      NEXT

   NEXT

   StrFile( hb_jsonEncode( hError ), cJson, .F. )
   hb_jsonDecode( FileStr( cJson ), @hError )

RETURN

*-----------------------------------------------------------------------------*
FUNCTION x2c( c, l )
*-----------------------------------------------------------------------------*

   SWITCH ValType( c )
   CASE "C"
   CASE "M" ; RETURN c
   CASE "N" ; RETURN hb_ntos( c )
   CASE "D" ; RETURN DToC( c )
   CASE "L" ; RETURN iif( hb_defaultValue( l, .F. ), iif( c, "ON", "OFF" ), iif( c, ".T.", ".F." ) )
   ENDSWITCH

RETURN ""
