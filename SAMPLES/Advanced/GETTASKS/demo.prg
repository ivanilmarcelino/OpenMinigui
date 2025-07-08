/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018-2022 Grigory Filatov <gfilatov@gmail.com>
 *
 */

#include "hmg.ch"

FUNCTION Main()

   LOCAL cTask

   SET WINDOW MAIN FIRST OFF

   cTask := AChoice( ,,,, GetTasks(), "Processes in use" )

   IF ! Empty( cTask )
      MsgBox( cTask )
   ENDIF

RETURN NIL

*****************************************************************************

FUNCTION GetTasks()

   LOCAL aProcs := GetProcesses()
   LOCAL aExec := {}
   LOCAL aRet := {}
   LOCAL i

   ASort( aProcs,,, {| x, y | Upper( x[ 1 ] ) < Upper( y[ 1 ] ) } )

   FOR i = 1 TO Len( aProcs )
      IF AScan( aExec, aProcs[ i ][ 1 ] ) == 0
         AAdd( aExec, aProcs[ i ][ 1 ] )
         AAdd( aRet, GetLongPathName( aProcs[ i ][ 3 ] ) )
      ENDIF
   NEXT

RETURN aRet

*****************************************************************************

FUNCTION GetLongPathName ( cPath )

   LOCAL cLongPathName

RETURN iif( wapi_GetLongPathName( cPath, @cLongPathName ) > 0, cLongPathName, cPath )

*****************************************************************************

FUNCTION GetProcesses()

   LOCAL oWmi, oList, oProc
   LOCAL cUser, cDomain
   LOCAL aList := {}

   oWmi := WmiService()
   oList := oWmi:ExecQuery( "select * from Win32_Process" )
   TRY
      WaitWindow( "Please, wait...", .T. )
      FOR EACH oProc IN oList
         IF ( oProc:GetOwner( @cUser, @cDomain ) ) == 0 .AND. oProc:ExecutablePath <> NIL
            AAdd( aList, { oProc:Caption, oProc:Name, oProc:ExecutablePath, cUser, cDomain } )
         ENDIF
      NEXT
      WaitWindow()
   CATCH oError
      MsgStop ( "Error:     " + Transform( oError:GenCode, NIL ) + CRLF + ;
                "SubCode:   " + Transform( oError:SubCode, NIL ) + CRLF + ;
                "OSCode:    " + Transform( oError:OsCode, NIL ) + CRLF + ;
                "SubSystem: " + Transform( oError:SubSystem, NIL ) + CRLF + ;
                "Description:      " + oError:Description )
   END

RETURN aList

*****************************************************************************

FUNCTION WMIService()

   STATIC oWMI

   LOCAL oLocator

   IF oWMI == NIL

      oLocator := CREATEOBJECT( "wbemScripting.SwbemLocator" )
      oWMI := oLocator:ConnectServer()

   ENDIF

RETURN oWMI

*****************************************************************************

FUNCTION AChoice( t, l, b, r, aItems, cTitle, dummy, nValue )

   HB_SYMBOL_UNUSED( t )
   HB_SYMBOL_UNUSED( l )
   HB_SYMBOL_UNUSED( b )
   HB_SYMBOL_UNUSED( r )
   HB_SYMBOL_UNUSED( dummy )

   DEFAULT cTitle TO "Please, select", nValue TO 1

   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 + IF( IsXPThemeActive(), 7, 0 ) ;
         TITLE cTitle ;
         TOPMOST ;
         NOMAXIMIZE NOSIZE

      @ 415, 230 BUTTON Button_1 ;
         CAPTION 'OK' ;
         ACTION {|| nValue := Win_1.List_1.VALUE, Win_1.Release } ;
         WIDTH 80

      @ 415, 335 BUTTON Button_2 ;
         CAPTION 'Cancel' ;
         ACTION {|| nValue := 0, Win_1.Release } ;
         WIDTH 80

      @ 20, 15 LISTBOX List_1 ;
         WIDTH 600 ;
         HEIGHT 380 ;
         ITEMS aItems ;
         VALUE nValue ;
         FONT GetDefaultFontName() ;
         SIZE 10 ;
         ON DBLCLICK {|| nValue := Win_1.List_1.VALUE, Win_1.Release }

      ON KEY ESCAPE ACTION Win_1.Button_2.ONCLICK

   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN iif( nValue > 0, aItems[ nValue ], "" )

*****************************************************************************
