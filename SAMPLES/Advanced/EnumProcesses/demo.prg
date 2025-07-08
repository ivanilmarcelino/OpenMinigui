/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014 Dr. Claudio Soto <srvet@adinet.com.uy>
 *
 * The double click allow you terminate the process
 *
 * Used functions:
   - GetCurrentProcessId() --> return nProcessID
   - EnumProcessesID () ---> return array { nProcessID1, nProcessID2, ... }
   - GetProcessName ( [ nProcessID ] ) --> return cProcessName
   - GetProcessFullName ( [ nProcessID ] ) --> return cProcessFullName
   - GetWindowThreadProcessId (hWnd, @nThread, @nProcessID)
   - IsWow64Process ( [ nProcessID ] ) --> return lBoolean
     - return TRUE  if a 32-bit application is running under 64-bit Windows (WOW64)
     - return FALSE if a 32-bit application is running under 32-bit Windows
     - return FALSE if a 64-bit application is running under 64-bit Windows
     - WOW64 is the x86 emulator that allows 32-bit Windows-based applications to running on 64-bit Windows
*/

*************************************************************************************
* Attention: to detect processes 32 and 64 bits you should compiling with HMG-64 bits
*************************************************************************************

#include "hmg.ch"

#define WIN32_PREFIX "*32"


FUNCTION Main()

   LOCAL aRows := {}, i
   LOCAL nID, c32, cName, cNameFull
   LOCAL fColor, bColor
   LOCAL aProcessesID := EnumProcessesID ()

   FOR i = 1 TO Len ( aProcessesID )
      nID := aProcessesID[ i ]
      c32 := iif ( IsWow64Process( nID ), WIN32_PREFIX, "" )
      cName := GetProcessName( nID )
      cNameFull := GetProcessFullName( nID )
      IF .NOT. Empty ( cNameFull )
         AAdd ( aRows, { hb_ntos( nID ), cName + c32, cNameFull } )
      ENDIF
   NEXT

   ASort ( aRows, , , {| x, y | Upper( x[ 2 ] ) < Upper( y[ 2 ] ) } )

   DEFINE WINDOW Form_1 ;
      WIDTH 800 ;
      HEIGHT 550 ;
      BACKCOLOR TEAL ;
      TITLE 'EnumProcesses' ;
      MAIN

      fColor := {|| iif ( GetCurrentProcessID() == Val ( Form_1.Grid_1.Cell ( This.CellRowIndex, 1 ) ), AQUA, iif ( This.CellColIndex == 2 .AND. Right ( This.CellValue, Len ( WIN32_PREFIX ) ) == WIN32_PREFIX, RED, BLUE ) ) }
      bColor := {|| iif ( GetCurrentProcessID() == Val ( Form_1.Grid_1.Cell ( This.CellRowIndex, 1 ) ), GRAY, SILVER ) }

      @ 30, 10 GRID Grid_1 ;
         WIDTH 760 ;
         HEIGHT 450 ;
         BACKCOLOR SILVER ;
         FONT "Courier New" SIZE 12 ;
         HEADERS { 'ID', 'Name', 'Full Name' } ;
         WIDTHS { 100, 0, 0 } ;
         ITEMS aRows ;
         VALUE { 1, 1 } ;
         ON DBLCLICK PROC_Terminate_Process() ;
         DYNAMICFORECOLOR { fColor, fColor, fColor } ;
         DYNAMICBACKCOLOR { bColor, bColor, bColor } ;
         JUSTIFY { GRID_JTFY_RIGHT, NIL, NIL } ;
         CELLNAVIGATION

      Form_1.Grid_1.ColumnAutoFit ( 2 )
      Form_1.Grid_1.ColumnAutoFit ( 3 )

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE PROC_Terminate_Process

   LOCAL nCellRow := GetProperty ( "Form_1", "Grid_1", "CellRowFocused" )
   LOCAL nID := Val ( Form_1.Grid_1.Cell ( nCellRow, 1 ) )

   IF MsgYesNo ( { nID, " : ", Form_1.Grid_1.Cell ( nCellRow, 2 ) }, "Terminate Process" ) == .T.
      TerminateProcess ( nID )
      Form_1.Grid_1.DeleteItem ( nCellRow )
   ENDIF

RETURN
