/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
 *
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"
#include "tsbrowse.ch"

#define GWL_EXSTYLE          (-20)
#define WS_EX_DLGMODALFRAME  0x00000001

REQUEST DBFCDX

MEMVAR br_log, br_stack, br_sys, br_env, br_wa, br_err, br_var
MEMVAR lReadyErr

MEMVAR hError

FUNCTION ErrorView()

   LOCAL nColor := GetSysColor( COLOR_BTNFACE )
   LOCAL aColor := { GetRed( nColor ), GetGreen( nColor ), GetBlue( nColor ) }
   LOCAL oWnd
   LOCAL aKeys
   LOCAL aLogEntry := {}

   PUBLIC br_log, br_stack, br_sys, br_env, br_wa, br_err, br_var
   PUBLIC lReadyErr := .F., hError

   rddSetDefault( "DBFCDX" )

   USE ( "TEST" ) INDEX ( "TEST" ) ALIAS A1 SHARED NEW
   USE ( "TEST" ) ALIAS A2 SHARED NEW
   USE ( "TEST" ) ALIAS A3 SHARED NEW


   ErrorBlock( {| oError | DefErrorJson( oError ) } )


   SET OOP ON

   DEFINE WINDOW Form_Err ;
         AT 0, 0 ;
         WIDTH 900 ;
         HEIGHT 600 ;
         TITLE "Program Error Viewer" ;
         MAIN ;
         NOSIZE ;
         NOMAXIMIZE


      oWnd := ThisWindow.OBJECT

      DEFINE STATUSBAR FONT "ARIAL" SIZE 9 BOLD
         STATUSITEM '' WIDTH 100 ACTION NIL
         STATUSITEM '' WIDTH 150 ACTION NIL
         STATUSITEM '' ACTION NIL
         STATUSITEM '' WIDTH 80 ACTION NIL
         STATUSITEM '' WIDTH 400 ACTION NIL
         DATE
         CLOCK
      END STATUSBAR

   END WINDOW


   DEFINE LABEL Label_List
      PARENT Form_Err
      ROW 4
      COL 2
      WIDTH 300
      HEIGHT 16
      FONTNAME 'Arial'
      FONTSIZE 10
      FONTBOLD .T.
      VALUE "JSON ErrorLog viewer sample: "
   END LABEL


   DEFINE BUTTONEX Button_Error
      PARENT Form_Err
      ROW 4
      COL oWnd:ClientWidth() - 152
      WIDTH 150
      HEIGHT 27
      ACTION {|| GenError( hb_RandomInt( 1, 9 ) ) }
      CAPTION "Generate new Error"
      PICTURE "BUG"
      TABSTOP .F.
      NOHOTLIGHT TRUE
      TOOLTIP ""
      FONTNAME "Arial Narrow"
      FONTSIZE 8
   END BUTTONEX


   hError := hb_Hash()

   IF hb_FileExists( "ErrorLog.Json" )
      hb_jsonDecode( FileStr( "ErrorLog.Json" ), @hError )
   ELSE
      GenError( 1 )
   ENDIF
   aKeys := hb_HKeys( hError )

   AEval( aKeys, {| e | AAdd( aLogEntry, { e, hError[ e ][ "Message" ], hError[ e ][ "User" ], hError[ e ][ "Time From Start" ] } ) } )

   DEFINE TBROWSE br_log AT 35, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 6 ;
         HEIGHT 200 - 35 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE


   SetWindowLong( br_log:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_log

      :SetArray( aLogEntry, TRUE )
      :lCellBrw := TRUE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 30
      :nHeightCell := 20

      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора
      :SetColor( { 12 }, { {| a, b, c | IF( c:nCell == b, -RGB( 228, 228, 228 ), -RGB( 228, 228, 228 ) ) } } ) // фон курсора

      :bChange := {|| ErrorChange() }

      :Getcolumn( 1 ):cName := "Date"
      :Getcolumn( 1 ):cHeading := "DateTime"
      :SetColSize( 1, 150 )
      :Getcolumn( 1 ):uBmpHead := {|| LoadImage( "CALENDAR" ) }


      :Getcolumn( 2 ):cName := "Error"
      :Getcolumn( 2 ):cHeading := "Error descr"
      :Getcolumn( 2 ):uBmpHead := {|| LoadImage( "APPERROR" ) }
      :SetColSize( 2, 350 )


      :Getcolumn( 3 ):cName := "User"
      :Getcolumn( 3 ):cHeading := "User"
      :SetColSize( 3, 160 )
      :Getcolumn( 3 ):uBmpHead := {|| LoadImage( "USER" ) }

      :Getcolumn( 4 ):cName := "WorkTime"
      :Getcolumn( 4 ):cHeading := "WorkTime"
      :SetColSize( 4, 200 )
      :Getcolumn( 4 ):uBmpHead := {|| LoadImage( "ALARM_OK" ) }

   END

   AEval( br_log:aColumns, {| oCol | oCol:nClrSeleBack := oCol:nClrFocuBack, oCol:nClrSeleFore := oCol:nClrFocuFore, oCol:lFixLite := TRUE } )


   DEFINE TAB Tab_Err ;
         PARENT Form_Err ;
         AT 200, 2 ;
         WIDTH oWnd:clientWidth() - 4 ;
         HEIGHT oWnd:clientHeight() - oWnd:StatusBar:Height() - 205 ;
         VALUE 1 ;
         FONT "Arial Narrow" SIZE 9 ;
         BOLD ;
         BACKCOLOR aColor ;
         HOTTRACK ;
         HTFORECOLOR BLACK ;
         HTINACTIVECOLOR { 124, 124, 124 } ;
         ON CHANGE {|| NIL } ;
         TOOLTIP '' ;
         NOTABSTOP

      PAGE "Stack Trace" IMAGE "TREEERROR"
         StackBrowse( oWnd )
      END PAGE
      PAGE "System Info" IMAGE "COMHELP"
         SysBrowse( oWnd )
      END PAGE
      PAGE "Enviroment" IMAGE "INTERNET"
         EnvBrowse( oWnd )
      END PAGE
      PAGE "Work Area" IMAGE "DBHELP"
         WaBrowse( oWnd )
      END PAGE
      PAGE "Error Info" IMAGE "APPERROR"
         ErrBrowse( oWnd )
      END PAGE
      PAGE "Variables" IMAGE "VARWARN"
         VarBrowse( oWnd )
      END PAGE

   END TAB


   lReadyErr := TRUE

   Form_err.CENTER
   Form_err.ACTIVATE

RETURN NIL

FUNCTION LoadKey( cKey )

   LOCAL aHash := NIL
   LOCAL aArray := {}
   LOCAL x

   aHash := hError[ br_log:aArray[ br_log:nAt ][ 1 ] ][ cKey ]

   FOR EACH x IN aHash
      AAdd( aArray, hb_HPairAt( aHash, hb_enumindex( x ) ) )
   END

RETURN aArray

FUNCTION LoadKeyWA( cKey )

   LOCAL aHash := hError[ br_log:aArray[ br_log:nAt ][ 1 ] ][ cKey ]
   LOCAL aLine

   LOCAL aArray := {}
   LOCAL x


   IF hb_HHasKey( aHash, "1" )
      FOR EACH x IN aHash
         aLine := {}
         AAdd( aLine, x[ "Alias" ] )
         AAdd( aLine, x[ "Current Recno" ] )
         AAdd( aLine, x[ "Current Filter" ] )
         AAdd( aLine, x[ "IndexOrd" ] )
         AAdd( aLine, x[ "IndexKey" ] )
         AAdd( aArray, aLine )
      END
   ELSE
      aLine := { "", "", "", "", "" }
      AAdd( aArray, aLine )
   END

RETURN aArray

FUNCTION LoadKeyVar( cKey )

   LOCAL aHash := hError[ br_log:aArray[ br_log:nAt ][ 1 ] ][ cKey ]
   LOCAL aLine

   LOCAL aArray := {}
   LOCAL x


   FOR EACH x IN aHash
      aLine := {}
      AAdd( aLine, x[ "Name" ] )
      AAdd( aLine, x[ "Type" ] )
      AAdd( aLine, x[ "Value" ] )
      AAdd( aArray, aLine )
   END
   // MsgDebug(aArray)

RETURN aArray

FUNCTION ErrorChange()
   IF lReadyErr

      br_stack:SetArray( LoadKey( "StackTrace" ) )
      br_stack:Reset()

      br_sys:SetArray( LoadKey( "SystemInformation" ) )
      br_sys:Reset()

      br_env:SetArray( LoadKey( "EnvironmentalInformation" ) )
      br_env:Reset()

      br_wa:SetArray( LoadKeyWA( "WorkAreaInformation" ) )
      br_wa:Getcolumn( 1 ):cHeading := "Alias" + "(" + hb_ntoc( Len( br_wa:aArray ) ) + ")"
      br_wa:Reset()

      br_err:SetArray( LoadKey( "Internal Error Handling Information" ) )
      br_err:Reset()

      br_var:SetArray( LoadKeyVar( "Available Memory Variables" ) )
      br_var:Reset()

   END

RETURN NIL

FUNCTION StackBrowse( oWnd )

   LOCAL aArray := LoadKey( "StackTrace" )

   DEFINE TBROWSE br_stack AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_stack:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_stack

      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 1 }, { {|| if( hb_regexLike( ".+([0-9][^0])+.+", br_stack:aArray[ br_stack:nAt ][ 2 ], .F. ), CLR_RED, CLR_BLACK ) } } ) // фон курсора
      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора


      :Getcolumn( 1 ):cName := "#"
      :Getcolumn( 1 ):cHeading := "#"
      :SetColSize( 1, 30 )

      :Getcolumn( 2 ):cName := "ProcName"
      :Getcolumn( 2 ):cHeading := "ProcName ( ProcLine )"
      :SetColSize( 2, oWnd:clientWidth() - 70 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL


FUNCTION SysBrowse( oWnd )

   LOCAL aArray := LoadKey( "SystemInformation" )

   DEFINE TBROWSE br_sys AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_sys:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_sys

      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора

      :Getcolumn( 1 ):cName := "Name"
      :Getcolumn( 1 ):cHeading := "Info Name"
      :Getcolumn( 1 ):bDecode := {| x | Space( 3 ) + x }
      :SetColSize( 1, 150 )

      :Getcolumn( 2 ):cName := "Value"
      :Getcolumn( 2 ):cHeading := "Info Value"
      :SetColSize( 2, oWnd:clientWidth() - 180 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL


FUNCTION ErrBrowse( oWnd )

   LOCAL aArray := LoadKey( "Internal Error Handling Information" )

   DEFINE TBROWSE br_err AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_sys:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_err

      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора

      :Getcolumn( 1 ):cName := "Name"
      :Getcolumn( 1 ):cHeading := "Info Name"
      :Getcolumn( 1 ):bDecode := {| x | Space( 3 ) + x }
      :SetColSize( 1, 150 )

      :Getcolumn( 2 ):cName := "Value"
      :Getcolumn( 2 ):cHeading := "Info Value"
      :SetColSize( 2, oWnd:clientWidth() - 180 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL


FUNCTION EnvBrowse( oWnd )

   LOCAL aArray := LoadKey( "EnvironmentalInformation" )

   DEFINE TBROWSE br_env AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_env:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_env
      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора


      :Getcolumn( 1 ):cName := "Name"
      :Getcolumn( 1 ):cHeading := "Info Name"
      :Getcolumn( 1 ):bDecode := {| x | Space( 3 ) + x }
      :SetColSize( 1, 200 )

      :Getcolumn( 2 ):cName := "Value"
      :Getcolumn( 2 ):cHeading := "Info Value"
      :SetColSize( 2, oWnd:clientWidth() - 230 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL

FUNCTION VarBrowse( oWnd )

   LOCAL aArray := LoadKeyVar( "Available Memory Variables" )

   DEFINE TBROWSE br_var AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_var:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_var
      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора


      :Getcolumn( 1 ):cName := "Name"
      :Getcolumn( 1 ):cHeading := "var Name"
      :Getcolumn( 1 ):bDecode := {| x | Space( 3 ) + x }
      :SetColSize( 1, 200 )

      :Getcolumn( 2 ):cName := "Type"
      :Getcolumn( 2 ):cHeading := "var Value"
      :SetColSize( 2, 100 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }

      :Getcolumn( 3 ):cName := "Value"
      :Getcolumn( 3 ):cHeading := "Var Value"
      :SetColSize( 3, oWnd:clientWidth() - 330 )
      :Getcolumn( 3 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL


FUNCTION WaBrowse( oWnd )

   LOCAL aArray := LoadKeyWa( "WorkAreaInformation" )

   DEFINE TBROWSE br_wa AT 40, 4 ;
         OF Form_err ;
         WIDTH oWnd:clientWidth() - 12 ;
         HEIGHT 400 - 100 ;
         FONT "Arial" ;
         SIZE 9 ;
         CELL

   END TBROWSE

   SetWindowLong( br_wa:hWnd, GWL_EXSTYLE, WS_EX_DLGMODALFRAME )

   WITH OBJECT br_wa

      :SetArray( aArray, TRUE )
      :lCellBrw := FALSE
      :lNoChangeOrd := TRUE
      :lNoHScroll := TRUE
      :nHeightHead := 20
      :nHeightCell := 20
      :lNoGrayBar := .T.
      :lNoLiteBar := .T.

      :SetColor( { 1 }, { {|| if( br_wa:nAt == CurRecNo(), CLR_RED, CLR_BLACK ) } } ) // фон курсора
      :SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора


      :Getcolumn( 1 ):cName := "Alias"
      :Getcolumn( 1 ):cHeading := "Alias" + "(" + hb_ntoc( Len( br_wa:aArray ) ) + ")"
      :Getcolumn( 1 ):bDecode := {| x | Space( 3 ) + x }
      :SetColSize( 1, 150 )

      :Getcolumn( 2 ):cName := "Recno"
      :Getcolumn( 2 ):cHeading := "Recno"
      :SetColSize( 2, 100 )
      :Getcolumn( 2 ):bDecode := {| x | Space( 3 ) + x }

      :Getcolumn( 3 ):cName := "Filter"
      :Getcolumn( 3 ):cHeading := "Filter"
      :SetColSize( 3, 200 )
      :Getcolumn( 3 ):bDecode := {| x | Space( 3 ) + x }

      :Getcolumn( 4 ):cName := "IndexOrd"
      :Getcolumn( 4 ):cHeading := "IndexOrd"
      :SetColSize( 4, 100 )
      :Getcolumn( 4 ):bDecode := {| x | Space( 3 ) + x }

      :Getcolumn( 5 ):cName := "IndexKey"
      :Getcolumn( 5 ):cHeading := "IndexKey"
      :SetColSize( 5, 200 )
      :Getcolumn( 5 ):bDecode := {| x | Space( 3 ) + x }
   END

RETURN NIL

FUNCTION CurRecNo()

   LOCAL n
   n := CToN( hError[ br_log:aArray[ br_log:nAt ][ 1 ] ][ "SystemInformation" ][ "Current Work Area" ] )

RETURN n

FUNCTION GenError( nCh )

   LOCAL n, aKeys, aLogEntry := {}
   MEMVAR xVar

   switch nCh
   CASE 1 //
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n++
      END
      EXIT
   CASE 2
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n := 0
         n := nCh / n
      END
      EXIT
   CASE 3
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n := "abc" + nCh
      END
      EXIT
   CASE 4
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         A1->( RLock() )
         A2->F0 := 5
         A1->( dbUnlock() )
      END
      EXIT
   CASE 5
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n := {}
         n := n[ 1 ]
      END
      EXIT

   CASE 6
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n := xVar
      END
      EXIT

   CASE 7
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         n := A10->F0
      END
      EXIT
   CASE 8
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }

         DEFINE WINDOW Form_Err ;
               AT 0, 0 ;
               WIDTH 800 ;
               HEIGHT 600 ;
               TITLE "Windos Form_err exist !" ;
               MAIN ;
               NOSIZE

         END WINDOW
      END
      EXIT
   CASE 9
      BEGIN SEQUENCE WITH {| o | saveErr( o ) }
         SetProperty( "Text_100", "Form_err", "Value", "Control not found !" )
      END
      EXIT

   END


   IF ValType( br_log ) == 'O'
      aKeys := hb_HKeys( hError )
      AEval( aKeys, {| e | AAdd( aLogEntry, { e, hError[ e ][ "Message" ], hError[ e ][ "User" ], hError[ e ][ "Time From Start" ] } ) } )
      br_Log:SetArray( aLogEntry )
      br_log:Reset( .T. )
   END

RETURN NIL

FUNCTION saveErr( o )
   // msgdebug( __objGetValueList(o))
   ErrorLogJson( o )
   Break( o )

RETURN NIL
