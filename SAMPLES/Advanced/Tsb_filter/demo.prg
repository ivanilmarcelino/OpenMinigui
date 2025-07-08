/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
 *
 * Revised by Grigory Filatov and Sergej Kiselev
*/

#include "minigui.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

STATIC aFont := {}

MEMVAR oBrw_1

FUNCTION Main()

   LOCAL cDbf := hb_DirBase() + 'Test_1.dbf'

   SET CENTURY ON
   SET DELETED ON

   DEFINE FONT Font_1 FONTNAME "Times New Roman" SIZE 11
   DEFINE FONT Font_2 FONTNAME "Times New Roman" SIZE 10
   DEFINE FONT Font_3 FONTNAME "Times New Roman" SIZE 9 BOLD

   AAdd( aFont, GetFontHandle( "Font_1" ) )
   AAdd( aFont, GetFontHandle( "Font_2" ) )
   AAdd( aFont, GetFontHandle( "Font_3" ) )

   DEFINE WINDOW Form_0 ;
         WIDTH 620 ;
         HEIGHT 400 ;
         TITLE 'TsBrowse sample: Incremental search' ;
         ICON 'lupa.ico' ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE

      DEFINE LABEL NUL
         ROW 7
         COL 10
         WIDTH 80
         HEIGHT 16
         VALUE 'Search for :'
         FONTBOLD .T.
      END LABEL

      DEFINE TEXTBOX Text_1
         ROW 5
         COL 90
         WIDTH 345
         HEIGHT 21
         ON CHANGE {|| RefreshBrowse() }
      END TEXTBOX

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   DEFINE WINDOW Form_1 ;
         WIDTH 600 ;
         HEIGHT 40 ;
         CHILD ;
         NOSYSMENU ;
         NOCAPTION

      DEFINE LABEL NUL
         ROW iif( IsVista() .OR. IsSeven(), 5, 10 )
         COL 10
         WIDTH 580
         HEIGHT 24
         VALUE ''
         CENTERALIGN .T.
      END LABEL

   END WINDOW

   ScanSoft( cDbf )

   USE ( cDbf ) Alias ( cFileNoExt( cDbf ) ) SHARED READONLY NEW

   CreateBrowse( "oBrw_1", 'Form_0', 30, 2, Form_0.WIDTH - 10, Form_0.HEIGHT - 60, Alias() )

   Form_0.Text_1.Setfocus

   CENTER WINDOW Form_0
   ACTIVATE WINDOW ALL

RETURN NIL


FUNCTION CreateBrowse( cBrw, cParent, nRow, nCol, nWidth, nHeight, cAlias )

   PUBLIC &cBrw

   DEFINE TBROWSE &cBrw ;
         AT nRow, nCol ;
         ALIAS cAlias ;
         OF &cParent ;
         WIDTH nWidth ;
         HEIGHT nHeight ;
         COLORS { CLR_BLACK, CLR_WHITE } ;
         FONT "MS Sans Serif" ;
         SIZE 8

      :SetAppendMode( .F. )
      :SetDeleteMode( .F. )

      ADD COLUMN TO &cBrw DATA "" TITLE "" SIZE 16

      LoadFields( cBrw, cParent )

      :aColumns[ 2 ]:cHeading := "Date of" + CRLF + "installation"
      :aColumns[ 2 ]:nAlign := DT_CENTER

      :aColumns[ 3 ]:cHeading := "Application Name"
      :aColumns[ 3 ]:nAlign := DT_LEFT

      :aColumns[ 4 ]:cHeading := "Version"
      :aColumns[ 4 ]:nAlign := DT_RIGHT

      :lCellBrw := .F.
      :lNoHScroll := .T.
      :lNoMoveCols := .T.

      :hBmpCursor := LoadImage( "pointer.bmp" )

      :nClrLine := COLOR_GRID

      :SetColor( { 16 }, { RGB( 43, 149, 168 ) } )                                  // SuperHeader backcolor
      :SetColor( { 3 }, { RGB( 255, 255, 255 ) } )                                  // Header font color
      :SetColor( { 4 }, { {|| { RGB( 43, 149, 168 ), RGB( 0, 54, 94 ) } } } )       // Header backcolor
      :SetColor( { 17 }, { RGB( 255, 255, 255 ) } )                                 // Font color in SuperHeader
      :SetColor( { 6 }, { {|| { RGB( 255, 255, 74 ), RGB( 240, 240, 0 ) } } } )     // Cursor backcolor
      :SetColor( { 12 }, { {|| { RGB( 128, 128, 128 ), RGB( 250, 250, 250 ) } } } ) // Inactive cursor backcolor
      :SetColor( { 2 }, { {|| RGB( 230, 240, 255 ) } } )                            // Grid backcolor
      :SetColor( { 1 }, { {|| RGB( 0, 0, 0 ) } } )                                  // Text color in grid
      :SetColor( { 5 }, { {|| RGB( 0, 0, 255 ) } } )                                // Text color of cursor in grid
      :SetColor( { 11 }, { {|| RGB( 0, 0, 0 ) } } )                                 // Text color of inactive cursor in grid

      :nHeightCell += 6
      :nHeightHead += 6
      :nWheelLines := 1

      :ChangeFont( aFont[ 1 ], , 1 )
      :ChangeFont( aFont[ 3 ], 2, 1 )
      :ChangeFont( aFont[ 3 ], , 2 )

      :AdjColumns()

      :ResetVScroll( .T. )
      :SetNoHoles()

   END TBROWSE

RETURN NIL


STATIC FUNCTION RefreshBrowse()

   LOCAL cSeek := AllTrim( Form_0.Text_1.Value )

   IF ! Empty( cSeek )
      oBrw_1:FilterFTS( cSeek, .T. )
   ELSE
      oBrw_1:FilterFTS( Nil )
   ENDIF

RETURN NIL


STATIC FUNCTION ScanSoft( cDbf )

   LOCAL oWmi, oItem
   LOCAL cSW_Name, dSW_InstallDate, cSW_Version
   LOCAL aStr := {}, cAlias

   IF ! File( cDbf )
      AAdd( aStr, { 'F1', 'D', 8, 0 } )
      AAdd( aStr, { 'F2', 'C', 60, 0 } )
      AAdd( aStr, { 'F3', 'C', 20, 0 } )

      dbCreate( cDbf, aStr )

      cAlias := cFileNoExt( cDbf )
      USE ( cDbf ) Alias ( cAlias ) NEW

      Form_1.Label_1.VALUE := 'Reading the list of installed programs... Wait, please!'
      Form_1.CENTER
      Form_1.Show

      oWmi := WmiService()

      FOR EACH oItem IN oWmi:ExecQuery( "SELECT * FROM Win32_Product" )
         dSW_InstallDate := iif( ISSTRING( oItem:InstallDate ), SToD( Left( oItem:InstallDate, 8 ) ), Date() )
         cSW_Name := oItem:Caption
         cSW_Version := oItem:Version

         ( cAlias )->( dbAppend() )
         ( cAlias )->F1 := dSW_InstallDate
         IF HB_ISCHAR( cSW_Name )
            ( cAlias )->F2 := cSW_Name
         ENDIF
         IF HB_ISCHAR( cSW_Version )
            ( cAlias )->F3 := cSW_Version
         ENDIF
      NEXT

      ( cAlias )->( dbCloseArea() )

      Form_1.Hide
   ENDIF

RETURN NIL


FUNCTION WMIService()

   LOCAL oLocator
   STATIC oWmi

   IF oWmi == NIL
      oLocator := CreateObject( "wbemScripting.SwbemLocator" )
      oWmi := oLocator:ConnectServer()
   ENDIF

RETURN oWmi
