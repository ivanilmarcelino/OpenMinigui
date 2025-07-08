/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
 *
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
 */

#include "minigui.ch"
#include "TSBrowse.ch"

STATIC oBrw
STATIC lGo := .F.
STATIC lUp := .T.

PROCEDURE MAIN

   DEFINE WINDOW Form_0 ;
         TITLE "TsBrowse last row sticking workaround" ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      DEFINE STATUSBAR
         STATUSITEM "Item 1" WIDTH 0 FONTCOLOR BLACK
         STATUSITEM "Item 2" WIDTH 230 FONTCOLOR BLACK
         DATE
         CLOCK
         KEYBOARD
      END STATUSBAR

   END WINDOW

   oBrw := CreateBrowse()

   DEFINE LABEL Label_3
      PARENT Form_0
      ROW 25
      COL 400
      WIDTH 300
      HEIGHT 16
      FONTNAME 'Arial'
      FONTSIZE 9
      FONTBOLD .F.
      VALUE ""
   END LABEL

   DEFINE LABEL Label_1
      PARENT Form_0
      ROW 5
      COL 5
      WIDTH 80
      HEIGHT 16
      FONTNAME 'Arial'
      FONTSIZE 9
      FONTBOLD .F.
      VALUE "nHeightCell"
   END LABEL

   DEFINE SPINNER Spinner_1
      PARENT Form_0
      ROW 22
      COL 2
      WIDTH 80
      HEIGHT 20
      RANGEMIN 1
      RANGEMAX 100
      VALUE oBrw:nHeightCell
      FONTNAME 'Arial'
      FONTSIZE 9
      TOOLTIP ''
      WRAP .T.
      ON CHANGE {|| oBrw:nHeightCell := This.Value, RefreshBrw( oBrw ) }
   END SPINNER

   DEFINE LABEL Label_2
      PARENT Form_0
      ROW 5
      COL 90
      WIDTH 80
      HEIGHT 16
      FONTNAME 'Arial'
      FONTSIZE 9
      FONTBOLD .F.
      VALUE "nHeightHead"
   END LABEL

   DEFINE SPINNER Spinner_2
      PARENT Form_0
      ROW 22
      COL 90
      WIDTH 80
      HEIGHT 20
      RANGEMIN 1
      RANGEMAX 100
      VALUE oBrw:nHeightHead
      FONTNAME 'Arial'
      FONTSIZE 9
      TOOLTIP ''
      WRAP .T.
      ON CHANGE {|| oBrw:nHeightHead := This.Value, RefreshBrw( oBrw ) }
   END SPINNER

   DEFINE BUTTONEX Button_Go
      PARENT Form_0
      ROW 12
      COL 180
      WIDTH 100
      HEIGHT 30
      ACTION {|| Go() }
      CAPTION "Start"
      PICTURE ""
      TABSTOP .F.
      TOOLTIP ""
      FONTNAME "Arial"
      FONTSIZE 8
      VERTICAL FALSE
      FLAT TRUE
   END BUTTONEX

   DEFINE BUTTONEX Button_Stop
      PARENT Form_0
      ROW 12
      COL 290
      WIDTH 100
      HEIGHT 30
      ACTION {|| lGo := .F. }
      CAPTION "Stop"
      PICTURE ""
      TABSTOP .F.
      TOOLTIP ""
      FONTNAME "Arial"
      FONTSIZE 8
      VERTICAL FALSE
      FLAT TRUE
   END BUTTONEX

   Form_0.ACTIVATE

RETURN

FUNCTION RefreshBrw( oBrw )
   IF oBrw:nRowPos > oBrw:nRowCount()
      oBrw:nRowPos := oBrw:nRowCount()
   END
   oBrw:lRePaint := .T. ;   oBrw:Display() ;   oBrw:ResetVScroll()

   oBrw:Refresh( .T. )
   SetProperty( "Form_0", "Label_3", "Value", "nHole = " + hb_ntoc( oBrw:SetNoHoles(, .F. ) ) + "   RowCount = " + hb_ntoc( oBrw:nRowCount() ) )

RETURN NIL


FUNCTION Go()

   lGo := .T.

   WHILE lGo
      IF lUp
         oBrw:GoDown()
         InkeyGUI()
         lUp := ! obrw:lHitBottom
      ELSE
         oBrw:GoUp()
         InkeyGUI()
         lUp := obrw:lHitTop
      END
      DoEvents()
   END

RETURN NIL

FUNCTION CreateBrowse()

   LOCAL i
   LOCAL aDatos := {}

   FOR i := 1 TO 200
      AAdd( aDatos, { i, RandStr( 30 ), Date() - i, if( i % 2 == 0, TRUE, FALSE ) } )
   NEXT

   IF IsControlDefined( "oBrw", "Form_0" )
      DoMethod( "Form_0", "oBrw", "Release" )
   END

   DEFINE TBROWSE oBrw AT 45, 2 ;
         OF Form_0 ;
         WIDTH App.ClientWidth - 4 ;
         HEIGHT App.ClientHeight - GetProperty( "Form_0", "StatusBar", "Height" ) - 47 ;
         GRID ;
         SELECTOR TRUE ;
         FONT "Arial" SIZE 12

      oBrw:SetArray( aDatos, .T. )
      oBrw:nWheelLines := 1
      oBrw:nClrLine := COLOR_GRID
      oBrw:lNoChangeOrd := TRUE
      oBrw:lCellBrw := TRUE
      oBrw:hBrush := CreateSolidBrush( 242, 245, 204 )

      oBrw:lNoVScroll := TRUE
      oBrw:nHeightCell := 24
      oBrw:nHeightHead := 26 + iif( IsSeven(), 1, 0 )
      oBrw:nLineStyle := 6

      // prepare for showing of Double cursor
      AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := oCol:lEdit := TRUE } )

      // assignment of column's names
      oBrw:aColumns[ 1 ]:cName := "NUMBER"
      oBrw:aColumns[ 2 ]:cName := "TEXT"
      oBrw:aColumns[ 3 ]:cName := "DATE"
      oBrw:aColumns[ 4 ]:cName := "LOGIC"

      // the reference to columns by names
      oBrw:SetColSize( "NUMBER", 100 )
      oBrw:SetColSize( "TEXT", 500 )
      oBrw:SetColSize( "DATE", 200 )

      // Checking the method nColumn()
      oBrw:SetColSize( oBrw:nColumn( "LOGIC" ), 300 )


      oBrw:GetColumn( 'NUMBER' ):nAlign := DT_CENTER
      oBrw:GetColumn( 'TEXT' ):nAlign := DT_LEFT
      oBrw:GetColumn( 'DATE' ):nAlign := DT_CENTER
      oBrw:GetColumn( 'LOGIC' ):nAlign := DT_CENTER


      oBrw:SetColor( { 1 }, { RGB( 0, 12, 120 ) } )
      oBrw:SetColor( { 2 }, { RGB( 242, 245, 204 ) } )
      oBrw:SetColor( { 5 }, { RGB( 0, 0, 0 ) } )
      oBrw:SetColor( { 6 }, { {| a, b, c | a := NIL, IF( c:nCell == b, { RGB( 66, 255, 236 ), RGB( 111, 183, 155 ) }, ;
         { RGB( 255, 255, 255 ), RGB( 200, 200, 200 ) } ) } } ) // cursor backcolor

   END TBROWSE

RETURN oBrw


FUNCTION RandStr( nLen )

   LOCAL cSet := "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
   LOCAL cPass := ""
   LOCAL i

   FOR i := 1 TO nLen
      cPass += SubStr( cSet, Random( 52 ), 1 )
   NEXT

RETURN cPass
