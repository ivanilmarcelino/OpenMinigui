/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Таблица в МиниГуи из массива / Копировать/вставка ячейки из клипборд
 * A table in a MiniGui from an array / Copy / paste cells from clipboard
*/

#include "minigui.ch"
#include "TSBrowse.ch"

PROCEDURE MAIN

   LOCAL aDatos, aArray, aHead, aSize, aFoot, aPict, aAlign, aName
   LOCAL oBrw, nY, nX, nW, nH, cTbrName, aFontHF

   SET DECIMALS TO 4
   SET DATE     TO GERMAN
   SET EPOCH    TO 2000
   SET CENTURY  ON
   SET EXACT    ON

   SET FONT TO 'Arial', 12

   DEFINE FONT DejaVuSM FONTNAME "DejaVu Sans Mono"   SIZE 11 BOLD ITALIC
   DEFINE FONT ComicSM  FONTNAME "Comic Sans MS"      SIZE 11 BOLD
   DEFINE FONT BoldDflt FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD

   DEFINE WINDOW test ;
      TITLE "SetArray for Сlipboard Demo - Left and Right Mouse button for column 2,4,10" ;
      MAIN ;
      NOMAXIMIZE NOSIZE

      DEFINE STATUSBAR
         STATUSITEM "0"
         STATUSITEM "Item 1" WIDTH 230
         STATUSITEM "Item 2" WIDTH 230
         STATUSITEM "Item 3" WIDTH 230
      END STATUSBAR

      nY := 1 + iif( IsVistaOrLater(), GetBorderWidth ()/2, 0 )
      nX := 1 + iif( IsVistaOrLater(), GetBorderHeight()/2, 0 )
      nW := test.WIDTH  - 2 * GetBorderWidth()
      nH := test.HEIGHT - 2 * GetBorderHeight() -    ;
            GetTitleHeight() - test.StatusBar.Height

      aDatos   := CreateDatos()

      aArray   := aDatos[ 1 ]
      aHead    := aDatos[ 2 ]
      aSize    := aDatos[ 3 ]
      aFoot    := aDatos[ 4 ]
      aPict    := aDatos[ 5 ]
      aAlign   := aDatos[ 6 ]
      aName    := aDatos[ 7 ]
      aFontHF  := { GetFontHandle("ComicSM"), GetFontHandle("BoldDflt") } // head, foot

      cTbrName := "oBrwTxt"
      DEFINE TBROWSE &cTbrName OBJ oBrw ;
           AT nY, nX            ;
           WIDTH  nW            ;
           HEIGHT nH            ;
           FONT "DejaVuSM"      ;
           GRID                 ;
           EDIT
  
      :SetArrayTo( aArray, aFontHF, aHead, aSize, aFoot, aPict, aAlign, aName )

        AEval( :aColumns, {|oc| oc:lFixLite          := .T., ;
                                oc:lEmptyValToChar   := .T., ;
                                oc:lOnGotFocusSelect := .T. } )
        mySetTsb  ( oBrw )
        myColorTsb( oBrw )
        mySet2Tsb ( oBrw )

      END TBROWSE  ON END {|obr| obr:SetNoHoles(), obr:SetFocus() }

      myClickTsb( oBrw )
  
   END WINDOW

   DoMethod( "test", "Activate" )

RETURN 

STATIC FUNCTION myClickTsb( oBrw )
   // left and right mouse button for column 2,4,10 of the table
   WITH OBJECT oBrw
      :aColumns[ 2]:bLClicked := {|nrp,ncp,nat,obr| MyCellClick(1,obr,nrp,ncp,nat) }
      :aColumns[ 2]:bRClicked := {|nrp,ncp,nat,obr| MyCellClick(2,obr,nrp,ncp,nat) }
      :aColumns[ 4]:bLClicked := {|nrp,ncp,nat,obr| MyCellClick(1,obr,nrp,ncp,nat) }
      :aColumns[ 4]:bRClicked := {|nrp,ncp,nat,obr| MyCellClick(2,obr,nrp,ncp,nat) }
      :aColumns[10]:bLClicked := {|nrp,ncp,nat,obr| MyCellClick(1,obr,nrp,ncp,nat) }
      :aColumns[10]:bRClicked := {|nrp,ncp,nat,obr| MyCellClick(2,obr,nrp,ncp,nat) }
   END WITH
RETURN Nil

FUNCTION mySetTsb( oBrw )
   WITH OBJECT oBrw
      :nHeightCell  += 6
      :nColOrder    := 0
      :lNoChangeOrd := .F.
      :nWheelLines  := 1
      :lNoGrayBar   := .F.
      :lNoLiteBar   := .F.
      :lNoResetPos  := .F.
      :lNoHScroll   := .T.
      :lNoPopUp     := .T.
   END WITH
RETURN Nil

FUNCTION mySet2Tsb( oBrw )
   LOCAL nLen, cBrw, nTsb, nCol

   WITH OBJECT oBrw
      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth
      nCol := Len( :aColumns )
      nLen := :GetAllColsWidth() + nCol + GetVScrollBarWidth()
      IF nLen > nTsb
         :lAdjColumn  := .T.
         :lNoHScroll  := .F.
         :lMoreFields := ( nCol > 45 )
      ELSE
         :AdjColumns() 
      ENDIF
   END WITH
RETURN Nil

FUNCTION myColorTsb( oBrw )
   WITH OBJECT oBrw
      :nClrLine              := RGB(180,180,180) // COLOR_GRID
      :SetColor( { 11 }, { { || RGB(0,0,0) } } )
      :SetColor( {  2 }, { { || RGB(255,255,240) } } )
      :SetColor( {  5 }, { { || RGB(0,0,0) } } )
      :SetColor( {  6 }, { { |a,b,c| a:=b, iif( c:nCell == b, -CLR_HRED, -RGB(128,225,225) ) } } )
      :SetColor( { 12 }, { { |a,b,c| a:=b, iif( c:nCell == b, -CLR_HRED, -RGB(128,225,225) ) } } ) 
   END WITH
RETURN Nil

STATIC FUNCTION MyCellClick( nClick, oBrw, nRowPix, nColPix )  
   LOCAL cNam := {'Left mouse', 'Right mouse'}[ nClick ]
   LOCAL nRow, nCol, nLine
   LOCAL cCel, cTyp, cType, xVal, hFont1, hFont2, hFont3
   LOCAL nY, nX, cForm

   DO EVENTS

   WITH OBJECT oBrw
   :DrawSelect() ; DO EVENTS
   cForm := :cParentWnd
   nLine := :nAt                           // table line number
   nRow  := :GetTxtRow(nRowPix)            // table row cursor number
   nCol  := Max( :nAtCol(nColPix), 1 )     // cursor column number in table
   xVal  := :aArray[ nLine ][nCol]         // real value
   cTyp  :=  ValType( xVal )               // real valtype
   cType :=  ValType( :GetValue(nCol) )    // virtual valtype
   END WITH

   cCel  := "Cell position row/column: " + hb_ntos(nLine) + '/' + hb_ntos(nCol) + CRLF 
   cCel  += "Type Cell: " + cType + " (" + cTyp + ")" + CRLF 
   cCel  += "Get Cell value: [" + cValToChar(xVal) + "]" + CRLF + cNam

   nX := _HMG_MouseCol
   nY := _HMG_MouseRow

   hFont1 := GetFontHandle( "DejaVuSM" )
   hFont2 := GetFontHandle( "ComicSM " )
   hFont3 := GetFontHandle( "BoldDflt" )

   SET MENUSTYLE EXTENDED     // switch menu style to advanced
   SetMenuBitmapHeight( 32 )  // set icon size 32x32

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  StrTran( cCel, CRLF, " " )      ACTION  {|| MsgInfo(cCel), oBrw:SetFocus() }  FONT hFont1
       SEPARATOR                             
       MENUITEM  "Clear field"                   ACTION  {|| oBrw:SetValue(nCol , "" ) , oBrw:DrawSelect(), oBrw:SetFocus() }  FONT hFont2
       SEPARATOR                             
       MENUITEM  "Copy field to clipboard"       ACTION  {|| System.Clipboard := oBrw:GetValue(nCol), oBrw:SetFocus() } FONT hFont2
       SEPARATOR                             
       MENUITEM  "Copy to field from clipboard"  ACTION  {|| oBrw:SetValue(nCol, System.Clipboard), oBrw:DrawSelect(), oBrw:SetFocus() }  FONT hFont2
       SEPARATOR                             
       MENUITEM  "Exit"                          ACTION  {|| oBrw:SetFocus() } FONT hFont3
   END MENU 

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU     
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   RETURN Nil

* ======================================================================
STATIC FUNCTION CreateDatos()

   LOCAL i, k := 1000, aDatos, aHead, aSize, aFoot, aPict, aAlign, aName

   aDatos := Array( k )
   FOR i := 1 TO k
      aDatos[ i ] := { ;                              // 1
         i, ;                                         // 2
         "Text " + ntoc( i ) + "_" + ntoc( i ) + SPACE(10) + ".", ;      // 3
         Date() + i, ;                                // 4
         PadR( "Test line - " + ntoc( i ), 20 ), ;    // 5
         Round( ( 10000 -i ) * i / 3, 2 ), ;          // 6
         100.00 * i, ;                                // 7
         0.12, ;                                      // 8
         Round( 100.00 * i * 0.12, 2 ), ;             // 9
         Round( 1234567.00 / i, 3 ), ;                // 10
         PadR( "Line " + StrZero( i, 5 ), 20 )+ ".", ;// 11
         Date(), ;                                    // 12
         Time(), ;                                    // 13
         i % 2 == 0 }                                 // 14
   NEXT

   aHead  := AClone( aDatos[ 1 ] )
   AEval( aHead, {| x, n| x:=nil, aHead[ n ] := "Head" + hb_ntos( n ) + ;
      iif( n % 2 == 0, CRLF + "SubHead" + hb_ntos( n ), "" ) } )

   aFoot  := Array( Len( aDatos[ 1 ] ) )
   AEval( aFoot, {| x, n|  x:=nil, aFoot[ n ] := n } )

   aPict     := Array( Len( aDatos[ 1 ] ) )   // можно не задавать, формируются
   aPict[ 10 ] := "99999999999.999"           // автоматом для C,N по мах значению

   aSize     := Array( Len( aDatos[ 1 ] ) )   // можно не задавать, формируются
   aSize[ 10 ] := aPict[ 10 ]                 // автоматом по мах значению в колонке

   aAlign    := Array( Len( aDatos[ 1 ] ) )   // тип поля C   - DT_LEFT
   aAlign[ 2 ] := DT_CENTER                   // D,L - DT_CENTER
                                              // N   - DT_RIGHT

   aName     := Array( Len( aDatos[ 1 ] ) )
   AEval( aName, {| x, n| x:=nil, aName[ n ] := "MyName_" + hb_ntos( n ) } )

RETURN { aDatos, aHead, aSize, aFoot, aPict, aAlign, aName }
