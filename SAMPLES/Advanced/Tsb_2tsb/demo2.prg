/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
*/

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

FUNCTION Main()

   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oBrw1, oBrw2, nY, nX, nH, nW, nG, oTsb1, oTsb2

   rddSetDefault( "DBFCDX" )

   SET OOP ON

   SET EPOCH TO 2000
   SET DATE TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET FONT TO cFont, nSize

   USE ( "CUSTOMER" ) ALIAS CUST1 NEW SHARED
   USE ( "CUSTOMER" ) ALIAS CUST2 NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 2 TBrowse" MAIN TOPMOST ;
         ON INIT ( This.Topmost := .F. ) ;
         ON RELEASE ( dbCloseAll() )

      This.Maximize

      nY := nX := nG := 20
      nW := This.ClientWidth - nG * 2
      nH := Int( This.ClientHeight / 2 ) - nG - nG / 2

      oTsb1 := oHmgData()
      oTsb1:lDrawSpecHd := .T.
      oTsb1:uSelector := .T.
      oTsb1:nBrw := 1

      @ 0, nX LABEL Label_1 VALUE "F1 - window covers the table" FONTCOLOR RED SIZE 8 AUTOSIZE TRANSPARENT
      oBrw1 := _TBrowse( oTsb1, "CUST1", "Brw_1", nY, nX, nW, nH )

      nY += nH + 1 + nG
      nH -= 1

      oTsb2 := oHmgData()
      oTsb2:lDrawSpecHd := .T.
      oTsb2:uSelector := .T.
      oTsb2:nBrw := 2

      @ nH + 22, nX LABEL Label_2 VALUE "F2 - window covers the table without a header" FONTCOLOR RED SIZE 8 AUTOSIZE TRANSPARENT
      oBrw2 := _TBrowse( oTsb2, "CUST2", "Brw_2", nY, nX, nW, nH )

      oBrw1:SetFocus()

      ON KEY TAB ACTION {| cf | cf := ThisWindow.FocusedControl, ;
         iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      ON KEY ESCAPE ACTION ( iif( oBrw1:IsEdit, oBrw1:SetFocus(), ;
         iif( oBrw2:IsEdit, oBrw2:SetFocus(), ;
         ThisWindow.Release ) ) )

      ( This.Object ):Event( 1, {| ow | myWin( ow, "Brw_1", .F. ) } )
      ( This.Object ):Event( 2, {| ow | myWin( ow, "Brw_2", .T. ) } )

      ON KEY F1 ACTION _wPost( 1 )
      ON KEY F2 ACTION _wPost( 2 )

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

FUNCTION myWin( oWnd, cBrw, lHead )

   LOCAL oBrw, nBrw, nW, nH, nRow, nCol, oTsb, oc1, nw1

   SET WINDOW THIS TO oWnd:Name

   oBrw := This.&(cBrw).OBJECT
   oTsb := oBrw:Cargo:oParam // parameters oTsb1 or oTsb2
   nBrw := oTsb:nBrw
   oc1 := oBrw:GetCellSize( 1, 1 )
   nw1 := oBrw:GetColumn( "SELECTOR" ):nWidth

   nRow := oc1:nRow
   nCol := oc1:nCol + 1
   nW := GetWindowWidth ( oBrw:hWnd ) - 2
   nH := GetWindowHeight( oBrw:hWnd ) - 2

   IF Empty( lHead )
      nRow -= ( oBrw:nHeightSpecHd + oBrw:nHeightHead )
      nRow += 1
      nCol += 1
      nW -= 1
   ELSE
      nCol += nw1
      nRow += 1
      nCol += 1
      nW -= ( nw1 + GetVScrollBarWidth() + 1 )
      nH -= ( oBrw:nHeightHead + oBrw:nHeightSpecHd + GetHScrollBarHeight() + 1 )
   ENDIF

   DEFINE WINDOW wZero AT nRow, nCol WIDTH nW HEIGHT nH MODAL NOCAPTION NOSIZE BACKCOLOR YELLOW

      This.Cargo := oHmgData()
      This.Cargo:oParent := oWnd
      This.Cargo:cBrw := cBrw
      This.Cargo:oBrw := oBrw
      This.Cargo:nBrw := nBrw
      This.Cargo:oTsb := oTsb

      @ nH / 2 - 20, 0 LABEL Label_0 VALUE "ESC - exit" WIDTH nW HEIGHT 40 FONTCOLOR RED SIZE 28 CENTERALIGN TRANSPARENT

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   ACTIVATE WINDOW wZero

   SET WINDOW THIS TO

   oBrw:SetFocus()

RETURN NIL
