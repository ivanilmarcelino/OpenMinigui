/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/
#define _HMG_OUTLOG
#include "hmg.ch"

REQUEST DBFCDX, DBFFPT

*----------------------------------------------------------------------------*
FUNCTION Main()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12
   LOCAL aBackColor := {215,166, 0}

   rddSetDefault( "DBFCDX" )

   SET CENTURY   ON
   SET DATE      GERMAN
   SET DELETED   ON
   SET EXCLUSIVE ON
   SET EPOCH TO  2000
   SET AUTOPEN   ON
   SET EXACT     ON
   SET SOFTSEEK  ON

   SET OOP       ON

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"

   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }    // for HMG_Alert()

   SET FONT TO cFont, nSize

   DEFINE FONT Normal    FONTNAME cFont SIZE nSize
   DEFINE FONT Bold      FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic    FONTNAME cFont SIZE nSize ITALIC
   DEFINE FONT Underline FONTNAME cFont SIZE nSize UNDERLINE

   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono" SIZE 16

   DEFINE WINDOW wMain CLIENTAREA 132,132 TITLE "DEMO" ;
          ICON      "1MAIN_ICO"                     ;
          MAIN      NOMAXIMIZE  NOMINIMIZE           ;
          BACKCOLOR  aBackColor                      ;
          ON INIT  _wPost(1)
          This.Cargo := oKeyData()

      (This.Object):Event( 1, {|ow| myAlert(), _wPost(99, ow) } )
      (This.Object):Event(99, {|ow| ow:Release()              } )

      DRAW ICON IN WINDOW wMain AT 0,0 PICTURE "2MAIN_64" WIDTH 128 HEIGHT 128 ;
                                       COLOR aBackColor

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

STATIC FUNCTION myAlert()
   LOCAL aBack
   LOCAL oWnd := This.Object
   LOCAL nOld := HMG_Alert_RowStart(10)

   SET MSGALERT BACKCOLOR TO GRAY STOREIN aBack
   SET MSGALERT FONTCOLOR TO YELLOW

   SET WINDOW THIS TO ThisWindow.Name

   This.Cargo:cLblValue1 := 'Lbl Value 1'
   This.Cargo:cLblValue2 := 'Lbl Value 2'
   This.Cargo:cGetValue1 := 'Get Value 1'+space(20)
   This.Cargo:cGetValue2 := 'Get Value 2'+space(20)
   This.Cargo:nGetModify := 0

   AlertOKCancel( "MessageBox with the Big Font and Icon Size.", "Test Alert", /*def_btn*/, ;
                  "Edit32", 32, { LGREEN, RED }, .T., {|| bInitAlert2GetBox( oWnd) } )

   SET WINDOW THIS TO

   IF This.Cargo:nGetModify > 0
      MsgBox("Get_1 = " + This.Cargo:cGetValue1 + CRLF + ;
             "Get_2 = " + This.Cargo:cGetValue2, "Press OK")
   ENDIF

   HMG_Alert_RowStart( nOld )
   SET MSGALERT BACKCOLOR TO aBack[1]
   SET MSGALERT FONTCOLOR TO aBack[2]

RETURN Nil

STATIC FUNCTION bInitAlert2GetBox( oWnd, cFnt )
   Local oCar  := oWnd:Cargo
   Local cFont := iif( empty(cFnt), "DlgFont", cFnt )
   Local nSize := GetFontParam( GetFontHandle(cFont) )[2]
   Local oDlu  := oDlu4Font(nSize)
   Local y := This.Say_01.Row + This.Say_01.Height + oDlu:Top * 2
   Local x := oDlu:Left
   Local w := oDlu:W1   // oDlu:W(1.5)  // oDlu:W2  // задаем размер по width для Label
   Local h := oDlu:H1

     This.Topmost := .F.
   @ y,x LABEL Lbl_1 WIDTH w HEIGHT h FONT cFont ;
                     VALUE oCar:cLblValue1 VCENTERALIGN
     x += This.Lbl_1.Width + oDlu:GapsWidth
   @ y,x GETBOX Get_1 WIDTH This.ClientWidth - x - oDlu:Left HEIGHT h ;
                      VALUE oCar:cGetValue1 FONT cFont ;
                      PICTURE "@K" /* NOBORDER */ ;
                      ON CHANGE   ( oCar:cGetValue1 := This.Value, oCar:nGetModify += 1 ) ;
                      ON GOTFOCUS ( DrawRR( RED ), _OnGotFocusSelect(This.Handle, This.Value) ) ;
                      ON LOSTFOCUS  DrawRR( .F. )

     y += This.Lbl_1.Height + oDlu:GapsHeight
     x := oDlu:Left
   @ y,x LABEL Lbl_2 WIDTH w HEIGHT h FONT cFont ;
                     VALUE oCar:cLblValue2 VCENTERALIGN
     x += This.Lbl_2.Width + oDlu:GapsWidth
   @ y,x GETBOX Get_2 WIDTH This.ClientWidth - x - oDlu:Left HEIGHT h ;
                      VALUE oCar:cGetValue2 FONT cFont ;
                      PICTURE "@K" /* NOBORDER */ ;
                      ON CHANGE   ( oCar:cGetValue2 := This.Value, oCar:nGetModify += 1 ) ;
                      ON GOTFOCUS ( DrawRR( RED ), _OnGotFocusSelect(This.Handle, This.Value) ) ;
                      ON LOSTFOCUS  DrawRR( .F. )

     y := This.Btn_01.Row + oDlu:Top * 2 + oDlu:GapsHeight
     This.Btn_01.Row := y
     This.Btn_02.Row := y
     This.Height := This.Height + oDlu:Top * 2
     /*
     This.Btn_01.Action := {|| MsgBox("Get_1 = "+ This.Get_1.Value+CRLF+ ;
                                      "Get_2 = "+ This.Get_2.Value, "Press OK"), ;
                               ThisWindow.Release }
     */
     This.Get_1.SetFocus

RETURN Nil

PROCEDURE DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 7
   DEFAULT nPen  := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE                ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
        ROUNDWIDTH  nCurve ROUNDHEIGHT nCurve   ;
        PENCOLOR    aColor PENWIDTH    nPen

RETURN

FUNCTION _OnGotFocusSelect( hGet, uValue )
    // #define EM_SETSEL  177
    If ValType(uValue) == "C"
       SendMessage( hGet, 177, 0, iif( Empty(uValue), -1, Len(Trim(uValue))) )
    ElseIf ValType(uValue) $ "ND"
       SendMessage( hGet, 177, 0, -1 )
    EndIf

RETURN Nil
