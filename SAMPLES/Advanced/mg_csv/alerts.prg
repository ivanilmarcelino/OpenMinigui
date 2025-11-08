/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2019-2024 Grigory Filatov <gfilatov@gmail.com>
 * Changed 22.02.24 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Changed 07.10.25
*/
#define _HMG_OUTLOG
#include "minigui.ch"
#include "i_winuser.ch"

*-----------------------------------------------------------------------------*
FUNCTION AlertYesNo ( Message, Title, RevertDefault, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21] }
   LOCAL lRet, nDefaultButton := 1
   DEFAULT bInit  := {|| bInitAlertSomeBtn() }

   IF HB_ISARRAY(Title)
      aOptions := Title[2]
      Title    := Title[1]
   ENDIF

   IF hb_defaultValue( RevertDefault, .F. )
      nDefaultButton := 2
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      Icon    := App.Cargo:cIcoYesNo64
      nSize   := App.Cargo:nIcoSize
      IF !IsArray(aColors)
         aColors := { LGREEN , {189,30,73} }
      ENDIF
   ENDIF

   lRet := _Alert( Message, aOptions, Title, , nDefaultButton, Icon, nSize, aColors, lTopMost, bInit ) == IDOK

RETURN lRet

*-----------------------------------------------------------------------------*
FUNCTION AlertYesNoCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21], '&' + _HMG_aABMLangButton [13] }
   DEFAULT bInit  := {|| bInitAlertSomeBtn() }

   IF HB_ISARRAY(Title)
      aOptions := Title[2]
      Title    := Title[1]
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      Icon   := App.Cargo:cIcoYesNo64
      nSize  := App.Cargo:nIcoSize
      IF !IsArray(aColors)
         aColors := { LGREEN , {189,30,73} }
      ENDIF
   ENDIF

   SWITCH _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. )

   CASE 1
      RETURN ( 1 )
   CASE 2
      RETURN ( 0 )

   END SWITCH

RETURN ( -1 )

*-----------------------------------------------------------------------------*
FUNCTION AlertRetryCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] } // P.D. July 3, 2021
   DEFAULT bInit  := {|| bInitAlertSomeBtn() }

   IF HB_ISARRAY(Title)
      aOptions := Title[2]
      Title    := Title[1]
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      Icon   := App.Cargo:cIcoYesNo64
      nSize  := App.Cargo:nIcoSize
      IF !IsArray(aColors)
         aColors := { LGREEN , {189,30,73} }
      ENDIF
   ENDIF

RETURN ( _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. ) == IDOK )

*-----------------------------------------------------------------------------*
FUNCTION AlertOkCancel ( Message, Title, nDefaultButton, Icon, nSize, aColors, lTopMost, bInit )
*-----------------------------------------------------------------------------*
   LOCAL aOptions := { _HMG_BRWLangButton [4], _HMG_BRWLangButton [3] }
   DEFAULT bInit  := {|| bInitAlertSomeBtn() }

   IF HB_ISARRAY(Title)
      aOptions := Title[2]
      Title    := Title[1]
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      Icon   := App.Cargo:cIcoYesNo64
      nSize  := App.Cargo:nIcoSize
      IF !IsArray(aColors)
         aColors := { LGREEN , {189,30,73} }
      ENDIF
   ENDIF

RETURN ( _Alert( Message, aOptions, Title, , hb_defaultValue( nDefaultButton, 1 ), Icon, nSize, aColors, lTopMost, bInit, .T. ) == IDOK )

*-----------------------------------------------------------------------------*
FUNCTION AlertExclamation ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec, aTmpColors, lRet, aBColor, aFColor, aButton

   IF HB_ISARRAY(Title)
      nWaitSec := Title[2]
      Title    := Title[1]
   ENDIF

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ENDIF

   IF Empty( lNoSound )
      PlayExclamation()
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      aBColor := { 238, 249, 142 }   // светло-жёлтый
      aFColor := BLACK  //{ 174, 134,  33 }
      IF !IsArray(aColors)
         aColors := { ORANGE }
      ENDIF
      IF Icon == NIL
         aButton := App.Cargo:aBtnCaptOk
         Icon    := App.Cargo:cIcoExclm64
         nSize   := App.Cargo:nIcoSize
      ENDIF
      IF !ISBLOCK(bInit)
         bInit   := {|| bInitAlert1Btn( RGB(174,134,33), CLR_WHITE, aButton) }
      ENDIF
   ENDIF

   aTmpColors := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   lRet := _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [10] ), , , Icon, nSize, aColors, lTopMost, bInit )

   // восстановить цвета
   SET MSGALERT FONTCOLOR TO aTmpColors[2]
   SET MSGALERT BACKCOLOR TO aTmpColors[1]

RETURN lRet

*-----------------------------------------------------------------------------*
FUNCTION AlertInfo ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec, aTmpColors, lRet, aBColor, aFColor, aButton, a, aTmp

   IF HB_ISARRAY(Title)
      a := ASize(AClone(Title), 3)
      Default a[2] := aButton
      nWaitSec := a[3]
      aButton  := a[2]
      Title    := a[1]
   ENDIF

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ENDIF

   IF Empty( lNoSound )
      PlayAsterisk()
   ENDIF

   IF HB_ISOBJECT(App.Cargo)
      aTmp    := _SetMsgAlertColors()
      // цвета заданные для:
      // SET MSGALERT BACKCOLOR  TO  {168,251,181}
      // SET MSGALERT FONTCOLOR  TO  { 0 ,  0, 0 }
      //aBColor := { 141, 179, 226 }   // светло-синий
      aBColor := aTmp[1]
      aFColor := aTmp[2]
      IF !IsArray(aColors)
         aColors := { BLUE }
      ENDIF
      IF Icon == NIL
         aButton := App.Cargo:aBtnCaptOk
         Icon    := App.Cargo:cIcoInfo64
         nSize   := App.Cargo:nIcoSize
      ENDIF
      // всегда делать 05.10.25
      aButton := App.Cargo:aBtnCaptOk
      IF !IsArray(aButton)
         aButton := NIL
      ENDIF
      IF !ISBLOCK(bInit)
         bInit   := {|| bInitAlert1Btn( RGB(1,119,214), CLR_WHITE, aButton) }
      ENDIF
   ENDIF

   aTmpColors := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   lRet := _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [11] ), ICON_INFORMATION, , Icon, nSize, aColors, lTopMost, bInit )

   // восстановить цвета
   SET MSGALERT FONTCOLOR TO aTmpColors[2]
   SET MSGALERT BACKCOLOR TO aTmpColors[1]

RETURN lRet

*-----------------------------------------------------------------------------*
FUNCTION AlertStop ( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
*-----------------------------------------------------------------------------*
   LOCAL nWaitSec, aTmpColors, lRet, aBColor, aFColor, aButton, a

   IF HB_ISARRAY(Title)
      a := ASize(AClone(Title), 3)
      Default a[2] := aButton
      nWaitSec := a[3]
      aButton  := a[2]
      Title    := a[1]
   ENDIF

   IF ISNUMERIC( Title )
      nWaitSec := Title
   ENDIF

   IF Empty( lNoSound )
      PlayHand()
   ENDIF

   IF HB_ISOBJECT(App.Cargo) //.AND. VALTYPE()
      aBColor := {248,209,211}      // светло-красный
      aFColor := RED
      IF !IsArray(aColors)
         aColors :=  { {189, 30, 73} }
      ENDIF
      IF Icon == NIL
         aButton := App.Cargo:aBtnCaptOk
         Icon    := App.Cargo:cIcoStop64
         nSize   := App.Cargo:nIcoSize
      ENDIF
      IF !ISBLOCK(bInit)
         bInit   := {|| bInitAlert1Btn(CLR_HRED, CLR_WHITE, aButton) }
      ENDIF
   ENDIF

   aTmpColors := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   lRet := _Alert( Message, nWaitSec, hb_defaultValue( Title, _HMG_MESSAGE [12] ), ICON_STOP, , Icon, nSize, aColors, lTopMost, bInit )

   // восстановить цвета
   SET MSGALERT FONTCOLOR TO aTmpColors[2]
   SET MSGALERT BACKCOLOR TO aTmpColors[1]

RETURN lRet

*-----------------------------------------------------------------------------*
STATIC FUNCTION _Alert ( cMsg, aOptions, cTitle, nType, nDefault, xIcon, nSize, aColors, lTopMost, bInit, lClosable )
*-----------------------------------------------------------------------------*
   __defaultNIL( @cMsg, "" )
   hb_default( @nDefault, 0 )

   IF ! Empty( nDefault )
      _HMG_ModalDialogReturn := nDefault
   ENDIF

   IF hb_defaultValue( lTopMost, .T. ) .AND. Empty( bInit )
      bInit := {|| This.TopMost := .T. }
   ENDIF

   IF AScan( _HMG_aFormType, 'A' ) == 0
      _HMG_MainWindowFirst := .F.
   ENDIF

   IF !IsNumeric(nSize) ;  nSize := 64   // 05.10.25
   ENDIF

RETURN HMG_Alert( cMsg, aOptions, cTitle, nType, xIcon, nSize, aColors, bInit, lClosable )

*-----------------------------------------------------------------------------*
FUNCTION bInitAlert1Btn(nClr1,nClr2,aSayBtn)   // одна кнопка
*-----------------------------------------------------------------------------*
   Local cWnd, cBtn, aCnl, cTyp, aGrOver, aGrFill, cText, nWBtn, nCol, hFont
   Local ow := ThisWindow.Object
   Local y, x, h, y1, x1
   Default nClr1 := CLR_RED, nClr2 := CLR_WHITE
   Default aSayBtn := {}

   h    := This.Btn_01.Handle
   y    := This.Btn_01.Row + 60
   x    := This.Btn_01.Col + 30
   y1   := GetWindowRow(ow:Handle)
   x1   := GetWindowCol(ow:Handle)
   cBtn := "Btn_01"
   cWnd := _HMG_THISFORMNAME
   aCnl := HMG_GetFormControls(cWnd)
   cTyp := GetControlType( cBtn, cWnd )

   IF "OBUT" $ cTyp         // ButtonEx
      aGrOver := { { 0.5, nClr2 , nClr1  }, { 0.5, nClr1 , nClr2 } }
      aGrFill := { { 0.5, nClr1 , nClr2  }, { 0.5, nClr2 , nClr1 } }

      SetProperty( cWnd, cBtn, "BackColor"   , aGrOver)
      SetProperty( cWnd, cBtn, "GradientFill", aGrFill)

      IF LEN(aSayBtn) > 0 .AND. IsChar(aSayBtn[1])
         cText := aSayBtn[1] + "AA"
         hFont := GetFontHandle('DlgFont')
         nWBtn := GetTextWidth(, cText, hFont)
         nCol  := GetProperty( cWnd, "ClientWidth")
         nCol  -= nWBtn + 10
         SetProperty( cWnd, cBtn, "Caption", aSayBtn[1] )
         SetProperty( cWnd, cBtn, "Col"    , nCol       )
         SetProperty( cWnd, cBtn, "Width"  , nWBtn      )
      ENDIF
      This.Btn_01.OnGotFocus  := {|| DrawRR( RED ) }
      This.Btn_01.OnLostFocus := {|| DrawRR( .F. ) }
      This.Btn_01.Fontcolor   := BLACK //YELLOW
      This.Btn_01.SetFocus
      DoEvents()
      y := GetWindowHeight(h) * 0.5
      x := GetWindowWidth (h) * 0.5
      HMG_SetMousePos( h, y, x )

   ENDIF

Return Nil

*----------------------------------------------------------------------------*
FUNCTION bInitAlertSomeBtn()                               // several buttons
*-----------------------------------------------------------------------------*
   Local cBtn, cTyp, aGrOver, aGrFill, nI, aGrO, aGrF, aBtn
   Local nBClr, aBClr, cWnd := _HMG_THISFORMNAME
   LOCAL aCnl := HMG_GetFormControls(cWnd, "OBUTTON")
   Local ow := ThisWindow.Object
   Local y, x, h, y1, x1
   //Local aBtnGrd := { HMG_RGB2n( GRAY ), CLR_WHITE }  // одинаковый цвет на кнопках

   h    := This.Btn_01.Handle
   y    := This.Btn_01.Row + 60
   x    := This.Btn_01.Col + 30
   y1   := GetWindowRow(ow:Handle)
   x1   := GetWindowCol(ow:Handle)
   cBtn := "Btn_01"
   cWnd := _HMG_THISFORMNAME
   aCnl := HMG_GetFormControls(cWnd)
   cTyp := GetControlType( cBtn, cWnd )

   nI   := Len(aCnl)
   aGrO := Array( nI )
   aGrF := Array( nI )
   aBtn := Array( nI )
   FOR nI := 1 TO LEN(aCnl)
       cBtn     := aCnl[nI]
       aBtn[nI] := cBtn
       aBClr    := GetProperty( cWnd, cBtn, "BackColor")
       nBClr    := HMG_RGB2n( aBClr )
       aGrO[nI] := {{ 0.5, CLR_WHITE, nBClr }, { 0.5, nBClr, CLR_WHITE }}
       aGrF[nI] := {{ 0.5, nBClr, CLR_WHITE }, { 0.5, CLR_WHITE, nBClr }}
   NEXT

   FOR EACH cBtn, aGrOver, aGrFill IN aBtn, aGrO, aGrF
       nI := hb_EnumIndex( cBtn )
       cTyp := ""
       IF _IsControlDefined( cBtn, cWnd ) ; cTyp := GetControlType( cBtn, cWnd )
       ENDIF
       IF ! Empty(cTyp) .and. "OBUT" $ cTyp     // ButtonEx
          aGrOver := aGrO[ nI ]
          aGrFill := aGrF[ nI ]
          SetProperty( cWnd, cBtn, "BackColor"  , aGrOver)
          SetProperty( cWnd, cBtn, "GradientFill", aGrFill)
          SetProperty( cWnd, cBtn, "OnGotFocus" , {|| DrawRR( RED ) } )
          SetProperty( cWnd, cBtn, "OnLostFocus", {|| DrawRR( .F. ) } )
       ENDIF
   NEXT

   // выделить первую кнопку
   This.Btn_01.Fontcolor   := BLACK //YELLOW
   This.Btn_01.SetFocus
   DoEvents()
   y := GetWindowHeight(h) * 0.5
   x := GetWindowWidth (h) * 0.5
   HMG_SetMousePos( h, y, x )

Return Nil

////////////////////////////////////////////////////////////////////
STATIC FUNCTION DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
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

RETURN NIL

////////////////////////////////////////////////////////////////////
STATIC FUNCTION HMG_SetMousePos( nHandle, y1, x1 )
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   Local y := GetWindowRow(nHandle)
   Local x := GetWindowCol(nHandle)
   Default y1 := 1, x1 := 1

   SetCursorPos( x + x1, y + y1 )

RETURN {c,r}

