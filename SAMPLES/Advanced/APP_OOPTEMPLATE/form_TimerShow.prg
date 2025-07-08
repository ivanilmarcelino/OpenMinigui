/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/

#include "minigui.ch"
#include "metrocolor.ch"
//////////////////////////////////////////////////////////////////////////////
// Запуск окна для всех таймеров
// Launch a window for all timers
FUNCTION myWndTimerShow()
   LOCAL nY, nX, nW, nH, nG, aBClr, aBClr3, aBClr2, cForm, cLabel
   LOCAL cTitl, cTitl2, cIco, aFClr, cFont, nFSize, nHLbl
   LOCAL aBtnFC, nHIco, cBtnFnt, nBtnFSz, cTxt, aBtn, bAct, nWBtn, nHBtn

   nG      := GetTitleHeight()
   nHLbl   := App.Cargo:nDefFontSize
   nW      := 290
   nH      := nHLbl*2 + nG
   nY      := 0
   nX      := App.Cargo:aDisplayMode[1] - nW
   aBClr   := YELLOW
   aBClr2  := HMG_ColorWinActiveCaption()       // color of the Active window caption
   aBClr3  := COLOR_YELLOW_METRO
   aFClr   := YELLOW
   cForm   := App.Cargo:cTimer_Wnd              // "Form_Timer"
   cLabel  := App.Cargo:cTimer_Label            // "Lbl_9"
   cTitl   := "Timer-9 in the program"
   cTitl2  := "Таймер-9 в программе"
   cIco    := "iFind64x1"
   cFont   := "Comic Sans MS"
   nFSize  := App.Cargo:nDefFontSize - 2
   aBtnFC  := OLIVE
   cBtnFnt := cFont
   nBtnFSz := 5
   nWBtn   := nHBtn := nG - 1
   nHIco   := nG - 4          // высота иконки

   ? ProcNL(), cTitl, cLabel

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH          ;
      TITLE cTitl ICON cIco                                  ;
      WINDOWTYPE STANDARD TOPMOST                            ;
      NOMAXIMIZE NOSIZE NOSYSMENU NOCAPTION                  ;
      BACKCOLOR aBClr2                                       ;
      FONT cFont SIZE nFSize                                 ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT     {|| DoEvents(), _wPost(0) }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nX := 0
      This.Cargo := oHmgData()
      This.Cargo:cForm := cForm
      This.Cargo:cObjTimer := "Timer_10"

      @ 0, 0 LABEL Label_0 OF &cForm VALUE "" WIDTH nW HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN

      @ 0, nG+1 LABEL Label_1 OF &cForm VALUE cTitl WIDTH nW-nG*3-4 HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr TRANSPARENT CENTERALIGN VCENTERALIGN ;
        ACTION MoveActiveWindow() OnMouseHover RC_CURSOR( "hand32" )

      cTxt := ""
      aBtn := { "Button_1", cTxt, "iW_cas16", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| MsgDebug(This.Cargo), _wSend(10) }
      myDrawButtonGrad(nY, nX, nWBtn, nHBtn, aBtn, bAct, aBClr3)

      aBtn := { "Button_2", cTxt, "iW_Yellow", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ _wSend(12) }
      myDrawButtonGrad(nY, nW-nG*2-1, nWBtn, nHBtn, aBtn, bAct, aBClr3)

      aBtn := { "Button_3", cTxt, "iW_Green", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| _wSend(13) }
      myDrawButtonGrad(nY, nW-nG, nWBtn, nHBtn, aBtn, bAct, aBClr3)

      nY += nG

      @ nY, 5 LABEL &cLabel OF &cForm VALUE "program idle timer" WIDTH nW HEIGHT nHLbl*2 ;
        FONTCOLOR aFClr TRANSPARENT CENTERALIGN VCENTERALIGN

      (This.Object):Event( 0, {|ow| /*This.Minimize ,;*/ _logfile(.t.,"   ---[ :Event(0) ]---" + ow:Name, ProcNL() ) ,;
                                    This.Timer_10.Enabled := .T.  ,;  // включить таймер
                                    DoEvents()   })

      (This.Object):Event( 10, {|ow| ow:Setfocus("Label_0"), DoEvents()                 })
      (This.Object):Event( 12, {|ow| ow:Hide(), ow:Setfocus("Label_0"), DoEvents()      })
      (This.Object):Event( 13, {|ow| This.Minimize, ow:Setfocus("Label_0"), DoEvents()  })

      (This.Object):Event( 99, {|ow| InkeyGui(100) ,;
                                     _logfile(.t.,"   ---[ :Event(99) ]---" + ow:Name, ProcNL() ) ,;
                                     ow:Release() })
      ON KEY F1  ACTION NIL

      DEFINE TIMER Timer_10 INTERVAL 1000 ACTION {|| // таймер для показа счётчика времени
                                                     Local ow := ThisWindow.Object
                                                     Local cForm := ow:Cargo:cForm
                                                     SetProperty(cForm, "Timer_10" , "Enabled", .F.)   // отключить таймер
                                                     ShowTimer10(ow)
                                                     SetProperty(cForm, "Timer_10" , "Enabled", .T.)   // включить таймер
                                                     Return Nil
                                                  }
           This.Timer_10.Enabled := .F.  // отключить таймер до ON INIT

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm NOWAIT

RETURN .T.

///////////////////////////////////////////////////////////////
STATIC FUNCTION ShowTimer10(oWnd)
   Local nVal, cVal, oac := App.Cargo
   Local cWndTmr  := oac:cTimer_Wnd     // Form_Timer
   Local cLblTmr  := oac:cTimer_Label   // "Lbl_9"
   Local cTime, o := oac:oJobWait       //  oStat_JobWait   // сделаем общий доступ
   Local cFormMain := oac:cWinMain
   Local cFormThis := oWnd:Cargo:cForm

   //  lQuit := nMin >= o:nJobMinMax            // время простоя >= заданному
   //  cVal := hb_valtoexp({nSec, nMin, o:nJobMinMax, lQuit, o:nJobTimer})

   nVal  := GetProperty( cFormMain, "Timer_9", "Value" )
   cTime := SECTOTIME( nVal / 1000 - Seconds() - o:nJobMinMax )
   cTime := SECTOTIME( Seconds() - o:nJobMinMax )
   //cTime := SECTOTIME( Seconds() - o:nJobTimer ) // прошло
   // вывести значение в окно Form_Timer9
   cVal := "Timer_9 [ " + HB_NtoS(o:nJobMinMax) + "мин.] -> осталось: "
   cVal += cTime
   SetProperty(cWndTmr, cLblTmr, "Value", cVal)

RETURN NIL

///////////////////////////////////////////////////////////////
#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

STATIC Procedure MoveActiveWindow( hWnd )
    DEFAULT hWnd := GetActiveWindow()
    PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )
    RC_CURSOR( "Grabbed32" )
Return
