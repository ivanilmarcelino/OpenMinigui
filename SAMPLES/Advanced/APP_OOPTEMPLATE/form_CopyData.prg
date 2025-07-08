/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/

#include "minigui.ch"
#include "metrocolor.ch"
//////////////////////////////////////////////////////////////////////////////
// Запуск окна сообщений WM_COPYDATA
// Run message box WM_COPYDATA
FUNCTION myWndCopyData(nTime)
   LOCAL nY, nX, nW, nH, nG, aBClr, aBClr3, aBClr2, cForm, cMemo
   LOCAL cTitl, cTitl2, cIco, cLog, tTime, aFClr, cFont, nFSize
   LOCAL aBtnFC, nHIco, cBtnFnt, nBtnFSz, cTxt, aBtn, bAct, nWBtn, nHBtn
   DEFAULT nTime := 0   // не использую

   nY      := nX := 0
   nW      := 400
   nH      := App.Cargo:aDisplayMode[2]         //System.ClientHeight
   nH      -= GetTaskBarHeight()                // высота Панели задач Desktop
   aBClr   := YELLOW
   aBClr2  := HMG_ColorWinActiveCaption()       // color of the Active window caption
   aBClr3  := COLOR_YELLOW_METRO
   aFClr   := MAROON
   cForm   := App.Cargo:cCopyData_Wnd           //"Form_ListCD"
   cMemo   := App.Cargo:cCopyData_Memo          //"Edit_Memo"
   cTitl   := "Message log WM_COPYDATA"
   cTitl2  := "Журнал сообщений WM_COPYDATA"
   cLog    := App.Cargo:cCopyDataLog            // _copydata.log
   cIco    := "iFind64x1"
   tTime   := App.Cargo:tStart
   cFont   := "Comic Sans MS"
   nFSize  := App.Cargo:nDefFontSize - 3
   aBtnFC  := OLIVE
   nG      := GetTitleHeight()
   cBtnFnt := cFont
   nBtnFSz := 5
   nWBtn   := nHBtn := nG - 1
   nHIco   := nG - 4          // высота иконки

   ? ProcNL(), cTitl, nTime

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH          ;
      TITLE cTitl ICON cIco                                  ;
      WINDOWTYPE STANDARD                                    ;
      NOMAXIMIZE NOSIZE NOSYSMENU NOCAPTION                  ;
      BACKCOLOR aBClr                                        ;
      FONT cFont SIZE nFSize                                 ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT     {|| DoEvents(), _wPost(0) }

      nW := This.ClientWidth
      nH := This.ClientHeight

      This.Cargo := oHmgData()
      This.Cargo:cForm := cForm
      This.Cargo:cMemo := cMemo
      This.Cargo:cLog  := cLog
      This.Cargo:cMsg  := REPL("=",10) + " " + HB_TTOC( tTime ) + " " + REPL("=",10)

      @ 0, 0 LABEL Label_0 OF &cForm VALUE cTitl WIDTH nW HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN

      @ 0, nG+1 LABEL Label_1 OF &cForm VALUE cTitl WIDTH nW-nG*3-4 HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN ;
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

      @ nY+10, nX+10 EDITBOX &cMemo WIDTH nW-20 HEIGHT nH-nG*2-20 VALUE "" ;
         BACKCOLOR SILVER FONTCOLOR aFClr MAXLENGTH 1200 NOHSCROLL READONLY

      @ nH-nG, 0 LABEL Label_Bottom OF &cForm VALUE cTitl2 WIDTH nW HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN

      (This.Object):Event( 0, {|ow| This.Minimize ,;
                                    _logfile(.t.,"   ---[ :Event(0) ]---" + ow:Name, ProcNL() ) ,;
                                    _wPost(2,ow:Name), DoEvents()          })

      (This.Object):Event( 2, {|ow| // создать журнал заново
                                    Local cMsg := ow:Cargo:cMsg + CRLF + CRLF
       //AADD( aRun, { "Загрузка путей/настроек программы из *.ini-файла"   , "myLoadIni()"     , 100 } )
       // выше этой функции, можно использовать параметр в ини-файле: lShow_COPYDATA = ? ; Показ окна WM_COPYDATA
                                    IF ! App.Cargo:WM_CD_lShow
                                       ow:Hide()
                                       // DoMethod( App.Cargo:cCopyData_Wnd, "Hide" )
                                    ENDIF
                                    DELETEFILE(ow:Cargo:cLog)
                                    STRFILE( cMsg, ow:Cargo:cLog, .F. )
                                    SetProperty(ow:Name, ow:Cargo:cMemo, "Value", cMsg)
                                    DO EVENTS
                                    wApi_Sleep(100)
                                    Return Nil
                               })

      (This.Object):Event( 10, {|ow| ow:Setfocus("Label_0"), DoEvents()                 })
      (This.Object):Event( 12, {|ow| ow:Hide(), ow:Setfocus("Label_0"), DoEvents()      })
      (This.Object):Event( 13, {|ow| This.Minimize, ow:Setfocus("Label_0"), DoEvents()  })

      (This.Object):Event(99, {|ow| InkeyGui(100) ,;
                                    _logfile(.t.,"   ---[ :Event(99) ]---" + ow:Name, ProcNL() ) ,;
                                    ow:Release() })
      ON KEY F1  ACTION NIL

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm NOWAIT

RETURN .T.

///////////////////////////////////////////////////////////////
#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

STATIC Procedure MoveActiveWindow( hWnd )
    DEFAULT hWnd := GetActiveWindow()
    PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )
    RC_CURSOR( "Grabbed32" )
Return

