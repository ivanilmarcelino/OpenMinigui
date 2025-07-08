/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/

#include "minigui.ch"
#include "i_winuser.ch"

//////////////////////////////////////////////////////////////////////////////
// Запуск окна отладки / Launch the debug window
FUNCTION WinDebugLog(cFile)
   LOCAL nY, nX, nW, nH, nG, aBClr, aBClr3, aBClr2, cForm, cMemo, aBClrM
   LOCAL cTitl, cTitl2, cIco, aFClr, cFont, nFSize, cFile2, cCnfg, oCfg, cFMd5
   LOCAL aBtnFC, nHIco, cBtnFnt, nBtnFSz, cTxt, aBtn, bAct, nWBtn, nHBtn

   ? ProcNL(), cFile
   nY      := nX := 0
   nW      := App.Cargo:aDisplayMode[1]
   nH      := App.Cargo:aDisplayMode[2]         // System.ClientHeight
   nH      -= GetTaskBarHeight()                // высота Панели задач Desktop
   aBClr   := YELLOW
   aBClr2  := HMG_ColorWinActiveCaption()       // color of the Active window caption
   aBClr3  := {231,178, 30}                     // серо-желтый
   aBClrM  := {238,244,167}
   aFClr   := MAROON
   cForm   := AtRepl( ".", cFileNoPath(cFile), "_" )
   cMemo   := "Edit_Memo"
   cTitl   := "Debug log " + cFileNoPath(cFile)
   cTitl2  := "Журнал отладки " + cFile
   cFile2  := App.Exename + " + " + cFileNoPath(cFile)
   cFMd5   := "MG_" + HB_MD5(cFile2) + ".ini"
   // запись ини-файла C:\Users\XXXX\AppData\Local\Temp\*.cnfg
   cCnfg   := GetUserTempFolder() + "\" + cFMd5
   //cCnfg := GetStartUpFolder() + "\" + cFMd5
   cIco    := "iDebug64"
   cFont   := "DejaVu Sans Mono"  //"Comic Sans MS"
   nFSize  := App.Cargo:nFontSize - 3
   aBtnFC  := OLIVE
   nG      := GetTitleHeight()
   cBtnFnt := cFont
   nBtnFSz := 5
   nWBtn   := nHBtn := nG - 1
   nHIco   := nG - 4          // высота иконки

   IF App.Cargo:aWinLog == NIL
      App.Cargo:aWinLog  := {}
   ENDIF
   // добавить хендл окна в список журнала
   AADD( App.Cargo:aWinLog, { cForm, cMemo, cFile, cCnfg } )

   IF LEN(App.Cargo:aWinLog) > 1
      cFont  := "Comic Sans MS"
      aBClr  := {  3, 84, 68}
      aBClr3 := {  6,175,143}
      aBClrM := {171,249,233}
   ENDIF

   ?? "|", cTitl, "cForm=", cForm
   ? "   Start -", App.ExeName
   ? "   cFile2=",cFile2
   ? "   ParamCnfg -", cCnfg
   // читаем параметры из ини-файла C:\Users\XXXX\AppData\Local\Temp\*.cnfg
   oCfg   := myThisWinCnfg(cCnfg,cFile2,aBClr,aBClr3,aBClrM)
   nY     := oCfg:MAIN:aWin[1]
   nX     := oCfg:MAIN:aWin[2]
   nW     := oCfg:MAIN:aWin[3]
   nH     := oCfg:MAIN:aWin[4]
   aBClr  := oCfg:MAIN:aBClr
   aBClr3 := oCfg:MAIN:aBClr3
   aBClrM := oCfg:MAIN:aBClrM

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH          ;
      TITLE cTitl ICON cIco                                  ;
      WINDOWTYPE STANDARD NOSYSMENU NOCAPTION /*NOSIZE*/     ;
      ON MAXIMIZE ( ResizeForm( This.Cargo ) )               ;
      ON SIZE     ( ResizeForm( This.Cargo ) )               ;
      BACKCOLOR aBClr                                        ;
      FONT cFont SIZE nFSize                                 ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT     {|| DoEvents(), _wPost(0) }                ;
      ON RELEASE  {|| _wSend(90) }                           ;
      ON MOUSECLICK MoveActiveWindow()

      nW := This.ClientWidth
      nH := This.ClientHeight

      This.Cargo := oHmgData()
      This.Cargo:cForm    := cForm
      This.Cargo:cMemo    := cMemo
      This.Cargo:cForm    := cForm
      This.Cargo:aBClr    := This.Backcolor
      This.Cargo:aBClr3   := aBClr3
      This.Cargo:aBClrm   := aBClrM
      This.Cargo:nG       := nG
      This.Cargo:cCnfg    := cCnfg
      This.Cargo:oCfg     := oCfg  // настройки окна
      This.Cargo:ahIcoDel := {}    // для удаления хендлов иконок

      @ 0, 0 LABEL Label_0 OF &cForm VALUE cTitl WIDTH nW HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN
      This.Cargo:cObj0 := "Label_0"

      @ 0, nG+1 LABEL Label_1 OF &cForm VALUE cTitl WIDTH nW-nG*3-4 HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN ;
        ACTION MoveActiveWindow() OnMouseHover RC_CURSOR( "hand32" )
      This.Cargo:cObj1 := "Label_1"

      nY   := nX := 0
      cTxt := ""
      aBtn := { "Button_1", cTxt, "iW_cas16", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| MsgDebug(This.Cargo), _wSend(10) }
      myDrawButtonGrad(nY, nX, nWBtn, nHBtn, aBtn, bAct, aBClr3, This.Cargo)
      This.Cargo:cBtn1 := "Button_1"

      aBtn := { "Button_2", cTxt, "iW_Yellow", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ _wSend(12) }
      myDrawButtonGrad(nY, nW-nG*2-1, nWBtn, nHBtn, aBtn, bAct, aBClr3, This.Cargo)
      This.Cargo:cBtn2 := "Button_2"

      aBtn := { "Button_3", cTxt, "iW_Green", "iW_Invert", nHIco, aBtnFC, YELLOW, cBtnFnt, nBtnFSz, .T. }
      bAct := {|| _wSend(13) }
      myDrawButtonGrad(nY, nW-nG, nWBtn, nHBtn, aBtn, bAct, aBClr3, This.Cargo)
      This.Cargo:cBtn3 := "Button_3"

      nY += nG

      @ nY+10, nX+10 EDITBOX &cMemo WIDTH nW-20 HEIGHT nH-nG*2-20 VALUE "" ;
         BACKCOLOR aBClrM FONTCOLOR aFClr MAXLENGTH 1200 NOHSCROLL READONLY

      @ nH-nG, 0 LABEL Label_Bottom OF &cForm VALUE cTitl2 WIDTH nW HEIGHT nG ;
        SIZE 10 FONTCOLOR aFClr BACKCOLOR aBClr3 CENTERALIGN VCENTERALIGN
      This.Cargo:cObj3 := "Label_Bottom"

      (This.Object):Event( 0, {|ow| This.Minimize ,;
                                    _logfile(.t.,"   ---[ :Event(0) ]---" + ow:Name, ProcNL() ) ,;
                                    _wPost(2,ow:Name), DoEvents()          })

      (This.Object):Event( 2, {|ow| // обновить содержимое окна
                                    Local n := LEN(App.Cargo:aWinLog)
                                    UPDATELOG(n)
                                    ow:Setfocus("Label_0")
                                    Return Nil
                               })

      (This.Object):Event( 10, {|ow| ow:Setfocus("Label_0"), DoEvents()                 })
      (This.Object):Event( 12, {|ow| ow:Hide(), ow:Setfocus("Label_0"), DoEvents()      })
      (This.Object):Event( 13, {|ow| This.Minimize, ow:Setfocus("Label_0"), DoEvents()  })
      (This.Object):Event( 90, {|ow,ky| // Release
                                        Local aWin, ah := ow:Cargo:ahIcoDel
                                        Local oCfg := ow:Cargo:oCfg
                                        If IsArray(ah)
                                           AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                                        Endif
                                        ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                                        ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                                        // сохранить размеры окна
                                        aWin := { ow:Row, ow:Col, ow:Width, ow:Height }
                                        oCfg:MAIN:aWin := {ow:Row, ow:Col, ow:Width, ow:Height}
                                        oCfg:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
                                        oCfg:Write()        // НЕ UTF8, т.е. нет BOM на выходе
                                        Return Nil
                           })
      (This.Object):Event( 99, {|ow| ow:Release() })

      ON KEY F1  ACTION NIL

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm NOWAIT

RETURN .T.

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm( owc )
   Local nG, nW, nH, cObjMemo, cObj0, cObj1, cObj3, nHLbl, cBtn

   nW := This.ClientWidth
   nH := This.ClientHeight
   nG := owc:nG
   cObjMemo := owc:cMemo

   cObj0    := owc:cObj0
   nHLbl    := This.&(cObj0).Height
   This.&(cObj0).Width  := nW

   cObj1    := owc:cObj1
   This.&(cObj1).Width  := nW - nG*3 - 4

   cObj3    := owc:cObj3
   This.&(cObj3).Row    := nH - nHLbl
   This.&(cObj3).Width  := nW

   This.&(cObjMemo).Row    := nHLbl + nG
   This.&(cObjMemo).Col    := nG
   This.&(cObjMemo).Width  := nW - nG * 2
   This.&(cObjMemo).Height := nH - nG*3 - nHLbl

   cBtn := owc:cBtn1
   This.&(cBtn).Row := 0
   This.&(cBtn).Col := 0

   cBtn := owc:cBtn2
   This.&(cBtn).Row := 0
   This.&(cBtn).Col := nW-nG*2-1

   cBtn := owc:cBtn3
   This.&(cBtn).Row := 0
   This.&(cBtn).Col  := nW-nG

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////
#define HTCAPTION          2
//#define WM_NCLBUTTONDOWN   161
STATIC Procedure MoveActiveWindow( hWnd )
    DEFAULT hWnd := GetActiveWindow()
    PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )
    RC_CURSOR( "Grabbed32" )
Return

///////////////////////////////////////////////////////////////
STATIC FUNCTION myThisWinCnfg(cCnfg,cFile2,aBClr,aBClr3,aBClrM)
   LOCAL aWin, lCnf, oCfg //:= oHmgData()

   lCnf := hb_FileExists(cCnfg)
   oCfg := TIniData():New(cCnfg, .T.):Read()
   // доступ к ини-файлу везде в программе - oWnd:Cargo:oCfg

   Default oCfg:INFO := oHmgData()
   Default oCfg:INFO:Developed_in   := MiniGUIVersion()
   Default oCfg:INFO:xBase_compiler := Version()
   Default oCfg:INFO:C_compiler     := Hb_Compiler()
   Default oCfg:INFO:Programm       := App.Cargo:cTitle
   Default oCfg:INFO:ProgVers       := App.Cargo:cVersion
   Default oCfg:INFO:Avtor          := App.Cargo:cAvtor
   Default oCfg:INFO:Email          := App.Cargo:cEmail

   Default oCfg:MAIN := oHmgData()
   Default oCfg:MAIN:Files   := cFile2
   Default oCfg:MAIN:aBClr   := aBClr
   Default oCfg:MAIN:aBClr3  := aBClr3
   Default oCfg:MAIN:aBClrM  := aBClrM
   Default oCfg:MAIN:aWin    := {0, 0, 0, 0}

   IF !lCnf
      // если нет файла, то создадим его
      oCfg:cCommentBegin  := " Modify: " + hb_TtoC( hb_DateTime() )
      oCfg:Write()  // НЕ UTF8, т.е. нет BOM на выходе
   ENDIF

   // список переменных в oCfg
   _o2log(oCfg, 17, ProcNL() + "  oCfg: => ", .T. )

   // считать параметры из ини-файла
   aWin := oCfg:MAIN:aWin

   IF IsArray(aWin)
      ? ProcNL(), "oCfg:MAIN:aWin=", HB_ValToExp(aWin)
      IF aWin[1] < 0 .OR. aWin[2] < 0
         // это скрытие окна
         oCfg:MAIN:aWin := {0, 0, App.Cargo:aDisplayMode[1], App.Cargo:aDisplayMode[2]}
      ELSEIF aWin[3] <= 0 .OR. aWin[4] <= 0
         // это сбой координат окна
         oCfg:MAIN:aWin := {0, 0, App.Cargo:aDisplayMode[1], App.Cargo:aDisplayMode[2]}
      ENDIF
      // проверка на размер тек.экрана
      IF aWin[3] > App.Cargo:aDisplayMode[1]
         aWin[3] := App.Cargo:aDisplayMode[1]
      ENDIF
      IF aWin[4] > App.Cargo:aDisplayMode[2]
         aWin[4] := App.Cargo:aDisplayMode[2]
      ENDIF
      ? ProcNL(), "oCfg:MAIN:aWin=", HB_ValToExp(aWin)
   ENDIF

RETURN oCfg

///////////////////////////////////////////////////////////////
FUNCTION UPDATELOG(n)
   LOCAL aWinLog := App.Cargo:aWinLog
   LOCAL aDim    := aWinLog[n], cMsg, cObj, cForm
   // aDim := { cForm, cMemo, cFile)
   cForm := aDim[1]
   cObj  := aDim[2]

   DO EVENTS
   wApi_Sleep(100)

   cMsg := FILESTR(aDim[3])

   IF _IsWindowDefined(cForm)
      SetProperty(cForm, cObj, "Value", cMsg)
      // в конец файла
      SendMessage( GetControlHandle(cObj, cForm) , WM_VSCROLL , SB_BOTTOM , 0 )
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aColor, owc)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, aFClr2, aFClr1
   LOCAL cIco1x2, cIco1x1, nSize, aGrFill, aGrOver, hIco1, hIco2

   cObj    := aBtn[1]
   cCapt   := aBtn[2]
   cIco1x1 := aBtn[3]
   cIco1x2 := aBtn[4]
   nSize   := aBtn[5]
   aFClr1  := aBtn[6]
   aFClr2  := aBtn[7]
   cFont   := aBtn[8]
   nFSize  := aBtn[9]
   lBold   := aBtn[10]
   aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }

   hIco1 := LoadIconByName( cIco1x1, nSize, nSize )
   hIco2 := LoadIconByName( cIco1x2, nSize, nSize )

   @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
     ICON hIco1                                      ;
     WIDTH nWBtn HEIGHT nHBtn                        ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/     ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
     BACKCOLOR aGrOver GRADIENTFILL aGrFill          ;
     ON MOUSEHOVER ( This.Icon := hIco2 ,;
                     This.Fontcolor := aFClr2, This.GradientFill := aGrFill  ) ;
     ON MOUSELEAVE ( This.Icon := hIco1 ,;
                     This.Fontcolor := aFClr1, This.GradientOver := aGrOver ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aColor }  }
     //ACTION Eval(bAction) - не надо так

   This.&(cObj).Action   := bAction
   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

   AADD( owc:ahIcoDel, hIco1 )
   AADD( owc:ahIcoDel, hIco2 )

RETURN NIL

///////////////////////////////////////////////////////////////////
// color of the Active window caption
Function HMG_ColorWinActiveCaption()
   LOCAL aClr

   aClr := HMG_n2RGB( GetSysColor(COLOR_ACTIVECAPTION) )
   aClr := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aClr )

RETURN aClr

///////////////////////////////////////////////////////////////////
Function Color10_ActiveCaption  // "Active window caption"
   LOCAL i, xKey, aClr := RED
   // Если флаг ColorPrevalence имеет значение 0 то цвет окна либо чёрный либо белый.
   // Если флаг имеет значение 1 то цвет окна берётся из значения AccentColor.
   i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\ColorPrevalence")
   IF i == 1
      xKey := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\AccentColor")
      aClr := HMG_n2RGB(xKey)
   ELSE
      IF HMG_AppsUseTheme(.F.) == 0
         aClr := BLACK
      ELSE
         aClr := WHITE
      ENDIF
   ENDIF
RETURN aClr

///////////////////////////////////////////////////////////////////
Function HMG_AppsUseTheme(lRet)
  LOCAL i, cRet
  DEFAULT lRet := .T.
  i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize\AppsUseLightTheme")
  IF i == 0
     cRet := "(0) dark theme"
  ELSE
     cRet := "(1) light theme"
  ENDIF
RETURN IIF(lRet,cRet,i)
