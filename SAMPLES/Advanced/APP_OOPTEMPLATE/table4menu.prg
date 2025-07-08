/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "metrocolor.ch"

//////////////////////////////////////////////////////////////////////
FUNCTION myTable4menu(oWnd,nKy,cObj)
   LOCAL hWin, oWin, nH, nW, nG, nWBtn, nHBtn, cIco, cTitle, aRet
   LOCAL cFont, nFSize, cBFont, nBFSize, nHIco, nCol, nRow, cFrm
   LOCAL aBackColor, aBackClr2, aBtnFC, cTxt, aBtn, bAct, cVal, nI
   LOCAL aMonth, nWCombo, nWTxt, aDay, aBtnDay, nL

   ? ProcNL(), "Table", oWnd:Name, nKy, cObj
   cFrm := App.Cargo:cWinMain       // имя окна MAIN формы
   //FOR EACH cVal IN HMG_GetFormControls( cFrm, "TIMER" )
   //   SetProperty(cFrm, cVal, "Enabled", .F.)  // выключить таймер
   //NEXT

   cIco       := "iDbInfo64x1"
   cTitle     := 'MODAL (1)'
   cFont      := 'Tahoma'
   nFSize     := 12
   aBackColor := COLOR_AZURE3         // Цвет фона всей формы
   aBackClr2  := COLOR_DARK_PURPLE    // Цвет фона вверху формы
   nW         := 720
   nH         := 690
   nWBtn      := 250                // ширина кнопок внизу
   nHBtn      := 100                // высота кнопок внизу
   nHIco      := 72
   nG         := 20                 // отступ
   cBFont     := "Comic Sans MS"
   nBFSize    := nFSize + 4         // шрифт для кнопок
   aBtnFC     := BLACK              // цвет инверт.фонта кнопок
   nWCombo    := nWTxt := nL := 0
   aMonth     := Array(12)
   aDay       := Array(7)
   aBtnDay    := ARRAY(7)

   FOR nI := 1 TO 12
      aMonth[nI] := " " + LOWER( NTOCMONTH( nI ) ) + " "
      nWTxt      := GetTxtWidth( aMonth[nI], nBFSize, cBFont ) + nG
      nWCombo    := MAX( nWCombo, nWTxt )
   NEXT

   FOR nI := 1 TO 7
      aDay[nI] := " " + HB_NtoS(nI) +  " - " + LOWER( NTOCDOW( nI ) ) + " "
      nWTxt      := GetTxtWidth( aDay[nI], nBFSize, cBFont ) + nG
      nWCombo    := MAX( nWCombo, nWTxt )
      nL         := MAX(LEN(aDay[nI]), nL)
   NEXT

   FOR nI := 1 TO LEN(aBtnDay)
      aBtnDay[nI] := PADR(aDay[nI],nL)
   NEXT

   DEFINE WINDOW Form_M1          ;
      At 0, 0 WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco      ;
      MODAL NOSIZE                ;
      BACKCOLOR aBackColor        ;
      FONT cFont SIZE nFSize      ;
      ON INIT { || /*This.Topmost := .F.,*/ This.Label_Buff.Setfocus }

      hWin := GetFormHandle('Form_M1')
      nW   := This.ClientWidth
      nH   := This.ClientHeight
      oWin := This.Object
      cFrm := oWin:Name
      cVal := cFrm + " - " + cTitle
      This.Title := cVal  //SetProperty(cFrm, "Title", cVal)

      @ 0, 0 LABEL Label_Buff WIDTH nW HEIGHT 80 VALUE cTitle SIZE nFSize + 10 ;
        FONTCOLOR YELLOW BACKCOLOR aBackClr2  CENTERALIGN VCENTERALIGN
      nRow := This.Label_Buff.Height + nG

      cTitle := "Lang: " + hb_CdpSelect()
      nWTxt  := GetTxtWidth( cTitle, nBFSize, cBFont ) + nG
      @ nRow, nG*2 LABEL Label_0 WIDTH nWTxt HEIGHT nBFSize*2 VALUE cTitle  ;
        FONTCOLOR BLACK SIZE nBFSize FONT cBFont TRANSPARENT VCENTERALIGN
      nRow += This.Label_0.Height + 2

      nWCombo := IIF( nWCombo < nWTxt, nWTxt, nWCombo )
      @ nRow, nG*2 COMBOBOXEX ComboEx_Month WIDTH nWCombo HEIGHT 450 ;
        ITEMS aMonth VALUE 1 BACKCOLOR aBackClr2 SIZE nBFSize FONT cBFont
      nRow += nBFSize*1.6 + nG

      @ nRow, nG*2 COMBOBOXEX ComboEx_Day WIDTH nWCombo HEIGHT 350 ;
        ITEMS aDay VALUE 1 BACKCOLOR aBackClr2 SIZE nBFSize FONT cBFont
      nRow += nBFSize*1.6 + nG

      nCol := nG*2
      aBtn := { "Button_Month", cTitle, "none.ico", "none.ico", 32, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|a| a := MenuAchoice(aBtnDay), MsgDebug(a), M1Setfocus()  }  // Меню по массиву (по умолчанию) -> util_achoice.prg
      myDrawButtonGrad(nRow, nCol, nWCombo, nBFSize*3, aBtn, bAct, GRAY)

      /////////////////////// Button Down ////////////////////////////
      nRow := nH - nHBtn - nG
      nCol := nW / 2 - nWBtn - nG
      cTxt := "Search" + CRLF + "by address"
      aBtn := { "Button_Find", cTxt, "iPiople64x1", "iPiople64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ _wPost(90) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_PURPLE_METRO)

      nCol := nW / 2 + nG
      cTxt := "Exit" + CRLF + "this menu"
      aBtn := { "Button_Exit", cTxt, "iExit64x1", "iExit64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ _wPost(98) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_BRIGHT_RED)

      ON KEY ESCAPE OF Form_M1 ACTION _wPost(98)
      ON KEY F1     OF Form_M1 ACTION NIL

      WITH OBJECT This.Object
         :Event( 0, {|| InkeyGui(200)  } )

         :Event( 90, {|ow| // Search by address
                          LOCAL nPost
                          HMG_SetMousePos(This.Button_Find.Handle)
                          DoEvents()
                          This.Button_Find.Enabled := .F.
                          aRet := Modal2(cFont, nFSize)
                          IF LEN(aRet) > 0
                             IF AlertYesNo("Add a new record to the table?;" + ;
                                HB_ValToExp(aRet))
                                nPost := 99
                             ELSE
                                nPost := 2
                             ENDIF
                          ELSE
                             nPost := 2
                          ENDIF
                          _wPost(nPost, ow:Name)
                          RETURN NIL
                          } )

         :Event( 2, {|| This.Button_Find.Enabled := .T. ,;
                        Form_M1.Label_Buff.Setfocus          } )

         :Event(98, {|ow| // Exit this menu
                          HMG_SetMousePos(This.Button_Exit.Handle)
                          InkeyGui(200)
                          This.Button_Exit.Enabled := .F.
                          aRet := {}
                          _wPost(99, ow:Name)
                          Return Nil
                          } )
         :Event(99, {|ow| ow:Release()  } )
      END WITH

   END WINDOW

   CENTER WINDOW Form_M1
   ACTIVATE WINDOW Form_M1

RETURN aRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION M1Setfocus()
     Form_M1.Label_Buff.Setfocus
RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION Modal2(cFont, nFontSize)
   LOCAL nW, nH, cIco, cTitle, aBtnFC, lCheck1, cBFont, nBFSize, aBtnBClr
   LOCAL oWnd, aBackColor, aBackUpColor, aBackUpColor2, nHIco, cFrm
   LOCAL nRow2, nCol2, nW2Btn, nH2Btn, aNew, cTxt, aBtn, bAct

   cTitle        := "MODAL (2)"
   cIco          := "iDbInfo64x1"
   nBFSize       := nFontSize + 10
   cBFont        := "Comic Sans MS"
   aBackColor    := COLOR_LIGHT_GREEN3   // Цвет фона всей формы
   aBackUpColor  := COLOR_OF2016_GREEN   // Цвет фона верха формы
   aBackUpColor2 := COLOR_LIGHT_ORANGE   // Цвет верха фона формы
   aBtnFC        := BLACK
   nW            := 850
   nH            := 780
   aBtnBClr      := ARRAY(100)
   AFILL(aBtnBClr, GRAY )

   DEFINE WINDOW Form_M2          ;
      At 0, 0 WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco      ;
      MODAL NOSIZE                ;
      BACKCOLOR aBackColor        ;
      FONT cFont SIZE nFontSize   ;
      ON INIT {|| _wPost(0) }

      nW   := This.ClientWidth
      nH   := This.ClientHeight
      oWnd := This.Object
      cFrm := oWnd:Name

      This.Cargo := oHmgData()
      This.Cargo:aBtnBClr := aBtnBClr
      This.Title := cFrm + " - " + cTitle
      // SetProperty(cFrm, "Title", cFrm + " - " + cTitle)

      @ 0, 0 LABEL Label_0 WIDTH nW HEIGHT 150 VALUE cTitle SIZE nFontSize + 10 ;
        BOLD FONTCOLOR YELLOW BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      @ 2, nW-60 CHECKBOX Check_1 CAPTION "(1)" VALUE lCheck1 ;
        WIDTH 50 HEIGHT (nFontSize)*2 BACKCOLOR aBackUpColor SIZE nFontSize-2 ;
        ON CHANGE {|| lCheck1 := This.Check_1.Value }

      /////////////////////// Button Up ////////////////////////////
      nRow2  := This.Label_0.Row + This.Label_0.Height + 20
      nCol2  := 30
      nH2Btn := ( nH - 150 - 5*20 ) / 4
      nW2Btn := nW - nCol2*2
      aBtnFC := OLIVE
      nHIco  := 64

      cTxt := "1. Menu ..... New recto !!!" + Repl(".",10)
      aBtn := { "Button_1", cTxt, "iPiople64x1", "iPiople64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ This.Enabled := .F., _wPost(10) }
      aBtnBClr[10] := aBtnBClr[12] := ORANGE
      myDrawButtonGrad(nRow2, nCol2, nW2Btn, nH2Btn, aBtn, bAct, ORANGE)
      nRow2 += nH2Btn + 20

      cTxt := "2. Menu ....." + Repl(".",20)
      aBtn := { "Button_2", cTxt, "iMess64x1", "iMess64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ This.Enabled := .F., _wPost(20) }
      aBtnBClr[20] := aBtnBClr[22] := COLOR_BRIGHT_PURPLE
      myDrawButtonGrad(nRow2, nCol2, nW2Btn, nH2Btn, aBtn, bAct, COLOR_BRIGHT_PURPLE)
      nRow2 += nH2Btn + 20

      cTxt := "3. Menu ....."  + Repl(".",20)
      aBtn := { "Button_3", cTxt, "iMusic64x1", "iMusic64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ This.Enabled := .F., _wPost(30) }
      aBtnBClr[30] := aBtnBClr[32] := COLOR_BLUE_SKYPE
      myDrawButtonGrad(nRow2, nCol2, nW2Btn, nH2Btn, aBtn, bAct, COLOR_BLUE_SKYPE)
      nRow2 += nH2Btn + 20

      cTxt := "Exit this menu " + Repl(".",20)
      aBtn := { "Button_Ex", cTxt, "iExit64x1", "iExit64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ This.Enabled := .F., _wSend(97) }
      aBtnBClr[97] := HMG_n2RGB( CLR_HRED )
      myDrawButtonGrad(nRow2, nCol2, nW2Btn, nH2Btn, aBtn, bAct, CLR_HRED)
      nRow2 += nH2Btn + 20

      // Нельзя использовать _DefineHotKey - вешает чужие программы
      ON KEY ESCAPE OF Form_M2 ACTION _wPost(97, ThisWindow.Name)
      ON KEY F2     OF Form_M2 ACTION _wPost(87, ThisWindow.Name)
      ON KEY F1     OF Form_M2 ACTION NIL

      WITH OBJECT This.Object
         :Event( 0, {|  | InkeyGui(200)               } )
         //:Event( 0, {|  | my_ModalZero(.T.), DoEvents(), my_ModalZero(.F.) } )
         :Event( 1, {|ow| ow:Setfocus("Label_0")                  } )

         :Event(10, {|ow,ky| // 1.
                             LOCAL aColor, aBClr
                             aColor := ow:Cargo:aBtnBClr
                             aBClr  := aColor[ky]
                             aNew := {"Adres", "Code adres", ow:Name, This.Button_1.Caption }
                             //Modal3(cFont, nFontSize, This.Button_1.Caption, aBClr)
                             This.Button_1.Enabled := .T.
                             DoEvents()
                             _wPost(99, ow:Name)
                             RETURN NIL
                             } )

          :Event(20, {|ow,ky| // 2.
                             LOCAL nMsg, aColor, aBClr
                             aColor := ow:Cargo:aBtnBClr
                             aBClr  := aColor[ky]
                             aNew := {} //{"Menu-2"}
                             aNew := Modal3(cFont, nFontSize, This.Button_2.Caption, aBClr)
                             nMsg := iif( LEN(aNew) > 0, 99, 1 )
                             This.Button_2.Enabled := .T.
                             DoEvents()
                             _wPost(nMsg, ow:Name)
                             RETURN NIL
                             } )

          :Event(30, {|ow,ky| // 3.
                             LOCAL nMsg, aColor, aBClr
                             aColor := ow:Cargo:aBtnBClr
                             aBClr  := aColor[ky]
                             aNew :=  {} //{"Menu-3"}
                             aNew := Modal3(cFont, nFontSize, This.Button_3.Caption, aBClr)
                             nMsg := iif( LEN(aNew) > 0, 99, 1 )
                             This.Button_3.Enabled := .T.
                             DoEvents()
                             _wPost(nMsg, ow:Name)
                             RETURN NIL
                             } )

         :Event( 87, {|ow| _HMG_InplaceParentHandle := This.Handle , HelpThisWindow(), ;
                            _HMG_InplaceParentHandle := 0 , DoEvents(), _wPost(97, ow)  } )

         // Выход
         :Event( 97, {|ow,ky| MsgDebug(ow:Name,This.Button_Ex.Caption,ky) /*SaveIni()*/,;
                              aNew := {} , DoEvents(),  _wPost(99, ow:Name) } )

         :Event(99, {|ow| ow:Release()  } )
      END WITH

   END WINDOW

   CENTER WINDOW Form_M2
   ACTIVATE WINDOW Form_M2

RETURN aNew

//////////////////////////////////////////////////////////////////////////////
FUNCTION Modal3(cFont, nFontSize, cNameBtn, aBackUpColor)
   LOCAL nW, nH, cIco, cTitle, cBtnFont, nFSize, cText, aBtnFC, oWnd, cFrm
   LOCAL nRow, nCol, nWBtn, nHBtn, aNew, aBtn, bAct, aBackColor, nHIco
   DEFAULT aBackUpColor := COLOR_AZURE4 // Цвет фона верха формы

   cTitle        := "MODAL (3) - " + cNameBtn
   cIco          := "iDbInfo64x1"
   nFSize        := nFontSize + 10
   cBtnFont      := "Comic Sans MS"
   aBackColor    := COLOR_AZURE3          // Цвет фона всей формы
   nW            := System.DesktopWidth * 0.7
   nH            := 540

   DEFINE WINDOW Form_M3              ;
      At 0, 0 WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco      ;
      MODAL NOSIZE                ;
      BACKCOLOR aBackColor        ;
      FONT cFont SIZE nFontSize   ;
      ON INIT {|| /*This.Topmost := .T.,*/ DoEvents() }

      nW   := This.ClientWidth
      nH   := This.ClientHeight
      oWnd := This.Object
      cFrm := oWnd:Name
      This.Title := cFrm + " - " + cTitle
      // SetProperty(cFrm, "Title", cFrm + " - " + cTitle)

      @ 0, 0 LABEL Label_0 WIDTH nW HEIGHT 50 VALUE cTitle SIZE nFontSize + 10 BOLD ;
        FONTCOLOR YELLOW BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      /////////////////////// Button Up ////////////////////////////
      nRow   := This.Label_0.Row + This.Label_0.Height + 20
      nCol   := 20
      nHBtn  := ( nH - 50 - 5*20 ) / 4
      nHIco  := nHBtn - 4*2          // высота иконки
      nWBtn  := nW - nCol*2
      aBtnFC := OLIVE
      ? "nHIco=",nHIco

      cText := "1.Меню ... MODAL (3) .................."
      aBtn  := { "Button_1", cText, "iPiople64x1", "iPiople64x2", nHIco, aBtnFC, YELLOW, cBtnFont, nFSize, .T. }
      bAct  := {|| MsgDebug(This.Cargo), _wSend(10), DoEvents(), _wPost(12) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_DARK_GREY)
      nRow  += nHBtn + 20

      cText := "2.Меню ... MODAL (3) ....................."
      aBtn  := { "Button_2", cText, "iMess64x1", "iMess64x2", nHIco, aBtnFC, YELLOW, cBtnFont, nFSize, .T. }
      bAct  := {|| MsgDebug(This.Cargo), _wSend(20), DoEvents(), _wPost(22) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_DARK_GREY)
      nRow  += nHBtn + 20

      cText := "3.Меню ... MODAL (3) ....................."
      aBtn  := { "Button_3", cText, "iMusic64x1", "iMusic64x2", nHIco, aBtnFC, YELLOW, cBtnFont, nFSize, .T. }
      bAct  := {|| MsgDebug(This.Cargo), _wSend(30), DoEvents(), _wPost(32) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_DARK_GREY)
      nRow  += nHBtn + 20

      cText := "Выход ... MODAL (3) ........"
      aBtn  := { "Button_4", cText, "iExit64x1", "iExit64x2", nHIco, aBtnFC, YELLOW, cBtnFont, nFSize, .T. }
      bAct  := {|| This.Enabled := .F., MsgDebug(This.Cargo), _wSend(97) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_RED_METRO)

      // Нельзя использовать _DefineHotKey - вешает чужие программы
      ON KEY ESCAPE OF Form_M3 ACTION _wPost(97)
      ON KEY F2     OF Form_M3 ACTION _wPost(87, ThisWindow.Name)
      ON KEY F1     OF Form_M3 ACTION NIL

      WITH OBJECT This.Object
         :Event( 0, {|  | InkeyGui(200)                                  } )

         :Event(10, {|  | This.Button_1.Enabled := .F. } )
         :Event(11, {|ow| This.Button_1.Enabled := .T. , _wPost(90, ow) } )
         :Event(12, {|ow,ky| // 1.
                          aNew := {"Menu-1"}
                          MsgDebug(ow:Name,This.Button_1.Caption,ky)
                          DoEvents()
                          _wPost(11, ow:Name)
                          RETURN NIL
                          } )

          :Event(20, {|  | This.Button_2.Enabled := .F. } )
          :Event(21, {|ow| This.Button_2.Enabled := .T. , _wPost(90, ow) } )
          :Event(22, {|ow,ky| // 2.
                           LOCAL nMsg
                           aNew := {} //{"Menu-2"}
                           MsgDebug(ow:Name,This.Button_2.Caption,ky)
                           nMsg := iif( LEN(aNew) > 0, 99, 21 )
                           DoEvents()
                           _wPost(nMsg, ow:Name)
                           RETURN NIL
                           } )

          :Event(30, {|  | This.Button_3.Enabled := .F. } )
          :Event(31, {|ow| This.Button_3.Enabled := .T.  , _wPost(90, ow) } )
          :Event(32, {|ow,ky| // 3.
                           LOCAL nMsg
                           aNew :=  {} //{"Menu-3"}
                           MsgDebug(ow:Name,This.Button_3.Caption,ky)
                           nMsg := iif( LEN(aNew) > 0, 99, 31 )
                           DoEvents()
                           _wPost(nMsg, ow:Name)
                           RETURN NIL
                           } )

         :Event( 87, {|ow| _HMG_InplaceParentHandle := This.Handle , HelpThisWindow(), ;
                            _HMG_InplaceParentHandle := 0 , DoEvents(), _wPost(90, ow)  } )

         :Event( 90, {|ow| // остаться в окне выбора
                           Local cMsg := "Window not found ! " + ow:Name + ";" + ProcNL()
                           IF ! To_Focus(ow:Name , "Label_0")
                              AlertStop(cMsg, "ERROR")
                           ENDIF
                           DO EVENTS
                           wApi_Sleep(100)
                           Return Nil
                           } )
         // Выход
         :Event( 97, {|ow,ky| MsgDebug(ow:Name,This.Button_4.Caption,ky) /*SaveIni()*/,;
                              aNew := {} , DoEvents(),  _wPost(99, ow:Name) } )

         :Event(99, {|ow| ow:Release()  } )
      END WITH

   END WINDOW

   CENTER WINDOW Form_M3
   ACTIVATE WINDOW Form_M3

RETURN aNew

//////////////////////////////////////////////////////////////////////////////
FUNCTION my_ModalZero(lVxod)
   LOCAL cMsg, cForm := ProcName()
   LOCAL y := 0, x := 0, w := 100, h := 70

   cMsg := "[***] Вход в окно MODAL !  " + cForm + ", lVxod="
   cMsg += cValToChar(lVxod) + ";;" + ProcNL() + ";" + ProcNL(1)
   ? cMsg

   DEFINE WINDOW &cForm AT y, x WIDTH w HEIGHT h TITLE " " ;
      MODAL NOCAPTION NOSIZE BACKCOLOR RED ;
      ON INIT {|| _wPost( iif( lVxod, 99, 0 ) ) }

      (This.Object):Event( 0, {|ow| AlertInfo(cMsg + ":Event(0)"),;
                                    _logfile(.t.,"   ---[ :Event(0) ]---" + ProcNL() ) ,;
                                    _wPost(90,ow:Name), DoEvents(), _wPost(99) })
      (This.Object):Event(87, {|ow| _HMG_InplaceParentHandle := This.Handle , HelpThisWindow(), ;
                               _HMG_InplaceParentHandle := 0 , DoEvents(), _wPost(90, ow)  } )
      (This.Object):Event(90, {|ow| // остаться в окне выбора
                                   Local cMsg := "Window not found ! " + ow:Name + ";" + ProcNL()
                                   IF ! To_Focus(ow:Name , "Label_0")
                                      AlertStop(cMsg, "ERROR")
                                   ENDIF
                                   DO EVENTS
                                   wApi_Sleep(100)
                                   Return Nil
                               })
      (This.Object):Event(99, {|ow| InkeyGui(500) ,;
                                    _logfile(.t.,"   ---[ :Event(99) ]---" + ProcNL() ) ,;
                                    ow:Release() })

      ON KEY ESCAPE ACTION _wPost(99)
      ON KEY F2     ACTION _wPost(87, ThisWindow.Name)
      ON KEY F1     ACTION _wPost(87, ThisWindow.Name)

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN Nil
