/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте AVI + LABEL / ACHOICE() menu on AVI + LABEL object
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "i_winuser.ch"
#define MYRED       { 211, 107, 107 }
#define MYBACK      { 211, 153, 244 }
#define MY_LBLFONT  { WHITE      , YELLOW, GRAY   }    // цвет фонта кнопки {MOUSEHOVER,MOUSELEAVE,Блокировка}
#define MY_LBLBACK  { {94,59,185}, BLACK , SILVER }    // цвет фона кнопок  {MOUSEHOVER,MOUSELEAVE,Блокировка}

//////////////////////////////////////////////////////////////////////////////
FUNCTION MenuAviAchoice(aMenu, aParWin, aParMenu, aParAvi, aIcoWin)
   LOCAL nY, nX, nK, nL, nH, nW, nG, nI, nWBtn, cIco, cTitle, cTxt
   LOCAL aRet, cFont, nFSize, aDim, nLine, nJ
   LOCAL cFontExit, aBackColor, cForm, nRow, nCol, nWidth
   LOCAL nHBtn, nHWin, nWVirt, nHVirt, nHDsktp
   LOCAL nWAvi, nHAvi, nGAvi, nTxtAlign, lLblBordr, lAviShow
   LOCAL nAviAlign, lAvi, lWinIcon, nWIcon, nIcoAlign
   LOCAL aLblFClr, aLblBClr
   LOCAL nIcoSz, nIcoX, nIcoY, cIcoRes, lLblClEdge
   LOCAL nTxtAlignDef := 3       // Align Text: 0-None, 1-Left, 2-Right, 3-Center
   LOCAL lVScroll, bEvents_Old

   DEFAULT aMenu    := { "No menu array" }
   DEFAULT aParWin  := { "Form_Achoice", "Menu-Achoice", App.Cargo:cIconDef, MYBACK, 0, 0, 0, 0 }
   // параметры меню
   DEFAULT aParMenu := { 40, 20, "Tahoma", "Arial", 20, MY_LBLFONT, MY_LBLBACK, .F., .T., nTxtAlignDef }
   // параметры Avi   // 1-Left Align Avi, 2-Right Align Avi
   DEFAULT aParAvi  := { 1, .F. }   // { nAviAlign, lAviShow } F-нет показа avi на форме
   DEFAULT aIcoWin  := {}           // нет иконки на форме

   IF !HB_IsArray(aMenu)
      AlertStop("Parameters aMenu is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF !HB_IsArray(aParWin)
      AlertStop("Parameters aParWin is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF LEN(aParWin) < 8
      AlertStop("Array must be equal to aParWin[8] !;;"+ProcNL())
      RETURN {}
   ENDIF

   IF !HB_IsArray(aParMenu)
      AlertStop("Parameters aParMenu is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF LEN(aParMenu) # 10
      AlertStop("Parameters aParMenu[10] is not # 10 !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF !HB_IsArray(aParAvi)
      AlertStop("Parameters aParAvi is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   lWinIcon := .F.
   IF !HB_IsArray(aIcoWin) // иконка на форме
      AlertStop("Parameters aIcoWin[] is not an array !;;"+ProcNL())
      RETURN {}
   ELSE
      IF LEN(aIcoWin) == 0
      ELSEIF LEN(aIcoWin) < 5
         AlertStop("Parameters aIcoWin[] < 5 !;;"+ProcNL())
         RETURN {}
      ELSE
         lWinIcon := .T. // иконка на форме
      ENDIF
   ENDIF

   aRet       := {}                                       // вернуть массив из этой функции
   cForm      := aParWin[1]
   cTitle     := aParWin[2]
   cIco       := aParWin[3]
   aBackColor := aParWin[4]  ; Default aBackColor := MYBACK  // цвет фона всей формы
   nRow       := aParWin[5]  ; Default nRow       := 0
   nCol       := aParWin[6]  ; Default nCol       := 0
   nWidth     := aParWin[7]  ; Default nWidth     := 0
   // nHeight := aParWin[8]  ; Default nHeight    := 0
   nHBtn      := aParMenu[1] ; Default nHBtn      := 40
   nG         := aParMenu[2] ; Default nG         := 10                         // отступ между кнопками
   cFont      := aParMenu[3] ; Default cFont      := "DejaVu Sans Mono"         // фонт для кнопок
   cFontExit  := aParMenu[4] ; Default cFontExit  := "Comic Sans MS"            // фонт для кнопки Exit
   nFSize     := aParMenu[5] ; Default nFSize     := 20
   aLblFClr   := aParMenu[6]
   aLblBClr   := aParMenu[7]
   lLblBordr  := aParMenu[8]  ; Default lLblBordr  := .F.
   lLblClEdge := aParMenu[9]  ; Default lLblClEdge := .F.
   nTxtAlign  := aParMenu[10] ; Default nTxtAlign  := 1       // Align Text: 0-None, 1-Left, 2-Right, 3-Center
   // aIcoWin := { "iHP1", 150, 0, 20, nIcWinAlgn }  // иконка на форме
   nWIcon := nIcoAlign := 0
   IF lWinIcon    // иконка на форме
      cIcoRes   := aIcoWin[1]
      nIcoSz    := aIcoWin[2]
      nIcoY     := aIcoWin[3]
      nIcoX     := aIcoWin[4]
      nIcoAlign := aIcoWin[5]
      nWIcon    := nIcoSz + nIcoX * 2
   ENDIF

   // aParAvi  := { 1, .F. }   // { nAviAlign, lAviShow } F-нет показа avi на форме
   IF LEN(aParAvi) == 0
      nAviAlign := 1           // 1-Left Align Avi, 2-Right Align Avi
      lAviShow  := .T.         // T-нет показа avi на форме
   ELSE
      nAviAlign := aParAvi[1]
      lAviShow  := aParAvi[2]
   ENDIF
   // кол-во строк на кнопке
   nLine := 1
   nL    := 0
   nK    := Len(aMenu)
   FOR nI := 1 TO nK
      cTxt := aMenu[nI,1]
      IF ";" $ cTxt       // в строке есть CRLF
         aDim  := HB_ATokens(cTxt, ";")
         nLine := MAX( Len(aDim), nLine )
         FOR nJ := 1 TO LEN(aDim)
            nL := MAX( Len(aDim[nJ]), nL )
         NEXT
      ELSE
         nL := MAX( Len(cTxt), nL )
      ENDIF
   NEXT
   cTxt  := REPL("a",nL+2)
   //nWBtn := GetTxtWidth( cTxt, nFSize+2, cFont, .F. ) + nG*2  // получить Width текста
   nWBtn := GetTxtWidth( cTxt, nFSize+2, cFont, .T. ) + nG*2  // получить Width текста с BOLD

   // Изменить массив - выровнять текст
   aMenu := myChangeMenuArray(aMenu,nL,nTxtAlign)
   // Изменить цвета фона кнопки
   aMenu := myChangeMenuColor(aMenu)
   // проверка Avi на окне и возврат исправленного aMenu
   lAvi  := CheckAviWinSize(@aMenu, @nWAvi, @nHAvi)
   nGAvi := 4
   IF !lAvi            // T-нет показа avi на форме
      lAviShow := .F.
   ENDIF
   IF lAviShow         // показ avi на форме
      nWBtn += nGAvi*2 + nWAvi
      IF nLine == 1 .AND. nHBtn < nHAvi + nGAvi*2   // одна строка
         nHBtn := nHAvi + nGAvi*2                   // новая высота кнопки
      ENDIF
   ENDIF

   nW := nWBtn + nG*2 //+ GetBorderWidth()*2               // ширина окна

   // кол-во строк на кнопке
   IF nLine > 1
      nHBtn += nFSize * (nLine-1)  + 6
   ENDIF

   // координаты окна
   nY := IIF( nRow > 0, nRow, 0 )
   nX := IIF( nCol > 0, nCol, 0 )
   nW := IIF( nWidth  > 0, nWidth , nW )
   // расчёт на высоту окна
   nHWin   := CalcWinHeight(nG,aMenu,nHBtn)
   nWVirt  := NIL
   nHVirt  := NIL
   nHDsktp := GetDesktopHeight() - GetTaskBarHeight() // высота Панели задач Desktop
   // установим внешнюю высоту окна
   IF nHDsktp < nHWin
      nHVirt := nHWin + nG*3
      nH     := nHDsktp - GetTitleHeight() - GetBorderHeight()*2
   ELSE
      nH := nHWin
   ENDIF

   IF lWinIcon            // иконка на форме
      nW  += nWIcon       // добавить к ширине формы
   ENDIF

   IF ( lVScroll := iif( Empty(nHVirt), .F., nHVirt > nH ) )
      bEvents_Old := Set_bEvents( {|hwnd,nMsg,wParam,lParam|
         Local nRet := 0, nKey
         IF !_IsWindowDefined(cForm)     ; RETURN nRet
         ELSEIF !IsWindowActive(cForm)   ; RETURN nRet
         ENDIF
         IF hwnd != GetFormHandle(cForm) ; RETURN nRet
         ENDIF
         SWITCH nMsg
         CASE WM_MOUSEWHEEL
               If HiWord( wParam ) == WHEEL_DELTA
                  If GetScrollPos( hwnd , SB_VERT ) < 25
                     //nKey := VK_HOME
                     nKey := SB_TOP
                  Else
                     //nKey := VK_UP
                     nKey := SB_LINEUP
                  Endif
               Else
                  If GetScrollPos( hwnd , SB_VERT ) >= GetScrollRangeMax ( hwnd , SB_VERT ) - 10
                     //nKey := VK_END
                     nKey := SB_BOTTOM
                  Else
                     //nKey := VK_DOWN
                     nKey := SB_LINEDOWN
                  Endif
               Endif
               //SendMessage ( hwnd, WM_KEYDOWN, nKey, 0 ) }
               _wPost("PressKey", cForm, nKey)
               nKey := lParam
               nRet := 1
         END
         Return nRet
         } )
   ENDIF

   //DEFINE WINDOW &cForm At nY, nX WIDTH nW HEIGHT nH ;
   DEFINE WINDOW &cForm AT nY, nX CLIENTAREA nW, nH    ;
      VIRTUAL WIDTH  nWVirt                            ;
      VIRTUAL HEIGHT nHVirt                            ;
      TITLE cTitle ICON cIco                           ;
      MODAL NOSIZE                                     ;
      BACKCOLOR aBackColor                             ;
      FONT cFont SIZE nFSize                           ;
      ON INIT     {|| _wPost( 0) }                     ;
      ON RELEASE  {|| _wSend(90) }                     ; // executed before destroying the window
      ON INTERACTIVECLOSE This.Cargo:lClose

      This.Cargo := oHmgData()
      This.Cargo:lClose := .T.
      This.Cargo:lDebug := App.Cargo:lDebug // включить отладку в лог-файл
      This.Cargo:aRezult := {}              // результат на выход из этого меню

      nW    := This.ClientWidth
      //nH  := This.ClientHeight
      nY    := nX := nG

      IF lWinIcon  // иконка на форме
         nWBtn := nW - nG * 2 - nWIcon
      ELSE
         nWBtn := nW - nG * 2
      ENDIF

      @ 0,0 Label Buff Value "" AUTOSIZE

      IF nIcoAlign == 1   // 1-Left Align Icon
         nX += nWIcon     // с учётом картинки если иконка слева
      ENDIF

      Draw_Label(cForm, aMenu, nWAvi, nHAvi, nGAvi, nAviAlign, lAviShow, ;
                   aLblFClr, aLblBClr, cFont, cFontExit, nFSize,;
                   nG, nY, nX, nWBtn, nHBtn, nLine, lLblBordr, lLblClEdge, nTxtAlign )

      IF lWinIcon    // avi на форме
         This.Cargo:hIcon := LoadIconByName( cIcoRes, nIcoSz, nIcoSz )
         IF nIcoAlign == 2          // 2-Right Align Icon
            nIcoX += nWBtn + nG*2
         ENDIF
         DRAW ICON IN WINDOW &cForm AT nIcoY, nIcoX HICON This.Cargo:hIcon ;
                                     WIDTH nIcoSz HEIGHT nIcoSz COLOR aBackColor
      ENDIF

      ON KEY F1     ACTION MsgAbout()
      ON KEY ESCAPE ACTION _wSend(99)
         IF lVScroll
      ON KEY HOME   ACTION _wPost("PressKey", , SB_TOP     )
      ON KEY END    ACTION _wPost("PressKey", , SB_BOTTOM  )
      ON KEY PRIOR  ACTION _wPost("PressKey", , SB_PAGEUP  )
      ON KEY NEXT   ACTION _wPost("PressKey", , SB_PAGEDOWN)
      ON KEY UP     ACTION _wPost("PressKey", , SB_LINEUP  )
      ON KEY DOWN   ACTION _wPost("PressKey", , SB_LINEDOWN)
         ENDIF
      // Установка событий на это окно
      WITH OBJECT This.OBJECT
         :Event( 0, {|ow|
                      ow:Cargo:lClose := .F.                   // блокировать выход по [X]
                      This.Topmost    := .F.
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 ) // Show form ICO
                      _wSend(1, ow)
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 ) // Show form ICO
                      ow:Cargo:lClose := .T.                   // разрешить выход по [X]
                      Return Nil
                      })
         :Event( 1, {|ow| // ON INIT windows
                      LOCAL aBC
                      // запомним и сменим фон окна
                      aBC := ow:BackColor
                      ow:BackColor := GRAY
                      //aBC := GetProperty( ow:Name, "Backcolor"  )
                      //SetProperty( ow:Name, "Backcolor", GRAY )
                      DoMethod(ow:Name, "DisableUpdate")  // блокировать всю форму
                                                          // block the whole form
                      IF ow:Cargo:lDebug
                         ? ProcNL(), "----- :Event(1)", ow:Name
                      ENDIF

                      DO EVENTS
                      DoMethod(ow:Name, "EnableUpdate")   // разблокировка всей формы
                                                          // unlock the whole form
                      // восстановим фон окна
                      //SetProperty( ow:Name, "Backcolor", aBC )
                      ow:BackColor := aBC
                      ow:Setfocus("Buff")
                      DO EVENTS
                      Return Nil
                      })
         :Event({2, "PressKey"}, {|ow,ky,nn| // Press key
                      ky := WM_VSCROLL
                      SendMessage( ow:Handle, ky, nn, 0 )      // VScroll position
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 ) // Show form ICO
                      Return Nil
                      })
         :Event({3, "OnClick"}, {|olbl| // ONCLICK button-label
                      Local ow := olbl:Window
                      Local o  := olbl:Cargo
                      Local o1 := ow:GetObj("Buff")
                      Local cmsg
                      //
                      IF ow:Cargo:lDebug
                         ? ProcNL(), "----- :Event(3, OnClick)", ow:Name, o:lAction
                      ENDIF
                      IF o:lAction ; Return Nil   // повторный вход в событие
                      ENDIF
                      o:lAction := .T.            // блокируем повторный вход
                      cmsg := o:cObj + ";" + o:cTxt
                      cmsg += ";" + HB_NtoS(o:nBtn)
                      IF ow:Cargo:lDebug
                         ? ProcNL(), "oBtn=", cmsg
                         _o2Log(o, 15, "==> .T. Cargo: ", .T.)  // вывод в лог-файл
                      ENDIF
                      // остановить все avi-объекты
                      _wSend("AviStopAll", ow, olbl)
                      o1:Setfocus()
                      DO EVENTS

                      SET WINDOW THIS TO ow:Name
                          AlertInfo(cmsg, ProcNL())
                      SET WINDOW THIS TO

                      // запустить все avi-объекты
                      _wSend("AviOpenAll", ow, olbl)
                      //
                      o1:Setfocus()
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 ) // Show form ICO
                      ow:Cargo:aRezult := {o:nBtn,o:cTxt} // результат запомнили
                      IF ow:Cargo:lDebug
                         ? ProcNL(), "----- :Event(3, OnClick)", ow:Name, "EXIT !!!"
                         ? SPACE(5) + HB_ValToExp( ow:Cargo:aRezult )
                      ENDIF
                      o:lAction := .F.  // завершили работу события
                      _wSend(99, ow)    // выход из окна
                      Return Nil
                      })
         :Event({5, "AviOpenAll" }, {|ow,ky,olbl| // запустить все avi-объекты
                      Local oAvi, cObj, lDbg := ow:Cargo:lDebug
                      IF lDbg
                         ? ProcNL(), "----- :Event(5,AviOpenAll)", ow:Name
                      ENDIF
                      // берём все открытые Avi на форме
                      FOR EACH oAvi IN ow:GetObj4Type("ANIMATEBOX")
                         _OpenAnimateBox( oAvi:Name, ow:Name, oAvi:Cargo:cFile )
                         IF lDbg
                            ? SPACE(10) + HB_NtoS(hb_enumIndex(oAvi)), oAvi:Name, oAvi:Cargo:cFile
                         ENDIF
                      NEXT
                      IF lDbg
                         ? ProcNL(), "----- :Event(5,AviOpenAll)", ow:Name
                      ENDIF
                      IF !Empty(olbl)
                         // смена цвета фона кнопки и фонта
                         ky   := olbl:Cargo
                         //cObj := ky:cObj    // имя объекта Label
                         //This.&(cObj).BackColor := ky:aFontBClr[1]
                         //This.&(cObj).FontColor := ky:aFontFClr[1]
                         olbl:BackColor := ky:aFontBClr[1]
                         olbl:FontColor := ky:aFontFClr[1]
                         IF lDbg
                            ? SPACE(10) + "ky:cObj=", ky:cObj, "BackColor/FontColor"
                         ENDIF
                         // смена цвета фона avi
                         cObj := ky:cAvi    // имя объекта avi
                         IF lDbg
                            ?? "ky:cAvi=", cObj, LEN(cObj) > 0
                         ENDIF
                         IF !Empty(cObj)
                            This.&(cObj).BackColor := ky:aFontBClr[1]
                         ENDIF
                      ENDIF
                      DO EVENTS
                      Return Nil
                      })
         :Event({6, "AviStopAll" }, {|ow,ky,olbl|  // остановить все avi-объекты
                      Local oAvi, lDbg := ow:Cargo:lDebug
                      IF lDbg ; ? ProcNL(), "----- :Event(6,AviStopAll)", ow:Name
                      ENDIF
                      // берём все открытые Avi на форме
                      FOR EACH oAvi IN ow:GetObj4Type("ANIMATEBOX")
                         _StopAnimateBox( oAvi:Name, ow:Name )
                         IF lDbg
                            ? SPACE(10) + HB_NtoS(hb_enumIndex(oAvi)), oAvi:Name, oAvi:Cargo:cFile
                         ENDIF
                      NEXT
                      // резерв
                      IF !Empty(olbl)
                         ky := olbl:Cargo
                      ENDIF
                      Return Nil
                      })
         :Event(90, {|ow|  // закрытие окна
                      aRet := ow:Cargo:aRezult        // результат работы возвращаем
                      DestroyIcon(ow:Cargo:hIcon)     // убить хендл иконки
                      _logfile(.t.,"   ---[ :Event(90) ]---" + ProcNL(), ">>> RELEASE: " + ow:Name )
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() } )
      END WITH

   END WINDOW

   IF nRow == 0 .AND. nCol == 0
      CENTER WINDOW &cForm
   ENDIF
   ACTIVATE WINDOW &cForm

   IF !Empty( bEvents_Old ) ; Set_bEvents( bEvents_Old )
   ENDIF

RETURN aRet

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_Label(cForm, aMenu, nW0Avi, nH0Avi, nGAvi, nAviAlign, lAviShow, ;
   aLblFClr, aLbl0BClr, cFont, cFontExit, nFSize, nG, nY, nX, nWBtn, nHBtn, nLine, lLblBordr, lLblClEdge, nTxtAlign )
   LOCAL cTxt, aBtnBClr, aBtnFClr, cResAvi, aWH, nRow, nCol, nWAvi, nHAvi
   LOCAL nK, nI, cN, cI, cA, oo

   nK := Len(aMenu)
   FOR nI := 1 TO nK
      cI   := StrZero(nI, 2)
      cN   := "Lbl_" + cI
      cA   := "Avi_" + cI
      cTxt := aMenu[ nI, 1 ]
      IF lAviShow
         cResAvi := aMenu[ nI, 2 ]
      ENDIF

      IF ";" $ cTxt ; cTxt := StrTran( cTxt, ";", CRLF )
      ENDIF

      IF LEN(aLbl0BClr) == 0
         // цвет берем с меню
         aBtnBClr := { aMenu[ nI, 3 ], aMenu[ nI, 4 ], aMenu[ nI, 5 ] }
      ELSE
         // единый цвет для кнопок
         aBtnBClr := aLbl0BClr
      ENDIF
      aBtnFClr := aLblFClr

      @ nY, nX LABEL &cN OF &cForm VALUE cTxt WIDTH nWBtn HEIGHT nHBtn  ;
        FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[1] BORDER CLIENTEDGE   ;
        ON MOUSEHOVER {|| RC_CURSOR( "MINIGUI_FINGER" ), MouseFocus_Label(.T., This.Cargo) } ;
        ON MOUSELEAVE {|| MouseFocus_Label(.F., This.Cargo) } ;
        ON CLICK {| | _wPost(This.Cargo:nPost , This.Index) } ;
        ON INIT  {|o|                       // ^^^^ This среда контрола
                   This.Cargo := oHmgData()
                   o := This.Cargo
                   o:oObj    := This.Object
                   o:cObj    := This.Name
                   o:nRow    := This.Row
                   o:nCol    := This.Col
                   o:nWidth  := This.Width
                   o:nHeight := This.Height
                   o:lAction := .F.
                   o:lHover  := .F.
                   //o:nBtn := nI    // !!! не делать так - внешние переменные
                   //o:cTxt := cTxt  // !!! не делать так - внешние переменные
                   Return Nil
                   }

      oo := This.&(cN).Cargo          // быстрее отрабатывает такой вариант кода т.к. один This
      oo:nObj       := nI
      oo:nBtn       := nI
      oo:cTxt       := cTxt
      oo:nPost      := "OnClick"      // 3  - вызов события строка/число
      oo:nFontHover := nFSize + 2
      oo:nFontLeave := nFSize
      oo:aFontFClr  := aBtnFClr
      oo:aFontBClr  := aBtnBClr
      oo:cAviName   := ""             // имя файла
      oo:cAvi       := ""             // имя объекта

      // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      IF nTxtAlign == 2
         This.&(cN).Alignment := "RIGHT"
      ELSEIF nTxtAlign == 3
         This.&(cN).Alignment := "CENTER"
      ENDIF

      IF nLine == 1
         This.&(cN).Alignment := "VCENTER"
      ENDIF

      IF !lLblBordr
         ChangeStyle( GetControlHandle( cN, cForm ), , WS_BORDER )
      ENDIF

      IF !lLblClEdge
         ChangeStyle( GetControlHandle( cN, cForm ), , WS_EX_CLIENTEDGE, .T. )
      ENDIF

      IF nI == nK  // последняя кнопка
         oo:nPost := 99
         //This.&(cN).Caption   := "Exit menu / Выход из меню"
         This.&(cN).Alignment := "CENTER"
         This.&(cN).Alignment := "VCENTER"
         This.&(cN).FontName  := cFontExit
         This.&(cN).FontBold  := .T.
         This.&(cN).Action    := {|| _wPost(This.Cargo:nPost) }  // This среда окна
      ELSE
         This.&(cN).FontName := cFont
      ENDIF

      IF HB_ISCHAR(cResAvi) .AND. !Empty(cResAvi) // LEN(ALLTRIM(cResAvi)) > 0
                                                  // cResAvi := " " -> даст ошибку
         oo:cAviName := cResAvi               // !!! имя файла avi в Label
         oo:cAvi := cA                        // !!! имя контрола avi в Label
         aWH     := GetAviResSize(cResAvi)
         nWAvi   := aWH[1]
         nHAvi   := aWH[2]

         IF nAviAlign == 1 // 1-Left Align Avi, 2-Right Align Avi
            nCol := nX + nGAvi*2
            IF nHAvi < nHBtn
               nRow := nY + (nHBtn - nHAvi) / 2
            ELSE
               nRow := nY + nGAvi
            ENDIF
         ELSE
            nCol := nWBtn - nGAvi*2 - nWAvi + nX
            IF nHAvi < nHBtn
               nRow := nY + (nHBtn - nHAvi) / 2
            ELSE
               nRow := nY + nGAvi
            ENDIF
         ENDIF
         IF nW0Avi > nWAvi    // резерв
         ENDIF
         IF nH0Avi > nHAvi    // резерв
         ENDIF
         // в МиниГуи показ AVI поддерживается кодеком Microsoft RLE Video, современные кодеки не поддерживаются
         // in MiniGui, AVI display is supported by the Microsoft RLE Video codec, modern codecs are not supported
         @ nRow, nCol ANIMATEBOX &cA WIDTH nWAvi HEIGHT nHAvi FILE cResAvi ;
                      AUTOPLAY TRANSPARENT BACKCOLOR aBtnBClr[1] NOBORDER
         This.&(cA).Cargo := oHmgData()
         oo := This.&(cA).Cargo
         oo:oObj   := This.&(cA).Object  // объект свой  для avi
         oo:cObj   := cA                 // имя avi контрола
         oo:cLabel := cN                 // имя Label для avi
         oo:oLabel := This.&(cN).Object  // объект Label для avi
         oo:cFile  := cResAvi            // имя файла для avi

      ENDIF

      nY += nHBtn + nG

   NEXT

RETURN nY

////////////////////////////////////////////////////////////////////////////////////
FUNCTION MouseFocus_Label(lFocus, o)
   LOCAL n //, oo, ns := 4
   DEFAULT o := This.Cargo

   //oo := o:oObj

   IF !Empty(lFocus)
      IF !o:lHover
         n := iif( o:lAction, 3, 2 )
         This.FontSize  := o:nFontHover
         This.FontColor := o:aFontFClr[n]
         This.BackColor := o:aFontBClr[n]
         IF !Empty(o:cAvi)
            This.&(o:cAvi).BackColor := o:aFontBClr[n]
         ENDIF
         o:lHover := .T.
         // изменим размеры Label
         //oo:SetSize(oo:nRow - ns, oo:nCol - ns, oo:nWidth + ns*2, oo:nHeight + ns*2)
      ENDIF
   ELSE
      n := iif( o:lAction, 3, 1 )
      This.FontSize  := o:nFontLeave
      This.FontColor := o:aFontFClr[n]
      This.BackColor := o:aFontBClr[n]
      IF !Empty(o:cAvi)
         This.&(o:cAvi).BackColor := o:aFontBClr[n]
      ENDIF
      o:lHover := .F.
      // восстановим размеры Label
      //oo:SetSize(oo:nRow, oo:nCol, oo:nWidth, oo:nHeight)
   ENDIF
   // на кнопках лучше не использовать изменение размера в Hover\Lover
   // медленная перерисовка кнопок при смене размеров
   //    oo:SetSize(oo:Row - ns, oo:Col - ns, oo:Width + ns*2, oo:Height + ns*2)


RETURN Nil

///////////////////////////////////////////////////////////////////////////////////
FUNCTION CalcWinHeight(nG,aMenu,nHBtn)
   LOCAL nI, nH := nG

   FOR nI := 1 TO Len(aMenu)
       nH += nHBtn + nG
   NEXT

RETURN nH

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myChangeMenuArray(aMenu,nL,nTxtAlign)
   LOCAL nI, nK, nJ, aDim, cTxt, cStr

   // Align Text: 0-None, 1-Left, 2-Right, 3-Center
   nK  := Len(aMenu)
   FOR nI := 1 TO nK
      cTxt  := aMenu[nI,1]
      // работа с текстом
      IF ";" $ cTxt       // в строке есть CRLF
         aDim := HB_ATokens(cTxt, ";")
         cStr := ""
         FOR nJ := 1 TO LEN(aDim)
            cTxt := aDim[nJ]
            IF nTxtAlign == 1 .OR. nTxtAlign == 0
               cStr += " " + PADR(cTxt,nL)
            ELSEIF nTxtAlign == 2
               cStr += " " + PADL(cTxt,nL)
            ELSEIF nTxtAlign == 3
               cStr += " " + ALLTRIM(cTxt)
            ENDIF
            cStr += IIF(nJ == LEN(aDim),"",";")
         NEXT
         aMenu[nI,1] := cStr
      ELSEIF CRLF $ cTxt       // в строке есть CRLF
         aDim := HB_ATokens(cTxt, CRLF)
         cStr := ""
         FOR nJ := 1 TO LEN(aDim)
            cTxt := aDim[nJ]
            IF nTxtAlign == 1 .OR. nTxtAlign == 0
               cStr += " " + PADR(cTxt,nL)
            ELSEIF nTxtAlign == 2
               cStr += " " + PADL(cTxt,nL)
            ELSEIF nTxtAlign == 3
               cStr += " " + ALLTRIM(cTxt)
            ENDIF
            cStr += IIF(nJ == LEN(aDim),"",";")
         NEXT
         aMenu[nI,1] := cStr
      ELSE
         IF nTxtAlign == 1  .OR. nTxtAlign == 0
            aMenu[nI,1] := " " + PADR(cTxt,nL)
         ELSEIF nTxtAlign == 2
            aMenu[nI,1] := PADL(cTxt,nL)
         ELSEIF nTxtAlign == 3
            aMenu[nI,1] := ALLTRIM(cTxt)
         ENDIF
      ENDIF
   NEXT

RETURN aMenu

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION  myChangeMenuColor(aMenu)  // Изменить цвета фона кнопки
   LOCAL nI, nK, aClr1, aClr2, aClr3, aVal

   nK  := Len(aMenu)
   FOR nI := 1 TO nK
      aVal  := aMenu[nI]
      IF LEN(aVal) >= 5
         aClr1 := aVal[3]
         aClr2 := aVal[4]
         aClr3 := aVal[5]
         // фон кнопки
         IF !HB_IsArray(aClr1)
            aMenu[nI,3] := GRAY
         ENDIF
         IF !HB_IsArray(aClr2)
            aMenu[nI,4] := BLACK
         ENDIF
         IF !HB_IsArray(aClr3)
            aMenu[nI,5] := SILVER
         ENDIF
      ENDIF
   NEXT

RETURN aMenu

///////////////////////////////////////////////////////////////////////////////////
// проверка Avi на окне
STATIC FUNCTION CheckAviWinSize(aMenu, nW, nH)
   LOCAL nI, lRet, cResAvi, aAviSz, cMsg, cStr, aVal

   nW   := nH := 0
   lRet := .F.
   cMsg := ""

   FOR nI := 1 TO LEN(aMenu)
      aVal  := aMenu[nI]
      IF LEN(aVal) >= 5
         cResAvi := aVal[2]
         IF HB_ISCHAR(cResAvi)
            IF LEN(ALLTRIM(cResAvi)) > 0
               aAviSz := GetAviResSize(cResAvi)
               IF aAviSz[1] == NIL .OR. aAviSz[2] == NIL
                  cMsg += cResAvi + ";"
                  aMenu[nI,2] := ""
               ELSE
                  nW   := MAX(aAviSz[1], nW)
                  nH   := MAX(aAviSz[2], nH)
                  lRet := .T.   // есть AVI на окне
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   IF LEN(cMsg) > 0
      cStr := "Error! There is no resource in the exe-file !;"
      AlertStop(cStr + cMsg, App.ExeName)
      ? "===[] " + ProcNL(), cStr + cMsg
   ENDIF

RETURN lRet

