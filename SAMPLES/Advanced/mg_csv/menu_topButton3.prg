/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Changed 07.10.25
 *
 * Верхнее меню окна с кнопками / Top window menu with buttons
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
//////////////////////////////////////////////////////////////////////////////
FUNCTION SampleMenuData()  // в качестве примера / as an example
   LOCAL oMenu := oHmgData()
   oMenu:aObj   := { "_ATable" , "_AExport", "_AExit"  }
   oMenu:aIco   := { {"iATable32x1","iATable32x2"} , {"iAExport32x1","iAExport32x2"} , {"iAExit32x1","iAExit32x2"} }
   oMenu:aMnRu  := { "Таблица" , "Экспорт", "Выход" }
   oMenu:aMnEn  := { "Table"   , "Export" , "Exit"  }
   oMenu:aTipRu := { "Список таблиц" , "Экспорт таблицы", "Выход из программы" }
   oMenu:aTipEn := { "List of tables", "Export table"   , "Exit program" }
   oMenu:aCap   := IIF( App.Cargo:lRu, oMenu:aMnRu , oMenu:aMnEn  )
   oMenu:aTtip  := IIF( App.Cargo:lRu, oMenu:aTipRu, oMenu:aTipEn )
   oMenu:aFont  := { "Comic Sans MS", 12, .T., .F. , 14, "Increase button font size - reserve" }
   oMenu:aFClr  := { BLACK  , YELLOW }
   oMenu:aBClr  := { LGREEN , BLACK  }
   oMenu:nHIco  := 64          // 32,55  - задаём размер картинки на кнопке
   oMenu:nHIco  := IIF( App.Cargo:aDisplayMode[2] <= 720, 28, oMenu:nHIco )
   oMenu:nG     := IIF( App.Cargo:aDisplayMode[2] <= 720, 5, 10 )
   oMenu:nY     := oMenu:nG
   oMenu:nX     := oMenu:nG
   oMenu:nWBtn  := GetTxtWidth( oMenu:aCap[1], oMenu:aFont[2], oMenu:aFont[1], oMenu:aFont[3] )
   oMenu:nWBtn  += oMenu:nG*2 + oMenu:nHIco + oMenu:nG * 3
   oMenu:nHBtn  := oMenu:nHIco + oMenu:nG
   oMenu:lCaptu := .T.                                 // кнопка с надписями / button with inscriptions
   oMenu:nHMenu := oMenu:nY + oMenu:nHBtn + oMenu:nG   // высота вернего меню кнопок
   oMenu:lVertText := .F.                              // НЕ вертикальный текст
RETURN oMenu
//////////////////////////////////////////////////////////////////////////////
// Использование: / Usage:
//      oMenu := SampleMenuData()
//      TopMenuButtons(owc,oMenu,nG,nG,96-nG*2,96-nG*2,nG)
//
//////////////////////////////////////////////////////////////////////////////
FUNCTION TopMenuButtons(owc,oMenu,nY,nX,nWBtn,nHBtn,nG)
   LOCAL hFont, aFont, cFont, nFSize, lBold, aIco, aObj, cCap
   LOCAL nWtxt, nWCap, cObj, aCap, cForm, aBtnObj, cErr, i, o
   LOCAL aIcon, aHIco, bAct, aBtnFClr, aBtnBClr, aBtnFont, cTxt
   LOCAL lVertText, nHIco, aBtn, cTtip, aTtip

   IF !IsObject(owc)  // owc - это Cargo окна откуда вызывается функция
      cErr := "ERROR ! No (owc) - Object !;;"
      cErr += ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      RETURN NIL
   ENDIF

   IF !IsObject(oMenu)
      cErr := "ERROR ! No (oMenu) - Object !;;"
      cErr += ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      RETURN NIL
   ENDIF
   DEFAULT nY := oMenu:nY, nX := oMenu:nX, nWBtn := oMenu:nWBtn
   DEFAULT nHBtn := oMenu:nHBtn, nG := oMenu:nG

   cForm    := owc:cForm                   // имя окна
   IF !IsString(oMenu:hFont)
      hFont := GetFontHandle('ItalBold')
      aFont := GetFontParam(hFont)
   ENDIF
   IF IsArray(oMenu:aFont)
      aFont := oMenu:aFont
   ENDIF
   cFont    := aFont[1]
   nFSize   := aFont[2]
   lBold    := aFont[3]
   aBtnFont := { cFont, nFSize, lBold }
   //
   IF IsArray(oMenu:aFClr) ; aBtnFClr := oMenu:aFClr
   ELSE                    ; aBtnFClr := { WHITE   , BLACK }
   ENDIF
   IF IsArray(oMenu:aBClr) ; aBtnBClr := oMenu:aBClr
   ELSE                    ; aBtnBClr := { WHITE   , BLACK }
   ENDIF
   //
   IF IsNumeric(oMenu:nHIco) ; nHIco := oMenu:nHIco
   ELSE
      nHIco := 32
      nHIco := IIF( App.Cargo:aDisplayMode[2] <= 720, 28, nHIco )
   ENDIF
   //

   IF !IsLogic(oMenu:lVertText)
      lVertText := .T.       // вертикальный текст
   ELSE
      lVertText := oMenu:lVertText  // задан в меню
   ENDIF

   aObj    := oMenu:aObj
   aIco    := oMenu:aIco
   aCap    := oMenu:aCap
   aTtip   := oMenu:aTtip
   aBtnObj := {}

   // кнопки без надписей
   IF IsLogic(oMenu:lCaptu) .AND. !oMenu:lCaptu
      AFILL( aCap, "" )
   ENDIF

   // расчёт по тексту кнопки
   nWtxt := 0
   FOR i := 1 TO LEN(aCap)
      cCap  := aCap[ i ]
      nWCap := GetTxtWidth(cCap, nFSize, cFont, lBold )
      //nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt += 10
   //
   nWBtn := IIF(nWTxt > nWBtn, nWTxt, nWBtn )   // ширина кнопки
   //nHBtn := nHIco + 5 + nFSize + 5 + 20       // высота кнопки

   FOR i := 1 TO LEN(aCap)

      cObj  := aObj[i]    // контрол на окне
      cTxt  := aCap[i]
      aIcon := aIco[i]
      IF IsArray(aTtip) ; cTtip := aTtip[i]
      ELSE              ; cTtip := ""
      ENDIF
      aBtn  := { cObj, cTxt, aIcon[1], aIcon[2], nHIco, aBtnFClr, aBtnFont, cTtip }
      ahIco := my2DrawButton(nY, nX, nWBtn, nHBtn, aBtn, bAct, aBtnBClr, lVertText)

      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         IF !IsArray(owc:ahIcoDel)
             owc:ahIcoDel := {}
         ENDIF
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
      o:nBtn := i   ; o:cImage := aIco[i]   // пример
      o:Post := cObj                        // событие на форме

      This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:Post, , This.Name) }

      AADD( aBtnObj, { i, cObj, "-object name", aCap[i], nY, nX, This.&(cObj).Width, nHBtn, cObj, "-event", aIco, This.&(cObj).Cargo } )

      nX += This.&(cObj).Width + nG

   NEXT

   owc:nWBtnEnd := nX + nG         // конец кнопок
   owc:nHTBar   := nHBtn + nG*2    // высота ToolBar
   owc:aBtnObj  := aBtnObj         // массив кнопок на форме
   //? ProcNL() ; ? "owc:aBtnObj=", aBtnObj ; ?v aBtnObj

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION my2DrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aBColor, lVertText)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, cTtipt, aRetIco, aBClr1, aBClr2
   LOCAL cIco1x2, cIco1x1, nSizeIc, aFClr1, aFClr2, hIco1, hIco2
   DEFAULT lVertText := .T.

   // aBtn := { "Btn_Dir", cTxt, "iDir48x3", "iDir48x2", nHIco, aBtnFClr, aBtnFont, cVal }
   cObj    := aBtn[1]
   cCapt   := aBtn[2]
   cIco1x1 := aBtn[3]
   cIco1x2 := aBtn[4]
   nSizeIc := aBtn[5]
   aFClr1  := aBtn[6,1]
   aFClr2  := aBtn[6,2]
   cFont   := aBtn[7,1]
   nFSize  := aBtn[7,2]
   lBold   := aBtn[7,3]
   cTtipt  := aBtn[8]
   aRetIco := {}         // вернуть хендлы иконок, если нужно
   bAction := {|| Nil }  // резерв
   aBClr1  := aBColor[1]
   aBClr2  := aBColor[2]

   IF LEN(cIco1x1) > 0
      hIco1 := LoadIconByName(cIco1x1, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco1 )
   ENDIF
   IF LEN(cIco1x2) > 0
      hIco2 := LoadIconByName(cIco1x2, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco2 )
   ENDIF

   IF lVertText  //  VERTICAL
      @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
        ICON hIco1 FLAT                                 ;
        WIDTH nWBtn HEIGHT nHBtn                        ;
        NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL         ;
        FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
        BACKCOLOR aBClr1 /*GRADIENTFILL aGrFill*/       ;
        TOOLTIP cTtipt                                  ;
        ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.Backcolor := aBClr2 ) ;
        ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.Backcolor := aBClr1 )
   ELSE
      @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
        ICON hIco1 FLAT                                 ;
        WIDTH nWBtn HEIGHT nHBtn                        ;
        NOXPSTYLE HANDCURSOR NOTABSTOP                  ;
        FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
        BACKCOLOR aBClr1 /*GRADIENTFILL aGrFill*/       ;
        TOOLTIP cTtipt                                  ;
        ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.Backcolor := aBClr2 ) ;
        ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.Backcolor := aBClr1 )
   ENDIF

   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

RETURN aRetIco

///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

///////////////////////////////////////////////////////////////////////////////
// получить Width текста
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth

