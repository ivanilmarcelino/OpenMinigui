/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Верхнее меню окна с кнопками / Top window menu with buttons
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
//////////////////////////////////////////////////////////////////////////////
FUNCTION SampleMenuData()  // в качестве примера
   LOCAL lRus, cLng, oMenu := oHmgData()
   cLng        := IIF( App.Cargo:cLang == NIL, "EN", "RU" )
   lRus        := IIF( cLng == "RU", .T., .F. )
   oMenu:aObj  := { "_ATable" , "_AExport", "_AExit"  }
   oMenu:aImg  := { {"iATable32x1","iATable32x2"} , {"iAExport32x1","iAExport32x2"} , {"iAExit32x1","iAExit32x2"} }
   oMenu:aMnRu := { "Таблица" , "Экспорт", "Выход" }
   oMenu:aMnEn := { "Table"   , "Export" , "Exit"  }
   oMenu:aCap  := IIF( lRus, oMenu:aMnRu, oMenu:aMnEn )
RETURN oMenu

//////////////////////////////////////////////////////////////////////////////
FUNCTION TopWindowsMenuButtons(owc)
   LOCAL nW, nH, nX, nY, nG2, hFont, aFont, cFont, nFSize, lBold, aImg, aObj
   LOCAL nWBtn, nHBtn, cCap, nWtxt, nWCap, cObj, aCap, cForm, aBtnObj, i, o
   LOCAL aIco, aHIco, bAct, aBtnFClr, aBtnBClr, aBtnFont, cTxt, nHIco, aBtn
   LOCAL aBColor, cErr

   IF IsObject(owc)  // owc - это Cargo окна откуда вызывается функция
      cErr := "ERROR ! No (owc) - Object !;;"
      cErr += ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      RETURN NIL
   ENDIF

   IF owc:oMenu == NIL    // верхнее меню
      cErr := "ERROR ! owc:oMenu == NIL !;;"
      cErr += ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      RETURN NIL
   ENDIF

   cForm    := oWC:cForm                   // имя окна
   hFont    := GetFontHandle('ItalBold')
   aFont    := GetFontParam(hFont)
   cFont    := aFont[1]
   nFSize   := aFont[2]
   lBold    := aFont[3]
   aBColor  := owc:aBColor
   aBtnFClr := { WHITE   , BLACK }
   aBtnBClr := { aBColor , WHITE }
   aBtnFont := { cFont, nFSize, .T. }
   nHIco    := 32          // 32,55  - задаём размер картинки на кнопке
   nG2      := 5
   aBtnObj  := {}

   IF App.Cargo:aDisplayMode[2] <= 720
      nHIco  := 32
   ENDIF

   aObj := owc:oMenu:aObj
   aImg := owc:oMenu:aImg
   aCap := owc:oMenu:aCap

   // расчёт по тексту
   nWtxt  := nW := nH := 0
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHIco, nHIco, nWTxt )   // nHImg-высота bmp
   nWBtn := nWTxt + 20                          // ширина кнопки
   nHBtn := nHIco + 5 + nFSize + 5 + 20         // высота кнопки

   nY := nG2
   nX := owc:nG
   FOR i := 1 TO LEN(aCap)

      cObj  := aObj[i]    // контрол на окне
      cTxt  := aCap[i]
      aIco  := aImg[i]
      aBtn  := { cObj, cTxt, aIco[1], aIco[2], nHIco, aBtnFClr, aBtnFont, "" }
      //bAct  := { || _wPostaObj[i]  // событие на форме
      ahIco := my2DrawButton(nY, nX, nWBtn, nHBtn, aBtn, bAct, aBtnBClr )
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
      o:nBtn := i   ; o:cImage := aImg[i]   // пример
      o:Post := cObj                        // событие на форме

      This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:Post, , This.Name) }

      AADD( aBtnObj, { i, cObj, "-имя объекта", aCap[i], 0, nW, This.&(cObj).Width, nHBtn, cObj, "-событие", This.&(cObj).Cargo } )

      nX += This.&(cObj).Width + nG2

   NEXT

   nW := nX + nG2                 // + owc:nG
   owc:nWEndTB := nW              // конец кнопок
   owc:nHTBar  := nHBtn + nG2*2   // высота ToolBar
   owc:aBtnObj := aBtnObj         // массив кнопок на форме
   ? ProcNL() ; ? "owc:aBtnObj=", aBtnObj ; ?v aBtnObj

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION my2DrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aBColor)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, cTtipt, aRetIco, aBClr1, aBClr2
   LOCAL cIco1x2, cIco1x1, nSizeIc, aFClr1, aFClr2, hIco1, hIco2

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

   @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
     ICON hIco1 FLAT                                 ;
     WIDTH nWBtn HEIGHT nHBtn                        ;
     NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL         ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
     BACKCOLOR aBClr1 /*GRADIENTFILL aGrFill*/       ;
     TOOLTIP cTtipt                                  ;
     ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.Backcolor := aBClr2 ) ;
     ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.Backcolor := aBClr1 )
     //ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aBColor }  }
     //ACTION Eval(bAction) - не надо так

   //This.&(cObj).Action := bAction - назначается выше
   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

RETURN aRetIco

