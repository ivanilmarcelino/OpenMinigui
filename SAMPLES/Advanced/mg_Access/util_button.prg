/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 *  нопки с градиентом / Gradient buttons
*/

#include "minigui.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION my2DrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aColor)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, aFClr2, aFClr1, cTtipt, aRetIco
   LOCAL cIco1x2, cIco1x1, nSizeIc, aGrFill, aGrOver, hIco1, hIco2, bZero

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
   aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aRetIco := {}        // вернуть хендлы иконок, если нужно

   IF LEN(cIco1x1) > 0
      hIco1 := LoadIconByName(cIco1x1, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco1 )
   ENDIF
   IF LEN(cIco1x2) > 0
      hIco2 := LoadIconByName(cIco1x2, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco2 )
   ENDIF

   @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
     ICON hIco1                                      ;
     WIDTH nWBtn HEIGHT nHBtn                        ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/     ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
     BACKCOLOR aGrOver GRADIENTFILL aGrFill          ;
     TOOLTIP cTtipt                                  ;
     ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.GradientFill := aGrFill ) ;
     ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.GradientOver := aGrOver ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aColor }  }
     //ACTION Eval(bAction) - не надо так

   bZero := bAction
   //This.&(cObj).Action   := bAction - перенес на форму
   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

RETURN aRetIco  // вернуть хендлы иконок дл€ удалени€ с формы
