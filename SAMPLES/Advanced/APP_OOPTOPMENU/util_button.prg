/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * 30.09.2024 - 07.11.2024
 * Кнопки с градиентом / Gradient buttons
 * Кнопки без градиента / Buttons without gradient
 * (4х) Кнопки без градиента - 2 кнопки на одной
 * (4х) Buttons without gradient - 2 buttons on one
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
///////////////////////////////////////////////////////////////////////////////
// Кнопки с градиентом / Gradient buttons
FUNCTION my2DrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aColor)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, aFClr2, aFClr1, cTtipt, aRetIco
   LOCAL cIco1x2, cIco1x1, nSizeIc, aGrFill, aGrOver, hIco1, hIco2, lVert

   // aBtn := { "Btn_Dir", cTxt, "iDir48x3", "iDir48x2", nHIco, aBtnFClr, aBtnFont, Tooltipt, lVert }
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
   IF LEN(aBtn) < 9
      lVert := .F.       // НЕ вертикальный текст
   ELSE
      lVert := aBtn[9]   // вертикальный текст
   ENDIF
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

   IF lVert  // вертикальный текст на кнопке
      @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
        ICON hIco1                                      ;
        WIDTH nWBtn HEIGHT nHBtn                        ;
        NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL         ;
        FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill          ;
        TOOLTIP cTtipt                                  ;
        ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.GradientOver := aGrOver ) ;
        ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aColor }  }
   ELSE
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
   ENDIF

   This.&(cObj).Action   := bAction
   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

RETURN aRetIco  // вернуть хендлы иконок для удаления с формы

///////////////////////////////////////////////////////////////////////////////
// Кнопки без градиента / Buttons without gradient
FUNCTION my2DrawButton2(nRow, nCol, nWBtn, nHBtn, aBtn)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, cTtipt, aRetIco, aBClr1, aBClr2
   LOCAL aFClr1, aFClr2, hIco1, hIco2
   LOCAL cForm := _HMG_ThisFormName

   //? ProcNL() ; ? HB_ValToExp(aBtn)
   cObj    := aBtn[1]
   cCapt   := aBtn[2]
   hIco1   := aBtn[3,1]
   hIco2   := aBtn[3,2]
   aBClr1  := aBtn[4,1]
   aBClr2  := aBtn[4,2]
   aFClr1  := aBtn[5,1]
   aFClr2  := aBtn[5,2]
   cFont   := aBtn[6,1]
   nFSize  := aBtn[6,2]
   lBold   := aBtn[6,3]
   cTtipt  := aBtn[7]

   @ nRow, nCol BUTTONEX &cObj PARENT &cForm CAPTION cCapt ;
     ICON hIco1 FLAT                                       ;
     WIDTH nWBtn HEIGHT nHBtn                              ;
     NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL               ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize               ;
     BACKCOLOR aBClr1 /*GRADIENTFILL aGrFill*/             ;
     TOOLTIP cTtipt                                        ;
     ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.Backcolor := aBClr2 ) ;
     ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.Backcolor := aBClr1 )

     This.&(cObj).Icon     := hIco1
     This.&(cObj).FontBold := lBold

RETURN aRetIco

///////////////////////////////////////////////////////////////////////////////
// (4х) Кнопки без градиента - 2 кнопки на одной
//  NBTN  =  2
//  AICO  =  {"i1Search48x1", "i1Search48x2", "i1Clear48x1", "i1Clear48x2"}
// AHICO  =  {107809011, 33751319, 56164449, 64029491}
// NHICO  =  32
// AFCLR  =  {{255, 255, 255}, {0, 0, 0}, {255, 0, 0}, {0, 0, 0}}
// ABCLR  =  {{109, 1, 1}, {255, 255, 255}, {255, 255, 0}, {128, 128, 128}}
//  CTXT  =  {e"Поиск\r\nоператора", e"Очистить\r\nпоиск"}
//  ATXT  =  {e"Поиск\r\nоператора", e"Очистить\r\nпоиск"}
//  NDIM  =  2
//    NI  =  1
//  POST  =  _Search
Function RefreshButton(cn)
   Local ocb := This.&(cn).Cargo
   Local bMouseHover, bMouseLeave

   //_o2log(ocb, 25, ProcNL() + "  o => This.Cargo: " + cn, .T. ) ; ?
   //? "ocb:aFClr=", ocb:aFClr, HB_ValToExp(ocb:aFClr)
   IF ocb:nI == 1
      This.&(cn).Fontcolor := ocb:aFClr[1]
      This.&(cn).Backcolor := ocb:aBClr[1]
      This.&(cn).Caption   := ocb:aTxt[1]
      This.&(cn).Icon      := ocb:aIco[1]
      bMouseHover := {|| This.Backcolor := This.Cargo:aBClr[2], This.Fontcolor := This.Cargo:aFClr[2] ,;
                         This.Icon := This.Cargo:ahIco[2] }
      bMouseLeave := {|| This.Backcolor := This.Cargo:aBClr[1], This.Fontcolor := This.Cargo:aFClr[1] ,;
                         This.Icon := This.Cargo:ahIco[1] }
   ELSE
      This.&(cn).Fontcolor := ocb:aFClr[3]
      This.&(cn).Backcolor := ocb:aBClr[3]
      This.&(cn).Caption   := ocb:aTxt[2]
      This.&(cn).Icon      := ocb:aIco[3]
      bMouseHover := {|| This.Backcolor := This.Cargo:aBClr[4], This.Fontcolor := This.Cargo:aFClr[4] ,;
                         This.Icon := This.Cargo:ahIco[4] }
      bMouseLeave := {|| This.Backcolor := This.Cargo:aBClr[3], This.Fontcolor := This.Cargo:aFClr[3] ,;
                         This.Icon := This.Cargo:ahIco[3] }
   ENDIF
   This.&(cn).OnGotFocus  := bMouseHover
   This.&(cn).OnLostFocus := bMouseLeave
   This.&(cn).Enabled     := .T.
   This.&(cn).Setfocus
   DO EVENTS

RETURN NIL

