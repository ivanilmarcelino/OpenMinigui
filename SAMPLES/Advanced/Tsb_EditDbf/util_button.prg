/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2019-2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2015-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
 * Edit 05.05.25
 *
*/
#include "minigui.ch"
// взята из проекта hbpMergeProjects2 - 28.07.23 / переделана 20.04.25
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION my2BUTTON(y, x, w, h, cObj, cCapt, aBtnGrad, aBtnClr, aIcon, aFntClr, aFnt, nwPost, lBlock, lHide, i )
   LOCAL aGrOver, aGrFill, nSizeIcon, lSizeIcon, y1, x1, lTextVertical, o
   LOCAL cForm := _HMG_ThisFormName
   DEFAULT cCapt    := "" , aFntClr := {  BLACK, YELLOW }
   DEFAULT aFnt     := { "Tahona", 12 , .T. , .F. } , aIcon := {"Icon1x1","Icon1x1",.F.,48}
   DEFAULT aBtnGrad := {} , aBtnClr := { BLUE, YELLOW }
   DEFAULT lBlock   := .F. // не блокировать кнопки
   DEFAULT lHide    := .F. // показывать и не скрывать кнопки

   IF LEN(aFnt) == 4
      lTextVertical := aFnt[4]  //  VERTICAL
   ELSE
      lTextVertical := .F.
   ENDIF

   IF LEN(aIcon) < 3  ;  lSizeIcon := .F.
   ELSE               ;  lSizeIcon := aIcon[3]
   ENDIF
   IF LEN(aIcon) < 4  ;  nSizeIcon := 32
   ELSE               ;  nSizeIcon := aIcon[4]
   ENDIF

   IF LEN(aBtnGrad) > 0

      aGrOver := { { 0.5, aBtnGrad[2], aBtnGrad[1] }, { 0.5, aBtnGrad[1], aBtnGrad[2] } }
      aGrFill := { { 0.5, aBtnGrad[1], aBtnGrad[2] }, { 0.5, aBtnGrad[2], aBtnGrad[1] } }

      IF lSizeIcon
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj PARENT &cForm                                      ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                            ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              FONTCOLOR aFntClr[1] BACKCOLOR aGrOver GRADIENTFILL aGrFill             ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD  VERTICAL                               ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Fontcolor := aFntClr[2]  ,;
                              This.Icon := LoadIconByName(aIcon[2], nSizeIcon, nSizeIcon) ) ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Fontcolor := aFntClr[1]  ,;
                              This.Icon := LoadIconByName(aIcon[1], nSizeIcon, nSizeIcon) )
         ELSE
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                     ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                            ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              FONTCOLOR aFntClr[1] BACKCOLOR aGrOver GRADIENTFILL aGrFill             ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD                                         ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Fontcolor := aFntClr[2]  ,;
                              This.Icon := LoadIconByName(aIcon[2], nSizeIcon, nSizeIcon) ) ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Fontcolor := aFntClr[1]  ,;
                              This.Icon := LoadIconByName(aIcon[1], nSizeIcon, nSizeIcon) )
         ENDIF
         // при первом построении изменить размер иконки
         This.&(cObj).Icon := LoadIconByName( aIcon[1], nSizeIcon, nSizeIcon )
      ELSE
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                     ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                            ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              FONTCOLOR aFntClr[1] BACKCOLOR aGrOver GRADIENTFILL aGrFill             ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD  VERTICAL                               ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )
         ELSE
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                     ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                            ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              FONTCOLOR aFntClr[1] BACKCOLOR aGrOver GRADIENTFILL aGrFill             ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD                                         ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )
         ENDIF
      ENDIF

   ELSE

      IF lSizeIcon
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                      ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                             ;
              NOXPSTYLE HANDCURSOR NOTABSTOP FONTCOLOR aFntClr[1] BACKCOLOR aBtnClr[1] ;
              FONT aFnt[1] SIZE aFnt[2] BOLD VERTICAL                                  ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Fontcolor := aFntClr[2] ,;
                              This.Icon := LoadIconByName(aIcon[2],nSizeIcon,nSizeIcon) );
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Fontcolor := aFntClr[1] ,;
                              This.Icon := LoadIconByName(aIcon[1],nSizeIcon,nSizeIcon) )
         ELSE
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                      ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                             ;
              NOXPSTYLE HANDCURSOR NOTABSTOP FONTCOLOR aFntClr[1] BACKCOLOR aBtnClr[1] ;
              FONT aFnt[1] SIZE aFnt[2] BOLD                                           ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Fontcolor := aFntClr[2] ,;
                              This.Icon := LoadIconByName(aIcon[2],nSizeIcon,nSizeIcon) );
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Fontcolor := aFntClr[1] ,;
                              This.Icon := LoadIconByName(aIcon[1],nSizeIcon,nSizeIcon) )
         ENDIF
         // при первом построении изменить размер иконки
         This.&(cObj).Icon := LoadIconByName( aIcon[1], nSizeIcon, nSizeIcon )
      ELSE
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                       ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                              ;
              NOXPSTYLE HANDCURSOR NOTABSTOP  FONTCOLOR aFntClr[1] BACKCOLOR aBtnClr[1] ;
              FONT aFnt[1] SIZE aFnt[2] BOLD  VERTICAL                                  ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                           ;
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )
         ELSE
            @ y, x  BUTTONEX &cObj  PARENT &cForm                                      ;
              WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]                             ;
              NOXPSTYLE HANDCURSOR NOTABSTOP FONTCOLOR aFntClr[1] BACKCOLOR aBtnClr[1] ;
              FONT aFnt[1] SIZE aFnt[2] BOLD                                           ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                          ;
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )
         ENDIF
      ENDIF
   ENDIF

   // создадим This.Cargo
   This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
   o:nBtn  := i
   o:aIco  := aIcon
   o:cTxt  := cCapt
   o:aFClr := aBtnClr
   o:aGrad := aBtnGrad
   o:Post  := cObj       // номер - события на форме
   o:nPost := nwPost     // строка - событие на форме
   o:cBtn  := This.Name
   o:cCapt := This.Caption
   o:aBClr := This.BackColor
   o:nY    := This.Row
   o:nX    := This.Col

   IF lBlock
     //This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo, , This.Name) }
   ENDIF
   // переназначим aBtnPst на aBtnObj
   This.&(cObj).Action := {|| This.Enabled := .F., DoEvents() ,;
                              _LogFile(.T.," $$$$$ =>", This.Name, This.Cargo, ThisWindow.Name, This.Cargo:Post) ,;
                             _wPost(This.Cargo:Post, , This.Name) }

   IF lHide
      SetProperty(cForm, cObj, "Visible", .F. )
   ENDIF

   y1 := y + This.&(cObj).Height
   x1 := x + This.&(cObj).Width

RETURN { y1, x1 }

///////////////////////////////////////////////////////////////////////////////////
FUNCTION Draw_BtnEx( y, x, oBtn, nWBtn, nHBtn, nGBtn, lRow )
   LOCAL cObj, cCap, cIco, nPst, aClr, lBlk, aFntClr, aGradClr, nLen, nK, cMsg
   LOCAL i, w, h, o := oBtn
   DEFAULT oBtn:nWBtn := 160
   DEFAULT oBtn:nHBtn :=  60
   DEFAULT oBtn:nGBtn :=  20
   DEFAULT nWBtn := oBtn:nWBtn
   DEFAULT nHBtn := oBtn:nHBtn
   DEFAULT nGBtn := oBtn:nGBtn
   DEFAULT oBtn:aFnt := { "Comic Sans MS", _HMG_DefaultFontSize , .T. , .F. }
   DEFAULT oBtn:aFntClr  := { BLACK, YELLOW }
   DEFAULT oBtn:aGradClr := WHITE
   DEFAULT oBtn:aPst     := {}
   DEFAULT lRow := .T.    // кнопки по горизонтали

   oBtn:nWBtn := nWBtn
   oBtn:nHBtn := nHBtn
   oBtn:nGBtn := nGBtn

   aFntClr  := o:aFntClr
   aGradClr := o:aGradClr
   w := o:nWBtn
   h := o:nHBtn
   nLen := Len(o:aCap)
   DEFAULT o:aObj := array(nLen)
   IF Empty(o:aWBtn)
      o:aWBtn := array(nLen) ; aFill(o:aWBtn, w)  // ширина кнопок
   ENDIF
   IF Empty(o:aBlk)
      o:aBlk := array(nLen) ; aFill(o:aBlk, .T.)  // блокировать кнопки при нажатии
   ENDIF
   //? "[[[[[[[[[[[[[[", ProcNL(), HB_ValToExp(oBtn:aFnt)
   nK := LEN(o:aCap)
   IF nK == LEN(o:aObj) .AND. nK == LEN(o:aIco) .AND. ;
      nK == LEN(o:aClr) .AND. nK == LEN(o:aBlk) .AND. nK == LEN(o:aWBtn)
   ELSE
      cMsg := "ERROR !;Arrays are not the same !;;"
      cMsg += "LEN(o:aObj)=" + HB_NtoS(LEN(o:aObj)) + ";"
      cMsg += "LEN(o:aCap)=" + HB_NtoS(LEN(o:aCap)) + ";"
      //cMsg += "LEN(o:aPst)=" + HB_NtoS(LEN(o:aPst)) + ";"
      cMsg += "LEN(o:aClr)=" + HB_NtoS(LEN(o:aClr)) + ";"
      cMsg += "LEN(o:aBlk)=" + HB_NtoS(LEN(o:aBlk)) + ";"
      cMsg += "LEN(o:aWBtn)=" + HB_NtoS(LEN(o:aWBtn))
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      ? ProcNL(), "------ ERROR !;Arrays are not the same ! --------"
      ? "o:aObj )="  , o:aObj  , HB_ValToExp(o:aObj )
      ? "o:aCap )="  , o:aCap  , HB_ValToExp(o:aCap )
      //? "o:aPst )="  , o:aPst  , HB_ValToExp(o:aPst )
      ? "o:aClr )="  , o:aClr  , HB_ValToExp(o:aClr )
      ? "o:aBlk )="  , o:aBlk  , HB_ValToExp(o:aBlk )
      ? "o:aWBtn)="  , o:aWBtn , HB_ValToExp(o:aWBtn)
   ENDIF

   //FOR EACH cObj, cCap, cIco, nPst, aClr, lBlk, w IN o:aObj, o:aCap, o:aIco, o:aPst, o:aClr, o:aBlk, o:aWBtn
   FOR EACH cObj, cCap, cIco, aClr, lBlk, w IN o:aObj, o:aCap, o:aIco, o:aClr, o:aBlk, o:aWBtn
       i := hb_EnumIndex(cObj)
       //? i, cObj, cCap
       //DEFAULT cObj := "Btn_"+StrZero(hb_EnumIndex(cObj), 2)
       IF ";" $ cCap ; cCap := StrTran( cCap, ";", CRLF )
       ENDIF

       my2BUTTON(y, x, w, h, cObj, cCap, {aClr, aGradClr}, , cIco, aFntClr, oBtn:aFnt, nPst, lBlk, , i )  // цифры - событие

       IF lRow ; x += This.&(cObj).Width  + o:nGBtn
       ELSE    ; y += This.&(cObj).Height + o:nGBtn
       ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aColor)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, aFClr2, aFClr1
   LOCAL cIco1x2, cIco1x1, nSize, aGrFill, aGrOver

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

   @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
     ICON LoadIconByName( cIco1x1, nSize, nSize )    ;
     WIDTH nWBtn HEIGHT nHBtn                        ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/     ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
     BACKCOLOR aGrOver GRADIENTFILL aGrFill          ;
     ON MOUSEHOVER ( This.Icon := LoadIconByName( cIco1x2, nSize, nSize ) ,;
                     This.Fontcolor := aFClr2, This.GradientFill := aGrFill  ) ;
     ON MOUSELEAVE ( This.Icon := LoadIconByName( cIco1x1, nSize, nSize ) ,;
                     This.Fontcolor := aFClr1, This.GradientOver := aGrOver ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aColor }  }
     //ACTION Eval(bAction) - не надо так

   This.&(cObj).Action   := bAction
   This.&(cObj).Icon     := LoadIconByName( cIco1x1, nSize, nSize )
   This.&(cObj).FontBold := lBold

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAction)

   @ nRow, nCol BUTTONEX &(aBtn[1]) CAPTION aBtn[2] ;
     ICON LoadIconByName( aBtn[3], aBtn[5], aBtn[5] ) ;
     WIDTH nWBtn HEIGHT nHBtn BACKCOLOR aBtn[6] BOLD ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/    ;
     ON MOUSEHOVER ( This.Icon := LoadIconByName( aBtn[4], aBtn[5], aBtn[5] ) ) ;
     ON MOUSELEAVE ( This.Icon := LoadIconByName( aBtn[3], aBtn[5], aBtn[5] ) ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name }  } ;
     ACTION Eval(bAction)

   This.&(aBtn[1]).Icon := LoadIconByName( aBtn[3], aBtn[5], aBtn[5] )

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myScreenIconSize(nH)     // высота иконки от экрана
   LOCAL nSize := 16

   //RETURN { "640x480"  ,  "768x560",  "800x600", "1024x768", "1280x720" ,;
   //         "1280x768" , "1280x800", "1280x960","1280x1024", "1360x768" ,;
   //         "1366x768" ,"1440x1080", "1600x900","1600x1024", "1680x1054",;
   //         "1940x1080","1920x1440","2560x1440","2580x1080", "3440x1440",;
   //         "3840x2160" }

   IF nH <= 600
      nSize := 16
   ELSEIF nH > 600 .AND. nH < 768
      nSize := 24
   ELSEIF nH >= 768 .AND. nH < 800
      nSize := 24
   ELSEIF nH >= 800 .AND. nH < 960
      nSize := 32
   ELSEIF nH >= 960 .AND. nH < 1024
      nSize := 48
   ELSEIF nH >= 1024 .AND. nH < 1440
      nSize := 64
   ELSEIF nH >= 1440 .AND. nH < 1600
      nSize := 72
   ELSE
      nSize := 96
   ENDIF

RETURN nSize
