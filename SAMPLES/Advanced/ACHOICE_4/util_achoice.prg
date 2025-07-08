/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте ButtonEx / мenu on the ButtonEx object
*/
#include "hmg.ch"
#define MYRED     { 211, 107, 107 }
#define MYBACK    {  10, 141, 115 }
#define MYBUTTON  {   6, 175, 143 }

STATIC bStaticBtnHoverIcoGrad, bStaticBtnLeaveIcoGrad, bStaticBtnHoverIco, bStaticBtnLeaveIco
STATIC bStaticBtnHoverGrad, bStaticBtnLeaveGrad, bStaticBtnHover, bStaticBtnLeave
//////////////////////////////////////////////////////////////////////////////
FUNCTION MenuAchoice(aMenu, aParWin, aParMenu, aParIco, aIcoWin, lTxtAlign)
   LOCAL nY, nX, nK, nL, nH, nW, nG, nI, nWBtn, cIco, cTitle, cTxt
   LOCAL aIcon, aIcoExit, aFntClr, aBtnBClr, aRet, cFont, nFSize, nColBtn
   LOCAL aBtnGrad, nIcoAlign, lBtnIco, cFontExit, aBackColor, cForm
   LOCAL nIcoSize, lTextLeft, lTextVert, lBtnFBold, lBtnHide, lWinImg
   LOCAL lBtnGrad, lBtnFlat, lBtnUpText, nRow, nCol, nHeight, nWidth
   LOCAL lGrdHoriz, nHBtn, lBtnExit, nHWin, nWVirt, nHVirt, nHDsktp
   LOCAL cImgRes, nImgSz, nImgY, nImgX, nImgAlign, aDim, nLine, nJ
   DEFAULT aParWin  := { "Form_Achoice", "Menu-Achoice", App.Cargo:cIconDef, MYBACK, 0, 0, 0, 0 }
   DEFAULT aParMenu := { 48 + 6, 20, 20, .T., .F. }    // параметры меню
   DEFAULT aMenu    := { "No menu array" }
   DEFAULT aIcoWin  := {}   // нет иконки на форме
   DEFAULT aParIco  := {}   //{ {}, aIcoExit, {WHITE, YELLOW}, {PURPLE, BLACK}, {}, 1 , .F. , .F. }
   // aParIco := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
   DEFAULT lTxtAlign  := 0  // Align Text: 0-None, 1-Left, 2-Right, 3-Center

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
   IF LEN(aParMenu) # 5
      AlertStop("Parameters aParMenu[5] is not < 5 !;;"+ProcNL())
      RETURN {}
   ENDIF

   nHBtn      := aParMenu[1]  ; Default nHBtn    := 40
   nFSize     := aParMenu[2]  ; Default nFSize   := 20
   nG         := aParMenu[3]  ; Default nG       := 10    // отступ между кнопками
   lBtnExit   := aParMenu[4]  ; Default lBtnExit := .T.
   lBtnGrad   := aParMenu[5]  ; Default lBtnGrad := .F.
   aRet       := {}                             // вернуть массив из этой функции
   nHeight    := nWidth := nCol := nRow := 0    // координаты окна
   nW         := System.DesktopWidth            // ширина окна - потом уменьшим
   nH         := System.DesktopHeight           // высота окна - потом уменьшим
   nY         := nX := 0
   cForm      := aParWin[1]
   cTitle     := aParWin[2]
   cIco       := aParWin[3]
   aBackColor := aParWin[4] ; Default aBackColor := MYBACK  // цвет фона всей формы
   nRow       := aParWin[5] ; Default nRow    := 0
   nCol       := aParWin[6] ; Default nCol    := 0
   nWidth     := aParWin[7] ; Default nWidth  := 0
   nHeight    := aParWin[8] ; Default nHeight := 0
   cFont      := "DejaVu Sans Mono"             // фонт для кнопок
   cFontExit  := "Comic Sans MS"                // фонт для кнопки Exit
   lBtnIco    := .F.                            // иконок на кнопках нет
   lBtnFBold  := .F.                            // фонт на кнопках
   lBtnHide   := .F.                            // НЕ скрывать кнопку

   IF LEN(aParIco) == 0
      aIcon     := {}                                // цвет фонта кнопок, цвет фона кнопок,  цвет градиента кнопок
      aIcoExit  := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", {WHITE,YELLOW}, {MYRED,BLACK}, {HMG_RGB2n(RED),CLR_WHITE} }   // иконки для выхода
      aFntClr   := { WHITE, YELLOW }                // цвет фонта кнопок
      aBtnBClr  := { MYBUTTON       , BLACK     }   // цвет фона кнопок
      aBtnGrad  := { HMG_RGB2n(GRAY), CLR_WHITE }   // цвет градиента кнопок
      nIcoAlign := 1
      lBtnFlat  := .F.                               // T-нет окантовки по краям кнопки, F-есть
      lGrdHoriz := .F.                // для градиента кнопки, направление градиента, F-горизонт, T-вертикальный
   ELSE
      aIcon     := aParIco[1]
      aIcoExit  := aParIco[2]
      aFntClr   := aParIco[3]         // цвет фонта кнопок
      aBtnBClr  := aParIco[4]         // цвет фона кнопок
      aBtnGrad  := aParIco[5]         // цвет градиента кнопок
      nIcoAlign := aParIco[6]         // иконка слева/справа/сверху/внизу кнопки
      lBtnFlat  := aParIco[7]         // окантовка по краям кнопки, F-есть
      lGrdHoriz := aParIco[8]         // для градиента кнопки, направление градиента
   ENDIF

   IF !HB_IsArray(aIcon)
      AlertStop("Parameters aIcon is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF LEN(aIcon) > 0
      lBtnIco := .T.   // ЕСТЬ иконки на кнопках
      IF LEN( aIcon ) # LEN( aMenu )
         cTxt := "The lengths of the menu and icon arrays are NOT EQUAL!;"
         cTxt += "aMenu[" + HB_NtoS(LEN(aMenu))
         cTxt += "] # aIcon[" + HB_NtoS(LEN(aIcon)) + "];"
         cTxt += ";" + ProcNL() + ";" + ProcNL(1)
         AlertStop(cTxt)
         RETURN {}
      ENDIF
   ENDIF

   IF !HB_IsArray(aBtnGrad)
      AlertStop("Parameters aBtnGrad is not an array !;;"+ProcNL())
      RETURN {}
   ENDIF
   IF lBtnGrad         // значит кнопки рисуем с градиентом
      IF LEN(aBtnGrad) < 2
         AlertStop("Parameters aBtnGrad < 2 !;;"+ProcNL())
         RETURN {}
      ENDIF
   ENDIF

   lWinImg := .F.
   IF !HB_IsArray(aIcoWin) // иконка на форме
      AlertStop("Parameters aIcoWin is not an array !;;"+ProcNL())
      RETURN {}
   ELSE
      IF LEN(aIcoWin) == 0
      ELSEIF LEN(aIcoWin) < 5
         AlertStop("Parameters aIcoWin < 5 !;;"+ProcNL())
         RETURN {}
      ELSE
         lWinImg := .T. // иконка на форме
      ENDIF
   ENDIF

   nColBtn := nImgAlign := 0
   IF lWinImg    // иконка на форме
      // aIcoWin   := {"iHP2", 190, 20, 20, nImgAlign}
      cImgRes   := aIcoWin[1]
      nImgSz    := aIcoWin[2]
      nImgY     := aIcoWin[3]
      nImgX     := aIcoWin[4]
      nImgAlign := aIcoWin[5]
      nColBtn   := nImgSz + nImgX * 2
   ENDIF

   // иконка слева/справа/сверху/внизу кнопки
   lTextLeft := lTextVert := lBtnUpText := .F.
   IF nIcoAlign == 1
   ELSEIF nIcoAlign == 2
      lTextLeft := .T.
   ELSEIF nIcoAlign == 3
      lTextVert := .T.
   ELSEIF nIcoAlign == 4
      lTextVert  := .T.
      lBtnUpText := .T.   // UPPERTEXT
   ENDIF

   nLine := 1            // кол-во строк на кнопке
   nL    := 0
   nK    := Len(aMenu)
   FOR nI := 1 TO nK
      cTxt := aMenu[nI]
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
   // Изменить массив - выровнять текст
   aMenu := myChangeArray(aMenu,nL,lTxtAlign)

   // добавим кнопку
   IF lBtnExit
      IF !HB_IsArray(aIcoExit)
         AlertStop("Parameters aIcoExit is not an array !;;"+ProcNL())
         RETURN {}
      ENDIF
      IF LEN(aIcoExit) < 5
         AlertStop("Parameters aIcoExit < 5 !;;"+ProcNL())
         RETURN {}
      ENDIF
      // aIcoExit  := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", aBtnFClr, aBtnBClr, aBtnGrad }   // иконки для выхода
      AADD( aMenu, PADC(aIcoExit[2],nL) )
      IF lBtnIco   // ЕСТЬ иконки на кнопках
         AADD( aIcon, aIcoExit[1] )
      ENDIF
   ENDIF

   cTxt  := REPL("a",nL)
   nWBtn := GetTxtWidth( cTxt, nFSize+2, cFont, .F. ) + nG  // получить Width текста
   nW    := nWBtn + nG*2 + 10 + GetBorderWidth()*2          // ширина окна
   IF lBtnIco      // ЕСТЬ иконки на кнопках
      nW += nHBtn  // добавим высоту кнопки
   ENDIF

   IF lWinImg    // иконка на форме
      nW        += nColBtn              // добавить к ширине формы
   ENDIF

   // кол-во строк на кнопке
   IF nLine > 1
      nHBtn += nFSize * (nLine-1)  + 6
   ENDIF
   // получить размер иконки кнопки в зависимости от высоты кнопки
   nIcoSize := INT(nHBtn - 3*2)
   // координаты окна
   nY := IIF( nRow > 0, nRow, 0 )
   nX := IIF( nCol > 0, nCol, 0 )
   nW := IIF( nWidth  > 0, nWidth , nW )
   nH := IIF( nHeight > 0, nHeight, nH )
   // расчёт на высоту окна
   nHWin   := CalcWinHeight(nG,aMenu,nHBtn)
   nWVirt  := NIL
   nHVirt  := NIL
   nHDsktp := GetDesktopHeight() - GetTaskBarHeight() // высота Панели задач Desktop
   // установим внешнюю высоту окна
   IF nHDsktp < nHWin
      nHVirt := nHWin + nG*4 + GetBorderHeight()
      nH     := nHDsktp - GetTitleHeight() - GetBorderHeight()*2
   ELSE
      nH := nHWin
   ENDIF

   //DEFINE WINDOW &cForm At nY, nX WIDTH nW HEIGHT nH                 ;
   DEFINE WINDOW &cForm AT nY, nX CLIENTAREA nW, nH                    ;
      VIRTUAL WIDTH  nWVirt                                            ;
      VIRTUAL HEIGHT nHVirt                                            ;
      TITLE cTitle ICON cIco                                           ;
      MODAL NOSIZE                                                     ;
      BACKCOLOR aBackColor                                             ;
      FONT cFont SIZE nFSize                                           ;
      ON INIT     {|| This.Topmost := .F., DoEvents(), _wPost(0) }     ;
      ON RELEASE  {|| AEval({91,92,93}, {|n| _wSend(n), DoEvents()}) } ; // executed before destroying the window

      This.Cargo := oHmgData()
      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nY    := nX := nG

      IF lWinImg    // иконка на форме
         nWBtn := nW - nG * 2 - nColBtn
      ELSE
         nWBtn := nW - nG * 2
      ENDIF

      @ 0,0 Label Buff Value "" AUTOSIZE

      IF nImgAlign == 1  // 1-Left Align Icon
         nX += nColBtn   // с учётом картинки если иконка слева
      ENDIF
      Draw_Buttons(cForm,aMenu,aIcon,nIcoSize,aFntClr,aBtnBClr,aBtnGrad,;
                   cFont,cFontExit,nFSize,lBtnExit,aIcoExit,nG,nY,nX,nWBtn,nHBtn,lBtnIco,;
                   lTextVert,lTextLeft,lBtnFlat,lBtnFBold,lBtnGrad,lGrdHoriz,lBtnUpText,lBtnHide)

      IF lWinImg    // иконка на форме
         This.Cargo:hIcon := LoadIconByName( cImgRes, nImgSz, nImgSz )
         IF nImgAlign == 2  //2-Right Align Icon
            nImgX += nWBtn + nG*2
         ENDIF
         DRAW ICON IN WINDOW &cForm AT nImgY, nImgX HICON This.Cargo:hIcon ;
                                     WIDTH nImgSz HEIGHT nImgSz COLOR aBackColor
      ENDIF

      WITH OBJECT This.Object
         :Event( 0, {|    |
                     This.Topmost := .F.
                     Return Nil
                     })
         :Event( 1, {|obtn|
                     Local ow := obtn:Window
                     Local ct := obtn:Cargo:cBtn
                     Local i  := obtn:Cargo:nBtn
                     Local ai := obtn:Cargo:aIco
                     aRet := { i, ct, hb_valtoexp(ai), obtn:Cargo:nIcoWidth }
                     InkeyGui(200)
                     ow:Enabler(obtn:Name, .T.)
                     _wPost(99)
                     Return Nil
                     })
         :Event(91, {|ow  | DestroyIcon(ow:Cargo:hIcon) } )  // убить хендл иконки
         :Event(99, {|ow  | ow:Release })
      END WITH

      ON KEY F1     OF &cForm ACTION NIL
      ON KEY ESCAPE OF &cForm ACTION _wSend(99)

   END WINDOW

   IF nRow == 0 .AND. nCol == 0
      CENTER WINDOW &cForm
   ENDIF
   ACTIVATE WINDOW &cForm

RETURN aRet

///////////////////////////////////////////////////////////////////////////////////
FUNCTION CalcWinHeight(nG,aMenu,nHBtn)
   LOCAL nI, nH := nG

   FOR nI := 1 TO Len(aMenu)
       nH += nHBtn + nG
   NEXT

RETURN nH

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myChangeArray(aMenu,nL,lTxtAlign)
   LOCAL nI, nK, nJ, aDim, cTxt, cStr

   IF lTxtAlign > 0
      // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      nK  := Len(aMenu)
      FOR nI := 1 TO nK
         cTxt := aMenu[nI]
         IF ";" $ cTxt       // в строке есть CRLF
            aDim := HB_ATokens(cTxt, ";")
            cStr := ""
            FOR nJ := 1 TO LEN(aDim)
               cTxt := aDim[nJ]
               IF lTxtAlign == 1
                  cStr += PADR(cTxt,nL)
               ELSEIF lTxtAlign == 2
                  cStr += PADL(cTxt,nL)
               ELSEIF lTxtAlign == 3
                  cStr += ALLTRIM(cTxt)
               ENDIF
               cStr += IIF(nJ == LEN(aDim),"",";")
            NEXT
            aMenu[nI] := cStr
         ELSEIF CRLF $ cTxt       // в строке есть CRLF
            aDim := HB_ATokens(cTxt, CRLF)
            cStr := ""
            FOR nJ := 1 TO LEN(aDim)
               cTxt := aDim[nJ]
               IF lTxtAlign == 1
                  cStr += PADR(cTxt,nL)
               ELSEIF lTxtAlign == 2
                  cStr += PADL(cTxt,nL)
               ELSEIF lTxtAlign == 3
                  cStr += ALLTRIM(cTxt)
               ENDIF
               cStr += IIF(nJ == LEN(aDim),"",CRLF)
            NEXT
            aMenu[nI] := cStr
         ELSE
            IF lTxtAlign == 1
               aMenu[nI] := PADR(cTxt,nL)
            ELSEIF lTxtAlign == 2
               aMenu[nI] := PADL(cTxt,nL)
            ELSEIF lTxtAlign == 3
               aMenu[nI] := ALLTRIM(cTxt)
            ENDIF
         ENDIF
      NEXT
   ENDIF

RETURN aMenu

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_Buttons(cForm,aMenu,aIcon,nIcoSize,aFntClr,aBtnBClr,aBtnGrad,;
                   cFont,cFontExit,nFSize,lBtnExit,aIcoExit,nG,nY,nX,nWBtn,nHBtn,lBtnIco,;
                   lTextVert,lTextLeft,lBtnFlat,lBtnFBold,lBtnGrad,lGrdHoriz,lBtnUpText,lBtnHide)
   LOCAL nK, nI, cObj, cTxt, aBtnIco, aGrOver, aGrFill, o
   LOCAL bMouseHover, bMouseLeave

   Sets_BtnHoverLeave() // для всех ButtonEx кнопок

   IF lBtnGrad  // значит кнопки рисуем с градиентом
      aGrOver := { { 0.5, aBtnGrad[2], aBtnGrad[1] }, { 0.5, aBtnGrad[1], aBtnGrad[2] } }
      aGrFill := { { 0.5, aBtnGrad[1], aBtnGrad[2] }, { 0.5, aBtnGrad[2], aBtnGrad[1] } }
   ELSE
      aGrOver := aGrFill := {}
   ENDIF

   nK := Len(aMenu)
   FOR nI := 1 TO nK

      cObj := "Btn_" + StrZero(nI, 2)
      cTxt := aMenu[ nI ]
      IF lBtnIco
         aBtnIco := aIcon[nI]  // {"iSanta1" ,"iSanta2" }
      ENDIF

      IF ";" $ cTxt ; cTxt := StrTran( cTxt, ";", CRLF )
      ENDIF

      DEFINE BUTTONEX &cObj
         PARENT       &cForm
         ROW          nY
         COL          nX
         WIDTH        nWBtn
         HEIGHT       nHBtn
         ICON         IIF(lBtnIco, aBtnIco[1], Nil )
         CAPTION      cTxt
         ACTION       {|| This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) }
         TOOLTIP      Nil
         VERTICAL     lTextVert
         LEFTTEXT     lTextLeft
         FLAT         lBtnFlat
         FONTNAME     Nil
         FONTSIZE     Nil
         FONTBOLD     lBtnFBold
         FONTCOLOR    aFntClr[1]
         BACKCOLOR    IIF(lBtnGrad, aGrOver, aBtnBClr[1] )
         GRADIENTFILL IIF(lBtnGrad, aGrFill, Nil )
         HORIZONTAL    lGrdHoriz
         NOTRANSPARENT .F.
         UPPERTEXT    lBtnUpText
         NOHOTLIGHT   .F.
         NOXPSTYLE    .T.
         HANDCURSOR   .T.
         NOTABSTOP    .T.
         ONGOTFOCUS    Nil
         ONLOSTFOCUS   Nil
         INVISIBLE    lBtnHide
         ONINIT       {|| This.Cargo := oHmgData(), This.Cargo:cObj := This.Name , This.Cargo:oCtl := This.Object }
      END BUTTONEX

      o := This.&(cObj).Cargo  // берем адрес объекта для быстроты доступа
      o:nBtn    := nI
      o:cBtn    := cTxt
      o:nPost   := 1
      o:aBClr   := aBtnBClr
      o:aFClr   := aFntClr
      o:aGrad   := aBtnGrad
      o:cFont   := cFont
      o:nFSize  := nFSize
      o:aIco    := aBtnIco
      o:aGrOver := aGrOver
      o:aGrFill := aGrFill
      o:oWnd    := This.Object
      o:nIcoWidth  := nIcoSize
      o:nIcoHeight := nIcoSize

      // изменим размеры иконок, если они есть
      IF lBtnIco
         IF lTextVert  // уменьшим размер иконки для nIcoAlign= 3-Top Align Icon, 4-Bottom Align Icon
            nIcoSize := nHBtn - nFSize*2    // размер фонта на кнопках
         ENDIF
         o:nIcoWidth  := nIcoSize
         o:nIcoHeight := nIcoSize
         This.&(cObj).ImageWidth  := nIcoSize
         This.&(cObj).ImageHeight := nIcoSize
         This.&(cObj).Icon := LoadIconByName( aBtnIco[1], nIcoSize, nIcoSize )
      ENDIF

      IF nI == nK .AND. lBtnExit
      // aIcoExit  := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", aBtnFClr, aBtnBClr, aBtnGrad }   // иконки для выхода
         o:aFClr := aIcoExit[3]
         o:aBClr := aIcoExit[4]  // { MAROON , BLACK }
         o:aGrad := aIcoExit[5]
         IF lBtnGrad  // значит кнопки рисуем с градиентом
            o:aGrOver := { { 0.5, o:aGrad[2], o:aGrad[1] }, { 0.5, o:aGrad[1], o:aGrad[2] } }
            o:aGrFill := { { 0.5, o:aGrad[1], o:aGrad[2] }, { 0.5, o:aGrad[2], o:aGrad[1] } }
            This.&(cObj).GradientFill := o:aGrFill
         ELSE
            o:aGrOver := {}
            o:aGrFill := {}
            This.&(cObj).Backcolor := o:aBClr[1]
         ENDIF
         This.&(cObj).Action    := {|| _wPost(99) }
         This.&(cObj).FontName  := cFontExit
         This.&(cObj).FontBold  := .T.
         This.&(cObj).FontSize  := nFSize
         This.&(cObj).Fontcolor := o:aFClr[1]
         //This.&(cObj).Caption   := aIcoExit[2]
      ENDIF

      IF lBtnIco
         bMouseHover := iif( lBtnGrad, bStaticBtnHoverIcoGrad, ; // +Icon+Gradient
                                       bStaticBtnHoverIco )      // +Icon
         bMouseLeave := iif( lBtnGrad, bStaticBtnLeaveIcoGrad, ; // +Icon+Gradient
                                       bStaticBtnLeaveIco )      // +Icon
      ELSE
         bMouseHover := iif( lBtnGrad, bStaticBtnHoverGrad, ;    // +Gradient
                                       bStaticBtnHover )
         bMouseLeave := iif( lBtnGrad, bStaticBtnLeaveGrad, ;    // +Gradient
                                       bStaticBtnLeave )
      ENDIF

      This.&(cObj).OnGotFocus  := bMouseHover
      This.&(cObj).OnLostFocus := bMouseLeave

      nY += nHBtn + nG

   NEXT

RETURN nY

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_BtnHoverLeave() // для всех ButtonEx кнопок

   bStaticBtnHoverIcoGrad := {||   // bMouseHover + Icon + Gradient
       //Local o := This.Cargo
       //? "*=> :bBtnHoverIcoGrad, o:aIco[2]", o:cObj, o:aIco[2]
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.GradientFill := o:aGrFill
       This.Backcolor    := o:aBClr[2]
       This.Fontcolor    := o:aFClr[2]
       This.FontSize     := o:nFSize + 2
       This.Icon := LoadIconByName( o:aIco[2], o:nIcoWidth, o:nIcoHeight )
       Return Nil
       }
   bStaticBtnLeaveIcoGrad := {||   // bMouseLeave + Icon + Gradient
       //Local o := This.Cargo
       //? "*=> :bBtnLeaveIcoGrad, o:aIco[1]", o:cObj, o:aIco[1]
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.GradientOver := o:aGrOver
       This.Backcolor    := o:aBClr[1]
       This.Fontcolor    := o:aFClr[1]
       This.FontSize     := o:nFSize
       This.Icon := LoadIconByName( o:aIco[1], o:nIcoWidth, o:nIcoHeight )
       Return Nil
       }
   bStaticBtnHoverIco     := {||   // bMouseHover + Icon
       //Local o := This.Cargo
       //? "*=> :bBtnHoverIco, o:aIco[2]", o:cObj, o:aIco[2]
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.Backcolor := o:aBClr[2]
       This.Fontcolor := o:aFClr[2]
       This.FontSize  := o:nFSize + 2
       This.Icon := LoadIconByName( o:aIco[2], o:nIcoWidth, o:nIcoHeight )
       Return Nil
       }
   bStaticBtnLeaveIco     := {||   // bMouseLeave + Icon
       //Local o := This.Cargo
       //? "*=> :bBtnLeaveIco, o:aIco[1]", o:cObj, o:aIco[1]
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.Backcolor := o:aBClr[1]
       This.Fontcolor := o:aFClr[1]
       This.FontSize  := o:nFSize
       This.Icon := LoadIconByName( o:aIco[1], o:nIcoWidth, o:nIcoHeight )
       Return Nil
       }
   bStaticBtnHoverGrad   := {||   // bMouseHover + Gradient
       //Local o := This.Cargo
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.GradientFill := o:aGrFill
       This.Fontcolor    := o:aFClr[2]
       This.FontSize     := o:nFSize + 2
       Return Nil
       }
   bStaticBtnLeaveGrad   := {||   // bMouseLeave + Gradient
       //Local o := This.Cargo
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.GradientOver := o:aGrOver
       This.Fontcolor    := o:aFClr[1]
       This.FontSize     := o:nFSize
       Return Nil
       }
   bStaticBtnHover    := {||   // bMouseHover
       //Local o := This.Cargo
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.Backcolor := o:aBClr[2]
       This.Fontcolor := o:aFClr[2]
       This.FontSize  := o:nFSize + 2
       Return Nil
       }
   bStaticBtnLeave    := {||   // bMouseLeave
       //Local o := This.Cargo
       Local o := _HMG_aControlMiscData2[ _HMG_ThisIndex ]
       This.Backcolor := o:aBClr[1]
       This.Fontcolor := o:aFClr[1]
       This.FontSize  := o:nFSize
       Return Nil
       }

RETURN Nil
