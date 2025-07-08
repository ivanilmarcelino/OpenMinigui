/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2024-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Верхнее меню окна с кнопками / Top window menu with buttons
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
////////////////////////////////////////////////////////////////////////////////////
FUNCTION MenuTopIconButtons( owc, oMenu, a2BtnBClr, lNoSaveBtnObj, nGNew, c1Post )
   LOCAL nH, nX, nY, nG, nG2, cFont, nFSize, lBold, nWButton, nHButton
   LOCAL nWBtn, nHBtn, cCap, nWtxt, nWCap, cObj, cForm, aBtnObj, i, o
   LOCAL aIco, aBtnFClr, aBtnBClr, aBtnFont, nHIco, aBtn, j, aColor, a1BClr
   LOCAL aBColor, aCapt, aObj, aIcon, nW, lAutoSize, cMsg, aDim, cVal, nJ
   LOCAL a2Cap, n1Cap, n2Cap, aVal, nType, hIco1, hIco2, hIco3, hIco4
   LOCAL cTooltipt, lVert, nClientW, lGradient, nK, a2Clr, nH1Ico, nHWIco
   DEFAULT lNoSaveBtnObj := .T. , nGNew := 0, a2BtnBClr := {}  // резерв
   DEFAULT c1Post := ""  // много событий - имя объекта / "_NewPost" - одно событие

   SET WINDOW MAIN OFF

   ? ProcNL(), "oWC=", oWC, oMenu
   cForm := owc:Name            // ThisWindow.Name    // имя окна
   IF !IsString(cForm)
      cMsg := "ERROR !; owc:Name == NIL !;;"
      cMsg += "Window name not set !"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      RETURN NIL
   ENDIF
   aBColor   := owc:aBColor            // цвет фона окна
   aBtnFClr  := oMenu:aBtnFClr         // { WHITE   , BLACK }
   aBtnBClr  := oMenu:aBtnBClr         // { aBColor , WHITE }
   aBtnFont  := oMenu:aBtnFont         // { cFont, nFSize, .T. }
   nHIco     := nHWIco := oMenu:nHIco  // 32,55  - задаём размер картинки на кнопке
   nH1Ico    := oMenu:nH1Ico           // высота первой иконки на кнопке
   nH1Ico    := IIF(!IsNumeric(nH1Ico), 0, nH1Ico )
   nH1Ico    := IIF( nH1Ico== 0, nHIco, nH1Ico )
   nG2       := oMenu:nHG2             // добавочная высота к тексту кнопки
   nG        := owc:nG                 // отступы
   IF nGNew > 0  ; nG := nGNew
   ?? "nG=", nG
   ENDIF
   cFont     := aBtnFont[1]
   nFSize    := aBtnFont[2]
   lBold     := aBtnFont[3]
   lAutoSize := oMenu:lAutoSize    // T - автоматический расчёт высоты и ширины кнопки
   nWButton  := oMenu:nWBtn     ; Default nWButton  := 0    // ручное задание ширины кнопки
   nHButton  := oMenu:nHBtn     ; Default nHButton  := 0    // ручное задание высоты кнопки
   lVert     := oMenu:lVert     ; Default lVert     := .T.  // вертикальный текст на кнопке
   nClientW  := oMenu:nClientW  ; Default nClientW  := 0    // ширина окна куда нужно вместить кнопки
   lGradient := oMenu:lGradient ; Default lGradient := .F.  // градиент на кнопках
   aBtnObj   := {}

   //IF App.Cargo:aDisplayMode[2] <= 720
   //   nHIco := App.Cargo:nTsbHCell  // высота ячейки таблицы = высоте иконки
   //   nG    := nG/2
   //ENDIF
   IF !IsArray(aBtnFont)
      cMsg := "ERROR !; oMenu:aBtnFont == NIL !;;"
      cMsg += "Form [" + cForm + "];"
      cMsg += "No array - aBtnFont"
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      RETURN NIL
   ENDIF
   cFont     := aBtnFont[1]
   nFSize    := aBtnFont[2]
   lBold     := aBtnFont[3]

   IF !IsArray(owc:ahIcoDel)
      cMsg := "ERROR !;Icon handle deletion array not defined !;;"
      cMsg += "Form [" + cForm + "];"
      cMsg += "No array - owc:ahIcoDel"
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      RETURN NIL
   ENDIF

   // имя объекта + имя события
   aObj   := oMenu:aObj
   aIcon  := oMenu:aImg
   aCapt  := oMenu:aCapt
   a1BClr := oMenu:a1BClr       // каждая кнопка со своим цветом
   IF !IsArray(aCapt)
      cMsg := "ERROR !;Arrays are not the same !;;"
      cMsg += "oMenu:aCapt= NIL;"
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      RETURN NIL
   ENDIF
   nK := LEN(aCapt)
   IF nK == LEN(aIcon) .AND. nK == LEN(aObj) .AND. nK == LEN(a1BClr)
   ELSE
      cMsg := "ERROR !;Arrays are not the same !;;"
      cMsg += "LEN(oMenu:aObj )=" + HB_NtoS(LEN(oMenu:aObj )) + ";"
      cMsg += "LEN(oMenu:aImg )=" + HB_NtoS(LEN(oMenu:aImg )) + ";"
      cMsg += "LEN(oMenu:aCapt)=" + HB_NtoS(LEN(oMenu:aCapt)) + ";"
      cMsg += "LEN(oMenu:a1BClr)=" + HB_NtoS(LEN(oMenu:a1BClr))
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      ? ProcNL(), "------ ERROR !;Arrays are not the same ! --------"
      ? "oMenu:aObj="  , aObj   , HB_ValToExp( aObj  )
      ? "oMenu:aImg="  , aIcon  , HB_ValToExp( aIcon )
      ? "oMenu:aCapt=" , aCapt  , HB_ValToExp( aCapt )
      ? "oMenu:a1BClr=", a1BClr , HB_ValToExp( a1BClr)
   ENDIF
   //oMenu:aObj  := { "_POpen" ,"_PRunCode" , "_PExit"  }
   //oMenu:aImg  := { {"iAReopen32x1","iAReopen32x2"} , {"iATable32x1","iATable32x2"} , {"iAExit32x1","iAExit32x2"} }
   //oMenu:aMnRu := { "Код-prg" , "Выполнить", "Выход" }
   //oMenu:aMnEn := { "Code-prg", "Execute"  , "Exit"  }
   //oMenu:aCapt := IIF( App.Cargo:cLang == "RU", oMenu:aMnRu, oMenu:aMnEn )

   IF lAutoSize .AND. lVert   // вертикальный текст на кнопке
      // расчёт по тексту
      nWtxt := nH := 0
      FOR i := 1 TO LEN(aCapt)
         cCap := aCapt[ i ]
         IF IsArray(cCap)          // пример {"Поиск;оператора","Очистить;поиск"}
            a2Cap := cCap
            n1Cap := LEN(a2Cap[1])
            n2Cap := LEN(a2Cap[2])
            IF n1Cap > n2Cap
               cCap := a2Cap[1]
            ELSE
               cCap := a2Cap[2]
            ENDIF
         ENDIF
         // надпись на кнопке
         IF AT(";",cCap) > 0
            aDim := HB_ATokens(cCap, ";")
            FOR nJ := 1 TO LEN(aDim)
               cVal := ALLTRIM(aDim[nJ])
               nWCap := GetTxtWidth(cVal, nFSize, cFont, lBold )
               nWTxt := MAX(nWTxt,nWCap)
            NEXT
         ELSE
            nWCap := GetTxtWidth(cCap, nFSize, cFont, lBold )
         ENDIF
         //nWCap := GetTextWidth( NIL, cCap, hFont )
         nWTxt := MAX(nWTxt,nWCap)
      NEXT
      nWTxt := IIF(nWTxt < nHIco, nHIco, nWTxt )   // nHImg-высота bmp
      nWBtn := nWTxt + nG * 2                      // ширина кнопки
      nHBtn := nHIco + 5 + nFSize + 5 + nG2 * 2    // высота кнопки
      IF nHButton > 0
         nHBtn := nHButton    // высота кнопки задана вручную
      ENDIF
   ELSEIF lAutoSize .AND. !lVert   // НЕ вертикальный текст на кнопке
      IF nClientW == 0
         cMsg := "ERROR !;"
         cMsg += "Не задана ширина окна, куда нужно вместить кнопки;;"
         cMsg += "oMenu:nClientW = 0;"
         cMsg += ";;" + ProcNL()
         AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      ENDIF
      nWButton := nClientW - nG * (LEN(aCapt)+1)  // ширина окна куда нужно вместить кнопки
      nWBtn := INT( nWButton / LEN(aCapt) )  // ширина кнопки
      IF nHButton > 0
         nHBtn := nHButton       // высота кнопки задана вручную
      ELSE
         nHBtn := nHIco + nG     // высота кнопки
      ENDIF
      nG2   := nG
   ELSE
      nWBtn := nWButton       // ширина кнопки заданы вручную выше
      nHBtn := nHButton       // высота кнопки заданы вручную выше
   ENDIF

   SET WINDOW MAIN ON

   nY := IIF( IsNumeric(oMenu:nY), oMenu:nY, nG )
   nX := IIF( IsNumeric(oMenu:nX), oMenu:nX, nG )

   FOR i := 1 TO LEN(aCapt)

      nType := 0
      cCap  := aCapt[i]
      IF IsArray(cCap)      // пример {"Поиск;оператора","Очистить;поиск"}
         aVal := cCap
         FOR j := 1 TO LEN(aVal)
            IF AT(";",aVal[j]) > 0
               aVal[j] := AtRepl( ";", aVal[j], CRLF )
            ENDIF
         NEXT
         cCap  := aVal[1]
         nType := 2         // две кнопки
      ENDIF

      IF AT(";",cCap) > 0
         cCap := AtRepl( ";", cCap, CRLF )
      ENDIF

      cTooltipt := ""  // резерв
      cObj  := aObj[i]    // контрол на окне
      aIco  := aIcon[i]
      IF i == 1  ;  nHIco := nH1Ico
      ELSE       ;  nHIco := nHWIco
      ENDIF
      hIco1 := LoadIconByName(aIco[1], nHIco, nHIco)
      hIco2 := LoadIconByName(aIco[2], nHIco, nHIco)
      hIco3 := IIF( nType == 2, LoadIconByName(aIco[3], nHIco, nHIco) , 0 )
      hIco4 := IIF( nType == 2, LoadIconByName(aIco[4], nHIco, nHIco) , 0 )
      // для удаления хендлов иконок с формы
      AADD( owc:ahIcoDel, hIco1 )
      AADD( owc:ahIcoDel, hIco2 )
      IF nType == 2
         AADD( owc:ahIcoDel, hIco3 )
         AADD( owc:ahIcoDel, hIco4 )
         aColor := a1BClr[i,1]  // каждая кнопка со своим цветом - здесь 2 цвета { 1,2 }
      ELSE
         aColor := a1BClr[i]    // каждая кнопка со своим цветом
      ENDIF

      aBtn := { cObj, cCap, {hIco1,hIco2}, aBtnBClr, aBtnFClr, aBtnFont, cTooltipt, lVert }
      IF lGradient
         // Кнопки с градиентом
         Draw4ButtonGrad(cForm, nY, nX, nWBtn, nHBtn, aBtn, aColor)
      ELSE
         // построение кнопки без градиента -> util_button.prg
         Draw4Button(cForm, nY, nX, nWBtn, nHBtn, aBtn, aColor)
      ENDIF

      This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
      o:nBtn   := i
      o:aIco   := aIco       // [2] или [4] иконки
      o:ahIco  := {hIco1,hIco2,hIco3,hIco4}  // 4 иконки - хендлы
      o:nHIco  := nHIco
      o:aFClr  := aBtnFClr   // [2] или [4] цвета
      o:aBClr  := aBtnBClr   // [2] или [4] цвета
      o:cTxt   := aCapt[i]   // 1-я надпись на кнопке
      o:aTxt   := aCapt[i]   // 1 или 2 надписи на кнопке
      o:nDim   := IIF( VALTYPE(aCapt[i]) == "A", 2, 1)   // кол-во значений на кнопке 1/2
      o:nI     := 1
      o:Post   := cObj         // событие на форме
      o:c1Post := c1Post       // одно событие на форме - общее событие
      o:aBtnGrad := aColor

      IF lGradient .AND. nType == 2
         a2Clr := a1BClr[i]
         IF LEN(a2Clr) # 2
            cCap := aCapt[i]
            IF IsArray(cCap)      // пример {"Поиск;оператора","Очистить;поиск"}
               cCap := HB_ValToExp(cCap)
            ENDIF
            cMsg := "ERROR !;"
            cMsg += "Кнопка = " + cCap + ";"
            cMsg += "Не задано 2 цвета на эту кнопку !;"
            cMsg += "a1BClr["+HB_NtoS(i)+"] = { RED, ORANGE } ;"
            cMsg += ";;" + ProcNL()
            AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
         ENDIF
         o:a2xBClr := { a2Clr[1], a2Clr[2] }   // добавить 2 цвета на кнопку
      ENDIF

      IF LEN(c1Post) > 0
         This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:c1Post, , This.Name) }
      ELSE
         This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:Post, , This.Name) }
      ENDIF

      AADD( aBtnObj, { i, cObj, "-имя объекта", cCap, nY, nX, This.&(cObj).Width, nHBtn, cObj, "-событие", This.&(cObj).Cargo } )

      nX += This.&(cObj).Width + nG
      // вывод в лог
      //_o2log(o, 25, ProcNL() + "  o => This.Cargo: " + cObj, .T. ) ; ?

   NEXT

   nW := nX - nG // + owc:nG
   owc:nWEndXBtn := nW                // конец кнопок
   owc:nHTBar    := nHBtn + nG*2      // высота ToolBar
   owc:nHBtn2    := nHBtn             // высота кнопок
   IF lNoSaveBtnObj
      owc:aBtnObj   := aBtnObj        // запись массив кнопок на форме
   ENDIF
   //? "aBtnObj=",aBtnObj ; ?v aBtnObj
   ? ProcNL(), "End oMenu", "owc:nWEndXBtn=", owc:nWEndXBtn, "owc:nHTBar=",owc:nHTBar

RETURN NIL


/////////////////////////////////////////////////////////////////////////
FUNCTION Draw_LineIcon(lOne,oWnd)
   LOCAL aBClr, cForm, nI, nY, nX, nW, nH, nG, o, a4Ico, aIcon, hIcon

   o     := oWnd:Cargo
   cForm := oWnd:Name
   aBClr := ThisWindow.BACKCOLOR
   aIcon := o:aIcoLogo           // { {"1MG",64,64,5}, {"iUser128",64,64,5} }
   nY    := o:aIconYX[1]         // {  0, nG }
   nX    := o:aIconYX[2]         // {  0, nG }
   nW    := 0

   //_o2log(o, 17, ProcNL() + "  o:Cargo =>  !!!!!!!!!", .T. ) ; ?

   // сменим иконку
   IF !lOne
      //DestroyIcon(o:hIcon)
   ENDIF

   FOR nI := 1 TO LEN(aIcon)

      a4Ico := aIcon[nI]
      nW    := a4Ico[2]
      nH    := a4Ico[3]
      nG    := a4Ico[4]
      hIcon := LoadIconByName( a4Ico[1], nW, nH )
      DRAW ICON IN WINDOW &cForm AT nY, nX HICON hIcon WIDTH nW HEIGHT nH COLOR aBClr

      nX += nW + nG

      o:ahIcoLogo[nI] := hIcon   // для перерисовке на форме - резерв
      AADD(o:ahIcoDel, hIcon )   // для удаления хендлов иконок с формы

   NEXT
   o:nIconWEnd := nX

RETURN NIL
