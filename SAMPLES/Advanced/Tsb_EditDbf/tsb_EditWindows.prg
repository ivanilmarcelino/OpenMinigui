/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 15.04.25
 *
 * _TBrowse() Разные функции для редактирований ячеек таблицы
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_ContexMenu(oBrw, aDim, cImg, lIcon)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, aRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, nMenu, aMsg
   LOCAL aFont, nFSize, cName, nWCell, nHCell, oCell
   DEFAULT lIcon := .T.   // иконки в меню, иначе BMP

   cForm  := oBrw:cParentWnd
   oWnd   := _WindowObj(oBrw:cParentWnd)
   // координаты ячейки в которой Edit
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oWnd:Row + oCell:nRow - 2 //+ GetTitleHeight() /*+ GetMenuBarHeight()*/
   //nY   += oBrw:nTop + IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   //nY   += oBrw:nHeightHead
   nX     := oWnd:Col + oCell:nCol + 2
   nWCell := oCell:nWidth - 2
   nHCell := oCell:nHeight - 2

#ifdef KEY_ENG // for this project demo1-en.hbp
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("ComSanMS")
   aMsg   := { "Delete value", "Exit" }
#else
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("FntBtn_1")   // Фонт-1 кнопок других форм
   aMsg   := { "Удалить значение", "Выход" }
#endif

   aFont  := GetFontParam(hFont1)
   nFSize := aFont[2]
   nMenu  := 0

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cMenu   := aDim[nI]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         hFont   := IIF( lDis, hFont2, hFont1 )

         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      SEPARATOR
      MENUITEM  aMsg[1] ACTION  {|| nMenu := -1 } FONT hFont2  ICON "iDelVal32"
      SEPARATOR
      MENUITEM  aMsg[2]  ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      aRet := { nMenu, aDim[nMenu] }
   ELSEIF nMenu == -1
      aRet := { 0, "-.-" }
   ELSE
      aRet := {}
   ENDIF

   DO EVENTS

RETURN aRet

////////////////////////////////////////////////////////////////////////////
FUNCTION CellEdit_DT(oBrw,cType,xGet, cForm)
   LOCAL oCell, nY, nX, nWCell, nHCell
   LOCAL oJWnd, aRet, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH
   DEFAULT cForm := "Win_Cell"

   oJWnd  := _WindowObj(oBrw:cParentWnd)
   // координаты ячейки в которой стоит курсор
   oCell  := oBrw:GetCellSize(oBrw:nRowPos, oBrw:nCell, ) //oBrw:lDrawSuperHd)
   nY     := oCell:nRow     - 2
   nX     := oCell:nCol
   nWCell := oCell:nWidth
   nHCell := oCell:nHeight
   //
   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   nHObj  := nHCell - 5
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4            // две кнопки
   nW     := nWDate + nWBtn + 5
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   nH     := nHCell
   nW     := nWCell //- GetVScrollBarWidth()  // сделаем до конца ячейки - НЕ НАДО !

   // новое окно в ячейку таблицы
   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH       ;
          MODAL NOCAPTION NOSIZE FONT cFont SIZE nFSize-2 ;
          BACKCOLOR RED                                   ;
          ON INIT  DoEvents()

      IF cType == "D"

         IF VALTYPE(xGet) == "C"
            xGet := CTOD(xGet)
         ELSEIF VALTYPE(xGet) == "D"
         ELSE
            xGet := CTOD('')
         ENDIF
         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
                DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
                 ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR        ;
                 ACTION ( aRet := { This.Date_1.Value }, ThisWindow.Release )

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
                 ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR     ;
                 ACTION ( aRet := {}, ThisWindow.Release )

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         IF IsString(xGet)        ; tDTime := hb_CToT(xGet)
         ELSE                     ; tDTime := xGet
         ENDIF
         IF tDTime == hb_CToT("") ; tDTime := hb_DateTime()
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;
                SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"

         This.Date_2.VALUE := { Year(dDate1), Month(dDate1), Day(dDate1), ;
                                aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
                 ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR        ;
                 ACTION ( tDTime := This.Date_2.Value, ;
                          aRet := { tDTime }, ThisWindow.Release )

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
                 ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
                 ACTION ( aRet := {} , ThisWindow.Release )

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      ENDIF

      ON KEY ESCAPE ACTION ThisWindow.Release
      ON KEY RETURN ACTION ThisWindow.Release

      SetWindowLong( This.Handle, GWL_STYLE, WS_BORDER)

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////
FUNCTION CellEdit_A(oBrw,aGet,aDim14,aDim15,cForm)
   Local oCell, nY, nX, nW, nH, nG, nI, nWBtn, nWLine, nWLbl, cText
   Local nHGet, nHObj, nHIco, nHCell, cN, cN2, xGet, cPct, aRet
   Local aFont, cFont, nFSize, nWScr, lVert, aType
   Local op := oBrw:Cargo:oParam
   Local nc := oBrw:nCell
   Local nr := oBrw:nRowPos
   Local oc := oBrw:aColumns[nc]
   Local xv := oBrw:GetValue(nc)   // резерв
   Local cv := cValToChar(xv)      // резерв
   DEFAULT aDim15 := {}, cForm := "Cell_A"

   IF !IsArray(aDim15)
      // создадим пустой массив
      aDim15 := ARRAY(LEN(aDim14))
      AFILL( aDim15, "" )
   ENDIF

   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   cForm  := "w" + oc:cName
   oCell  := oBrw:GetCellSize(nr, nc)
   nY     := oCell:nRow   - 3 ; nX := oCell:nCol
   nW     := oCell:nWidth     ; nH := oCell:nHeight
   nHCell := nH
   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // общая ширина getbox'в в строке
   cText  := ""
   FOR nI := 1 TO LEN(aDim14)
      cPct := aDim14[nI]
      IF !IsString(cPct)
         cPct := cValToChar(aGet[nI])
      ENDIF
      cText += cPct + "0"
   NEXT
   IF LEN(aDim15) > 0
      FOR nI := 1 TO LEN(aDim15)
         cText += aDim15[nI] + "0"
      NEXT
   ELSE
      // создадим пустой массив
      aDim15 := ARRAY(LEN(aDim14))
      AFILL( aDim15, "" )
   ENDIF
   aType := {}
   FOR nI := 1 TO LEN(aGet)
      AADD( aType, VALTYPE(aGet[nI]) )
   NEXT

   lVert  := .F.
   nG     := 10
   nWBtn  := nHCell + nHCell + nG*2     // две кнопки
   nWLine := GetTxtWidth( cText, nFSize, cFont, .T. )
   nWLine += nWBtn
   nWScr  := App.Cargo:aDisplayMode[1]
   ? ProcNL(), aGet,aDim14,aDim15
   ? "aGet :=", HB_ValToExp(aGet)
   ? "aDim14 :=", HB_ValToExp(aDim14)
   ? "aDim15 :=", HB_ValToExp(aDim15)
   ? "   ширина экрана =", nWScr
   ?? "общая ширина getbox'в в строке =", nWLine
   // выход за границы экрана/прижимаем к правому концу ячейки
   IF nWLine + nX > nWScr
      lVert := .T. // вертик.расположение getbox'ов
      nH    := nHCell * LEN(aDim14) + nG/2*LEN(aDim14)
   ENDIF
   ?? "вертик.расположение getbox'ов=", lVert

   // nW -= GetVScrollBarWidth()  // сделаем до конца ячейки - НЕ НАДО !!!

   // новое окно в ячейку таблицы
   DEFINE WINDOW &cForm AT nY, nX WIDTH nW  HEIGHT nH    ;
      MODAL NOCAPTION NOSIZE FONT cFont SIZE nFSize-2    ;
      BACKCOLOR RED                                      ;
      ON INIT  DoEvents()

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nX    := nY := 2
      nHGet := nHObj := nHIco := nHCell - 4

      FOR nI := 1 TO LEN(aGet)

         cText := aDim15[nI]
         nWLbl := GetTxtWidth( cText, nFSize, cFont, .T. )

         cN := 'Lbl_' + StrZero(nI,2)
         @ nY+2, nX LABEL &cN WIDTH nWLbl HEIGHT nHGet VALUE cText ;
           FONTCOLOR WHITE RIGHTALIGN VCENTERALIGN TRANSPARENT
         IF LEN(cText) > 0
            nX += This.&(cN).Width + nG/2
         ENDIF

         cN2  := 'GBox_' + StrZero(nI,2)
         xGet := aGet[nI]
         cPct := aDim14[nI]
         nWLbl := GetTxtWidth( cPct, nFSize, cFont, .T. ) + 5
         @ nY, nX GETBOX &cN2 WIDTH nWLbl HEIGHT nHGet VALUE xGet ;
            PICTURE cPct ON INIT  {|| This.Cargo := nI }          ;
            ON CHANGE {|| aGet[This.Cargo] := This.Value, This.Value := aGet[This.Cargo] }

         IF Valtype(xGet) == "N"
            //SetProperty(This.Name, cN2, "ALIGNMENT", "LEFT")
            This.&(cN2).Alignment := "LEFT"
         ENDIF

         IF lVert
            IF nI < LEN(aDim14)
               nY += This.&(cN2).Height + nG/2
            ELSE
               nX += This.&(cN2).Width + nG/2
            ENDIF
         ELSE
            nX += This.&(cN2).Width + nG/2
         ENDIF

      NEXT

      @ nY, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
              ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR         ;
              ACTION {|| aRet := { aGet, myDim2Format(aGet,aDim14) } , ThisWindow.Release }

      This.Btn_Ok.ImageWidth  := nHIco
      This.Btn_Ok.ImageHeight := nHIco
      This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

      nX += This.Btn_Ok.Width + 5

      @ nY, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
              ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR     ;
              ACTION ( aRet := {}, ThisWindow.Release )

      This.Btn_Esc.ImageWidth  := nHIco
      This.Btn_Esc.ImageHeight := nHIco
      This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      nX += This.Btn_Esc.Width + 5

      ON KEY ESCAPE ACTION ThisWindow.Release
      ON KEY RETURN ACTION ( aRet := { aGet, myDim2Format(aGet,aDim14) }, ThisWindow.Release )

      SetWindowLong( This.Handle, GWL_STYLE, WS_BORDER)

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_myWinCalc(oBrw,aDim,oDop,aLine,cForm)
   LOCAL oTWnd, aRet, nWBtn, nHBtn, nHIco, cImg, aIcon, cVal6, lMemo
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate, nArr, a1One, cTitle
   LOCAL nY, nX, nW, nH, nG, cType, nI, nJ, aBClr, xVal, cErr, aBtnClr
   LOCAL aName, aType, aSprv, aXArr, nWLine, cMsg, cVal, nText, nVal
   LOCAL nWText, nWGBox, aFClr, nHText, aArCmb, cObj, nCol, cCap, aFont2
   LOCAL a2Code, aVal13, nHBox, owc, aRet1, aBZebra, aIco1, aIco2, aPict
   LOCAL aColor, aGrOver, aGrFill, cBtnFont, nBtnFSize, aBtnFClr, aLang
   LOCAL nY0, nX0, nW0, nH0, lWin0
   DEFAULT cForm := "Tsb_Win"

   IF App.Cargo:lPosWinOrTsb  
      // позиция окна по родит.окну / window position by parent window
      oTWnd  := _WindowObj( oBrw:cParentWnd )    // parent window
      nY0    := oTWnd:Row
      nX0    := oTWnd:Col
      nW0    := nW := oTWnd:Width
      nH0    := nH := oTWnd:Height
      lWin0  := .T.
   ELSE
      // позиция окна по ТСБ / window position according to TSB
      oTWnd  := _WindowObj( oBrw:cParentWnd )   // родительское окно
      nY     := GetWindowRow(oBrw:hWnd)
      nX     := GetWindowCol(oBrw:hWnd)
      nW     := GetWindowWidth(oBrw:hWnd)
      nH     := GetWindowHeight(oBrw:hWnd)
      lWin0  := .F.
   ENDIF

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("ComSanMS"))
   aLang  := { 'Save', 'Cancel' }
#else
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("FntBtn_1"))   // Фонт-1 кнопок других форм
   // {"Comic Sans MS"   , nFSize  , .F., .F., .F., .F., 0,  0,  0, "SuperHd" }  // Фонт суперхидера
   aLang  := { "Сохранить", "Отмена" }
#endif

   cTitle    := aLine[1] + SPACE(5) + CValToChar(App.Cargo:lPosWinOrTsb)
   cTitle    += SPACE(5) + ProcNL()
   nG        := 20
   aRet      := {}   // всегда массив - пусто, значит отказ от ввода
   // кнопки на форме
   cFont     := aFont[1]
   nFSize    := aFont[2]
   nHText    := nFSize * 2
   nHIco     := 48
   nHBtn     := nHIco + 10
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   aBtnFClr  := { BLUE , YELLOW }
   // две кнопки "Save" "Cancel"
   nWBtn     := nHIco + nG + GetTxtWidth( REPL("H",10) , nBtnFSize, cBtnFont, .T. )

   IF !ISOBJECT(oDop)
      cMsg := "Error ! Not an oDop object !"
      MsgDebug(cMsg, oDop, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF

   aName := oDop:aName                   //{"Тип заявки","Вид заявки","Массив","Дата"}
   aType := oDop:aType                   //{"S","S","A","D"}
   aSprv := oDop:aTFor                   //{ {"tipza","Ktipza","tipza",2} , {"Works","KWorks","Works",2} }
   aIcon := oDop:aIcon                   // иконки в контекстное меню
   aPict := oDop:aPict                   // может и не быть
   IF !IsArray(aPict) ; aPict := {}
   ENDIF
   IF !IsArray(aIcon) ; aIcon := {}
   ENDIF
   IF LEN(aIcon) > 0
      IF LEN(aName) # LEN(aIcon)
         cMsg := "Error ! LEN(oDop:aName) # LEN(oDop:aIcon) !"
         MsgDebug(cMsg, LEN(oDop:aName), LEN(oDop:aIcon), ProcNL(),ProcNL(1))
         RETURN {}
      ENDIF
   ENDIF
   IF !IsArray(aSprv)
      cMsg := "Error ! oDop:aForS - not an array of references !"
      MsgDebug(cMsg, oDop:aTFor, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   IF LEN(aType) # LEN(aSprv)
      cMsg := "Error ! LEN(oDop:aType) # LEN(oDop:aTFor) !"
      MsgDebug(cMsg, LEN(oDop:aType), LEN(oDop:aTFor), ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   cErr  := ""
   aXArr := Get2Dim_Spravki(aSprv,aType,@cErr)  // -> demo1_util.prg   все значения из 1/2/3/Х Dbf
   IF LEN(cErr) > 0
      cMsg := "Error ! Can't get reference data !"
      MsgDebug(cMsg, cErr, aSprv, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   //
   aBClr := oDop:aBClr
   aFClr := oDop:aFClr
   IF !IsArray(aBClr)  ;  aBClr := RED
   ENDIF
   IF !IsArray(aFClr)  ;  aFClr := YELLOW
   ENDIF
   aBZebra := oBrw:Cargo:oParam:aBZebra
   aBClr   := HMG_n2RGB(aBzebra[2])

   // первоначальные массивы значений
   cVal6  := aLine[ACOL_6]      // (6)  - функция заполнения массива для правки - коды
   aVal13 := aLine[ACOL_13]     // (13) - первоначальные значения - коды
   aRet1  := ACLONE(aVal13)     // что пришло   !!! только через ACLONE()
   nArr   := LEN(aName)
   aArCmb := ARRAY(nArr)
   a2Code := ARRAY(nArr)
   a1One  := ARRAY(nArr)
   AFILL(a1One, 0 )
   ? "=======",ProcNL() , nArr, hb_valtoexp(aRet1)
   ? SPACE(20) + "aName=", aName, hb_valtoexp(aName)
   ? SPACE(20) + "aVal13=", aVal13, hb_valtoexp(aVal13)
   ? SPACE(20) + "aType=", aType, hb_valtoexp(aType)
   ? SPACE(20) + "aRet1=", aRet1, hb_valtoexp(aRet1)
   ? SPACE(20) + "aIcon=", aIcon, hb_valtoexp(aIcon)

   IF LEN(aVal13) # LEN(aName)
      cMsg := "Error ! LEN(aVal13) # LEN(aName) ! aVal13 считывает функция: ["+cVal6+"]"
      MsgDebug(cMsg, LEN(aVal13),"#",LEN(aName), aVal13, aName, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF

   FOR nI := 1 TO nArr
      cType := aType[nI]
      xVal  := aVal13[nI]
      //? nI, cType, "xVal=", xVal
      IF cType == "S"
         aArCmb[nI] := aXArr[nI,2]    // все значения из 1/2/3/Х  Dbf
         a2Code[nI] := aXArr[nI,1]    // коды значений
         aDim       := a2Code[nI]
         FOR nJ := 1 TO LEN(aDim)
            nVal := aDim[nJ]
            IF xVal == nVal
               a1One[nI] := nJ  // номер в массиве COMBOBOX
                //?? "Ok=", nJ
               // исправим вход.данные на порядк.номер массива Combo
               aRet1[nI] := nJ
               EXIT
            ENDIF
         NEXT
      ELSEIF cType == "A"
      ENDIF
   NEXT

   // расчёт ширины/высоты окна
   nText := nVal := 0
   nHBox := nG                // высота отступа сверху
   lMemo := .F.
   FOR nI := 1 TO LEN(aName)
      nText := MAX(nText,LEN(aName[nI]))
//? nI, aName[nI], nText
      cType := aType[nI]
      IF cType == "S"
         aDim := aXArr[nI,2]    // все значения из двух Dbf
         FOR nJ := 1 TO LEN(aDim)
            nVal := MAX(nVal,LEN(aDim[nJ]))
         NEXT
      ELSEIF cType == "D"
         nVal := LEN("dd'.'0MMMM'0'yyyy" + "00")
      ELSEIF cType == "C"
         nVal := 40
      ELSEIF cType == "M"
         nVal := 40
         lMemo := .T.
      ELSEIF cType == "N"
         nVal := 14
      ELSEIF cType == "A"
         aDim := aXArr[nI,2]    // все значения из массива - {"отключить", "включить"}
         FOR nJ := 1 TO LEN(aDim)
            nVal := MAX(nVal,LEN(aDim[nJ]))
         NEXT
      ELSE
         nVal := LEN("REZERV")
      ENDIF
      nHBox += nHText + nG
   NEXT
   IF lMemo
      nHBox += nHText*2  // для мемо-поля
   ENDIF
   nHBox  += nG + nHBtn + nG*2
   nHBox  += GetTitleHeight() + GetBorderHeight()
   cText  := REPL("H",nText) + "HH"
   cVal   := REPL("H",nVal)  + "HH"
   nWText := GetTxtWidth( cText, nFSize, cFont, .T. )
   nWGBox := GetTxtWidth( cVal, nFSize, cFont, .T. )
   nWLine := nG + nWText + nG/2
   nWLine += nWGBox + nG*2
   nWDate := GetTxtWidth( "dd0'.'0MMMM'0'yyyy0" , nFSize, cFont, .T. )

   nX := (nW - nWLine)/2 + oTWnd:Col
   nW := nWLine                       // ширина окна
   IF (nWBtn*2 + nG*3) > nWLine
      nW := (nWBtn*2 + nG*3)
   ENDIF
   nH := nHBox
   ? ProcNL(), "******** ["+cForm+"] высота окна =", nH
   nY := (oTWnd:Height - oTWnd:Row - nH) / 2
   ?? "nY=",nY
   // testing
   IF ( nY + nH ) > Sys.ClientHeight
      nI := ( nY + nH ) - Sys.ClientHeight
      nY -= nI
   ENDIF
   nX := iif( nX < 0, 0, nX )

   aColor  := GRAY
   aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aBtnClr := { {225,225,225}, GRAY }

   IF lWin0
      // позиция окна по родит.окну / window position by parent window
      nX := ( nW0 - nW ) / 2 + nX0
      nY := ( nH0 - nH ) / 2 + nY0
   ENDIF

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle   ;
          MODAL NOSIZE FONT cFont  SIZE nFSize BACKCOLOR aBClr     ;
          ON INIT ( This.Label_0.Setfocus, DoEvents() )
          This.Cargo := oHmgData() ; owc := This.Cargo

      nY        := nX := nG
      nW        := This.ClientWidth
      nH        := This.ClientHeight
      owc:nFor  := 0                  // нажата кнопка/getbox/combo
      owc:aRet1 := aRet1              // массив значений для правки

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      FOR nI := 1 TO LEN(aName)
         cType := aType[nI]
         cText := aName[nI] + ":"
         cObj  := "Lbl_" + STRZERO(nI,2)

         @ nY, nX LABEL &cObj VALUE cText WIDTH nWText HEIGHT nHText ;
                  BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT

         nCol := nX + This.&(cObj).Width + nG/2
         cObj := "GBox_" + STRZERO(nI,2)

         IF cType     == "C"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] BOLD ;
                       WIDTH nWGBox HEIGHT nHText        ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }   // правим массив в контейнере формы

            This.&(cObj).Cargo := nI

         ELSEIF cType  == "N"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] BOLD ;
                       WIDTH nWGBox HEIGHT nHText        ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

            This.&(cObj).Alignment := "LEFT"  // "CENTER" or "RIGHT"
            This.&(cObj).Cargo := nI

         ELSEIF cType == "D"
            dDate := aRet1[nI]
            @ nY, nCol DATEPICKER &cObj VALUE dDate WIDTH nWGBox HEIGHT nHText ;
                       DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE BOLD             ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

            This.&(cObj).Cargo := nI

         ELSEIF cType == "A"
            aDim := aXArr[nI,2]    // все значения из массива - {"отключить", "включить"}
            cImg := aIcon[nI]
            IF aRet1[nI] == 0               ; cCap := "- - -"
            ELSEIF aRet1[nI] <= LEN(aDim)   ; cCap := aDim[aRet1[nI]]
            ELSE                            ; cCap := "??? code: " + HB_NtoS(aRet1[nI])
            ENDIF
            //cCap += SPACE(70) + "..."
            cObj := "Btn_" + STRZERO(nI,2)
            @ nY, nCol BUTTONEX &cObj WIDTH nWGBox HEIGHT nHText CAPTION cCap         ;
              PICTURE "ArrowDown20" NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT           ;
              FONTCOLOR BLACK BACKCOLOR aBtnClr[1] BOLD                               ;
              ON MOUSEHOVER ( This.Fontcolor := WHITE, This.Backcolor := aBtnClr[2] ) ;
              ON MOUSELEAVE ( This.Fontcolor := BLACK, This.Backcolor := aBtnClr[1] ) ;
              ACTION  {| | This.Enabled := .F., _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) } ;
              ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                           o:nBtn  := nI             ,;  // номер нажатой кнопки
                           o:nRet  := 0              ,;  // возврат значения
                           o:nPost := 10             ,;  // номер события
                           o:cBtn  := This.Name      ,;
                           o:cCapt := This.Caption   ,;
                           o:aBClr := This.BackColor ,;
                           o:nY    := This.Row       ,;
                           o:nX    := This.Col       ,;
                           o:aFClr := aBtnFClr       ,;
                           o:cTxt  := cCap           ,;
                           o:aTxt  := aDim           ,;
                           o:cIco  := cImg        }

              Btn_MaxTxtWidth(cForm, cObj)  // максимальный размер надписи с пробелами
              //? _o2log(This.&(cObj).Cargo, 27, ProcNL() + "  This.&("+cObj+").Cargo => ", .T. ) // проверка
              // проверить простым способом
              //ACTION {|| This.Enabled := .F., ; //!!!
              //           _LogFile(.T.,"$$$$$", This.Name, This.Cargo, ThisWindow.Name), ;
              //           _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) } ;

         ELSEIF cType == "S"
            aDim  := aArCmb[nI]
            cObj  := "CmBox_" + STRZERO(nI,2)
            @ nY, nCol COMBOBOXEX &cObj BOLD WIDTH nWGBox HEIGHT 300  ;
                       ITEMS aDim  VALUE a1One[nI]                    ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value ,;   // правим массив в контейнере формы
                                     This.Label_0.Setfocus   }
            This.&(cObj).Cargo := nI

         ELSEIF cType == "M"
            cObj  := "EditBox_" + STRZERO(nI,2)
            @ nY, nCol EDITBOX &cObj WIDTH nWGBox HEIGHT nHText * 4  ;
              VALUE aRet1[nI] BACKCOLOR {240, 240, 240} NOTABSTOP    ;
              ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }

            This.&(cObj).Cargo := nI

         ELSE
            cMsg := "Error ! cType == ??? [" + cType + "] !"
            MsgDebug(cMsg, cType, nI, cText)

         ENDIF

         nY += This.&(cObj).ClientHeight + nG

      NEXT

      /////////////////////// кнопки на форме ////////////////////////////////
      nY    := nH - nG - nHBtn
      nX    := ( nW - nWBtn * 2 - nG ) / 2
      aIco1 := { "iMg_Ok48x1"    , "iMg_Ok48x2"     }
      aIco2 := { "iMg_Cancel48x1", "iMg_Cancel48x2" }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[1] ;
               ICON aIco1[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco1[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco1[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(90 ,, This.Name) }

      nX += This.Btn_Ok.Width + nG

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[2] ;
               ICON aIco2[1] NOXPSTYLE HANDCURSOR NOTABSTOP                ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD     ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                      ;
               ON MOUSEHOVER ( This.Icon := aIco2[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco2[1],         ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(98 ,, This.Name) }

      ON KEY ESCAPE ACTION _wPost(98)

      IF !lMemo
         ON KEY RETURN ACTION _wPost(90)
      ENDIF

      (This.Object):Event( 10, {|ow,ky,cn|  // выбор по кнопкам
                                 Local aTxt, cImg, aRet, cCapt, ocBtn, nI, nRet
                                 Local owc := ow:Cargo           // cargo окна
                                 ocBtn := This.&(cn).Cargo       // cargo нажатой кнопки
                                 _SetThisFormInfo(ow)            // save This среду окна ow
                                 aTxt  := ocBtn:aTxt
                                 cImg  := ocBtn:cIco
                                 nRet  := ocBtn:nRet             // первоначальное значение
                                 nI    := ocBtn:nBtn             // номер нажатой кнопки
                                 owc:nFor := nI
                                 cCapt := ocBtn:cCapt
                                 //MsgDebug(ow:Name, ky, cn, ocBtn, "nI=",nI,owc:aRet1)
                                 aRet := Btn_ContexMenu(ow, ocBtn:nY, ocBtn:nX, aTxt, cImg, .T.)
                                 IF LEN(aRet) > 0
                                     owc:aRet1[nI] := aRet[1]     // ВОТ ТАК НУЖНО !!! -->> правим массив в контейнере формы
                                     ocBtn:cCapt   := aRet[2]
                                     nRet          := aRet[1]     // возврат значения
                                     cCapt         := aRet[2]
                                     ocBtn:nRet    := nRet        // НЕ возвращает значение выше
                                 ENDIF
                                 _SetThisFormInfo()               // restore This среду окна ow
                                 This.&(cn).Enabled    := .T.
                                 This.&(cn).Caption    := cCapt   // возврат на кнопку
                                 This.&(cn).Cargo:nRet := nRet    // НЕ возвращает значение выше
                                 Btn_MaxTxtWidth(ow:Name, cn)     // максимальный размер надписи с пробелами
                                 ow:Setfocus('Label_0')
                                 DO EVENTS
                                 ky := cn
                                 Return Nil
                                 } )

      (This.Object):Event( 90, {|ow,ky,cn|  // сохранить данные
                                 Local aRet2, owc := ow:Cargo
                                 aRet2 := owc:aRet1           // массив значений для правки
                                 ? "### Save", ProcNL(), HB_ValToExp(aRet2)
                                 cVal := myCode2Str(@aRet2, aXArr, aType, aPict)
                                 ? "### myCode2Str()", HB_ValToExp(aRet2), cVal
                                 aRet := {aRet2,cVal}
                                 _wPost(99,ow)
                                 ky := cn
                                 Return Nil
                                 } )

      (This.Object):Event( 98, {|ow| aRet := {}, _wPost(99,ow) } )

      (This.Object):Event( 99, {|ow| ow:Release() } )

      //SetWindowLong( This.Handle, GWL_STYLE, WS_BORDER)
   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////////////
// Функция заменит на максимальный размер надписи с пробелами
FUNCTION Btn_MaxTxtWidth(cForm, cObj)
   LOCAL cFType, cFName, nFSize, lFBold, nWidth, nWTxt, cText

   cFType := GetProperty( cForm, cObj, "Type" )
   IF cFType == "LABEL" .OR. cFType == "GETBOX" .OR. cFType == "TEXTBOX"
      cText := GetProperty( cForm, cObj, "Value"   )
   ELSEIF cFType == "OBUTTON"
      cText := GetProperty( cForm, cObj, "Caption" )
   ELSE
      cText := ProcNL() + " - нет объекта [" + cObj + "]"
   ENDIF
   cFName  := GetProperty( cForm, cObj, "FontName"     )
   nFSize  := GetProperty( cForm, cObj, "FontSize"     )
   lFBold  := GetProperty( cForm, cObj, "FontBold"     )
   nWidth  := GetProperty( cForm, cObj, "ClientWidth"  ) - 20 - 10*2 // иконка

   DO WHILE .T.
      cText += SPACE(5)
      nWTxt := GetTxtWidth( cText, nFSize, cFName, lFBold )
      IF nWTxt > nWidth
         cText := SUBSTR(cText,1,LEN(cText)-5)
         EXIT
      ENDIF
   ENDDO

   SetProperty( cForm, cObj, "Caption", cText )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Btn_ContexMenu(oJWnd, nY0, nX0, aDim, cImg, lIcon, lDelMn)
   LOCAL hFont1, hFont2, aRet, nI, cMenu, bAction, nY, nX
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, nMenu
   LOCAL aFont, nFSize, cName, cForm, aMsg
   DEFAULT lIcon  := .T.   // иконки в меню, иначе BMP
   DEFAULT lDelMn := .T.   // меню удалить

   cForm  := oJWnd:Name
   nY     := oJWnd:Row + nY0 + GetTitleHeight() + 5
   nX     := oJWnd:Col + nX0 + GetBorderWidth()

#ifdef KEY_ENG // for this project demo1-en.hbp
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("ComSanMS")
   aMsg   := { "Delete value", "Exit" }
#else
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("FntBtn_1")   // Фонт-1 кнопок других форм
   aMsg   := { "Удалить значение", "Выход" }
#endif

   aFont  := GetFontParam(hFont1)
   nFSize := aFont[2]
   nMenu  := 0

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cMenu   := aDim[nI]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         hFont   := IIF( lDis, hFont2, hFont1 )

         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      SEPARATOR
      IF lDelMn
         MENUITEM  aMsg[1] ACTION  {|| nMenu := -1 } FONT hFont2
         SEPARATOR
      ENDIF
      MENUITEM  aMsg[2] ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      aRet := { nMenu, aDim[nMenu] }
   ELSEIF nMenu == -1
      aRet := { 0, "-.-" }
   ELSE
      aRet := {}
   ENDIF

   DO EVENTS

RETURN aRet

////////////////////////////////////////////////////////////////////////////////
FUNCTION myContexMenuSort(oWnd,nKy,cName)
   LOCAL cForm, oBrw, nI, hFont1, hFont2, nMenu //,  nY, nX,

   ? "===>", ProcNL(), oWnd, nKy, cName
   cForm    := oWnd:Name
   oBrw     := oWnd:Cargo:oTbl              // получить данные из объекта
   hFont1   := GetFontHandle( "MnNormal" )
   hFont2   := GetFontHandle( "ComSanMS" )
   nMenu    := 0

   //nY       := GetProperty(cForm, "Row") + GetTitleHeight()
   //nY       += GetProperty(cForm, cNBtn, "Row")
   //nY       += GetProperty(cForm, cNBtn, "Height") + 3
   //nX       := GetProperty(cForm, "Col") + GetBorderWidth()
   //nX       += GetProperty(cForm, cNBtn, "Col") - 4

   SET MENUSTYLE EXTENDED      // switch menu style to advanced
   SetMenuBitmapHeight( 28 )   // set image size

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM 'Скрыть все строки таблицы = 0' ACTION {|| nMenu := 1 } FONT hFont1 IMAGE "bEye24"
       SEPARATOR
       MENUITEM 'Скрыть %2  строки таблицы = 0' ACTION {|| nMenu := 2 } FONT hFont1 IMAGE "bEye24"
       SEPARATOR
       MENUITEM 'Показ всех строк таблицы > 0'  ACTION {|| nMenu := 3 } FONT hFont2 IMAGE "bEye24"
       SEPARATOR
   END MENU

   _ShowContextMenu(cForm, , , .f. )     // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu > 0
      //oBrw:lDrawLine := .F.        // блокировать прорисовку
      oBrw:Hide()
      oBrw:Cargo:nModify ++          // счётчик-изменения в таблице
      FOR nI := 1 TO oBrw:nLen
         oBrw:GotoRec(nI)
         IF nMenu == 1
            oBrw:SetValue(cName,0)
         ELSEIF nMenu == 2
            IF nI % 2 == 0
               oBrw:SetValue(cName,0)
            ENDIF
         ELSEIF nMenu == 3
            oBrw:SetValue(cName,nI)
         ENDIF
         //oBrw:DrawSelect()        // перерисовать текущую ячейку таблицы
      NEXT
      oBrw:GoTop()
      oBrw:Refresh()
      //oBrw:lDrawLine := .T.
      oBrw:Show()
      oBrw:Setfocus()
   ENDIF

   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////
FUNCTION myCode2Str(aRet, aXArr, aType, aPict)
   LOCAL nI, nJ, cType, a2Val, a2Code, cRet, nVal, aDim, aDim2
   LOCAL cVal, nK, nCode
   DEFAULT aPict := {}

   cRet   := ""
   nJ     := LEN(aType)
   a2Val  := ARRAY(nJ)
   a2Code := ARRAY(nJ)

   FOR nI := 1 TO nJ
      cType := aType[nI]
      IF cType     == "C" .OR. cType == "M"
         cRet += ALLTRIM(aRet[nI])
         //aRet[nI] := ALLTRIM(aRet[nI]) - нельзя, иначе нужно вводить формат ввода значения
      ELSEIF cType == "N"
         IF LEN(aPict) > 0
            cRet += TRANSFORM(aRet[nI], aPict[nI])
         ELSE
            cRet += HB_NtoS(aRet[nI])
         ENDIF
      ELSEIF cType == "D"
         cRet += DTOC(aRet[nI])
      ELSEIF cType == "S"
         cVal       := "???"
         nVal       := aRet[nI]       // это порядк.номер в массиве
         a2Val[nI]  := aXArr[nI,2]    // все значения из 1/2/3/Х  Dbf
         a2Code[nI] := aXArr[nI,1]    // коды значений
         aDim       := a2Val[nI]
         aDim2      := a2Code[nI]
         ?? "nVal=",nVal, "aDim2=", HB_ValToExp(aDim2), HB_ValToExp(aDim)
         nCode      := aDim2[nVal]
         ?? "nCode=",nCode
         FOR nK := 1 TO LEN(aDim2)
            IF nCode == aDim2[nK]
               ?? "->"+aDim[nK], nK
               cVal     := aDim[nK]     // строка из массива COMBO
               aRet[nI] := nCode
               EXIT
            ENDIF
         NEXT
         cRet += cVal
      ELSEIF cType == "A"
         nVal := aRet[nI]       // это порядк.номер в массиве
         aDim := aXArr[nI,2]    // все значения из массива {"отключить", "включить", "...."}
         IF nVal == 0
            cVal := "-"
         ELSEIF nVal > LEN(aDim)
            cVal := "??? code: " + HB_NtoS(nVal)
         ELSE
            cVal := aDim[nVal]     // строка из массива
         ENDIF
         cRet += cVal
      ENDIF
      cRet += IIF( nI == nJ, "", " , " )
   NEXT
   ? ProcNL(), HB_ValToExp(aRet)

RETURN cRet

///////////////////////////////////////////////////////////////////////
FUNCTION Tsb_myWinCalc2(oBrw,aDim,oDop,aLine, cForm)
   LOCAL oTWnd, aRet, nWBtn, nHBtn, nHIco, nWLbl, aFont2, aLang
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate, nArr, a1One
   LOCAL nY, nX, nW, nH, nG, nI, nJ, cType, aBClr, xVal, cTitle
   LOCAL aName, aType, aSprv, aXArr, nWLine, cMsg, cVal, nText, nVal
   LOCAL nWText, nWGBox, aFClr, nHText, aArCmb, cObj, nCol, lDay
   LOCAL a2Code, aVal13, nHBox, owc, aRet1, aBZebra, aIco1, aIco2
   LOCAL aColor, aGrOver, aGrFill, cBtnFont, nBtnFSize, aBtnFClr
   LOCAL cFileIni, cMetka, cObjDate, a3Code, a3Val, dZDate
   LOCAL nY0, nX0, nW0, nH0, lWin0
   DEFAULT cForm := "Tsb_Win2"

   dZDate := oBrw:Cargo:dZDate             // Дата заявки
   IF App.Cargo:lPosWinOrTsb
      // позиция окна по родит.окну / window position by window
      oTWnd  := _WindowObj( oBrw:cParentWnd )    // parent window
      nY0    := oTWnd:Row
      nX0    := oTWnd:Col
      nW0    := nW := oTWnd:Width
      nH0    := nH := oTWnd:Height
      lWin0  := .T.
   ELSE
      // позиция окна по ТСБ / window position according to TSB
      oTWnd  := _WindowObj( oBrw:cParentWnd )   // родительское окно
      nY     := GetWindowRow(oBrw:hWnd)
      nX     := GetWindowCol(oBrw:hWnd)
      nW     := GetWindowWidth(oBrw:hWnd)
      nH     := GetWindowHeight(oBrw:hWnd)
      lWin0  := .F.
   ENDIF

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("ComSanMS"))
   aLang  := { 'Save', 'Cancel' , "Application receipt date: ", "Accounting for weekends (Saturday and Sunday)" ,;
               "Day of completion:", "Completion date" }
#else
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("FntBtn_1"))   // Фонт-1 кнопок других форм
   aLang  := { "Сохранить", "Отмена", "Дата поступления заявки: ", "Учитывать выходные дни (суббота и воскресенье)" ,;
               "Дата выполнения:" , "выполнение"}
#endif

   nG        := 20
   aRet      := {}   // всегда массив - пусто, значит отказ от ввода
   cTitle    := aLine[1] + SPACE(5) + CValToChar(App.Cargo:lPosWinOrTsb)
   cTitle    += SPACE(5) + ProcNL()
   // кнопки на форме
   cFont     := aFont[1]
   nFSize    := aFont[2]
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   nHText    := nFSize * 2
   nHIco     := 48
   nHBtn     := nHIco + 10
   // две кнопки "Save" "Cancel"
   nWBtn     := nHIco + nG + GetTxtWidth( REPL("H",10) , nBtnFSize, cBtnFont, .T. )
   aBtnFClr  := { BLUE , YELLOW }
   lDay      := .F.
   cFileIni  := App.Cargo:cPathTemp + "tmp_myWinCalc2.ini"
   cMetka    := "24.03.25"   // для ввода новых данных в ини-файл
   // считать введённые ранее данные
   IniLoadFileChk(cFileIni,cMetka,@lDay)

   IF !ISOBJECT(oDop)
      cMsg := "Error ! Not an oDop object !"
      MsgDebug(cMsg, oDop, ProcNL(),ProcNL(1))
      RETURN NIL
   ENDIF

   aName := oDop:aName // {"Тип срочности","Срок выполнения"}
   aType := oDop:aType // {"S","D"}
   aSprv := oDop:aSprv // { {"srokza","Ksrokza","srokza","srokza0","srokza2"} , {} }
   // заполняется для справочника "S", для других не надо
   //oSrkZa:aCod1 := { 1, 2, 3, 4, 7, 5, 6, 10, 20, 30 }
   //oSrkZa:aVal1 := { "1-день", "2-дня", "3-дня", "4-дня", "7-дней", "вне очереди (4 часа)","текущий день (до 17:30)", "ремонтная (10-дней)", "ремонтная (20-дней)", "конец текущего месяца" }
   aXArr := oDop:aXArr // { {oSrkZa:aCod1,oSrkZa:aVal1} , {}  }        // все значения из одной Dbf + дата
   //oSrkZa:aFld  := {"Ksrokza","DateSrok" }  // поля записи в базу

   aBClr := oDop:aBClr // цвет внутри
   aFClr := oDop:aFClr //  MAROON    // PINK  PURPLE
   IF !IsArray(aBClr)  ;  aBClr := RED
   ENDIF
   IF !IsArray(aFClr)  ;  aFClr := YELLOW
   ENDIF
   aBZebra := oBrw:Cargo:oParam:aBZebra
   aBClr   := HMG_n2RGB(aBzebra[2])

   // первоначальные массивы значений
   aVal13 := aLine[13]          // (13) - первоначальные значения - коды
   aRet1  := ACLONE(aVal13)     // что пришло   !!! только через ACLONE()
   nArr   := LEN(aName)
   aArCmb := ARRAY(nArr)
   a2Code := ARRAY(nArr)
   a1One  := ARRAY(nArr)
   AFILL(a1One, 0 )
   IF LEN(aVal13) # LEN(aName)
      cMsg := "Error ! LEN(aVal13) # LEN(aName) !"
      MsgDebug(cMsg, aVal13, aName, ProcNL(),ProcNL(1))
   ENDIF
   ? "=======",ProcNL() , nArr, hb_valtoexp(aRet1)
   FOR nI := 1 TO nArr
      cType := aType[nI]
      xVal  := aVal13[nI]
      ? nI, cType, "xVal=", xVal
      IF cType == "S"
         aArCmb[nI] := aXArr[nI,2]    // все значения из 1/2/3/Х  Dbf
         a2Code[nI] := aXArr[nI,1]    // коды значений
         aDim       := a2Code[nI]
         FOR nJ := 1 TO LEN(aDim)
            nVal := aDim[nJ]
            IF xVal == nVal
               a1One[nI] := nJ  // номер в массиве COMBOBOX
                ?? "Ok=", nJ
               // исправим вход.данные на порядк.номер массива Combo
               aRet1[nI] := nJ
               EXIT
            ENDIF
         NEXT
      ENDIF
   NEXT
   a3Code := a2Code[1]  // { "1-день", "2-дня", "3-дня"...
   a3Val  := aArCmb[1]  // { "1-день", "2-дня", "3-дня"...

   // расчёт ширины/высоты окна
   nText := nVal := 0
   nHBox := nG + nHText*2 + nG*3          // высота отступа сверху
   FOR nI := 1 TO LEN(aName)
      nText := MAX(nText,LEN(aName[nI]))
//? nI, aName[nI], nText
      cType := aType[nI]
      IF cType == "S"
         aDim := aXArr[nI,2]    // все значения из двух Dbf
         FOR nJ := 1 TO LEN(aDim)
            nVal := MAX(nVal,LEN(aDim[nJ]))
         NEXT
      ELSEIF cType == "D"
         nVal := LEN("dd'.'00MMMM'000'yyyy" + "00")
      ELSEIF cType == "C"
         nVal := 40
      ELSEIF cType == "N"
         nVal := 14
      ELSE
         nVal := LEN("REZERV")
      ENDIF
      nHBox += nHText + nG
   NEXT
   nHBox  += nG + nHText + nG + nHBtn + nG*2
   nHBox  += GetTitleHeight() + GetBorderHeight()
   cText  := REPL("H",nText) + "HH"
   cVal   := REPL("H",nVal)  + "HH"
   nWText := GetTxtWidth( cText, nFSize, cFont, .T. ) + 20
   nWGBox := GetTxtWidth( cVal, nFSize, cFont, .T. )
   nWLine := nG + nWText + nG/2
   nWLine += nWGBox + nG*2
   nWDate := GetTxtWidth( "dd0'.'0MMMM'0'yyyy0" , nFSize, cFont, .T. )
   //
   nX := (nW - nWLine)/2 + oTWnd:Col
   nW := GetTxtWidth( aLang[4] , nFSize, cFont, .T. ) + nG*2 + 40  // CheckT28
   nH := nHBox
   ? ProcNL(), "******** ["+cForm+"] высота окна =", nH
   nY := (oTWnd:Height - oTWnd:Row - nH) / 2
   ?? "nY=",nY
   // testing
   IF ( nY + nH ) > Sys.ClientHeight
      nI := ( nY + nH ) - Sys.ClientHeight
      nY -= nI
   ENDIF
   nX := iif( nX < 0, 0, nX )

   IF lWin0
      // позиция окна по родит.окну / window position by parent window
      nX := ( nW0 - nW ) / 2 + nX0
      nY := ( nH0 - nH ) / 2 + nY0
   ENDIF

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle ;
          MODAL NOSIZE FONT cFont SIZE nFSize  BACKCOLOR aBClr   ;
          ON INIT ( This.Label_0.Setfocus, DoEvents() )
          This.Cargo := oHmgData() ; owc := This.Cargo

      nY := nX := nG
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      cText  := aLang[3] + DTOC(dZDate)
      cText  += " - " + CDoW(dZDate)
      nWLbl  := GetTxtWidth( cText, nFSize, cFont, .F. ) + 10

      @ nY, nX LABEL Label_1 VALUE cText WIDTH nW HEIGHT nHText ;
               FONTCOLOR BLACK TRANSPARENT CENTERALIGN

      nY += This.Label_1.Height + nG

      cText := aLang[4]
      nWLbl := GetTxtWidth( cText, nFSize, cFont, .T. ) + 40
      nCol  := ( nW - nWLbl ) / 2

      @ nY, nCol CHECKLABEL Chk_1 WIDTH nWLbl HEIGHT 35 VALUE cText ;
                 LEFTCHECK IMAGE { 'CheckT28', 'CheckF28' }         ;
                 FONTCOLOR BLACK BACKCOLOR aBClr /*BOLD*/           ;
                 ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )        ;
                 ON INIT {|| This.Checked := lDay }                 ;
                 ACTION  {|| This.Checked := ! This.Checked, ;
                             lDay := This.Checked }

      nY += This.Chk_1.Height + nG

      FOR nI := 1 TO LEN(aName)
         cType := aType[nI]
         cText := aName[nI] + ":"
         cObj  := "Lbl_" + STRZERO(nI,2)

         @ nY, nX LABEL &cObj VALUE cText WIDTH nWText HEIGHT nHText ;
                  BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT

         This.&(cObj).Cargo := nI

         nCol := nX + This.&(cObj).Width + nG/2
         cObj := "GBox_" + STRZERO(nI,2)

         IF     cType  == "C"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] BOLD ;
                       WIDTH nWGBox HEIGHT nHText        ;
                       ON CHANGE {|| aRet1[ This.Cargo ] := This.Value }

         ELSEIF cType  == "N"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] WIDTH nWGBox HEIGHT nHText ;
                       BOLD ON CHANGE ( aRet1[ This.Cargo ] := This.Value )
            This.&(cObj).Alignment := "LEFT"   // "CENTER" or "RIGHT"

         ELSEIF cType == "D"
            dDate := aRet1[nI]
            @ nY, nCol DATEPICKER &cObj VALUE dDate WIDTH nWGBox BOLD       ;
                       HEIGHT nHText DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE ;
                       ON CHANGE ( aRet1[ This.Cargo ] := This.Value )

            cObjDate := cObj

         ELSEIF cType == "S"
            aDim  := aArCmb[nI]
            cObj  := "CmBox_" + STRZERO(nI,2)
            @ nY, nCol COMBOBOXEX &cObj WIDTH nWGBox HEIGHT 300 ;
                       ITEMS aDim  VALUE a1One[nI]   BOLD       ;
                       ON CHANGE ( aRet1[ This.Cargo ] := This.Value,;
                                   myChangeDate(This.Value,lDay,dZDate,a3Code,a3Val,cObjDate,"Say_Day"), ;
                                   This.Label_0.Setfocus )
         ENDIF

         This.&(cObj).Cargo := nI

         nY += This.&(cObj).ClientHeight + nG

      NEXT

      cText := aLang[5]

      @ nY, nX LABEL Label_Day VALUE cText WIDTH nWText HEIGHT nHText ;
               BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT

      nCol := nX + This.Label_Day.Width + nG/2

      ? dDate := aRet1[2]
      cText := CDOW(dDate)

      @ nY, nCol LABEL Say_Day VALUE cText WIDTH nWGBox HEIGHT nHText ;
                 BOLD FONTCOLOR aFClr TRANSPARENT

      nY += This.Label_Day.ClientHeight + nG

      /////////////////////// buttons on the form ////////////////////////////////
      nY := nH - nG - nHBtn
      nX := ( nW - nWBtn * 2 - nG ) / 2

      aColor  := GRAY
      aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
      aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }

      aIco1   := { "iMg_Ok48x1"      , "iMg_Ok48x2"     }
      aIco2   := { "iMg_Cancel48x1"  , "iMg_Cancel48x2" }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[1] ;
               ICON aIco1[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco1[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco1[1], ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {||
                       ? "### Save", ProcNL(), HB_ValToExp(aRet1)
                       IniSaveFileChk(cFileIni, cMetka, lDay)
                       cVal := myCode2Str(@aRet1, aXArr, aType)
                       cVal += " - " + aLang[6] + " - "
                       cVal += CDoW(This.&(cObjDate).Value)
                       ? "### myCode2Str()", HB_ValToExp(aRet1), cVal
                       aRet := {aRet1,cVal}
                       ThisWindow.Release
                       Return Nil
                       }

      nX += This.Btn_Ok.Width + nG

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[2] ;
               ICON aIco2[1] NOXPSTYLE HANDCURSOR NOTABSTOP               ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD    ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                     ;
               ON MOUSEHOVER ( This.Icon := aIco2[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco2[1],         ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION ( aRet := {}, ThisWindow.Release )

      ON KEY ESCAPE ACTION ThisWindow.Release
      ON KEY RETURN ACTION ThisWindow.Release

      //SetWindowLong(This.Handle, GWL_STYLE, WS_BORDER)

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

/////////////////////////////////////////////////////////////////////////////////
Static Function myChangeDate(nI,lDay,dZDate,a3Code,a3Val,cObjDate,cObjSay)
   LOCAL nDay, cVal, dZDt := dZDate

   //#  1  2  3  4  5  6  7  8   9   10  - порядковый номер в массиве
   // { 1, 2, 3, 4, 7, 5, 6, 10, 20, 30 }
   // { "1-день", "2-дня", "3-дня", "4-дня", "7-дней", "вне очереди (4 часа)","текущий день (до 17:30)",;
   //      "ремонтная (10-дней)", "ремонтная (20-дней)", "конец текущего месяца" }

   nDay := 0
   // nI - порядковый номер в массиве
   IF nI >= 1 .AND. nI <= 5
      dZDate += a3Code[nI]
      nDay   := a3Code[nI]
   ELSEIF nI >= 6 .AND. nI <= 7  // вне очереди (4 часа) , текущий день (до 17:30)
      //dZDate += 0
      nDay   := 0
   ELSEIF nI >= 8 .AND. nI <= 9
      dZDate += a3Code[nI]
      nDay   := a3Code[nI]
   ELSEIF nI == 10
      dZDate := EOM(dZDate)
      nDay   := a3Code[nI]
   ELSE
      MsgDebug("Error! nI=",nI,"Out of array range !",a3Code,a3Val)
   ENDIF

   IF lDay
      IF DoW(dZDate) == 7  // суббота
         dZDate += 1
      ENDIF
      IF DoW(dZDate) == 1  // воскресенье
         dZDate += 1
      ENDIF
   ENDIF
   cVal := DTOC(dZDate) + "-" + CDoW(dZDate)+" !"

   This.&(cObjDate).Value := dZDate
   This.&(cObjSay).Value  := CDoW(dZDate)
   //MsgDebug(nI, "nDay=",nDay, a3Val[nI],DTOC(dZDt),a3Code,cObjDate,cVal)
RETURN NIL

///////////////////////////////////////////////////////////////////////
// считать данные с ини-файла
Static Function IniLoadFileChk(cFileIni, cMetkaIni, lLog)
   LOCAL cStr, aRet

   IF !FILE(cFileIni)
      IniSaveFileChk(cFileIni, cMetkaIni, lLog )
   ENDIF

   cStr := ALLTRIM( hb_MemoRead(cFileIni) )
   IF LEN(cStr) == 0
     // нет данных
   ELSE
      // чтобы при добавлении нового параметра была смена без ошибки
      IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0 .AND. AT( cMetkaIni, cStr ) > 0
         aRet      := &cStr
         lLog      := aRet[1]
         cMetkaIni := aRet[2]  // чтобы при добавлении нового параметра была смена без ошибки
      ELSE
        // нет данных
      ENDIF
   ENDIF

Return Nil

//////////////////////////////////////////////////////////////////////////////////////
Static Function IniSaveFileChk(cFileIni, cMetkaIni, lLog)
   LOCAL aSave
   // значения первоначальные
   aSave := { lLog, cMetkaIni, App.ExeName }
   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )
Return Nil


///////////////////////////////////////////////////////////////////////
// oVipZa:aName := {"Тип выполнения", "Дата выполнения", "Время выполнения", "Отправка на сайт"}
// oVipZa:aType := {"S","D","N","N" }
// oVipZa:aSpav := { {"VipZa","KVipZa","VipZa",2,""} , {}, {}, {} }
// oVipZa:aBClr := BLUE      // цвет внутри
// oVipZa:aFClr := MAROON    // PINK  PURPLE        // Флаг для передачи на сайт
// oVipZa:aFld  := {"Ksrokza", "DATEVip", "TimeVip", "INET" }  // поля записи в базу                                                                        //  8    9  10  11   12  13           14     15
// AADD( aDim, {"(*) Тип выполнения заявки", SPACE(20), 2, "CALC"  , "" , "Za_VipZa()", "SetDim2Wrt()" ;
// , "myWinCalc3()" , "W", "", "", "", nil, oVipZa:aFld , oVipZa  } )
///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_myWinCalc3(oBrw,aDim,oDop,aLine, cForm)
   LOCAL oTWnd, aRet, nWBtn, nHBtn, nHIco, cImg, aIcon, cVal6, lMemo, aPict
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate, nArr, a1One, nY2
   LOCAL nY, nX, nW, nH, nG, cType, nI, nJ, aBClr, xVal, cErr, aBtnClr
   LOCAL aName, aType, aSprv, aXArr, nWLine, cMsg, cVal, nText, nVal, nX2
   LOCAL nWText, nWGBox, aFClr, nHText, aArCmb, cObj, nCol, cCap, aFont2
   LOCAL a2Code, aVal13, nHBox, owc, aRet1, aBZebra, aIco1, aIco2, cObj1
   LOCAL aColor, aGrOver, aGrFill, cBtnFont, nBtnFSize, aBtnFClr, aLang
   LOCAL nY0, nX0, nW0, nH0, lWin0, cTitle
   DEFAULT cForm := "Tsb_Win3"

   IF App.Cargo:lPosWinOrTsb
      // позиция окна по родит.окну / window position by window
      oTWnd  := _WindowObj( oBrw:cParentWnd )    // parent window
      nY0    := oTWnd:Row
      nX0    := oTWnd:Col
      nW0    := nW := oTWnd:Width
      nH0    := nH := oTWnd:Height
      lWin0  := .T.
   ELSE
      // позиция окна по ТСБ / window position according to TSB
      oTWnd  := _WindowObj( oBrw:cParentWnd )   // родительское окно
      nY     := GetWindowRow(oBrw:hWnd)
      nX     := GetWindowCol(oBrw:hWnd)
      nW     := GetWindowWidth(oBrw:hWnd)
      nH     := GetWindowHeight(oBrw:hWnd)
      lWin0  := .F.
   ENDIF

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("ComSanMS"))
   aLang  := { "Save", "Cancel", "(send)" }
#else
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("FntBtn_1"))   // Фонт-1 кнопок других форм
   // {"Comic Sans MS"   , nFSize  , .F., .F., .F., .F., 0,  0,  0, "SuperHd" }  // Фонт суперхидера
   aLang  := { "Сохранить", "Отмена", "(отправить)" }
#endif

   nG        := 20
   aRet      := {}   // всегда массив - пусто, значит отказ от ввода
   cTitle    := aLine[1] + SPACE(5) + CValToChar(App.Cargo:lPosWinOrTsb)
   cTitle    += SPACE(5) + ProcNL()
   // кнопки на форме
   cFont     := aFont[1]
   nFSize    := aFont[2]
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   aBtnFClr  := { BLUE , YELLOW }
   nHText    := nFSize * 2
   nHIco     := 48
   nHBtn     := nHIco + 10
   // две кнопки "Save" "Cancel"
   nWBtn     := nHIco + nG + GetTxtWidth( REPL("H",10) , nBtnFSize, cBtnFont, .T. )

   IF !ISOBJECT(oDop)
      cMsg := "Error ! Not an oDop object !"
      MsgDebug(cMsg, oDop, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF

   aName := oDop:aName                   //{"Тип заявки","Вид заявки","Массив","Дата"}
   aType := oDop:aType                   //{"S","S","A","D"}
   aSprv := oDop:aTFor                   //{ {"tipza","Ktipza","tipza",2} , {"Works","KWorks","Works",2} }
   aIcon := oDop:aIcon                   // иконки в контекстное меню
   aPict := oDop:aPict                   // { Repl("x", 22), "@D", "@Z 99:99", "@Z 999" }

   IF !IsArray(aIcon)
      aIcon := {}
   ENDIF
   IF LEN(aIcon) > 0
      IF LEN(aName) # LEN(aIcon)
         cMsg := "Error ! LEN(oDop:aName) # LEN(oDop:aIcon) !"
         MsgDebug(cMsg, LEN(oDop:aName), LEN(oDop:aIcon), ProcNL(),ProcNL(1))
         RETURN {}
      ENDIF
   ENDIF
   IF !IsArray(aSprv)
      cMsg := "Error ! oDop:aForS - not an array of references !"
      MsgDebug(cMsg, oDop:aTFor, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   IF LEN(aType) # LEN(aSprv)
      cMsg := "Error ! LEN(oDop:aType) # LEN(oDop:aTFor) !"
      MsgDebug(cMsg, LEN(oDop:aType), LEN(oDop:aTFor), ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   cErr  := ""
   aXArr := Get2Dim_Spravki(aSprv,aType,@cErr)  // -> demo1_util.prg   все значения из 1/2/3/Х Dbf
   IF LEN(cErr) > 0
      cMsg := "Error ! Can't get reference data !"
      MsgDebug(cMsg, cErr, aSprv, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF
   //
   aBClr := oDop:aBClr
   aFClr := oDop:aFClr
   IF !IsArray(aBClr)  ;  aBClr := RED
   ENDIF
   IF !IsArray(aFClr)  ;  aFClr := YELLOW
   ENDIF
   aBZebra := oBrw:Cargo:oParam:aBZebra
   aBClr   := HMG_n2RGB(aBzebra[2])

   // первоначальные массивы значений
   cVal6  := aLine[ACOL_6]      // (6)  - функция заполнения массива для правки - коды
   aVal13 := aLine[ACOL_13]     // (13) - первоначальные значения - коды
   aRet1  := ACLONE(aVal13)     // что пришло   !!! только через ACLONE()
   aRet1[4] := 1  // всегда !!! код отправки на сайт

   nArr   := LEN(aName)
   aArCmb := ARRAY(nArr)
   a2Code := ARRAY(nArr)
   a1One  := ARRAY(nArr)
   AFILL(a1One, 0 )
   ? "=======",ProcNL() , nArr, hb_valtoexp(aRet1)
   ? SPACE(20) + "aName=", aName, hb_valtoexp(aName)
   ? SPACE(20) + "aVal13=", aVal13, hb_valtoexp(aVal13)
   ? SPACE(20) + "aType=", aType, hb_valtoexp(aType)
   ? SPACE(20) + "aRet1=", aRet1, hb_valtoexp(aRet1)
   ? SPACE(20) + "aIcon=", aIcon, hb_valtoexp(aIcon)

   IF LEN(aVal13) # LEN(aName)
      cMsg := "Error ! LEN(aVal13) # LEN(aName) ! aVal13 считывает функция: ["+cVal6+"]"
      MsgDebug(cMsg, LEN(aVal13),"#",LEN(aName), aVal13, aName, ProcNL(),ProcNL(1))
      RETURN {}
   ENDIF

   FOR nI := 1 TO nArr
      cType := aType[nI]
      xVal  := aVal13[nI]
      //? nI, cType, "xVal=", xVal
      IF cType == "S"
         aArCmb[nI] := aXArr[nI,2]    // все значения из 1/2/3/Х  Dbf
         a2Code[nI] := aXArr[nI,1]    // коды значений
         aDim       := a2Code[nI]
         FOR nJ := 1 TO LEN(aDim)
            nVal := aDim[nJ]
            IF xVal == nVal
               a1One[nI] := nJ  // номер в массиве COMBOBOX
                //?? "Ok=", nJ
               // исправим вход.данные на порядк.номер массива Combo
               aRet1[nI] := nJ
               EXIT
            ENDIF
         NEXT
      ELSEIF cType == "A"
      ENDIF
   NEXT

   // расчёт ширины/высоты окна
   nText := nVal := 0
   nHBox := nG                // высота отступа сверху
   lMemo := .F.
   FOR nI := 1 TO LEN(aName)
      nText := MAX(nText,LEN(aName[nI]))
//? nI, aName[nI], nText
      cType := aType[nI]
      IF cType == "S"
         aDim := aXArr[nI,2]    // все значения из двух Dbf
         FOR nJ := 1 TO LEN(aDim)
            nVal := MAX(nVal,LEN(aDim[nJ]))
         NEXT
      ELSEIF cType == "D"
         nVal := LEN("dd'.'0MMMM'000'yyyy" + "0000")
      ELSEIF cType == "C"
         nVal := 40
      ELSEIF cType == "M"
         nVal := 40
         lMemo := .T.
      ELSEIF cType == "N"
         nVal := 14
      ELSEIF cType == "A"
         aDim := aXArr[nI,2]    // все значения из массива - {"отключить", "включить"}
         FOR nJ := 1 TO LEN(aDim)
            nVal := MAX(nVal,LEN(aDim[nJ]))
         NEXT
      ELSE
         nVal := LEN("REZERV")
      ENDIF
      nHBox += nHText + nG
   NEXT
   IF lMemo
      nHBox += nHText*2  // для мемо-поля
   ENDIF
   nHBox  += nG + nHBtn + nG*2
   nHBox  += GetTitleHeight() + GetBorderHeight()
   cText  := REPL("H",nText) + "HH"
   nVal   := IIF(nVal < 22, 22, nVal )
   cVal   := REPL("H",nVal)  + "HH"
   nWText := GetTxtWidth( cText, nFSize, cFont, .T. )
   nWGBox := GetTxtWidth( cVal, nFSize, cFont, .T. )
   nWLine := nG + nWText + nG/2
   nWLine += nWGBox + nG*2
   nWDate := GetTxtWidth( "dd0'.'0MMMM'0'yyyy0000" , nFSize, cFont, .T. )

   nX := (nW - nWLine)/2 + oTWnd:Col
   nW := nWLine                        // ширина окна
   IF (nWBtn*2 + nG*3) > nWLine
      nW := (nWBtn*2 + nG*3)
   ENDIF
   nH := nHBox
   ? ProcNL(), "******** ["+cForm+"] высота окна =", nH
   nY := (oTWnd:Height - oTWnd:Row - nH) / 2
   ?? "nY=",nY
   // testing
   IF ( nY + nH ) > Sys.ClientHeight
      nI := ( nY + nH ) - Sys.ClientHeight
      nY -= nI
   ENDIF
   nX := iif( nX < 0, 0, nX )

   aColor  := GRAY
   aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aBtnClr := { {225,225,225}, GRAY }

   IF lWin0
      // позиция окна по родит.окну / window position by parent window
      nX := ( nW0 - nW ) / 2 + nX0
      nY := ( nH0 - nH ) / 2 + nY0
   ENDIF

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle ;
          MODAL NOSIZE FONT cFont  SIZE nFSize BACKCOLOR aBClr   ;
          ON INIT ( This.Label_0.Setfocus, DoEvents() )
          This.Cargo := oHmgData() ; owc := This.Cargo

      nY        := nX := nG
      nW        := This.ClientWidth
      nH        := This.ClientHeight
      owc:nFor  := 0                  // нажата кнопка/getbox/combo
      owc:aRet1 := aRet1              // массив значений для правки

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      FOR nI := 1 TO LEN(aName)
         cType := aType[nI]
         cText := aName[nI] + ":"
         cObj1 := "Lbl_" + STRZERO(nI,2)

         @ nY, nX LABEL &cObj1 VALUE cText WIDTH nWText HEIGHT nHText ;
                  BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT

         nCol := nX + This.&(cObj1).Width + nG/2
         cObj := "GBox_" + STRZERO(nI,2)

         IF cType     == "C"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] BOLD ;
                       WIDTH nWGBox HEIGHT nHText        ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }   // правим массив в контейнере формы

            This.&(cObj).Cargo := nI

         ELSEIF cType  == "N"
            @ nY, nCol GETBOX &cObj VALUE aRet1[nI] BOLD ;
                       WIDTH nWGBox HEIGHT nHText        ;
                       PICTURE aPict[nI]                 ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

            This.&(cObj).Alignment := "LEFT"  // "CENTER" or "RIGHT"
            This.&(cObj).Cargo := nI

         ELSEIF cType == "D"
            dDate := aRet1[nI]
            @ nY, nCol DATEPICKER &cObj VALUE dDate WIDTH nWGBox HEIGHT nHText ;
                       DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE BOLD             ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

            This.&(cObj).Cargo := nI

         ELSEIF cType == "A"
            aDim := aXArr[nI,2]    // все значения из массива - {"отключить", "включить"}
            cImg := aIcon[nI]
            IF aRet1[nI] == 0               ; cCap := "- - -"
            ELSEIF aRet1[nI] <= LEN(aDim)   ; cCap := aDim[aRet1[nI]]
            ELSE                            ; cCap := "??? code: " + HB_NtoS(aRet1[nI])
            ENDIF
            //cCap += SPACE(70) + "..."
            cObj := "Btn_" + STRZERO(nI,2)
            @ nY, nCol BUTTONEX &cObj WIDTH nWGBox HEIGHT nHText CAPTION cCap         ;
              PICTURE "ArrowDown20" NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT           ;
              FONTCOLOR BLACK BACKCOLOR aBtnClr[1] BOLD                               ;
              ON MOUSEHOVER ( This.Fontcolor := WHITE, This.Backcolor := aBtnClr[2] ) ;
              ON MOUSELEAVE ( This.Fontcolor := BLACK, This.Backcolor := aBtnClr[1] ) ;
              ACTION  {| | This.Enabled := .F., _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) } ;
              ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                           o:nBtn  := nI             ,;  // номер нажатой кнопки
                           o:nRet  := 0              ,;  // возврат значения
                           o:nPost := 10             ,;  // номер события
                           o:cBtn  := This.Name      ,;
                           o:cCapt := This.Caption   ,;
                           o:aBClr := This.BackColor ,;
                           o:nY    := This.Row       ,;
                           o:nX    := This.Col       ,;
                           o:aFClr := aBtnFClr       ,;
                           o:cTxt  := cCap           ,;
                           o:aTxt  := aDim           ,;
                           o:cIco  := cImg        }

              Btn_MaxTxtWidth(cForm, cObj)  // максимальный размер надписи с пробелами
              //? _o2log(This.&(cObj).Cargo, 27, ProcNL() + "  This.&("+cObj+").Cargo => ", .T. ) // проверка
              // проверить простым способом
              //ACTION {|| This.Enabled := .F., ; //!!!
              //           _LogFile(.T.,"$$$$$", This.Name, This.Cargo, ThisWindow.Name), ;
              //           _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) } ;

         ELSEIF cType == "S"
            aDim  := aArCmb[nI]
            cObj  := "CmBox_" + STRZERO(nI,2)
            @ nY, nCol COMBOBOXEX &cObj BOLD WIDTH nWGBox HEIGHT 300  ;
                       ITEMS aDim  VALUE a1One[nI]                    ;
                       ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value ,;   // правим массив в контейнере формы
                                     This.GBox_02.Value := DATE() ,;
                                     This.GBox_03.Value := VAL( SUBSTR(TIME(), 1, 2)+SUBSTR(TIME(), 4, 2) ) ,;
                                     This.Label_0.Setfocus   }
            This.&(cObj).Cargo := nI

         ELSEIF cType == "M"
            cObj  := "EditBox_" + STRZERO(nI,2)
            @ nY, nCol EDITBOX &cObj WIDTH nWGBox HEIGHT nHText * 4  ;
              VALUE aRet1[nI] BACKCOLOR {240, 240, 240} NOTABSTOP    ;
              ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }

            This.&(cObj).Cargo := nI

         ELSE
            cMsg := "Error ! cType == ??? [" + cType + "] !"
            MsgDebug(cMsg, cType, nI, cText)

         ENDIF

         nY += This.&(cObj).ClientHeight + nG
         ? nI, cType, cObj , This.&(cObj).Picture
      NEXT

      // 1  S  CmBox_01
      // 2  D  GBox_02
      // 3  N  GBox_03
      // 4  N  GBox_04
      This.GBox_04.Width     := 40
      //This.GBox_04.Picture   := "999"
      This.GBox_04.Alignment := "LEFT"  // "CENTER" or "RIGHT"
      This.GBox_04.READONLY  := .T.
      nX2 := This.GBox_03.Value
      //This.GBox_03.Picture   := "@Z 99:99"
      //This.GBox_03.Value     := nX2
      //This.GBox_03.Setfocus
      DO EVENTS

      cText  := aLang[3]
      nWText := GetTxtWidth( cText, nFSize, cFont, .T. ) + 10
      nY2    := This.GBox_04.Row
      nX2    := This.GBox_04.Col + This.GBox_04.Width + nG
      @ nY2, nX2 LABEL Help VALUE cText WIDTH nWText HEIGHT nHText ;
        FONTCOLOR aFClr RIGHTALIGN TRANSPARENT

      /////////////////////// кнопки на форме ////////////////////////////////
      nY    := nH - nG - nHBtn
      nX    := ( nW - nWBtn * 2 - nG ) / 2
      aIco1 := { "iMg_Ok48x1"    , "iMg_Ok48x2"     }
      aIco2 := { "iMg_Cancel48x1", "iMg_Cancel48x2" }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[1] ;
               ICON aIco1[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco1[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco1[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(90 ,, This.Name) }

      nX += This.Btn_Ok.Width + nG

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION aLang[2] ;
               ICON aIco2[1] NOXPSTYLE HANDCURSOR NOTABSTOP                ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD     ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                      ;
               ON MOUSEHOVER ( This.Icon := aIco2[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco2[1],         ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(98 ,, This.Name) }

      ON KEY ESCAPE ACTION _wPost(98)

      IF !lMemo
         ON KEY RETURN ACTION _wPost(90)
      ENDIF

      (This.Object):Event( 10, {|ow,ky,cn|  // выбор по кнопкам
                                 Local aTxt, cImg, aRet, cCapt, ocBtn, nI, nRet
                                 Local owc := ow:Cargo           // cargo окна
                                 ocBtn := This.&(cn).Cargo       // cargo нажатой кнопки
                                 _SetThisFormInfo(ow)            // save This среду окна ow
                                 aTxt  := ocBtn:aTxt
                                 cImg  := ocBtn:cIco
                                 nRet  := ocBtn:nRet             // первоначальное значение
                                 nI    := ocBtn:nBtn             // номер нажатой кнопки
                                 owc:nFor := nI
                                 cCapt := ocBtn:cCapt
                                 //MsgDebug(ow:Name, ky, cn, ocBtn, "nI=",nI,owc:aRet1)
                                 aRet := Btn_ContexMenu(ow, ocBtn:nY, ocBtn:nX, aTxt, cImg, .T.)
                                 IF LEN(aRet) > 0
                                     owc:aRet1[nI] := aRet[1]     // ВОТ ТАК НУЖНО !!! -->> правим массив в контейнере формы
                                     ocBtn:cCapt   := aRet[2]
                                     nRet          := aRet[1]     // возврат значения
                                     cCapt         := aRet[2]
                                     ocBtn:nRet    := nRet        // НЕ возвращает значение выше
                                 ENDIF
                                 _SetThisFormInfo()               // restore This среду окна ow
                                 This.&(cn).Enabled    := .T.
                                 This.&(cn).Caption    := cCapt   // возврат на кнопку
                                 This.&(cn).Cargo:nRet := nRet    // НЕ возвращает значение выше
                                 Btn_MaxTxtWidth(ow:Name, cn)     // максимальный размер надписи с пробелами
                                 ow:Setfocus('Label_0')
                                 DO EVENTS
                                 ky := cn
                                 Return Nil
                                 } )

      (This.Object):Event( 90, {|ow,ky,cn|  // сохранить данные
                                 Local aRet2, owc := ow:Cargo
                                 aRet2 := owc:aRet1           // массив значений для правки
                                 ? "### Save", ProcNL(), HB_ValToExp(aRet2)
                                 cVal := myCode2Str(@aRet2, aXArr, aType, aPict)
                                 ? "### myCode2Str()", HB_ValToExp(aRet2), cVal
                                 aRet := {aRet2,cVal}
                                 _wPost(99,ow)
                                 ky := cn
                                 Return Nil
                                 } )

      (This.Object):Event( 98, {|ow| aRet := {}, _wPost(99,ow) } )

      (This.Object):Event( 99, {|ow| ow:Release() } )

      //SetWindowLong( This.Handle, GWL_STYLE, WS_BORDER)
   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_RClickContexMenu(oBrw, aCol, nAt)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, aRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, nMenu, aLang, cImg
   LOCAL lIcon, aFont, nFSize, cName, nWCell, nHCell, oCell, aDim, aMenu
   LOCAL cLine1, cLine2, cTitle, cField, cFunc, cRType, cMsg, xRet, l4Menu

#ifdef KEY_ENG // for this project demo1-en.hbp
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("ComSanMS")
   aLang  := { "Show line text", "Show line contents" ,;
               "Copy text of a line to the buffer", "Copy the contents of a string to the buffer",;
               "Exit" }
#else
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("FntBtn_1")   // Фонт-1 кнопок других форм
   aLang  := { "Показать текст строки ", "Показать содержимое строки " ,;
               "Копировать текст строки в буфер", "Копировать содержимое строки в буфер",;
               "Выход" }
#endif

   //MsgDebug("[nAt]=",nAt,":aArray[nAt]=",ob:aArray[nAt])
   cForm  := oBrw:cParentWnd
   oWnd   := _WindowObj(oBrw:cParentWnd)
   // координаты ячейки в которой клик правой кнопкой
   oCell  := oBrw:GetCellSize(oBrw:nRowPos, oBrw:nCell, ) //oBrw:lDrawSuperHd)
   nY     := oCell:nRow
   nX     := oCell:nCol
   nWCell := oCell:nWidth
   nHCell := oCell:nHeight
   //
   cLine1 := aCol[1] + " | " + myVal2Str( aCol[2] )
   cLine2 := myTsbArrayLine(oBrw , .T., .F.)
   cField := aCol[5]
   cRType := aCol[ACOL_4]           // тип обработки ячеек
   aFont  := GetFontParam(hFont1)
   nFSize := aFont[2]
   nMenu  := -2
   cFunc  := ""

   IF "LINE" $ cRType ; RETURN {}
   ENDIF

   lIcon := .T.   // иконки в меню, иначе BMP
   aDim  := {}                            // .T.-нет выбора  .F.-есть выбор
   AADD( aDim, { "iView1_32" , aLang[1]     , .F. , "MsgDebug",  1 } )
   AADD( aDim, { "iView2_32" , aLang[2]     , .F. , "MsgDebug",  2 } )
   AADD( aDim, {                                                   } )
   AADD( aDim, { "iCopy1_32" , aLang[3]     , .F. , "MsgDebug",  3 } )
   AADD( aDim, { "iCopy2_32" , aLang[4]     , .F. , "MsgDebug",  4 } )
   l4Menu := .T.
   // добавим меню на определенное поле
   IF IsString(cField) .AND. cField == "MAKTVIP"
      aDim  := {}                            // .T.-нет выбора  .F.-есть выбор
      cFunc := aCol[15]
      AADD( aDim, { "iMaster48" , "Run func: " + cFunc , .F. , "Its function",  3 } )
      l4Menu := .F.
   ENDIF

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( 32 )                      // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         aMenu   := aDim[nI]
         IF Empty(aMenu) .or. aMenu[1] == NIL .or. Empty(aMenu[2]) .or. "SEPARATOR" $ aMenu[2]
            SEPARATOR
            LOOP
         ENDIF
         cMenu   := aMenu[2]
         cName   := StrZero(nI, 10)
         cImg    := aMenu[1]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         lDis    := aMenu[3] //.F.- DISABLED
         hFont   := IIF( lDis, hFont2, hFont1 )
         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      SEPARATOR
      MENUITEM  aLang[5]  ACTION  {|| nMenu := 0 } FONT hFont2 ICON "iExit32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      aRet := { nMenu, aDim[nMenu,2] }
      IF l4Menu
         IF nMenu == 1
            cTitle := aDim[nMenu,2]
            AlertInfo(cLine1, cTitle, 64, , {ORANGE})
         ELSEIF nMenu == 2
            cTitle := aDim[nMenu-1,2]
            AlertInfo(cLine2, cTitle, 64, , {ORANGE})
         ELSEIF nMenu == 4
           //#translate System.Clipboard := <arg> => CopyToClipboard ( <arg> )
            CopyToClipboard(cLine1)
         ELSEIF nMenu == 5
            CopyToClipboard(cLine2)
         ENDIF
      ELSE
         IF nMenu == 1
            // используются только мои функции, стандартные тоже можно после переделки
            IF AT("(",cFunc) > 0
               cFunc := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
            ENDIF
            IF !hb_IsFunction( cFunc )
               cMsg := "No such function "+cFunc+"() in EXE file!;"
               cMsg += 'Contact the program developer;;'
               cMsg += ProcNL() + ";" + ProcNL(1)
               AlertStop(cMsg, "Launching a function", , 64, {RED})
            ELSE
               cFunc += "(" + hb_valtoexp(oWnd,aCol) + ")"
               xRet  := Run_Line64(oWnd, oBrw, aCol, nAt)
               //xRet  := myMacro(cFunc, .F.)   // .T. режим отладки
               IF IsString(xRet) .AND. "ERROR! MACRO=" $ UPPER(xRet)
                  ? "___ERROR!___", ProcNL() , "xRet=",xRet
                  MsgDebug("___ERROR!___","xRet=",xRet)
               ENDIF
            ENDIF
         ENDIF
      ENDIF  //  l4Menu
   ELSE
      aRet := {}
   ENDIF

   DO EVENTS

RETURN aRet
