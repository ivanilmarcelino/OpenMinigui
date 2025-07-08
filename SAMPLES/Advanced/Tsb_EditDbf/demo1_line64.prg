/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Разное редактирование ячеек таблицы (для массивов) из DBf-файла
 * _TBrowse() Miscellaneous editing of table cells (for arrays) from DBf-file
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
/////////////////////////////////////////////////////////////////////////
// aDim, {"  ** List of apartments", SPACE(20) , 64, "CALC"  , "MAKTVIP"  , "Get_Fld01()"  , "Set_Fld01()"
// , "Win_Fld01()"  , "W", "", "", "", nil, aFld, "Run_Fld01()" } )
// aFld := {"NAKTVIP","MAKTVIP"}  // {"C","M"} поля записи в базу / database entry fields
FUNCTION Get_Fld64(aVal)     // aPara := { cRType, cField, x13Col, x14Col, x15Col, nI, aLine }
   LOCAL cRet, aRet, aFld, cSay, cMemo, cKvar, cHlp, cFld2, cFld1, cAls := ALIAS()
   LOCAL cRType, cField, x13Col, x14Col, x15Col, nI, aLine
   cRType := aVal[1] ; cField := aVal[2] ; x13Col := aVal[3]
   x14Col := aVal[4] ; x15Col := aVal[5] ; nI  := aVal[6]
   aLine  := aVal[7]
   //1  CALC
   //2  MAKTVIP
   //3  T/O,additional,additional installation of UKP-12,1 pcs., price
   //4  {"NAKTVIP", "MAKTVIP"}
   //5  Run_Fld01()
   //6  64
   //7  {"  ** List of apartments", " ", 64, "CALC", "MAKTVIP", "Get_Fld01()", "Set_Fld01()", "Win_Fld01()", "W", "", "", "", NIL, {"MAKTVIP", "NAKTVIP"}, "Run_Fld01()"}
   //
   aFld  := x14Col
   cFld1 := aFld[1]
   cFld2 := aFld[2]
#ifdef KEY_ENG // for this project demo1-en.hbp
   cSay := " apartment selected:"
   cHlp := "(right mouse button - apartment creation menu)"
#else
   cSay := " выбрана квартира:"
   cHlp := "(правая кнопка мышки - меню создания квартир)"
#endif

   cKvar := ALLTRIM((cAls)->&cFld1)
   cMemo := (cAls)->&cFld2
   cRet  := cSay + " " + cKvar + Space(5) + cHlp
   aRet  := {cKvar, cMemo}   // пишется в ACOL_13, потом для показа в ТСБ в ACOL_10 -> GetSet_array_dbf.prg
                             // written in ACOL_13, then for display in TSB in ACOL_10 -> GetSet_array_dbf.prg
RETURN { cRet, aRet }

////////////////////////////////////////////////////////////////////////////
// запись массива полей колонки (13) в базу
FUNCTION Set_Fld64(aVal)    // aPara := { cRType, cField, x13Col, x14Col, x15Col, nI, aLine }
   LOCAL nI, nFld, cFld, cMsg, cErr, cAls, a4Val, a5Fld, cFld1, cFld2
   //? "==============================" + ProcNL()
   //?v aVal
   // [1]   выбрана квартира:     (правая кнопка мышки - меню создания квартир)
   // [2]  CALC
   // [3]  MAKTVIP
   // [4]  {"", "106, 105, 104/1, 103, 102, 101, 100/2, 99, 98, 97, 96/3, 95, 94, 93, 92/4, 91, 90, 89, 88/5, 87, 86, 85, 84/6, 83, 82, 81, 80/7, 26, 25, 24/1, 23, 22, 21/2, 20, 19, 18/3, 17, 16, 15/4, 14, 13, 12/5, 11, 10, 9/6, 8, 7, 6/7, 5, 4, 3/8, 2, 1 "}
   // [5]  {"NAKTVIP", "MAKTVIP"}
   // [6]  Run_Line64()
   // [7]  64
   // [8]  {"  ** Список квартир (шахматка)", " выбрана квартира:     (правая кнопка мышки - меню создания квартир)", 64, "CALC", "MAKTVIP", "Get_Fld64()", "Set_Fld64()", "Win_Fld64()", "W", '{"", "106, 105, 104/1, 103, 102, 101, 100/2, 99, 98, 97, 96/3, 95, 94, 93, 92/4, 91, 90, 89, 88/5, 87, 86, 85, 84/6, 83, 82, 81, 80/7, 26, 25, 24/1, 23, 22, 21/2, 20, 19, 18/3, 17, 16, 15/4, 14, 13, 12/5, 11, 10, 9/6, 8, 7, 6/7, 5, 4, 3/8, 2, 1 "}', '{"NAKTVIP", "MAKTVIP"}', "Run_Line64()", {"", "106, 105, 104/1, 103, 102, 101, 100/2, 99, 98, 97, 96/3, 95, 94, 93, 92/4, 91, 90, 89, 88/5, 87, 86, 85, 84/6, 83, 82, 81, 80/7, 26, 25, 24/1, 23, 22, 21/2, 20, 19, 18/3, 17, 16, 15/4, 14, 13, 12/5, 11, 10, 9/6, 8, 7, 6/7, 5, 4, 3/8, 2, 1 "}, {"NAKTVIP", "MAKTVIP"}, "Run_Line64()"}
   //
   a4Val := aVal[4]    // значение для записи в базу
   a5Fld := aVal[5]    // поля базы
   //? "c5Fld=", c5Fld, VALTYPE(c5Fld)
   //? "c4Val=", c4Val, VALTYPE(c4Val)
   cMsg  := "=.=.=.=.=.= Error! Line: " + aVal[1] + ";"
   cAls  := ALIAS()
   cErr  := ""

   FOR nI := 1 TO LEN(a5Fld)
      cFld  := a5Fld[nI]
      nFld  := FIELDNUM( cFld )
      IF nFld == 0
         cErr += "No such field ["+cFld+"] in DB-"+cAls+";"
      ENDIF
   NEXT

   IF (cAls)->( RLock() )
      cFld1 := a5Fld[1]
      cFld2 := a5Fld[2]
      (cAls)->&cFld1 := a4Val[1]
      (cAls)->&cFld2 := a4Val[2]
      (cAls)->( DbUnlock() )
      (cAls)->( DbCommit() )
   ELSE
      cErr += "WRITE ERROR ! "
      cErr += HB_NtoS(RECNO()) + " blocked !;"
   ENDIF

   IF LEN(cErr) > 0
      cMsg += cErr + ";"
      cMsg += ProcNL() + ";" + ProcNL(1)
      cMsg += ";" + ProcNL(2) + ";" + ProcNL(3)
      AlertStop(cMsg, "", , 64, {RED})
      cMsg += ";" + REPL("=.",40) + ";"
      ? "==============================" + ProcNL()
      ? ATREPL( ";", cMsg, CRLF )
      ? "==============================" ; ? "."
   ENDIF

RETURN "RECORDS-ARE-ALREADY-CLOSED!"  //"ЗАПИСЬ УЖЕ ЗАВЕРШЕНА!"

//////////////////////////////////////////////////////////////////////////////////
FUNCTION Run_Line64(oWnd, oBrw, aCol, nAt)
   LOCAL nY, nX, nW, nH, nG, aRet, cFont, nFSize, aFont, aFont2, cText, owc
   LOCAL aIco1, aIco2, aBtnFClr, cBtnFont, nBtnFSize, aGrOver, aGrFill
   LOCAL cForm, aLang, aColor, aBClr, aFClr, cTitle, nHIco, nHText, cHelp
   LOCAL aBtnClr, nWBtn, nHBtn, aRet1, aRet2, aName, nI, nWGBox, nWText
   LOCAL cObj1, cObj2, lChkDel

   ? ProcNL(), oWnd:Name,oBrw:cAlias, aCol
   //MsgDebug(oWnd:Name,oBrw:cAlias, aCol)

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("ComSanMS"))
   aLang  := { "Execute", "Cancel", "Launching your functions", "Delete entire list" ,;
               "Create a list of apartments by conditions" ,;
               "Apartment numbering: 106-80% are apartments with a fraction" }
#else
   aFont  := GetFontParam(GetFontHandle("Normal"))     // Фонт колонок таблицы
   aFont2 := GetFontParam(GetFontHandle("FntBtn_1"))   // Фонт-1 кнопок других форм
   aLang  := { "Выполнить", "Отмена", "Запуск своих функций", "Удалить весь список" ,;
               "Создать список квартир по условиям" ,;
               "Нумерация квартир: 106-80  %-это квартиры с дробью"   }
#endif

   // позиция окна ТСБ
   nY     := GetWindowRow(oBrw:hWnd)
   nX     := GetWindowCol(oBrw:hWnd)
   nW     := GetWindowWidth(oBrw:hWnd)
   nH     := GetWindowHeight(oBrw:hWnd)
   nG     := 20
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   cForm  := "Tsb_Win_myRun"
   cTitle := aLang[3]

   // кнопки на форме
   cFont     := aFont[1]
   nFSize    := aFont[2] + 4
   nHText    := nFSize * 2
   nHIco     := 48
   nWBtn     := 220
   nHBtn     := nHIco + 10
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   aBtnFClr  := { BLUE , YELLOW }
   aColor    := GRAY
   aGrOver   := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill   := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aBtnClr   := { {225,225,225}, GRAY }
   cText     := ""
   aBClr     := oWnd:Cargo:aBClr
   aFClr     := BLUE
   aName     := { "For :=", "To :=" , "%" }
   cHelp     := aLang[6]
   aRet1     := {   106, 80, 4 }
   aRet2     := {    26, 1 , 3 }
   lChkDel   := .F.

   // новое окно в координаты таблицы / new window in table coordinates
   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle ;
          MODAL   FONT cFont  SIZE nFSize BACKCOLOR aBClr        ;
          ON INIT ( This.Label_0.Setfocus, DoEvents() )
          This.Cargo := oHmgData() ; owc := This.Cargo

      nY        := nX := nG
      nW        := This.ClientWidth
      nH        := This.ClientHeight
      owc:nFor  := 0                  // нажата кнопка/getbox/combo
      owc:aRet1 := aRet1              // массив значений для правки
      owc:aRet2 := aRet2              // массив значений для правки

      @ 0, 0 LABEL Buff VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      cText  := aLang[4]
      nWText := GetTxtWidth( cText, nFSize, cFont, .T. ) + 50
      @ nY, nX CHECKLABEL Chk_1 WIDTH nWText HEIGHT nHText            ;
               VALUE cText LEFTCHECK IMAGE { 'CheckT28', 'CheckF28' } ;
               FONTCOLOR RED BACKCOLOR aBClr                          ;
               ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )            ;
               ON INIT {|| This.Checked := .F.          }             ;
               ACTION  {|| This.Checked := ! This.Checked, lChkDel := This.Checked }

      nY += nHText + nG

      cText  := aLang[5]
      @ nY, nX LABEL Label_0 VALUE cText WIDTH nW-nG*2 HEIGHT nHText ;
        FONTCOLOR RED CENTERALIGN TRANSPARENT

      nY += nHText + nG
      nX := nG*4

      FOR nI := 1 TO LEN(aName)

         cText := aName[nI]
         cObj1 := "Lbl_" + STRZERO(nI,2)

         nWText := GetTxtWidth( cText, nFSize, cFont, .T. ) + 10
         @ nY, nX LABEL &cObj1 VALUE cText WIDTH nWText HEIGHT nHText ;
                  BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT VCENTERALIGN

         nX    += This.&(cObj1).ClientWidth + nG
         cObj2 := "GBox_" + STRZERO(nI,2)

         cText  := "99999"
         nWGBox := GetTxtWidth( cText, nFSize, cFont, .T. )
         @ nY, nX GETBOX &cObj2 VALUE aRet1[nI] BOLD ;
                    WIDTH nWGBox HEIGHT nHText        ;
                    PICTURE "999"                     ;
                    ON CHANGE {|| ThisWindow.Cargo:aRet1[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

         This.&(cObj2).Alignment := "LEFT"  // "CENTER" or "RIGHT"
         This.&(cObj2).Cargo := nI

         nX += This.&(cObj2).ClientWidth + nG

      NEXT

      nWText := GetTxtWidth( cHelp, nFSize, cFont, .T. ) + 10
      @ nY, nX LABEL Lbl_Hlp1 VALUE cHelp WIDTH nWText HEIGHT nHText ;
        SIZE nFSize - 4  FONTCOLOR aFClr TRANSPARENT VCENTERALIGN

      nY += nHText + nG
      nX := nG*4

      FOR nI := 1 TO LEN(aName)

         cText := aName[nI]
         cObj1 := "Lbl_" + STRZERO(nI,2) + "02"

         nWText := GetTxtWidth( cText, nFSize, cFont, .T. ) + 10
         @ nY, nX LABEL &cObj1 VALUE cText WIDTH nWText HEIGHT nHText ;
                  BOLD FONTCOLOR aFClr RIGHTALIGN TRANSPARENT VCENTERALIGN

         nX    += This.&(cObj1).ClientWidth + nG
         cObj2 := "GBox_" + STRZERO(nI,2) + "02"

         cText  := "99999"
         nWGBox := GetTxtWidth( cText, nFSize, cFont, .T. )
         @ nY, nX GETBOX &cObj2 VALUE aRet2[nI] BOLD ;
                    WIDTH nWGBox HEIGHT nHText        ;
                    PICTURE "999"                     ;
                    ON CHANGE {|| ThisWindow.Cargo:aRet2[ This.Cargo ] := This.Value }  // правим массив в контейнере формы

         This.&(cObj2).Alignment := "LEFT"  // "CENTER" or "RIGHT"
         This.&(cObj2).Cargo := nI

         nX += This.&(cObj2).ClientWidth + nG

      NEXT

      nWText := GetTxtWidth( cHelp, nFSize, cFont, .T. ) + 10
      @ nY, nX LABEL Lbl_Hlp2 VALUE cHelp WIDTH nWText HEIGHT nHText ;
        SIZE nFSize - 4 FONTCOLOR aFClr TRANSPARENT VCENTERALIGN

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

      (This.Object):Event( 90, {|ow| aRet := myDim64(lChkDel,aRet1,aRet2),;
                                     WriteArray64(oBrw,nAt,aRet) ,;
                                     _wPost(99,ow) } )

      (This.Object):Event( 98, {|ow| aRet := {}, _wPost(99,ow) } )

      (This.Object):Event( 99, {|ow| ow:Release() } )

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

//////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION WriteArray64(oBrw,nAt,aRet)
   LOCAL aCol13, aVal1, aVal2, aNew

   ? ProcNL()
   aCol13 := oBrw:aArray[nAt][ACOL_13]
   aVal1  := aCol13[1]
   aVal2  := aRet[1]           // меняем на то что в myDim64()
   aNew   := {aVal1,aVal2}
   oBrw:aArray[nAt][ACOL_13] := aNew                 // пишется в ACOL_13
   oBrw:aArray[nAt][ACOL_10] := HB_ValToExp(aNew)    // преобразование в "C" колонки (13), для показа в ТСБ
   ? "###### [ACOL_13] = ", HB_ValToExp(aNew)
   oBrw:Cargo:nModify ++          // счётчик-изменения в таблице - данные были изменены

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myDim64(lChkDel,aRet1,aRet2)
   LOCAL nI, n1, n2, n3, cDr, cKv, nJ, aRet, cStr := ""

   IF lChkDel
      aRet := {SPACE(10)}
      RETURN aRet
   ENDIF

   n1 := aRet1[1]
   n2 := aRet1[2]
   n3 := aRet1[3]
   nJ := 1
   FOR nI := n1 TO n2 STEP -1
     cKv := HB_NtoS(nI)
     cDr := ""
     IF nI%n3 == 0
        cDr := "/" + HB_NtoS(nJ++)
     ENDIF
     cStr += cKv + cDr + ", "
   NEXT

   n1 := aRet2[1]
   n2 := aRet2[2]
   n3 := aRet2[3]
   nJ := 1
   FOR nI := n1 TO n2 STEP -1
     cKv := HB_NtoS(nI)
     cDr := ""
     IF nI%n3 == 0
        cDr := "/" + HB_NtoS(nJ++)
     ENDIF
     cStr += cKv + cDr + IIF( n2==nI, " ", ", ")
   NEXT
   aRet := {cStr}

RETURN aRet

