/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Окно шахматка / Checkerboard window  // 08.05.25
 */
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

////////////////////////////////////////////////////////////////////////////////////
FUNCTION TestChess(cForm,cObj2)
   LOCAL aDim, aLine, cTtl2

   aDim  :=  {"116/1", " 115", " 114", " 113", " 112/2", " 111", " 110", " 109", " 108/3", " 107", " 106",;
              " 105", " 104/4", " 103", " 102", " 101", " 100/5", " 99", " 98", " 97", " 96/6", " 95", " 94",;
              " 93", " 92/7", " 91", " 90", " 89", " 88/8", " 87", " 86", " 85", " 84/9", " 83", " 82", " 81",;
              " 80/10", " 26", " 25", " 24/1", " 23", " 22", " 21/2", " 20", " 19", " 18/3", " 17", " 16", " 15/4",;
              " 14", " 13", " 12/5", " 11", " 10", " 9/6", " 8", " 7", " 6/7", " 5", " 4", " 3/8", " 2", " 1 "}
   //aDim :=  {"116/1", " 115", " 114", " 110"}
   //AMERGE( aDim, aDim )
   aLine := Nil
   cTtl2 := "Checkerboard test on the button"
   // cObj2 - для привязки координат меню к объекту
   Tsb_Checkerboard(,aDim,aLine,cForm,cTtl2,cObj2)

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_Checkerboard(oBrw,aDim,aLine,cParentWnd,cTtl2,cObj2)
   LOCAL oWnd, cForm, o, owc, aRet, cTitle, aBClr, cMsg, nWTitl, nW1Cel, nH1Cel
   LOCAL nW, nH, nY, nX, nG, oCell, nWCell, nHCell, cMaska, hFont, nX2, nCols
   LOCAL oTsb, nWTsb, nHTsb, aXDim, aIsx, nYWnd, nXWnd, nWWnd, nHWnd, nLen, lTsbPos
   LOCAL lMod := _HMG_IsModalActive
   LOCAL hMod := _HMG_ActiveModalHandle
   DEFAULT aLine := {}, cParentWnd := "None!" , cTtl2 := "Chess Window Title"
   DEFAULT cObj2 := "None!"

   IF IsObject(oBrw)
      oWnd := _WindowObj(oBrw:cParentWnd)
      // координаты ячейки которую редактируем
      oCell   := oBrw:GetCellSize(oBrw:nRowPos, oBrw:nCell, ) //oBrw:lDrawSuperHd)
      nY      := oCell:nRow
      nX      := oCell:nCol
      nWCell  := oCell:nWidth
      nHCell  := oCell:nHeight
      lTsbPos := .T.
   ELSE
      lTsbPos := .F.
      IF !_IsWindowDefined(cParentWnd)
         MsgDebug("Error! There is no such WINDOW:",cParentWnd)
      ENDIF
      oWnd := _WindowObj(cParentWnd)
      // координаты объекта от которого берем координаты
      IF _IsControlDefined(cObj2, cParentWnd)
         nY := This.&(cObj2).Row + GetTitleHeight()
         nX := This.&(cObj2).Col
      ELSE
         MsgDebug("Error! There is no such object:",cObj2)
         nY := nX := 0
      ENDIF
   ENDIF
   nYWnd  := oWnd:Row
   nXWnd  := oWnd:Col
   nWWnd  := oWnd:Width
   nHWnd  := oWnd:Height
   IF !lTsbPos ; nY += nYWnd
   ENDIF
   //
   aRet   := {}
   aBClr  := YELLOW
   cForm  := "Form_Cell_Checkerboard"
   IF LEN(aLine) > 0
      cTitle := ALLTRIM(aLine[1])
   ELSE
      cTitle := cTtl2
   ENDIF
   //
   cMaska := "H999/99H"
   hFont  := GetFontHandle( "Bold" )
   nW1Cel := GetTextWidth( Nil, cMaska, hFont )             // ширина ячейки таблицы
   nH1Cel := GetTextHeight( Nil, "H", hFont ) * 2           // высота ячейки таблицы
   nWTitl := GetTxtWidth( cTitle, 10, "Arial", .T. )        // ширина строки титула
   nWTitl += 32 * 2 // иконки
   nW     := nWTitl                                         // минимальная ширина окна
   //
   aIsx   := aDim
   IF IsString(aDim)
      aDim := HB_ATokens( aDim, ",", .T., .T. )
   ENDIF
   aDim  := mySortKvar(aDim)
   //MsgDebug(aIsx, aDim)

   aXDim := mySquarXDim(aDim, 3)   // 3-мин.кол-во колонок / min.number of columns
   IF LEN(aDim) == 0
      nH   := nH1Cel * 2
      cMsg := "Array aDim is empty!"
   ELSE
      nCols := LEN(aXDim[1])
      nLen  := LEN(aXDim)
      nH    := nH1Cel * nLen  + GetBorderHeight() - 4
      nW    := nW1Cel * nCols //+ GetBorderWidth() - 6      // ширина окна
      aBClr := SILVER
   ENDIF
   //nX2 := nWCell + nX - nW   // прижать к концу таблицы
   nX2 := nX

   _HMG_IsModalActive     := .F.
   _HMG_ActiveModalHandle := 0

   // поправки для размера окна и координаты куда окно
   //nYWnd, nXWnd, nWWnd, nHWnd - родительское окно
   IF nY + nH > (nHWnd + nYWnd)
      nY := nHWnd + nYWnd - nH - GetTitleHeight() - GetBorderHeight()
   ENDIF

   IF nW + nX2 > ( nWWnd + nXWnd )
      nX2 := (nWWnd + nXWnd ) - nW - GetBorderHeight()
   ENDIF

   // новое окно в координаты таблицы
   DEFINE WINDOW &cForm AT nY, nX2 CLIENTAREA nW, nH TITLE cTitle ;
          MODAL NOSIZE BACKCOLOR aBClr                            ;
          ON INIT {|| _wPost(0) }                                 ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr := This.Backcolor
      owc:lZero := IIF(LEN(aDim) == 0, .T. , .F. )
      owc:aRet  := {}                         // ВАЖНО/IMPORTANT !!!

      nY := nX := nG := 0
      nW := This.ClientWidth
      nH := This.ClientHeight

      IF owc:lZero
         @ 0, 0 LABEL Buff VALUE cMsg WIDTH nW HEIGHT nH FONTCOLOR RED TRANSPARENT
      ELSE
         @ 0, 0 LABEL Buff VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT
      ENDIF

      nWTsb := nW  ; nHTsb := nH
      /////////////////////// таблица /////////////////////////////////////////////
      oTsb := TablePatamChess( cForm, aXDim, "cTableChess", nWTsb, nW1Cel, nH1Cel)
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDim, "cTableChess", nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы
      oBrw:Cargo:nModify  := 0                            // счётчик изменений
      This.Cargo:oBrw     := oBrw                         // объект положим на окно

      o := This.Object
      o:Event( 0, {|ow,ky| // ON INIT
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> ON INIT WINDOW <=", ow:Name
                           This.Topmost := .F.
                           IF ow:Cargo:lZero
                              ow:SetFocus('Buff')
                           ELSE
                              ow:Cargo:oBrw:Setfocus()
                           ENDIF
                           Return Nil
                           } )

      // назначим клавиши в таблице, см. ниже
      // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4
      //             _wPost(  32  ,  33  , 34

      o:Event(90, {|ow,ky| // ON Release
                           aRet := ow:Cargo:aRet              // ВАЖНО/IMPORTANT !!!
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           ?? "APARTMENT -> aRet=", hb_valtoexp(aRet)
                           Return Nil
                           })

      o:Event(99, {|ow| ow:Release()    })

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) )

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_IsModalActive     := lMod
   _HMG_ActiveModalHandle := hMod

RETURN aRet  // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamChess(cForm,aXDim,cBrw,nWTsb,nW1Cel,nH1Cel)
   LOCAL oTsb, nClr1, nClr2, a, aWSize, aBClr, nColumns, aHead, aName, nI
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm                    // <--- обязательно так !!!
   oTsb:cFormName      := cForm                    // или так
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Bold" , "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   //oTsb:aNumber      := { 1, 30 }                // колонка нумерации и её ширина
   oTsb:nHeightCell    := nH1Cel                   // высота ячеек расчитана выше
   oTsb:nHeightHead    := 1                        // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := 1                        // высота подвала
   oTsb:lFooting       := .F.                      // НЕ ставить в таблице подвал
   oTsb:lSpecHd        := .F.                      // НЕ ставить в таблице нумератор колонок
   oTsb:lSuperHd       := .F.                      // НЕ ставить в таблице суперхидер
   oTsb:cSuperHd       := ""                       // текст суперхидера
   oTsb:nHeightSuperHd := 0                        // высота суперхидера
   oTsb:nCellMarginLR  := 0                        // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   //oTsb:uSelector    := 20                       // селестор слева таблицы
   oTsb:lNoPicture     := .T.

   nColumns      := LEN(aXDim[1])
   aName         := {}
   FOR nI := 1 TO nColumns
      AADD( aName, "Col_"+HB_NtoS(nI) )
   NEXT
   oTsb:aName    := aName

   aHead := ARRAY(nColumns)
   AFILL( aHead, "")
   oTsb:aHead    := aHead
   aWSize := ARRAY(nColumns)
   AFILL( aWSize, nW1Cel - 3 )
   oTsb:aSize    := aWSize                                // назначим ширины колонок для ТСБ
   oTsb:aHideCol := {}                                    // скрыть колонки, учитываем SELECTOR и ARRAYNO

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
      oTsb:aFoot       := .T.                            // заполнить подвал
   ENDIF

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // высота нумератора
   ENDIF

   aBClr               := This.Backcolor
   nClr1               := HMG_RGB2n(aBClr)                  // цвет фона шапка+подвал
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := aBClr                             // цвет фона под таблицей
   // цвета в таблицу
   oTsb:lZebra    := .T.                                    // это вкл.\откл. механизм zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // серый цвет
   oTsb:aZebra    := { aBClr, {206,211,242} }
   a := {}
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_WHITE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_RED                  })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , RGB(240,240,240)         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a

   oTsb:lChess := .T.                         // вкл.\откл. механизм расскраска в шахматы
   oTsb:aChess := { CLR_HGRAY, CLR_WHITE }    // цвета по умолчанию

   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) } // блок кода после END TBROWSE НЕ использовать
                                               // без необходимости, работает DEFAULT значение
                                               // !!! все делать в oTsb:bAfter !!!
   // Проверка
   //  ?v aXDim
   //  ?v oTsb:aName
   //  ?v oTsb:aHead
   //  ?v oTsb:aSize
   //
   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   Local oc, lChess
                   lChess := !Empty( op:lChess )
                   Default op:lChess := lChess
                   IF lChess
                      Default op:aChess := { CLR_HGRAY, CLR_WHITE }
                   ENDIF
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)            // скрыть колонки
                   ob:lDrawHeaders := .F.                         // убрать шапку таблицы
                   ob:lNoHScroll   := .T.                         // нет показа горизонтального скролинга
                   ob:oHScroll     := NIL
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aName), "op:lChess=",op:lChess
                   // --- вариант 1 ---
                   //  меняем цвета текста в ячейках таблицы, cтавим по всем колонкам и убрать пробелы
                   AEval(ob:aColumns, {|oCol|
                         oCol:nClrBack := {|nr,nc,obr| myColorTsb(nr,nc,obr) }
                         oCol:bDecode  := {|cv| Alltrim(cv) }
                         oCol:nAlign   := DT_CENTER
                         Return Nil
                         })
                   //
                   // --- вариант 2 ---
                   // расскраска в шашечки и убрать пробелы
                   FOR EACH oc IN ob:aColumns
                      oc:bDecode   := {|cv| Alltrim(cv) }
                      oc:nAlign    := DT_CENTER
                      IF !lChess ; LOOP
                      ENDIF
                      oc:nClrBack  := {|nr,nc,obr|
                         Local nClr, aClr := { CLR_HGRAY, CLR_WHITE }
                         IF obr:Cargo:oParam:lChess ; aClr := obr:Cargo:oParam:aChess
                         ENDIF
                         IF nr % 2 == 0
                            nClr := iif( nc % 2 == 0, aClr[1], aClr[2] )
                         ELSE
                            nClr := iif( nc % 2 == 0, aClr[2], aClr[1] )
                         ENDIF
                         Return nClr
                         }
                   NEXT
                   //myTsbEditTvr(ob,op)   // здесь не нужно   // редактирование ячеек таблицы
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd, nBClr, hFont
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nCol, nw := 0
                    Local aColSz := {}
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       nCol := hb_enumindex(oc)
                       IF nCol % 2 == 0
                          AADD( aColSz, oc:cName )
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                    NEXT
                    //ob:AdjColumns(aColSz) // растянем width - это в качестве примера
                    ? repl(" ", Len(ProcNL()))+".", "=== TSB === nWidth =", nw
                    ? repl(" ", Len(ProcNL()))+".", "=== nWTsb =", nWTsb
                    ? hb_valtoexp(aColSz)
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } } ;
                     }

   // выбор и выход / choice and exit
   AAdd(oTsb:aUserKeys, {VK_RETURN, {|ob|
                                     Local owc := _WindowCargo(ob:cParentWnd)
                                     owc:aRet := { ob:aArray[ob:nAt][ob:nCell] }   // ВАЖНО/IMPORTANT !!!
                                     _wPost(99, ob:cParentWnd)
                                     Return Nil
                                     } })

   //////////////////////////////////////////////////////////////////
   // можно и так / you can do it this way too
   //   b := {|ob|
   //         Local owc  := _WindowCargo(ob:cParentWnd)
   //         owc:aRet := ob:aArray[ob:nAt][ob:nCell]
   //         _wPost(99, ob:cParentWnd)
   //         Return Nil
   //         }
   //   oTsb:aUserKeys := { ;
   //        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
   //        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
   //        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
   //        {VK_RETURN, b                                   } }  ;
   //                     }
   //  или можно так / or you can do it like this
   //   oTsb:aUserKeys := { ;
   //          {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
   //          {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
   //          {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }  ;
   //                       }
   //  AAdd(oTsb:aUserKeys, {VK_RETURN, b})
   /////////////////////////////////////////////////////////////////

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }, ;
        {50, {|ow,ky,ob| _wPost(98,ow,ob)     , ky:=ow:=ob                 } }  ;
                     }

   // Двойной клик мышки на курсоре в таблице
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // Вот так !!!
   // Правый клик мышки на курсоре в таблице
   //oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }
   // Левый клик мышки на курсоре в таблице
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsb( nAt, nCol, oBrw)
   LOCAL nColor, nColCount := oBrw:nColCount()

   // расскраска в шашечки
   IF nAt % 2 == 0
      If nCol % 2 == 0; nColor := CLR_HGRAY
      Else            ; nColor := CLR_WHITE
      EndIf
   ELSE
      If nCol % 2 == 0; nColor := CLR_WHITE
      Else            ; nColor := CLR_HGRAY
      EndIf
   ENDIF
   /*
   // расскраска в линеечку
   IF nAt % 2 == 0
      nColor := CLR_HGRAY
   ELSE
      nColor := CLR_WHITE
   ENDIF
   */
RETURN nColor

///////////////////////////////////////////////////////////////
FUNCTION mySquarXDim(aDim, nMinCol)
   LOCAL nSquare, nI, nCol, nMaxCol, aVal, aRet, cDim
   // Округление до меньшего значения квадратного корня
   nSquare := Floor(SQRT(LEN(aDim)))
   // Основание должно быть не меньше nMinCol
   nSquare := MAX(nSquare, nMinCol)
   aVal    := ARRAY(nSquare)
   AFILL(aVal,"")
   aRet    := {}
   nCol    := 1
   nMaxCol := nSquare
   FOR nI := 1 TO LEN(aDim)
      IF nI % nMaxCol == 0
         aVal[nCol] := aDim[nI]
         AADD( aRet, aVal )
         aVal := ARRAY(nSquare)
         AFILL(aVal,"")
         nCol := 1
         nI++
      ENDIF
      cDim := IIF( nI > LEN(aDim), "-", aDim[nI])
      aVal[nCol] := cDim
      nCol ++
      IF nI == LEN(aDim) .AND. nCol <= nMaxCol
         AADD( aRet, aVal )
      ENDIF
   NEXT
   // дополним массив
   IF LEN(aRet) < nMaxCol
      FOR nI := LEN(aRet) TO nMaxCol - 1
         aVal := ARRAY(nSquare)
         AFILL(aVal,"")
         AADD(aRet, aVal)
      NEXT
   ENDIF

RETURN aRet

///////////////////////////////////////////////////////////////
FUNCTION mySortKvar(aDim)
   LOCAL nI, aRet, cVal, aVal, cNumba, cBukva, a3Dim, xVal

   a3Dim := {}
   FOR nI := 1 TO LEN(aDim)
      xVal   := aDim[nI]
      cVal   := IIF( IsString(xVal), xVal, cValToChar(xVal) )
      cVal   := ALLTRIM(aDim[nI])
      aVal   := MyF8Kvar(cVal)
      cNumba := aVal[1]
      cBukva := UPPER(aVal[2])           // буква квартиры или дома
      AADD( a3Dim, { cVal, cBukva, PADL(cNumba,6,"0")+"|"+cBukva } )
   NEXT
   a3Dim := ASORT( a3Dim,,, { |x, y| x[3] < y[3] } )
   aRet := ARRAY( LEN(a3Dim) )
   FOR nI := 1 TO LEN(a3Dim)
      aRet[nI] := a3Dim[nI, 1]
   NEXT

RETURN aRet

//////////////////////////////////////////////////////////////////////
// Сортировку квартир с буквами
FUNCTION MyF8Kvar(cVal)
  LOCAL nI, nK, aRet := {0,""}, cRet, cStr, c1 := ""
  LOCAL cLat := "abcomktxdnyep"
  LOCAL cRus := "авсомктхднуер"

  nK := 0
  cVal := ALLTRIM(cVal)
  FOR nI := 1 TO LEN(cVal)
     cStr := SUBSTR(cVal,nI,1)
     IF ASC(cStr) > 47 .AND. ASC(cStr) < 58
        c1 += cStr
     ELSE
        nK := nI
        EXIT
     ENDIF
  NEXT

  IF nK > 0
    cRet := SUBSTR(cVal,nK)
    cRet := CharRepl(cLat, cRet, cRus)
    cRet := CharRepl( UPPER(cLat), cRet, UPPER(cRus) )
  ELSE
    cRet := ""
  ENDIF

  aRet[1] := VAL(c1)
  aRet[2] := ALLTRIM(UPPER( cRet ))

RETURN aRet

