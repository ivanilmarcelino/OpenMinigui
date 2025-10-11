/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Показ массива в _TBrowse() / Show array in _TBrowse()
 * Расчёт ширины колонок по массиву / Calculating column widths from an array
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
/////////////////////////////////////////////////////////////////////////
FUNCTION Table_Rprt(cParentWnd, a4Dim, aHead, oBrw)
   LOCAL oTsb, owc, cNam, nY, nX, nH, nW, nG, nTbl, cTtl, aBClr, cFocus
   LOCAL oRpt, aTbClr, cSpHd, a3Clr, cForm, cIcon, aXDim, aTbl, cWinTtl

   ? ProcNL(), cParentWnd, a4Dim
   // a4Dim := {aRpt, aTable, cWinTtl, HB_NtoS(nPar) + ProcName() }
   aXDim   := a4Dim[1]
   aTbl    := a4Dim[2]
   cWinTtl := a4Dim[3]
   cForm   := "w" + a4Dim[4]

   IF _IsWindowDefined( cForm )
      IF IsIconic( nH := GetFormHandle(cForm) ) ; _Restore( nH )
      ENDIF
      DoMethod(cForm, "SetFocus")
      RETURN NIL
   ENDIF

   a3Clr  := { ;
               { { 90,217,217}, {192,217,217}, {146,244,244} } ,;
               { {125,125,253}, {199,199,249}, {155,155,244} } ,;
               { {197,17 ,98 }, {207,86,141 }, {244,244,244} } ,;
               { {184,107,228}, {244,202,242}, {238,130,238} } ,;
               { {181,172,98} , {230,222,152}, {209,199,133} }   }

   cIcon  := "iQuest64"
   nTbl   := aTbl[1]
   cTtl   := aTbl[2]
   cNam   := "Run_S_"
   cFocus := "Buff"
   nY     := 0  //(nTbl-1) * 50
   nX     := 0  //(nTbl-1) * 50
   nW     := Sys.ClientWidth  - nX
   nH     := Sys.ClientHeight - nY
   nG     := 20
   //MsgDebug(nTbl,aTbl)
   aBClr  := a3Clr[nTbl,1]
   //         window     line-1         line-2       aBrush
   aTbClr := { aBClr , a3Clr[nTbl,2], a3Clr[nTbl,3], aBClr }
   cSpHd  := cTtl   // титул суперхидера

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH     ;
          TITLE cWinTtl ICON cIcon                       ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE  ;
          BACKCOLOR aBClr                                ;
          ON INIT    ( _wSend( 0) )                      ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:cFocus  := cFocus

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

#ifdef KEY_ENG
      ButtonBar({ "Excel-1", "Excel-2"  , "Calc-1", "Exit" }, cNam, , , , nG*2, nG)
      owc:cLang := "TOTAL:"
#else
      ButtonBar({ "Эксель-1", "Эксель-2", "Калк-1", "Выход"}, cNam, , , , nG*2, nG)
      owc:cLang := "ИТОГО:"
#endif

      nY := nG + nG * 3
      nX := nG
      nW := This.ClientWidth  - nG*2
      nH := This.ClientHeight - nY - nG

      ////////////////////////////////////////////////////////////////////////
      oTsb := TablePatam( cForm, aXDim, "cReport", aTbClr, nW, cSpHd, aHead)
      oTsb:cLang := owc:cLang
      //? _o2log(oTsb, 27, ProcNL() + "  oTsb => ", .T. ) // check in log
      // function in library \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oRpt := _TBrowse( oTsb, aXDim, "cReport", nY, nX, nW, nH )
      This.Cargo:oRpt  := oRpt

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oRpt, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | This.Topmost := .F., ow:Cargo:oRpt:SetFocus() })
         :Event( 1, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      //MsgDebug(ow:Name,ky,cn,This.&(cn).Caption)
                      myExportXls(oRpt,oBrw,ky,cn)
                      SET WINDOW THIS TO
                      This.&(cn).Enabled := .T.
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      //MsgDebug(ow:Name,ky,cn,This.&(cn).Caption)
                      myExportXls(oRpt,oBrw,ky,cn)
                      SET WINDOW THIS TO
                      This.&(cn).Enabled := .T.
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 3, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      //MsgDebug(ow:Name,ky,cn,This.&(cn).Caption)
                      myExportCalc(oRpt,oBrw,ky,cn)
                      SET WINDOW THIS TO
                      This.&(cn).Enabled := .T.
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event(90, {|ow,ky|
                      ? ProcNL(), ">>> ON RELEASE: "+ow:Name+":Event("+hb_ntos(ky)+")"
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   //ACTIVATE WINDOW &cForm NOWAIT
   ACTIVATE WINDOW &cForm

RETURN cForm

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatam(cForm,aXDim,cBrw,aIniClr,nWTsb,cSpHTtl,aHead)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, nI, aName, nHCell
   LOCAL nLine, aWSize, nWNum, aHd
   // максимальная высота строк в колонках
   nLine := 0
   FOR EACH a IN aHead
      IF ";" $ a
         aHd := HB_ATokens(a, ";")
         nLine := MAX( nLine, LEN(aHd) )
      ENDIF
   NEXT
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   oTsb:lNoPicture     := .T.
   //oTsb:aFont        := { "DlgFont", "DlgFont", "DlgFont", "DlgFont" , "DlgFont", "DlgFont" }
   //                         cell     Head     Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold3", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := INT( GetFontHeight(oTsb:aFont[1])*1.5 )
   //oTsb:aNumber      := NIL                                    // не ставить колонку нумерации
   nWNum               := GetFontWidth(oTsb:aFont[4], 4)
   oTsb:aNumber        := { 1, nWNum }                         // колонка нумерации и её ширина
   oTsb:lFooting       := .T.                                  // поставить в таблице подвал
   oTsb:lSuperHd       := .T.                                  // поставить в таблице суперхидер
   oTsb:cSuperHd       := cSpHTtl
   //oTsb:uSelector    := 20
   oTsb:lFooting       := .T.                                  // поставить в таблице подвал
   oTsb:aFoot          := .T.                                  // заполнить подвал
   nHFnt               := INT( GetFontHeight(oTsb:aFont[1]) )  // только целые числа
   nHCell              := INT( nHFnt * 1.4 )                   // только целые числа
   oTsb:nHeightCell    := nHCell                               // высота ячеек
   oTsb:nHeightHead    := nHFnt * nLine                        // высота шапки
   oTsb:nHeightFoot    := nHFnt                                // высота подвала
   oTsb:nHeightSpecHd  := nHFnt                                // высота нумератора
   oTsb:nHeightSuperHd := nHFnt * 2                            // высота суперхидера

   nClr1 := HMG_RGB2n(aIniClr[1])                              // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)                                    // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }      // цвет: текст и фон суперхидера
   oTsb:aBrush         := aIniClr[4]                           // цвет фона под таблицей

   // цвета в таблицу
   a := {}
   // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a
   oTsb:lZebra    := .T.
   //oTsb:aZebra  := { {230,230,230}, SILVER }    // серый
   oTsb:aZebra    := { HMG_RGB2n(aIniClr[2]), HMG_RGB2n(aIniClr[3]) }

   oTsb:aHead := aHead
   // table header and column titles
   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead  := {}
      aName  := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, "" )
         AADD( aName, "COL_" + HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF
   a      := aXDim[1]
   aName  := {}
   FOR nI := 1 TO LEN(a)
      AADD( aName, "COL_" + HB_ValToExp(nI) )
   NEXT
   oTsb:aName    := aName
   oTsb:aHideCol := {}                                               // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidths(aXDim,0,nWTsb,nWNum,aHead)  // подсчёт ширины колонок - показ всех колонок
   oTsb:aSize    := aWSize                                           // назначим ширину колонок для ТСБ

   // Проверка колонок таблицы / Checking table columns
   //  ? "oTsb:aField=", oTsb:aField ; ?v oTsb:aField
   //  ? "oTsb:aName= ", oTsb:aName  ; ?v oTsb:aName
   //  ? "oTsb:aPict=" , oTsb:aPict  ; ?v oTsb:aPict
   //  ? "oTsb:aHead= ", oTsb:aHead  ; ?v oTsb:aHead
   //  ? "oTsb:aFoot= ", oTsb:aFoot  ; ? IIF( IsArray(oTsb:aFoot), HB_ValToExp(oTsb:aFoot), oTsb:aFoot )
   //  ? "oTsb:aSize= ", oTsb:aSize  ; ?v oTsb:aSize
   //  ? "oTsb:aAlign=", oTsb:aAlign //; ?v oTsb:aAlign

   // такой порядок работы блоков кода / this is the order of the code blocks
   oTsb:bInit := {|ob,op| // TSB initialization
                   Local ox := op // oTsb
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)            // скрыть колонки
                   //? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO") + 1    // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   //myTsbEdit(ob,op)                             // Editing table cells
                   ox:Rezerv := "Rezerv"
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // other TSB settings
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll    := NIL
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   //? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                   //  "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }


   oTsb:bAfter := {|ob,op| // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
                    Local aSum, aNum, aLine, nCol, oCol, nPos, xVal, k, cLang
                    ? "### oTsb:bAfter", ProcNL()
                    ? "op:aHead=",LEN(op:aHead), hb_valtoexp(op:aHead)
                    ? "op:aName=",LEN(op:aName), hb_valtoexp(op:aName)
                    ? "ob:aColumns=",LEN(ob:aColumns)
                    ? "ob:aArray[1]=",LEN(ob:aArray[1])
                    ? "ob:aArray=",LEN(ob:aArray) ; ?v ob:aArray
                    cLang := op:cLang 
                    ? "op:cLang=", cLang
                    nPos := 0          // надо учитывать доп. колонки
                    IF ob:nColumn("SELECTOR", .T.) > 0 ; nPos += 1
                    ENDIF
                    IF ob:nColumn("ARRAYNO" , .T.) > 0 ; nPos += 1
                    ENDIF
                    k    := Len(ob:aArray[1])  
                    aSum := array(k) ; AFill(aSum, 0)
                    aNum := array(k) ; AFill(aNum, 0)
                    FOR EACH aLine IN ob:aArray
                        FOR EACH xVal IN aLine
                            nCol := hb_enumindex(xVal)  // номер элемента массива
                            IF !IsNumeric( xVal ) ; LOOP
                            ENDIF
                            aSum[ nCol ] += xVal       // итог
                            aNum[ nCol ] += 1          // счетчик
                        NEXT
                    NEXT
                    oCol := ob:aColumns[2]
                    oCol:cFooting := cLang 
                    oCol:nFAlign  := DT_CENTER
                    FOR EACH nCol, xVal IN aNum, aSum
                        IF nCol > 0        // поле числовое и есть сумма
                           nCol := hb_enumindex(nCol) + nPos // реал. колонка
                           oCol := ob:aColumns[nCol]
                           IF Empty(xVal) ; oCol:cFooting := ""
                           ELSE           ; oCol:cFooting := hb_ntos(xVal)
                           ENDIF
                           oCol:nFAlign  := DT_CENTER
                        ENDIF
                    NEXT
                    ob:DrawFooters()
                    DO EVENTS
                    Return Nil
                    }
RETURN oTsb

/////////////////////////////////////////////////////////////////////
// расчёт ширины колонок / calculating column widths
STATIC FUNCTION CalculatColumnWidths(aXDim,nCol,nWTsb,nWNum,aHead)
   LOCAL aDim, v, a, i, hFont, nW, aWSize, aWHead, nLen, aStr

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle("DlgFont")
   aWSize := Array(Len(aDim[1]))
   aWHead := Array(Len(aDim[1]))
   aFill(aWSize, 0)
   aFill(aWHead, 0)

   FOR EACH a IN aDim
      FOR EACH v IN a
         i := hb_enumindex(v)
         // показ только 2 колонок
         //IF i > nCol ; LOOP
         //ENDIF
         IF !IsChar(v) ; v := cValToChar(v)
         ENDIF
         v  += "HH"  // добавка
         nW := GetTextWidth( Nil, v, hFont )
         aWSize[ i ] := MAX(nW,aWSize[ i ])
         //IF i > nCol
         //  aWSize[ i ] := 0
         //ENDIF
      NEXT
   NEXT

   nW := 0
   FOR i := 1 TO nCol - 1
      nW += aWSize[ i ]
   NEXT

   //oTsb:aNumber := { 1, 30 }     nWNum        // колонка нумерации и её ширина
   nCol := LEN(aWSize)
   // для колонки nCol делаем всю ширину экрана показа, за минусом колонки 1
   aWSize[nCol] := nWTsb - nW - GetHScrollBarHeight() - nWNum - 1

   // пересчёт ширины шапки таблицы
   FOR EACH a IN aHead
      IF ";" $ a
         aStr := HB_ATokens(a, ";")
         nLen := 0
         FOR i := 1 TO LEN(aStr)
             nLen := MAX( nLen, LEN(aStr[i]) )
         NEXT
         v := REPL("H",nLen)  // добавка
         nW := GetTextWidth( Nil, v, hFont )
      ELSE
         nW := GetTextWidth( Nil, a, hFont )
      ENDIF
      i := hb_enumindex(a)
      aWHead[i] := nW
   NEXT
   // приведём к большей ширине колонок
   FOR i := 1 TO LEN(aWSize)
      IF aWHead[i] > aWSize[i]
         aWSize[i] := aWHead[i]
      ENDIF
   NEXT

RETURN aWSize

/////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ButtonBar(aBtn, cPref, nY, nX, nW, nH, nG, l99)
   LOCAL cTxt, nBtn, cBtn, aNam := {}, cTool, cCapt
   LOCAL aFnt, cFnt, nFSz
   Default cPref := "Btn_", l99 := .T.

   Default nG := 15, aBtn := {}, nBtn := 0
   Default nY := nG, nX := nG , ;
           nW := App.Object:W1, ;
           nH := App.Object:H2

   aFnt := GetFontParam(GetFontHandle("ComSanMS"))
   cFnt := aFnt[1]
   nFSz := aFnt[2]

   FOR EACH cTxt IN aBtn
       nBtn  := hb_enumindex(cTxt)
       IF IsArray(cTxt) ; cTool := cTxt[2] ; cCapt := cTxt[1]
       ELSE             ; cTool := NIL     ; cCapt := cTxt
       ENDIF
       cBtn := cPref + hb_ntos( nBtn )
       @ nY, nX BUTTONEX &cBtn WIDTH nW HEIGHT nH CAPTION cCapt ;
                TOOLTIP  cTool                                  ;
                FONT cFnt SIZE nFSz BOLD                        ;
                NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
                ACTION ( This.Enabled := .F., _wPost(This.Cargo,, This.Name) )
       This.&(cBtn).Cargo := iif( l99 .and. nBtn == Len(aBtn), 99, nBtn )
       nX += This.&(cBtn).Width + nG
       This.Cargo:cBtn_Exit := cBtn
       AAdd(aNam, cBtn)
   NEXT

RETURN aNam

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myExportXls(oRpt, oBrw, ky)      // Export to Ole-Excel b/w (xls)
   LOCAL cPathExp, cFileMsk, cHeadline, cAls

   cPathExp  := oBrw:Cargo:cPathExport           // путь записи файлов отчётов
   cFileMsk  := "Event_log"                      // маска файла
   //cHeadline := oBrw:Cargo:c2Title             // журнал-событий-программы
   cHeadline := oBrw:Cargo:cNameTitle
   cAls      := oRpt:cAlias

   LogUserExcel(oRpt,ky,cPathExp,cFileMsk,cHeadline) // -> user2report_excel.prg

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myExportCalc(oRpt, oBrw)         // Export to Ole-Calc b/w (xls)
   LOCAL cPathExp, cFileMsk, cHeadline, cAls

   cPathExp  := oBrw:Cargo:cPathExport           // путь записи файлов отчётов
   cFileMsk  := "Event_log"                      // маска файла
   //cHeadline := oBrw:Cargo:c2Title             // журнал-событий-программы
   cHeadline := oBrw:Cargo:cNameTitle
   cAls      := oRpt:cAlias

   LogUserCalc(oRpt,1,cPathExp,cFileMsk,cHeadline)  // -> user2report_excel.prg

RETURN NIL
