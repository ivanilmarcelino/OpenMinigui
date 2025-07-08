/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018-2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2018-2020 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
*/
#define _HMG_OUTLOG

#define PBM_SETPOS       1026
#define LINE_WRITE       100   // Количество строк для записи блоками

#define WIN_VT_VARIANT   12

#define Number_Characters_String_Cell  100  // Количество символов строки для ячейки

#include "minigui.ch"
#include "tsbrowse.ch"
#include "excel.ch"
* =======================================================================================
// Внимание ! Выгружать больше 65533 строк в Excel НЕЛЬЗЯ ! Ограничение Excel 2003.
// Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.
FUNCTION Brw4XlsOle( oBrw, cFile, lActivate, hProgress, aTitle, hFont, lSave, bExtern, aColSel, aImage, aTitle2)
   LOCAL lTsbSuperHd, lTsbHeading, lTsbFooting, cVal, nCol, nLine, nTotal, nCount, nEvery
   LOCAL nColHead, nColDegTbl, flag_new_OutXls := .f.
   LOCAL aSet[ Min(LINE_WRITE,oBrw:nLen), NumbColumnsForTbl( oBrw,aColSel) ], nIndexaSet
   LOCAL oExcel, oBook, oSheet, oRange, cRange, nColDbf, nBeginTable
   LOCAL hWnd, uData, aTipeChars[NumbColumnsForTbl( oBrw,aColSel)]
   LOCAL aFont, aFontSHF, aClr, oCol, cMsg, cTitle, nStart, nRow, aCol
   LOCAL nRecNo := ( oBrw:cAlias )->( RecNo() ), nAt := oBrw:nAt
   LOCAL nOldRow := oBrw:nLogicPos(), nOldCol := oBrw:nCell
   LOCAL rType, nPoint, rPicture

   Default cFile := "", lActivate := .T., hProgress := nil, aColSel := nil
   Default aTitle := {"",0}, hFont := 10, lSave := .F. , bExtern := nil
   Default aImage := {}, aTitle2 := {}

   ////////////// структура отчёта ///////////////
   // nLine := 1  // титул таблицы
   // nLine := 2  // пустая строка
   // nLine := 3  // суперхидер таблицы, если есть
   // nLine := 4  // шапка таблицы, если есть
   // nLine := 5  // ячейки таблицы, первая ячейка (если есть суперхидер и шапка таблицы)
   // nLine := nLine + oBrw:nLen // подвал таблицы, если есть
   // если aTitle2 > 0, то вставка подзаголовка таблицы перед nLine := 3
   // если aImage > 0, то вставка картинки в левый угол листа nLine := 1

   CursorWait()
   WaitThreadCreateIcon( 'Loading the report in', 'EXCEL OLE ...' )   // запуск без времени

   // Используем Ole из HBWIN.lib
   IF ( oExcel := win_oleCreateObject( "Excel.Application" ) ) == NIL
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "MS Excel не доступен !;;   Ошибка"
         cVal := "Ошибка!"
      ELSE
         cMsg += "MS Excel is not available !;;   Error"
         cVal := "Error!"
      ENDIF
      WaitThreadCloseIcon()  // kill the window waiting
      CursorArrow()

      cMsg += " [ " + win_oleErrorText() + " ];;"
      cMsg += REPLICATE( "-._.", 16 ) + ";;"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgStop( cMsg , cVal )
      RETURN Nil
   ENDIF

   oExcel:Visible := .F.          // .T. показать Excel на экране для отладки
   oExcel:DisplayAlerts := .F.    // убрать предупреждения Excel

   oExcel:WorkBooks:Add()
   oBook  := oExcel:ActiveWorkBook
   oSheet := oExcel:ActiveSheet

   // назначение фонта для суперхидера + шапки + подвал таблицы
   aFontSHF := GetFontParam( hFont )  // GetFontParam( aTitle[2] ) - как заколовок

   // проверка суперхидер таблицы
   lTsbSuperHd := oBrw:lDrawSuperHd
   IF lTsbSuperHd
      lTsbSuperHd := ( AScan( oBrw:aSuperHead, {|a| !Empty(a[3]) } ) > 0 )
      // если суперхидер таблицы задан пустым, то нет вывода суперхидера таблицы
      // пустой суперхидер задаётся в demo2.prg строка 232 - :aSuperhead[ 1, 3 ] := ''
   ENDIF

   // проверка шапки таблицы
   lTsbHeading := oBrw:lDrawHeaders
   If lTsbHeading
      lTsbHeading := ( AScan( oBrw:aColumns, { |o| !Empty( o:cHeading ) } ) > 0 )
      // если шапка таблицы задана пустые колонки, то нет вывода шапки таблицы
      // пустая шапка задаётся в demo2.prg строка 266 - oCol:cHeading := '' или NIL
   Endif

   // проверка подвала таблицы
   lTsbFooting := oBrw:lDrawFooters
   If lTsbFooting
      lTsbFooting := ( AScan( oBrw:aColumns, { |o| !Empty( o:cFooting ) } ) > 0 )
      // если подвал таблицы задан пустые колонки, то нет вывода подвала таблицы
      // пустой подвал задаётся в demo2.prg строка 269 - oCol:cFooting := '' или NIL
   Endif

   // бегунок таблицы, если есть
   If hProgress != Nil
      nTotal := oBrw:nLen
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   // Если aColSel == nil то не выводиться сепаратор и скрытые колонки
   if aColSel = Nil .or. Len(aColSel) = 0
     aColSel := CalcAcolselForTbl( oBrw,aColSel)
   Endif

   nColDbf := Len(aColSel)
   nLine  := 1
   cTitle := aTitle[1]
   cTitle := AllTrim( cTitle )

   // Картинка в верхний угол таблицы
   If ! Empty( aImage )
        oRange:=oSheet:Range(osheet:cells(1,1),osheet:cells(1,1))
        //  oExcel:ActiveSheet:Shapes:AddPicture(aImage[1],0, -1, oRange:Left, oRange:Top, -1, -1 ) не работает
        oExcel:ActiveSheet:Shapes:AddPicture(aImage[1],0, -1, oRange:Left, oRange:Top, PixelToPointX(aImage[2]),PixelToPointY(aImage[3]))
   Endif
   // Заголовок таблицы
   If ! Empty( cTitle )
      oSheet:Cells( nLine++, 1 ):Value := AllTrim( cTitle )
      IF nColDbf > 26
         cRange :=  Chr( 90 ) + '1' // буква Z
      ELSE
         cRange :=  Chr( 64 + nColDbf) + '1'
      ENDIF
      oRange := oSheet:Range( "A1:"+ cRange )
      oRange:HorizontalAlignment := xlHAlignCenterAcrossSelection
      oRange:Merge()
      If aTitle != Nil
        aFont := GetFontParam( aTitle[2] )
        aClr := BLACK
        oRange:Font:Name := aFont[ 1 ]
        oRange:Font:Size := aFont[ 2 ]
        oRange:Font:Bold := aFont[ 3 ]
        oRange:Font:Color := RGB(aClr[1],aClr[2],aClr[3])
      EndIf
      ++nLine
   EndIf
   // Второй подзаголовок таблицы
   If ! Empty( aTitle2 )
      For nRow := 1 TO Len(aTitle2)
         if Len(aTitle2[nRow]) >0
            cTitle := aTitle2[nRow,1]
            cTitle := AllTrim( cTitle )
            oSheet:Cells( nLine, 1 ):Value := AllTrim( cTitle )
            IF nColDbf > 26
              cRange :=  Chr( 90 ) + alltrim(str(nLine)) // буква Z
            ELSE
              cRange :=  Chr( 64 + nColDbf) + alltrim(str(nLine))
            ENDIF
            oRange := oSheet:Range( "A"+alltrim(str(nLine))+":"+ cRange )
            If aTitle2[nRow,4] != Nil
              oRange:HorizontalAlignment := TbsXlsAlign( aTitle2[nRow,4] )
            Else
              oRange:HorizontalAlignment := TbsXlsAlign( DT_CENTER )
            Endif
            oRange:Merge()
            If aTitle2[nRow,2] != Nil
              aFont := aTitle2[nRow,2]
              aClr  := aTitle2[nRow,3]
              oRange:Font:Name := aFont[ 1 ]
              oRange:Font:Size := aFont[ 2 ]
              oRange:Font:Bold := aFont[ 3 ]
              oRange:Font:Color = RGB(aClr[1,1],aClr[1,2],aClr[1,3])
              oRange:Interior:Color := RGB(aClr[2,1],aClr[2,2],aClr[2,3])
            EndIf
            ++nLine
         EndIf
       Next
      ++nLine
   EndIf
   nColDegTbl := ++nLine  // начальная строка заголовка таблицы

   // Выводим суперхидер таблицы
   If lTsbSuperHd

      FOR EACH aCol IN oBrw:aSuperHead
         uData := If( ValType( aCol[3] ) == "B", Eval( aCol[3] ), aCol[3] )
         oSheet:Cells( nLine,  MaxNumFromArr(aColSel,aCol[1])):Value := uData
         cRange :=  HeadXls( MaxNumFromArr(aColSel,aCol[1])) + Hb_NtoS( nLine )  + ":" + ;
                    HeadXls( MinNumFromArr(aColSel,aCol[2])) + Hb_NtoS( nLine )
         oSheet:Range( cRange ):HorizontalAlignment  := xlHAlignCenterAcrossSelection
         aFontSHF := GetFontParam( hFont )
         oSheet:Range( cRange ):Font:Name := aFontSHF[ 1 ]
         oSheet:Range( cRange ):Font:Size := aFontSHF[ 2 ]
         oSheet:Range( cRange ):Font:Bold := aFontSHF[ 3 ]
      NEXT
      ++nLine
   Endif

   // Выводим шапку таблицы
   If lTsbHeading
      nColHead := 0
      nCol :=0
      FOR EACH nCol IN aColSel
         oCol  := oBrw:aColumns[ nCol ]

         uData := If( ValType( oCol:cHeading ) == "B", Eval( oCol:cHeading ), ;
                               oCol:cHeading )

         If ValType( uData ) != "C"
            Loop
         EndIf

         uData := StrTran( uData, CRLF, Chr( 10 ) )
         nColHead ++
         oSheet:Cells( nLine, nColHead ):Value := uData
         // oSheet:Cells( nLine, nCol ):Borders():LineStyle := xlContinuous
         oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
         oSheet:Cells( nLine, nColHead ):Font:Size := aFontSHF[ 2 ]
         oSheet:Cells( nLine, nColHead ):Font:Bold := aFontSHF[ 3 ]
         // aWidthChars [nCol] := max(aWidthChars [nCol], LenStrokaWithCRLF(uData))
      Next
      ++ nLine
   Endif

   Eval( oBrw:bGoTop )  // переход на начало таблицы
   nCount := 0

   // Печать - СТРОК таблицы ВСЕГДА !
   nIndexaSet := 1
   nStart := nLine
   nBeginTable := nStart
   For nRow := 1 TO oBrw:nLen

      nColHead := 0
      FOR EACH nCol IN aColSel

         nColHead++

         oCol     := oBrw:aColumns[ nCol ]
         uData    := oBrw:GetValue( nCol )
         rType    := ValType( uData )
         Do case
            Case (rType=='@'.or.rType='D').and.Empty(uData)
               uData := ''
            Case rType=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case rType == 'L'
               rType :='C'
            Case rPicture != Nil .and. uData != Nil .and. rType !='N'
              uData := Transform( uData, rPicture )
         endCase
         If oCol:cPicture != Nil .and. uData != Nil .and. rType !='N'
            uData := Transform( uData, oCol:cPicture )
         EndIf

         // определяем тип поля в колонке
         If !(rType = "U") .and. Empty(aTipeChars[nColHead]) .and. !Empty(uData )
        aTipeChars[nColHead] := rType
             cRange :=  HeadXls(nColHead)

             //Тип полей колонок таблицы Excel по типу данных таблицы oBrw
             Do case
                 // при необходимости можно поставить тип для других полей
                Case rType=="D"
//для типа поля Дата для русифицированного Excel
//                  oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := "ДД.ММ.ГГГГ"
//для типа поля строка, не зависим от множества настроек
                  oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := "@"
                case aTipeChars[nColHead] =='C'
                  oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(oBrw:nLen+nBeginTable-1)):NumberFormat := '@'
                  // oSheet:Range(cRange+LTrim( Str(nBeginTable))+':'+cRange+LTrim( Str(oBrw:nLen+nBeginTable-1))):WrapText := .f.
                  oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(oBrw:nLen+nBeginTable-1)):ColumnWidth := Number_Characters_String_Cell
                case aTipeChars[nColHead] =='N'
                  rPicture := oCol:cPicture
                  If Empty(rPicture)
                    rPicture := Transform( uData, oCol:cPicture )
                  Endif
                  nPoint   := AT('.', rPicture )
                  if nPoint == 0
                     rPicture :='#0'
                  else
                     rPicture := Repl("#",nPoint-2) + '0,' + Repl("0",Len(rPicture)-nPoint)
                  endif
                  // формат типа := '## ### ###0' или '## ### ###0,00'
                  oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(oBrw:nLen+nBeginTable-1)):NumberFormat := rPicture
             Endcase
    Endif
         uData := If( ValType( uData )=="N", uData , ;
                  If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) )

         // запоминаем данные в массив
         aSet[ nIndexaSet , nColHead ] := uData
      Next

      IF (nIndexaSet == LINE_WRITE).or.(nRow == oBrw:nLen) // По заполнению масиива или конца таблицы
   flag_new_OutXls := .t. // массив заполнен - нужно пересылать в таблицу в Excel
      ENDIF

      ++nLine

      // Заполнение таблицы по LINE_WRITE строк из накопленного масссива
      IF flag_new_OutXls
         cRange :=  "A" + HB_NtoS(nStart)+":" +  HeadXls(nColDbf) + HB_NtoS(nLine-1)
         oRange:=oSheet:Range(cRange):Value := __oleVariantNew( WIN_VT_VARIANT, aSet, nIndexaSet, nColDbf ) // Microsoft Excel 8.0 Object Library
         nIndexaSet := 1        // Следующее заполнение с начала массива
         nStart := nLine        // начало нового диапазона строк
         flag_new_OutXls := .f.
      ELSE
         nIndexaSet++          // будем заполнять массив дальше
      EndIf

      If hProgress != Nil
         If nCount % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nCount,0)
         EndIf
         nCount ++
      EndIf

      oBrw:Skip(1)
   Next

   // выводим подвал таблицы
   nColHead := 0
   If lTsbFooting
        FOR EACH nCol IN aColSel

         oCol := oBrw:aColumns[ nCol ]
         uData := If( ValType( oCol:cFooting ) == "B", Eval( oCol:cFooting ), ;
                               oCol:cFooting )
         uData := cValTochar( uData )
         uData := StrTran( uData, CRLF, Chr( 10 ) )
         nColHead ++
         oSheet:Cells( nLine, nColHead):Value := uData
         // oSheet:Cells( nLine, nCol ):Borders():LineStyle := xlContinuous
         oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
         oSheet:Cells( nLine, nColHead ):Font:Size := aFontSHF[ 2 ]
         oSheet:Cells( nLine, nColHead ):Font:Bold := aFontSHF[ 3 ]
         // aWidthChars [nCol] := max(aWidthChars [nCol], LenStrokaWithCRLF(uData))
      Next
      nLine++
   Endif

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()

   // Шрифт только таблицы с данными
   //cRange :=  "A" + HB_NtoS(nBeginTable)+":" + Chr(64+nColDbf) + HB_NtoS(oBrw:nLen+nBeginTable-1)
   cRange :=  "A" + HB_NtoS(nBeginTable)+":" + HeadXls(nColDbf) + HB_NtoS(oBrw:nLen+nBeginTable-1)
   oRange:=oSheet:Range(cRange)
   // oRange:Borders():LineStyle := xlContinuous
   aFont := GetFontParam( hFont )
   oRange:Font:Name := aFont[ 1 ]
   oRange:Font:Size := aFont[ 2 ]
   oRange:Font:Bold := aFont[ 3 ]

   // создать сетку на таблицу
   cRange :=  "A" + HB_NtoS(nColDegTbl)+":" + HeadXls(nColDbf) + HB_NtoS(nLine-1)
   oRange:=oSheet:Range(cRange)
   oRange:Borders():LineStyle := xlContinuous
   //oRange:Columns:AutoFit() - нельзя ! Будут сбиваться ширина колонок.

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   // вызов внешнего блока дообработки таблицы
   If bExtern != Nil
      Eval( oBrw:bGoTop )  // переход на начало таблицы
      Eval( bExtern, oSheet, oBrw, oExcel , aColSel, aTitle2)
   EndIf

   // вернуть первоначальную позицию курсора в таблице
   oBrw:Reset()
   If oBrw:lIsDbf
      ( oBrw:cAlias )->( DbGoTo( nRecNo ) )
      oBrw:GoPos(nOldRow, nOldCol)
   EndIf
   oBrw:nAt := nAt

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   oRange:Columns:AutoFit() // автоматически поменять ширину всех столбцов и высоту всех строк
                            // в диапазоне, чтобы туда уместился текст ячеек.
                            // Можно применять только к тем диапазонам, которые состоят из
                            // набора столбцов (полностью) или набора ячеек (также полностью), иначе будет ошибка.

   If ! Empty( cFile ) .and. lSave
      oBook:SaveAs( cFile, xlWorkbookNormal )
   EndIf

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lActivate
      oSheet:Range( "A1" ):Select()
      oExcel:Visible := .T.           // показать Excel на экране
      hWnd := oExcel:hWnd             // хендл окна Excel
      ShowWindow( hWnd, 6 )           // MINIMIZE windows
      ShowWindow( hWnd, 3 )           // MAXIMIZE windows
      BringWindowToTop( hWnd )        // a window on the foreground
   Else
      oExcel:Application:Quit()       // закрыть Excel
   EndIf

   RETURN Nil

* =======================================================================================
// Количество выводимых колонок
FUNCTION NumbColumnsForTbl( oBrw,aColSel)
LOCAL lIsNotVisible :=.f., Arab, InCol, nCol , lRet
InCol := If(oBrw:lSelector,2,1)
If aColSel != Nil .and. Len( aColSel) >0
    LRet := Len(aColSel)
Else
   lIsNotVisible :=.f.
   For nCol := InCol TO Len( oBrw:aColumns )
      if !oBrw:aColumns[nCol]:lVisible
         lIsNotVisible :=.t.
         Exit
      Endif
   Next
   if lIsNotVisible
      Arab:={}
      For nCol := InCol TO Len( oBrw:aColumns )
         if oBrw:aColumns[nCol]:lVisible
            Aadd(Arab,nCol)
         Endif
      Next
      LRet := Len(Arab)
   else
      LRet := Len(oBrw:aColumns)
   endif
Endif
RETURN lRet

* =======================================================================================
// Массив выводимых колонок
STATIC FUNCTION CalcAcolselForTbl( oBrw,aColSel)
   LOCAL lIsNotVisible :=.f., Arab, InCol, nCol
   InCol := If(oBrw:lSelector,2,1)
   If aColSel != Nil .and. Len( aColSel) >0
       aRab := aColSel
   Else
       Arab:={}
       For nCol := InCol TO Len( oBrw:aColumns )
          if oBrw:aColumns[nCol]:lVisible
             Aadd(Arab,nCol)
          Endif
       Next
   Endif
   RETURN aRab
* =======================================================================================
// Взять номер из массива равным или меньше заданного
STATIC FUNCTION MinNumFromArr(aColSel,nIn)
   LOCAL nRet := Ascan(aColSel,nIn), nI
   if nRet == 0
     For nI := 1 to Len(aColSel)
        nRet := aColSel[nI]
        if nRet> nIn .and. nI>1
          nRet := aColSel[nI-1]
        endif
     Next
     nRet:=Ascan(aColSel,nRet)
   Endif
   RETURN nRet
* =======================================================================================
// Взять номер из массива равным или больше заданного
STATIC FUNCTION MaxNumFromArr(aColSel,nIn)
   LOCAL nRet := Ascan(aColSel,nIn), nI
   if nRet == 0
     For nI := 1 to Len(aColSel)
        nRet := aColSel[nI]
        if nRet > nIn
          Exit
        endif
     Next
     nRet:=Ascan(aColSel,nRet)
   Endif
   RETURN nRet

* =======================================================================================
// Отбивка строки из TSB в XLS
STATIC FUNCTION TbsXlsAlign(nAlign)
   LOCAL nRet := 0

   IF nAlign == DT_LEFT
      nRet := xlHAlignLeft
   ELSEIF nAlign == DT_RIGHT
      nRet := xlHAlignRight
   ELSE
      nRet := xlHAlignCenterAcrossSelection  // DT_CENTER
   ENDIF

   RETURN nRet

* =======================================================================================
// Например можно сделать так: bExtern := {|oSheet,oBrw| ExcelOleExtern(oSheet, oBrw) }
// Сформировать Sheet и получил вызов в блок кода, можно пройтись по ячекам
// Sheet и перебрать ячейки и строки oBrw и задать формулы, форматы, цвета, ...
// доступны все ячейки excel.
FUNCTION ExcelOle4Extern( hProgress, lTsbFont, oSheet, oBrw, oExcel, aColSel, aTitle2 )
   LOCAL lTsbSuperHd, lTsbHeading, lTsbFooting, cRange, oRange
   LOCAL nLine, nCol, nRow, aFColor, nBColor, nFColor, uData
   LOCAL nCount, nTotal, nEvery, aFont, hFont, oCol, nColHead
   LOCAL oldnFColor, aRCnFColor[4], oldaFont[3]
   LOCAL oldnBColor, aRCnBColor[4], aRCaFont[4]
   LOCAL aCol, cVal, lEndTabl
   LOCAL oFont
   LOCAL nColDbf :=Len(aColSel)

   // Цвет шрифта титула таблицы (пример смены цвета)
   aFColor := BLUE
   nLine := 1
   oSheet:Cells( nLine, 1):Font:Color := RGB(aFColor[1],aFColor[2],aFColor[3])

   // проверка суперхидер таблицы
   lTsbSuperHd := oBrw:lDrawSuperHd
   IF lTsbSuperHd
      lTsbSuperHd := ( AScan( oBrw:aSuperHead, {|a| !Empty(a[3]) } ) > 0 )
   ENDIF

   // проверка шапки таблицы
   lTsbHeading := oBrw:lDrawHeaders
   If lTsbHeading
      lTsbHeading := ( AScan( oBrw:aColumns, { |o| !Empty( o:cHeading ) } ) > 0 )
   Endif

   // проверка подвала таблицы
   lTsbFooting := oBrw:lDrawFooters
   If lTsbFooting
      lTsbFooting := ( AScan( oBrw:aColumns, { |o| !Empty( o:cFooting ) } ) > 0 )
   Endif

   If hProgress != Nil
      nTotal := oBrw:nLen
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   if aColSel= Nil .or. Len(aColSel) = 0
     aColSel := CalcAcolselForTbl( oBrw,aColSel)
   Endif

   nLine := 4 +len(aTitle2)// начальная строка заголовка таблицы

   // выводим цвета фона,текста и шрифты суперхидера таблицы
   If lTsbSuperHd
      For nCol := 1 To Len( oBrw:aSuperHead )
          aCol   := oBrw:aSuperHead[ nCol ]
          oSheet:Cells( nLine,  MaxNumFromArr(aColSel,aCol[1])):Value := uData
          cRange := HeadXls(MaxNumFromArr(aColSel,aCol[1])) + Hb_NtoS( nLine )  + ":" + ;
                    HeadXls(MinNumFromArr(aColSel,aCol[2])) + Hb_NtoS( nLine )

          nFColor := myColorN( aCol[4], oBrw, nCol )             // oBrw:nClrSpcHdFore
          nBColor := myColorN( aCol[5], oBrw, nCol )             // oBrw:nClrSpcHdBack
          aFont   := GetFontParam( aCol[7] )                     // шрифт суперхидера
          oRange := oSheet:Range( cRange )
          oFont :=  oRange:Font
          oFont:Color    := nFColor        // Цвет шрифта шапки
          oRange:Interior:Color:= nBColor        // Цвет фона
          If lTsbFont
            oFont:Name := aFont[ 1 ]
            oFont:Size := aFont[ 2 ]
            oFont:Bold := aFont[ 3 ]
          Endif
      Next
      ++nLine
   Endif

   // выводим цвета фона и текста хидера таблицы
   If lTsbHeading
         nColHead := 0

         FOR EACH nCol IN aColSel

            oCol  := oBrw:aColumns[ nCol ]
            uData := If( ValType( oCol:cHeading ) == "B", Eval( oCol:cHeading ), ;
                                  oCol:cHeading )
            If ValType( uData ) != "C"
               Loop
            EndIf

            uData := StrTran( uData, CRLF, Chr( 10 ) )
            If ValType( uData ) != "C"
               Loop
            EndIf

            nFColor := myColorN( oCol:nClrHeadFore, oBrw, nCol )
            nBColor := myColorN( oCol:nClrHeadBack, oBrw, nCol )

            nColHead ++
            oCol    := oBrw:aColumns[ nCol ]

            oRange := oSheet:Cells( nLine, nColHead )
            oFont :=  oRange:Font
            oFont:Color    := nFColor        // Цвет шрифта шапки
            oRange:Interior:Color:= nBColor        // Цвет фона
            If lTsbFont
              hFont := oCol:hFontHead              // шрифт шапки таблицы
              aFont := myFontParam( hFont, oBrw, nCol, 0 )
              oFont:Name := aFont[ 1 ]
              oFont:Size := aFont[ 2 ]
              oFont:Bold := aFont[ 3 ]
            Endif
         Next
         ++nLine
   Endif

   Eval( oBrw:bGoTop )  // переход на начало таблицы

   // начальные данные формирования блоков раскраски и шрифтов
   nCount     := 0
   oldnFColor := Nil
   oldnBColor := Nil
   aFill(oldaFont,Nil)
   lEndTabl   := .f.

   // выводим цвета фона и текста ячеек всех колонок таблицы
   For nRow := 1 TO oBrw:nLen

      nColHead := 0
      FOR EACH nCol IN aColSel
          nColHead++

          If nRow == oBrw:nLen .and. nColHead == nColDbf
             lEndTabl :=.t. //флаг последней ячейки таблицы
          Endif

          oCol    := oBrw:aColumns[ nCol ]
          nFColor := myColorN( oCol:nClrFore, oBrw, nCol, oBrw:nAt )
          nBColor := myColorN( oCol:nClrBack, oBrw, nCol, oBrw:nAt )
          if (!oldnFColor == nFColor)
             // при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnFColor==Nil
                ChangeRangeFontColor( oSheet,oldnFColor, aRCnFColor, ncoldbf )
             Endif
             oldnFColor:=nFColor
             aRCnFColor[1] :=  nLine; aRCnFColor[2] :=  nColHead
          Endif
          aRCnFColor[3] :=  nLine; aRCnFColor[4] :=  nColHead
          If lEndTabl
                ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, ncoldbf)
          Endif
          // Фон шрифта
          if (!oldnBColor == nBColor)
             // при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnBColor==Nil
                ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, ncoldbf)
             Endif
             oldnBColor:=nBColor
             aRCnBColor[1] :=  nLine; aRCnBColor[2] :=  nColHead
          Endif
          aRCnBColor[3] :=  nLine; aRCnBColor[4] :=  nColHead
          If lEndTabl
             ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, ncoldbf)
          Endif
          // Фонт шрифта
          If lTsbFont
            aFont := myFontParam( oCol:hFont, oBrw, nCol, oBrw:nAt )

            if (!(oldaFont[1] == aFont[1].and.oldaFont[2] == aFont[2].and.oldaFont[3] == aFont[3])).or.lEndTabl
               // при изменении цвета либо по концу меняем шрифты области
               if !oldaFont[1] == Nil
                  ChangeRangeFont( oSheet, oldaFont, aRCaFont, ncoldbf)
               Endif
               oldaFont[1] := aFont[1]; oldaFont[2] := aFont[2]; oldaFont[3] := aFont[3]
               aRCaFont[1] :=  nLine; aRCaFont[2] :=  nColHead
             Endif
             aRCaFont[3] :=  nLine; aRCaFont[4] :=  nColHead
             if lEndTabl
              if !oldaFont[1] == Nil
                  ChangeRangeFont( oSheet, oldaFont, aRCaFont, ncoldbf)
               Endif
            Endif
          Endif
      Next

      If hProgress != Nil
         If nCount % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nCount,0)
         EndIf
         nCount ++
      EndIf
      ++nLine
      oBrw:Skip(1)
   Next

   // выводим цвета фона и текста подвала таблицы
   If lTsbFooting

      nColHead := 0
      FOR EACH nCol IN aColSel

         oCol    := oBrw:aColumns[ nCol ]
         nFColor := myColorN( oCol:nClrFootFore, oBrw, nCol, oBrw:nAt )
         nBColor := myColorN( oCol:nClrFootBack, oBrw, nCol, oBrw:nAt )

         nColHead ++

         oRange := oSheet:Cells( nLine, nColHead )
         oFont :=  oRange:Font
         oFont:Color    := nFColor         // Цвет шрифта шапки
         oRange:Interior:Color:= nBColor   // Цвет фона
         If lTsbFont
           aFont := myFontParam( oCol:hFontFoot, oBrw, nCol, 0 )
           oFont:Name := aFont[ 1 ]
           oFont:Size := aFont[ 2 ]
           oFont:Bold := aFont[ 3 ]
         Endif
      Next
      nLine++
   Endif

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()

   // хвост таблицы
   ++nLine

   // Доп.надпись под таблицей
   cVal := "End table ! - Version (" + oExcel:Version + ") " + ExcelVersion( VAL( oExcel:Version ) )
   cVal += "  Path - " + ExcelPath()

   aFColor := RED
   oRange := oSheet:Cells( nLine, 1 )
   oFont :=  oRange:Font
   oFont:Color := RGB(aFColor[1],aFColor[2],aFColor[3])
   oFont:Name  := "Times New Roman"
   oFont:Size  := 16
   oFont:Bold  := .T.
   oRange:Value := cVal
   cRange :=  "A" + HB_NtoS(nLine) + ":" + HeadXls(Len( oBrw:aColumns)) + HB_NtoS(nLine)
   oRange := oSheet:Range( cRange )
   oRange:Merge()

   RETURN Nil
* =======================================================================================
STATIC FUNCTION ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, nMaxCol )
LOCAL nDif := aRCnBColor[3] - aRCnBColor[1]
LOCAL cRange
   Do case
      case nDif == 0
   cRange :=  HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
              HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
      case ndif =1
   cRange :=  HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[1]) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim( Str( aRCnBColor[3]) )+":" + ;
              HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
     Otherwise
   cRange :=  HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[1]) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim(Str( aRCnBColor[1]+1 ))+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[3]-1 ) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim( Str( aRCnBColor[3]) )+":" + ;
              HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
   oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
   Endcase
RETURN Nil
* =======================================================================================
* =======================================================================================
STATIC FUNCTION ChangeRangeFontColor( oSheet,oldnFColor, aRCnFColor, nMaxCol )
LOCAL nDif := aRCnFColor[3] - aRCnFColor[1]
LOCAL cRange
   Do case
      case nDif == 0
   cRange :=  HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
              HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
      case ndif =1
   cRange :=  HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[1]) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim( Str( aRCnFColor[3]) )+":" + ;
              HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
     Otherwise
   cRange :=  HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[1]) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim(Str( aRCnFColor[1]+1 ))+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[3]-1 ) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
   cRange :=  HeadXls(1) + LTrim( Str( aRCnFColor[3]) )+":" + ;
              HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
   oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
   Endcase
RETURN Nil
* =======================================================================================
STATIC FUNCTION ChangeRangeFont( oSheet, oldaFont, aRCaFont, nMaxCol)
LOCAL nDif := aRCaFont[3] - aRCaFont[1]
LOCAL cRange
Local oFont
   Do case
      case nDif == 0
          cRange :=  HeadXls(aRCaFont[2]) + LTrim( Str( aRCaFont[1]) )+":" + ;
              HeadXls(aRCaFont[4]) + LTrim( Str(aRCaFont[3]) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
      case ndif =1
          cRange :=  HeadXls(aRCaFont[2]) + LTrim( Str( aRCaFont[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCaFont[1]) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
          cRange :=  HeadXls(1) + LTrim( Str( aRCaFont[3]) )+":" + ;
              HeadXls(aRCaFont[4]) + LTrim( Str( aRCaFont[3]) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
     Otherwise
          cRange :=  HeadXls(aRCaFont[2]) + LTrim( Str( aRCaFont[1]) )+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCaFont[1]) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
         cRange :=  HeadXls(1) + LTrim(Str( aRCaFont[1]+1 ))+":" + ;
              HeadXls(nMaxCol) + LTrim( Str( aRCaFont[3]-1 ) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
         cRange :=  HeadXls(1) + LTrim( Str( aRCaFont[3]) )+":" + ;
              HeadXls(aRCaFont[4]) + LTrim( Str( aRCaFont[3]) )
              oFont := oSheet:Range(cRange):Font
                  oFont:Name := oldaFont[ 1 ]
                  oFont:Size := oldaFont[ 2 ]
                  oFont:Bold := oldaFont[ 3 ]
   Endcase
RETURN Nil
* =======================================================================================

* =======================================================================================
STATIC FUNCTION myColorN( nColor, oBrw, nCol, nAt )
   If Valtype( nColor ) == "B"
      If empty(nAt)
         nColor := Eval( nColor, nCol, oBrw )
      Else
         nColor := Eval( nColor, nAt, nCol, oBrw )
      EndIf
   EndIf

   If Valtype( nColor ) == "A"
      nColor := nColor[1]
   EndIf

RETURN nColor

* =======================================================================================
STATIC FUNCTION myFontParam( hFont, oBrw, nCol, nAt )
   LOCAL aFont, oCol := oBrw:aColumns[ nCol ]
   DEFAULT nAt := 0
   // шрифт ячеек таблицы
   hFont := If( hFont == Nil, oBrw:hFont, hFont )
   hFont := If( ValType( hFont ) == "B", Eval( hFont, nAt, nCol, oBrw ), hFont )

   If empty(hFont)
      aFont    := array(3)
      aFont[1] := _HMG_DefaultFontName
      aFont[2] := _HMG_DefaultFontSize
      aFont[3] := .F.
   Else
      aFont := GetFontParam( hFont )
   EndIf

RETURN aFont

////////////////////////////////////////////////////////////
STATIC FUNCTION HeadXls(nCol)
RETURN IF(nCol>26,Chr(Int((nCol-1)/26)+64),'')+CHR((nCol-1)%26+65)

///////////////////////////////////////////////////////////////////////////////////////////
Function ExcelAdr(nRow, nCol)
Return IF(nCol>26,Chr(Int((nCol-1)/26)+64),'')+CHR((nCol-1)%26+65) + HB_NtoS(Int(nRow))

///////////////////////////////////////////////////////////////////////////////////////////
Function ExcelAdr2(nRow1, nCol1, nRow2, nCol2)
Return ExcelAdr(nRow1, nCol1) + ':' + ExcelAdr(nRow2, nCol2)

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки версии Excel
FUNCTION ExcelVersion(nVer)
   LOCAL aDim[20]
   DEFAULT nVer := 1

   AFILL(aDim,"???")
   aDim[01] := "No Excel on this computer!"
   aDim[09] := "Excel 2000"
   aDim[10] := "Excel XP"
   aDim[11] := "Excel 2003"
   aDim[12] := "Excel 2007"
   aDim[14] := "Excel 2010"
   aDim[15] := "Excel 2013"
   aDim[16] := "Excel 2016"
   aDim[17] := "Excel 2019"
   aDim[18] := "Excel New!"

   RETURN aDim[nVer]

///////////////////////////////////////////////////////////////////////////////////////////
// Функция пути к Excel
// http://clipper.borda.ru/?1-20-0-00000371-000-0-0-1195742832
// Pasha - Пост N: 645
FUNCTION ExcelPath()
   LOCAL cPath := NIL
   cPath := win_regRead( "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\excel.exe\Path" )
   Return cPath

///////////////////////////////////////////////////////////////////////////////////////////
Function PixelToPointX(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIX()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

///////////////////////////////////////////////////////////////////////////////////////////
Function PixelToPointY(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIY()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

#pragma BEGINDUMP

#include <mgdefs.h>

HB_FUNC( GETDPIX )
{
   HDC    hDC = GetDC( GetDesktopWindow() );
   hb_retni( ( LONG ) GetDeviceCaps(hDC, LOGPIXELSX) );
}
HB_FUNC( GETDPIY )
{
   HDC    hDC = GetDC( GetDesktopWindow() );
   hb_retni( ( LONG ) GetDeviceCaps(hDC, LOGPIXELSY) );
}

#pragma ENDDUMP
