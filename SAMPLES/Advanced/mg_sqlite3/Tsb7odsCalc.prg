/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Pavel Tsarenko <tpe2@mail.ru>
 * Copyright 2020 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
 *
*/
#define _HMG_OUTLOG

#define PBM_SETPOS           1026  // Устанавливает текущую позицию для индикатора выполнения и перерисовывает полосу, чтобы отразить новую позицию
#define LINE_WRITE           100   // Количество строк для записи блоками
#define TYPE_EXCEL_FORMULA   '#'   // мой тип ФОРМУЛА для экселя

#include "minigui.ch"
#include "tsbrowse.ch"
* =======================================================================================
FUNCTION Brw7OleCalc( aTsb, aParam, aTitle, aFoot, aImage, hProgress, bExtern )
   LOCAL cVal, nCol, nLine, nLinef, nTotal, nCount, nEvery, hFont
   LOCAL nColHead, flag_new_OutXls := .f.
   LOCAL aSet, nIndexaSet, uData, aTipeChars
   LOCAL oSM, oSD, oDoc, oDispatch, aProps := {}
   LOCAL oSheet, oRange, nColDbf, nBeginTable
   LOCAL aFont, aClr, aCol, cMsg, cTitle, nStart, nRow
   LOCAL cFile, lActivate, lSave, aFontSet
   LOCAL nRecNo, nAt, nOldRow, nOldCol, lBrSelector, oBrw
   LOCAL lTsbSuperHd, lTsbHeading, lTsbFooting, lFormula
   LOCAL nColSh1, nColSh2
   LOCAL nColBegTbl, rType, rPicture, nPoint
   //LOCAL nRecNo := ( oBrw:cAlias )->( RecNo() ), nAt := oBrw:nAt
   //LOCAL nOldRow := oBrw:nLogicPos(), nOldCol := oBrw:nCell
   //Local lBrSelector := oBrw:lSelector

   Default hProgress := nil, aFoot := nil, aImage := {}, bExtern := nil

   nRecNo := nAt := nOldRow := nOldCol := lBrSelector := nil

   ////////////// структура отчёта ///////////////
   // титул ods-файла, если есть
   // таблица
   // подвал ods-файла, если есть

   lTsbSuperHd := lTsbHeading := lTsbFooting := lFormula :=.f.
   nTotal      := Len(aTsb[4])     // количество строк таблицы
   nColDbf     := Len(aTsb[4,1])   // количество колонок

   cFile       := aParam[1]
   lActivate   := aParam[2]
   lSave       := aParam[3]
   aFontSet    := aParam[4]

   // проверка суперхидер таблицы
   If Len(aTsb[1]) > 0
      lTsbSuperHd := .t.
   ENDIF

   // проверка шапки таблицы
   If Len(aTsb[2]) > 0
      lTsbHeading := .t.
   Endif

   // проверка подвала таблицы
   If Len(aTsb[5]) > 0
      lTsbFooting := .t.
   Endif

   // Определяем стиль названия отчета
   If !Empty(aTitle)
   Endif

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Загружаю отчёт в'
   ELSE                              ; cMsg := 'Upload report to'
   ENDIF
   WaitThreadCreateIcon( cMsg, 'OO Calc OLE ...' )   // запуск без времени

   IF ( oSM := win_oleCreateObject( "com.sun.star.ServiceManager",'-headless' ) ) == NIL
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "OO Calc не доступен !;;  Ошибка"
         cVal := "Ошибка!"
      ELSE
         cMsg += "OO Calc is not available !;;  Error"
         cVal := "Error!"
      ENDIF
      WaitThreadCloseIcon()  // kill the window waiting
      CursorArrow()

      cMsg += " [ " + win_oleErrorText() + " ];;"
      cMsg += REPLICATE( "-._.", 16 ) + ";;"
      cMsg := AtRepl( ";", cMsg, CRLF )
      AlertStop( cMsg, cVal , "ZZZ_B_STOP64", 64, {RED} )
      RETURN Nil
   ENDIF

   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   aProps[1]:Name := "Hidden"
   aProps[1]:Value := .T.

   //   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   //   aProps[2]:Name := "UpdateDocMode"
   //   aProps[2]:Value := 3 // com.sun.star.document.UpdateDocMode.FULL_UPDATE
   //   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   //   aProps[3]:Name := "AsLink"
   //   aProps[3]:Value := .F.
   //   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   //   aProps[4]:Name := "Style"
   //   aProps[4]:Value := "Graphics"//"Графика"
   //   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   //   aProps[5]:Name = "FilterName"
   //   aProps[5]:Value = "<All formats>"
   //   AADD(aProps, oSM:Bridge_GetStruct("com.sun.star.beans.PropertyValue"))
   //   aProps[6]:Name := "MacroExecutionMode"
   //   aProps[6]:Value := 4 // com.sun.star.document.MacroExecMode.ALWAYS_EXECUTE_NO_WARN
   ////   aProps[7]:Name := "FilterName"
   ////   aProps[7]:Value := "MS Word 97"//Не работает

   oSD := oSM:createInstance("com.sun.star.frame.Desktop")
   oDoc := oSD:LoadComponentFromURL( "private:factory/scalc", "_blank", 0, aProps)

   oSheet := oDoc:Sheets:getByIndex(0)
   oDispatch := oSM:createInstance("com.sun.star.frame.DispatchHelper")

   // бегунок таблицы, если есть
   If hProgress != Nil
      nTotal := oBrw:nLen
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   aSet := Array(Min(LINE_WRITE,nTotal), nColDbf )
   aTipeChars := Array( nColDbf )

   nLine  := 1
   // Картинка в верхний угол таблицы
   //If ! Empty( aImage )
   //   Paint_Png(oDoc,aImage)
   //Endif

   // Заголовок таблицы
   If !Empty(aTitle)
      For nRow := 1 TO Len(aTitle)
         if Len(aTitle[nRow]) >0
            cTitle := aTitle[nRow,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aTitle[nRow,2]),nColDbf,aTitle[nRow,2])
            SetOOValue( oSheet, nLine, aTitle[nRow,1], AllTrim( cTitle ), aTitle[nRow,6])
            oRange := OORange( oSheet, nLine,aTitle[nRow,1],, nCol)
            oRange:Merge( .t. )
            If aTitle[nRow,4] != Nil
              aFont := aTitle[nRow,4]
              aClr  := aTitle[nRow,5]
              SetOOFont( oRange, aFont, RGB(aClr[1,1],aClr[1,2],aClr[1,3]),RGB(aClr[2,1],aClr[2,2],aClr[2,3]) )
            EndIf
         EndIf
         ++nLine
       Next
       ++nLine
   EndIf

   nColBegTbl := nLine  // начальная строка заголовка таблицы
   // Выводим суперхидер таблицы
   If Len(aTsb[1])>0
      nCol :=0
      nColSh2 :=0
      FOR EACH aCol IN aTsb[1]
         nCol++
         nColSh1 := if(aCol[5]>0, aCol[5], nColSh2+1)
         // Если  с -1 не последняя и следующая нормальная, то берем до следующей
         if aCol[6]>0.and.nCol<Len(aTsb[1])
            if aTsb[1,nCol+1,5]>0
                nColSh2 := aTsb[1,nCol,5]-1
            endif
         endif
         nColSh2 := if(aCol[6]>0, aCol[6], if(nCol==Len(aTsb[1]), nColDbf, nColSh1))
         if nColSh2>nColDbf
         //Если в суперхидере задано больше колонок, чем в таблице
         // (в случае наличия Селектора, например)
             nColSh2 := nColDbf
         endif
         SetOOValue( oSheet, nLine,  nColSh1, if(Empty(aCol[4]),' ',aCol[4]))
         oRange := OORange( oSheet, nLine, nColSh1,, nColSh2,1)
         oRange:Merge(.t.)
         oRange:HoriJustify := 2
         aFont := GetFontParam(aCol[3])
         SetOOFont( oRange, aFont )
         // OOCellAllBorder(oRange)
      NEXT
      ++nLine
   Endif

   // Выводим шапку таблицы
   If Len(aTsb[2])>0
      FOR nColHead:= 1 to Len(aTsb[2])
         uData := StrTran( aTsb[2,nColHead,4], CRLF, Chr( 10 ) )
         oRange := SetOOValue( oSheet, nLine, nColHead, uData )
         aFont := GetFontParam( aTsb[2,nColHead,3])
         SetOOFont( oRange, aFont )
         //   OOCellAllBorder(oRange)
      Next
      ++ nLine
   Endif

   // Нумератор таблицы
   If Len(aTsb[3])>0
      FOR nCol:= 1 to Len(aTsb[3])
         uData := StrTran( if(empty(aTsb[3,nCol,4]),' ',aTsb[3,nCol,4]), CRLF, Chr( 10 ) )
         oRange := SetOOValue( oSheet, nLine, nCol, uData , 2)
         aFont := GetFontParam( aTsb[3,nCol,3])
         SetOOFont( oRange, aFont )
         // OOCellAllBorder(oRange)
      Next
      ++nLine
   Endif

   // Печать - СТРОК таблицы ВСЕГДА !
   nIndexaSet := 1
   nStart := nLine
   nLinef      := nLine
   nBeginTable := nStart
   FOR nRow:= 1 to nTotal
      FOR nColHead:= 1 to nColDbf
         uData    := aTsb[4,nRow,nColHead,4]

         rType    := aTsb[4,nRow,nColHead,5]
         rPicture := aTsb[4,nRow,nColHead,6]
         do Case
            Case (rType==TYPE_EXCEL_FORMULA)
               uData    := ''
               lFormula := .t. //это формула
            Case (rType=='@'.or.rType=='D').and.Empty(uData)
               uData := ''
            Case ValType( uData )=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case rType == 'L'
               rType := 'C'
            Case rPicture != Nil .and. uData != Nil .and. rType !='N'
              uData := Transform( uData, rPicture )
         endCase

         // определяем тип поля в колонке
         If !(rType = "U") .and. Empty(aTipeChars[nColHead]) .and. !Empty(uData )
            aTipeChars[nColHead] := rType

            Do case
                  // при необходимости можно поставить тип для других полей
                 // oSheet:Range(cRange+LTrim( Str(nBeginTable))+':'+cRange+LTrim( Str(nTotal+nBeginTable-1))):WrapText := .f.
               case (rPicture != NIL).and.(aTipeChars[nColHead] =='N'.or.aTipeChars[nColHead] =='+'.or.aTipeChars[nColHead] =='^').and.Valtype(udata)='N'
                 nPoint   := AT('.', rPicture )
                 if nPoint == 0
                    rPicture :='#'
                 else
                    //Количесво после . плавоющее   rPicture := Repl("#",nPoint-2) + '0.' + Repl("#",Len(rPicture)-nPoint)
                    rPicture := Repl("#",nPoint-2) + '0,' + Repl("0",Len(rPicture)-nPoint)
                 endif
              oRange := OORange( oSheet, nBeginTable, nColHead, nTotal+nBeginTable-1, nColHead)
              oRange:NumberFormat := getNumberFormat (oDoc, rPicture)
            Endcase
       Endif
         uData := If( ValType( uData )=="N", uData , ;
               If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) )
         // запоминаем данные в массив
         aSet[ nIndexaSet , nColHead ] := uData

      Next

      IF (nIndexaSet == LINE_WRITE).or.(nRow == nTotal) // По заполнению масиива или конца таблицы
         flag_new_OutXls := .t. // массив заполнен - нужно пересылать в таблицу в Excel
      ENDIF

      ++nLine

      // Заполнение таблицы по LINE_WRITE строк из накопленного масссива
      IF flag_new_OutXls

         oRange := OORange( oSheet, nStart, 1, nLine-1, nColDbf )
         if Len( aSet ) > nLine - nStart
            ASize(aSet, nLine - nStart )       // Последний блок
         endif
         oRange:setDataArray( aSet )

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

   Next
   // Выводим формулы

   if lFormula  // есть формулы

      FOR nRow:= 1 to nTotal //Len(aTsb[4])
         FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
            rType    := aTsb[4,nRow,nColHead,5]
            uData    := aTsb[4,nRow,nColHead,4]
            if rType==TYPE_EXCEL_FORMULA.or.rType=='C'.and.Left(uData,1)='='
               // oRange := OORange( oSheet, nLinef, nColHead, nLine, nColHead)

                oRange := oSheet:getCellByPosition( nColHead-1, nLinef-1 )
                rPicture := aTsb[4,nRow,nColHead,6]
              /*  Формат ячеек, если будет задаваться
                              nPoint   := AT('.', rPicture )
                              if nPoint == 0
                                  rPicture :='#'
                              else
                                 rPicture := Repl("#",nPoint-2) + '0,' + Repl("0",Len(rPicture)-nPoint)

                              endif
                               // формат типа := '## ### ###0' или '## ### ###0,00'
                              oRange:NumberFormat := getNumberFormat (oDoc, rPicture)
              */
      if At('[',uData)>0
         uData := AtRepl('RC[',uData,'INDIRECT("RC[')
         uData := AtRepl(']',uData,']";0)')
      endif
                oRange:SetFormula(uData)
            //проверка записи формулы ? "Formula=",oRange:Formula; ?nRow ; ?
            endif
         Next
        nLinef++
      Next
  endif

   // выводим подвал таблицы
   nColHead := 0
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to Len(aTsb[5])
         uData := aTsb[5,nColHead,4]
         oRange := SetOOValue( oSheet, nLine, nColHead, uData )
         aFont := GetFontParam( aTsb[5,nColHead,3])
         SetOOFont( oRange, aFont )
      Next
      nLine++
   Endif

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()
   // Шрифт только таблицы с данными
   oRange := OORange( oSheet, nBeginTable, 1, nLine-2, nColDbf )
   aFont := GetFontParam( hFont )

   // создать сетку на таблицу
   oRange := OORange( oSheet, nColBegTbl, 1, nLine-1, nColDbf )
   OOCellAllBorder(oRange)
   // Здесь можно делать дообработку таблицы: поместить подписи под таблицей и т.vд.
   // Текстовый подвал таблицы
   If !Empty(aFoot)
      For nRow := 1 TO Len(aFoot)
         if Len(aFoot[nRow]) >0
            cTitle := aFoot[nRow,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aFoot[nRow,2]),nColDbf,if(aFoot[nRow,2]<0,aFoot[nRow,1],aFoot[nRow,2]))
            SetOOValue( oSheet, nLine, aFoot[nRow,1], AllTrim( cTitle ), aFoot[nRow,6])
            oRange := OORange( oSheet, nLine,aFoot[nRow,1],, nCol)
            oRange:Merge( .t. )
            If aFoot[nRow,4] != Nil
              aFont := aFoot[nRow,4]
              aClr  := aFoot[nRow,5]
              SetOOFont( oRange, aFont, RGB(aClr[1,1],aClr[1,2],aClr[1,3]), RGB(aClr[2,1],aClr[2,2],aClr[2,3]))
            EndIf
         EndIf
         ++nLine
       Next
   EndIf

   // хвост таблицы
   ++nLine
   ++nLine

   // Доп.надпись под таблицей
   cVal := "End table !  -  Calc Version: "+  OO_Version()
   cVal += "  Path - " + CalcPath() + "  +  " + MiniGuiVersion()
   SetOOValue( oSheet, nLine, 1, cVal )
   //   oRange := oSheet:Cells( nLine, 1 )
   aClr := RED
   //   aClr := BLUE
   oRange := OORange( oSheet, nLine,1,nLine, nColDbf)
   SetOOFont( oRange, aFont, RGB(aClr[1],aClr[2],aClr[3]))
   oRange:Merge( .t. )
   aClr := BLUE

   //-----------------
   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   If bExtern != Nil
      Eval( bExtern, oSheet, aTsb, aTitle)   // вызов внешнего блока дообработки таблицы
   EndIf

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   for nCol := 1 to nColDbf
      oRange := oSheet:getColumns():getByIndex(nCol-1)
      oRange:OptimalWidth := .t.
      oRange:IsTextWrapped := .t.
   next

   If ! Empty( cFile ) .and. lSave
      oDoc:StoreAsURL( ConvFileOO( cFile ), {})
      // oDoc:StoreAsURL( ConvFileOO( cFile ), aProps)
   EndIf

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lActivate
      oDoc:getCurrentController:getFrame:getContainerWindow:setVisible( .t. )
      SetCalcWindowToForeground(cFile)
   Else
     oDoc:close(.t.) // закрыть Calc
   EndIf

   RETURN Nil

* =======================================================================================
// Например можно сделать так: bExtern := {|oSheet,oBrw| ExcelOleExtern(oSheet, oBrw) }
// Сформировать Sheet и получил вызов в блок кода, можно пройтись по ячекам
// Sheet и перебрать ячейки и строки oBrw и задать формулы, форматы, цвета, ...
// доступны все ячейки excel.
FUNCTION CalcOle7Extern( hProgress, oSheet, aTsb, aXlsTitle)
   LOCAL oRange, nCol, nRow, nBColor, nFColor
   LOCAL nCount, nTotal, nEvery, aFont, nColHead
   LOCAL oldnFColor, aRCnFColor[4], oldaFont[3]
   LOCAL oldnBColor, aRCnBColor[4], aRCaFont[4]
   LOCAL aCol, lEndTabl, nColDbf, nCell
   LOCAL nLine, lTsbFontTable, lTsbFontHeader
   LOCAL nBeginTable, nColSh1, nColSh2

   nLine          := 1
   nTotal         := Len(aTsb[4])     // количество строк таблицы
   nColDbf        := Len(aTsb[4,1])   // количество колонок
   lTsbFontTable  := .t.              // менять фонты таблицы
   lTsbFontHeader := .f.              // менять фонты заголовка и подвала

   // Заголовок таблицы
   If !Empty(aXlsTitle)
      nLine += Len(aXlsTitle)+1
   EndIf

   If hProgress != Nil
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   // выводим цвета фона,текста и шрифты суперхидера таблицы
   nCell:=0
   // Выводим суперхидер таблицы
   If Len(aTsb[1])>0
      nCol :=0
      nColSh2 :=0
      FOR EACH aCol IN aTsb[1]
         nCol++
         nColSh1 := if(aCol[5]>0, aCol[5], nColSh2+1)
         // Если  с -1 не последняя и следующая нормальная, то берем до следующей
         if aCol[6]>0.and.nCol<Len(aTsb[1])
       if aTsb[1,nCol+1,5]>0
                nColSh2 := aTsb[1,nCol,5]-1
            endif
         endif
         nColSh2 := if(aCol[6]>0, aCol[6], if(nCol==Len(aTsb[1]), nColDbf, nColSh1))
         if nColSh2>nColDbf
         //Если в суперхидере задано больше колонок, чем в таблице
         // (в случае наличия Селектора, например)
            nColSh2:=nColDbf
         endif
         nFColor := myColorFirst(aCol[1])
         nBColor := myColorFirst(aCol[2])
         SetOOValue( oSheet, nLine,  nColSh1, if(Empty(aCol[4]),' ',aCol[4]))
         oRange := OORange( oSheet, nLine, nColSh1,, nColSh2,1)
         SetOOColor( oRange, nFColor ,nBColor)
      NEXT
      ++nLine
   Endif
   // Выводим шапку таблицы
   If Len(aTsb[2])>0
      nCol :=0
      FOR nColHead:= 1 to Len(aTsb[2])
         nFColor := myColorFirst(aTsb[2,nColHead,1])
         nBColor := myColorFirst(aTsb[2,nColHead,2])
         oRange := OORange( oSheet, nLine,nColHead,nLine, nColHead)
         SetOOColor( oRange, nFColor ,nBColor)
      Next
      ++ nLine
   Endif

   // Нумератор таблицы
   If Len(aTsb[3])>0
      FOR nCol:= 1 to Len(aTsb[3])
         nFColor := myColorFirst(aTsb[3,nCol,1])
         nBColor := myColorFirst(aTsb[3,nCol,2])
         oRange := OORange( oSheet, nLine,nCol,nLine, nCol)
         SetOOColor( oRange, nFColor ,nBColor)
      Next
      ++nLine
   Endif
   // начальные данные формирования блоков раскраски и шрифтов
   nCount     := 0
   oldnFColor := Nil
   oldnBColor := Nil
   aFill(oldaFont,Nil)
   lEndTabl   := .f.

   // выводим цвета фона и текста ячеек всех колонок таблицы//
   nBeginTable := nLine

   FOR nRow:= 1 to nTotal //Len(aTsb[4])
      FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
          If nRow == nTotal .and. nColHead == nColDbf
             lEndTabl :=.t. //флаг последней ячейки таблицы
          Endif

          nFColor := myColorFirst(aTsb[4,nRow,nColHead,1])
          nBColor := myColorFirst(aTsb[4,nRow,nColHead,2])
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
                ChangeRangeFontColor( oSheet,oldnFColor, aRCnFColor, ncoldbf )
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
          If lTsbFontTable
            aFont := GetFontParam(aTsb[4,nRow,nColHead,3])
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
   Next

   // выводим цвета фона и текста подвала таблицы
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to Len(aTsb[5])
         nFColor := myColorFirst(aTsb[5,nColHead,1])
         nBColor := myColorFirst(aTsb[5,nColHead,2])
         oRange := OORange( oSheet, nLine,nColHead,nLine, nColHead)

         SetOOColor( oRange, nFColor ,nBColor)
      Next
      nLine++
   Endif
   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()

   RETURN Nil


* =======================================================================================
STATIC FUNCTION ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, nMaxCol )
   LOCAL oRange, nDif := aRCnBColor[3] - aRCnBColor[1]
   Do case
      case nDif == 0
         oRange := OORange( oSheet, aRCnBColor[1], aRCnBColor[2],aRCnBColor[3], aRCnBColor[4])
         SetOOColor( oRange, ,oldnBColor)

      case ndif =1
         oRange := OORange( oSheet, aRCnBColor[1], aRCnBColor[2],aRCnBColor[1], nMaxCol)
         SetOOColor( oRange, ,oldnBColor)
         oRange := OORange( oSheet, aRCnBColor[3], 1,aRCnBColor[3], aRCnBColor[4])
         SetOOColor( oRange, ,oldnBColor)

      Otherwise
         oRange := OORange( oSheet, aRCnBColor[1], aRCnBColor[2],aRCnBColor[1], nMaxCol)
         SetOOColor( oRange, ,oldnBColor)
         oRange := OORange( oSheet, aRCnBColor[1]+1, 1,aRCnBColor[3]-1, nMaxCol)
         SetOOColor( oRange, ,oldnBColor)
         oRange := OORange( oSheet, aRCnBColor[3], 1,aRCnBColor[3], aRCnBColor[4])
         SetOOColor( oRange, ,oldnBColor)

   Endcase
RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFontColor( oSheet,oldnFColor, aRCnFColor, nMaxCol )
   LOCAL oRange, nDif := aRCnFColor[3] - aRCnFColor[1]
   Do case
      case nDif == 0
         oRange := OORange( oSheet, aRCnFColor[1], aRCnFColor[2],aRCnFColor[3], aRCnFColor[4])
         SetOOColor( oRange,oldnFColor)
      case ndif =1
         oRange := OORange( oSheet, aRCnFColor[1], aRCnFColor[2],aRCnFColor[1], nMaxCol)
         SetOOColor( oRange,oldnFColor)
         oRange := OORange( oSheet, aRCnFColor[3], 1,aRCnFColor[3], aRCnFColor[4])
         SetOOColor( oRange,oldnFColor)
      Otherwise
         oRange := OORange( oSheet, aRCnFColor[1], aRCnFColor[2],aRCnFColor[1], nMaxCol)
         SetOOColor( oRange,oldnFColor)
         oRange := OORange( oSheet, aRCnFColor[1]+1, 1,aRCnFColor[3]-1, nMaxCol)
         SetOOColor( oRange,oldnFColor)
         oRange := OORange( oSheet, aRCnFColor[3], 1,aRCnFColor[3], aRCnFColor[4])
         SetOOColor( oRange,oldnFColor)
   Endcase
RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFont( oSheet, oldaFont, aRCaFont, nMaxCol)
   LOCAL oRange, nDif := aRCaFont[3] - aRCaFont[1]

   Do case
      case nDif == 0
         oRange := OORange( oSheet, aRCaFont[1], aRCaFont[2],aRCaFont[3], aRCaFont[4])
         SetOOFont( oRange, oldaFont)
      case ndif =1
         oRange := OORange( oSheet, aRCaFont[1], aRCaFont[2],aRCaFont[1], nMaxCol)
         SetOOFont( oRange, oldaFont)
         oRange := OORange( oSheet, aRCaFont[3], 1,aRCaFont[3], aRCaFont[4])
         SetOOFont( oRange, oldaFont)
      Otherwise
         oRange := OORange( oSheet, aRCaFont[1], aRCaFont[2],aRCaFont[1], nMaxCol)
         SetOOFont( oRange, oldaFont)
         oRange := OORange( oSheet, aRCaFont[1]+1, 1,aRCaFont[3]-1, nMaxCol)
         SetOOFont( oRange, oldaFont)
         oRange := OORange( oSheet, aRCaFont[3], 1,aRCaFont[3], aRCaFont[4])
         SetOOFont( oRange, oldaFont)
   Endcase

RETURN Nil

* =======================================================================================
STATIC FUNCTION myColorFirst(nColor)
   If Valtype( nColor ) == "A"
      nColor := nColor[1]
   EndIf
Return nColor

* =======================================================================================
Static func SetOOValue( oSheet, nRow, nCol, cValue, nAlign )
   Local oRange := oSheet:getCellByPosition( nCol-1, nRow-1 )
   if cValue # nil
      if Valtype(cValue)=='N'
        oRange:Value( cValue )
      else
        oRange:SetString( cValue )
      Endif
   endif
   if nAlign # nil
      oRange:HoriJustify := nAlign
   endif
Return oRange

* =======================================================================================
Static func OORange( oSheet, nRow1, nCol1, nRow2, nCol2)
   Return oSheet:getCellRangeByPosition( nCol1-1, nRow1-1, nCol2-1, if(nRow2==nil, nRow1, nRow2) - 1)

* =======================================================================================
Static func SetOOFont( oRange, aFont, nRGB ,nBackRBG)
Local aClr
   oRange:CharFontName := aFont[ 1 ]
   oRange:CharHeight := aFont[ 2 ]
   if aFont[ 3 ]
      oRange:CharWeight := 150
   else
      oRange:CharWeight := 100
   endif
   if nRGB # nil
      aClr := HMG_n2RGB(nRGB)
      oRange:CharColor := RGB( aClr[3], aClr[2], aClr[1] )
   endif
   if nBackRBG # nil
      aClr := HMG_n2RGB(nBackRBG)
      oRange:CellBackColor := RGB( aClr[3], aClr[2], aClr[1] )
   endif
Return nil

* =======================================================================================
Static func SetOOColor( oRange, nRGB ,nBackRBG)
   Local aClr
   if nRGB # nil
      aClr := HMG_n2RGB(nRGB)
      oRange:CharColor := RGB( aClr[3], aClr[2], aClr[1] )
   endif
   if nBackRBG # nil
      aClr := HMG_n2RGB(nBackRBG)
      oRange:CellBackColor := RGB( aClr[3], aClr[2], aClr[1] )
   endif
Return nil

* =======================================================================================
Static func ConvFileOO(cFile)
   cFile := StrTran(cFile, "\", "/")
   cFile := StrTran(cFile, " ", "%20")
   cFile := "file:///" + cFile
   Return cFile

* =======================================================================================
Function OOCellBorder(oRange, nNSEW, nWidth)
   Local oBorder := oRange:TableBorder
   Local oLine

   if nNSEW == 8
      oLine := oBorder:TopLine
   elseif nNSEW == 9
      oLine := oBorder:BottomLine
   elseif nNSEW == 10
      oLine := oBorder:RightLine
   elseif nNSEW == 7
      oLine := oBorder:LeftLine
   elseif nNSEW == 11
      oLine := oBorder:VerticalLine
   elseif nNSEW == 12
      oLine := oBorder:HorizontalLine
   endif
   if nNSEW < 11
      oLine:OuterLineWidth := if(nWidth == nil, 10, nWidth)
   else
      oLine:InnerLineWidth := if(nWidth == nil, 10, nWidth)
   endif

   if nNSEW == 8
      oBorder:TopLine := oLine
   elseif nNSEW == 9
      oBorder:BottomLine := oLine
   elseif nNSEW == 10
      oBorder:RightLine := oLine
   elseif nNSEW == 7
      oBorder:LeftLine := oLine
   elseif nNSEW == 11
      oBorder:VerticalLine := oLine
   elseif nNSEW == 12
      oBorder:HorizontalLine := oLine
   endif

   oRange:TableBorder := oBorder

Return nil

* =======================================================================================
// окно Calc на передний план
// срабатывает только если нет окна ОО Calc на рабочем столе компа.
STATIC FUNCTION SetCalcWindowToForeground(cFile)
   LOCAL hWnd, cTitle, cMsg

   //  поиск ХЕНДЛА открытого окна документа
   cTitle := hb_FNameNameExt(cFile) + " - OpenOffice Calc"
   cMsg   := "Не нашёл окно / Didn't find the window : " + cTitle
   hWnd := FindWindowEx(,,, cTitle )
   IF hWnd == 0
      AlertStop( cMsg, "Error", "ZZZ_B_STOP64", 64, {RED} )
   ENDIF

   IF hWnd > 0
      ShowWindow( hWnd, 6 )      // MINIMIZE windows
      ShowWindow( hWnd, 3 )      // MAXIMIZE windows
      BringWindowToTop( hWnd )   // A window on the foreground
   ENDIF

   RETURN NIL

* =======================================================================================
STATIC FUNCTION OOCellAllBorder(oRange, nWidth)
   OOCellBorder( oRange, 7 , nWidth)
   OOCellBorder( oRange, 8 , nWidth)
   OOCellBorder( oRange, 9 , nWidth)
   OOCellBorder( oRange, 10 , nWidth)
   OOCellBorder( oRange, 11 , nWidth)
   OOCellBorder( oRange, 12 , nWidth)
RETURN NIL

* =======================================================================================
STATIC Function getNumberFormat (iDoc, iFormat)//, iLang, iCountry)
   STATIC aFormat:={}
   Local Numformat, iFormatId
   // ищем в более раннних поисках
   if (Numformat:=Ascan(aFormat,{|x|x[1]==iFormat})) >0
      iFormatId:= aFormat[Numformat,2]
   Else
   // Не нашли, ищем или добавляем новый номер формата
      iFormatId := iDoc:NumberFormats:queryKey(iFormat,, .t.)
      If iFormatId = -1
        iFormatId := iDoc:NumberFormats:addNew(iFormat,nil)
      EndIf
      Aadd(aFormat,{iFormat,iFormatId})
   Endif
return iFormatId

* =======================================================================================
Function Paint_Png(oDoc,aImage)
   local oStarManager//,oStarDesktop
   Local oImagen_obj
   Local oSize, oPos
   Local oDP
   Local oDrawPages
   Local filename
   oStarManager:=win_oleCreateObject('com.sun.star.ServiceManager')
   oImagen_obj:=oDoc:createInstance("com.sun.star.drawing.GraphicObjectShape")
   //filename:="file:///"+aImage[1]
   //filename:= atrepl("DOCUME~1",aImage[1],"Documents and Settings")
   //filename:= atrepl("LOCALS~1",filename,"Local Settings")
   filename:= "file:///"+GetStartUpFolder()+"/Res/MiniGuiLogo.png"
   filename:= atrepl("\",filename,"/")
   oImagen_obj:GraphicURL :=filename
   oSize := oImagen_obj:Size
   oSize:Height := aImage[2]*14
   oSize:Width :=  aImage[3]*43
   oImagen_obj:Size := oSize
   oPos := oImagen_obj:Position
   oPos:X := 1
   oPos:Y := 1
   oImagen_obj:Position := oPos
   oDrawPages := oDoc:DrawPages
   oDP := oDoc:DrawPages:getByIndex(0)
   oDP:add(oImagen_obj)
Return NIL

* =======================================================================================
Function OO_Version()
   local oStarManager,oStarDesktop
   local oSet, oConfigProvider
   local oParm
   local sProvider, sAccess

   sProvider:= "com.sun.star.configuration.ConfigurationProvider"
   sAccess := "com.sun.star.configuration.ConfigurationAccess"

   oStarManager:=win_oleCreateObject('com.sun.star.ServiceManager')
   oConfigProvider:=oStarManager:createInstance(sProvider)
   oStarDesktop:=oStarManager:createInstance('com.sun.star.frame.Desktop')

   oParm:=oStarDesktop:bridge_GetStruct("com.sun.star.beans.PropertyValue")
   oParm:Name:="nodepath"
   oParm:Value:="/org.openoffice.Setup/Product"

   oSet := oConfigProvider:createInstanceWithArguments(sAccess, {oParm} )

return oSet:getByName("ooSetupVersion")

* =======================================================================================
STATIC FUNCTION CalcPath()
   LOCAL cPath := NIL
   cPath := win_regRead( "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\scalc.exe\Path" )
Return cPath
