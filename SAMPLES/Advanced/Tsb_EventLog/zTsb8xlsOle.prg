/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2020-2025 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
 *
 * Экспорт таблицы Excel в файл xls / не показывать нули на листе
 * Export Excel spreadsheets to file xls / do not show zeros on the sheet
*/
//#define  _HMG_OUTLOG
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
FUNCTION Brw8XlsOle( aTsb, aXlsParam, aXlsTitle, aXlsFoot, aImage, hProgress, bExtern )
   LOCAL cVal, nCol, nLine, nTotal, nCount, nEvery
   LOCAL nColHead, nColBegTbl, flag_new_OutXls := .f.
   LOCAL oExcel, oBook, oSheet, oRange, cRange, nColDbf, nBeginTable
   LOCAL hWnd, uData , aTipeChars[Len(aTsb[4,1])]
   LOCAL aSet[ Min(LINE_WRITE,Len(aTsb[4])), Len(aTsb[4,1]) ]
   LOCAL aFont, aFontSHF, aClr, cMsg, cTitle, nStart, nRow, aCol
   LOCAL rType, nPoint, rPicture, nColSh1, nColSh2, xVal
   LOCAL cFile := aXlsParam[1], lActivate := aXlsParam[2]
   LOCAL lSave := aXlsParam[3], hFont := aXlsParam[4]
   LOCAL NINDEXASET, lShowZeros
   Default hProgress := nil
   Default aXlsTitle := nil, aXlsFoot := nil
   Default aImage := {}, bExtern := nil

   ////////////// структура отчёта / report structure ///////////////
   // титул таблицы, если есть   / table title, if any
   // сама таблица               / the table itself
   // подвал таблицы, если есть  / table footer, if any

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Загружаю отчёт в'
   ELSE                              ; cMsg := 'Upload report to'
   ENDIF

   WaitWindow( { cMsg, 'EXCEL OLE ...', App.Exename } , .T., 600, 16, NIL, YELLOW, LGREEN )

   // Используем Ole из HBWIN.lib
   IF ( oExcel := win_oleCreateObject( "Excel.Application" ) ) == NIL
      cMsg := ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "MS Excel не доступен !;;   Ошибка"
         cVal := "Ошибка!"
      ELSE
         cMsg += "MS Excel is not available !;;   Error"
         cVal := "Error!"
      ENDIF
      WaitWindow()
      DO EVENTS
      CursorArrow()
      cMsg += " [ " + win_oleErrorText() + " ];;"
      AlertStop( cMsg, cVal, , 64, {RED} )
      RETURN Nil
   ENDIF

   oExcel:Visible := .F.          // .T. показать Excel на экране для отладки
   oExcel:DisplayAlerts := .F.    // убрать предупреждения Excel

   oExcel:WorkBooks:Add()
   oBook  := oExcel:ActiveWorkBook
   oSheet := oExcel:ActiveSheet

   // назначение фонта для суперхидера + шапки + подвал таблицы
   aFontSHF := GetFontParam( hFont )

   // бегунок таблицы, если есть
   nTotal := Len(aTsb[4]) //количество строк таблицы
   If hProgress != Nil
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   nColDbf := Len(aTsb[4,1])//количество колонок
   nLine  := 1

   // Картинка в верхний угол таблицы
   If ! Empty( aImage )
      oRange:=oSheet:Range(osheet:cells(1,1),osheet:cells(1,1))
      //  oExcel:ActiveSheet:Shapes:AddPicture(aImage[1],0, -1, oRange:Left, oRange:Top, -1, -1 ) не работает
      oExcel:ActiveSheet:Shapes:AddPicture(aImage[1],0, -1, oRange:Left, oRange:Top, PixelToPointX(aImage[2]),PixelToPointY(aImage[3]))
   Endif
   // Заголовок таблицы

   If !Empty(aXlsTitle)
      For nRow := 1 TO Len(aXlsTitle)
         if Len(aXlsTitle[nRow]) >0
            cTitle := aXlsTitle[nRow,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXlsTitle[nRow,2]),nColDbf,aXlsTitle[nRow,2])
            oSheet:Cells( nLine, aXlsTitle[nRow,1]):NumberFormat := '@'
            oSheet:Cells( nLine, aXlsTitle[nRow,1]):Value := AllTrim( cTitle )
            cRange :=  HeadXls( aXlsTitle[nRow,1]) + Hb_NtoS( nLine )  + ":" + ;
                       HeadXls( nCol) + Hb_NtoS( nLine )
            oRange := oSheet:Range( cRange )
            If aXlsTitle[nRow,6] != Nil
              oRange:HorizontalAlignment := TbsXlsAlign( aXlsTitle[nRow,6] )
            Else
              oRange:HorizontalAlignment := TbsXlsAlign( DT_CENTER )
            Endif
            oRange:Merge()
            If aXlsTitle[nRow,4] != Nil
              aFont := aXlsTitle[nRow,4]
              aClr  := aXlsTitle[nRow,5]
              oRange:Font:Name := aFont[ 1 ]
              oRange:Font:Size := aFont[ 2 ]
              oRange:Font:Bold := aFont[ 3 ]
              oRange:Font:Color = RGB(aClr[1,1],aClr[1,2],aClr[1,3])
              oRange:Interior:Color := RGB(aClr[2,1],aClr[2,2],aClr[2,3])
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
         oSheet:Cells( nLine,  nColSh1):NumberFormat := '@'
         oSheet:Cells( nLine,  nColSh1):Value := if(Empty(aCol[4]),' ',aCol[4])
         cRange :=  HeadXls( nColSh1) + Hb_NtoS( nLine )  + ":" + ;
                    HeadXls( nColSh2) + Hb_NtoS( nLine )
         oSheet:Range( cRange ):HorizontalAlignment  := xlHAlignCenterAcrossSelection
         aFontSHF := GetFontParam(aCol[3])
         oSheet:Range( cRange ):Font:Name := aFontSHF[ 1 ]
         oSheet:Range( cRange ):Font:Size := aFontSHF[ 2 ]
         oSheet:Range( cRange ):Font:Bold := aFontSHF[ 3 ]
      NEXT
      ++nLine
   Endif

   // Выводим шапку таблицы
   If Len(aTsb[2])>0
      nCol := 0
      FOR nColHead := 1 to Len(aTsb[2])
         // IF oCel:lMultiLine
         xVal := aTsb[2,nColHead,4]
         xVal := myMultiHeadStr(xVal)   // см. внизу файла
         oSheet:Cells( nLine, nColHead):NumberFormat := '@'
         oSheet:Cells( nLine, nColHead ):Value := xVal
         oRange:=oSheet:Range(osheet:cells(nLine, nColHead),osheet:cells(nLine, nColHead))
         // oSheet:Cells( nLine, nColHead ):Borders():LineStyle := xlContinuous
         oRange:HorizontalAlignment := TbsXlsAlign( DT_CENTER )
         aFontSHF := GetFontParam( aTsb[2,nColHead,3])
         oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
         oSheet:Cells( nLine, nColHead ):Font:Size := aFontSHF[ 2 ]
         oSheet:Cells( nLine, nColHead ):Font:Bold := aFontSHF[ 3 ]
         // aWidthChars [nCol] := max(aWidthChars [nCol], LenStrokaWithCRLF(uData))
      Next
      ++ nLine
   Endif

   // Нумератор таблицы
   If Len(aTsb[3])>0
      FOR nCol:= 1 to Len(aTsb[3])
         oSheet:Cells( nLine, 1):NumberFormat := '@'
         oSheet:Cells( nLine, nCol ):Value := if(empty(aTsb[3,nCol,4]),' ',aTsb[3,nCol,4])
         oRange:=oSheet:Range(osheet:cells(nLine, nCol),osheet:cells(nLine, nCol))
         oRange:HorizontalAlignment := TbsXlsAlign( DT_CENTER )
         aFontSHF := GetFontParam( aTsb[3,nCol,3])
         oSheet:Cells( nLine, nCol ):Font:Name := aFontSHF[ 1 ]
         oSheet:Cells( nLine, nCol ):Font:Size := aFontSHF[ 2 ]
         oSheet:Cells( nLine, nCol ):Font:Bold := aFontSHF[ 3 ]
      Next
      ++nLine
   Endif

   nCount := 0
   // Печать - СТРОК таблицы ВСЕГДА !
   nIndexaSet := 1
   nStart := nLine
   nBeginTable := nStart

   FOR nRow:= 1 to nTotal //Len(aTsb[4])
      FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
         uData    := aTsb[4,nRow,nColHead,4]
         rType    := aTsb[4,nRow,nColHead,5]
         rPicture := aTsb[4,nRow,nColHead,6]
         do Case
            Case (rType=='@'.or.rType=='D').and.Empty(uData)
               uData := ''
            Case ValType( uData )=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case rType == 'L'
               rType :='C'
            Case rPicture != Nil .and. uData != Nil .and. rType !='N'
              uData := Transform( uData, rPicture )
         endCase
         // определяем тип поля в колонке
         If !(rType = "U") .and. Empty(aTipeChars[nColHead]) .and. !Empty(uData )
             aTipeChars[nColHead] := rType
             cRange :=  HeadXls(nColHead)

             //Тип полей колонок таблицы Excel по типу данных таблицы oBrw
             Do case
                  // при необходимости можно поставить тип для других полей
               Case rType=="D"
                 //для типа поля Дата для русифицированного Excel
                 //oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := "ДД.ММ.ГГГГ"
                 //для типа поля строка, не зависим от множества настроек
                 oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := "@"
               case aTipeChars[nColHead] =='C'.or.aTipeChars[nColHead] =='L'.or.aTipeChars[nColHead] =='='.or.aTipeChars[nColHead] =='@'
                 oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := '@'
                 // oSheet:Range(cRange+LTrim( Str(nBeginTable))+':'+cRange+LTrim( Str(nTotal+nBeginTable-1))):WrapText := .f.
                 oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):ColumnWidth := Number_Characters_String_Cell
               case aTipeChars[nColHead] =='N'.or.aTipeChars[nColHead] =='+'.or.aTipeChars[nColHead] =='^'
                 If Empty(rPicture)
                   rPicture := Transform( uData, rPicture )
                 Endif
                 nPoint   := AT('.', rPicture )
                 if nPoint == 0
                    rPicture :='#0'
                 else
                    rPicture := Repl("#",nPoint-2) + '0,' + Repl("0",Len(rPicture)-nPoint)
                  //  rPicture :="#,##0.00"
                 endif
                 // формат типа := '## ### ###0' или '## ### ###0,00'
              oSheet:Range(cRange+hb_NtoS(nBeginTable)+':'+cRange+hb_NtoS(nTotal+nBeginTable-1)):NumberFormat := rPicture
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

   Next

   // выводим подвал таблицы
   nColHead := 0
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to Len(aTsb[5])
         oSheet:Cells( nLine, nColHead):NumberFormat := '@'
         oSheet:Cells( nLine, nColHead ):Value := aTsb[5,nColHead,4]
         oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
         oSheet:Cells( nLine, nColHead ):Font:Size := aFontSHF[ 2 ]
         oSheet:Cells( nLine, nColHead ):Font:Bold := aFontSHF[ 3 ]
         // aWidthChars [nCol] := max(aWidthChars [nCol], LenStrokaWithCRLF(uData))
      Next
      nLine++
   Endif
   SysRefresh()

   // Шрифт только таблицы с данными
   cRange :=  "A" + HB_NtoS(nBeginTable)+":" + HeadXls(nColDbf) + HB_NtoS(nTotal+nBeginTable-1)
   oRange:=oSheet:Range(cRange)
   // oRange:Borders():LineStyle := xlContinuous
   aFont := GetFontParam( hFont )
   oRange:Font:Name := aFont[ 1 ]
   oRange:Font:Size := aFont[ 2 ]
   oRange:Font:Bold := aFont[ 3 ]

   // создать сетку на таблицу
   cRange :=  "A" + HB_NtoS(nColBegTbl+1)+":" + HeadXls(nColDbf) + HB_NtoS(nLine-1)
   oRange:=oSheet:Range(cRange)
   oRange:Borders():LineStyle := xlContinuous

   oRange:Columns:AutoFit() // автоматически поменять ширину всех столбцов и высоту всех строк
                            // в диапазоне, чтобы туда уместился текст ячеек.
                            // Можно применять только к тем диапазонам, которые состоят из
                            // набора столбцов (полностью) или набора ячеек (также полностью),
                            // иначе будет ошибка !!!

   // Текстовый подвал таблицы
   If !Empty(aXlsFoot)
      For nRow := 1 TO Len(aXlsFoot)
         if Len(aXlsFoot[nRow]) >0
            cTitle := aXlsFoot[nRow,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXlsFoot[nRow,2]),nColDbf,if(aXlsFoot[nRow,2]<0,aXlsFoot[nRow,1],aXlsFoot[nRow,2]))
            oSheet:Cells( nLine, aXlsFoot[nRow,1]):NumberFormat := '@'
            oSheet:Cells( nLine, aXlsFoot[nRow,1]):Value := AllTrim( cTitle )
            cRange :=  HeadXls( aXlsFoot[nRow,1]) + Hb_NtoS( nLine )  + ":" + ;
                       HeadXls( nCol) + Hb_NtoS( nLine )

            oRange := oSheet:Range( cRange )
            If aXlsFoot[nRow,6] != Nil
              oRange:HorizontalAlignment := TbsXlsAlign( aXlsFoot[nRow,6] )
            Else
              oRange:HorizontalAlignment := TbsXlsAlign( DT_CENTER )
            Endif
            oRange:Merge()
            If aXlsFoot[nRow,4] != Nil
              aFont := aXlsFoot[nRow,4]
              aClr  := aXlsFoot[nRow,5]
              oRange:Font:Name := aFont[ 1 ]
              oRange:Font:Size := aFont[ 2 ]
              oRange:Font:Bold := aFont[ 3 ]
              oRange:Font:Color = RGB(aClr[1,1],aClr[1,2],aClr[1,3])
              oRange:Interior:Color := RGB(aClr[2,1],aClr[2,2],aClr[2,3])
            EndIf
         EndIf
         ++nLine
       Next
   EndIf

   // хвост таблицы
   ++nLine
   ++nLine

   // Доп.надпись под таблицей
   cVal := "End table ! - Version (" + oExcel:Version + ") " + ExcelVersion( VAL( oExcel:Version ) )
   cVal += "  Path - " + ExcelPath() + "  +  " + MiniGuiVersion()
   aClr := RED
   oRange := oSheet:Cells( nLine, 1 )
   oRange:Font:Color := RGB(aClr[1],aClr[2],aClr[3])
   oRange:Font:Name  := "Times New Roman"
   oRange:Font:Size  := 16
   oRange:Font:Bold  := .T.
   oRange:Value := cVal
   cRange :=  "A" + HB_NtoS(nLine) + ":" + HeadXls(nColDbf) + HB_NtoS(nLine)
   oRange := oSheet:Range( cRange )
   oRange:Merge()

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   // вызов внешнего блока дообработки таблицы
   If bExtern != Nil
      Eval( bExtern, oSheet, aTsb, aXlsTitle)
   EndIf

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   If ! Empty( cFile ) .and. lSave
      oBook:SaveAs( cFile, xlWorkbookNormal )
   EndIf

   WaitWindow()
   CursorArrow()

   If lActivate
      //А как задать показ листа в масштабе 77% ?
      oExcel:ActiveWindow:Zoom:= 77
      oSheet:Range( "A1" ):Select()   // переход курсора Excel

      // 5. Альтернативный способ - настройка Excel
      lShowZeros = .F.  // не показывать нули
      oExcel:ActiveWindow:DisplayZeros := lShowZeros

      oExcel:Visible := .T.           // показать Excel на экране
      hWnd := oExcel:hWnd             // хендл окна Excel
      ShowWindow( hWnd, 6 )           // MINIMIZE windows
      ShowWindow( hWnd, 3 )           // MAXIMIZE windows
      BringWindowToTop( hWnd )        // a window on the foreground
   Else
      oExcel:Application:Quit()       // закрыть Excel
   EndIf
   DO EVENTS

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
FUNCTION ExcelOle8Extern( hProgress, oSheet, aTsb, aXlsTitle)
   LOCAL cRange, oRange, nCol, nRow, nBColor, nFColor
   LOCAL nCount, nTotal, nEvery, aFont, nColHead
   LOCAL oldnFColor, aRCnFColor[4], oldaFont[3]
   LOCAL oldnBColor, aRCnBColor[4], aRCaFont[4]
   LOCAL aCol, lEndTabl, oFont, nColDbf, nCell
   LOCAL nLine, lTsbFontTable, lTsbFontHeader
   LOCAL aFontSHF, nBeginTable, nColSh1, nColSh2

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
         cRange :=  HeadXls( nColSh1) + Hb_NtoS( nLine )  + ":" + ;
                    HeadXls( nColSh2) + Hb_NtoS( nLine )
         oSheet:Range( cRange ):HorizontalAlignment  := xlHAlignCenterAcrossSelection
         nFColor := myColorFirst(aCol[1])
         nBColor := myColorFirst(aCol[2])
         oRange := oSheet:Range( cRange )
         oFont :=  oRange:Font
         oFont:Color          := nFColor        // Цвет шрифта шапки
         oRange:Interior:Color:= nBColor        // Цвет фона
         If lTsbFontHeader
            aFontSHF := GetFontParam( aCol[3])
            oSheet:Range( cRange ):Font:Name := aFontSHF[ 1 ]
            oSheet:Range( cRange ):Font:Size := aFontSHF[ 2 ]
            oSheet:Range( cRange ):Font:Bold := aFontSHF[ 3 ]
         Endif
      NEXT
      ++nLine
   Endif
     nBeginTable := nLine

   // Выводим шапку таблицы
   If Len(aTsb[2])>0
      nCol :=0
      FOR nColHead:= 1 to Len(aTsb[2])
         nFColor := myColorFirst(aTsb[2,nColHead,1])
         nBColor := myColorFirst(aTsb[2,nColHead,2])
         oRange := oSheet:Cells( nLine, nColHead )
         oFont :=  oRange:Font
         oFont:Color    := nFColor        // Цвет шрифта шапки
         oRange:Interior:Color:= nBColor        // Цвет фона
         If lTsbFontHeader
            aFontSHF := GetFontParam( aTsb[2,nColHead,3])
            oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
            oSheet:Cells( nLine, nColHead ):Font:Size := aFontSHF[ 2 ]
            oSheet:Cells( nLine, nColHead ):Font:Bold := aFontSHF[ 3 ]
         Endif
      Next
      ++ nLine
   Endif

   // Нумератор таблицы
   If Len(aTsb[3])>0
      FOR nCol:= 1 to Len(aTsb[3])
         nFColor := myColorFirst(aTsb[3,nCol,1])
         nBColor := myColorFirst(aTsb[3,nCol,2])
         oRange := oSheet:Cells( nLine, nCol)
         oFont :=  oRange:Font
         oFont:Color    := nFColor        // Цвет шрифта шапки
         oRange:Interior:Color:= nBColor        // Цвет фона
         If lTsbFontHeader
            aFontSHF := GetFontParam( aTsb[3,nCol,3])
            oSheet:Cells( nLine, nCol ):Font:Name := aFontSHF[ 1 ]
            oSheet:Cells( nLine, nCol ):Font:Size := aFontSHF[ 2 ]
            oSheet:Cells( nLine, nCol ):Font:Bold := aFontSHF[ 3 ]
         Endif
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

   cRange :=  "A" + HB_NtoS(nBeginTable)+":" + HeadXls(nColDbf) + HB_NtoS(nLine)
   oRange:=oSheet:Range(cRange)
   oRange:Columns:AutoFit() // автоматически поменять ширину всех столбцов и высоту всех строк
                            // в диапазоне, чтобы туда уместился текст ячеек.
                            // Можно применять только к тем диапазонам, которые состоят из
                            // набора столбцов (полностью) или набора ячеек (также полностью), иначе будет ошибка.

   // выводим цвета фона и текста подвала таблицы
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to Len(aTsb[5])
         nFColor := myColorFirst(aTsb[5,nColHead,1])
         nBColor := myColorFirst(aTsb[5,nColHead,2])
         oRange := oSheet:Cells( nLine, nColHead )
         oFont :=  oRange:Font
         oFont:Color    := nFColor         // Цвет шрифта шапки
         oRange:Interior:Color:= nBColor   // Цвет фона
         If lTsbFontHeader
            aFontSHF := GetFontParam( aTsb[5,nColHead,3])
            oSheet:Cells( nLine, nColHead ):Font:Name := aFontSHF[ 1 ]
            oSheet:Cells( nLine,nColHead ):Font:Size := aFontSHF[ 2 ]
            oSheet:Cells( nLine,nColHead ):Font:Bold := aFontSHF[ 3 ]
         Endif
         // aWidthChars [nCol] := max(aWidthChars [nCol], LenStrokaWithCRLF(uData))
      Next
      nLine++
   Endif

//   cRange :=  "A" + HB_NtoS(nBeginTable)+":" + HeadXls(nColDbf) + HB_NtoS(nLine-1)
//   oRange:=oSheet:Range(cRange)
//   oRange:Columns:AutoFit() // автоматически поменять ширину всех столбцов и высоту всех строк
                            // в диапазоне, чтобы туда уместился текст ячеек.
                            // Можно применять только к тем диапазонам, которые состоят из
                            // набора столбцов (полностью) или набора ячеек (также полностью), иначе будет ошибка.


   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 100, 0 )
   EndIf
   SysRefresh()

   RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeInterior( oSheet,oldnBColor, aRCnBColor, nMaxCol )
LOCAL cRange, nDif := aRCnBColor[3] - aRCnBColor[1]

   Do case
      case nDif == 0
         cRange := HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
                   HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
      case ndif =1
         cRange := HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[1]) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim( Str( aRCnBColor[3]) )+":" + ;
                   HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
      Otherwise
         cRange := HeadXls(aRCnBColor[2]) + LTrim( Str( aRCnBColor[1]) )+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[1]) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim(Str( aRCnBColor[1]+1 ))+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnBColor[3]-1 ) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim( Str( aRCnBColor[3]) )+":" + ;
                   HeadXls(aRCnBColor[4]) + LTrim( Str( aRCnBColor[3]) )
         oSheet:Range(cRange):Interior:Color    := oldnBColor  // Фон шрифта
   Endcase

RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFontColor( oSheet,oldnFColor, aRCnFColor, nMaxCol )
LOCAL cRange, nDif := aRCnFColor[3] - aRCnFColor[1]

   Do case
      case nDif == 0
         cRange := HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
                   HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
      case ndif =1
         cRange := HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[1]) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim( Str( aRCnFColor[3]) )+":" + ;
                   HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
      Otherwise
         cRange := HeadXls(aRCnFColor[2]) + LTrim( Str( aRCnFColor[1]) )+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[1]) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim(Str( aRCnFColor[1]+1 ))+":" + ;
                   HeadXls(nMaxCol) + LTrim( Str( aRCnFColor[3]-1 ) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
         cRange := HeadXls(1) + LTrim( Str( aRCnFColor[3]) )+":" + ;
                   HeadXls(aRCnFColor[4]) + LTrim( Str( aRCnFColor[3]) )
         oSheet:Range(cRange):Font:Color    := oldnFColor  // Фон шрифта
   Endcase

RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFont( oSheet, oldaFont, aRCaFont, nMaxCol)
LOCAL cRange, oFont, nDif := aRCaFont[3] - aRCaFont[1]

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
STATIC FUNCTION myColorFirst(nColor)
   If Valtype( nColor ) == "A"
      nColor := nColor[1]
   EndIf
Return nColor

////////////////////////////////////////////////////////////
STATIC FUNCTION HeadXls(nCol)
RETURN IF(nCol>26,Chr(Int((nCol-1)/26)+64),'')+CHR((nCol-1)%26+65)

/////////////////////////////////////////////////////////////
// Функция проверки версии Excel
STATIC FUNCTION ExcelVersion(nVer)
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

/////////////////////////////////////////////////////////////////
// Функция пути к Excel
// http://clipper.borda.ru/?1-20-0-00000371-000-0-0-1195742832
// Pasha - Пост N: 645
STATIC FUNCTION ExcelPath()
   LOCAL cPath := NIL
   cPath := win_regRead( "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\excel.exe\Path" )
   Return cPath

/////////////////////////////////////////////////////////////////
STATIC Function PixelToPointX(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIX()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

/////////////////////////////////////////////////////////////////
STATIC Function PixelToPointY(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIY()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

HB_FUNC( GETDPIX )
{
   HDC    hDC = GetDC( GetDesktopWindow() );
   hb_retni( ( LONG ) GetDeviceCaps(hDC, LOGPIXELSX) );
   return;
}
HB_FUNC( GETDPIY )
{
   HDC    hDC = GetDC( GetDesktopWindow() );
   hb_retni( ( LONG ) GetDeviceCaps(hDC, LOGPIXELSY) );
   return;
}

#pragma ENDDUMP

/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Использование вспомогательного класса TSBcell для быстрого экспорта данных.
 * Using the auxiliary TSBcell class for quick data export.
 */

//#define _HMG_OUTLOG
//#include "hmg.ch"
//#include "TSBrowse.ch"
* ======================================================================
FUNCTION myGetTsbContent(oBrw)     // Содержание таблицы
   LOCAL aTsb, aTsbEnum, aTsbFoot, aTsbHead, aTsbSupH, aTsbCell

   // нет вывода скрытых колонок
   aTsbEnum := myGetTsbEnum(oBrw)  // массив цвет/фонт/номер нумератора таблицы
   aTsbFoot := myGetTsbFoot(oBrw)  // массив цвет/фонт подвала таблицы
   aTsbHead := myGetTsbHead(oBrw)  // массив цвет/фонт шапки таблицы
   aTsbSupH := myGetTsbSupH(oBrw)  // массив цвет/фонт суперхидера таблицы
   aTsbCell := myGetTsbCell(oBrw)  // массив цвет/фонт ячеек таблицы
   aTsb     := { aTsbSupH, aTsbHead, aTsbEnum, aTsbCell, aTsbFoot }

   RETURN aTsb

* =======================================================================================
// массив цвет/фонт суперхидера таблицы
FUNCTION myGetTsbSupH(oBrw)
   LOCAL aRet, oCel, xVal, aFore, aBack, hFnt, aSup, nFrom, nTo
   LOCAL nCol, oCol, nEnd

   aRet := {}
   aSup := oBrw:DrawSuper( .F. )
   FOR EACH oCel IN aSup
      hFnt  := oCel:hFont
      aFore := oCel:nClrFore
      aBack := oCel:nClrBack
      xVal  := oCel:cValue
      //   - added the new variables :nFromCol, :nToCol in the class TSBcell.
      nFrom := oCel:nFromCol
      nTo   := oCel:nToCol
      //? oCel

      IF nFrom > 0 .AND. nTo > 0

         nEnd := 0

         FOR nCol := nFrom TO nTo
            ?? "=", nFrom, nTo
            oCol := oBrw:aColumns[ nCol ]
            If nCol == 1 .and. oBrw:lSelector ; LOOP
            ElseIf ! oCol:lVisible            ; LOOP
            ElseIf oCol:lBitMap               ; LOOP
            EndIf
            // ... обрабатываем тут видимые колонки
            nEnd := nCol
            //?? "nEnd=", nEnd
         NEXT
         nTo := nEnd

         IF nEnd > 0
            AADD( aRet, { aFore, aBack, hFnt, xVal, nFrom, nTo } )
         ENDIF

      ENDIF


   NEXT
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )
//? "------- проверка -----------"
//? "aRet="     ,aRet      ; ?v aRet      ; ?

RETURN aRet

* =======================================================================================
// массив цвет/фонт шапки таблицы
FUNCTION myGetTsbHead(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // он создает для Header, SpcHd, Footer, Enum

   IF oBrw:lDrawHeaders
      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellHead
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         IF oCel:lMultiLine
            //xVal := StrTran(xVal, CRLF, " ") // не надо
         ENDIF
         AADD( aRet, { aFore, aBack, hFnt, xVal } )
      NEXT
   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

RETURN aRet

* =======================================================================================
// массив цвет/фонт/номер нумератора таблицы
FUNCTION myGetTsbEnum(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt, lCol

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // создает для Header, SpcHd, Footer, Enum

   IF oBrw:lDrawSpecHd

      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellEnum
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         lCol  := oCol:lVisible
         IF xVal == ""
            xVal := HB_NtoS(nCol)
         ENDIF
         AADD( aRet, { aFore, aBack, hFnt, xVal } )

      NEXT

   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

RETURN aRet

* =======================================================================================
// массив фонт/цвет_текст/цвет_фона/значение/тип/формат/имя_поля ячеек таблицы
FUNCTION myGetTsbCell(oBrw)
   LOCAL aRet, aLine, nAt, nCol, oCol, oCel, xVal, aFore, aBack
   LOCAL cName, hFnt, cType, cPict, lCol

   aRet := {}

   WITH OBJECT oBrw
   :GoTop()
   :lDrawLine := .F.
   :GoTop()

   FOR nAt := 1 TO :nLen
      :DrawLine()
      aLine := {}
      FOR nCol := 1 TO :nColCount()
         oCol := :aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel := oCol:oCell
         hFnt := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         if Valtype(oCel:uValue)='L'
            IF Hb_LangSelect() == "ru.RU1251" ; xVal := iif(oCel:uValue,'да','нет')
            ELSE                              ; xVal := iif(oCel:uValue,'yes','no')
            ENDIF
            //xVal  := if(oCel:uValue,'[+]','[ ]')
            cPict := 'XXX'
         else
            //xVal  := oCel:cValue - так нельзя ! переводит в текстовый формат
            xVal  := oCel:uValue
            cPict := oCol:cPicture
         endif
         //cType := Valtype(oCel:uValue) - не так
         cType := oCol:cFieldTyp
         cPict := oCol:cPicture
         cName := oCol:cName
         lCol  := oCol:lVisible
         AADD( aLine, { aFore, aBack, hFnt, xVal, cType, cPict, cName } )
         DO EVENTS

      NEXT
      AADD( aRet, aLine )  // строка таблицы
      :GoDown()
      DO EVENTS
   NEXT

   :lDrawLine := .T.
   :Reset()
   // освобождаем переменные (память)
   AEval( :aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

   END WITH

RETURN aRet

* =======================================================================================
// массив цвет/фонт подвала таблицы
FUNCTION myGetTsbFoot(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt, lCol

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // он создает для Header, SpcHd, Footer, Enum
   IF oBrw:lDrawFooters

      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellFoot
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         lCol  := oCol:lVisible
         AADD( aRet, { aFore, aBack, hFnt, xVal } )

      NEXT

   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )
RETURN aRet

* ======================================================================
FUNCTION myImageReport(cRes)     // для картинки
   LOCAl aImage, cFileLogo, aXY, cMsg, nResult, cFile
   Default cRes := "LogoMG"

   cFile  := cRes + ".png"
   aImage := {}  // нет картинки !  файл лого для экспорта !

   //cFileLogo := GetStartUpFolder() + "\LogoMG.png"
   cFileLogo := GetUserTempFolder() + "\" + cFile
   If !hb_FileExists( cFileLogo )
      nResult := RCDataToFile( cRes, cFileLogo, "PNG" )
      If nResult > 0
      Else
        MsgStop( "RCDataToFile() - Code: " + hb_NtoS( nResult ), "Error" )
      Endif
   Endif
   If hb_FileExists( cFileLogo )
      aXY  := hb_GetImageSize( cFileLogo )
      cMsg := ( cFileLogo + ": " + hb_NtoS( aXY[1] ) + " x " + hb_NtoS( aXY[2] ) + " Pixels" )
      //MsgInfo( cMsg, "Info!" )
      aImage := { cFileLogo, aXY[1], aXY[2] }
   Endif

   RETURN aImage

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FUNCTION myMultiHeadStr(cStr)
   LOCAL l := .T.
   IF Left(cStr, 2) == 'e"' .AND. Right(cStr, 1) == '"'
      l := .F.
      BEGIN SEQUENCE WITH { |e|break(e) }
         cStr := &(cStr)
         l := .T.
      END SEQUENCE
   ENDIF
   IF     CRLF    $ cStr ; cStr := StrTran(cStr, CRLF   , " ")
   ELSEIF chr(13) $ cStr ; cStr := StrTran(cStr, chr(13), "|")
   ELSEIF chr(10) $ cStr ; cStr := StrTran(cStr, chr(10), "|")
   ENDIF
   cStr := StrTran(cStr, "|" , CRLF)
   IF !l ; ? "***Error =", cStr
   ENDIF
RETURN cStr

