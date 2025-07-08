/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2020 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
 *
*/
#define _HMG_OUTLOG
#define PBM_SETPOS          1026   // Устанавливает текущую позицию для индикатора выполнения и перерисовывает полосу, чтобы отразить новую позицию
#define TYPE_EXCEL_FORMULA  '#'    // мой тип ФОРМУЛА для экселя

#include "hmg.ch"
#include "excel.ch"
* ======================================================================
// вызов допольнительного внешнего блока дообработки таблицы по bExtern2
FUNCTION myTuningExternExcel( hProgress, oSheet, oExcel)
   LOCAL aParams := hb_aParams(), nParams := PCount()
   LOCAL nTotal, nRow, nColHead, oRange, nRowXls, nI, cMsg
   LOCAL uValue, aFClr, aBClr, nXlsRow, nXlsColumn, xVal
   LOCAL nRowLine1, nRowRecno, nRowFoot, nLinef, oBook

   ? "----" + ProcNL()
   ? "nParams=", nParams
   ? "aParams=", aParams
   //?v aParams

   oExcel:Visible        := .F.      // .T. показать Excel на экране для отладки
   oExcel:DisplayAlerts  := .F.      // убрать предупреждения Excel
   oExcel:ReferenceStyle := xlR1C1   // стиль таблицы колонок - цифры
   oExcel:ActiveWindow:Zoom:= 75     // % задать показ листа в масштабе
   oExcel:UserName       := cFileNoPath( App.ExeName, "" )
                                                                     // Название свойства
   //oExcel:ActiveWorkbook:BuiltinDocumentProperties:Item(1) := "Harbour + MiniGui + Excel"     // заголовок
   //oExcel:ActiveWorkbook:BuiltinDocumentProperties:Item(3) := cFileNoPath( App.ExeName, "" )  // Автор (создавший документ)

   oRange     := oExcel:ActiveCell:SpecialCells( xlLastCell )
   nXlsRow    := oRange:Row                      // количество строк таблицы Excel
   nXlsColumn := oRange:Column                   // количество колонок Excel - НЕ всегда РАБОТАЕТ НОРМАЛЬНО !
   nXlsColumn := oSheet:UsedRange:Columns:Count  // количество колонок Excel - РАБОТАЕТ

   ?  "XLS= row/col:", nXlsRow, nXlsColumn

   nRowLine1 := nRowRecno := 0
   FOR nRow := 1 to nXlsRow                 // по строкам таблицы
      xVal := oSheet:Cells( nRow, 1 ):Value
      IF xVal == NIL
      ELSE
         IF VALTYPE(xVal) == "N"
            IF nRowLine1 == 0
               nRowLine1 := nRow
            ENDIF
         ELSE
            xVal := VAL(xVal)
         ENDIF
         IF xVal > 0
            nRowRecno ++
         ENDIF
      ENDIF
   NEXT
   // нашли первую и последнюю строку таблицы и подвал ИТОГО по столбцам
   nRowRecno -= 1             // число в строке итого
   nRowFoot := nRowRecno - 1  // подвал таблицы итого
   ? "  нашли первую и последнюю строку таблицы и подвал ИТОГО по столбцам"
   ? "  =",nRowLine1, nRowRecno, nRowFoot

   // проход по таблице и по заданным столбцам
   // все значения с суммой < 0 пометить цветом
   aFClr := WHITE   // цвет текста ячейки
   aBClr := PURPLE  // цвет фона ячейки
   nLinef := nRowLine1
   nTotal := nRowFoot
   FOR nRow := 1 to nTotal                     // по строкам таблицы
      //FOR nColHead := 1 to nColDbf           // по всем столбцам таблицы
      FOR nColHead := 31 to 32                 // по заданным столбцам таблицы
         // только для колонок - формула
          uValue := oSheet:Cells( nLinef, nColHead):Value
          // все значения с суммой < 0 пометить заданным цветом и фоном
          if !Empty(uValue) .and. oSheet:Cells( nLinef, nColHead):Value < 0
              oSheet:Cells( nLinef, nColHead):Font:Color     := RGB(aFClr[1],aFClr[2],aFClr[3])
              oSheet:Cells( nLinef, nColHead):Interior:Color := RGB(aBClr[1],aBClr[2],aBClr[3])
          endif
      NEXT
      If hProgress != Nil
         SendMessage(hProgress, PBM_SETPOS,nLinef,0)
      EndIf
      nLinef++
      DO EVENTS
   NEXT

   nRowXls := nRowFoot + nRowLine1  // подвал таблицы в экселе
   FOR nI := 31 TO 32    // ставим формулу для ИТОГО
      oSheet:Cells[ nI, nRowXls ]:NumberFormat        := '' // очистка формата - пустой формат
      oSheet:Cells[ nI, nRowXls ]:Font:ColorIndex     := 3  // шрифт красный
      oSheet:Cells[ nI, nRowXls ]:Font:Bold           := .T.
      oSheet:Cells[ nI, nRowXls ]:Borders():LineStyle := 1
      // формула для ячейки таблицы
      oSheet:Cells[ nI, nRowXls ]:Formula := "=СУММ(R[-" + HB_NtoS(nRowXls + 1 - nTotal) + "]C:R[-1]C)"
   NEXT

   nLinef := nRowLine1
   FOR nRow := 1 to nTotal                     // по строкам таблицы
      // проход по таблице и по 3 столбцу таблицы
      // все значения содержащие "Dmitrov" пометить цветом
      // пометить всю строку
      aFClr := PURPLE           // цвет текста ячейки
      aBClr := SILVER           // цвет фона ячейки
      nColHead := 3             // поиск по 3 столбцу
      uValue := oSheet:Cells( nLinef, nColHead):Value
      if At("Dmitrov",uValue)>0
        // Метим всю строку
        oRange:=oSheet:Range(osheet:cells(nLinef,1),osheet:cells(nLinef,nXlsColumn))
        oRange:Font:Color     := RGB(aFClr[1],aFClr[2],aFClr[3])
        oRange:Interior:Color := RGB(aBClr[1],aBClr[2],aBClr[3])
        // Если только по одной колонке
        //        oSheet:Cells( nLinef, nColHead):Font:Color     := RGB(aFClr[1],aFClr[2],aFClr[3])
        //        oSheet:Cells( nLinef, nColHead):Interior:Color := RGB(aBClr[1],aBClr[2],aBClr[3])
      endif
      If hProgress != Nil
         SendMessage(hProgress, PBM_SETPOS,nLinef,0)
      EndIf
      nLinef++
      DO EVENTS
   NEXT
//------------------------------------------------------------
   WorkingOtherSheets(oExcel)    // работа с листами 2 и 3

   WorkingFormulaSheets(oExcel)  // работа с формулами - лист 2
//------------------------------------------------------------
   // параметры печати таблицы
   cMsg := UPPER(oExcel:ActivePrinter)
   IF AT("НЕИЗВЕСТНЫЙ", cMsg ) > 0
      // если нет принтеров  в системе
      AlertStop("Неизвестный принтер !;Проверте Панель Управления !;", "Ошибка при печати")
   ELSE
      oExcel:ActiveSheet:PageSetup:Zoom := FALSE
      oExcel:ActiveSheet:PageSetup:FitToPagesWide := 1
      oExcel:ActiveSheet:PageSetup:FitToPagesTall := 10
      oExcel:ActiveSheet:PageSetup:Orientation := xlLandscape // страница - Портрет
   ENDIF

   oBook   := oExcel:ActiveWorkBook
   oSheet  := oBook:Sheets("My_Table"):Select()  // вернутся на 1 лист

   RETURN NIL

* ======================================================================
// Работа с другими листами Экселя
FUNCTION WorkingOtherSheets(oExcel)
   LOCAL oBook, oSheet, oSheets, nCnt, cMsg, xVal, oRange, nXlsRow, nXlsColumn, nI

   ? "----" + ProcNL()

   oExcel:Visible  := .T. //показать Excel на экране для отладки

   oBook   := oExcel:ActiveWorkBook
   oSheets := oBook:Sheets

   // проверка листов экселя
   nCnt    := oSheets:Count   // количество листов в книге экселя
   IF nCnt == 1
      oSheets:Add() // 3
      oSheets:Add() // 2
   ELSEIF nCnt == 2
      oSheets:Add()
   ENDIF

   oSheet := oBook:Sheets("Лист1")   // перейдём на 1 лист
   oSheet:name := "My_Table"         // имя листа изменить
   oSheet := oBook:Sheets("Лист2")   // перейдём на 2 лист
   oSheet:name := "My_Report"        // имя листа изменить
   oSheet := oBook:Sheets("Лист3")   // перейдём на 2 лист
   oSheet:name := "Other"            // имя листа изменить
   oSheet:Visible := .F.             // скрыть лист 3 в экселе

   nCnt := oBook:Sheets:Count   // количество листов в книге экселя
   cMsg := "Number of sheets in a book = " + HB_NtoS(nCnt) + ";"

   FOR EACH oSheet IN oBook:WorkSheets
      cMsg += oSheet:Name + ";"
   NEXT
   //AlertInfo(cMsg)
   cMsg := ATREPL( ";", cMsg, CRLF )
   ? cMsg

   oSheet := oBook:Sheets("My_Report")  // перейдём на 2 лист
   oSheet:Cells:Font:Name := "Arial"
   oSheet:Cells:Font:Size := 12

   oSheet:Cells( 1, 1 ):Value := "Progr:"
   oSheet:Cells( 2, 1 ):Value := "Avtor:"
   oSheet:Cells( 3, 1 ):Value := "Lib:"
   oSheet:Cells( 4, 1 ):Value := "Lang:"

   oSheet:Columns( 1 ):Font:Bold := .T.
   oSheet:Columns( 1 ):AutoFit()
   oSheet:Columns( 2 ):AutoFit()

   oSheet:Cells( 1, 2 ):Value     := App.ExeName
   oSheet:Cells( 2, 2 ):Value     := "(c) 2020 Verchenko Andrey. Dmitrov, Moscow region"
   oSheet:Cells( 1, 2 ):Font:Size := 14
   oSheet:Cells( 2, 2 ):Font:Size := 14
   oSheet:Cells( 3, 2 ):Value     := MiniGuiVersion()
   oSheet:Cells( 4, 2 ):Value     := Version()

   oSheet     := oBook:Sheets("My_Table")        // перейдём на лист
   oRange     := oExcel:ActiveCell:SpecialCells( xlLastCell )
   nXlsRow    := oRange:Row                      // количество строк таблицы Excel
   nXlsColumn := oSheet:UsedRange:Columns:Count  // количество колонок Excel - РАБОТАЕТ
   ?  "XLS= row/col:", nXlsRow, nXlsColumn

   // считать данные с 1-го листа экселя
   xVal := oSheet:Cells( 1, 1 ):Value
   ? "  Read data from Excel - Cell(1,1)=", VALTYPE(xVal) , xVal ; ? // прочитать ТИТУЛ формы

   xVal := oSheet:Cells( 11, 3 ):Value
   ? "  Read data from Excel - Cell(11,3)=", VALTYPE(xVal) , xVal ; ?

   // можно и так
   xVal := oSheet:Range("R11:C3"):Value
   ? "  Read data from Excel - Cell(R11:C3)=", VALTYPE(xVal) , xVal ; ?
   ?v xVal

   ? "------"
   ? " Row 11:"
   FOR nI := 1 TO nXlsColumn
       xVal := oSheet:Cells( 11, nI ):Value
       ? "       .", nI, VALTYPE(xVal) , xVal
   NEXT

RETURN NIL

* ======================================================================
// Работа с с формулами - лист 2
FUNCTION WorkingFormulaSheets(oExcel)
   LOCAL oBook, oSheet

   oBook  := oExcel:ActiveWorkBook
   oSheet := oBook:Sheets("My_Report")       // перейдём на 2 лист

   // задание фонта и цвета для всей страницы
   //oSheet:Cells:Font:Name       := "Arial Black"
   //oSheet:Cells:Font:Size       := 12
   //oSheet:Cells:Font:ColorIndex := 6
   //oSheet:Cells:Font:Bold       := .T.

   oSheet:Cells( 6, 2 ):Value               := "Summa:"
   oSheet:Cells( 6, 3 ):Value               := 123456.1234
   oSheet:Cells( 6, 3 ):NumberFormat        := '# ##0,0000'
   oSheet:Cells( 6, 3 ):Font:ColorIndex     := 3
   oSheet:Cells( 6, 3 ):Borders():LineStyle := 1

   oSheet:Cells( 7, 2 ):Value := "10 % ="
   oSheet:Cells( 7, 3 ):NumberFormat        := '' // очистка формата - пустой формат
   oSheet:Cells( 7, 3 ):Font:ColorIndex     := 5
   oSheet:Cells( 7, 3 ):Borders():LineStyle := 1
   oSheet:Cells( 7, 3 ):Value               := "=R[-1]C/10"        // формула для ячейки таблицы

   oSheet:Cells( 8, 2 ):Value := "28 % ="
   oSheet:Cells( 8, 3 ):NumberFormat        := '' // очистка формата - пустой формат
   oSheet:Cells( 8, 3 ):Font:ColorIndex     := 4
   oSheet:Cells( 8, 3 ):Borders():LineStyle := 1
   oSheet:Cells( 8, 3 ):Value               := "=R[-2]C*(28/100)"  // формула для ячейки таблицы

   oSheet := oBook:Sheets("My_Table")  // вернутся на лист
   //oSheets:Item(1):Select()          
   //oBook:Sheets(1):Select()          

   RETURN NIL
