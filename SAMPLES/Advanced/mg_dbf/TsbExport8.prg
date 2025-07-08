/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Экспорт таблицы в Excel, Open Office в файлы: xls/ods
 * Использование вспомогательного класса TSBcell для быстрого экспорта данных.
 * Export Excel, Open Office spreadsheets to files: xls/ods
 * Using the auxiliary TSBcell class for quick data export.
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "TSBrowse.ch"
* ========================================================================
FUNCTION ToExcel8(oBrw, nView, cFile)
   LOCAL hProgress, tTime, bExternXls, aTsb, aXlsParam, aXlsTitle, aImage
   LOCAL nRecno, aXlsFoot, bExtern2

   nRecno := (oBrw:cAlias)->( RecNo() )
   oBrw:GoTop()  // Экспорт идёт с текущей позиции курсора
   DO EVENTS

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle            // хенд для ProgressBar на другой форме
   aTsb       := myGetTsbContent(oBrw)               // содержание таблицы
   aXlsParam  := myExcelParam(oBrw, cFile)           // параметры для экселя
   aXlsTitle  := myReportTitle(nView,"XLS","EXCEL")  // заголовок экселя
   aXlsFoot   := myReportFoot(nView,aTsb)            // подвал экселя
   aImage     := myImageReport()                     // картинка

   // Экспорт значений таблицы в массив идёт с первой позиции таблицы
   // принцип экспорта - что на экране в таблице, то и будет в экселе
   // плюс обработка в функции-окончания экселя (bExtern2) если нужно

/* ? "------- проверка/check -----------"
? "aTsb="     ,aTsb      ; ?v aTsb      ; ?
? "aXlsParam=",aXlsParam ; ?v aXlsParam ; ?
? "aXlsTitle=",aXlsTitle ; ?v aXlsTitle ; ?
? "aXlsFoot=" ,aXlsFoot  ; ?v aXlsFoot  ; ?
? "aImage="   ,aImage    ; ?v aImage    ; ? */

   IF nView == 1
      bExternXls := nil   // подключение внешнего блока для оформления oSheet
      aImage     := nil   // не нужна картинка
      bExtern2   := nil   // не нужна здесь
   ELSEIF nView == 2
      // Смотреть -> Tsb7xlsOle.prg
      bExternXls := {|oSheet,aTsb,aXlsTitle| ExcelOle7Extern( hProgress, oSheet, aTsb, aXlsTitle) }
      bExtern2   := nil   // не нужна здесь
   ELSEIF nView == 3
      // функции окончательной обработки экселя -> TsbXlsTuning.prg
      // подключение внешнего блока для оформления oSheet
      bExternXls := {|oSheet,aTsb,aXlsTitle| ExcelOle7Extern( hProgress, oSheet, aTsb, aXlsTitle) }
      // подключение внешнего блока для дополнительного оформления oExcel
      bExtern2   := {|oSheet,oExcel,aTsb,nLinecolor| myTuningExternExcel( hProgress, oSheet, oExcel, aTsb, nLinecolor) }
   ENDIF

   // сам экспорт в Эксель -> Tsb7xlsOle.prg
   Brw7XlsOle( aTsb, aXlsParam, aXlsTitle, aXlsFoot, aImage, hProgress, bExternXls, bExtern2 )
   TotalTimeExports("Brw7XlsOle(" + HB_NtoS(nView) + ")=", aXlsParam[1], tTime )

   oBrw:Refresh(.T.)
   oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

   RETURN Nil

* ======================================================================
// заголовок отчёта эксель/ворд/калс/хмл
FUNCTION myReportTitle(nView,cPrg,cName)
   LOCAL aTitle, cTitle, aFont, aColor, n1, n2, nG, lRus, ao, cRus, cEng
   DEFAULT cPrg := cName := ""

   IF cPrg == "WORD"   ; nG := 6
   ELSE                ; nG := 0
   ENDIF

   ao     := App.Cargo
   lRus   := iif( ao:cLang == "RU", .T., .F. )
   cRus   := "Выгрузка DBF-файла в " + cPrg
   cEng   := "Download DBF file to " + cPrg
   aTitle := {}
   cTitle := iif(lRus, cRus, cEng)
   aFont  := { "Comic Sans MS", 24 - nG, .f. , .f. }
   aColor := IIF(nView==1,{BLACK,WHITE},{RED,YELLOW})  // цвет/фон ячеек
   n1     := 1                                         // начало строки
   n2     := 0                                         // 0-объединить строку до конца таблицы
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {} )  // разделительная строка

   cRus   := "через OLE-объект " + cName
   cEng   := "via " + cName + " OLE object"
   cTitle := iif(lRus, cRus, cEng)
   aFont  := { "Times New Roman", 20 - nG, .T. , .f. }
   aColor := { BLACK , SILVER }                    // цвет/фон подписи
   n1     := 1                                     // начало строки
   n2     := 0                                     // 0-объединить строку до конца таблицы
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_RIGHT  } )
   AADD( aTitle, {} )  // разделительная строка

   /* IF nView == 2  // для цветного экселя

      aFont  := { "DejaVu Sans Mono", 14 - nG, .f. , .f. }
      n1     := 2     // начало строки
      n2     := 4     // объединить строку
      AADD( aTitle, { n1,n2,"Cell color from 91% and more", aFont, {BLACK,HMG_n2RGB(CLR_GREEN) }, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color from 76% to 91%"  , aFont, {BLACK,HMG_n2RGB(CLR_YELLOW)}, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color 51% to 76%"       , aFont, {BLACK,HMG_n2RGB(RGB(0,176,240)) }, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color less than 51%"    , aFont, {BLACK,HMG_n2RGB(CLR_HRED)  }, DT_LEFT } )
      AADD( aTitle, {} )  // разделительная строка

      n1 := 2 ; n2 := 8
      AADD( aTitle, { n1,n2,"Cell color if there is no debt for the second month", aFont, {BLUE,HMG_n2RGB(RGB(0,255,0))}, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color, if there is a debt for the second month", aFont, {BLUE,HMG_n2RGB(CLR_ORANGE)  }, DT_LEFT } )
      AADD( aTitle, {} )  // разделительная строка

   ENDIF */

   RETURN aTitle

* ======================================================================
// подвал экселя/ворда/калка
FUNCTION myReportFoot(nView,aTsb,cPrg)
   LOCAL aFoot, cFoot, aFont, aColor, n1, n2, nG
   LOCAL nI, aTsbFoot, aTsbHead
   DEFAULT nView := 1, cPrg := ""

   IF cPrg == "WORD"   ; nG := 6
   ELSE                ; nG := 0
   ENDIF

   aTsbHead := aTsb[2]    // массив цвет/фонт шапки таблицы
   aTsbFoot := aTsb[5]    // массив цвет/фонт подвала таблицы
   aFoot := {}
   AADD( aFoot, {} )   // разделительная строка
   AADD( aFoot, {} )   // разделительная строка

   cFoot    := aTsbFoot[3,4]
   aFont    := { "Comic Sans MS", 16 - nG, .T. , .f. }
   aColor   := { BLACK , WHITE }                     // цвет/фон ячеек
   n1       := 3                                     // начало строки
   n2       := 5                                     // объединить строку до
   //AADD( aFoot, {n1,n2, cFoot, aFont, aColor, DT_LEFT } )
   AADD( aFoot, {} )  // разделительная строка

   nI := 0
   /*  ----- резерв --------
   FOR nI := 7 TO 9
      cFoot  := "Total - " + StrTran(aTsbHead[nI,4], CRLF, " ") + ": " + aTsbFoot[nI,4]
      aFont  := { "Comic Sans MS", 16 - nG, .T.  , .f. }
      aColor := { BLACK ,  WHITE }                    // цвет/фон подписи
      n1     := 3                                     // начало строки
      n2     := 9                                     // объединить строку до
      AADD( aFoot, {n1,n2, cFoot, aFont, aColor, DT_LEFT } )
   NEXT
   AADD( aFoot, {} )  // разделительная строка

   cFoot := "The head of the calving" + SPACE(50) + "/Petrov I.I./"
   aFont  := { "Arial Black", 16 - nG, .T. , .T. }
   aColor := { BLACK ,  WHITE }
   AADD( aFoot, {2,-1, cFoot, aFont, aColor, DT_LEFT } )
   */
   /* IF nView == 2  // для цветного экселя

      AADD( aFoot, {} )  // разделительная строка
      aFont := { "DejaVu Sans Mono", 14 - nG, .f. , .f. }
      n1    := 2     // начало строки
      n2    := 8     // объединить строку до
      AADD( aFoot, { n1,n2,"Test color foot - Cell color from 91% and more", aFont, {BLACK,HMG_n2RGB(CLR_GREEN) }, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color from 76% to 91%"  , aFont, {BLACK,HMG_n2RGB(CLR_YELLOW)}, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color 51% to 76%"       , aFont, {BLACK,HMG_n2RGB(RGB(0,176,240)) }, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color less than 51%"    , aFont, {BLACK,HMG_n2RGB(CLR_HRED)  }, DT_LEFT } )
      AADD( aFoot, {} )  // разделительная строка

   ENDIF */

   RETURN aFoot

* ======================================================================
STATIC FUNCTION myExcelParam(oBrw,cFile)
   LOCAL cPath, cXlsFile, aXlsFont, lActivate, lSave, cMaska, cMsg, cNFile
   LOCAL nWidthTsb
   DEFAULT cFile := ""
   cPath     := GetStartUpFolder() + "\"        // путь записи файла
   cMaska    := "zTest_7XlsOle"                 // шаблон файла
   cXlsFile  := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".xls"
   cXlsFile  := IIF( LEN(cFile) == 0, cXlsFile, cFile )
   cNFile    := hb_FNameName(cXlsFile)          // убираем .xls - не надо
   cNFile    := CharRepl('.',cNFile,"_")        // '.'  - нельзя
   cNFile    += ".xls"
   cXlsFile  := cPath + cNFile
   cXlsFile  := GetFileNameMaskNum(cXlsFile)    // получить новое имя файла
   cNFile    := hb_FNameName(cXlsFile)          // убираем .xls
   cXlsFile  := cPath + cNFile
   lActivate := .T.                             // открыть Excel
   lSave     := .T.                             // сохранить файл
   nWidthTsb := oBrw:GetAllColsWidth()          // ширина всех колонок таблицы (пикселы)
   aXlsFont  := {"DejaVu Sans Mono", 9 }        // задать фонт таблицы для Excel
                                                // для черно-белого варианта
                                                // для цветного варианта фонт берется
                                                // с ячеек таблицы
   // Проверить имя файла на количества точек
   // В случае наличия нескольких точек в имени файла Excel может "отрезать" имя файла
   IF AtNum( ".", HB_FNameName( cXlsFile ) ) > 0
      cMsg := ProcNL(0) + ';' + ProcNL(1) + ';;'
      cMsg += 'Output File Name - "' + HB_FNameName( cXlsFile ) + '";'
      cMsg += 'contains several signs dot !;'
      cMsg += 'Excel can "truncate" the file name !;;'
      cMsg := AtRepl( ";", cMsg, CRLF )
      AlertStop( cMsg, "Error", "ZZZ_B_STOP64", 64, {RED} )
   ENDIF

   RETURN { cXlsFile, lActivate, lSave, aXlsFont }

* ======================================================================
FUNCTION ToCalc8(oBrw, nView, cFile)
   LOCAL hProgress, tTime, bExternCalc, aTsb, aCalcParam, aCalcTitle, aImage
   LOCAL nRecno, aCalcFoot

   nRecno := (oBrw:cAlias)->( RecNo() )
   oBrw:GoTop()  // Экспорт идёт с текущей позиции курсора
   DO EVENTS
   // скрыть колонки из списка колонок c формулами экселя
   oBrw:HideColumns( 31, .t.)
   oBrw:HideColumns( 32, .t.)

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle             // хенд для ProgressBar на другой форме
   aTsb       := myGetTsbContent(oBrw)                // содержание таблицы
   aCalcParam := myCalcParam(cFile)                   // параметры для Calc
   aCalcTitle := myReportTitle(nView,"ODS","CALC")    // заголовок Calc
   aCalcFoot  := myReportFoot(nView,aTsb)             // подвал Calc
   aImage     := myImageReport()                      // картинка

? "+++++++++++++++"
? HB_ValToExp(aCalcTitle)
?v  aCalcTitle
? "+++++++++++++++"
? HB_ValToExp(aCalcFoot)
?v aCalcFoot
? "+++++++++++++++"

   // Экспорт значений таблицы в массив идёт с первой позиции таблицы
   // принцип экспорта - что на экране в таблице, то и будет в экселе
/*
? "------- проверка/check -----------" + ProcNL()
? "aTsb="     ,aTsb      ; ?v aTsb      ; ?
? "aCalcParam=",aCalcParam ; ?v aCalcParam ; ?
? "aCalcTitle=",aCalcTitle ; ?v aCalcTitle ; ?
? "aCalcFoot=" ,aCalcFoot  ; ?v aCalcFoot  ; ?
? "aImage="   ,aImage    ; ?v aImage    ; ?
*/
   IF nView == 1
      bExternCalc := nil   // подключение внешнего блока для оформления oSheet
      aImage      := nil   // не нужна картинка
   ELSEIF nView == 2
      bExternCalc := {|oSheet,aTsb,aCalcTitle| CalcOle7Extern( hProgress, oSheet, aTsb, aCalcTitle) }
   ENDIF

   Brw7OleCalc( aTsb, aCalcParam, aCalcTitle, aCalcFoot, aImage, hProgress, bExternCalc )
   TotalTimeExports("Brw7OleCalc(" + HB_NtoS(nView) + ")=", aCalcParam[1], tTime )

   // восстановить колонки из списка колонок
   oBrw:HideColumns( 31, .f.)
   oBrw:HideColumns( 32, .f.)

   oBrw:Refresh(.T.)
   oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

   RETURN Nil

* ======================================================================
STATIC FUNCTION myCalcParam(cFileNew)
   LOCAL cPath, cFile, aFont, lActivate, lSave, cMaska
   DEFAULT cFileNew := ""

   cPath     := GetStartUpFolder() + "\"        // путь записи файла
   cMaska    := "zTest_7Calc"                   // шаблон файла
   cFile     := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".ods"
   cFile     := IIF( LEN(cFileNew) == 0, cFile, cFileNew )
   cFile     := GetFileNameMaskNum(cFile)       // получить новое имя файла
   lActivate := .T.                             // открыть Calc
   lSave     := .T.                             // сохранить файл
   aFont     := {"DejaVu Sans Mono", 10 }       // задать фонт таблицы для Calc
                                                // для черно-белого варианта
                                                // для цветного варианта фонт берется
                                                // с ячеек таблицы
   RETURN { cFile, lActivate, lSave, aFont }

* ======================================================================
FUNCTION TotalTimeExports( cMsg, cFile, tTime )
   ? "=> " + cMsg + " " + cFile
   ? "  Total time spent on exports - " + HMG_TimeMS( tTime )
   ? "  ."
   RETURN NIL

