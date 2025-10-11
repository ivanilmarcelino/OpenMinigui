/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2020-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Таблица отчёта и экспорт отчёта / Report table and report export
*/

#include "minigui.ch"
#include "TSBrowse.ch"

////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION LogUserExcel(oBrw,nView,cPathExp,cFileMsk,cHeadline)
   LOCAL hProgress, tTime, bExternXls, aTsb, aXlsParam, aXlsTitle, aImage, aXlsFoot
   //LOCAL nRecno

   //IF oBrw:lIsDbf                           // это для Dbf
   //   nRecno := (oBrw:cAlias)->( RecNo() )
   //ENDIF
   //oBrw:GoTop()  // Экспорт идёт с текущей позиции курсора
   DO EVENTS

   CursorWait()

#ifdef KEY_ENG
   WaitWindow( "... Wait for the preparation to complete ...", .T. )
#else
   WaitThreadCreateIcon( 'Подготовка таблицы', 'массив для xls' )   // запуск без времени
#endif

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle             // хенд для ProgressBar на другой форме
   aTsb       := myGetTsbContent(oBrw)                // содержание таблицы
   aXlsParam  := myExcelParam(cPathExp,cFileMsk)      // параметры для экселя
   aXlsTitle  := myReportTitle(nView,cHeadline)       // заголовок экселя
   aXlsFoot   := myReportFoot(nView)                  // подвал экселя
   //aImage   := myImageReport()                      // картинка

   // Экспорт значений таблицы в массив идёт с первой позиции таблицы
   // принцип экспорта - что на экране в таблице, то и будет в экселе
   // плюс обработка в функции-окончания экселя

   /* ? "------- проверка/check -----------"
   ? "aTsb="     ,aTsb      ; ?v aTsb      ; ?
   ? "aXlsParam=",aXlsParam ; ?v aXlsParam ; ?
   ? "aXlsTitle=",aXlsTitle ; ?v aXlsTitle ; ?
   ? "aXlsFoot=" ,aXlsFoot  ; ?v aXlsFoot  ; ?
   ? "aImage="   ,aImage    ; ?v aImage    ; ? */

   IF nView == 1
      bExternXls := nil   // подключение внешнего блока для оформления oSheet
      aImage     := nil   // не нужна картинка
   ELSEIF nView == 2
      bExternXls := {|oSheet,aTsb,aXlsTitle| ExcelOle8Extern( hProgress, oSheet, aTsb, aXlsTitle) }
      aImage     := nil   // не нужна картинка
   ENDIF

#ifdef KEY_ENG
   WaitWindow()
#else
   WaitThreadCloseIcon()  // kill the window waiting
#endif

   CursorArrow()

   Brw8XlsOle( aTsb, aXlsParam, aXlsTitle, aXlsFoot, aImage, hProgress, bExternXls )
   //TotalTimeExports("Brw8XlsOle(" + HB_NtoS(nView) + ")=", aXlsParam[1], tTime )

   //IF oBrw:lIsDbf      // это для Dbf
   //   DbSelectArea(oBrw:cAlias)
   //   oBrw:Refresh(.T.)
   //   oBrw:GoToRec( nRecno )
   //ENDIF
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

   RETURN Nil

////////////////////////////////////////////////////////////////////////////
// заголовок отчёта эксель/ворд/калс/хмл
FUNCTION myReportTitle(nView,cHeadline)
   LOCAL aXlsTitle, cTitle, aFont, aColor, n1, n2

   aXlsTitle := {}
   cTitle    := cHeadline                       // Журнал-событий-программы
   IF !IsString(cTitle)
      cTitle := 'Экспорт таблицы журнала'
   ENDIF
   aFont  := { "Comic Sans MS", 22, .T. , .f. }
   aColor := IIF(nView==1,{BLACK,WHITE},{RED,WHITE})   // цвет/фон ячеек
   n1     := 1                                         // начало строки
   n2     := 0                                         // 0-объединить строку до конца таблицы
   AADD( aXlsTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aXlsTitle, {} )  // разделительная строка

   RETURN aXlsTitle

/////////////////////////////////////////////////////////////////////////////
// подвал экселя
FUNCTION myReportFoot(nView)
   LOCAL aXlsFoot, cFoot, aFont, aColor, n1, n2

   aXlsFoot := {}
   AADD( aXlsFoot, {} )   // разделительная строка
   AADD( aXlsFoot, {} )   // разделительная строка

#ifdef KEY_ENG
   cFoot := "Report creation date: " + DTOC(DATE()) + SPACE(10) + "Who created: "
#else
   cFoot := " Дата создания отчёта: " + DTOC(DATE()) + SPACE(10) + "Кто создал: "
#endif
   cFoot    += hb_UserName()+"/"+NetName()
   aFont    := { "Comic Sans MS", 16, .T. , .f. }
   aColor   := IIF(nView==1, {BLACK,WHITE} ,{ PURPLE, YELLOW })  // цвет/фон ячеек
   n1       := 1                                     // начало строки
   n2       := 0                                     // объединять строку
   AADD( aXlsFoot, {n1,n2, cFoot, aFont, aColor, DT_LEFT } )
   AADD( aXlsFoot, {} )  // разделительная строка

   RETURN aXlsFoot

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myExcelParam(cPath,cMaska)
   LOCAL cXlsFile, aXlsFont, lActivate, lSave, cMsg  //, nWidthTsb
   DEFAULT cPath  := GetStartUpFolder() + "\"        // путь записи файла
   DEFAULT cMaska := "cMaska_7Excel"                 // шаблон файла

   cXlsFile   := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".xls"
   cXlsFile   := GetFileNameMaskNum(cXlsFile)    // получить новое имя файла
   cXlsFile   := cPath + hb_FNameName(cXlsFile)  // .xls - не надо
   lActivate  := .T.                             // открыть Excel
   lSave      := .T.                             // сохранить файл
   aXlsFont   := {"DejaVu Sans Mono", 13 }

   // Проверить имя файла на количества точек
   // В случае наличия нескольких точек в имени файла Excel может "отрезать" имя файла
   IF AtNum( ".", HB_FNameName( cXlsFile ) ) > 0
      cMsg := 'Calling from: ' + ProcName(0) + '(' + hb_ntos( ProcLine(0) )
      cMsg += ') --> ' + ProcFile(0) + ';;'
      cMsg += 'Output File Name - "' + HB_FNameName( cXlsFile ) + '";'
      cMsg += 'contains several signs dot !;'
      cMsg += 'Excel can "truncate" the file name !;;'
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgStop( cMsg , "Error" )
   ENDIF

   RETURN { cXlsFile, lActivate, lSave, aXlsFont }

* ======================================================================
FUNCTION LogUserCalc(oBrw,nView,cPathExp,cFileMsk,cHeadline)
   LOCAL hProgress, tTime, bExternCalc, aTsb, aCalcParam, aCalcTitle, aImage
   LOCAL nRecno, aCalcFoot

   nRecno := (oBrw:cAlias)->( RecNo() )
   oBrw:GoTop()  // Экспорт идёт с текущей позиции курсора
   DO EVENTS
   // скрыть колонки из списка колонок c формулами экселя
   //oBrw:HideColumns( 31, .t.)
   //oBrw:HideColumns( 32, .t.)
   // не надо ! формулы работают

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle        // хенд для ProgressBar на другой форме
   aTsb       := myGetTsbContent(oBrw)           // содержание таблицы
   aCalcParam := myCalcParam(cPathExp,cFileMsk)  // параметры для Calc
   aCalcTitle := myReportTitle(nView,cHeadline)  // заголовок Calc
   aCalcFoot  := myReportFoot(nView,aTsb,"CALC") // подвал Calc
   aImage     := myImageReport()                 // картинка

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
   //TotalTimeExports("Brw7OleCalc(" + HB_NtoS(nView) + ")=", aCalcParam[1], tTime )

   // восстановить колонки из списка колонок
   //oBrw:HideColumns( 31, .f.)
   //oBrw:HideColumns( 32, .f.)

   //oBrw:Refresh(.T.)
   //oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

RETURN Nil

* ======================================================================
STATIC FUNCTION myCalcParam(cPath,cMaska)
   LOCAL cFile, aFont, lActivate, lSave
   DEFAULT cPath  := GetStartUpFolder() + "\"        // путь записи файла
   DEFAULT cMaska := "cMaska_7Calc"                  // шаблон файла

   cFile     := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".ods"
   cFile     := GetFileNameMaskNum(cFile)       // получить новое имя файла
   lActivate := .T.                             // открыть Calc
   lSave     := .T.                             // сохранить файл
   aFont     := {"DejaVu Sans Mono", 14 }       // задать фонт таблицы для Calc
                                                // для черно-белого варианта
                                                // для цветного варианта фонт берется
                                                // с ячеек таблицы
RETURN { cFile, lActivate, lSave, aFont }

