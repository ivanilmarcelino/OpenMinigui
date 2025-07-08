/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "excel.ch"

* =======================================================================================
FUNCTION Brw4CsvOle( oBrw, cFile, cFileFormat, lOpenFile )
   LOCAL cMsg, cVal, cFileExport, cPth, cFil, cExt
   LOCAL oExcel, oBook, oSheet

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt) 
   cFileExport := cPth + cFil + ".csv"

   IF ! FILE( cFile )  
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "“акого файла нет !;;"
         cVal := "ќшибка!"
      ELSE
         cMsg += "There is no such file!;;"
         cVal := "Error!"
      ENDIF
      cMsg += cFile + ";;"
      cMsg += REPLICATE( "-._.", 16 ) + ";;"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgStop( cMsg , cVal )
      RETURN Nil 
   ENDIF

   CursorWait()
   WaitThreadCreateIcon( 'Loading the report in', 'EXCEL OLE ...' )   // запуск без времени

   oExcel := win_oleCreateObject( "Excel.Application" ) 

   // »спользуем Ole из HBWIN.lib
   IF ( oExcel := win_oleCreateObject( "Excel.Application" ) ) == NIL 
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "MS Excel не доступен !;;   ќшибка"
         cVal := "ќшибка!"
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

   oExcel:Visible := .F.          // .T. показать Excel на экране дл€ отладки
   oExcel:DisplayAlerts := .F.    // убрать предупреждени€ Excel

   oExcel:Workbooks:Open(cFile)
   oBook  := oExcel:ActiveWorkbook
   oSheet := oExcel:ActiveSheet

   DeleteOnTheExcelSheet(oBrw,oExcel,oSheet)

   IF cFileFormat == "CSV"
      oBook:SaveAs( cFileExport, xlCSVWindows )
   ELSEIF cFileFormat == "DBF"
      oBook:SaveAs( cFileExport, xlDBF4 )
   ELSE
      cMsg := "Unknown file format - " + cFileFormat + " !"
      MsgStop( cMsg, "Error!" )
   ENDIF

   oExcel:Application:Quit()

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lOpenFile
      ShellExecute( 0, "Open", cFileExport,,, 3 )
   EndIf

   RETURN Nil

* =======================================================================================
FUNCTION DeleteOnTheExcelSheet(oBrw,oExcel,oSheet)
   LOCAL nI, lTsbSuperHd, lTsbHeading, lTsbFooting, nLine, aLineDel := {}

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

   ////////////// структура отчЄта ///////////////
   // nLine := 1  // титул таблицы 
   // nLine := 2  // пуста€ строка 
   // nLine := 3  // суперхидер таблицы, если есть 
   // nLine := 4  // шапка таблицы, если есть 
   // nLine := 5  // €чейки таблицы, перва€ €чейка (если есть суперхидер и шапка таблицы)
   // nLine := nLine + oBrw:nLen // подвал таблицы, если есть 

   nLine  := 1
   AADD( aLineDel, nLine )  // титул таблицы 

   ++nLine
   AADD( aLineDel, nLine )  // пуста€ строка

   // суперхидер таблицы
   If lTsbSuperHd 
      ++nLine
      AADD( aLineDel, nLine )
   EndIf

   // шапка таблицы
   If lTsbHeading     
      ++nLine
      AADD( aLineDel, nLine )
   EndIf

   nLine := nLine + oBrw:nLen

   // выводим подвал таблицы
   If lTsbFooting
      ++nLine
      AADD( aLineDel, nLine )
   EndIf

   FOR nI := LEN(aLineDel) TO 1 STEP -1
      nLine := aLineDel[nI]
      oSheet:Cells( nLine, 1 ):Value := nI
      oExcel:Rows(nLine):Delete()
   NEXT

   RETURN Nil

