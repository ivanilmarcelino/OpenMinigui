/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с меню / Working with the menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "dbinfo.ch"
/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION Menu5Export(oWnd,ky,cn,ob)
   LOCAL aMenu, aLang, nBmpSize, nFSize, nChoice, nPos, lExit, cRun, aBtnObj
   LOCAL xRet, cForm, aFntExt, cFunc, cMsg, cFile, cIcon, aUse, aBtn5, nY, nX

   ? ProcNL(), oWnd:Name,ky,cn,ob:cAlias,This.&(cn).Caption
   cForm    := oWnd:Name
   aBtnObj  := oWnd:Cargo:aBtnObj     // массив кнопок на форме
   aBtn5    := aBtnObj[5]
   ? SPACE(3) + HB_ValToExp(aBtn5)
   //  1      2          3                4       5   6   7   8
   // {5, "_Config", "-имя объекта", "Настройки", 0, 384, 86, 69, "_Config", "-событие"}
   nY       := oWnd:Row + aBtn5[5] + aBtn5[8]
   nX       := oWnd:Col + aBtn5[6] + 5
   DBSELECTAREA(ob:cAlias)
   aUse     := oWnd:Cargo:aUse           // { cFile, lShared, cDrvDbf, cCdPg, cPsw, cSetDel }

   IF App.Cargo:cLang == "RU"
      aLang := { "Экспорт -> .csv"    , "Экспорт -> .dbf", "Экспорт Sqlite -> .sqlite", "Экспорт" }
   ELSE
      aLang := { "Export -> .csv"     , "Export -> .dbf" , "Export Sqlite -> .sqlite" , "Export"  }
   ENDIF

   aMenu := {}
   AADD( aMenu, { ""      , "Free format"     , .T. , ""               , ""    , 0,   ,    } )
   cFile := hb_FNameExtSet( aUse[1], ".csv" )
   AADD( aMenu, { "iCsv32", aLang[1]          , .F. , "myDbWriteCsv"   , cFile , 1, oWnd, ob } )    // -> util_dbf.prg
   cFile := aUse[1] + "(-).dbf"
   AADD( aMenu, { "iDbf32", aLang[2]          , .F. , "myDbWriteDbf"   , cFile , 2, oWnd, ob } )
   cFile := hb_FNameExtSet( aUse[1], ".sqlite" )
   AADD( aMenu, { "iSqlite32", aLang[3]       , .F. , "myDbWriteDbf"   , cFile , 3, oWnd, ob } )
   AADD( aMenu, {                                                                          } )
   AADD( aMenu, { ""      , "Microsoft Office", .T. , ""               , ""    , 0,   ,    } )
   cFile := hb_FNameExtSet( aUse[1], ".xls" )
   AADD( aMenu, { "iXls32", aLang[4] + " Ole-Excel        .xls" , .F. , "myDbWriteXls", cFile , 4, oWnd, ob } )
   AADD( aMenu, { "iXls32", aLang[4] + " Ole-Excel-white  .xls" , .F. , "ToExcel7"    , cFile , 5, oWnd, ob } )
   AADD( aMenu, { "iXls32", aLang[4] + " Ole-Excel-color  .xls" , .F. , "ToExcel7"    , cFile , 6, oWnd, ob } )
   AADD( aMenu, {                                                                                         } )
   AADD( aMenu, { ""      , "Open Office"     , .T. , ""               , ""    , 0,   ,    } )
   cFile := hb_FNameExtSet( aUse[1], ".ods" )
   AADD( aMenu, { "iOOCalc32", aLang[4] + " Ole-Calc-white  .ods" , .F. , "ToCalc7"   , cFile , 7, oWnd, ob } )
   AADD( aMenu, { "iOOCalc32", aLang[4] + " Ole-Calc-color  .ods" , .F. , "ToCalc7"   , cFile , 8, oWnd, ob } )

   nPos     := { nY, nX }
   nBmpSize := 32
   nFSize   := 16
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos  := aMenu[nChoice,6]
      cMsg  := aMenu[nChoice,5] + "' , '" + aMenu[nChoice,1] + "', '"
      cFunc := aMenu[nChoice,4]
      cRun  := cFunc + '("' + cMsg + '")' //+ HB_ValToExp(aMenu[nChoice]) + ')'
      cFile := aMenu[nChoice,5]
      cIcon := aMenu[nChoice,1]
      IF nPos == 1
         xRet := myDbWriteCsv(cFile, cIcon, oWnd, ob )
      ELSEIF nPos == 2
         xRet := myDbWriteDbf(cFile, cIcon, oWnd, ob )
      ELSEIF nPos == 3
         xRet := myDbWriteSqlite(cFile, cIcon, oWnd, ob )
      ELSEIF nPos == 4
         xRet := myDbWriteXls(cFile, cIcon, oWnd, ob )
      ELSEIF nPos == 5
         xRet := ToExcel8(ob,1,cFile)    // white/black  -> TsbExport8.prg
      ELSEIF nPos == 6
         xRet := ToExcel8(ob,2,cFile)    // color        -> TsbExport8.prg
      ELSEIF nPos == 7
         xRet := ToCalc8(ob,1,cFile)     // white/black
      ELSEIF nPos == 8
         xRet := ToCalc8(ob,2,cFile)     // color
      ELSE
         IF MyIsFunNoRun(cFunc)
            xRet := EVal( hb_MacroBlock( cRun ) , cMsg )
         ELSE
            xRet := NIL
         ENDIF
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

///////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cFile, cIcon, oWnd, oBrw)    // Выгрузить CSV
   LOCAL cMsg, cLng, cLn2, cTtl, cFCsv, cAls, nRec
   LOCAL aRet, cCdPg, lOpen, cPath

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   cAls  := oBrw:cAlias
   DbSelectArea(cAls)
   nRec  := LASTREC()

   IF App.Cargo:cLang == "RU"
      cTtl := 'Преобразование DBF в CSV (dbf -> csv)'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
   ELSE
      cTtl := 'Convert DBF to CSV (dbf -> csv)'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса

   aRet := oWnd:Cargo:aFileExport                    // считаем парметры экспорта - вариант 3
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile   := aRet[1]
   cCdPg   := aRet[2]
   lOpen   := aRet[3]
   cFCsv   := cFileNoPath( cFile )
   cPath   := cFilePath( cFile )

   WaitWindow( { cTtl, cFile }, .T., 800, 14, NIL, WHITE, App.Cargo:aDlgBColor, 14, BLUE, 4 ) // open the wait window

   GOTO TOP
   COPY TO (cFile) CODEPAGE cCdPg DELIMITED // запись в файл
   WaitWindow()

   IF lOpen
      cMsg := cLng + cFCsv + ";" + cPath + ";;" + cLn2
      IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
         ShellExecute( 0, "Open", cFile,,, 3 )
      ENDIF
   ELSE
      cMsg := cLng + cFCsv + ";" + cPath
      AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myDbWriteDbf(cFile, cIcon, oWnd, oBrw)   // Выгрузить DBF
   LOCAL xRet, cAls, cMsg, cFDbf, cTtl, aSay, nRec, cPath
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg, lSet, nBeg, l2Utf
   LOCAL cVal, cAls2, aStruct, nLField, nR, nI, c1CdPg, l1Utf

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   c1CdPg  := oBrw:Cargo:cCdPg   // читаем на окне ТСБ CodePage файла
   cAls    := oBrw:cAlias
   DbSelectArea(cAls)
   nRec    := LASTREC()          // кол-во записей базы на лок.компе
   nLField := fCount()           // кол-во полей базы на лок.компе
   aStruct := DbStruct()
   lSet    := Set( _SET_DELETED, .F. )   // SET DELETED OFF

   IF App.Cargo:cLang == "RU"
      cTtl := 'Преобразование DBF в DBF (dbf -> dbf)'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
      cLn3 := "Файл НЕ создан !;"
   ELSE
      cTtl := 'Convert DBF to DBF (dbf -> dbf)'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                           // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)        // форма-запроса

   aRet := oWnd:Cargo:aFileExport                         // считаем парметры экспорта - вариант 3
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]
   cFDbf := cFileNoPath( cFile )
   cPath := cFilePath(cFile) + "\"                      // путь к файлу

   // Для экспорта XXXX -> UTF8
   IF "UTF8" $ c1CdPg  ; l1Utf := .T.
   ELSE                ; l1Utf := .F.
   ENDIF
   IF "UTF8" $ cCdPg   ; l2Utf := .T.
   ELSE                ; l2Utf := .F.
   ENDIF
   IF !l1Utf .AND. l2Utf
      FOR nI := 1 TO LEN(aStruct)
         IF aStruct[nI,2] == "C"
            aStruct[nI,3] := aStruct[nI,3] * 2
         ENDIF
      NEXT
   ENDIF

   aSay := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, WHITE, App.Cargo:aDlgBColor, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFDbf )
   oBrw:GoTop()
   oBrw:lEnabled := .F.    // блокировать область таблицы (Строки не отображаются)

   dbCreate(cFile, aStruct)
   cAls2 := "TEMP"
   //------------- создать/открыть dbf для показа ------------
   USE ( cFile ) VIA "DBFCDX" ALIAS &cAls2 NEW CODEPAGE cCdPg SHARED

   DO WHILE ! ( cAls )->( EOF() )

      nR := (cAls)->( RecNo() ) // для бегунка

      nBeg := INT( ( nR / nRec ) * 100 )
      IF nBeg % 5 == 0   // вывод каждые 5%
         cVal := SPACE(5) + HB_NtoS(nR) + "/" + HB_NtoS(nRec)
         //SetProperty( aLbl[1], aLbl[2], "Value" , cSay + cVal ) // выводим надпись
      ENDIF

      SELECT(cAls2)
      dbAppend()
      FOR nI := 1 TO nLField
         IF FieldType( nI ) $ "+^="      // защита записи
         ELSEIF FieldType( nI ) $ "Y"    // Money - пропуск формата
         ELSE
            FieldPut( nI, ( cAls )->( FieldGet(nI) ) )
         ENDIF
      NEXT
      IF ( cAls )->( deleted() ); dbDelete()  // в local dbf удалена
      ENDIF
      ( cAls )->( dbSkip(1) )

      DO EVENTS    // ОБЯЗАТЕЛЬНО ! Чтобы форма НЕ замирала

   ENDDO

   DBSELECTAREA(cAls2)  // ОБЯЗАТЕЛЬНО !!!
   dbCommit()
   dbGotop()
   (cAls2)->( DbCloseArea() )  // закрыть dbf

   DbSelectArea(cAls)
   oBrw:lEnabled := .T.    // разблокировать область таблицы (Строки отображаются)
   oBrw:Display()
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   WaitWindow()
   Set( _SET_DELETED, lSet )   // SET DELETED ON - восстановить

   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFDbf + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFDbf + ";" + cPath
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFDbf
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteXls(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cAls, cMsg, cFXls, cTtl, aSay, nRec, cPath
   LOCAL lActivate, lSave, cTtlXls, aTitle, hFont, bExtrnXls, aColSel, bPrintRow
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Выгружать больше 65533 строк в Excel НЕЛЬЗЯ ! Ограничение Excel 2003.
   // Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.

   lActivate := .F.                                         // открыть Excel
   lSave     := .T.                                         // сохранить файл
   cTtlXls   := "_" + Space(20) + "Convert to XLS"
   aTitle    := { cTtlXls, GetFontHandle( "ComSanMS" ) }    // титул со своим фонтом
   hFont     := GetFontHandle( "Normal" )                   // указать свой фонт для Excel
   bExtrnXls := nil  // подключение внешнего блока для оформления oSheet и объект Tsbrowse
   aColSel   := nil  // определяет по заданным колонкам (номера колонок) вывод в таблицу
   //aColSel := { 1,2,3,4,5,6,7,8,9,10 } // пример задания колонок
   bPrintRow := nil  // блок кода на каждой строке, возвращает T/F - если .F. пропускает строку

   cAls  := oBrw:cAlias
   DbSelectArea(cAls)
   nRec  := LASTREC()

   IF App.Cargo:cLang == "RU"
      cTtl := 'Преобразование DBF в XLS (dbf -> xls)'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
      cLn3 := "Файл НЕ создан !;"
   ELSE
      cTtl := 'Convert DBF to XLS (dbf -> xls)'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                           // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F.)   // форма-запроса .F.-без смены кодировки

   aRet := oWnd:Cargo:aFileExport                         // считаем парметры экспорта - вариант 3
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]

   cPath := cFilePath(cFile) + "\"                      // путь к файлу
   cFXls := hb_FNameName(cFile)                         // .xls - не надо
   cFXls := CharRepl('.',cFXls,"_")                     // '.'  - нельзя

   // Проверить имя файла на количества точек
   IF AtNum( ".", cFXls ) > 0
      IF App.Cargo:cLang == "RU"
         cMsg := 'Имя выходного файла;' + cFXls + ';'
         cMsg += 'содержит несколько знаков точки "." !;'
         cMsg += 'Excel может "обрезать" имя файла !;;'
      ELSE
         cMsg := 'Output File Name;' + cFXls + ';'
         cMsg += 'contains several signs dot "." !;'
         cMsg += 'Excel can "truncate" the file name !;;'
      ENDIF
      cMsg += ProcNL()
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

   aSay := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, WHITE, App.Cargo:aDlgBColor, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFXls )
   oBrw:GoTop()
   oBrw:lEnabled := .F.  // блокировать область таблицы (Строки не отображаются)

   oBrw:ExcelOle( cPath + cFXls, lActivate, , aTitle, hFont, lSave, bExtrnXls, aColSel, bPrintRow )

   oBrw:lEnabled := .T.    // разблокировать область таблицы (Строки отображаются)
   oBrw:Display()
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   WaitWindow()

   cFile := cPath + cFXls + ".xls"
   cFXls := cFileNoPath(cFile)

   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFXls + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFXls + ";" + cPath
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFXls
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet
