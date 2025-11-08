/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с меню экспорта / Working with the export menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////////////////
FUNCTION Menu_Export(oWnd, ky, cBtn, oBrw)
   LOCAL nY, nX, cForm, hFont1, hFont2, nChoice, cLang
   LOCAL aFile, cFile1, cFile2, cFile3, cFile4, aIcon, cMaska

   ? ProcNL(), oWnd:ClassName, ky, cBtn, oBrw:cAlias
   cForm   := oWnd:Name
   cMaska  := oWnd:Cargo:cFile
   cFile1  := cMaska + "(0).csv"
   cFile2  := cMaska + "(0).dbf"
   cFile3  := cMaska + "(0).xls"
   cFile4  := cMaska + "(0).arr"
   aFile   := { cFile1, cFile2, cFile3, cFile4 }
   aIcon   := { "iCsv32", "iDbf32", "iXls32", "iArr32" }
   //
   hFont1  := GetFontHandle( "ComSanMS" )
   hFont2  := GetFontHandle( "DlgFont"  )
   // координаты вывода окна / window output coordinates
   nY      := GetProperty(cForm, "Row") + GetTitleHeight()
   nY      += GetProperty(cForm, cBtn, "Row") + GetProperty(cForm, cBtn, "Height")
   nX      := GetProperty(cForm, "Col") + GetBorderWidth()
   nX      += GetProperty(cForm, cBtn, "Col") - 4
   nChoice := 0
   cLang  := IIF( App.Cargo:lRu, "Экспорт" , "Export" )

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   DEFINE CONTEXT MENU OF &cForm
      MENUITEM cLang + " -> " + cFileNoPath(aFile[1]) NAME SetFile1 ACTION {|| nChoice := 1 } ICON aIcon[1] FONT hFont2
      SEPARATOR
      MENUITEM cLang + " -> " + cFileNoPath(aFile[2]) NAME SetFile2 ACTION {|| nChoice := 2 } ICON aIcon[2] FONT hFont2
      SEPARATOR
      MENUITEM cLang + " -> " + cFileNoPath(aFile[3]) NAME SetFile3 ACTION {|| nChoice := 3 } ICON aIcon[3] FONT hFont2
      SEPARATOR
      MENUITEM cLang + " -> " + cFileNoPath(aFile[4]) NAME SetFile3 ACTION {|| nChoice := 4 } ICON aIcon[4] FONT hFont1
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ / SHOW DROP-DOWN MENU

   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nChoice > 0
      TableToExport(oWnd,oBrw,nChoice,aFile[nChoice],aIcon[nChoice])
   ENDIF

   DO EVENTS

RETURN nChoice

///////////////////////////////////////////////////////////////////////////////
FUNCTION TableToExport(ow,ob,nPos,cFile,cIcon)
   LOCAL xRet

   IF nPos == 1
      xRet := myDbWriteCsv(cFile, cIcon, ow, ob )
   ELSEIF nPos == 2
      xRet := myDbWriteDbf(cFile, cIcon, ow, ob )
   ELSEIF nPos == 3
      xRet := myDbWriteXls(cFile, cIcon, ow, ob )
   ELSEIF nPos == 4
      xRet := myDbWriteArr(cFile, cIcon, ow, ob )
   ELSE
   ENDIF

RETURN xRet

/////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cFile, cIcon, oWnd, oBrw)           // Выгрузить CSV
   LOCAL cMsg, cTtl, cFCsv, nRec, cStr, cLng, cLn2
   LOCAL cTyp, nI, xVal, aRet, cCdPg, lOpen, nPos, nJ
   LOCAL cSetCP, cLngSel, cSelCdp, cPath, aSay, aLine

   PUBLIC aPubFileExport

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   nRec  := LEN(oBrw:aArray)

   IF App.Cargo:lRu
      cTtl := 'Преобразование в CSV'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
   ELSE
      cTtl := 'Convert to CSV'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // вернуть парметры экспорта
   M->aPubFileExport := {}                           // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса

   aRet := M->aPubFileExport                         // считаем парметры экспорта - вариант 1
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile   := ALLTRIM(aRet[1])
   cCdPg   := aRet[2]
   lOpen   := aRet[3]
   cFCsv   := cFileNoPath( cFile )
   cPath   := cFilePath( cFile )
   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()
   aSay    := { cTtl, cFile, App.Exename }

   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   ? ProcNL() ; ? " ###["+cFile+"]###########"
   nRec := 0
   HB_FileDelete( cFile )

   nPos := 0          // надо учитывать доп. колонки
   IF oBrw:nColumn("SELECTOR", .T.) > 0 ; nPos += 1
   ENDIF
   IF oBrw:nColumn("ARRAYNO" , .T.) > 0 ; nPos += 1
   ENDIF
   // вывод только для массива
   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      cStr := ""
      //? "nJ=",nJ  , "aLine=",LEN(aLine)
      FOR EACH xVal IN aLine
          nI := hb_enumindex(xVal)  // номер элемента массива
          //?? "nI=", nI, "xVal=",xVal
          cTyp := VALTYPE(xVal)
          IF cTyp == "U"
             xVal := ""
          ENDIF
          IF cTyp == "C"
             xVal := ALLTRIM(xVal)
          ELSE
             xVal := cValToChar(xVal)
          ENDIF
          cStr += xVal + ";"
      NEXT
      cStr := hb_Translate( cStr, cSelCdp, cCdPg )
      STRFILE( cStr + CRLF, cFile, .T. )
      DO EVENTS
   NEXT

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

   RELEASE aPubFileExport

RETURN NIL

////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteDbf(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFDbf, cTtl, aSay, nRec, aStruct, cAls
   LOCAL xVal, nI, cTyp, cPath, cCol, aLine, nPos, nLen, nJ
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg, cRdd, uTyp

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   cForm := oWnd:Name
   nRec  := LEN(oBrw:aArray)

   IF App.Cargo:lRu
      cTtl := 'Преобразование в DBF'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
      cLn3 := "Файл НЕ создан !;"
   ELSE
      cTtl := 'Convert to DBF'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса

   aRet := oWnd:Cargo:aFileExport
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF

   aSay  := { cTtl + ' ...', cFile, App.Exename }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   aStruct := {}  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
   oBrw:GoTop()
   oBrw:lEnabled := .F.  // блокировать область таблицы (Строки не отображаются)

   nPos := 0             // надо учитывать доп. колонки
   IF oBrw:nColumn("SELECTOR", .T.) > 0 ; nPos ++
   ENDIF
   IF oBrw:nColumn("ARRAYNO" , .T.) > 0 ; nPos ++
   ENDIF

   // вывод только для массива
   aLine := oBrw:aArray[1]
   FOR EACH xVal IN aLine
       nI := hb_enumindex(xVal)  // номер элемента массива
       //?? "nI=", nI, "xVal=",xVal
       cTyp := VALTYPE(xVal)
       IF cTyp == "U"
          xVal := ""
          cTyp := "C"
       ENDIF
       IF cTyp == "C"
          xVal := ALLTRIM(xVal)
       ELSE
          xVal := cValToChar(xVal)
       ENDIF
       cCol := "COL_" + STRZERO(nI,3)
       AADD( aStruct, { cCol, cTyp, 2, 0 } )
   NEXT
   // подсчет длины полей для базы
   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      //? "nJ=",nJ  , "aLine=",LEN(aLine)
      FOR EACH xVal IN aLine
          nI := hb_enumindex(xVal)
          //?? "nI=", nI, "xVal=",xVal
          cTyp := VALTYPE(xVal)
          IF cTyp == "U"
             xVal := ""
             cTyp := "C"
          ENDIF
          nLen := 0
          IF cTyp == "C"
             xVal := ALLTRIM(xVal)
             nLen := LEN(xVal)
          ELSEIF cTyp == "N"
             xVal := HB_NtoS(xVal)
             nLen := LEN(xVal)
          ELSEIF cTyp == "D"
             nLen := 8
          ELSEIF cTyp == "L"
             nLen := 1
          ENDIF
          aStruct[nI,3] := MAX( aStruct[nI,3] , nLen )
      NEXT
      DO EVENTS
   NEXT
   //?v aStruct

   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]
   cPath := cFilePath(cFile)
   cRdd  := "DBFCDX"

   HB_FileDelete( cFile )
   dbCreate(cFile, aStruct, cRdd)
   cAls := "TEMP"
   //------------- создать/открыть dbf для показа ------------
   USE ( cFile ) VIA "DBFCDX" ALIAS &cAls NEW CODEPAGE cCdPg SHARED

   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      //? "nJ=",nJ  , "aLine=",LEN(aLine)
      DBSELECTAREA(cAls)
      APPEND BLANK
      FOR EACH xVal IN aLine
          nI := hb_enumindex(xVal)
          //? "nI=", nI, "xVal=",xVal
          cTyp := VALTYPE(xVal)
          //?? cTyp, "|"
          uTyp := FIELDTYPE( nI )
          IF cTyp == "C" .AND. uTyp == "N"
             xVal := VAL(xVal)
          ELSEIF cTyp == "C" .AND. uTyp == "M"
             xVal := xVal
          //ELSEIF uTyp == "N"
          //   xVal := 0
          //ELSEIF uTyp == "L"
          //   xVal := .F.
          //ELSEIF uTyp == "D"
          //   xVal := CTOD("")
          //ELSEIF uTyp == "T"
          //   xVal := hb_CToT("")
          ENDIF
          //?? uTyp, VALTYPE(xVal)
          FieldPut( nI, xVal )
      NEXT
      DO EVENTS
   NEXT

   oBrw:GoTop()
   oBrw:lEnabled := .T.    // разблокировать область таблицы (Строки отображаются)
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:SetFocus()
   DO EVENTS

   (cAls)->( DbCloseArea() )  // закрыть dbf

   WaitWindow()

   cFDbf := cFileNoPath(cFile)
   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFDbf + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFDbf
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFDbf + ";" + cPath
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteXls(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFXls, cTtl, aSay, nRec, cPath
   LOCAL lActivate, lSave, cTtlXls, aTitle, hFont, bExtrnXls, aColSel, bPrintRow
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Выгружать больше 65533 строк в Excel НЕЛЬЗЯ ! Ограничение Excel 2003.
   // Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.

   cForm     := oWnd:Name
   nRec      := LEN(oBrw:aArray)
   lActivate := .F.                                         // открыть Excel
   lSave     := .T.                                         // сохранить файл
   cTtlXls   := "_" + Space(20) + "Convert to XLS"
   aTitle    := { cTtlXls, GetFontHandle( "ComSanMS" ) }    // титул со своим фонтом
   hFont     := GetFontHandle( "Normal" )                   // указать свой фонт для Excel
   bExtrnXls := nil  // подключение внешнего блока для оформления oSheet и объект Tsbrowse
   aColSel   := nil  // определяет по заданным колонкам (номера колонок) вывод в таблицу
   //aColSel := { 1,2,3,4,5,6,7,8,9,10 } // пример задания колонок
   bPrintRow := nil  // блок кода на каждой строке, возвращает T/F - если .F. пропускает строку

   IF App.Cargo:lRu
      cTtl := 'Преобразование в XLS'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
      cLn3 := "Файл НЕ создан !;"
   ELSE
      cTtl := 'Convert to XLS'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F.)   // форма-запроса .F.-без смены кодировки

   aRet := oWnd:Cargo:aFileExport
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
      IF App.Cargo:lRu
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

   aSay := { cTtl + ' ...', cFile, App.Exename }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

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

/////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteArr(cFile, cIcon, oWnd, oBrw)           // Выгрузить ARR
   LOCAL cMsg, cTtl, cFCsv, nRec, cStr, cLng, cLn2, nCol, uData
   LOCAL cTyp, nI, xVal, aRet, cCdPg, lOpen, nPos, nJ, oCol
   LOCAL cSetCP, cLngSel, cSelCdp, cPath, aSay, aLine, cCol

   PUBLIC aPubFileExport

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   nRec  := LEN(oBrw:aArray)

   IF App.Cargo:lRu
      cTtl := 'Преобразование в ARR'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
   ELSE
      cTtl := 'Convert to ARR'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // вернуть парметры экспорта
   M->aPubFileExport := {}                           // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса

   aRet := M->aPubFileExport                         // считаем парметры экспорта - вариант 1
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile   := ALLTRIM(aRet[1])
   cCdPg   := aRet[2]
   lOpen   := aRet[3]
   cFCsv   := cFileNoPath( cFile )
   cPath   := cFilePath( cFile )
   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()
   aSay    := { cTtl, cFile, App.Exename }

   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   ? ProcNL() ; ? " ###["+cFile+"]###########"
   nRec := 0
   HB_FileDelete( cFile )

   nPos := 0          // надо учитывать доп. колонки
   IF oBrw:nColumn("SELECTOR", .T.) > 0 ; nPos += 1
   ENDIF
   IF oBrw:nColumn("ARRAYNO" , .T.) > 0 ; nPos += 1
   ENDIF

   IF oBrw:lDrawHeaders       // проверка шапки таблицы
      cStr := "HEAD = { "     // шапка таблицы в файл
      nCol := LEN(oBrw:aColumns)
      FOR EACH oCol IN oBrw:aColumns
          nJ   := hb_EnumIndex(oCol)
          cCol := oCol:cName
          IF cCol == "SELECTOR" .OR. cCol == "ARRAYNO"
             LOOP
          ELSE
             uData := If( ValType( oCol:cHeading ) == "B", Eval( oCol:cHeading ), ;
                                   oCol:cHeading )

             If ValType( uData ) != "C"
                uData := ""
             EndIf

             uData := StrTran( uData, CRLF, ";" )
             cStr  += ' "' + uData + '"'
             cStr  += IIF( nJ == nCol, ' } ' , ' , ' )
          ENDIF
      NEXT
      STRFILE( cStr + CRLF + CRLF, cFile, .T. )
   ENDIF

   // вывод только для массива
   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      cStr := " { "
      nCol := LEN(aLine)
      FOR EACH xVal IN aLine
          nI := hb_enumindex(xVal)
          cTyp := VALTYPE(xVal)
          IF cTyp == "U"
             xVal := ""
          ENDIF
          IF cTyp == "C"
             xVal := '"'+ALLTRIM(xVal)+'"'
          ELSE
             xVal := cValToChar(xVal)
          ENDIF
          cStr += xVal
          cStr += IIF( nI == nCol, '' , ' , ' )
      NEXT
      cStr += " } "
      cStr := hb_Translate( cStr, cSelCdp, cCdPg )
      STRFILE( cStr + CRLF, cFile, .T. )
      DO EVENTS
   NEXT

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

   RELEASE aPubFileExport

RETURN NIL

