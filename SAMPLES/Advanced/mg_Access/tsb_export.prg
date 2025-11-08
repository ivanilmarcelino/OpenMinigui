/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с меню экспорта / Working with the export menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION TableToExport(ow,ky,cn,ob)
   LOCAL aMenu, aLang, nBmpSize, nFSize, nChoice, nPos, lExit, cRun
   LOCAL xRet, cForm, aFntExt, cFunc, cMsg, cMenu, cIcon, cFile

   ? ProcNL(), ow:Name,ky,cn,ob:cAlias,This.&(cn).Caption
   cForm := ow:Name

   IF App.Cargo:cLang == "RU"
      aLang := { "Экспорт -> " , "Экспорт -> ", "Экспорт -> " }
   ELSE
      aLang := { "Export -> "  , "Export -> " , "Export -> "  }
   ENDIF

   aMenu := {}
   cFile := ow:Cargo:cFile + ".csv"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iCsv32", aLang[1]+cMenu         , .F. , "myDbWriteCsv"   , cFile , 1, ow, ob } )    // -> util_dbf.prg
   AADD( aMenu, {                                                                               } )
   cFile := ow:Cargo:cFile + ".dbf"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iDbf32", aLang[2]+cMenu         , .F. , "myDbWriteDbf"   , cFile , 2, ow, ob } )
   AADD( aMenu, {                                                                               } )
   cFile := ow:Cargo:cFile + ".xls"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iXls32", aLang[3]+cMenu+" (OLE)", .F. , "myDbWriteXls"   , cFile , 3, ow, ob } )

   nPos     := 3
   nBmpSize := 32
   nFSize   := 16
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos  := aMenu[nChoice,6]
      cMsg  := aLang[nPos]
      cFunc := aMenu[nChoice,4]
      cRun  := cFunc + '("' + cMsg + '")' //+ HB_ValToExp(aMenu[nChoice]) + ')'
      cFile := aMenu[nChoice,5]
      cIcon := aMenu[nChoice,1]
      IF nPos == 1
         xRet := myDbWriteCsv(cFile, cIcon, ow, ob )
      ELSEIF nPos == 2
         xRet := myDbWriteDbf(cFile, cIcon, ow, ob )
      ELSEIF nPos == 3
         xRet := myDbWriteXls(cFile, cIcon, ow, ob )
      ELSE
         IF MyIsFunNoRun(cFunc)
            xRet := EVal( hb_MacroBlock( cRun ) , cFile )
         ELSE
            xRet := NIL
         ENDIF
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cFile, cIcon, oWnd, oBrw)      // Выгрузить CSV
   LOCAL cMsg, cTtl, cFCsv, oRSet, nRec, cStr, cLng, cLn2
   LOCAL oCol, cCol, cTyp, nI, xVal, aRet, cCdPg, lOpen
   LOCAL cSetCP, cLngSel, cSelCdp, cPath

   PUBLIC aPubFileExport

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   oRSet := oBrw:Cargo:oRSet            // данные этой таблицы
   nRec  := oRSet:RecordCount()

   IF App.Cargo:cLang == "RU"
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

   oWnd:Cargo:aFileExport := {}                 // вернуть парметры экспорта
   M->aPubFileExport := {}                      // вернуть парметры экспорта

   //aRet := Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса --- можно так
   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // форма-запроса

   //aRet := oWnd:Cargo:aFileExport      // считаем парметры экспорта - вариант 3
   aRet := M->aPubFileExport             // считаем парметры экспорта - вариант 1
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

   WaitWindow( { cTtl, cFile }, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   ? ProcNL() ; ? "    ###["+cFile+"]###########"
   nRec := 0
   HB_FileDelete( cFile )
   oRSet:MoveFirst()
   DO WHILE !oRSet:EOF()
      //? "RecNo = "+HB_NtoS(++nRec) //, oRSet:Fields(0):Name, oRSet:Fields(0):Value, ;
                                     // oRSet:Fields(1):Name, oRSet:Fields(1):Value
      cStr := ""
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cCol := oCol:cName
          cTyp := oCol:cFieldTyp
          //?? nI, cCol, cTyp
          IF cCol == "SELECTOR"
          ELSE
             xVal := oBrw:GetValue(nI)
             //?? xVal, VALTYPE(xVal)
             IF VALTYPE(xVal) == "U"
                xVal := ""
             ENDIF
             IF cTyp == "C"
                xVal := ALLTRIM(xVal)
             ELSE
                xVal := cValToChar(xVal)
             ENDIF
             cStr += xVal + ";"
          ENDIF
      NEXT
      cStr := hb_Translate( cStr, cSelCdp, cCdPg )
      STRFILE( cStr + CRLF, cFile, .T. )
      DO EVENTS
      oRSet:MoveNext()
   ENDDO
   oRSet:MoveFirst()

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

////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteXls(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFXls, cTtl, aSay, nRec, oRSet, cPath
   LOCAL lActivate, lSave, cTtlXls, aTitle, hFont, bExtrnXls, aColSel, bPrintRow
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Выгружать больше 65533 строк в Excel НЕЛЬЗЯ ! Ограничение Excel 2003.
   // Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.

   cForm     := oWnd:Name
   oRSet     := oBrw:Cargo:oRSet                            // данные этой таблицы
   nRec      := oRSet:RecordCount()
   lActivate := .F.                                         // открыть Excel
   lSave     := .T.                                         // сохранить файл
   cTtlXls   := "_" + Space(20) + "Convert to XLS"
   aTitle    := { cTtlXls, GetFontHandle( "ComSanMS" ) }    // титул со своим фонтом
   hFont     := GetFontHandle( "Normal" )                   // указать свой фонт для Excel
   bExtrnXls := nil  // подключение внешнего блока для оформления oSheet и объект Tsbrowse
   aColSel   := nil  // определяет по заданным колонкам (номера колонок) вывод в таблицу
   //aColSel := { 1,2,3,4,5,6,7,8,9,10 } // пример задания колонок
   bPrintRow := nil  // блок кода на каждой строке, возвращает T/F - если .F. пропускает строку

   IF App.Cargo:cLang == "RU"
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
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFXls )
   oRSet:MoveFirst()

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

////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteDbf(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFDbf, cTtl, aSay, nRec, oRSet, aStruct, cAls
   LOCAL uTyp, xVal, nI, cFld, oCol, cTyp, cDbf, cPath
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Наименование колонок может быть больше 10 символов, DBF этого НЕ ПОДДЕРЖИВАЕТ !
   // Attention! Column names can be longer than 10 characters, DBF DOES NOT SUPPORT this !
   aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
   ?v aStruct
   cForm     := oWnd:Name
   oRSet     := oBrw:Cargo:oRSet                            // данные этой таблицы
   nRec      := oRSet:RecordCount()

   IF App.Cargo:cLang == "RU"
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
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]
   cPath := cFilePath(cFile)
   aSay  := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFile )
   dbCreate(cFile, aStruct)
   cAls := "TEMP"
   //------------- создать/открыть dbf для показа ------------
   USE ( cFile ) VIA "DBFCDX" ALIAS &cAls NEW CODEPAGE cCdPg SHARED

   oRSet:MoveFirst()

   oBrw:GoTop()
   oBrw:lEnabled := .F.  // блокировать область таблицы (Строки не отображаются)

   DO WHILE !oRSet:EOF()
      //? "RecNo = "+HB_NtoS(++nRec) //, oRSet:Fields(0):Name, oRSet:Fields(0):Value, ;
                                     // oRSet:Fields(1):Name, oRSet:Fields(1):Value
      DBSELECTAREA(cAls)
      APPEND BLANK
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cFld := oCol:cName //oCol:cField
          cTyp := oCol:cFieldTyp
          cDbf := oCol:cFooting         // подвал колонки - поля БД
          ? nI, "cFld=", cFld, cTyp, "cDbf=", cDbf
          IF cFld == "SELECTOR"
          ELSE
             xVal := oBrw:GetValue(cFld)
             ?? "[", xVal, "]"
             uTyp := FIELDTYPE( FIELDPOS(cDbf) )
             ?? uTyp
             IF VALTYPE(xVal) == "U"
                IF uTyp == "C"
                   xVal := ""
                ELSEIF uTyp == "M"
                   xVal := ""
                ELSEIF uTyp == "N"
                   xVal := 0
                ELSEIF uTyp == "L"
                   xVal := .F.
                ELSEIF uTyp == "D"
                   xVal := CTOD("")
                ELSEIF uTyp == "T"
                   xVal := hb_CToT("")
                ENDIF
             ENDIF
             IF ( uTyp == "T" .OR. uTyp == "@" ).AND. VALTYPE(xVal) == "C"
                (cAls)->&cDbf := HB_STOT(xVal)
             ELSE
                (cAls)->&cDbf := xVal
             ENDIF
          ENDIF
      NEXT
      DO EVENTS
      oRSet:MoveNext()
   ENDDO
   oRSet:MoveFirst()

   oBrw:lEnabled := .T.    // разблокировать область таблицы (Строки отображаются)
   oBrw:Display()
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:GoTop()
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

