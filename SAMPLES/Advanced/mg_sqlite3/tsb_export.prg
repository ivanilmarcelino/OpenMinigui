/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с меню экспорта / Working with the export menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION TableToExport(ow,ky,cn,ob)
   LOCAL aMenu, aLang, nBmpSize, nFSize, nChoice, nPos, lExit, cRun, cTable
   LOCAL xRet, cForm, aFntExt, cFunc, cMsg, cMenu, cIcon, cFile, cMaska, cAls

   ? ProcNL(), ow:Name,ky,cn,ob:cAlias,This.&(cn).Caption
   cForm  := ow:Name
   cTable := ow:Cargo:cTable               // имя таблицы
   cAls   := ow:Cargo:cAls                 // имя алиаса
   cMaska := ow:Cargo:cFile + "." + cAls

   IF App.Cargo:cLang == "RU"
      aLang := { "Экспорт -> " , "Экспорт -> ", "Экспорт -> ", "Экспорт -> " }
   ELSE
      aLang := { "Export -> "  , "Export -> " , "Export -> " , "Export -> "  }
   ENDIF

   aMenu := {}
   cFile := cMaska + ".csv"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iCsv32", aLang[1]+cMenu         , .F. , "myDbWriteCsv"   , cFile , 1, ow, ob } )    // -> util_dbf.prg
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".dbf"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iDbf32", aLang[2]+cMenu         , .F. , "myDbWriteDbf"   , cFile , 2, ow, ob } )
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".xls"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iXls32", aLang[3]+cMenu+" (OLE)", .F. , "myDbWriteXls"   , cFile , 3, ow, ob } )
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".ods"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iOOCalc32", aLang[4]+cMenu+" (OLE)", .F. , "myDbWriteCalc", cFile , 4, ow, ob } )

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
      ELSEIF nPos == 4
         xRet := myDbWriteOOCalc(cFile, cIcon, ow, ob )
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
   LOCAL cMsg, cTtl, cFCsv, nRec, cStr, cLng, cLn2
   LOCAL oCol, cCol, cTyp, nI, xVal, aRet, cCdPg, lOpen
   LOCAL cSetCP, cLngSel, cSelCdp, cPath

   PUBLIC aPubFileExport

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   nRec  := oBrw:Cargo:nTotalRecno

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

   WaitWindow( { cTtl, cFile }, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   ? ProcNL() ; ? "    ###["+cFile+"]###########"
   nRec := 0
   HB_FileDelete( cFile )

   DBSELECTAREA(oBrw:cAlias)
   GOTO TOP
   DO WHILE !EOF()
      //? "RecNo = "+HB_NtoS(RECNO())
      cStr := ""
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cCol := oCol:cName //oCol:cField
          cTyp := oCol:cFieldTyp
          //? "   nI=",nI, "cCol=", cCol, cTyp
          IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
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
      dbSkip()
   ENDDO
   dbGoTop()

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
   LOCAL xRet, cForm, cMsg, cFDbf, cTtl, aSay, nRec, aStruct, cAls, i
   LOCAL uTyp, xVal, nI, cFld, oCol, cTyp, cDbf, cPath
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg, cRdd

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Наименование колонок может быть больше 10 символов, DBF этого НЕ ПОДДЕРЖИВАЕТ !
   // Attention! Column names can be longer than 10 characters, DBF DOES NOT SUPPORT this !
   aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
      FOR i := 1 TO Len(aStruct)
         IF AT(" ", aStruct[ i ][ 1 ] ) > 0
            aStruct[ i ][ 1 ] := ATREPL( " ", aStruct[ i ][ 1 ], "_" )
         ENDIF
         IF aStruct[ i ][ 2 ] != "N"
            aStruct[ i ][ 4 ] := 0
         ELSEIF aStruct[ i ][ 2 ] == "N"
            aStruct[ i ][ 4 ] := Set( _SET_DECIMALS )
         ENDIF
         IF aStruct[ i ][ 2 ] == "I"
            aStruct[ i ][ 2 ] := "N"
            aStruct[ i ][ 3 ] += 2
            aStruct[ i ][ 4 ] := 0
         ENDIF
         IF aStruct[ i ][ 2 ] == "V"
            aStruct[ i ][ 2 ] := "M"
            aStruct[ i ][ 3 ] := 10
            aStruct[ i ][ 4 ] := 0
         ENDIF
      NEXT
   //?v aStruct
   cForm     := oWnd:Name
   nRec      := oBrw:Cargo:nTotalRecno

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
   cRdd  := "DBFCDX"
   aSay  := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFile )
   dbCreate(cFile, aStruct, cRdd)
   cAls := "TEMP"
   //------------- создать/открыть dbf для показа ------------
   USE ( cFile ) VIA "DBFCDX" ALIAS &cAls NEW CODEPAGE cCdPg SHARED

   oBrw:GoTop()
   oBrw:lEnabled := .F.  // блокировать область таблицы (Строки не отображаются)

   DBSELECTAREA(oBrw:cAlias)
   dbGoTop()

   DO WHILE !EOF()
      DBSELECTAREA(cAls)
      APPEND BLANK
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cFld := oCol:cName //oCol:cField
          cTyp := oCol:cFieldTyp
          cDbf := FIELDNAME( FIELDPOS(cFld) )
          IF EMPTY(cDbf)
             cDbf := ATREPL( " ", SUBSTR(cFld, 1, 10), "_" )
          ENDIF
          //? nI, "cFld=", cFld, cTyp, "cDbf=", cDbf
          IF cFld == "SELECTOR" .OR. cFld == "ORDKEYNO"
          ELSE
             xVal := oBrw:GetValue(cFld)
             //?? "[", xVal, "]"
             uTyp := FIELDTYPE( FIELDPOS(cFld) )
             //?? uTyp
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
      DBSELECTAREA(oBrw:cAlias)
      dbSkip()
   ENDDO
   dbGoTop()

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

////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteXls(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFXls, cTtl, aSay, nRec, cPath
   LOCAL lActivate, lSave, cTtlXls, aTitle, hFont, bExtrnXls, aColSel, bPrintRow
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // Внимание ! Выгружать больше 65533 строк в Excel НЕЛЬЗЯ ! Ограничение Excel 2003.
   // Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.

   cForm     := oWnd:Name
   nRec      := oBrw:Cargo:nTotalRecno
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
   DBSELECTAREA(oBrw:cAlias)
   dbGoTop()

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

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteOOCalc(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFOds, cTtl, aSay, nRec, cPath, aFont, aRet, cTitle
   LOCAL lSave, aTitle, aImage, hProgress, bExternCalc, cLng, cLn2, cLn3, n1, n2
   LOCAL lOpen, cCdPg, aTsb, aCalcParam, aCalcTitle, aCalcFoot, aColor, tTime

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   cForm  := oWnd:Name
   nRec   := oBrw:Cargo:nTotalRecno

   IF App.Cargo:cLang == "RU"
      cTtl := 'Преобразование в ODS'
      cMsg := "Всего записей в таблице = " + HB_NtoS( nRec )
      cLng := "Файл успешно создан!;"
      cLn2 := "Открыть этот файл ?;"
      cLn3 := "Файл НЕ создан !;"
   ELSE
      cTtl := 'Convert to ODS'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   // доп.параметры для OO-Calc
   aFont  := {"DejaVu Sans Mono", 11 }                 // задать фонт таблицы для Calc
   aTitle := {}
   cTitle := cTtl
   aFont  := { "Comic Sans MS", 24, .f. , .f. }
   aColor := {BLACK,WHITE}                             // цвет/фон ячеек
   n1     := 1                                         // начало строки
   n2     := 0                                         // 0-объединить строку до конца таблицы
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {} )                                  // разделительная строка
   lSave     := .T.                                    // сохранить файл

   oWnd:Cargo:aFileExport := {}                           // вернуть парметры экспорта

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F.)   // форма-запроса .F.-без смены кодировки

   aRet := oWnd:Cargo:aFileExport
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]    // открыть после записи
   HB_FileDelete( cFile )

   aSay := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   tTime       := HB_DATETIME()
   aTsb        := myGetTsbContent(oBrw)                // содержание таблицы
   aCalcParam  := { cFile, .F., lSave, aFont }         // параметры для Calc
   aCalcTitle  := aTitle                               // заголовок Calc
   aCalcFoot   := {}                                   // подвал Calc
   aImage      := {}
   hProgress   := NIL         // хенд для ProgressBar на другой форме
   bExternCalc := NIL         // для варианта-2

   DBSELECTAREA(oBrw:cAlias)
   oBrw:GoTop()          // Экспорт идёт с текущей позиции курсора
   oBrw:lEnabled := .F.  // блокировать область таблицы (Строки не отображаются)

   Brw7OleCalc( aTsb, aCalcParam, aCalcTitle, aCalcFoot, aImage, hProgress, bExternCalc )

   oBrw:lEnabled := .T.    // разблокировать область таблицы (Строки отображаются)
   oBrw:Display()
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   WaitWindow()

   cFOds := cFileNoPath(cFile)
   cPath := cFilePath(cFile)
   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFOds + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFOds + ";" + cPath
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFOds
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

