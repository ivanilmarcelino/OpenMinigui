/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Верхнее меню окна с кнопками / Top window menu with buttons
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION TopMenuViewButtons(owc)
   LOCAL nW, nH, nX, nY, nG2, hFont, aFont, cFont, nFSize, lBold, aImg, aObj
   LOCAL nWBtn, nHBtn, cCap, nWtxt, nWCap, cObj, aCap, cForm, aBtnObj, i, o
   LOCAL aIco, aHIco, bAct, aBtnFClr, aBtnBClr, aBtnFont, cTxt, nHIco, aBtn
   LOCAL aBColor

   ? ProcNL(), "oWC=", oWC

   cForm    := oWC:cForm                   // имя окна
   hFont    := GetFontHandle('ItalBold')
   aFont    := GetFontParam(hFont)
   cFont    := aFont[1]
   nFSize   := aFont[2]
   lBold    := aFont[3]
   aBColor  := owc:aBColor
   aBtnFClr := { WHITE   , BLACK }
   aBtnBClr := { aBColor , WHITE }
   aBtnFont := { cFont, nFSize, .T. }
   nHIco    := 32          // 32,55  - задаём размер картинки на кнопке
   nG2      := 5
   aBtnObj  := {}

   IF App.Cargo:aDisplayMode[2] <= 720
      nHIco  := 28
   ENDIF

   // имя объекта + имя события
   aObj  := { "_ATable" , "_AFilter", "_AExport", "_AExit"  }
   aImg  := { {"iADbase32x1","iADbase32x2"} , {"iAFilter32x1","iAFilter32x2"} , {"iAExport32x1","iAExport32x2"} , {"iAExit32x1","iAExit32x2"} }
   owc:aTopMenuRu := { "Таблица" , "Фильтр" , "Экспорт", "Выход", "Очистка" }
   owc:aTopMenuEn := { "Table"   , "Filter" , "Export" , "Exit" , "Clean"   }
   aCap := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRu, owc:aTopMenuEn )
   // смена меню
   owc:aMn2Text   := IIF( App.Cargo:cLang == "RU", { "Фильтр" , "Очистка" }, { "Filter", "Clean" } )

   // расчёт по тексту
   nWtxt  := nW := nH := 0
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHIco, nHIco, nWTxt )   // nHImg-высота bmp
   nWBtn := nWTxt + 20                          // ширина кнопки
   nHBtn := nHIco + 5 + nFSize + 5 + 20         // высота кнопки

   nY := nG2
   nX := owc:nG
   FOR i := 1 TO LEN(aObj)

      cObj  := aObj[i]    // контрол на окне
      cTxt  := aCap[i]
      aIco  := aImg[i]
      aBtn  := { cObj, cTxt, aIco[1], aIco[2], nHIco, aBtnFClr, aBtnFont, "" }
      //bAct  := { || _wPostaObj[i]  // событие на форме
      ahIco := my2DrawButton(nY, nX, nWBtn, nHBtn, aBtn, bAct, aBtnBClr )
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
      o:nBtn := i   ; o:cImage := aImg[i]   // пример
      o:Post := cObj                        // событие на форме

      This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:Post, , This.Name) }

      AADD( aBtnObj, { i, cObj, "-имя объекта", aCap[i], 0, nW, This.&(cObj).Width, nHBtn, cObj, "-событие", This.&(cObj).Cargo } )

      nX += This.&(cObj).Width + nG2

   NEXT

   nW := nX + nG2 // + owc:nG
   owc:nWEndTB := nW              // конец кнопок
   owc:nHTBar  := nHBtn + nG2*2   // высота ToolBar
   owc:aBtnObj := aBtnObj         // массив кнопок на форме

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION myTableStruct(oWnd,ky,cn,oBrw)        // Структура этой таблицы
   LOCAL cMsg, cFile, cFTxt, cTitle, aStru, nI, aVal, cTxt, aMsg, obc

   ? ProcNL(), oWnd:Name,ky,cn,oBrw:cAlias
   cTitle := This.&(cn).Caption
   obc    := oBrw:Cargo
   aStru  := obc:aStruct                             // структура базы для экспорта
   cFile  := obc:cFile + "." + obc:cTable            // путь к файлу + имя таблицы
   cFTxt  := cFile + '.txt'
   // ручная правка структуры 
   FOR nI := 1 TO Len(aStru)
      IF AT(" ", aStru[ nI ][ 1 ] ) > 0
         aStru[nI][ 1 ] := ATREPL( " ", aStru[nI][ 1 ], "_" )
      ENDIF
      IF aStru[nI][ 2 ] != "N"
         aStru[nI][ 4 ] := 0
      ELSEIF aStru[nI][ 2 ] == "N"
         aStru[nI][ 4 ] := Set( _SET_DECIMALS )
      ENDIF
      IF aStru[nI][ 2 ] == "I"
         aStru[nI][ 2 ] := "N"
         aStru[nI][ 3 ] += 2
         aStru[nI][ 4 ] := 0
      ENDIF
      IF aStru[nI][ 2 ] == "V"
         aStru[nI][ 2 ] := "M"
         aStru[nI][ 3 ] := 10
         aStru[nI][ 4 ] := 0
      ENDIF
   NEXT

   IF App.Cargo:cLang == "RU"
      aMsg := {"Таблица: ", "ФАЙЛ: ", "Путь к базе данных - ",;
               "Файл успешно создан!;", "Открыть этот файл ?;" }
   ELSE
      aMsg := {"Table: ", "FILE: ", "Database path - " ,;
               "File created successfully !;", "Open this file ?;" }
   ENDIF
   cMsg := aMsg[1] + obc:cTable + ";" + aMsg[2] + cFileNoPath(obc:cFile)
   cMsg += ";" + aMsg[3] + cFilePath( obc:cFile ) + ";;"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += "   " + HB_NtoS(nI) + ". "
      cMsg += IIF( nI < 10, " ", "" )
      cMsg += PADR(aVal[1],13)
      cMsg += PADR(aVal[2],3) + PADL( HB_NtoS(aVal[3]), 5 )
      cMsg += " " + PADL( HB_NtoS(aVal[4]), 3 ) + ";"
   NEXT

   cMsg += CRLF
   cMsg += REPL("--",40) + ";"
   cMsg  += ";;{"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += IIF( nI == 1 , "", " " )
      cMsg += HB_ValToExp(aVal)
      cMsg += IIF( nI == LEN(aStru) , " ", "," ) + " | ;"
   NEXT
   cMsg += "}; "

   cMsg += CRLF
   cMsg += REPL("--",40) + ";"
   cMsg += SPACE(3) + "aDbf := {};"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cTxt := aVal[1] + '"'
      cMsg += SPACE(3) + 'AADD( aDbf, { "' + PADR(cTxt, 15)
      cMsg += ',' + PADR('"' + aVal[2] + '"',6) + ',' + STR(aVal[3],4)
      cMsg += ',' + STR(aVal[4],2) + ', "Not-show" , .T. } );'
   NEXT
   cMsg += REPL(";",8)

   AlertInfo( cMsg, cTitle, "iADbase32x1", 64, {RED} )

   cMsg := AtRepl( ";", cMsg, CRLF )
   cMsg := AtRepl( "|", cMsg, ";"  )
   cMsg += CRLF + MiniGuiVersion() + CRLF + ">>"
   HB_MemoWrit(cFTxt, cMsg)

   cMsg := aMsg[4] + ";" + cFTxt + ";;" + aMsg[5]

   IF AlertYesNo(cMsg, , , "iADbase32x2", 64, {LGREEN,RED} )
      ShellExecute( 0, "Open", cFTxt,,, 3 )
   ENDIF

RETURN NIL

