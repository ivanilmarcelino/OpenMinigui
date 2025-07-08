/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Функции работы Dbf-файлом / Functions of working with a Dbf file
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "dbinfo.ch"
#include "set.ch"
#include "hbsix.ch"
#include "error.ch"

Static bStaticErrStd := {|oE| if(oE:GenCode==5, 0, Break(oE))}  //  глобальный обработчик ошибок при USE
////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDriverDbf()
   LOCAL aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, cTypeRes, aFontExt
   LOCAL cMsg, cVal

   cForm := ThisWindow.Name
   IF App.Cargo:cLang == "RU"
      cMsg := "Драйвер БД"
   ELSE
      cMsg := "DB Driver"
   ENDIF

   aDim  := {}  //   1               2                                                   3     4      5            6
   AADD( aDim, { "iDbf48"    , cMsg + ": DBFCDX - memo *.fpt (FoxPro compatible)"     , .F. , "", "DBFCDX"       , 1 } )
   AADD( aDim, { "iDbf48"    , cMsg + ": DBFNTX - memo *.dbt (Clipper compatible)"    , .F. , "", "DBFNTX"       , 2 } )
   AADD( aDim, { "iDbfPass48", cMsg + ": DBFNSX - memo *.smt (Six compatible)"        , .F. , "", "DBFNSX"       , 3 } )
   AADD( aDim, { "iDbf48"    , cMsg + ": SIXCDX - memo *.fpt/smt (FoxPro compatible)" , .F. , "", "SIXCDX"       , 4 } )
   AADD( aDim, {                                                                                                     } )
   AADD( aDim, { "iDbf48"    , cMsg + ": BMDBFCDX - bitmap filter,compatible with dbfcdx", .F. , "", "BMDBFCDX"     , 5 } )
   AADD( aDim, { "iDbf48"    , cMsg + ": BMDBFNTX - bitmap filter,compatible with dbfntx", .F. , "", "BMDBFNTX"     , 6 } )
   AADD( aDim, { "iDbfPass48", cMsg + ": BMDBFNSX - bitmap filter,compatible with dbfnsx", .F. , "", "BMDBFNSX"     , 7 } )
   AADD( aDim, { "iDbf48"    , cMsg + ": BM_DBSEEKWILD - bitmap filter"                  , .F. , "", "BM_DBSEEKWILD", 8 } )

   //SetThemes(2)  // тема "Office 2000 theme" в ContextMenu
   //SetThemes(3)  // тема "Dark theme" в ContextMenu
   aRet     := {}
   nPos     := 3
   cTypeRes := "ICO" // "BMP"
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize
   aFontExt := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, cTypeRes )

   IF nChoice > 0
      cVal := SUBSTR(aDim[nChoice,2], 1, AT("-",aDim[nChoice,2]) - 1 )
      aRet := { aDim[nChoice,6], aDim[nChoice,5], cVal }
   ENDIF
   DO EVENTS

RETURN aRet

/////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myCodePageDbf()
   LOCAL nI, aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, aFntExt, cTypeRes
   LOCAL cMsg, cVal, cVal2, cVal3, lView, a2Dim

   a2Dim := myCodePage()
   cForm := ThisWindow.Name
   IF App.Cargo:cLang == "RU"
      cMsg := "Кодовая страница: "
   ELSE
      cMsg := "Code page: "
   ENDIF

   aDim  := {}   //   1           2               3     4       5           6
   FOR nI := 1 TO LEN(a2Dim)
       cVal  := a2Dim[nI,1]
       cVal2 := a2Dim[nI,2]
       lView := IIF( LEN(cVal2) > 0 , .F. , .T. ) 
       cVal3 := IIF( LEN(cVal2) > 0 , " - " + cVal2, " - connect language" ) 
       AADD( aDim, { "", cMsg + cVal + cVal3 , lView , "", cVal , nI } )
   NEXT

   //SetThemes(2)  // тема "Office 2000 theme" в ContextMenu
   //SetThemes(3)  // тема "Dark theme" в ContextMenu
   aRet     := {}
   nPos     := 3
   cTypeRes := "NONE" // ICO" // "BMP"
   nBmpSize := 0
   nFSize   := App.Cargo:nFontSize - 2
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFntExt, cTypeRes )
   IF nChoice > 0
      cVal := SUBSTR(aDim[nChoice,2], 1, AT("-",aDim[nChoice,2]) - 1 )
      aRet := { aDim[nChoice,6], aDim[nChoice,5], cVal }
   ENDIF
   DO EVENTS

RETURN aRet

/////////////////////////////////////////////////////////////////////////////
// Как узнать в каком режиме открыта база - SHARED/EXCLUSIVE ?
FUNCTION my_UseMode()
   LOCAL cMsg

   IF dbInfo( DBI_SHARED )
      // use ... shared
      cMsg := "SHARED"
   ELSE
      cMsg := "EXCLUSIVE"
   ENDIF

RETURN cMsg

//////////////////////////////////////////////////////////////////////////////
// Список открытых БД / List of open databases
FUNCTION myDbGetAllUse()
   LOCAL nI, cMsg, aAlias := {}, aSelect := {}, aRdd := {}

   hb_waEval( {|| AADD(aAlias, Alias())} )
   hb_waEval( {|| AADD(aSelect, Select())} )
   hb_waEval( {|| AADD(aRdd, RddName())} )

   cMsg := "Список открытых БД / List of open databases:" + CRLF
   FOR nI := 1 TO LEN(aAlias)
       cMsg += "Select: " + HB_NtoS(aSelect[nI])
       cMsg += ",  Alias: " + aAlias[nI]
       cMsg += " ,  RddName: " + aRdd[nI]
       cMsg += " , " + (aAlias[nI])->( DBINFO(DBI_FULLPATH) ) + CRLF
   NEXT
   cMsg += REPL(";",10)

   AlertInfo( cMsg, "", "iDbC64x1", 64, {RED} )

RETURN cMsg

//////////////////////////////////////////////////////////////////////////////
// Список открытых индексов / List of open indexes
FUNCTION myGetIndexUse()
   LOCAL nI, nTags, cOrd, cFor, nOrder, cOrder, cMsg

   cMsg   := "Список открытых индексов / List of open indexes:" + CRLF
   cMsg   += (ALIAS())->( DBINFO(DBI_FULLPATH) ) + CRLF
   cMsg   += "Alias: " + ALIAS()  + CRLF + CRLF
   nOrder := INDEXORD()
   cOrder := ( ALIAS() )->( ordKey( nOrder ) )
   nTags  := ( ALIAS() )->( ordCount() )
   FOR nI := 1 TO nTags
      cOrd := ( ALIAS() )->( ordName( nI ) )
      cFor := ( ALIAS() )->( ordKey( nI ) )
      cMsg += SPACE(5) + "DbSetOrder(" + HB_NtoS(nI) + ") " + cOrd + " - " + cFor + CRLF
   NEXT
   cMsg += CRLF
   cMsg += "Current index: " + HB_NtoS(nOrder) + " - " + cOrder + CRLF
   DbSetOrder(nOrder)

RETURN cMsg

////////////////////////////////////////////////////////////////
FUNCTION myDbStructure(cTitle)        // Структура этой базы
   LOCAL cMsg, cFile, cFTxt, cAls, aStru, nI, aVal, cTxt

   cAls  := Alias()
   aStru := DbStruct()
   cFile := DBINFO( DBI_FULLPATH )
   cFTxt := ChangeFileExt( cFile, '.txt' )

   IF App.Cargo:cLang == "RU"
      cMsg := "Открыть базу данных - псевдоним: " + cAls + " RddName: " + RddName() + ";"
      cMsg += "Путь к базе данных - " + cFile + ";"
      cMsg += "Файл с базовой структурой - " + cFTxt + ";;"
      cMsg += " ФАЙЛ: " + cFileNoPath( cFile ) + ";;"
   ELSE
      cMsg := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
      cMsg += "Path to the database - " + cFile + ";"
      cMsg += "File with base structure - " + cFTxt + ";;"
      cMsg += "     FILE: " + cFileNoPath( cFile ) + ";;"
   ENDIF

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += "   " + HB_NtoS(nI) + ". "
      cMsg += IIF( nI < 10, " ", "" )
      cMsg += PADR(aVal[1],13)
      cMsg += PADR(aVal[2],3) + PADL( HB_NtoS(aVal[3]), 5 )
      cMsg += " " + PADL( HB_NtoS(aVal[4]), 3 ) + ";"
   NEXT

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
   cMsg += CRLF

   AlertInfo( cMsg, cTitle, "iDbC64x1", 64, {RED} )

   cMsg := AtRepl( ";", cMsg, CRLF )
   cMsg := AtRepl( "|", cMsg, ";"  )
   cMsg += CRLF + MiniGuiVersion() + CRLF + ">>"
   HB_MemoWrit(cFTxt, cMsg)

   IF App.Cargo:cLang == "RU"
      cMsg := "Файл успешно создан!;"
      cMsg += cFTxt + ";;"
      cMsg += "Открыть этот файл?;"
   ELSE
      cMsg := "File created successfully !;"
      cMsg += cFTxt + ";;"
      cMsg += "Open this file ?;"
   ENDIF

   IF AlertYesNo(cMsg, , , "iDbC64x1", 64, {LGREEN,RED} )
      ShellExecute( 0, "Open", cFTxt,,, 3 )
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexesThis(cTitle,cParRet)
   LOCAL nI, nSel, nOrder, cAlias, cIndx, aIndx, xRet, cMsg, cVal, aOrdN
   LOCAL xSc2, xSc1, cZn
   DEFAULT cTitle := "Info index", cParRet := "SAY"

   cAlias := ALIAS()
   nSel   := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, cTitle )
      RETURN NIL
   ENDIF

   nOrder := INDEXORD()
   cMsg   := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + ";"
   cMsg   += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cMsg   += "Open indexes: "
   aIndx  := {}
   aOrdN  := {}

   
   IF OrdCount() == 0 //nOrder == 0
      cMsg += " (no) !;;"
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' );;'
      FOR nI := 1 TO 200
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += STR(nI,3) + ')  Index file: ' + DBORDERINFO(DBOI_FULLPATH) + ";"
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ");"
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '" ;'
            cMsg += '       FOR index: "' + OrdFor() + '" ' + SPACE(3)
            //cMsg += 'DBOI_ISCOND: "' + cValToChar(DBORDERINFO(DBOI_ISCOND)) + '" ' + SPACE(5)
            cMsg += 'DESCENDing: ' + cValToChar(DBORDERINFO(DBOI_ISDESC)) + SPACE(3)
            cMsg += 'UNIQUE: ' + cValToChar(DBORDERINFO(DBOI_UNIQUE)) + SPACE(3)+ ';'
            xSc2 := DbOrderInfo( DBOI_SCOPEBOTTOM )
            xSc1 := DbOrderInfo( DBOI_SCOPETOP )
            cZn  := '"'
            IF VALTYPE(xSc1) # "C"
               xSc1 := cValToChar(xSc1)
               xSc2 := cValToChar(xSc2)
               cZn  := ''
            ENDIF
            IF LEN(xSc1) > 0 .OR. LEN(xSc2) > 0
               cMsg += '    SET SCOPE TO: ' + cZn +  xSc1 + cZn + ' , ' + cZn + xSc2 + cZn + ';'
            ENDIF
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT)) + ' );;'
            //cVal := STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI)
            cVal := "(" + HB_NtoS(nI) + "), OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]"
            cVal += " , DESCENDing: " + cValToChar(DBORDERINFO(DBOI_ISDESC))
            cVal += " , UNIQUE: " + cValToChar(DBORDERINFO(DBOI_UNIQUE))
            IF LEN(xSc1) > 0 .OR. LEN(xSc2) > 0
               cVal += '    SET SCOPE TO: ' + cZn +  xSc1 + cZn + ' , ' + cZn + xSc2 + cZn + ';'
            ENDIF
            AADD( aIndx, cVal )
            AADD( aOrdN, OrdName(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) // переключить на основной индекс
      cMsg += REPL("-",60) + ";"
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS() + ";"
   ENDIF
   cMsg += "Number of records by index DBOI_KEYCOUNT(?) = " + HB_NtoS(ORDKEYCOUNT()) + ";"
   cMsg += REPL("; ",30)

   IF cParRet == "ADIM"
      xRet := { aIndx, aOrdN }
   ELSEIF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, "iDbC64x1", 64, {RED} )
   ELSEIF cParRet == "LOG"
      xRet := HB_NtoS(nOrder)+" , OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]  "
      xRet += ", DESCENDing: " + cValToChar(DBORDERINFO(DBOI_ISDESC))
      xRet += ", UNIQUE: " + cValToChar(DBORDERINFO(DBOI_UNIQUE))
      xRet += ", DBOI_KEYCOUNT() = " + HB_NtoS(ORDKEYCOUNT())
   ELSE
      xRet := cMsg
   ENDIF

RETURN xRet

//////////////////////////////////////////////////////////////
// Открыть таблицу или DBF
// Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
FUNCTION myUse2Area( cDbf, cAls, lShared, cRdd, cCdp, cPsw, nWhl )
   LOCAL lRet := .T., cPth, cFil, cExt, cMsg, aParams
   Default cAls := cPsw := ""
   Default lShared := .T.
   Default cRdd := RddSetDefault()
   Default cCdp := "RU866"
   DEFAULT nWhl := 10

   aParams := hb_aParams()
   //? ProcNL()
   //? "aParams=", aParams ; ?v aParams
   If !hb_vfExists(cDbf)
      cMsg := "No database file !;" + cDbf + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF

   NetErr( .F. )

   // убрал 24.07.24 - сбой на таких именах: oborud.ru866.test.dbf
   cPth := cFil := cExt := ""
   //hb_FNameSplit(cDbf, @cPth, @cFil, @cExt)
   //cDbf := hb_FNameMerge(cPth, cFil, "")

   IF LEN(cAls) == 0
      cAls := cFileNoPath(cDbf)
      cAls := SUBSTR(cAls,1,AT(".",cAls)-1)
   ENDIF

   IF SELECT(cAls) > 0
      cMsg := "There is already such an alias !;"
      cMsg += "ALIAS()=" + cAls + ";" + cDbf + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF

   SELECT 0
   If     empty (cAls)    ; cAls := '_XYZ_'+hb_ntos(select())
   ElseIf select(cAls) > 0; cAls += '_'    +hb_ntos(select())
   EndIf
   // убираем в алиасе знаки
   //cAls := CharRepl('()|-',cAls,"____") 
   //
   DO WHILE nWhl-- > 0
      lRet := .F.
      IF cRdd == "DBFNSX" .OR. cRdd == "BMDBFNSX"
         BEGIN SEQUENCE WITH { |e|break(e) }
            IF lShared
               USE (cDbf) VIA (cRdd) ALIAS &cAls NEW CODEPAGE cCdp PASSWORD cPsw SHARED
            ELSE
               USE (cDbf) VIA (cRdd) ALIAS &cAls NEW CODEPAGE cCdp PASSWORD cPsw EXCLUSIVE
            ENDIF
            lRet := ! NetErr() .and. Used()
         END SEQUENCE
      ELSE
         BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
            DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
            lRet := ! NetErr() .and. Used()
         END SEQUENCE
      ENDIF
      IF lRet; EXIT
      ENDIF
      wApi_Sleep(100)
   ENDDO
   // ^^^ - цикл главное, т.к. из за сбоя сети пробуем несколько раз открыть
   IF lRet
      dbGoTop()
   ENDIF
   //? ProcNL(), ALIAS(), cAls, "ОТКРЫТА БАЗА=", lRet, Used()

RETURN lRet

//////////////////////////////////////////////////////////////
// Открыть таблицу или DBF
// Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
FUNCTION myUse3Area( cDbf, cAls, lShared, cRdd, cCdp, cPsw, nWhl )
   LOCAL lRet := .T., cMsg, cLng, aParams, ao, lRus
   LOCAL cErr, oErr, bErr, msgErrOpen, msgErrIndx
   Default cAls := cPsw := ""
   Default lShared := .T.
   Default cRdd := RddSetDefault()
   Default cCdp := "RU866"
   DEFAULT nWhl := 10

   lRus  := .T.
   ao    := App.Cargo
   IF !ISOBJECT(ao)
      lRus := IIF( ao:cLang == "RU", .T., .F.)
   ENDIF
   aParams := hb_aParams()
   //? ProcNL()
   //? "aParams=", aParams ; ?v aParams
   If !hb_vfExists(cDbf)
      cLng := IIF( lRus, "Нет файла БД !", "No database file !" ) 
      cMsg := cLng + ";" + cDbf + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF

   NetErr( .F. )

   IF LEN(cAls) == 0
      cAls := cFileNoPath(cDbf)
      cAls := SUBSTR(cAls,1,AT(".",cAls)-1)
   ENDIF
   // убираем в алиасе знаки
   cAls := CharRem('()|-.',cAls)          // убираем в алиасе знаки

   IF SELECT(cAls) > 0
      cLng := IIF( lRus, "Такой АЛИАС уже есть", "There is already such an alias" ) 
      cMsg := cLng + " !;"
      cMsg += "ALIAS()=" + cAls + ";" + cDbf + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF

   SELECT 0
   If     empty (cAls)    ; cAls := '_XYZ_'+hb_ntos(select())
   ElseIf select(cAls) > 0; cAls += '_'    +hb_ntos(select())
   EndIf
   //
   lRet  := .F.

   msgErrOpen := IIF( lRus, "Ошибка открытия файла !", "Error opening file!" ) 
   msgErrIndx := IIF( lRus, "Открыть без индекса ?", "Open without index?" ) 

   DO WHILE nWhl-- > 0

      IF cRdd == "DBFNSX" .OR. cRdd == "BMDBFNSX"
         BEGIN SEQUENCE WITH { |e|break(e) }
            IF lShared
               USE (cDbf) VIA (cRdd) ALIAS &cAls NEW CODEPAGE cCdp PASSWORD cPsw SHARED
            ELSE
               USE (cDbf) VIA (cRdd) ALIAS &cAls NEW CODEPAGE cCdp PASSWORD cPsw EXCLUSIVE
            ENDIF
            lRet := ! NetErr() .and. Used()
         END SEQUENCE
      ELSE
         bErr := ErrorBlock(bStaticErrStd) 
         Begin sequence
            DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
            // ^^^^^^^^^ - use dbf как SET AUTOPEN ON и EXCLUSSIVE для DBFСВX
            lRet := ! NetErr() .and. Used()
         Recover using oErr 
            // обработка возникшей ошибки, данные о ней в oErr
            //? oErr, oErr:description, oErr:operation, oErr:genCode, EG_NOFUNC, EG_NOVAR, EG_OPEN
            cErr := msgErrOpen + ';' + cDbf + ';' + oErr:description 
            cErr += if(!Empty(oErr:operation),';'+oErr:operation,'') 
            //? cErr
            if (oErr:genCode == EG_NOFUNC .or. oErr:genCode == EG_NOVAR) 
                IF AlertYesNo(cErr + ';;' + msgErrIndx, , ,, 64 , {LGREEN,RED} )
                   Set(_SET_AUTOPEN, .F.)   // как SET AUTOPEN OFF - без индексов
                   DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
                   lRet := ! NetErr() .and. Used()
                   Set(_SET_AUTOPEN, .T.)  // восстановим значение
                ENDIF
             else 
                AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
             endif 
         end sequence
         ErrorBlock(bErr) 
      ENDIF
      IF lRet; EXIT
      ENDIF
      wApi_Sleep(100)
   ENDDO
   // ^^^ - цикл главное, т.к. из за сбоя сети пробуем несколько раз открыть
   IF lRet
      dbGoTop()
   ENDIF
   //? ProcNL(), ALIAS(), cAls, "ОТКРЫТА БАЗА=", lRet, Used()

RETURN lRet

////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDriverHelp()
   LOCAL cMsg, cTtl

   IF App.Cargo:cLang == "RU"
      cTtl := "Помощь"
      cMsg := "Драйвера БД: DBFCDX/DBFNTX/DBFNSX/SIXCDX в Harbour;;"
      cMsg += "Все эти драйвера унифицированы в HARBOUR, т.е. сделан общий код для всех RDD.;"
      cMsg += "SIXCDX – это слегка модифицированный DBFCDX, о котором не стоит говорить.;;"
      cMsg += "DBT, FMT, SPT — это разные форматы MEMO.;Все они поддерживаются Harbor и автоматически распознаются при открытии файла DBF.;"
      cMsg += "NTX, CDX и NSX — это разные форматы индексов. Их можно использовать в любых комбинациях форматов MEMO,;"
      cMsg += "т.е. DBFCDX прекрасно работает с файлами мемо DBT так же, как с файлами FPT и SMT.;;"
      cMsg += "В Harbour все основные RDD, использующие вышеуказанные форматы индексов (DBFNSX, DBFCDX, DBFNSX),;"
      cMsg += "имеют почти одинаковую функциональность, которая охватывает почти все функции индексов, известные в мире xbase,;"
      cMsg += "и многие из них уникальны только для [x]Harbour, поэтому они не поддерживаются.;;"
      cMsg += "Со всеми вышеперечисленными RDD пользователь может использовать все функции ord*(), db*(), sx_*(), hsx_*() ;"
      cMsg += "может создавать многотеговые индексы (много индексов в одном файле, также для формата NTX),;"
      cMsg += "autooreder, автооткрытие, произвольные индексы и т. д.;"
      cMsg += "Поэтому для программиста, используемого RDD, не должно создавать никакой разницы. ;;"
      cMsg += "Также можно включить отключение некоторых функций с помощью интерфейса RDDI_*, т. е. этот код меняет;"
      cMsg += "поведение DBFNTX по умолчанию, поэтому он ведет себя точно так же, как DBFCDX, и даже использует «.cdx» в качестве;"
      cMsg += "расширения файла по умолчанию (конечно, внутри это все еще;формат NTX с расширениями Harbour – поддержка формата CTX от CLIP)."
      cMsg += ";;Драйвера БД: BMDBFNTX/BMDBFCDX/BMDBFNSX/BM_DBSEEKWILD - это альтернативная реализация драйверов базы,; учитывающая наследование RDD."
      cMsg += ";Смотреть http://github.com/harbour/core/archive/master.zip - \contrib\rddbm;"
   ELSE
      cTtl := "Help"
      cMsg := "DB drivers: DBFCDX/DBFNTX/DBFNSX/SIXCDX in Harbour;;"
      cMsg += "All these drivers are unified in HARBOUR, i.e. a common code is made for all RDDs.;"
      cMsg += "SIXCDX – it’s slighlt modified DBFCDX not worth to talk about.;"
      cMsg += "DBT, FMT, SPT are different MEMO formats.;All of them are supported by Harbour and automatically recognized when DBF file is open.;"
      cMsg += "NTX, CDX and NSX are different index formats.;They can be used in any combinations of MEMO formats,;i.e. DBFCDX perfectly well works with DBT memo files just like with FPT and SMT ones.;;"
      cMsg += "In Harbour all core RDDs using above index formats (DBFNSX, DBFCDX, DBFNTX) have nearly;the same functionality which cover nearly all index feautres known in xbase world and many of them are unique;to [x]Harbour only so they are not supported by other drivers.;;"
      cMsg += "With all above RDDs user can use all ord*(), db*(), sx_*(), hsx_*() , … functions,;can create multitag indexes (many orders in single file, also for NTX format),;autooreder, autoopen, production indexes, etc. so for programmer used RDD should not create any difference.;"
      cMsg += "It’s also possible to enable disable some features using RDDI_* interface, i.e. this code change default DBFNTX; behavior so it behaves just like DBFCDX and even uses “.cdx” as default file extnesion;(of course internally it’s still NTX format with Harbour extenssions – we support CTX format from CLIP)"
      cMsg += ";;DB drivers: BMDBFNTX/BMDBFCDX/BMDBFNSX/BM_DBSEEKWILD is an alternative;implementation of database drivers that takes into account RDD inheritance."
      cMsg += ";View http://github.com/harbour/core/archive/master.zip - \contrib\rddbm;"
   ENDIF

   AlertInfo( cMsg, cTtl, "2MG_64", 64, {RED}, .T. , , .F. )

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////////
// Преобразование АЛИАСА базы
FUNCTION mySetAlias(cStr)

    //IF LEN(cStr) > 12
    //   cStr := SUBSTR(cStr,1,12)
    //ENDIF

    IF IsRusChar(cStr)
       cStr := TranRusLat(cStr)
    ENDIF

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////////////
// Преобразование клавиши к верхнему регистру латинского алфавита
STATIC FUNCTION TranRusLat(cStr)
   cStr := UPPER(cStr)
   cStr := CharRepl('АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЬЭЮЯ',cStr,;
                    "ABVGDEJZIIKLMNOPRSTUFXCCHHY_EUI")
   cStr := CHARREM(",;.-",cStr) 
Return cStr

//////////////////////////////////////////////////////////////////////////////////////////
// проверка на русские буквы
FUNCTION IsRusChar(cStr)
   LOCAL lRet := .F., nI, cBukva

   FOR nI := 1 TO LEN(cStr)
      cBukva := SUBSTR(cStr,nI,1)
      IF ASC(cBukva) > 127
         lRet := .T.
         EXIT
      ENDIF
   NEXT

RETURN lRet

