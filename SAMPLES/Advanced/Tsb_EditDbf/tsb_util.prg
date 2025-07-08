/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Разные функции для редактирований ячеек таблицы
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsb_mydef.ch"

///////////////////////////////////////////////////////////////////////////////
FUNCTION myVal2Str(xVal)
   LOCAL cRet := ""

   IF xVal == NIL
      cRet := "Nil"
   ELSEIF IsString(xVal)
      cRet := xVal
   ELSEIF IsArray(xVal)
      cRet := HB_ValToExp(xVal)
   ELSEIF IsObject(xVal)
      cRet := _o2log(xVal, 17, "Object: ", .T. , .T.)
      cRet := ALLTRIM(cRet)
   ELSE
      cRet := cValToChar(xVal)
   ENDIF

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myMacro(cRun,lDebug)
   LOCAL lRsc, oError, cRet
   DEFAULT lDebug := .F.
   
   IF !lDebug
      BEGIN SEQUENCE  WITH { |e|break( e ) }
         cRet := &(cRun)
         lRsc := .T.
      RECOVER USING oError
         lRsc := .F.
      END SEQUENCE
      // результат обработки макро
      IF !lRsc
         cRet := "ERROR! macro= [" + cRun + "]"
      ENDIF
   ELSE
      // отладка
      cRet := &(cRun)
   ENDIF

RETURN cRet

///////////////////////////////////////////////////////////////////////////////
FUNCTION myStrToArray( cBuf )
   LOCAL aBuf, lBuf

   IF left(cBuf, 3) == ["{"]
      cBuf := subs(cBuf, 2)
   ENDIF
   IF right(cBuf, 3) == ["}"]
      cBuf := left(cBuf, Len(cBuf)-1)
   ENDIF
   lBuf := .F.
   IF left(cBuf, 1) == "{" .and. right(cBuf, 1) == "}"
      BEGIN SEQUENCE WITH {|e| break( e ) }
         aBuf := &(cBuf)
         lBuf := .T.
      END SEQUENCE
   ELSE
      aBuf := {}
   ENDIF
   IF !lBuf
      aBuf := {}
   ENDIF

RETURN aBuf

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myIsFunct(cRun,xPar,nI,cStr)
   LOCAL cFun, lRet, cMsg

   IF !IsChar(xPar) ; xPar := cValToChar(xPar)
   ENDIF

   lRet := .T.
   cFun := SUBSTR(cRun, 1, AT("(",cRun) - 1)
   cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
   cMsg += "Нет такой функции "+cFun+"("+xPar+") в ЕХЕ-файле !;"
   cMsg += 'Обратитесь к разработчику программы;;'
   cMsg += cStr + ";;"
   cMsg += ProcNL() + ";" + ProcNL(1)

   IF !hb_IsFunction( cFun )
      AlertStop(cMsg, "Запуск функции", , 64, {RED})
      lRet := .F.
      ? ATREPL( ";", cMsg, CRLF )
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////
FUNCTION myDim2Format(aGet,aPict)
   LOCAL cRet, nI, nK, cVal, xVal, cPict, cRzd

   cRet := ""
   nK   := LEN(aGet)
   cRzd := aPict[LEN(aPict)]

   FOR nI := 1 TO nK
      xVal  := aGet[nI]
      cPict := aPict[nI]
      //? ProcNL(), nI, xVal, cPict
      cVal := TRANSFORM(xVal,cPict)
      IF !IsString(cVal)
         cVal := cValToChar(cVal)
      ENDIF
      cRet += ALLTRIM(cVal) 
      cRet += IIF( nI == nK, "", cRzd)
   NEXT

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
// получить все значения из dbf-справочников
// aSprv := { {"tipza","Ktipza","tipza",2} , {"Works","KWorks","Works",2} , {"D"}, {"N"}, {"C"}, {"Master","Manager","Warehouse"} }
// aType := {"S","S","D","N","C","A"}
FUNCTION Get2Dim_Spravki(aSprv,aType,cErr)
  LOCAL nI, nJ, cAls, cFld1, cFld2, nOrd, aName, aCode, xRet, aXArr
  LOCAL aDim, nSel, cTyp, cTekAls := ALIAS()

  aXArr := {}
  FOR nI := 1 TO LEN(aSprv)
     cTyp  := aType[nI]
     aDim  := aSprv[nI]
     aName := {}
     aCode := {}

     IF cTyp == "S"
        cAls  := aDim[1]
        cFld1 := aDim[2]
        cFld2 := aDim[3]
        nOrd  := aDim[4]

        nSel  := SELECT( cAls )
        IF nSel == 0
           cErr += "Ошибка! Нет алиаса: " + cAls + " !;"
           RETURN aXArr
        ENDIF

        DBSELECTAREA( cAls )
        DbSetOrder(nOrd)

        IF FIELDNUM(ALLTRIM(cFld1)) == 0
           cErr += 'Нет поля "'+cFld1+'" в БД '+cAls+' !;'
           DbSelectArea(cTekAls)
           RETURN aXArr
        ENDIF
        IF FIELDNUM(ALLTRIM(cFld2)) == 0
           cErr += 'Нет поля "'+cFld2+'" в БД '+cAls+' !;'
           DbSelectArea(cTekAls)
           RETURN aXArr
        ENDIF

        // поиск по базе
        GOTO TOP
        DO WHILE !EOF()
           IF !DELETED()
              IF  (cAls)->&cFld1 == 0
              ELSE
                xRet := (cAls)->&cFld2
                IF IsString(xRet)
                   xRet := ALLTRIM(xRet)
                ELSE
                   xRet := cValToChar(xRet)
                ENDIF
                AADD( aName, xRet           )
                AADD( aCode, (cAls)->&cFld1 )
              ENDIF
           ENDIF
           SKIP
        ENDDO
     ELSEIF cTyp == "A"
        FOR nJ := 1 TO LEN(aDim)
            AADD( aName, aDim[nJ] )
            AADD( aCode, nJ       )
        NEXT
     ELSE
        // нет обработки
     ENDIF
     AADD( aXArr, {aCode,aName} )
  NEXT
  DbSelectArea(cTekAls)

RETURN aXArr

///////////////////////////////////////////////////////////////////////////////////////////////////
// получить значение - как по set relation / база открыта и есть 1 индекс по коду
FUNCTION GET_from_DBF(xDbf, x14Col)
   LOCAL cRet, xVal, cAls, cFld2, cFld3,  nOrd, cFltr
   LOCAL c1Als, nOrder, cVal, xRet, nSel, nRet

   c1Als := ALIAS()
   // x14Col = {"Master","KMaster","Master", 2,"KFIRMA==2"}
   xVal  := xDbf         // значение поля основной бд
   IF !IsArray(x14Col)
      cRet := "Ошибка! (x14Col) не массив !" + cValToChar(x14Col)
      RETURN cRet
   ENDIF
   IF LEN(x14Col) == 0
      cRet := "Ошибка! x14Col[] = 0 ! " + HB_ValtoExp(x14Col)
      RETURN cRet
   ENDIF
   cAls  := x14Col[1]    // база справочника
   cFld2 := x14Col[2]    // поле базы по которому ищем - 1-ый индекс
   cFld3 := x14Col[3]    // возврат по этому полю
   nOrd  := x14Col[4]    // резерв
   cFltr := x14Col[5]    // резерв

   nSel  := SELECT( cAls )
   IF nSel == 0
      cRet := "Ошибка! Нет алиаса: " + cAls
      RETURN cRet
   ENDIF

   DBSELECTAREA( cAls )
   nOrder := INDEXORD()
   nRet   := FIELDNUM(ALLTRIM(cFld2))
   IF nRet == 0
      cRet := 'Нет поля "'+cFld2+'" в БД '+ALIAS()+' !'
      DBSELECTAREA( c1Als )
      RETURN cRet
   ENDIF

   nRet   := FIELDNUM(ALLTRIM(cFld3))
   IF nRet == 0
      cRet := 'Нет поля "'+cFld3+'" в БД '+ALIAS()+' !'
      DBSELECTAREA( c1Als )
      RETURN cRet
   ENDIF

   // поиск по базе
   xRet := "---"
   DBSETORDER(1)
   GOTO TOP
   SEEK xVal
   IF FOUND()
      xRet := FIELDGET(FIELDNUM(cFld3))
      IF IsString(xRet)
         cRet := ALLTRIM(xRet)
      ELSE
         cRet := "[" + cValToChar(xRet) + "]"
      ENDIF
   ELSE
      cVal := cValToChar(xVal)
      cRet := "нет данных ! " + cFld2 + "=" + cVal
   ENDIF
   // вернём текущий индекс
   DBSELECTAREA( cAls )
   DBSETORDER(nOrder)

   // вернём текущую базу
   DBSELECTAREA( c1Als )

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
// Здесь можно открыть базу и брать данные оттуда
FUNCTION Spr_1Dbf(aParam)   // тип "SPR_S" // aParam := { cRType, cField, xDbf, x14Col }
   LOCAL cRet, cType, xVal, cFld, aBase, cAls, cFld2, cFld3,  nOrd, cFltr
   LOCAL c1Als, nOrder, cVal, xRet, nSel, nRet

   c1Als := ALIAS()
   // x14Col = {"Master","KMaster","Master",2,"KFIRMA==2"}
   cType := aParam[1]   // тип "SPR_S"
   cFld  := aParam[2]   // поле бд
   xVal  := aParam[3]   // значение поля бд
   aBase := aParam[4]   // база откуда берем данные
   cAls  := aBase[1]
   cFld2 := aBase[2]    // резерв
   cFld3 := aBase[3]    // возврат по этому полю
   nOrd  := aBase[4]    // резерв
   cFltr := aBase[5]    // резерв

   nRet  := FIELDNUM(ALLTRIM(cFld))
   IF nRet == 0
      cRet := 'Нет поля "'+cFld+'" в БД '+ALIAS()+' !'
      DBSELECTAREA( c1Als )
      RETURN cRet
   ENDIF

   nSel  := SELECT( cAls )
   IF nSel == 0
      cRet := "Ошибка! Нет алиаса: " + cAls
      RETURN cRet
   ENDIF

   DBSELECTAREA( cAls )
   nOrder := INDEXORD()
   nRet   := FIELDNUM(ALLTRIM(cFld3))
   IF nRet == 0
      cRet := 'Нет поля "'+cFld3+'" в БД '+ALIAS()+' !'
      DBSELECTAREA( c1Als )
      RETURN cRet
   ENDIF

   // поиск по базе
   xRet := "---"
   DBSETORDER(1)
   GOTO TOP
   SEEK xVal
   IF FOUND()
      xRet := FIELDGET(FIELDNUM(cFld3))
      IF IsString(xRet)
         cRet := ALLTRIM(xRet)
      ELSE
         cRet := "[" + cValToChar(xRet) + "]"
      ENDIF
   ELSE
      cVal := cValToChar(xVal)
      cRet := "нет данных ! " + cFld + "=" + cVal
   ENDIF
   //MsgDebug(cAls, FOUND(), "xRet=",xRet, xVal, cRet)
   // вернём текущий индекс
   DBSELECTAREA( cAls )
   DBSETORDER(nOrder)

   // вернём текущую базу
   DBSELECTAREA( c1Als )

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
// Здесь можно открыть базу и брать данные оттуда
// Here you can open the database and take data from there
FUNCTION Spr_2Dbf(aDim)     // тип "SPR_S" // aDim :=  {"Master","KMaster","Master",2,"KFIRMA==2"}
   LOCAL c1Als, cAls, cFld1, cFld2, aName, aCode, nOrd, cFltr
   LOCAL nSel, nOrder, nRet, cRet, xRet, cVal

   aName := {}
   aCode := {}
   c1Als := ALIAS()
   cAls  := aDim[1]
   cFld1 := aDim[2]
   cFld2 := aDim[3]
   nOrd  := aDim[4]
   cFltr := aDim[5]

   nSel  := SELECT( cAls )
   IF nSel == 0
      cRet := "Ошибка! Нет алиаса: " + cAls
      AADD( aName, cRet  )
      AADD( aCode,  -1   )
      RETURN { aName, aCode }
   ENDIF

   DBSELECTAREA( cAls )
   nOrder := INDEXORD()
   nRet   := FIELDNUM(ALLTRIM(cFld1))
   IF nRet == 0
      cRet := 'Нет поля "'+cFld1+'" в БД '+cAls+' !'
      DBSELECTAREA( c1Als )
      AADD( aName, cRet   )
      AADD( aCode,  -2    )
      RETURN { aName, aCode }
   ENDIF

   // поиск по базе
   DBSETORDER(nOrd)
   IF LEN(cFltr) > 0  // условие фильтра
      cVal := myMacro(cFltr)
      IF !IsString(cVal)
         cVal := cValToChar(cVal)
      ENDIF
      //MsgDebug(cFltr, cVal)
      IF "ОШИБКА" $ cVal
         cRet := 'Нет такого фильтра "'+cFltr+'" в БД '+cAls+' ! aDim[5]'
         DBSELECTAREA( c1Als )
         AADD( aName, cRet   )
         AADD( aCode,  -3    )
         RETURN { aName, aCode }
      ELSE
         DbSetFilter( &("{||" + cFltr + "}"), cFltr )
      ENDIF
   ENDIF
   GOTO TOP
   DO WHILE !EOF()
      IF !DELETED()
         IF  (cAls)->&cFld1 == 0
         ELSE
           xRet := (cAls)->&cFld2
           IF IsString(xRet)
              xRet := ALLTRIM(xRet)
           ELSE
              xRet := cValToChar(xRet)
           ENDIF
           AADD( aName, xRet           )
           AADD( aCode, (cAls)->&cFld1 )
         ENDIF
      ENDIF
      SKIP
   ENDDO

   cRet := 'БД '+cAls+' !'
   IF LEN(cFltr) > 0  // условие фильтра
      cRet := 'Фильтр "'+cFltr+'" в БД '+cAls+' !'
      DbSetFilter()
   ENDIF
   IF LEN(aName) == 0
      cVal := 'Нет списка в БД '+cAls+' ! aDim[5]'
      AADD( aName, cVal   )
      AADD( aCode,  -4    )
      AADD( aName, cRet   )
      AADD( aCode,  -5    )
      cVal := HB_ValToExp(aDim)
      AADD( aName, cVal   )
      AADD( aCode,  -6    )
   ENDIF
   // вернём текущий индекс
   DBSELECTAREA( cAls )
   DBSETORDER(nOrder)

   // вернём текущую базу
   DBSELECTAREA( c1Als )

RETURN { aName, aCode }

///////////////////////////////////////////////////////////////////////////////////////////////////
// (5) - функция чтение поля (4) и запись в колонку (2)
FUNCTION GetNum2Log(aParam)    // aParam := { cRType, cField, xDbf, x9Col }
   LOCAL lRet, xVal
   xVal := aParam[3]
   lRet := IIF(xVal == 1, .T., .F.)
RETURN lRet

// AADD( aDim, {"Источник заявки"    , SPACE(20) , "SPR_A" , "KDispJil" , "Za_Istok()"     ,;
// "FuncWrtNum()"  , nil, "W", {"Жилец","Диспечерская","Ростелеком","ОДС (объ.дисп.служба)","Город 77"} } )
FUNCTION GetDim2Str(aParam)    // aParam := { cRType, cField, xDbf, x9Col }
   LOCAL cRet, xVal, a9Col
   xVal  := aParam[3]
   a9Col := aParam[4]
   cRet  := "---"
   IF     xVal == 0          ; cRet  := "---"
   ELSEIF xVal < 0           ; cRet  := "<-0"
   ELSEIF xVal > LEN(a9Col)  ; cRet  := "???"
   ELSE
      cRet  := a9Col[xVal]
   ENDIF
// AADD( aDim, {"Источник заявки"    , SPACE(20) , "SPR_A" , "KDispJil" , "Za_Istok()"     ,;
// "FuncWrtNum()"  , nil, "W", {"Жилец","Диспечерская","Ростелеком","ОДС (объ.дисп.служба)","Город 77"} } )
RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
// (6) - функция записи в поле (4) из колонки (2) или (7)
FUNCTION SetLog2Num(aVal)
   LOCAL nRet, xVal
   //? ProcNL(), "aVal=", aVal, VALTYPE(aVal)
   xVal := aVal[1]
   //? "         =>", "xVal=", xVal, VALTYPE(xVal)
   nRet := IIF( xVal== .T., 1, 0 )
RETURN nRet

////////////////////////////////////////////////////////////////////////////
// запись массива полей (10) и значений (8) в базу
FUNCTION SetDim2Wrt(aVal)     // aPara := { cRType, cField, x8Col, x10Col, nI }
   LOCAL a8Dim, a10Dim        // для типа CALC без поля cField = ""
   LOCAL nI, nFld, cFld, cMsg, cErr, xVal, cFType, cXType, cAls, lWrite

   a8Dim := aVal[3]  ; a10Dim := aVal[4]
   cMsg  := "=.=.=.=.=.= Error! Line: " + HB_NtoS(aVal[5]) + ";"
   cMsg  += HB_ValToExp(aVal) + ";;"
   cAls  := ALIAS()
   cErr  := ""
   //? ProcNL(), "aVal=", aVal ; ?v aVal
   FOR nI := 1 TO LEN(a8Dim)
      cFld  := a10Dim[nI]
      xVal  := a8Dim[nI]
      nFld  := FIELDNUM( cFld )
      IF nFld == 0
         cErr += "No such field ["+cFld+"] in DB-"+cAls+";"
      ELSE
         cFType := FieldType( FIELDNUM( cFld ) )
         cXType := VALTYPE(xVal)
         lWrite := .T.
         IF cFType # cXType
            IF !IsChar(xVal) ; xVal := cValToChar(xVal)
            ENDIF
            IF cFType == "M" .AND. cXType == "C"
               // пропуск ошибки
            ELSE
               cErr += "Different database field types ["+cFType+"] and record values ["+cXType+"];"
               cErr += "xVal=" + xVal + ";;"
               lWrite := .F.
            ENDIF
         ENDIF
         IF lWrite
            //? ProcNL(), cFld, xVal
            IF (cAls)->( RLock() )
               (cAls)->&cFld := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ELSE
               cErr += "WRITE ERROR ! Write "
               cErr += HB_NtoS(RECNO()) + " blocked !;"
            ENDIF
         ENDIF
      ENDIF
   NEXT
   IF LEN(cErr) > 0
      cMsg += cErr + ";"
      cMsg += ProcNL() + ";" + ProcNL(1)
      cMsg += ";" + ProcNL(2) + ";" + ProcNL(3)
      AlertStop(cMsg, "", , 64, {RED})
      cMsg += ";" + REPL("=.",40) + ";"
      ? ATREPL( ";", cMsg, CRLF )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
FUNCTION FuncWrtNum()
RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on the list of columns'
#else
   cTitle := 'Инфо по списку колонок'
#endif

   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      cMsg  += HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT
   cMsg += ";"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := cValToChar( oCol:lEdit )    // oCol:cPicture
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = "
      cMsg  += ' "'+ cFld + '"  ;'
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, 64, , {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListFont( oBrw )
   LOCAL cMsg, cTitle, aFont, nI, aFPar, hFont, cFont

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on table fonts'
#else
   cTitle := 'Инфо по фонтам таблицы'
#endif

   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   cMsg   += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg   += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg   += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg   += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg   += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg   += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";;"

   cMsg += Replicate( "-._.", 22 ) + ";;"
   cMsg += "1) Height = " + HB_NtoS(oBrw:nHeight) + ";"
   cMsg += "2) HeightHead = " + HB_NtoS(oBrw:nHeightHead) + ";"
   cMsg += "3) HeightSuper = " + HB_NtoS(oBrw:nHeightSuper) + ";"
   cMsg += "4) HeightFoot = " + HB_NtoS(oBrw:nHeightFoot) + ";"
   cMsg += "5) HeightSpecHd = " + HB_NtoS(oBrw:nHeightSpecHd) + ";"
   cMsg += "6) HeightCell = " + HB_NtoS(oBrw:nHeightCell) + ";;"
   cMsg += "Number of rows in the table = " + HB_NtoS(oBrw:nRowCount()) + ";;"
   cMsg += "GetHScrollBarHeight() = " + HB_NtoS(GetHScrollBarHeight()) + ";"
   cMsg += "GetVScrollBarWidth() = " + HB_NtoS(GetVScrollBarWidth()) + ";"
   cMsg += Replicate( "-._.", 22 ) + ";"

   nI := cFont := hFont := aFPar
   aFont := oBrw:Cargo:aFont
   /*FOR nI := 1 TO Len(aFont)
      cFont := aFont[nI]
      hFont := GetFontHandle(cFont)
      aFPar := GetFontParam( hFont )
      cMsg  += "  " + HB_NtoS(nI) + ": "
      cMsg  += cFont + " - ["
      cMsg  += hb_ntos(hFont) + "] - "
      cMsg  += hb_valtoexp(aFPar) + ";"
   NEXT */
   cMsg   += REPL("; ",20)


   AlertInfo(cMsg , cTitle, 64, , {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbArrayLine( oBrw , lFlag, lShow)
   LOCAL cMsg, cTitle, aDim, nI, xVal, cVal, nCol, cAls, cFld, cNam, cTyp
   DEFAULT lFlag := .F. , lShow := .T.

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on the current table row Info on the current table row'
   cVal   := IIF(oBrw:lIsDbf," - this is a DBF !"," - this is an ARRAY !")
#else
   cTitle := 'Инфо по текущей строке таблицы'
   cVal   := IIF(oBrw:lIsDbf," - это DBF !"," - это МАССИВ !")
#endif

   cMsg := "Table alias: " + oBrw:cAlias + cVal + ";;"
   IF oBrw:lIsDbf 
      aDim  := {}
      cAls  := oBrw:cAlias
      FOR nCol := 1 TO oBrw:nColCount()
          cFld := oBrw:aColumns[ nCol ]:cField 
          cTyp := oBrw:aColumns[ nCol ]:cFieldTyp
          cNam := oBrw:aColumns[ nCol ]:cName 
          IF cNam == "ORDKEYNO"  ; cVal := cNam
          ELSE                   ; cVal := (cAls)->&cFld
          ENDIF
          AADD( aDim, { cTyp, cVal, cFld } )
      NEXT
   ELSE
      aDim  := oBrw:aArray[oBrw:nAt]
   ENDIF

   FOR nI := 1 TO Len(aDim)
      cMsg += "(" + STR(nI,2) + ") - "
      xVal := aDim[nI]
      cMsg += "[" + VALTYPE(xVal) + "]   "
      IF IsArray(xVal)
         xVal := HB_ValToExp(xVal)
      ENDIF
      IF !IsString(xVal)
         xVal := myVal2Str(xVal)
      ENDIF
      xVal := ATREPL( ";", xVal, "|" )
      xVal := ATREPL( CRLF, xVal, "|" )
      cMsg += ALLTRIM(xVal) + ";"
   NEXT
   cMsg += ';// ' + REPL("-",50) + ";"

   IF lFlag
#ifdef KEY_ENG // for this project demo1-en.hbp
   cMsg += '// (1) - display column ;'
   cMsg += '// (2) - display column, value editing ;'
   cMsg += '// (3) - order of displaying rows in the table ;'
   cMsg += '// (4) - type of processing table cells ;'
   cMsg += '// (5) - base field in dbf ;'
   cMsg += '// (6) - function reading field (4) and writing to column (2) ;'
   cMsg += '// (7) - function writing to field (4) from column (2) or (8) - where NIL ;'
   cMsg += '// (8) - function for editing variables window for type CALC, SPR_A, SPR_J, SPR_S ;'
   cMsg += '// (9) - Write/Read cell editing access ;'
   cMsg += '// (10) - conversion to "C" of column (12) ;'
   cMsg += '// (11) - conversion to "C" of column (13) ;'
   cMsg += '// (12) - conversion to "C" of column (14) ;'
   cMsg += '// (13) - value of corrected field {} for type CALC,SPR_A,SPR_J,SPR_S from (3), otherwise NIL ;'
   cMsg += '// (14) - additional data for type (3): SPR_A,CALC,SPR_J,SPR_S,CALC ;'
   cMsg += '// (15) - additional data different ;'
#else
   cMsg += '// (1)  - колонка показа ;'
   cMsg += '// (2)  - колонка показа, правка значения ;'
   cMsg += '// (3)  - порядок показа строк в таблице ;'
   cMsg += '// (4)  - тип обработки ячеек таблицы ;'
   cMsg += '// (5)  - поле базы в dbf ;'
   cMsg += '// (6)  - функция чтение поля (4) и запись в колонку (2) ;'
   cMsg += '// (7)  - функция записи в поле (4) из колонки (2) или (8)-где NIL ;'
   cMsg += '// (8)  - функция для окна редактирования переменных для типа CALC,SPR_A,SPR_J,SPR_S ;'
   cMsg += '// (9)  - доступ редактирования ячеек Write/Read ;'
   cMsg += '// (10) - преобразование в "C" колонки (12) ;'
   cMsg += '// (11) - преобразование в "C" колонки (13) ;'
   cMsg += '// (12) - преобразование в "C" колонки (14) ;'
   cMsg += '// (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S из (3), в остальных случаях NIL ;'
   cMsg += '// (14) - доп.данные для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC ;'
   cMsg += '// (15) - доп.данные разные ;'
#endif
   ENDIF
   cMsg   += REPL('; ',20)

   IF lShow 
      AlertInfo(cMsg , cTitle, 64, , {RED})
   ENDIF

RETURN cMsg
