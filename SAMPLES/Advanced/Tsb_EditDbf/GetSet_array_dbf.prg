/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Загрузка/сохранение данных в dbf
 * _TBrowse() Load/save data to dbf
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

#define  THIS_DEBUG  .F.
//////////////////////////////////////////////////////////////////////////////////
FUNCTION ArrayDbfLoad(aXdim,cAls,cIni,aWrtIni,nPage)  // чтение из базы в массив
   LOCAL nI, xVal, cField, cFunc, cRType, xDbf, xRet, cMsg, nFld, aPara, aLine
   LOCAL aUSort, aFld, x2Dbf, aVal, nJ, aPict, cName, cDbf, lDbg, nFlag
   LOCAL x15Col, x14Col, x13Col
   DEFAULT cIni := App.Cargo:cIniSortUser
   DEFAULT aWrtIni := {}, nPage := 0  // для других проектов - резерв

   lDbg  := THIS_DEBUG
   DBSELECTAREA(cAls)
   IF lDbg  ; ? ProcNL(), "чтение данных из базы "+cAls+" в массив !"
   ENDIF

   FOR nI := 1 TO LEN(aXdim)

      aLine  := aXdim[nI]
      cName  := aXdim[nI,ACOL_1]
      xVal   := aXdim[nI,ACOL_2]           // ячейка таблицы для чтения из базы
      cRType := aXdim[nI,ACOL_4]           // тип обработки ячеек
      cField := aXdim[nI,ACOL_5]           // поле базы
      cFunc  := ALLTRIM(aXdim[nI,ACOL_6])  // чтение(4) и запись в (2)
      x13Col := aXdim[nI,ACOL_13]          // (13) - значение исправленного поля, если NIL, то править ненужно
      x14Col := aXdim[nI,ACOL_14]          // (14) - доп.данные для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
      x15Col := aXdim[nI,ACOL_15]

      aXdim[nI,ACOL_10] := ""   // это для показа в таблице
      aXdim[nI,ACOL_11] := ""
      aXdim[nI,ACOL_12] := ""
      aVal              := {}   // массив значений полей

      IF "LINE" $ cRType ; LOOP
      ENDIF

      IF lDbg  ; ? nI, cName, "[тип поля: "+cRType+"]", cField
      ENDIF

      IF cRType == ""           // без поля - обработка через функцию
      ELSEIF cRType $ "CLDNM"   // известные типы
      ELSEIF cRType == "DMN"    // дата с календарем
      ELSEIF cRType == "DT"     // дата+время
      ELSEIF cRType == "A"      // вложенный массив полей базы {"TelFIO","TelFIO3","TelFIO2"}
      ELSEIF cRType == "CALC"   // вложенный справочник - отдельная форма
      ELSEIF cRType == "SPR_A"  // вложенный массив - отдельная форма
      ELSEIF cRType == "SPR_S"  // справочник из dbf файла - контекстное меню {"Master","KMaster","Master",2,"KFIRMA==1"}
      ELSEIF cRType == "K"      // код в базе по set relation из другой базы - обработка через внутреннюю функцию
         // только показ, без правки
      ELSE
         cMsg := 'Ошибка !; Нет обработки типа: "'
         cMsg += cRType + '" !;'
         cMsg += "Строка: " + HB_NtoS(nI) + " ! "
         cMsg += cName + " ..... ;"
         cMsg += "Сделайте самостоятельно"
         cMsg += ProcNL() + ";" + ProcNL(1)
         AlertStop(cMsg, "Загрузка данных с DBF " + cAls, , 64, {RED})
         ? ATREPL( ";", cMsg, CRLF )
      ENDIF

      IF cRType == "A"                   // тип обработки ячеек
         aFld  := aXdim[nI,ACOL_5]       // массив полей базы
         aPict := aXdim[nI,ACOL_14]      // здесь формат для массива
         IF LEN(aFld) == LEN(aPict)
            cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
            cMsg += "Нет разделителя значений для типа [A] в БД "+cAls+";"
            cMsg += 'Пример: (5 колонка) - {"NKvar","CKvar"} ;'
            cMsg += '       (14 колонка) - {"@Z 99999","xxxxx", "/" } - 3 значение ;;'
            cMsg += ' (5 колонка) - ' + HB_ValToExp(aFld)  + ' ;'
            cMsg += '(14 колонка) - ' + HB_ValToExp(aPict) + ';;'
            cMsg += HB_ValToExp(aXdim[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
            AlertStop(cMsg, "", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF

         xDbf  := ""
         FOR nJ := 1 TO LEN(aFld)
            cField := aFld[nJ]
            x2Dbf  := FIELDGET( FIELDNUM( cField ) )
            nFld   := FIELDNUM( cField )
            IF nFld == 0 .AND. LEN(cField) > 0
               cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
               cMsg += "Нет такого поля ["+cField+"] в БД "+cAls+";"
               cMsg += HB_ValToExp(aXdim[nI]) + ";;"
               cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
               AlertStop(cMsg, "", , 64, {RED})
               ? ATREPL( ";", cMsg, CRLF )
            ENDIF
            AADD( aVal, x2Dbf )  // массив значений полей
         NEXT
         xDbf := myDim2Format(aVal,aPict)
      ELSE
         xDbf := FIELDGET( FIELDNUM( cField ) )
         nFld := FIELDNUM( cField )
         IF nFld == 0 .AND. LEN(cField) > 0
            cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
            cMsg += "Нет такого поля ["+cField+"] в БД "+cAls+";"
            cMsg += HB_ValToExp(aXdim[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
            IF cRType # "K"
               AlertStop(cMsg, "", , 64, {RED})
            ENDIF
            ? ATREPL( ";", cMsg, CRLF )
            xDbf := "Ошибка ! Нет такого поля ["+cField+"] в БД "+cAls+" !"
            aXdim[nI,ACOL_2] := xDbf
            LOOP
         ENDIF
      ENDIF

      IF lDbg  ; ?? "cFunc=",cFunc
      ENDIF
      IF LEN(cFunc) > 0
         IF AT("[",cFunc) > 0
            nFlag := 1            // стандартные харборовские функции / standard harbour functions
            cFunc := CHARREPL('[',cFunc,'(')
            cFunc := CHARREPL(']',cFunc,')')
         ELSEIF AT("(",cFunc) > 0
            nFlag := 2            // мои функции / my functions
         ELSE
            nFlag := 0
            cMsg := "Ошибка ! Не определена функция !;"
            cMsg += "Нет скобок () или [];"
            cMsg += "Функция: " + cFunc + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1)
            AlertStop(cMsg, "Загрузка базы", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF
         //
         IF myIsFunct(cFunc,xDbf,nI,"ЧТЕНИЕ! "+HB_ValToExp(aXdim[nI]) )
            aPara := { cRType, cField, xDbf, x14Col, x15Col, nI, aLine }
            IF nFlag == 2  // мои функции
               cFunc := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cFunc += "(" + hb_valtoexp(aPara) + ")"      // передаем параметры этой строки массива
            ENDIF
            IF lDbg ; ?? cFunc
            ENDIF

            xRet  := myMacro(cFunc, .F.)   // .T. режим отладки
            IF IsString(xRet) .AND. "ERROR! MACRO=" $ UPPER(xRet)
               ? "___ERROR!___", nI, aXdim[nI,1], xRet
            ENDIF
            IF VALTYPE(xRet) == "A"            // для типа "CALC"
               aXdim[nI,ACOL_2]  := xRet[1]
               aXdim[nI,ACOL_13] := xRet[2]    // здесь будет массив значений для дальнейшей правки
            ELSE                               // в функии окна для редактирования
               aXdim[nI,ACOL_2] := xRet
            ENDIF
         ENDIF
         IF lDbg  ; ?? "xRet=", xRet
         ENDIF
         // преобразование в "C" колонки
         aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
         aXdim[nI,ACOL_11] := myVal2Str(aXdim[nI,ACOL_14])
         aXdim[nI,ACOL_12] := myVal2Str(aXdim[nI,ACOL_15])
      ELSE
         IF cRType $ "CLDNM"
            aXdim[nI,ACOL_2] := xDbf
         ELSEIF cRType == "DMN"  // дата с календарем
            aXdim[nI,ACOL_2] := xDbf
         ELSEIF cRType == "DT"   // дата+время
            aXdim[nI,ACOL_2] := HB_TTOC(xDbf)
         ELSEIF cRType == "A"   // вложенный массив
            aXdim[nI,ACOL_13] := aVal    // здесь будет массив значений для дальнейшей правки
            aXdim[nI,ACOL_2]  := xDbf
            // преобразование в "C" колонки
            aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
         ELSEIF cRType == "K"            // код в базе по set relation из другой базы - только показ, без правки
            IF !IsNumeric(xDbf)
               xDbf := -15
            ENDIF
            // xDbf - нужно число
            x14Col := aXdim[nI,ACOL_14]          // (14) - доп.данные для типа (K): из справочника DBF
            aXdim[nI,ACOL_11] := myVal2Str(aXdim[nI,ACOL_14])
            aXdim[nI,ACOL_13] := HB_NtoS(xDbf)   // здесь будет код значения справочника для дальнейшей правки
            aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
            // ещё раз проверим поле базы
            nFld := FIELDNUM( cField )
            IF nFld == 0 .AND. LEN(cField) > 0
               cMsg := "Ошибка ! Нет такого поля ["+cField+"] в БД "+cAls+" !"
               cDbf := cMsg
            ELSE
               cDbf := GET_from_DBF(xDbf, x14Col)   // получить значение - как по set relation
            ENDIF
            aXdim[nI,ACOL_2]  := cDbf
         ENDIF
      ENDIF

      IF cRType == "CALC" .AND. lDbg
         ?? "13", VALTYPE(aXdim[nI,ACOL_13]), aXdim[nI,ACOL_13]
         ?? "14", VALTYPE(aXdim[nI,ACOL_14]), aXdim[nI,ACOL_14]
         ?? "15", VALTYPE(aXdim[nI,ACOL_15]), aXdim[nI,ACOL_15]
      ENDIF

   NEXT

   // считать данные с ини-файла - порядок сортировки показа юзера
   IniLoadUserSort(cIni, App.Cargo:cTsbVersion, @aUSort)
   //? ProcNL(), cIni, aUSort, VALTYPE(aUSort)
   IF !IsArray(aUSort)
   ELSEIF LEN(aUSort) == 0
   ELSEIF LEN(aUSort) # LEN(aXdim)
      cMsg := "Ошибка ! Пропуск сортировки !;"
      cMsg := "Массив сортировки юзера ("
      cMsg += HB_NtoS(LEN(aUSort)) + ");"
      cMsg += "не равен массиву строк таблицы ("
      cMsg += HB_NtoS(LEN(aXdim)) + ") ;;"
      cMsg += cIni + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg, "Загрузка настроек", , 64, {RED})
      ? ATREPL( ";", cMsg, CRLF )
      AADD( aWrtIni, { cFileNoPath(cIni), nPage } )   // потом записать этот ини
   ELSE
      FOR nI := 1 TO LEN(aXdim)
         aXdim[nI,ACOL_3] := aUSort[nI]  // (3)  - порядок показа строк в таблице
      NEXT
   ENDIF

RETURN aXdim

////////////////////////////////////////////////////////////////////////
// запись из массива в базу
FUNCTION ArrayDbfSave(aArray)
   LOCAL nI, xVal, cField, cFunc, cRType, xRet, cMsg, nFld, cAls
   LOCAL cFType, cXType, x13Col, c9Col, x14Col, aFld, nF, aVal
   LOCAL x15Col, aLine, aPara, cSay, lDbg, aUSort := {}

   lDbg := THIS_DEBUG
   cAls := ALIAS()
   IF lDbg ; ? ProcNL(), "запись из массива в базу !",cAls
   ENDIF

   FOR nI := 1 TO LEN(aArray)

      aLine  := aArray[nI]
      xVal   := aArray[nI,ACOL_2]           // ячейка таблицы для записи в базу
      cRType := aArray[nI,ACOL_4]           // (4) - тип обработки ячеек таблицы
      cField := aArray[nI,ACOL_5]           // (5) поле базы
      cFunc  := ALLTRIM(aArray[nI,ACOL_7])  // (7) запись в (4) через функцию (6)
      c9Col  := aArray[nI,ACOL_9]           // (9) - доступ редактирования ячеек Write/Read
      x13Col := aArray[nI,ACOL_13]          // (13) - значение исправленного поля, если NIL, то править ненужно
      x14Col := aArray[nI,ACOL_14]          // (14) - доп.данные для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
      x15Col := aArray[nI,ACOL_15]          // (15) - доп.данные
      AADD(aUSort,aArray[nI,ACOL_3])        // (3)  - порядок показа строк в таблице
      IF lDbg ; ? nI,aArray[nI,ACOL_1], "[тип поля: "+cRType+"]",c9Col
      ENDIF

      IF "LINE" $ cRType
          IF lDbg ; ?? "[пропуск записи]"
          ENDIF
          LOOP
      ELSEIF c9Col == "R" .AND. cRType # "K"
          IF lDbg ; ?? "[пропуск записи]"
          ENDIF
          LOOP
      ENDIF

      IF cRType $ "CLDNM" .OR. cRType == "DMN" .OR. cRType == "DT"
         // есть обработка дальше, всегда переписываем эти данные
      ELSE
         IF x13Col == NIL
            IF lDbg ; ?? "x13Col == NIL,  [пропуск записи]"
            ENDIF
            LOOP  // если NIL, то править ненужно
         ENDIF
      ENDIF

      IF cRType # "A"          // тип обработки ячеек
         nFld   := FIELDNUM( cField )
         IF nFld == 0 .AND. LEN(cField) > 0
            cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
            cMsg += "Нет такого поля ["+cField+"] в БД-"+cAls+";"
            cMsg += HB_ValToExp(aArray[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1)
            AlertStop(cMsg, "", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF
         cMsg := ""
      ELSE
         cMsg := HB_ValToExp(aArray[nI,ACOL_5])      // (5) поле базы
      ENDIF

      IF lDbg ; ?? cFunc, cField, cMsg
      ENDIF
      IF LEN(cFunc) > 0
         //
         IF myIsFunct(cFunc,xVal,nI,"ЗАПИСЬ! "+HB_ValToExp(aArray[nI]) )
            IF LEN(cField)  == 0
               // для типа CALC без поля cField = ""
               // for CALC type without field cField = ""
               aPara := { cRType, cField, x13Col, x14Col, nI, aLine }
               cFunc := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cFunc += "(" + HB_ValToExp(aPara) + ")"
               xRet  := myMacro(cFunc)
               IF lDbg ; ?? '[без поля cField = ""]', "Func:Write=", cFunc
               ENDIF

            ELSE
               // для типа CALC c полем cField = "XXXX"
               // for type CALC with field cField = "XXXX"
               aPara  := { xVal, cRType, cField, x13Col, x14Col, x15Col, nI, aLine }
               cFunc  := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cSay   := cFunc + "({...})"
               cFunc  += "(" + HB_ValToExp(aPara) + ")"
               IF lDbg ; ?? '[c полем cField = "'+cField+'" ]' , '[cFunc = "' + cSay + '" ]'
               ENDIF
               // запуск функций для одиночного сохранения поля базы
               // launch functions for single saving of a database field
               xRet   := myMacro(cFunc, .T. )   // .T. режим отладки

               cFType := FieldType( FIELDNUM( cField ) )
               cXType := VALTYPE(xRet)
               IF cFType == "M" .AND. cXType == "C"
                  cFType := "C"  // это не ошибка
               ENDIF
               IF cXType == "C" .AND. xRet == "RECORDS-ARE-ALREADY-CLOSED!"  //"ЗАПИСЬ УЖЕ ЗАВЕРШЕНА!"
                  // пропуск дальнейшей проверки и записи в базу
                  // skip further checking and writing to the database
               ELSE
                  IF cFType # cXType
                     IF lDbg ; ?? "--- cFType # cXType", cFType, "#", cXType
                     ENDIF

                     IF !IsChar(xRet) ; xRet := cValToChar(xRet)
                     ENDIF
                     cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
                     cMsg += HB_ValToExp(aArray[nI]) + ";;"
                     cMsg += "Разные типы поля базы ["+cFType+"] и значения для записи ["+cXType+"];"
                     cMsg += "xRet=" + xRet + ";;"
                     cMsg += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
                     AlertStop(cMsg, "", , 64, {RED})
                     ? ATREPL( ";", cMsg, CRLF )
                  ELSE
                     // запись в поле через cFunc
                     IF (cAls)->( RLock() )
                        (cAls)->&cField := xRet
                        (cAls)->( DbUnlock() )
                        (cAls)->( DbCommit() )
                        IF lDbg ; ?? "Write:",cField, "->", "xRet=", xRet
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF  // "RECORDS ARE ALREADY CLOSED!"  //"ЗАПИСЬ УЖЕ ЗАВЕРШЕНА!"
            ENDIF
         ELSE
            //MsgDebug(cFunc,"Пропуск записи в поле:",cField,xRet)
         ENDIF
      ELSE

         // запись сразу в поле если НЕТ cFunc
         IF cRType $ "CLDNM" .OR. cRType == "DMN" // дата с календарем
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF
            IF lDbg ; ?? "xVal=", xVal
            ENDIF

         ELSEIF cRType == "A"   // вложенный массив
            aFld := aArray[nI,ACOL_5]        // (5) список полей базы
            aVal := aArray[nI,ACOL_13]      // здесь исправленный массив значений для записи
            IF (cAls)->( RLock() )
               FOR nF := 1 TO LEN(aFld)
                  xVal   := aVal[nF]
                  cField := aFld[nF]
                  (cAls)->&cField := xVal
                  IF lDbg ; ?? nF, cField+"=", "xVal=", xVal
                  ENDIF
               NEXT
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSEIF cRType  == "DT"  // дата + время
            IF (cAls)->( RLock() )
               IF VALTYPE(xVal) != "T"
                  // C  27.03.25 17:31:47.000   -> T  2025-03-27 17:31:47
                  xVal := hb_CToT(xVal)  // вот так !
               ENDIF
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF
            IF lDbg ; ?? "xVal=", xVal
            ENDIF

         ELSEIF cRType == "SPR_A" .OR. cRType == "SPR_S"
            // (12)  - значение исправленного поля "NCA" для типа CALC,SPR_A,SPR_J,SPR_S из (3)
            // в остальных случаях NIL
            xVal := x13Col
            IF lDbg ; ?? "Write=", xVal
            ENDIF

            IF xVal == NIL
               ? ProcNL()
               ? "ERROR - line:", nI, aArray[nI,ACOL_1], cRType, "x13Col=",x13Col
               ? "     aArray[nI,14]=", HB_ValToExp(aArray[nI,ACOL_14])
               MsgDebug(ProcNL(),"ERROR - line:", nI, cField, xVal, x13Col)
            ENDIF
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSEIF cRType == "K"            // код в базе по set relation из другой базы - только показ, без правки
            // x13Col - строка, нужно в число
            xVal := x13Col                    // (13) - значение исправленного поля
            IF lDbg ; ?? "Write=", xVal, VALTYPE(xVal)
            ENDIF

            IF !IsString(xVal)
               cMsg := "     Должна быть строка !"
               ? ProcNL()
               ? "ERROR - line:", nI, aArray[nI,ACOL_1], cRType, "x13Col=",x13Col
               ? cMsg
               MsgDebug(ProcNL(),"ERROR - line:", nI, cMsg, cField, xVal, x13Col)
            ENDIF
            xVal := Val(ALLTRIM(x13Col))
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSE
            ? ProcNL()
            ? "ERROR - line:", nI, aArray[nI,1], cRType, "x13Col=",x13Col
            ? "     aArray[nI,14]=", HB_ValToExp(aArray[nI,ACOL_14])
            MsgDebug(ProcNL(),"ERROR - line:", nI, "НЕТ записи типа:", cRType, cField, xVal, x13Col)
         ENDIF
      ENDIF
   NEXT
   IF lDbg ; ? ProcNL(), "Конец записи:", cAls
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// запись сортировки показа строк таблицы
FUNCTION IniSave(aArray,cIni)
   LOCAL nI, aUSort := {}
   DEFAULT cIni := App.Cargo:cIniSortUser

   FOR nI := 1 TO LEN(aArray)
      AADD(aUSort,aArray[nI,ACOL_3])  // (3)  - порядок показа строк в таблице
   NEXT
   // записать порядок сортировки показа юзера
   IniSaveUserSort(cIni, App.Cargo:cTsbVersion, aUSort)

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
// записать данные в ини-файл
STATIC Function IniSaveUserSort(cFileIni, cMetkaIni, aDim)
   LOCAL aSave
   aSave := { aDim, cMetkaIni, App.ExeName }
   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )
Return Nil

///////////////////////////////////////////////////////////////////////
// считать данные с ини-файла
STATIC Function IniLoadUserSort(cFileIni, cMetkaIni, aDim)
   LOCAL cStr, aRet

   IF !FILE(cFileIni)
      aDim := {}
      Return Nil
   ENDIF

   cStr := ALLTRIM( hb_MemoRead(cFileIni) )
   IF LEN(cStr) == 0
     // нет данных
   ELSE
      // чтобы при добавлении нового параметра была смена без ошибки
      IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0 .AND. AT( cMetkaIni, cStr ) > 0
         aRet      := &cStr
         aDim      := aRet[1]
         cMetkaIni := aRet[2]  // чтобы при добавлении нового параметра была смена без ошибки
      ELSE
        // нет данных
      ENDIF
   ENDIF

Return Nil
