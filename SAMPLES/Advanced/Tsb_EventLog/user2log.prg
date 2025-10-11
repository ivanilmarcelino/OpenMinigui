/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 04.09.25
 * user-action-log entry
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "Dbinfo.ch"
///////////////////////////////////////////////////////////////////
FUNCTION User2Login()
   LOCAL nEvtn, cEvtn, nSel0, dDT, nID, cRem

#ifdef KEY_ENG
   cEvtn   := "Entering the program"
#else
   cEvtn   := "Вход в программу"
#endif

   nSel0   := SELECT()
   nEvtn   := 2
   cRem    := ""

   SELECT User2Log
   APPEND BLANK
   dDT := User2Log->DT_MODIFY
   nID := User2Log->ID
   IF User2Log->( RLock() )
      //User2Log->ID                               // AutoInc      4   0
      //User2Log->DT_MODIFY                        // ModTime      8   0
      FieldPut(FieldPos('EVENT'), dDT)
      User2Log->DEVENT     := DATE()               // Date         8   0
      User2Log->TEVENT     := TIME()               // Character    8   0
      User2Log->IDEVENT    := nID                  // !!! тек.запись входа в программу - сеанс работы
                                                   // !!! Current entry into the program - work session
      User2Log->NEVENT     := nEvtn                // Numeric      6   0
      User2Log->CEVENT     := cEvtn                // Character   40   0
      User2Log->COMPUTER   := NetName()            //
      User2Log->LOGIN      := hb_UserName()        //
      User2Log->NUSER      := App.Cargo:nUser      // Numeric      3   0
      User2Log->USER       := App.Cargo:cUser      // Character   70   0
      User2Log->DT_NEW     := dDT                  // TimeStamp    8   0
      User2Log->REM        := cRem                 // Character  500   0
      User2Log->( DbUnlock() )
   ENDIF
   User2Log->( DbCommit() )

   App.Cargo:nIDSession := nID    // тек.запись входа в программу -  Сессия-программы
                                  // current entry into the program - Session-program
   DBSELECTAREA(nSel0)

RETURN .T.

///////////////////////////////////////////////////////////////////
FUNCTION User2Close(cTime)
   LOCAL nEvtn, cEvtn, cRem

#ifdef KEY_ENG
   cEvtn := "Exit the program"
#else
   cEvtn := "Выход из программы"
#endif

   nEvtn := 900
   cRem  := ""
   User2LogWrite(nEvtn,cEvtn,cRem,cTime)

RETURN .T.

///////////////////////////////////////////////////////////////////
FUNCTION User2LogWrite(nEvtn, cEvtn, cRem, cTime)
   LOCAL cMsg, nSel, nSel0, dDT, lRecno := .F.
   Default nEvtn := -2 , cEvtn := "-?-" , cRem := "", cTime := ""

#ifdef KEY_ENG
   cMsg := "ACTION LOG DATABASE closed!;"
   cMsg += "Cannot write to the log! ;"
   cMsg += "You need to contact the program developer!"+SPACE(10)+";;"
#else
   cMsg := "БАЗА ЖУРНАЛА-событий закрыта !;"
   cMsg += "Не могу записать в журнал ! ;"
   cMsg += "Необходимо обратиться к разработчику программы !"+SPACE(10)+";;"
#endif

   nSel0   := SELECT()

   nSel := SELECT("User2Log")
   IF nSel == 0
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, , , 64, {RED} )
      ? ATREPL( ";", cMsg, CRLF )
   ELSE
      SELECT User2Log
      APPEND BLANK
      dDT := User2Log->DT_MODIFY
      IF User2Log->( RLock() )
         //User2Log->ID                               // AutoInc      4   0
         //User2Log->DT_MODIFY                        // ModTime      8   0
         FieldPut(FieldPos('EVENT'), dDT)
         //User2Log->EVENT    := dDT                  // TimeStamp    8   0
         User2Log->DEVENT     := DATE()               // Date         8   0
         User2Log->TEVENT     := TIME()               // Character    8   0
         User2Log->IDEVENT    := App.Cargo:nIDSession // !!!
         User2Log->NEVENT     := nEvtn                // Numeric      6   0
         User2Log->CEVENT     := cEvtn                // Character   40   0
         User2Log->COMPUTER   := NetName()            //
         User2Log->LOGIN      := hb_UserName()        //
         User2Log->NUSER      := App.Cargo:nUser      // Numeric      3   0
         User2Log->USER       := App.Cargo:cUser      // Character   70   0
         User2Log->DT_NEW     := dDT                  // TimeStamp    8   0
         //User2Log->DT_DEL                           // TimeStamp    8   0
         //User2Log->DT_REST                          // TimeStamp    8   0
         User2Log->REM        := cRem                 // Character  500   0
         User2Log->CTIME      := cTime                // program running time
         User2Log->( DbUnlock() )
      ENDIF
      User2Log->( DbCommit() )
   ENDIF //  SELECT("User2Log")

   DBSELECTAREA(nSel0)

Return NIL

/////////////////////////////////////////////////////////////////////
FUNCTION Struct_User2Log()
   LOCAL aStruct

   aStruct  := {;
   {"ID"        , "+",    4, 0},;
   {"DT_MODIFY" , "=",    8, 0},;
   {"DT_NEW"    , "@",    8, 0},;
   {"DT_DEL"    , "@",    8, 0},;
   {"DT_REST"   , "@",    8, 0},;
   {"IDEVENT"   , "N",    6, 0},;    // Сессия-программы       / Program-session
   {"EVENT"     , "@",    8, 0},;    // событие-дата-время     / event-date-time
   {"DEVENT"    , "D",    8, 0},;    // DATE()                 / DATE()
   {"TEVENT"    , "C",    8, 0},;    // TIME()                 / TIME()
   {"NEVENT"    , "N",    6, 0},;    // код события            / event code
   {"CEVENT"    , "C",   40, 0},;    // наименование события   / event name
   {"CTIME"     , "C",   15, 0},;    // время работы программы / program running time
   {"COMPUTER"  , "C",   20, 0},;    // Компьютер              / Computer
   {"LOGIN"     , "C",   20, 0},;    // Пользователь           / User
   {"NUSER"     , "N",    5, 0},;    // Код оператора          / Operator code
   {"USER"      , "C",   20, 0},;    // Оператор               / Operator
   {"REM"       , "M",    8, 0},;
   {"CTIME2"    , "C",   15, 0},;    // время работы юзера     / user work time
   {"D_06_09_25", "C",    1, 0};
       }

Return aStruct

////////////////////////////////////////////////////////////////////////
// проверка версии базы по последнему полю
// Checking the database version by the last field
FUNCTION ChkDbfUser2Log(cFileDbf,cAlias)
   LOCAL aStru, cPath, cVia, cCdPg, lNewArea, cLang, lNew, lRet, cAls
   LOCAL cErr, cMsg, cField, nF, cStru, aVal, cAls0, aLang, cMsg2

   ? ProcNL() , cFileDbf, cAlias
   cAls0 := ALIAS()
   cVia  := "DBFCDX"
   cCdPg := "RU1251"
   cPath := cFilePath( cFileDbf ) + "\"
   aStru := Struct_User2Log()
   aVal  := aStru[LEN(aStru)]
   cStru := UPPER(ALLTRIM(aVal[1]))
   cAls  := "TMP_" + cAlias

#ifdef KEY_ENG
   cLang := "Creating a Journal"
   aLang := { "Structure is correct !; ", "Last field: ", "Structure does not match !; " ,;
              "Could not check structure !; ", "Error: " }
   cMsg  := "Event LOG DATABASE!; Old database structure !; "
   cMsg2 := "Needs correction!;;Contact the program developer!;;"
#else
   cLang := "Создание журнала"
   aLang := { "Структура правильная !;", "  Посл.поле: ", "Структура не совпадает !; " ,;
              "Не смог проверить структуру !; ", "  Ошибка: "  }
   cMsg  := "БАЗА ЖУРНАЛА-событий !; Старая структура БД !;"
   cMsg2 := "Необходимо исправить !;;Обратиться к разработчику программы !;;"
#endif

   IF ".dbf" $ LOWER(cFileDbf)
   ELSE
      cFileDbf += ".dbf"
   ENDIF
   IF !FILE( cFileDbf )
      lNew := .T.
   ELSE
      lNew := .F.
      // значит база есть и нужно её проверить / So the database exists and it needs to be checked.
      cErr := ""
      lRet := myUseArea( cFileDbf, cAls, .T. , , "RU1251",,@cErr ) // lShared, cRdd, cCdp, nWhl )
      ? "   lRet=", lRet, cAls, cFileDbf
      IF lRet
         nF     := fCount()
         cField := FieldName( nF )
         IF cField == cStru  // "D_20_09_20"
            ? "--------------" + ProcNL()
            ? SPACE(5) + aLang[1] + cField
            ? SPACE(5) + aLang[2] + cField, "=", cStru
         ELSE
            ? "--------------" + ProcNL()
            ? SPACE(5) + aLang[3] + cFileDbf
            ? SPACE(5) + aLang[2] + cField, "#", cStru
            FILECOPY( cFileDbf, cFileDbf + ".old" )
            cMsg += cField + " # " + cStru + ";" + cMsg2
            AlertStop( cMsg , , , 64, {RED} )
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF
         (cAls)->( DbCloseArea() )
      ELSE
         ? "--------------" + ProcNL()
         ? SPACE(5) + aLang[4] + cFileDbf
         ? SPACE(5) + aLang[5] + cErr
      ENDIF
   ENDIF

   IF lNew
      lNewArea := .T.
      DbCreate( cFileDbf, aStru, cVia, lNewArea, cAlias, , cCdPg )
      SELECT User2Log
      APPEND BLANK
      IF User2Log->( RLock() )
         User2Log->EVENT      := User2Log->DT_MODIFY
         User2Log->DEVENT     := DATE()
         User2Log->TEVENT     := TIME()
         User2Log->IDEVENT    := User2Log->ID            // current entry into the program Session-program
         User2Log->NEVENT     := 1
         User2Log->CEVENT     := cLang
         User2Log->COMPUTER   := NetName()
         User2Log->LOGIN      := hb_UserName()
         User2Log->NUSER      := M->nOperat
         User2Log->USER       := M->cOperator
         User2Log->DT_NEW     := User2Log->DT_MODIFY
         User2Log->REM        := cLang
         User2Log->( DBUnlock() )
      ENDIF
      User2Log->( DBCommit() )
      User2Log->( DbCloseArea() )
   ENDIF

   IF LEN(cAls0) > 0
      DbSelectArea(cAls0)
   ENDIF

RETURN .T.

///////////////////////////////////////////////////////////////////
FUNCTION EVENTS_Dim(n)
   LOCAL a2Dim := {}
   DEFAULT n := 0

#ifdef KEY_ENG

   IF n == 0  // ошибки для поиска
      AADD( a2Dim, {   1, "Creating a log"       } )
      AADD( a2Dim, {   2, "Entering the program" } )
      AADD( a2Dim, { 900, "Exiting the program"  } )
      AADD( a2Dim, { 910, "Stopping the program" } )
      AADD( a2Dim, { 990, "Error exit !"         } )
      AADD( a2Dim, { 991, "Program error - test" } )
   ENDIF

   // ошибки для отчетов и поиска
   AADD( a2Dim, { 100, "Button: Log-Tsb"      } )
   AADD( a2Dim, { 101, "Button: Test 1"       } )
   AADD( a2Dim, { 102, "Button: Test 2"       } )
   AADD( a2Dim, { 103, "Button: Test 3"       } )
   AADD( a2Dim, { 104, "Button: Error"        } )
   AADD( a2Dim, { 105, "Button: Index"        } )
   AADD( a2Dim, { 300, "Button: search by phone" } )
   AADD( a2Dim, { 301, "Button: search by personal account" } )
   AADD( a2Dim, { 302, "Button: search by address" } )
   AADD( a2Dim, { 400, "New application"     } )
   AADD( a2Dim, { 501, "Print receipt A35"   } )
   AADD( a2Dim, { 502, "Print debt of subscriber A32" } )
   AADD( a2Dim, { 503, "Print debt of subscriber A33" } )

#else

   IF n == 0  // ошибки для поиска
      AADD( a2Dim, {   1, "Создание журнала"             } )
      AADD( a2Dim, {   2, "Вход в программу"             } )
      AADD( a2Dim, { 900, "Выход из программы"           } )
      AADD( a2Dim, { 910, "Остановка программы (*.stop)" } )
      AADD( a2Dim, { 990, "Ошибка программы !"           } )
      AADD( a2Dim, { 991, "Ошибка программы - тест"      } )
   ENDIF

   // ошибки для отчетов и поиска
   AADD( a2Dim, { 300, "Кнопка: поиск по телефону"   } )
   AADD( a2Dim, { 301, "Кнопка: поиск по л/счёту"    } )
   AADD( a2Dim, { 302, "Кнопка: поиск по адресу"     } )
   AADD( a2Dim, { 400, "Новая заявка"                } )
   AADD( a2Dim, { 401, "Новая заявка Старая прг."    } )
   AADD( a2Dim, { 402, "Новая заявка Основная прг."  } )
   AADD( a2Dim, { 501, "Печать квитанции А70"        } )
   AADD( a2Dim, { 502, "Печать долга абонента А32"   } )
   AADD( a2Dim, { 503, "Печать долга абонента А33"   } )

#endif

   a2Dim := ASORT( a2Dim,,, { | x, y | x[ 1 ] < y[ 1 ] } )

RETURN a2Dim

///////////////////////////////////////////////////////////////////
// search for event by number
FUNCTION FindEventsDim(nVal)
   LOCAL nI, a2Dim, cLang, cRet := ""

#ifdef KEY_ENG
   cLang := " didn't find the code "
#else
   cLang := " не нашёл кода "
#endif

   a2Dim := EVENTS_Dim()
   FOR nI := 1 TO LEN(a2Dim)
      IF nVal == a2Dim[nI,1]
         cRet := a2Dim[nI,2]
         EXIT
      ENDIF
   NEXT
   IF cRet == ""
      cRet := cLang + HB_NtoS(nVal)
   ENDIF

RETURN cRet
