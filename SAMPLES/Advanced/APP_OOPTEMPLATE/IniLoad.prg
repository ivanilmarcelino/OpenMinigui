/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Andrey Verchenko <verchenkoag@gmail.com>. Dmitrov, Russia
 *
*/
#define _HMG_OUTLOG
#include "minigui.ch"

#define LEN_SPC     50
#define INI_FILE  ChangeFileExt( Application.ExeName, ".ini" )
///////////////////////////////////////////////////////////////////////
FUNCTION oIniData( cIni, lMacro, lUtf8, cRazd )
RETURN TIniData():New( cIni, lMacro, lUtf8, cRazd )

///////////////////////////////////////////////////////////////////////
FUNCTION oIniRead( cIni, lMacro, lUtf8, cRazd )
RETURN oIniData( cIni, lMacro, lUtf8, cRazd ):Read()

///////////////////////////////////////////////////////////////////////
// Функция присвоение в object-hash данных по ключу
// Запись на диск происходит всего hash ini по методу write -> IniWriteParam()
FUNCTION SetIniData( oIni, cSection, cKey, xVal )
   oIni:Get(cSection):Set(cKey, xVal)
RETURN .T.  // или oIni:Get(cSection):Get(cKey) != NIL
            // или oIni:Get(cSection):Pos(cKey) > 0

///////////////////////////////////////////////////////////////////////
FUNCTION GetIniData2(oIni, cSec, cKey, xDef)
    LOCAL oSec := oIni:Get(cSec, oHmgData())
    LOCAL xRet := oSec:Get(cKey, xDef)

    IF HB_ISBLOCK(xRet) ; xRet := EVal(xRet)
    ENDIF

 RETURN xRet

///////////////////////////////////////////////////////////////////////
FUNCTION GetIniData(oIni, cSection, cKey, xDefault, lSay)
   LOCAL oSect, cErr, xRet, cIni, cPath, cType, cMsg, cPnl
   DEFAULT lSay := .F.

   // секция DATA_SOURCE должна быть во всех файлах .Ini .Cfg
   IF ! Empty( oIni:Get("Data_Source") )
      cIni  := oIni:DATA_SOURCE:cFile  // секцию пишу большими буквами, что бы понимать,
      cPath := oIni:DATA_SOURCE:cPath  // что работаю с переменными секции
      cType := oIni:DATA_SOURCE:cType
   ELSE
      cIni  := "не определено"
      cPath := "не определено"
      cType := "не определено"
   ENDIF
   cMsg := '{' + cIni + ',' + cPath + ',' + cType + '}'
   cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
   cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)

   IF Empty( oSect := oIni:Get(cSection) )      // NIL
      // not found section
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += 'Ошибка ! Секция [' + cSection + '];'
      cErr += 'Нет такой секции ! ;;'
      cErr += cMsg + ";;"
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "Ошибка в ини-файле" )
      ? AtRepl( ";", cErr, CRLF )
      RETURN xDefault
   ENDIF

   xRet := oSect:Get(cKey)
   //? ProcNL(), "**************", cKey, "=", xRet
   IF xRet == NIL
      // not found key
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += 'Ошибка ! Секция [' + cSection + '];'
      cErr += 'Нет ключа "' + cKey + '" = ...;'
      cErr += 'Исправьте ключ в ини-файле !;;'
      cErr += cMsg + ";;"
      cErr += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      IF lSay
         AlertStop(cErr, "Ошибка в ини-файле" )
         ? AtRepl( ";", cErr, CRLF )
      ENDIF
      xRet := xDefault
   ELSE
      //xRet := oSect:Get(cKey)
      // xRet := oSect:&(cKey)    // other way
   ENDIF

RETURN xRet

/////////////////////////////////////////////////////////////////////
FUNCTION GetIniFor(oCnf, cSection, cKeyMsk, xDefault, lSay)
   LOCAL nI, xI, xVal, cErr, aDim, cKey, oSec, cPnl
   LOCAL cIni, cPath, cType, cMsg
   DEFAULT cKeyMsk := cSection := "", lSay := .F.

   oSec := oCnf:Get("Data_Source")
   //? ProcNL(), oSec, Empty( oSec )
   //_o2Log(oSec, 10, "==> .T. ini: ", .T.)

   // секция DATA_SOURCE должна быть во всех файлах .Ini .Cfg
   IF ! Empty( oCnf:Get("Data_Source") )
      cIni  := oCnf:DATA_SOURCE:cFile  // секцию пишу большими буквами, что бы понимать,
      cPath := oCnf:DATA_SOURCE:cPath  // что работаем с переменными секции
      cType := oCnf:DATA_SOURCE:cType
   ELSE
      cIni  := "не определено"
      cPath := "не определено"
      cType := "не определено"
   ENDIF
   cMsg := '{' + cIni + ',' + cPath + ',' + cType + '}'
   cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
   cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)

   IF Empty( oCnf:Get(cSection) )
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += 'Секция [' + cSection + '];'
      cErr += 'Нет такой секции !;'
      cErr += 'cSection = "' + cSection + '"   <<---  ОШИБКА !;;'
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "Ошибка в ини-файле" )
      ? AtRepl( ";", cErr, CRLF )
      RETURN {}
   ENDIF

   IF LEN(cKeyMsk) == 0
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += 'Секция [' + cSection + '];'
      cErr += 'Нет такой маски ключа ! Маска пустая !;'
      cErr += 'cKeyMaska = ""     <<---  ОШИБКА !;;'
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "Ошибка в ини-файле" )
      ? AtRepl( ";", cErr, CRLF )
   ENDIF

   aDim := {}
   FOR EACH xI IN Array(1000)
      nI   := hb_enumindex(xI)
      cKey := cKeyMsk + HB_NtoS(nI)
      xVal := GetIniData(oCnf, cSection, cKey, xDefault, lSay)
      //                                       ^^^^ - вернуть это значение если нет ключа
      //                                 ^^^^ - это ключ
      //                       ^^^^^^^^ - это секция
      //      ^^^^^^^^^^^^^^^ - читаем значение ключа
      //? SPACE(3) + "...", nI, cSection, cKey, xVal
      IF xVal == NIL ; EXIT
      ENDIF
      IF LEN(xVal) == 0 ; EXIT
      ENDIF
      AADD( aDim, xVal )
   NEXT

   IF LEN(aDim) == 0
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += 'Секция [' + cSection + '];'
      cErr += 'Не смог считать цикл по маске ключа ! ;'
      cErr += 'cKeyMaska = "' + cKeyMsk + '"   <<---  ОШИБКА !;;'
      cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
      cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC)
      AlertStop(cErr, "Ошибка в ини-файле" )
      ? AtRepl( ";", cErr, CRLF )
   ENDIF

RETURN aDim
/////////////////////////////////////////////////////////////////////
Function IniFileYes()
   Local cText, cFileIni, lRet := .T.

   cFileIni := INI_FILE
   IF ! File( cFileIni )
      cText := "[Information]" + CRLF
      cText += "PROGRAM   = " +  App.Cargo:cProgTtlEn + CRLF
      cText += "ABOUTPRG  = " +  App.Cargo:cProgTtlRu + CRLF
      cText += "PROGVERS  = " +  App.Cargo:cProgVersion + CRLF
      cText += "Copyright = " +  App.Cargo:cCopyright + CRLF
      cText += "Email     = " +  App.Cargo:cEmail + CRLF
      cText += "ExeName   = " + Application.ExeName + CRLF
      cText += "Developed_in   = " + MiniGUIVersion() + CRLF
      cText += "xBase_compiler = " + Version()        + CRLF
      cText += "C_compiler     = " + Hb_Compiler()    + CRLF
      cText += CRLF + CRLF
      cText += '[COM]' + CRLF
      cText += "DateEdit   = " + HB_TTOC( HB_DATETIME() ) + CRLF
      //HB_MemoWrit( cFileIni, cText )
      lRet := .F.
      cText := "There is no ini-file for the program !;"
      cText += cFileIni + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cText)
      lRet := .F.
      ?? cText
   ENDIF

   // сохраним здесь глобальный ини-файл
   App.Cargo:cFileIni := cFileIni

Return lRet

//////////////////////////////////////////////////////////////////////////////
Function IniGetParam()
   LOCAL nMenu, oIni, cSec, oSec, aSec, cPath, aDim, nVal, lVal, cVal, lUtf8
   LOCAL cFileIni

   cFileIni := App.Cargo:cFileIni
   lUtf8    := .F.    // лучше .T. и файл делать в UTF-8  // делаю в кодировке RU1251
   // параметр при чтении Ini файла, который вкл. или нет
   // алгоритм получения значения ключей уже в нужном формате\типе данных, а не только "C", т.е.
   //oIni := TIniData():New(cIni, .T.):Read() - значения для ключей будут форматированы по типам
   //oIni := TIniData():New(cIni, .F.):Read() - значения для ключей будут только типа "C"
   //oIni := oIniData( cFileIni, .T., lUtf8, ):Read() - это читать из файла
   //oIni -> это hash с вложенными hash секциями и вложенными в них hash ключами
   // --- ВНИМАНИЕ!
   // класс сделан на функции чтения ini в hash hIni := hb_IniRead(...)
   // особенность для "[", встреченный в любом месте файла, воспринимается как начало секции,
   // т.е. след. символы - имя секции [TEST], кроме строк комментарий со знаком ";"
   //Пример 1 = { "test2.dbf" , "[TEST]"  , "[секция]", "test" , "!Deleted[]" }
   // делаем замену "[]" на "<>" везде кроме секций и строк со знаком ";"
   //Пример 1 = { "test2.dbf" , "<TEST>"  , "[секция]", "test" , "!Deleted<>" }
   // особенность-2 для функций, если нет функции, то массив превращается в строку
   // не желательно так использовать - myFunColor(), т.к. не определён тип переменной
   //Пример 2 = { "funct.dbf" , myFunColor() , "Func2"    , "Func"   , "SHARED" , 3 }
   // потом выполнить a[2] := Eval( a[2] )
   //Пример 3 = { "funct.dbf" , {|| myFunColor() }, "Func2"    , "Func"   , "SHARED" , 3 }
   // потом выполнить a[2] := Eval( a[2] )

   // считать настройку ини-файл в глобальную App.Cargo из файла App.Cargo:cFileIni
   oIni := oIniData( cFileIni, .T., lUtf8, ):Read()

   /* не надо STATIC/PUBLIC переменных, используем
   LOCAL oIni := App.Cargo:oIni
   LOCAL oCom := oIni:COM                  // секция [COM]
   или сразу, если ключей из др. секций не надо
   LOCAL oCom := App.Cargo:oIni:COM        // oCom := oIni:COM
   LOCAL oPrj := App.Cargo:oIni:MergePrj   // секция [MergePrj]
   или
   LOCAL o := oIni:MergePrj */
   /* ------------ как писать даты ---------
   cDate := "0d"+Dtos(Date())
   cDate := hb_valtoexp(Date())
   cDateTime := hb_Valtoexp(hb_DataTime())
   cDateTime := "t"+hb_TtoS(hb_DataTime())
   */
   // добавляем руками в oIni
   oIni:Data_Source := oHmgData()  // секцию добавили (строим дерево контейнеров)
   oIni:Data_Source:cFile := cFileNoPath(cFileIni)
   oIni:Data_Source:cPath := cFilePath(cFileIni)
   oIni:Data_Source:cType := "File"
   // или
   IF !Empty( oSec := oIni:Get("Data_Source") )
      oIni:Set("Data_Source", oHmgData())  // секцию добавили
      oIni:Data_Source:Set("cFile", cFileNoPath(cFileIni))
      oIni:Data_Source:Set("cPath", cFilePath(cFileIni))
      oIni:Data_Source:Set("cType", "File")
   ENDIF
   // или
   IF !Empty( oSec := oIni:Get("Data_Source") )
      oSec:Set("cFile", cFileNoPath(cFileIni))
      oSec:Set("cPath", cFilePath(cFileIni))
      oSec:Set("cType", "File")
   ENDIF

   // запомнить ini-файл в глобальную App.Cargo
   App.Cargo:oIni := oIni

   aSec := oIni:Keys()                   // все секции ини-файла
   ? ProcNL(), "Ini-file " + cFileIni ; ? "   All sections =", aSec, hb_valtoexp(aSec)

   FOR EACH cSec IN oIni:Keys()          // перебираем секции
      oSec := oIni:Get(cSec)
      //? "   Section: " + cSec, "Number of variables in section", oSec:GetAll()
      // ?v oSec:GetAll() ; ?  // все значения переменных из ини-файла
   NEXT

   //_o2Log(oIni, 15, "==> .F. ini: ", .F.) ; ?

   oSec := oIni:Get("Data_Source")
   //_o2Log(oSec, 10, "==> .T. ini: ", .T.) ; ?

   // читать русские переменные - секция [PATH]  доступ только через функции
   App.Cargo:nVerNetworkLocal := oIni:PATH:Get("Версия_сеть_локальный", "")
   App.Cargo:cDirTemp         := oIni:PATH:Get("Временная папка"      , "")
   App.Cargo:cDirExec         := oIni:PATH:Get("Папка с программой"   , "")
   App.Cargo:cPath_Server     := oIni:PATH:Get("Путь_на_сервер"       , "")
   App.Cargo:cPath_Local      := oIni:PATH:Get("Путь_локальный"       , "")
   App.Cargo:lFileSay         := GetIniData( oIni, "PATH", "Показ_файлов", .F. , .T.)
   IF App.Cargo:nVerNetworkLocal == 1
      cPath := App.Cargo:cPath_Server
   ELSE
      cPath := App.Cargo:cPath_Local
   ENDIF
   App.Cargo:cPath_Dbase := cPath + oIni:PATH:Get("Путь к базам"    , "")
   App.Cargo:cPath_Index := cPath + oIni:PATH:Get("Путь к индексам" , "")
   App.Cargo:cPath_Semaf := cPath + oIni:PATH:Get("Путь к семафору" , "")

   ? ProcNL(), "русские переменные - секция [PATH]  доступ только через функции"
   ? SPACE(3)+">", App.Cargo:nVerNetworkLocal
   ?? App.Cargo:cDirTemp
   ?? App.Cargo:cDirExec
   ?? App.Cargo:cPath_Server
   ?? App.Cargo:cPath_Local
   ? SPACE(3)+">", App.Cargo:cPath_Dbase
   ?? App.Cargo:cPath_Index
   ?? App.Cargo:cPath_Semaf
   ?? "Показ_файлов=",App.Cargo:lFileSay

   // читать переменные - секция [PATH_EN]
   nVal := oIni:PATH_EN:Version_network_local   ; Default nVal := 0
   App.Cargo:nVerNetworkLocal  := nVal          // Версия_сеть_локальный

   cVal := oIni:PATH_EN:Temporary_folder        ; Default cVal := ""
   App.Cargo:cDirTemp          := cVal          // Временная папка

   cVal := oIni:PATH_EN:Program_folder          ; Default cVal := ""
   App.Cargo:cDirExec         := cVal           // Папка с программой

   cVal := oIni:PATH_EN:Path_PCServer           ; Default cVal := ""
   App.Cargo:cPath_Server     := cVal           // Путь_на_сервер

   cVal := oIni:PATH_EN:Path_PCLocal            ; Default cVal := ""
   App.Cargo:cPath_Local      := cVal           // Путь_локальный

   ? ProcNL(), "читать переменные - секция [PATH_EN]"
   ? SPACE(3)+">", App.Cargo:nVerNetworkLocal , nVal
   ?? App.Cargo:cDirTemp
   ?? App.Cargo:cDirExec
   ?? App.Cargo:cPath_Server
   ?? App.Cargo:cPath_Local
   App.Cargo:lFileSay := GetIniData( oIni, "PATH_EN", "Path_lSay", .F. , .T.)
   ?? "Path_lSay=", App.Cargo:lFileSay

   // читать переменные - секция [COM]
   lVal := oIni:Com:lShow_COPYDATA       ; Default lVal := .F.
   App.Cargo:WM_CD_lShow := lVal         // Показ окна WM_COPYDATA

   lVal := oIni:Com:lCopyDataLog         ; Default lVal := .F.
   App.Cargo:lCopyDataLog := lVal        // Удалить файл _copydata.log

   lVal := oIni:Com:lFileLog             ; Default lVal := .F.
   App.Cargo:lFileLog := lVal            // Режим файл cFileLog F-не удалять/T-Удалить

   nVal := oIni:Com:TimerSec             ; Default nVal := 30
   App.Cargo:nTimerSec := nVal           // таймер для отображения времени Timer_1 каждые 30 секунд

   Default App.Cargo:lCopyFile := .F.
   lVal := oIni:Com:lCopyFile            ; Default lVal := .F.
   App.Cargo:lCopyFile := lVal           // Режим копирования файлов F-проверка/T-замена

   nMenu := oIni:Com:nWorkMode           ; Default nMenu := 1        // режим сравнения в программе
   App.Cargo:nMenuType := nMenu          // записать - тип меню действия Сравнения

   // ---------------------- цвета таблицы / окон -----------------------
   nVal := oIni:Com:nModeColor   ; Default nVal := 1          // режим цветов программы
   App.Cargo:nModeColor := nVal                               // записать

   nVal := oIni:Com:nTsbColor1    ; Default nVal := 5460819
   App.Cargo:nTsbColor1 := nVal                                // записать

   nVal := oIni:Com:nTsbColor2    ; Default nVal := 8421504
   App.Cargo:nTsbColor2 := nVal                                // записать

   nVal := oIni:Com:nTsbColor3    ; Default nVal := 3881787
   App.Cargo:nTsbColor3 := nVal                                // записать

   nVal := oIni:Com:nTsbColor4    ; Default nVal := 11075583
   App.Cargo:nTsbColor4 := nVal                                // записать

   nVal := oIni:Com:nTsbColor5    ; Default nVal := 8388736
   App.Cargo:nTsbColor5 := nVal                                // записать

   nVal := oIni:Com:nTsbColor6    ; Default nVal := 568567
   App.Cargo:nTsbColor6 := nVal                                // записать

   nVal := oIni:Com:nTsbClr01     ; Default nVal := 0
   App.Cargo:nTsbClr01 := nVal                                // записать

   nVal := oIni:Com:nTsbClr01a    ; Default nVal := 16777215
   App.Cargo:nTsbClr01a := nVal                                // записать

   nVal := oIni:Com:nTsbClr16     ; Default nVal := 6381921
   App.Cargo:nTsbClr16 := nVal                                // записать

   nVal := oIni:Com:nTsbClr17     ; Default nVal := 4227327
   App.Cargo:nTsbClr17 := nVal                                // записать

   nVal := oIni:Com:nTsbClr04     ; Default nVal := 4539717
   App.Cargo:nTsbClr04 := nVal                                // записать

   nVal := oIni:Com:nTsbClr03     ; Default nVal := 33023
   App.Cargo:nTsbClr03 := nVal                                // записать

   nVal := oIni:Com:nTsbClrSpecHd1 ; Default nVal := 2960685
   App.Cargo:nTsbClrSpecHd1 := nVal                          // записать

   nVal := oIni:Com:nTsbClrSpecHd2 ; Default nVal := 12632256
   App.Cargo:nTsbClrSpecHd2 := nVal                          // записать

   nVal := oIni:Com:nTsbClr2Ico    ; Default nVal := 9145227
   App.Cargo:nTsbClr2Ico  := nVal                          // записать

   nVal := oIni:Com:nClrWinMain1  ; Default nVal := 4144959
   App.Cargo:nClrWinMain1 := nVal                                // записать

   nVal := oIni:Com:nClrWinMain2  ; Default nVal := 14737632
   App.Cargo:nClrWinMain2 := nVal                                // записать

   nVal := oIni:Com:nClrWin1Btn   ; Default nVal := 32896
   App.Cargo:nClrWin1Btn := nVal                                // записать

   nVal := oIni:Com:nClrWinCnfg   ; Default nVal := 6118749
   App.Cargo:nClrWinCnfg := nVal                                // записать

   aDim := oIni:Com:aTsbMyColor   ; Default aDim := {}
   App.Cargo:aTsbMyColor := aDim                                // записать

   // ---------------------- фонты таблицы -----------------------
   aDim := oIni:Com:aTsbFont_1  ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F., 0, 18, 31, "Norm"} // Фонт колонок таблицы
   App.Cargo:aTsbFont_1 := aDim

   aDim := oIni:Com:aTsbFont_2  ; Default aDim := {"Times New Roman", 13, .T., .F., .F., .F., 0, 18, 31, "Bold"}  // Фонт заголовка таблицы
   App.Cargo:aTsbFont_2 := aDim

   aDim := oIni:Com:aTsbFont_3  ; Default aDim := {"Times New Roman", 12, .F., .F., .F., .F., 0, 18, 31, "Ital"}  // Фонт подвала таблицы
   App.Cargo:aTsbFont_3 := aDim

   aDim := oIni:Com:aTsbFont_4  ; Default aDim := {"Arial", 11, .F., .T., .F., .F., 0, 21, 38, "SpecHd"}          // Фонт спецхидера/нумератора
   App.Cargo:aTsbFont_4 := aDim

   aDim := oIni:Com:aTsbFont_5  ; Default aDim := {"DejaVu Sans Mono", 12, .T., .F., .F., .F., 0, 21, 38, "SuperHd"} // Фонт суперхидера
   App.Cargo:aTsbFont_5 := aDim

   aDim := oIni:Com:aTsbFont_6  ; Default aDim := {"Arial", 12, .F., .F., .F., .F., 0, 0, 0, "Edit"}              // Фонт редактирования ячейки
   App.Cargo:aTsbFont_6 := aDim

   App.Cargo:aTsbFonts := { App.Cargo:aTsbFont_1, App.Cargo:aTsbFont_2, App.Cargo:aTsbFont_3, ;
                            App.Cargo:aTsbFont_4, App.Cargo:aTsbFont_5, App.Cargo:aTsbFont_6  }     // все фонты таблицы

   // ----------------------- фонты окна и кнопок окна -----------------------
   aDim := oIni:Com:aFntBtnMain   ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aFntBtnMain := aDim                      // Фонт кнопок главной формы
   App.Cargo:cFName      := App.Cargo:aFntBtnMain[1]
   App.Cargo:nFSize      := App.Cargo:aFntBtnMain[2]
   App.Cargo:lFBold      := App.Cargo:aFntBtnMain[3]

   aDim := oIni:Com:aWinFont    ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aWinFont := aDim                         // Фонт для формы всех окон
   App.Cargo:cFName   := App.Cargo:aWinFont[1]
   App.Cargo:nFSize   := App.Cargo:aWinFont[2]

   aDim := oIni:Com:aBtnFont_1  ; Default aDim := {"Comic Sans MS", 14, .T., .F.}
   App.Cargo:aBtnFont_1 := aDim

   aDim := oIni:Com:aBtnFont_2  ; Default aDim := {"Snap ITC"     , 14, .T., .F.}
   App.Cargo:aBtnFont_2 := aDim

   App.Cargo:aBtnFont  := App.Cargo:aBtnFont_1   // Фонт кнопок других форм
   App.Cargo:cFName2   := App.Cargo:aBtnFont[1]
   App.Cargo:nFSize2   := App.Cargo:aBtnFont[2]

   // фонты в контекстном меню
   aDim := oIni:Com:aFontCnMn1  ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn1 := aDim

   aDim := oIni:Com:aFontCnMn2  ; Default aDim := {"Arial", 13, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn2 := aDim

   aDim := oIni:Com:aFontCnMn3  ; Default aDim := {"Comic Sans MS", 14, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn3 := aDim

   aDim := oIni:Com:aFontCnMn4  ; Default aDim := {"Times New Roman", 14, .t., .F., .F., .F. }
   App.Cargo:aFontCnMn4 := aDim

   App.Cargo:aFontCnMn  := { App.Cargo:aFontCnMn1, App.Cargo:aFontCnMn2 ,;
                             App.Cargo:aFontCnMn3, App.Cargo:aFontCnMn4 }  // все фонты в контекстном меню

   // --------------------- колонки таблицы ----------------------
   lVal := oIni:Com:lTsbCol10    ; Default lVal := .T.         // показ 10 колонки
   App.Cargo:lTsbCol10 := lVal                                 // записать

Return Nil

/////////////////////////////////////////////////////////////////////
// Запись переменной в ини файл
Function IniSetWrite(cSection,cName,xVal)
   LOCAL oIni, oSec, cMsg

   oIni := App.Cargo:oIni                    // берем адрес объекта oIni и от него работаем

   IF Empty( oSec := oIni:Get(cSection) )    // нет секции
      oIni:Set(cSection, oHmgData())         // секцию добавили
      cMsg := "Добавили секцию [" + cSection + "] в файл;"
      cMsg += INI_FILE + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertInfo(cMsg)
   ENDIF

   IF !Empty( oSec := oIni:Get(cSection) )
      oSec:Set(cName, xVal)
   ENDIF

   IniWriteParam() // Запись всего ини файла

RETURN NIL

/////////////////////////////////////////////////////////////////////
// Запись всего ини файла
Function IniWriteParam()
   LOCAL oIni, oInfo, cInfo := [Information]

   oIni := App.Cargo:oIni                    // берем адрес объекта oIni и от него работаем

   oInfo := oIni:Information                 //  секции [Information]
   oInfo:Developed_in   := MiniGUIVersion()
   oInfo:xBase_compiler := Version()
   oInfo:C_compiler     := Hb_Compiler()
   oInfo:IniEdit        := HB_TTOC( HB_DATETIME() )

   // записать новый ини-файл
   //cFile := oApp:cIni2
   //? "New file ini =", cFile

   //oIni:cCommentBegin := "# my Start !"
   //oIni:cCommentEnd   := "# my Stop !"
   //oIni:lYesNo := .T.             // Yes или No в логических значениях при создании ini используем
   //oIni:aYesNo := {"Да", "Нет"}   // Yes или No в логических значениях при создании ini

   //oIni:Write( cFile, .F. )     // НЕ UTF8, т.е. нет BOM на выходе (на входе был с BOM)
   //oIni:Write( cFile )            // как оригинальный файл UTF8 с BOM

   /*
      oIni:Write( cFile, .F. )     // НЕ UTF8, т.е. нет BOM на выходе
                    ^     ^
   Если не задавать cFile,|то имя будет тоже, что было определено при создании oIni
   Если не задавать 2-ой -- параметр, то кодировка будет та же что и при создании
   oIni, т.е. если был BOM, то Utf-8, если его не было, то и при :Write() не будет
   */

   // Запись значения на переменные - через метод :Set(...), т.е.
   // пример как делать
   oIni:PATH:Set("Временная папка"   , App.Cargo:cDirTemp      )
   oIni:PATH:Set("Папка с программой", App.Cargo:cDirExec      )
   oIni:PATH:Set("Путь_на_сервер"    , App.Cargo:cPath_Server  )
   oIni:PATH:Set("Путь_локальный"    , App.Cargo:cPath_Local   )

   // Запись значения на переменные
   // пример как делать
   oIni:PATH_EN:Temporary_folder := App.Cargo:cDirTemp
   oIni:PATH_EN:Program_folder   := App.Cargo:cDirExec
   oIni:PATH_EN:Path_PCServer    := App.Cargo:cPath_Server
   oIni:PATH_EN:Path_PCLocal     := App.Cargo:cPath_Local

   oIni:Write()     // НЕ UTF8, т.е. нет BOM на выходе

Return Nil
