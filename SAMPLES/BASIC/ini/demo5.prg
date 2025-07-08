/*
  * MINIGUI - Harbour Win32 GUI library Demo
  *
  * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
  * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
  *
  * Пример работы с ини-файлом через контейнер oHmgData() и Class TIniData
  * Преобразование строки ини-файла в нужные типы
  * An example of working with an ini file through a container oHmgData() and Class TIniData
  * Converting an ini file string to desired types
*/
#define _HMG_OUTLOG
#include "hmg.ch"
#include "hbclass.ch"

ANNOUNCE RDDSYS

Function Main()

   LOCAL oApp, oIni, oCom, aSec, cSec, oSec, nI, cFile, hIni, oTmp
   LOCAL cIni     := GetStartUpFolder() + "\demo5-utf8.ini"   // кодировка Utf-8
   LOCAL cIni2    := GetStartUpFolder() + "\demo5-utf8-2.ini" // новый файл
   LOCAL cFileLog := GetStartUpFolder() + "\_5Msg.log"

   //SET CODEPAGE TO UNICODE          // for Unicode version
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
   SET DATE     TO GERMAN

   App.Cargo := oHmgData() ; oApp := App.Cargo

   oApp:cLog  := cFileLog   ; fErase( oApp:cLog )
   oApp:cIni  := cIni
   oApp:cIni2 := cIni2

   SET LOGFILE TO (oApp:cLog)            // отладочный log файл

   oApp:oIni := oIniData( cIni, .T. ):Read()
   //oApp:oIni := oIniData( cIni, .T. ) ; oIni:Read()
   //oApp:oIni := oIniRead( cIni, .T. )

   // в качестве примера чтение нескольких ини-файлов
   //oApp:oIni1 := oIniRead( ".\demo1.ini", .T. )     // .T. - lMacro
   //oApp:oIni2 := oIniRead( ".\demo2.ini", .T. )

   ? Repl("-",20) + " example log file: " + cFileNoPath( App.ExeName ) + Repl("-",20) ; ?

   // oIni := App.Cargo:oIni
   oIni := oApp:oIni                     // берем адрес объекта oIni и от него работаем

   aSec := oIni:Keys()                   // все секции ини-файла

   ? "all sections of ini-file " + cIni+" =", aSec, hb_valtoexp(aSec) ; ?

   FOR EACH cSec IN oIni:Keys()          // перебираем секции
       oSec := oIni:Get(cSec)
       ? cSec, oSec:GetAll() ; ?v oSec:GetAll() ; ?
   NEXT

   cSec := [COM] //[COMMON]  /* нет такой секции */
   oCom := oIni:Get(cSec, oIniData())               // секция [COM], параметр 2, если нет [COM]
   ? cSec+" =", oCom:GetAll() ; ?v oCom:GetAll() ; ?
   aSec := oCom:GetAll()     // вытаскиваем все из секции
   FOR nI := 1 TO LEN(aSec)
       aVal := aSec[ nI ]
       cKey := aVal[1]
       xVal := aVal[2]
       ? nI, cKey, VALTYPE(xVal) , xVal
   NEXT
   // или так
   IF Len(aSec) > 0
      FOR EACH aVal IN aSec
          cKey := aVal[1]
          xVal := aVal[2]
          ? hb_enumindex(aVal), cKey, VALTYPE(xVal) , xVal
      NEXT
   ENDIF

   // Проверка наличия ключа
   oApp:lLanguage := oCom:Pos("Language") > 0  // позиция ключа в контейнере

   IF oApp:lLanguage
      ? "["+cSec+"]  Key exists Language=", oCom:Language
   ELSE
      ? "["+cSec+"]  NO key Language = !"
   ENDIF

   Test( oIni ) // проверка переменных из ини

   // записать новый ини-файл
   cFile := oApp:cIni2
   ? "New file ini =", cFile

   //oIni:cCommentBegin := "# my Start !"
   //oIni:cCommentEnd   := "# my Stop !"
   //oIni:lYesNo := .T.             // Yes или No в логических значениях при создании ini используем
   //oIni:aYesNo := {"Да", "Нет"}   // Yes или No в логических значениях при создании ini

   //oIni:Write( cFile, .F. )     // НЕ UTF8, т.е. нет BOM на выходе (на входе был с BOM)
   oIni:Write( cFile )            // как оригинальный файл UTF8 с BOM

   ? ; ? "--- End ---"
   DO EVENTS

   // показать отладочный файл
   cStr     := HB_MemoRead(App.Cargo:cLog)
   cFileLog := GetStartUpFolder() + "\_"+Set( _SET_CODEPAGE )+".log"
   HB_MemoWrit( cFileLog, cStr )

   ShellExecute( 0, "Open", cFileLog, ,, SW_SHOWNORMAL )

Return

///////////////////////////////////////////////////////////////////////
FUNCTION Test( oIni )

   LOCAL nMode, lLog, cPath, aLang, bClr1, xTest, cBtn5, xVal
   LOCAL oCom, cSec := [COM], Dtm1, Dtm2, Dtm3, Bufer, Buf2

   ? "------------- verify variables from ini-file ---------"
   // читать переменные - секция [RU] переменная "Btn_05"
   cBtn5 := oIni:RU:Btn_05 ; Default cBtn5 := "none"
   ? "oIni:RU:Btn_05 = ", cBtn5

   // читать переменные - секция [COM] переменная "ModeBAK"
   nMode := oIni:Com:ModeBAK ; Default nMode := 0
   // или так
   oCom  := oIni:Get( cSec, oIniData() ) // это просто адрес в oCom
   nMode := oCom:ModeBAK ; Default nMode := 0
   // можно в отдельной ф-ии проверить все ключи и добавить их в oIni в
   // нужную секцию, что бы потом просто работать без Default nMode := 0

   nMode := oCom:Get("ModeBAK", 0)                // это функция-метод
   nMode := oCom:ModeBAK ; Default nMode := 0     // это удобно нет кавычек

   // писать\устанавливать в oIni:COM
   nMode := 21
   oCom:ModeBAK := nMode
   oCom:Set("ModeBAK", nMode)                 // это функция-метод

   nMode := oCom:Get("ModeBAK" , 0   )
   lLog  := oCom:Get("lLangLog", .F. )
   cPath := oCom:Get("PathXml" , ""  )
   aLang := oCom:Get("aLangName", {} )
   bClr1 := oCom:Get("Color_1"  , {||Nil} )
   xTest := oCom:Get("PathXXX"  , "not" )
   Dtm1  := oCom:Dtm1
   Dtm2  := oCom:Dtm2
   Dtm3  := oCom:Dtm3
   Bufer := oCom:Buffer
   Buf2  := oCom:Buffer2
   xVal  := oCom:None     // такой переменной нет

   ? "nMode=", ValType(nMode) , nMode
   ? "lLog =", ValType(lLog ) , lLog
   ? "cPath=", ValType(cPath) , cPath
   ? "aLang=", ValType(aLang) , aLang, HB_ValToExp(aLang)
   ? "bClr1=", ValType(bClr1) , bClr1
   ? "xTest=", ValType(xTest) , xTest
   ? "Dtm1 =", ValType(Dtm1 ) , Dtm1
   ? "Dtm2 =", ValType(Dtm2 ) , Dtm2
   ? "Dtm3 =", ValType(Dtm3 ) , Dtm3
   ? "Bufer=", ValType(Bufer) , Bufer
   ? "Buf2 =", ValType(Buf2)  , Buf2
   ? "xVal =", ValType(xVal)  , xVal

   ? "------- through the own function GetIniData() ------"
   ? "ModeBAK" , GetIniData( oIni, [COM], "ModeBAK"  ,  0 )
   ? "ModeNone", GetIniData( oIni, [COM], "ModeNone" , -1 )
   ?

Return Nil

///////////////////////////////////////////////////////////////////////
FUNCTION GetIniData(oIni, cSection, cKey, xDefault)

   LOCAL oSect, cSect, cErr, cIni := App.Cargo:cIni

   oSect := oIni:Get(cSection, oIniData())

   // Check of key existing
   IF oSect:Pos(cKey) > 0         // position of key
      xRet := oSect:Get(cKey)
      // xRet := oSect:&(cKey)    // other way
   ELSE
      cErr := 'Ошибка ! Секция [' + cSection + ']' + CRLF
      cErr += 'Нет ключа "' + cKey + '" = ...' + CRLF
      cErr += 'Исправьте ключ в ини-файле !' + CRLF + CRLF
      cErr += 'Чтение ини-файла ' + cIni + CRLF + CRLF
      cErr += 'Error! Section ['+ cSection +'] '+ CRLF
      cErr += 'No key "' + cKey + '" = ...' + CRLF
      cErr += 'Correct the key in the ini file!' + CRLF + CRLF
      cErr += 'Reading ini-file ' + cIni + CRLF + CRLF
      cErr += ProcName(0) + "(" + HB_NtoS(ProcLine(0)) + ")" + CRLF
      cErr += ProcName(1) + "(" + HB_NtoS(ProcLine(1)) + ")" + CRLF
      cErr += ProcName(2) + "(" + HB_NtoS(ProcLine(2)) + ")" + CRLF
      MsgStop(cErr, "Error of ini-file" )
      xRet := xDefault
   ENDIF

Return xRet

///////////////////////////////////////////////////////////////////////
FUNCTION oIniData( cIni, lMacro, lUtf8, cRazd )
RETURN TIniData():New( cIni, lMacro, lUtf8, cRazd )

///////////////////////////////////////////////////////////////////////
FUNCTION oIniRead( cIni, lMacro, lUtf8, cRazd )
RETURN oIniData( cIni, lMacro, lUtf8, cRazd ):Read()
