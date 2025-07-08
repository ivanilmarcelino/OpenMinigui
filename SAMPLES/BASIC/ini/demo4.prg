/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Пример работы с ини-файлом через контейнер oHmgData()
 * An example of working with an ini file through a container oHmgData()
*/

#define _HMG_OUTLOG
#include "minigui.ch"

ANNOUNCE RDDSYS

Function Main()
  Local o := oHmgData()    // oIni
  Local c := oHmgData()    // oSection
  Local a := oHmgData()    // oLanguage
  Local cIni := "demo4.ini", s, aDim
  Local cLog := GetStartUpFolder() + "\_4Msg.log"

//  SET CODEPAGE TO RUSSIAN
//  SET LANGUAGE TO RUSSIAN
  SET LOGFILE  TO (cLog)           // отладочный log файл
  SET DATE     TO GERMAN

  fErase( cLog )

  IsIniFile( cIni )                // проверка на наличие ini-файла 

  ? Repl("-",20) + " example log file: " + cFileNoPath( App.ExeName ) + Repl("-",20) ; ?

  o:Set( hb_IniRead( cIni, .F. ) ) // ключи в upper (секция MAIN добавляется, если нет ее)

  // все секции ини-файла
  ? "File " + cIni,"=", o:Keys(), HB_ValToExp(o:Keys()) ; ?

  ? "INI =", o:GetAll() ; ?v o:GetAll() ; ?

  c:Set( o:Com )                   // секция [COM]

  ? "COM =", c:GetAll() ; ?v c:GetAll() ; ? "---- selected values -----"
  ? "  Number    = ", ValType(c:Number   ) , c:Number   
  ? "  String    = ", ValType(c:String   ) , c:String   
  ? "  Logical   = ", ValType(c:Logical  ) , c:Logical  
  ? "  Date      = ", ValType(c:Date     ) , c:Date     
  ? "  Host      = ", ValType(c:Host     ) , c:Host     
  ? "  aLangName = ", ValType(c:aLangName) , c:aLangName
  ? "  aLangList = ", ValType(c:aLangList) , c:aLangList
  aDim := hb_Atokens( c:aLangName, "," )
  ? "  aLangName = ",aDim,ValType(aDim), hb_ValToExp(aDim)
  aDim := hb_Atokens( c:aLangList, "," )
  ? "  aLangList = ",aDim,ValType(aDim), hb_ValToExp(aDim)
  ? "------------------------------"

  s := c:Language                  // язык
  IF ! s $ "RU,EN"
     s := iif( Set( _SET_CODEPAGE ) == 'RU1251', [RU], [EN] )
  ENDIF
  ? "Language =", s

  a:Set( o:Get(s) )                // секция [RU] или [EN]

  ? "Text =", a:GetAll() ; ?v a:GetAll() ; ?
  ? a:Title
  ? a:Btn_01
  ? a:Btn_02
  ? a:Btn_03
  ? a:Btn_04

  ? "---- End ----"

  ShellExecute(0,"Open",cLog,,,SW_SHOWNORMAL)  // показать отладочный файл

Return Nil

/////////////////////////////////////////////////////////////////////
Function IsIniFile(cIni)
   LOCAL cText, lUtf := Set( _SET_CODEPAGE ) == "UTF8"

   IF !File( cIni)
      cText := "[Information]" + CRLF
      cText += "Program = " + Application.ExeName + CRLF
      cText += "Free Open Source Software = " + Version() + CRLF
      cText += "Free Compiler = " + hb_compiler() + CRLF
      cText += "Free Library  = " + MiniGUIVersion() + CRLF
      cText += CRLF
      cText += "[Main]" + CRLF
      cText += "cIni  = " + cIni + CRLF
      cText += "cCode = " + Set( _SET_CODEPAGE ) + CRLF
      cText += "lUtf8 = " + cValToChar(lUtf) + CRLF
      cText += CRLF
      cText += "[COM]" + CRLF
      cText += "Number     = 13" + CRLF
      cText += "String     = Строка пример / Example string" + CRLF
      cText += "Logical    = " + cValToChar(lUtf) + CRLF
      cText += "Date       = " + DtoC(Date()) + CRLF
      cText += "Host       = 127.0.0.1" + CRLF
      cText += "aLangName  = English,Russian,Belarusian, Ukrainian, Latvian" + CRLF
      cText += "aLangList  = EN,RU,BE,UA,LV" + CRLF
      cText += "Language   = EN" + CRLF
      cText += CRLF
      cText += "[RU]" + CRLF
      cText += "Title   = Демонстрация работы с ини-файлом через контейнер oHmgData()" + CRLF
      cText += "Btn_01  = Помощь" + CRLF
      cText += "Btn_02  = Настройки" + CRLF
      cText += "Btn_03  = Проверка" + CRLF
      cText += "Btn_04  = Выход" + CRLF
      cText += CRLF
      cText += "[EN]" + CRLF
      cText += "Title = Demonstration of working with ini-file through the oHmgData() container" + CRLF
      cText += "Btn_01 = Help" + CRLF
      cText += "Btn_02 = Settings" + CRLF
      cText += "Btn_03 = Check" + CRLF
      cText += "Btn_04 = Exit" + CRLF 
      hb_MemoWrit( cIni, cText )
   ENDIF

Return Nil
