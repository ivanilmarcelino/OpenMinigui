/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/

#include "minigui.ch"

/////////////////////////////////////////////////////////////////////
// Список проверок/открытия файлов при запуске программы
// List of checks/opening of files when starting the program
Function StartupChecks()
   Local aRun := {}

   IF M->nProgLang == 2  // 1-русский, 2-english, 3-украинский язык
      AADD( aRun, { "Start of programm/Запуск программы"                 , "MyStart()"       , 200 } )
      AADD( aRun, { "Dummy procedure 1"                                  , "Dummy_1()"       , 100 } )
      AADD( aRun, { "Dummy procedure 2"                                  , "Dummy_2()"       , 100 } )
      AADD( aRun, { "Loading program paths/settings from *.ini file"     , "myLoadIni()"     , 100 } )
      AADD( aRun, { "Checking the existence of program folders and files", "myCheckDir()"    , 100 } )
      AADD( aRun, { "User password prompt"                               , "myPassword()"    , 100 } )
      AADD( aRun, { "Opening Database:"                                  , "myOpenDbf()"     , 100 } )
      AADD( aRun, { "Checking / copying files:"                          , "myCopyFiles()"   , 100 } )
      AADD( aRun, { "Run message box WM_COPYDATA"                        , "myWndCopyData()" , 150 } )
      AADD( aRun, { "Loading protected variables from a *.cnf file"      , "myLoadCnfg()"    , 100 } )
      AADD( aRun, { "Launch additional *.exe files"                      , "myStart()"       , 150 } )
      //AADD( aRun, { "Run message box TIMER"                            , "myWndTimerShow()", 150 } )  // reserve
      AADD( aRun, { "Starting the main form of the program"              , "myStart()"       , 150 } )
   ELSE
      AADD( aRun, { "Запуск программы/Запуск программы"                  , "MyStart()"       , 200 } )
      AADD( aRun, { "Фиктивная процедура 1"                              , "Dummy_1()"       , 100 } )
      AADD( aRun, { "Фиктивная процедура 2"                              , "Dummy_2()"       , 100 } )
      AADD( aRun, { "Загрузка путей/настроек программы из *.ini-файла"   , "myLoadIni()"     , 100 } )
      AADD( aRun, { "Проверка существования программных папок и файлов"  , "myCheckDir()"    , 100 } )
      AADD( aRun, { "Открытие базы данных:"                              , "myOpenDbf()"     , 100 } )
      // если пароль читаем из базы, то ставим так
      AADD( aRun, { "Запрос пароля пользователя"                         , "myPassword()"    , 100 } )
      AADD( aRun, { "Проверка/копирование файлов:"                       , "myCopyFiles()"   , 100 } )
      AADD( aRun, { "Запуск окна сообщений WM_COPYDATA"                  , "myWndCopyData()" , 150 } )
      AADD( aRun, { "Загрузка защищённых переменных из *.cnf-файла"      , "myLoadCnfg()"    , 100 } )
      AADD( aRun, { "Запуск дополнительных *.ехе файлов"                 , "myExeStart()"    , 150 } )
      //AADD (aRun, { "Запуск окна сообщений TIMER"                      , "myWndTimerShow()", 150 } )  // резерв
      AADD( aRun, { "Запуск главной формы программы"                     , "myStart()"       , 150 } )
   ENDIF

Return aRun

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myStart(xVal)
   wApi_Sleep(xVal)  // просто для тестирования
Return .t.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myExeStart(xVal)
   wApi_Sleep(xVal)  // просто для тестирования
Return .t.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function Dummy_1(xVal)
   wApi_Sleep(xVal)  // просто для тестирования
Return .t.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function Dummy_2(xVal)
   wApi_Sleep(xVal)  // просто для тестирования
Return .t.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myLoadIni(xVal)
   LOCAL lRet
   lRet := IniFileYes()    // configuration file check  -> IniLoad.prg
   IF lRet
      IniGetParam()        // configuration file check  -> IniLoad.prg
   ENDIF
   //IniWriteParam()       // Запись всего ини файла   -> IniLoad.prg
   wApi_Sleep(xVal)        // просто для тестирования
Return lRet

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myLoadCnfg(xVal)
   LOCAL lRet
   lRet := CnfgFileYes()      // configuration file check  -> IniConfig.prg
   IF lRet
      lRet := CnfgGetParam()  // configuration file check  -> IniConfig.prg
   ENDIF
   //CnfgWriteParam()         // Запись всего ини файла   -> IniConfig.prg
   wApi_Sleep(xVal)           // просто для тестирования
Return lRet

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myCheckDir(xVal)
   wApi_Sleep(xVal)  // просто для тестирования
Return .T.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function myPassword(xVal)
   LOCAL nI, cTitle, cUserName, cPassword, nRet, lRet
   LOCAL cFrm := App.Cargo:cWinMain          // или cFrm := oFrm:Name

   lRet   := cUserName := cPassword := .T.
   cTitle := cFileNoPath(App.ExeName)
   // переключиться на открытую базу паролей пользователей Users.dbf
   FOR nI := 1 TO 3  // 3-кpатная пpовека ввода паpоля

      // ==>> form_LoginPassw.prg
      nRet := 0 // myGetPassword( cTitle, @cUserName, @cPassword, 0 )
      IF nRet == 0  // пароль набран
         // проверка по базе Users.dbf - cUserName, cPassword
         lRet := .T.
         EXIT
      ELSE
         // пароль НЕ набран / отказ !
         // AlertStop("Отказ от ввода пароля !", cTitle)
         lRet := .F.
         // снимаем блокировку выхода .F. на .T. или Nil
         SetProperty(cFrm, "OnInterActiveClose", {||.T.})
         EXIT
      ENDIF

   NEXT
   xVal := 10
   wApi_Sleep(xVal)  // просто для тестирования
Return lRet

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function MyOpenDbf(xVal)
Local aFilesDbf := {}, nI, cVal

   AADD ( aFilesDbf, "Base01.dbf" )
   AADD ( aFilesDbf, "Base02.dbf" )
   AADD ( aFilesDbf, "Base03.dbf" )
   AADD ( aFilesDbf, "Base04.dbf" )

   cVal := GetProperty("Form_Splash","Label_1","Value")
   For nI := 1 TO LEN(aFilesDbf)
       SetProperty("Form_Splash","Label_1","Value",cVal + "->" + aFilesDbf[nI])
       wApi_Sleep(xVal)  // просто для тестирования
   NEXT

Return .T.

/////////////////////////////////////////////////////////////////////
// Эта функция заготовка (сделать самостоятельно)
// This function is blank (do it yourself)
Function MyCopyFiles(xVal)
Local aFiles := {}, nI, cVal, cMask := "Rep-"

   For nI := 1 TO 5
       AADD ( aFiles, cMask + StrZero(nI,6) + ".txt" )
   NEXT

   cVal := GetProperty("Form_Splash","Label_1","Value")
   For nI := 1 TO LEN(aFiles)
      SetProperty("Form_Splash","Label_1","Value",cVal + "->" + aFiles[nI])
      wApi_Sleep(xVal)  // просто для тестирования
   NEXT

Return .T.
