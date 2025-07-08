/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * The idea of 2013-2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Заготовка-пример готовой программы с таймерами
 * Blank-an example of a finished program with timers
*/

#define _HMG_OUTLOG
#define APP_TITLE  "Template of the finished program on MiniGui"
#define APP_VERS   "Version 2.2 - 2025.02.02"
#define APP_ID     555
#define LEN_SPC    50

#include "hmg.ch"
#include "i_winuser.ch"
#include "i_ExtEvents.ch"
#include "metrocolor.ch"

REQUEST HB_CODEPAGE_UA1251, HB_CODEPAGE_UA866    // украинский язык
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // русский язык
REQUEST HB_CODEPAGE_UTF8
REQUEST DBFCDX, DBFFPT
REQUEST HB_MEMIO

STATIC  lStat_WM_COPYDATA := .F.  // разрешено принимать данные при .T.
STATIC  lStat_MainClose   := .T.  // разрешено завершить программу
STATIC  lStat_AppOnEvents := .F.  // разрешено отрабатывать App_OnEvents() при .T.
STATIC  nStat_StopMinutes := 5    // (минут) закрыть программу без работы пользователя
STATIC  oStat_JobWait             // ожидание работы пользователя
STATIC  lStatBallonNotify         // отображать или нет в трее сообщения

//////////////////////////////////////////////////////////////////////////
// Главный модуль. Показ логотипа программы и загрузка основной формы.
// Список функций проверки при запуске программы.
// The main module. Logo show program and load the main form.
// The list of features check at startup.
FUNCTION Main(...)
   Local hIcon, nY, nX, nW, nH, nG, cVal, aBCWinAct, nWBtn
   Local nHBtn, nHLbl, nWLbl, aBtnCap, aBtnIco, aBtnGrd
   Local cForm, cTimer, aBackColor, aBtnPst, o
   LOCAL nMode := App.Cargo:nMode
   LOCAL aMode := App.Cargo:aMode
   LOCAL cFunc := "wMain_" + hb_ntos(nMode)

   ? ProcNL(), "=== Start function Main() ==="
   ?
   ? "===> MODE =", nMode, hb_valtoexp(aMode)
   //SET WINDOW MAIN OFF

   IF nMode > 0
      App.Cargo:cWinOther := cFunc
      ? "<===", App.Cargo:cWinOther, cFunc, hb_IsFunction( cFunc )  ; ?
      // отдельный запуск функций из программы
      // wMain_1() // обновить и перезапуск программы    nMode == 1            //  -> main_other.prg
      // wMain_2() // Индексация БД                      nMode == 2            //  -> main_other.prg
      // Button "Sample 4"  // функция wMain_4()  -> main_other.prg  nMode == 4  //  -> main_other.prg
      // No Button ""       // функция wMain_5()  -> main_other.prg  nMode == 5  //  -> main_other.prg
      IF hb_IsFunction( cFunc ) ; hb_ExecFromArray( cFunc )
      ENDIF
      RETURN NIL
   ENDIF

   //IF IsWin7() .OR. IsVistaOrLater()
   IF ! os_isWinXP()    // можно так _HMG_IsXP
      lStatBallonNotify := .T.  // разрешено отображать в трее сообщения
   ELSE
      lStatBallonNotify := .F.  // запрещено отображать в трее сообщения
   ENDIF

   Sets_Event2AppObject()  // ставим события на объект приложения
                           // назначаем все таймера

   // Проверка на запуск второй копии программы
   // Check to run a second copy of the program
   OnlyOneInstance( APP_TITLE , , APP_ID)

   // Перед запуском основной формы / Before starting the main form
   BeforeStartingMain()       // -> main_before.prg: 1-русский, 2-english, 3-украинский язык
   BeforeStartingDisplay()    // -> main_before.prg

   // Проверки/открытия при запуске / Checks/opens at startup
   (App.Cargo):aRunCheck := StartupChecks()  // -> main_check.prg

   IsDbfUsers() // создадим базу операторов -> Users.prg
   //SET WINDOW MAIN ON

   // назначить обработчик событий программы в функции App_OnEvents()
   // assign a program event handler to the App_OnEvents() function
   //SET EVENTS FUNC TO App_OnEvents
   //Set_bEvents("App_OnEvents") // добавить в обработчик
   Set_bEvents( {|hH,nM,wP,lP| App_OnEvents(hH,nM,wP,lP) } )    // блок кода, сделать STATIC
   //Set_bEvents( {|...| App_OnEvents(...) } ) // STATIC FUNCTION App_OnEvents(...)

   nY := nX := 5 ; nW := System.ClientWidth ; nH := 140 ; nG := GetTitleHeight() + 3
   cForm      := App.Cargo:cWinMain
   aBCWinAct  := HMG_ColorWinActiveCaption()
   aBackColor := {153,152,255}

   // ВНИМАНИЕ ! Это окно при старте будет всегда NOSHOW ! Чтобы окна MODAL не вызывали это окно на передний план. 
   // ATTENTION ! This window will always be NOSHOW at startup! To prevent MODAL windows from bringing this window to the foreground.
   DEFINE WINDOW &cForm AT 0,0 WIDTH nW HEIGHT nH + nG TITLE APP_TITLE    ;
      MAIN NOSHOW TOPMOST NOSIZE BACKCOLOR aBackColor                     ;
      FONT "Comic Sans MS" SIZE App.Cargo:nDefFontSize + 2                ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name  }             ;
      ON INIT     {|| This.Topmost := .F., _wPost("myInit_Window") }      ; // executed after window initialization
      ON RELEASE  {|| AEval({90,91,92,93}, {|n| _wSend(n), DoEvents()}) } ; // executed before destroying the window
      NOTIFYICON "i_Smile32" NOTIFYTOOLTIP APP_TITLE                      ;
      ON NOTIFYCLICK _wPost(50)                                           ; // контекстное меню в трее панели задач
      ON INTERACTIVECLOSE {|| // close window by [x] - этот код будет пред последним перед завершением
                             Local lRet := myMainClose()
                             Local oFrm := App.Cargo:oWinMenu          // окно MAIN, имя окна - "wMainInit"
                             Local cFrm := App.Cargo:cWinMain          // или cFrm := oFrm:Name
                             Local oMMP := App.Cargo:oMainMenuProg     // объект окна главного меню программы
                             Local cMMP := App.Cargo:cMainMenuProg     // имя окна главного меню программы
                             ? ProcNL(), ThisWindow.Name, "lRet=", lRet, cFrm, oFrm, oFrm, cMMP, oMMP
                             IF lRet
                                Timers_Off("выход")           // отключить все таймера
                                // снимаем блокировку выхода .F. на .T. или Nil
                                SetProperty(cFrm, "OnInterActiveClose", {||.T.})
                                SetProperty(cMMP, "OnInterActiveClose", {||.T.})
                                oMMP:Release()
                                wApi_Sleep(100)
                                // тут пред последние действия в окне
                                ? ">>> STOP INTERACTIVECLOSE "+cFrm+" <<< " + HMG_TimeMS(M->tPubStart), ProcNL()
                             ELSE
                                // тут можно фокус восстановить\переключить на какое то окно
                             ENDIF
                             Return lRet
                           }
      // другой вариант  {|lRet| lRet := myMainClose(), iif(lRet, _wSend(98), Nil), lRet }   // close window by [x]
      // ON RELEASE  {|| AEval({90,91,92,93}, ....
      //                        ^^^^^^^^^^^ тут делаем что нужно, этот код будет последним перед завершением окна
      //                                     here we do what we need, this code will be the last one before completion
      //
      myMainMenuNotify("NOTIFY", This.Name)  // создать меню в трее

      This.Cargo := oHmgData()        // контейнер для ЭТОГО окна
      // в контейнер запоминаем основной процесс окна
      App.Cargo:oWinMain := This.Object  // объект окна это как в событии {|ow|...}
      App.Cargo:hWinMain := This.Handle  // handle окна, просто для примера

      nW  := This.ClientWidth     // width inside window
      nH  := This.ClientHeight      // height inside the window
      nWBtn := 150                    // button width
      nHBtn := nH - nY*2 - nG       // button height
      nHLbl := (nH - nG) / 3         // 3 timer
      nWLbl := 400

      hIcon := LoadIconByName( "1MG", nH, nH )
      This.Cargo:hIcon := hIcon

      DRAW ICON IN WINDOW &cForm AT nY, nX HICON hIcon WIDTH nHBtn HEIGHT nHBtn COLOR aBackColor
      nX += 5*2 + nHBtn

      cVal   := App.Cargo:cDisplayMode
      aBtnCap := { cVal, "Start", "Exit" }
      aBtnPst := { 11, 12, 99 }                 // _wPost(This.Cargo, , This.Name) - button
      aBtnIco := { {"iFind64x1", "iFind64x2"}, {"iOkey64x1", "iOkey64x2"} , {"iExit64x1", "iExit64x2"} }
      aBtnGrd := { CLR_FB , COLOR_GREEN_METRO, COLOR_RED_METRO }
      // меню кнопок этого окна
      nX    := myMainMenuButton( nY, nX, 10, nHBtn, nWBtn, aBtnCap, aBtnIco, aBtnGrd, aBtnPst )

      cVal := "Панель программы - " + APP_TITLE + "  (" + APP_VERS + ")" + SPACE(10) +  MiniGuiVersion()
      @ nH-nG, 0 LABEL Label_Bottom VALUE cVal WIDTH nW HEIGHT nG ;
        SIZE App.Cargo:nDefFontSize - 2 FONTCOLOR YELLOW BACKCOLOR aBCWinAct CENTERALIGN VCENTERALIGN

       cVal := "Timer1()"
       @   0, nX LABEL Label_1 VALUE cVal WIDTH nWLbl HEIGHT nHLbl TRANSPARENT VCENTERALIGN

       cVal := "Timer2()"
       @ nHLbl, nX LABEL Label_2 VALUE cVal WIDTH nWLbl HEIGHT nHLbl TRANSPARENT VCENTERALIGN

       cVal := "Timer3()"
       @ nHLbl*2, nX LABEL Label_3 VALUE cVal WIDTH nWLbl HEIGHT nHLbl TRANSPARENT VCENTERALIGN

      ON KEY F1     ACTION NIL
      //ON KEY ESCAPE ACTION {|| _wSend(99) } - нельзя

      DEFINE TIMER Timer_1 INTERVAL App.Cargo:nTimer1 * 1000  ACTION {|| NIL }
              This.Timer_1.Enabled := .F.  // отключить таймер до ON INIT

      DEFINE TIMER Timer_2 INTERVAL App.Cargo:nTimer2 * 1000 ACTION {|| NIL }
              This.Timer_2.Enabled := .F.  // отключить таймер до ON INIT

      DEFINE TIMER Timer_3 INTERVAL App.Cargo:nTimer3 * 1000 ACTION {|| NIL }
              This.Timer_3.Enabled := .F.  // отключить таймер до ON INIT

      o := oStat_JobWait ; cTimer := o:cJobName

      DEFINE TIMER &(cTimer) INTERVAL o:nJobSleep * 1000 ACTION {|| NIL }
              This.&(cTimer).Enabled := .F.  // отключить таймер до ON INIT

      // Установка событий на главное окно программы
      Sets_Event2MainWindow()

      ? SPACE(5) + "MAIN window name:", App.FormName
      ? SPACE(5) + " Window handle:", App.Handle
      ? SPACE(5) + " _HMG_MainHandle:", _HMG_MainHandle
      ? SPACE(5) + "   This.Handle:", This.Handle

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN Nil

//////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Send_WM_CopyData( hWnd )
   LOCAL cVal := cFileNoPath( GetExeFileName() ) + " | "
   STATIC s_nCountSend := 0
   DEFAULT hWnd := _HMG_MainHandle

   s_nCountSend += 1
   cVal += HB_TSTOSTR( HB_DATETIME() )
   cVal += " - Message sent: " + HB_NtoS( s_nCountSend )

   // Transfer data to window -> APP_TITLE
   SendMessageData( hWnd, cVal, APP_ID )

RETURN Nil

////////////////////////////////////////////////////////////////////////
// Инициализация происходит в первую очередь, потом работает MAIN(...)
// Initialization happens first, then MAIN(...)
INIT PROCEDURE MyInitWin()
   LOCAL tTime := HB_DATETIME()
   LOCAL cFont := "DejaVu Sans Mono", nSize := 12
   LOCAL cLog  := GetStartupFolder() + "\_msg.log"
   LOCAL cIconDef := "1MG"
   LOCAL aBClrDlg := { 238, 249, 142 }    // светло-желтый цвет
   LOCAL cUser, cFLog, cMsg, cErrLog, cDir, nErr, o
   LOCAL aParam := hb_aParams(), nMode

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

   RDDSETDEFAULT('DBFCDX')

   SET EXCLUSIVE ON   // по умолчанию USE ... EXCLUSIVE
   SET DELETED  OFF   // ! для примера надо OFF
   // SET AUTOPEN  ON  // ! в многопользовательском варианте надо ставить auto подкл. индекса !
   SET DATE  TO GERMAN // это и есть SET DATE FORMAT "DD.MM.YYYY"
   SET EPOCH TO 2000
   SET EXACT    ON
   SET SOFTSEEK ON
   SET CENTURY  ON
   // ООП используем
   SET OOP ON
   // new log filename for debug output
   _SetGetLogFile( cLog )
   IF Empty( FindWindowEx( ,,, APP_TITLE ) )
      DELETEFILE(cLog)
   ELSE
      ? ; ? repl("*", 30) + " START 2 " + repl("*", 30) ; ?
   ENDIF

   IF !IsFontExist( cFont )
      cFont := "Courier New"
   ENDIF
   ? "The main font of the program - " + cFont, nSize

   // основной Default фонт программы
   SET FONT TO cFont, nSize
   // фонт для HMG_Alert() и Alert...() ф-ий
   DEFINE FONT DlgFont FONTNAME cFont SIZE nSize + 2
   // фонты для _TBrowse()
   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold   FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic FONTNAME cFont SIZE nSize ITALIC
   // other my font
   DEFINE FONT ComSnMs FONTNAME "Comic Sans MS" SIZE nSize + 2
   DEFINE FONT FSmart  FONTNAME "Snap ITC" SIZE nSize 
   DEFINE FONT MnMain2 FONTNAME "Courier New" SIZE nSize BOLD
   // other setting
   SET DEFAULT ICON TO cIconDef
   SET NAVIGATION EXTENDED
   SET TOOLTIP BALLOON ON
   SET WINDOW MODAL PARENT HANDLE ON  // окна Modal получают родителя - активное окно MiniGui с версии 23.04.4
  
   // для HMG_Alert() и Alert...() ф-ий
   SET MSGALERT BACKCOLOR TO aBClrDlg
   SET MSGALERT FONTCOLOR TO BLACK
   HMG_Alert_MaxLines( , 120) 

   IF Empty(aParam) ; aParam :=  {"0"}
   ENDIF
   nMode := Val( aParam[1] )

   // создадим контейнер для всего приложения
   WITH OBJECT ( App.Cargo := oHmgData() )
      // запомнили параметры входа
      :nMode        := nMode
      :aMode        := aParam
      //
      :cFileLog     := cLog
      :cAppTitle    := APP_TITLE
      :cProgVersion := APP_VERS
      :cProgTtlRu  := "Заготовка-пример готовой программы с таймерами"
      :cProgTtlEn  := "Blank-an example of a finished program with timers"
      :cCopyright  := "Copyright (c) 2023, Verchenko A.G., Russia, Dmitrov."
      :cEmail      := "E-mail: verchenkoag@gmail.com"
      :cPrgInfo1   := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
      :cPrgInfo2   := "Tips and tricks programmers from our forum http://clipper.borda.ru"
      :cPrgInfo3   := "SergKis, Igor Nazarov, and other..."
      // запомним базовые фонты
      :cDefFontName := cFont
      :nDefFontSize := nSize
      :cDlgFontName := cFont
      :nDlgFontSize := nSize + 2
      :cBtnFontName := "Comic Sans MS"
      :nBtnFontSize := nSize + 4
      :hFontCSM     := GetFontHandle( "ComSnMs" )
      :hFontDlg     := GetFontHandle( "DlgFont" )
      :hFontNormal  := GetFontHandle( "Normal"  )
      :hFontBold    := GetFontHandle( "Bold"    )
      :hFontItlc    := GetFontHandle( "Italic"  )
      :hFontMnMain  := GetFontHandle( "MnMain2" )
      :hFontFSmart  := GetFontHandle( "FSmart" )
      // разрешение программы и проверка экранных форм
      :aDisplayMode := { System.DesktopWidth , System.DesktopHeight }
      :cDisplayMode := HB_NtoS(System.DesktopWidth) + "x" + HB_NtoS(System.DesktopHeight)
      // WM_COPYDATA для обработки
      :WM_CD_oWnd   := NIL   // окно с обработкой
      :WM_CD_cData  := ""    // данные для обработки
      :WM_CD_lData  := .F.   // нет данных
      :WM_CD_lShow  := .T.   // Показ окна WM_COPYDATA
      // для обработки таймеров
      :nTimer1    := 15      // 15 сек.
      :nTimer2    := 5       // 5  сек.
      :nTimer3    := 30      // 30 сек.
      :nMeter1    := 0       // счётчик таймера 1
      :nMeter2    := 0       // счётчик таймера 2
      :nMeter3    := 0       // счётчик таймера 3
      :nMeter9    := 0       // счётчик таймера 9
      :aTimer     := {}
      :nTimeStart := SECONDS()  // включить время для показа таймера
      :tStart     := tTime
      :nSecTick   := 1
      :TimerSec   := 10      // пример
      // other
      :cFormGotFocus := ""  // сохранение текущего фокуса формы
      :cLogEvents  := ""  // журнал событий
      :cErrLogHtm  := GetStartupFolder() + "\ErrorsLog\ErrorLog.htm" // журнал ошибок
      :cPathDbf    := GetStartupFolder() + "\DBASE\"
      :cExeAlert   := GetStartupFolder() + "\SmallAlert.exe"
      :cExeSite    := GetStartupFolder() + "\SmallAlert.exe"
      // main window
      :cWinMain := "wMainInit"      // имя окна MAIN формы
      :oWinMain := NIL              // объект окна это как в событии {|ow|...}
      :hWinMain := NIL              // handle окна, просто для примера
      // name of the program main menu window
      :cMainMenuProg := ""          // имя окна главного меню программы
      :oMainMenuProg := ""          // объект окна главного меню программы
      :nLenSpc := LEN_SPC
      // Message log WM_COPYDATA
      :cCopyData_Wnd  := "Form_ListCD"
      :cCopyData_Memo := "Edit_Memo"
      :cCopyDataLog   := GetStartupFolder() + "\_copydata.log"
      :lCopyDataLog   := .F.           // удалить файл _copydata.log
      :cTimer_Wnd     := "Form_Timer"
      :cTimer_Label   := "Lbl_9"
      :cFormGotFocus  := ""               // текущий фокус на форме / current focus on form
   END WITH
   //
   ? REPL("=",20) + " Program start - " + HB_TTOC( tTime ) + " " + REPL("=",20)
   //? MiniGuiVersion()  ; ? Version() ; ? hb_Ccompiler() ; ? ProcNL()
   AboutComputer()
   ? ProcNL()
   // в качестве примера / as an example
   PUBLIC cPubMainFolder, aPubListFrom, aPubListTo, tPubStart
   M->tPubStart   := tTime
   // Узнаём текущий путь программы / Know the current path of the program
   M->cPubMainFolder := GetStartUpFolder() + "\"

   // для ErrorSys3.prg и _LogFile() - util_misc.prg
   PUBLIC cPubGlobLang, cPubLangCdp
   M->cPubGlobLang := Upper( Left( Set ( _SET_LANGUAGE ), 2 ) )
   M->cPubLangCdp  := hb_CdpSelect()

   // Записываем события программы в журнал событий
   // Write program events to the event log
   cUser := "-" + NetName() + "-" + hb_UserName()
   cFLog := cFilePath( App.Cargo:cErrLogHtm )
   cFLog += "\_events" + cUser + ".log"
   App.Cargo:cLogEvents := cFLog  // журнал событий программы

   ? "   Журнал событий программы/Application event log"
   ? "   " + App.Cargo:cLogEvents
   ? "."

   cMsg := REPL(".",10) + "||" + CRLF
   cMsg += HB_TSTOSTR( App.Cargo:tStart ) + " | "
   cMsg += PADR("# Start the program " + cFileNoPath(App.ExeName),LEN_SPC)
   cMsg += " | " + ProcNL() + CRLF
   STRFILE( cMsg, App.Cargo:cLogEvents, .T. )

   /////////////////////////////////////////////////////////////////////
   //IF Empty(nMode)                  // основной запуск для таймеров
      oStat_JobWait := oHmgData()

      o := oStat_JobWait

      o:nMouseRow  := 0                // движение мыши
      o:nMouseCol  := 0                // отслеживаем
      o:cJobName   := "Timer_9"        // имя таймера слежения
      //STATIC  nStat_StopMinutes := 2 // (минут) закрыть программу без работы пользователя
      o:nJobMinMax := nStat_StopMinutes  // Счетчик (минут) Max вызовов таймера для выхода => Quit
      o:nJobSleep  := 60                 // Секунды (60-для минут) ожидания для таймера
      o:nJobTimer  := 0
      o:lJobClose  := .T.       // разрешен выход по таймеру, при запусках excel и
                                // др. программ надо ставить
                                // o:lJobClose := .F.
                                // по возврату в прогу делать o:lJobClose := .T.
   //ENDIF
   App.Cargo:oJobWait := oStat_JobWait // сделаем общий доступ

   SET SHOWREDALERT ON  //[ON | OFF] - переключатель окна ошибки HMG_Alert() или MsgStop()
   // SET ERRORLOG TO <cFile> - set new errorlog file (default {APPDIR>}\ErrorLog.htm)
   // SET ERRORLOG TO         - reset errorlog file to default value
   //cErrLog := GetStartupFolder() + "\ErrorsLog\ErrorLog.htm"
   cErrLog := App.Cargo:cErrLogHtm  // -> main.prg
   SET ERRORLOG TO cErrLog
   ? SPACE(3) + "SET ERRORLOG = " + _GetErrorlogFile()

   // создадим папку для контроля работы юзера, сюда будут записываться
   // ошибки и экраны компа при ошибке, можно делать запись и на сетевую папку
   // create a folder to control the user's work, they will be written here
   // errors and computer screens in case of an error, you can also record to a network folder
   cDir := cFilePath(cErrLog)
   IF !hb_DirExists( cDir )
      nErr := MakeDir( cDir )
      IF nErr == 0
         cMsg := Application.ExeName + ";;"
         cMsg += "Create a folder for the SCREENS of the user's work !;;"
         cMsg += cDir + ";;" + ProcNL()
         AlertInfo( cMsg, "Folder creation" )
      ELSEIF nErr == 5
         cMsg := Application.ExeName + ";"
         cMsg += "Directory " + cDir + " exists already"
      ELSE
         cMsg := Application.ExeName + ";;"
         cMsg += "Error while creating a folder !;"
         cMsg += "ERROR DOS(" + HB_NtoS(DosError()) + ");;"
         cMsg += cDir + ";;" + ProcNL()
         AlertStop( cMsg, "Folder creation error")
      ENDIF
   ENDIF
   App.Cargo:cPathPng := cDir + "\"

   PUBLIC cPubVersProg, cOperator
   M->cPubVersProg := "[Version " + App.Cargo:cProgVersion + "]"
   M->cOperator    := "Start-User"
   ? SPACE(3) + M->cPubVersProg, M->cOperator

   PUBLIC nProgLang      // номер языка в программе
   M->nProgLang     := 2  // 1-русский, 2-english, 3-украинский язык
   App.Cargo:nLang  := M->nProgLang
   ? SPACE(3) + "номер языка в программе/program language number:", M->nProgLang
   // PUBLIC переменными лучше не пользоваться, альтернатива - App.Cargo:XXXXX

RETURN

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myMainMenuNotify(cName, cForm, lDel)

   IF HB_ISOBJECT(cForm) ; cForm := cForm:Name
   ENDIF

   Default cForm := ThisWindow.Name

   ? _o2log({cForm, cName, cForm, lDel}, , "|==> "+ProcNL()+": ") // проверка

   IF !Empty(lDel)
      DEFINE CONTEXT MENU OF &cForm
      END MENU
      RETURN Nil
   ELSEIF cName == "NOTIFY" ; DEFINE NOTIFY  MENU OF &cForm
   ELSE                    ; DEFINE CONTEXT MENU OF &cForm
   ENDIF
      //DEFINE NOTIFY MENU
         ITEM "About..."         ACTION _wPost(13) ICON "i_Smile32"
         Item "List of all program forms" ACTION {|| PromptGetForms() } ICON "i_About32"
         ITEM "Get Forms All"  ACTION _wPost(14)   ICON "i_About32"
         SEPARATOR
         ITEM "Show MAIN window" ACTION _wPost(15) ICON "i_Menu32x1"
         ITEM "Hide MAIN window" ACTION _wPost(16) ICON "i_Menu32x2"
         SEPARATOR
         Item 'Show log WM_COPYDATA' ACTION {|| DoMethod( App.Cargo:cCopyData_Wnd, "Show" ) }  ICON "iFind64x1"
         Item 'Hide log WM_COPYDATA' ACTION {|| DoMethod( App.Cargo:cCopyData_Wnd, "Hide" ) }  ICON "iFind64x1"
         SEPARATOR
         Item 'Send message WM_COPYDATA' ACTION {|| Send_WM_CopyData() }  ICON "iFind64x1"
         SEPARATOR
         ITEM "Exit Application" ACTION _wPost(99, App.Cargo:oWinMain) ICON "i_Exit32"
      //END MENU
   END MENU

RETURN Nil

///////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myMainMenuButton( y, x, nGaps, h, w, aBtnCap, aBtnIco, aBtnGrd, aBtnPst)
   LOCAL a2FntClr, lVertFnt, lBoldFnt, a4BtnFnt, nWidth, lBlock, lHide
   LOCAL nI, cCapt, cObj, aGrad, nPost, aBtnObj

   aBtnObj  := {}
   a2FntClr := {  BLACK, YELLOW }
   lVertFnt := .T.  // вертикально
   lBoldFnt := .F.
   a4BtnFnt := { App.Cargo:cBtnFontName, App.Cargo:nBtnFontSize, lBoldFnt , lVertFnt }
   lBlock   := .T.  // блокировать кнопку
   lHide  := .F.  // не скрывать кнопку

   FOR nI := 1 TO Len(aBtnCap)
      cCapt := ATREPL( ";", aBtnCap[nI], CRLF )
      cObj  := "oBtn_" + HB_NtoS(nI)
      nPost := aBtnPst[nI]                // цифры - событие на кнопке
      aGrad := { aBtnGrd[nI], WHITE }
      my2BUTTON(y, x, w, h, cObj, cCapt, aGrad, , aBtnIco[nI], a2FntClr, a4BtnFnt, ;
                             nPost, lBlock, lHide )
      x += This.&( cObj ).Width + nGaps
      AADD( aBtnObj, cObj )
   NEXT

   (This.Cargo):aBtnObj := aBtnObj  // положить массив объектов кнопок в cargo окна
   nWidth  := x // конец последней кнопки

RETURN nWidth

////////////////////////////////////////////////////////////
STATIC FUNCTION myMainClose()
   LOCAL cEnd, cMsg, lExit, cTtl

   ? ProcNL() , "lStat_MainClose=", lStat_MainClose

   IF ! lStat_MainClose  // проверка на завершение процедур в таймерах
                         // check for completion of procedures in timer
      cMsg := 'Таймера в программе заняты работой !;'
      cMsg += 'Попробуйте сделать выход через некоторое время;;'
      cMsg := 'The timers in the program are busy!;'
      cMsg += 'Try to exit after a while;'
      AlertStop(cMsg)
      RETURN .F.
   ENDIF

   cEnd := "[" + HMG_TimeMS(App.Cargo:tStart) + "]"
   cMsg := 'You have worked in the program '
   cMsg +=  cEnd + ';;'
   cMsg += 'Do you really want to exit ? ;;'
   cTtl := "Exit programm - windows: " + App.Cargo:cWinMain

   lExit := AlertYesNo( cMsg, cTtl, .T., "iAbout64x1", 64, {COLOR_LIGHT_GREEN_LIME,ORANGE}, .T.,  )
   ? ProcNL() + " **** AlertYesNo("+cTtl+") ->", lExit
   DO EVENTS

RETURN lExit

////////////////////////////////////////////////////////////
// Установка событий на главное окно программы
STATIC FUNCTION Sets_Event2MainWindow()

      WITH OBJECT This.Object
      // events begin
      :Event({ 0, "myInit_Window"}, {|ow|  // запуск при инициализации окна
                   Local hWnd, cWnd
                   hWnd := ow:Handle
                   cWnd := ow:Name
                   // Показ логотипа программы / Program logo display
                   _DefineSplashWindow( "Form_Splash",,,,, "DEMO", cWnd )
                   ? SPACE(5) + "_HMG_MainHandle:" , hWnd, IsIconic( hWnd ), _HMG_MainHandle
                   DO EVENTS
                   _wPost("Main-Menu", ow)  // запуск события 1
                   Return Nil
                   })

      // показываем главную форму-меню задачи / show the main-menu form
      :Event({ 1, "Main-Menu"}, {|ow| // главное событие запуска всех отложенных событий
                        Local cT_1, cT_2, cT_3, cT_9
                        Local o  := oStat_JobWait
                        Local cn := o:cJobName
                        lStat_WM_COPYDATA  := .T.
                        lStat_AppOnEvents  := .T. // разрешаем принимать и отрабатывать
                        This.Timer_1.Enabled := .T. // включить таймер
                        This.Timer_2.Enabled := .T. // включить таймер
                        This.Timer_3.Enabled := .T. // включить таймер
                        This.&(cn).Enabled   := .T. // включить таймер 9
                        o:lJobClose := .T.          // вкл. Quit выход таймер 9
                        o:nJobTimer := Seconds()
                        cT_1 := iif( This.Timer_1.Enabled, "ON", "OFF" )
                        cT_2 := iif( This.Timer_2.Enabled, "ON", "OFF" )
                        cT_3 := iif( This.Timer_3.Enabled, "ON", "OFF" )
                        cT_9 := iif( This.&(cn).Enabled  , "ON", "OFF" )
                        ? "@@@ Timer_1("+cT_1+")", HB_DATETIME(), ProcNL()
                        ? "@@@ Timer_2("+cT_2+")", HB_DATETIME(), ProcNL()
                        ? "@@@ Timer_3("+cT_3+")", HB_DATETIME(), ProcNL()
                        ? "@@@ Timer_9("+cT_9+")", HB_DATETIME(), ProcNL()
                        //Timers_Off("для отладки")    // отключить все таймера
                        Form_Menu_Main(ow)             // -> form_menu_main.prg
                        ow:Setfocus('Label_Bottom')
                        Return Nil
                        })
      :Event( 11, {|  | // кнопка Разрешение экрана
                        // здесь НЕ This среда кнопки  - This.Enabled := .F. нельзя
                        Local cVal, ow := ThisWindow.Object
                        Local cn := ow:Cargo:aBtnObj[1]
                        Local cw := ow:Name
                        //SetProperty(cw,cn, "Enabled", .F.)  // блокировка кнопки - сделано ранее
                        SET WINDOW THIS TO ow
                        cVal := ChangeClientModeDisplay(cw)
                        SET WINDOW THIS TO
                        This.&(cn).Enabled := .T.     // снять блокировку кнопки
                        This.&(cn).Caption := cVal
                        ow:SetFocus('Label_Bottom')
                        Return Nil
                        } )
      :Event( 12, {|  | // кнопка Start
                        // здесь НЕ This среда кнопки - This.Enabled := .F. нельзя
                        Local ow := ThisWindow.Object
                        Local cn := ow:Cargo:aBtnObj[2]
                        Local cw := ow:Name
                        //lStat_WM_COPYDATA := .F.
                        //lStat_AppOnEvents := .F.
                        //lStat_MainClose   := .F.
                        SET WINDOW THIS TO ow
                        // Здесь нужно закрыть все открытые окна
                        // и заново запустить Form_Menu_Main(ow)
                        RestartMainMenu()                 // -> main_misc.prg
                        SET WINDOW THIS TO
                        DbCloseAll()    // закрыть все базы / close all bases
                        SetProperty(cw,cn, "Enabled", .T.)  // снять блокировку кнопки
                        ow:SetFocus('Label_Bottom')
                        _wPost(1, ow)  // запуск события 1
                        Return Nil
                        } )
      :Event( 13, {|ow| // "About..."
                        MsgAbout(ow:Name)
                        Return Nil
                        } )
      :Event( 14, {|ow| // "Get Forms All"
                        AlertInfo( oGetForms(.T.), ow:Name )
                        Return Nil
                        } )
                                  // прорисовка иконки сразу на форме
      :Event( 15, {|ow| ow:Show(), SendMessage( ow:Handle, WM_PAINT, 0, 0 )  } )   // "Show MAIN window"
      :Event( 16, {|ow| ow:Hide()  } )   // "Hide MAIN window"

      :Event( 50, {|ow| // меню в трее
                        Local x := Sys.DesktopWidth  - 250 //ow:ClientWidth  - 2
                        Local y := Sys.DesktopHeight -  50 //ow:ClientHeight - 2
                        Local m
                        myMainMenuNotify("CONTEXT", ow)
                        m := _HMG_aFormNotifyMenuHandle[ ow:Index ]
                        TrackPopupMenu( m, x, y, ow:Handle, .T. )
                        myMainMenuNotify("CONTEXT", ow, .T.)        // delete context menu
                        Return Nil
                        })

      // ON RELEASE .... AEval({90,91,92,93} // executed before destroying the window
      :Event( 90, {|ow| DestroyIcon(ow:Cargo:hIcon) /*, IniSaveFileThis()*/                } )
      :Event( 91, {|ow| _LogFile(.T., ">>> ON RELEASE WINDOW: "+ow:Name+" - close .EXE" )  } )
      :Event( 92, {|  | // список открытых окон
                        Local nI, hWnd, cForm, aWnd := HMG_GetForms()
                        ? SPACE(4) + "HMG_GetForms()=", aWnd
                        For nI := 1 TO LEN(aWnd)
                           cForm := aWnd[nI]
                           hWnd  := GetFormHandle(cForm)
                           ? SPACE(4) + HB_NtoS(nI) + ")"
                           ?? cForm, "=>"
                           ?? GetProperty( cForm, "Title" ), ","
                           // Как получить по хендлу hWnd - имя окна ?
                           ?? GetFormNameByIndex( GetFormIndexByHandle( hWnd ) )
                           ?? hWnd
                        Next
                        Return Nil
                        })
      :Event( 93, {|ow| // событие 93
                        _LogFile(.T., ">>> STOP - "+ow:Name+" <<<  " + HMG_TimeMS(M->tPubStart) )
                         // удалить журналы
                        If App.Cargo:lCopyDataLog
                           ? "DELETEFILE()=",App.Cargo:cCopyDataLog
                           wApi_Sleep(100)
                           DELETEFILE(App.Cargo:cCopyDataLog)
                        EndIf
                        If App.Cargo:lFileLog
                           ? "DELETEFILE()=",App.Cargo:cFileLog
                           wApi_Sleep(100)
                           DELETEFILE(App.Cargo:cFileLog)
                        EndIf
                        Return Nil
                        } )

      :Event( 98, {|  | // закрытие окна, -> MainMenu_form.prg - пример, пока не используется
                        Local cwn := App.Cargo:cWinMain
                        ? ProcNL(), ":Event(98)=", cwn
                        Timers_Off()   // отключить все таймера -> main.prg
                        IF _IsWindowDefined(cwn)
                           SetProperty(cwn, "OnInterActiveClose", {|| NIL }) //или nil
                           DoMethod(cwn, "Release")
                           DO EVENTS
                        ENDIF
                        Return Nil
                        } )
      // закрытие окна без параметров
      :Event({99, "ExitWindow"}, {|ow| ow:Release() } )

      // событие для печати в лог-файл в качестве примера / event to print to a log file as an example
      :Event(100, {|ow,ky,cv|  // выполняем из _SplashDelay(...)
                    IF "START" $ cv ; ?
                    ENDIF
                    ? "==>>>", ky, "_SplashDelay(...): "+ow:Name + " :", cv
                    IF "STOP" $ cv
                       ? SPACE(5) + ProcNL(), "This.Names=", This.Names
                       ? SPACE(5) + HB_ValToExp( This.Names )
                    ENDIF
                    Return Nil
                    })
      // events end
      END WITH

RETURN Nil

////////////////////////////////////////////////////////////
// отключить все таймера
FUNCTION Timers_Off(cVal)
   LOCAL nI, cName, cForm := App.Cargo:cWinMain
   LOCAL aTimer := {"Timer_1","Timer_2","Timer_3","Timer_9"}
   DEFAULT cVal := ""

   FOR nI := 1 TO LEN(aTimer)
      cName := aTimer[nI]
      SetProperty(cForm, cName, "Enabled", .F.)
      ? SPACE(5)+"@@@ ("+cName+") = OFF", ProcNL(), cVal
   NEXT
   DO EVENTS

RETURN Nil

////////////////////////////////////////////////////////////
// Установка событий на объект приложения
STATIC FUNCTION Sets_Event2AppObject()

   WITH OBJECT App.Object                   // объект программы
   // set events App\Program begin
   :Event({ 1, "Timer_1" }, {|oa,ky,nn,cnam| // таймер 1
                Local cForm := App.Cargo:cWinMain
                Local cFunc := "myTimer" + hb_ntos(nn)
                Local oThis := _oThis()
                Local cky := oa:oEvents:Get(ky, "?") // имя события от кода его
                Local nky := oa:oEvents:Get(cky, 0)  // код события от его имени
                lStat_MainClose := .F.
                SetProperty(cForm, cnam, "Enabled", .F.) // отключить таймер
                oThis:cName := cnam
                oThis:cKey  := cky
                oThis:nKey  := nky
                oThis:xResult := NIL
                //? "App.Object 1:", oa, oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled"), nky, cky
                hb_ExecFromArray( cFunc, {oThis} ) // myTimer1()  // -> main_timer.prg - пишем всё туда
                //? "App.Object 2:", oa, oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled")
                //? "App.Object 3:", oThis:xResult, hb_valtoexp(oThis:xResult) ;  ?
                SetProperty(cForm, cnam, "Enabled", .T.) // включить заново таймер
                lStat_MainClose := .T.
                Return Nil
                })
   :Event({ 2, "Timer_2" }, {|oa,ky,nn,cnam| // таймер 2
                Local cForm := App.Cargo:cWinMain
                Local cFunc := "myTimer" + hb_ntos(nn)
                Local oThis := _oThis()
                Local cky := oa:oEvents:Get(ky, "?") // имя события от кода его
                Local nky := oa:oEvents:Get(cky, 0)  // код события от его имени
                lStat_MainClose := .F.
                SetProperty(cForm, cnam, "Enabled", .F.) // отключить таймер
                oThis:cName := cnam
                oThis:cKey  := cky
                oThis:nKey  := nky
                oThis:xResult := NIL
                //? "App.Object 1:", oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled"), nky, cky
                hb_ExecFromArray( cFunc, {oThis} ) // myTimer2()  // -> main_timer.prg - пишем всё туда
                //? "App.Object 2:", oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled")
                //? "App.Object 3:", oThis:xResult, hb_valtoexp(oThis:xResult) ;  ?
                SetProperty(cForm, cnam, "Enabled", .T.) // включить заново таймер
                lStat_MainClose := .T.
                Return Nil
                })
   :Event({ 3, "Timer_3" }, {|oa,ky,nn,cnam| // таймер 3
                Local cForm := App.Cargo:cWinMain
                Local cFunc := "myTimer" + hb_ntos(nn)
                Local oThis := _oThis()
                Local cky := oa:oEvents:Get(ky, "?") // имя события от кода его
                Local nky := oa:oEvents:Get(cky, 0)  // код события от его имени
                lStat_MainClose := .F.
                SetProperty(cForm, cnam, "Enabled", .F.) // отключить таймер
                oThis:cName := cnam
                oThis:cKey  := cky
                oThis:nKey  := nky
                oThis:xResult := NIL
                //? "App.Object 1:", oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled"), nky, cky
                hb_ExecFromArray( cFunc, {oThis} ) // myTimer3()  // -> main_timer.prg - пишем всё туда
                //? "App.Object 2:", oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled")
                //? "App.Object 3:", oThis:xResult, hb_valtoexp(oThis:xResult) ; ?
                SetProperty(cForm, cnam, "Enabled", .T.) // включить заново таймер
                lStat_MainClose := .T.
                Return Nil
                })

   :Event({ 9, "Timer_9" }, {|oa,ky,nn,cnam| // таймер 9 -> oStat_JobWait
                Local oac := App.Cargo
                Local cForm := oac:cWinMain
                Local lQuit := .F., nSec, nMin, cVal
                Local o := oStat_JobWait
                Local oThis := _oThis()
                Local cky := oa:oEvents:Get(ky, "?") // имя события от кода его
                Local nky := oa:oEvents:Get(cky, 0)  // код события от его имени

                lStat_MainClose := .F.
                SetProperty(cForm, cnam, "Enabled", .F.)  // отключить таймер
                ? "@@@ Timer_9", HB_DATETIME(), ProcNL(), nn
                nSec := Round(Seconds() - o:nJobTimer, 0) // секунды стоим
                oThis:cName := cnam
                oThis:cKey  := cky
                oThis:nKey  := nky
                //? "App.Object 1:", oa:ClassName, ky, nn, cnam, cForm, ;
                //  "Enabled", GetProperty(cForm, cnam, "Enabled"), ;
                //  Seconds(), o:nJobTimer, nSec, nky, cky
                nMin := Round(nSec / 60, 0)             // минуты стоим
                lQuit := nMin >= o:nJobMinMax            // время простоя >= заданному
                cVal := hb_valtoexp({nSec, nMin, o:nJobMinMax, lQuit, o:nJobTimer})
                //? "App.Object 2:", oa:ClassName, ky, "nn=",nn, cnam, cForm, "Enabled", ;
                //   GetProperty(cForm, cnam, "Enabled"), cVal, cFunc ; ?
                //
                SetProperty(cForm, cnam, "Enabled", .T.) // включить заново таймер
                lStat_MainClose := .T.
                ky := oa
                IF lQuit
                   o:nJobTimer := Seconds()
                   IF o:lJobClose                                   // разрешен выход, выходим
                      FOR EACH cVal IN HMG_GetFormControls ( cForm, "TIMER" )
                          SetProperty(cForm, cVal, "Enabled", .F.)  // выключить таймер
                      NEXT
                      myExitWithoutUserWork(o:nJobMinMax)           // Выход без работы юзера -> main_misc.prg
                      DoMethod(cForm, "Release")
                   ENDIF
                ENDIF
                Return Nil
                })
   // показ сообщений в трее
   :Event( 51, {|oa,ky,nn,as| // -> myEventTreatment(cData) -> main_misc.prg
                     LOCAL nico, cmsg, cttl
                     Local ow := App.Cargo:oWinMain
                     Default as := {1, "Message","Title"}
                     nico := as[1] ; cmsg := as[2] ; cttl := as[3]
                     ? "==> 51 "+ProcNL(), ow:Name, ky, as,  HB_ValToExp(as), "// проверка"
                     IF "(no display in tray)" $ LOWER(cmsg)
                        // пропуск показа сообщения в трее
                     ELSE
                        IF lStatBallonNotify  // разрешено отображать в трее сообщения
                           MsgBalloon(ow, cmsg, cttl, nico)
                           wApi_Sleep( 3 * 1000 )     // seconds wait
                           ActivateNotifyMenu(ow)
                        ENDIF
                     ENDIF
                     nn := oa
                     Return Nil
                     })

   :Event(101, {|oa,ky,nn,xv|  // резерв
                Local nDataID := nn
                Local cData := xv
                ? ProcNL(), oa, ky, nDataID, cData
                // отсюда заполняем данные и поднимаем окно
                // нельзя фокус ставить на то окно, может поменяться This среда
                // _pPost(ky, nDataID, cData) // активация выполнения
                Return Nil
                })

   // передать сообщение и показ сообщений в трее - не нужно
   /*:Event(110, {|oa,ky,nn,as| // -> OnlyOneInstance() -> main_misc.prg
                     LOCAL nico, cmsg, cttl, cData, hWnd
                     Local ow  := App.Cargo:oWinMain
                     Local cEx := cFileNoPath(App.ExeName)
                     Default as := {2, "Window: FORM_MAIN to foreground","Message - " + cEx}
                     nico := as[1] ; cmsg := as[2] ; cttl := as[3]
                     ? "==> 110 "+ProcNL(), ow:Name, ky, as,  HB_ValToExp(as), "// проверка"
                     IF lStatBallonNotify  // разрешено отображать в трее сообщения
                        MsgBalloon(ow, cmsg, cttl, nico)
                        wApi_Sleep( 3 * 1000 )     // seconds wait
                        ActivateNotifyMenu(ow)
                     ENDIF
                     cData := cFileNoPath(App.ExeName) + " | "
                     cData += HB_TSTOSTR( HB_DATETIME() )
                     cData += " - Message WM_SetFocus !"
                     hWnd := FindWindowEx( ,,, APP_TITLE )
                     // Transfer data to window -> APP_TITLE
                     SendMessageData( hWnd, cData, APP_ID )
                     nn := oa
                     Return Nil
                     }) */

   :Event({111, "WM_CopyData"}, {|oa,ky,nid,cdata|
                Local ow := App.Cargo:oWinMain
                Local o := oStat_JobWait
                Local oThis := _oThis()
                Local cky := oa:oEvents:Get(ky, "?")
                Local nky := oa:oEvents:Get(cky, 0)
                oThis:cName := cValToChar(nid)
                oThis:cData := cdata
                oThis:cKey  := cky
                oThis:nKey  := nky
                cdata := hb_defaultValue(cdata, "")
                ? Space(24)+"|---[ App.Object:Event 111 ]--- |" + ProcNL()
                ? Space(24)+"|oa=",oa,"nId=",nid,"cData=",cdata,"|"
                ? Space(24)+"|ow:Name",ow:Name,"ow:Handle",ow:Handle,"|"
                ? Space(24)+"|oThis=",oThis:GetAll() ; ?v oThis:GetAll() ; ?
                o:nJobTimer := Seconds()
                myEventTreatment(cdata)
                DoEvents()
                ky := oa
                Return Nil
                })
   // set events App\Program end
   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////
// Обработчик событий программы
// этот обработчик вызывается всегда вместо Events(...)
STATIC FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )
   LOCAL cData, nDataID, cMsg, nRet := 0, lQuit := .F.
   LOCAL ky, cn, i, o, ow, cName, cTitle
   LOCAL cFileTxt, cExeRun, cParam, nHandle := 0
   LOCAL cVal, cTime := HB_TSTOSTR( HB_DATETIME() )
   LOCAL cLogEvents := App.Cargo:cLogEvents

   IF ! lStat_AppOnEvents                                  // обработка - для ВСЕХ окон
   //IF ! lStat_AppOnEvents .or. Empty( _HMG_MainHandle )   // nMsg НЕ вкл. обработка - только для окна MAIN
      RETURN 0 //Events( hWnd, nMsg, wParam, lParam)       // nMsg НЕ выполненно
   ENDIF

   //ow := _WindowObj( _HMG_MainHandle )
   //IF hb_IsObject(ow)
   // nHandle := ow:Handle
   //ENDIF

   //IF hWnd == nHandle // только для MAIN window - убрали
   // для всех окон движения мыши и ее клики
      SWITCH nMsg
      **********************************************************************
      CASE WM_MBUTTONDOWN
      CASE WM_RBUTTONDOWN
      CASE WM_LBUTTONDOWN
      CASE WM_MOUSEMOVE
      CASE WM_MOVE
      *********************************************************************
        o := oStat_JobWait
        i := 0
        IF o:nMouseRow != _HMG_MouseRow
           i++
           o:nMouseRow := _HMG_MouseRow
        ENDIF
        IF o:nMouseCol != _HMG_MouseCol
           i++
           o:nMouseCol := _HMG_MouseCol
        ENDIF
        IF i > 0
           o:nJobTimer := Seconds()
        ENDIF
        EXIT

      *********************************************************************
      CASE WM_HOTKEY
      CASE WM_KEYDOWN
      CASE WM_KEYUP
      CASE WM_MOUSEWHEEL
      CASE WM_INITMENUPOPUP
      CASE WM_UNINITMENUPOPUP
      CASE WM_SETFOCUS
      CASE WM_HELP
      CASE WM_HSCROLL
      CASE WM_VSCROLL
      CASE WM_TASKBAR
      CASE WM_NEXTDLGCTL
      CASE WM_DROPFILES
      CASE WM_CONTEXTMENU
      CASE WM_SIZE
      CASE WM_COMMAND
      CASE WM_NOTIFY
      *********************************************************************
        o := oStat_JobWait
        o:nJobTimer := Seconds()
        EXIT

      **********************************************************************
      CASE WM_TIMER
      **********************************************************************
        cMsg := "Event arrived - WM_TIMER"

        IF ( i := AScan ( _HMG_aControlIds, wParam ) ) > 0
           cn := _HMG_aControlNames[ i ]
           ky := Val( subs(cn, RAt("_", cn) + 1) )
           cMsg += " = " + cn + " ( ky = "+hb_ntos(ky)+" )"
           //DO EVENTS
           // не меняет среду This
           i := "Timer_"+hb_ntos(ky)  // имя события
           _pPost(i, ky, cn)
           //_pPost(ky, ky, cn)
           //(App.Object):PostMsg(ky, ky, cn)  // свое событие у timer-а
           //IF ky == 9
           //  (App.Object):PostMsg(ky,  0, cn)  // 1.свое событие у timer-а
           //ELSE
           //_pPost("Timer_All", ky, cn)
           //  (App.Object):PostMsg(10, ky, cn) // 2.одно событие для 3-х timer-ов,
           //ENDIF
        ENDIF

        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        //STRFILE( cVal, cLogEvents, .T. ) ; ? cVal - !!! включить если надо

        nRet := 1  // выполненно nMsg
        EXIT

      **********************************************************************
      CASE WM_COPYDATA
      **********************************************************************
        cMsg := "Event - WM_COPYDATA"
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal, cLogEvents, .T. ) ; ? cVal

        cData := GetMessageData( lParam, @nDataID )

        IF nDataID == APP_ID
           _pPost("WM_CopyData", nDataID, cData)
           //_pPost(111, nDataID, cData)
           //(App.Object):PostMsg(111, nDataID, cData)
        ENDIF

        nRet := 1  // выполненно nMsg
        EXIT

      **********************************************************************
      CASE WM_POWERBROADCAST
      **********************************************************************
        cMsg := "Event - WM_POWERBROADCAST"
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        cVal := "wParam=" + cValToChar(wParam)
        cMsg := ""
        IF    wParam == PBT_APMSUSPEND
           cMsg  := "PBT_APMSUSPEND"
           lQuit := .T.
        ELSEIF wParam == PBT_APMSTANDBY
           cMsg := "PBT_APMSTANDBY"
        ELSEIF wParam == PBT_APMRESUMESUSPEND
           cMsg := "PBT_APMRESUMESUSPEND"
        ELSEIF wParam == PBT_APMRESUMESTANDBY
           cMsg := "PBT_APMRESUMESTANDBY"
        ELSEIF wParam == PBT_APMRESUMEAUTOMATIC
           cMsg := "PBT_APMRESUMEAUTOMATIC"
        ENDIF
        IF !Empty(cMsg)
           cMsg := cVal + " - " + cMsg
        ENDIF
        cVal := Space(Len(cTime)) + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        ? Space(Len(cTime)) + " | hWnd, nMsg, wParam, lParam", hWnd, nMsg, wParam, lParam

        IF lQuit
           cMsg := "WARNING! Your computer has gone to sleep !"
           ? Space(Len(cTime)) + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()

           cExeRun := App.Cargo:cExeAlert
           IF FILE(cExeRun)
              cMsg    := "WARNING !;Your computer has gone to sleep;;"
              cMsg    += "Program: " + APP_TITLE + ";STOPPED"
              cMsg    += " !  " + cTime
              cParam  := '-warning "' + cMsg + '"'
              cMsg := "ShellExecute()=" + cFileNoPath(cExeRun) + "," + cParam
              ? Space(Len(cTime)) + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
              ShellExecute( , 'open', cExeRun, cParam, , SW_SHOWNORMAL)
           ELSE
              cFileTxt := ChangeFileExt( ExeName(), '.txt' )
              cMsg     := AtRepl( ";", cMsg, CRLF )
              HB_MemoWrit( cFileTxt , cMsg + CRLF)
              wApi_Sleep(100)
              ShellExecute(0,"Open",cFileTxt,,,SW_SHOWNORMAL)
           ENDIF
           wApi_Sleep(100)
           ow := _WindowObj( _HMG_MainHandle )          // MAIN окно
           _wSend(99, ow, {hWnd, nMsg, wParam, lParam})  // посылаем сообщение MAIN окну
        ENDIF

        nRet := 1  // выполненно nMsg
        EXIT

      **********************************************************************
      CASE WM_CLOSE
      **********************************************************************
        cName := cTitle := "-?-"
        ow   := _WindowObj( hWnd )
        IF hb_IsObject(ow)
           cName  := ow:Name
           cTitle := ow:Title
        ENDIF
        cMsg := "Event - WM_CLOSE " + cName + ": " + cTitle
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      **********************************************************************
      CASE WM_QUIT
      **********************************************************************
        cMsg := "Event - WM_QUIT " + cName
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      **********************************************************************
      CASE WM_SYSTEMERROR
      **********************************************************************
        cMsg := "Event - WM_SYSTEMERROR"
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      **********************************************************************
      CASE WM_DESTROY
      **********************************************************************
        cName := "" // GetFormNameByIndex( GetFormIndexByHandle( hWnd ) )
        cMsg  := "Event - WM_DESTROY " + cName    // окна уже нет
        cVal  := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      **********************************************************************
      CASE WM_ENDSESSION
      **********************************************************************
        cMsg := "Event - WM_ENDSESSION"
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      **********************************************************************
      CASE WM_QUERYENDSESSION
      **********************************************************************
        cMsg := "Event - WM_QUERYENDSESSION"
        cVal := cTime + " | " + PADR(cMsg,LEN_SPC) + " | " + ProcNL()
        STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal
        EXIT

      END SWITCH
   //ENDIF  hWnd == nHandle   // только для MAIN window

   IF nRet > 0 ; RETURN 1                       // nMsg выполненно
   ENDIF

RETURN 0 //Events( hWnd, nMsg, wParam, lParam)  // nMsg НЕ выполненно

///////////////////////////////////////////////////////////////////
FUNCTION MsgBalloon( ow, cMessage, cTitle, nIconIndex )
   LOCAL i := ow:Index
   LOCAL h := ow:Handle
   LOCAL n := _HMG_aFormNotifyIconName[i]
   LOCAL t := _HMG_aFormNotifyIconToolTip[i]

   Default cMessage := "Prompt", cTitle := APP_TITLE, nIconIndex := 1 //NIIF_INFO

   ShowNotifyInfo( h, .F. , NIL, NIL, NIL, NIL, 0 )

   ShowNotifyInfo( h, .T., LoadTrayIcon( GetInstance(), n ), t, cMessage, cTitle, nIconIndex )

RETURN Nil

///////////////////////////////////////////////////////////////////
FUNCTION ActivateNotifyMenu(ow)
   LOCAL i := ow:Index
   LOCAL h := ow:Handle
   LOCAL n := _HMG_aFormNotifyIconName[i]
   LOCAL t := _HMG_aFormNotifyIconToolTip[i]

   ShowNotifyInfo( h, .F. , NIL, NIL, NIL, NIL, 0 )

   ShowNotifyIcon( h, .T. , LoadTrayIcon( GetInstance(), n ), t )

   myMainMenuNotify("NOTIFY", ow)

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
// MINIGUI - Harbour Win32 GUI library Demo
// MiniGUI\SAMPLES\Advanced\TrayBalloon
// Notify Icon Infotip flags
#define NIIF_NONE     0x00000000
// icon flags are mutualy exclusive
// and take only the lowest 2 bits
#define NIIF_INFO     0x00000001
#define NIIF_WARNING  0x00000002
#define NIIF_ERROR      0x00000003
/*
 * C-level
*/
#pragma BEGINDUMP

#define _WIN32_WINNT_VISTA       0x0600
#define _WIN32_WINNT _WIN32_WINNT_VISTA
#ifndef NTDDI_VERSION
#define NTDDI_VERSION        0x06000000
#endif

#include <windows.h>
#include "hbapi.h"
#include <shellapi.h>

static void ShowNotifyInfo(HWND hWnd, BOOL bAdd, HICON hIcon, LPSTR szText, LPSTR szInfo, LPSTR szInfoTitle, DWORD nIconIndex);

HB_FUNC ( SHOWNOTIFYINFO )
{
   ShowNotifyInfo( (HWND) hb_parnl(1), (BOOL) hb_parl(2), (HICON) hb_parnl(3), (LPSTR) hb_parc(4),
         (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (DWORD) hb_parnl(7) );
}

static void ShowNotifyInfo(HWND hWnd, BOOL bAdd, HICON hIcon, LPSTR szText, LPSTR szInfo, LPSTR szInfoTitle, DWORD nIconIndex)
{
   NOTIFYICONDATA nid;

   ZeroMemory( &nid, sizeof(nid) );

   nid.cbSize    = sizeof(NOTIFYICONDATA);
   nid.hIcon      = hIcon;
   nid.hWnd  = hWnd;
   nid.uID       = 0;
   nid.uFlags    = NIF_INFO | NIF_TIP | NIF_ICON;
   nid.dwInfoFlags      = nIconIndex;

   lstrcpy( nid.szTip, TEXT(szText) );
   lstrcpy( nid.szInfo, TEXT(szInfo) );
   lstrcpy( nid.szInfoTitle, TEXT(szInfoTitle) );

   if(bAdd)
      Shell_NotifyIcon( NIM_ADD, &nid );
   else
      Shell_NotifyIcon( NIM_DELETE, &nid );

   if(hIcon)
      DestroyIcon( hIcon );
}

#pragma ENDDUMP
