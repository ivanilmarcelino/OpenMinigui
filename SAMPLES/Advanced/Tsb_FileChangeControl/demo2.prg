/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Работа с классом TThrData (доступ в потоках к переменным идет с блокировками)
 * Запуск программ через hb_ProcessOpen() в отдельном потоке
 * Проверка на вторую копию запуска процесса
 * Контроль изменения файлов в своей программе
 * Закрытие процессов запущенных из своей программы
 * Working with the TThrData class (access to variables in threads comes with locks)
 * Running programs via hb_ProcessOpen() in a separate thread
 * Check for a second copy of the startup process
 * Control file changes in your program
 * Closing processes running from your program
*/
// !!! ONLY for version from Harbour MiniGUI Extended Edition 23.04 (Update 2) and higher

REQUEST DBFCDX, DBFFPT

#define _HMG_OUTLOG
#define PRG_NAME  "Control file changes in another program / 06.2023 / Without timer"
#define LINE_FILL 110

#include "hmg.ch"
#include "TSBrowse.ch"
#include "hbthread.ch"

MEMVAR oThread
////////////////////////////////////////////////////////////////////////////////
INIT PROCEDURE My_Sets_Env
   LOCAL cFont := "DejaVu Sans Mono", nSize := 14
   LOCAL cMsg, tTime, cIcon := "1MAIN_ICO"
   LOCAL cLog := ChangeFileExt( App.ExeName, '.log' )

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN
   SET DATE  TO GERMAN
   SET EPOCH TO ( Year(Date()) - 50 )
   SET NAVIGATION EXTENDED
   SET MULTIPLE OFF WARNING
   SET OOP ON
   RddSetDefault("DBFCDX")
   SET FONT TO cFont, nSize
   SET DEFAULT ICON TO "1MAIN_ICO"
   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }      // for HMG_Alert()
   DEFINE FONT DlgFont FONTNAME cFont SIZE nSize    // for HMG_Alert()

   _SetGetLogFile( cLog ) ; DELETEFILE(cLog)  // new log file name for debug output

   tTime := HB_DATETIME()
   ? REPL("=",20) + " Program start - " + HB_TTOC( tTime ) + " " + REPL("=",20)
   ? MiniGuiVersion() ; ?

   IF ! hb_mtvm()
      cMsg := "No multithreading support !" + CRLF
      cMsg += "Program compilation key -mt !" + CRLF
      AlertStop( cMsg, App.ExeName  )
      ? cMsg
      QUIT
   ENDIF

   // 1-вариант использования переменной для потоков (пример, далее нигде не используется)
   // 1st option for using a variable for threads (example, not used anywhere else)
   PUBLIC oThread ; oThread := oThrData()  // создадим контейнер для работы с потоками
                                           // create a container for working with streams
   oThread:hPid     := 0
   oThread:hProcess := 0
   oThread:aProcess := {}
   oThread:aTest    := {1,2,3,4,5,6,7} 

   // 2-вариант использования переменной для потоков
   // 2nd option for using a variable for threads
   // this object is always available anywhere in the program
   App.Cargo := oThrData()         // создадим контейнер - доступ везде в программе
                                   // create a container - access everywhere in the program
   App.Cargo:nTimerSec := 5
   App.Cargo:tStart    := HB_DATETIME()
   App.Cargo:cFileDbf  := GetStartUpFolder() + "\test2.dbf"
   App.Cargo:cAlias    := "MyTest"
   App.Cargo:cLog      := cLog          // _SetGetLogFile()
   App.Cargo:cWinForm  := "wMain"       // main form name
   App.Cargo:cWinMemo  := "Edit_Memo"   // object for debug output to the form
   App.Cargo:cFont     := cFont
   App.Cargo:cFSize    := 12
   App.Cargo:aProcess  := {}            // an array to write the data of the running thread
   App.Cargo:aTest     := {1,2,3,4,5,6,7} 

RETURN

////////////////////////////////////////////////////////////////////////////////
// !!! ONLY for version from Harbour MiniGUI Extended Edition 23.04 (Update 2) and higher
FUNCTION oThrData()     // class TThrData see h_objects.prg
 RETURN TThrData():New()

////////////////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL nG, nY, nX, nW, nH, nHBtn, nWBtn, nRecno, cFormName, cFilter
   LOCAL cAls, cTsb, oBrw, nWTsb, nHTsb, aTsbClrBC, aTsbClrText
   LOCAL cFont := App.Cargo:cFont, nFSize := App.Cargo:cFSize

   cAls := myCreateFiles() // preparation of buttons on the form (reading files)
   IF LEN(cAls) == 0
      AlertStop( "The base is not open !;" + (App.Cargo):cFileDbf , App.ExeName )
      QUIT
   ENDIF

   nRecno             := (cAls)->( LASTREC() )
   oThread:aProcess   := ARRAY(nRecno)
   App.Cargo:aProcess := ARRAY(nRecno)
   AFILL( oThread:aProcess  , {} )  // номер записи для блокирования, если={}, то не было запуска внешнего потока
   AFILL( App.Cargo:aProcess, {} )  // record number to block, if={}, then no external thread was started

   cFormName := App.Cargo:cWinForm

   DEFINE WINDOW &cFormName                                     ;
      AT 150,150 WIDTH 800 HEIGHT 780                           ;
      TITLE PRG_NAME                                            ;
      MAIN NOMAXIMIZE NOSIZE TOPMOST                            ;
      BACKCOLOR  { 0,64,80 }                                    ;
      FONT cFont SIZE nFSize                                    ;
      ON INIT  {|| This.Topmost := .F., DoEvents(), _wPost(0) } ;  // executed after window initialization
      ON RELEASE {|| _wSend(90), DoEvents(), _wSend(92) }       ;  // executed before destroying the window
      ON INTERACTIVECLOSE {|| NIL }                                // close window by [x]

      This.Cargo        := oHmgData()          // for the window we create an object without variables (conditionally empty)
      This.Cargo:cLog   := _SetGetLogFile()
      This.Cargo:cMemo  := App.Cargo:cWinMemo  // as an example how to do
      This.Cargo:tStart := HB_DATETIME()
      // в контейнер для работы в thread-ах и основного процесса запоминаем
      // into the container for work in threads and the main process, remember
      App.Cargo:oWindow := This.Object  // объект окна это как в событии {|ow|...}
      App.Cargo:cWindow := This.Name    // имя окна, просто для примера
      App.Cargo:hWindow := This.Handle  // handle окна, просто для примера

      nY     := nX := 20
      nG     := 10
      nW     := This.ClientWidth
      nH     := This.ClientHeight
      nHBtn  := 50
      nY     := nG
      nWBtn  := ( nW - nX * 4 ) / 3

      DEFINE STATUSBAR FONT cFont SIZE nFSize - 4
         STATUSITEM PRG_NAME    WIDTH 180 FONTCOLOR BLUE
         STATUSITEM "Vers 0.1"    WIDTH 100  FONTCOLOR RED
         STATUSITEM DTOC(DATE())  WIDTH 120  FONTCOLOR BLUE
      END STATUSBAR

      @ 10, 0 LABEL Label_0 WIDTH 10 HEIGHT 10 VALUE "" TRANSPARENT

      @ nY, nX BUTTONEX Button_1 WIDTH nWBtn HEIGHT nHBtn ;
        CAPTION 'App.Cargo' BACKCOLOR CLR_ORANGE NOXPSTYLE ACTION _wPost(5, , This.Name)

      @ nY, nX*2 + nWBtn BUTTONEX Button_2 WIDTH nWBtn HEIGHT nHBtn ;
        CAPTION 'oWnd:Cargo' BACKCOLOR CLR_CYAN NOXPSTYLE ACTION _wPost(5, , This.Name)

      @ nY, nX*3 + nWBtn*2 BUTTONEX Button_Ex WIDTH nWBtn HEIGHT nHBtn ;
        CAPTION 'EXIT' BACKCOLOR CLR_RED NOXPSTYLE ACTION _wPost(99, , This.Name)
      nY += nG + nHBtn

      cTsb        := "Tsb_File"
      nWTsb       := nW - nX * 2
      nHTsb       := 270
      aTsbClrBC   := {0,176,240}
      aTsbClrText := BLUE
      DEFINE TBROWSE &cTsb OBJ oBrw AT nY, nX WIDTH nWTsb HEIGHT nHTsb CELL ;
         OF wMain          ;
         ALIAS cAls        ;
         LOADFIELDS        ;  // automatic creation of columns by fields of the active database
         SELECTOR NIL      ;
         ENUMERATOR        ;
         COLNUMBER  {1,40} ;
         ON GOTFOCUS NIL   ;
         ON CHANGE   NIL   ;
         ON INIT {|ob| Tsb_Init( ob ) }       // see below

         Tsb_Create( oBrw, aTsbClrBC, aTsbClrText )

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      // в качестве примера - установка фильтра на базу
      // as an example - setting the filter on the base
      //cFilter := 'Empty(F1)'
      //oBrw:FilterData( cFilter )      
      //oBrw:aColumns[3]:cFooting := "Filter: " + cFilter

      cFilter := 'Without a filter !'
      oBrw:aColumns[3]:cFooting := cFilter
      oBrw:GetColumn("NAME_3"):nFAlign := DT_LEFT

      This.Cargo:cFltr := cFilter  // reserve, can be used later
      This.Cargo:oBrw  := oBrw     // reserve, can be used later
      This.Cargo:cAls  := ALIAS()  // reserve, can be used later
      nY += nG + nHTsb

      //  App.Cargo:cWinMemo := "Edit_Memo"    -  должно совпадать !!! must match
      @ nY, nX EDITBOX Edit_Memo WIDTH nWTsb HEIGHT nFSize*20 VALUE "" ;
         BACKCOLOR SILVER FONTCOLOR MAROON FONT cFont SIZE nFSize - 5 ;
         MAXLENGTH 1200 NOHSCROLL READONLY
      nY += This.Edit_Memo.Height + nG

      // set the outer height of the window, reduce it
      ThisWindow.Height := nY + nG*2 + GetBorderHeight() + GetMenuBarHeight()

      WITH OBJECT This.Object
         :Event( 0, {|  | LogScreen(), DoEvents() } )

         // select row from table
         :Event( 2, {|ow,ky,xv,ob| ob := This.Tsb_File.Object, myRunFiles(ow,ky,xv,ob)  } )
         // variable output
         :Event( 5, {|ow,ky,xv| myDebugVar(ow,ky,xv), DoEvents()  } )
         // update table on event
         :Event(10, {|ow,ky,xv| // if the file is changed - reread the table
                     Local cVal, oBrw := This.Tsb_File.Object
                     Local nRec  := xv[1]  // nRecno
                     Local nPos  := xv[2]  // nRowPos
                     Local cFltr := (oBrw:cAlias)->(dbFilter())
                     cVal := HMG_TimeMS(App.Cargo:tStart)
                     ? cVal , ProcNL(), ow:Name, "ky=", ky, "xv=", HB_ValToExp(xv), cFltr
                     IF ! oBrw:IsEdit                                  // TBROWSE НЕ в режиме редактирования
                        IF !Empty( cFltr )                             // если есть фильтр по базе
                           oBrw:Reset()                                // обновим всю таблицу
                           ?? "-1-:Reset() with filter !"
                        ELSE
                           ?? "-2-:Refresh() without a filter !"
                           IF oBrw:lRowPosAtRec                        // режим заполнения Recno для всех записей :RowCount()
                              IF aScan(oBrw:aRowPosAtRec, nRec) > 0    // измененная запись на экране
                                 // Надо обновить буфер записи RDD в основном процессе, в thread dbcommit() сделан,
                                 // на диск записано, теперь перечитаем с диска запись в буфер Rdd
                                 // We need to update the RDD write buffer in the main process, thread dbcommit() is done,
                                 // written to disk, now reread from disk write to Rdd buffer                                 nPos := (oBrw:cAlias)->(RecNo()) 
                                 (oBrw:cAlias)->(dbGoto(nRec)) 
                                 (oBrw:cAlias)->(dbGoto(nPos))
                                 oBrw:Refresh()           // перечитаем всю таблицу
                              ENDIF
                           ELSE
                              oBrw:GotoRec(nRec, nPos)    // eyebrow sync
                           ENDIF
                        ENDIF
                        ?? "table update success" ; ?
                     ELSE
                        ?? "skipping table update, editing in progress" ; ?
                     ENDIF
                     _wSend(0, ow)   // LogScreen()
                     Return Nil
                    })

         :Event(90, {|  | CloseProcess(), DoEvents(), _wSend(91)                                                } )
         :Event(91, {|  | _LogFile(.T., ">>> End of program <<<  "+HMG_TimeMS((App.Cargo):tStart) ), DoEvents() } )
         //:Event(92, {|  | DoEvents(), ShellExecute(0,"Open",cLog,,,1)                           } )
         //:Event(92, {|ow| DoEvents(), ShellExecute(0,"Open",ow:Cargo:cLog,,,1)  /* could be so */ } )
         :Event(99, {|ow| ow:Release()  } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|| _wPost(99) }

   END WINDOW

   //CENTER   WINDOW &cFormName // move here so that the window does not twitch
   ACTIVATE WINDOW &cFormName ON INIT  {|| This.Minimize, This.Restore }

RETURN

///////////////////////////////////////////////////////////////////////////////
// read the log-file and output to the form
FUNCTION LogScreen()
   LOCAL cWnd, cLog, cMemo, cStr

   cWnd  := App.Cargo:cWinForm
   cLog  := App.Cargo:cLog
   cMemo := App.Cargo:cWinMemo
   cStr  := hb_MemoRead(cLog)

   SetProperty(cWnd, cMemo, "Value", cStr)
   SendMessage(GetControlHandle(cMemo, cWnd), WM_VSCROLL, SB_BOTTOM, 0)
   DO EVENTS
   // В потоках (один или много их) нельзя делать команды hmg
   // SetProperty(...) которые меняют внутренние PUBLIC _HMG_... переменные, т.к
   // эти переменные без блокировок доступа, а значит доступны ТОЛЬКО для чтения,
   // теоретически их можно менять, но одновременный доступ из разных потоков на
   // чтение или запись даст что то плохое, т.к. память, адрес данных могут уже
   // для них оказаться разными.
   // В данном случае можно так делать, т.к. нет в основном потоке
   // действий одновременных с потоком над контролом EDITBOX Edit_Memo.
   // В этом примере notepad, EDITBOX и oBrw работают последовательно,
   // т.к. запустив несколько notepad, работаем последовательно с ними
   // и выход из них такой же, т.е. нет одновременного доступа для
   // изменений данных внутренних PUBLIC _HMG_... переменных.
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_Init( oBrw )    // !!! only these settings in ON INIT !!!

   WITH OBJECT oBrw
       :nColOrder := 0         // remove sort by field icon
       :lNoChangeOrd  := .T.   // remove sorting by field
       :nWheelLines   := 1     // scroll the mouse wheel in increments ...
       :lNoGrayBar    := .F.   // show an inactive cursor in the table
       :lNoLiteBar    := .F.   // when switching focus to another window, do not remove the "light" Bar
       :lNoResetPos   := .F.   // prevents the position of the record on gotfocus from being reset
       :lNoPopUp      := .T.   // avoid popup menu when right clicking column header
       :nStatusItem   := 0     // in the 1st Item StatusBar do not display automatically from the tb
       :lPickerMode   := .F.   // date format is normal
       :nMemoHV       := 1     // show one line memo field
       :lCellBrw      := .F.   // marker for the entire table
       :lNoHScroll    := .T.   // NO display of horizontal scrolling
       :lFooting      := .T.   // use footer
       :lDrawFooters  := .T.   // draw cellars
       :lNoMoveCols   := .T.   // .T. - DO NOT allow users to resize or move columns
       :nCellMarginLR := 1     // indent from the cell line when pressed to the left, to the right by the number of spaces
       :lRowPosAtRec := .T.    // save RecNo\nAt position for :DrawLine
       :Cargo := oHmgData()    // RESERVE - init Cargo as THmgData container object
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////
FUNCTION Tsb_Create( oBrw, aClrBC, aClrText )
   LOCAL nCol, oCol, cParent := oBrw:cParentWnd
   LOCAL aHead := { "№№", "Status", "File", "Size", "Date, time", "File", "Path" }

   WITH OBJECT oBrw

      :SetAppendMode( .F. )  // inserting a record at the end of the database with a down arrow is prohibited
      :SetDeleteMode( .F. )  // deletion is not allowed

      :nHeightCell   += 2               // cell row height
      :nHeightHead   += 6               // table header height
      :nHeightFoot   := :nHeightHead    // basement height
      :nHeightSpecHd := 14              // special header height ENUMERATOR
      :lFooting      := .T.             // use the basement
      :lDrawFooters  := .T.             // draw cellars
      :nClrLine       := COLOR_GRID     // line color between table cells
      :nColOrder      := 0              // function :LoadFields() sets :nColOrder, must be canceled
      :nFreeze        := 2              // Freeze column
      :lLockFreeze    := .T.            // Avoid drawing cursor on frozen columns
      :HideColumns( {6,7} ,.t.)         // hide columns
      :AdjColumns( {5} )                // stretch the column to fill the void in the eyebrow on the right
      :ResetVScroll( .T. )              // be sure to reread the state of vertical scrolling

      :SetColor( {  1 },  { { || Rgb(aClrText[1], aClrText[2], aClrText[3] )}})    // text color in cells
      :SetColor( {  2 },  { { || Rgb(aClrBC[1], aClrBC[2], aClrBC[3]) }})          // background color not in cursor
      :SetColor( {  3 },  {        Rgb( 255, 255, 255 )  })                        // header text color
      :SetColor( {  4 },  { { || { Rgb(  43, 149, 168 ), Rgb(  0,  54,  94 )}}})   // header background
      :SetColor( {  5 },  { { || Rgb(  0, 0, 255 )     } })                        // text in cells with focus
      :SetColor( {  6 },  { { || { Rgb( 255, 255,  74 ), Rgb( 255, 0,   0 )}}})    // cursor background
      :SetColor( {  9 },  {        Rgb( 255, 255, 255 )  })                        // footer text color
      :SetColor( { 10 },  { { || { Rgb(  43, 149, 168 ), Rgb(  0,  54,  94 )}}})   // footer background
      :SetColor( { 11 },  { { || Rgb(  0, 0, 0 )  }})                              // text color in inactive cursor
      :SetColor( { 12 },  { { || { Rgb( 128, 128, 128 ), Rgb( 250, 250, 250 )}}})  // cursor background inactive
      :SetColor( { 16 },  {        Rgb(  43, 149, 168 )})                          // Background color in superheader
      :SetColor( { 17 },  {        Rgb( 255, 255, 255 )})                          // text color in superheader

      // Prompt output using inner TBrowse loop - fallback
      //:bEvents := { |a,b| myEventBrowse(a,b) }

      // Double click on the MARKER
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

      // processing the ENTER and ESC keys
      :UserKeys(VK_RETURN, {|ob| _wPost( 2, ob:cParentWnd), .F. })
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })

      // put the title of the columns in the header and footer
      FOR nCol := 1 TO Len(:aColumns)
         oCol          := oBrw:aColumns[ nCol ]
         oCol:cName    := "NAME_" + HB_NtoS(nCol)
         oCol:cHeading := aHead[ nCol ]
         //oCol:cFooting := ""
      NEXT

   END WITH

   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myRunFiles(oWnd, nKy, xPar3, oBrw)
   LOCAL cWnd, aFile, cAls, nRecno, aParams := hb_aParams()
   DEFAULT oWnd := ThisWindow.Object
   DEFAULT oBrw := oWnd:Cargo:oBrw

   cWnd := oWnd:Name
   cAls := oBrw:cAlias  // as an example

   ? Repl("-",LINE_FILL)
   ? ProcNL(), "(", oWnd:ClassName, nKy, xPar3, oBrw:ClassName, ")"
   ?? "oWnd:Name= " + cWnd, cAls

   nRecno := (cAls)->( Recno() )
   aFile  := { nRecno,                ;  // 1
               ALLTRIM( (cAls)->F6 ), ;  // 2
               ALLTRIM( (cAls)->F5 ), ;  // 3
               (cAls)->F4,            ;  // 4
               oBrw:nRowPos }            // 5
   ? ; ? "aFile= " + HB_ValToExp(aFile)
   ? Repl("-",LINE_FILL)

   _wSend(0)   // LogScreen() // read the log-file and output to the form

   ? ProcNL(), "calling a new thread function - myThreadFile(aFile,oWnd)"
   myThreadFile(aFile,oWnd)

   _wSend(0)   // LogScreen() // read the log-file and output to the form

   wApi_Sleep(100)

   oBrw:SetFocus()

   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myThreadFile(aFile, oWnd)
   // Start in a separate thread
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @RunExternal(), aFile, oWnd ) )
RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION RunExternal( aFile, oWnd )   // oWnd - just as an example
   LOCAL cFileRun, nWaitCode, cMsg, hProcess, hWnd, hPid, cWnd, cInfo, aRet
   LOCAL hProcessOpenHandle, cFileExe, aProcess, nRecno, cTestWin, a3Proces
   LOCAL nRowPos

   ? Repl("-",LINE_FILL)
   ? ProcNL(), "(", aFile, oWnd:ClassName, ")"

   aProcess := App.Cargo:aProcess
   nRecno   := aFile[1]        // the number in the array is the same as the record number
   cFileRun := aFile[2] + aFile[3]
   nRowPos  := aFile[5]
   a3Proces := aProcess[nRecno]
   cInfo  := " -> hProcessOpenHandle = "
   cFileExe := "Notepad.exe"
   cTestWin := oWnd:Name           // just as an example of usage

   IF LEN(a3Proces) > 0 // значит уже запущен внешний поток / means the external thread is already running
      hPid := a3Proces[1]
      cWnd := a3Proces[2]
      hWnd := a3Proces[3]
      ? "The program is already running ...", a3Proces
      ?? "Restore Windows: " + HB_ValToExp(a3Proces)
      IF hWnd > 0
         IF IsIconic( hWnd )
            _Restore( hWnd )
         ENDIF
         SetForegroundWindow(hWnd)
         DO EVENTS
      ENDIF
      //!!! NO, we are in the STREAM _wSend(0, oWnd)   // LogScreen()
      DO EVENTS ; _wPost(0, oWnd) ; wApi_Sleep(50)
      RETURN NIL
   ENDIF

   IF FILE( cFileRun )
      hProcessOpenHandle := hb_processOpen( cFileExe + " " + cFileRun,,,,.F., @hProcess)
      cMsg := cInfo + HB_NtoS(hProcessOpenHandle)
      ? "Run ->", cFileRun, cMsg, "hProcess=", hProcess
      DO EVENTS ; wApi_Sleep(150)                // waiting for drawing

      aRet := myFindWndFromPid( hProcess )     // search for a running thread
      hWnd := aRet[1]
      cWnd := aRet[2]
      aRet := { hProcessOpenHandle, cWnd, hWnd, hProcess }
      App.Cargo:aProcess[nRecno] := aRet
      ? ProcNL(), "App.Cargo:aProcess[nRecno]= " + HB_ValToExp(aRet) + "[nRecno]=", nRecno

      //!!! NO, we are in the STREAM _wSend(0, oWnd)    // LogScreen() 
      DO EVENTS ; _wPost(0, oWnd) ; wApi_Sleep(50)      // так правильно

      // Watchman of a running program - control of the process launched from its program
      // Waits until the specified object is in the signaled state or the time-out interval elapses
      nWaitCode := WaitForSingleObject(hProcessOpenHandle)
      // nWaitCode = 258 - timeout process not completed
      // nWaitCode =  -1 - incorrect process
      // nWaitCode = 0 - process completed
      cMsg := cFileRun + "; "
      cMsg += "STOP hb_ProcessOpen() ! RunFileInternal: " + ALLTRIM(STR(nWaitCode))
      IF nWaitCode == 0
         cMsg += "; Process completed !"
         AlertInfo(cMsg, App.ExeName)
      ELSE
         cMsg += "; Process termination error !"
         AlertStop(cMsg, App.ExeName)
      ENDIF
      // end process, close window
      App.Cargo:aProcess[nRecno] := {} // reset the value - to unblock the launch of the thread
      ? "Close ->", cMsg
      ? ; ? "Checking for file changes and writing to the DBASE !"
      ?? App.Cargo:cFileDbf
      IF myFile2Compare(aFile)
         ?? "[need to update the table]"
         // обновить таблицу - файл изменён / update table - file changed
         DO EVENTS ; _wPost(10, oWnd, {nRecno, nRowPos}) ; wApi_Sleep(50)
      ENDIF

   ELSE
      cMsg := "Not file: " + cFileRun
      AlertStop( cMsg, App.ExeName)
      ? "Error ! " + cMsg
   ENDIF

   //!!! NO, we are in the STREAM _wSend(0, oWnd)  // LogScreen()
   DO EVENTS ; _wPost(0, oWnd) ; wApi_Sleep(50)    // так правильно

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myFindWndFromPid( hPID, cClass )              // search other than Explorer
   LOCAL aWin, hWnd, nTread, nProcID, cClassName, hRet := 0, cText := ""
   DEFAULT cClass := "Notepad"

   aWin := EnumWindows()
   ? SPACE(5) + ProcNL(), "aWin=", aWin, "hPID=", hPID
   // Author: Igor Nazarov
   For each hWnd In aWin
      cClassName := GetClassName( hWnd )
      If cClassName ==  cClass
         GetWindowThreadProcessID(hWnd, @nTread, @nProcID)
         If hPID == nProcID
            ? SPACE(5) + ProcNL(), "hWnd=", hWnd  , cClassName
            hRet  := hWnd
            cText := GetWindowText( hWnd )
         Endif
      Endif
   Next
   ?? "Return= " + HB_ValToExp( { hRet, cText } )

RETURN { hRet, cText }

////////////////////////////////////////////////////////////////////////////////
FUNCTION myFile2Compare(aFile)
   LOCAL lRet, cFile, tDtm, nRecno, tFile, cMsg, cFileDbf, cAlias, nFileSize
   LOCAL lModify

   ? Repl("-",LINE_FILL)
   ? ProcNL(), "(", aFile, ")", HB_ValToExp(aFile)

   lRet      := .F.
   lModify   := .F.
   cFileDbf  := App.Cargo:cFileDbf
   cAlias    := App.Cargo:cAlias
   nRecno    := aFile[1]            // the number in the array is the same as the record number
   cFile     := aFile[2] + aFile[3]
   tFile     := aFile[4]
   nFileSize := hb_fsize( cFile )
   hb_FGetDateTime( cFile, @tDtm )

   IF tDtm == tFile
      RETURN lRet  // the file has not changed
   ENDIF

   SELECT 0 // NEW Area
   // to re-open a previously opened dbf !!! DO NOT REMOVE!!!
   IF  Empty(cAlias)     ; cAlias := "_"+hb_ntos(Select())
   ELSEIF Select(cAlias) > 0 ; cAlias += "_"+hb_ntos(Select())
   ENDIF
   cAlias += "_"+hb_ntos(Select())
   cAlias := Upper(Alltrim(cAlias))
   ?? "cAlias=",cAlias

   BEGIN SEQUENCE WITH {|e| break( e ) }
      USE ( cFileDbf ) Alias &cAlias SHARED
      lRet := .T.
   RECOVER
      lRet := .F.
      cMsg := "Error Database !;" + cFileDbf + ";"
      cMsg += " busy with another process!" + CRLF
      cMsg += "WARNING! Changes will not be written to the database!" + CRLF
      AlertStop( cMsg, App.ExeName )
      ? cMsg
   END SEQUENCE

   If lRet
      DbGoto(nRecno)
      If (cAlias)->( RLock() )
         lModify      := .T.
         (cAlias)->F1 := "changed"
         (cAlias)->F3 := nFileSize
         (cAlias)->F4 := tDtm
         (cAlias)->( dbCommit() )
         (cAlias)->( dbUnLock() )
         ? "Success !; Recno: [" + HB_NtoS(nRecno) + "] changed !", nFileSize, tDtm
         ? "."
      Else
         cMsg := "Error !; Recno: [" + HB_NtoS(nRecno)
         cMsg += "] busy with another user !" + CRLF
         AlertStop( cMsg, App.ExeName )
         ? cMsg
      EndIf
      (cAlias)->( dbCloseArea() )
   EndIf

   IF !lModify
      lRet := .F. // record locked 
   ENDIF

RETURN lRet

////////////////////////////////////////////////////////////////////////////////
// Close the handle to open the process of program
FUNCTION CloseProcess()
   LOCAL nI, hPid, cWnd, hWnd, lRes, a3Prcs
   LOCAL aClose := (App.Cargo):aProcess

   ? Repl("-",LINE_FILL)
   ? ProcNL() , "(App.Cargo):aProcess=", aClose
   IF LEN( aClose ) > 0
      FOR nI := 1 TO LEN(aClose)
         a3Prcs := aClose[nI]
         IF LEN(a3Prcs) > 0
            hPid := a3Prcs[1]
            cWnd := a3Prcs[2]
            hWnd := a3Prcs[3]
            IF hPid > 0
               // returns a non-zero number if the process completed successfully and zero if an error occurred
               ? "   -->", nI, "Close hb_ProcessClose()=", hPid, cWnd, hWnd
               lRes := hb_ProcessClose( hPid )
               ?? "lRes= ", lRes
            ENDIF
         ENDIF
      NEXT
   ELSE
      ? "aClose=", aClose, "no programs running through - hb_processOpen(...)"
   ENDIF
   ?
RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myDebugVar(oWnd,ky,xv)  // variable output
   LOCAL cLog, cObj, cMsg, o, a, nI, a2Dim, nPost := ky

   // reduce the font in this function
   RELEASE FONT DlgFont 
   DO EVENTS
   DEFINE FONT DlgFont FONTNAME App.Cargo:cFont SIZE App.Cargo:cFSize - 3   // for HMG_Alert()

   cObj := UPPER(xv)
   IF cObj == "BUTTON_1"
      cLog := GetStartUpFolder() + "\_msg.App-Cargo.log"
      DELETEFILE(cLog)
      _SetGetLogFile( cLog )  // new log file output name

      cMsg := "  App.Cargo:"
      ? ProcNL(), "oWnd:Name =", oWnd:Name, oWnd,ky,xv
      ? " Объект App.Cargo доступен всегда в любом месте программы"
      ? " The App.Cargo object is always available anywhere in the program"
      o := App.Cargo

   ELSE
      cLog := GetStartUpFolder() + "\_msg.oWnd-Cargo.log"
      DELETEFILE(cLog)
      _SetGetLogFile( cLog )  // new log file output name

      cMsg := "  oWnd:Cargo:"
      ? ProcNL(), "oWnd:Name =", oWnd:Name, oWnd,ky,xv
      ? " Объект окна доступен через переменную oWnd в функции или через App.Cargo:oWindow"
      ? " В данном случае App.Cargo:oWindow это главное окно этой программы"
      ? " The window object is available through the oWnd variable in the function or through App.Cargo:oWindow"
      ? " In this case, App.Cargo:oWindow is the main window of this program"
      o := oWnd:Cargo           // we take data from the button that we put earlier

   ENDIF

   ? Repl("-",LINE_FILL)
   a2Dim  := o:GetAll()               // get an array with a key
   FOR EACH a IN a2Dim                // this is a list of ALL keys and their values
      nI := hb_EnumIndex( a )
      ? cMsg, nI, "Key =", a[1], "Val ="
      IF hb_IsArray(a[2])
         ?? SayHmgArray(a[2])
      ELSEIF hb_IsChar(a[2])
         ?? AtRepl( CRLF, a[2], " | " )
      ELSEIF hb_IsObject(a[2])
         ?? SayHmgObject(a[2])
      ELSE
         ?? a[2]
      ENDIF
   NEXT
   ? "..."

   cMsg := HB_MemoRead(cLog)
   AlertInfo(cMsg, "log file - " + cLog )

   _SetGetLogFile( App.Cargo:cLog ) // return the main output name of the log file

   // restore the font what it was
   RELEASE FONT DlgFont 
   DO EVENTS
   DEFINE FONT DlgFont FONTNAME App.Cargo:cFont SIZE App.Cargo:cFSize // for HMG_Alert()

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION SayHmgObject(ob)
   LOCAL cStr, aName

   IF !hb_IsObject(ob)
      RETURN " This is not an Object !"
   ENDIF

   cStr := "'OBJECT' " + ob:ClassName + " "
   IF ob:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA"                // контейнеры
      cStr += "ARRAY["  + hb_NToS( Len( ob:GetAll() ) ) + "] "
      cStr += HB_ValToExp( ob:GetAll() ) 
   ELSEIF ob:ClassName == "TSBROWSE"                             // таблица
      cStr += ob:cParentWnd + " " + ob:cControlName + " "
      cStr += ob:cAlias + " " + HB_NtoS( ob:nLen )
      aName := __objGetMethodList( ob )
   ELSEIF ob:ClassName $ "TCNLDATA,TGETDATA,TSTBDATA,TTSBDATA"  // контролы
      cStr += ob:Name + " " + ob:Type + " " + ob:Index
      cStr += " " + HB_NtoS( ob:Handle )
   ELSEIF ob:ClassName $ "TINIDATA"                             //  ini file
      cStr += ob:cIni + " " + cValToChar(ob:lIni) + " " 
      cStr += cValToChar( ob:lUtf8 )
   ELSEIF ob:ClassName $ "TWNDDATA"                             //  окно формы
      cStr += ob:Name + " " 
      cStr += "ARRAY["  + hb_NToS( Len( ob:GETLISTTYPE() ) ) + "] "
      cStr += HB_ValToExp( ob:GETLISTTYPE() )
      cStr += " " + HB_NtoS( ob:CLIENTWIDTH ) + " " + HB_NtoS( ob:CLIENTHEIGHT)
   ELSE
      aName := __objGetMethodList( ob )
      cStr += HB_ValToExp(aName)
   ENDIF

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////
FUNCTION SayHmgArray(a)
   LOCAL i, cStr := ""

   IF !hb_IsArray(a)
      RETURN " This is not an array !"
   ENDIF

   IF Len(a) > 0 .and. hb_IsArray(a[1])
      cStr += "{ "
      FOR i := 1 TO LEN(a)
         cStr += HB_ValToExp(a[i]) + IIF( i==LEN(a), "", "," )
      NEXT
      cStr += " }"
   ELSE
      cStr += HB_ValToExp(a)
   ENDIF
   cStr := "ARRAY["  + hb_NToS( Len(a) ) + "] = " + cStr

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myCreateFiles()
   LOCAL aFiles, cFile, cBuf, cRes, tDtm, nI, cAls
   LOCAL aList, cPath := GetStartUpFolder() + "\"

   aFiles := { "test1.log" , "test2.log" , "xtest1.lst", "xtest2.lst", "ztest1.txt", "ztest2.txt" }

   FOR nI := 1 TO LEN(aFiles)
      cFile := cPath + aFiles[nI]
      IF !FILE(cFile)
         cRes := CHARREM(".",aFiles[nI])
         cBuf := RCDataToMem( cRes )          // reading ini file from resources to buffer
         HB_MemoWrit( cFile, cBuf )
      ENDIF
   NEXT
   DO EVENTS
   wApi_Sleep(300)

   aFiles := DIRECTORY( cPath + "*.log" )
   aList  := Directory( cPath + "*.lst" )
   AMERGE( aFiles, aList )
   aList  := Directory( cPath + "*.txt" )
   AMERGE( aFiles, aList )

   ASORT(aFiles,,, { |x, y| x[1] < y[1] })

   aList := {}
   FOR nI := 1 TO LEN(aFiles)
      cFile := aFiles[nI,1]
      hb_FGetDateTime( cFile, @tDtm )
      AADD( aList, { aFiles[nI,1], aFiles[nI,2], tDtm, cPath } )
   NEXT

   cAls := UseOpenBase(aList)

RETURN cAls

/////////////////////////////////////////////////////////////////////
FUNCTION UseOpenBase(aList)
   LOCAL i, aStr, cAls, cDbf

   cDbf := (App.Cargo):cFileDbf
   cAls := (App.Cargo):cAlias

   IF ! File( cDbf + '.dbf' )

      aStr := {}
      AAdd( aStr, { 'F1' , 'C',  10, 0 } )
      AAdd( aStr, { 'F2' , 'C',  30, 0 } )
      AAdd( aStr, { 'F3' , 'N',  10, 0 } )
      AAdd( aStr, { 'F4' , 'T',  8, 0 } )
      AAdd( aStr, { 'F5' , 'C', 120, 0 } )
      AAdd( aStr, { 'F6' , 'C', 120, 0 } )
      dbCreate( cDbf, aStr )
      USE ( cDbf ) ALIAS TEST EXCLUSIVE NEW

      FOR i := 1 TO LEN(aList)
         TEST->( dbAppend() )
         TEST->F1 := ''
         TEST->F2 := '-> ' + cFileNoPath( aList[i,1] )
         TEST->F3 := aList[i,2]
         TEST->F4 := aList[i,3]  // hb_FGetDateTime()
         TEST->F5 := aList[i,1]
         TEST->F6 := aList[i,4]
      NEXT

      USE

   ENDIF

   USE ( cDbf ) ALIAS (cAls) SHARED NEW
   GO TOP

   cAls := ALIAS()

RETURN cAls

///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN " Call from: " + ProcName( nVal+1 ) + ;
        "(" + hb_ntos( ProcLine( nVal+1 ) ) + ") -> " + ProcFile( nVal+1 )
