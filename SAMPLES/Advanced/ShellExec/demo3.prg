/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Работа с классом TThrData (доступ в потоках к переменным идет с блокировками)
 * Запуск программ через hb_ProcessOpen() в отдельном потоке
 * Проверка на вторую копию запуска процесса
 * Закрытие процесса запущенного из своей программы
 * Working with the TThrData class (access to variables in threads comes with locks)
 * Running programs via hb_ProcessOpen() in a separate thread 
 * Check for a second copy of the startup process
 * Closing the process running from your program
*/

ANNOUNCE RDDSYS

#define _HMG_OUTLOG
#define PRG_NAME     "hb_ProcessOpen() + Working with the TThrData class"
#define PRG_BOTTOM   " -> hProcessOpenHandle = "
#define PRG_NOTEPAD  GetStartUpFolder() + "\demo3.prg"
#define EXE_NOTEPAD  "notepad.exe"

#include "minigui.ch"
#include "hbthread.ch"

MEMVAR o_Thr

////////////////////////////////////////////////////////////////////////////////
INIT PROCEDURE My_Sets_Env
   LOCAL cFont := "DejaVu Sans Mono", nSize := 14
   LOCAL cIcon := "1MAIN_ICO"
   LOCAL aBClr := { 138, 249, 142 }
   LOCAL cLog  := "_msg.log"

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN
   SET OOP ON
   SET DATE  TO GERMAN
   SET EPOCH TO ( Year(Date()) - 50 )
   SET FONT TO cFont, nSize
   SET DEFAULT ICON TO cIcon
   SET NAVIGATION EXTENDED
   SET MULTIPLE OFF WARNING
   DEFINE FONT DlgFont FONTNAME cFont SIZE nSize  // for HMG_Alert()
   SET MSGALERT BACKCOLOR TO aBClr                // for HMG_Alert()

   _SetGetLogFile( cLog ) ; fErase( cLog )
   ? ProcName() + "(" + HB_NtoS(ProcLine()) + ") " + REPL("=",20) + " Start" , HB_DATETIME()

   App.Cargo := oHmgData()                   // создадим контейнер - доступ везде в программе
   App.Cargo:cLog := cLog                    // create a container - access everywhere in the program

   PUBLIC o_Thr ; o_Thr := TThrData():New()  // создадим контейнер для работы с потоками
                                             // create a container for working with streams
   o_Thr:hStaticPid := 0
   o_Thr:hProcessOpenHandle := 0
   o_Thr:cMsg  := ""
   o_Thr:aBClr := aBClr

RETURN

////////////////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL nW := 540, nH := 400, aBackcolor := o_Thr:aBClr
   LOCAL nWBtn, nHBtn, nG, nRow, cMsg, nHLbl 

   IF ! o_Thr:MT         // hb_mtvm()
      MsgStop("No multithreading support !" + CRLF + ;
              "Program compilation key -mt !" + CRLF + ;
              App.ExeName  )
      QUIT
   ENDIF
   ? ProcName() + "(" + HB_NtoS(ProcLine()) + ") " + REPL("=",20) 

   DEFINE WINDOW wMain AT 250,nH WIDTH nW HEIGHT nH ;
      TITLE PRG_NAME               ;
      MAIN NOMAXIMIZE NOSIZE       ;
      BACKCOLOR  aBackcolor        ;
      FONT "Tahoma" SIZE 14        ;
      ON INIT {|| _wPost(0) }      ;   // вывод текста в LABEL из контейнера / displaying text in LABEL from container
      ON INTERACTIVECLOSE {|lRet| lRet := myQuit(), This.Label_0.Setfocus } ; // NO exit while there is a window ShellExecuteEx()
      ON RELEASE {|| 
                  o_Thr:oWindow := Nil
                  o_Thr:hWindow := 0
                  ? "o_Thr =", o_Thr:GetAll()
                  ?v o_Thr:GetAll()
                  ?
                  ? REPL("=",20) + " End", HB_DATETIME()
                  DoEvents()      
                  Return Nil
                 }  // executed before destroying the window

      // в контейнер для работы в thread-ах и основного процесса запоминаем
      // into the container for work in threads and the main process, remember
      o_Thr:oWindow := This.Object   // объект окна это как в событии {|ow|...}
      o_Thr:cWindow := This.Name     // имя окна, просто для примера
      o_Thr:hWindow := This.Handle   // handle окна, просто для примера

      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nRow  := nG := 20
      nWBtn := nW - nG * 2
      nHLbl := nG*3
      nHBtn := ( nH - nG * 4 - nHLbl ) / 3

      @ nRow, nG BUTTONEX Button_1 WIDTH nWBtn HEIGHT nHBtn ;
      CAPTION 'hb_ProcessOpen()' ACTION StartFileStream()
      nRow += nHBtn + nG

      @ nRow, nG BUTTONEX Button_2 WIDTH nWBtn HEIGHT nHBtn ;
      CAPTION 'hb_ProcessClose()' ACTION CloseProcess()
      nRow += nHBtn + nG

      @ nRow, nG BUTTONEX Button_Exit WIDTH nWBtn HEIGHT nHBtn ;
      CAPTION 'EXIT' ACTION IIF( myQuit() , ThisWindow.Release , This.Label_0.Setfocus )
      nRow += nHBtn + nG*2

      // пишем в основном потоке в контейнер текст из cMsg
      // write in the main thread to the container text from cMsg
      cMsg := PRG_BOTTOM + HB_NtoS(o_Thr:hProcessOpenHandle)
      o_Thr:cMsg := cMsg
      // делаем лабел с данными " " / make a label with data " "
      @ nRow, 0 LABEL Label_0 WIDTH nW HEIGHT nG*2 VALUE " " TRANSPARENT VCENTERALIGN

      // ставим событие заполнения значением лабел из переменной контейнер oTrhData()
      (This.Object):Event( 0, {|  | This.Label_0.Value := o_Thr:cMsg, DoEvents() })
      (This.Object):Event(99, {|ow| ow:Release() })

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN
///////////////////////////////////////////////////////////////////////////////
FUNCTION myQuit()    // NO exit while there is a window ShellExecuteEx()
   LOCAL cMsg, nWin, lRet, lQuit := .T.

   nWin := o_Thr:hProcessOpenHandle
   cMsg := "You need to close the program window!" + CRLF
   cMsg += EXE_NOTEPAD + " " + PRG_NOTEPAD + CRLF + CRLF
   cMsg += "hProcessOpenHandle > 0"

   IF lQuit
      lRet := IIF( nWin == 0, .T., ( MsgExclamation(cMsg, "Attention"), .F. ) )
   ELSE
      lRet := .T.  // always exit is for debugging
   ENDIF

RETURN lRet

////////////////////////////////////////////////////////////////////////////////

FUNCTION StartFileStream()
   // Start in a separate thread
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @RunInternal(), Nil ) )
RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION My_Label_0( cMsg )
   DEFAULT cMsg := ""

   // пишем значение cMsg в контейнер, для использования в основном процессе
   // write the cMsg value to the container, for use in the main process
   o_Thr:cMsg := cMsg
   // посылаем сообщение 0 окну o_Thr:oWindow - это wMain окно
   // используем такой синтаксис (он внутри _wPost(), _wSend() ф-ий), т.к.
   // эти ф-ии обращаются через имя формы для получения объекта окна и потом
   // посылают окну сообщение, для вып. события. Тут уже объект окна есть
   // send message 0 to the window o_Thr:oWindow is the wMain window
   // use this syntax (it's inside _wPost(), _wSend() f-th), because
   // these functions are accessed via the form name to get the window object and then
   // send a message to the window, for issue. events. There is already a window object
   o_Thr:oWindow:PostMsg(0)
   DO EVENTS
   // ждем прорисовку лабел на окне, но это не обязятельно тут, но где то понадобится
   // waiting for the label to be drawn on the window, but this is not necessary here, but it will be needed somewhere
   wApi_Sleep(50)  

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
FUNCTION RunInternal()
   LOCAL cFileRun, nWaitCode, cMsg, hProcess, hWnd

   cFileRun := PRG_NOTEPAD
   IF FILE( cFileRun )
      IF o_Thr:hProcessOpenHandle == 0
         o_Thr:hProcessOpenHandle := hb_processOpen( EXE_NOTEPAD + " " + cFileRun,,,,.F., @hProcess)
         cMsg := PRG_BOTTOM + HB_NtoS(o_Thr:hProcessOpenHandle)
         ? "Run ->", cFileRun, cMsg, "hProcess=", hProcess
         o_Thr:hStaticPid := hProcess
         // пишем cMsg в контейнер и посылаем окну сообщение сообщение для показа Label
         // write cMsg to the container and send the window a message to show the Label
         My_Label_0( cMsg )
         //wMain.Label_0.Value := cMsg  // в потоке так делать НЕЛЬЗЯ ! / Do NOT do this in a stream!

         // Waits until the specified object is in the signaled state or the time-out interval elapses
         nWaitCode := WaitForSingleObject(o_Thr:hProcessOpenHandle) //, 2000) // ms. waiting time
         // nWaitCode = 258 - timeout process not completed
         // nWaitCode =  -1 - incorrect process
         // nWaitCode = 0 - process completed
         cMsg := cFileRun + CRLF
         cMsg += "STOP hb_ProcessOpen() ! RunFileInternal: " + ALLTRIM(STR(nWaitCode))
         IF nWaitCode == 0
            cMsg += CRLF + "Process completed !"
            MsgInfo(cMsg)
         ELSE
            cMsg += CRLF + "Process termination error !"
            MsgStop(cMsg)
         ENDIF
         // end process, close window
         o_Thr:hProcessOpenHandle := 0  // STOP ShellExecuteEx() - NECESSARILY !!!
         cMsg := PRG_BOTTOM + HB_NtoS(o_Thr:hProcessOpenHandle)
         My_Label_0( cMsg )
         //wMain.Label_0.Value := cMsg  // в потоке так делать НЕЛЬЗЯ ! / Do NOT do this in a stream!
      ELSE
         cMsg := PRG_BOTTOM + HB_NtoS(o_Thr:hProcessOpenHandle)
         ? "The program is already running ...", cMsg, "hStaticPid=",o_Thr:hStaticPid
         hWnd := myFindWndFromPid( o_Thr:hStaticPid )
         IF hWnd > 0
            IF IsIconic( hWnd )
               _Restore( hWnd )
            ENDIF
            SetForegroundWindow(hWnd)
            DO EVENTS
         ENDIF
      ENDIF
   ELSE
      MsgStop("Not file: " + cFileRun)
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
// Close the handle to open the process of program
FUNCTION CloseProcess()
   LOCAL lResult, cMsg

   IF o_Thr:hProcessOpenHandle > 0
      lResult := hb_ProcessClose( o_Thr:hProcessOpenHandle )
      cMsg  := EXE_NOTEPAD + " " + PRG_NOTEPAD + CRLF
      cMsg  += "Close hb_ProcessOpen(...) = " + cValToChar(lResult) + " !" + CRLF
      MsgInfo(cMsg)
      ? cMsg, "lResult=",lResult, VALTYPE(lResult)
      o_Thr:hProcessOpenHandle := 0
      o_Thr:hStaticPid := 0
   ELSE
      cMsg := "Process not started by hb_ProcessOpen(...) !"
      MsgStop(cMsg)
      ? cMsg
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myFindWndFromPid( hPID )              // search other than Explorer
   LOCAL aWin, hWnd, nTread, nProcID, cClassName, hRet := 0

   aWin := EnumWindows()
   ? SPACE(5) + ProcName() + "()", "aWin=", aWin, "hPID=", hPID
   For each hWnd In aWin
      cClassName := GetClassName( hWnd )
      If cClassName ==  "Notepad"
         GetWindowThreadProcessID(hWnd, @nTread, @nProcID)
         If hPID == nProcID
            ? SPACE(5) + ProcName() + "()", "hWnd=", hWnd  , GetProcessName(hPID), cClassName
            hRet := hWnd
         Endif
      Endif
   Next

RETURN hRet
