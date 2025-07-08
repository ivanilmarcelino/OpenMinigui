/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * WaitForSingleObject() for MiniGui
 *
 * Запуск программы через hb_ProcessOpen()
 * Проверка на вторую копию запуска процесса
 * Сторож запущенной программы - контроль процесса запущенного из своей программы 
 * Run the program via hb_ProcessOpen()
 * Check for a second copy of the startup process
 * Watchman of a running program - control of the process launched from its program
*/

ANNOUNCE RDDSYS

#define _HMG_OUTLOG
#define PRG_NAME      "Launching external programs hb_ProcessOpen()" 
#define PRG_BOTTOM    " -> hProcessOpenHandle = " 
#define PRG_NOTEPAD   GetStartUpFolder() + "\demo2.prg"  
#define EXE_NOTEPAD   "notepad.exe"  

#include "minigui.ch"
#include "hbthread.ch"

STATIC hStaticPid := 0, hProcessOpenHandle := 0
////////////////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL nW := 540, nH := 400, aBackcolor := {6,175,143}
   LOCAL nWBtn, nHBtn, nG, nRow, cMsg, nHLbl, lQ

   SET DEFAULT ICON TO "1MAIN_ICO"

   IF !hb_mtvm()
      MsgStop("No multithreading support !" + CRLF + ;
              "Program compilation key -mt !" + CRLF + ;
              App.ExeName    )
      QUIT
   ENDIF
   ? ; ? REPL("=",20) + " Start" , HB_DATETIME()

   DEFINE WINDOW wMain             ;
      AT 150,nW WIDTH nW HEIGHT nH ;
      TITLE PRG_NAME               ;
      MAIN                         ;
      NOMAXIMIZE NOSIZE            ; 
      BACKCOLOR  aBackcolor        ;
      FONT "Tahoma" SIZE 14        ;
      ON RELEASE {|| _LogFile(.T., REPL("=",20) + " End", ;
                     HB_DATETIME() ), DoEvents()      }   ;  // executed before destroying the window
      ON INTERACTIVECLOSE {|lRet| lRet := myQuit() }         // NO exit while there is a window ShellExecuteEx()

      nW    := This.ClientWidth
      nH    := This.ClientHeight
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
      CAPTION 'EXIT' ACTION IIF( lQ := myQuit() , ThisWindow.Release , wMain.Label_0.Setfocus )
      nRow += nHBtn + nG*2

      cMsg := PRG_BOTTOM + HB_NtoS(hProcessOpenHandle)
      @ nRow, 0 LABEL Label_0 WIDTH nW HEIGHT nG*2 VALUE cMsg TRANSPARENT VCENTERALIGN

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN
///////////////////////////////////////////////////////////////////////////////
FUNCTION myQuit()        // NO exit while there is a window ShellExecuteEx()
   LOCAL cMsg, nWin, lRet, lQuit := .T.

   nWin := hProcessOpenHandle
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
FUNCTION RunInternal()
   LOCAL cFileRun, nWaitCode, cMsg, hProcess, hWnd

   cFileRun := PRG_NOTEPAD       
   IF FILE( cFileRun ) 
      IF hProcessOpenHandle == 0
         hProcessOpenHandle := hb_processOpen( EXE_NOTEPAD + " " + cFileRun,,,,.F., @hProcess) 
         cMsg := PRG_BOTTOM + HB_NtoS(hProcessOpenHandle)
         ? "Run ->", cFileRun, cMsg, "hProcess=", hProcess
         hStaticPid := hProcess
         wMain.Label_0.Value := cMsg
         DO EVENTS ; wApi_Sleep(50)                // waiting for drawing 

         // Watchman of a running program - control of the process launched from its program
         // Waits until the specified object is in the signaled state or the time-out interval elapses
         nWaitCode := WaitForSingleObject(hProcessOpenHandle) //, 2000) // ms. waiting time
         // nWaitCode = 258 - timeout process not completed
         // nWaitCode =  -1 - incorrect process
         // nWaitCode =   0 - process completed
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
         hProcessOpenHandle := 0  // STOP ShellExecuteEx() - NECESSARILY !!!
         cMsg := PRG_BOTTOM + HB_NtoS(hProcessOpenHandle)
         wMain.Label_0.Value := cMsg
      ELSE
         cMsg := PRG_BOTTOM + HB_NtoS(hProcessOpenHandle)
         ? "The program is already running ...", cMsg, "hStaticPid=",hStaticPid
         hWnd := myFindWndFromPid( hStaticPid )
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

   IF hProcessOpenHandle > 0
      lResult := hb_ProcessClose( hProcessOpenHandle )
      cMsg    := EXE_NOTEPAD + " " + PRG_NOTEPAD + CRLF 
      cMsg    += "Close hb_ProcessOpen(...) = " + cValToChar(lResult) + " !" + CRLF
      MsgInfo(cMsg)
      ? cMsg, "lResult=",lResult, VALTYPE(lResult)
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
   // Author: Igor Nazarov
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
