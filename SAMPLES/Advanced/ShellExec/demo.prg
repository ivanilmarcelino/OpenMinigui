/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * ShellExecuteEx() and TerminateProcess() and WaitForSingleObject() for MiniGui
 * (c) 2014-2023 Artyom Verchenko <artyomskynet@gmail.com>
 *
 * Run the program via ShellExecuteEx()
 * Check for a second copy of the startup process
 * Watchman of a running program - control of the process launched from its program
*/

ANNOUNCE RDDSYS

#define _HMG_OUTLOG
#define PRG_NAME      "Launching external programs ShellExecuteEx()" 
#define PRG_BOTTOM    " -> hProcessHandleShellExecuteEx = " 
#define HELP_MINIGUI  GetEnv( "MG_ROOT" ) + "\Doc\MiniGUI.chm"  

#include "minigui.ch"

STATIC hProcessHandleShellExecuteEx := 0
////////////////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL nW := 540, nH := 400, aBackcolor := { 0,64,80 }
   LOCAL nWBtn, nHBtn, nG, nRow, cMsg, nHLbl, lQ

   SET DEFAULT ICON TO "1MAIN_ICO"

   IF !hb_mtvm()
      MsgStop("No multithreading support !" + CRLF + ;
              "Program compilation key -mt !" + CRLF + ;
              App.ExeName    )
      QUIT
   ENDIF
   ? ; ? REPL("=",20) + " Start" , HB_DATETIME()

   DEFINE WINDOW wMain            ;
      AT 50,50 WIDTH nW HEIGHT nH ;
      TITLE PRG_NAME              ;
      MAIN                        ;
      NOMAXIMIZE NOSIZE           ; 
      BACKCOLOR  aBackcolor       ;
      FONT "Tahoma" SIZE 14       ;
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
      CAPTION 'ShellExecuteEx()' ACTION RunFileStream() 
      nRow += nHBtn + nG 

      @ nRow, nG BUTTONEX Button_2 WIDTH nWBtn HEIGHT nHBtn ;
      CAPTION 'TerminateProcess()' ACTION CloseProcess() 
      nRow += nHBtn + nG 

      @ nRow, nG BUTTONEX Button_Exit WIDTH nWBtn HEIGHT nHBtn ;
      CAPTION 'EXIT' ACTION IIF( lQ := myQuit() , ThisWindow.Release , wMain.Label_0.Setfocus )
      nRow += nHBtn + nG*2

      cMsg := PRG_BOTTOM + HB_NtoS(hProcessHandleShellExecuteEx)
      @ nRow, 0 LABEL Label_0 WIDTH nW HEIGHT nG*2 VALUE cMsg FONTCOLOR WHITE TRANSPARENT VCENTERALIGN

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN
///////////////////////////////////////////////////////////////////////////////
FUNCTION myQuit()        // NO exit while there is a window ShellExecuteEx()
   LOCAL cMsg, nWin, lRet, lQuit := .T.

   nWin := hProcessHandleShellExecuteEx
   cMsg := "You need to close the program window!" + CRLF
   cMsg += HELP_MINIGUI + CRLF + CRLF
   cMsg += "hProcessHandleShellExecuteEx > 0"

   IF lQuit
      lRet := IIF( nWin == 0, .T., ( MsgExclamation(cMsg, "Attention"), .F. ) )
   ELSE
      lRet := .T.  // always exit is for debugging
   ENDIF

RETURN lRet

////////////////////////////////////////////////////////////////////////////////
#include "hbthread.ch"

FUNCTION RunFileStream()        
   // Start in a separate thread 
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @RunFileInternal(), Nil ) )
RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION RunFileInternal()
      LOCAL cFileRun, nWaitCode, cMsg

      cFileRun := HELP_MINIGUI         // where is the MiniGui
      IF FILE( cFileRun ) 
         IF hProcessHandleShellExecuteEx == 0
            // handle process (launched HELP MiniGUI.chm)
            // It is very convenient to get handle its public document .doc or .xls
            // Then to close it independently from the program. 
            hProcessHandleShellExecuteEx := ShellExecuteEx( , 'open', cFileRun, , , SW_SHOWNORMAL)
            cMsg := PRG_BOTTOM + HB_NtoS(hProcessHandleShellExecuteEx)
            ? "Run ->", cFileRun, cMsg
            wMain.Label_0.Value := cMsg

            // Watchman of a running program - control of the process launched from its program
            // Waits until the specified object is in the signaled state or the time-out interval elapses
            nWaitCode := WaitForSingleObject(hProcessHandleShellExecuteEx) //, 2000) // ms. waiting time
            // nWaitCode = 258 - timeout process not completed
            // nWaitCode =  -1 - incorrect process
            // nWaitCode =   0 - process completed
            cMsg := cFileRun + CRLF
            cMsg += "STOP ShellExecuteEx() ! RunFileInternal: " + ALLTRIM(STR(nWaitCode))
            IF nWaitCode == 0
               cMsg += CRLF + "Process completed !"
               MsgInfo(cMsg) 
            ELSE
               cMsg += CRLF + "Process termination error !"
               MsgStop(cMsg)
            ENDIF
            // end process, close window
            hProcessHandleShellExecuteEx := 0  // STOP ShellExecuteEx() - NECESSARILY !!!
            cMsg := PRG_BOTTOM + HB_NtoS(hProcessHandleShellExecuteEx)
            wMain.Label_0.Value := cMsg
         ELSE
            cMsg := PRG_BOTTOM + HB_NtoS(hProcessHandleShellExecuteEx)
            ? "The program is already running ...", cMsg
         ENDIF
      ELSE
         MsgStop("Not file: " + cFileRun)
      ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
// Close the handle to open the process of program
FUNCTION CloseProcess() 
   LOCAL nRet, cMsg

   IF hProcessHandleShellExecuteEx > 0
      nRet := TerminateProcess( hProcessHandleShellExecuteEx )
      cMsg := "CloseProcess ShellExecuteEx(...): " + HB_NtoS(nRet) + CRLF
      cMsg += "Returns a non-zero number if the process completed successfully and zero if an error occurred"
      MsgInfo(cMsg)
      ? cMsg 
   ELSE
      cMsg := "Process not started by ShellExecuteEx(...) !" 
      MsgStop(cMsg)
      ? cMsg 
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
// (c) 2023 Artyom Verchenko <artyomskynet@gmail.com>
#pragma BEGINDUMP

#include <windows.h>
#include <hbapi.h>
#include <shlobj.h>

HB_FUNC( SHELLEXECUTEEX )
{
   SHELLEXECUTEINFO  SHExecInfo;
   ZeroMemory( &SHExecInfo, sizeof( SHExecInfo ) );

   SHExecInfo.cbSize = sizeof( SHExecInfo );
   SHExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;
   SHExecInfo.lpVerb = ( LPCSTR ) hb_parcx( 2 );
   SHExecInfo.lpFile = ( LPCSTR ) hb_parcx( 3 );
   SHExecInfo.lpParameters = ( LPCSTR ) hb_parcx( 4 );
   SHExecInfo.lpDirectory = ( LPCSTR ) hb_parcx( 5 );
   SHExecInfo.nShow = hb_parni( 6 );

   if( !ShellExecuteEx( &SHExecInfo ) )
   {
      hb_retnl( NULL );
   }
   else
   {
      hb_retnl( ( LONG ) SHExecInfo.hProcess );
   }
}

HB_FUNC( TERMINATEPROCESS )
{
   hb_retnl( ( BOOL ) TerminateProcess( ( HANDLE ) hb_parni( 1 ), 0 ) );
}

// Waits until the specified object is in the signaled state or the time-out interval elapses
HB_FUNC( WAITFORSINGLEOBJECT )
{
   DWORD waitTime = hb_parnl( 2 );
   if( waitTime == 0 )
   {
      waitTime = INFINITE;
   }

   hb_retnl( WaitForSingleObject( ( HANDLE ) hb_parnl( 1 ), waitTime ) );
}

#pragma ENDDUMP
