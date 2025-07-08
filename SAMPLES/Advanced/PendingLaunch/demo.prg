/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Запуск второй копии программы с ожиданием с помощью мьютекса
 * Launching a second copy of a program with a wait using a mutex
 */

//#define _HMG_OUTLOG
#define SHOW_TITLE  "My test program"

#include "hmg.ch"

PROCEDURE MAIN(cParam)
   LOCAL nY, nX, nW, nH, cMsg:= ""
   DEFAULT cParam := ""

   SET MSGALERT BACKCOLOR TO { 255, 205, 216 }               // for HMG_Alert()
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()
   SET FONT TO "DejaVu Sans Mono", 12

   IF cParam == "RESTART"
      cMsg := 'Starting a second copy of the program'
      SET WINDOW MAIN OFF
      WaitWindow( cMsg, .T. )
      // Проверка на окончание первой копии программы по мютексу
      WHILE IsExe2Run()
         wApi_Sleep(500)
      END
      WaitWindow()    // close the wait window
      SET WINDOW MAIN ON
   ELSE
      IF IsExe2Run()   // Проверка второго запуска программы
         cMsg += "Trying to start a second copy of the program!;"
         cMsg += App.ExeName + ";Launch denied."
         AlertStop(cMsg, "ERROR")
         RETURN
      ENDIF
   ENDIF

   ? "..." ; ? "Program is running [" + SHOW_TITLE + "]  " + TIME() + " Mutex = " + IsExe2Run(.F.)

   nY := nX := 0; nW := System.ClientWidth ; nH := 150

   DEFINE WINDOW test                           ;
      AT nY, nX WIDTH nW HEIGHT nH              ;
      TITLE SHOW_TITLE                          ;
      ICON "1MAIN_ICO"                          ;
      MAIN NOMAXIMIZE NOSIZE                    ;
      BACKCOLOR { 255, 205, 216 }               ;
      ON RELEASE {|| myExitPrg() }              ;
      ON INTERACTIVECLOSE {|| myExitPrg("[x]")  }

      nW := This.ClientWidth
      nH := This.ClientHeight - GetTitleHeight()

      DEFINE MAIN MENU
         POPUP "Run"
            ITEM "Running a copy of the program"  ACTION myRestart()
            SEPARATOR
            ITEM "Exit"                           ACTION ThisWindow.Release
         END POPUP
         POPUP "About"
            ITEM "Info"                           ACTION MsgAbout()
         END POPUP
      END MENU

      @ 0, 0 LABEL Label_1 VALUE "" WIDTH nW HEIGHT nH SIZE 22 TRANSPARENT CENTERALIGN VCENTERALIGN

      This.Minimize ;  This.Restore ; DO EVENTS

   END WINDOW

   test.Activate

RETURN

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myExitPrg(cVal)
   LOCAL nI
   DEFAULT cVal := ""

   ? "..." ; ? "Program is exit [" + SHOW_TITLE + "]  " + cVal + "   " + TIME()

   DbCloseAll()

   IF ! ThisWindow.Closable
      For nI := 20 to 1 Step -1
          This.Label_1.Value := "Closing a program through " + HB_NtoS(nI)
          wApi_Sleep(200)
      Next
   ENDIF

RETURN .T.

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myRestart()

   ThisWindow.Closable := .F.    // по [X] закрыть окно блокируем и потом анализируем

   WaitWindow( 'Restart the program ...', .T. )
   wApi_Sleep(500)

   ? "  ."
   ? "  ShellExecute( , 'open', '" + App.ExeName + "' , RESTART , 2 )"
   ? "  ."
   ShellExecute( , 'open', App.ExeName, "RESTART", , 2 )

   ReleaseAllWindows()  // закрыть программу

RETURN NIL

//////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Starting a second copy of the program with the wait, use the mutex;;"
   cMsg += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";;"
   cMsg += PadC( "This program is Freeware!", 70 ) + ";"
   cMsg += PadC( "Copying is allowed!", 70 )
   AlertInfo(cMsg,"About this demo")

RETURN NIL

//////////////////////////////////////////////////////////////////
// Проверка второго запуска программы + (режим или ini)
FUNCTION IsExe2Run( cDop )
   LOCAL i, cMut, hMut, lMut
   LOCAL cExe := hb_ProgName()
   LOCAL lRet := .T.
   STATIC s_hMutex := 0, s_cMutex := ""
   DEFAULT cDop := ""

   IF ISLOGICAL(cDop)
      IF cDop                              // release mutex
         IF ! empty(s_hMutex)
            wapi_ReleaseMutex( s_hMutex )
            s_hMutex := 0
            s_cMutex := ""
         ENDIF
         RETURN lRet
      ELSE                                 // get name mutex
         RETURN s_cMutex
      ENDIF
   ENDIF

   IF ! empty(cDop) .and. ( i := RAt("\", cDop) ) > 0
      cDop := subs(cDop, i+1)
   ENDIF

   cMut := upper(cExe + cDop)

   AEval( {".","\",":","/"," "}, {|cs| cMut := StrTran(cMut, cs, "_") } )

   hMut := wapi_CreateMutex( NIL, NIL, cMut )
   lMut := ( ! Empty( hMut ) .and. wapi_GetLastError() == 0 )

   IF lMut
      s_hMutex := hMut
      s_cMutex := cMut
      lRet := .F.
   ENDIF

RETURN lRet     // .T. - повторный запуск (mutex уже есть)
