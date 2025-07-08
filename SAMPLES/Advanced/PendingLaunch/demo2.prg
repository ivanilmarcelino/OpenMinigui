/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Запуск второй копии программы с ожиданием.
 * Launching a second copy of the program pending.
 */

//#define _HMG_OUTLOG
#define SHOW_TITLE  "My test program (2)"

#include "hmg.ch"

PROCEDURE MAIN(cParam)
   LOCAL nY, nX, nW, nH, cMsg
   DEFAULT cParam := ""

   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }               // for HMG_Alert()
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()
   SET FONT TO "DejaVu Sans Mono", 12

   IF cParam == "RESTART"
      cMsg := SHOW_TITLE +": Starting a second copy of the program"
      ? cMsg
      SET WINDOW MAIN OFF
      WaitWindow( cMsg, .T. )
      nY := 0
      DO WHILE myIsProgaInMemory(SHOW_TITLE,"HMG_", .T.)
         ? "." ; ? "-------- Is there a program in memory ["+SHOW_TITLE+",HMG_*] ? ------ For:",nY++
         wApi_Sleep(500)
      ENDDO
      WaitWindow()    // close the wait window
      SET WINDOW MAIN ON
   ENDIF

   // Проверка на запуск второй копии программы
   // Check to run a second copy of the program
   OnlyOneInstance( SHOW_TITLE, .T. )

   nY := nX := 0; nW := System.ClientWidth ; nH := 150

   DEFINE WINDOW test                           ;
      AT 155, nX WIDTH nW HEIGHT nH             ;
      TITLE SHOW_TITLE                          ;
      ICON "1MAIN_ICO"                          ;
      MAIN NOMAXIMIZE NOSIZE                    ;
      BACKCOLOR { 238, 249, 142 }               ;
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

   ? "  ."
   ? "  ShellExecute( , 'open', '" + Application.ExeName + "' , RESTART , 2 )"
   ? "  ."
   ShellExecute( , 'open', Application.ExeName, "RESTART", , 2 )

   ReleaseAllWindows()  // закрыть программу

RETURN NIL

//////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Launching a second copy of the program pending;;"
   cMsg += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";;"
   cMsg += PadC( "This program is Freeware!", 70 ) + ";"
   cMsg += PadC( "Copying is allowed!", 70 )
   AlertInfo(cMsg,"About this demo")

RETURN NIL

//////////////////////////////////////////////////////////////////
// Использование EnumWindows - список окон программ в памяти
// Using EnumWindows - a list of program windows in memory
FUNCTION myIsProgaInMemory( cAppTitle, cMskClass, lLogOut)
LOCAL nI, cI, lRet := .F., ahWnd := EnumWindows()

   cAppTitle := UPPER(cAppTitle)

   IF ! Empty(lLogOut)
      FOR nI := 1 TO Len(ahWnd)
          cI := ""
          BEGIN SEQUENCE WITH {|e| break( e ) }
             cI := GetClassName(ahWnd[nI])
          END SEQUENCE
          IF empty(cI) ; LOOP
          ENDIF
          IF empty(cMskClass) .or. cMskClass $ cI
             ? nI, '.', ahWnd[nI], cI, GetWindowText(ahWnd[nI])
          ENDIF
      NEXT
      ?
   ENDIF

   FOR nI := 1 TO Len(ahWnd)
       // проверка запуска программы ТОЛЬКО для МиниГуи
       // check the launch of the program ONLY for MiniGui
       cI := ""
       BEGIN SEQUENCE WITH {|e| break( e ) }
          cI := GetClassName(ahWnd[nI])
       END SEQUENCE
       IF empty(cI) ; LOOP
       ENDIF
       IF cMskClass $ cI .AND. UPPER(GetWindowText(ahWnd[nI])) == cAppTitle
          lRet := .T.
          EXIT
       ENDIF
   NEXT

RETURN lRet

//////////////////////////////////////////////////////////////////
// Проверка запуска программы на ВТОРУЮ копию программы
// Check the start of the program on the second copy of the program
Function OnlyOneInstance( cAppTitle, lLogOut )
Local hWnd := FindWindowEx( ,,, cAppTitle )

if hWnd # 0
   iif( IsIconic( hWnd ), _Restore( hWnd ), SetForeGroundWindow( hWnd ) )
   if ! Empty(lLogOut)
       ? PROCNAME(0), cAppTitle, hWnd, "ExitProcess( 0 ) -> Program is already running"
   endif
   ExitProcess( 0 )
else
   if ! Empty(lLogOut)
      ? "..." ; ? "Program is running [" + SHOW_TITLE + "]  " + TIME() + "  ProcLine()=",ProcLine()
   endif
endif

Return NIL
