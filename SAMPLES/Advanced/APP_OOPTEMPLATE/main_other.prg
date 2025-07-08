/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Разные доп.функции для отдельного запуска
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "metrocolor.ch"
#include "i_winuser.ch"
#include "i_ExtEvents.ch"
//////////////////////////////////////////////////////////////////////
FUNCTION wMain_1()   // nMode == 1
   LOCAL nI, aMsg, cAppTtl, cFileExe

   cFileExe := App.ExeName
   cAppTtl  := App.Cargo:cAppTitle
   aMsg     := {'Перезапуск программы ...','Подождите немного ....',;
                'Restarting the program ...', 'Wait a moment ....'}
   ? ProcNL()
   SET WINDOW MAIN OFF
   WaitWindow( aMsg, .T. , 600, 18, NIL, BLACK, ORANGE )
   nI := 0
   // выгрузка программы cAppTtl из памяти, если программа была ранее запущена
   // unload the cAppTtl program from memory if the program was previously launched
   DO WHILE myIsProgaInMemory( cAppTtl, "HMG_FORM_", .T. )
      ? "." ; ? "-------- Is there a program in memory ["+cAppTtl+",HMG_FORM_] --- For:", ++nI
      wApi_Sleep(500)
   ENDDO
   InkeyGui(2000)
   WaitWindow()    // close the wait window
   SET WINDOW MAIN ON
   // запуск новой копии программы
   // launch a new copy of the program
   ShellExecute( , 'open', cFileExe, , , SW_SHOWNORMAL)
   ? ProcNL(), "RUN > " + cFileExe

RETURN Nil

//////////////////////////////////////////////////////////////////
// Использование EnumWindows - список окон программ в памяти
// Using EnumWindows - a list of program windows in memory
STATIC FUNCTION myIsProgaInMemory( cAppTitle, cMskClass, lLogOut)
LOCAL nI, cI, lRet := .F., hWnd, ahWnd := EnumWindows()

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
             // пример как будет в лог-файле:
             // 21 .  984750   HMG_FORM__HMG_CHILDWAITWINDOW ''
             // 24 .  1115838  HMG_FORM__HMG_CHILDWAITWINDOW ''
             // 26 .  15795926 HMG_FORM_test  My test program
             // 34 .  591448   HMG_FORM_Forma_1  Convert Dbf to Xls
             // 86 .  9504564  HMG_FORM_Form_Main   Compare / merge two dbf
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
          hWnd := ahWnd[nI]
          ? "-> Program running  - PostMessage( "+HB_NtoS(hWnd)+", WM_CLOSE, 0, 0 )"
          PostMessage( hWnd, WM_CLOSE, 0, 0 )
          wApi_Sleep(500)
          EXIT
       ENDIF
   NEXT

RETURN lRet

//////////////////////////////////////////////////////////////////////
FUNCTION wMain_2()   // nMode == 2
   LOCAL nMode := App.Cargo:nMode
   LOCAL aMode := App.Cargo:aMode     // len(aMode) может быть > 1, там параметры
   LOCAL cForm := App.Cargo:cWinOther // wMain_1
//...
   Sets_Mode2_App_Cargo ()            // настройка App.Cargo
   Sets_Mode2_App_Object()            // настройка App.Object
//...
//...
//   DEFINE WINDOW &cForm ... MAIN ...
     //...
     Sets_Mode2_Events()                // настройка :Event(...) окна
     //...
//   END WINDOW

RETURN Nil

STATIC FUNCTION Sets_Mode2_App_Cargo ()
//...
RETURN Nil

STATIC FUNCTION Sets_Mode2_App_Object()
//...
RETURN Nil

STATIC FUNCTION Sets_Mode2_Events()
//...
RETURN Nil

//////////////////////////////////////////////////////////////////////
FUNCTION wMain_4()   // nMode == 4
   LOCAL nMode  := App.Cargo:nMode
   LOCAL aMode  := App.Cargo:aMode
   LOCAL cForm  := App.Cargo:cWinOther // wMain_4
   LOCAL cTitle := "WINDOWTYPE STANDARD: Sample-4"

   Sets_Mode4_App_Cargo ()            // настройка App.Cargo
   Sets_Mode4_App_Object()            // настройка App.Object
   ? ProcNL(), "=== Start function ==="

   DEFINE WINDOW &cForm AT 0,0 WIDTH 200 HEIGHT 200 TITLE cTitle ;
      MAIN NOMAXIMIZE NOMINIMIZE NOSHOW                      ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT     {|| _wPost( 0) } ; // executed after window initialization
      ON RELEASE  {|| _wSend(90) } ; // executed before destroying the window

      This.Cargo := oHmgData()        // контейнер для ЭТОГО окна
      This.Cargo:tStart := HB_DATETIME()
      This.Cargo:aBClr  := COLOR_LIGHT_BLUE

      // в контейнер запоминаем основной процесс окна
      App.Cargo:oFormMain := This.Object

      WITH OBJECT This.Object
         // my_Standard4( cForm, nBtn, cTitle, aBClr, nY, nX, nW, nH, cAls, cWndMain )
         :Event(  0, {|ow|
                       _HMG_IsModalActive := .T.
                       // -> table4.prg
                       my_Standard4("form_Separate", 4, cTitle, ow:Cargo:aBClr, , , , , "EXT_ALS" , ow:Name)
                       DoEvents()
                       _wPost(99,ow)
                       Return Nil
                       } )
         :Event( 90, {|  | AEval({91,92,93}, {|n| _wSend(n) }) })
         :Event( 91, {|  | DbCloseAll()    /* закрыть все базы / close all bases*/        } )
         :Event( 92, {|ow| _LogFile(.T., ">>> ON RELEASE WINDOW: "+ow:Name, ProcNL() )    } )
         :Event( 93, {|ow| _LogFile(.T., ">>> Time and date of window operation " + HMG_TimeMS(ow:Cargo:tStart) + " <<<") } )
         :Event( 99, {|ow| ow:Release()  } )
      END WITH

      Sets_Mode4_Events()                // настройка :Event(...) окна

      ON KEY F1 ACTION NIL

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN Nil

////////////////////////////////////////////////////////
STATIC FUNCTION Sets_Mode4_App_Cargo ()
    ? ProcNL()
RETURN Nil

////////////////////////////////////////////////////////
STATIC FUNCTION Sets_Mode4_App_Object()
    ? ProcNL()
RETURN Nil

////////////////////////////////////////////////////////
STATIC FUNCTION Sets_Mode4_Events()
    ? ProcNL()
RETURN Nil


//////////////////////////////////////////////////////////////////////
FUNCTION wMain_5()                      // nMode == 5
   LOCAL nMode  := App.Cargo:nMode
   LOCAL aMode  := App.Cargo:aMode
   LOCAL cForm  := App.Cargo:cWinOther  // wMain_5
   LOCAL aBClr, cTitle := "MAIN WINDOWS: Sample-5"

   ? ProcNL(), "=== Start function ===", nMode, aMode, "_HMG_MainHandle=",_HMG_MainHandle
   aBClr := COLOR_LIGHT_BLUE

   SET WINDOW MAIN OFF // без главного окна

   // в этой функции MAIN окно создаётся автоматом
   // in this MAIN function the window is created automatically
   my_Standard4("form_Separate", 0, cTitle, aBClr, , , , , , "Form_None")  // -> table4.prg

RETURN Nil
