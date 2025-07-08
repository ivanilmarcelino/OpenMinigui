/*
 * MINIGUI - Harbour Win32 GUI library
 * Потоки в Harbour / Streams in Harbour
 * Потоки в MiniGui / Streams in MiniGui
 * Прелодер в MiniGui / Preloader in MiniGui
 * Работа с Gif-объектом / Working with a Gif object
 *
 * Copyright 2015-24 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2015 Grigory Filatov <gfilatov@inbox.ru>
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
*/
#include "hmg.ch"
#include "hbthread.ch"

/* У себя в программе можно делать так: / In your program you can do this:

   nTime := SECONDS()
   // создаём окно ожидания с потоком
   aThread := WaitThreadGif( 'Расчёт начислений ...' )

   // основной цикл вычислений/расчётов/созданий баз и т.д.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "50"
      WaitThreadGifSay( aThread, cVal )
      ...................
   NEXT

   WaitThreadGifClose( aThread )

   cMsg := "Расчет успешно произведён за " + SECTOTIME( SECONDS() - nTime )
   MsgInfo(cMsg)
-----------------------------------------------------------------------------
Можно и так:

   WaitThreadGif( 'Расчёт начислений ...' )  // создаём окно ожидания с потоком

   // основной цикл вычислений/расчётов/созданий баз и т.д.
   ...................

   WaitThreadGifClose()   // закрыть окно "ожидания"
*/
// for the start of the cycle in the window "waiting"
STATIC cStatWinWait              // имя окна
STATIC lStatWinWait   := .T.     // для старта бесконечного цикла preloding в потоке
STATIC nStaticSeconds := 0       // время старта прелодера для WaitThreadGifTimer()
STATIC oStaticGif                // для картинки GIF
STATIC nStatWinWidth             // ширина внутри окна
//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadGif( cTitle, nTimeRun, lFocus )
   LOCAL cFormName := "WaitWin_" + HB_NtoS( _GetId() )
   LOCAL cFont := 'Tahoma', nFontSize := 12
   LOCAL lFor  := .T., lModal := .T. , aResGif := "Ani3dMan128"
   LOCAL nWTxt, nW, nH, cWnd, hWnd, cFoc, aParam
   DEFAULT cTitle := "Ожидание...", nTimeRun := Seconds(), lFocus := .F.
                                 // nTimeRun - продолжение времени показа в окне

   IF lFocus
      cWnd := ThisWindow.Name            // текущее окно если оно есть
      hWnd := ThisWindow.Handle          // хендл  - текущее окно
      cFoc := ThisWindow.FocusedControl  // контрол в фокусе - текущее окно
   ENDIF
   // размеры окна
   nW := 420
   nH := 230
   cStatWinWait   := cFormName
   lStatWinWait   := .T.        // для старта бесконечного цикла preloding в потоке
   nStaticSeconds := SECONDS()  // время старта прелодера для WaitThreadGifTimer()

   // Подбор размера шрифта, чтобы надпись уместилась в окошке
   DO WHILE lFor
      nWTxt := GetTxtWidth( cTitle , nFontSize, cFont) + 40
      IF nWTxt > nW    // ширина окна
         nFontSize--
      ELSE
         lFor := .F.
      ENDIF
   ENDDO

   SET INTERACTIVECLOSE OFF

   IF !Empty( _HMG_ThisFormName )          // текущее окно есть
      lModal := ( _HMG_ThisType == "M" )   // это модал ?
   ENDIF

   SetCursorSystem( IDC_WAIT )

IF lModal
   DEFINE WINDOW &cFormName          ;
     ROW 0 COL 0 WIDTH nW HEIGHT nH  ;
     TITLE ''                        ;
     MINWIDTH nW MINHEIGHT nH        ;
     MAXWIDTH nW MAXHEIGHT nH        ;
     MODAL NOCAPTION                 ;
     BACKCOLOR WHITE                 ;
     FONT cFont SIZE nFontSize       ;
     ON MOUSECLICK MoveActiveWindow()
ELSE
   DEFINE WINDOW &cFormName          ;
     ROW 0 COL 0 WIDTH nW HEIGHT nH  ;
     TITLE ''                        ;
     MINWIDTH nW MINHEIGHT nH        ;
     MAXWIDTH nW MAXHEIGHT nH        ;
     CHILD NOCAPTION TOPMOST         ; // окно на передний план
     BACKCOLOR WHITE                 ;
     FONT cFont SIZE nFontSize       ;
     ON MOUSECLICK MoveActiveWindow()
ENDIF

     // установим обработчики окна
     This.OnInit    := {|| DoEvents(), This.Topmost := .T. , _wPost(0) }   // выполняется после инициализации окна
     This.OnRelease := {|| _wPost(90)                                  }   // выполняется перед разрушением окна
     This.OnInterActiveClose := {|| This.Cargo:lClose }

     // установим контейнер на окно
     This.Cargo := oHmgData()
     This.Cargo:lClose := .T.

     nW := This.ClientWidth
     nH := This.ClientHeight
     nStatWinWidth := nW

     @ 10,10 LABEL Label_1  WIDTH nW - 10*2 HEIGHT 33  ;
       VALUE "Прошло 00:00:00" CENTERALIGN VCENTERALIGN TRANSPARENT

     @ 10,10 LABEL Label_0 WIDTH 100 HEIGHT 33 VALUE "" VCENTERALIGN TRANSPARENT

     @ 40, ( nW - 128 ) / 2 ANIGIF Gif_1 OBJ oStaticGif PICTURE aResGif ;
       WIDTH 142 HEIGHT 128 ;
       DELAY 5 BACKGROUNDCOLOR WHITE

     @ 40 + 128 + 10, 10 LABEL Label_2 WIDTH 400 HEIGHT 33 VALUE cTitle  ;
       CENTERALIGN VCENTERALIGN TRANSPARENT

     (This.Object):Event( 0, {|ow| _logfile(.t.,"   ---[ :Event(0) ]---" + ProcNL() ) ,;
                                   SetWaitCursor( ow:Handle )                            })

     (This.Object):Event(90, {|ow| _logfile(.t.,"   ---[ :Event(90) ]--- " + ProcNL(),;
                                   ">>> RELEASE: " + ow:Name ) , InkeyGUI(50)            })

     (This.Object):Event(99, {|ow| ow:Cargo:lClose := .T., ow:Release() })

   END WINDOW

   oStaticGif:Update()

     Center Window &cFormName
   Activate Window &cFormName NOWAIT

   ? "===[] CREATE FORM -> cFormName=", cFormName, "lModal=", lModal, ProcNL()

   DO EVENTS
   // Start preloding in a separate thread
   // Запускаем preloding в отдельном потоке
   aParam := { cFormName, nTimeRun, nW, nH }
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @WaitThreadGifTimer(), aParam ) )

   IF lFocus
      If ! empty(cWnd) .and. _IsWindowDefined(cWnd)
         BringWindowToTop( hWnd )   // переход на текущее окно
         // или эти строки, а может и то и другое
         //Domethod(cWnd, "Minimize")
         //Domethod(cWnd, "Restore" )
         DO EVENTS
         IF ! Empty(cFoc)                       // setfocus где были
            DoMethod(cWnd, cFoc, 'SetFocus')
         ENDIF
      Endif
   ENDIF

   DO EVENTS

RETURN aParam

//////////////////////////////////////////////////////////////////////
// завершить функцию в потоке / complete function in the stream
FUNCTION WaitThreadGifClose(aDim)
   LOCAL cFormName, nTime, cMsg
   DEFAULT aDim := {}

   IF !hb_IsArray(aDim)
      cMsg := "aDim is not an array! To correct !;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
      aDim := {}
   ENDIF

   IF LEN(aDim) == 0
      cFormName := cStatWinWait
   ELSE
      cFormName := aDim[ 1 ]
      nTime     := aDim[ 2 ]
   ENDIF

   oStaticGif:Stop()

   ? "===[] CLOSE FORM -> cFormName=", cFormName, ProcNL()

   // complete function in the stream
   lStatWinWait := .F.
   InkeyGUI( 100 )

   SET INTERACTIVECLOSE ON

   DO EVENTS
   RELEASE oStaticGif
   Domethod(cFormName,"Release")

   SetCursorSystem( IDC_ARROW )

   DO MESSAGE LOOP

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadGifTimer(aDim)
   LOCAL cFormName, nTime, cTime

   cFormName := aDim[ 1 ]
   nTime     := aDim[ 2 ]     // Variable nTime is equal SECONDS() - gives an example of the transmission
   cFormName := cStatWinWait  // или так

   DO WHILE lStatWinWait
      IF ABS( SECONDS() - nStaticSeconds ) >= 0.05

         cTime := "Прошло " + SECTOTIME( SECONDS() - nTime )
         SetProperty( cFormName, "Label_1", "Value", cTime )

         InkeyGui(50)
         nStaticSeconds := SECONDS()

      ENDIF
   ENDDO

   IF ! oStaticGif:IsRunning()
      oStaticGif:Play()
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// вывод доп.информации / output of additional information
FUNCTION WaitThreadGifSay( aDim, cVal )
   LOCAL cFormName
   DEFAULT cVal := "?-cVal"

   IF hb_IsArray(aDim)
      cFormName := aDim[ 1 ]
   ELSE
      cFormName := cStatWinWait
   ENDIF

   IF _IsWindowActive( cFormName )
      IF _IsControlDefined("Label_0",cFormName)
         SetProperty( cFormName, "Label_0", "Value", cVal )
         SetProperty( cFormName, "Label_1", "Col"  , 100  )
         SetProperty( cFormName, "Label_1", "Width", nStatWinWidth-100-10*2 )
      ENDIF
   ENDIF

   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

STATIC PROCEDURE MoveActiveWindow( hWnd )
   DEFAULT hWnd := GetActiveWindow()

   PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )

   RC_CURSOR( "Grabbed32" )

RETURN

///////////////////////////////////////////////////////////////////////////////
// получить Width текста
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;
           nFontSize := _HMG_DefaultFontSize,  ;
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth
