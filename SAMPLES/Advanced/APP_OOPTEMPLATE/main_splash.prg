/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * The idea of 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "metrocolor.ch"

#define WM_PAINT  15
////////////////////////////////////////////////////////////
// Показ логотипа программы.
// Show of program's Logo.
PROCEDURE _DefineSplashWindow( name, nRow, nCol, nW, nH, cbitmap, cForm )
   Local aImgSize := BmpSize( cbitmap )
   DEFAULT nRow := 0, nCol := 0, nW := aImgSize[1], nH := aImgSize[2]

   ? ProcNL() ; ? "--- Show of Logo"

   DEFINE WINDOW &name AT nRow,nCol WIDTH nW HEIGHT nH ;
      CHILD TOPMOST                                    ;
      NOSIZE NOMAXIMIZE NOMINIMIZE NOSYSMENU NOCAPTION ;
      ON INIT {|| _wPost(0) }
      // ON INIT _SplashDelay( name, aImgSize[1], cForm ) - так не надо делать
      // ON RELEASE {|| Eval( bRelease, cForm ) } - резерв

      This.Cargo := oHmgData()
      This.Cargo:cForm := cForm
      This.Cargo:nImgWidth  := aImgSize[1]
      This.Cargo:nImgHeight := aImgSize[2]

      @ 0,0 IMAGE Image_1 PICTURE cbitmap WIDTH nW HEIGHT nH

      // надпись под бегунком / signature on progresbar
      @ 360,25 LABEL Label_1 VALUE "" WIDTH nW - 55 HEIGHT 22 TRANSPARENT ;
        FONT "Arial" SIZE 10 BOLD FONTCOLOR RED

      @ 113,20 LABEL Label_2 VALUE "Free open source GUI: " + MiniGUIVersion() ;
        WIDTH nW - 30 HEIGHT 22 CENTERALIGN TRANSPARENT                     ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR YELLOW

      @ 245,20 LABEL Label_3 VALUE "Free open source: " + Version() ;
        WIDTH nW - 30 HEIGHT 22 CENTERALIGN TRANSPARENT          ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR BLACK

      @ 275,220 LABEL Label_4 VALUE hb_compiler()  ;
        WIDTH nW - 30 HEIGHT 22 TRANSPARENT      ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR YELLOW

      DRAW LINE IN WINDOW &name AT 0, 0 TO  0,nW PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &name AT nH,0 TO nH,nW PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &name AT 0, 0 TO nH, 0 PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &name AT 0,nW TO nH,nW PENCOLOR BLACK PENWIDTH 2

      (This.Object):Event(0, {|ow|
                               Local cForm := ow:Cargo:cForm
                               Local nImgW := ow:Cargo:nImgWidth
                               This.Topmost := .F.
                               //_SplashDelay( ow:Name, nImgW, cForm )
                               hb_ExecFromArray("_SplashDelay",{ow:Name, nImgW, cForm})
                               Return Nil
                               })
   END WINDOW

   IF EMPTY(nRow) .AND. EMPTY(nCol)
      CENTER WINDOW &name
   ENDIF

   ACTIVATE WINDOW Form_Splash

RETURN

/////////////////////////////////////////////////////////////////////////////////////////////
// Показ бегунка и надписи. Запуск функции из списка функций проверки при запуске программы.
// Show the slider and labels. Running function from the function test is started.
PROCEDURE _SplashDelay( name, nWidthImg, cForm )
   Local a, i, k, n, cRun, cTxt, tStart, aRun, cMsg, tTime, lRun := .T.

   aRun   := (App.Cargo):aRunCheck    // -> main_check.prg
   tStart := HB_DATETIME()
   ? "--- Running a function from a list ! Start:", tStart , ProcNL()

   SendMessage( GetFormHandle(name), WM_PAINT, 0, 0 )

   // просто как пример / just as an example
   _wSend(100, cForm, "START. Running a function from a list !")

   k := LEN(aRun)
   n := 100 / k
   FOR EACH a IN aRun

      i := hb_enumindex(a)

      tTime := hb_datetime()
      ? "---" + HB_NtoS(i) + REPL("-",5) + HB_ValToExp(aRun[i]) + " --- " + HB_TTOC(tTime)

      IF hb_IsBlock(a)
         cRun := "???"
         cTxt := "???"
         lRun := hb_ExecFromArray(a, {i, k, { name, "Label_1" }} )
      ELSE
         SetProperty(name, "Label_1", "Value", a[1])
         cRun := StrTran(a[2], "()", "")
         cTxt := a[1]
         lRun := hb_ExecFromArray(cRun, {a[3]})
      ENDIF
      Custom_Progress_Bar(name,335,25,nWidthImg-55,25,{255,0,0},n*i,100)
      SendMessage( GetFormHandle(name), WM_PAINT, 0, 0 )
      DO EVENTS
      ? "---" + HB_NtoS(i) + REPL("-",20) + " " + HB_TTOC(hb_datetime()) + " --- Time spent:", HMG_TimeMS( tTime )
      ?? lRun
      IF !hb_IsLogical(lRun)
         cMsg := "ERROR ! Exiting the program!;"
         cMsg += 'Function: ' + cRun + ";"
         cMsg += ' Section: ' + cTxt + ";"
         cMsg += 'Returned: '+cValToChar(lRun)+' - there must be .T./.F. ;;'
         cMsg += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
         AlertStop(cMsg, "Result")
         lRun := .F.
         ? cMsg
      ENDIF
      IF !lRun ; EXIT
      ENDIF
   NEXT

   IF ! lRun
       cMsg := "ERROR ! Exiting the program!;"
       cMsg += 'Function: ' + cRun + ";"
       cMsg += ' Section: ' + cTxt + ";"
       cMsg += 'Returned: .F.;;'
       cMsg += ProcNL(0) + ";" + ProcNL(1) + ";"
       cMsg += ProcNL(2) + ";" + ProcNL(3) + ";"
       cMsg += ProcNL(4) + ";" + ProcNL(5)
       AlertStop(cMsg, "Result")
       ? Repl("*",20), ProcNL()
       ? cMsg ; ? Repl("*",20), ProcNL()
       _wSend(99, cForm)  // выше сообщение _wSend(100, cForm, ...) есть
          /*
          можно на MAIN окне сделать событие (не обязательно)
          :Event({98, "ExitWindowMsg"}, {|ow,ky,cTxt|
                 ky := ow
                 AlertStop(cTxt, "Result")
                 ? CRLF + cTxt
                 ow:Release()
                 } )
          тогда делаем как удобнее
          cMsg := ...
          cMsg += ...
          _wSend(98, cForm, {cMsg, 99}) или _wSend("ExitWindowMsg", cForm, {cMsg,99})
          Тогда событие на Main
          :Event({98, "ExitWindowMsg"}, {|ow,ky,cTxt|
                 ky := 0
                 IF hb_IsArray(cTxt)
                    ky   := cTxt[2]  // выполнить след. событие
                    cTxt := cTxt[1]  // текст для Alert
                 ENDIF
                 AlertStop(cTxt, "Result")
                 ? CRLF + cTxt, ky
                 IF ky > 0
                    _wPost(ky)  // или _wSend(ky)
                 ENDIF
                 } )
          */
   ENDIF

   // просто как пример / just as an example
   _wSend(100, cForm, "STOP. Running a function from a list !")

   // Удаление окна Form_Splash / Removing window Form_Splash
   DoMethod( name, 'Release' )

   cMsg := "[ TOTAL time spent: " + HMG_TimeMS(tStart) + " ]" + REPL("-",10) ; ? "."
   ? ProcNL(), name, 'Release'
   ? "--- Running a function from a list ! --------- ", cMsg
   ? "=>>>", cForm, ProcNL(); ?

RETURN

////////////////////////////////////////////////////////////
// Функция рисования бегунка на логотипе программы
// The drawing slider on the logo program
FUNCTION Custom_Progress_Bar(cFormName,nRow,nCol,nWidth,nHeight,aColor,nValue,nMax)
   LOCAL nStartRow, nStartCol, nFinishRow, nFinishCol

   // progress bar
   IF nWidth > nHeight  // Horizontal Progress Bar
      nStartRow := nRow + 1
      nStartCol := nCol + 1
      nFinishRow := nRow + nHeight - 1
      nFinishCol := nCol + 1 + ((nWidth - 2) * nValue / nMax)
   ELSE  // Vertical Progress Bar
      nStartRow := nRow + nHeight - 1
      nStartCol := nCol + 1
      nFinishRow := nStartRow - ((nHeight - 2) * nValue / nMax)
      nFinishCol := nCol + nWidth - 1
   ENDIF

   DRAW RECTANGLE IN WINDOW &cFormName AT nStartRow,nStartCol TO nFinishRow,nFinishCol ;
        PENCOLOR aColor FILLCOLOR aColor

   DO EVENTS

RETURN NIL


