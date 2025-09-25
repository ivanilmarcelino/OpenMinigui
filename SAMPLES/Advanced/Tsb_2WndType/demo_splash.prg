/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2013-2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Показ логотипа программы / Show of program's Logo
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#define WM_PAINT  15
//////////////////////////////////////////////////////////////////////
PROCEDURE _SplashWindow( cForm, nY, nX, nW, nH, cBitmap, aRun )
   LOCAL hFont, aFont, cFont, nFSize, cFocus, cTitle, cIco
   Local aBClr, h, a, owc
   DEFAULT cForm := "MG_Logo", nY := 0, nX := 0

   // bmp, png, jpg, jpeg, emf, tiff
   h := LoadImage(cBitmap)
   a := GetBitmapSize( h )  // BmpSize( cBitmap )
   DeleteObject( h )
   // gif - a := hb_GetImageSize( cFile )

   nW     := a[1]
   nH     := a[2]
   cIco   := App.Cargo:cIcoDef
   cTitle := "Initialization"   // App.Cargo:cTitle
   hFont  := GetFontHandle("DlgFont")
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   cFocus := "Buff"
   aBClr  := SILVER

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco                          ;
      FONT cFont SIZE nFSize                          ;
      BACKCOLOR aBClr                                 ;
      MODAL NOCAPTION  NOSIZE                         ;
      ON INIT    _wPost( 0)                           ;
      ON RELEASE _wSend(90)
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:cFocus := cFocus
      owc:nImgW  := a[1]
      owc:nImgH  := a[2]

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      @ 0,0 IMAGE Image_1 PICTURE cBitmap WIDTH nW HEIGHT nH

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

      DRAW LINE IN WINDOW &cForm AT 0, 0 TO  0,nW PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &cForm AT nH,0 TO nH,nW PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &cForm AT 0, 0 TO nH, 0 PENCOLOR BLACK PENWIDTH 2
      DRAW LINE IN WINDOW &cForm AT 0,nW TO nH,nW PENCOLOR BLACK PENWIDTH 2

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION NIL

      WITH OBJECT This.Object
         :Event( 0, {|ow,ky| _LogFile(.T., ProcNL(), ">>> ON INIT WINDOW: "+ow:Name+" - Event:",ky),;
                             ow:SetFocus(ow:Cargo:cFocus), _wSend(1,ow)  } )

         :Event( 1, {|ow   | // Launching check/settings functions
                               Local cForm := ow:Name
                               Local nImgW := ow:Cargo:nImgW
                               This.Topmost := .F.
                               _SplashDelay( cForm, nImgW, aRun )
                               Return Nil
                               })

         :Event(90, {|ow,ky| _LogFile(.T., ProcNL(), ">>> ON RELEASE WINDOW: "+ow:Name+" - Event:",ky) })
         :Event(99, {|ow   | ow:Release() })
      END WITH

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm NOWAIT

   ? "===>>>", cForm, _IsWindowDefined(cForm)
   DO EVENTS

RETURN

/////////////////////////////////////////////////////////////////////////////////////////////
// Show the slider and labels. Running function from the function test is started.
PROCEDURE _SplashDelay( cForm, nWImg, aRun )
   Local a, i, k, n, bRun, cTxt, tStart, cMsg, tTime, lRun := .T.

   tStart := HB_DATETIME()
   ? "--- Running a function from a list ! Start:", tStart , ProcNL()

   SendMessage( GetFormHandle(cForm), WM_PAINT, 0, 0 )

   k := LEN(aRun)
   n := 100 / k
   FOR EACH a IN aRun

      i    := hb_enumindex(a)
      cTxt := a[1]
      bRun := a[2]

      tTime := hb_datetime()
      ? "---" + HB_NtoS(i) + REPL("-",5) + cTxt + " --- " + HB_TTOC(tTime)

      SetProperty(cForm, "Label_1", "Value", cTxt)
      Custom_Progress_Bar(cForm,335,25,nWImg-55,25,{255,0,0},n*i,100)
      SendMessage( GetFormHandle(cForm), WM_PAINT, 0, 0 )
      DO EVENTS

      lRun := EVal(bRun) ; Default lRun := .F.   // !!! function start

      wApi_Sleep( 1000 )  // !!! REMOVE !!! made for verification

      ? "---" + HB_NtoS(i) + REPL("-",20) + " " + HB_TTOC(hb_datetime()) + " --- Time spent:", HMG_TimeMS( tTime )
      ?? lRun

      IF !hb_IsLogical(lRun)
         cMsg := "ERROR ! Exiting the program!;"
         cMsg += ' Section: ' + cTxt + ";"
         cMsg += 'Returned: ' + cValToChar(lRun) + ' - there must be .T./.F. ;;'
         cMsg += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
         AlertStop(cMsg, "Result",,64,{RED})
         lRun := .F.
      ENDIF
      IF !lRun ; EXIT
      ENDIF
   NEXT

   IF ! lRun
      cMsg := "ERROR ! Exiting the program!;"
      cMsg += ' Section: ' + cTxt + ";"
      cMsg += 'Returned: .F.;;'
      cMsg += ProcNL(0) + ";" + ProcNL(1) + ";"
      cMsg += ProcNL(2) + ";" + ProcNL(3) + ";"
      cMsg += ProcNL(4) + ";" + ProcNL(5)
      AlertStop(cMsg, "Result",,64,{RED})
      ? Repl("*",20), ProcNL()
      ? cMsg ; ? Repl("*",20), ProcNL()
   ELSE
      cMsg := "[ TOTAL time spent: " + HMG_TimeMS(tStart) + " ]" 
      ? "--- Running a function from a list !", cMsg , ProcNL() ; ? "."
   ENDIF

   // Removing window Form_Splash
   //DoMethod( cForm, 'Release' )
   _wSend(99, cForm)  

RETURN

////////////////////////////////////////////////////////////
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


