/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Preloader in MiniGui
 *
 * Copyright 2015-2023 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * An example of showing a preloader. Displays the loading process: file, index, calculations, etc.
 * SAMPLES\BASIC\WAIT_WINDOW_2  - Edit 05.06.2023
*/

#include "minigui.ch"

STATIC oStaticGif, nStaticSeconds := 0
/////////////////////////////////////////////////////////////////////////
FUNCTION my_WaitWindow( cMsg, nStart, nStop, bBody, nSleep, bVal )
   LOCAL xRet, aBegin, nI, nK, lA, cVal, xVal, lModal := .F., bRun
   DEFAULT nStart := 1, nStop := 5, nSleep := 300, cMsg := 'Create a table ...'
   DEFAULT bVal  := {|n,k| hb_ntos(n) + "/" + hb_ntos(k) }
   DEFAULT bBody := {|| NIL }

   lA := hb_IsArray(bBody)             // массив строк для hb_MacroBlock()
   nK := iif( lA, Len(bBody), nStop )

   IF !Empty( _HMG_ThisFormName )          // текущее окно есть
      lModal := _HMG_ThisType == "M"       // это модал ?
   ENDIF

   IF lModal ; aBegin := WaitWinCreateModal( cMsg )
   ELSE      ; aBegin := WaitWinCreate( cMsg )
   ENDIF

   FOR nI := nStart TO nK
      xVal := EVal( bVal, nI, nK, aBegin ) // hb_ntos(nI) + "/" + hb_ntos(nK)
      cVal := cValToChar( xVal )
      WaitWinTimer( aBegin, cVal )
      IF lA
         IF !Empty( bBody[ nI ] )
            IF hb_IsChar( bBody[ nI ] )
               bRun := hb_MacroBlock( bBody[ nI ] )
            ELSE
               bRun := bBody[ nI ]
            ENDIF
            xVal := EVal( bRun, nI, nK, aBegin )
         ENDIF
      ELSE
         xVal := EVal( bBody, nI, nK, aBegin )  // что то делать тут можно
      ENDIF
      IF nSleep > 0              // для тестирования / just for testing
         wApi_Sleep( nSleep )
      ENDIF
      IF hb_IsChar(xVal) .and. "EXIT" $ upper(xVal) // остановка цикла, пример возврата "exit {'code', 100}"
         nI := nK
      ENDIF
      IF nI == nK ; xRet := xVal
      ENDIF
   NEXT
   WaitWinClose(aBegin)  // убить окно ожидания / kill waiting window

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitWinCreate( cTitle, lCenter, nWRow, nWCol )
   LOCAL cForm := "WaitWin_" + HB_NtoS( _GetId() )
   LOCAL cFont := "DejaVu Sans Mono", nFSize := 12
   LOCAL nW, nH, nTime := SECONDS()
   DEFAULT cTitle := "Wait", lCenter := .T., nWRow := 0, nWCol := 0

   SET INTERACTIVECLOSE OFF

   DEFINE WINDOW &cForm             ;
      ROW 0 COL 0 WIDTH 420 HEIGHT 230  ;
      TITLE ''                          ;
      MINWIDTH 420 MINHEIGHT 230        ;
      MAXWIDTH 420 MAXHEIGHT 230        ;
      CHILD NOCAPTION                   ;
      BACKCOLOR WHITE                   ;
      FONT cFont SIZE nFSize
      ON INIT { || This.Topmost := .T., DoEvents()  }

      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 10, 10 LABEL Label_1  WIDTH 400 HEIGHT nFSize*2 ;
        VALUE "Прошло 00:00:01" CENTERALIGN VCENTERALIGN TRANSPARENT

      @ 40, ( 420 - 128 ) / 2 ANIGIF Gif_1 OBJ oStaticGif PICTURE "Ani3dMan128" ;
        WIDTH 142 HEIGHT 128  DELAY 5 BACKGROUNDCOLOR WHITE

      @ 40 + 128 + 10, 10 LABEL Label_2 WIDTH 400 HEIGHT 20 ;
        VALUE cTitle  CENTERALIGN VCENTERALIGN TRANSPARENT

   END WINDOW

   Center Window &cForm
   IF lCenter == .F.
      SetProperty( cForm, "Row", nWRow )
      SetProperty( cForm, "Col", nWCol )
   ENDIF

   Activate Window &cForm NoWait

   ? "===[] CREATE FORM -> cForm=", cForm, ProcNL() ; ?

RETURN { cForm, nTime } // вернуть номер окна, чтобы по нему потом убить окно

//////////////////////////////////////////////////////////////////////////
FUNCTION WaitWinCreateModal( cTitle, lCenter, nWRow, nWCol )
   LOCAL cForm := "WaitWin_" + HB_NtoS( _GetId() )
   LOCAL cFont := "DejaVu Sans Mono", nFSize := 12
   LOCAL nTime := SECONDS()

   DEFAULT cTitle  := "Wait", lCenter := .T., nWRow := 0, nWCol := 0

   SET INTERACTIVECLOSE OFF

   DEFINE WINDOW &cForm            ;
      ROW 0 COL 0 WIDTH 420 HEIGHT 230 ;
      TITLE ''                         ;
      MINWIDTH 420 MINHEIGHT 230       ;
      MAXWIDTH 420 MAXHEIGHT 230       ;
      MODAL NOCAPTION                  ;
      BACKCOLOR WHITE                  ;
      FONT cFont SIZE nFSize           ;
      ON INIT { || This.Topmost := .T. , DoEvents() }

     @ 10, 10 LABEL Label_1 WIDTH 400 HEIGHT nFSize*2 ;
       VALUE "Прошло 00:00:01" CENTERALIGN VCENTERALIGN TRANSPARENT

      @ 40, ( 420 - 128 ) / 2 ANIGIF Gif_1 OBJ oStaticGif PICTURE "Ani3dMan128" ;
        WIDTH 142 HEIGHT 128 DELAY 5 BACKGROUNDCOLOR WHITE

      @ 40 + 128 + 10, 10 LABEL Label_2 WIDTH 400 HEIGHT nFSize*2 ;
        VALUE cTitle CENTERALIGN VCENTERALIGN TRANSPARENT

   END WINDOW

   Center Window &cForm
   IF lCenter == .F.
      SetProperty( cForm, "Row", nWRow )
      SetProperty( cForm, "Col", nWCol )
   ENDIF
   Activate Window &cForm NoWait

   ? "===[] CREATE FORM -> cForm=", cForm, ProcNL()

RETURN { cForm, nTime } // вернуть номер окна, чтобы по нему потом убить окно

//////////////////////////////////////////////////////////////////////////
FUNCTION WaitWinTimer( aDim, cDop )
   LOCAL cForm := aDim[ 1 ], cTime, nTime := aDim[ 2 ]
   DEFAULT cDop := ""

   IF ABS( SECONDS() - nStaticSeconds ) >= 0.1
      cTime := IIF( LEN( cDop ) > 0, cDop + SPACE( 5 ), "" )
      cTime += "Прошло " + SECTOTIME( SECONDS() - nTime )
      SetProperty( cForm, "Label_1", "Value", cTime )
      nStaticSeconds := SECONDS()
      DO EVENTS
   ENDIF

   IF ! oStaticGif:IsRunning()
      oStaticGif:Play()
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION WaitWinClose( aDim )
   LOCAL cForm := aDim[ 1 ]

   SET INTERACTIVECLOSE ON

   Domethod( cForm, "Release" )

   DO MESSAGE LOOP

   ? "===[] RELEASE FORM -> cForm=", cForm, ProcNL()

RETURN NIL

