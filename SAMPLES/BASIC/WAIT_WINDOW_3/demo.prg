/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Threads in MiniGui
 * Preloader in MiniGui
 *
 * Copyright 2023-24 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2023-24 Sergej Kiselev <bilance@bilance.lv>
 *
 * An example of showing a preloader. Displays the loading process for windows
 * Пример показа прелоадера. Отображает процесс загрузки для окна
 * Работа с Gif-объектом / Working with a Gif object
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "hbthread.ch"

//////////////////////////////
SET PROCEDURE TO WaitThreadGif

// общие цвета: фон окна
#define COLOR_0  { 179, 116, 215 }
// общие цвета: фон кнопок
#define COLOR_1  { 159, 191, 236 }
#define COLOR_2  {  94, 230, 203 }
#define COLOR_3  { 250, 253, 214 }
#define COLOR_4  { 195, 224, 133 }
#define COLOR_5  { 255, 178, 178 }

STATIC nStaticTimeStart     := 0
STATIC nStaticProgressStart := 0

PROCEDURE Main

   LOCAL cLog, nW, nH, nX, nY, nG, nHBtn, nWBtn
   LOCAL aBtn, tTime
   LOCAL y, x, w, h

   cLog := _SetGetLogFile( "_msg.log" )
   DeleteFile( cLog )
   SET LOGFILE TO &cLog // установить имя файла для вывода отладки

   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 16 // for HMG_Alert()
   //////////
   SET OOP ON
   //////////
   SET DEFAULT ICON TO "1MAIN_ICO"

   IF ! hb_mtvm()
      AlertStop ( "No support for multi-threading!" + CRLF + CRLF + ;
         "Compiling with a key /mt" + CRLF )
      QUIT
   ENDIF

   tTime := hb_DateTime()
   ? REPL( "=", 20 ) + " Program launch - " + hb_TToC( hb_DateTime() ) + " " + REPL( "=", 20 )
   ? MiniGuiVersion() ;  ?

   SET FONT TO "Tahoma", 16

   DEFINE WINDOW Form_Main AT 150, 150 WIDTH 540 HEIGHT 390 ;
      MAIN BACKCOLOR COLOR_0 ;
      TITLE "Window with expectation (preloading) in stream" ;
      ON INIT {|| _LogFile( .T., "--- Window [Form_Main] create ---  " + HMG_TimeMS( tTime ) ) } ;
      ON RELEASE { || _LogFile( .T., ">>> End prog <<<  " + HMG_TimeMS( tTime ) ) } //,;
      ;// DoEvents() , wApi_Sleep(200) , ;
      ;// ShellExecute(0,"Open",cLog,,,1), DoEvents() }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nX := nG := 20
      nY := 50
      nHBtn := 60 // высота кнопки
      nWBtn := 230 // ширина кнопки

      @ nY, 0 LABEL Label_1 VALUE "MAIN: Waiting for the start of the calculation ..." ;
         WIDTH nW HEIGHT 60 FONTCOLOR WHITE BOLD TRANSPARENT CENTERALIGN
      nY += 60

      aBtn := Btn_Form_Main_Init() // массив кнопок - 6 штук

      y := nY ; x := nG ; w := nWBtn ; h := nHBtn

      Btn_Forma( y, x, w, h, aBtn[ 1 ] ) // положить кнопку 1 на форму
      y += h + nG
      Btn_Forma( y, x, w, h, aBtn[ 2 ] ) // положить кнопку 2 на форму
      y += h + nG

      y := nY // вернули позицию левой кнопки
      x := nW - nG - w
      Btn_Forma( y, x, w, h, aBtn[ 3 ] ) // положить кнопку 3 на форму
      y += h + nG
      Btn_Forma( y, x, w, h, aBtn[ 4 ] ) // положить кнопку 4 на форму
      y += h + nG
      Btn_Forma( y, x, w, h, aBtn[ 5 ] ) // положить кнопку 5 на форму ( Exit )

   END WINDOW

   // CENTER WINDOW Form_Main
   ACTIVATE WINDOW Form_Main

RETURN

////////////////////////////////////////////////////////////
STATIC FUNCTION Btn_Form_Main_Init()

   LOCAL aBtn := {}, oBtn, aColor

   oBtn := oHmgData() // создать контейнер для кнопки

   aColor := COLOR_1
   oBtn:nBtn := 1
   oBtn:cBtn := "Btn_1"
   oBtn:cCapt := "Start (1);WaitWinType=1"
   oBtn:aBClr := aColor // цвет фона текущей кнопки
   oBtn:aBtnFClr := { BLACK, YELLOW } // два цвета фонта текущей кнопки для события по мышке
   oBtn:aBtnBClr := { aColor, BLACK } // два цвета фона текущей кнопки для события по мышке
   oBtn:c2Form := "Form_Table_Dog(1)" // имя нового окна
   oBtn:c2Title := "Window example (1)" // титул нового окна
   oBtn:a2WinBClr := aColor // цвет фона нового окна
   oBtn:nWaitType := 1 // тип окна ожидания 1/2/3
   oBtn:nWaitTime := 1 // тип показа времени: 0-новое время/1-продолжение времени
   oBtn:cAction := "{|| StartSample1() }"
   oBtn:bAction := {|| StartSample1() }
   // Градиент и иконки на кнопках
   oBtn:aIcon := { "iDbg48x1", "iDbg48x2" }
   oBtn:aGradientFill := { { 0.5, CLR_WHITE, aColor }, { 0.5, aColor, CLR_WHITE } }
   oBtn:aGradientOver := { { 0.5, aColor, CLR_WHITE }, { 0.5, CLR_WHITE, aColor } }
   oBtn:nPost := oBtn:nBtn
   oBtn:cAction := "{|| DoEvents(), _wPost(This.Cargo:nPost, This.Index) }"
   oBtn:bAction := {|| DoEvents(), _wPost( This.Cargo:nPost, This.Index ) }
   oBtn:cEvent := "{|obtn| StartSample1(obtn) }"
   oBtn:bEvent := {| obtn | StartSample1( obtn ) }

   AAdd( aBtn, oBtn )

   oBtn := oHmgData() // создать контейнер для кнопки

   aColor := COLOR_2
   oBtn:nBtn := 2
   oBtn:cBtn := "Btn_2"
   oBtn:cCapt := "Start (2);WaitWinType=1"
   oBtn:aBClr := aColor // цвет фона текущей кнопки
   oBtn:aBtnFClr := { BLACK, YELLOW } // два цвета фонта текущей кнопки для события по мышке
   oBtn:aBtnBClr := { aColor, BLACK } // два цвета фона текущей кнопки для события по мышке
   oBtn:c2Form := "Form_Table_Abon(2)" // имя нового окна
   oBtn:c2Title := "Window example (2)" // титул нового окна
   oBtn:a2WinBClr := aColor // цвет фона нового окна
   oBtn:nWaitType := 1 // тип окна ожидания 1/2/3
   oBtn:nWaitTime := 0 // тип показа времени: 0-новое время/1-продолжение времени
   oBtn:cAction := "{|| StartSample1() }"
   oBtn:bAction := {|| StartSample1() }
   // Градиент и иконки на кнопках
   oBtn:aIcon := { "iDbg48x1", "iDbg48x2" }
   oBtn:aGradientFill := { { 0.5, CLR_WHITE, aColor }, { 0.5, aColor, CLR_WHITE } }
   oBtn:aGradientOver := { { 0.5, aColor, CLR_WHITE }, { 0.5, CLR_WHITE, aColor } }
   oBtn:nPost := oBtn:nBtn
   oBtn:cAction := "{|| DoEvents(), _wPost(This.Cargo:nPost, This.Index) }"
   oBtn:bAction := {|| DoEvents(), _wPost( This.Cargo:nPost, This.Index ) }
   oBtn:cEvent := "{|obtn| StartSample1(obtn) }"
   oBtn:bEvent := {| obtn | StartSample1( obtn ) }

   AAdd( aBtn, oBtn )

   oBtn := oHmgData() // создать контейнер для кнопки

   aColor := COLOR_3
   oBtn:nBtn := 3
   oBtn:cBtn := "Btn_3"
   oBtn:cCapt := "Start (3);WaitWinType=2"
   oBtn:aBClr := aColor // цвет фона текущей кнопки
   oBtn:aBtnFClr := { BLACK, YELLOW } // два цвета фонта текущей кнопки для события по мышке
   oBtn:aBtnBClr := { aColor, BLACK } // два цвета фона текущей кнопки для события по мышке
   oBtn:c2Form := "Form_Table_Zakaz(3)" // имя нового окна
   oBtn:c2Title := "Window example (3)" // титул нового окна
   oBtn:a2WinBClr := aColor // цвет фона нового окна
   oBtn:nWaitType := 2 // тип окна ожидания 1/2/3
   oBtn:nWaitTime := 1 // тип показа времени: 0-новое время/1-продолжение времени
   oBtn:cAction := "{|| StartSample2() }"
   oBtn:bAction := {|| StartSample2() }

   AAdd( aBtn, oBtn )

   oBtn := oHmgData() // создать контейнер для кнопки

   aColor := COLOR_4
   oBtn:nBtn := 4
   oBtn:cBtn := "Btn_4"
   oBtn:cCapt := "Start (4);WaitWinType=3"
   oBtn:aBClr := aColor // цвет фона текущей кнопки
   oBtn:aBtnFClr := { BLACK, YELLOW } // два цвета фонта текущей кнопки для события по мышке
   oBtn:aBtnBClr := { aColor, BLACK } // два цвета фона текущей кнопки для события по мышке
   oBtn:c2Form := "Form_Table_Tovar(4)" // имя нового окна
   oBtn:c2Title := "Window example (4)" // титул нового окна
   oBtn:a2WinBClr := aColor // цвет фона нового окна
   oBtn:nWaitType := 3 // тип окна ожидания 1/2/3
   oBtn:nWaitTime := 1 // тип показа времени: 0-новое время/1-продолжение времени
   oBtn:cAction := "{|| StartSample2() }"
   oBtn:bAction := {|| StartSample2() }

   AAdd( aBtn, oBtn )

   oBtn := oHmgData() // создать контейнер для кнопки

   oBtn := oHmgData() // создать контейнер для кнопки

   oBtn:nBtn  := 5
   oBtn:cBtn  := "Btn_5"
   oBtn:cCapt := "Exit"
   oBtn:aBClr := COLOR_5 // цвет фона текущей кнопки
   oBtn:aBtnFClr := { BLACK, YELLOW } // два цвета фонта текущей кнопки для события по мышке
   oBtn:aBtnBClr := { COLOR_5, BLACK } // два цвета фона текущей кнопки для события по мышке
   oBtn:cAction := "{|| ThisWindow.Release() }"
   oBtn:bAction := {|| ThisWindow.Release() }
   // иконка на кнопке
   oBtn:aIcon := { "iExit48x1", "iExit48x1" }

   AAdd( aBtn, oBtn )

RETURN aBtn

////////////////////////////////////////////////////////////
FUNCTION Btn_Forma( nY, nX, nW, nH, oBtn )

   LOCAL cBtn := oBtn:cBtn, oTmp, aBtn
   LOCAL cCapt := oBtn:cCapt
   DEFAULT oBtn:cAction := "{|| InkeyGui(1000), This.Enabled := .T. }"
   DEFAULT oBtn:bAction := {|| InkeyGui( 1000 ), This.Enabled := .T. }
   DEFAULT cCapt := ""

   IF ! Empty( cCapt ) .AND. ";" $ cCapt ; cCapt := AtRepl( ";", cCapt, CRLF )
   ENDIF
   // координаты заданы в контейнере кнопки
   IF HB_ISNUMERIC( oBtn:nRow ) ; nY := oBtn:nRow
   ENDIF
   IF HB_ISNUMERIC( oBtn:nCol ) ; nX := oBtn:nCol
   ENDIF
   IF HB_ISNUMERIC( oBtn:nWidth ) ; nW := oBtn:nWidth
   ENDIF
   IF HB_ISNUMERIC( oBtn:nHeight ) ; nH := oBtn:nHeight
   ENDIF

   IF Empty( oBtn:aGradientFill ) .OR. Empty( oBtn:aGradientOver )
      IF Empty( oBtn:aIcon )
         @ nY, nX BUTTONEX &( cBtn ) WIDTH nW HEIGHT nH CAPTION cCapt ;
            NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT ;
            FONTCOLOR oBtn:aBtnFClr[ 1 ] BACKCOLOR oBtn:aBtnBClr[ 1 ] ;
            ON MOUSEHOVER ( This.FONTCOLOR := This.Cargo:aBtnFClr[ 2 ], This.BACKCOLOR := This.Cargo:aBtnBClr[ 2 ] ) ;
            ON MOUSELEAVE ( This.FONTCOLOR := This.Cargo:aBtnFClr[ 1 ], This.BACKCOLOR := This.Cargo:aBtnBClr[ 1 ] ) ;
            ACTION {|| This.Enabled := .F., Eval( This.Cargo:bAction ) } ;
            ON INIT {|| This.Cargo := oHmgData() }

      ELSE
         @ nY, nX BUTTONEX &( cBtn ) WIDTH nW HEIGHT nH CAPTION cCapt ICON oBtn:aIcon[ 1 ] ;
            NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT ;
            FONTCOLOR oBtn:aBtnFClr[ 1 ] BACKCOLOR oBtn:aBtnBClr[ 1 ] ;
            ON MOUSEHOVER ( This.FONTCOLOR := This.Cargo:aBtnFClr[ 2 ], This.BACKCOLOR := This.Cargo:aBtnBClr[ 2 ], This.ICON := This.Cargo:aIcon[ 2 ] ) ;
            ON MOUSELEAVE ( This.FONTCOLOR := This.Cargo:aBtnFClr[ 1 ], This.BACKCOLOR := This.Cargo:aBtnBClr[ 1 ], This.ICON := This.Cargo:aIcon[ 1 ] ) ;
            ACTION {|| This.Enabled := .F., Eval( This.Cargo:bAction ) } ;
            ON INIT {|| This.Cargo := oHmgData() }

      ENDIF

   ELSEIF ! Empty( oBtn:aIcon )
      @ nY, nX BUTTONEX &( cBtn ) WIDTH nW HEIGHT nH CAPTION cCapt ICON oBtn:aIcon[ 1 ] ;
         NOXPSTYLE HANDCURSOR NOTABSTOP ;
         BACKCOLOR oBtn:aGradientOver GRADIENTFILL oBtn:aGradientFill ;
         ON MOUSEHOVER ( This.GradientFill := This.Cargo:aGradientFill, This.ICON := This.Cargo:aIcon[ 2 ] ) ;
         ON MOUSELEAVE ( This.GradientOver := This.Cargo:aGradientOver, This.ICON := This.Cargo:aIcon[ 1 ] ) ;
         ACTION {|| This.Enabled := .F., Eval( This.Cargo:bAction ) } ;
         ON INIT {|| This.Cargo := oHmgData() }

   ELSE
      @ nY, nX BUTTONEX &( cBtn ) WIDTH nW HEIGHT nH CAPTION cCapt ;
         NOXPSTYLE HANDCURSOR NOTABSTOP ;
         BACKCOLOR oBtn:aGradientOver GRADIENTFILL oBtn:aGradientFill ;
         ON MOUSEHOVER ( This.GradientFill := This.Cargo:aGradientFill ) ;
         ON MOUSELEAVE ( This.GradientOver := This.Cargo:aGradientOver ) ;
         ACTION {|| This.Enabled := .F., Eval( This.Cargo:bAction ) } ;
         ON INIT {|| This.Cargo := oHmgData() }

   ENDIF

   oTmp := This.&( cBtn ).Cargo
   FOR EACH aBtn IN oBtn:GetAll() ; oTmp:Set( aBtn[ 1 ], aBtn[ 2 ] ) // перенесли данные в Cargo из oBtn
   NEXT

   IF ! Empty( oBtn:nPost ) .AND. HB_ISBLOCK( oBtn:bEvent ) // ставим событие в окно на кнопку
      ( This.Object ):Event( oBtn:nPost, oBtn:bEvent )
   ENDIF

RETURN This.&( cBtn ).OBJECT

////////////////////////////////////////////////////////////
FUNCTION StartSample1( oBtn )

   LOCAL cForm := ThisWindow.NAME
   LOCAL oForm := ThisWindow.OBJECT
   LOCAL cBtn := This.NAME, oBtnCargo
   LOCAL c2Form, c2Title, a2WinBClr, tTimeStart
   LOCAL nWaitType, nWaitTime, aParam, cBlock
   DEFAULT oBtn := This.OBJECT

   oBtnCargo := oBtn:Cargo

   ? ProcNL()
   ? "Win =", cForm, oForm:Name, oForm:Index, oForm:Handle, oForm:Title, oForm:Cargo
   ? "Btn =", cBtn, oBtn:Name, oBtn:Index, oBtn:Handle, oBtn:Caption
   ? "Cargo", oBtnCargo:GetAll() ; ?v oBtnCargo:GetAll() ; ?

   c2Form     := oBtnCargo:c2Form     // имя нового окна
   c2Title    := oBtnCargo:c2Title    // титул нового окна
   a2WinBClr  := oBtnCargo:a2WinBClr  // цвет фона нового окна
   nWaitType  := oBtnCargo:nWaitType  // тип окна ожидания 1/2/3
   nWaitTime  := oBtnCargo:nWaitTime  // тип показа времени: 0-новое время/1-продолжение времени
   tTimeStart := hb_DateTime()        // время начала процесса

   SetProperty( cForm, cBtn, "Enabled", .F. ) // lock a button
   DoMethod( cForm, "Minimize" ) ; DO EVENTS

   nStaticTimeStart := Seconds()
   Wait1Window( { 1, 25, "50", nWaitTime } )

   aParam := { c2Form, c2Title, a2WinBClr, tTimeStart, nWaitType, nWaitTime }
   cBlock := 'myWinTable( ' + hb_ValToExp( aParam ) + ' )'
   ? "----->", cBlock ; ?

   Eval( hb_macroBlock( cBlock ) )

   SetProperty( cForm, cBtn, "Enabled", .T. ) // unlock a button
   DoMethod( cForm, "Restore" ) ; DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION StartSample2()

   LOCAL cForm := ThisWindow.NAME
   LOCAL oForm := ThisWindow.OBJECT
   LOCAL cBtn := This.NAME
   LOCAL oBtn := This.OBJECT
   LOCAL oBtnCargo := oBtn:Cargo
   LOCAL c2Form, c2Title, a2WinBClr, tTimeStart
   LOCAL nWaitType, nWaitTime, aParam, cBlock

   ? ProcNL()
   ? "Win =", cForm, oForm:Name, oForm:Index, oForm:Handle, oForm:Title, oForm:Cargo
   ? "Btn =", cBtn, oBtn:Name, oBtn:Index, oBtn:Handle, oBtn:Caption
   ? "Cargo", oBtnCargo:GetAll() ; ?v oBtnCargo:GetAll() ; ?

   c2Form     := oBtnCargo:c2Form     // имя нового окна
   c2Title    := oBtnCargo:c2Title    // титул нового окна
   a2WinBClr  := oBtnCargo:a2WinBClr  // цвет фона нового окна
   nWaitType  := oBtnCargo:nWaitType  // тип окна ожидания 1/2/3
   nWaitTime  := oBtnCargo:nWaitTime  // тип показа времени: 0-новое время/1-продолжение времени
   tTimeStart := hb_DateTime()        // время начала процесса

   SetProperty( cForm, cBtn, "Enabled", .F. ) // lock a button
   DoMethod( cForm, "Minimize" ) ; DO EVENTS

   nStaticTimeStart := Seconds()
   IF nWaitType == 2
      Wait2Window( { 1, 30, "60", nWaitTime } )
   ELSE
      Wait3Window( { 1, 30, "60", nWaitTime } )
   ENDIF

   aParam := { c2Form, c2Title, a2WinBClr, tTimeStart, nWaitType, nWaitTime }
   cBlock := 'myWinTable( ' + hb_ValToExp( aParam ) + ' )'
   ? "----->", cBlock ; ?

   Eval( hb_macroBlock( cBlock ) )

   SetProperty( cForm, cBtn, "Enabled", .T. ) // unlock a button
   DoMethod( cForm, "Restore" ) ; DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION Wait1Window( aBeg )

   LOCAL aThread, nI, nJ1, nJ2, cJ3, cVal, nWTime, nTimeRun
   DEFAULT aBeg := { 1, 50, "50", 0 }

   nJ1 := aBeg[ 1 ]
   nJ2 := aBeg[ 2 ]
   cJ3 := aBeg[ 3 ]
   nWTime := aBeg[ 4 ] // тип показа времени: 0-новое время/1-продолжение времени
   IF nWTime == 0
      nTimeRun := Seconds()
   ELSE
      nTimeRun := nStaticTimeStart
   ENDIF

   aThread := WaitThreadGif( 'Create a calculation window ...', nTimeRun )

   FOR nI := nJ1 TO nJ2
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + cJ3
      WaitThreadGifSay( aThread, cVal )
      // final waiting
      // INKEYGUI(100)
   NEXT

   WaitThreadGifClose( aThread )

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION Wait2Window( aBeg )

   LOCAL cWWin, nI, aSay, nJ1, nJ2, cJ3, cVal, nWTime, nTimeRun, cSpc
   DEFAULT aBeg := { 1, 50, "50", 0 }

   nJ1 := aBeg[ 1 ]
   nJ2 := aBeg[ 2 ]
   cJ3 := aBeg[ 3 ]
   nWTime := aBeg[ 4 ] // тип показа времени: 0-новое время/1-продолжение времени
   cSpc := Space( 30 )

   IF nWTime == 0
      nTimeRun := Seconds()
   ELSE
      nTimeRun := nStaticTimeStart
   ENDIF

   aSay := { '00/00' + cSpc + '00:00:00', 'Program download ...', GetExeFileName() }
   cWWin := WaitWindow( aSay, .T., 980, 18, NIL, BLACK, { 247, 172, 8 } ) // open the wait window

   DoMethod( cWWin, "Minimize" ) ; DO EVENTS
   DoMethod( cWWin, "Restore" ) ; DO EVENTS

   // increase label height in 1st row
   SetProperty( cWWin, "Message", "Height", 32 )

   FOR nI := nJ1 TO nJ2
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + cJ3 + cSpc + SECTOTIME( Seconds() - nTimeRun )
      cVal += CRLF + aSay[ 2 ] + CRLF + aSay[ 3 ]
      SetProperty( cWWin, "Message", "Value", cVal )
      // final waiting
      // INKEYGUI(100)
   NEXT

   WaitWindow()

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION Wait3Window( aBeg, lProgressStart )

   LOCAL cWWin, aSay, nJ1, nJ2, cJ3, nWTime, nTimeRun
   LOCAL y, x, w, h, bWait
   DEFAULT aBeg := { 1, 50, "50", 0 }
   DEFAULT lProgressStart := .T.

   IF Len( aBeg ) < 5 ; aBeg := ASize( aBeg, 5 )
   ENDIF

   nJ1    := aBeg[ 1 ]
   nJ2    := aBeg[ 2 ]
   cJ3    := aBeg[ 3 ]
   nWTime := aBeg[ 4 ] // тип показа времени: 0-новое время/1-продолжение времени
   bWait  := aBeg[ 5 ] // вып. блок кода в окне WaitWindow

   IF nWTime == 0
      nTimeRun := Seconds()
   ELSE
      nTimeRun := nStaticTimeStart
   ENDIF

   DEFAULT bWait := {| cw, nt, ap | // {|cForm, nTimeRun, {nJ1, nJ2, nJ3}| ... }
      LOCAL nI, nK, cVal
      LOCAL cSpc := Space( 60 )
      FOR nI := ap[ 1 ] TO ap[ 2 ]
         cVal := hb_ntos( nI ) + "/" + ap[ 3 ] + cSpc + SECTOTIME( Seconds() - nt )
         SetProperty( cw, "Message4", "Value", cVal )
         nK := GetProperty( cw, "Progress", "Value" )
         SetProperty( cw, "Progress", "Value", nK + 1 )
         IF InkeyGUI( 1 * 500 ) == VK_ESCAPE
            EXIT
         ENDIF
      NEXT

      RETURN nI
      }

   aSay := { 'Program download ...', GetExeFileName(), '', '' }
   cWWin := WaitWindow( aSay, .T., 800, 18, NIL, BLACK, { 247, 172, 8 }, 20, BLUE, 4 ) // open the wait window

   DoMethod( cWWin, "Minimize" ) ; DO EVENTS
   DoMethod( cWWin, "Restore" ) ; DO EVENTS

   SET WINDOW THIS TO cWWin

   h := This.Message2.HEIGHT - 6
   This.Message.FONTCOLOR := BLUE
   This.Message.FONTBOLD := .T.
   This.Message3.Hide

   y := This.Message2.ROW + This.Message2.HEIGHT
   x := This.Message2.COL
   w := This.Message2.WIDTH

   IF lProgressStart ; nStaticProgressStart := 0
   ENDIF

   @ y, x PROGRESSBAR PROGRESS OF &( cWWin ) RANGE 0, nJ2 WIDTH w HEIGHT h

   nJ1 := nStaticProgressStart
   This.Progress.VALUE := nJ1
   DO EVENTS

   Eval( bWait, cWWin, nTimeRun, { nJ1, nJ2, cJ3 } )

   nStaticProgressStart := GetProperty( cWWin, "Progress", "Value" )

   IF lProgressStart .AND. nStaticProgressStart != nJ2
      SetProperty( cWWin, "Progress", "Value", nJ2 )
   ENDIF

   SET WINDOW THIS TO

   WaitWindow()

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myWinTable( aParam )

   LOCAL nWinWidth, nWinHeight, cFormName, cFormTitle, aBackColor, tTimeStart
   LOCAL nWaitType, nWaitTime

   cFormName  := aParam[ 1 ]
   cFormTitle := aParam[ 2 ]
   aBackColor := aParam[ 3 ]
   tTimeStart := aParam[ 4 ]
   nWaitType  := aParam[ 5 ] // тип окна ожидания 1/2/3
   nWaitTime  := aParam[ 6 ] // тип показа времени: 0-новое время/1-продолжение времени
   nWinWidth  := System.DesktopWidth
   nWinHeight := System.DesktopHeight - 30
   ? ProcNL(), "aParam=", hb_ValToExp( aParam )

   IF ! _IsWindowActive( cFormName )

      // чистка памяти, уборка мусора / memory cleaning, garbage collection
      DO EVENTS ; hb_gcAll() ; DO EVENTS

      SET FONT TO "Tahoma", 22

      DEFINE WINDOW &cFormName AT 0, 0 ;
            WIDTH nWinWidth HEIGHT nWinHeight ;
            TITLE cFormTitle ;
            WINDOWTYPE STANDARD TOPMOST ;
            NOMAXIMIZE NOSIZE ;
            BACKCOLOR aBackColor ;
            ON INIT {|| myLogForms( { This.NAME, "init", 1 } ), DoEvents(), _wPost( 1 ) } ;
            ON RELEASE {| w | w := This.NAME, myLogForms( { w, "release", 1 } ), ;
            DoMethod( "Form_Main", "Restore" ), ;
            myLogForms( { w, "release", 2 } ) }

         @ 50, 20 BUTTON oBut_Help CAPTION "Info" WIDTH 200 HEIGHT 68 ACTION _wPost( 3 )

         @ 50, 300 BUTTON oBut_Exit CAPTION "Exit" WIDTH 200 HEIGHT 68 ACTION _wPost( 99 )

         @ 220, 0 LABEL Label_1 VALUE "Calculation in progress" WIDTH nWinWidth HEIGHT 60 ;
            SIZE 26 FONTCOLOR BLACK BOLD TRANSPARENT CENTERALIGN

         WITH OBJECT This.OBJECT
            :Event( 0, {|| InkeyGui( 200 ) } ) // just as an example

            :Event( 1, {| ow | // ON INIT windows + close the "calculation" window
               LOCAL cMsg, Ctrl
               This.Topmost := .F.
               DoMethod(ow:Name, "DisableUpdate")  // блокировать всю форму
                                                               // block the whole form
               FOR EACH Ctrl IN HMG_GetFormControls( ow:Name )
                  IF "oBut" $ Ctrl
                     SetProperty( ow:Name, Ctrl, "Enabled", .F. )
                  ENDIF
               NEXT
               IF nWaitType == 1
                  Wait1Window( { 26, 50, "50", nWaitTime } )
               ELSEIF nWaitType == 2
                  Wait2Window( { 31, 60, "60", nWaitTime } )
               ELSEIF nWaitType == 3
                  Wait3Window( { 30, 60, "60", nWaitTime }, .F. )
               ELSE
                  MsgDebug( "Error! No processing nWaitType=", nWaitType )
               ENDIF
               cMsg := "Elapsed processing time - " + HMG_TimeMS( tTimeStart )
               ? ProcNL(), cMsg
               This.Label_1.VALUE := cMsg
               DoMethod( ow:Name, "oBut_Help", "SetFocus" )
               DO EVENTS
               myLogForms( { ow:Name, "init", 2 } )
               FOR EACH Ctrl IN HMG_GetFormControls( ow:Name )
                  IF "oBut" $ Ctrl
                     SetProperty( ow:Name, Ctrl, "Enabled", .T. )
                  ENDIF
               NEXT
               DoMethod(ow:Name, "EnableUpdate")    // разблокировка всей формы
                                                               // unlock the whole form
               RETURN NIL
               } )

            :Event( 3, {|| // INFO button
               LOCAL aForm := HMG_GetForms()
               LOCAL nI, nK := Len( aForm )
               LOCAL cForm, hHandle, cInf := ""
               hHandle := ThisWindow.Handle
               This.oBut_Help.Enabled := .F.
               FOR nI := 1 TO nK
                  cForm := aForm[ nI ]
                  cInf += Str( nI, 4 ) + "  [" + PadC( _HMG_aFormType[ nI ], 3 ) + "]  "
                  cInf += cValToChar( _HMG_aFormDeleted[ nI ] ) + "  "
                  cInf += PadR( hb_ntos( _HMG_aFormHandles[ nI ] ), 15 ) + PadR( cForm, 20 )
                  cInf += "  Visible=" + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
                  cInf += " , " + cValToChar( GetProperty( cForm, "Visible" ) ) + ";"
               NEXT
               AlertInfo( cInf )
               This.oBut_Help.Enabled := .T.
               This.oBut_Help.Setfocus
               RETURN NIL
               } )

            :Event( 99, {| ow | ow:Release() } )
         END WITH

      END WINDOW

      DoMethod( cFormName, "Center" )
      ACTIVATE WINDOW &cFormName ON INIT {|| This.Minimize, DoEvents(), ;
         This.Restore, DoEvents() }

   ELSE

      ? "===> " + ProcNL(), "SwitchToWin(" + cFormName + ")"
      SwitchToWin( cFormName )
      DO EVENTS

   ENDIF // !IsWindowActive()

   // чистка памяти, уборка мусора / memory cleaning, garbage collection
   DO EVENTS ; hb_gcAll() ; DO EVENTS

RETURN NIL

/////////////////////////////////////////////////
FUNCTION SwitchToWin( cForm )

   IF _IsWindowDefined( cForm )
      IF IsIconic( GetFormHandle( cForm ) )
         _Restore( GetFormHandle( cForm ) )
      ELSE
         DoMethod( cForm, "SetFocus" )
      ENDIF
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION myLogForms( uTxt, lOut )

   LOCAL aForm := HMG_GetForms() // все открытые окна программы / all open program windows
   LOCAL nI, nK := Len( aForm )
   LOCAL cForm
   DEFAULT lOut := .T.
   // если надо отключить вывод в log, ставить lOut := .F. тут (не меняя текст программы)
   // if you need to disable output to log, set lOut := .F. here (without changing the text of the program)
   IF Empty( lOut ) ; RETURN NIL
   ENDIF

   IF ! Empty( uTxt )
      ? "====", ProcNL(), hb_ValToExp( uTxt )
   ENDIF
   FOR nI := 1 TO nK
      cForm := aForm[ nI ]
      ? Str( nI, 4 ), _HMG_aFormType[ nI ], _HMG_aFormDeleted[ nI ], _HMG_aFormHandles[ nI ], cForm
      ?? "Visible=", IsWindowVisible( GetFormHandle( cForm ) ), GetProperty( cForm, "Visible" )
   NEXT
   ? "==== end ===", ProcNL() ; ?

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL( nVal )
   DEFAULT nVal := 0
RETURN "Call from: " + ProcName( nVal + 1 ) + "(" + ;
       hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )
