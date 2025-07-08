/*
 * Copyright 2018, Verchenko Andrey <verchenkoag@gmail.com>
 * Tips and tricks programmers from our forum http://clipper.borda.ru
 *
 * ѕоказ простого прелодера. ќтображение процесса загрузки: индекса, расчЄтов и т.д.
 * Showing a simple preloader. Displays the boot process: index, calculations, etc.
*/

#include "minigui.ch"
#include "hbthread.ch" 

// дл€ старта цикла в окне "ожидани€"/ for the start of the cycle in the window "waiting"
STATIC lStatWinWait  := .T. 
// массив картинок дл€ окна "ожидани€" / an array of images to window "waiting"
STATIC aStatPictWait := {"zmk01","zmk02","zmk03","zmk04","zmk05","zmk06","zmk07","zmk08"}
// номер картинки из массива
STATIC nStaticNum

STATIC nStaticSeconds := 0, nStaticAllSeconds, aStaticParam
//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadCreateIcon( cTitle, cIndicator ) 
   LOCAL nMaxWidth, nMaxHeight, nFWidth, nFHeight, lFlagTimer
   LOCAL cFormName := "WaitWin_" + HB_NtoS( _GetId() ) 
   LOCAL cFont := _HMG_DefaultFontName, nFSize := 14
   LOCAL nIRow, nICol, nIWH, nRow, nCol, nGapsH, nWLbl
   LOCAL  nCol2, nWLbl2
   DEFAULT cTitle := "Working...", cIndicator := "00:00:00"

   IF !hb_mtvm()
      MsgStop ("No support for multi-threading!" + CRLF + CRLF + ;
                "Compiling with a key /mt" + CRLF )
      RETURN NIL
   ENDIF

   lStatWinWait := .T.             // запуск бесконечного цикла
   nStaticNum := 1                 // номер картинки из массива
   nStaticAllSeconds := SECONDS()  // врем€ старта прелодера

   lFlagTimer := IIF( AT("00:00:00", cIndicator ) > 0, .T., .F. )

   nWLbl := GetTxtWidth( cTitle, nFSize, cFont )
   nWLbl := IIF( nWLbl < 250, 250, nWLbl )
   nMaxWidth := 20 + 64 + 20 + nWLbl + 20

   SET INTERACTIVECLOSE OFF

   DEFINE WINDOW &cFormName     ;
     CLIENTAREA nMaxWidth, 104  ;
     MODAL NOCAPTION            ;
     BACKCOLOR WHITE            ;
     FONT cFont SIZE nFSize     ;
     ON MOUSECLICK MoveActiveWindow() 

     nMaxWidth  := GetProperty( cFormName, "Width"  )
     nMaxHeight := GetProperty( cFormName, "Height" )

     SetProperty( cFormName, "MinWidth"  , nMaxWidth  )
     SetProperty( cFormName, "MinHeight" , nMaxHeight )

     SetProperty( cFormName, "MaxWidth"  , nMaxWidth  )
     SetProperty( cFormName, "MaxHeight" , nMaxHeight )

     nFWidth  := This.ClientWidth   
     nFHeight := This.ClientHeight

     DRAW ICON IN WINDOW &cFormName AT 20, 20 PICTURE aStatPictWait[1] WIDTH 64 HEIGHT 64 TRANSPARENT
     nIRow := 20 ; nICol := 20 ; nIWH := 64

     nGapsH := (nFHeight - nFSize * 2) / 3    // рассто€ние между label по вертикали

     nRow  := nGapsH
     nCol  := 20 + 64 + 20
     nWLbl := nFWidth - nCol

     @ nRow, nCol LABEL Label_1 WIDTH nWLbl HEIGHT nFSize*2 VALUE cTitle ;
       CENTERALIGN VCENTERALIGN TRANSPARENT

     nRow   := nGapsH * 2 + nFSize
     nWLbl2 := GetTxtWidth( cIndicator, nFSize, cFont ) + 20
     nCol2  := nCol + ( nWLbl - nWLbl2 ) / 2 
     @ nRow, nCol2 LABEL Label_2 WIDTH nWLbl2 HEIGHT nFSize*2 VALUE cIndicator ;
       CENTERALIGN VCENTERALIGN TRANSPARENT

     IF lFlagTimer
        // включить таймер дл€ отображени€ времени каждую секунду
        DEFINE TIMER Timer_1 INTERVAL 1000 ACTION TimerIconShow()
     ENDIF

   END WINDOW

   Center Window &cFormName
   Activate Window &cFormName NoWait

   // передача параметров ниже
   aStaticParam := { cFormName, nIRow, nICol, nIWH, lFlagTimer }

   // Start preloding in a separate thread
   // «апускаем preloding в отдельном потоке
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @WaitThreadTimerIcon(), Nil ) )

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadCloseIcon()
   LOCAL cFormName := aStaticParam[1]

   //IF _IsWindowActive( cFormName )
      // завершить функцию в потоке / complete function in the stream
      lStatWinWait := .F.  

      InkeyGui(10) 

      SET INTERACTIVECLOSE ON

      Domethod(cFormName,"Release")

      DO MESSAGE LOOP
   //ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadTimerIcon()
   LOCAL cFormName, nRow, nCol, nWH //, lFlagTimer, cTime

   cFormName  := aStaticParam[1]
   nRow       := aStaticParam[2]
   nCol       := aStaticParam[3]
   nWH        := aStaticParam[4]

   DO WHILE lStatWinWait
      IF ABS( SECONDS() - nStaticSeconds ) >= 0.1

         nStaticNum++
         nStaticNum := IIF( nStaticNum > LEN(aStatPictWait), 1, nStaticNum )
         DRAW ICON IN WINDOW &cFormName AT nRow, nCol PICTURE aStatPictWait[nStaticNum] ;
                   WIDTH nWH HEIGHT nWH TRANSPARENT

         nStaticSeconds := SECONDS()
         DO EVENTS
         //INKEYGUI(10)

      ENDIF
   ENDDO

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TimerIconShow()
    LOCAL cFormName, cTime

    cFormName := aStaticParam[1]

    IF _IsWindowActive( cFormName )

        cTime := SECTOTIME( SECONDS() - nStaticAllSeconds )
        SetProperty( cFormName, "Label_2", "Value", cTime )
 
    ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadSayIcon(cTitle, cIndicator)
    LOCAL cFormName 
    DEFAULT cTitle := "", cIndicator := ""

    cFormName := aStaticParam[1]

    IF _IsWindowActive( cFormName )
       IF !Empty( cTitle ) 
         SetProperty( cFormName, "Label_1", "Value", cTitle     )
       ENDIF
       IF !Empty( cIndicator ) 
         SetProperty( cFormName, "Label_2", "Value", cIndicator )
       ENDIF
    ENDIF

RETURN NIL


/////////////////////////////////////////////////
#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

STATIC PROCEDURE MoveActiveWindow( hWnd )
   DEFAULT hWnd := GetActiveWindow()

   PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )

   RC_CURSOR( "MINIGUI_FINGER" )

RETURN

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName )  // получить width текста
LOCAL hFont, nWidth

 IF valtype(cText) == 'N'
    cText := repl('A', cText)
 ENDIF

 DEFAULT cText     := repl('A', 2),          ;
         cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
         nFontSize := _HMG_DefaultFontSize       // из MiniGUI.Init()

 hFont  := InitFont(cFontName, nFontSize)
 nWidth := GetTextWidth(0, cText, hFont)
 
 DeleteObject (hFont)                    

RETURN nWidth

