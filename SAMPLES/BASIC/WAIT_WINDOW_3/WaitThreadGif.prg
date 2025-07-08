/*
 * MINIGUI - Harbour Win32 GUI library
 * ������ � Harbour / Streams in Harbour
 * ������ � MiniGui / Streams in MiniGui
 * �������� � MiniGui / Preloader in MiniGui
 * ������ � Gif-�������� / Working with a Gif object
 *
 * Copyright 2015-24 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2015 Grigory Filatov <gfilatov@inbox.ru>
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
*/
#include "hmg.ch"
#include "hbthread.ch"

/* � ���� � ��������� ����� ������ ���: / In your program you can do this:

   nTime := SECONDS()
   // ������ ���� �������� � �������
   aThread := WaitThreadGif( '������ ���������� ...' )

   // �������� ���� ����������/��������/�������� ��� � �.�.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "50"
      WaitThreadGifSay( aThread, cVal )
      ...................
   NEXT

   WaitThreadGifClose( aThread )

   cMsg := "������ ������� ��������� �� " + SECTOTIME( SECONDS() - nTime )
   MsgInfo(cMsg)
-----------------------------------------------------------------------------
����� � ���:

   WaitThreadGif( '������ ���������� ...' )  // ������ ���� �������� � �������

   // �������� ���� ����������/��������/�������� ��� � �.�.
   ...................

   WaitThreadGifClose()   // ������� ���� "��������"
*/
// for the start of the cycle in the window "waiting"
STATIC cStatWinWait              // ��� ����
STATIC lStatWinWait   := .T.     // ��� ������ ������������ ����� preloding � ������
STATIC nStaticSeconds := 0       // ����� ������ ��������� ��� WaitThreadGifTimer()
STATIC oStaticGif                // ��� �������� GIF
STATIC nStatWinWidth             // ������ ������ ����
//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadGif( cTitle, nTimeRun, lFocus )
   LOCAL cFormName := "WaitWin_" + HB_NtoS( _GetId() )
   LOCAL cFont := 'Tahoma', nFontSize := 12
   LOCAL lFor  := .T., lModal := .T. , aResGif := "Ani3dMan128"
   LOCAL nWTxt, nW, nH, cWnd, hWnd, cFoc, aParam
   DEFAULT cTitle := "��������...", nTimeRun := Seconds(), lFocus := .F.
                                 // nTimeRun - ����������� ������� ������ � ����

   IF lFocus
      cWnd := ThisWindow.Name            // ������� ���� ���� ��� ����
      hWnd := ThisWindow.Handle          // �����  - ������� ����
      cFoc := ThisWindow.FocusedControl  // ������� � ������ - ������� ����
   ENDIF
   // ������� ����
   nW := 420
   nH := 230
   cStatWinWait   := cFormName
   lStatWinWait   := .T.        // ��� ������ ������������ ����� preloding � ������
   nStaticSeconds := SECONDS()  // ����� ������ ��������� ��� WaitThreadGifTimer()

   // ������ ������� ������, ����� ������� ���������� � ������
   DO WHILE lFor
      nWTxt := GetTxtWidth( cTitle , nFontSize, cFont) + 40
      IF nWTxt > nW    // ������ ����
         nFontSize--
      ELSE
         lFor := .F.
      ENDIF
   ENDDO

   SET INTERACTIVECLOSE OFF

   IF !Empty( _HMG_ThisFormName )          // ������� ���� ����
      lModal := ( _HMG_ThisType == "M" )   // ��� ����� ?
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
     CHILD NOCAPTION TOPMOST         ; // ���� �� �������� ����
     BACKCOLOR WHITE                 ;
     FONT cFont SIZE nFontSize       ;
     ON MOUSECLICK MoveActiveWindow()
ENDIF

     // ��������� ����������� ����
     This.OnInit    := {|| DoEvents(), This.Topmost := .T. , _wPost(0) }   // ����������� ����� ������������� ����
     This.OnRelease := {|| _wPost(90)                                  }   // ����������� ����� ����������� ����
     This.OnInterActiveClose := {|| This.Cargo:lClose }

     // ��������� ��������� �� ����
     This.Cargo := oHmgData()
     This.Cargo:lClose := .T.

     nW := This.ClientWidth
     nH := This.ClientHeight
     nStatWinWidth := nW

     @ 10,10 LABEL Label_1  WIDTH nW - 10*2 HEIGHT 33  ;
       VALUE "������ 00:00:00" CENTERALIGN VCENTERALIGN TRANSPARENT

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
   // ��������� preloding � ��������� ������
   aParam := { cFormName, nTimeRun, nW, nH }
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @WaitThreadGifTimer(), aParam ) )

   IF lFocus
      If ! empty(cWnd) .and. _IsWindowDefined(cWnd)
         BringWindowToTop( hWnd )   // ������� �� ������� ����
         // ��� ��� ������, � ����� � �� � ������
         //Domethod(cWnd, "Minimize")
         //Domethod(cWnd, "Restore" )
         DO EVENTS
         IF ! Empty(cFoc)                       // setfocus ��� ����
            DoMethod(cWnd, cFoc, 'SetFocus')
         ENDIF
      Endif
   ENDIF

   DO EVENTS

RETURN aParam

//////////////////////////////////////////////////////////////////////
// ��������� ������� � ������ / complete function in the stream
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
   cFormName := cStatWinWait  // ��� ���

   DO WHILE lStatWinWait
      IF ABS( SECONDS() - nStaticSeconds ) >= 0.05

         cTime := "������ " + SECTOTIME( SECONDS() - nTime )
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
// ����� ���.���������� / output of additional information
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
// �������� Width ������
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
   nWidth := GetTextWidth(0, cText, hFont)         // ������ ������
   DeleteObject (hFont)

RETURN nWidth
