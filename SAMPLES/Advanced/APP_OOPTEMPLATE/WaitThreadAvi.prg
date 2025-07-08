/*
 * MINIGUI - Harbour Win32 GUI library
 * ������ � Harbour / Streams in Harbour
 * ������ � MiniGui / Streams in MiniGui
 * �������� � MiniGui / Preloader in MiniGui
 * ������ � AVI-�������� / Working with a AVI object
 *
 * Copyright 2015-24 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2015 Grigory Filatov <gfilatov@inbox.ru>
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
*/
#include "hmg.ch"
#include "hbthread.ch"

/* � ���� � ��������� ����� ������ ���: / In your program you can do this:

   nTime := SECONDS()
   // ������ ���� �������� � �������
   aThread := WaitThreadAvi( '������ ���������� ...' )

   // �������� ���� ����������/��������/�������� ��� � �.�.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "50"
      WaitThreadAviSay( aThread, cVal )
      ...................
   NEXT

   WaitThreadAviClose( aThread )

   cMsg := "������ ������� ��������� �� " + SECTOTIME( SECONDS() - nTime )
   MsgInfo(cMsg)
-----------------------------------------------------------------------------
����� � ���:
   WaitThreadAvi( '������ ���������� ...' )   // ������ ���� �������� � �������

   // �������� ���� ����������/��������/�������� ��� � �.�.
   ...................
   WaitThreadGifClose()   // ������� ���� "��������"
*/
// for the start of the cycle in the window "waiting"
STATIC cStatWinWait              // ��� ����
STATIC lStatWinWait   := .T.     // ��� ������ ������������ ����� preloding � ������
STATIC nStaticSeconds := 0       // ����� ������ ��������� ��� WaitThreadAviTimer()
STATIC nStatWinWidth             // ������ ������ ����
//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadAvi( cTitle, tRunTime, lFocus, nAvi, aNew )
   LOCAL cFormName := "WaitWin_" + HB_NtoS( _GetId() )
   LOCAL cFont := 'Tahoma', nFontSize := 16, aFontColor
   LOCAL lFor  := .T., lModal := .T. , aResAvi, aParam
   LOCAL nWTxt, nG, nW, nH, cWnd, hWnd, cFoc, aBackColor, nY, nX, aImgWH
   DEFAULT cTitle := "��������...", tRunTime := hb_DateTime()
                                 // tRunTime - ����������� ������� ������ � ����
   DEFAULT lFocus := .F., nAvi := 1, aNew := {}

   IF lFocus
      cWnd := ThisWindow.Name            // ������� ���� ���� ��� ����
      hWnd := ThisWindow.Handle          // �����  - ������� ����
      cFoc := ThisWindow.FocusedControl  // ������� � ������ - ������� ����
   ENDIF

   // ����: �������, ����
   nW         := 420
   nH         := 230
   nG         := 20
   aBackColor := WHITE
   aFontColor := BLACK
   aResAvi    := "Avi3dMan"   // Resource\res\man3d128.avi
   IF nAvi == 2
     aResAvi    := "ZIPAVI"   // Resource\res\a_compress.avi
     aBackColor := SILVER
   ENDIF
   IF LEN(aNew) > 0
     aResAvi    := aNew[1]   
     aBackColor := aNew[2]
     aFontColor := aNew[3]
   ENDIF

   aImgWH := GetAviResSize(aResAvi)
   IF aImgWH[1] == NIL .OR. aImgWH[2] == NIL
      MsgDebug("ERROR! NO RESOURCES in EXE file!", aResAvi, aImgWH)
      RETURN NIL
   ENDIF

   // �������������
   cStatWinWait   := cFormName
   lStatWinWait   := .T.        // ��� ������ ������������ ����� preloding � ������
   nStaticSeconds := SECONDS()  // ����� ������ ��������� ��� WaitThreadAviTimer()

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
      lModal := _HMG_ThisType == "M"       // ��� ����� ?
   ENDIF

   SetCursorSystem( IDC_WAIT )

IF lModal
   DEFINE WINDOW &cFormName          ;
     ROW 0 COL 0 WIDTH nW HEIGHT nH  ;
     TITLE ''                        ;
     MODAL NOCAPTION NOSIZE          ;
     BACKCOLOR aBackColor            ;
     FONT cFont SIZE nFontSize       ;
     ON MOUSECLICK MoveActiveWindow()
ELSE
   DEFINE WINDOW &cFormName          ;
     ROW 0 COL 0 WIDTH nW HEIGHT nH  ;
     TITLE ''                        ;
     CHILD TOPMOST                   ; // ���� �� �������� ����
     NOSIZE NOMAXIMIZE NOMINIMIZE NOSYSMENU NOCAPTION ;
     BACKCOLOR aBackColor            ;
     FONT cFont SIZE nFontSize       ;
     ON MOUSECLICK MoveActiveWindow()
ENDIF

     // ��������� ����������� ����
     This.OnInit    := {|| DoEvents(), This.Topmost := .T. , _wPost(0) }   // ����������� ����� ������������� ����
     This.OnRelease := {|| _wPost(90)                                  }   // ����������� ����� ����������� ����  
     //This.OnInterActiveClose := {|| This.Cargo:lClose      }

     // ��������� ��������� �� ����
     This.Cargo := oHmgData()
     This.Cargo:lClose := .T.

     nY := nX := nG
     nW := This.ClientWidth
     nH := This.ClientHeight
     nStatWinWidth := nW

     @ nY, nX LABEL Label_1  WIDTH nW - nG*2 HEIGHT nFontSize*2  ;
       FONTCOLOR aFontColor VALUE "������ 00:00:00.000" CENTERALIGN VCENTERALIGN TRANSPARENT

     @ nY, nX LABEL Label_0 WIDTH 100 HEIGHT nFontSize*2 VALUE "" FONTCOLOR aFontColor VCENTERALIGN TRANSPARENT
     nY += This.Label_1.Height + nG

     nX := ( nW - aImgWH[1] ) / 2

     @ nY, nX ANIMATEBOX Avi_1 WIDTH aImgWH[1] HEIGHT aImgWH[2] File aResAvi AUTOPLAY ;
       TRANSPARENT BACKCOLOR aBackColor NOBORDER

     nY += This.Avi_1.Height + nG

     @ nY, 5 LABEL Label_2 WIDTH nW - 5*2 HEIGHT nFontSize*2 VALUE cTitle  ;
       FONTCOLOR aFontColor CENTERALIGN VCENTERALIGN TRANSPARENT

     nY += This.Label_2.Height //+ nG
     nH := nY + GetBorderHeight()*2 
     // ���������� ������� ������ ����, ��������� �, ��� �������
     // set the outer height of the window, reduce it, for beauty
     This.Height := nH

     DRAW LINE IN WINDOW &cFormName AT 0, 0 TO  0,nW PENCOLOR RED PENWIDTH 2
     DRAW LINE IN WINDOW &cFormName AT nH,0 TO nH,nW PENCOLOR RED PENWIDTH 2
     DRAW LINE IN WINDOW &cFormName AT 0, 0 TO nH, 0 PENCOLOR RED PENWIDTH 2
     DRAW LINE IN WINDOW &cFormName AT 0,nW TO nH,nW PENCOLOR RED PENWIDTH 2

     (This.Object):Event( 0, {|ow| _logfile(.t.,"   ---[ :Event(0) ]--- " + ProcNL() ) ,;
                                   SetWaitCursor( ow:Handle ) ,;
                                   DoEvents() })
 
     (This.Object):Event(90, {|ow| _logfile(.t.,"   ---[ :Event(90) ]--- " + ProcNL(),;
                                   ">>> RELEASE: " + ow:Name ) , InkeyGUI(50)            })
     (This.Object):Event(99, {|ow| ow:Cargo:lClose := .T., ow:Release() })

   END WINDOW

     Center Window &cFormName
   Activate Window &cFormName NOWAIT

   ? "===[] CREATE FORM -> cFormName=", cFormName, "lModal=", lModal, ProcNL() 

   DO EVENTS
   // Start preloding in a separate thread
   // ��������� preloding � ��������� ������
   aParam := { cFormName, tRunTime, nW, nH }
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @WaitThreadAviTimer(), aParam ) )
   InkeyGUI( 100 )
   DO EVENTS

   IF lFocus
      If ! empty(cWnd) .and. _IsWindowDefined(cWnd)
         ? "===[] Return focus from where the call originated: cWnd=", cWnd, hWnd, ProcNL() 
         BringWindowToTop( hWnd )   // ������� �� ������� ����
         // ��� ��� ������, � ����� � �� � ������
         //Domethod(cWnd, "Minimize")
         //Domethod(cWnd, "Restore" )
         DO EVENTS
         IF ! empty(cFoc)                       // setfocus ��� ����
            DoMethod(cWnd, cFoc, 'SetFocus')
         ENDIF
      Endif
   ENDIF

   DO EVENTS

RETURN aParam

//////////////////////////////////////////////////////////////////////
// ��������� ������� � ������ / complete function in the stream
FUNCTION WaitThreadAviClose(aDim)
   LOCAL cFormName, cMsg
   DEFAULT aDim := {}

   IF !hb_IsArray(aDim)
      cMsg := "aDim is not an array! To correct !;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      aDim := {}
      AlertStop(cMsg)
   ENDIF

   IF LEN(aDim) == 0
      cFormName := cStatWinWait
   ELSE
      cFormName := aDim[ 1 ]
   ENDIF

   ? "===[] CLOSE FORM -> cFormName=", cFormName, ProcNL(), "aDim=", HB_ValToExp(aDim)

   // ��������� ������� � ������ / complete function in the stream
   lStatWinWait := .F.
   InkeyGui(100)

   SET INTERACTIVECLOSE ON

   Domethod(cStatWinWait,"Release")

   SetCursorSystem( IDC_ARROW )

   DO MESSAGE LOOP

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadAviTimer(aDim)
   LOCAL cFormName, tStart, cVal

   cFormName := aDim[ 1 ]
   tStart    := aDim[ 2 ]     
   cFormName := cStatWinWait  

   DO WHILE lStatWinWait
      IF ABS( SECONDS() - nStaticSeconds ) >= 0.05

         cVal := "������ " + HMG_TimeMS( tStart ) 
         SetProperty( cFormName, "Label_1", "Value", cVal )

         InkeyGui(50)
         nStaticSeconds := SECONDS()
         DO EVENTS

      ENDIF
   ENDDO

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// ����� ���.���������� / output of additional information
FUNCTION WaitThreadAviSay( aDim, cVal )
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
           cFontName := _HMG_DefaultFontName,  ;   // �� MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // �� MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ������ ������
   DeleteObject (hFont)

RETURN nWidth

