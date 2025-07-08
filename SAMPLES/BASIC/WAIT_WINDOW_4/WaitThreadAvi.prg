/*
 * MINIGUI - Harbour Win32 GUI library
 * ������ � Harbour / Streams in Harbour
 * ������ � MiniGui / Streams in MiniGui
 * �������� � MiniGui / Preloader in MiniGui
 * ������ � AVI-�������� � ������ / Working with an AVI object in a stream
 * ������ � ������� TThrData (������ � ������� � ���������� ���� � ������������)
 * Working with the TThrData class (access to variables in threads comes with locks)
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
   cWin := WaitThreadAvi( '������ ���������� ...' )

   // �������� ���� ����������/��������/�������� ��� � �.�.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "50"
      _wSend("@Say", cNWin, cVal)
      // _wSend(3, cNWin, cVal)        // ��� ���
      ...................
   NEXT

   WaitThreadAvi()

   cMsg := "������ ������� ��������� �� " + SECTOTIME( SECONDS() - nTime )
   MsgInfo(cMsg)
-----------------------------------------------------------------------------
����� � ���:
   WaitThreadAvi( '������ ���������� ...' )   // ������ ���� �������� � �������

   // �������� ���� ����������/��������/�������� ��� � �.�.
   ...................
   WaitThreadAvi()   // ������� ���� "��������"
*/
MEMVAR o_Thr
////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION WaitThreadAvi( cMessage, tRunTime, lFocus, cResAvi, aBackColor, aFontColor, nWidth )
   LOCAL cFormName, lDefined, cFont, nFontSize, cMsg, lFor, lDebug, cPassed, cFormCurr
   LOCAL nY, nX, nG, nW, nH, cFocus, nWTxt, aImgWH, nHLine, aParam, oWnd, ow, o

   cFormName := "_HMG_MODAL_WaitThreadAvi" //+ HB_NtoS( _GetId() )
   lDefined  := _IsWindowDefined( cFormName )
   lDebug    := App.Cargo:lDebug  ; Default lDebug := .F.

   hb_default( @cMessage, "Expectation..." )   // "��������..."
   hb_default( @tRunTime, hb_DateTime() )      // ����������� ������� ������ � ����
   hb_default( @lFocus, .F. )                  // .T.-������� �����, ������ ���� ������� ����
   hb_default( @cResAvi, "Avi3dMan128" )
   hb_default( @aFontColor, BLACK )
   hb_default( @aBackColor, WHITE )
   hb_default( @nWidth, 0 )

   // ������������� o_Thr
   o_Thr:cFormName := cFormName           // ��� ����
   o_Thr:tRunTime  := tRunTime            // ����� ������� ��� WaitWinAviSay()
   o_Thr:nSeconds  := SECONDS()           // ����� ������ ��������� ��� WaitWinAviSay()
   o_Thr:lWinWait  := .T.                 // ��� ������ ������������ ����� preloding � ������
   o_Thr:cPassed   := "Time has passed"   // "������"
   o_Thr:cMsg0     := ""                  // ���  Label_0
   o_Thr:cMsg1     := ""                  // ���  Label_1
   o_Thr:lDebug    := lDebug              // ������� � ���-����

   IF lFocus
      cFormCurr := _HMG_THISFORMNAME
      oWnd      := _WindowObj( cFormCurr )    // oWnd - parent window object
      cFocus    := oWnd:FocusedControl        // ������� � ������ - ������� ����
      o_Thr:cFocus := cFocus
      o_Thr:cFormCurr := cFormCurr
      o_Thr:oWndCurr  := oWnd
   ELSE
      o_Thr:cFormCurr := ""
      o_Thr:lFocus    := lFocus
      o_Thr:cFocus    := ""
   ENDIF

   IF lDefined

      IF pCount() == 0                       // close wait window
         // ��������� ������� � ������
         // complete function in the stream
         WaitThreadAviClose()
      ELSE                                   // Say cMessage value
        IF !HB_ISCHAR( cMessage )
           cMessage := cValToChar( cMessage )
        ENDIF
        ow := _WindowObj( cFormName )       // object _HMG_MODAL_WaitWinAvi window
        _wSend(2, ow, cMessage)
      ENDIF
      DO EVENTS

      RETURN cFormName

   ENDIF

   cFont     := 'Tahoma'
   nFontSize := 16
   cPassed   := "Time has passed"   // "������"
   aImgWH    := GetAviResSize(cResAvi)

   IF aImgWH[1] == NIL .OR. aImgWH[2] == NIL
      cMsg := "Error! There is no resource [" + cResAvi + "] in the exe-file !;"
      cMsg += "Dimensions not defined: { Nil, Nil }"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg, App.ExeName)
      IF lDebug   ;  ? "===[] " + ProcNL(), cMsg
      ENDIF
      DO EVENTS
      RETURN ""  // ������ ������ - ��� ���� / empty string - window name
   ENDIF

   // ����: �������
   nW := 420
   nW := IIF( nWidth > 0 .AND. nWidth > nW, nWidth, nW )
   nH := 230
   nG := 20

   // ������ ������� ������, ����� ������� ���������� � ����
   lFor := .T.
   DO WHILE lFor
      nWTxt := GetTxtWidth( cMessage , nFontSize, cFont) + 10
      IF nWTxt > nW    // ������ ����
         nFontSize--
      ELSE
         lFor := .F.
      ENDIF
   ENDDO

   SetCursorSystem( IDC_WAIT )

   DEFINE WINDOW &cFormName ROW 0 COL 0 WIDTH nW HEIGHT nH TITLE '' ;
          MODAL NOCAPTION NOSIZE                                    ;
          BACKCOLOR aBackColor                                      ;
          FONT cFont SIZE nFontSize                                 ;
          ON INIT    {|| This.Topmost := .T. , _wPost(0) }          ;
          ON RELEASE {|| _wSend(90) }                               ;
          ON MOUSECLICK MoveActiveWindow()
          This.Cargo := oHmgData()

     ON KEY F1  ACTION NIL

     nY     := nX := nG
     nW     := This.ClientWidth
     nH     := This.ClientHeight
     nHLine := nFontSize * 1.6

     // ��������� ��������� �� ����
     o := This.Cargo
     o:lClose   := .T.
     o:oWindow  := This.Object     // ������ ���� ��� ��� � ������� {|ow|...}
     o:cWindow  := This.Name       // ��� ����, ������ ��� �������
     o:hWindow  := This.Handle     // handle ����, ������ ��� �������
     o:nWidth   := nW              // ������ ����
     o:tRunTime := tRunTime        // ��� Label_1 -> WaitWinAviSecond()
     o:nSeconds := SECONDS()       // ��� Label_1 -> WaitWinAviSecond()
     o:cPassed  := o_Thr:cPassed   // ��� Label_1 -> WaitWinAviSecond()
     o:cMessage := cMessage        // ��� Label_2
     o:cValue   := ""              // ��� Label_0
     o:cForm    := cFormName
     o:oWnd     := oWnd            // Parent window - ���� ��������
     o:lFocus   := lFocus          // focus for parent window
     o:cFocus   := cFocus          // ...
     // � ��������� ��� ������ � thread-�� � ��������� �������� ����������
     // into the container for work in threads and the main process, remember
     o_Thr:oWindow := This.Object   // ������ ���� ��� ��� � ������� {|ow|...}
     o_Thr:cWindow := This.Name     // ��� ����, ������ ��� �������
     o_Thr:hWindow := This.Handle   // handle ����, ������ ��� �������

     @ nY, nX LABEL Label_1  WIDTH nW - nG*2 HEIGHT nHLine VALUE " " ;
              FONTCOLOR aFontColor CENTERALIGN VCENTERALIGN TRANSPARENT

     @ nY, nX LABEL Label_0 WIDTH 100 HEIGHT nHLine VALUE " " ;
              FONTCOLOR aFontColor VCENTERALIGN TRANSPARENT

     nY += This.Label_1.Height + nG
     nX := ( nW - aImgWH[1] ) / 2
     // � ������� ����� AVI �������������� ������� Microsoft RLE Video, ����������� ������ �� ��������������
     // in MiniGui, AVI display is supported by the Microsoft RLE Video codec, modern codecs are not supported
     @ nY, nX ANIMATEBOX Avi_1 WIDTH aImgWH[1] HEIGHT aImgWH[2] File cResAvi AUTOPLAY ;
              TRANSPARENT BACKCOLOR aBackColor NOBORDER

     nY += This.Avi_1.Height + nG

     @ nY, 5 LABEL Label_2 WIDTH nW - 5*2 HEIGHT nHLine VALUE " "  ;
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

     o := This.Object
     o:Event( 0, {|ow|
                   AEval({1, 2}, {|nn| _wSend(nn) })
                   SetWaitCursor( ow:Handle )
                   _logfile(.t.,"   ---[ :Event(0) ]--- " + ProcNL() )
                   DO EVENTS
                   Return Nil
                   })
     o:Event( 1, {|ow,ky,ct|  // SetProperty(ow:Name, "Label_1", "Value", v)
                   Local cLbl := "Label_1"
                   ky := ow:Cargo
                   Default ct := ky:cPassed + " " + HMG_TimeMS( ky:tRunTime )
                   _SetValue ( cLbl, ow:Name, ct )
                   DO EVENTS
                   Return Nil
                   })
     o:Event( 2, {|ow,ky,ct|  // SetProperty(ow:Name, "Label_2", "Value", v)
                   Local cLbl := "Label_2"
                   ky := ow:Cargo
                   Default ct := ky:cMessage
                   _SetValue ( cLbl, ow:Name, ct )
                   DO EVENTS
                   Return Nil
                   })
     o:Event({3, "@Say"}, {|ow,ky,ct|  // SetProperty(ow:Name, "Label_0", "Value", v)
                   Local cLbl := "Label_0", nPos := 100, nNew
                   ky := ow:Cargo
                   Default ct := ky:cValue
                   ct := iif( HB_ISCHAR(ct), ct, cValToChar(ct) )
                   _SetValue ( cLbl, ow:Name, ct )
                   cLbl := "Label_1"
                   IF GetProperty( ow:Name, cLbl, "Col" ) != nPos
                      nNew := ky:nWidth - nPos - 5 * 2
                      SetProperty( ow:Name, cLbl, "Col"  , nPos )
                      SetProperty( ow:Name, cLbl, "Width", nNew )
                   ENDIF
                   DO EVENTS
                   Return Nil
                   })

     // ������ ������� ���������� ��������� LABEL �� ���������� ��������� oTrhData()
     o:Event(10, {|  | This.Label_0.Value := o_Thr:cMsg0, DoEvents() })
     o:Event(11, {|  | This.Label_1.Value := o_Thr:cMsg1, DoEvents() })

     o:Event(90, {|ow| _logfile(.t.,"   ---[ :Event(90) ]--- " + ProcNL(),;
                      ">>> RELEASE: " + ow:Name ) , InkeyGUI(50)            })
     o:Event(99, {|ow| ow:Cargo:lClose := .T., ow:Release() })

   END WINDOW

   Center Window &cFormName
   ACTIVATE WINDOW &cFormName ON INIT {|| This.Minimize, DoEvents(), This.Restore, DoEvents() } NOWAIT

   IF lDebug  ; ? "===[] CREATE FORM ->", cFormName, ProcNL(), "lFocus=", lFocus
   ENDIF

   // Start preloding in a separate thread
   // ��������� preloding � ��������� ������
   aParam := { cFormName, tRunTime, nW, nH }
   hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @ThreadAviSecond(), aParam ) )
   InkeyGUI( 100 )
   // ����������� ����� �� ����, ���� ��� ������
   IF ! empty(o_Thr:cFocus)          // setfocus ��� ����
      o_Thr:oWndCurr:SetFocus(o_Thr:cFocus)
   ENDIF
   DO EVENTS

RETURN cFormName

//////////////////////////////////////////////////////////////////////////////
FUNCTION ThreadAviSecond(aParam)
   LOCAL cFormName, tStart, cVal, cPassed, nSeconds, lDebug

   lDebug    := o_Thr:lDebug             // ������� � ���-����
   cFormName := o_Thr:cFormName          // ��� ����
   tStart    := o_Thr:tRunTime           // ����� �������
   nSeconds  := o_Thr:nSeconds           // ����� ������ ���������
   cPassed   := o_Thr:cPassed            // "������"

   DO WHILE o_Thr:lWinWait               //  ����������� ���� preloding � ������
      IF ABS( SECONDS() - nSeconds ) >= 0.05

         IF _IsWindowActive( cFormName )
            IF _IsControlDefined("Label_1",cFormName)
               cVal := cPassed + " " + HMG_TimeMS( tStart )
               myLabel_1(cVal)
               // � ������ ��� ������ ������ ! / Do NOT do this in a stream!
               //SetProperty( cFormName, "Label_1", "Value", cVal )
            ENDIF
            InkeyGui(50)
            o_Thr:nSeconds := SECONDS()
         ENDIF

         DO EVENTS

      ENDIF
   ENDDO

   IF lDebug  ; ? "===[]", ProcNL(), "aParam=", HB_ValToExp(aParam)
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myLabel_1( cMsg )
   DEFAULT cMsg := ""

   // ����� �������� cMsg � ���������, ��� ������������� � �������� ��������
   // write the cMsg value to the container, for use in the main process
   o_Thr:cMsg1 := cMsg
   // �������� ��������� 0 ���� o_Thr:oWindow - ��� wMain ����
   // ���������� ����� ��������� (�� ������ _wPost(), _wSend() �-��), �.�.
   // ��� �-�� ���������� ����� ��� ����� ��� ��������� ������� ���� � �����
   // �������� ���� ���������, ��� ���. �������. ��� ��� ������ ���� ����
   // send message 0 to the window o_Thr:oWindow is the wMain window
   // use this syntax (it's inside _wPost(), _wSend() f-th), because
   // these functions are accessed via the form name to get the window object and then
   // send a message to the window, for issue. events. There is already a window object
   o_Thr:oWindow:PostMsg(11)        // �������� ������� 11
   DO EVENTS
   // ���� ���������� label �� ����, �� ��� �� ����������� ���, �� ��� �� �����������
   // waiting for the label to be drawn on the window, but this is not necessary here, but it will be needed somewhere
   wApi_Sleep(50)

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
FUNCTION myLabel_0( cMsg )
   DEFAULT cMsg := ""

   o_Thr:cMsg0 := cMsg
   o_Thr:oWindow:PostMsg(10)   // �������� ������� 10
   DO EVENTS
   wApi_Sleep(50)

RETURN Nil

//////////////////////////////////////////////////////////////////////
// ��������� ������� � ������ / complete function in the stream
FUNCTION WaitThreadAviClose()
   LOCAL cFormName, tStart, cMsg, lDebug

   cFormName := o_Thr:cFormName          // ��� ����
   tStart    := o_Thr:tRunTime           // ����� �������
   cMsg      := "Window lifetime = " + HMG_TimeMS( tStart )
   lDebug    := App.Cargo:lDebug  ; Default lDebug := .F.

   // ��������� ������� � ������ / complete function in the stream
   o_Thr:lWinWait := .F.
   InkeyGui(100)

   IF lDebug ; ? "===[] CLOSE FORM ->", cFormName, ProcNL(), cMsg
   ENDIF

   Domethod(cFormName,"Release")

   SetCursorSystem( IDC_ARROW )

   IF o_Thr:lFocus .and. _IsWindowDefined(o_Thr:cFormCurr)
      BringWindowToTop( o_Thr:oWndCurr:Handle ) // ������� �� ������� ����
      // ��� ��� ������, � ����� � �� � ������
      //Domethod(cWnd, "Minimize")
      //Domethod(cWnd, "Restore" )
      DO EVENTS
      IF lDebug
         ? "===[] Return focus from where the call originated: cWnd=", o_Thr:cFormCurr, ProcNL()
      ENDIF
   ENDIF

   DO MESSAGE LOOP

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
