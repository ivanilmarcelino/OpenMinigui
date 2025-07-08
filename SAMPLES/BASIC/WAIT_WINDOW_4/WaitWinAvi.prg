/*
 * MINIGUI - Harbour Win32 GUI library
 * ������ � AVI-�������� / Working with a AVI object
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
*/
#include "hmg.ch"

/* � ���� � ��������� ����� ������ ���: / In your program you can do this:

   nTime := SECONDS()
   // ������ ���� �������� � �������
   cWin := WaitWinAvi( '������ ���������� ...', tRunTime, lFocus, cResAvi )

   // �������� ���� ����������/��������/�������� ��� � �.�.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "50"
      _wSend("S_a_y", cNWin, cVal)
      // _wSend(3, cNWin, cVal)        // ��� ���
      // WaitWinAviSay( cVal )         // ��� ���
      ...................
   NEXT

   WaitWinAvi()

   cMsg := "������ ������� ��������� �� " + SECTOTIME( SECONDS() - nTime )
   MsgInfo(cMsg)
-----------------------------------------------------------------------------
����� � ���:
   WaitWinAvi( '������ ���������� ...' )   // ������ ���� �������� � �������
   // �������� ���� ����������/��������/�������� ��� � �.�.
   ...................
   WaitWinAvi()   // ������� ���� "��������"
*/
STATIC cStatWinWait   := "_HMG_MODAL_WaitWinAvi"       // ��� ���� wait -- ������ ���� ���� !!!
////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION WaitWinAvi( cMessage, tRunTime, lFocus, cResAvi, aBackColor, aFontColor, nWidth )
   LOCAL cFont, nFontSize, cMsg, lFor, lDebug, cPassed
   LOCAL nY, nX, nG, nW, nH, nWTxt, aImgWH, nHLine
   LOCAL oWnd, lWnd, ow, cFocus, o, k := pCount()
   LOCAL cFormName := cStatWinWait
   LOCAL lDefined  := _IsWindowDefined( cFormName )
   DEFAULT aBackColor := GRAY
   DEFAULT aFontColor := WHITE
   DEFAULT nWidth     := 0               // ������ ���� �������� �������
   DEFAULT tRunTime   := hb_DateTime()   // ����� ������ �������� / ����� ��������� ����.�����
   DEFAULT lFocus     := .F.             // ������� ����� �� ����.����, .F. �� �������

   IF HB_ISOBJECT( App.Cargo )
      lDebug := App.Cargo:lDebug 
      IF hb_IsLogical( lDebug ) 
         lDebug := .F.  
      ENDIF
   ENDIF
   DEFAULT lDebug := .F.  

   IF lDefined
      ow   := _WindowObj( cFormName )    // object _HMG_MODAL_WaitWinAvi window
      o    := ow:Cargo                   // Cargo  ...
      oWnd := o:oWnd                     // parent window
      lWnd := HB_ISOBJECT(oWnd)

      IF k == 0                          // close wait window
         lFocus := o:lFocus
         cFocus := o:cFocus
         cMsg := "Window lifetime = "
         cMsg += HMG_TimeMS( o:tRunTime )
         IF lDebug
            ? "===[] CLOSE FORM ->", cFormName, ProcNL(), cMsg
         ENDIF
         ow:Release()           // DoMethod ( cFormName, "Release" )

         DO EVENTS ; wApi_Sleep(100)

         SetCursorSystem( IDC_ARROW )

         IF lWnd                // reset This variable for parent window
            SET WINDOW THIS TO
            IF lFocus .and. _IsWindowDefined(oWnd:Name)
               BringWindowToTop( oWnd:Handle ) // ������� �� ������� ����
               // ��� ��� ������, � ����� � �� � ������
               //Domethod(cWnd, "Minimize")
               //Domethod(cWnd, "Restore" )
               DO EVENTS
               IF ! empty(cFocus)                       // setfocus ��� ����
                  oWnd:SetFocus(cFocus)
               ENDIF
               IF lDebug
                  ? "===[] Return focus from where the call originated: cWnd=", oWnd:Name, oWnd:Handle, ProcNL()
               ENDIF
            ENDIF

         ENDIF

      ELSE                               // Say cMessage value
        IF !HB_ISCHAR( cMessage )
           cMessage := cValToChar( cMessage )
        ENDIF
         _wSend(2, ow, cMessage)
         // ���
         // o:cMessage := cMessage
         // _wSend(2, ow)

      ENDIF
      DO EVENTS

      RETURN cFormName

   ENDIF

   oWnd := _WindowObj( _HMG_THISFORMNAME ) // oWnd - parent window object

   IF ( lWnd := HB_ISOBJECT(oWnd) )        // lWnd - ���� ������ ���� ��������
      cFocus := oWnd:FocusedControl        // ������� � ������ - ������� ����
      SET WINDOW THIS TO oWnd:Name         // sets This variable for parent window
   ENDIF

   hb_default( @cMessage, "Expectation..." )   // "��������..."
   hb_default( @tRunTime, hb_DateTime() )      // ����������� ������� ������ � ����
   hb_default( @lFocus, .F. )                  // .T.-������� �����, ������ ���� ������� ����
   hb_default( @cResAvi, "Avi3dMan128" )
   hb_default( @aFontColor, BLACK )
   hb_default( @aBackColor, WHITE )
   hb_default( @nWidth, 0 )

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
   nW := 450
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
     o:oWindow  := This.Object  // ������ ���� ��� ��� � ������� {|ow|...}
     o:cWindow  := This.Name    // ��� ����, ������ ��� �������
     o:hWindow  := This.Handle  // handle ����, ������ ��� �������
     o:nWidth   := nW           // ������ ����
     o:tRunTime := tRunTime     // ��� Label_1 -> WaitWinAviSecond()
     o:nSeconds := SECONDS()    // ��� Label_1 -> WaitWinAviSecond()
     o:cPassed  := cPassed      // ��� Label_1 -> WaitWinAviSecond()
     o:cMessage := cMessage     // ��� Label_2
     o:cValue   := ""           // ��� Label_0
     o:cForm    := cFormName
     o:oWnd     := oWnd         // Parent window - ���� ��������
     o:lFocus   := lFocus       // focus for parent window
     o:cFocus   := cFocus       // ...
     o:lTimer   := .T.          // ����� �������� ������� TIMER_10

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

     DEFINE TIMER Timer_10 INTERVAL 1000 ACTION {|| _wPost(10) }
     This.Timer_10.Enabled := .F.  // ��������� ������ �� ON INIT

     o := This.Object
     o:Event( 0, {|ow|
                   AEval({1, 2}, {|nn| _wSend(nn) })
                   SetWaitCursor( ow:Handle )
                   _logfile(.t.,"   ---[ :Event(0) ]--- " + ProcNL() )
                   ow:Enabler("Timer_10", .T.)     // �������� ������
                   //This.Timer_10.Enabled := .T.
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
     o:Event({3, "S_a_y"}, {|ow,ky,ct|  // SetProperty(ow:Name, "Label_0", "Value", v)
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
     o:Event(10, {|ow|                           // Timer Timer_10
                   Local o := ow:Cargo
                   IF o:lTimer                   // ����� ��������
                      o:lTimer := .F.
                      ow:Enabler("Timer_10", .F.)
                      IF ABS( SECONDS() - o:nSeconds ) >= 0.05
                         _wSend(1)               // value to Label_1
                         o:nSeconds := SECONDS()
                      ENDIF
                      ow:Enabler("Timer_10", .T.)
                      o:lTimer := .T.
                   ENDIF
                   Return Nil
                   })
     o:Event(90, {|ow| _logfile(.t.,"   ---[ :Event(90) ]--- " + ProcNL(),;
                      ">>> RELEASE: " + ow:Name ) , InkeyGUI(50)            })
     o:Event(99, {|ow| ow:Cargo:lClose := .T., ow:Release() })

   END WINDOW

   Center Window &cFormName
   ACTIVATE WINDOW &cFormName ON INIT {|| This.Minimize, DoEvents(), This.Restore, DoEvents() } NOWAIT

   IF lDebug
      ? "===[] CREATE FORM ->", cFormName, ProcNL(), "lFocus=", lFocus
   ENDIF

   DO EVENTS

RETURN cFormName

//////////////////////////////////////////////////////////////////////////////
// ����� ���.���������� / output of additional information
FUNCTION WaitWinAviSay( cVal )
   LOCAL cFormName := cStatWinWait
   LOCAL ow := _WindowObj( cFormName )
   LOCAL o  := ow:Cargo

   IF !HB_ISOBJECT(ow) .or. !_IsWindowActive( cFormName ) ; RETURN Nil
   ENDIF

   _wSend(3, ow, cVal)

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

