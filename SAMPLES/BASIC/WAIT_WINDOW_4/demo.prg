/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023-24 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023-24 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������ � AVI-�������� � ������ / Working with an AVI object in a stream
 * ������ � ������� TThrData (������ � ������� � ���������� ���� � ������������)
 * Working with the TThrData class (access to variables in threads comes with locks)
*/
ANNOUNCE RDDSYS
#define _HMG_OUTLOG
#include "hmg.ch"
#include "hbthread.ch"

#define PROGRAM   "Working with a AVI object in a Thread"
#define PROGVER   'Version 0.2 (18.01.2024)'
MEMVAR o_Thr
/////////////////////////////////////////////////////////////////
FUNCTION Main(...)
   LOCAL i, j, k, y, x, w, h, p, g
   LOCAL aFntClr, aBtnBClr, nHBtn, aBtn

   y := x := 20 ; nHBtn := 40 ; g := 20

   aFntClr  := { BLUE  , BLACK  }
   aBtnBClr := { SILVER, YELLOW }
   aBtn     := wMainBtn_Init()

   h := Len(aBtn) * ( nHBtn + g ) + g * 3

   DEFINE WINDOW wMain AT y,x WIDTH 500 HEIGHT h ;
      TITLE App.Cargo:cTitle MAIN NOSIZE TOPMOST ;
      BACKCOLOR App.Cargo:aBColor                ;
      FONT App.Cargo:cFDlg SIZE App.Cargo:nFDlg  ;
      ON INIT    {|| _wPost( 0) }                ;
      ON RELEASE {|| _wSend(90) }

      This.Cargo := oHmgData()
      This.Cargo:aBtnAll := aBtn

      w := This.ClientWidth
      h := nHBtn

      @ 0,0 Label Buff Value "" AUTOSIZE

      FOR i := 1 TO ( k := Len(aBtn) )
          j := "Btn_" + StrZero(i, 2)
          p := aBtn[ i ][1]

         @ y, x BUTTONEX &j WIDTH w - g*2 HEIGHT h CAPTION p     ;
           ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP          ;
           BOLD FONTCOLOR aFntClr[1] BACKCOLOR aBtnBClr[1]       ;
           ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.Backcolor := aBtnBClr[2] ) ;
           ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.Backcolor := aBtnBClr[1] ) ;
           ACTION  {|| This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) }       ;
           ON INIT {||
                    This.Cargo := oHmgData()
                    This.Cargo:cObj := This.Name
                    This.Cargo:oCtl := This.Object
                    Return Nil
                    }

          This.&(j).Cargo:nBtn  := i
          This.&(j).Cargo:aBtn  := aBtn[ i ]
          This.&(j).Cargo:nPost := 1
          This.&(j).Cargo:aBClr := aBtnBClr
          This.&(j).Cargo:oWnd  := This.Object

          IF i == k
             This.&(j).Caption := "Exit"
             This.&(j).Action  := {|| _wPost(99) }
          ENDIF
          y += h + g
      NEXT

      WITH OBJECT This.Object
      :Event( 0, {|ow  |
                  This.Topmost := .F.
                  ? ProcNL(), "----- :Event(0)"
                  ?? ow:Name
                  ow:Setfocus("Buff")
                  Return Nil
                  })

      :Event( 1, {|obtn|
                  Local ow := obtn:Window
                  ? ProcNL(), "----- :Event(1)"
                  ?? ow:Name
                  ow:Enabler( obtn:Name, .T. )
                  DoMethod( ow:Name, "Hide" )
                  DO EVENTS

                  myWinTable( obtn:Cargo , ow:Name )  // ��� "wMain"

                  ow:Enabler(obtn:Name, .T.)
                  Domethod(ow:Name, "Minimize")
                  DO EVENTS
                  Domethod(ow:Name, "Restore" )
                  DO EVENTS
                  DoMethod( ow:Name, "Show" )
                  Return Nil
                  })

      :Event(90, {|ow  | // �������� ����
                  ? ProcNL(), "----- :Event(90)"
                  ?? ">>> RELEASE: " + ow:Name
                  DO EVENTS
                  Return Nil
                  })

      :Event(99, {|ow  | ow:Release })
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN Nil

/////////////////////////////////////////////////////////////////
STATIC FUNCTION wMainBtn_Init()
   Local aBtn

   aBtn := {{"Test-1  (Avi3dMan128.avi)" , "Avi3dMan128",       ,        , , }, ;
            {"Test-2  (ZipAvi.avi)"      , "ZipAvi"     , SILVER, RED    , , }, ;
            {"Test-3  (FindFolder.avi)"  , "FindFolder" , AQUA  , FUCHSIA, , }, ;
            {"Exit"                      ,              ,       ,        , , }  ;
           }

RETURN aBtn

/////////////////////////////////////////////////////////////////
// ��� ��������� ������ �������� ������ !
// this procedure ALWAYS starts FIRST!
INIT PROCEDURE Sets_ENV()
   LOCAL aBClrDlg := { 141, 179, 226 }
   LOCAL aBColor  := {   2,  59, 143 }
   LOCAL cMsg, o

   SET LANGUAGE TO RUSSIAN // ����
   SET CODEPAGE TO RUSSIAN // ������� ��������

   SET MULTIPLE QUIT

   //RddSetDefault("DBFCDX")    // ������

   o := SetsEnv()               // -> demo_util.prg

   fErase( o:cLog )

   ? REPL("=",20) + " Start" , HB_DATETIME() , REPL("=",20)
   ? MG_Version(), MG_Version(.T.), MG_Version(.F.)

   o:cTitle        := PROGRAM
   o:cVersion      := PROGVER
   o:cTitleRu      := "������ � AVI-�������� � ������"
   o:cAvtor        := "Copyright 2023-24 Verchenko Andrey"
   o:cEmail        := "<verchenkoag@gmail.com> Dmitrov, Moscow region"
   o:cPrgInfo1     := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2     := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:aBColor       := aBColor
   o:lDebug        := .T.    // �������� ������� � ���-����

   // !!! ONLY for version from Harbour MiniGUI Extended Edition 23.04 (Update 2) and higher
   PUBLIC o_Thr ; o_Thr := TThrData():New()     // �������� ��������� ��� ������ � ��������
                                                // create a container for working with streams
   o_Thr:aBClr      := aBColor
   o_Thr:hWaitForm  := 0          // ����� ���������� ��� WaitThreadAvi.prg
   o_Thr:cWaitForm  := ""         // some of the parameters for WaitThreadAvi.prg
   o_Thr:cMsg0      := ""
   o_Thr:cMsg1      := ""

   IF ! o_Thr:MT         // hb_mtvm()
      cMsg := "No multithreading support !" + CRLF
      cMsg += "Program compilation key -mt !" + CRLF
      AlertStop( cMsg, App.ExeName  )
      ? cMsg
      QUIT
   ENDIF

RETURN

///////////////////////////////////////////////////////////////////////////////
FUNCTION myWinTable(oBtnCargo,cMainForm)
   LOCAL nW, nH, nG, nX, cFormName, cFormTitle, aBackColor, tTimeStart, cNWin
   LOCAL cVal, nI, nBtn, cCapt, cResAvi, aFontClr, aBackClr, lDebug, lFcs
   LOCAL cResAvi2, aResWH

   lDebug     := App.Cargo:lDebug  ; Default lDebug := .F.
   aBackColor := App.Cargo:aBColor
   nW         := System.DesktopWidth
   nH         := System.DesktopHeight - GetTaskBarHeight()   // ������ ������ ����� Desktop
   nG         := 20
   tTimeStart := hb_DateTime()                         // ����� ������ ��������
   IF lDebug
      ? ProcNL(), "oBtnCargo=", oBtnCargo
      _o2Log(oBtnCargo, 15, "==> .T. oBtnCargo: ", .T.)   // ����� � ���-����
   ENDIF
   nBtn       :=  oBtnCargo:nBtn                       // ����� ������
   cCapt      :=  oBtnCargo:aBtn[1]                    // �������� ������
   cResAvi    :=  oBtnCargo:aBtn[2]                    // ��� �������
   aBackClr   :=  oBtnCargo:aBtn[3]                    // ���� ���� �����
   aFontClr   :=  oBtnCargo:aBtn[4]                    // ���� ����� �� �����
   cFormName  := "Form_" + HB_NtoS(nBtn)
   cFormTitle := "STANDART: " + cFormName + " - " + cCapt
   cResAvi2   := "FindFolder"

   IF _IsWindowActive( cFormName )
      IF lDebug ;  ? "===> " + ProcNL(), "�������� ����� ���������� ����, SwitchToWin(" + cFormName + ")"
         ?? "��� �����:", cMainForm, "������ Minimize"
      ENDIF
      Domethod(cMainForm, "Minimize")
      SwitchToWin( cFormName )
      DO EVENTS
      RETURN NIL
   ENDIF

   IF lDebug ; ? ProcNL(), cFormName, "[creating a window]"
   ENDIF

   // ������ ������, ������ ������ / memory cleaning, garbage collection
   // ��� ��� ������� �������� / this is for large programs
   DO EVENTS ; hb_gcAll() ; DO EVENTS

   SET FONT TO "Tahoma", 22

   /////////////// --------------------- �������� ���, ���������� �� ���-�����
   lFcs  := .F.   // ������� ����� �� ����.����
   // ������ ���� �������� � �������
   cNWin := WaitThreadAvi( 'Creating a table, Thread 1',tTimeStart,lFcs,cResAvi, aBackClr, aFontClr )
   IF Empty(cNWin)
      IF lDebug ;  ? "===[] " + ProcNL(), "Error creating window !"
      ENDIF
      RETURN NIL
   ENDIF
   // �������� - ���� ����������/��������/�������� ��� � �.�.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/100"
      //_wSend("@Say", cNWin, cVal)
      // _wSend(3, cNWin, cVal)   // ��� ���
   NEXT
   WaitThreadAvi()   // ������� ���� "��������"
   /////////////// --------------------- �������� ���, ���������� �� ���-�����

   DEFINE WINDOW &cFormName         ;
      AT 0, 0 WIDTH nW HEIGHT nH    ;
      TITLE cFormTitle              ;
      WINDOWTYPE STANDARD TOPMOST   ;
      NOMAXIMIZE NOSIZE             ;
      BACKCOLOR aBackColor          ;
      ON INIT    {|| DoEvents(), _wPost(1) }  ;  // ����������� ����� ������������� ����
      ON RELEASE {|| _wSend(90)            }     // ����������� ����� ����������� ����

      // ��������� ����������� ���� - ����� � ���
      //This.OnInit    := {|| _wPost(1)   }   // ����������� ����� ������������� ����
      //This.OnRelease := {|| _wPost(90)  }   // ����������� ����� ����������� ����
      //This.OnInterActiveClose := {|| This.Cargo:lClose }

      // ��������� ��������� �� ����
      This.Cargo := oHmgData()
      This.Cargo:lClose    := .T.
      This.Cargo:tStart    := tTimeStart       // ����� ������ ��������
      This.Cargo:cBtnTitle := cCapt
      This.Cargo:cResAvi2  := cResAvi2

      @ nG, nG BUTTON oBut_Exit CAPTION "Exit" WIDTH 200 HEIGHT 60 ACTION _wPost( 99 )
      nX := nG + This.oBut_Exit.Width + nG

      @ nG, nX BUTTON oBut_About CAPTION "About" WIDTH 200 HEIGHT 60 ACTION _wPost( "@About", This.Index )
      nX += This.oBut_About.Width + nG

      aResWH := GetAviResSize(cResAvi2)
      // � ������� ����� AVI �������������� ������� Microsoft RLE Video, ����������� ������ �� ��������������
      // in MiniGui, AVI display is supported by the Microsoft RLE Video codec, modern codecs are not supported
      @ nG, nX ANIMATEBOX Avi_1 WIDTH aResWH[1] HEIGHT aResWH[2] File cResAvi2 AUTOPLAY ;
        TRANSPARENT BACKCOLOR aBackColor NOBORDER

      @ 220, 0 LABEL Label_1 VALUE "Calculation in progress - " + cCapt WIDTH nW HEIGHT 60 ;
         SIZE 26 FONTCOLOR BLACK BOLD TRANSPARENT CENTERALIGN

      WITH OBJECT This.OBJECT
         :Event( 0, {|  | DoEvents()             } ) // just as an example

         :Event( 1, {|ow| // ON INIT windows + close the "calculation" window
                          LOCAL cMsg, cNWin, cVal, nI, t2, aBC
                          t2   := ow:Cargo:tStart               // ����� ����������� ��������
                          cMsg := ow:Cargo:cBtnTitle + " - "

                          This.Topmost := .F.
                          //_StopAnimateBox( "Avi_1" , ow:Name )
                          aBC := GetProperty( ow:Name, "Backcolor"  )
                          SetProperty( ow:Name, "Backcolor", GRAY )
                          SetProperty( ow:Name, "Avi_1", "Backcolor", GRAY )

                          DoMethod(ow:Name, "DisableUpdate")  // ����������� ��� �����
                                                              // block the whole form
                          ? ProcNL(), "----- :Event(1)", ow:Name
                          /////////////// ---- ������ ���� ���. ��������
                          // ������ ���� �������� � �������
                          cNWin := WaitThreadAvi( 'Creating a table, Thread 2',t2,, cResAvi, aBackClr, aFontClr )
                          IF !Empty(cNWin)
                             // �������� - ���� ����������/��������/�������� ��� � �.�.
                             FOR nI := 50 TO 100
                                wApi_Sleep( 100 )
                                DO EVENTS
                                cVal := hb_ntos( nI ) + "/100"
                                _wSend("@Say", cNWin, cVal)
                                //_wSend(3, cNWin, cVal)      // ��� ���
                             NEXT
                             WaitThreadAvi()   // ������� ���� "��������"
                          ENDIF
                          /////////////// ---- ������ ���� ���. ��������

                          DO EVENTS
                          DoMethod(ow:Name, "EnableUpdate")    // ������������� ���� �����
                                                               // unlock the whole form
                          SetProperty( ow:Name, "Backcolor", aBC )
                          SetProperty( ow:Name, "Avi_1", "Backcolor", aBC )
                          //_PlayAnimateBox( "Avi_1" , ow:Name )
                          _OpenAnimateBox( "Avi_1" , ow:Name, ow:Cargo:cResAvi2 )
                          cMsg += "Elapsed processing time - " + HMG_TimeMS( ow:Cargo:tStart )
                          This.Label_1.VALUE := cMsg
                          DoMethod( ow:Name, "Label_1", "SetFocus" )
                          // ow:Setfocus("Label_1")   // ��� ���
                          ? ProcNL(), cMsg
                          DO EVENTS

                          RETURN NIL
                        } )

         :Event({10,"@About"}, {|obtn| // About
                          Local ow := obtn:Window
                          ? ProcNL(), "----- :Event(10,@About)"
                          ?? ow:Name, obtn:Name, "Status:", obtn:Enabled()
                          // "�������� ������/Block the button"
                          obtn:Disable()
                          _StopAnimateBox( "Avi_1", ow:Name )
                          DO EVENTS
                          SET WINDOW THIS TO ow:Name
                          MsgAbout()                  // -> demo_util.prg
                          SET WINDOW THIS TO
                          // "����������� ������/Unlock the button"
                          obtn:Enable()
                          _OpenAnimateBox( "Avi_1" , ow:Name, ow:Cargo:cResAvi2 )
                          ow:Setfocus("Label_1")
                          Return Nil
                          })

         :Event(90, {|ow| _logfile(.t.,"   ---[ :Event(90) ]---" + ProcNL(), ">>> RELEASE: " + ow:Name ) })
         :Event(99, {|ow| ow:Release() } )
      END WITH

   END WINDOW

   DoMethod( cFormName, "Center" )
   ACTIVATE WINDOW &cFormName ON INIT {|| This.Minimize, DoEvents(), This.Restore, DoEvents() }

   // ������ ������, ������ ������ / memory cleaning, garbage collection
   // ��� ��� ������� �������� / this is for large programs
   DO EVENTS ; hb_gcAll() ; DO EVENTS

RETURN NIL
