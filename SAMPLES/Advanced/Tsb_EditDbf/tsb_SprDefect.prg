/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 15.04.25
 *
 * _TBrowse() ������ ������� ��� �������������� ����� �������
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_ZaListNeis(oBrw1,aDim,oDop,aLine,cForm)
   LOCAL oWnd, aRet, nWBtn, nHBtn, nHIco, aBtnCap, nHText, cIcon, cTitle
   LOCAL cFont, nFSize, aFont, aFont2, cBtnFont, nBtnFSize, aXDfct, cMsg1
   LOCAL nY, nX, nW, nH, nG, aBClr, aBtnFClr, aBtnBClr, nWText, aFldDbf
   LOCAL owc, aIco1, aIco2, aIco3, aColor, aGrOver, aGrFill, aValDbf, aChk
   LOCAL cMsg, nY2, nX2, nW2, nH2, nWTsb, nHTsb, oBrw, oTsb, oGet, aCode, cErr
   DEFAULT cForm := "Tsb_Win_Defect"

   //MsgDebug(aDim,"oDop=",oDop,"aLine=",aLine,cForm)
   ? "======",ProcNL(),aDim,"oDop=",oDop,"aLine=",aLine,cForm
   //?v  aLine
   aFldDbf := aDim  // ������ ������������ - ���� ����������� ���� ��� ������ � �� ���� �� ������ �������
                    // ����� ������ ���. ��������
   aValDbf := oDop  // ���� aDefect - ���� ������ ������������ - ������
   aCode   := aLine[ACOL_13]  //  (13) - �������� ������������� ���� {} ��� ���� CALC
   IF "DEMO3.EXE" $ UPPER(cFileNoPath(App.ExeName))
      // ������� ���� �� ���� / window position by window
      oWnd   := _WindowObj( oBrw1:cParentWnd )    // parent window
      nY     := oWnd:Row
      nX     := oWnd:Col
      nW     := oWnd:Width
      nH     := oWnd:Height
   ELSE
      // ������� ���� �� ��� / window position according to TSB
      oWnd   := _WindowObj( oBrw1:cParentWnd )   // ������������ ����
      nY     := GetWindowRow(oBrw1:hWnd)
      nX     := GetWindowCol(oBrw1:hWnd)
      nW     := GetWindowWidth(oBrw1:hWnd)
      nH     := GetWindowHeight(oBrw1:hWnd)
   ENDIF
   nG     := 10   // ����� ���������
   aRet   := {}   // ������ ������ - �����, ������ ����� �� �����
   nHIco  := 48
   nWBtn  := 220      // 3 ������
   nHBtn  := nHIco + 10
   aBClr  := { 84,183,128}
   cIcon  := "gear48x1"

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont   := GetFontParam("DlgFont")
   aFont2  := GetFontParam("ComSanMS")
   aBtnCap := { "Select", "Cleaning", "Cancel" }
   cTitle  := "Malfunction on request"
   cMsg    := "Find:"
   cMsg1   := "ATTENTION!; The number of selected items is MORE than 10!; I am leaving only 10 items!"
   cErr    := "Error! No array of dbf faults!;Array oWnd:Cargo:aXDfct not defined!;;"
#else
   aFont   := GetFontParam("FntCnMn1")
   aFont2  := GetFontParam("FntCnMn2")
   aBtnCap := { "��������", "�������", "��������" }
   cTitle  := "������������� �� ������"
   cMsg    := "�����:"
   cMsg1   := "�������� !; ���������� ��������� ������� ������ 10 !; �������� ������ 10 ������� !"
   cErr    := "������ ! ��� ������� dbf-�������������� !;������ oWnd:Cargo:aXDfct �� �������� !;;"
#endif
   cTitle  += SPACE(5) + ProcFile()

   aXDfct  := App.Cargo:aSprDfct         // ������ dbf-�������������� �� Cargo ���� // -> demo1_util.prg
   IF !IsArray(aXDfct)
      cErr += ProcNL() + ";" + ProcNL(1) + ";;"
      AlertStop(cErr, "", , 64, {RED})
      ? ATREPL( ";", cErr, CRLF )
   ENDIF
   aXDfct := mySortDim(aXDfct,aCode,@aChk)    // ��������� �������� � ������ + aChk-������ � ���������,
                                              // ����� ���� ������ {}
   cFont     := aFont[1]
   nFSize    := aFont[2]
   nHText    := nFSize * 2
   // ������ �� �����
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   aBtnFClr  := { BLUE , YELLOW }
   aColor    := GRAY
   aGrOver   := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill   := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aBtnBClr  := { {225,225,225}, GRAY }
   aBtnFClr  := { RED, YELLOW }

   // ����������� ��� ���� MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // ����� ���� � ���������� �������
   nW2 := nW //* 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2, nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE FONT cFont SIZE nFSize BACKCOLOR aBClr             ;
      ON INIT    {|| _wSend(0), DoEvents()     }                      ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90) }   // ��������� ���� ������ ������ Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr := This.Backcolor
      nY  := nX := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      nY     := nHBtn + nG*2 - nHText - nG
      nWText := GetTxtWidth( cMsg, nFSize, cFont, .T. ) + 5
      @ nY, nX LABEL Lbl_Find VALUE cMsg WIDTH nWText HEIGHT nHText ;
        FONTCOLOR WHITE TRANSPARENT BOLD VCENTERALIGN //RIGHTALIGN

      nX += This.Lbl_Find.Width + 2
      // GetBox ��� ������ � �������
      @ nY, nX GETBOX GB_Find OBJ oGet WIDTH 180 HEIGHT nHText VALUE " " ;
        PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
        ACTION       {|| ThisWindow.Cargo:nGB_Find := 1, This.Value := "" }   ;
        ACTION2      {|| ThisWindow.Cargo:nGB_Find := 2, This.Value := "" }   ;
        IMAGE        {"bTofix24", "bDelRed24" } ; // tobegin, tofix, collect
        BUTTONWIDTH  nHText                     ;
        ON GOTFOCUS  {|ob| ob := ThisWindow.Cargo:oBrw, ob:nCell := 3, ob:DrawSelect() } ;
        ON CHANGE    {|| Search_ATSB( ThisWindow.Object ) } ;
        ON INIT      {|| This.Cargo := .T. }
        //ON LOSTFOCUS {|| This.Cargo := .F., This.Value := space(30) } ;
        //ON CHANGE    {|| iif( Empty( This.Cargo ), NIL, Search_ATSB( ThisWindow.Object, .T. ) ) } ;

      This.Cargo:oGet := oGet
      This.Cargo:cGet := "GB_Find"    // ��������� ��� ����������� �������������
      This.Cargo:nGB_Find := 0

      nY    := nHBtn + nG*2 ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// ������� ///////////////////////////////////////////////////////
      oTsb := TablePatamDfct( cForm, aXDfct, "cTableDfc", nWTsb, aLine[1])
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDfct, "cTableDfc", nY, nX, nWTsb, nHTsb )
      // ����� ������ ����������� �������
      oBrw:Cargo:nCellFoc := 2                            // �������� ������
      oBrw:Cargo:nModify  := 0                            // ������� ���������
      oBrw:Cargo:nChk     := LEN(aChk)                    // ������� ���-�� ��������
      oBrw:Cargo:aIsx     := oBrw:aArray                  // �������� ������ ����� !!!
      oBrw:Cargo:nArray   := 1                            // �������.������� 1-oBrw:aArray, 2-owc:aIsx
      // ������ ������� �� ����
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cSpTable"
      This.Cargo:aChk     := aChk                         // ������ � ��������� .T. ������
      //
      /////////////////////// ������ �� ����� ////////////////////////////////
      nY    := nG
      nX    := nW - nWBtn * 3 - nG * 3

      aIco1 := { "iMg_Ok48x1"    , "iMg_Ok48x2"     }
      aIco2 := { "iCircle48x1"   , "iCircle48x2"    }
      aIco3 := { "iMg_Cancel48x1", "iMg_Cancel48x2" }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[1] ;
               ICON aIco1[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco1[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco1[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(10 ,, This.Name) }

      nX := nW - nWBtn * 2 - nG * 2

      @ nY, nX BUTTONEX Btn_Cls WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[2] ;
               ICON aIco2[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco2[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco2[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(20 ,, This.Name) }

      nX := nW - nWBtn  - nG

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[3] ;
               ICON aIco3[1] NOXPSTYLE HANDCURSOR NOTABSTOP                ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD     ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                      ;
               ON MOUSEHOVER ( This.Icon := aIco3[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco3[1],         ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(98 ,, This.Name) }

      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents(), _wSend(5,ow) } )

         :Event(  5, {|ow| // ������� ���-�� �������� � ������� �������
                           Local ob   := ow:Cargo:oBrw
                           Local nChk := ob:Cargo:nChk
                           ob:aColumns[2]:cFooting := HB_NtoS(nChk)
                           ob:DrawFooters()
                           DO EVENTS
                           Return Nil
                           } )

         :Event( 10, {|ow,ky,cn|  // ��������� ������
                       Local a, i, obr, owc, aTxt, aChk
                       owc  := ow:Cargo
                       obr  := owc:oBrw
                       a    := obr:Cargo:aIsx
                       aChk := {}  ; aTxt := {}
                       For i := 1 TO LEN(a)
                          If a[i,1]
                             AADD(aChk, a[i,3]) // ���� �� ��������
                             AADD(aTxt, a[i,2]) // ������������
                          Endif
                       Next
                       IF LEN(aChk) > 10
                          AlertStop(cMsg1,,,64,{RED})
                       ELSEIF LEN(aChk) == 0
                          aChk := ARRAY(10)
                          AFILL( aChk, 0 )
                          aTxt := ARRAY(10)
                          AFILL( aTxt, "" )
                       ELSE
                          For i := LEN(aChk) TO 10
                             AADD(aChk, 0 ) // ���� �� ��������
                             AADD(aTxt, "") // ������������
                          Next
                       ENDIF
                       //MsgDebug( owc:oBrw:Cargo:nModify, owc:oBrw:Cargo:nChk )
                       // ������� ���������   // ������� ���-�� ��������
                       ? "### Save Tsb Check:", ProcNL(), owc:oBrw:Cargo:nChk
                       ?? "aChk=", Len(aChk), HB_ValToExp(aChk)
                       aRet := { aChk, aTxt }
                       _wPost(99,ow)
                       ky := cn
                       Return Nil
                       } )

         :Event( 20, {|ow,ky,cn|  // �������� �������
                       Local obr, a, i, owc := ow:Cargo
                       owc:oBrw:Cargo:nModify := 0
                       owc:oBrw:Cargo:nChk    := 0
                       obr := owc:oBrw
                       a   := obr:aArray
                       For i := 1 TO LEN(a)
                          a[i,1] := .F.
                       Next
                       obr:aColumns[2]:cFooting := ""
                       obr:DrawFooters()
                       obr:Refresh()
                       DO EVENTS
                       ky := cn
                       Return Nil
                       } )

         // �������� ������� � �������, ��. ����
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4
         //             _wPost(  32  ,  33  , 34

         :Event(90, {|ow,ky| // ON Release windows
                       Local cm
                       cm := ProcNL()
                       ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                       ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                       DO EVENTS
                       Return Nil
                       })

         :Event( 98, {|ow| aRet := {}, _wPost(99,ow) } )

         :Event( 99, {|ow| ow:Release() } )
      END WITH

      ON KEY F1   ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

   END WINDOW

   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)  // ����������� / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ����������� ��� ���� MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // ������ ������, ���� ����� - ������ ����� �� �����

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_ATSB(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw, aDim := {}, a
   LOCAL cGet  := oWnd:Cargo:cGet             // ��� "GB_Find"
   LOCAL cVal  := trim( This.&(cGet).Value )
   LOCAL nBtn  := oWnd:Cargo:Get("n"+cGet, 0)
   LOCAL lSwap := .F.
   LOCAL nLen  := Len( cVal )

   IF     nLen == 0
      ? "~~~~~~~>>>", cGet, nBtn, cVal
      IF nBtn == 1       // ������ ������ "������ c V � ������"
         // ���������� ��������� ������� ��� ����� ������� ������
         oBrw:aArray := ASORT( oBrw:aArray,,, { |x, y| x[1] > y[1] } )
      ELSE               // ������ ������ "��������\����� ������"
         // ��������\����� ������ � �������� �������� ������
         oBrw:aArray := ASORT( oBrw:aArray,,, { |x, y| x[2] < y[2] } )
      ENDIF
      oBrw:Cargo:nArray := 1             // �������.������� 1-oBrw:aArray, 2-oBrw:Cargo:aIsx
      oBrw:aArray := oBrw:Cargo:aIsx
      lSwap := .T.
   ELSEIF nLen > 2    // �� 3-� �������� �����
     cVal := upper(cVal)
     FOR EACH a IN oBrw:Cargo:aIsx
         IF cVal $ upper(a[2]) ; AAdd( aDim, AClone( a ) )
         ENDIF
     NEXT
     IF Len(aDim) = 0
        a := array( Len(oBrw:aArray[1]) )
        a[2] := '��� ����� ������: "' + cVal + '" ....'
        AAdd( aDim, a )
     ENDIF
     oBrw:Cargo:nArray := 2              // �������.������� 1-oBrw:aArray, 2-owc:aIsx
     oBrw:aArray := aDim
     lSwap := .T.

   ENDIF

   IF lSwap
      oBrw:nCell := 3
      oBrw:Reset()
      DO EVENTS
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySortDim(aXDfct,aCode,aChk)          // ��������� �������� � ������
   LOCAL nI, nJ, nCode, aRet := {}

   aChk := {}
   IF !IsArray(aCode)
      RETURN aXDfct
   ENDIF
   IF LEN(aCode) == 0
      RETURN aXDfct
   ENDIF
   FOR nI := 1 TO LEN(aCode)
      nCode := aCode[nI]
      IF nCode > 0
         AADD( aChk, nCode )          // ���� ������� �� ����
         FOR nJ := 1 TO LEN(aXDfct)
            IF nCode == aXDfct[nJ,3]  // ���� ��������������
               aXDfct[nJ,1] := .T.
            ENDIF
         NEXT
      ENDIF
   NEXT
   aRet := ASORT( aXDfct,,, { |x, y| x[1] > y[1] } )

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamDfct(cForm,aXDim,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   oTsb:aNumber        := { 1, 30 }                // ������� ��������� � � ������
   oTsb:nHeightCell    := 28                       // ������ ����� = ������ �������� ��������
   oTsb:lDrawHeaders   := .F.                      // �� ������� � ������� ����� !!! ��� ������ ���������
   oTsb:nHeightHead    := oTsb:nHeightCell         // ������ ����� - ������ ����� �������
   oTsb:nHeightFoot    := oTsb:nHeightCell         // ������ �������
   oTsb:lFooting       := .T.                      // ������� � ������� ������
   oTsb:lSpecHd        := .T.                      // ��������� � ������� ��������� �������
   oTsb:lSuperHd       := .T.                      // ��������� � ������� ����������
   oTsb:cSuperHd       := cTitle                   // ����� �����������
   oTsb:nHeightSuperHd := 24                       // ������ �����������
   oTsb:nCellMarginLR  := 0                        // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
   //oTsb:uSelector    := 20                       // �������� ����� �������
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "F_CHK", "F_NAME", "F_CODE", "F_GRP", "F_VID", "F_SRK" }

#ifdef KEY_ENG // for this project demo1-en.hbp
   //               1         2                          3              4                        5                    6
   oTsb:aHead := { "*", "Fault name"            , "Codes; faulty." , "Fault;group"        , "Fault type"       , "Urgency type"   }
#else
   oTsb:aHead := { "*", "�������� �������������", "����;������.", "������;�������������", "���;�������������", "���;���������" }
#endif

   oTsb:aHideCol := {}   // ������ �������, ��������� SELECTOR � ARRAYNO
   aWSize        := CalculatColumnWidthsDfc(aXDim,2,nWTsb)   // ������� ������ ������� - ������� �� 2 �������
   oTsb:aSize    := aWSize                                  // �������� ������ ������� ��� ���

   // ������ ����� ������ � ���
   nHFnt              := App.Cargo:nFontSize * 1.8
   //oTsb:nHeightHead := nHFnt                          // ������ �����
   //oTsb:nHeightFoot := nHFnt                          // ������ �������

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // ��������� � ������� ������
      oTsb:aFoot       := .T.                            // ��������� ������
   ENDIF

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // �� ��������� � ������� ���������
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // ������ ����������
   ENDIF

   aBClr               := This.Backcolor
   nClr1               := HMG_RGB2n(aBClr)                  // ���� ���� �����+������
   nClr2               := RGB( 48, 29,26)                   // ����-������ ���
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // ����: ����� � ��� �����������
   oTsb:aBrush         := aBClr                             // ���� ���� ��� ��������
   // ����� � �������
   oTsb:lZebra    := .T.                                    // ��� ���.\����. �������� zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // ����� ����
   oTsb:aZebra    := { aBClr, {190,244,214} }
   a := {}
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , ������ ����� �������
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , ���� ����� �������
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , ������ �������������� ����
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , ���� �������������� ����
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , ������ ������� �������
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, ���� ������� �������
   AAdd(a, { CLR_SPCF , CLR_RED                  })  // 18, specheader text - ���������
   AAdd(a, { CLR_SPCB , RGB(240,240,240)         })  // 19, specheader back - ���������
   oTsb:aColorAdd := a

   // ����� ���� ��� _TBrowse(...) - �������� ���������� bInit,bBody,bEnd,bAfter ������ ������
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // ��������� ���
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // ������ ��������� ���
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // ���� ���� ����� END TBROWSE, ����� �� �������� oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) } // ���� ���� ����� END TBROWSE �� ������������
                                               // ��� �������������, �������� DEFAULT ��������
                                               // !!! ��� ������ � oTsb:bAfter !!!
   // ����� ������� ������ ������ ����
   oTsb:bInit := {|ob,op| // ��������� ���
                   //ob:Hide()                                    // ������ ������� ��� ���������� ����������
                   //ob:HideColumns( op:aHideCol ,.t.)            // ������ �������
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO")        // ���������� �������
                   ob:lLockFreeze := .T.                          // �������� ���������� ������� �� ������������ ��������
                   ob:nCell       := ob:nFreeze + 1               // ����������� ������
                   ob:lNoKeyChar  := .F.                          // ���� � ������ �� ����, ����
                   myTsbEditDfct(ob,op)                             // �������������� ����� �������
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // ������ ��������� ���
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // ���� ���������
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - ��� ���
                   ob:lPickerMode := .F.
                   oc := ob:aColumns[2]
                   oc:lEdit     := .T.
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nAlign    := DT_LEFT
                   oc:nEditMove := 0    // ���������� ������
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // ��������� ����� ���� ����������� �������
                         oc:nClrFore    := CLR_RED          // ��������� ����� ������ ����������� �������
                         oc:hFont       := hFont            // ��������� ����� ����������� �������
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         // ����� �� �������� ���������� ������ �������
                         //oc:nWidth := GetTextWidth( Nil, "0000", hFont )   // ���-�� ������
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   ob:lNoHScroll := .T.   // ��� ������ ��������������� ���������
                   ob:oHScroll   := NIL
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// ����� END TBROWSE
                    Local oc, nw := 0, nn, nc := ob:nColumn("F_NAME")
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       IF oc:cName == "ARRAYNO"
                          nn := oc:nWidth
                          // ��� ����� ������ ���������� ������ �������
                          oc:nWidth := GetTextWidth( Nil, "00000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                       // ������ �������������� � ������� 2 - F_CHK
                       IF hb_enumindex(oc) > nc
                          oc:bGotFocus := {|nold,ncel,ob|
                                           nold := ob:nColumn("F_CHK")
                                           IF ncel != nold
                                              ob:nCell := nold
                                              ob:DrawSelect()
                                              DO EVENTS
                                           ENDIF
                                           Return Nil
                                           }
                       ENDIF
                    NEXT
                    IF !Empty(nn)
                       oc := ATail(ob:aColumns)
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // �������� ������� � ������� --> tsb_util.prg
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }  ;
                     }

   // ��������� ������� �� ����
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }  ;
                     }
RETURN oTsb

///////////////////////////////////////////////////////////////////
// ������ ������ �������
STATIC FUNCTION CalculatColumnWidthsDfc(aXDim,nCol,nWTsb)
   LOCAL aDim, v, a, i, hFont, nW, aWSize

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle("Normal")
   aWSize := Array(Len(aDim[1]))
   aFill(aWSize, 0)

   FOR EACH a IN aDim
       FOR EACH v IN a
           i := hb_enumindex(v)
           // ����� ������ 2 �������
           //IF i > nCol ; LOOP
           //ENDIF
           // ������ ������ �� ��� �������, �.�. ����� ����� ������ �������
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           v  += "HH"  // �������
           nW := GetTextWidth( Nil, v, hFont )
           nW := IIF( nW > 400, 400, nW )
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT

   //oTsb:aNumber := { 1, 30 }                // ������� ��������� � � ������
   // ��� ������� 2 ������ ��� ������ ������ ������, ����� ������� 1
   //aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ������ ������ ������ ������� � ����������� �� ������ ������
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb,"nCol=",nCol
   //?? "�������:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
STATIC FUNCTION myTsbEditDfct( oBrw )
   LOCAL oCol, cCol, nI

   //? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // oCol:lEdit := .T. // ������ ���� oc:lEdit  := .T.
      // oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ����� �� �����
      // oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> ����� �� �����
      IF "F_CHK" $ cCol
         oCol:lEdit     := .T.   // ������� ����
         oCol:nEditMove := 0     // ����. ����������� ������� ����� :Edit()
         oCol:bPrevEdit := {|xv,ob| xv = ob, DoEvents() , ob:Cargo:nModify++ }         // ������� ���������
// ---------- ������ ������� ----------
//      1              2              3         4               5           6      7
// 1  {.T., "���������� ��� ������", 108, "��� ��������", "�����������", "1-����", 35}
// 2  {.F., "������", 240, "�� -> �������� ������", "������", "1-����", 1}
// 3  {.F., "����� �������", 221, "�� -> ��", "�����������", "3-���", 2}
// 4  {.F., "����� �������������", 101, "��� ��������", "�����������", "������� ���� (�� 17:30)", 3}
         oCol:bPostEdit := {|val, brw| // ����� �����
                                       Local nArr := brw:Cargo:nArray          // �������.������� 1-oBrw:aArray, 2-oBrw:Cargo:aIsx
                                       Local lChk, id, a, b, i, j := 0
                                       // :nAt �� ��������� � ���������� ������� � ������� ��-�� ���������� �������
                                       brw:Cargo:nChk += IIF( val, 1, -1 )     // ������� ���-�� ��������
                                       lChk := val
                                       b    := brw:Cargo:aIsx
                                       If nArr == 1                     // ������ ������� ��� ���������-���������������
                                          a  := brw:aArray[brw:nAt]     // ������� ������
                                          id := a[7]                    // id � ����� ��������
                                          ? ")-~-~->", nArr, ":nAt=", brw:nAt, val, "id=", id, a[2]
                                          For i := 1 To LEN(b)
                                             //? i, ")", id, "==", b[i][7], b[i][2]
                                             If id == b[i][7]
                                                j := i
                                                Exit
                                             Endif
                                          Next
                                          ?? "j=",j
                                          If j > 0
                                             brw:Cargo:aIsx[j][1] := lChk
                                             ?? brw:Cargo:aIsx[j][2] , brw:Cargo:aIsx[j][7]
                                          Endif
                                       Else
                                          // ������� ������, ������ � ��� ��� ������ - ����������
                                          a  := brw:aArray[brw:nAt]     // ������� ������
                                          id := a[7]                    // id � ����� ��������
                                          ? "]=~=~=>", nArr, ":nAt=", brw:nAt, val, "id=", id, a[2]
                                          For i := 1 To LEN(b)
                                             //? i, "==", b[i][7] , b[i][2]
                                             If id == b[i][7]
                                                j := i
                                                Exit
                                             Endif
                                          Next
                                          ?? "j=",j
                                          If j > 0
                                             brw:Cargo:aIsx[j][1] := lChk
                                             ?? brw:Cargo:aIsx[j][2] , brw:Cargo:aIsx[j][7]
                                          Endif
                                       Endif
                                       _wSend(5,brw:cParentWnd)
                                       Return Nil
                                       }
      ENDIF

      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� ������������� - ��� ������� �� ��������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

