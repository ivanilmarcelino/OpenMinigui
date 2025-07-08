/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * _TBrowse() ������ ������� ��� �������������� ����� �������
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Tovar( nY, nX, nGBtn, nW, nWBtn, nHIco )
   LOCAL lIco, nHBtn, aBtnCap, nBtnLen, aFont2, nHBtn2, oBtn := oHmgData()
   LOCAL lRow := .T.  // ������ �� �����������
   DEFAULT nWBtn := 0, nHIco := 0

#ifdef KEY_ENG // for this project demo1-en.hbp
   aBtnCap := { "Save", "Cancel" }
#else
   aBtnCap := { "���������", "��������" }
#endif

   IF nHIco == 0
      nHIco  := myScreenIconSize(App.Cargo:aDisplayMode[2])  // ������ ������ �� ������
      nHIco  := 48
   ENDIF
   lIco      := .T.  // ����������� ������ ������
   nHBtn     := nHIco + 15
   oBtn:aCap := aBtnCap
   oBtn:aObj := { "_3Save", "_3Cancel"  }
   nBtnLen   := LEN(aBtnCap)
   IF nWBtn == 0
      nWBtn  := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ������ ������
   ENDIF
   oBtn:aWBtn:= { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr := { { 35,179, 15} , {254, 73, 83} }
   oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(�) - ����� ������� �� ������
   oBtn:aObj := { "_3Save", "_3Cancel"  }
   oBtn:aIco := { {"iFloppy48x1"   , "iFloppy48x2"  , lIco, nHIco} ,;
                  {"iReturn48x1"   , "iReturn48x2"  , lIco, nHIco}  }
   aFont2    := GetFontParam(GetFontHandle("ComSanMS"))
   oBtn:aFnt := { aFont2[1], aFont2[2], aFont2[3] }  // ����� ��� ���� ������
   nHBtn2    := aFont2[2] * 1                        // 1 ������ ������ �� �������
   nHBtn     := MAX(nHBtn,nHBtn2)                    // ������������� ������ ������

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_Tovar_Outfit(oBrw1,cForm)
   LOCAL cPrnt, hWnd, aRet, nWBtn, nHIco, nHText, cIcon, cTitle, oBrw, oTsb
   LOCAL nY, nX, nW, nH, nG, aBClr, cFont, nFSize, nHUp, owc, nWText
   LOCAL cMsg, nY2, nX2, nW2, nH2, nWTsb, nHTsb, oGet, oWnd, cRetAls
   DEFAULT cForm := "Tsb_Win_Outfit"

   ? "======",ProcNL(),"oBrw1=",oBrw1:cAlias,oBrw1:cParentWnd,"cForm=",cForm
   ////////// ������� ���� ���
   cPrnt  := oBrw1:cParentWnd      // ������������ ����
   oWnd   := _WindowObj(cPrnt)     // !!!
   hWnd   := GetFormHandle(cPrnt)
   nY     := GetWindowRow(hWnd)
   nX     := GetWindowCol(hWnd)
   nW     := GetWindowWidth(hWnd)
   nH     := GetWindowHeight(hWnd)
   nG     := 15   // ����� ���������
   aRet   := {}   // ������ ������ - �����, ������ ����� �� �����
   nHIco  := 48
   nWBtn  := 260      // 2 ������
   aBClr  := { 84,183,128}
   cIcon  := "gear48x1"
   cFont  := App.Cargo:cFontName
   nFSize := App.Cargo:nFontSize
   nHText := nFSize * 2

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := "Directory of equipment and works on request"
   cMsg   := "Find:"
#else
   cTitle := "���������� ������������ � ����� �� ������"
   cMsg   := "�����:"
#endif
   cTitle  += SPACE(5) + ProcFile()

   cRetAls := ALIAS()
   DbSelectArea("Ob4orud")
   OrdSetFocus(3)            // "KVIEW == 1 .AND. !Deleted()"
   //OrdSetFocus("KVIEW")    // ������ ����� ����� !!!
   DbGotop()

   // ����������� ��� ���� MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // ����� ���� � ���������� �������
   nW2 := nW * 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE FONT cFont SIZE nFSize BACKCOLOR aBClr            ;
      ON INIT    {|| oBrw:Setfocus(), DoEvents()  }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // ��������� ���� ������ ������ Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr   := This.Backcolor
      owc:cAls    := ALIAS()
      owc:cRetAls := cRetAls
      nY  := nX := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // ������ � ������� ������ ��� �������� / set and display buttons above the table
      nX     := nW - nG - nWBtn*2 - nG
      nHUp   := Draw_BtnEx_Tovar( nY, nX, nG, nW, nWBtn, nHIco)

      nY     := nHUp - nHText - nG
      nX     := nG
      nWText := GetTxtWidth( cMsg, nFSize, cFont, .T. ) + 5
      @ nY, nX LABEL Lbl_Find VALUE cMsg WIDTH nWText HEIGHT nHText ;
        FONTCOLOR WHITE TRANSPARENT BOLD VCENTERALIGN //RIGHTALIGN

      nX += This.Lbl_Find.Width + 2
      // GetBox ��� ������ � �������
      @ nY, nX GETBOX GB_Find OBJ oGet WIDTH 120 HEIGHT nHText VALUE " " ;
        PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
        ACTION       {|| This.Value := "" }   ;
        IMAGE        {"bDelRed24","" }        ;
        ON GOTFOCUS  {|ob| ob := ThisWindow.Cargo:oBrw, ob:nCell := 3, ob:DrawSelect() } ;
        ON CHANGE    {|| Search_TsbDbf( ThisWindow.Object ) } ;
        ON INIT      {|| This.Cargo := .T. }
        //ON LOSTFOCUS {|| This.Cargo := .F., This.Value := space(30) } ;
      This.Cargo:oGet := oGet
      This.Cargo:cGet := "GB_Find"    // ��������� ��� ����������� �������������

      nY    := nHUp         ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// ������� ///////////////////////////////////////////////////////
      oTsb := TablePatam( cForm, owc:cAls, "cTable", nWTsb, cTitle )
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, owc:cAls, "cTable", nY, nX, nWTsb, nHTsb )
      // ����� ������ ����������� �������
      oBrw:Cargo:nModify  := 0                            // ������� ���������
      // ������ ������� �� ����
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cSpTable"
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents() } )
         // ��� ������� + ��� ������� //   ������ �������� ����
         //            VVVV           //   oBtn:aObj := { "_3Save","_3Cancel"}
         :Event({10,"_3Save"}, {|ow,ky,cn,ob| // ���������
                                              ob := ow:Cargo:oBrw
                                              If !IsString(cn)  // ��� ����� ����������� �� ������� ENTER
                                                 cn := "_3Save"
                                              Endif
                                              _SetThisFormInfo(ow)
                                              //MsgDebug(ow:Name,ky,cn,ob:cAlias)
                                              aRet := myTsbCloseLine( ob )
                                              _SetThisFormInfo()
                                              This.&(cn).Enabled := .T.   // �������������� ������
                                              // ob:SetFocus(), ow:Setfocus('Lbl_0')
                                              _wPost(99,ow)
                                              ky := cn
                                              Return Nil
                                              })

         // �������� ������� � �������, ��. ����
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_RETURN   , MsgDebug(...)
         //             _wPost(  32  ,  33  , 34   , 35-"_3Save" , 36

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DbSelectArea(ow:Cargo:cRetAls)
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_3Cancel"}, {|ow| aRet := {}, ow:Release() } )
         :Event({99,"_3Releas"}, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

   END WINDOW

   ACTIVATE WINDOW &cForm

   //IF _IsWindowDefined(cPrnt)               // ����� ��� / you can do it like this
   //   DoMethod(cPrnt, "SetFocus")
   //ENDIF

   IF _IsWindowDefined(oWnd:Name)             // ����������� / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ����������� ��� ���� MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // ������ ������, ���� ����� - ������ ����� �� �����

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_TsbDbf(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw
   LOCAL cGet  := oWnd:Cargo:cGet             // ��� "GB_Find"
   LOCAL cVal, lSwap := .F.
   //-----------------------------------------------------------------
   // ������ ������������ ��� ����� ������� - oTsb:aNumber := {1, 30}
   //-----------------------------------------------------------------
   SET WINDOW THIS TO oWnd
   cVal := Trim( This.&(cGet).Value )
   cVal := upper(cVal)
   SET WINDOW THIS TO

   IF     Len( cVal ) == 0
      oBrw:FilterFTS()       // ������� �������
      lSwap := .T.
   ELSEIF Len( cVal ) > 2    // �� 3-� �������� �����
      oBrw:FilterFTS( cVal, .T. )   // Empty(cVal) ��������� ������ ������
      lSwap := .T.
   ENDIF

   IF lSwap
      oBrw:nCell := 3
      oBrw:Reset()
      DO EVENTS
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCloseLine( oBrw )
   LOCAL aRet, cAls, nCol, cNam, xVal

   aRet  := {}
   cAls  := oBrw:cAlias
   FOR nCol := 1 TO oBrw:nColCount()
       cNam := oBrw:aColumns[ nCol ]:cName
       IF cNam == "ORDKEYNO"  ; LOOP
       ELSEIF cNam == "SELECTOR"  ; LOOP
       ELSE
          xVal := oBrw:GetValue(cNam)
          IF IsString(xVal)
             xVal := ALLTRIM(xVal)
          ENDIF
          AADD( aRet, xVal )
       ENDIF
   NEXT
RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatam(cForm,cAls,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr, nHCell
   //
   oTsb := oHmgData()
   oTsb:cAls           := cAls
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   //---------------------------------------------------------------------------------------------------
   //oTsb:aNumber      := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // ������� ��������� � � ������ / numbering column and its width
   //oTsb:aNumber      := { 1, 30 }                // ������� ��������� � � ������ - ������ ������ !!!
                                                   // �.�. ����� ������������ oBrw:FilterFTS()
   //---------------------------------------------------------------------------------------------------
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   oTsb:nHeightCell    := nHCell                   // ������ �����
   oTsb:lDrawHeaders   := .F.                      // �� ������� � ������� ����� !!! ��� ������ ���������
   oTsb:nHeightHead    := nHCell                   // ������ ����� - ������ ����� �������
   oTsb:nHeightFoot    := nHCell                   // ������ �������
   oTsb:lFooting       := .T.                      // ������� � ������� ������
   oTsb:lSpecHd        := .T.                      // ��������� � ������� ��������� �������
   oTsb:lSuperHd       := .T.                      // ��������� � ������� ����������
   oTsb:cSuperHd       := cTitle                   // ����� �����������
   oTsb:nHeightSuperHd := nHCell                   // ������ �����������
   oTsb:nCellMarginLR  := 0                        // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
   oTsb:uSelector      := 20                       // �������� ����� �������
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "�_Ob4orud", "�_CENA"  , "C_Obor"  , "C_Mast"  , "�_CODE"  , "�_OB1"   , "�_OB2"    }  // ����� ����� �������
   oTsb:aField         := { "Ob4orud"  , "CENA_ALL", "CenaObor", "CenaMast", "KOb4orud", "KOB1ZAIV", "KOB2WORK" }  // ����� ����� ����
   ? ProcNL(), "######## nHCell=", nHCell

#ifdef KEY_ENG // for this project demo1-en.hbp
   //                        1                               2             3         4        5               6                  7
   oTsb:aHead := { "Name;of equipment or work"          , "Price", "Equipment", "Master'y", "Codes"      , "Bid;Group"    , "Work;Group"    }
#else
   oTsb:aHead := { "������������;������������ ��� �����", "����" , "������."  , "�������" , "����;������.", "������;������", "������;�����" }
#endif

   oTsb:aHideCol := {}   // ������ �������, ��������� SELECTOR � ARRAYNO
   aWSize        := DbfCalculatColumnWidths(cAls,oTsb:aField,2,nWTsb)   // ������� ������ ������� - ������� �� 2 �������
   oTsb:aSize    := aWSize                                              // �������� ������ ������� ��� ���

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
   // ��������
   //  ?v oTsb:aName   //
   //  ?v oTsb:aHead   //
   //  ?v oTsb:aSize   //
   //  ?v oTsb:aField  //

   // ����� ������� ������ ������ ����
   oTsb:bInit := {|ob,op| // ��������� ���
                   //ob:Hide()                                    // ������ ������� ��� ���������� ����������
                   //ob:HideColumns( op:aHideCol ,.t.)            // ������ �������
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   //ob:nFreeze     := ob:nColumn("ORDKEYNO")     // ���������� �������
                   //ob:lLockFreeze := .T.                        // �������� ���������� ������� �� ������������ ��������
                   //ob:lNoKeyChar  := .F.                        // ���� � ������ �� ����, ����
                   //myTsbEditDbf(ob,op)                          // �������������� ����� �������
                   ob:nCell        := 1                           // ����������� ������
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // ������ ��������� ���
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // ���� ���������
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - ��� ���
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // ��������� ����� ���� ����������� �������
                         oc:nClrFore    := CLR_RED          // ��������� ����� ������ ����������� �������
                         oc:hFont       := hFont            // ��������� ����� ����������� �������
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         // ����� �� �������� ���������� ������ �������
                         //oc:nWidth := GetTextWidth( Nil, "00000", hFont )   // ���-�� ������
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
                    Local oc, nw := 0, nn, i
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       i := hb_enumindex(oc)
                       IF oc:cName == "ORDKEYNO"
                          nn := oc:nWidth
                          // ��� ����� ������ ���������� ������ �������
                          oc:nWidth := GetTextWidth( Nil, "00000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                       //? i, oc:cName, oc:nWidth
                    NEXT
                    IF !Empty(nn)
                       oc := ATail(ob:aColumns)
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // �������� ������� � �������
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_RETURN, {|ob| _wPost(35, ob:cParentWnd, ob) } }  ;
                     }

   // ��������� ������� �� ����  --> tsb_util.prg
   oTsb:aEvents   := { ;                                       //!!!
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow     } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow     } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow     } }, ;
        {35, {|ow,ky,ob| _wPost("_3Save",ow)  , ky:=ow:=ob                } }, ;
        {36, {|ow,ky,ap| MsgDebug(ap[1],ap[2],ap[3],ap[4]:cAlias), ky:=ow } }  ;
                     }

   // ������� ���� ����� �� ������� � �������
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // ��� ��� !!!
   // ������ ���� ����� �� ������� � �������
   oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(36, ob:cParentWnd, {p1,p2,p3,ob}) }
   // ����� ���� ����� �� ������� � �������
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

///////////////////////////////////////////////////////////////////
// ������ ������ �������
STATIC FUNCTION DbfCalculatColumnWidths(cAls,aFld,nCol,nWTsb)
   LOCAL v, i, j, hFont, nW, aWSize, cFld

   hFont  := GetFontHandle("Normal")
   aWSize := Array(Len(aFld))
   aFill(aWSize, 0)

   DBSELECTAREA(cAls)
   FOR j := 1 TO  ORDKEYCOUNT()
       ORDKEYGOTO(j)
       FOR EACH cFld IN aFld
           i := hb_enumindex(cFld)
           // ������ ������ �� ��� �������
           v := (cAls)->&cFld
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           v  := ALLTRIM(v)
           v  += "HH"  // �������
           nW := GetTextWidth( Nil, v, hFont )
           nW := IIF( nW > 400, 400, nW )
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT
   DbGotop()

   //oTsb:aNumber := { 1, 30 }                // ������� ��������� � � ������
   // ��� ������� 2 ������ ��� ������ ������ ������, ����� ������� 1
   //aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ������ ������ ������ ������� � ����������� �� ������ ������
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb,"nCol=",nCol
   //?? "�������:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
/*STATIC FUNCTION myTsbEditDbf( oBrw )
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
      //IF "F_CHK" $ cCol
      //   oCol:lEdit     := .T.   // ������� ����
      //   oCol:nEditMove := 0     // ����. ����������� ������� ����� :Edit()
      //   oCol:bPrevEdit := {|xv,ob| xv = ob, DoEvents() , ob:Cargo:nModify++ }         // ������� ���������
      //   oCol:bPostEdit := {|val, brw| brw:Cargo:nChk += IIF( val, 1, -1 ) ,;          // ������� ���-�� ��������
      //                                 brw:aColumns[2]:cFooting := HB_NtoS(brw:Cargo:nChk) ,;
      //                                 brw:DrawFooters()  }
      //ENDIF

      //IF "F_NAME" $ cCol
      //   oCol:lEdit     := .F.
      //   oCol:nEditMove := 0  // ����. ����������� ������� ����� :Edit()
      //   oCol:bPrevEdit := {|xv,ob| ob:nCell := 2, ob:DrawSelect(), xv = ob, DoEvents() , ;
      //                              ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0), .F. }
      //ENDIF
      //oCol:bPrevEdit := {|xv,ob| ob:nCell := 3, ob:DrawSelect(), xv = ob, DoEvents() , ;
      //                           ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0), .F. }
                                                                    // ������� .F. ����� �� �������� get-������
      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� ������������� - ��� ������� �� ��������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL */

