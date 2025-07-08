/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * �������� �� ������� / Card by table
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Card( nY, nX, nGBtn, nW )
   LOCAL nHIco, lIco, nWBtn, nHBtn, aBtnCap, nBtnLen, aFont2, oBtn
   LOCAL nHBtn2, lRow := .T.  // ������ �� �����������

   oBtn       := oHmgData()
   aBtnCap    := {"Button;(1)","Button;(2)", "Button;(3)", "Button;(4)", "Exit" }
   nHIco      := myScreenIconSize(App.Cargo:aDisplayMode[2])  // ������ ������ �� ������
   nHIco      += 5
   lIco       := .T.  // ����������� ������ ������
   nHBtn      := nHIco + 15
   oBtn:aCap  := aBtnCap
   oBtn:aObj  := { "_Card1" ,"_Card2" ,"_Card3"  , "_Card4", "_CardExit"  } // ����� �������
   nBtnLen    := LEN(aBtnCap)
   nWBtn      := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ������ ������
   oBtn:aWBtn := { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr  := { {26,84,91}, {115,96,241}, {0,215,87}, {38,158,218} , {254, 73, 83} }
   //oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(�) - ����� ������� �� ������ - �� ���������
   aFont2     := GetFontParam(GetFontHandle("FntBtn_1"))
   oBtn:aFnt  := { aFont2[1], aFont2[2], aFont2[3] }       // ����� ��� ���� ������
   nHBtn2     := aFont2[2] * 4                             // 2 ������ ������ �� �������
   nHBtn      := MAX(nHBtn,nHBtn2)                         // ������������� ������ ������
   nHIco      := nHBtn - 10                                // ������-������ ������ �� ������
   oBtn:nHBtn := nHBtn                                     // ������ ������� ������ ������
   oBtn:aIco  := { {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iReturn48x1"   , "iReturn48x2" , lIco, nHIco }  }
   oBtn:aFntClr  := { BLACK, OLIVE }
   ? ProcNL(), "^^^^^^^^^^^ 2 rows of buttons=", nHBtn, nHBtn2

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Card_for_table(oWnd,ky,oBrw1,cIcon,cTitle,aBClr)
   LOCAL aRet, owc, cForm, nTekRec, cAdres, aFont2, cAls, nWTxt
   LOCAL nY, nX, nW, nH, nG, nHUp, nY2, nX2, nW2, nH2, nWTsb, nHTsb
   LOCAL cFont, nFSize, lCaEdit, cEdit, aEdBClr

   ? ProcNL(),oWnd,ky,oBrw1
   // ������� ���� ���
   //oWnd  := _WindowObj( oBrw1:cParentWnd )   // ������������ ����
   nY      := GetWindowRow(oBrw1:hWnd)
   nX      := GetWindowCol(oBrw1:hWnd)
   nW      := GetWindowWidth(oBrw1:hWnd)
   nH      := GetWindowHeight(oBrw1:hWnd)
   // ���������� ���� ����� ��� ��������
   nY      := oWnd:Row   ; nX := oWnd:Col
   nW      := oWnd:Width ; nH := oWnd:Height
   nG      := 15                                // ����� ��������� �� �����
   aRet    := {}                                // ������ ������ - �����, ������ ����� �� �����
   cForm   := oWnd:Name + "_Card_Modal"
   aFont2  := GetFontParam(GetFontHandle("FntBtn_1"))
   cFont   := aFont2[1]
   nFSize  := aFont2[2]
   cAls    := oBrw1:cAlias
   nTekRec := RECNO()         // ����� ������ ��������
   cAdres  := ALLTRIM((cAls)->ADRESPRN)
   lCaEdit := .F.
   IF (cAls)->Dateza > DATE() - 30
      lCaEdit := .T.
   ENDIF
   cEdit   := IIF( lCaEdit, "[editing allowed]", "[editing prohibited]" )
   aEdBClr := IIF( lCaEdit, BLUE, RED )

   // ����������� ��� ���� MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // ����� ���� � ���������� �������
   nW2 := nW * 0.94
   nH2 := nH * 0.98
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY
   cTitle  += SPACE(5) + ProcFile()

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL ICON cIcon BACKCOLOR aBClr                               ;
      ON INIT    {|| _wSend(0)                    }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // ��������� ���� ������ ������ Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr    := This.Backcolor
      owc:ObjWin   := This.Object
      owc:hWin     := This.Handle                     // ����� ����� ����
      owc:Name     := This.Name
      owc:oWin     := This.Object
      owc:ahIcoDel := {}                              // ��� �������� ������� ������ � �����
      owc:nG       := nG                              // ������ �� ���� ����
      owc:aObjHide := {}                              // ��� �������� ������
      owc:aTsbDel  := {}                              // ��� �������� 5-������
      owc:cAls     := ALIAS()
      owc:nRecno   := RECNO()                         // ����� ������ ������
      owc:nTekRec  := nTekRec                         // ������� ��������� ������ ��� ���
      owc:lCaEdit  := lCaEdit

      nY := nX := nG
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // ������ � ������� ������ ��� �������� / set and display buttons above the table
      nHUp  := Draw_BtnEx_Card( nY, nX, nG, nW )
      nY    := nHUp

      nWTxt := GetTxtWidth( cEdit, nFSize, cFont, .T. ) + 10
      @ nY, nW - nG - nWTxt LABEL Lbl_Edit VALUE cEdit WIDTH nWTxt HEIGHT nFSize*2 ;
        FONT cFont SIZE nFSize FONTCOLOR WHITE BACKCOLOR aEdBClr CENTERALIGN

      @ nY, nX LABEL Lbl_Adr VALUE cAdres WIDTH nW-nG*2-nWTXt HEIGHT nFSize*4 ;
        FONT cFont SIZE nFSize FONTCOLOR WHITE TRANSPARENT
      myBigSizeLabel(ThisWindow.Name,"Lbl_Adr")

      nY    += This.Lbl_Adr.Height + nG/2
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      owc:aTbl := { nY, nX, nWTsb, nHTsb, "����� ��� ������� / Space for table" }

      @ nY, nX LABEL Lbl_Wait VALUE "TABLES ARE BEING CONSTRUCTED" WIDTH nWTsb HEIGHT nHTsb ;
        SIZE 42 FONTCOLOR WHITE TRANSPARENT CENTERALIGN VCENTERALIGN BORDER 
      //myBigSizeLabel(ThisWindow.Name,"Lbl_Wait")

      // �� ��������� oMenu:aObj := { "_3Tab1", "_3Tab2", "_3Tab3", "_3Tab4", "_3Tab5", "_3Tab6" }
      // �� form_card_6tsb.prg - ������ ���� ������� �� ���� ������� "_3Tab0"
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| // ON INIT 
                            DoEvents()
                            /////// ������� 6 ���� ////////////
                            DBSELECTAREA(ow:Cargo:cAls)
                            Card_6Tsb(ow:Cargo:oWin,ow:Cargo:aBClr,{},"",ow:Cargo:lCaEdit)
                            This.Lbl_Wait.Hide
                            //_wSend("_3Tab0",ow,"_3Zaivka")
                            DoEvents()
                            Return Nil
                            } )

         // ��� ������� + ��� ������� //   ������ �������� ����
         //            VVVV           //   oBtn:aObj := { "_Card1" ,"_Card2" ,"_Card3"  , "_Card4", "_CardExit"  }
         :Event({10,"_Card1"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({11,"_Card2"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({12,"_Card3"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({13,"_Card4"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         // :Event( 32, 33, 34 ������ - VK_F2 VK_F3 VK_F4

         :Event({40,"_3Tab0"}, {|ow,ky,cn| // 6 ������ �� ����
                                              Local i, obr, a6Tbl
                                              Local aTsb  := ow:Cargo:aTsbDel
                                              Local aBrw  := ow:Cargo:aBrw
                                              Local nBrw  := ow:Cargo:nBrw      // ���. ��� � ������
                                              Local obc   := This.&(cn).Cargo
                                              Local nBtn  := obc:nBtn           // ����� ������
                                              Local cForm := ow:Name
                                              Local owc   := ow:Cargo

                                              _LogFile(.T., "  -->> Event: ",ky, cn, ow:Name, "--- 6 ������ ---")
                                              ? "  -->> " + ProcNL(), "nBrw=",nBrw, owc:nBrw, "aBrw=", aBrw
                                              ?? "  ����� ������/�������",nBtn

                                              a6Tbl := HMG_GetFormControls(cForm, "TBROWSE")
                                              //?v a6Tbl

                                              For i := 1 TO LEN(aTsb)
                                                 DoMethod(cForm, aTsb[i], "Hide")
                                                 //? "  -->> ", i, aTsb[i], "Hide"
                                              Next

                                              DoMethod(cForm, aTsb[nBtn], "Show")
                                              This.&(aTsb[nBtn]).Setfocus
                                              This.&(aTsb[nBtn]).Show

                                              owc:nBrw := This.&(cn).Cargo:nBtn  // ���. ��� � ������

                                              FOR EACH obr IN ow:Cargo:aBrw
                                                 i := hb_EnumIndex(obr)
                                                 //? "   >>", i, obr:cControlName
                                                 //obr:Hide()
                                              NEXT
                                              //IF owc:nBrw > 0 ; AEval(owc:aBrw, {|ob| ob:Hide() })
                                              //ENDIF
                                              //obr := owc:aBrw[ owc:nBrw ]
                                              //obr := ow:Cargo:aBrw[n]
                                              //obr:SetFocus()
                                              //obr:Show()
                                              This.&(cn).Enabled := .T.
                                              DO EVENTS
                                              Return Nil
                                              } )

         :Event(42, {|ow,ky| // ����������� ��������� �������� �� ������� �������, ������� (2)  ob:aColumns[2]:bFLClicked
                      Local aIsx, obr, cName, oCol, nCol, nI, lChk, aVal, aDim, i, aUser, nBtn, nAlg, owc
                      Local aHide := {"F_NN","F_PROCES", "F_BASE", "F_READ", "F_WRITE", "F_WINDOWS", "F_ACCESS","V_CALC", "V_SPR","V_AADD"}
                      //                 3         4         5        6           7             8        9         10        11      12
                      owc  := ow:Cargo
                      nBtn := owc:nBrw               // ������� ��� � ������
                      obr  := owc:aBrw[nBtn]         // ���.������ �� ������� �������� �������
                      aIsx := obr:Cargo:aIsxTbl      // �������� ������ � ������� �������� �������
                      lChk := obr:Cargo:lChkTsb      // ������ �������� ����� �������
                      //
                      obr:Hide()                     // ������ ������� ��� ���������� ����������
                      obr:aArray := aIsx             // ��������� ������ ��� �� �������� ������
                      obr:nLen   := LEN(aIsx)
                      obr:Reset()
                      //
                      IF lChk
                         // "---- ����� ���� ������� ----"
                         FOR EACH oCol IN obr:aColumns
                            nCol := hb_EnumIndex(oCol)
                            oCol:lVisible := lChk
                         NEXT
                         //obr:lDrawSpecHd   := .T.  - ������ ����� ���
                      ELSE
                         //obr:lDrawSpecHd   := .F.  - ������ ����� ���
                         // "---- ������ ����� ������� ----"
                         FOR EACH oCol IN obr:aColumns
                            nCol  := hb_EnumIndex(oCol)
                            cName := oCol:cName
                            IF "_" $ cName             // �������-2
                               //oCol:lVisible := .F.
                            ENDIF
                            FOR nI := 1 TO LEN(aHide)
                               IF cName == aHide[nI]
                                  oCol:lVisible := .F.
                               ENDIF
                            NEXT
                         NEXT
                         // ���������� �����
                         obr:Hide()
                         aUser := {}
                         aDim  := obr:aArray
                         aDim  := ASORT( aDim,,, { |x, y| x[ADIM_SORT] < y[ADIM_SORT] } )
                         For i := 1 TO Len(aDim)
                            aVal := aDim[i]
                            If aVal[ADIM_SORT] # 0
                               AADD( aUser, aVal )
                            Endif
                         Next
                         If Len(aUser) == 0
                            aVal    := ACLONE(aDim[1])
                            aVal[1] := "��� ����� ������� ��� ������ !"
                            aVal[2] := "��������� ���������� ������ ����� (3) ������� !"
                            AADD( aUser, aVal )
                            AADD( aUser, aVal )
                         Endif
                         // ����� ��������� ������ ��� �� ����� !!!
                         obr:aArray := aUser
                         obr:nLen   := LEN(aUser)
                         obr:Reset()
                         obr:Refresh()
                         obr:Show()
                         DO EVENTS
                      ENDIF
                      // ������������ �������
                      nAlg := iif( lChk, DT_RIGHT, DT_CENTER )
                      obr:nCell  := obr:Cargo:nCellFoc
                      obr:oHScroll := NIL
                      obr:Refresh()
                      obr:DrawSelect()
                      obr:SetFocus()
                      obr:Show()
                      DO EVENTS
                      obr:nAlignSupHdSet(1,, nAlg)
                      obr:SendMsg(WM_KEYDOWN, VK_RIGHT, 0) // _PushKey(VK_RIGHT)
                      DO EVENTS
                      obr:Cargo:lChkTsb := ! lChk  // ����� ���������
                      ky := nBtn
                      Return Nil
                      } )

         //  ob:aColumns[3]:bFLClicked - ���� ���������� ������������
         :Event(43, {|ow,ky,cN| _SetThisFormInfo(ow) , myContexMenuSort(ow,ky,cN) ,;
                                _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_CardExit"}, {|ow,ky,cn| // �����
                                             _LogFile(.T., "  -->> Button: ",cn, ow:Name, ky)
                                             _SetThisFormInfo(ow)
                                             IF  lCaEdit
                                                // ������ ���� ������ ����
                                                ThisWriteRecno(owc)
                                             ENDIF
                                             _SetThisFormInfo()
                                             aRet := {}
                                             _wSend(99,ow:Name)
                                             Return Nil
                                             } )

         :Event({99,"_CardRls"}, {|ow| ow:Release() } )
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

////////////////////////////////////////////////////////////////////////////////////////
FUNCTION ThisWriteRecno(owc) // ������ ���� ������
   LOCAL cMsg, nTime, cStr, cAls, i, j, aTsb, aTsbIni, iWrt, aBrw, oBrw, cForm, cBrw

   ? ProcNL(), "----- ������ �������� � ����", owc:cAls
   owc:cTxtZaivk := "����� ����� �� ������!"  // �������
   cStr    := ""
   cAls    := owc:cAls
   nTime   := VAL( SUBSTR(TIME(), 1, 2)+SUBSTR(TIME(), 4, 2) )
   aTsb    := owc:aTsbDel         // ������� ��� ��������
   aTsbIni := owc:aTsbIni         // ��������� ���������� ����� � �������
   aBrw    := owc:aBrw            // ������ �������� ���
   cForm   := owc:Name
   iWrt    := 0

   WaitWindow( {'������ ������ � ��-' + owc:cAls, App.ExeName }, .T., 400, 16, NIL, WHITE, {192,0,255} )
   DbSelectArea(cAls)
   DbGoto(owc:nRecno)            // ����� ������ ���� ������

   If Len(aTsb) > 0 // ������� ��� ��������
      DbGoto(owc:nRecno)
      For i := 1 TO Len(aTsb)   // { "cTable_1", "cTable_2", ...
         cBrw := aTsb[i]
         ? "   >>> RECNO()=",RECNO(), i, cForm, cBrw
         If _IsControlDefined( cBrw, cForm )
            oBrw := aBrw[i]
            If Len(owc:aWrtIni) > 0
               For j := 1 To Len(owc:aWrtIni)
                  If i == owc:aWrtIni[j,2]
                     ?? "[������ "+ owc:aWrtIni[j,1] + "]"
                     oBrw:Cargo:nModify++
                     iWrt++
                  Endif
               Next
            Endif
            ?? "oBrw=", oBrw, cFileNoPath(aTsbIni[i]), oBrw:Cargo:nModify
            IF oBrw:Cargo:nModify >  0           // ������� ���������
               iWrt++
               ArrayDbfSave(oBrw:aArray)         // ������ �� ������� � ����
                      // V--- �������� ������ ����� !!!
               IniSave(oBrw:Cargo:aIsxTbl,aTsbIni[i])  // ������ ���������� ������
               ?? "[write dbf]"
            ENDIF
         Endif
      Next
   Endif

   If (cAls)->(RLock())
      IF iWrt > 0
         //(cAls)->KOPERAT  := M->nOperat  // ��� ������ ������ ?
         (cAls)->DATEVVOD := DATE()
         (cAls)->TIMEVVOD := nTime
         (cAls)->( DBCommit() )
         (cAls)->( DBUnlock() )
      ENDIF
   ELSE
      cMsg := "�� " + cAls + " ! "
      cMsg += "������ " +HB_NtoS(owc:nRecno)+ " ������������� !;"
      cMsg += "���������� ��� ��� ���� �����"
      AlertStop( cMsg, "������!", "iMgStop128", 72, {RED} )
      ? ProcNL(), AtRepl( ";", cMsg, CRLF )
   ENDIF

   DbGoto(owc:nTekRec)    // ������� ��������� ������ ��� ���
   WaitWindow()

RETURN NIL
