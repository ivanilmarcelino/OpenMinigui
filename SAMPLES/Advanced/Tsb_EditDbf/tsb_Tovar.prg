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
STATIC FUNCTION Draw_BtnEx_Tovar( nY, nX, nGBtn, nW )
   LOCAL nHIco, lIco, nWBtn, nHBtn, aBtnCap, nBtnLen, aFont2, nHBtn2, oBtn
   LOCAL lRow := .T.  // ������ �� �����������

#ifdef KEY_ENG // for this project demo1-en.hbp
   aBtnCap := {"Ins-insert;recno","Del-delete;recno", "Delete;all recno", "Save", "Cancel" }
#else
   aBtnCap := {"Ins-�������;������","Del-�������;������", "�������;�� ������", "���������", "��������" }
#endif

   oBtn      := oHmgData()
   nHIco     := myScreenIconSize(App.Cargo:aDisplayMode[2])  // ������ ������ �� ������
   //nHIco   := 48
   lIco      := .T.  // ����������� ������ ������
   nHBtn     := nHIco + 15
   oBtn:aCap := aBtnCap
   nBtnLen   := LEN(aBtnCap)
   nWBtn     := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ������ ������
   oBtn:aWBtn:= { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr := { GRAY, GRAY, GRAY, { 35,179, 15} , {254, 73, 83} }
   oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(�) - ����� ������� �� ������
   oBtn:aObj := { "_2InsRec" ,"_2DelRec" ,"_2DelAll"  , "_2Save", "_2Cancel"  }
   oBtn:aIco := { {"iRecIns48x1"   , "iRecIns48x2"  , lIco, nHIco} ,;
                  {"iRecDel48x1"   , "iRecDel48x2"  , lIco, nHIco} ,;
                  {"iRecAll48x1"   , "iRecAll48x1"  , lIco, nHIco} ,;
                  {"iFloppy48x1"   , "iFloppy48x2"  , lIco, nHIco} ,;
                  {"iReturn48x1"   , "iReturn48x2"  , lIco, nHIco}  }
   aFont2    := GetFontParam(GetFontHandle("ComSanMS"))
   oBtn:aFnt := { aFont2[1], aFont2[2], aFont2[3] }  // ����� ��� ���� ������
   nHBtn2    := aFont2[2] * 4                        // 2 ������ ������ �� �������
   ? ProcNL(), nHIco, "$$$$$$$ 2 rows of buttons=", nHBtn, nHBtn2, HB_ValToExp(App.Cargo:aDisplayMode)

   IF App.Cargo:aDisplayMode[2] > 799
      nHBtn := MAX(nHBtn,nHBtn2)                    // ������������� ������ ������
   ENDIF

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_Tovar_HMG(oBrw1,cVal13,aLine,cForm)   // ����� �� ������ myWinCalc()
   LOCAL oWnd, aRet, cTitle, cIcon, aXTovar, owc, cMsg
   LOCAL nY, nX, nW, nH, nG, aBClr, nHUp, a1Dim, aLang
   LOCAL nY2, nX2, nW2, nH2, nWTsb, nHTsb, oBrw, oTsb
   DEFAULT cForm := "Tsb_Win_Tovar"

   //MsgDebug(aDim,"oDop=",oDop,"aLine=",aLine,cForm)
   ? ProcNL(),"cVal13=",cVal13,"aLine=",aLine,cForm

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

   nG     := 15   // ����� ��������� �� �����
   aRet   := {}   // ������ ������ - �����, ������ ����� �� �����
   aBClr  := {143,153,219}
   cIcon  := "gear48x1"
   cTitle := aLine[ACOL_1] + SPACE(5) + ProcFile()

#ifdef KEY_ENG // for this project demo1-en.hbp
   cMsg  := "selection from directory"
   aLang := { "Insert an entry"   , "You want to insert a NEW record into the table ?;" ,;
              "Insert an entry"   , "Do you want to delete this recno ?;"    ,;
              "Clearing the table", "Do you want to drop the ENTIRE table?;" ,;
              "The table is empty, there are no records!;" }
#else
   cMsg  := "����� �� �����������"
   aLang := { "������� ������" , "�� ������ �������� ����� ������ � ������� ?;" ,;
              "�������� ������", "�� ������ ������� ��� ������ ?;"  ,;
              "������� �������", "�� ������ ������� ��� ������� ?;" ,;
              "������� ������, ��� ������� !;" }
#endif

   // ������� �� 1-������
   //          1   2    3   4   5    6    7   8  9  10
   a1Dim := { "", "", cMsg, 0, 0.0, 0.0, 0.0, 0, 0, 0  }
   IF LEN(cVal13) == 0
      aXTovar := { a1Dim }
   ELSE
      aXTovar := GetLineDim(cVal13)
   ENDIF

   // ����������� ��� ���� MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // ����� ���� � ���������� �������
   nW2 := nW //* 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE ICON cIcon BACKCOLOR aBClr                        ;
      ON INIT    {|| _wSend(0)                    }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // ��������� ���� ������ ������ Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr := This.Backcolor
      owc:aLang := aLang
      nY        := nX := nG
      nW        := This.ClientWidth
      nH        := This.ClientHeight

      @ 0, 0 LABEL Lbl_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // ������ � ������� ������ ��� �������� / set and display buttons above the table
      nHUp  := Draw_BtnEx_Tovar( nY, nX, nG, nW )

      nY    := nHUp         ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// ������� /////////////////////////////////////////////////
      oTsb := TablePatamTvr( cForm, aXTovar, "cTableTvr", nWTsb, cTitle)
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXTovar, "cTableTvr", nY, nX, nWTsb, nHTsb )
      // ����� ������ ����������� �������
      oBrw:Cargo:nCellFoc := 2                            // �������� ������
      oBrw:Cargo:nModify  := 0                            // ������� ���������
      oBrw:Cargo:a1Dim    := a1Dim                        // ������ ������ �������
      oBrw:Cargo:aLang    := aLang
      // ������ ������� �� ����
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cTableTvr"
      This.Cargo:aIsx     := oBrw:aArray                  // �������� ������ ����� !!!
      This.Cargo:aItog    := oTsb:aItogo                  // ���� ��� ������������ ����� (������ �������)
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents(), _wSend("_2Itogo",ow) } )
         // ��� ������� + ��� ������� //   ������ �������� ����
         //            VVVV           //   oBtn:aObj := { "_2InsRec","_2DelRec","_2DelAll","_2Save","_2Cancel"}
         :Event({10,"_2InsRec"}, {|ow,ky,cn| // ������� ������
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local aRec := ACLONE(ob:Cargo:a1Dim)
                                             Local nK, lYes := .F.
                                             If !IsString(cn)  // ��� ����� ����������� �� ������� INS
                                                cn := "_2InsRec"
                                             Endif
                                             _SetThisFormInfo(ow)
                                             IF AlertYesNo(aLng[2]+";", aLng[1],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                AAdd(ob:aArray, aRec)
                                                nK := Len(oBrw:aArray)
                                                ob:Enabled(.T.)
                                                ob:Reset()
                                                ob:GotoRec(nK)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             ENDIF
                                             _SetThisFormInfo()

                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         :Event({13,"_2DelRec"}, {|ow,ky,cn| // �������� ������
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local cVal := ALLTRIM(ob:aArray[1][1])
                                             Local i, k, oc, xVal, lNoRows, lYes := .F.
                                             If !IsString(cn)  // ��� ����� ����������� �� ������� DEL
                                                cn := "_2DelRec"
                                             Endif
                                             lNoRows := Len(ob:aArray) == 1 .and. LEN(cVal) == 0 // .T. - ��� ������ �������
                                             _SetThisFormInfo(ow)
                                             If AlertYesNo(aLng[4]+";", aLng[3],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                If Len(ob:aArray) == 1
                                                   // ������ ������� �������
                                                   k := 0
                                                   IF ob:nColumn("SELECTOR", .T.) > 0 ; k++
                                                   ENDIF
                                                   IF ob:nColumn("ARRAYNO", .T.) > 0  ; k++
                                                   ENDIF
                                                   For i := 1 TO Len(ob:aColumns)
                                                      oc := ob:aColumns[ i ]
                                                      If oc:cName $ "SELECTOR,ARRAYNO" ; LOOP
                                                      Endif
                                                      xVal := ob:aArray[1][i - k]
                                                      ob:aArray[1][i - k] := NIL
                                                   Next
                                                   ob:DrawSelect()   // ob:Refresh()
                                                   ? "$$$$$$$$$$$$$ ===> ob:aArray[1]", ob:aArray[1]
                                                   ?v ob:aArray[1]  ; ?
                                                Else
                                                   ob:DeleteRow( ) // Array method - Delete selected row
                                                   ob:Reset()
                                                Endif
                                                ob:Enabled(.T.)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             Endif
                                             _SetThisFormInfo()
                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         :Event({15,"_2DelAll"}, {|ow,ky,cn| // ������� ���� �������
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local aRec := ACLONE(ob:Cargo:a1Dim)
                                             Local lYes := .F.
                                             _SetThisFormInfo(ow)
                                             If AlertYesNo(aLng[6]+";", aLng[5],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                oBrw:aArray := { aRec }
                                                ob:Reset()
                                                ob:nCell := 4
                                                ob:Enabled(.T.)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             Endif
                                             _SetThisFormInfo()
                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         // �������� ������� � �������, ��. ����
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_INSERT   , VK_DELETE
         //             _wPost(  32  ,  33  , 34   ,  10         ,  13

         :Event({60,"_2Itogo"}, {|ow| Itog_Table(ow), ow:Cargo:oBrw:SetFocus(), DoEvents() } )  // ����� � ������� �������

         :Event({80,"_2Save"}, {|ow,ky,cn| // ��������� �������
                                           Local ob := ow:Cargo:oBrw
                                           _SetThisFormInfo(ow)
                                           aRet := myTsbCloseTvr(ob)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.
                                           ob:SetFocus()
                                           DO EVENTS
                                           ky := cn
                                           _wSend("_2Releas",ow:Name)
                                           Return Nil
                                           })

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_2Cancel"}, {|ow| aRet := {}, ow:Release() } )
         :Event({99,"_2Releas"}, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)
      ON KEY F7     ACTION {|ob|// �������� �������
                                ob := ThisWindow.Cargo:oBrw
                                ? ":aArray", ob:aArray, "[1]", ob:aArray[1]
                                ?v ob:aArray[1]
                                ?
                                ?v ob:aDefValue
                                MsgDebug(":aArray=",ob:aArray,"[1]=",;
                                          ob:aArray[1],":aDefValue=", ob:aDefValue)
                                Return Nil
                                }
   END WINDOW

   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)             // ����������� / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ����������� ��� ���� MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // ������ ������, ���� ����� - ������ ����� �� �����

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION GetLineDim(cVal)
   LOCAL aTmp, a4Tmp, aDim, aVal, nSum, nCode, cName, nI, nCod1, nCod2

   aDim := {}
   // cVal = "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;"
   aTmp := HB_ATokens( cVal, ";", .T., .T. )
   FOR nI := 1 TO LEN(aTmp)
      IF LEN(aTmp) > 1
         a4Tmp := HB_ATokens( aTmp[nI], ",", .T., .T. )
         IF LEN(a4Tmp) == 4
            aVal := {}
            nCod1 := VAL(a4Tmp[1])
            cName := Alltrim( SAY_SEL_DIM( nCod1,'OB1ZAIV','OB1ZAIV'  ) )
            AADD( aVal, cName )
            nCod2 := VAL(a4Tmp[2])
            cName := Alltrim( SAY_SEL_DIM( nCod2,'OB2WORK','OB2WORK'  ) )
            AADD( aVal, cName )
            nCode := VAL(a4Tmp[3])
            cName := Alltrim( SAY_SEL_DIM( nCode,'Ob4orud','Ob4orud'  ) )
            AADD( aVal, cName )
            AADD( aVal, VAL(a4Tmp[4]) ) // ���-��
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','Cena_All'  )
            AADD( aVal, nSum )       // "Cena_All" , "N",10, 2, "���� ����� ($/���.)
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','CenaObor'  )
            AADD( aVal, nSum )       // "CenaObor" , "N",10, 2, "���� ������.,������. ($/���.)
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','CenaMast'  )
            AADD( aVal, nSum )       // "CenaMast" , "N",10, 2, "���� ������� ($/���.)
            AADD( aVal, nCode )      // 8  - ��� Ob4orud
            AADD( aVal, nCod1 )      // 9  - ��� KOB1ZAIV
            AADD( aVal, nCod2 )      // 10 - ��� KOB2WORK
            // �����
            AADD( aDim,  aVal )
         ENDIF
      ENDIF
   NEXT

RETURN aDim

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCloseTvr( oBrw )
   LOCAL aRet, cAls, nCol, cNam, xVal, nI, aVal, aDim, cTemp, cCode, aLng
   LOCAL nSum_All, nSumObor, nSumMast

   aDim  := {}
   cAls  := oBrw:cAlias
   oBrw:GoTop()

   FOR nI := 1 TO oBrw:nLen
       aVal   := {}
       oBrw:GoPos(nI,)     // ����������� ������ �� nI-������, nCol-�������
       FOR nCol := 1 TO oBrw:nColCount()
           cNam := oBrw:aColumns[ nCol ]:cName
           IF     cNam == "ORDKEYNO"  ; LOOP
           ELSEIF cNam == "SELECTOR"  ; LOOP
           ELSE
              xVal := oBrw:GetValue(cNam)
              IF IsString(xVal)
                 xVal := ALLTRIM(xVal)
              ENDIF
              AADD( aVal, xVal )
           ENDIF
       NEXT
       AADD( aDim, aVal )
   NEXT
   // ---------- ����� � ��� -----------
   // nAtPos := nI
   // aVal   := {}
   // FOR nCol := 1 TO oBrw:nColCount()
   //    xVal := oBrw:aArray[nAtPos][ nCol ]
   //    AADD( aVal , xVal )
   // NEXT
   // AADD( aRecno , aVal )

#ifdef KEY_ENG // for this project demo1-en.hbp
   aLng := { " pcs.", ", price ", "cost " }
#else
   aLng := { " ��.", ", ���� ", "���������� " }
#endif

   aRet  := {}
   cTemp := cCode := ""
   nSum_All := nSumObor := nSumMast := 0
   //      1    2       3             4         5       6      7      8      9  10  11
   // 1  {"1", "�", "����.����.", "K-DOM 750", 2.00, 1650.00, 0.00, 230.00, 277, 2, 35}
   // 2  {"2", "�", "�����", "�/� DP-20H", 1.00, 700.00, 0.00, 70.00, 244, 2, 31}
   // 3  {"3", "�/�", "������/���", "��� ����� DP 400-TD", 1.00, 0.00, 0.00, 0.00, 141, 1, 2}
   FOR nI := 1 TO LEN(aDim)
      aVal := aDim[nI]
      ? nI, HB_ValToExp(aVal)
      IF aVal[5] > 0 // Kolvo
         cCode += HB_NtoS(aVal[10]) + "," + HB_NtoS(aVal[11])+","
         cCode += HB_NtoS(aVal[9]) + "," + HB_NtoS(aVal[5]) + ";"
         //
         cTemp += aVal[2] + "," + aVal[3] + "," + aVal[4] + ","
         cTemp += HB_NtoS(INT(aVal[5])) + aLng[1]
         IF aVal[6] > 0  // Cena_All
            cTemp += aLng[2] + ALLTRIM(TRANSFORM(aVal[6],"999 999.99" )) + " "
            IF aVal[5] > 1
               cTemp += aLng[3] + ALLTRIM(TRANSFORM(aVal[6]*aVal[5],"999 999.99" )) + " "
            ENDIF
            cTemp += "; "
         ELSE
            cTemp += "; "
         ENDIF
         nSum_All += aVal[5] * aVal[6]
         nSumObor += aVal[5] * aVal[7]
         nSumMast += aVal[5] * aVal[8]
      ENDIF
   NEXT
   //      ����-���� "MKob4or"     ������ ����� � �������� ��� ������
   aRet := { cTemp  , cCode    , { {"SumVsego","SumWObor","SumMaster"} , {nSum_All,nSumObor,nSumMast} } }

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamTvr(cForm,aXDim,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr, nHCell
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35       // ������ �����  / cell height
   oTsb:nHeightCell    := nHCell                                  // the supplement depends on the screen size
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // ������� ��������� � � ������ / numbering column and its width
   oTsb:nHeightHead    := nHCell * 1.6                            // ������ ����� - ������ ����� �������
   oTsb:nHeightFoot    := nHCell * 1.6                            // ������ �������
   oTsb:lFooting       := .T.                                     // ������� � ������� ������
   oTsb:lSpecHd        := .T.                                     // ��������� � ������� ��������� �������
   oTsb:lSuperHd       := .T.                                     // ��������� � ������� ����������
   oTsb:cSuperHd       := cTitle                                  // ����� �����������
   oTsb:nHeightSuperHd := nHCell * 1.6                            // ������ �����������
   oTsb:nCellMarginLR  := 0                                       // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
   //oTsb:uSelector    := 20                                      // �������� ����� �������
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "F_GRP", "F_WORK", "F_NAME", "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER", "F_CODE","F_KGRP", "F_KWORK" }
   oTsb:aItogo         := { "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER" }  // ���� ��� ������������ ����� (������ �������)
   ? ProcNL(), "######## nHCell=", nHCell

#ifdef KEY_ENG // for this project demo1-en.hbp
   //                    1                  2                       3                       4             5                 6                7                8        9  10
   oTsb:aHead := { "Group;requests" , "Group;works", "Name of equipment/works"        , "Quantity", "TOTAL;price", "EQUIPMENT;price" , "MASTER;price" , "Codes;equip.","","" }
#else
   oTsb:aHead := { "������;������" , "������;�����", "������������ ������������/�����", "����������", "�����;����", "��������.;����" , "�������;����" , "����;����."  ,"","" }
#endif

   oTsb:aHideCol := {10,11}   // ������ �������, ��������� SELECTOR � ARRAYNO
   aWSize        := CalculatColumnWidthsTvr(aXDim,3,nWTsb)   // ������� ������ ������� - ������� �� 2 �������
   oTsb:aSize    := aWSize                                   // �������� ������ ������� ��� ���

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
   oTsb:aZebra    := { aBClr, {206,211,242} }
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
   //  ?v aXDim
   //  ?v oTsb:aName
   //  ?v oTsb:aHead
   //  ?v oTsb:aSize
   //
   // ����� ������� ������ ������ ����
   oTsb:bInit := {|ob,op| // ��������� ���
                   //ob:Hide()                                    // ������ ������� ��� ���������� ����������
                   ob:HideColumns( op:aHideCol ,.t.)              // ������ �������
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("F_WORK")         // ���������� �������
                   ob:lLockFreeze := .T.                          // �������� ���������� ������� �� ������������ ��������
                   ob:nCell       := ob:nFreeze + 1               // ����������� ������
                   ob:lNoKeyChar  := .F.                          // ���� � ������ �� ����, ����
                   myTsbEditTvr(ob,op)                            // �������������� ����� �������
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // ������ ��������� ���
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // ���� ���������
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - ��� ���
                   ob:lPickerMode := .F.
                   //oc := ob:aColumns[2]
                   //oc:lEdit     := .T.
                   //oc:cPicture  := Nil
                   //oc:lCheckBox := .T.
                   //oc:nAlign    := DT_LEFT
                   //oc:nEditMove := 0    // ���������� ������
                   //IF ob:nHeightCell > 40
                   //   oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   //ELSE
                   //   oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   //ENDIF
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
                    Local oc, nw := 0, nn
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
                    NEXT
                    IF !Empty(nn)
                       //oc := ATail(ob:aColumns)
                       oc := ob:GetColumn("F_NAME")
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
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_INSERT, {|ob| _wPost(10, ob:cParentWnd, ob) } }, ;
        {VK_DELETE, {|ob| _wPost(13, ob:cParentWnd, ob) } }  ;
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
STATIC FUNCTION CalculatColumnWidthsTvr(aXDim,nCol,nWTsb)
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
STATIC FUNCTION myTsbEditTvr( oBrw )
   LOCAL oCol, cCol, nI

   oBrw:SetAppendMode( .F. )     // ��������� ������� ������ � ����� ���� �������� ����
   oBrw:SetDeleteMode( .T., .F. )
   //oBrw:SetDeleteMode( .T., .T. ) - ������ �� ��������

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // oCol:lEdit := .T. // ������ ���� oc:lEdit  := .T.
      // oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ����� �� �����
      // oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> ����� �� �����
      IF "F_NAME" $ cCol .OR. "F_KOLVO" $ cCol
         oCol:lEdit     := .T.   // ������� ����
         //oCol:nEditMove := 0     // ����. ����������� ������� ����� :Edit()
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }
      ENDIF
      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� ������������� - ��� ������� �� ��������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, nCode
   LOCAL cTyp, cMsg, lWrt, cStr, aVal, aRet, nAt, cName
   LOCAL aLng := ob:Cargo:aLang

   WITH OBJECT ob
      aVal    := :aArray[:nAt]         // ��� ������ �������
      nCol    := :nCell
      oCol    := :aColumns[ nCol ]
      nAt     := nAt                   // ����� � �������
      oCol:Cargo := oHmgData()         // �������� ��������� �� �������
      cAls    := :cAlias
      cTyp    := oCol:cFieldTyp        // ��� ��������� �������
      cNam    := oCol:cName
      Default cJTyp := "+"
      IF cNam == "F_NAME"              // ������ ��� ����
         cJTyp := "CALC"
      ELSEIF cNam == "F_KOLVO"
         cJTyp := "N"
      ENDIF
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   //cStr += 'Column array type: "' + cTyp + '" ;'
   cStr += 'Column array cJTyp: "' + cJTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .F.                    // �� ���������� ����
   lRet := .F.                    // �� ������ ������������� ���� � :get

   IF LEN(cJTyp) == 0             // ��� ���������
   ELSEIF cJTyp == "0"            // ��� ���������
   ELSEIF cJTyp $ "NLCD"
      lRet := .T.                 // ������������� ���� � :get
      lWrt := .T.                 // ���������� � ������
   ELSEIF cJTyp == "M"
      aRet := CellEditMemo(uVal, ob)   // -->> tsb_memo_cell.prg
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                // ���������� � ������
      ENDIF

   ELSEIF cJTyp $ "CALC"

      SET WINDOW THIS TO ob:cParentWnd
      aRet := Tsb_Tovar_Outfit(ob)        // -> tsb_tovar_outfit.prg
      IF LEN(aRet) > 0
         nCode := aRet[6]
         cName := Alltrim( SAY_SEL_DIM( nCode,'OB1ZAIV','OB1ZAIV'  ) )
         ob:SetValue("F_GRP"   , cName )
         nCode := aRet[7]
         cName := Alltrim( SAY_SEL_DIM( nCode,'OB2WORK','OB2WORK'  ) )
         ob:SetValue("F_WORK"  , cName   )
         ob:SetValue("F_NAME"  , aRet[1] )   // ��������� ��-3099
         ob:SetValue("F_CODE"  , aRet[5] )   // ���: ��������� ��-3099
         ob:SetValue("F_KOLVO" , 0       )
         ob:SetValue("F_TOTAL" , aRet[2] )
         ob:SetValue("F_MASTER", aRet[3] )
         ob:SetValue("F_DEVICE", aRet[4] )
         ob:SetValue("F_KGRP"  , aRet[6] )
         ob:SetValue("F_KWORK" , aRet[7] )
         //ob:Reset() - �� ����
         ob:DrawLine()                // ������������ ������� ������ �������
         lWrt := .T.                  // ���������� � ������
      ENDIF

      SET WINDOW THIS TO

      ? "###### " + ProcNL(), cJTyp, "aRet=", aRet, hb_valtoexp(aRet)

   ELSE
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                           // ���������� ������
      //ob:Cargo:nModify ++          // �������-��������� � �������
      //ob:SetValue(nCol,uVal)       // <== ������ ��� lRet == .F., � :bPostEdit ��� ����� ���, ��� ������ �������
   ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   //    ob:Cargo:nModify ++          // �������-��������� � �������
   //    ��� �������� � :bPostEdit ��� lRet == .T., ��������� �������� � �������
   //    IF oCol:xOldEditValue != uVal  <== :bPostEdit !!!
   //       ob:Cargo:nModify ++
   //    ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   // ��� ���� ������ ����� ������ !!! ����� ��� ���������� � ���� ��� ���� (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   IF lWrt .AND. !lRet
      ob:Cargo:nModify ++          // �������-��������� � �������
      //ob:SetValue(nCol,uVal)     // <== ������ ��� lRet == .F., � :bPostEdit ��� ����� ���, ��� ������ �������
      ob:DrawSelect()              // ������������ ������� ������ �������
      ob:SetFocus()
   ENDIF

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
// ����������� ����� ����� � get
STATIC FUNCTION myTsbEditPost( uVal, ob )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod, cJTyp
   LOCAL oWnd  := _WindowObj(ob:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT ob
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp               // ��� ��������� �������
      uOld := oCol:xOldEditValue           // old value
      lMod := ! uVal == uOld               // .T. - modify value
      cAls := :cAlias
      cJTyp := VALTYPE(uVal)              // ��� ��������� ������
   END WITH

   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   //cStr += ';Column processing type: "' + cTyp + '" ;'
   cStr += ';Column array cJTyp: "' + cJTyp + '" ;'

   IF cJTyp $ "CNDLM"
      // ����������� ���������
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF
   //    ��� �������� � :bPostEdit ��� lRet == .T., ��������� �������� � �������
   IF oCol:xOldEditValue != uVal  // <== :bPostEdit !!!
      ob:Cargo:nModify ++         // �������-��������� � �������
   ENDIF

   ? "###### " + ProcNL(), cJTyp, "[2]=", HB_ValToExp( ob:aArray[ob:nAt][ACOL_2] )
   ob:DrawSelect()    // ������������ ������� ������ �������
   ob:SetFocus()

   IF "F_KOLVO" $ cNam
      _wSend("_2Itogo",ob:cParentWnd)  // ����� � ������� �������
   ENDIF

   DO EVENTS

RETURN .T.

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Itog_Table( ow )
   //oTsb:aItogo := { "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER" }
   Local aCol := ow:Cargo:aItog
   Local oBrw := ow:Cargo:oBrw
   Local aLine, nCol, oCol, aItg, cPic, cNam
   //     1       2               3       4      5       6      7      8
   //1  {"�", "����.����.", "K-DOM 750", 2.00, 1650.00, 0.00, 230.00, 277}
   //2  {"�", "�����", "�/� DP-20H", 1.00, 700.00, 0.00, 70.00, 244}
   //3  {"�/�", "������/���", "��� ����� DP 400-TD", 1.00, 0.00, 0.00, 0.00, 141}
   aItg := ARRAY(Len(aCol))
   AFILL(aItg,0)
   FOR EACH aLine IN oBrw:aArray
      nCol := hb_EnumIndex(aLine)
      IF IsNumeric(aLine[4]) .AND. IsNumeric(aLine[5]) .AND. ;
         IsNumeric(aLine[6]) .AND. IsNumeric(aLine[7])
         aItg[1] += aLine[4]
         aItg[2] += aLine[4] * aLine[5]
         aItg[3] += aLine[4] * aLine[6]
         aItg[4] += aLine[4] * aLine[7]
      ENDIF
   NEXT
   FOR EACH cNam IN aCol
      nCol := hb_EnumIndex(cNam)
      oCol := oBrw:GetColumn(aCol[nCol])
      cPic := IIF( nCol == 1, "@Z 999 999", "@Z 999 999.99" )
      oCol:cFooting := ALLTRIM(TRANSFORM(aItg[nCol],cPic))  // ����� � ������ �������
   NEXT
   oBrw:DrawFooters()
   DO EVENTS

RETURN Nil

