/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
///////////////////////////////////////////////////////////////////
STATIC FUNCTION Menu_Tab(aBColor)
   LOCAL hFont, aFont, oMenu := oHmgData()
   DEFAULT aBColor := GRAY

   hFont := GetFontHandle('ItalBold')
   //aFont := GetFontParam(hFont)
   aFont := { App.Cargo:cFontName2 , App.Cargo:nBtnFontSize, .T. } // "Comic Sans MS" nSize + 4
   // ��� ������� + ��� �������
   oMenu:aObj  := { "_3Tab1"  , "_3Tab2" , "_3Tab3" , "_3Tab4" , "_3Tab5" , "_3Tab6"  }
   oMenu:aImg  := { {NIL,NIL} , {NIL,NIL}, {NIL,NIL}, {NIL,NIL}, {NIL,NIL}, {NIL,NIL} }
                //      1                 2                     3              4                 5            6
   oMenu:aMnRu  := { "Tab-1"        , "Tab-2"        , "Tab-3"        , "Tab-4"        , "Tab-5"        , "Tab-6"         }
   oMenu:aHelp  := { "Data on tab-1", "Data on tab-2", "Data on tab-3", "Data on tab-4", "Data on tab-5", "Data on tab-6" }
   oMenu:a1BClr := { {159,191,236}  , {251, 230, 148}, {255, 178, 178}, {214,166,242}   , {195,224,133} , {249,193,71}    }
   //oMenu:aMnEn := { "Help"  , {"Search;user"    ,"Clear;search"  }, {"Print" ,"Notepad" }, "Exit", "Exit", "Exit" }
   oMenu:aCapt    := oMenu:aMnRu //IIF( App.Cargo:cLang == "RU", oMenu:aMnRu, oMenu:aMnEn )
   oMenu:nHIco    := 30    //IIF( App.Cargo:aDisplayMode[2] <= 900, 48, 64 )  // ������-������ ������ �� ������
   oMenu:nHG2     := 5     // ���������� ������ � ������ ������
   oMenu:aBtnFClr := { BLACK, RED, BLACK, YELLOW     }           // ���� ����� ������ + 2/3/4-���� ���������
   //oMenu:aBtnBClr := { {66,92,251} , WHITE, YELLOW, GRAY }     // ���� ���� ������ + 2/3/4-���� ���������
   oMenu:aBtnBClr := { aBColor , WHITE, YELLOW, GRAY }            // ���� ���� ������ + ���� ���������
   oMenu:aBtnFont := { aFont[1], aFont[2], aFont[3]  }            // ���� �� �������
   //oMenu:aBtnFont  := { "Tahoma", aFont[2]+2, .T.  }  // ���� �� ������� - ����� � ��� ������
   oMenu:nX        := 0
   oMenu:nY        := 0
   oMenu:lAutoSize := .T.        // T - �������������� ������ ������ � ������ ������ �� ������ ������
   oMenu:nWBtn     := 0          // ������ ������� ������ ������
   oMenu:nHBtn     := 0          // ������ ������� ������ ������
   oMenu:lVert     := .F.        // �� ������������ ����� �� ������
   oMenu:nClientW  := 0          // ������ ����
   //oMenu:lAutoSize := .F.      // F - ������ �������
   //oMenu:nWBtn   := 120        // ������ ������� ������ ������
   //oMenu:nHBtn   := 100        // ������ ������� ������ ������
   oMenu:lGradient := .T.        // �������� �� ������� - ��������
   oMenu:lGradient := .F.        // ��� ��������� �� �������

RETURN oMenu

//////////////////////////////////////////////////////////////////////////
FUNCTION Card_6Tsb(oWnd,aWinBClr,aAdr,cTtl,lZaEdit)
   LOCAL nH, nW, nG, nX, nY, oMenu, lNoSave, cVal, cObj, nHBtn, aBClr
   LOCAL cForm, owc, nCapt, cPost, aTbl, nI, aObj, aRet, aXdim, aSupHd
   LOCAL cTitle, aName, aHead, nYTsb, nXTsb, nWTsb, nHTsb, oTsb, oXBrw
   LOCAL cIni, cAls, aUser, aVal, nJ, aDim
   DEFAULT lZaEdit := .T.

   ? "-->>", ProcNL(), oWnd, aWinBClr, aAdr
   ? ProcNL(), Za_VipZa()
   //? "    >" ; ?v aAdr
   cForm  := oWnd:Name
   owc    := oWnd:Cargo
   cAls   := owc:cAls
   aTbl   := owc:aTbl           // ���������� �������
   nG     := owc:nG
   nY     := aTbl[1]
   nX     := aTbl[2]
   nW     := aTbl[3]
   nH     := aTbl[4]
   /////////////////////// ������ ��� ������� �������� /////////////////////////////////////
   oMenu          := Menu_Tab(owc:aBClr)      // menu-tab options
   oMenu:nY       := nY
   oMenu:nX       := nX                       // ���������� ������ ������
   oMenu:nHBtn    := 40                       // ������ ������� ������ ������
   oMenu:nG       := 1                        // ������ �������
   oMenu:nClientW := nW                       // ������ ����
   lNoSave        := .F.                      // ��� ������ ������� ������ �� �����,
                                              // ����� ����� �������������� ����� ����������� �������
   cPost          := "_3Tab0"                 // ���� ������� �� ���� �������
   aObj           := oMenu:aObj               // { "_3Tab1", "_3Tab2", "_3Tab3", "_3Tab4", "_3Tab5", "_3Tab6" }

   cObj := aObj[1]
   IF _IsControlDefined(cObj, cForm)
      FOR nI := 1 TO LEN(aObj)
         DoMethod(cForm, aObj[nI], "Show")
         ? "    >", nI, cForm, aObj[nI], "Show"
      NEXT
   ELSE
      FOR nI := 1 TO LEN(aObj)
         AADD( owc:aObjHide, aObj[nI] )   // ��� �������� �����
      NEXT
      // ����� ����
      MenuTopIconButtons( owc, oMenu,, lNoSave, oMenu:nG, cPost )  // -> menu_topButton.prg (owc:nG)
      DO EVENTS
   ENDIF

   // ������� �������
   nHBtn   := owc:nHBtn2
   nCapt   := LEN(oMenu:aMnRu)
   aSupHd  := oMenu:aHelp
   // ������� �������� ������ �����
   owc:aTsbDel := {}       // ������� ������
   owc:aTsbIni := {}       // ��������� ���������� ����� � �������
   owc:aWrtIni := {}       // ��������� ���������� ����� � ������� ��� ������
   owc:aBrw    := {}
   owc:nBrw    := 0        // ��� ��� � ������ (������ �� ������)

   FOR nI := 1 TO nCapt

      cObj   := "Label_Table" + HB_NtoS(nI)
      cVal   := CRLF + "����� ������� - " + oMenu:aMnRu[nI] + CRLF
      cVal   += "����� ������� = " + HB_NtoS(nI)
      aBClr  := oMenu:a1BClr[nI]
      cTitle := cTtl + aSupHd[nI]

      //@ aTbl[1] + nHBtn, aTbl[2] LABEL &cObj PARENT &cForm WIDTH aTbl[3] HEIGHT aTbl[4] - nHBtn ;
      //  VALUE cVal SIZE owc:nFSize*2 BACKCOLOR aBClr CENTERALIGN //VCENTERALIGN

      cObj  := "cTable_" + HB_NtoS(nI)
      cIni  := App.Cargo:cPathTemp + "6" + cObj + ".ini"  // ��������� ���������� ����� � �������
      aRet  := LoadDimZaivCard(nI)                // ������� �������   -> card_array_zaiv.prg
      aXdim := aRet[1]                            // ������ �������
      aName := aRet[2]                            // ����� ����� �������
      aHead := aRet[3]                            // ����� �������
      aXdim := ArrayDbfLoad(aXdim,cAls,cIni,owc:aWrtIni,nI)  // ��������� ������ � ������� �� ����
      /////////////////////// ������� //////////////////////////////////////////////////////////////
      nYTsb := aTbl[1] + nHBtn
      nXTsb := aTbl[2]
      nWTsb := aTbl[3]
      nHTsb := aTbl[4] - nHBtn
      oTsb  := TableParam2( cForm, aXdim, cObj, aName, aHead, nWTsb, aBClr, cTitle)
      oTsb:lZaEdit := lZaEdit                      // ����� !!!
      // ? _o2log(oTsb, 27, ProcNL() + "  oTsb => ", .T. ) // ��������
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oXBrw := _TBrowse( oTsb, aXdim, cObj, nYTsb, nXTsb, nWTsb, nHTsb )
      oXBrw:Cargo:nBrw     := nI
      oXBrw:Cargo:nCellFoc := 3                            // �������� ������
      oXBrw:Cargo:nModify  := 0                            // ������� ���������
      oXBrw:Cargo:lChkTsb  := .F.                          // ������ �������� ����� �������
      oXBrw:Cargo:dZDate   := (cAls)->DateZa               // ���� ������ ! ����� ! ������������ ����� � ����
      oXBrw:Cargo:aIsxTbl  := oXBrw:aArray                 // �������� ������ ����� !!!
      //
      AADD( owc:aBrw   , oXBrw )  // ������ �������� �������
      AADD( owc:aTsbDel, cObj  )  // ��� �������� 5-������ - �������������� �������
      AADD( owc:aTsbIni, cIni  )  // ��������� ���������� ����� � �������

      // �������� ���������� �����
      //oXBrw:Hide()
      aUser := {}
      aDim  := oXBrw:aArray
      aDim := ASORT( aDim,,, { |x, y| x[ADIM_SORT] < y[ADIM_SORT] } )
      FOR nJ := 1 TO LEN(aDim)
         aVal := aDim[nJ]
         IF aVal[ADIM_SORT] # 0
            AADD( aUser, aVal )
         ENDIF
      NEXT
      If Len(aUser) == 0
         aVal    := ACLONE(aDim[1])
         aVal[1] := "��� ����� ������� ��� ������ !"
         aVal[2] := "��������� ���������� ������ ����� (3) ������� !"
         AADD( aUser, aVal )
         AADD( aUser, aVal )
      Endif
      // ����� ��������� ������ ��� �� ����� !!!
      oXBrw:aArray := aUser
      oXBrw:nLen   := LEN(aUser)
      oXBrw:Reset()
      oXBrw:Refresh()
      //oXBrw:Show()
      oXBrw:Setfocus()

      //IF nI > 1
      //   DoMethod(cForm, cObj, "Hide")
      //   ? nI, cObj, "Hide"
      //ENDIF
      DO EVENTS
   NEXT
   This.Cargo:nBrw := 1  // (���. ��� � ������)

   oXBrw := owc:aBrw[1]
   oXBrw:Show()

   CalcMemoLine(oXBrw)  // ������� �������� ����� ������������ / the sizes change after reboot
                        // for function MEMOLINE(xxxx,App.Cargo:nMemoChar,1)

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TableParam2(cForm,aXDim,cBrw,aName,aHead,nWTsb,aBClr,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nI, aWSize, cT, nHCell

   cT := space(3) + Alltrim(cTitle) + space(3)
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // ������� ��������� � � ������
   oTsb:nHeightCell    := nHCell                   // ������ �����
   oTsb:lDrawHeaders   := .F.                      // !!! ��� ������ ���������
   oTsb:nHeightHead    := 1                        // ������ ����� - ������ ����� �������
   oTsb:nHeightFoot    := nHCell                   // ������ �������
   oTsb:lFooting       := .T.                      // ������� � ������� ������
   oTsb:lSpecHd        := .F.                      // �� ��������� � ������� ��������� �������
   oTsb:lSuperHd       := .T.                      // ��������� � ������� ����������
   oTsb:cSuperHd       := cT                       // ����� �����������
   oTsb:nHeightSuperHd := nHCell - 6               // ������ �����������
   oTsb:nCellMarginLR  := 0                        // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
   //oTsb:uSelector    := 20                       // �������� ����� �������
   oTsb:lNoPicture     := .T.
   ? ProcNL(), "######## nHCell=", nHCell
   oTsb:aName := aName
   oTsb:aHead := aHead
   // ����� ������ ������ ����� � ����� �������
   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead  := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF
   //              1  2  3  4  5  6  7  8  9 10 11 12 13
   oTsb:aHead    := {"","","","","","","","","","","",""} // ������� ����� ������� - ���������� 11 ������� �������
   oTsb:aHideCol := { 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}   // ������ �������, ��������� SELECTOR � ARRAYNO
   aWSize        := CalculatColumnWidths(aXDim,2,nWTsb)   // ������� ������ ������� - ����� ������ 2 �������, ��������� �� �����
   oTsb:aSize    := aWSize                                // �������� ������ ������� ��� ���

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

   nClr1               := HMG_RGB2n(aBClr)                  // ���� ���� �����+������
   nClr2               := RGB( 48, 29,26)                   // ����-������ ���
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // ����: ����� � ��� �����������
   //oTsb:aBrush       := {240,240,240}                     // ���� ���� ��� ��������
   oTsb:aBrush         := aBClr                             // ���� ���� ��� ��������

   // ����� � �������
   oTsb:lZebra    := .F.                                    // ��� ���.\����. �������� zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // ����� ����
   oTsb:aZebra    := { {251,227,227}, {207,152,149} }
   oTsb:aBZebra   := {RGB(251,227,227), nClr1 /*RGB(255,178,178)*/ ,;
                      CLR_HRED, CLR_MAGENTA, CLR_YELLOW }   // ��� ����� ������ ������
   oTsb:nBZebraNo := 3
   oTsb:aFZebra   := {CLR_BLACK, CLR_HBLUE, CLR_BLUE, CLR_RED, CLR_MAGENTA}  // ��� ������ �����
   oTsb:nBZebraNo := 4

   a := {}
   //AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , ������ �����
   AAdd(a, { CLR_TEXT, {|nr,nc,ob| // ������ ���� �� ���� ����� - �������-3
                        Local nClr := ob:nClrPane, cTyp, aClr, oTsb, oCol, nCol
                        IF !IsArray(ob:aArray) ; Return nClr
                        ENDIF
                        //? ProcNL(), nr,nc,ob, "ob:nLen=", ob:nLen, ob:aColumns
                        nc   := IIF( nc > LEN(ob:aColumns), LEN(ob:aColumns), nc )
                        //?? nc
                        oCol := ob:aColumns[nc]
                        oTsb := ob:Cargo:oParam
                        nCol := ob:nColumn("F_PROCES")
                        nCol += IIF( ob:nColumn("ARRAYNO", .T.) > 0, -1, 0)
                        nCol += IIF( ob:nColumn("SELECTOR", .T.) > 0, -1, 0)
                        aClr := oTsb:aFZebra         // ����� ������
                        nClr := aClr[1]
                        cTyp := ob:aArray[nr][nCol]
                        IF cTyp $ "LNCDT"           ; nClr := aClr[1]
                        ELSEIF cTyp == "SPR_A"      ; nClr := aClr[2]
                        ELSEIF cTyp == "SPR_S"      ; nClr := aClr[3]
                        ELSEIF cTyp == "M"          ; nClr := CLR_MAGENTA
                        ELSEIF cTyp == "CALC"       ; nClr := CLR_BLUE
                        ELSEIF cTyp == ""           ; nClr := CLR_HRED
                        ELSEIF "LINE" $ cTyp        ; nClr := CLR_WHITE
                        ELSE                        ; nClr := CLR_BLACK
                        ENDIF
                        Return nClr
                        }})

   // 2 , ���� � ������� �������
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   //AAdd(a, { CLR_PANE, {|nr,nc,ob| // ������ ���� �� ���� ����� - �������-2
   //                                Local nClr := ob:nClrPane, cVal
   //                                Local aClr := {RGB(251,227,227), ;
   //                                               RGB(207,152,149), ;
   //                                               CLR_HRED}
   //                                IF !IsArray(ob:aArray) ; Return nClr
   //                                ENDIF
   //                                nc := nr % 2
   //                                nClr := aClr[ nc + 1 ]
   //                                cVal := ob:aArray[nr][3]
   //                                IF cVal == "LINE1" ; nClr := aClr[3]
   //                                ENDIF
   //                                Return nClr
   //                                } } )

   AAdd(a, { CLR_PANE, {|nr,nc,ob| // ������ ���� �� ���� ����� - �������-3
                        Local nClr := ob:nClrPane, cVal, aClr, oTsb, oCol, nKey
                        IF !IsArray(ob:aArray) ; Return nClr
                        ENDIF
                        oCol := ob:aColumns[nc]
                        oTsb := ob:Cargo:oParam
                        nKey := ob:nColumn("F_PROCES")
                        nKey += IIF( ob:nColumn("ARRAYNO", .T.) > 0, -1, 0)
                        nKey += IIF( ob:nColumn("SELECTOR", .T.) > 0, -1, 0)
                        aClr := oTsb:aBZebra
                        nClr := aClr[ nr % 2 + 1 ]
                        cVal := ob:aArray[nr][nKey]
                        IF cVal == "LINE1"               ; nClr := aClr[3]
                        ELSEIF cVal == "LINE2"           ; nClr := aClr[4]
                        // ������� � ��. ���� ��� ��������� zebra
                        ELSEIF oCol:cName == "F_PROCES"  ; nClr := aClr[5]
                        ELSEIF oCol:cName == "F_BASE"    ; nClr := CLR_ORANGE
                        ELSEIF oCol:cName == "F_READ"    ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_WRITE"   ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_WINDOWS" ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_ACCESS"  ; nClr := RGB(92,242,212)
                        ELSEIF oCol:cName == "F_NN"      ; nClr := RGB(120,242,137)
                        ENDIF
                        Return nClr
                        }})

   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , ������ ����� �������
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , ���� ����� �������
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , ������ �������������� ����
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , ���� �������������� ����
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , ������ ������� �������
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, ���� ������� �������

   // --- �� ���� ���, ������� ����    oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }
   //AAdd(a, { CLR_SUPF , CLR_WHITE              })  // 16, ���� �����������
   //AAdd(a, { CLR_SUPB , CLR_RED                })  // 17, ������ �����������

   //AAdd(a, { CLR_SPCF , CLR_YELLOW             })  // 18, specheader text - ���������
   //AAdd(a, { CLR_SPCB , { nClr1, nClr2 }       })  // 19, specheader back - ���������
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
                   ob:Hide()                                      // ������ ������� ��� ���������� ����������
                   ob:HideColumns( op:aHideCol ,.t.)              // ������ �������
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO") + 1    // ���������� �������
                   ob:lLockFreeze := .T.                          // �������� ���������� ������� �� ������������ ��������
                   ob:nCell       := ob:nFreeze + 1               // ����������� ������
                   ob:lNoKeyChar  := .F.                          // ���� � ������ �� ����, ����
                   // --------- ��������� ��������, ��������� ����� �������� ������� ��������� ------
                   ob:aBitMaps    := { LoadImage("bGear24"), LoadImage("bEye24"), LoadImage("bGear20") ,;
                                       LoadImage("bGear16"), LoadImage("bEye16"), LoadImage("bAttach24") ,;
                                       LoadImage("bAttach24x2")  }
                   // �������������� ����� ������� -> ��. ����
                   //MsgDebug(op:lZaEdit, ob:Cargo:oParam:lZaEdit)
                   IF op:lZaEdit
                      myTsbEdit(ob,op)
                   ENDIF
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // ������ ��������� ���
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // ���� ���������
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - ��� ���
                   ob:lPickerMode := .F.
                   oc := ob:aColumns[3]
                   oc:lEdit     := op:lZaEdit
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nAlign    := DT_LEFT
                   oc:nEditMove := 0    // ���������� ������
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   ob:lNoHScroll  := .T.   // ��� ������ ��������������� ���������
                   ob:oHScroll    := NIL
                   IF ob:nHeightHead < 3 ; ob:GetColumn(1):cHeading := " "
                   ENDIF
                   IF ob:nHeightFoot < 3 ; ob:GetColumn(1):cFooting := " "
                   ENDIF
                   oc := ob:aColumns[2]
                   oc:cFooting     := "��������� ����� �������"
                   oc:uBmpFoot     := ob:aBitMaps[3]  // [3] �������� � ������� ������� �������
                   oc:nFAlign      := nMakeLong( DT_LEFT, DT_LEFT  )
                   oc:nBmpMaskFoot := 0x00BB0226    // MERGEPAINT
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(42, ob:cParentWnd, ob) }
                   oc := ob:aColumns[4]
                   oc:uBmpFoot     := ob:aBitMaps[2]  // [2] �������� � ������� ������� �������
                   oc:nFAlign      := nMakeLong( DT_CENTER, DT_CENTER )
                   oc:nBmpMaskFoot := 0x00BB0226  //0x00CC0020    // SRCCOPY
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(43, ob:cParentWnd, ob:aColumns[4]:cName) }
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // ��������� ����� ���� ����������� �������
                         oc:nClrFore    := CLR_RED          // ��������� ����� ������ ����������� �������
                         oc:hFont       := hFont            // ��������� ����� ����������� �������
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         //oc:nWidth    := GetTextWidth( Nil, "0000", hFont )   // ���-�� ������ - ����� �� ����� ��������
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   // ��������� �������� �� ������ �������
                   oc := ob:GetColumn("REDIT")
                   oc:uBmpCell := {|nc,ob|
                                     Local hBmp/*, oc := ob:aColumns[nc]*/, nn := 0
                                     Local aRec := ob:aArray[ ob:nAt ]
                                     Local cKey := aRec[5]
                                     //? ProcNL(), nc, HB_ValToExp(aRec), cKey
                                     IF IsString(cKey)
                                        cKey := upper(cKey)
                                        IF     cKey == upper("aDefect")  ; nn := 6  // "bAttach24" ��.����
                                        ELSEIF cKey == upper("MOb4orud") ; nn := 7  // "bAttach24x2" ��.����
                                        ENDIF
                                     ENDIF
                                     IF nn > 0 ; hBmp := ob:aBitMaps[ nn ]
                                     ENDIF
                                     nn := nc
                                     Return hBmp
                                     }
                   oc:nAlign := DT_LEFT
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
                          oc:nWidth := GetTextWidth( Nil, "0000", oc:hFont )
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
                    // ����� ���
                    //ob:UserKeys(VK_F2 ,  {|ob| myTsbListColumn( ob ), ob:Setfocus() })  // ���� �� ������ �������
                    //ob:UserKeys(VK_F3 ,  {|ob| myTsbListFont( ob )  , ob:Setfocus() })  // ���� �� ������ �������
                    //ob:UserKeys(VK_F4 ,  {|ob| myTsbArrayLine( ob ) , ob:Setfocus() })  // ���� �� ������ �������
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
FUNCTION CalculatColumnWidths(aXDim,nCol,nWTsb)
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
           IF i > 3
              nW := IIF( nW > 400, 400, nW )
           ENDIF
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT

   //oTsb:aNumber := { 1, 30 }                // ������� ��������� � � ������
   // ��� ������� 2 ������ ��� ������ ������ ������, ����� ������� 1
   aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ������ ������ ������ ������� � ����������� �� ������ ������
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb
   ?? "�������:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   //? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // ������� "V_CALC", "V_SPR", "V_AADD"
      IF "V_" $ cCol
         oCol:lEdit     := .T.
         oCol:nEditMove := 0  // ����. ����������� ������� ����� :Edit()
         oCol:bPrevEdit := {|xv,ob| // �������� � ����
                                   Local nc, oc, cVal
                                   nc   := ob:nCell
                                   oc   := ob:GetColumn( nc )
                                   cVal := ob:GetValue(nc)
                                   SET WINDOW THIS TO ob
                                   //AlertInfo(cVal)    // ����� � ���
                                   AlertInfo(xv)
                                   SET WINDOW THIS TO
                                   Return .F. // ������ ��������� � get-������
                            }
      ELSEIF cCol == "F_NN"
         oCol:lEdit     := .T.
         oCol:nEditMove := 0  // ����. ����������� ������� ����� :Edit()
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ��.����
      ELSE
         //oCol:lEdit := .T. // ������ ���� oc:lEdit  := .T.
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ��.����
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> ��.����
      ENDIF
      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� ������������� - ��� ������� �� ��������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, cFunc, cVal, nVal
   LOCAL cTyp, cMsg, lWrt, cStr, aVal, aDim14, aRet, xDop15, a2Dim, aCode
   LOCAL cAccess, aDim, aVal13, aText, cMemo, a3Dim, cErr, nAt, nI, cFld
   LOCAL lYes, cText, nJ, cCol5, aFld2, hWin

   WITH OBJECT ob
      aVal    := :aArray[:nAt]         // ��� ������ �������
      nCol    := :nCell
      oCol    := :aColumns[ nCol ]
      oCol:Cargo := oHmgData()         // �������� ��������� �� �������
      cAls    := :cAlias
      cTyp    := oCol:cFieldTyp        // ��� ��������� �������
      cNam    := oCol:cName
      cJTyp   := aVal[ACOL_4]          // ��� ��������� ������
      Default cJTyp := "+"
      cFunc   := aVal[ACOL_8]          // ������� ��������� ������� ��������
      aVal13  := aVal[ACOL_13]         // ����� ����� ������ �������� ��� ���������� ������
      aDim14  := aVal[ACOL_14]
      xDop15  := aVal[ACOL_15]
      cAccess := aVal[ACOL_9]
      IF cAccess == "R"
         cJTyp := "0"
      ENDIF
      IF cNam == "F_NN"  // ������ ��� ����
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

   // ������� ���� ����� ������ ��� ���� �������
   IF cNam == "REDIT" .AND. Valtype(uVal) == "N"
      ob:aEditCellAdjust[3] := 150 - oCol:nWidth
   ENDIF

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
   ELSEIF cJTyp $ "DMN"
      cTyp := "D"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                 // ���������� � ������
      ENDIF
   ELSEIF cJTyp $ "DT"
      cTyp := "T"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                 // ���������� � ������
      ENDIF

   ELSEIF cJTyp $ "A"                // ������ ����� ����
      aRet := CellEdit_A(ob,aVal13,aDim14,xDop15)
      IF LEN(aRet) > 0
         ob:aArray[ob:nAt][ACOL_13] := aRet[1]
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(aRet[1])  // �������������� � "C" ������� (13)
         uVal := aRet[2]
         lWrt := .T.                 // ���������� � ������
      ENDIF

   ELSEIF cJTyp $ "SPR_A"
      aRet := Tsb_ContexMenu(ob,aDim14,xDop15)  // ��.����
      IF LEN(aRet) > 0
         uVal := aRet[2]
         ob:aArray[ob:nAt][ACOL_2]  := aRet[2]
         ob:aArray[ob:nAt][ACOL_13] := aRet[1]
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(nVal)  // �������������� � "C" ������� (13)
         lWrt := .T.                 // ���������� � ������
      ENDIF

   ELSEIF cJTyp $ "SPR_S"
      IF UPPER(cFunc) == "SPR_2DBF()"
         a2Dim := Spr_2Dbf(aDim14)
         aDim  := a2Dim[1]             // ����� ���������� ������
         aCode := a2Dim[2]             // ���� ��������
      ENDIF
      aRet := Tsb_ContexMenu(ob,aDim,xDop15)  // tsb_EditWindows.prg
      IF LEN(aRet) > 0
         uVal := aRet[2]
         nVal := aRet[1]
         IF nVal == 0   ; nVal := 0           ; cVal := "..."
         ELSE           ; nVal := aCode[nVal] ; cVal := aRet[2]
         ENDIF
         ob:aArray[ob:nAt][ACOL_2]  := cVal
         ob:aArray[ob:nAt][ACOL_13] := nVal
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(nVal)  // �������������� � "C"
         lWrt := .T.                   // ���������� � ������
      ENDIF

   ELSEIF cJTyp $ "CALC"
      hWin := _WindowObj(ob:cParentWnd):Handle
      SET WINDOW THIS TO ob:cParentWnd
      //Darken2Open(hWin)              // ��������� ���� ���������
      ? ProcNL(), cJTyp, HB_ValToExp( ob:aArray[ob:nAt][13] )
      DO EVENTS
      IF UPPER(cFunc) = "MYWINCALC()"
         aRet := Tsb_myWinCalc(ob,aDim14,xDop15,aVal)       // tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal               // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)  // �������������� � "C" ������� (13)
            lWrt := .T.                                      // ���������� � ������
         ENDIF

      ELSEIF UPPER(cFunc) = "MYWINCALC2()"
         aRet := Tsb_myWinCalc2(ob,aDim14,xDop15,aVal)        // tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal                // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)   // �������������� � "C" ������� (13)
            lWrt := .T.                                       // ���������� � ������
         ENDIF

      ELSEIF UPPER(cFunc) = "MYWINCALC3()"
         aRet := Tsb_myWinCalc3(ob,aDim14,xDop15,aVal)        // tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal                // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)   // �������������� � "C" ������� (13)
            lWrt := .T.                                       // ���������� � ������
         ENDIF

      ELSEIF UPPER(cFunc) = UPPER("ZaListNeis()")             // -> tsb_EditWindows.prg
         aFld2 := aVal[ACOL_14]  // ���� � ������� ����� �������� �������� - ������ � ob:aArray[nJ][ACOL_2]
         IF !IsArray(aFld2)
            cErr := "������ ! ������� [ACOL_14] �� ������ !;;"
            cErr += ProcNL() + ";" + ProcNL(1)
            AlertStop(cErr,ProcNL(),,64,{RED})
         ENDIF
         ? "###### ", "aFld2=",aFld2, HB_ValToExp(aFld2)
         a2Dim := Tsb_ZaListNeis(ob,aDim14,xDop15,aVal)
         IF LEN(a2Dim) > 0
            aCode := a2Dim[1]
            aText := a2Dim[2]
            // ����������� � ��� �� Tsb_ZaListNeis()
            uVal += "."                                       // ����� ��� ���������� ����� ���
            ob:aArray[ob:nAt][ACOL_13] := aCode               // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aCode)  // �������������� � "C" ������� (13)
            ? "###### [ACOL_13] = ", HB_ValToExp(aCode)
            //MsgDebug("����� ������� ������ � 10 ������� ���� ��� ��� ����� �� �� ����� !",LEN(a2Dim),a2Dim)
            nAt  := ob:nAt    // ��������� ������ ����
            cErr := ""
            FOR nI := 1 TO 10
               cFld  := UPPER(aFld2[nI])
               ? "###### ", nI, cFld, "##"
               lYes  := .F.
               FOR nJ := 1 TO LEN(ob:aArray)
                  cCol5 := ob:aArray[nJ][ACOL_5]
                  //?? nJ, cCol5, VALTYPE(cCol5)
                  IF !IsString(cCol5)
                     cCol5 := "��� �� ���� !" + cValToChar(cCol5)
                  ENDIF
                  IF cFld == UPPER(cCol5)
                     cVal := HB_NtoS(aCode[nI])              // ��� �������� ����������� � �����
                     ?? nJ, cCol5, "OLD:", ob:aArray[nJ][ACOL_2], "NEW:", aText[nI]
                     ob:aArray[nJ][ACOL_2]  := aText[nI]
                     ob:aArray[nJ][ACOL_13] := cVal          // (13) - �������� ������������� ���� {} ��� ���� CALC
                     ob:aArray[nJ][ACOL_10] := cVal          // �������������� � "C" ������� (13)
                     lYes := .T.
                  ENDIF
               NEXT
               IF !lYes
                  cErr += "�� ������� ���� � ������� (5): " + cFld + ";"
               ENDIF
            NEXT
            IF LEN(cErr) > 0
               cErr += ";; �������� log-������, ������: " + ProcNL() + ";"
               AlertStop(cErr,ProcNL(),,64,{RED})
            ENDIF
            ob:nAt := nAt                               // ��������� ������ ����
            ob:Refresh()
            lWrt  := .T.                                // ���������� � ������
         ENDIF
         ? "###### " + ProcNL(), "-> � Tsb_ZaListNeis()", HB_ValToExp(aCode), aText

      ELSEIF UPPER(cFunc) = UPPER("Tovar_HMG()")            // -> tsb_Tovar.prg
         // ���� ��� C - ����� �������� ��������: "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;"
         aVal13 := aVal[ACOL_13]   // �����  ������ �������� ��� ���������� ������
         IF !IsString(aVal13)
            cErr := "������ ! ������� [ACOL_13] �� ������ !;"
            cErr += "[" + VALTYPE(aVal13) + "];;"
            ? ProcNL() , ATREPL( ";", cErr, CRLF )
            ?? VALTYPE(aVal13), aVal13
            ? HB_ValTOExp(aVal13)
            cErr += ProcNL() + ";" + ProcNL(1)
            AlertStop(cErr,ProcNL(),,64,{RED})
         ENDIF
         a3Dim := Tsb_Tovar_HMG(ob,aVal13,aVal)
         IF LEN(a3Dim) > 0
            cMemo := a3Dim[1]
            cText := a3Dim[2]   // ->  [ACOL_13]
            a2Dim := a3Dim[3]   // ->  { {"SumVsego","SumWObor","SumMaster"} , {nSum_All,nSumObor,nSumMast} } }
            uVal  := cMemo
            ob:aArray[ob:nAt][ACOL_2]  := cMemo
            ob:aArray[ob:nAt][ACOL_13] := cText              // (13) - �������� ������������� ���� {} ��� ���� CALC
            ob:aArray[ob:nAt][ACOL_14] := a2Dim              // ����� ������� (10)
            ob:aArray[ob:nAt][ACOL_15] := ""                 // ������
            ob:aArray[ob:nAt][ACOL_10] := cText              // �������������� � "C" ������� (13)
            ob:aArray[ob:nAt][ACOL_11] := HB_ValToExp(a2Dim) // �������������� � "C" ������� (14)
            ob:aArray[ob:nAt][ACOL_12] := ""                 // �������������� � "C" ������� (15)
            lWrt  := .T.                                     // ���������� � ������
         ENDIF
         ? "###### " + ProcNL(), "-> � Tsb_ZaListNeis()", HB_ValToExp(aCode), aText

      ELSE
         MsgDebug("ERROR! ��� ��������� ������� !",cFunc)
      ENDIF
      ? "###### " + ProcNL(), cJTyp,"[13]=" ,HB_ValToExp( ob:aArray[ob:nAt][ACOL_13] )
      //Darken2Close(hWin)        // ��������� ���� ������
      SET WINDOW THIS TO

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
      ob:SetValue(nCol,uVal)       // <== ������ ��� lRet == .F., � :bPostEdit ��� ����� ���, ��� ������ �������
      ob:DrawSelect()              // ������������ ������� ������ �������
      ob:SetFocus()
   ENDIF
   ? "###### " + ProcNL(), lWrt,lRet, cJTyp, "[2]=", HB_ValToExp( ob:aArray[ob:nAt][ACOL_2] )

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
      cJTyp := ob:aArray[ob:nAt][ACOL_4]   // ��� ��������� ������
      Default cJTyp := "+"
   END WITH

   IF Valtype(uVal) == "N"                           // !!!!!!!!!!!!!!!!!!!!!
      ob:aEditCellAdjust[3] := 0
   ENDIF

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

   DO EVENTS

RETURN .T.

