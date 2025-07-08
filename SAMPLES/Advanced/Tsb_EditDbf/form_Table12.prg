/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
//#include "metrocolor.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION FormTable12(nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, cSuperHd, oClr, oMenu, oColnm)
   LOCAL nH, nW, nG, nX, nY, cTitle, cFont, nFSize, aBackColor
   LOCAL aHide, aForm, cFormCurr, cFormMain, lVsbl, nI, hWnd
   LOCAL oTsb, oBrw, aRet, owc, cBrw

   ? "-->> #### " + ProcNL(), nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, oMenu
   cFont      := App.Cargo:cDefFontName
   nFSize     := App.Cargo:nDefFontSize
   cFormMain  := App.Cargo:cWinMain         // ��� ���� MAIN �����
   cFormCurr  := ThisWindow.Name            // ������� ����� - ������������ ����
   aBackColor := oClr:aBClr                 // ���� ���� ���� �����
   cTitle     := cTtl + '  (' + App.Cargo:cDisplayMode + ')'
   cTitle     += SPACE(5) + ProcFile()
   nW         := App.Cargo:aDisplayMode[1]  // System.ClientWidth
   nH         := App.Cargo:aDisplayMode[2]  // System.ClientHeight
   //nH       -= GetTaskBarHeight()         // ������ ������ ����� Desktop
   nG         := IIF(App.Cargo:aDisplayMode[1]<=1440, 10, 15)  // ������
   nY         := nX := 0
   nY         := _WindowCargo(cFormCurr):nHMain     // ������ ������������ �����

   // ������ ��� ���� ����� ��������, ��������� ������� ���� - ������
   aHide := {}
   aForm := HMG_GetForms()
   nI := lVsbl
   FOR nI := 1 TO Len(aForm)
      lVsbl := IsWindowVisible( GetFormHandle( aForm[nI] ) )
      hWnd  := GetFormHandle( aForm[nI] )
      //? nI, aForm[nI], hWnd, lVsbl, cFormMain
      IF aForm[nI] == cFormMain     ; LOOP
      ELSEIF aForm[nI] == cFormCurr ; LOOP
      ELSEIF !lVsbl                 ; LOOP
      ENDIF
      //DoMethod(aForm[nI], "Hide")
      //AADD( aHide, aForm[nI] )
   NEXT

   // �������� �� ��� ��������� ����
   ? ProcNL(), "["+cForm+"]", VALTYPE(cForm)
   If _IsWindowDefined( cForm )
      DO EVENTS
      If IsIconic( GetFormHandle(cForm) )
         _Restore( GetFormHandle(cForm) )
      Else
         DoMethod( cForm, "SetFocus" )
      EndIf
      aRet := { .T., "���� ���� " + cForm }
      ? "-->> Return", ProcNL(), "aRet=", HB_ValToExp(aRet)
      RETURN aRet
   EndIf

   aRet := { .F., "��� ���� " + cForm }

   DbSelectArea(cAls)
   // ��� �������� TBrowse ���� ������� ���������� ��� �������
   // TBrowse ��� ����������, ���� �� ������������ �������� ����� � ��������
   //OrdSetFocus("DOCDTV")   // ������ ����� ����� !!!

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW-nX HEIGHT nH-nY       ;
      TITLE cTitle ICON cIco                                    ;
      WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE             ;
      BACKCOLOR aBackColor  FONT cFont SIZE nFSize              ;
      ON GOTFOCUS  {|| App.Cargo:cFormGotFocus := This.Name }   ; // ������� ������ �� �����
      ON INIT     {|| _wPost(0)  }                              ; // ����������� ����� ������������� ����
      ON RELEASE  {|| _wSend(90) }                              ; // ����������� ����� ����������� ����
      //ON LOSTFOCUS {|| myLangRecoverLost(This.Cargo:aCurrLang) }     // ������ ������ � �����

      This.Cargo    := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor   := This.BackColor                  // ���� ����
      owc:hWin      := This.Handle                     // ����� ����� ����
      owc:Name      := This.Name
      owc:oWin      := This.Object
      owc:ahIcoDel  := {}                              // ��� �������� ������� ������ � �����
      owc:nG        := nG                              // ������ �� ���� ����
      owc:cFormCurr := cFormCurr                       // ������������ ����
      owc:cBtnEnabled := cBtnEnabled                   // �������������� ������

      nY := 0
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff WIDTH 10 HEIGHT 10 VALUE "" CENTERALIGN VCENTERALIGN INVISIBLE

      /////////////////////// ������ ������ ����� /////////////////////////////////////
      oMenu:nY       := nY + nG
      oMenu:nX       := nX + nG                   // ���������� ������ ������
      oMenu:nClientW := nW                        // ������ ���� ���� ����� �������� ������
      MenuTopIconButtons( owc, oMenu )            // -> menu_topButton.prg (owc:nG)
      // owc:nHTBar                               // ������ ������ + �������

      owc:nTsbY := owc:nHTBar
      owc:nTsbX := nG
      owc:nTsbW := nW - nG*2
      owc:nTsbH := nH - owc:nTsbY - nG

      //@ owc:nTsbY, owc:nTsbX  LABEL Label_Tsb WIDTH owc:nTsbW HEIGHT owc:nTsbH VALUE cTitle ;
      //  BACKCOLOR GRAY CENTERALIGN VCENTERALIGN
      /////////////////////// ������� //////////////////////////////////////////////////////////
      cBrw := "Tsb_" + cForm
      oTsb := TsbPatamDbf( nTable, cForm, cAls, cBrw, cSuperHd, owc:nTsbW, oClr, oColnm )
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, cAls, cBrw, owc:nTsbY, owc:nTsbX, owc:nTsbW, owc:nTsbH )
      //
      oBrw:Cargo:nModify := 0         // ������� ��������� - �� ���������
      This.Cargo:oBrw    := oBrw      // �������� �� �����

      // ��������� ������� �� ��� ����
      IF nTable == 1
         Sets_Event2Zaivka()             // -> demo3_1Base.prg
      ELSEIF nTable == 2
         //Sets_Event2Dogovor()          // -> demo3_2Base.prg
      ENDIF

      ON KEY ESCAPE OF &cForm ACTION _wPost(99,cForm)
      ON KEY F1     OF &cForm ACTION NIL

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   ?  ProcNL(), "-->> End " + cForm

   // ������������ ������� ���� / restore hidden windows
   FOR nI := 1 TO Len(aHide)
      //IF _IsWindowDefined(aHide[nI])
      //   DoMethod(aHide[nI], "Show")
      //ENDIF
   NEXT

   ? ProcNL(), "-->> Return","aRet=", HB_ValToExp(aRet)

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatamDbf(nTable, cForm, cAls, cBrw, cSuperHd, nTsbW, oColor, oColnm )
   LOCAL oTsb, nClr1, nClr2, a, nI, nHCell

   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cAls           := cAls
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   oTsb:nTable         := nTable     // ����� ������� - ����������� !!!
   oTsb:nMemoHV        :=  1         // ����� ����� ������ ����-���� - �� �������� !!!
   oTsb:lNoPicture     := .T.
   oTsb:lFooting       := .T.        // ������� � ������� ������
   oTsb:lNoPicture     := .T.
   oTsb:lSpecHd        := .F.        // �� ��������� � ������� ��������� �������
   oTsb:lSuperHd       := .T.        // ��������� � ������� ����������
   oTsb:cSuperHd       := cSuperHd
   oTsb:aFont          := oColnm:aFont
   nI := nTsbW // ������

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // ��������� � ������� ������
      oTsb:aFoot       := .T.                            // ��������� ������
   ENDIF

   //oTsb:uSelector    := 20                                      // �� ���������
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // ������� ��������� � � ������
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   nHCell              := IIF( nHCell < 32, 32, nHCell )
   oTsb:nHeightCell    := nHCell                            // ������ �����
   oTsb:nHeightHead    := nHCell                            // ������ �����
   oTsb:nHeightFoot    := nHCell                            // ������ �������

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                               // �� ��������� � ������� ���������
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := GetFontHeight(oTsb:aFont[4])    // ������ ����������
   ENDIF

   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      oTsb:nHeightSuperHd := nHCell                         // ������ �����������
   ENDIF

   nClr1 := HMG_RGB2n(oColor:aBClr)                         // ���� ���� �����+������
   nClr2 := RGB( 48, 29,26)                                 // ����-������ ���
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // ����: ����� � ��� �����������
   oTsb:aBrush         := oColor:aBrush   //{240,240,240}   // ���� ���� ��� ��������

   // ����� � �������
   a := {}
   // 1 , ������ �����
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , ������ �����
   // 2 , ���� � ������� �������
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , ������ ����� �������
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , ���� ����� �������
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , ������ �������������� ����
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , ���� �������������� ����
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , ������ ������� �������
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, ���� ������� �������
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - ���������
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - ���������
   oTsb:aColorAdd := a
   oTsb:lZebra    := oColor:lZebra
   oTsb:aZebra    := oColor:aZebra

   // ���������� ���� ������� ������� / Assigning all columns of a table
   oTsb:aHideCol := {} //{ 4, 5, 6, 7, 8}   // ������ �������, ��������� SELECTOR � ARRAYNO
   oTsb:aField   := oColnm:aField           // ����������� ��� dbf !!!
   oTsb:aHead    := oColnm:aHead
   oTsb:aName    := oColnm:aName
   oTsb:aSize    := oColnm:aSize            // �������� ������ ������� ��� ���
   oTsb:aPict    := oColnm:aPict            // ������ �����, ���� �����

   ThisCheckFieldDbf(oTsb:aField)           // �������� ����� ����
   // �������� ������� �������
   //  ? "oTsb:aField=", oTsb:aField ; ?v oTsb:aField
   //  ? "oTsb:aName= ", oTsb:aName  ; ?v oTsb:aName
   //  ? "oTsb:aPict=" , oTsb:aPict  ; ?v oTsb:aPict
   //  ? "oTsb:aHead= ", oTsb:aHead  ; ?v oTsb:aHead
   //  ? "oTsb:aFoot= ", oTsb:aFoot  ; ? IIF( IsArray(oTsb:aFoot), HB_ValToExp(oTsb:aFoot), oTsb:aFoot )
   //  ? "oTsb:aSize= ", oTsb:aSize  ; ?v oTsb:aSize
   //  ? "oTsb:aAlign=", oTsb:aAlign //; ?v oTsb:aAlign
   //

   // ��� ���. ������ �� �������� ����� �� demo3_1Base.prg
   oTsb:aColPrc  := oColnm:aColPrc     // ��� ��������� �������
   oTsb:aFunc1   := oColnm:aFunc1      // �������-1 :bPrevEdit ��� ��������� ������� �������
   oTsb:aFunc2   := oColnm:aFunc2      // �������-2 :bPostEdit ��� ��������� ������� �������
   oTsb:aBlock   := oColnm:aBlock      // ������� ���� �� ��������� ���� � �������
   oTsb:aDecode  := oColnm:aDecode     // ��� ������� oCol:bDecode
   oTsb:aCol     := oColnm:aCol        // ������ ������� ������� - �������� ����������� !!!
   oTsb:aTable   := oColnm:aTable      // �������� ���� ������ ������� � cargo ����, �� ������ ������

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
                   Local oTsb
                   ob:HideColumns( op:aHideCol ,.t.)              // ������ �������
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ORDKEYNO")       // ���������� �������
                   //ob:nFreeze   := ob:nColumn("ADRESPRN")       // ���������� �������
                   ob:lLockFreeze := .T.                          // �������� ���������� ������� �� ������������ ��������
                   ob:lNoKeyChar  := .F.                          // ���� � ������ �� ����, ����
                   ob:nMemoHV     :=  1                           // ����� ����� ������ ����-����
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nCell       := 3                            // ����������� ������
                   WITH OBJECT ob
                     oTsb := op
                     // ��� ���. ������ �� ��������
                     :Cargo:aColPrc := oTsb:aColPrc     // ��� ��������� �������
                     :Cargo:aFunc1  := oTsb:aFunc1      // �������-1 :bPrevEdit ��� ��������� ������� �������
                     :Cargo:aFunc2  := oTsb:aFunc2      // �������-2 :bPostEdit ��� ��������� ������� �������
                     :Cargo:aTable  := oTsb:aTable      // �������� ���� ������ ������� � cargo ����, �� ������ ������
                     :Cargo:aBlock  := oTsb:aBlock      // ������� ���� �� ��������� ���� � �������
                     :Cargo:aDecode := oTsb:aDecode     // ��� ������� oCol:bDecode
                     :Cargo:lRecINS := .F.              // ���������� ������� INS
                     :Cargo:lRecDEL := .F.              // ���������� ������� DEL
                     :Cargo:nTable  := oTsb:nTable      // ����� ������� - ����������� !!!
                   END WITH
                   Column_Init( ob, op )   // ������ ���� �� ���� ����
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // ������ ��������� ���
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // ���� ���������
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   Local oc, i := 0, aBmp, aMsg, bBmpCell, hImg, cImg, cField
                   Local cMsg := "", nHImg := 32
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - ��� ���
                   ob:lNoHScroll  := .T.   // ��� ������ ��������������� ���������
                   ob:oHScroll    := NIL
                   // ������ ������ ������� � ����������
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // ��������� ����� ���� ����������� �������
                         oc:nClrFore    := CLR_RED          // ��������� ����� ������ ����������� �������
                         oc:hFont       := hFont            // ��������� ����� ����������� �������
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         oc:nFAlign     := DT_LEFT
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   // ������ ������� (2) ������ �������
                   oc := ob:aColumns[2]   // "MARK"
                   oc:lEdit     := .T.
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nEditMove := 0    // ���������� ������
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   // ������ ������� (3) ��� ���� - ����� ������
                   IF ob:Cargo:nTable == 1        // -> demo3_1Base.prg
                      cField := "KZBID"
                      aBmp := {"bFWord32","bFExcel32","bFCalc32","bFText32","bFCSV32","bFZero32"}
                      aMsg := {"File MS Word", "File MS Excel", "File OO Calc", "File *.txt",;
                                      "File *.csv", "Delete value" }
                   ELSEIF ob:Cargo:nTable == 2    // -> demo3_2Base.prg
                   ENDIF
                   cField := "KZBID"
                   nI := ob:nColumn(cField, .T.)
                   IF nI > 0
                      oc := ob:aColumns[nI]
                      oc:Cargo := oHmgData()         // �������� ��������� �� �������
                      oc:Cargo:cField := cField
                      oc:Cargo:aBmp   := aBmp
                      oc:Cargo:aMsg   := aMsg
                      oc:nClrBack  := CLR_WHITE
                      oc:lEdit     := .T.
                      oc:nEditMove := 0              // ���������� ������
                      oc:lBitMap   := .T.            // ������ ����� �������� ���� �� �������
                      //oc:nWidth := nHImg           // ������ ������� ��� � �������� - ������ � oTsb
                      oc:aBitMaps := {}
                      For i := 1 To Len(aBmp)
                         cImg := aBmp[i]
                         hImg := LoadImage(cImg,,nHImg,nHImg)
                         AAdd( oc:aBitMaps, hImg )
                         If hImg == 0
                            cMsg += "No image ["+cImg+"] in resources;"
                         Endif
                      Next
                      If Len(cMsg) > 0
                         cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
                         AlertStop(cMsg,,,64,{RED})
                      Endif
                      bBmpCell := {|nc,ob| // ����� �������� � ����������� �� ���� "K????"
                                    Local ocol  := ob:aColumns[nc]
                                    Local ni    := 0                      // bFZero32
                                    Local nMax  := LEN(ocol:aBitMaps)     // bFZero32
                                    Local nCode := ob:GetValue(ob:Cargo:cField)   // ������� ���� ���� ������
                                    //? ProcName(), nCode, ocol:cName, ocol:cField
                                    //nCode := FIELDGET(FIELDNUM(ob:Cargo:cField))  // ����� � ���
                                    IF !IsNumeric(nCode)
                                       nCode := 0
                                    ENDIF
                                    IF nCode <= 0 .OR. nCode >= nMax
                                       ni := nMax
                                    ELSE
                                       ni := nCode
                                    ENDIF
                                    Return ocol:aBitMaps[ni]  // �������� � ������� �������
                                    }

                      oc:uBmpCell := bBmpCell  // ����-��� ����� ��������
                      oc:nAlign   := nMakeLong( DT_CENTER, DT_CENTER )
                      oc:nHAlign  := DT_CENTER
                      //oc:bData  :=  {||Nil}
                      //oc:cData  := '{||Nil}'
                      // �������������� ������� (3)
                      oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }  // -> ��.����
                   ENDIF
                   //
                   // �������� � ������
                   // ����� ������� ������� / change table cursor
                   // cFooting := Eval( oColumn:cFooting, nCol, oBrw )
                   oc := ob:GetColumn("ADRESPRN")
                   oc:nFAlign  := DT_LEFT
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   nc := ""
                                   If ob:nLen > 0
                                      nc := hb_ntos(na)+ "/" + hb_ntos(nl)
                                      nc += Space(5) + " [!]"
                                   EndIf
                                   Return nc
                                   }
                   ob:bChange := {|ob|  _wPost(19, ob:cParentWnd, ob) } // ��� ����� ������� �������
                   //
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   // �������� ������� � �������
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_RETURN, {|ob|
                      Local oc := ob:aColumns[ ob:nCell ]
                      Local xval, lRet
                      IF oc:cName == "MARK"
                      ELSEIF oc:cName == "KZBID"
                         //oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }
                         xval := ob:GetValue(ob:nCell)
                         lRet := EVal(oc:bPrevEdit, xval, ob )
                      ELSE
                        _wPost(40, ob:cParentWnd, ob)
                      ENDIF
                      Return Nil
                      } } }

   // ��������� ������� �� ����
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;   // ���� �� ������ �������
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;   // ���� �� ������ �������
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }, ;   // ���� �� ������ �������
        {40, {|ow,ky,ob| _wPost(40,ow)  , ky:=ow:=ob                       } }, ;   // ��������
        {50, {|ow,ky,ob| _wPost("_TsbRClick",ow) , ky:=ow:=ob              } }  ;   // ������ ���� �����
                     }

   // ������� ���� ����� �� ������� � ������� - ��� ����
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // ��� ��� !!!
   // ������ ���� ����� �� ������� � �������
   oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(50, ob:cParentWnd, {p1,p2,p3,ob}) }
   // ����� ���� ����� �� ������� � �������
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

//////////////////////////////////////////////////////////////////////////
// ������ ���� �� ���� ���� �� ��������
STATIC FUNCTION Column_Init( ob, op )
   Local oCol, aDim, nI, nJ, nO, nS, bBlock, aBlock, cStr, nMax
   Local aColPrc, aFunc1, aFunc2, aDecode, bDecode, cMsg

   ? "### oTsb:bInit - "+ProcNL()
   // ��� ���������� ���������
   aDim    := op:aCol     // ���� ������ �������
   aBlock  := op:aBlock   // ���� ����
   aColPrc := op:aColPrc  // ��� ��������� �������: "BMP", "K", "S", "C", "N", "D"
   aFunc1  := op:aFunc1   // �������-1 :bPrevEdit ��� ��������� ������� �������
   aFunc2  := op:aFunc2   // �������-2 :bPostEdit ��� ��������� ������� �������
   aDecode := op:aDecode  // ��� ������� oCol:bDecode
   IF !IsArray(aDim)
      cMsg := "Error! No table column array !;;"
      cMsg += "oCol:aCol := aCol"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN NIL
   ENDIF

   nMax := 0
   nO   := IIF( ob:nColumn("ORDKEYNO", .T.) > 0, 1, 0) // �������� ����, ���� ���, �� ����� 0
   nS   := IIF( ob:lSelector, 1, 0 )   // ���� ����/��� ��������
   nJ   := nO + nS
   ?? "ORDKEYNO:", nO, "lSelector:", nS

   FOR EACH oCol IN ob:aColumns
      cStr := ATREPL( CRLF, oCol:cHeading, "|" )
      nMax := MAX( nMax, LEN(cStr) )
   NEXT

   // ������ ���� �� ���� ���� � ������ bDecode
   FOR EACH oCol IN ob:aColumns
       nI := hb_EnumIndex(oCol)
       ? nI, oCol:cName, PADR( ATREPL( CRLF, oCol:cHeading, "|" ), nMax ) + ","
       ?? oCol:cField, "FieldPos()=", FieldPos(oCol:cName), "nW=",oCol:nWidth
       oCol:Cargo := oHmgData()
       oCol:Cargo:cName  := oCol:cName
       oCol:Cargo:lTotal := .F.
       oCol:Cargo:nTotal :=  0                 // ���� �� �������
       IF nI <= ob:nColumn("ORDKEYNO") ; LOOP
       ENDIF
       nJ := nI - nO // ��������� ������� ORDKEYNO
       IF nJ > 0
          bBlock := aBlock[nJ]
          ?? "nJ=", nJ
          IF ISBLOCK(bBlock)
            ?? "bBlock=" , bBlock
            ?? aDim[nJ,8]
            //oCol:bData  := &(bBlock)
            oCol:bData    := bBlock
            oCol:nAlign   := DT_LEFT
            //nLen        := LEN(aDim[nJ,6])  // ������ ���� - �� ���������
            //oCol:nWidth := oCol:ToWidth( REPL("a",nLen) ) - ��� �������
            //oCol:cPicture := aDim[nJ,7]       // ������ ����
            //oCol:bDecode  := {|cv| Alltrim(cv) }
            //oCol:l3DLook   := .T.
            oCol:l3DTextCell := .T.
            //oCol:nClr3DLCell := CLR_RED
          ENDIF
          //?? VALTYPE(bBlock), bBlock
          bDecode := aDecode[nJ]
          IF ISBLOCK(bDecode)
             ?? "bDecode=", bDecode
             oCol:bDecode := bDecode
          ENDIF
          //oCol:cFooting := ""
       ENDIF
   NEXT

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ColumnEditPrev_Two(xVal, oBrw)
   LOCAL nCol, oCol, cFld, cAls, cTyp, cNam, lIco, aRet, xOld, cMsg, lModify
   LOCAL aBmp, aMsg, nTime

   nCol  := oBrw:nCell
   oCol  := oBrw:aColumns[ nCol ]
   cAls  := oBrw:cAlias
   cTyp  := oCol:cFieldTyp
   cFld  := oCol:cField
   cNam  := oCol:cName
   xOld  := xVal                 // �������� ���������� ��������
   nTime := VAL( SUBSTR(TIME(),1,2) + SUBSTR(TIME(),4,2) )
   // ��������� �� ���������� �������
   aBmp  := oCol:Cargo:aBmp
   aMsg  := oCol:Cargo:aMsg
   IF !IsArray(aBmp) .OR. !IsArray(aMsg)
      cMsg := "Error! NO arrays in column container!;;"
      cMsg += "oCol:Cargo:aBmp = ???;"
      cMsg += "oCol:Cargo:aMsg = ???"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN .F.
   ENDIF

   //MsgDebug(nCol, oCol, cFld, cAls, cTyp, cNam)
   lIco := .F.   // BMP type
   aRet := Tsb_ContexMenuTwo(oBrw,aMsg,aBmp,lIco)  // -> ��.����
   IF LEN(aRet) > 0
      xVal := aRet[1]
      //(cAls)->&cFld := xVal               // ��� � ����
      IF (cAls)->(RLock())                  // ������ ������
         oBrw:SetValue(cNam, xVal)          // ������ � ����
         //(cAls)->KOPERAT   := M->nOperat  // ��� ������ ������
         (cAls)->DATEVVOD  := DATE()        // ���� ������
         (cAls)->TIMEVVOD  := nTime         // 9999 ����� ������
         IF ( lModify := xOld != xVal )     // modify value
            oBrw:Cargo:nModify ++           // ���� ����������� �������
            //������ � ������-��������-�������������-���������
            //write to the program-user-actions-log
         ENDIF
      ELSE
         cMsg := "������ ������������� !;"
         cMsg += "Recno blocked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop(cMsg,,,64,{RED})
      ENDIF
      (cAls)->(dbUnLock())
      oBrw:Skip(0)
      oBrw:DrawSelect()    // ������������ ������� ������ �������

   ENDIF

RETURN .F.

//////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisCheckFieldDbf(aNames)
   LOCAL cTmp, n, nE, cMsg, cAls, nCols

   cAls  := ALIAS()
   cTmp  := ""

   IF aNames == NIL
      cMsg := "Error! No list of DB fields: " + cAls + ";;"
      cMsg += "aNames == NIL"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN NIL
   ENDIF

   nCols := Len( aNames )

   FOR n := 1 TO nCols
      nE := ( cAls )->( FieldPos( aNames[ n ] ) )
      IF nE == 0
         cTmp += HB_NtoS(n) + ". " + aNames[ n ] + ";"
      ENDIF
   NEXT
   IF LEN(cTmp) > 0
      cMsg := "Error! No fields in DB: " + cAls + ";;" + cTmp
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_ContexMenuTwo(oBrw, aDim, aImg, lIcon, lDopMenu)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, aRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, nMenu, aMsg
   LOCAL aFont, nFSize, cName, nWCell, nHCell, oCell, cImg
   DEFAULT lIcon := .T.   // ������ � ����, ����� BMP
   DEFAULT lDopMenu := .F.

   cForm  := oBrw:cParentWnd
   oWnd   := _WindowObj(oBrw:cParentWnd)
   // ���������� ������ � ������� Edit
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oWnd:Row + oCell:nRow - 2 //+ GetTitleHeight() /*+ GetMenuBarHeight()*/
   //nY   += oBrw:nTop + IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   //nY   += oBrw:nHeightHead
   nX     := oWnd:Col + oCell:nCol + 2
   nWCell := oCell:nWidth - 2
   nHCell := oCell:nHeight - 2

#ifdef KEY_ENG // for this project demo1-en.hbp
   hFont1 := GetFontHandle("Normal")     // ���� ������� �������
   hFont2 := GetFontHandle("ComSanMS")
   aMsg   := { "Delete value", "Exit" }
#else
   hFont1 := GetFontHandle("Normal")     // ���� ������� �������
   hFont2 := GetFontHandle("FntBtn_1")   // ����-1 ������ ������ ����
   aMsg   := { "������� ��������", "�����" }
#endif

   aFont  := GetFontParam(hFont1)
   nFSize := aFont[2]
   nMenu  := 0

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cMenu   := aDim[nI]
         cImg    := aImg[nI]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         hFont   := IIF( lDis, hFont2, hFont1 )

         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      IF lDopMenu
         SEPARATOR
         MENUITEM  aMsg[1] ACTION  {|| nMenu := -1 } FONT hFont2  ICON "iDelVal32"
         SEPARATOR
         MENUITEM  aMsg[2]  ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
      ENDIF
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      aRet := { nMenu, aDim[nMenu] }
   ELSEIF nMenu == -1
      aRet := { 0, "-.-" }
   ELSE
      aRet := {}
   ENDIF

   DO EVENTS

RETURN aRet

