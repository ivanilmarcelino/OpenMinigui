/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ����� �������� � ���� / Export to file form
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbColumn(cTitle, oWnd, oBrw, nPos)
    LOCAL cCol, aRet, nCol

    IF nPos == 3
       aRet := MG_YesNo2Get()
       IF LEN(aRet) > 0
          nCol := aRet[2] + IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, 1, 0 )
          nCol += IIF( oBrw:lSelector, 1, 0 )
          oBrw:GoPos(aRet[1], nCol)
       ENDIF
    ELSE
       Form_DbColumn(cTitle, oWnd, oBrw)
       aRet := App.Cargo:aGoColumn
       IF LEN(aRet) > 0
          //oBrw:GoPos(oBrw:nRowPos, oBrw:nColumn("ColName"))
          cCol := aRet[2]
          oBrw:GoPos(aRet[1], oBrw:nColumn(cCol))
       ENDIF
    ENDIF

    oBrw:Setfocus()
    DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION Form_DbColumn(cTitle, oWnd, oBrw)
   LOCAL cIcon, cFont, nFontSize, aBackColor, cFontBtn, nHIco, cTxt, aBtn, bAct
   LOCAL nY, nX, nW, nH, nG, nLR, nWTxt, nHTxt, nHBtn, nWBtn, nYBtn, nXBtn, cMsg
   LOCAL ao, owc, oDlu, cBrw, oTsb, ahIco, cForm, cAls, a4Clr, cMd5
   LOCAL aBtnFont, aBtnFClr, nFBtnSize, cDbfCnfg, cMsgDown, nColNum
   LOCAL cField, cWTtl, cAlsParent, n3Btn, nHFrm, oCol, cMsgDown2

   ? ProcNL(), oWnd:Name, oBrw:cAlias
   App.Cargo:aGoColumn := {}          // ������� { ����� ������, ��� ������� }
   cAlsParent := oBrw:cAlias          // ���� � ������������� ����
   ao         := (App.Cargo)
   cFont      := ao:cFontName                     // DejaVu Sans Mono
   nFontSize  := ao:nFontSize
   cFontBtn   := ao:cFontName2                    // Comic Sans MS
   nFBtnSize  := ao:nDlgSize                      // ao:nFontSize + 2
   aBtnFont   := { cFontBtn, nFBtnSize, .T. }     // ���� ��� ������
   // ����� �������: [1]-����, [2]-(�����+������), [3]-(������ %1), [4]-(������ %2)
   a4Clr      := { {148,85,185}, {142, 25,142}, {238,130,238} , {232,212,244} }  // ���������� 1
   aBackColor := a4Clr[1]                         // ���� ���� �����
   cForm      := "Form_Search"
   cIcon      := "iFilter48"
   cWTtl      := IIF( ao:cLang == "RU", "���� ������", "Search window")
   cMsgDown2  := IIF( ao:cLang == "RU", "����� ������:", "Line number:")
   cMsgDown   := "<-- " + IIF( ao:cLang == "RU", "������� ������ ������", "enter search string")
   aBtnFClr   := { BLACK, YELLOW }     // ���� ����� ������ / ���� ����� ������ ��� ������� ������
   cBrw       := "Tsb_Search"
   cMsg       := IIF( ao:cLang == "RU", "�� ���� ������� ���� ��� ������ !", "Could not create base for Search !")
   cMd5       := HB_MD5(App.Exename) + cAlsParent              // ����.�����, ������ ��������� ���
   cDbfCnfg   := ao:cPathTemp + "mg_Search_" + cMd5 + ".dbf"   // ����, ��� ���� �������� �������
   // ������� ����� ���� ��� ������� �������
   cAls       := CreateConfigFilter(cDbfCnfg,oBrw)
   IF LEN(cAls) == 0
      cMsg += ";" + cDbfCnfg + ";;" + ProcNL()
      AlertStop( cMsg , , "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF
   DbSelectArea(cAls)

   // ���������� ������ � ������� �������� �� ������� ����� �� dlu � pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top  // ����� ���
   nW   := ao:aDisplayMode[1] * IIF( App.Cargo:aDisplayMode[2] <= 720, 0.7, 0.6) // ������� ����
   nH   := ao:aDisplayMode[2] * 0.9        // ������� ����
   nG   := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   nX   := nLR := nG                       // ������ ����� � �����
   nY   := nG                              // ������ ������ � �����

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH   ;
      ICON cIcon TITLE cWTtl BACKCOLOR aBackColor      ;
      MODAL NOSIZE                                     ;
      FONT cFont SIZE nFontSize                        ;
      ON INIT    _wPost( 0)                            ;
      ON RELEASE _wSend(98)

      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor    := This.BackColor   // ���� ����
      owc:ahIcoDel   := {}               // ��� �������� ������� ������ � �����
      owc:oWnd       := oWnd             // ��������� ���� ������ Cargo -> ������������� ����  - ������� 3
      owc:cAlsParent := cAlsParent       // ���� � ������������� ����
      owc:cAls       := cAls             // ����� ���� ������� �������

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nHBtn := IIF( App.Cargo:aDisplayMode[2] <= 720, 38, 64 ) // ������ ������ ����� ����

      nWTxt := nW - nLR * 2
      nHTxt := nFontSize * 2.5
      nHFrm := nHTxt

      @ 0, 0 LABEL Label_0 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      //@ nY, nX LABEL Label_1 WIDTH nW-nG*2 HEIGHT nH-nHBtn-nG*3 VALUE 'Table' BACKCOLOR WHITE
      oTsb := TsbPatam(cBrw,a4Clr,cAls,cTitle,a4Clr)
      // ����� ���� ��� _TBrowse(...) - ������ ������ // (op=oTsb)
      oTsb:bInit  := {|ob,op| myTsbInit(ob,op), myTsbFont(ob,op), myTsbSuperHd(ob,op) }  // ��������� ���  -> ��.����
      oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)  }  // ������ ��������� ���  -> ��.����
      //oTsb:bEnd := {|ob,op| myTsbEnd(ob,op)                      }  // ���� ���� ����� END TBROWSE -> ��.����
      oTsb:nW     := nW-nG*2
      oTsb:nH     := nH-nHBtn-nG*3 - nHFrm
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, cAls, cBrw, nY, nX, oTsb:nW, oTsb:nH )
      // ��������� �������� � ���
      myTsbEnd(oBrw,oTsb)                // <<----- ��� ����������
      //App.Cargo:oBrw  := oBrw          // ��������� ��� ������� ������� - ����� �� ����
      owc:oBrw := oBrw                   // ��������� �� ����
      owc:cBrw := cBrw                   // ��������� �� ����
      oBrw:Cargo:ObjWnd := This.Object   // ������� ����

      oCol  := oBrw:GetColumn("FNAME")
      nX    := oBrw:nLeft + oCol:oCell:nCol
      nWTxt := oCol:oCell:nWidth
      nY    := nY + oTsb:nH

      cField := SPACE(15)
      @ nY, nX GETBOX GB_Find VALUE cField WIDTH nWTxt HEIGHT nHTxt FONTCOLOR BLACK BACKCOLOR WHITE ;
        PICTURE REPL('x',80) ON CHANGE {|| cField := This.Value, DoEvents(), _wPost(10) }
      owc:cGetBox1 := "GB_Find"       // ��������� �� ����

      nX += This.GB_Find.Width + 10

      @ nY, nX LABEL Label_1 WIDTH 350 HEIGHT nHTxt VALUE cMsgDown FONTCOLOR WHITE TRANSPARENT VCENTERALIGN

      oCol  := oBrw:GetColumn("FREM")
      nX    := oBrw:nLeft + oCol:oCell:nCol
      nWTxt := oCol:oCell:nWidth

      nColNum := 0
      @ nY, nX GETBOX GB_ColNum VALUE nColNum WIDTH nWTxt HEIGHT nHTxt FONTCOLOR BLACK BACKCOLOR WHITE ;
        PICTURE REPL('9',3) ON CHANGE {|| nColNum := This.GB_ColNum.Value, DoEvents(), _wPost(11) }
      owc:cGetBox2 := "GB_ColNum"       // ��������� �� ����

      nX := This.GB_ColNum.Col - GetTxtWidth( cMsgDown2, nFontSize, cFont, .F. ) - 10
      @ nY, nX LABEL Label_2 WIDTH 350 HEIGHT nHTxt VALUE cMsgDown2 FONTCOLOR WHITE TRANSPARENT VCENTERALIGN

      /////////////////////// ������ ����� ����� ////////////////////////////
      nWBtn := 300
      n3Btn := nWBtn*2 + nG*3
      IF n3Btn > nW
         nWBtn := ( nW - nG*4 ) / 3
      ENDIF
      nYBtn := nH - nG - nHBtn
      nHIco := nHBtn - nG/2

      cTxt  := IIF( ao:cLang == "RU", "������� �� �������", "Switch to column" )
      nXBtn := nW - nWBtn*2 - nG*2
      aBtn  := { "Button_Filtr", cTxt, "iGoto64x1", "iGoto64x2", nHIco, aBtnFClr, aBtnFont, "" }
      bAct  := {|| This.Button_Filtr.Enabled := .F. ,  _wPost(90,cForm) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, {60,230,84} )
      // �������� ���� ������ �����
      This.Button_Filtr.Action := bAct
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "������", "Cancel" )
      nXBtn := nW - nWBtn - nG
      aBtn  := { "Button_Exit", cTxt, "iReturn64x1", "iReturn64x2", nHIco, aBtnFClr, aBtnFont, "" }
      //bAct:= {|| This.Button_Exit.Enabled := .F.,  _wPost(101,cForm) }   // �� �������� - �����
      bAct  := {|| App.Cargo:aGoColumn := {} ,  _wPost(99,cForm) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, CLR_HRED )
      // �������� ���� ������ �����
      This.Button_Exit.Action := bAct
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      //_o2log(owc, 15, ProcNL()+" -------------- ��������� ������� : => owc", .T.)

      // ��������� ������� �� ��� ���� ���������
      Sets_Event2ThisWindow()

      ON KEY ESCAPE OF &cForm ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL    // ������� ����� App.Cargo:nColumn

//////////////////////////////////////////////////////////////////////////////////////////////////////
// ��������� ������� �� ��� ���� ���������
STATIC FUNCTION Sets_Event2ThisWindow()

   WITH OBJECT This.Object
     :Event( 0, {|ow| // ������ ����� ���������� ����
                       This.Topmost := .F.
                       ? ProcNL(),">>> Start window: "+ow:Name, ow:Cargo:cGetBox1
                       ow:Setfocus(ow:Cargo:cGetBox1)
                       //ow:Cargo:oBrw:Setfocus()
                       DO EVENTS
                       Return Nil
                       })

     :Event(10, {|ow| Refresh_Tsb(ow) } )   // ������ �� ���
     :Event(11, {|ow| // ������� �� ������ �� ������
                      Local cObj := ow:Cargo:cGetBox2
                      //Local nRowPos
                      //nRowPos := This.&(cObj).Value
                      //ow:Cargo:oBrw:GoPos(nRowPos, )
                      ow:Setfocus(cObj)
                      DO EVENTS
                      Return Nil
                      })

     :Event(80, {|ow| // ������
                      SET WINDOW THIS TO ow:Name
                      myHelpFilter()      // ��.����
                      SET WINDOW THIS TO
                      This.Button_Help.Enabled := .T.
                      //ow:SetFocus('Label_0')
                      ow:Cargo:oBrw:Setfocus()
                      Return Nil
                      } )

     :Event(90, {|ow| // ������ ������� � ������� ����
                      Local owc  := ow:Cargo
                      Local ao   := App.Cargo
                      Local cCol := owc:oBrw:GetValue("FNAME")
                      // ���������� ����� ������, ��� ����� ������
                      ao:aGoColumn := { This.GB_ColNum.Value, cCol }
                       _wPost(99,ow:Name)
                      This.Button_Filtr.Enabled := .T.
                      Return Nil
                      } )

     :Event(98, {|ow| // ON Release
                      Local ah := ow:Cargo:ahIcoDel
                      ?  ProcNL()
                      ?? ">>> Exit button pressed! Window: "+ow:Name
                      ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                      ?? ah, HB_ValToExp(ah)
                      IF IsArray(ah)
                         AEval(ah, {|h| DestroyIcon(h) })  // ������� ������ ������
                      ENDIF
                      IF LEN(ow:Cargo:cAls) > 0
                         (ow:Cargo:cAls)->( dbCloseArea() )
                      ENDIF
                      DbSelectArea(ow:Cargo:cAlsParent)  // ���� � ������������� ����
                      DO EVENTS
                      Return Nil
                      } )

     :Event(99, {|ow| ow:Release() } )
   END WITH

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Refresh_Tsb(ow)                 // ����� �� ����
   LOCAL oBrw  := ow:Cargo:oBrw
   LOCAL cObj  := ow:Cargo:cGetBox1
   LOCAL cFind := This.&(cObj).Value

   cFind := Alltrim(cFind)
   IF Empty(cFind) ; cFind := Nil
   ENDIF

   IF ISOBJECT(oBrw)
      oBrw:FilterFTS( cFind, .T. )
      oBrw:Refresh()
      oBrw:SetFocus()
   ENDIF
   DO EVENTS
   ow:Setfocus(cObj)

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam(cBrw, a4Clr, cAls, cTitle)
   LOCAL oTsb

   oTsb := oHmgData()
   oTsb:cAls        := cAls
   oTsb:cBrw        := cBrw
   //                      cell     Head   foot    SpecHider   SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Bold", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }                      // ������ ��������� ����� �������
   //oTsb:uSelector := 20                             // ������ SELECTOR
   oTsb:lSpecHd     := .T.                            // ��������� � ������� ��������� �� ��������
   oTsb:lFooting    := .T.                            // ��������� � ������� ������
   oTsb:aFoot       := .T.                            // ��������� ������
   oTsb:nHeightFoot := 10                             // ������ �������
   oTsb:nHeightHead := 25                             // ������ �����
   oTsb:nHeightSpecHd := 12                           // ������ ���������� ENUMERATOR
   oTsb:aEdit       := .T.                            // ������������� �������
   oTsb:a4Clr       := a4Clr                          // �������� 4 ����� �������
   oTsb:aBrush      := a4Clr[3]                       // ���� ���� ��� ��������
   oTsb:aColor      := Color_Tsb(a4Clr,oTsb)          // ����� �������: 1(����),2(�����+������),3(������ %1),4(������ %2)
   oTsb:cTtlSupHead := cTitle
   oTsb:a4Clr       := a4Clr
   //                        1          2          3                   4                     5
   oTsb:aHead       := { "��� ����","��� ����","����� ����" ,'����������;���� �����;"."' ,"����������" }
   oTsb:aField      := { "FNAME"   ,"FTYPE"   ,"FLEN"       ,"FDEC"                      ,"FREM"       }
    // ������������� �������
   oTsb:aEdit       := .F.
   oTsb:aName       := oTsb:aField
   //oTsb:nHCell    := App.Cargo:nTsbHCell                            // ������ ������
   oTsb:nHCell      := App.Cargo:nFontSize + (App.Cargo:nFontSize/2)  // ������ ������

   IF App.Cargo:cLang == "EN"
      oTsb:aHead := { "Field name","Field type","Field length" ,'Decimal;sign after;"."',"Note" }
   ENDIF

RETURN oTsb

///////////////////////////////////////////////////////////
STATIC FUNCTION CreateConfigFilter(cDbf,oBrw)
   LOCAL aStr, cAls, cCdp, cTyp, cCol, oCol, aFld, nI
   LOCAL lRet, cRdd, lShared, nLen, nDec

   // ������ ����� ��
   aFld := {}
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         cTyp := oCol:cFieldTyp
         nLen := oCol:nFieldLen
         nDec := oCol:nFieldDec
         AADD( aFld, { cCol, cTyp, nLen, nDec } )
      ENDIF
   NEXT

   cCdp := hb_SetCodepage()
   cRdd := RDDSETDEFAULT()
   cAls := "Search"
   lRet := .T.
   aStr := {}
   AAdd( aStr, { 'FNAME' , 'C', 15, 0 } )
   AAdd( aStr, { 'FTYPE' , 'C',  1, 0 } )
   AAdd( aStr, { 'FLEN'  , 'N',  4, 0 } )
   AAdd( aStr, { 'FDEC'  , 'N',  4, 0 } )
   AAdd( aStr, { 'FREM'  , 'C', 25, 0 } )

   lShared := .F.
   IF !FILE(cDbf)
      dbCreate(cDbf, aStr)
      wApi_Sleep(100)
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
         lRet := ! NetErr() .and. Used()
      END SEQUENCE
      IF !lRet
         RETURN ""
      ENDIF

      FOR nI := 1 TO LEN(aFld)
         APPEND BLANK
         (cAls)->FNAME := aFld[nI,1]
         (cAls)->FTYPE := aFld[nI,2]
         (cAls)->FLEN  := aFld[nI,3]
         (cAls)->FDEC  := aFld[nI,4]
         (cAls)->FREM  := TypeFieldRem(aFld[nI,2])
      NEXT

      wApi_Sleep(100)
      (cAls)->( DbCloseArea() )

   ENDIF

   BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
      DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
      lRet := ! NetErr() .and. Used()
   END SEQUENCE

   DbSelectArea(cAls)
   IF LastRec() < LEN(aFld)
      (cAls)->( DbCloseArea() )
      wApi_Sleep(100)
      HB_FileDelete( cDbf )
   ENDIF

RETURN ALIAS()

//////////////////////////////////////////////////////////////////
STATIC FUNCTION Color_Tsb(aClr,oTsb)             // ����� �������
   LOCAL aColors, nPane2, nPane3, nPane, nHead1, nHead2, nBCSpH
   //                     1           2           3             4
   // aClr[4] �����:  ���� ����| �����+������ | ������ %1 | ������ %2 � ��� ��������

   nPane   := HMG_RGB2n(aClr[3])  // ���� ���� �������
   nPane2  := HMG_RGB2n(aClr[4])  // ������ % 2
   nPane3  := CLR_BLUE            // �������� ������
   nHead1  := HMG_RGB2n(aClr[2])  // ���� ���� �����+������
   nHead2  := RGB( 48, 29,26)     // ����-������ ���
   nBCSpH  := GetSysColor( COLOR_BTNFACE )   // ���� ���� ���������� �������
   aColors := {}
   //AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK             } } )      // 1 , ������ � ������� �������
   //AAdd( aColors, { CLR_PANE  , {|| RGB(247,239,221)      } } )      // 2 , ���� � ������� �������
   // �������� ������� ������
   AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_GRAY, CLR_BLACK ) } } ) // 1
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), nPane3 ,;
                                            iif( ob:nAt % 2 == 0, nPane2, nPane ) )   } } )    // 2 , ���� � ������� �������
   oTsb:aClr1  := CLR_BLACK
   oTsb:aClr16 := { nHead1, nHead2 }
   oTsb:aClr17 := CLR_WHITE

   AAdd( aColors, { CLR_HEADF , {|| CLR_YELLOW            } } )        // 3 , ������ ����� �������
   AAdd( aColors, { CLR_HEADB , {|| { nHead2, nHead1 }    } } )        // 4 , ���� ����� �������
   //AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLACK } } )                  // 5 , ������ ������� � ������� � �������
   AAdd( aColors, { CLR_FOCUSF, {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_WHITE, CLR_BLACK ) } } )  // 5 , ������ ������� � ������� � �������
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   //AAdd( aColors, { CLR_FOCUSB, {|nr,nc,ob| myFocusB(nr,nc,ob,-CLR_HRED,-CLR_BLUE,-CLR_YELLOW) } } ) // 6 , ���� �������

   AAdd( aColors, { CLR_EDITF , {|| CLR_ORANGE            } } )        // 7 , ������ �������������� ����
   AAdd( aColors, { CLR_EDITB , {|| CLR_GREEN             } } )        // 8 , ���� �������������� ����

   AAdd( aColors, { CLR_FOOTF , {|| CLR_YELLOW            } } )        // 9 , ������ ������� �������
   AAdd( aColors, { CLR_FOOTB , {|| { nHead1, nHead2 }    } } )        // 10, ���� ������� �������
   AAdd( aColors, { CLR_SELEF , {|| CLR_GRAY   }            } )        // 11, ������ ����������� ������� (selected cell no focused)
   AAdd( aColors, { CLR_SELEB , {|| { RGB(255,255,74), ;               // 12, ���� ����������� ������� (selected cell no focused)
                                         RGB(240,240, 0) } } } )

   AAdd( aColors, { CLR_ORDF  , {|| CLR_WHITE  }             } )       // 13, ������ ����� ���������� �������
   AAdd( aColors, { CLR_ORDB  , {|| CLR_RED    }             } )       // 14, ���� ����� ���������� �������
   AAdd( aColors, { CLR_LINE  , {|| CLR_WHITE  }             } )       // 15, ����� ����� �������� �������
   AAdd( aColors, { CLR_SUPF  , {|| { nHead1, nHead2 }     } } )       // 16, ���� ���������
   AAdd( aColors, { CLR_SUPB  , {|| CLR_HRED   }             } )       // 17, ������ ���������
   AAdd( aColors, { CLR_SPCF  , {|| CLR_RED    }             } )       // 18, specheader text
   AAdd( aColors, { CLR_SPCB  , {|| nBCSpH     }             } )       // 19, specheader back
   AAdd( aColors, { CLR_SPCA  , {|| CLR_GREEN  }             } )       // 20, active specheader back

RETURN aColors

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // ���������
   Local oCol, nHImg, oDlu, n

   //? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName
   // �������� ������� ������� �� �����
   oDlu := _Font2oDlu( oTsb:aFont[1] )
   n    := oDlu:nSize
   //!!! �������� ������� �������
   //? SPACE(5) + _HMG_DefaultFontName, _HMG_DefaultFontSize, "n=", n, oTsb:aFont[1]
   //? SPACE(5) + "!!!",n," oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   nHImg := oDlu:H1 + 6              // ������ �������� = ������ ����� � ���
   //                ^^^ - ���������
   nHImg := oDlu:H(1.25)             // ��� ����������, �� ������� ����� ������
   //              ^^^^  - ��������� �� ������� �����
   WITH OBJECT oBrw

      :lNoKeyChar    := .F.          // ��� ����� � ������ �� ����, ����
      :nHeightCell   := nHImg        // ������ ����� = ������ �������� - ����� ��� ��������
      :nHeightHead   := nHImg * 1.2  // ������ �����
      //:nHeightFoot   := nHImg + 4  // ������ �������
      :nHeightSpecHd := 12           // ������ ���������� ENUMERATOR
      :lFooting      := .T.          // ������������ ������
      :lDrawFooters  := .T.          // ��������  �������
      :nFreeze       := 3            // ���������� �������
      :nCell         := :nFreeze + 1
      :lLockFreeze   := .T.          // �������� ���������� ������� �� ������������ ��������
      :nCellMarginLR :=  1           // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
      :nMemoHV       :=  1           // ����� 2 ������ ����-����

      :Cargo:nModify := 0     // ��������� � �������

   END WITH

   oCol := oBrw:GetColumn("FNAME")
   oCol:bDecode := {|val| ALLTRIM(val) }
   oCol:nAlign  := DT_RIGHT

   oCol := oBrw:GetColumn("FTYPE")
   oCol:bDecode := {|val| ALLTRIM(val) }
   oCol:nAlign  := DT_CENTER

   oCol := oBrw:GetColumn("FLEN")
   oCol:bDecode := {|val| HB_NtoS(val) }
   oCol:nAlign  := DT_CENTER

   oCol := oBrw:GetColumn("FDEC")
   oCol:bDecode := {|val| HB_NtoS(val) }
   oCol:nAlign  := DT_CENTER

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont //, nI, oCol

   hFont := oBrw:aColumns[1]:hFontSpcHd  // 4-special header font
   // ���������� ���� ��� 1 ������� �������
   oBrw:aColumns[1]:hFont     := hFont     // 1-cells font
   oBrw:aColumns[1]:hFontFoot := hFont     // 3-footer font

   // ����� ��� ������� 3-4 �������, ��������� �� ����
   /*For nI := 2 To oBrw:nColCount()
      oCol       := oBrw:aColumns[ nI ]
      oCol:hFont := {|nr,nc,ob| // ����� ��� ����� �������
                      Local nGet, xv
                      nGet := ob:GetValue("PRIXOD") // ������� �����
                      xv   := ob:GetValue(nc)
                      //? "**** ob:aColumns["+HB_NtoS(nc)+"]", nr, nc, xv, nGet
                      //!!! nr := ob:aColumns[ nc ]:hFont   // GetFontHandle( "Normal" )
                      nr := ob:hFont
                      IF nGet < 0   // ��������� �����
                         nr := oBrw:aColumns[nc]:hFontHead
                      ENDIF
                      //IF "---" $ cval
                      //    nr := ob:Cargo:hTsbBold4 // GetFontHandle( "Bold" )
                      //ENDIF
                      Return nr
                      }
   Next */

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw, oTsb )
   LOCAL hFont, nHFont, aSupHd, cSprHd, nClr16, nClr17, nEnd, O

   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )
   aSupHd := oTsb:aSupHd
   O      := oBrw:Cargo
   cSprHd := oTsb:cTtlSupHead
   nClr16 := oTsb:aClr16
   nClr17 := oTsb:aClr17

   WITH OBJECT oBrw  // ���-1 ��. ���� // ���-2
      nEnd := :nColCount() //+ IIF( :lSelector, 1, 0 ) // ������ ��� !
      // ������ ���������� � ������� �������� 0
      :AddSuperHead( 1, nEnd, "Super_Header_Table" ) //,,, .F.,,, .F., .F., .F., 0, )
      :aSuperhead[ 1, 3 ] := cSprHd
      :nHeightSuper := nHFont * 1.5    // 1 ������
      // ������ ����� �����������
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, ���� ���������
      :SetColor( { 17 }, { nClr17           } ) // 17, ������ ���������
      // ������ �������
      //nHAlign := :nAlignSupHdGet(nCol)   // ��� �����������
      //:nAlignSupHdGet(nCol, nHAlign)
      //:aSuperHead[1,12] := DT_LEFT       // �������� ������� ������
   END WIDTH

   o:TitleSupHd := oBrw:aSuperhead[ 1, 3 ]    // ���������
   o:ColorSupHd := nClr16                     // 16, ���� ���������

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// ������ ��������� ���
STATIC FUNCTION myTsbKeyFX( oBrw, oTsb )
   LOCAL o := oBrw:Cargo      // ������������ �� ���������� ���� ����������
   LOCAL nHHead := oTsb:nHeightHead   // ������ ����� - � �������� �������

   WITH OBJECT oBrw
      // ��������� ������
      /*
      :UserKeys(VK_SPACE, {|ob|
                           Local lRet := .T., lval, cval
                           ob:Cargo:nModify ++  // ���� ����������� �������
                           IF ob:nCell == 2
                              lval := ob:GetValue( ob:nCell )
                              cval := ob:GetValue( ob:nCell + 1 )
                              IF ! "---" $ cval
                                 ob:SetValue( ob:nCell, ! lval )
                                 ob:DrawSelect()
                                 DO EVENTS
                                 lRet := .F.
                              ENDIF
                           ENDIF
                           Return lRet
                           })
      :UserKeys(VK_RETURN, {|ob|
                            Local lRet := .T.
                            ob:Cargo:nModify ++  // ���� ����������� �������
                            IF ob:nCell == 2
                               DO EVENTS
                               ob:PostMsg( WM_KEYDOWN, VK_SPACE, 0 )
                               lRet := .F.
                            ENDIF
                            Return lRet
                            })

      // ������� � ������������� ���������
      // �.�. ������� 2 ��� �� CheckBox, ��������� ����������, �� ��� ������ ���.��������
      //  �� ����� �� ������� oBrw:aMsg, ��� �������� �������� {"��", "���" ...}
      IF hb_IsArray( :aMsg ) .and. Len( :aMsg ) > 1
         :aMsg[1] := ""
         :aMsg[2] := ""
      ENDIF
      */

      :UserKeys(VK_RETURN, {|ob| ob:Cargo:nModify ++, _wSend(90, ob:cParentWnd) } )
      // ��������� �����
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :SetAppendMode( .F. )            // ��������� ������� ������ � ����� ���� �������� ����
      :SetDeleteMode( .F. )
      // ��������� ������� ESC � ������
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      //:UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      //:UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })
      // ������� FXX
      :UserKeys(VK_F4    , {|ob| myTsbListColumn( ob ) , ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F5    , {|ob| myTsbListFont( ob )   , ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F8    , {|ob| myTsbSelectorNew(ob)  , ob:Setfocus() })  //
      :UserKeys(VK_F9    , {|ob| myTsbSelectorOld(ob)  , ob:Setfocus() })  //

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol

   FOR EACH oCol IN oBrw:aColumns
      IF oCol:cName == "SELECTOR" .OR. oCol:cName == "ORDKEYNO"  ; LOOP
      ENDIF
      IF oCol:cFieldTyp $ "+=@T"
         oCol:lEdit := .F.
      ENDIF
      //IF "NAME" $ oCol:cName
      //   oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  //
      //   oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  //
      //ENDIF
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ����-���� ������� ������ ����� END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, nI, cCol, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol
   LOCAL cBrw, nTsb, nW1Col, nWEnd, nWTsb, aCol, nWSum

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // ���� ���� ���������� �������
   a4Clr  := oTsb:a4Clr                     // ������� 4 ����� �������
   nTest  := HMG_RGB2n(a4Clr[1])            // ���� ���� ����

   //? ProcNL(), MGVersNumba()
   //? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   // ������� ������ - 1
   // ������ ������ ���������� ������� �� ���� ����, ����� SELECTOR
   oBrw:lClrSelectorHdBack := .T. // background OFF
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      oCol:nClrSpcHdFore := CLR_RED
      oCol:nClrSpcHdBack := nBCSpH
   NEXT

   // ������� ������ - 2
   // ������� ���� ������� - SELECTOR
   /*oBrw:lClrSelectorHdBack := .F. // background OFF
   oCol := oBrw:GetColumn("SELECTOR")
   oCol:nClrBack      := nBCSpH
   oCol:nClrFore      := CLR_RED
   oCol:nClrFootBack  := nBCSpH
   oCol:nClrSpcHdBack := nBCSpH
   oCol:SaveColor()                       // ��������� ����� �������
   oBrw:nClrSelectorHdBack := nBCSpH      // Footer ��� "SELECTOR"
   */
   // ������� ������ - 3
   oCol := oBrw:GetColumn("ORDKEYNO")
   oCol:nClrBack      := nBCSpH
   oCol:nClrFore      := CLR_RED
   //oCol:nClrFootBack  := nBCSpH
   oCol:nClrFootFore  := CLR_WHITE
   oCol:SaveColor()                      // ��������� ����� �������

   // ��������� ������ ����������� �������
   nLen   := LEN(HB_NtoS(oBrw:nLen))
   nCol   := oBrw:nColumn("ORDKEYNO", .T.)
   nW1Col := 0
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // ���-�� ������ + 2 �����
      oCol:nWidth := nWCol                                       // ����� ������
      nW1Col := nWCol
      // ���������� ����
      hFont := oBrw:aColumns[nCol]:hFontSpcHd  // 4-special header font
      oBrw:aColumns[nCol]:hFont     := hFont   // 1-cells font
      oBrw:aColumns[nCol]:hFontFoot := hFont   // 3-footer font
   ENDIF

   IF oBrw:lDrawSuperHd  // ���-2
      // ��������� ���������� �� ����� �������
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF
   oBrw:DrawHeaders()   // ���������� ����������/�����/���������

   cBrw := oBrw:cControlName
   nTsb := This.&(cBrw).ClientWidth
   nLen := oBrw:GetAllColsWidth() - 1
   IF nLen > nTsb
      //oBrw:lAdjColumn  := .T.
      //oBrw:lNoHScroll  := .F.
      //oBrw:lMoreFields := ( oBrw:nColCount() > 45 )
   ELSE
      //oBrw:AdjColumns()
   ENDIF

   // ������ �������� ������ �������
   aCol  := {"FNAME","FTYPE","FLEN","FDEC"}
   nWTsb := oBrw:GetAllColsWidth() - 1    // ��� 100%
   nWCol := ( nWTsb - GetVScrollBarWidth() - nW1Col - 20 ) / 5

   nWSum := nW1Col
   FOR nI := 1 TO LEN(aCol)
      oCol := oBrw:GetColumn(aCol[nI])
      oCol:nWidth := nWCol
      nWSum += oCol:nWidth
   NEXT

   // ������ ��������� �������
   nWEnd := nWTsb - nWSum - GetVScrollBarWidth()
   oCol  := oBrw:GetColumn("FREM")
   oCol:nWidth := nWEnd

   oBrw:AdjColumns({"FREM"})
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL


////////////////////////////////////////////////////////////////////////////
FUNCTION TypeFieldRem(cVal)
   LOCAL nI, aType := {} , cRet := "Unknown"

   aAdd( aType, {"CICHARACTER", "C"  , "CiCharacter"                } )
   aAdd( aType, {"C",           "C"  , "Character"                  } )
   aAdd( aType, {"C:U",         "C"  , "nChar"                      } )
   aAdd( aType, {"C:B",         "C"  , "Raw"                        } )
   aAdd( aType, {"Q",           "N"  , "VarCharFox"                 } )
   aAdd( aType, {"Q:U",         "N"  , "nVarChar"                   } )
   aAdd( aType, {"Q:B",         "N"  , "VarBinaryFox"               } )
   aAdd( aType, {"D",           "D"  , "Date"                       } )
   aAdd( aType, {"T",           "T"  , "Time"                       } )
   aAdd( aType, {"@",           "T"  , "TimeStamp"                  } )
   aAdd( aType, {"=",           "T"  , "ModTime"                    } )
   aAdd( aType, {"I",           "N"  , "Integer, ShortInt, LongInt" } )
   aAdd( aType, {"B",           "N"  , "Double"                     } )
   aAdd( aType, {"+",           "N"  , "Autoinc"                    } )
   aAdd( aType, {"N",           "N"  , "Numeric"                    } )
   aAdd( aType, {"Y",           "N"  , "Money"                      } )
   aAdd( aType, {"Z",           "N"  , "Curdouble"                  } )
   aAdd( aType, {"^",           "N"  , "RowVersion"                 } )
   aAdd( aType, {"M",           "M"  , "Memo"                       } )
   aAdd( aType, {"M:U",         "M"  , "nMemo"                      } )
   aAdd( aType, {"W",           "M"  , "Binary"                     } )
   aAdd( aType, {"P",           "M"  , "Image"                      } )
   aAdd( aType, {"L",           "L"  , "Logik"                      } )
   aAdd( aType, {"V",           "V"  , "Variable - Six3"            } )

   FOR nI := 1 TO LEN(aType)
       IF aType[nI,1] == cVal
          cRet := aType[nI,3]
          EXIT
       ENDIF
   NEXT

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MG_YesNo2Get()
   Local cMsg, cTtl, lRet, aTmp, aBClr, aFClr, aBtnClr, bOnInit, aRet, ao
   LOCAL cRus, cEng, nLine, nColumn, cMsg1, cMsg2, cMsg3, cFont, nFSize

   aRet    := {}  // �������
   ao      := App.Cargo
   cRus    := ";������� �� ������� � ������ � �������;"
   cEng    := ";Move to column and row in table;"
   cMsg    := IIF( ao:cLang == "RU", cRus, cEng )
   cTtl    := IIF( ao:cLang == "RU", "�������������", "Confirmation" )
   cMsg    += REPL("-.", 10)
   cMsg    += ";;;;;"  // ����������� !!!
   aBClr   := ao:aDlgBColor
   cFont   := ao:cFontName2
   nFSize  := ao:nDlgSize
   aFClr   := BLACK
   aBtnClr := { LGREEN , RED }
   cMsg1   := IIF( ao:cLang == "RU", "����� �������:", "Column number:" )
   cMsg2   := IIF( ao:cLang == "RU", "����� ������:" , "Line number:"   )
   cMsg3   := IIF( ao:cLang == "RU", "�������� ! �������� = 0 ������������ !" ,;
                                     "Attention ! Values = 0 are ignored !"   )
   nColumn := nLine := 0

   aTmp    := _SetMsgAlertColors(aBClr,aFClr)  // ����� �����

   bOnInit := {|| // ���� ��������� ����
                  Local ow := ThisWindow.Object
                  Local i, y, x, h, w, g, y1, x1, x2, aObj, cForm
                  Local cMsg, aBColor, cObj, w2, nW1
                  h  := This.Btn_01.Handle
                  y  := This.Btn_01.Row + 60
                  x  := This.Btn_01.Col + 30
                  y1 := GetWindowRow(ow:Handle)
                  x1 := GetWindowCol(ow:Handle)
                  g  := 10
                  //? ProcNL(), This.Name, ow:Name, "Btn_01.Handle: h, y, x:", h, y, x
                  //? ow:Name, ow:Handle, "Row win:", y1, "Col win:", x1
                  //This.Btn_01.Action      := {|| DoEvents(), _wPost(0, This.Index) }
                  This.Btn_01.Fontcolor   := YELLOW
                  This.Btn_02.Fontcolor   := YELLOW
                  This.Btn_02.SetFocus
                  DoEvents()

                  aObj := HMG_GetFormControls(ow:Name) // ��� �������
                  ? ProcNL(), ow:Name, "aObj=", aObj ; ?v aObj
                  y := This.Say_01.Row
                  x := This.Say_01.Col
                  w := This.Say_01.Width
                  h := This.Say_01.Height
                  //? y, x, w, h
                  cForm   := ow:Name
                  aBColor := GetProperty( cForm, "Backcolor" )
                  For i :=  1 TO Len(aObj)
                     If "Say_" $ aObj[i]
                       cObj := aObj[i]
                       cMsg := GetProperty( cForm, cObj, "Value" )
                       If "-.-." $ cMsg
                          y := GetProperty( cForm, cObj, "Row" )
                          SetProperty( cForm, cObj, "Value", "" )
                          EXIT
                       Endif
                     Endif
                  Next
                  y += This.&(cObj).Height
                  ? "-.-.  y=", y, "cObj=", cObj

                  nW1 := GetTxtWidth( cMsg1, nFSize, cFont, .F. ) + 10

                  @ y, x LABEL Lbl_Msg1 VALUE cMsg1 FONT cFont SIZE nFSize WIDTH nW1 HEIGHT h ;
                    VCENTERALIGN RIGHTALIGN FONTCOLOR YELLOW TRANSPARENT

                  x2 := x + This.Lbl_Msg1.Width + 10
                  w2 := w - nW1 - x2 + 50

                  @ y, x2 GETBOX GB_NumCol VALUE nColumn WIDTH w2 HEIGHT h ;
                    PICTURE REPL('9',10) ON CHANGE {|| nColumn := This.Value }

                  y  += This.Lbl_Msg1.Height + 15

                  @ y, x LABEL Lbl_Msg2 VALUE cMsg2 FONT cFont SIZE nFSize WIDTH nW1 HEIGHT h ;
                    VCENTERALIGN RIGHTALIGN FONTCOLOR YELLOW TRANSPARENT

                  @ y, x2 GETBOX GB_Line VALUE nLine WIDTH w2 HEIGHT h ;
                    PICTURE REPL('9',10) ON CHANGE {|| nLine := This.Value }

                  y  += This.Lbl_Msg2.Height + 20

                  @ y, x LABEL Lbl_Msg3 VALUE cMsg3 FONT cFont SIZE nFSize WIDTH w HEIGHT h ;
                    VCENTERALIGN CENTERALIGN  FONTCOLOR WHITE TRANSPARENT

                  This.GB_NumCol.Setfocus

                  DO EVENTS

                  Return Nil
               }

   // ------------ alerts.prg ---------
   lRet  := AlertYesNo( cMsg, cTtl, ,"iGoto64x1", 64, aBtnClr, .T. , bOnInit, .F. )
   IF lRet
      IF nColumn > 0 .AND. nLine > 0
         aRet := { nLine, nColumn }
      ENDIF
   ELSE
      aRet := {}   // {} - �����
   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])       // ������������ �����

RETURN aRet
