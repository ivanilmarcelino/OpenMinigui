/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ���� � �������� / Window with table
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
#include "hbsqlit3.ch"
#include "dbinfo.ch"

///////////////////////////////////////////////////////////////////
FUNCTION Tsb_ViewSqlite(oWnd, nPos, cWIco, cTable, lWin1)
   LOCAL cForm, hForm, aBColor, cTitle, cMsg, cSelect
   LOCAL oBrw, cBrw, oTsb, nY, nX, nH, nW, nG, o, owc, ao
   LOCAL nH1, nW2, a4Clr, nDelta, cFont, nFSize
   LOCAL nTotalRecno, cAlias
   DEFAULT lWin1 := .T.

   ? ProcNL(), oWnd, nPos, cWIco, cTable, lWin1

   ao       := App.Cargo
   nY       := ao:nHMain                // ������ ���� ������� �����
   nX       := 0  ; nG := 20
   nW       := ao:aDisplayMode[1]       // ������� ���� ���������� � 0main.prg
   nH       := ao:aDisplayMode[2] - nY  // ������� ���� ���������� � 0main.prg
   nG       := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   //nW     := Sys.ClientWidth
   //nH     := Sys.ClientHeight - nY
   cTitle   := HB_NtoS(nPos) + ":" + cTable
   cForm    := "Form_Tsb" + HB_NtoS(nPos)
   a4Clr    := App.Cargo:a4Clr             // ����� ��� ���� �������
   aBColor  := a4Clr[1]
   cBrw     := "Tsb_" + HB_NtoS(nPos)
   cFont    := ao:cFontName2
   nFSize   := ao:nDlgSize
   nDelta   := 40

   IF !lWin1   // ��� ���� ����
      nY += (nPos - 1) * nDelta
      nX += (nPos - 1) * nDelta
   ENDIF

   If MGVersNumba() == 231206
   Elseif MGVersNumba() > 240700
   Else
      cMsg := IIF( ao:cLang == "RU", "������ ������ �� !;", "ERROR DISPLAYING DB !;" )
      cMsg += IIF( ao:cLang == "RU", "���������� ������ �� ������ ������� 24.08 � ����;",;
                          "Compilation only on MiniGui version 24.08 and higher;" )
      cMsg += IIF( ao:cLang == "RU", '��������� ����� "������"...', 'The program may "crash"...' )
      cMsg += ";;" + MiniGuiVersion() + ";;" + ProcNL()
      AlertStop( cMsg , , "ZZZ_B_STOP64", 64 )
   Endif

   IF _IsWindowDefined(cForm)
      hForm := GetFormHandle(cForm)
      IF hForm != 0
         IF IsIconic( hForm ) ; _Restore( hForm )
         ENDIF
         DoMethod(cForm, "SetFocus")
      ENDIF
      RETURN "" // �����, ��� ����� ��� �������
   ENDIF

   // ������ ��������� ���� �������
   AADD( App.Cargo:aWinOpen, cForm )

   //cTable := "Country Official Languages"
   IF AT(" ", cTable ) > 0
      cAlias := ATREPL( " ", cTable, "_" )
      cTable := "'" + cTable + "'"
   ELSE
      cAlias := cTable
   ENDIF
   cSelect := "SELECT * FROM " + cTable
   //dbUseArea( TRUE,, cSelect, cTable,,, "UTF8" )
   dbUseArea( TRUE,, cSelect, cAlias,,, "UTF8" )

   cMsg := IIF( ao:cLang == "RU", "�������� �������: ", "Open the table:" ) + cTable
   WaitWindow( {cMsg,oWnd:Cargo:cFile}, .T., 800, 13, NIL, BLACK, App.Cargo:aDlgBColor, 14, BLUE, 4 )
   nTotalRecno := SQLITE_RECCOUNT(cTable, oWnd:Cargo:cFile)
   WaitWindow()

   ? Alias(), "cTable=", cTable, cSelect, RddName(), "nTotalRecno=", nTotalRecno
   ? oWnd:Cargo:cFile

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE cTitle ICON cWIco                        ;
      MINWIDTH 500 MINHEIGHT 500                     ; // ���������� ���������� �������� ����
      WINDOWTYPE STANDARD TOPMOST                    ;
      ON MAXIMIZE ( ResizeForm( This.Object ) )      ;
      ON SIZE     ( ResizeForm( This.Object ) )      ;
      BACKCOLOR aBColor                              ;
      FONT cFont SIZE nFSize                         ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor  := This.BackColor      // ���� ����
      owc:oMainCrg := oWnd:Cargo          // Cargo ������������ ����
      owc:nG       := nG
      owc:cAls     := ALIAS()             // ����� � ���� ����
      owc:ahIcoDel := {}                  // ��� �������� ������� ������ � �����
      owc:cTable   := cTable
      owc:cFile    := oWnd:Cargo:cFile    // ���� � �����
      owc:cRequest := cSelect
      owc:aIcoFltr := { "iAFilter32x1", "iAFilter32x3" }    // ����� ������             - ������
      owc:aMn2FClr := { WHITE, RED    }                     // ����� ����� ����� ������ - ������
      owc:aMn2BClr := { owc:aBColor, YELLOW }               // ����� ����� ���� ������  - ������
      owc:nFilter  := 1                                     //                          - ������
      // ������� ���� ���� � ��������
      TopMenuViewButtons(owc)          // -> tsb_ViewMenu.prg
      //owc:aMn2Text                   // ����� ���� { "������" , "�������" } -> tsb_ViewMenu.prg

      nY  := owc:nHTBar //+ nG
      nX  := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nH1 := 5
      nW2 := owc:nWEndTB    // ����� ������

      owc:cRus  := "F2-����,   Ins-����� ������, Del-�������/������������ ������"
      owc:cEng  := "F2-info,   Ins-new recno, Del-delete/restore recno"
      owc:cMsg1 := IIF( ao:cLang == "RU", owc:cRus, owc:cEng)
      @ nH1, nW2 + nG LABEL Lbl_1 VALUE owc:cMsg1 AUTOSIZE FONTCOLOR WHITE TRANSPARENT
      nH1 += This.Lbl_1.Height + 1

      owc:cRus  := "" //F9-������� ������"
      owc:cEng  := "" //F9-Delete list"
      owc:cMsg2 := IIF( ao:cLang == "RU", owc:cRus, owc:cEng)
      @ nH1, nW2 + nG LABEL Lbl_2 VALUE owc:cMsg2 AUTOSIZE FONTCOLOR WHITE TRANSPARENT

      This.MinWidth  := owc:nWEndTB + nG + GetBorderWidth()*2  // ���������� ���������� �������� ����
      //This.MinHeight := owc:nHBtnEnd + GetBorderHeight()*2   // ���������� ���������� �������� ����

      /////////////////////// ������� ///////////////////////////////////////////////////
      oTsb := oHmgData()
      oTsb:cForm  := cForm
      oTsb:cAls   := ALIAS()
      oTsb:nTotal := nTotalRecno   // ����� ������� � �������
      // ���������� �������
      oTsb:nY     := nY
      oTsb:nX     := nG
      oTsb:nW     := nW - oTsb:nX * 2
      oTsb:nH     := nH - oTsb:nY - nG

      @ oTsb:nY, oTsb:nX LABEL Label_Table PARENT &cForm WIDTH oTsb:nW HEIGHT oTsb:nH ;
        VALUE '' SIZE 20 CENTERALIGN BACKCOLOR WHITE INVISIBLE
      owc:cLabel := 'Label_Table'

      oBrw := Draw_Table( oTsb, cBrw, oWnd, owc )         // �������
      IF IsObject(oBrw)
         oBrw:Cargo:owc := owc                  // ��������� �� �������
         owc:oBrw       := oBrw                 // ��������� �� ����
         owc:cBrw       := cBrw                 // ��������� �� ����
         //_o2log(owc , 15, ProcNL()+" -------------- ��������� ������� : => owc", .T.)
         //_o2log(oTsb, 15, ProcNL()+" ��������� ������� : => oTsb", .T.)
         //ON KEY ESCAPE ACTION ( iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) ) ) - ���� ����
         owc:lSayTable := .T.             // ��� �������
      ELSE
         IF IsString(oBrw) ;  cMsg := oBrw
         ENDIF             ;  cMsg := 'Table'
         This.Lbl_2.Value       := ""
         This.Label_Table.Value := cMsg
         This.Label_Table.Show
         owc:lSayTable := .F.             // ��� �� �������
      ENDIF

      ON KEY F1     ACTION NIL

      o := This.Object
      o:Event( 0, {|ow| // ������ ����� ���������� ����
                        This.Topmost := .F.
                        ? ProcNL(),">>> Start window: "+ow:Name
                        IF ! owc:lSayTable          // ��� �� �������
                           This.&("_ATable").Enabled  := .F.
                           This.&("_AExport").Enabled := .F.
                        ELSE
                           ow:Cargo:oBrw:SetFocus()
                        ENDIF
                        DO EVENTS
                        Return Nil
                        })
      // ��� ������� + ��� �������    aObj  := { "_ATable" , "_AExport", "_AExit"  }
      //            VVVV
      o:Event({10,"_ATable" }, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                              _SetThisFormInfo(ow)       ,;
                                              myTableStruct(ow,ky,cn,ob) ,;  // -> tsb_ViewMenu.prg
                                              _SetThisFormInfo()         ,;
                                              This.&(cn).Enabled := .T.  ,;
                                              ky:=cn , ob:Setfocus()  } )

      o:Event({11,"_AFilter"}, {|ow,ky,cn,ob| // ������ �� �������
                                             Local cFltr, owc := ow:Cargo
                                             Local cCapt, cIco, aFClr, aBClr
                                             This.&(cn).Enabled := .F.
                                             ob    := ow:Cargo:oBrw
                                             cIco  := owc:aIcoFltr[1]
                                             cCapt := owc:aMn2Text[1]
                                             aFClr := owc:aMn2FClr[1]
                                             aBClr := owc:aMn2BClr[1]
                                             ? "  ###", ProcNL(), ky,cn, ob:cAlias, "!!! ������ !!!"
                                             IF owc:nFilter == 1               // ������� ������
                                                _SetThisFormInfo(ow)
                                                Form4Filter(ow,ky,cn,ob)       // -> tsb_ViewFilter.prg - ����� �������
                                                _SetThisFormInfo()
                                                DbSelectArea(ob:cAlias)        // �� ������ ������
                                                cFltr := App.Cargo:cRetFilter  // ������� ������ �������
                                                If LEN(cFltr) > 0
                                                   ob:FilterData( cFltr )      // ��������� ������� �� ����
                                                   mySuperHdFilter(ob, cFltr)  // ����� ������� � �����������
                                                   owc:nFilter := 2
                                                   cIco  := owc:aIcoFltr[2]
                                                   cCapt := owc:aMn2Text[2]
                                                   aFClr := owc:aMn2FClr[2]
                                                   aBClr := owc:aMn2BClr[2]
                                                Endif
                                             Else                               // ������� ��������
                                                ob:FilterData()                 // ������� ������� �� ����
                                                mySuperHdFilter(ob, "")         // ����� ������� � �����������
                                                owc:nFilter := 1
                                             Endif
                                             This.&(cn).Enabled   := .T.
                                             This.&(cn).Caption   := cCapt
                                             This.&(cn).Fontcolor := aFClr
                                             This.&(cn).Backcolor := aBClr
                                             This.&(cn).Icon      := cIco
                                             ob:SetFocus()
                                             DO EVENTS
                                             Return Nil
                                             } )

      o:Event({12,"_AExport"}, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                             _SetThisFormInfo(ow)         ,;
                                             TableToExport(ow,ky,cn,ob)   ,;     // -> tsb_export.prg
                                             _SetThisFormInfo()           ,;
                                             This.&(cn).Enabled := .T.    ,;
                                             ob:Setfocus()  } )

      o:Event({89,"_AExit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

      o:Event(90, {|ow,ky,ah,i| // ON Release
                              ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                              ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                              IF LEN(ow:Cargo:cAls) > 0
                                 (ow:Cargo:cAls)->( dbCloseArea() )    // ����������� !!!
                              ENDIF
                              ah := ow:Cargo:ahIcoDel
                              ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                              ?? ah, HB_ValToExp(ah)
                              IF IsArray(ah)
                                 AEval(ah, {|h| DestroyIcon(h) })  // ������� ������ ������
                              Endif
                              // ������� �������� ���� ������� �� App.Cargo:aWinOpen
                              If Len(App.Cargo:aWinOpen) > 0
                                 ? Repl(".", 10),"App.Cargo:aWinOpen =" + HB_ValtoExp(App.Cargo:aWinOpen)
                                 For i := 1 TO Len(App.Cargo:aWinOpen)
                                     If UPPER(App.Cargo:aWinOpen[i]) == UPPER(ow:Name)
                                        ? Repl(".", 10),"Delete " + ow:Name + " from App.Cargo:aWinOpen"
                                        hb_ADel(App.Cargo:aWinOpen, i, .T.)
                                     Endif
                                 Next
                              Endif
                              DO EVENTS
                              Return Nil
                              })

      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

   //CENTER WINDOW &cForm
   IF lWin1   // ��� ������ ����
      ACTIVATE WINDOW &cForm
   ENDIF

RETURN cForm

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm( oWnd )
   LOCAL nG, owc, nTsbY, nTsbX, cBrw, nH, nW, nHTBar, oBrw, cObj
   DEFAULT oWnd := _WindowObj( GetActiveWindow() )

   nW     := This.ClientWidth
   nH     := This.ClientHeight
   owc    := oWnd:Cargo
   nG     := owc:nG
   nHTBar := owc:nHTBar      // ����� ������ �� ������
   oBrw   := oWnd:Cargo:oBrw   // ������� � ����

   ? ProcNL(), oBrw, oBrw:cAlias, oBrw:cControlName
   IF ISOBJECT(oBrw)
      // ������ Tbrowse ��������
      nTsbY  := owc:nTsbY
      nTsbX  := owc:nTsbX
      cBrw   := owc:cBrw

      //cBrw   := oBrw:cControlName
      This.&(cBrw).Enabled := .F. // ����������� ������� ������� (������ �� ������������)

      // �� ������ Move() ����������� ReSize() - �������� ���������� ��. TControl.prg
      oBrw:Move( oBrw:nLeft, oBrw:nTop, This.ClientWidth - oBrw:nLeft - nG, This.ClientHeight - oBrw:nTop - nG, .T. )

      This.&(cBrw).Enabled := .T. // �������������� ������� ������� (������ ������������)

      oBrw:Paint()
      oBrw:Refresh(.T.)
      oBrw:SetNoHoles()
      oBrw:SetFocus()

   ELSE
      // ������ Label ��������
      cObj := owc:cLabel
      This.&(cObj).Width  := nW - nG*2
      This.&(cObj).Height := nH - nG - nHTBar
   ENDIF

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION mySuperHdFilter(oBrw, cFilter)   // ����� ������� � �����������
   LOCAL cText, nSupHd1, nSupHd2, aClr16, nClr17
   DEFAULT cFilter := ""

   // �� ��� ����� ���������
   cText   := oBrw:Cargo:TitleSupHd
   aClr16  := oBrw:Cargo:Clr16SupHd
   nClr17  := oBrw:Cargo:Clr17SupHd

   IF LEN(cFilter) > 0
      //oBrw:aSuperHead[2,3] := cText1    // �������� ����������
      oBrw:aSuperHead[1,3] := "  FILTER: " + cFilter
      nSupHd1 := CLR_YELLOW
      nSupHd2 := CLR_RED
      aClr16 := { nSupHd1 , nSupHd2 }
      nClr17  := CLR_BLACK
   ELSE
      oBrw:aSuperHead[1,3] := cText
   ENDIF
   // ������ ����� �����������
   oBrw:SetColor( {16}, { aClr16  } )   // 16, ���� �����������
   oBrw:SetColor( {17}, { nClr17  } )   // 17, ������ �����������

   oBrw:DrawHeaders()                  // ���������� ����������/�����/���������
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_Table( oTsb, cBrw, oWnd, owc )
   LOCAL oBrw, cForm, a4Clr, cSuperHd, nG

   nG               := owc:nG
   cSuperHd         := "FILE: " + cFileNoPath(oWnd:Cargo:cFile)
   cSuperHd         += "   TABLE: " + owc:cTable
   cSuperHd         += "   SCHEME: " + owc:cRequest
   //                      cell     Head    foot     SpecHider   SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }
   oTsb:uSelector   := 20
   oTsb:lSpecHd     := .T.    // ��������� � ������� ���������
   oTsb:lFooting    := .T.    // ��������� � ������� ������
   oTsb:aFoot       := .T.
   oTsb:aEdit       := .T.    // ������������� �������
   a4Clr            := App.Cargo:a4Clr                // ����� ��� ���� �������
   oTsb:a4Clr       := a4Clr                          // �������� 4 ����� �������
   oTsb:aBrush      := a4Clr[3]                       // ���� ���� ��� ��������
   oTsb:aColor      := Color_Tsb(a4Clr,oTsb)          // ����� �������: 2(�����+������),3(������ %1),4(������ %2)
   oTsb:cTtlSupHead := cSuperHd
   cForm            := oTsb:cForm                     // ���������� ����
   oTsb:aNumber     := { 1, 60 }

   // ������ ������� �� �������� ��������
   DEFINE TBROWSE &cBrw OBJ oBrw OF &cForm ALIAS oTsb:cAls     ;
      AT oTsb:nY, oTsb:nX  WIDTH oTsb:nW HEIGHT oTsb:nH CELL   ;
      FONT   oTsb:aFont                                        ; // ��� ����� ��� �������
      COLORS oTsb:aColor                                       ; // ����� �������
      BRUSH  oTsb:aClrBrush                                    ; // ���� ���� ��� ��������
      COLNUMBER oTsb:aNumber                                   ; // ����� ������� ����������� ������� � ����������
      ENUMERATOR                                               ; // ��������� �������
      EDITABLE                                                 ; // ������������� �������
      SELECTOR .T.                                             ; // ������ ������� - ��������
      AUTOCOLS                                                 ; // ���� ������ �������� ������� �� width
      ON CHANGE oBrw:Refresh(.f.,.f.)                          ; // ������ ���������� ����� � �������
      ON INIT  {|ob| ob:Cargo := oHmgData(), ;
                 ob:lNoChangeOrd  := .T., ;     // ��������� ����������
                 ob:nColOrder     :=  0 , ;     // ������ ������ ���������� �� �������
                 ob:lNoGrayBar    := .F., ;     // T-�� ���������� ���������� ������ � �������
                 ob:lNoLiteBar    := .F., ;     // ��� ������������ ������ �� ������ ���� �� ������� "������" Bar
                 ob:lNoResetPos   := .F., ;     // ������������� ����� ������� ������ �� gotfocus
                 ob:lPickerMode   := .F., ;     // ������ ���� ���������� ����� �����
                 ob:nStatusItem   :=  0 , ;     // � 1-� Item StatusBar �� �������� ��������� �� ���
                 ob:lNoKeyChar    := .T., ;     // .T. - ����. ����� KeyChar(...) - ���� �� ����, ����
                 ob:nWheelLines   :=  1 , ;     // ��������� ������� ����
                 ob:nCellMarginLR :=  1 , ;     // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
                 oB:aEditCellAdjust[1] := -3,; // correction of cell row
                 ob:lMoveCols     := .F., ;
                 ob:nMemoHV       :=  1 , ;     // ����� ����� ������ ����-����
                 ob:nLineStyle := LINES_ALL ,;
                 ob:nClrLine   := COLOR_GRID,;
                 ob:lCheckBoxAllReturn := .T. }

      :Cargo:nModify := 0                           // ��������� � �������
      :Cargo:aFont   := oTsb:aFont                  // �������� �����
      :Cargo:aSupHd  := oTsb:aSupHd                 // ���������� �������
      :Cargo:aIconDel:= oTsb:aIconDel               // ������� ��������
      :Cargo:lRecINS := .F.                         // ���������� ������� INS
      :Cargo:lRecDEL := .F.                         // ���������� ������� DEL
      :Cargo:aStruct := {}                          // ��������� ���� ��� ��������
      :Cargo:cTable  := owc:cTable                  // ��� �������
      :Cargo:cFile   := owc:cFile                   // ���� � �����
      :Cargo:cCdPg   := oWnd:Cargo:cCdPg            // ������� �� ��� CodePage �����
      :Cargo:nTotalRecno := oTsb:nTotal             // ��������� �� ������� - ����� �������

      myTsbInit(oBrw,oTsb)            // ��������� �������
      myTsbFont(oBrw,oTsb)            // ����� � �������
      myTsbEnum(oBrw)                 // ENUMERATOR �� �������
      myTsbSuperHd(oBrw,oTsb)         // SuperHeader
      myTsbKeyFX(oBrw,oTsb)           // ��������� ������
      myTsbEdit(oBrw,oTsb)            // ��������� ��������������

   END TBROWSE
   //END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }   // ������ ����� ����� ������� - ��� ��� array � dbf

   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   myTsbEnd(oBrw,oTsb)             // ��������� �������� � ���

RETURN oBrw

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
   //AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_GRAY, CLR_BLACK ) } } ) // 1
   AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK             } } )      // 1 , ������ � ������� �������
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), nPane3 ,;
                                            iif( ob:nAt % 2 == 0, nPane2, nPane ) )   } } )    // 2 , ���� � ������� �������
   oTsb:aClr1  := CLR_BLACK
   oTsb:aClr16 := { nHead1, nHead2 }
   oTsb:aClr17 := CLR_WHITE

   AAdd( aColors, { CLR_HEADF , {|| CLR_YELLOW            } } )        // 3 , ������ ����� �������
   AAdd( aColors, { CLR_HEADB , {|| { nHead2, nHead1 }    } } )        // 4 , ���� ����� �������
   AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLUE              } } )        // 5 , ������ ������� � ������� � �������
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   //AAdd( aColors, { CLR_FOCUSF, {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_WHITE, CLR_BLACK ) } } )  // 5 , ������ ������� � ������� � �������
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

/*/////////////////////////////////////////////////////////////////
STATIC FUNCTION myFocusB(nAt, nCol, oBrw, nFoc, nClr, nDel)
   HB_SYMBOL_UNUSED(nAt)          // or Default nAt  := oBrw:nAtPos
   Default nFoc := -CLR_HRED
   Default nClr := -CLR_BLUE
   Default nDel := -CLR_YELLOW

   IF oBrw:nCell == nCol
      nClr := nFoc
   ELSEIF (oBrw:cAlias)->( Deleted() )
      nClr := nDel
   ENDIF

RETURN nClr*/

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // ��������� �������
   LOCAL nHImg, nI, oCol, cCol, n, oDlu, cVal, aFont, cFont, nFSize
   LOCAL hFont, cHead, nWCol

   ? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName
   //_o2log(oTsb , 15, ProcNL()+" -------------- ��������� ������� : => oTsb", .T.)

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // �������� ������� ������� �� �����
   oDlu := _Font2oDlu( oTsb:aFont[1] )
   n    := oDlu:nSize

   //!!! �������� ������� �������
   ? SPACE(5) + _HMG_DefaultFontName, _HMG_DefaultFontSize, "n=", n, oTsb:aFont[1]
   ? SPACE(5) + "!!!",n," oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   nHImg := oDlu:H1 + 6              // ������ �������� = ������ ����� � ���
   //                ^^^ - ���������
   nHImg := oDlu:H(1.25)             // ��� ����������, �� ������� ����� ������
   //              ^^^^  - ��������� �� ������� �����

   WITH OBJECT oBrw
      :Cargo:nModify := 0     // ��������� � �������

      :lNoKeyChar    := .F.          // ��� ����� � ������ �� ����, ����
      :nHeightCell   := nHImg        // ������ ����� = ������ ��������
      :nHeightHead   := nHImg * 1.2  // ������ �����
      :nHeightFoot   := nHImg + 4    // ������ �������
      :nHeightSpecHd := n + n/2      // ������ ���������� ENUMERATOR
      :lFooting      := .T.          // ������������ ������
      :lDrawFooters  := .T.          // ��������  �������
      //:nFreeze     := 2            // ���������� �������
      //:nCell       := :nFreeze + 1
      :lLockFreeze   := .T.          // �������� ���������� ������� �� ������������ ��������
      :nCellMarginLR :=  1           // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
      :nMemoHV       :=  1           // ����� ����� ������ ����-����
      :lNoKeyChar    := .F.          // ��� ����� � ������ �� ����, ����

      // --------- ��������� ��������, ��������� ����� �������� ������� ��������� ------
      :aBitMaps      := { Nil, LoadImage("bRecDel16") }

      :nHeightCell := :nHeightCell + 1
      :nHeightHead := :nHeightCell
   END WITH

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         cVal := oCol:cFieldTyp + '('
         cVal += HB_NtoS(oCol:nFieldLen) + ','
         cVal += HB_NtoS(oCol:nFieldDec) + ')'
         oCol:cFooting := cVal
         oCol:nFAlign  := DT_CENTER
         AADD( oBrw:Cargo:aStruct , {cCol, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldLen} )
      ENDIF
      // ��� ����� MONO - DejaVu Sans Mono ������ �������
      IF oCol:cFieldTyp == "C"
         //oCol:cPicture := Nil
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
      ELSEIF oCol:cFieldTyp $ "N"
         n := 2 + 1  // :nCellMarginLR :=  1
         oCol:nWidth   += oCol:ToWidth(n)
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         IF LEN( SET( _SET_DATEFORMAT ) ) > 8
            oCol:nWidth := oCol:ToWidth(10+2)   // "01.01.2024"
         ELSE
            oCol:nWidth := oCol:ToWidth(10)   // "01.01.24"
         ENDIF
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99" // 23 �������
         //oCol:bDecode  := {|tval| iif( tval == hb_CToT(""), "", hb_TtoS(tval) ) }
         //oCol:bDecode:= {|tval| hb_TtoS(tval) }
         //oCol:nAlign   := DT_LEFT
         // ����� ���
         oCol:cPicture := NIL
         IF nFSize > 14  ; oCol:nAlign   := DT_LEFT
         ELSE            ; oCol:nAlign   := DT_CENTER
         ENDIF
         oCol:nWidth   := oCol:ToWidth(25)
         //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - ��� �� ��������, ���� ����� oCol:cPicture, �� � ����������
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp == "M"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
      ENDIF
      // �������� ������ �������
      hFont := oCol:hFontHead                  // ����� ���� � ������� �����
      cHead := oCol:cHeading + "H"
      nWCol := GetTextWidth( Nil, cHead, hFont )
      IF oCol:nWidth < nWCol
         oCol:nWidth := nWCol
      ENDIF
   NEXT

   // � ������ ��������� ���� ������� ���, ��������� ������ �����
   // ���� ���� ����� END TBROWSE
   ? ProcNL()
   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   ? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)
   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont, oCol, aFont

   hFont := oBrw:aColumns[1]:hFontSpcHd  // 4-special header font
   aFont := GetFontParam(hFont)
   ? ProcNL(), hFont , HB_ValToExp(aFont)

   // ���������� ���� ��� 1 ������� �������
   oBrw:aColumns[1]:hFont     := hFont     // 1-cells font
   oBrw:aColumns[1]:hFontFoot := hFont     // 3-footer font

   // ���������� ���� ��� ���� ������� �������
   // ���������� - ENUMERATOR (��������� �������)
   FOR EACH oCol IN oBrw:aColumns
      oCol:hFontSpcHd := hFont
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
// ENUMERATOR �� ������� ������� ����
STATIC FUNCTION myTsbEnum( oBrw, nOneCol )
   LOCAL oCol, nBCSpH, nI := 0, nCnt := 0
   DEFAULT nOneCol := 1

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // ���� ���� ���������� �������

   FOR EACH oCol IN oBrw:aColumns
      nI++
      oCol:cSpcHeading := NIL
      oCol:cSpcHeading := IIF( nI == nOneCol, "#" , "+" )
      IF nI > nOneCol
         IF oCol:lVisible
            oCol:cSpcHeading := hb_ntos( ++nCnt )
         ENDIF
      ENDIF
      // ��������� ����� ���������� - ENUMERATOR (��������� �������)
      oCol:nClrSpcHdBack := nBCSpH      // ::aColorsBack[ 18 ]
      oCol:nClrSpcHdFore := CLR_RED     // ::aColorsBack[ 19 ]
   NEXT

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw, oTsb )
   LOCAL hFont, nHFont, aSupHd, cSprHd, nClr16, nClr17, O

   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )
   aSupHd := oTsb:aSupHd
   O      := oBrw:Cargo
   cSprHd := oTsb:cTtlSupHead
   nClr16 := oTsb:aClr16
   nClr17 := oTsb:aClr17

   WITH OBJECT oBrw
      // ������ ���������� � ������� �������� 0
      :AddSuperHead( 1, :nColCount()+1, "Super_Header_Table" ) //,,, .F.,,, .F., .F., .F., 0, )
      :aSuperhead[ 1, 3 ] := cSprHd
      :nHeightSuper := nHFont * 1.5    // 1 ������
      // ������ ����� �����������
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, ���� ���������
      :SetColor( { 17 }, { nClr17           } ) // 17, ������ ���������
   END WIDTH

   o:TitleSupHd := oBrw:aSuperhead[ 1, 3 ]    // ���������
   o:Clr16SupHd := nClr16                     // 16, ����
   o:Clr17SupHd := nClr17                     // 17, ������

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// ������ ��������� ���
STATIC FUNCTION myTsbKeyFX( oBrw, oTsb )
   LOCAL o := oBrw:Cargo      // ������������ �� ���������� ���� ����������
   LOCAL nLen, cBrw, nTsb

   ? ProcNL(), oBrw:cAlias, oTsb

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

      // ��������� �����
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :SetAppendMode( .F. )            // ��������� ������� ������ � ����� ���� �������� ����
      //oBrw:SetDeleteMode( .T., .F. )
      //oBrw:SetDeleteMode( .T., .T. ) // ����������� ������ �� ��������
      :SetDeleteMode( .T., .F., {|| // ���� ��� ��������/��������������
                                    Local lDel, cDel, cIns, cMsg, cTtl
                                    Local lRet, aClrs := { {45,223,70} , ORANGE }
                                    Local aTmp, aBClr, aFClr
                                    If App.Cargo:cLang == "RU"
                                       cDel := "�������� !;������� ������ � ������� ?"
                                       cIns := "�������� !;������������ ������ � ������� ?"
                                       cTtl := "�������������"
                                    Else
                                       cDel := "ATTENTION !;Delete a record in a table ?"
                                       cIns := "ATTENTION !;Restore a record in a table ?"
                                       cTtl := "Confirmation"
                                    Endif
                                    lDel  := (oBrw:cAlias)->(Deleted())
                                    cMsg  := iif(lDel, cIns, cDel)
                                    aBClr := {248,209,211}      // ������-�������
                                    aFClr := MAROON
                                    aTmp  := _SetMsgAlertColors(aBClr,aFClr)  // ����� �����
                                    lRet  := AlertYesNo( cMsg, cTtl, ,"ZZZ_B_STOP64", 64, aClrs )
                                    _SetMsgAlertColors(aTmp[1],aTmp[2])       // ������������ �����
                                    Return lRet
                                } )
      // ��������� ������� ESC � ������
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      :UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      :UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })

      // ������� FXX
      :UserKeys(VK_F2    , {|ob| myTsbListColumn( ob ), ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F3    , {|ob| myTsbListFont( ob )  , ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F9    , {|ob| ListDelete( ob )     , ob:Setfocus() })  // ������� ������

      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth
      nLen := :GetAllColsWidth() - 1
      IF nLen > nTsb
         :lAdjColumn  := .T.
         :lNoHScroll  := .F.
         :lMoreFields := ( :nColCount() > 45 )
      ELSE
         :AdjColumns()
      ENDIF

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, nI, cCol

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      //? "    .",nI, cCol
      oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> tsb_view_func.prg
      oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> tsb_view_func.prg
      oCol:lEdit     := .T.
      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� �������������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ������ ����� END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // ���� ���� ���������� �������
   a4Clr  := oTsb:a4Clr                     // ������� 4 ����� �������
   nTest  := HMG_RGB2n(a4Clr[1])            // ���� ���� ����

   ? ProcNL(), MGVersNumba()
   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   ? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   oBrw:lClrSelectorHdBack := .F. // background OFF
   // ����������� ������� / virtual column
   IF oBrw:lSelector
      oCol := oBrw:GetColumn("SELECTOR")
      oCol:nClrBack        := nBCSpH
      oCol:nClrFore        := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      //oCol:nClrSpcHdBack := nBCSpH
      oCol:SaveColor()                       // ��������� ����� �������
      oBrw:nClrSelectorHdBack := nBCSpH      // Footer ��� "SELECTOR"
   ENDIF
   // ����� ���� TBROWSE
   oBrw:nClrHeadBack := nBCSpH

   // ��������� ����������� �������
   nTest := oBrw:Cargo:nTotalRecno
   nLen  := LEN(HB_NtoS(nTest))
   nCol  := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // ���-�� ������ + 2 �����
      oCol:nWidth := nWCol                                       // ����� ������
      oCol:cFooting := HB_NtoS(nTest)                            // RecCount
      // ������� ������ - ����
      oCol:nClrBack      := nBCSpH
      oCol:nClrFore      := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      oCol:nClrFootFore  := CLR_WHITE
      oCol:SaveColor()             // ��������� ����� �������
      // ������ ����� ��� ������� �������
      hFont := oBrw:aColumns[2]:hFontSpcHd       // 4-special header font
      oBrw:aColumns[nCol]:hFont     := hFont     // 1-cells font
      oBrw:aColumns[nCol]:hFontFoot := hFont     // 3-footer font
   ENDIF

   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// ����� ������ � ���� ����������� � ����� ���� � ��������� ����� � ��������������
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL nRecno, cMsg, aTmp, aBColor, aFColor, aColors, cTitle //, oRst

   ? " -Ins- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTitle := '���������� ������'
      cMsg   := "�������� !;�������� ������ � ������� ?;"
   ELSE
      cTitle := 'Adding recno'
      cMsg   := "ATTENTION!;Insert a record into the table ?;"
   ENDIF

   aColors := { {45,223,70} , ORANGE }
   aBColor := { 238, 249, 142 }   // ������-�����
   aFColor := BLACK
   aTmp    := _SetMsgAlertColors(aBColor,aFColor)  // ����� �����

   IF AlertYesNo( cMsg, cTitle, , , 64, aColors )
      // ����������� ����� ��� ���������� ������
      oBrw:bAddAfter := {|ob,ladd|
                          Local nRecno := (ob:cAlias)->( RecNo() )
                          LOCAL cTable  := ATREPL( "_", ob:cAlias, " " )
                          LOCAL cInsertQuery := "INSERT INTO " + cTable + " DEFAULT VALUES"

                          If ladd
                             ? "+++ :bAddAfter",ProcNL()
                             ?? "RecNo()= ", nRecno
                             IF rddInfo( RDDI_EXECUTE, cInsertQuery )
                             EndIf
                          EndIf
                          Return Nil
                        }

      // oBrw:bAddAfter  := Nil  // ��� ���� �� ����� ��� ���������� ����� ��� �������� ����� ������

      // ���������� ����� ��� ���������� ������
      oBrw:AppendRow()

      nRecno := (oBrw:cAlias)->( RecNo() )
      ? "+++ " + ProcNL(), hb_DateTime(), "Insert!", "RecNo()=", nRecno

      oBrw:nCell := 3  // � ������ ������� ��� ��������������
      oBrw:Reset()
      oBrw:GoBottom()     // ������ �� ����� ������, ���� ��� �������
      DO EVENTS

   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])      // ������������ �����

RETURN Nil

//////////////////////////////////////////////////////////////////////////
// �������� ������ � ������� ����� ������������
//                        ������ ����� �� ������ �� ���� ������� !!!
// � ���������� ��������� ������ � ����� �����-sqlite ����������� ���
//  ��������� ������ � �� �������� �������������� !!!
// ������ ����� ����.������� ����� � �� ������ ������������ ����������
STATIC FUNCTION RecnoDelete(oBrw)
   LOCAL lChange, nAt, lDelete, nRecno, nCell, nMetod, nRec

   ? " -Del- "+ProcNL(), oBrw:ClassName
   ?? ":nLen=", oBrw:nLen //,":lIsXXX=", oBrw:lIsDbf, oBrw:lIsArr
   ?? ":nRowPos=", oBrw:nRowPos

   // ����������� ����� ��� �������� ������
   oBrw:bDelAfter := {|nr,ob|
                             Local cAls := ob:cAlias
                             Local nOld := (cAls)->( RecNo() )
                             LOCAL cTable  := ATREPL( "_", ob:cAlias, " " )
                             LOCAL cDeleteQuery := "DELETE FROM " + cTable
                             LOCAL cWhere := " WHERE "
                             LOCAL aStruct := oB:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - ��������� ����

                             cWhere += aStruct[ 1 ][ 1 ] + "="
                             cWhere += ClipValue2SQL( (cAls)->( FieldGet( 1 ) ) )
                             cDeleteQuery += cWhere
                             ? " -Del-  :bDelAfter" + ProcNL(), "nRecno=", nOld
                             ?? "cDeleteQuery=", cDeleteQuery
                             IF rddInfo( RDDI_EXECUTE, cDeleteQuery )
                             EndIf

                             Return nr
                            }

   lDelete := (oBrw:cAlias)->( Deleted() )
   nRecno  := (oBrw:cAlias)->( RecNo() )
   nCell   := oBrw:nCell    // ������ �� ������� �������
   nAt     := oBrw:nAt      // ��� ������� - ������ ������� �� ������
   nAt     := oBrw:nRowPos  // ��� dbf     - ������ ������� �� ������
   ? " -Del-  lDelete=", lDelete, "nRecno=",nRecno

   nMetod  := 0
   IF oBrw:lIsArr                 //  ��� �������
      ? " -Del- :nLen == :nAt", oBrw:nLen, oBrw:nAt
      IF oBrw:nLen == oBrw:nAt
         nMetod := 1  // ��� ��������� ������
      ENDIF
   ELSEIF oBrw:lIsDbf            //  ��� dbf
      ? " -Del- ordKeyNo() == ordKeyCount()"
      ?? ordKeyNo(), ordKeyCount()
      IF ordKeyNo() == ordKeyCount()
         nMetod := 1  // ��� ��������� ������
      ENDIF
      ?? ":nRowPos=", oBrw:nRowPos
   ENDIF
   ?? "nMetod=",nMetod

   // ��������/�������������� ������ ��������� !!!
   // ���������� ����� ��� �������� ������� ������
   lChange := oBrw:DeleteRow(.F., .T.)

   IF lChange                              // ��������� ����
      ? " -Del- " + ProcNL(), "lChange="+cValToChar(lChange), "�������! ����� ������!"
      ?? "-> nMetod=" + HB_NtoS(nMetod)
      IF nMetod == 1        // ��� ��������� ������ � ���� � �������
         IF oBrw:lIsArr                   // ��� �������
            oBrw:Refresh(.T., .T.)
            nRec := oBrw:nLen
            oBrw:GoPos(nRec, nCell)
            ?? "������� :GoPos(:nLen=", nRec
         ELSEIF oBrw:lIsDbf               // ��� dbf
            (oBrw:cAlias)->( dbSkip(0) )
            oBrw:Reset()
            oBrw:Refresh(.T., .T.)
            oBrw:GoBottom()               // �� ��������� ������
            nRec   := oBrw:nRowPos        // ����� ������ � �������
            nRecno := (oBrw:cAlias)->( RecNo() )
            oBrw:GoToRec( nRecno )
            DO EVENTS
            ?? "������� :GoToRec()=", nRecno, ":nRowPos=",nRec
         ENDIF
      ELSE
         IF nAt == 1
            oBrw:Reset()
            oBrw:Refresh()
            nRecno += 1
         ENDIF
         oBrw:GoToRec( nRecno )
         ?? "GoToRec()=", nRecno
      ENDIF

      oBrw:DrawFooters()   // ���������� ������
      DO EVENTS
      //������ � ������-��������-�������������-���������
      //write to the program-user-actions-log
   ELSE
      ?? "������ ��������", lChange
   ENDIF

   DO EVENTS
   ? " -Del-  .end"

RETURN Nil

//////////////////////////////////////////////////////////////////////////
// �������� ������ � ������� ����� ������������
//                        ������ ����� �� ������ �� ���� ������� !!!
// � ���������� ��������� ������ � ����� �����-sqlite ����������� ���
//  ��������� ������ � �� �������� �������������� !!!
// ������ ����� ����.������� ����� � �� ������ ������������ ����������
STATIC FUNCTION ListDelete(oBrw)
   LOCAL owc, cTtl, cMsg, cLng

   ? " -DelList- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTtl := '�������� ������ �������'
      cMsg := "�������� !;������� ������� ������ ������� � ������� ?;;"
      cLng := "����� !;������� ����� ������ ������ �� ������� !;;"
   ELSE
      cTtl := 'Delete list of records'
      cMsg := "ATTENTION!;Delete the current LIST of records in the table?;;"
      cLng := "REFUSAL!;You can delete ONLY records by filter!;;"
   ENDIF

   owc := oBrw:Cargo:owc             // Cargo ����
   IF owc:nFilter == 1               // ������� ������
      AlertStop( cLng, , , 64, {RED} )
      RETURN NIL
   ENDIF

   IF AlertYesNo(cMsg, cTtl, , "iQuestion64", 64, {ORANGE,RED} )

   ENDIF

   DO EVENTS
   ? " -DelList-  .end"

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, aRet
   LOCAL cTyp, cMsg, xRet, lWrt, cStr

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // ��� ��������� �������
      cNam  := oCol:cName
   END WITH
   oCol:nClrEditBack := CLR_GRAY
   ? SPACE(5) + ProcNL(), nCol, cTyp

   uOld := uVal
   lWrt := .T.       // �������� ������
   lRet := .T.       // ������ ������������� ���� � :get
   aRet := {uVal}    // �������� � ������ - �� ��� ������
   cMsg := ''
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += '��� ���� �������: "' + cTyp + '" ;'
      cStr += '��� ��������� ��� ����� ���� !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   ? ProcNL(), cTyp, uVal, VALTYPE(uVal)
   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_RED
      oCol:nClrEditBack := CLR_GRAY
   ELSEIF cTyp $ "CMV"
      // ���.��������
      IF !IsString(uVal)
         uVal := cValToChar(uVal)
      ENDIF
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // ������ ��� ����� �������
      IF AT(CRLF,uVal) > 0           // ���� � ���� "C" ���� CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // �� ������ ������������� ���� � :get
      ELSEIF cTyp $ "MV"
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                // �� ������ ������������� ���� � :get
      ENDIF
   ELSEIF cTyp $ "=@T" .OR. cTyp $ "D"
      aRet := CellEdit_DT(oBrw, cTyp, uVal)
      lRet := .F.             // �� ������ ������������� ���� � :get
   ELSE
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_RED
      ? SPACE(5) + ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      //cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      //AlertInfo(cMsg + cStr)
      //lWrt := .F.             // �� ���������� � ������
      //lRet := .F.             // �� ������ ������������� ���� � :get
   ENDIF

   IF lWrt                                         // ���������� ������
      //IF (oBrw:cAlias)->(RLock())                  // ������ ������
         // !!! ������ ������, ���� ������, �� ��� ����� �� �����
         IF LEN(aRet) > 0
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // �������-��������� � �������
            xRet := aRet[1]
            oBrw:SetValue(nCol,xRet)
            // !!! oBrw:cAlias - SQLMIX/0 �������� �� ��������������
            //(oBrw:cAlias)->KOPERAT  := 555       // ��� ������ ������
            //(oBrw:cAlias)->DATEVVOD := DATE()    // ���� ������
            //(oBrw:cAlias)->TIMEVVOD := 9999      // ����� ������
            //(oBrw:cAlias)->( DbUnlock() )
            //(oBrw:cAlias)->( DbCommit() )
         ENDIF
      //ELSE
      //   cMsg := "Recording is locked !; Recno="
      //   cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
      //   AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      //ENDIF
   ENDIF
   oBrw:DrawSelect()    // ������������ ������� ������ �������
   oBrw:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, oBrw )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod
   LOCAL oWnd  := _WindowObj(oBrw:cParentWnd)
   LOCAL aItog := oWnd:Cargo:aItogo
   LOCAL cTyp, cMsg, cStr
   LOCAL cTable  := ATREPL( "_", oBrw:cAlias, " " )
   LOCAL cUpdateQuery := "UPDATE " + cTable + " SET "
   LOCAL i, ni, cWhere := " WHERE "
   LOCAL aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec}

   WITH OBJECT oBrw
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // ��� ��������� �������
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
   END WITH

   ? SPACE(5) + ProcNL(), nCol, cTyp
   cStr := 'oCol:bEditPost !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += '��� ���� �������: "' + cTyp + '" ;'
      cStr += '��� ��������� ��� ����� ���� !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   IF cTyp $ "CNDLIV"
      // ����������� ���������
      FOR i := 1 TO Len( aStruct )
         IF lMod
            cUpdateQuery += aStruct[ i ][ 1 ] + "=" + ClipValue2SQL( FieldGet( i ) ) + ","
         ENDIF
      NEXT
      // no Change
      IF Right( cUpdateQuery, 4 ) == "SET "
         RETURN .F.
      ENDIF
      // remove last comma
      cUpdateQuery := Left( cUpdateQuery, Len( cUpdateQuery ) - 1 )
         FOR nI := 1 TO 1
            cWhere += aStruct[ nI ][ 1 ] + "="
            // use original value
            cWhere += ClipValue2SQL( (cAls)->( FieldGet( nI ) ) )
            cWhere += " AND "
         NEXT
         // remove last " AND "
         cWhere := Left( cWhere, Len( cWhere ) - 5 )
         cUpdateQuery += cWhere
                             ? " -Upd-  :bPostEdit" + ProcNL()
                             ?? "cUpdateQuery=", cUpdateQuery
                             IF rddInfo( RDDI_EXECUTE, cUpdateQuery )
                             EndIf
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN .T.

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle //, aStruct

   //aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - ��������� ���� ��� ��������
   IF App.Cargo:cLang == "RU"
      cTitle := '���� �� ������ �������'
   ELSE
      cTitle := 'Info on the list of columns'
   ENDIF
   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      cMsg  += HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT
   cMsg += ";"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := cValToChar( oCol:cPicture )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = "
      cMsg  += ' "'+ cFld + '"  ;'
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight - 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH

   ? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet)

   oJWnd := oBrw:Cargo:ObjWnd        // ������� ����
   cForm := oJWnd:Name
   nY    += oJWnd:Row
   nX    += oJWnd:Col + 7
   IF oBrw:lDrawSpecHd
      nY -= oBrw:nHeightSpecHd    // ������ ���������� ENUMERATOR
   ENDIF

   nY     += IIF( App.Cargo:aDisplayMode[2] <= 720, 8, 4 )
   nHCell += IIF( App.Cargo:aDisplayMode[2] <= 720, 3, 0 )
   //nY     += IIF( Sys.ClientHeight <= 720, 8, 4 )
   //nHCell += IIF( Sys.ClientHeight <= 720, 3, 0 )

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]

   nHObj  := nHCell - 7 //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4       // ��� ������
   nW     := nWDate + nWBtn
   aRet   := {}   // ������ ������ - �����, ������ ����� �� �����

   // ����� �� ������� ������/��������� � ������� ����� ������
   IF nX + nW > App.Cargo:aDisplayMode[2] //Sys.ClientWidth
      nX := (nWCell + nX) - nW
   ENDIF
   nH := nHCell

   // ����� ���� � ������ �������
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH  ;
      MODAL NOCAPTION                              ;
      FONT cFont SIZE nFSize                       ;
      ON LOSTFOCUS {|| oWnd:Release() }            ;
      ON INIT      {|| DoEvents() }

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle

      IF cType == "D"

         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := { This.Date_1.Value } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         //tDTime := HB_STRTOTS( xGet )
         tDTime := xGet
         IF tDTime == hb_CToT("")
            tDTime := hb_DateTime()
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value  ,;
                      aRet   := { tDTime } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ENDIF

       DRAW LINE IN WINDOW Cell AT 2, 2 TO 2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT nH-2, 2 TO nH-2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, 2 TO nH, 2 PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, nW-2 TO nH, nW-2 PENCOLOR RED PENWIDTH 4

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } )
   Cell.Activate

RETURN aRet // ������ ������, ���� ����� - ������ ����� �� �����

////////////////////////////////////////////////////////////////////////////
