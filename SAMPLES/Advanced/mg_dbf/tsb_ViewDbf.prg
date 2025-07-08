/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ��������/����� TBrowse (���) / Creation/display of TBrowse (TSB)
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
///////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_ViewDbf(aUse,oWnd)
   LOCAL owc, cForm, oTsb, aColor, oBrw, cBrw, cSuperHd, cAls, cMsg, ao

   ? ProcNL(), aUse, oWnd, "|", HB_ValToExp(aUse)
   cForm := oWnd:Name
   owc   := oWnd:Cargo
   oTsb  := owc:oTsb            // ���������� � ����� �������
   cAls  := ALIAS()
   cBrw  := "Tsb1_" + cForm
   ao    := App.Cargo
   ? "   cBrw=", cBrw, cForm, "cAls=",cAls

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

   aColor := oTsb:a4Clr    // ��� ����� �������  - ������� � ������� ������
   /////////////////////// ������� ///////////////////////////////////////////////////
   //oTsb := oHmgData() - ������� � ������� ������
   //                      cell     Head   foot      SpecHider  SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }
   oTsb:uSelector   := 20
   oTsb:lSpecHd     := .T.           // ��������� � ������� ���������
   oTsb:lFooting    := .T.           // ��������� � ������� ������
   oTsb:aFoot       := .T.
   oTsb:nHeightFoot := 25            // ������ �������
   oTsb:nHeightHead := 25            // ������ �����
   oTsb:aEdit       := .T.           // ������������� �������
   oTsb:aBrush      := aColor[3]                       // ���� ���� ��� ��������
   oTsb:aColor      := Color_Tsb(aColor,oTsb)          // ����� �������: 2(�����+������),3(������ %1),4(������ %2)
   oTsb:cTtlSupHead := SuperHider()

   // ����� ���� ��� _TBrowse(...) - ������ ������ // (op=oTsb)
   oTsb:bInit  := {|ob,op| myTsbInit(ob,op), myTsbFont(ob,op), myTsbSuperHd(ob,op) }  // ��������� ���  -> ��.����
   oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)  }  // ������ ��������� ���  -> ��.����
   //oTsb:bEnd := {|ob,op| myTsbEnd(ob,op)                      }  // ���� ���� ����� END TBROWSE -> ��.����

   _o2log(oTsb, 15, ProcNL()+" -------------- ��������� ������� : => oTsb", .T.)

   // ------ ������� 1
   /* SET WINDOW THIS TO oWnd  // ������������� �� ������� ���� � ���
   // ����������� ����� ��� ������ !!!
   // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
   oBrw := _TBrowse( oTsb, cAls, cBrw, oTsb:nY, oTsb:nX, oTsb:nW, oTsb:nH )
   SET WINDOW THIS TO oWnd */

   // ------ ������� 2
   oTsb:cForm     := oWnd:Name    // <--- ����������� ��� !!!
   oTsb:cFormName := oWnd:Name    // ��� ���
   ? ProcNL(), cAls, ALIAS(), "cBrw=",cBrw
   // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
   oBrw := _TBrowse( oTsb, cAls, cBrw, oTsb:nY, oTsb:nX, oTsb:nW, oTsb:nH )
   // ��������� �������� � ���
   myTsbEnd(oBrw,oTsb)                  // <<----- ��� ����������

   // ����� ����� ���������� ���������� ��� oBrw:Cargo:XXXX � ����� �� ������������ � ���
   oBrw:Cargo:aFont  := oTsb:aFont
   oBrw:Cargo:cCdPg  := oWnd:Cargo:cCdPg    // ������� �� ��� CodePage �����
   //App.Cargo:oBrw  := oBrw                // ��������� ��� ������� ������� - ����� �� ����
   oWnd:Cargo:oBrw   := oBrw                // ��������� �� ����
   oWnd:Cargo:cBrw   := cBrw                // ��������� �� ����
   oBrw:Cargo:ObjWnd := oWnd                // ������� ����
   oBrw:Cargo:cSprHd := cSuperHd

RETURN oBrw

//////////////////////////////////////////////////////////////////
STATIC FUNCTION SuperHider(cCodePage)                   // Super Hider
   LOCAL cSuperHd, nOrder, cOrder, lModeUse, cFile
   LOCAL cRddDbf, cCdPg, cPsw, cSetDel, aUse
   DEFAULT cCodePage := ""

   aUse     := App.Cargo:aUse
   cFile    := aUse[1]
   lModeUse := aUse[2]
   cRddDbf  := aUse[3]
   cCdPg    := IIF( LEN(cCodePage)==0, aUse[4], cCodePage )
   cPsw     := aUse[5]
   cSetDel  := aUse[6]
   cSuperHd := "HB_LANGSELECT()= " + HB_LANGSELECT() + SPACE(5)
   cSuperHd += "VIA: " + cRddDbf + ", CODEPAGE: " + cCdPg

   IF cRddDbf == "DBFNSX"
      cSuperHd += ", PASSWORD: [" + cPsw + "]"
   ENDIF
   cSuperHd  += ", SET_DELETED: " + IIF( Set(_SET_DELETED), "OFF", "ON" )
   cSuperHd  += ",  USE_MODE: " + my_UseMode() + ",  ALIAS: " + ALIAS()
   cSuperHd  += ",  ORDER: ["
   IF OrdCount() > 0
      nOrder := INDEXORD()
      cOrder := HB_NtoS(nOrder) + "/" + OrdName(nOrder)
      cSuperHd  += cOrder + "]"
   ELSE
      cSuperHd  += "0]"
   ENDIF

RETURN cSuperHd

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
   //AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , ���� �������
   AAdd( aColors, { CLR_FOCUSB, {|nr,nc,ob| myFocusB(nr,nc,ob,-CLR_HRED,-CLR_BLUE,-CLR_YELLOW) } } ) // 6 , ���� �������

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

RETURN nClr

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // ���������
   Local nHCell, nI, cCol, oCol, nDlu, oDlu, cVal, n
   LOCAL aFont, cFont, nFSize, nHHead, nHFoot
   LOCAL hFont, cHead, nWCol

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // �������� ������� ������� �� �����
   oDlu := _Font2oDlu( aFont[1] )
   nDlu := oDlu:nSize
   // --- �������� ������� ������� ---
   //?  _HMG_DefaultFontName, _HMG_DefaultFontSize, "nDlu=", nDlu, oTsb:aTsbFont[1]
   //?  "oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   IF cFont == "DejaVu Sans Mono"
      // !!! ��� ����� MONO - DejaVu Sans Mono ������ �������
      IF nFSize <= 15                       ; n := 20
      ELSEIF nFSize > 15 .AND. nFSize <= 17 ; n := 70
      ELSEIF nFSize > 17 .AND. nFSize <= 19 ; n := 75
      ELSE                                  ; n := 50
      ENDIF
      //n := iif( nFSize < 15, 20, iif( nFSize < 20, 30, 40 ) )
      oDlu:nPixWidth    += n
      oDlu:nPixWidthDT  += n
      oDlu:nPixWidthDT1 += n
      oDlu:nPixWidthDT2 += n
   ENDIF
   //
   nHCell := oDlu:H1 + IIF(nFSize <= 15,6,12)     // ������ ����� � ���
   //                ^^^ - ���������
   nHCell := oDlu:H(IIF(nFSize <= 15,1.25,1.5))   // ��� ����������, �� ������� ����� ������
   //               ^^^^  - ��������� �� ������� �����
   // ����� ����� � ��� �������
   nHHead := oTsb:nHeightHead       // ������ �����
   nHFoot := oTsb:nHeightFoot       // ������ �������

   WITH OBJECT oBrw

      :lNoKeyChar    := .F.           // ��� ����� � ������ �� ����, ����
      :nHeightCell   := nHCell        // ������ ����� = ������ ��������
      :nHeightHead   := nHCell * 1.2  // ������ �����
      :nHeightFoot   := nHCell + 4    // ������ �������
      :nHeightSpecHd := 12            // ������ ���������� ENUMERATOR
      :lFooting      := .T.           // ������������ ������
      :lDrawFooters  := .T.           // ��������  �������
      //:nFreeze     := 2             // ���������� �������
      //:nCell       := :nFreeze + 1
      :lLockFreeze   := .T.           // �������� ���������� ������� �� ������������ ��������
      :nCellMarginLR :=  1            // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
      :nMemoHV       :=  1            // ����� 2 ������ ����-����

      // --------- ��������� ��������, ��������� ����� �������� ������� ��������� ------
      :aBitMaps      := { Nil, LoadImage("bRecDel16") }

      :GetColumn("ORDKEYNO"):nWidth   := nHCell + 10   // ������ �������
      // ��������� �������� ��� �������� ������� � ������� ORDKEYNO
      :GetColumn("ORDKEYNO"):aBitMaps := :aBitMaps //oTsb:aBmp1[2]
      :GetColumn("ORDKEYNO"):uBmpCell := {|nc,ob| nc:=nil, iif( (ob:cAlias)->(Deleted()), ob:aBitMaps[2], ob:aBitMaps[1] ) }

      :Cargo:nModify := 0     // ��������� � �������

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

   // � ������ ��������� ������� SELECTOR, ORDKEYNO - ���
   // ��������� ������ ����� END TBROWSE
   //? ProcNL()
   //? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)
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
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, ����
      :SetColor( { 17 }, { nClr17           } ) // 17, ������
      // ������ �������
      //nHAlign := :nAlignSupHdGet(nCol)   // ��� �����������
      //:nAlignSupHdGet(nCol, nHAlign)
      //:aSuperHead[1,12] := DT_LEFT       // �������� ������� ������
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
      :UserKeys(VK_F2    , {|ob| myTsbCodePage( ob, 1 ), ob:Setfocus() })  // ����� ������� ��������
      :UserKeys(VK_F3    , {|ob| myTsbCodePage( ob, 2 ), ob:Setfocus() })  // ����� ������� ��������
      :UserKeys(VK_F4    , {|ob| myTsbListColumn( ob ) , ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F5    , {|ob| myTsbListFont( ob )   , ob:Setfocus() })  // ���� �� ������ �������
      :UserKeys(VK_F8    , {|ob| myTsbSelectorNew(ob)  , ob:Setfocus() })  //
      :UserKeys(VK_F9    , {|ob| myTsbSelectorOld(ob)  , ob:Setfocus() })  //

      cBrw := :cControlName
      nLen := nTsb := 0
      //nTsb := This.&(cBrw).ClientWidth   // This. ������ ������������,
      //nLen := :GetAllColsWidth() - 1     // ���� ���������� ��� ��� � ������ ����
      IF nLen > nTsb
         //:lAdjColumn  := .T.
         //:lNoHScroll  := .F.
         //:lMoreFields := ( :nColCount() > 45 )
      ELSE
         //:AdjColumns()
      ENDIF

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   //? "***", ProcNL(), Alias()
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
// ����-���� ������� ������ ����� END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, nI, cCol, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol

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

   // ��������� ����������� �������
   nLen := LEN(HB_NtoS(oBrw:nLen))
   nCol := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // ���-�� ������ + 2 �����
      oCol:nWidth := nWCol                                       // ����� ������
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

   // ��������� ������ ���� ���� "+"
   nLen := LEN(HB_NtoS(oBrw:nLen))
   FOR EACH oCol IN oBrw:aColumns
      nCol := hb_EnumIndex(oCol)
      IF oCol:cFieldTyp == "+"
         hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
         nWCol := GetTextWidth( Nil, REPL("0", nLen + 6), hFont )   // ���-�� ������ + 2 ����� + 2 ���� ������
         oCol:nWidth := nWCol                                       // ����� ������
      ENDIF
   NEXT

   IF oBrw:lDrawSuperHd  // ���-2
      // ��������� ���������� �� ����� �������
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF

   oBrw:DrawHeaders()   // ���������� ����������/�����/���������
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// ����� ������ � ���� ����������� � ����� ���� � ��������� ����� � ��������������
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL nRecno, cMsg, aTmp, aBColor, aFColor, aColors, cTitle

   ? " -Ins- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTitle := '���������� ������'
      cMsg   := "�������� !;�������� ������ � ������� ?"
   ELSE
      cTitle := 'Adding recno'
      cMsg   := "ATTENTION!;Insert a record into the table ? "
   ENDIF

   aColors := { {45,223,70} , ORANGE }
   aBColor := { 238, 249, 142 }   // ������-�����
   aFColor := BLACK
   aTmp    := _SetMsgAlertColors(aBColor,aFColor)  // ����� �����

   IF AlertYesNo( cMsg, cTitle, , , 64, aColors )
      // ����������� ����� ��� ���������� ������
      // �������� � ���� ����+����� ������� ������
      oBrw:bAddAfter := {|ob,ladd|
                          Local cRecno := HB_NtoS( (ob:cAlias)->( RecNo() ) )
                          If ladd
                             ? "+++ :bAddAfter",ProcNL(), "INDEXORD()=", INDEXORD()
                             ?? "RecNo()= " + cRecno
                             /*(ob:cAlias)->KOPERAT   := M->nOperat   // ��� ������� ������
                             (ob:cAlias)->DATEVVOD  := DATE()       // ����/����� ������
                             (ob:cAlias)->TIMEVVOD  := nTime*/
                             (ob:cAlias)->( dbSkip(0) )
                          EndIf
                          Return Nil
                        }

      // oBrw:bAddAfter  := Nil  // ��� ���� �� ����� ��� ���������� ����� ��� �������� ����� ������

      // ���������� ����� ��� ���������� ������
      oBrw:AppendRow(.T.)

      nRecno := (oBrw:cAlias)->( RecNo() )
      IF (oBrw:cAlias)->(RLock())
         // ���� ����� ������ � ���� ����+����� ��� ���� �������� (������� ����)
         //(oBrw:cAlias)->IM        := hb_DateTime()    // ����� �������� ������
         //(oBrw:cAlias)->KOPERAT   := M->nOperat       // ��� ������� ������ - ������
         //(oBrw:cAlias)->DATEVVOD  := DATE()
         //(oBrw:cAlias)->TIMEVVOD  := nTime
         (oBrw:cAlias)->(DbCommit())
         (oBrw:cAlias)->(DBUnlock())
         ? "+++ " + ProcNL(), "INDEXORD()=", INDEXORD(), "RecNo()=", nRecno
      ENDIF

      nRecno := (oBrw:cAlias)->( RecNo() )
      ? "+++ " + ProcNL(), hb_DateTime(), "Insert!", "RecNo()=", nRecno

      oBrw:nCell := 1 // oBrw:nColumn("PRIDAT", .T.)  // � ������ ������� ��� ��������������
      oBrw:Reset()
      //oBrw:Refresh(.T.,.T.)
      oBrw:GoBottom()     // ������ �� ����� ������, ���� ��� �������
      DO EVENTS

   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])      // ������������ �����

RETURN Nil

//////////////////////////////////////////////////////////////////////////
STATIC FUNCTION RecnoDelete(oBrw)
   LOCAL lChange, nAt, lDelete, nRecno, nCell, nMetod, nRec

   ? " -Del- "+ProcNL(), oBrw:ClassName
   ?? ":nLen=", oBrw:nLen //,":lIsXXX=", oBrw:lIsDbf, oBrw:lIsArr
   ?? ":nRowPos=", oBrw:nRowPos

   // ����������� ����� ��� �������� ������
   oBrw:bDelAfter := {|nr,ob|
                             Local cAls := ob:cAlias
                             Local nOld := (cAls)->( RecNo() )
                             //If (cAls)->( deleted() )
                             ? " -Del-  :bDelAfter" + ProcNL(), "nRecno=", nOld
                             ?? "INDEXORD()=", INDEXORD()
                             If (cAls)->( RLock() )
                                // ���� ����� ������ � ���� ����+����� ��� ���� ��������
                                //If lDel ; (cAls)->DT_DEL  := hb_DateTime()
                                //Else    ; (cAls)->DT_REST := hb_DateTime()
                                //EndIf
                                //(cAls)->KOPERAT  := M->nOperat  // ��� ������ ������
                                //(cAls)->DATEVVOD := DATE()      // ����� ������� ������
                                //(cAls)->TIMEVVOD := Time()
                                (cAls)->( DbUnLock() )
                                ?? "Write field: ", DATE(), Time()
                                (cAls)->( dbSkip(0) )
                             EndIf
                             //EndIf
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
   ? SPACE(5) + ProcNL(), nCol, cTyp

   uOld := uVal
   lWrt := .T.       // �������� ������
   lRet := .T.       // ������ ������������� ���� � :get
   aRet := {uVal}    // �������� � ������ - �� ��� ������

   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += '��� ���� �������: "' + cTyp + '" ;'
      cStr += '��� ��������� ��� ����� ���� !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF cTyp $ "CM"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // ������ ��� ����� �������
      IF AT(CRLF,uVal) > 0           // ���� � ���� "C" ���� CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // �� ������ ������������� ���� � :get
      ELSEIF cTyp == "M"
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
      IF (oBrw:cAlias)->(RLock())                  // ������ ������
         // !!! ������ ������, ���� ������, �� ��� ����� �� �����
         IF LEN(aRet) > 0
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // �������-��������� � �������
            xRet := aRet[1]
            oBrw:SetValue(nCol,xRet)
            //(oBrw:cAlias)->KOPERAT  := 555       // ��� ������ ������
            //(oBrw:cAlias)->DATEVVOD := DATE()    // ���� ������
            //(oBrw:cAlias)->TIMEVVOD := 9999      // ����� ������
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
         ENDIF
      ELSE
         cMsg := "Recording is locked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      ENDIF
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

   IF cTyp $ "CNDL"
      // ����������� ���������
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      RETURN .F.
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCodePage( oBrw, nMenu )             // ����� ������� ��������
   LOCAL aYX, aRet, cCdpg, cTitle

   ? ProcNL(), oBrw, nMenu
   aYX := App.Cargo:aLbl3  // { nH1, nW1 + nG }   // ���������� ������

   IF nMenu == 1
      SET WINDOW THIS TO oBrw:cParentWnd         // ����������� !!!
      aRet := myCodePagePart(aYX) // { 2 , "RU866", cMsg + "RU866","iFlag_Ru32" }
      SET WINDOW THIS TO
   ELSE
      SET WINDOW THIS TO oBrw:cParentWnd         // ����������� !!!
      aRet := myCodePageDbf(aYX)
      SET WINDOW THIS TO
   ENDIF
   IF LEN(aRet) > 0
      cCdpg     := aRet[2]
      ReopenDbase(oBrw,cCdpg)
      DO EVENTS
      // ��������� ����
      cTitle := SuperHider(cCdpg)
      oBrw:aSuperhead[1,3] := cTitle
      oBrw:DrawHeaders()             // ���������� ����������/�����/���������
      oBrw:Reset()
      oBrw:GoTop()
      oBrw:SetFocus()
   ENDIF
   DO EVENTS
RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION ReopenDbase(oBrw, cCodePage)
   LOCAL cDbf, cFltr, nI, cIndex, aIndx, cFile, nOrd, cAls
   LOCAL lNew, nJ, lOpen, cOld, oError, cMsg

   BEGIN SEQUENCE  WITH { |e|break( e ) }
      cOld := hb_cdpSelect(cCodePage)
      IF hb_cdpSelect() == cCodePage
         // ���� ����� ������� ��������
         // there is such a code page
      ENDIF
      hb_cdpSelect(cOld)
      lOpen := .T.
   RECOVER USING oError
      cMsg := "Code page error!;"
      cMsg += "No driver for CodePage: "
      cMsg += cCodePage + ";" + ProcNL()
      AlertStop( cMsg, "ERROR", "ZZZ_B_STOP64", 64)
      lOpen := .F.
   END SEQUENCE

   IF lOpen

      cAls  := oBrw:cAlias
      DbSelectArea(cAls)
      nOrd  := ORDNAME()
      aIndx := {}
      cDbf  := DBINFO( DBI_FULLPATH )
      cFltr := (cAls)->( DbFilter() )
      FOR nI := 1 TO 500
         IF LEN(ORDNAME(nI)) == 0
            EXIT
         ELSE
            DBSetOrder(nI)
            cIndex := DBORDERINFO( DBOI_FULLPATH,,ORDNAME(nI) )
            IF LEN(aIndx) == 0
               AADD(aIndx, cIndex )
            ELSE
               FOR nJ := 1 TO LEN(aIndx)
                  lNew := .T.
                  IF cIndex == aIndx[nJ]
                     lNew := .F.
                     EXIT
                  ENDIF
                  IF lNew
                    AADD(aIndx, cIndex )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
      (cAls)->( dbCloseArea() )
      DO EVENTS
      INKEYGUI(100)

      USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW SHARED
      DO EVENTS
      IF LEN(aIndx) > 0
         FOR nI := 1 TO LEN(aIndx)
            cFile := aIndx[nI]
            ORDLISTADD( cFile )
         NEXT
         DBSetOrder(nOrd)
      ENDIF

   ENDIF  // lOpen

RETURN NIL

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
