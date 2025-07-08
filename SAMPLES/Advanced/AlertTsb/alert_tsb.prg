/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * �������� �������� � ���� ����� _TBrowse()
 * Viewing arrays in a window using _TBrowse()
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
#define PROGVER  "Version 0.63 (16.03.2025)"
/////////////////////////////////////////////////////////////////////////////////////////
FUNCTION AlertTsb(cType,nIcoSize,aXDim,cTitle,aButton,oWin,o1Brw,bInitForm)
   LOCAL nFontSize, nY, nX, nW, nH, nG, cForm, cIcon, nWTsb, nHTsb, aRet, cMsg
   LOCAL nBtnH, nBtnW, cN, aFColor, aBColor, aBtnBClr2, aBtnFClr2, cTmp, aPost
   LOCAL nWDim, nHDim, hWndIsx, cFrmIsx, cFont, aFont, lBold, nW2, nH2, cVal, nI
   LOCAL aBtnTxt, aBtnBClr, aBtnFClr, aBtnFont, aWHDim, oTsb, oBrw, nWTxt, nPost
   LOCAL cHelp, aHelpBClr, aHelpFClr, nLine, nHhelp, cFontHlp, nFSizeHlp, lBoldHlp
   LOCAL lInitForm, cInitForm, nHDsk, nWFlg, aVal, nCol
   DEFAULT cType := "", nIcoSize := 128, cTitle := ""
   DEFAULT aXDim := { {"Error !!!","aXDim = {} empty array  !!!", ProcNL() } }
   DEFAULT aButton := {}, oWin := oHmgData(), o1Brw := oHmgData()
   DEFAULT bInitForm := {|| Nil }

   aRet      := {}  // ������� ������
   cForm     := "MG_Form_Tsb_" + HB_NtoS( _GetId() )
   aFont     := GetFontParam("Bold")      ; Default aFont := {}
   cFontHlp  := IIF( LEN(aFont) > 0, aFont[1], cFont     )
   nFSizeHlp := IIF( LEN(aFont) > 0, aFont[2], nFontSize )
   lBoldHlp  := IIF( LEN(aFont) > 0, aFont[3], .F.       )
   aFont     := GetFontParam("Normal")    ; Default aFont := {}
   cFont     := "DejaVu Sans Mono"
   nFontSize := 14
   cFont     := IIF( LEN(aFont) > 0, aFont[1], cFont     )
   nFontSize := IIF( LEN(aFont) > 0, aFont[2], nFontSize )
   lBold     := IIF( LEN(aFont) > 0, aFont[3], .F.       )
   aBtnFont  := {"Comic Sans MS",nFontSize + 2,.T.}

   // ��������� ���������� ����, ���� ��� ����
   hWndIsx := 0
   cFrmIsx := _HMG_ThisFormName
   IF !Empty(cFrmIsx) .and. _IsWindowDefined(cFrmIsx)
      hWndIsx := GetFormHandle( cFrmIsx )
   ENDIF

   IF Empty( _HMG_MainHandle )  // ���� ��� MAIN ����
      MsgDebug("ERROR ! NO MAIN window !"+ProcNL()+";"+ProcNL(1))
      SET WINDOW MAIN OFF
   ENDIF

   IF IsArray(aButton) .AND. LEN(aButton) > 6
      cMsg := "ERROR ! aButton array greater than 6 is not allowed !;;"
      cMsg += ProcNL()+";"+ProcNL(1)
      AlertStop(cMsg, , , 64)
      RETURN NIL
   ENDIF

   IF !IsArray(aXDim)  // ���� ��� �������
      cTmp  := aXDim
      aXDim := { {"ERROR !!!", "aXDim not an array !", cTmp, ProcNL(), ProcNL(1) } }
   ELSE
      // one-dimensional array
      IF Len( aXDim ) > 0 .and. !IsArray( aXDim[1] )
         aXDim := { {"ERROR !!!", "aXDim one-dimensional array !!!", ProcNL(), ProcNL(1) } }
      ELSEIF Len( aXDim ) == 0
         aXDim := { { "Error !!!" , "aXDim = {} empty array  !!!", ProcNL(), ProcNL(1) } }
      ENDIF
   ENDIF
   nCol  := LEN(aXDim[1])
   nLine := 0
   FOR nI := 1 TO LEN(aXDim)
      aVal  := aXDim[nI]
      nLine := MAX(nCol,LEN(aVal))
   NEXT
   IF nCol # nLine
      aXDim := { { "Error !!!" , "aXDim[1] # aXDim[nI] Difference in columns !!!", ProcNL(), ProcNL(1) } }
   ENDIF

   cType    := UPPER(ALLTRIM(cType))
   aBColor  := App.Cargo:aDlgBColor ;  DEFAULT aBColor := SILVER
   aFColor  := App.Cargo:aDlgFColor ;  DEFAULT aFColor := BLACK
   IF IsString(App.Cargo:cLang) .AND. App.Cargo:cLang == "RU"
      aBtnTxt  := { '&��' }
   ELSE
      aBtnTxt  := { '&Ok' }
   ENDIF

   aPost    := { 98    }               // ������� - �����
   DEFAULT aBtnBClr := { BLUE   }
   DEFAULT aBtnFClr := { YELLOW }
   aBtnFClr2 := oWin:aBtnFClr2  ; DEFAULT aBtnFClr2 := WHITE    // ��������� ���� ����� ������ (����� �� ������)
   aBtnBClr2 := oWin:aBtnBClr2  ; DEFAULT aBtnBClr2 := BLACK    // ��������� ���� ���� ������  (����� �� ������)

   IF cType == "INFO"
      cIcon    := "iMgInfo128"
      aBColor  := { 133, 205, 242 }      // ��� ���� ������-�����
      aFColor  := BLUE
      aBtnBClr := { {42,174,239} }
      aBtnFClr := { YELLOW }             // ���� ����� ������
   ELSEIF cType == "STOP"
      cIcon    := "iMgStop128"
      aBColor  := {248,209,211}          // ��� ���� ������-�������
      aFColor  := RED
      aBtnBClr := { {222, 14, 32} }
      aBtnFClr := { YELLOW }             // ���� ����� ������
   ELSEIF cType == "EXCLAM"
      cIcon    := "iMgExclam128"
      aBColor  := { 238, 249, 142 }      // ��� ���� ������-�����
      aFColor  := { 174, 134,  33 }
      aBtnBClr := { {254,184,1}   }      // ���� ���� ������
      aBtnFClr := { BLACK }              // ���� ����� ������
   ELSEIF cType == "YESNO" .OR. cType == "YN" .OR. ;
          cType == "RC"  .OR. cType == "RETRYCANCEL"
      cIcon    := "iMgQuest128"
      aBColor  := { 251, 225, 170 }                 // ��� ���� ������-���������
      aBtnBClr := { LGREEN , {222, 14, 32} }        // ���� ���� ������
      aBtnFClr := { YELLOW , YELLOW        }        // ���� ����� ������
      // ������� ���������� ������: 1-Yes, 2-No
      aBtnTxt  := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21] }
      IF cType == "RC"  .OR. cType == "RETRYCANCEL"
         aBtnTxt := { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] }
      ENDIF
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aBtnTxt := { "Button-1" , "Button-2" }
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
         NEXT
      ENDIF
      aPost    := { 80, 98 }
   ELSEIF cType == "YESNOCANCEL" .OR. cType == "YNCANCEL" .OR. cType == "YNC"
      cIcon    := "iMgQuest128"
      aBtnBClr := { LGREEN , {66, 92,251}, {222, 14, 32} }    // ���� ���� ������
      aBtnFClr := { YELLOW , YELLOW      , YELLOW }           // ���� ����� ������
      aBtnTxt  := { '&' + _HMG_aABMLangLabel[20], '&' + _HMG_aABMLangLabel[21], '&' + _HMG_aABMLangButton[13] }
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aBtnTxt := { "Button-1" , "Button-2", "Button-3" }
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
         NEXT
      ENDIF
      aPost := { 80, 80, 98 }
   ELSE
      cIcon := cType          // ������ = cType
      DEFAULT cIcon := "iMgNone128"
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aPost    := {}
         aBtnFClr := {}
         aBtnBClr := {}
         aBtnTxt  := ARRAY(LEN(aButton))
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
            nPost := IIF(nI == LEN(aButton),98,80)
            AADD(aPost,nPost)
            AADD(aBtnFClr, YELLOW  )
            AADD(aBtnBClr, LGREEN  )
         NEXT
         aBtnBClr[LEN(aButton)] := {222, 14, 32}  // ���� ���� ������
      ENDIF
      // ���� ������ �� oWin
      IF IsArray(oWin:aBtnFClr)
         IF LEN(oWin:aBtnFClr) # LEN(aBtnTxt)
            cMsg := "ERROR ! LEN(oWin:aBtnFClr) # LEN(aBtnTxt) !;;"
            cMsg += ProcNL()+";"+ProcNL(1)
            AlertStop(cMsg, , , 64)
         ENDIF
         aBtnFClr := oWin:aBtnFClr
      ENDIF
      IF IsArray(oWin:aBtnBClr)
         IF LEN(oWin:aBtnBClr) # LEN(aBtnTxt)
            cMsg := "ERROR ! LEN(oWin:aBtnBClr) # LEN(aBtnTxt) !;;"
            cMsg += ProcNL()+";"+ProcNL(1)
            AlertStop(cMsg, , , 64)
         ENDIF
         aBtnBClr := oWin:aBtnBClr
      ENDIF
   ENDIF

   IF Hb_LangSelect() == "ru.RU1251"
      cTitle := IIF( LEN(cTitle) == 0, "�������� !", cTitle )
   ELSE
      cTitle := IIF( LEN(cTitle) == 0, "ATTENTION!", cTitle )
   ENDIF
   // --------- ��������� ��� �������� ----------
   nHhelp    := 0
   cHelp     := oWin:cHelp      ; Default cHelp := ""
   aHelpBClr := oWin:aHelpBClr  ; Default aHelpBClr := aBColor
   aHelpFClr := oWin:aHelpFClr  ; Default aHelpFClr := aFColor
   IF LEN(cHelp) > 0
      nLine  := NumAt( CRLF , cHelp ) + 1
      nHhelp := nLine * nFontSize * 2
   ENDIF

   // --------- ������ ������ ----------
   nW2 := 0
   FOR nI := 1 TO LEN(aBtnTxt)
      cVal  := aBtnTxt[nI]
      IF LEN(cVal) == 3
         cVal += "HHHHHHHHH"
      ELSE
         cVal += "HHHHHH"
      ENDIF
      nWTxt := GetTxtWidth( cVal, nFontSize, cFont, lBold )  // �������� Width ������
      nW2   := MAX(nWTxt,nW2)
   NEXT

   nBtnH := 55      // ������ ������
   nBtnW := nW2     // ������ ������
   nG    := 20      // �������
   // ������ ������ �� ����� ������� ������ � �������
   aWHDim := MaxSize_Tsb(aXDim, o1Brw, nFontSize, cFont, lBold )
   nWDim  := aWHDim[1]                    // ������ �������
   nWDim  += GetScrollBarSize() + 40 + 50 // 40 - ��������� �����
   nHDim  := aWHDim[2]                    // ������ ������� - ���������
   nHDim  += nFontSize * 2 * 2            // ����� + ������ �������
   //? ProcNL(), "nWDim=" ,nWDim, "nHDim=", nHDim
   nW2   := nG*3 + nIcoSize + nWDim + GetBorderWidth()  // ������ ����
   nW    := IIF( nW2 > System.DesktopWidth*0.96, System.DesktopWidth*0.96, nW2 )
   nWFlg := IIF( nW2 > System.DesktopWidth*0.96, 1, 0 )
   nH2   := nHDim + nBtnH + nG*3
   nH2   += nHhelp + IIF(nHhelp==0,0,4)  // ������ ��������� ��� ��������
   nH2   := IIF( nH2 < nIcoSize + nBtnH + nG*3, nIcoSize + nBtnH + nG*3, nH2 )
   nH2   += GetTitleHeight() + GetBorderHeight()
   //?? nH2
   nHDsk := App.Cargo:aDisplayMode[2]  //{ Sys.ClientWidth , Sys.ClientHeight }
   nH    := IIF( nH2 > nHDsk*0.95, nHDsk*0.95, nH2 )
   //?? "nH=",nH

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH   ;
      TITLE cTitle                                     ;
      MODAL NOSIZE                                     ;
      FONT cFont SIZE nFontSize                        ;
      BACKCOLOR aBColor                                ;
      ON INIT     {|| _wPost( 0) }                     ;
      ON RELEASE  {|| _wSend(90) }

      This.Cargo := oHmgData()
      This.Cargo:hWndIsx := hWndIsx
      This.Cargo:aBtnTxt := aBtnTxt

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG
      nX := nG

      @ 0, 0 LABEL Buff PARENT &cForm WIDTH 10 HEIGHT 10 VALUE '' TRANSPARENT

      DRAW ICON IN WINDOW &cForm AT nY, nX PICTURE cIcon WIDTH nIcoSize HEIGHT nIcoSize COLOR aBColor

      nX    += nIcoSize + nG
      IF nWFlg == 1   // ������� �� ��������� ������
         nWTsb := nW - nIcoSize - nG*3
      ELSE
         nWTsb := nWDim
      ENDIF
      nHTsb := nH - nG*3 - nBtnH - nHhelp - IIF(nHhelp==0,0,4)  // ������ ��������� ��� ��������

      //@ nY, nX LABEL Label_Tsb PARENT &cForm WIDTH nWTsb HEIGHT nHTsb ;
      //  VALUE 'Table' BACKCOLOR aBColor FONTCOLOR aFColor BORDER
      /////////////////////// ������� ///////////////////////////////////////////////////////
      oTsb := TsbPatam(cForm,aXDim, "oBrw", aBColor,o1Brw)
      // ������� � ���������� \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDim, "oBrw", nY, nX, nWTsb, nHTsb )
      //
      oBrw:Cargo:nModify := 0     // ������� ���������
      This.Cargo:oBrw    := oBrw

      /////////////////////// ��������� ��� �������� //////////////////////////////////////
      IF LEN(cHelp) > 0
         nI := nY + nHTsb + 2
         @ nI, nX LABEL Help PARENT &cForm WIDTH nWTsb HEIGHT nHhelp VALUE cHelp ;
           FONTCOLOR aHelpFClr BACKCOLOR aHelpBClr FONT cFontHlp SIZE nFSizeHlp BOLD
         nHTsb += nHhelp + 2*2
      ENDIF

      /////////////////////// ������ ����� ����� ///////////////////////////////////////////
      nY := nH - nBtnH - nG
      nX := nW - nBtnW - nG

      FOR nI := LEN(aBtnTxt) TO 1 STEP - 1
         cN := "Btn_" + StrZero(nI,2)
         cN := IIF( nI == LEN(aBtnTxt) , "Btn_Exit", cN )
         @ nY, nX BUTTONEX &cN PARENT &cForm                     ;
           WIDTH nBtnW HEIGHT nBtnH CAPTION aBtnTxt[nI] ICON Nil ;
           NOHOTLIGHT NOXPSTYLE HANDCURSOR                       ;
           FONT aBtnFont[1] SIZE aBtnFont[2] BOLD                ;
           FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[nI]          ;
           ON MOUSEHOVER ( This.Backcolor := aBtnBClr2  , This.Fontcolor := aBtnFClr2   ) ; // ��������� ���� ���� � ����� ������
           ON MOUSELEAVE ( This.Backcolor := This.Cargo:aBClr, This.Fontcolor := This.Cargo:aFClr ) ;
           ACTION {|| This.Enabled := .F., _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) }

           This.&(cN).Cargo := oHmgData()
           WITH OBJECT This.&(cN).Cargo
              :nBtn     := nI           // ����� ������� ������
              :nPost    := aPost[nI]
              :cCapt    := aBtnTxt[nI]
              :aBClr    := aBtnBClr[nI]
              :aFClr    := aBtnFClr[nI]
              :cObj     := cN
           END WITH
         nX -= nBtnW + nG
      NEXT

      ////////////////////////// ���.��������� ��� ����, oBrw, oTsb /////////////////////
      cInitForm := Valtype(bInitForm)
      IF HB_ISCHAR( bInitForm )
         IF ! ( "|...|" $ bInitForm .and. "(...)" $ bInitForm )
            bInitForm := "{|...| "+bInitForm+"(...)"
         ENDIF
         cInitForm := bInitForm
         bInitForm := &( bInitForm )
      ENDIF

      lInitForm := .F.
      IF HB_ISBLOCK( bInitForm )
         BEGIN SEQUENCE WITH { |e|break(e) }
            EVal( bInitForm, This.Object, This.Cargo:oBrw, oTsb )
            lInitForm := .T.
         END SEQUENCE
         IF ! lInitForm
            cMsg := "ERROR ! "
            cMsg += "Must be a block of code !"
            cMsg += " cInitForm=" + cInitForm
            MsgDebug(cMsg, bInitForm)
         ENDIF
      ENDIF

      (This.Object):Event( 0, {|ow| // ON INIT
                                    Local ob := ow:Cargo:oBrw
                                    This.Topmost := .T.
                                    ob:Setfocus()
                                    DO EVENTS
                                    Return Nil
                                    })

      (This.Object):Event(80, {|ow,ky,cn,ob,nAt| ob  := ow:Cargo:oBrw ,;
                                             nAt := ow:Cargo:oBrw:nAt ,;  // ����� ������ � ���
                                             aRet := {This.&(cn).Cargo:nBtn, This.&(cn).Cargo:cCapt, ob:aArray, nAt, ky} ,;
                                             ow:Release() } )

      (This.Object):Event(90, {|ow| // ON RELEASE
                                    // ��������� �� ���������� ����
                                    ? "---- ON RELEASE ----", ow:Name, ProcNL()
                                    ? "---- aRet=",aRet ; ?v aRet
                                    If ow:Cargo:hWndIsx > 0
                                       SwitchToThisWindow(hWndIsx)
                                    Endif
                                    DO EVENTS
                                    Return Nil
                                    })

      (This.Object):Event(98, {|ow,ky,cn,ob| ob := ow:Cargo:oBrw,;
                                             aRet := {} ,;
                                             ky := cn := ob ,;
                                             ow:Release() } )

      ON KEY F1     OF &cForm ACTION NIL
      ON KEY ESCAPE OF &cForm ACTION {|| _wPost(98,cForm) }

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm //ON INIT {|| This.Minimize, DoEvents(), This.Restore }

RETURN aRet

//////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam(cForm,aXDim,cBrw,aBColor,o1Brw)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aHead, nI

   oTsb := o1Brw       ;   Default oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- ����������� ��� !!!
   oTsb:cFormName      := cForm      // ��� ���
   oTsb:lNoPicture     := .T.
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   IF IsArray(oTsb:aNumber) .AND. LEN(oTsb:aNumber) == 0
      oTsb:aNumber     := NIL
   ELSE
      oTsb:aNumber     := { 1, 30 }
   ENDIF
   //oTsb:uSelector    := 20
   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // ��������� � ������� ������
      oTsb:aFoot       := .T.                            // ��������� ������
   ENDIF

   nHFnt               := App.Cargo:nFontSize * 1.8
   oTsb:nHeightCell    := 28                             // ������ ����� = ������ �������� ��������
   oTsb:nHeightHead    := nHFnt                          // ������ �����
   oTsb:nHeightFoot    := nHFnt                          // ������ �������
   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // �� ��������� � ������� ���������
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // ������ ����������
   ENDIF

   //oTsb:lSuperHd      :=             // ��������� � ������� ����������
   //oTsb:cSuperHd      :=
   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      oTsb:nHeightSuperHd := 24                          // ������ �����������
   ENDIF
   nClr1 := HMG_RGB2n(aBColor)     // ���� ���� �����+������
   nClr2 := RGB( 48, 29,26)        // ����-������ ���
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // ����: ����� � ��� �����������
   oTsb:aBrush         := {240,240,240}                     // ���� ���� ��� ��������

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
   oTsb:lZebra    := .T.
   oTsb:aZebra    := { {230,230,230}, SILVER }

   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF

RETURN oTsb

/////////////////////////////////////////////////////////////////////////////////////
// ������ ������ �� ����� ������� ������
STATIC FUNCTION MaxSize_Tsb(aXDim, o1Brw, nFontSize, cFont, lBold )
   LOCAL aDim, aLen, nWTxt, nHTxt, nI, nJ, cVal, nHFnt, lSize, aHide, nS, cMsk

   //? ProcNL()
   aHide := o1Brw:aHideCol // { 4, 5, 6 }   // ������ �������
   IF UPPER("DejaVu") $ UPPER(cFont)
      cMsk := "HHHHH"  // ��� DejaVu Sans Mono
   ELSE
      cMsk := "HHH"    // ��� Arial � ��.�����
   ENDIF

   nWTxt := 0
   aLen  := ARRAY(LEN(aXDim[1]))
   AFILL(aLen, 0)
   FOR nI := 1 TO LEN(aXDim)
      aDim  := aXDim[nI]
      FOR nJ := 1 TO LEN(aDim)
         cVal  := aDim[nJ]
         lSize := .T.
         IF IsArray(aHide) .AND. LEN(aHide) > 0
            FOR nS := 1 TO LEN(aHide)
               IF nJ == aHide[nS]     // ������� ��������
                  lSize := .F.
                  EXIT
               ENDIF
            NEXT
         ENDIF
         IF lSize
            IF VALTYPE(cVal) # "C"
               cVal := cValToChar(cVal)
            ENDIF
            cVal += cMsk
            nWTxt    := GetTxtWidth( cVal, nFontSize, cFont, lBold )  // �������� Width ������
            aLen[nJ] := MAX(nWTxt, aLen[nJ])
         ENDIF
      NEXT
   NEXT
   nWTxt := 0
   FOR nI := 1 TO LEN(aLen)
      nWTxt += aLen[nI]
      //? SPACE(5)+".",nI, aLen[nI]
   NEXT
   nHTxt := LEN(aXDim) * 28
   IF IsLogic(o1Brw:lSpecHd) .AND. o1Brw:lSpecHd
      nHTxt += 12                        // ������ ���������� �������
   ENDIF
   IF IsLogic(o1Brw:lSuperHd) .AND. o1Brw:lSuperHd
      nHTxt += 30                        // ������ �����������
   ENDIF

   IF IsLogic(o1Brw:lFooting) .AND. !o1Brw:lFooting
      nHFnt := App.Cargo:nFontSize * 1.8    // ������ �������
      nHTxt -= nHFnt
   ENDIF

   // ������ ����� - ������ ����� �������
   IF IsNumeric(o1Brw:nHeightHead) .AND. o1Brw:nHeightHead == 0
      nHFnt := App.Cargo:nFontSize * 1.8   // ������ �����
      nHTxt -= nHFnt
   ENDIF

Return { nWTxt, nHTxt }

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
FUNCTION myAlertTsbEdit( oBrw )
   LOCAL oCol, cCol //, nI

   //? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      //nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ��.����
      oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> ��.����
      //oCol:lEdit := .T.
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ob:aArray[ob:nAt][4], ob:aArray[ob:nAt][5], ob:aArray[ob:nAt][6]
//  1                     2                               3              4         5      6
//{ "" , "Disable/enable display of cancelled applica:", .F.         , "lZa1"    , "L"  , ""   } )
//{ "" , "Example of displaying a menu selection with:", REPL("-?",3), "aDogA"   , "ARR", HB_ValToExp(aDog) } )
//{ "" , "Date and time of contract control:"          , cDT         , "cDTDg"   , "DT" , ""   } )
//{ "" , "Selection window - button font:"             , REPL("?",5) , "aFont"   , "FNT", ""     } )
//{ "" , "Selection window - button font color:"       , SPACE(5)    , "aBntFClr", "CLR", HB_ValToExp(RED)    } )
//{ "" , "Selection window - button background color:" , SPACE(5)    , "aBntBClr", "CLR", HB_ValToExp(YELLOW) } )
////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, aFnt, aClr
   LOCAL cTyp, cMsg, lWrt, cStr, xVal, aDim, cRet, aRet

   WITH OBJECT ob
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // ��� ��������� �������
      cNam  := oCol:cName
      cJTyp := ob:aArray[ob:nAt][5]  // ��� ��������� ������
      Default cJTyp := "+"
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cTyp
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   cStr += 'Column array cJTyp: "' + cJTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .T.        // ���������� ����

   IF LEN(cJTyp) == 0             // ��� ���������
      lRet := .F.                 // �� ������ ������������� ���� � :get
   ELSEIF cJTyp $ "NLCD"
      lRet := .T.                 // ������������� ���� � :get
   ELSEIF cJTyp $ "DMN"
      cTyp := "D"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
      ENDIF
      lRet := .F.                 // �� ������ ������������� ���� � :get
   ELSEIF cJTyp $ "DT"
      cTyp := "T"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
      ENDIF
      lRet := .F.                 // �� ������ ������������� ���� � :get
   ELSEIF cJTyp $ "CLR"
      xVal := ob:aArray[ob:nAt][6]
      aClr := myStrToArray( xVal )
      aRet := Tsb_ChangeColor(ob,aFnt) // ��.����
      IF LEN(aRet) > 0
         ob:aArray[ob:nAt][6] := HB_ValToExp(aRet)
         uVal := SPACE(5)
      ENDIF
      lRet := .F.                 // �� ������ ������������� ���� � :get
   ELSEIF cJTyp $ "FNT"
      aFnt := myStrToArray( "{" + uVal + "}" )
      aRet := Tsb_ChangeFont(ob,aFnt) // ��.����
      IF LEN(aRet) > 0
         cRet := HB_ValToExp(aRet)
         uVal := CHARREM( '{}', cRet )
      ENDIF
      lRet := .F.                  // �� ������ ������������� ���� � :get
   ELSEIF cJTyp $ "ARR"
      xVal := ob:aArray[ob:nAt][6]
      xVal := ALLTRIM(xVal)
      aDim := myStrToArray( xVal )
      cRet := Tsb_ContexMenu(ob,aDim)  // ��.����
      IF LEN(cRet) > 0
         uVal := cRet
      ENDIF
      lRet := .F.                      // �� ������ ������������� ���� � :get
   ELSE
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
      lWrt := .F.             // �� ���������� � ������
      lRet := .F.             // �� ������ ������������� ���� � :get
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                         // ���������� ������
      ob:Cargo:nModify ++          // �������-��������� � �������
      ob:SetValue(nCol,uVal)
   ENDIF
   ob:DrawSelect()    // ������������ ������� ������ �������
   ob:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, ob )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod, cJTyp
   LOCAL oWnd  := _WindowObj(ob:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT ob
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // ��� ��������� �������
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
      cJTyp := ob:aArray[ob:nAt][5]  // ��� ��������� ������
      Default cJTyp := "+"
   END WITH

   ? ProcNL(), nCol, cTyp
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   //cStr += ';Column processing type: "' + cTyp + '" ;'
   cStr += ';Column array cJTyp: "' + cJTyp + '" ;'

   IF cJTyp $ "CNDL"
      // ����������� ���������
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF
   ob:DrawSelect()    // ������������ ������� ������ �������
   ob:SetFocus()

   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
// CLR_PANE  , {|nr,nc,ob| // 2 , ���� � ������� �������
FUNCTION myAlertTsbColorBack(nr,nc,ob)
   Local nClr, aZebra, cType, nClr6, aColor, xVal, nBack
   // ob:aArray[ob:nAt][4], ob:aArray[ob:nAt][5], ob:aArray[ob:nAt][6]
   //  1                     2                                 3           4          5       6
   //{ "" , "Selection window - button font:"             , REPL("?",5), "aFont"   , "FNT", ""                  } )
   //{ "" , "Selection window - button font color:"       , SPACE(5)   , "aBntFClr", "CLR", HB_ValToExp(RED)    } )
   //{ "" , "Selection window - button background color:" , SPACE(5)   , "aBntBClr", "CLR", HB_ValToExp(YELLOW) } )
   //? ProcNL(), "nAt=", nr, "nCol=", nc

   nBack := GetSysColor( COLOR_BTNFACE )   // ���� ���� �������
   // ������ �������� �� ������ ����������� �������
   // remove highlight on the right virtual column
   IF IsObject( ob:oPhant ) ;  ob:oPhant:nClrBack := nBack
   ENDIF

   nr := nc  // nr - ����� �� ��������� - ��� ob:nAt
   aZebra := ob:Cargo:aZebra
   //? ProcNL(), "aZebra=",aZebra
   IF !IsArray(aZebra)
      aZebra := { HMG_RGB2n({230,230,230}), HMG_RGB2n(SILVER) }
   ENDIF

   IF LEN(ob:aArray[ob:nAt]) == 6
      cType := ob:aArray[ob:nAt][5]
      IF !IsString(cType) ; cType := "NOT5"
      ENDIF
      xVal := ob:aArray[ob:nAt][6]
      If cType == "CLR"
         xVal   := ALLTRIM(xVal)
         aColor := myStrToArray( xVal )
         IF !IsArray(aColor) ; aColor := BLACK
         ENDIF
         nClr6 := HMG_RGB2n(aColor)
      Endif
   ELSE
      cType := "NOT5"
      nClr6 := HMG_RGB2n(MAROON)
   ENDIF

   nClr := CLR_HRED
   If ob:nAt % 2 == 0
      nClr := aZebra[2]  // ������ % 2
   Else
      nClr := aZebra[1]  // ���� ���� �������
   Endif

   If cType == "CLR"
      nClr := nClr6
   Endif

RETURN nClr

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ContexMenu(oBrw, aDim)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, cRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, cImg, lIcon, nMenu
   LOCAL aFont, nFSize, cName, nWCell, nHCell, oCell

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

   hFont1 := GetFontHandle("ComSanMS")
   hFont2 := GetFontHandle("DlgFont")
   aFont  := GetFontParam("DlgFont")
   nFSize := aFont[2]
   nMenu  := 0
   lIcon  := .T.   // ������ � ���� - ������

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cImg    := ""
         cMenu   := aDim[nI]
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
      SEPARATOR
      MENUITEM  "Delete value" ACTION  {|| nMenu := -1 } FONT hFont2
      SEPARATOR
      MENUITEM  "Exit"  ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      cRet := "(" + HB_NtoS(nMenu)+")-("
      cRet += aDim[nMenu] + ")"
   ELSEIF nMenu == -1
      cRet := "-"
   ELSE
      cRet := ""
   ENDIF

   DO EVENTS

RETURN cRet

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ChangeFont(oBrw,aFnt)
   LOCAL aFont, hFont, aF, cForm, aRet := {}

   cForm := oBrw:cParentWnd
   hFont := GetFontHandle( "TsbEdit" )
   aF    := GetFontParam( hFont )

   IF LEN(aFnt) == 0
      aFnt := aF
   ENDIF

   // ������� ����������� ���� ����� ��� Windows
   aFont := GetFont( aFnt[1], aFnt[2], aFnt[3], aFnt[4], {0,0,0} , .f. , .f. , 0 )
   if ! empty ( aFont[1] )
      aRet := { aFont[1], aFont[2], aFont[3], aFnt[4] }
   EndIf

RETURN aRet

///////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ChangeColor(oBrw, aClr)
   LOCAL aColor := {}, cForm

   cForm := oBrw:cParentWnd
   // ������� ����������� WINDOWS ���� �����
   aColor := GetColor(aClr)
   IF aColor[1] # NIL
      aClr  := aColor
   ENDIF

RETURN aClr

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight //- 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH

   //? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet)
   cForm := oBrw:cParentWnd
   oJWnd := _WindowObj(oBrw:cParentWnd)

   nY    += oJWnd:Row - 5
   nX    += oJWnd:Col + 7
   IF oBrw:lDrawSpecHd
      nY -= oBrw:nHeightSpecHd    // ������ ���������� ENUMERATOR
   ENDIF

   nY     += IIF( App.Cargo:aDisplayMode[2] <= 720, 8, 4 )
   nHCell += IIF( App.Cargo:aDisplayMode[2] <= 720, 3, 0 )

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

         IF VALTYPE(xGet) == "C"
            xGet := CTOD(xGet)
         ELSEIF VALTYPE(xGet) == "D"
         ELSE
            xGet := CTOD('')
         ENDIF
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
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         IF IsString(xGet)
            tDTime := hb_CToT(xGet)
         ELSE
            tDTime := xGet
         ENDIF
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
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

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

///////////////////////////////////////////////////////////////////////////////
FUNCTION myStrToArray( cBuf )
   LOCAL aBuf, lBuf

   IF left(cBuf, 3) == ["{"]
      cBuf := subs(cBuf, 2)
   ENDIF
   IF right(cBuf, 3) == ["}"]
      cBuf := left(cBuf, Len(cBuf)-1)
   ENDIF
   lBuf := .F.
   IF left(cBuf, 1) == "{" .and. right(cBuf, 1) == "}"
      BEGIN SEQUENCE WITH {|e| break( e ) }
         aBuf := &(cBuf)
         lBuf := .T.
      END SEQUENCE
   ELSE
      aBuf := {}
   ENDIF
   IF !lBuf
      aBuf := {}
   ENDIF

RETURN aBuf

///////////////////////////////////////////////////////////////////////////////
// �������� Width ������
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;
           nFontSize := _HMG_DefaultFontSize,  ;
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ������ ������
   DeleteObject (hFont)

RETURN nWidth
