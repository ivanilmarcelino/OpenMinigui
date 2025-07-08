/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ���� �������� ����� Dbf  / Dbf file opening menu
 * ������ �������� ���������� �� ���� / Methods for returning parameters from a window
*/

#define _HMG_OUTLOG
#include "minigui.ch"

////////////////////////////////////////////////////////////////////////
// �������� ���� � ����� ������� TSBROWSE - ����������� �������
// ����� � ���������� ������
FUNCTION OpenFileView(aUse,oWnd)
   LOCAL cFile, cAls, lModeUse, cRddDbf, cCdPg, cPsw, ao, cForm
   LOCAL cMsg, cObjTsb, aObjLbl, lRet, nI, cSetDel, oBrw, cMs2

   ao      := App.Cargo
   aObjLbl := oWnd:Cargo:aLblUp           // ������ ���������
   cObjTsb := oWnd:Cargo:cObjTsb          // ������-�������
   cForm   := oWnd:Name

   IF IsArray(aUse)
      IF LEN(aUse) > 0

         cFile    := aUse[1]
         lModeUse := aUse[2]  // .F. - EXCLUSIVE
         cRddDbf  := aUse[3]
         cCdPg    := aUse[4]
         cPsw     := aUse[5]
         cSetDel  := aUse[6]
         cAls     := "Als_" + cFileNoExt(cFile)
         cAls     := CharRem('()|-.',cAls)           // ������� � ������ �����
         cAls     := mySetAlias(cAls)                // �������������� ������ ���� - ������� ����� � ��������
         oWnd:Cargo:aUse  := aUse                    // ������� �� ����� �������� ����
         oWnd:Cargo:cCdPg := cCdPg                   // ������� �� ����� CodePage �����
         App.Cargo:aUse   := aUse                    // �������� ����� ���� �������

         ? SPACE(5) + ProcNL(), HB_ValToExp(aUse)

         IF cSetDel == "ON" ;   SET DELETED ON
         ELSE               ;   SET DELETED OFF
         ENDIF

         lRet := myUse3Area( cFile, cAls, lModeUse, cRddDbf, cCdPg, cPsw, 1/*number of attempts*/)   // -> util_dbf.prg
         IF ! lRet
            cMs2 := IIF( ao:cLang == "RU", "��� ���� �� �������������� ���� ��������� !",;
                                           "OR FILE is not supported by this driver!" ) 
            cMsg := IIF( ao:cLang == "RU", "������ �������� �� !;", "ERROR opening database !;" ) 
            cMsg += IIF( ao:cLang == "RU", "���� ����� ���� ����� ������ ���������",;
                                "File may be in use by another process." ) 
            cMsg += ";" + cFile + ";;" 
            AlertStop( cMsg + cMs2 + ";;" + ProcNL(), , "ZZZ_B_STOP64", 64 )
         ELSE
            // ���� ��������� �c�����
            ? SPACE(5) + ProcNL(), "����� �������� !!!", OrdCount(), Alias(), "Database: ", cFile
            If OrdCount() > 0
               OrdSetFocus(1)
            EndIf
            DbGotop()
            //
            oBrw := Tsb_ViewDbf(aUse,oWnd)       // -> tsb_ViewDbf.prg  �����/����� ���
            // ���������� ��������� ������ �������
            //oWnd:Cargo:oBrw   := oBrw            // ��������� �� ����  --> ������ � tsb_ViewDbf.prg

            SetProperty(cForm, cObjTsb, "Visible", .F.)   // ������ ������-�������
            FOR nI := 1 TO LEN(aObjLbl)
               SetProperty(cForm, aObjLbl[nI], "Visible", .T.)   // ����� ������ ���������
            NEXT
            // ����� ����� �����
            SetProperty(cForm, aObjLbl[2], "Value", cFile)   
            oWnd:Cargo:aLbSay[2] := cFile
            // ����� 3-������ ���������
            SetProperty(cForm, aObjLbl[3], "Value", oWnd:Cargo:aLbSay[3])   
         ENDIF

      ENDIF
   ELSE
      cMsg := "ERROR in file selection parameters!;"
      cMsg += "Skip opening file !;;" +ProcNL()
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      SetProperty(cForm, aObjLbl[1], "Visible", .T.)   // ����� ������ ���������
      SetProperty(cForm, aObjLbl[2], "Visible", .T.)   // ����� ������ ���������
   ENDIF
   // RELEASE aPubOpenRetUse  - ������� 1 �������� ���� � ����������
   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
// �������� ���� � ����� ������� TSBROWSE
FUNCTION Menu2OpenFile(oWnd,nKy,cn,oBrw)
   LOCAL aUse, cFile, cAls, lModeUse, cRddDbf, cCdPg, cPsw, cMsg, cSetDel
   LOCAL cBrw, cForm, cObjTsb, aObjLbl, lRet, nI, cMs2, ao

   ? ProcNL(), oWnd, nKy, cn, oWnd:Name, oBrw, oWnd:Cargo:oBrw
   // ���� �������� ��������� ������
   //oBrw := "������ ������� ������� / Future table object"

   ao      := App.Cargo
   aObjLbl := oWnd:Cargo:aLblUp           // ������ ���������
   cObjTsb := oWnd:Cargo:cObjTsb          // ������-�������
   cForm   := oWnd:Name
   cAls    := Alias()
   IF LEN(cAls) > 0
      // ������ ��� ���� �������� ���� -  ������� �
      ? SPACE(5) + cAls, "CLOSE !!!", ISOBJECT(oWnd:Cargo:oBrw)
      // ������ ������ ��� �� ����� !!!
      IF ISOBJECT(oWnd:Cargo:oBrw)
         cBrw  := oWnd:Cargo:oBrw:cControlName
         cForm := oWnd:Cargo:oBrw:cParentWnd
         ?? cBrw, cForm
         IF _IsControlDefined(cBrw, cForm)
            ?? "Release: ",cBrw
            DoMethod( cForm, cBrw, "Release" )
         ENDIF
      ENDIF
      (cAls)->( dbCloseArea() )
      SetProperty(cForm, cObjTsb, "Visible", .T.)   // ����� ���� ������-�������
      FOR nI := 1 TO LEN(aObjLbl)
          SetProperty(cForm, aObjLbl[nI], "Visible", .F.)   // ������ ������ ���������
      NEXT
      // ��������� ������ �������� ����
      SetProperty(cForm, "_DBase" , "Enabled", .F.)
      SetProperty(cForm, "_Filter", "Enabled", .F.)
      SetProperty(cForm, "_Export", "Enabled", .F.)
      DO EVENTS
      oBrw := "����� ������ ������� ������� / New future table object"
   ENDIF

   // PUBLIC aPubOpenRetUse  := {} - ������� 1 �������� ���� � ����������
   // App.Cargo:aOpenRetUse  := {} - ������� 2 �������� ���� � ����������
   // oWnd:Cargo:aOpenRetUse := {} - ������� 3 �������� ���� � ����������
   // ����� �����
   //    Form_OpenFileDbf(oWnd)
   //    aUse := aPubOpenRetUse ���  App.Cargo:aOpenRetUse ��� oWnd:Cargo:aOpenRetUse
   // ����� � ���� ������������ ������ �����, ��. ����

   aUse := Form_OpenFileDbf(oWnd)  // ������� - ������� 3
   IF IsArray(aUse)
      IF LEN(aUse) > 0

         cFile    := aUse[1]
         lModeUse := aUse[2]  // .F. - EXCLUSIVE
         cRddDbf  := aUse[3]
         cCdPg    := aUse[4]
         cPsw     := aUse[5]
         cSetDel  := aUse[6]
         cAls     := "Als_" + cFileNoExt(cFile)
         cAls     := CharRem('()|-.',cAls)           // ������� � ������ �����
         cAls     := mySetAlias(cAls)                // �������������� ������ ���� - ������� ����� � ��������

         oWnd:Cargo:aUse  := aUse                    // ������� �� ����� �������� ����
         oWnd:Cargo:cCdPg := cCdPg                   // ������� �� ����� CodePage �����
         App.Cargo:aUse   := aUse                    // �������� ����� ���� �������

         ? SPACE(5) + ProcNL(), HB_ValToExp(aUse)

         IF cSetDel == "ON" ;   SET DELETED ON
         ELSE               ;   SET DELETED OFF
         ENDIF

         lRet := myUse3Area( cFile, cAls, lModeUse, cRddDbf, cCdPg, cPsw, 1 /*number of attempts*/)   // -> util_dbf.prg
         IF ! lRet
            cMs2 := IIF( ao:cLang == "RU", "��� ���� �� �������������� ���� ��������� !",;
                                           "OR FILE is not supported by this driver!" ) 
            cMsg := IIF( ao:cLang == "RU", "������ �������� �� !;", "ERROR opening database !;" ) 
            cMsg += IIF( ao:cLang == "RU", "���� ����� ���� ����� ������ ���������",;
                                "File may be in use by another process." ) 
            cMsg += ";" + cFile + ";;" + cMs2 + ";;" + ProcNL()
            AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
         ELSE
            // ���� ��������� �c�����
            ? SPACE(5) + ProcNL(), "����� �������� !!!", OrdCount(), Alias(), "Database: ", cFile
            If OrdCount() > 0
               OrdSetFocus(1)
            EndIf
            DbGotop()
            //
            oBrw := Tsb_ViewDbf(aUse,oWnd)       // -> tsb_ViewDbf.prg  �����/����� ���
            // ���������� ��������� ������ �������
            cBrw  := oBrw:cControlName
            cForm := oBrw:cParentWnd
            IF _IsControlDefined(cBrw, cForm)
               //MsgDebug(cBrw, cForm)
               // ������������ ������ �������� ����
               SetProperty(cForm, "_DBase", "Enabled", .T.)
               SetProperty(cForm, "_Filter", "Enabled", .T.)
               SetProperty(cForm, "_Export", "Enabled", .T.)
            ENDIF
            SetProperty(cForm, cObjTsb, "Visible", .F.)   // ������ ������-�������
            FOR nI := 1 TO LEN(aObjLbl)
                SetProperty(cForm, aObjLbl[nI], "Visible", .T.)   // ����� ������ ���������
            NEXT
            // ����� ����� �����
            SetProperty(cForm, aObjLbl[2], "Value", cFile)   
            oWnd:Cargo:aLbSay[2] := cFile
            // ����� 3-������ ���������
            SetProperty(cForm, aObjLbl[3], "Value", oWnd:Cargo:aLbSay[3])   
         ENDIF

      ENDIF
   ELSE
      cMsg := "ERROR in file selection parameters!;"
      cMsg += "Skip opening file !;;" +ProcNL()
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      SetProperty(cForm, aObjLbl[1], "Visible", .T.)   // ����� ������ ���������
      SetProperty(cForm, aObjLbl[2], "Visible", .T.)   // ����� ������ ���������
   ENDIF
   // RELEASE aPubOpenRetUse  - ������� 1 �������� ���� � ����������
   DO EVENTS

RETURN oBrw

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Form_OpenFileDbf(oWnd)
   LOCAL cTitle, cIcon, cFont, nFontSize, aBackColor, aBColorTxt
   LOCAL nY, nX, nW, nH, nG, nWTxt, nHTxt, nHBtn, nWBtn, nYBtn, nXBtn
   LOCAL nX2, nGBtn, nYCmb, nXCmb, nWCmb, ao, owc, oDlu, nHBtnDir
   LOCAL cBtnCapt, cShared, lShared, aBtnFont, aBtnFClr
   LOCAL cFile, cCdPg, cDrvDbf, aUseMode, cBtnDrv, aSetDel, cSetDelete
   LOCAL nWBtn2, nWBtnDir, nLR, aFontIni, cVal, ahIco
   LOCAL cPsw, aRet, cFontBtn, nHIco, cTxt, aBtn, bAct

   oWnd:Cargo:aRet := {}                 // ������� �������� ���� - ������� 3
   ao         := (App.Cargo)
   aFontIni   := ao:oIni:MAIN:COMSANMS   ; Default aFontIni := { "Tahona" , 14 , .F., .F. }
   cFontBtn   := aFontIni[1]
   nFontSize  := aFontIni[2]
   aBackColor := oWnd:Cargo:aBColor      // ���� ���� �����
   aBColorTxt := {255,255,240}
   cTitle     := IIF(ao:cLang == "RU", "������� ����", "Open database" )
   cIcon      := "i1FindFile48"
   cFont      := "DejaVu Sans Mono"
   cDrvDbf    := "DBFCDX"
   cCdPg      := "RU1251"
   cSetDelete := "OFF"
   cShared    := cPsw := ""
   aRet       := {}               // ������� { cFile, lShared, cDrvDbf, cCdPg, cPsw, cSetDel } - ������� 4
   lShared    := .F.
   aBtnFClr   := { BLACK, YELLOW }
   cFontBtn   := "Comic Sans MS"
   aBtnFont   := { cFontBtn, nFontSize, .T. }
   nWBtn2     := 0

   // ���������� ������ � ������� �������� �� ������� ����� �� dlu � pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top  // ����� ���
   // ������ ������ Desktop
   nH   := GetDesktopHeight()
   IF nH <= 600                       ;  nG := 10
   ELSEIF nH >= 768 .AND. nH <= 864   ;  nG := 15
   ELSEIF nH > 864  .AND. nH <= 1080  ;  nG := 20
   ELSE                               ;  nG := 25
   ENDIF

   nX     := nLR := nG                      // ������ ����� � �����
   nY     := nG                             // ������ ������ � �����
   nW     := 810                            // ������� ����
   nH     := 560                            // ������� ����
   nHTxt  := nFontSize*2 + nFontSize/2      // ������ GET'��
   cFile := ""

   DEFINE WINDOW Form_Open AT nY, nX WIDTH nW HEIGHT nH ;
      ICON cIcon TITLE cTitle BACKCOLOR aBackColor      ;
      MODAL NOSIZE                                      ;
      FONT cFont SIZE nFontSize                         ;
      ON INIT    _wPost( 0)                             ;
      ON RELEASE _wSend(98)
      //ON RELEASE {|o| _wSend(98), o := This.Cargo, aRet := {o:cFile, ...} } - ������� 4

      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor  := This.BackColor   // ���� ����
      owc:ahIcoDel := {}               // ��� �������� ������� ������ � �����
      owc:cFile    := cFile
      owc:lShared  := lShared
      owc:cDrvDbf  := cDrvDbf
      owc:cCdPg    := cCdPg
      owc:cPsw     := cPsw
      owc:cSetDel  := cSetDelete
      owc:oWnd     := oWnd            // ��������� ���� ������ Cargo -> ������������� ����  - ������� 3

      nW       := This.ClientWidth
      nH       := This.ClientHeight
      nWTxt    := nW - nLR * 2
      nWBtnDir := nHTxt*2 + nLR/2

      @ 0, 0 LABEL Label_0 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      cVal := IIF( ao:cLang == "RU", " �������� ������ � �� ", " Deleted records database " )
      @ nY, nX FRAME Frame_0 CAPTION cVal ;
        WIDTH nWTxt HEIGHT nG*4 BACKCOLOR aBackColor //OPAQUE

      nYCmb   := nY + nG + nG/2
      nXCmb   := nX + nLR
      nWCmb   := GetTxtWidth( "0SET0DELETED0OFF000000" , nFontSize-2, cFontBtn, .T. ) + 80

      aSetDel := { "SET DELETED OFF", "SET DELETED ON" }
      /*@ nYCmb, nXCmb COMBOBOXEX Combo_SetDel WIDTH nWCmb HEIGHT 220  ;
        ITEMS aSetDel VALUE 1 IMAGE {} BACKCOLOR SILVER    ;
        ON CHANGE { |nI,cS,ow| nI := This.Combo_SetDel.Value           ,;
                               cS := aSetDel[nI]                       ,;
                               cSetDelete := SUBSTR(cS, RAT("O", cS) ) ,;
                               ow := ThisWindow.Object                 ,;
                               ow:Cargo:cSetDel := cSetDelete          ,; // ���������
                               This.Label_0.Setfocus } */
      /////////////////////// Button ////////////////////////////
      nHIco := 32
      cTxt  := aSetDel[1]
      aBtn  := { "Button_Del", cTxt, "iDbase32x1", "iDbase32x2", nHIco, aBtnFClr, aBtnFont, "" }
      nYBtn := nYCmb
      nXBtn := nXCmb
      nHBtn := nHIco + 3*2
      nWBtn := nWCmb
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(4) }  // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      This.Frame_0.Width := nXCmb + nWCmb
      nX2 := This.Frame_0.Width + nG*2

      cVal := IIF( ao:cLang == "RU", " ����� �������� �� ", " Database opening mode " )
      @ nY, nX2 FRAME Frame_1 CAPTION cVal ;
        WIDTH nW - nX2 - nG HEIGHT nG*4 BACKCOLOR aBackColor //OPAQUE

      nXCmb    := nX2 + nLR
      nWCmb    := This.Frame_1.Width - nG*2
      aUseMode := { "EXCLUSIVE", "SHARED" }
      /*@ nYCmb, nXCmb COMBOBOXEX Combo_UseMode WIDTH nWCmb HEIGHT 220 ;
        ITEMS aUseMode VALUE 1 IMAGE {} BACKCOLOR SILVER       ;
        ON CHANGE { |nI,ow| nI := This.Combo_UseMode.Value    ,;
                            lShared := IIF(nI==1 , .F., .T. ) ,;
                            ow := ThisWindow.Object     ,;
                            ow:Cargo:lShared := lShared ,; // ���������
                            This.Label_0.Setfocus } */
      /////////////////////// Button ////////////////////////////
      nHIco := 32
      cTxt  := aUseMode[1]
      aBtn  := { "Button_Share", cTxt, "iDbase32x1", "iDbase32x2", nHIco, aBtnFClr, aBtnFont, "" }
      nYBtn := nYCmb
      nXBtn := nXCmb
      nHBtn := nHIco + 3*2
      nWBtn := nWCmb
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(5) }  // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY += This.Frame_1.Height + nG

      cVal := IIF( ao:cLang == "RU", " ���� � �� ", " Path to database " )
      @ nY, nX FRAME Frame_2  CAPTION cVal ;
        WIDTH nWTxt HEIGHT nHTxt*3 + nG BACKCOLOR aBackColor //OPAQUE
      nY += nG + nG/2
      nX += nLR

      @ nY, nX TEXTBOX Tb_Path VALUE cFile WIDTH nWTxt - nWBtnDir - nLR*2 HEIGHT nHTxt ;
        FONT "Tahoma" SIZE 12 FONTCOLOR BLACK BACKCOLOR aBColorTxt ;
        ON CHANGE {|ow| cFile := This.Tb_Path.Value ,;
                        ow := ThisWindow.Object , ow:Cargo:cFile := cFile }
      mySizeTBDir(ThisWindow.Name,"Tb_Path")  // ��������� ������

      /////////////////////// Button ////////////////////////////
      cVal  := IIF( ao:cLang == "RU", "������� ���� � Dbf-�����", "Specify the path to the Dbf file" )
      nHIco := 64
      cTxt  := ""
      aBtn  := { "Button_Dir", cTxt, "iDir48x3", "iDir48x2", nHIco, aBtnFClr, aBtnFont, cVal }
      nYBtn := nY
      nXBtn := nX + This.Tb_Path.Width + 5
      nHBtn := nHTxt*2 + nG/2
      nWBtn := nWBtnDir
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(1) }  // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY += This.Tb_Path.Height + nG/2

      /////////////////////// Button ////////////////////////////
      cBtnDrv := IIF(ao:cLang == "RU", "������� ��:", "DB Driver:" ) + cDrvDbf
      cVal    := IIF( ao:cLang == "RU", "�������� ������� ��", "Select database driver" )
      nWBtn   := GetTxtWidth( "�������0��:0BMDBFCDX0", nFontSize-2, cFontBtn, .T. ) + 80
      nHIco   := 32
      cTxt    := cBtnDrv
      aBtn    := { "Button_Drvr", cTxt, "iDbase32x1", "iDbase32x2", nHIco, aBtnFClr, aBtnFont, cVal }
      nYBtn   := nY
      nXBtn   := nX
      nHBtn   := nHIco + 2*5
      bAct    := {|| /*MsgDebug(This.Cargo),*/ _wPost(2) }   // ������� �� �����
      ahIco   := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nHBtnDir := This.Tb_Path.Height + nG/2 + nHBtn
      This.Button_Dir.Height := nHBtnDir  // ������������� ������ ������

      /////////////////////// Button ////////////////////////////
      cBtnCapt := IIF(ao:cLang == "RU", "������� ��������:", "Code page:" ) + cCdPg
      nYBtn    := nY
      nXBtn    := This.Button_Drvr.Col + This.Button_Drvr.Width + nLR
      nWBtn2   := This.Tb_Path.Width - This.Button_Drvr.Width - nLR + nG/2
      nHBtn    := nHTxt
      nHIco    := 0
      cTxt     := cBtnCapt
      aBtn     := { "Button_CdPg", cTxt, "", "", nHIco, aBtnFClr, aBtnFont, cVal }
      nYBtn    := nY
      nXBtn    := nX + This.Button_Drvr.Width + nG/2
      nHBtn    := 32 + 2*5  // ��� � ���������� ������
      bAct     := {|| /*MsgDebug(This.Cargo),*/ _wPost(3) }   // ������� �� �����
      ahIco    := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn2, nHBtn, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY   := This.Frame_2.Row + This.Frame_2.Height + nG
      nX   := nLR

      cVal := IIF( App.Cargo:cLang == "RU", " ������ �� �� ", " Database password " )
      @ nY, nX FRAME Frame_3  CAPTION cVal ;
        WIDTH nWTxt HEIGHT nHTxt*2 + nG BACKCOLOR aBackColor //OPAQUE

      nY += nG + nG/2
      nX += nLR

      DRAW ICON IN WINDOW Form_Open AT nY, nX PICTURE "iDbfPass48" WIDTH 48 HEIGHT 48 COLOR aBackColor
      nX += 48 + nG

      nWBtn := This.Frame_3.Width - nG*2 - 48 - nG/2*2
      @ nY + nG/2, nX GETBOX GB_Pass VALUE SPACE(30) WIDTH nWBtn HEIGHT nHTxt FONTCOLOR BLACK BACKCOLOR WHITE ;
        PICTURE REPL('x',30) ON CHANGE {|ow| cPsw := This.GB_Pass.Value ,;
                                             ow := ThisWindow.Object , ow:Cargo:cPsw := cPsw }

      nY := This.Frame_3.Row + This.Frame_3.Height + nG

      /////////////////////// Button ////////////////////////////
      nWBtn    := 220
      nHBtn    := 64 + nG/2
      nGBtn    := (nW - nWBtn*3) / 4
      nYBtn    := nY
      nXBtn    := nGBtn
      nHIco    := 64
      cBtnCapt := IIF( ao:cLang == "RU", "������", "Help" )
      cTxt     := cBtnCapt
      aBtn     := { "Button_Help", cTxt, "iQuestion64", "iQuestion64x2", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct     := {|| /*MsgDebug(This.Cargo),*/ _wPost(80) }   // ������� �� �����
      ahIco    := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, ORANGE)
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "�������", "Open" )
      nXBtn := nGBtn * 2 + nWBtn
      aBtn  := { "Button_Open", cTxt, "iDbC64x1", "iDbC64x2", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(90) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, {40,221,65})
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "������", "Cancel" )
      nXBtn := nGBtn * 3 + nWBtn * 2
      aBtn  := { "Button_Exit", cTxt, "iReturn64x1", "iReturn64x2", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(99) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, CLR_HRED )
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY += nHBtn + nG

      // �������� ������� ������ ����
      ThisWindow.Height := nY + GetTitleHeight() + GetBorderHeight()

      _o2log(owc, 15, ProcNL()+" -------------- ��������� ������� : => owc", .T.)

      // ��������� ������� �� ��� ���� ���������
      Sets_Event2ThisWindow()

      ON KEY ESCAPE OF Form_Open ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW Form_Open
   ACTIVATE WINDOW Form_Open

RETURN oWnd:Cargo:aRet // ������� - ������� 3
//RETURN aRet          // ������� - ������� 4

//////////////////////////////////////////////////////////////////////////////////////////////////////
// ��������� ������� �� ��� ���� ���������
STATIC FUNCTION Sets_Event2ThisWindow()

   WITH OBJECT This.Object
     :Event( 0, {|ow| // ������ ����� ���������� ����
                       This.Topmost := .F.
                       ? ProcNL(),">>> Start window: "+ow:Name
                       This.Frame_3.Enabled := .F.
                       This.GB_Pass.Enabled := .F.
                       ow:Setfocus('Label_0')
                       DO EVENTS
                       Return Nil
                       })

     :Event( 1, {|ow| // ������ - ������� ���� � Dbf-�����
                      Local aF, cFpsw, cPsw, cForm := ow:Name
                      Local cTtl  := "�������� ���� ��"
                      Local cTtl2 := "Select DB file"
                      Local cPath := GetStartUpFolder()
                      Local cObj  := "Tb_Path"
                      Local cObj2 := "Button_Dir"
                      Local ao    := App.Cargo
                      cTtl := IIF(ao:cLang == "RU", cTtl, cTtl2 )
                      This.&(cObj2).Enabled := .F.
                      SET WINDOW THIS TO ow:Name        // ����������� !!!
                      IF ao:cLang == "RU"
                         aF := GetFile( { {"����� DBF", "*.dbf"}, {"��� �����", "*.*"} }, cTtl, cPath , .T. )
                      ELSE
                         aF := GetFile( { {"DBF Files", "*.dbf"}, {"All Files", "*.*"} }, cTtl, cPath , .T. )
                      ENDIF
                      SET WINDOW THIS TO
                      IF LEN(aF) > 0
                         ow:Cargo:cFile := aF[1]                 // ��������� ���� �����
                         cPath          := hb_FNameDir( aF[1] )
                         SetProperty(cForm,cObj,"Value", ow:Cargo:cFile)
                         ? ProcNL()+"########", cObj, "["+ow:Cargo:cFile+"]"
                         mySizeTBDir(cForm,cObj)  // ������������ ������ �����
                         cPsw  := ""
                         cFpsw := ow:Cargo:cFile + '.psw'    // ���� � ������� ��� ����
                         IF FILE(cFpsw)
                            cPsw  := HB_MEMOREAD(cFpsw)
                         ENDIF
                         This.GB_Pass.Value := cPsw
                      ENDIF
                      This.&(cObj2).Enabled := .T.
                      ow:Setfocus('Label_0')
                      DO EVENTS
                      Return Nil
                      } )

     :Event( 2, {|ow| // �������� ����
                      LOCAL aRet
                      SET WINDOW THIS TO ow:Name        // ����������� !!!
                      aRet := myDriverDbf()             // -> util_dbf.prg
                      SET WINDOW THIS TO
                      IF LEN(aRet) > 0
                         ow:Cargo:cDrvDbf         := aRet[2]     // ���������
                         This.Button_Drvr.Caption := aRet[3]
                         IF aRet[1] == 3 .OR. aRet[1] == 7
                            This.Frame_3.Enabled := .T.
                            This.GB_Pass.Enabled := .T.
                         ELSE
                            This.Frame_3.Enabled := .F.
                            This.GB_Pass.Enabled := .F.
                         ENDIF
                      ENDIF
                      ow:Setfocus('Label_0')
                      DO EVENTS
                      Return Nil
                      } )

     :Event( 3, {|ow| // ������ - ����� ������� ��������
                      Local aR
                      SET WINDOW THIS TO ow:Name        // ����������� !!!
                      aR := myCodePageDbf()             // -> util_dbf.prg
                      SET WINDOW THIS TO 
                      If LEN(aR) > 0
                         ow:Cargo:cCdPg           := aR[2]       // ���������
                         This.Button_CdPg.Caption := aR[3]
                      Endif
                      ow:SetFocus('Label_0')
                      Return Nil
                      } )

     :Event( 4, {|ow| // ������ - ����� SET DELETED
                      Local aR
                      SET WINDOW THIS TO ow:Name        // ����������� !!!
                      aR := myUseDelete()
                      SET WINDOW THIS TO
                      If LEN(aR) > 0
                         ow:Cargo:cSetDel        := aR[2]       // ���������
                         This.Button_Del.Caption := aR[3]
                      Endif
                      ow:SetFocus('Label_0')
                      Return Nil
                      } )

     :Event( 5, {|ow| // ������ - ����� EXCLUSIVE/SHARED
                      Local aR, lShrd
                      SET WINDOW THIS TO ow:Name        // ����������� !!!
                      aR := myUseMode()
                      SET WINDOW THIS TO
                      If LEN(aR) > 0
                         lShrd := IIF(aR[2]=='EXCLUSIVE' , .F., .T. )
                         ow:Cargo:lShared := lShrd            // ���������
                         This.Button_Share.Caption := aR[3]
                      Endif
                      ow:SetFocus('Label_0')
                      Return Nil
                      } )

     :Event(80, {|ow| // ������
                      SET WINDOW THIS TO ow:Name
                      myDriverHelp()
                      SET WINDOW THIS TO
                      ow:SetFocus('Label_0')
                      Return Nil
                      } )

     :Event(90, {|ow| // ������� ���� � ������� ����
                      Local owc := ow:Cargo
                      Local ao  := App.Cargo
                      Local opc := owc:oWnd:Cargo    // ������ Cargo -> ������������� ����  - ������� 3
                      // Local opc := ow:Cargo:oWnd:Cargo
                      Local cTtl  := "������ ! �� ������ ���� �� !"
                      Local cTtl2 := "ERROR ! No database file selected !"
                      Local aR
                      cTtl := IIF(ao:cLang == "RU", cTtl, cTtl2 )
                      If LEN(owc:cFile) > 0
                         owc:cPsw := ALLTRIM(owc:cPsw)
                         aR := { owc:cFile, owc:lShared, owc:cDrvDbf, owc:cCdPg, owc:cPsw, owc:cSetDel } // ���������
                         opc:aRet := aR                    // ������� - ������� 3
                         //MsgDebug(aR)
                         ? ProcNL(), HB_ValToExp(opc:aRet)
                         // M->aPubOpenRetUse          := aR - ������� 1 �������� ���� � ����������
                         // App.Cargo:aOpenRetUse      := aR - ������� 2 �������� ���� � ����������
                         // owc:oWnd:Cargo:aOpenRetUse := aR - ������� 3 �������� ���� � ����������
                         _wPost(99,ow:Name)
                      Else
                         SET WINDOW THIS TO ow:Name        // ����������� !!!
                         AlertStop( cTtl, "Result", "ZZZ_B_STOP64", 64 )
                         SET WINDOW THIS TO
                         ow:SetFocus('Label_0')
                         aR := {}
                         opc:aRet := aR                    // ������� - ������� 3
                      Endif
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
                      DO EVENTS
                      Return Nil
                      } )

     :Event(99, {|ow| ow:Release() } )
   END WITH

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myUseDelete()
   LOCAL aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, cTypeRes, aFontExt, aLang, cVal

   IF App.Cargo:cLang == "RU"
      aLang  := { "���������� ��� ������ ��" , "���������� ������ �� ����� ��������" }
   ELSE
      aLang := { "display all database records" , "display database records except deleted ones" }
   ENDIF

   cForm := ThisWindow.Name
   aDim  := {}  //   1                   2                          3     4     5     6
   AADD( aDim, { "iDbase32x2"    , "SET DELETED OFF - " + aLang[1], .F. , "", "OFF" , 1 } )
   AADD( aDim, {                                                                        } )
   AADD( aDim, { "iDbase32x3"    , "SET DELETED ON - " + aLang[2] , .F. , "", "ON"  , 2 } )

   aRet     := {}
   nPos     := 3
   cTypeRes := "ICO" // "BMP"
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize
   aFontExt := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, cTypeRes )

   IF nChoice > 0
      cVal := SUBSTR(aDim[nChoice,2], 1, AT("-",aDim[nChoice,2])-1 )
      cVal := ALLTRIM(cVal)
      aRet := { aDim[nChoice,6], aDim[nChoice,5], cVal }
   ENDIF
   DO EVENTS

RETURN aRet

//////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myUseMode()
   LOCAL aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, cTypeRes, aFontExt, aLang, cVal

   IF App.Cargo:cLang == "RU"
      aLang  := { "����������� ������ � ��" , "��������������������� ������ � ��" }
   ELSE
      aLang  := { "exclusive access to the DB" , "multi-user access to the DB" }
   ENDIF

   cForm := ThisWindow.Name
   aDim  := {}  //   1                   2                      3    4          5      6
   AADD( aDim, { "iDbase32x2"    , "EXCLUSIVE  - " + aLang[1], .F. , "", "EXCLUSIVE" , 1 } )
   AADD( aDim, {                                                                         } )
   AADD( aDim, { "iDbase32x3"    , "SHARED - " + aLang[2]    , .F. , "", "SHARED"    , 2 } )

   aRet     := {}
   nPos     := 3
   cTypeRes := "ICO" // "BMP"
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize
   aFontExt := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, cTypeRes )

   IF nChoice > 0
      cVal := SUBSTR(aDim[nChoice,2], 1, AT("-",aDim[nChoice,2])-1 )
      cVal := ALLTRIM(cVal)
      aRet := { aDim[nChoice,6], aDim[nChoice,5], cVal }
   ENDIF
   DO EVENTS

RETURN aRet

///////////////////////////////////////////////////////////////////////////////
FUNCTION my2DrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAction, aColor)
   LOCAL cObj, cCapt, cFont, nFSize, lBold, aFClr2, aFClr1, cTtipt, aRetIco
   LOCAL cIco1x2, cIco1x1, nSizeIc, aGrFill, aGrOver, hIco1, hIco2

   // aBtn := { "Btn_Dir", cTxt, "iDir48x3", "iDir48x2", nHIco, aBtnFClr, aBtnFont, cVal }
   cObj    := aBtn[1]
   cCapt   := aBtn[2]
   cIco1x1 := aBtn[3]
   cIco1x2 := aBtn[4]
   nSizeIc := aBtn[5]
   aFClr1  := aBtn[6,1]
   aFClr2  := aBtn[6,2]
   cFont   := aBtn[7,1]
   nFSize  := aBtn[7,2]
   lBold   := aBtn[7,3]
   cTtipt  := aBtn[8]
   aGrOver := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aRetIco := {}        // ������� ������ ������, ���� �����

   IF LEN(cIco1x1) > 0
      hIco1 := LoadIconByName(cIco1x1, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco1 )
   ENDIF
   IF LEN(cIco1x2) > 0
      hIco2 := LoadIconByName(cIco1x2, nSizeIc, nSizeIc)
      AADD( aRetIco, hIco2 )
   ENDIF

   @ nRow, nCol BUTTONEX &cObj CAPTION cCapt         ;
     ICON hIco1                                      ;
     WIDTH nWBtn HEIGHT nHBtn                        ;
     NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/     ;
     FONTCOLOR aFClr1 FONT cFont SIZE nFSize         ;
     BACKCOLOR aGrOver GRADIENTFILL aGrFill          ;
     TOOLTIP cTtipt                                  ;
     ON MOUSEHOVER ( This.Icon := hIco2 , This.Fontcolor := aFClr2, This.GradientFill := aGrFill ) ;
     ON MOUSELEAVE ( This.Icon := hIco1 , This.Fontcolor := aFClr1, This.GradientOver := aGrOver ) ;
     ON INIT {|| This.Cargo := { aBtn, ThisWindow.Name, This.Name, aColor }  }
     //ACTION Eval(bAction) - �� ���� ���

   This.&(cObj).Action   := bAction
   This.&(cObj).Icon     := hIco1
   This.&(cObj).FontBold := lBold

RETURN aRetIco

