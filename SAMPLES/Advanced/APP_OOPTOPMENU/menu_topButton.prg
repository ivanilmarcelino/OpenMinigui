/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������� ���� ���� � �������� / Top window menu with buttons
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION MenuTopIconButtons( owc, oMenu, a2BtnBClr )
   LOCAL nH, nX, nY, nG, nG2, cFont, nFSize, lBold, nWButton, nHButton
   LOCAL nWBtn, nHBtn, cCap, nWtxt, nWCap, cObj, cForm, aBtnObj, i, o
   LOCAL aIco, aBtnFClr, aBtnBClr, aBtnFont, nHIco, aBtn, j
   LOCAL aBColor, aCapt, aObj, aIcon, nW, lAutoSize, cMsg, aDim, cVal, nJ
   LOCAL a2Cap, n1Cap, n2Cap, aVal, nType, hIco1, hIco2, hIco3, hIco4
   DEFAULT a2BtnBClr := {}  // ������

   //? ProcNL(), "oWC=", oWC, oMenu
   cForm     := ThisWindow.Name    // ��� ����
   aBColor   := owc:aBColor        // ���� ���� ����
   aBtnFClr  := oMenu:aBtnFClr     // { WHITE   , BLACK }
   aBtnBClr  := oMenu:aBtnBClr     // { aBColor , WHITE }
   aBtnFont  := oMenu:aBtnFont     // { cFont, nFSize, .T. }
   nHIco     := oMenu:nHIco        // 32,55  - ����� ������ �������� �� ������
   nG2       := oMenu:nHG2         // ���������� ������ � ������ ������
   nG        := owc:nG             // �������
   cFont     := aBtnFont[1]
   nFSize    := aBtnFont[2]
   lBold     := aBtnFont[3]
   lAutoSize := oMenu:lAutoSize    // T - �������������� ������ ������ � ������ ������
   nWButton  := oMenu:nWBtn        // ������ ������� ������ ������
   nHButton  := oMenu:nHBtn        // ������ ������� ������ ������
   aBtnObj   := {}

   IF !IsArray(owc:ahIcoDel)
      cMsg := "ERROR !;Icon handle deletion array not defined !;;"
      cMsg += "Form [" + cForm + "];"
      cMsg += "No array - owc:ahIcoDel"
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      RETURN NIL
   ENDIF

   // ��� ������� + ��� �������
   aObj  := oMenu:aObj
   aIcon := oMenu:aImg
   aCapt := oMenu:aCapt
   IF LEN(aCapt) == LEN(aIcon) .AND. LEN(aIcon) == LEN(aObj)
   ELSE
      cMsg := "ERROR !;Arrays are not the same !;;"
      cMsg += "LEN(oMenu:aObj )=" + HB_NtoS(LEN(oMenu:aObj )) + ";"
      cMsg += "LEN(oMenu:aImg )=" + HB_NtoS(LEN(oMenu:aImg )) + ";"
      cMsg += "LEN(oMenu:aCapt)=" + HB_NtoS(LEN(oMenu:aCapt))
      cMsg += ";;" + ProcNL()
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
   ENDIF

   IF lAutoSize
      // ������ �� ������
      nWtxt := nH := 0
      FOR i := 1 TO LEN(aCapt)
         cCap := aCapt[ i ]
         IF IsArray(cCap)
            a2Cap := cCap
            n1Cap := LEN(a2Cap[1])
            n2Cap := LEN(a2Cap[2])
            IF n1Cap > n2Cap
               cCap := a2Cap[1]
            ELSE
               cCap := a2Cap[2]
            ENDIF
         ENDIF
         // ������� �� ������
         IF AT(";",cCap) > 0
            aDim := HB_ATokens(cCap, ";")
            FOR nJ := 1 TO LEN(aDim)
               cVal := ALLTRIM(aDim[nJ])
               nWCap := GetTxtWidth(cVal, nFSize, cFont, lBold )
               nWTxt := MAX(nWTxt,nWCap)
            NEXT
         ELSE
            nWCap := GetTxtWidth(cCap, nFSize, cFont, lBold )
         ENDIF
         //nWCap := GetTextWidth( NIL, cCap, hFont )
         nWTxt := MAX(nWTxt,nWCap)
      NEXT
      nWTxt := IIF(nWTxt < nHIco, nHIco, nWTxt )   // nHImg-������ bmp
      nWBtn := nWTxt + nG                          // ������ ������
      nHBtn := nHIco + 5 + nFSize + 5 + nG2 * 2    // ������ ������
      IF nHButton > 0
         nHBtn := nHButton    // ������ ������ ������ �������
      ENDIF
   ELSE
      nWBtn := nWButton       // ������ ������
      nHBtn := nHButton       // ������ ������
   ENDIF

   nY := IIF( IsNumeric(oMenu:nY), oMenu:nY, nG2 )
   nX := IIF( IsNumeric(oMenu:nX), oMenu:nX, nG )

   FOR i := 1 TO LEN(aCapt)

      nType := 0
      cCap  := aCapt[i]
      IF IsArray(cCap)
         aVal := cCap
         FOR j := 1 TO LEN(aVal)
            IF AT(";",aVal[j]) > 0
               aVal[j] := AtRepl( ";", aVal[j], CRLF )
            ENDIF
         NEXT
         cCap  := aVal[1]
         nType := 2         // ��� ������
      ENDIF

      IF AT(";",cCap) > 0
         cCap := AtRepl( ";", cCap, CRLF )
      ENDIF

      cObj  := aObj[i]    // ������� �� ����
      aIco  := aIcon[i]
      hIco1 := LoadIconByName(aIco[1], nHIco, nHIco)
      hIco2 := LoadIconByName(aIco[2], nHIco, nHIco)
      hIco3 := IIF( nType == 2, LoadIconByName(aIco[3], nHIco, nHIco) , 0 )
      hIco4 := IIF( nType == 2, LoadIconByName(aIco[4], nHIco, nHIco) , 0 )
      aBtn  := { cObj, cCap, {hIco1,hIco2}, aBtnBClr, aBtnFClr, aBtnFont, "" }
      // ���������� ������ -> util_button.prg
      my2DrawButton2(nY, nX, nWBtn, nHBtn, aBtn )
      // ��� �������� ������� ������ � �����
      AADD( owc:ahIcoDel, hIco1 )
      AADD( owc:ahIcoDel, hIco2 )
      IF nType == 2
         AADD( owc:ahIcoDel, hIco3 )
         AADD( owc:ahIcoDel, hIco4 )
      ENDIF

      This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
      o:nBtn   := i
      o:aIco   := aIco       // [2] ��� [4] ������
      o:ahIco  := {hIco1,hIco2,hIco3,hIco4}  // 4 ������ - ������
      o:nHIco  := nHIco
      o:aFClr  := aBtnFClr   // [2] ��� [4] �����
      o:aBClr  := aBtnBClr   // [2] ��� [4] �����
      o:cTxt   := aCapt[i]   // 1-� ������� �� ������
      o:aTxt   := aCapt[i]   // 1 ��� 2 ������� �� ������
      o:nDim   := IIF( VALTYPE(aCapt[i]) == "A", 2, 1)   // ���-�� �������� �� ������ 1/2
      o:nI     := 1
      o:Post   := cObj       // ������� �� �����

      This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo:Post, , This.Name) }

      AADD( aBtnObj, { i, cObj, "-��� �������", cCap, nY, nX, This.&(cObj).Width, nHBtn, cObj, "-�������", This.&(cObj).Cargo } )

      nX += This.&(cObj).Width + nG2
      // ����� � ��� / output to log
      //_o2log(o, 25, ProcNL() + "  o => This.Cargo: " + cObj, .T. ) ; ?

   NEXT

   nW := nX - nG2
   owc:nWEndTB := nW              // ����� ������
   owc:nHTBar  := nHBtn + nG2*2   // ������ ToolBar
   owc:aBtnObj := aBtnObj         // ������ ������ �� �����
   //? "aBtnObj=",aBtnObj ; ?v aBtnObj

RETURN NIL
