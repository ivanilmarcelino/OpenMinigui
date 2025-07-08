/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() ���� �� ������� ButtonEx / �enu on the ButtonEx object
*/
#include "hmg.ch"
#define MYRED    { 211, 107, 107 }
#define MYBACK   {  10, 141, 115 }
////////////////////////////////////////////////////////////////////////////
//  nK := ACHOICE(nY,nX,nW,nH, aMenu,,"AchDirect",nK)
FUNCTION TestAchoice(nMode,cMenu)
   LOCAL aMenu, aIcon, nHBtn, aMenu2, aIcon2, aIcoExit, aRet, nWinAlign
   LOCAL aParWin, lBtnExit, aParIco, nIcoAlign, lBtnFlat, nG, aDay
   LOCAL aBtnFClr, aBtnBClr, aBtnGrad, nBtnFont, aParIco2, cTitle
   LOCAL nY, nX, nW, nH, lGrdHoriz, lBtnGrad, aParMenu, aIcoWin
   LOCAL aMenu3, aIcon3, aBackClr, aParIco3, lTxtAlign
   DEFAULT cMenu := "cMenu=Nil"

   // ��� ��������� ��� ������ �������
   nHBtn     := 48 + 6                   // ������ ������ XX ������ + 3*2 �������
   nG        := 20                       // ������ ����� ��������
   lBtnExit  := .T.                      // �������� ������ Exit
   nBtnFont  := 20                       // ������ ����� �� �������
   lBtnGrad  := .F.                      // ��� ��������� �� �������
   // ��������� ����
   aParMenu  := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
   // �������� ����
   aMenu     := TestDim(1,.F.)
   aIcon     := TestDim(2,.F.)
   // ������� ����
   aMenu2    := TestDim(1,.T.)
   aIcon2    := TestDim(2,.T.)
   // �������� ���� � ������� �� �����
   aMenu3    := TestDim(1,.F.,3)
   aIcon3    := TestDim(2,.F.,3)
   // ��������� ������ Exit
   aBtnFClr  := { WHITE, YELLOW             }   // ���� ����� ������
   aBtnBClr  := { MYRED, BLACK              }   // ���� ���� ������
   aBtnGrad  := { HMG_RGB2n(RED), CLR_WHITE }   // ���� ��������� ������
   aIcoExit  := { {"iExit1", "iExit2"}, "����� / EXIT", aBtnFClr, aBtnBClr, aBtnGrad }   // ������ ��� ������
   // ��������� ������ ������
   aBtnFClr   := { WHITE, YELLOW      }   // ���� ����� ������
   aBtnBClr   := { {94,59,185}, BLACK }   // ���� ���� ������
   aBtnGrad   := { HMG_RGB2n({0,176,240}) , CLR_WHITE }  // ���� ��������� ������
   nIcoAlign  := 1        // 1-Left Align Icon, 2-Right Align Icon, 3-Top Align Icon, 4-Bottom Align Icon
   lBtnFlat   := .T.      // ��� ��������� ������ �� ����� ������, F-����
   lGrdHoriz  := .F.      // ��� ��������� ������, ����������� ���������, F-��������, T-������������
   // ��������� ��������� ������
   aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
   aParIco2   := { aIcon2, aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
   nY         := nX := nW := nH := Nil
   cTitle     := "������ ������ ���� / Menu selection example - [" + cMenu + "]"
   aBackClr   := {215,184,236}
   aParWin    := { "Form_List1", cTitle, "iSanta1", aBackClr, nY,nX,nW,nH, aIcoWin }
   aIcoWin    := {}          // ��� ������ �� �����
   lTxtAlign  := 0           // Align Text: 0-None, 1-Left, 2-Right, 3-Center

   IF nMode == 1                  // ���� ��� ������ (�� ���������)
      aParWin[4] := Nil                             // ���� ���� ����� ����
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, {}, aIcoWin, lTxtAlign )  

   ELSEIF nMode == 2              // ���� ��� ������ (� �����������)
      aParWin[4] := {159,191,236}                  // ���� ���� ����� ����
      nBtnFont   := 22                             // ������ ����� �� �������
      lBtnExit   := .F.                            // �� �������� ������ Exit
      lBtnGrad   := .F.                            // ��� ��������� �� �������
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParIco[1] := aParIco[2] := {}               // ������ �� ������� ���
      aParIco[4] := { {0,176,240}, {94,59,185} }   // ���� ���� ������
      aParIco[5] := {}                             // ��� ��������� ������
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco, aIcoWin, lTxtAlign  )   

   ELSEIF nMode == 3                  // ���� � ������� (������ �����)
      aParIco[3] := { WHITE, YELLOW }              // ���� ����� ������
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 4                  // ���� � ������� (������ ������)
      aParIco[6] := 2                              // 2-Right Align Icon
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 5                 // ���� � ������� (������ ������)
      nG         := 10                             // ������ ����� ��������
      nHBtn      := 64 + nBtnFont * 2              // ������ ������
      aParIco[6] := 3                              // 3-Top Align Icon
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 6                // ���� � ������� (������ �����)
      nG         := 10                             // ������ ����� ��������
      nHBtn      := 64 + nBtnFont * 2              // ������ ������
      aParIco[6] := 4                              // 4-Bottom Align Icon
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 7                // ���� � ������� + ��������
      nG         := 10                             // ������ ����� ��������
      lBtnGrad   := .T.                            // �������� �� �������
      nHBtn      := 64 + 6                         // ������ ������ XX ������ + 3*2 �������
      nBtnFont   := 22                             // ������ ����� �� �������
      //lBtnExit := .F.                            // �� �������� ������ Exit
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aIcoExit   := { {"iExit1", "iExit2"}, "����� / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // ������ ��� ������
      aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      aParIco[3] := { BLACK, BLUE }                // ���� ����� ������
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 8                // ���� � ������� + �������� HORIZONTAL
      nG         := 10                             // ������ ����� ��������
      lBtnGrad   := .T.                            // �������� �� �������
      nHBtn      := 64 + 6                         // ������ ������ XX ������ + 3*2 �������
      nBtnFont   := 22                             // ������ ����� �� �������
      //lBtnExit := .F.                            // �� �������� ������ Exit
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aIcoExit   := { {"iExit1", "iExit2"}, "����� / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // ������ ��� ������
      aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      aParIco[3] := { BLACK, YELLOW }         // ���� ����� ������
      aParIco[5] := { HMG_RGB2n(PURPLE) , CLR_WHITE }  // ���� ��������� ������
      aParIco[8] := .T.                       // ��� ��������� ������, ����������� ���������, F-��������, T-������������
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 9                  // ������� ���� ��� ������
      aParMenu[3] := 10                       // ������ ����� ��������
      aRet        := MenuAchoice(aMenu2, aParWin, aParMenu, {}     )   

   ELSEIF nMode == 10                 // ������� ���� � ��������
      nG         := 5                         // ������ ����� ��������
      lBtnGrad   := .T.                       // �������� �� �������
      nHBtn      := 64 + 6                    // ������ ������ XX ������ + 3*2 �������
      nBtnFont   := 22                        // ������ ����� �� �������
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParIco2[3]:= { BLUE, RED }             // ���� ����� ������
      aRet       := MenuAchoice(aMenu2, aParWin, aParMenu, aParIco2 )   
   ELSEIF nMode == 11                   // ���� �� ������� (�� ���������)
      aRet       := MenuAchoice(aMenu)        

   ELSEIF nMode == 12                   // ���� �� ������� c ������� ����� �� �����
      nWinAlign := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin   := { "iHP1", 150, 0, 20, nWinAlign }             // ������ �� �����
      aParWin   := { "Form_Print", cTitle, "iHP1", TEAL, 80, 150, nW, nH }
      aRet      := MenuAchoice(aMenu3, aParWin, , , aIcoWin)     

   ELSEIF nMode == 13                 // ���� �� ������� c ������� ������ �� �����
      aBackClr   := {159,191,236}                                // ���� ���� ����� ����
      lBtnGrad   := .T.                                          // �������� �� �������
      nG         := 20                                           // ������ ����� ��������
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParWin    := { "Form_Print", cTitle, "iHP2", aBackClr, 100, 250, nW, nH }
      nWinAlign  := 2   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin    := { "iHP2", 128, 0, 20, nWinAlign }            // ������ �� �����
      aBtnFClr   := { BLACK, BLUE }                              // ���� ����� ������
      aIcoExit   := { {"iExit1", "iExit2"}, "����� / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // ������ ��� ������
      aParIco3   := { aIcon3, aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      lTxtAlign  := 1        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aRet       := MenuAchoice(aMenu3, aParWin, aParMenu, aParIco3, aIcoWin, lTxtAlign)     

   ELSEIF nMode == 14                  // ���� ���� ������ (��������� ����� �� ������ ����)
      lTxtAlign  := 1        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aDay := TestaDay()
      aRet := MenuAchoice(aDay, , , , , lTxtAlign)        

   ELSEIF nMode == 15                  // ���� ���� ������ (��������� ����� �� ������� ����)
      lTxtAlign  := 2        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aDay := TestaDay()
      aRet := MenuAchoice(aDay, , , , , lTxtAlign)        

   ENDIF

   //AlertInfo("aRet= " + HB_ValToExp(aRet) + ";;" + ProcNL(), "Return result" )

RETURN aRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TestDim(nMode,lMode,nAdd)
   LOCAL nI, aRet, aMenu, aIcon, aMenu3, aIcon3
   DEFAULT nAdd := 0

   aMenu := { "Selection menu-1,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-2,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-3,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-4,   additional text with spaces,   wide menu bar"    }

   aIcon := { {"iSanta1" ,"iSanta2" }  ,;
              {"iFolder1","iFolder2"}  ,;
              {"iHP1"    ,"iHP2"    }  ,;
              {"iHmg1"   ,"iHmg2"   }      }

   IF nAdd > 0
      aMenu3 := { "Selection menu-1, wide menu bar;second menu line" ,;
                  "Selection menu-2, wide menu bar;second menu line" ,;
                  "Selection menu-3, wide menu bar;second menu line" ,;
                  "Selection menu-4, wide menu bar;second menu line"    }

      aIcon3 := { {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" }    }
   ENDIF

   IF lMode
      FOR nI := 1 TO 12
          AADD( aMenu, "Additional selection menu (" + HB_NtoS(nI+4) + ") ......" )
          AADD( aIcon, {"iClock1","iClock2"} )
      NEXT
   ENDIF

   IF nMode == 1
      aRet := IIF(nAdd==0, aMenu, aMenu3)
   ELSE
      aRet := IIF(nAdd==0, aIcon, aIcon3)
   ENDIF

RETURN aRet

////////////////////////////////////////////////////////////////////////////
FUNCTION TestaDay()
   LOCAL nL, nI, aDay

   SET CODEPAGE TO ENGLISH   
   SET LANGUAGE TO ENGLISH   

   nL   := 0
   aDay := ARRAY(7)
   FOR nI := 1 TO LEN(aDay)
      aDay[nI] := HB_NtoS(nI) +  " - " + LOWER( NTOCDOW( nI ) ) + " "
      nL := MAX(LEN(aDay[nI]), nL)
   NEXT
   //FOR nI := 1 TO LEN(aDay)
   //   aDay[nI] := PADR(aDay[nI],nL)
   //NEXT

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

RETURN aDay
