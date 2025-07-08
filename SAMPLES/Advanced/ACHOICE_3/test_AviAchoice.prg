/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() ���� �� ������� AVI + LABEL / ACHOICE() menu on AVI + LABEL object
*/
#define _HMG_OUTLOG

#include "hmg.ch"

///////////////////////////////////////////////////////////////////
//  nK := ACHOICE(nY,nX,nW,nH, aMenu,,"AchDirect",nK)
FUNCTION TestAchoice(nMode,cMenu)
   LOCAL nY, nX, nW, nH, nG, aParMenu, aIcoWin, cTitle, aColor
   LOCAL nTxtAlign, lLblBordr, aParAvi, nIcWinAlgn, lLblClEdge
   LOCAL nLblFSize, cLblFont, cFontExit, nAviAlign, lAviShow
   LOCAL aMenu, nHBtn, aMenu2, aMenu3, aRet, aParWin
   LOCAL aLblFClr, aLblBClr
   DEFAULT cMenu := "cMenu=Nil"

   // ��� ��������� ��� ������ �������
   nHBtn      := 54                       // ������ ������
   nG         := 20                       // ������ ����� ��������
   cLblFont   := "DejaVu Sans Mono"       // ���� ��� ������
   cFontExit  := "Comic Sans MS"          // ���� ��� ������ Exit
   nLblFSize  := 20                       // ������ ����� �� �������
   // �������� ����
   aMenu      := TestDim(.T.)
   // ������� ����
   aMenu2     := TestDim(.F.)

   // ��������� ������
   aLblFClr   := { WHITE      , YELLOW, GRAY   }    // ���� ����� ������ {MOUSEHOVER,MOUSELEAVE,����������}
   //aLblBClr := { {94,59,185}, BLACK , SILVER }    // ���� ���� ������  {MOUSEHOVER,MOUSELEAVE,����������}
   aLblBClr   := {}          // ���� ���� ������ ����� �� ����
   nAviAlign  := 1           // 1-Left Align Avi, 2-Right Align Avi
   lAviShow   := .F.         // ��� ������ avi �� �����
   lLblBordr  := .T.         // T-BORDER , F-NOBORDER for LABEL
   lLblClEdge := .T.         // T-CLIENTEDGE
   nTxtAlign  := 1           // Align Text: 0-None, 1-Left, 2-Right, 3-Center
   // ��������� Avi
   aParAvi    := { nAviAlign, lAviShow }
   // ��������� ����
   aParMenu   := { nHBtn, nG, cLblFont, cFontExit, nLblFSize, aLblFClr, aLblBClr, lLblBordr, lLblClEdge, nTxtAlign }

   nY         := nX := nW := nH := Nil
   cTitle     := "������ ������ ���� / Menu selection example - [" + cMenu + "]"
   aParWin    := { "Form_List1", cTitle, App.Cargo:cIconDef, {215,184,236}, nY, nX, nW, nH }
   aIcoWin    := {}          // ��� ������ �� �����

   IF nMode == 1                  // ���� ��� avi (�� ���������)
      aRet   := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 2              // ���� ��� avi (������ �����)
      aParWin[4] := {159,191,236}                  // ���� ���� ����� ����
      nIcWinAlgn := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin    := { "iHP2", 150, 0, 20, nIcWinAlgn }             // ������ �� �����
      aRet       := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 3                  // ���� � avi (������ ������)
      aColor      := HMG_n2RGB(CLR_VIBER)
      aParMenu[6] := { BLACK, RED    , GRAY   }               // ���� ����� ������
      aParMenu[7] := { aColor, YELLOW, SILVER }              // ���� ���� ������ ����� ������
      nTxtAlign   := 3                                       // !!! --> Align Text:  2-Right, 3-Center
      aParMenu[10]:= nTxtAlign
      nIcWinAlgn  := 2   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP1", 150, 0, 20, nIcWinAlgn }     // ������ �� �����
      aParAvi     := { 1, .T. }                             // 1-Left Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 4
      aParMenu[6]  := { BLACK , RED   , YELLOW }              // ���� ����� ������
      aParMenu[7]  := {}                                      // ���� ���� ������ ����� � ����
      nTxtAlign    := 1                                       // !!! --> Align Text:  2-Right, 3-Center
      aParMenu[10] := nTxtAlign
      nIcWinAlgn   := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin      := { "iHP2", 150, 0, 20, nIcWinAlgn }     // ������ �� �����
      aParAvi      := { 2, .T. }                             // 2-Right Align Avi, ����� avi �� �����
      aRet         := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 5
      aMenu       := TestDim1()
      aParMenu[6] := { BLACK , YELLOW, GRAY }                // ���� ����� ������
      aParMenu[7] := {}                                      // ���� ���� ������ ����� � ����
      nTxtAlign   := 3                                       // !!! --> Align Text:  2-Right, 3-Center
      aParMenu[10]:= nTxtAlign
      nIcWinAlgn  := 2   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP1", 150, 0, 20, nIcWinAlgn }     // ������ �� �����
      aParAvi     := { 1, .T. }                             // 1-Left Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 6
      aMenu       := TestDim1()
      aColor      := HMG_n2RGB(CLR_VIBER)
      aParMenu[6] := { BLACK , RED   , GRAY   }              // ���� ����� ������
      aParMenu[7] := { aColor, YELLOW, SILVER }              // ���� ���� ������ ����� ������
      nTxtAlign   := 1                                       // !!! --> Align Text:  2-Right, 3-Center
      aParMenu[10]:= nTxtAlign
      nIcWinAlgn  := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP2", 150, 0, 20, nIcWinAlgn }     // ������ �� �����
      aParAvi     := { 2, .T. }                             // 2-Right Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 7               // Large menu without avi (default)
      nG          := 10                             // ������ ����� ��������
      aParMenu[2] := nG                             // ��������� ����
      aRet        := MenuAviAchoice( aMenu2, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 8    // Large menu without avi + icon
      aMenu3      := TestDim3(aMenu2)
      aColor      := HMG_n2RGB(CLR_VIBER)
      nG          := 1                              // ������ ����� ��������
      aParMenu[2] := nG                             // ��������� ����
      aParMenu[6] := { BLACK , RED   , GRAY   }     // ���� ����� ������
      aParMenu[7] := { aColor, YELLOW, SILVER }     // ���� ���� ������ ����� ������
      lLblClEdge  := .F.                            // T-CLIENTEDGE
      aParMenu[9] := lLblClEdge
      nIcWinAlgn  := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP1", 150, 0, 20, nIcWinAlgn }     // ������ �� �����
      aParAvi     := { 2, .T. }                             // 2-Right Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu3, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 9                  // Large menu with avi + icon
      nIcWinAlgn  := 2                                         // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP2", 100, 0, 20, nIcWinAlgn }        // ������ �� �����
      aParAvi     := { 2, .T. }                                // 2-Right Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu2, aParWin, aParMenu, aParAvi, aIcoWin )

   ELSEIF nMode == 10                // Large menu with avi left + icon
      nG          := 10                                        // ������ ����� ��������
      aParMenu[2] := nG                                        // ��������� ����
      nTxtAlign   := 3                                         // !!! --> Align Text:  2-Right, 3-Center
      aParMenu[10]:= nTxtAlign
      nIcWinAlgn  := 1                                         // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin     := { "iHP1", 120, 20, 20, nIcWinAlgn }       // ������ �� �����
      aParAvi     := { 1, .T. }                                // 2-Right Align Avi, ����� avi �� �����
      aRet        := MenuAviAchoice( aMenu2, aParWin, aParMenu, aParAvi, aIcoWin )

   ENDIF

   //AlertInfo("aRet= " + HB_ValToExp(aRet) + ";;" + ProcNL(), "Return result" )

RETURN aRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TestDim(lMode)
   LOCAL aRet, aMenu, aMenu2
   LOCAL aClrSk := HMG_n2RGB(CLR_SKYPE)
   LOCAL aClrVb := HMG_n2RGB(CLR_VIBER)
   LOCAL aClrOk := HMG_n2RGB(CLR_OK)
   DEFAULT lMode := .T.
                                                           //                  {MOUSEHOVER,MOUSELEAVE,����������}
   //                        text                                      avi              1       2       3
   aMenu := {{"Selection menu-1;   additional text with spaces" , "a_scanfiles"     , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-2;   additional text with spaces" , "a_findfolder"    , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-3;   additional text with spaces" , "a_scanfiles"     , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-4;   additional text with spaces" , "a_findfolder"    , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-5;   additional text with spaces" , "a_cogs"          , aClrVb, BLACK , SILVER  }, ;
             {"Selection menu-6;   additional text with spaces" , "a_printer2"      , aClrSk, BLACK , SILVER  }, ;
             {"Exit menu / ����� �� ����"                       , "a_exit"          , MAROON, BLACK , SILVER  }  ;
            }

   aMenu2 := {{"Selection menu-1;   additional text with spaces" , "a_scanfiles"    ,       ,       ,        }, ;
              {"Selection menu-2;   additional text with spaces" , "a_findfolder"   , aClrOk, BLACK , SILVER }, ;
              {"Selection menu-3;   additional text with spaces" , "a_scanfiles"    , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-4;   additional text with spaces" , "a_findfolder"   , aClrVb, BLACK , SILVER }, ;
              {"Selection menu-5;   additional text with spaces" , "a_cogs"         , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-6;   additional text with spaces" , "a_printer2"     , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-7;   additional text with spaces" , "a_findfolder"   , aClrSk, BLACK , SILVER }, ;
              {"Selection menu-8;   additional text with spaces" , "a_scanfiles"    , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-9;   additional text with spaces" , "a_findfolder"   , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-10;  additional text with spaces" , "a_scanfiles"    , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-11;  additional text with spaces" , "a_printer2"     , LGREEN, BLACK , SILVER }, ;
              {"Selection menu-12;  additional text with spaces" , "a_cogs"         , BLUE  , BLACK , SILVER }, ;
              {"Exit menu / ����� �� ����"                       , "a_exit"         , RED   , BLACK , SILVER }  ;
             }

   IF lMode
      aRet := aMenu
   ELSE
      aRet := aMenu2
   ENDIF

RETURN aRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TestDim1()
   LOCAL aMenu
   LOCAL aClrSk := HMG_n2RGB(CLR_SKYPE)
   LOCAL aClrVb := HMG_n2RGB(CLR_VIBER)
   LOCAL aClrOk := HMG_n2RGB(CLR_OK)

   //aLblBClr   := { LGREEN, BLACK , SILVER }       // ���� ���� ������     1         2        3
                                                    //               {MOUSEHOVER,MOUSELEAVE,����������}
   //               text                         avi              1       2       3
   aMenu := {{"Selection menu-1"         , "a_scanfiles"     , aClrOk, BLACK , SILVER  }, ;
             {"Selection menu-2"         , "a_findfolder"    , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-3"         , "a_scanfiles"     , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-4"         , "a_findfolder"    , LGREEN, BLACK , SILVER  }, ;
             {"Selection menu-5"         , "a_cogs"          , aClrVb, BLACK , SILVER  }, ;
             {"Selection menu-6"         , "a_printer2"      , aClrSk, BLACK , SILVER  }, ;
             {"Exit menu / ����� �� ����", "a_exit"          , MAROON, BLACK , SILVER  }  ;
            }

RETURN aMenu

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TestDim3(aMenu)
   LOCAL nI, cTxt

   FOR nI := 1 TO LEN(aMenu)
      cTxt := aMenu[nI,1]
      IF ";" $ cTxt
         cTxt        := SUBSTR(cTxt,1,AT(";",cTxt)-1)
         aMenu[nI,1] := cTxt
      ENDIF
   NEXT

RETURN aMenu
