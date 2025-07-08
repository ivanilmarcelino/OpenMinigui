/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
///////////////////////////////////////////////////////////////////
STATIC FUNCTION MenuButtonsTopThisForm(aBColor)
   LOCAL hFont, aFont, nHBtn, oMenu
   DEFAULT aBColor := GRAY

   oMenu := oHmgData()
   oMenu:aImg := { {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iReturn48x1" ,"iReturn48x2"  }     }
   // ��� ������� + ��� �������
                  //  1          2          3             4              5            6             7           8            9           10
   oMenu:aMnEn  := { ""      , "Card" ,"F3;Refresh" ,"F4 Calculat", "F5 Print", "F6 Sorting", "F7 Search", "F8 Menu","Table;settings", "Exit"  }
   oMenu:aCapt  := oMenu:aMnEn
   oMenu:aObj   := { "_2Help","_2Card","_2F3Refresh","_2F4Calc"   ,"_2F5Print", "_2F6Sort"  , "_2F7Find" ,"_2F8Menu","_2F9Config"    , "_2Exit" }
   oMenu:a1BClr := { {4,135,109}, BLUE, GREY, GREY, {210, 71, 38}, {151,  0,160} ,{  9, 77,181}, {255,160, 66}, { 94, 59,185} , {189, 30, 73}  }

   oMenu:nHIco     := 48           // ������ ������ �� ������
   oMenu:nH1Ico    := 96           // ������ ������ ������ �� ������
   oMenu:nHG2      := 10           // ���������� ������ � ������ ������
   oMenu:aBtnFClr  := { BLACK, YELLOW, RED, BLACK     }         // ���� ����� ������ + 2/3/4-���� ���������
   //oMenu:aBtnBClr:= { {66,92,251} , WHITE, YELLOW, GRAY }     // ���� ���� ������ + 2/3/4-���� ���������
   oMenu:aBtnBClr  := { aBColor , WHITE, YELLOW, GRAY }         // ���� ���� ������ + ���� ���������

   hFont := GetFontHandle('FntBtnMain')                         // ���� ������ ������� �����
   aFont := GetFontParam(hFont)
   oMenu:aBtnFont  := { aFont[1], aFont[2], aFont[3] }          // ���� �� �������
   //oMenu:aBtnFont := { "Tahoma", aFont[2], .F. }              // ���� �� ������� - ����� � ��� ������
   //nHBtn         := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 2 ������ ������ �� �������
   nHBtn           := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 1 ������ ������ �� �������
   oMenu:lVert     := .T.        // ������������ ����� �� ������
   ? ProcNL(), "$$$$$$$$ 2 rows of buttons=", nHBtn

   oMenu:nX        := 0
   oMenu:nY        := 0
   //oMenu:lAutoSize := .T.      // T - �������������� ������ ������ � ������ ������ �� ������ ������
   oMenu:nWBtn     := 0          // ������ ������� ������ ������
   //oMenu:nHBtn   := 0          // ������ ������� ������ ������
   oMenu:nClientW  := 0          // ������ ����

   oMenu:lAutoSize := .F.        // F - ������ �������
   oMenu:nWBtn     := nHBtn      // ������ ������� ������ ������
   oMenu:nHBtn     := nHBtn      // ������ ������� ������ ������
   oMenu:lGradient := .T.        // �������� �� ������� - ��������

RETURN oMenu

///////////////////////////////////////////////////////////////////
FUNCTION Table_Two(oWnd,ky,cn)
   LOCAL oMenu, cForm, cTtl, cIco, cAls, cBtnEnabled, cWnd
   LOCAL nTable, cSprHd, oColumn, oClr

   cWnd   := oWnd:Name
   oMenu  := MenuButtonsTopThisForm()
   cForm  := "FORM_TWO"
   cTtl   := "List Table-2"
   cIco   := "iMg96x1"
   cAls   := "NEW"
   nTable := 1
   cSprHd := "Take data from ini-config"
   cBtnEnabled := ky := cn

   oColumn := {} //Column_TSB(cWnd,cAls)   // ������ ������� �������
   oClr    := oHmgData()              // ��� ����� ��� �������
   oClr:aBClr  := {   6,211,170 }     // ���� ���� ���� �����
   oClr:aBrush := { 136,240,219 }     // ���� ���� ��� ��������
   oClr:lZebra := .T.                                    // ��� ���.\����. �������� zebra
   //oClr:aZebra := { {230,230,230}, SILVER }            // ����� ����
   oClr:aZebra := { oClr:aBClr, {187,244,233} }

   MsgDebug("Do the rest yourself !!!")
   //FormTable12(nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, cSprHd, oClr, oMenu, oColumn)

RETURN NIL

///////////////////////////////////////////////////////////////////
