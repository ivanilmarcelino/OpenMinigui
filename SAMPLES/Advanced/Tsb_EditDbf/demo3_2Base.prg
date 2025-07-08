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
   // имя объекта + имя события
                  //  1          2          3             4              5            6             7           8            9           10
   oMenu:aMnEn  := { ""      , "Card" ,"F3;Refresh" ,"F4 Calculat", "F5 Print", "F6 Sorting", "F7 Search", "F8 Menu","Table;settings", "Exit"  }
   oMenu:aCapt  := oMenu:aMnEn
   oMenu:aObj   := { "_2Help","_2Card","_2F3Refresh","_2F4Calc"   ,"_2F5Print", "_2F6Sort"  , "_2F7Find" ,"_2F8Menu","_2F9Config"    , "_2Exit" }
   oMenu:a1BClr := { {4,135,109}, BLUE, GREY, GREY, {210, 71, 38}, {151,  0,160} ,{  9, 77,181}, {255,160, 66}, { 94, 59,185} , {189, 30, 73}  }

   oMenu:nHIco     := 48           // высота иконки на кнопке
   oMenu:nH1Ico    := 96           // высота первой иконки на кнопке
   oMenu:nHG2      := 10           // добавочная высота к тексту кнопки
   oMenu:aBtnFClr  := { BLACK, YELLOW, RED, BLACK     }         // цвет фонта кнопки + 2/3/4-цвет инвертный
   //oMenu:aBtnBClr:= { {66,92,251} , WHITE, YELLOW, GRAY }     // цвет фона кнопки + 2/3/4-цвет инвертный
   oMenu:aBtnBClr  := { aBColor , WHITE, YELLOW, GRAY }         // цвет фона кнопки + цвет инвертный

   hFont := GetFontHandle('FntBtnMain')                         // Фонт кнопок главной формы
   aFont := GetFontParam(hFont)
   oMenu:aBtnFont  := { aFont[1], aFont[2], aFont[3] }          // фонт на кнопках
   //oMenu:aBtnFont := { "Tahoma", aFont[2], .F. }              // фонт на кнопках - можно и так задать
   //nHBtn         := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 2 строки текста на кнопках
   nHBtn           := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 1 строка текста на кнопках
   oMenu:lVert     := .T.        // вертикальный текст на кнопке
   ? ProcNL(), "$$$$$$$$ 2 rows of buttons=", nHBtn

   oMenu:nX        := 0
   oMenu:nY        := 0
   //oMenu:lAutoSize := .T.      // T - автоматический расчёт высоты и ширины кнопки от высоты иконки
   oMenu:nWBtn     := 0          // ручное задание ширины кнопки
   //oMenu:nHBtn   := 0          // ручное задание высоты кнопки
   oMenu:nClientW  := 0          // ширина окна

   oMenu:lAutoSize := .F.        // F - ручное задание
   oMenu:nWBtn     := nHBtn      // ручное задание ширины кнопки
   oMenu:nHBtn     := nHBtn      // ручное задание высоты кнопки
   oMenu:lGradient := .T.        // градиент на кнопках - включить

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

   oColumn := {} //Column_TSB(cWnd,cAls)   // список колонок таблицы
   oClr    := oHmgData()              // все цвета для таблицы
   oClr:aBClr  := {   6,211,170 }     // цвет фона всей формы
   oClr:aBrush := { 136,240,219 }     // цвет фона под таблицей
   oClr:lZebra := .T.                                    // это вкл.\откл. механизм zebra
   //oClr:aZebra := { {230,230,230}, SILVER }            // серый цвет
   oClr:aZebra := { oClr:aBClr, {187,244,233} }

   MsgDebug("Do the rest yourself !!!")
   //FormTable12(nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, cSprHd, oClr, oMenu, oColumn)

RETURN NIL

///////////////////////////////////////////////////////////////////
