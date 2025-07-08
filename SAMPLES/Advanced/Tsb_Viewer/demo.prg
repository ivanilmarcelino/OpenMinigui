/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр/правка Dbf файла. Опции/свойства по базе
 * Иконка зашита в текст программы. Ресурс иконки поместили в исходник программы
 * View/edit Dbf file. Options/properties by base
 * The icon is sewn into the program text.
 * The icon resource was placed in the source code of the program
*/

//#define  _HMG_OUTLOG           // вывод отладки в файл
#include "minigui.ch"
#include "Dbinfo.ch"

REQUEST HB_CODEPAGE_UA1251, HB_CODEPAGE_UA866    // украинский язык
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // русский язык
REQUEST HB_LANG_BEWIN                            // белорусский язык
REQUEST DBFCDX

#define  SHOW_TITLE  "TsbViewer(c)"
#define  SHOW_VERS   SPACE(5) + "Ver 0.5 - 31.12.21"

FUNCTION Main(cMode)
   LOCAL cTtl, nW, nH, nG, nCol, nWLbl, nHLbl, nEvent, aBClr, cFileIco, cLog
   DEFAULT cMode := "0"

   PUBLIC oMain

   SET OOP ON              // ОБЯЗАТЕЛЬНО ! / NECESSARILY !
   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET NAVIGATION EXTENDED

   RddSetDefault("DBFCDX")
   SET AUTOPEN OFF          // не открывать автоматически индексные файлы
   SET DELETED OFF

   SET FONT TO "DejaVu Sans Mono", 15
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()

   cLog := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + "\_Msg.log" )
   fErase( cLog )
   SET LOGFILE TO &cLog

   cTtl      := "TsbViewer(c) - " +  MiniGuiVersion()
   nW        := System.ClientWidth
   nH        := 100
   aBClr     := { 94, 59,185}
   nEvent    := IIF( cMode == "0", 0 , VAL(cMode) )
   cFileIco  := Icon64TempCreate()  // иконку создаём во временной папке
   App.Cargo := oHmgData()          // создадим контейнер для всего приложения
   App.Cargo:nHMain := nH           // сохраним высоту окна Main

   DEFINE WINDOW Form_Main WIDTH nW HEIGHT nH TITLE cTtl ICON cFileIco ;
      MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR  aBClr                  ;
      ON GOTFOCUS RefreshWin( ThisWindow.Handle )                      ;
      ON INIT    {|| This.TopMost := .F., This.Minimize ,;
                     DoEvents(), _wPost(nEvent) }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nG := ( nH - 64 ) / 2

      DRAW ICON IN WINDOW Form_Main AT nG, nG PICTURE cFileIco WIDTH 64 HEIGHT 64 COLOR aBClr

      nCol  := nG * 2 + 64 + 20
      nWLbl := nW - nCol - nG
      nHLbl := INT( nH / 3 )
      cTtl  := SHOW_TITLE + SHOW_VERS

      @ 0, nCol LABEL Label_0 WIDTH nWLbl HEIGHT nHLbl*2 VALUE cTtl   ;
        FONT "Comic Sans MS" SIZE nHLbl FONTCOLOR YELLOW TRANSPARENT VCENTERALIGN

      @ nHLbl*2+1, nCol LABEL Label_1 WIDTH nWLbl HEIGHT nHLbl VALUE MiniGuiVersion() ;
        SIZE nHLbl - 10 FONTCOLOR YELLOW TRANSPARENT VCENTERALIGN

      M->oMain := This.Object
      (This.Object):Event( 0, {|ow| EventZero(ow) } )   // событие 0
      (This.Object):Event( 1, {|ow| EventOne(ow)  } )   // событие 1
      (This.Object):Event(99, {|ow| ow:Release()  } )   // выход

      ON KEY F1 ACTION NIL

   END WINDOW

   ACTIVATE WINDOW Form_Main

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION EventZero()
   LOCAL cDbf, cAls, cFltr, cInd, cCodePage, nY, nX, nW, nH, lCntr
   LOCAL aTsbPar, aWinPar, cMsg

   nY        := App.Cargo:nHMain + GetBorderHeight()  // считаем высоту окна Main
   nX        := 0
   nW        := System.ClientWidth
   nH        := System.ClientHeight - nY
   lCntr     := .F.
   cAls      := "ENG_COMPANY"
   cDbf      := GetStartUpFolder() + '\_Engl.dbf'
   cCodePage := hb_SetCodepage()
   cFltr     := 'AT("ууу", LOWER(ENG_COMPANY->COMPANY)) == 0'

   SET CODEPAGE TO ENGLISH
   SET LANGUAGE TO ENGLISH
   cCodePage := hb_SetCodepage()
   //USE (cDbf) ALIAS (cAls) NEW EXCLUSIVE
   IF ! myUseArea( cDbf, cAls, .F., , , )   // ==>> TsbViewMisc.prg
      cMsg := "ERROR opening database!;;"
      cMsg += cDbf
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      Quit
   ENDIF
   SET FILTER TO &cFltr
   SET FONT TO "Times New Roman", 14  // фонты построения в таблице берутся от этого фонта

   aTsbPar := { cAls, cCodePage, "Checkpoint (1) !" }
   aWinPar := { "NOWAIT", "", nY, nX, nW, nH, lCntr, { 0,176,240 } }
   TsbViewer( aTsbPar, aWinPar)  // окно с таблицей
   //TsbViewer()  // можно без параметров - окно будет "WAIT"

   ? ProcNL(), Hb_LangSelect(), cCodePage

   nY        := 0 ; nX := 0
   nW        := System.ClientWidth / 2
   nH        := System.ClientHeight
   lCntr     := .F.
   cAls      := "BYE_STREET"
   cDbf      := GetStartUpFolder() + '\_Be1251.dbf'
   cCodePage := "RU1251"    // потому что нет кодовой страницы для белорусского языка
   // открыть базу для белорусского языка
   SET LANGUAGE TO BYELORUSSIAN
   SET FONT TO "Tahoma", 12    // фонты построения в таблице берутся от этого фонта
   USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW EXCLUSIVE

   aTsbPar := { cAls, cCodePage, "Checkpoint (2) !" }
   aWinPar := { "NOWAIT", "", nY, nX, nW, nH, lCntr, GRAY }
   TsbViewer( aTsbPar, aWinPar)  // окно с таблицей
   // TsbViewer()  // можно без параметров  - окно будет "WAIT"

   ? ProcNL(), Hb_LangSelect(), cCodePage


   nY        := 0 ; nX := System.ClientWidth / 2 + 1
   nW        := System.ClientWidth / 2 - 1
   nH        := System.ClientHeight
   lCntr     := .F.
   cAls      := "UA_STREET"
   cDbf      := GetStartUpFolder() + '\_Ua1251.dbf'
   // открыть базу для украинского языка
   SET CODEPAGE TO UKRAINIAN
   SET LANGUAGE TO UKRAINIAN
   cCodePage := hb_SetCodepage()
   SET FONT TO "Arial", 15       // фонты построения в таблице берутся от этого фонта
   USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW EXCLUSIVE

   aTsbPar := { cAls, cCodePage, "Checkpoint (3) !" }
   aWinPar := { "NOWAIT", "", nY, nX, nW, nH, lCntr, BLUE }
   TsbViewer( aTsbPar, aWinPar)  // окно с таблицей
   // TsbViewer()  // можно без параметров  - окно будет "WAIT"

   ? ProcNL(), Hb_LangSelect(), cCodePage


   // открыть базу для русского языка
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
   SET FONT TO "DejaVu Sans Mono", 13 // фонты построения в таблице берутся от этого фонта
   cAls      := "TEST_CALC"
   cDbf      := GetStartUpFolder() + '\_Ru866.dbf'
   cCodePage := "RU866"
   cInd      := ChangeFileExt( cDbf, '.cdx' )
   DeleteFile(cInd)                                      // обязательно удалить
   USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW EXCLUSIVE
   cFltr := 'TEST_CALC->NN # 0'
   //SET FILTER TO &cFltr  // так нельзя ! не будет правильного показа записей
                           // в ТСБ во второй колонке
   // в качестве примера
   INDEX ON RECNO() TAG ALL                    // DbSetOrder(1) показ всех записей
   INDEX ON RECNO() TAG NN     FOR !Deleted()  // DbSetOrder(2) показ НЕ удалённых записей
   INDEX ON RECNO() TAG DEL    FOR Deleted()   // DbSetOrder(3) показ удалённых записей
   INDEX ON RECNO() TAG NoZero FOR &cFltr      // DbSetOrder(4)
   DbSetOrder(4)
   DbGotop()

   aTsbPar := { cAls, cCodePage, "Checkpoint (4) !" }
   aWinPar := { "NOWAIT", "", , , , , , RED }
   TsbViewer( aTsbPar, aWinPar)  // окно с таблицей
   // TsbViewer()  // можно без параметров - окно будет "WAIT"

   ? ProcNL(), Hb_LangSelect(), cCodePage

   ? ProcNL(), "--- END --- EventZero() ---"

   // _wPost(99) // закрыть главное окно

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION EventOne()
   LOCAL cMsg

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   MsgDebug("Резерв ! Для новых БД !")

   cMsg := "--- END --- EventOne() ---"
   ? ProcNL(), cMsg

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
#define WM_PAINT  15                     // перересовка объектов на форме
Static Procedure RefreshWin( hWnd )
   SendMessage( hWnd, WM_PAINT, 0, 0 )
   Do Events
Return

///////////////////////////////////////////////////////////////////
FUNCTION Icon64TempCreate()
   LOCAL cBuff := "AAABAAEAQEAAAAEAIAAoQgAAFgAAACgAAABAAAAAgAAAAAEAIAAAAAAAAEIAAAAAAAAAAAAAAAAAAAAAAAAAAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD///////////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AL///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
   LOCAL cBin, cFile := GetUserTempFolder() + "\MiniGui_2dbf64.ico"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile
