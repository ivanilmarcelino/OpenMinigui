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

REQUEST DBFCDX
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // русский язык

#define  _HMG_OUTLOG           // вывод отладки в файл
#define  SHOW_TITLE  "TsbViewer(c)"
#define  SHOW_VERS   SPACE(5) + "Ver 0.7 - 08.08.23"

#include "minigui.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cTtl, nW, nH, cFileIco, cLog

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET NAVIGATION EXTENDED
   SET AUTOPEN OFF   // не открывать автоматически индексные файлы
   SET DELETED OFF
   SET OOP ON

   SET CODEPAGE TO ENGLISH            
   SET LANGUAGE TO ENGLISH            
   RddSetDefault("DBFCDX")

   SET FONT TO "DejaVu Sans Mono", 13
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()

   cLog := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + "\_Msg3.log" )
   fErase( cLog )
   SET LOGFILE TO &cLog

   cTtl      := SHOW_TITLE + SHOW_VERS
   nW        := System.ClientWidth
   nH        := System.DesktopHeight - GetTaskBarHeight()   // высота Панели задач Desktop
   cFileIco  := Icon64TempCreate()  // иконку создаём во временной папке
   App.Cargo := oHmgData()          // создадим контейнер для всего приложения
   App.Cargo:aDisplayMode := { nW, nH }
   App.Cargo:cDisplayMode := HB_NtoS(nW) + "x" + HB_NtoS(nH)

   DEFINE WINDOW Form_Main WIDTH nW HEIGHT nH TITLE cTtl ICON cFileIco ;
      MAIN NOMAXIMIZE NOSIZE TOPMOST                                   ;
      ON INIT    {|| This.TopMost := .F., This.Minimize ,;
                     DoEvents(), _wPost(1) }

      (This.Object):Event( 1, {|ow| Tsb4Test(ow) , DoEvents(),  _wPost(99,ow)  } )  // событие 1
      (This.Object):Event(99, {|ow| ow:Release()                               } )  // выход

      ON KEY F1 ACTION NIL

   END WINDOW

   ACTIVATE WINDOW Form_Main

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION Icon64TempCreate()
   LOCAL cBuff := "AAABAAEAQEAAAAEAIAAoQgAAFgAAACgAAABAAAAAgAAAAAEAIAAAAAAAAEIAAAAAAAAAAAAAAAAAAAAAAAAAAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA////////////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/////////////////ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD//wAA//8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP8AAP//AAD//wAA//8AAP//ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD//////////////////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP///////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/AAD///////////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP////////////////////////////////9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD/ZmYA/2ZmAP9mZgD///////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AL///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
   LOCAL cBin, cFile := GetUserTempFolder() + "\MiniGui_2dbf64.ico"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

////////////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb4Test(oWnd)
   LOCAL nY, nX, nW, nH, aBClr, cTitle, cWWin 
   LOCAL cPathDbf, cDbf, cAls, cCdPg, bInitForm
   LOCAL oWin, oUse, oMenu, oTsb, aEvent, oIndx

   Domethod(oWnd:Name, "Restore") // скинуть главное окно
   DO EVENTS

   cWWin := WaitWindow( {'Create a table ...', GetExeFileName() }, .T., 980, 18, NIL, BLACK, ORANGE ) // open the wait window
   DoMethod( cWWin, "Minimize" ) ; DO EVENTS
   DoMethod( cWWin, "Restore"  ) ; DO EVENTS

   SET FONT TO "DejaVu Sans Mono", 13  // новый фонт для таблицы
   ? ProcNL(), "---------------"

   nY       := nX := 0
   nW       := App.Cargo:aDisplayMode[1]
   nH       := App.Cargo:aDisplayMode[2]
   aBClr    := { 0,176,240 }  //{ 93,114,148}
   cTitle   := "WINDOW STANDARD - " + App.Cargo:cDisplayMode
   cTitle   += SPACE(5) + GetExeFileName()
   cPathDbf := GetStartUpFolder() + "\"
   cDbf     := '_Engl.dbf'
   cAls     := 'Engl'
   cCdPg    := "RU866"
   // назначим свойства объектам 
   oWin     := CreateDataWin(cTitle, aBClr, nY, nX, nW, nH)                // параметры окна
   oUse     := CreateDateDbf(cPathDbf,cDbf,cAls,cCdPg,.T.)                 // одна или несколько баз
   oIndx    := CreateDateIndex(oUse,cPathDbf)                              // создаю индексы
   oMenu    := CreateDateMenu( {1,2,3,4,99} )                              // меню-кнопки окна и события по кнопкам
   oTsb     := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)  // параметры ТСБ
   aEvent   := {}                                                  // события на окне, вызов функций
   AAdd( aEvent, { 1, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // кнопка 1
   AAdd( aEvent, { 2, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // кнопка 2
   AAdd( aEvent, { 3, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // кнопка 3
   AAdd( aEvent, { 4, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // кнопка 4
   AAdd( aEvent, {99, {|ow,ky,cn| SetProperty(ow:Name, cn, "Enabled", .T. ), ow:Release(), ky:=cn } } ) // выход

   ? ProcNL(), ALIAS(), Used() 

   // этот код выполняется уже в окне - TsbViewer.prg
   // this code is already executed in the window - TsbViewer.prg
   bInitForm  := {|ow,ob|
                  Local oc, cw, i, cv, ns := 0
                  cw := ow:Name
                  ?
                  ? "===>>> bInitForm:", cw, ob:cControlName
                  oc := ob:GetColumn("ORDKEYNO")
                  oc:nAlign  := DT_RIGHT
                  oc:nFAlign := oc:nAlign
                  oc:nSAlign := oc:nAlign
                  oc:cSpcHeading += Space( ob:nCellMarginLR )
                  FOR EACH oc IN ob:aColumns
                      ? hb_enumIndex(oc), oc:cName, oc:lEdit, oc:lCheckBox
                  NEXT
                  ?
                  FOR i := 1 TO ob:nLen
                     ob:GotoRec(i)
                     ns += ob:GetValue("TAXRATE")
                  NEXT
                  ob:GoTop()
                  cv := "Amount by field [TAXRATE] = " + HB_NtoS(ns)
                  // "LblDown" - взять из oWin:aDown
                  ob:GotoRec(20)
                  cv +=  SPACE(10) + "Jump to record 20  /  oBrw:GotoRec(20) "
                  IF GetControlIndex("LblDown", cw ) > 0
                     SetProperty(cw, "LblDown", "Value", cv)
                  ENDIF
                  Return Nil
                 }

   WaitWindow()      // close the wait window

   // Показ окна зависит от описания в oWin
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)   // окно с таблицей

   IF SELECT(cAls) > 0
      (cAls)->(dbCloseArea())  // закроем базу
   ENDIF
   
   Domethod(oWnd:Name, "Restore")  // поднять главное окно
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION my4Btn1(ow,ky,cn)
   MsgDebug(ow:Name,ky,cn,This.&(cn).caption)
   SetProperty(ow:Name, cn, "Enabled", .T. )
RETURN Nil

///////////////////////////////////////////////////////////////////////////
FUNCTION myFunc0(oWnd, nMsg, oBrw)
   MsgDebug(oWnd:Name,nMsg, oBrw:cAlias)
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDataWin(cTitle, aBClr, nY, nX, nW, nH)
   LOCAL oWin, aColor

   nY := nX := nW := nH := 0  // потом поправить
   oWin := oHmgData()
   oWin:lWait      := .T. // .T.-"WAIT", .F.="NOWAIT"
   oWin:lCenter    := .F.
   oWin:nPosY      := 0
   oWin:nPosX      := 0
   oWin:nPosW      := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   oWin:nPosH      := App.Cargo:aDisplayMode[2]  //System.ClientHeight
   //oWin:nPosH    -= GetTaskBarHeight()         // высота Панели задач Desktop, не делаем
   oWin:aBackcolor := aBClr  
   oWin:cTitle     := cTitle
   oWin:lTopmost   := .F.      // This.Topmost := lTopmost, если .T. то переключиться на другие окна будет нельзя
   oWin:bOnInit    := Nil      // по умолчанию берется в TsbViewer.prg
   oWin:bOnRelease := {||Nil}  // по умолчанию берется в TsbViewer.prg
   oWin:bIAClose   := {||Nil}  // по умолчанию берется в TsbViewer.prg
   oWin:aDown      := {}       // нет label внизу окна
   // есть label внизу окна {высота, цвет фона, цвет текста, центровка, сам текст}
   oWin:aDown      := { "LblDown", GetTitleHeight(), WHITE, {42,97,181}, .T., "! you can write something here ...." }
   aColor          := oWin:aDown[4]
   oWin:aDown[4]   := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aColor )

RETURN oWin

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateDbf(cPath, cFile, cAls, cCodePage, lShared)
   LOCAL oUse, cMsg, oError, cOld, cUse := cPath + cFile

   oUse := oHmgData()
   oUse:cFullPath := cUse
   oUse:cPath     := cPath
   oUse:cFile     := cFile
   oUse:cCodePage := cCodePage
   oUse:lShared   := lShared
   oUse:cError    := ""

   BEGIN SEQUENCE  WITH { |e|break( e ) }
      cOld := hb_cdpSelect(cCodePage) 
      IF hb_cdpSelect() == cCodePage
         // есть такая кодовая страница
         // there is such a code page
      ENDIF
      hb_cdpSelect(cOld) 
   RECOVER USING oError
      cMsg := "Code page error!;"
      cMsg += "No driver for CodePage: "
      cMsg += cCodePage + ";" + ProcNL()
      AlertStop( cMsg, "ERROR", "ZZZ_B_STOP64", 64)
      oUse:cError := cMsg
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      RETURN oUse
   END SEQUENCE

   IF hb_FileExists( cUse )

      BEGIN SEQUENCE  WITH { |e|break( e ) }

         IF lShared
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage SHARED NEW
         ELSE
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage EXCLUSIVE NEW
         ENDIF
         oUse:cAlias := ALIAS()
         oUse:lOpen  := .T.

      RECOVER USING oError

         cMsg := "Error opening Database!;"
         cMsg += "The Database is occupied by another process;"
         cMsg += cUse + ";" + ProcNL()
         AlertStop( cMsg, "ERROR")
         oUse:cError := cMsg
         oUse:cAlias := ""
         oUse:lOpen  := .F.

      END SEQUENCE

   ELSE

      cMsg := 'File not found !;' + cUse + ";" + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      oUse:cError := cMsg

   ENDIF

RETURN oUse

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateIndex(oUse,cPathTemp)                    // создать индексы
   LOCAL oIndx, cIndex, cAls, cMsg, nI, cOn, cFor, cTag

   oIndx  := oHmgData()
   cIndex := ChangeFileExt( oUse:cFullPath, '.cdx' )
   cIndex := cPathTemp + cFileNoPath( cIndex )
   DeleteFile(cIndex)   // обязательно удалить
   cAls   := oUse:cAlias

   oIndx:cIndex    := cIndex
   oIndx:aFor      := { ""         , "!Deleted()"   , "Deleted()"    , "CUSTNO>0"    }
   oIndx:aTag      := { "PRINT"    , "NO_DEL"       , "DEL"          , "Except_zero" }
   oIndx:aIndxOn   := { "CUSTNO"   , "CUSTNO"       , "CUSTNO"       , "CUSTNO"      }
   oIndx:cError    := ""
   oIndx:nSetOrder := 0

   IF LEN(cAls) > 0  // если база открыта

      IF !hb_DirExists( cPathTemp )
         cMsg := "Couldn't create indexes !; There is no such path for files - "
         cMsg += cPathTemp + ";" + ProcNL()
         AlertStop(cMsg, "ERROR")
         oIndx:cError  := cMsg
      ELSE
         dbSelectArea( cAls )
         FOR nI := 1 TO LEN(oIndx:aFor)
            cOn  := oIndx:aIndxOn[nI]
            cTag := oIndx:aTag[nI]
            cFor := oIndx:aFor[nI]
            IF LEN(cFor) == 0
               INDEX ON &cOn TAG (cTag) TO (cIndex) DESCENDING
            ELSE
               INDEX ON &cOn TAG (cTag) TO (cIndex) FOR &cFor DESCENDING
            ENDIF
         NEXT
         oIndx:nSetOrder := 1  // или так DbSetOrder(1)
      ENDIF

   ELSE
      cMsg := "Couldn't create indexes !; Database is not open!;"  + ProcNL()
      AlertStop(cMsg, "ERROR")
      oIndx:cError  := cMsg
   ENDIF

RETURN oIndx

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateMenu(aPost)             // меню-кнопки окна
   LOCAL oMenu, nWMenu, nKolvo

   oMenu := oHmgData()
   oMenu:lDebug    := .T.       // отладка, показ ошибок
   oMenu:nPosWin   := 1         // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := DT_LEFT   // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := DT_TOP    // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := { "New menu"  , "Print" , "Test-3" , "Test-4" , "Exit"      }
   oMenu:aBtnPost  := aPost     // _wPost(Х) - номер события на кнопке
   oMenu:aBColor   := { BLUE        , GRAY    , GRAY     , GRAY     , {189,30,73} }
   oMenu:lBtnIco   := .T.       // F-кнопки без иконок
   oMenu:aIcon     := { {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} ,;
                        {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} , { "iExit64x1", "iExit64x2" } }
   oMenu:nIcoSize  := 48
   //oMenu:lTextVert := .T. // вертикальный текст для кнопок
   //oMenu:lTextLeft := .F. // слева текст для кнопок
   oMenu:aFont     := { "Comic Sans MS", 15, .T., .F. , 17, "увеличение фонта кнопки" }
   oMenu:aFClr     := { BLACK , YELLOW }
   oMenu:aHelp     := {}
   oMenu:nIndent   := 0                  // отступ первой кнопки  - резерв
   oMenu:nHBtn     := 56                 // высота кнопки
   oMenu:nWBtn     := 220                // ширина кнопки
   oMenu:nGaps     := 5                  // отступ кнопки от края окна
   oMenu:nGapsBtn  := 10                 // между кнопками по ширине/высоте
   // проверим ширину всех кнопок
   nKolvo := LEN(oMenu:aCaption)
   nWMenu := oMenu:nGaps * 2 + oMenu:nWBtn * nKolvo + oMenu:nGapsBtn * nKolvo
   IF nWMenu > App.Cargo:aDisplayMode[1]  //System.ClientWidth
      oMenu:nWBtn := ( App.Cargo:aDisplayMode[1] - oMenu:nGaps*2 - oMenu:nGapsBtn * nKolvo ) / nKolvo
   ENDIF

   IF oMenu:nPosWin == 1 .OR. oMenu:nPosWin == 2
      // для 1-TopWindow, 2-BottomWindow
      oMenu:nHMenu   := oMenu:nHBtn + oMenu:nGaps * 2      // высота всего меню
   ELSE
      // для  3-LeftWindow, 4-RightWindow
      oMenu:nHMenu   := oMenu:nWBtn + oMenu:nGaps * 2      // ширина всего меню
   ENDIF

RETURN oMenu

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateTsb(oUse, c1Title, c2Title, oWin)      // параметры ТСБ
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsb, cTmp, cTyp
   LOCAL aBColor, nBColor, aGradient, nGrad

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsb := oHmgData()
   oTsb:nWGaps       := GetBorderWidth()        // отступ по ширине формы
   oTsb:nHGaps       := GetBorderHeight()       // отступ по высоте формы
   oTsb:cSupHd1Title := c1Title
   oTsb:cSupHd2Title := c2Title
   oTsb:cError       := ""
   // настройки таблицы
   oTsb:lSelector    := .T.         // F-убрать в таблице вирт.колонку SELECTOR
   oTsb:lColNumber   := .T.         // F-убрать в таблице вирт.колонку ORDKEYNO
   oTsb:aColNumber   := { 1, 60 }   // вирт.колонка с номерами - потом автоматом расчитывается ширина колонки
   oTsb:lSuperHead   := .T.         // F-убрать в таблице суперхидер
   oTsb:lSpecHd      := .T.         // F-убрать в таблице нумератор

   // цвета таблицы
   nGrad             := RGB(48,29,26)
   oTsb:aBrush       := aBColor                         // под таблицей
   oTsb:nClrNoDbf    := GetSysColor( COLOR_BTNFACE )    // селектора/нумератора/вирт.колонки
   aGradient         := { RGB(242,163,167), nGrad }
   oTsb:nClrNoEdit   := aGradient                       // шапка/подвал колонок типа "+=^"
   oTsb:nClrBackDel  := RGB(50, 50, 50)                 // фона удалённых записей
   oTsb:nClrForeDel  := CLR_GRAY                        // текст удалённых записей
   oTsb:nClr1Fore    := CLR_BLUE                        // 1 , текст в ячейках таблицы
   oTsb:nClr2Back    := nBColor       //CLR_WHITE       // 2 , фон   в ячейках таблицы
   oTsb:nClr3Fore    := CLR_YELLOW                      // 3 , текста шапки таблицы
   aGradient         := { RGB(40,122,237), nGrad }
   oTsb:nClr4Back    := aGradient                       // 4 , фона шапки таблицы
   oTsb:nClr9Fore    := CLR_YELLOW                      // 9 , текста подвала таблицы
   oTsb:nClr10Back   := aGradient                       // 10, фона подвала таблицы
   aGradient         := { RGB(96,255,255), nGrad }
   oTsb:nClr16Back   := aGradient                       // 16, фона суперхидера
   oTsb:nClr17Fore   := CLR_WHITE                       // 17, текста суперхидера
   oTsb:n1Clr16Back  := aGradient                       // 16, фона суперхидера колонка 1
   oTsb:n1Clr17Fore  := CLR_RED                         // 17, текста суперхидера колонка 1
   // цвета курсора
   oTsb:nClrFocus1   := -RGB(1,1,1)       // черная окантовка
   oTsb:nClrFocus2   := -CLR_HRED
   oTsb:nClrSeleF    := GetSysColor( COLOR_WINDOWTEXT )
   oTsb:nClrNoFocus1 := -CLR_BLUE
   oTsb:nClrNoFocus2 := -RGB( 128, 225, 225 )
   // добавка к цветам
   oTsb:lShowZebra   := .T.               // показ чётная\нечётная строка
   oTsb:nClr22Bck    := CLR_WHITE         // цвет чётная\нечётная row
   // работа с колонками  - цифры это кол-во знаков, а не пикселей
   oTsb:aWidthCol    := { {"LOGPRN", -3}, {"CUSTNO", -5}, {"FAX", +2}, {"TAXRATE", -4} }

   cAls := oUse:cAlias
   cErr := ""
   aDbf := {}                                                   // edit cell
   AADD( aDbf, { "LOGPRN"     , "L",  1, 0, "Print;recno"         , .T. } )
   AADD( aDbf, { "CUSTNO"     , "N", 15, 0, "Company;number"      , .T. } )
   AADD( aDbf, { "COMPANY"    , "C", 30, 0, "Company"             , .T. } )
   AADD( aDbf, { "ADDR1"      , "C", 30, 0, "Adres-1"             , .T. } )
   AADD( aDbf, { "ADDR2"      , "C", 30, 0, "not-show"            , .T. } )
   AADD( aDbf, { "CITY"       , "C", 15, 0, "City"                , .T. } )
   AADD( aDbf, { "STATE"      , "C", 20, 0, "State"               , .T. } )
   AADD( aDbf, { "ZIP"        , "C", 10, 0, "Zip"                 , .T. } )
   AADD( aDbf, { "COUNTRY"    , "C", 20, 0, "Country"             , .T. } )
   AADD( aDbf, { "PHONE"      , "C", 15, 0, "Phone;company"       , .T. } )
   AADD( aDbf, { "FAX"        , "C", 15, 0, "Fax;company"         , .T. } )
   AADD( aDbf, { "TAXRATE"    , "N", 19, 4, "Taxrate"             , .T. } )
   AADD( aDbf, { "CONTACT"    , "C", 20, 0, "Contact"             , .T. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "LAST INVOIC"         , .F. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "not-show"            , .T. } )

   nK      := LEN(aDbf)
   aHead   := {}  // список шапки колонок таблицы
   aFoot   := {}  // список подвала колонок таблицы
   aPict   := {}  // ширина колонок таблицы
   aName   := {}  // список PICTURE колонок таблицы
   aAlign  := {}  // список отбивки колонок таблицы
   aField  := {}  // список полей базы колонок таблицы
   aFSize  := {}  // список полей базы колонок таблицы
   aFAlign := {}  // список отбивки подвала колонок таблицы
   aEdit   := {}  // редактирование поля

   IF LEN(cAls) > 0  // если база открыта

      dbSelectArea( cAls )
      FOR nJ := 1 TO nK
         cFld := aDbf[nJ,1]
         cTyp := aDbf[nJ,2]
         cVal := aDbf[nJ,5]
         IF LOWER( cVal ) == "not-show"
            // пропуск
         ELSE
            IF FIELDNUM(cFld) == 0
               cVal := HB_ValToExp(aDbf[nJ])
               cVal := AtRepl( ";", cVal, "|" )
               cErr += HB_ValToExp(cVal) + ";"
            ELSE
               IF LEN(cVal) == 0
                  cTmp := cFld
               ELSE
                  cTmp := cVal
               ENDIF
               AADD( aHead  , cTmp )
               AADD( aFoot  , "[ " + cFld + " ]" )
               AADD( aName  , cFld      )
               AADD( aField , cFld      )
               AADD( aFAlign, DT_CENTER )
               IF cTyp == 'C' .OR. cTyp == 'M'
                  nAlgn := DT_LEFT
               ELSEIF cTyp == 'N'
                  nAlgn := DT_RIGHT
               ELSE
                  nAlgn := DT_CENTER
               ENDIF
               AADD( aAlign , nAlgn )
               AADD( aEdit  , aDbf[nJ,6] )
            ENDIF
         ENDIF
      NEXT

      IF LEN(cErr) > 0
         cMsg := "No field in the database " + Alias() + " !;"
         cErr += ProcNL()
         //AlertStop( cMsg + cErr, "ERROR")
         oTsb:cError := cMsg + cErr
      ENDIF

      oTsb:aHead   := aHead
      oTsb:aFoot   := aFoot
      oTsb:aPict   := aPict
      oTsb:aName   := aName
      oTsb:aAlign  := aAlign
      oTsb:aField  := aField
      oTsb:aFSize  := aFSize
      oTsb:aFAlign := aFAlign
      oTsb:aEdit   := aEdit
      //oTsb:aEdit  := .F.     // запрет правки всех ячеек, игнорирует aEdit

   ENDIF

RETURN oTsb

