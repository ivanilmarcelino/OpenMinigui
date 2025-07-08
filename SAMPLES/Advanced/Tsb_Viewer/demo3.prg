/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ��������/������ Dbf �����. �����/�������� �� ����
 * ������ ������ � ����� ���������. ������ ������ ��������� � �������� ���������
 * View/edit Dbf file. Options/properties by base
 * The icon is sewn into the program text.
 * The icon resource was placed in the source code of the program
*/

REQUEST DBFCDX
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // ������� ����

#define  _HMG_OUTLOG           // ����� ������� � ����
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
   SET AUTOPEN OFF   // �� ��������� ������������� ��������� �����
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
   nH        := System.DesktopHeight - GetTaskBarHeight()   // ������ ������ ����� Desktop
   cFileIco  := Icon64TempCreate()  // ������ ������ �� ��������� �����
   App.Cargo := oHmgData()          // �������� ��������� ��� ����� ����������
   App.Cargo:aDisplayMode := { nW, nH }
   App.Cargo:cDisplayMode := HB_NtoS(nW) + "x" + HB_NtoS(nH)

   DEFINE WINDOW Form_Main WIDTH nW HEIGHT nH TITLE cTtl ICON cFileIco ;
      MAIN NOMAXIMIZE NOSIZE TOPMOST                                   ;
      ON INIT    {|| This.TopMost := .F., This.Minimize ,;
                     DoEvents(), _wPost(1) }

      (This.Object):Event( 1, {|ow| Tsb4Test(ow) , DoEvents(),  _wPost(99,ow)  } )  // ������� 1
      (This.Object):Event(99, {|ow| ow:Release()                               } )  // �����

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

   Domethod(oWnd:Name, "Restore") // ������� ������� ����
   DO EVENTS

   cWWin := WaitWindow( {'Create a table ...', GetExeFileName() }, .T., 980, 18, NIL, BLACK, ORANGE ) // open the wait window
   DoMethod( cWWin, "Minimize" ) ; DO EVENTS
   DoMethod( cWWin, "Restore"  ) ; DO EVENTS

   SET FONT TO "DejaVu Sans Mono", 13  // ����� ���� ��� �������
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
   // �������� �������� �������� 
   oWin     := CreateDataWin(cTitle, aBClr, nY, nX, nW, nH)                // ��������� ����
   oUse     := CreateDateDbf(cPathDbf,cDbf,cAls,cCdPg,.T.)                 // ���� ��� ��������� ���
   oIndx    := CreateDateIndex(oUse,cPathDbf)                              // ������ �������
   oMenu    := CreateDateMenu( {1,2,3,4,99} )                              // ����-������ ���� � ������� �� �������
   oTsb     := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)  // ��������� ���
   aEvent   := {}                                                  // ������� �� ����, ����� �������
   AAdd( aEvent, { 1, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // ������ 1
   AAdd( aEvent, { 2, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // ������ 2
   AAdd( aEvent, { 3, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // ������ 3
   AAdd( aEvent, { 4, {|ow,ky,cn| my4Btn1(ow,ky,cn)        } } )   // ������ 4
   AAdd( aEvent, {99, {|ow,ky,cn| SetProperty(ow:Name, cn, "Enabled", .T. ), ow:Release(), ky:=cn } } ) // �����

   ? ProcNL(), ALIAS(), Used() 

   // ���� ��� ����������� ��� � ���� - TsbViewer.prg
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
                  // "LblDown" - ����� �� oWin:aDown
                  ob:GotoRec(20)
                  cv +=  SPACE(10) + "Jump to record 20  /  oBrw:GotoRec(20) "
                  IF GetControlIndex("LblDown", cw ) > 0
                     SetProperty(cw, "LblDown", "Value", cv)
                  ENDIF
                  Return Nil
                 }

   WaitWindow()      // close the wait window

   // ����� ���� ������� �� �������� � oWin
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)   // ���� � ��������

   IF SELECT(cAls) > 0
      (cAls)->(dbCloseArea())  // ������� ����
   ENDIF
   
   Domethod(oWnd:Name, "Restore")  // ������� ������� ����
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

   nY := nX := nW := nH := 0  // ����� ���������
   oWin := oHmgData()
   oWin:lWait      := .T. // .T.-"WAIT", .F.="NOWAIT"
   oWin:lCenter    := .F.
   oWin:nPosY      := 0
   oWin:nPosX      := 0
   oWin:nPosW      := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   oWin:nPosH      := App.Cargo:aDisplayMode[2]  //System.ClientHeight
   //oWin:nPosH    -= GetTaskBarHeight()         // ������ ������ ����� Desktop, �� ������
   oWin:aBackcolor := aBClr  
   oWin:cTitle     := cTitle
   oWin:lTopmost   := .F.      // This.Topmost := lTopmost, ���� .T. �� ������������� �� ������ ���� ����� ������
   oWin:bOnInit    := Nil      // �� ��������� ������� � TsbViewer.prg
   oWin:bOnRelease := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:bIAClose   := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:aDown      := {}       // ��� label ����� ����
   // ���� label ����� ���� {������, ���� ����, ���� ������, ���������, ��� �����}
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
         // ���� ����� ������� ��������
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
FUNCTION CreateDateIndex(oUse,cPathTemp)                    // ������� �������
   LOCAL oIndx, cIndex, cAls, cMsg, nI, cOn, cFor, cTag

   oIndx  := oHmgData()
   cIndex := ChangeFileExt( oUse:cFullPath, '.cdx' )
   cIndex := cPathTemp + cFileNoPath( cIndex )
   DeleteFile(cIndex)   // ����������� �������
   cAls   := oUse:cAlias

   oIndx:cIndex    := cIndex
   oIndx:aFor      := { ""         , "!Deleted()"   , "Deleted()"    , "CUSTNO>0"    }
   oIndx:aTag      := { "PRINT"    , "NO_DEL"       , "DEL"          , "Except_zero" }
   oIndx:aIndxOn   := { "CUSTNO"   , "CUSTNO"       , "CUSTNO"       , "CUSTNO"      }
   oIndx:cError    := ""
   oIndx:nSetOrder := 0

   IF LEN(cAls) > 0  // ���� ���� �������

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
         oIndx:nSetOrder := 1  // ��� ��� DbSetOrder(1)
      ENDIF

   ELSE
      cMsg := "Couldn't create indexes !; Database is not open!;"  + ProcNL()
      AlertStop(cMsg, "ERROR")
      oIndx:cError  := cMsg
   ENDIF

RETURN oIndx

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateMenu(aPost)             // ����-������ ����
   LOCAL oMenu, nWMenu, nKolvo

   oMenu := oHmgData()
   oMenu:lDebug    := .T.       // �������, ����� ������
   oMenu:nPosWin   := 1         // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := DT_LEFT   // �������������� ������: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := DT_TOP    // ������������ ������: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := { "New menu"  , "Print" , "Test-3" , "Test-4" , "Exit"      }
   oMenu:aBtnPost  := aPost     // _wPost(�) - ����� ������� �� ������
   oMenu:aBColor   := { BLUE        , GRAY    , GRAY     , GRAY     , {189,30,73} }
   oMenu:lBtnIco   := .T.       // F-������ ��� ������
   oMenu:aIcon     := { {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} ,;
                        {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} , { "iExit64x1", "iExit64x2" } }
   oMenu:nIcoSize  := 48
   //oMenu:lTextVert := .T. // ������������ ����� ��� ������
   //oMenu:lTextLeft := .F. // ����� ����� ��� ������
   oMenu:aFont     := { "Comic Sans MS", 15, .T., .F. , 17, "���������� ����� ������" }
   oMenu:aFClr     := { BLACK , YELLOW }
   oMenu:aHelp     := {}
   oMenu:nIndent   := 0                  // ������ ������ ������  - ������
   oMenu:nHBtn     := 56                 // ������ ������
   oMenu:nWBtn     := 220                // ������ ������
   oMenu:nGaps     := 5                  // ������ ������ �� ���� ����
   oMenu:nGapsBtn  := 10                 // ����� �������� �� ������/������
   // �������� ������ ���� ������
   nKolvo := LEN(oMenu:aCaption)
   nWMenu := oMenu:nGaps * 2 + oMenu:nWBtn * nKolvo + oMenu:nGapsBtn * nKolvo
   IF nWMenu > App.Cargo:aDisplayMode[1]  //System.ClientWidth
      oMenu:nWBtn := ( App.Cargo:aDisplayMode[1] - oMenu:nGaps*2 - oMenu:nGapsBtn * nKolvo ) / nKolvo
   ENDIF

   IF oMenu:nPosWin == 1 .OR. oMenu:nPosWin == 2
      // ��� 1-TopWindow, 2-BottomWindow
      oMenu:nHMenu   := oMenu:nHBtn + oMenu:nGaps * 2      // ������ ����� ����
   ELSE
      // ���  3-LeftWindow, 4-RightWindow
      oMenu:nHMenu   := oMenu:nWBtn + oMenu:nGaps * 2      // ������ ����� ����
   ENDIF

RETURN oMenu

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateTsb(oUse, c1Title, c2Title, oWin)      // ��������� ���
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsb, cTmp, cTyp
   LOCAL aBColor, nBColor, aGradient, nGrad

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsb := oHmgData()
   oTsb:nWGaps       := GetBorderWidth()        // ������ �� ������ �����
   oTsb:nHGaps       := GetBorderHeight()       // ������ �� ������ �����
   oTsb:cSupHd1Title := c1Title
   oTsb:cSupHd2Title := c2Title
   oTsb:cError       := ""
   // ��������� �������
   oTsb:lSelector    := .T.         // F-������ � ������� ����.������� SELECTOR
   oTsb:lColNumber   := .T.         // F-������ � ������� ����.������� ORDKEYNO
   oTsb:aColNumber   := { 1, 60 }   // ����.������� � �������� - ����� ��������� ������������� ������ �������
   oTsb:lSuperHead   := .T.         // F-������ � ������� ����������
   oTsb:lSpecHd      := .T.         // F-������ � ������� ���������

   // ����� �������
   nGrad             := RGB(48,29,26)
   oTsb:aBrush       := aBColor                         // ��� ��������
   oTsb:nClrNoDbf    := GetSysColor( COLOR_BTNFACE )    // ���������/����������/����.�������
   aGradient         := { RGB(242,163,167), nGrad }
   oTsb:nClrNoEdit   := aGradient                       // �����/������ ������� ���� "+=^"
   oTsb:nClrBackDel  := RGB(50, 50, 50)                 // ���� �������� �������
   oTsb:nClrForeDel  := CLR_GRAY                        // ����� �������� �������
   oTsb:nClr1Fore    := CLR_BLUE                        // 1 , ����� � ������� �������
   oTsb:nClr2Back    := nBColor       //CLR_WHITE       // 2 , ���   � ������� �������
   oTsb:nClr3Fore    := CLR_YELLOW                      // 3 , ������ ����� �������
   aGradient         := { RGB(40,122,237), nGrad }
   oTsb:nClr4Back    := aGradient                       // 4 , ���� ����� �������
   oTsb:nClr9Fore    := CLR_YELLOW                      // 9 , ������ ������� �������
   oTsb:nClr10Back   := aGradient                       // 10, ���� ������� �������
   aGradient         := { RGB(96,255,255), nGrad }
   oTsb:nClr16Back   := aGradient                       // 16, ���� �����������
   oTsb:nClr17Fore   := CLR_WHITE                       // 17, ������ �����������
   oTsb:n1Clr16Back  := aGradient                       // 16, ���� ����������� ������� 1
   oTsb:n1Clr17Fore  := CLR_RED                         // 17, ������ ����������� ������� 1
   // ����� �������
   oTsb:nClrFocus1   := -RGB(1,1,1)       // ������ ���������
   oTsb:nClrFocus2   := -CLR_HRED
   oTsb:nClrSeleF    := GetSysColor( COLOR_WINDOWTEXT )
   oTsb:nClrNoFocus1 := -CLR_BLUE
   oTsb:nClrNoFocus2 := -RGB( 128, 225, 225 )
   // ������� � ������
   oTsb:lShowZebra   := .T.               // ����� ������\�������� ������
   oTsb:nClr22Bck    := CLR_WHITE         // ���� ������\�������� row
   // ������ � ���������  - ����� ��� ���-�� ������, � �� ��������
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
   aHead   := {}  // ������ ����� ������� �������
   aFoot   := {}  // ������ ������� ������� �������
   aPict   := {}  // ������ ������� �������
   aName   := {}  // ������ PICTURE ������� �������
   aAlign  := {}  // ������ ������� ������� �������
   aField  := {}  // ������ ����� ���� ������� �������
   aFSize  := {}  // ������ ����� ���� ������� �������
   aFAlign := {}  // ������ ������� ������� ������� �������
   aEdit   := {}  // �������������� ����

   IF LEN(cAls) > 0  // ���� ���� �������

      dbSelectArea( cAls )
      FOR nJ := 1 TO nK
         cFld := aDbf[nJ,1]
         cTyp := aDbf[nJ,2]
         cVal := aDbf[nJ,5]
         IF LOWER( cVal ) == "not-show"
            // �������
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
      //oTsb:aEdit  := .F.     // ������ ������ ���� �����, ���������� aEdit

   ENDIF

RETURN oTsb

