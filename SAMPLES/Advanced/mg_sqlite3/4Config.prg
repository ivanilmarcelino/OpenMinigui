/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Настройка программы
*/

#define _HMG_OUTLOG
#include "minigui.ch"
////////////////////////////////////////////////////////////////////////////////
FUNCTION Menu4Config(oWnd,nKy,cn)
   LOCAL cForm, hFont1, hFont2, nY, nX, nMenu, aBtnObj, aBtn4, cLang, aLang

   ? ProcNL(), oWnd, nKy, cn, "|", oWnd:Name
   cForm    := oWnd:Name
   aBtnObj  := oWnd:Cargo:aBtnObj     // массив кнопок на форме
   aBtn4    := aBtnObj[4]
   ? SPACE(3) + HB_ValToExp(aBtn4)
   //  1      2          3                4       5   6   7   8
   //4  {4, "_Config", "-имя объекта", "Settings", 0, 261, 77, 69, "_Config", "-событие"}
   nY       := oWnd:Row + aBtn4[5] + aBtn4[8]
   nX       := oWnd:Col + aBtn4[6] + 5
   hFont1   := GetFontHandle( "ComSanMS" )
   hFont2   := GetFontHandle( "DlgFont" )
   nMenu    := 0
   cLang    := IIF( App.Cargo:cLang == "RU", "Установить язык: ", "Set language: ")
   cLang    += App.Cargo:cLang
   IF App.Cargo:cLang == "RU"
      aLang := { 'Настройка цветов', 'Настройка фонтов таблицы', 'Настройка фонтов меню', 'Другие настройки'}
   ELSE
      aLang := { 'Adjust colors' , 'Configure table fonts', 'Customize menu fonts', 'Other settings' }
   ENDIF

   SET MENUSTYLE EXTENDED                              // switch menu style to advanced
   //SetMenuBitmapHeight( App.Cargo:nMenuBmpHeight )   // set image size

   DEFINE CONTEXT MENU OF &cForm
       //MENUITEM "" DISABLED FONT hFont2
       //SEPARATOR
       Popup cLang FONT hFont1 // Level 2
          MENUITEM 'Russian language - RU'  ACTION {|| nMenu := 201 } FONT hFont1  ICON "iFlag_Ru32"
          MENUITEM 'English language - EN'  ACTION {|| nMenu := 202 } FONT hFont1  ICON "iFlag_En32"
       End Popup
       //SEPARATOR
       //MENUITEM aLang[1]  ACTION {|| nMenu := 1 } FONT hFont2 ICON "iColor32"
       //MENUITEM aLang[2]  ACTION {|| nMenu := 2 } FONT hFont2 ICON "iFont32"
       //MENUITEM aLang[3]  ACTION {|| nMenu := 3 } FONT hFont2 ICON "iFont32"
       //MENUITEM aLang[4]  ACTION {|| nMenu := 4 } FONT hFont2 ICON "iGear32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu == 1
      myRezerv(nMenu)
   ELSEIF nMenu == 2
      myRezerv(nMenu)
   ELSEIF nMenu == 3
      myRezerv(nMenu)
   ELSEIF nMenu == 4
      myRezerv(nMenu)
   ELSEIF nMenu == 201 .OR. nMenu == 202  // Сменить язык
      myConfig201menu(oWnd,nMenu)
   ENDIF

   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myRezerv(nMenu)
   Local cMsg, aBtnClr, aTmp, aBClr, aFClr, cTtl

   cMsg    := "RESERVE for menu: " + HB_NtoS(nMenu) + "!;;"
   cMsg    += ProcNL() + ";" + ProcNL(1)
   cTtl    := "Program configuration"
   aBtnClr := { {45,223,70} , ORANGE }
   aBClr   := {248,209,211}      // светло-красный
   aFClr   := MAROON
   aTmp    := _SetMsgAlertColors(aBClr,aFClr)  // новые цвета

   AlertInfo( cMsg, cTtl, ,"ZZZ_B_STOP64", 64, aBtnClr )

   _SetMsgAlertColors(aTmp[1],aTmp[2])       // восстановить цвета

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig201menu(oWnd, nMenu)
   LOCAL cMenu, aMenu, aMnTip, nI, aBtnObj, cForm, oIni, owc

   ? ProcNL(), oWnd:ClassName, nMenu, HB_ValToExp(hb_aParams())

   owc   := oWnd:Cargo
   cForm := oWnd:Name
   cMenu := IIF( nMenu == 201, "RU", "EN" )
   oIni  := App.Cargo:oIni

   // запись в ини-файл будет при выходе из программы
   App.Cargo:oIni:MAIN:cLang := cMenu

   App.Cargo:cLang  := cMenu

   // меняем язык верхнего меню
   aMenu   := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRu   , owc:aTopMenuEn    )
   aMnTip  := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRuTip, owc:aTopMenuEnTip )
   aBtnObj := owc:aBtnObj         // массив кнопок на форме
   // {1, "_Help", "-имя объекта", "Help", 0, 0, 77, 69, "_Help", "-событие"}

   FOR nI := 1 TO LEN(aBtnObj)
      SetProperty( cForm, aBtnObj[nI,2] , "Caption", aMenu[nI]  )
      SetProperty( cForm, aBtnObj[nI,2] , "TOOLTIP", aMnTip[nI] )
   NEXT

   DO EVENTS

   // запись в ини-файл
   oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
   oIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе

RETURN NIL
