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
FUNCTION Menu6Config(oWnd,nKy,cn,oBrw)
   LOCAL cForm, hFont1, hFont2, nY, nX, nMenu, cImg1, cImg2, lAutoInx
   LOCAL aBtnObj, aBtn6, cLang, aLang, cIndx, cLngCdPg, cLngDrv, cLngShrd
   LOCAL cLngSDel, cLSet

   ? ProcNL(), oWnd, nKy, cn, oBrw, oWnd:Name
   cForm    := oWnd:Name
   aBtnObj  := oWnd:Cargo:aBtnObj     // массив кнопок на форме
   aBtn6    := aBtnObj[6]
   ? SPACE(3) + HB_ValToExp(aBtn6)
   //  1      2          3                4       5   6   7   8
   // {5, "_Config", "-имя объекта", "Настройки", 0, 384, 86, 69, "_Config", "-событие"}
   nY       := oWnd:Row + aBtn6[5] + aBtn6[8]
   nX       := oWnd:Col + aBtn6[6] + 5
   lAutoInx := App.Cargo:oIni:MAIN:lAutoIndexOpen
   hFont1   := GetFontHandle( "ComSanMS" )
   hFont2   := GetFontHandle( "DlgFont" )
   nMenu    := 0
   cImg1    := "bAddFile32"     // ON
   cImg2    := "bAddFile32x2"   // OFF
   cLang    := IIF( App.Cargo:cLang == "RU", "Интерфейс программы: ", "Program interface: ")
   cLang    += App.Cargo:cLang
   cLngDrv  := IIF( App.Cargo:cLang == "RU", "Сменить драйвер БД: ", "Change DB driver: ")
   cLngDrv  += App.Cargo:oIni:MAIN:DrvDbf
   cLngCdPg := IIF( App.Cargo:cLang == "RU", "Сменить кодовую страницу БД: ", "Change DB code page: ")
   cLngCdPg += App.Cargo:oIni:MAIN:CodePade
   cLngShrd := IIF( App.Cargo:cLang == "RU", "Режим открытия БД: ", "DB opening mode: ")
   cLngShrd += App.Cargo:oIni:MAIN:Shared
   cLngSDel := IIF( App.Cargo:cLang == "RU", "Удалённые записи в БД: ", "Deleted recno in the DB: ")
   cLngSDel += "SET DELETED " + App.Cargo:oIni:MAIN:SetDeleted

   IF App.Cargo:cLang == "RU"
      aLang := { 'Настройка цветов', 'Настройка фонтов таблицы', 'Настройка фонтов меню', 'Другие настройки'}
      cIndx := 'Автооткрытие индексного файла'
      cLSet := "Запуск программы - кодовая страница"
   ELSE
      aLang := { 'Adjust colors' , 'Configure table fonts', 'Customize menu fonts', 'Other settings' }
      cIndx := 'Auto-open index file'
      cLSet := "Program startup - code page"
   ENDIF

   SET MENUSTYLE EXTENDED                              // switch menu style to advanced
   //SetMenuBitmapHeight( App.Cargo:nMenuBmpHeight )   // set image size

   DEFINE CONTEXT MENU OF &cForm
       //MENUITEM "" DISABLED FONT hFont2
       //SEPARATOR
       Popup cIndx IMAGE IIF(lAutoInx, cImg1, cImg2) FONT hFont1 // Level 2
          MENUITEM 'SET AUTOPEN ON  - enable'  ACTION {|| nMenu := 101 } FONT hFont1 ICON "iAddFile32"
          MENUITEM 'SET AUTOPEN OFF - disable' ACTION {|| nMenu := 102 } FONT hFont1 ICON "iAddFile32x2"
       End Popup
       SEPARATOR
       MENUITEM cLngDrv                        ACTION {|| nMenu := 103 } FONT hFont1 ICON "iDbase32x3"
       MENUITEM cLngCdPg                       ACTION {|| nMenu := 104 } FONT hFont1 ICON "iLang32"
       MENUITEM cLngShrd                       ACTION {|| nMenu := 105 } FONT hFont1 ICON "iDbase32x2"
       MENUITEM cLngSDel                       ACTION {|| nMenu := 106 } FONT hFont1 ICON "iDbase32x1"
       SEPARATOR
       Popup cLang FONT hFont1 // Level 2
          MENUITEM 'Russian language - RU'  ACTION {|| nMenu := 201 } FONT hFont1 ICON "iFlag_Ru32"
          MENUITEM 'English language - EN'  ACTION {|| nMenu := 202 } FONT hFont1 ICON "iFlag_En32"
       End Popup
       //SEPARATOR
       //MENUITEM aLang[1]  ACTION {|| nMenu := 1 } FONT hFont2 ICON "iColor32"
       //MENUITEM aLang[2]  ACTION {|| nMenu := 2 } FONT hFont2 ICON "iFont32"
       //MENUITEM aLang[3]  ACTION {|| nMenu := 3 } FONT hFont2 ICON "iFont32"
       //MENUITEM aLang[4]  ACTION {|| nMenu := 4 } FONT hFont2 ICON "iGear32"
       SEPARATOR
       Popup cLSet FONT hFont1 IMAGE "bDbfFile32" // Level 2
          // Кодовая страница для баз по умолчанию не используется нигде - так это меню не нужно
          // The default code page for databases is not used anywhere - so this menu is not needed
          //MENUITEM 'SET CODEPAGE TO - ' + App.Cargo:cSetCdpg ACTION {|| nMenu := 301 } FONT hFont1
          MENUITEM 'SET LANGUAGE TO - ' + App.Cargo:cSetLang ACTION {|| nMenu := 302 } FONT hFont1
       End Popup

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
   ELSEIF nMenu == 101 .OR. nMenu == 102  // Подключить индексный файл
      myConfig101menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 103                    // Сменить драйвер БД
      myConfig103menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 104                    // Сменить кодовую страницу БД
      myConfig104menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 105                    // Режим открытия БД
      myConfig105menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 106                    // Удалённые записи в БД
      myConfig106menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 201 .OR. nMenu == 202  // Сменить язык интерфейса
      myConfig201menu(oWnd,oBrw,nMenu)
   ELSEIF nMenu == 301 .OR. nMenu == 302  // Сменить язык  SET CODEPAGE/SET LANGUAGE
      myConfig301menu(oWnd,oBrw,nMenu)
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
FUNCTION myConfig101menu(oWnd, oBrw, nMenu)
   LOCAL lMenu

   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, HB_ValToExp(hb_aParams())

   lMenu := IIF( nMenu == 101, .T., .F. )
   // запись в ини-файл будет при выходе из программы
   App.Cargo:oIni:MAIN:lAutoIndexOpen := lMenu

   // автооткрытие индексного файла
   IF App.Cargo:oIni:MAIN:lAutoIndexOpen
      SET AUTOPEN ON
   ELSE
      SET AUTOPEN OFF
   ENDIF

   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig103menu(oWnd, oBrw, nMenu)    // Сменить драйвер БД
   LOCAL cStr, aRet
   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, nMenu, HB_ValToExp(hb_aParams())
   SET WINDOW THIS TO oWnd:Name        // ОБЯЗАТЕЛЬНО !!!
   aRet := myDriverDbf()               // -> util_dbf.prg
   SET WINDOW THIS TO
   IF LEN(aRet) > 0
      cStr := aRet[2]
      // запись в ини-файл будет при выходе из программы
      App.Cargo:oIni:MAIN:DrvDbf := cStr
   ENDIF
   DO EVENTS
RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig104menu(oWnd, oBrw, nMenu)    // Сменить кодовую страницу БД
   LOCAL cStr, aRet
   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, nMenu, HB_ValToExp(hb_aParams())
   SET WINDOW THIS TO oWnd:Name        // ОБЯЗАТЕЛЬНО !!!
   aRet := myCodePageDbf()             // -> util_dbf.prg
   SET WINDOW THIS TO
   IF LEN(aRet) > 0
      cStr := aRet[2]
      // запись в ини-файл будет при выходе из программы
      App.Cargo:oIni:MAIN:CodePade := cStr
   ENDIF
   DO EVENTS
RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig105menu(oWnd, oBrw, nMenu)    // Сменить Режим открытия БД
   LOCAL cStr, aRet
   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, nMenu, HB_ValToExp(hb_aParams())
   SET WINDOW THIS TO oWnd:Name        // ОБЯЗАТЕЛЬНО !!!
   aRet := myUseMode()
   SET WINDOW THIS TO
   IF LEN(aRet) > 0
      cStr := aRet[2]
      // запись в ини-файл будет при выходе из программы
      App.Cargo:oIni:MAIN:Shared := cStr
   ENDIF
   DO EVENTS
RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig106menu(oWnd, oBrw, nMenu)    // Удалённые записи в БД
   LOCAL cStr, aRet
   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, nMenu, HB_ValToExp(hb_aParams())
   // запись в ини-файл будет при выходе из программы
   SET WINDOW THIS TO oWnd:Name        // ОБЯЗАТЕЛЬНО !!!
   aRet := myUseDelete()
   SET WINDOW THIS TO
   IF LEN(aRet) > 0
      cStr := aRet[2]
      // запись в ини-файл будет при выходе из программы
      App.Cargo:oIni:MAIN:SetDeleted := cStr
   ENDIF
   DO EVENTS
RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION myConfig201menu(oWnd, oBrw, nMenu)
   LOCAL cMenu, aMenu, aMnTip, nI, aBtnObj, cForm, owc

   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, HB_ValToExp(hb_aParams())

   owc   := oWnd:Cargo
   cForm := oWnd:Name
   cMenu := IIF( nMenu == 201, "RU", "EN" )
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

   owc:cMsg3 := IIF( App.Cargo:cLang == "RU", owc:cRus, owc:cEng)
   SetProperty( cForm, owc:aLblUp[3] , "Value", owc:cMsg3  )

   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////////
// Сменить язык  SET CODEPAGE/SET LANGUAGE
FUNCTION myConfig301menu(oWnd, oBrw, nMenu)
   LOCAL cForm, owc, aRet, cStr, aObjLbl

   ? ProcNL(), oWnd:ClassName, oBrw:ClassName, HB_ValToExp(hb_aParams())

   owc     := oWnd:Cargo
   cForm   := oWnd:Name
   aObjLbl := oWnd:Cargo:aLblUp           // строки подсказки
   //owc:aLblUp := { "Lbl_1", "Lbl_2" , "Lbl_3"}        // строки подсказки
   cStr    := ""

   SET WINDOW THIS TO oWnd:Name        // ОБЯЗАТЕЛЬНО !!!
   aRet := myCodePageDbf()             // -> util_dbf.prg
   SET WINDOW THIS TO
   IF LEN(aRet) > 0
      cStr := aRet[2]
   ENDIF
   DO EVENTS

   IF LEN(cStr) > 0
      // запись в ини-файл будет при выходе из программы
      IF nMenu == 301
         App.Cargo:cSetCdpg               := cStr
         App.Cargo:oIni:MAIN:SET_CODEPAGE := cStr
         HB_CDPSELECT( App.Cargo:cSetCdpg )
      ELSE
         App.Cargo:cSetLang                  := cStr
         App.Cargo:oIni:MAIN:SET_LANGUAGE    := cStr
         owc:cLang := "HB_LANGSELECT()= " + cStr
         SetProperty(cForm, aObjLbl[3], "Value", owc:cLang)   // показ строки подсказки
         IF cStr == "RU1251" .OR. cStr == "RU866"
            SET LANGUAGE TO RUSSIAN
         ELSE
            HB_LANGSELECT( App.Cargo:cSetLang )
         ENDIF
      ENDIF
   ENDIF

   DO EVENTS

RETURN NIL
