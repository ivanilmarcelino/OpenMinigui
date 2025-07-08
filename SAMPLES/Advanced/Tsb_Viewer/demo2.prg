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

#define  _HMG_OUTLOG           // вывод отладки в файл
#include "minigui.ch"
#include "Dbinfo.ch"
#include "tsbrowse.ch"

REQUEST HB_CODEPAGE_UA1251, HB_CODEPAGE_UA866    // украинский язык
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // русский язык
REQUEST HB_LANG_BEWIN                            // белорусский язык
REQUEST DBFCDX
FIELD NN, NNN, KBOOKGRP

#define  SHOW_TITLE  "TsbViewer(c)"
#define  SHOW_VERS   SPACE(5) + "Ver 0.6 - 06.01.22"

FUNCTION Main(cMode)
   LOCAL cTtl, nW, nH, nG, nCol, nWLbl, nHLbl, nEvent, aBClr, cFileIco, cLog
   DEFAULT cMode := "2"

   PUBLIC oMain

   SET OOP ON

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET NAVIGATION EXTENDED

   RddSetDefault("DBFCDX")
   SET AUTOPEN OFF   // не открывать автоматически индексные файлы
   SET DELETED OFF

   SET FONT TO "DejaVu Sans Mono", 15
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()

   cLog := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + "\_Msg.log" )
   fErase( cLog )
   SET LOGFILE TO &cLog

   cTtl      := "TsbViewer(o) - " +  MiniGuiVersion()
   nW        := System.ClientWidth
   nH        := 100
   aBClr     := { 94, 59,185}
   nEvent    := IIF( cMode == "0", 0 , VAL(cMode) )
   cFileIco  := Icon64TempCreate()  // иконку создаём во временной папке
   App.Cargo := oHmgData()          // создадим контейнер для всего приложения
   App.Cargo:nHMain := nH           // сохраним высоту окна Main
   App.Cargo:cIco64 := cFileIco

   DEFINE WINDOW Form_Main WIDTH nW HEIGHT nH TITLE cTtl ICON cFileIco ;
      MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR  aBClr                  ;
      ON GOTFOCUS RefreshWin( ThisWindow.Handle )                      ;
      ON INIT    {|| This.TopMost := .F., This.Minimize ,;
                     DoEvents(), _wPost(nEvent) /*, _wPost(99)*/ }
      //ON RELEASE {|| dbCloseAll()      }

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
      (This.Object):Event( 0, {|ow| EventZero(ow)     } )   // событие 0
      (This.Object):Event( 2, {|ow| EventTwo(ow)      } )   // событие 2
      (This.Object):Event(99, {|ow| ow:Release()      } )   // выход

      ON KEY F1 ACTION NIL

   END WINDOW

   ACTIVATE WINDOW Form_Main

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION EventZero()

   ? ProcNL(), "--- END --- EventZero() ---"
   // _wPost(99) // закрыть главное окно

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION EventTwo()
   LOCAL oWin, oUse, oIndx, oMenu, oTsbW, aEvent

   ? ProcNL(), "--- START ------"
   // открыть базу для русского языка
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
   SET FONT TO "DejaVu Sans Mono", 13  // фонты построения в таблице берутся от этого фонта

   oWin  := CreateDataWin(1)                                             // параметры окна
   oUse  := CreateDateDbf(GetStartUpFolder(),'_Ru866.dbf',"RU_LIST","RU866",.T.)
   oIndx := CreateDateIndex(1,oUse,GetStartUpFolder())                   // создать индексы
   oMenu := CreateDateMenu( {99,1,2,3,4} )  // события                   // меню-кнопки окна
   oTsbW := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)   // параметры ТСБ
   // переключиться на ордер индекса
   oIndx:nSetOrder := 2
   // или можно так делать
   //DbSelectArea("BookAbon")
   //DbSetOrder(4)
   aEvent := {}                                             // события на окне, вызов функций
   AAdd( aEvent, {99, {|ow,ky,cn| myExit(ow,ky,cn) }} )     // выход
   AAdd( aEvent, { 1, {|ow,ky,cn| myBtn1(ow,ky,cn) }} )     // кнопка 1
   AAdd( aEvent, { 2, {|ow,ky,cn| myBtn2(ow,ky,cn) }} )     // кнопка 2
   AAdd( aEvent, { 3, {|ow,ky,cn| myBtn3(ow,ky,cn) }} )     // кнопка 3
   AAdd( aEvent, { 4, {|ow,ky,cn| myBtn4(ow,ky,cn) }} )     // кнопка 4

   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsbW, aEvent)   // окно с таблицей

   // ----- вторая таблица / second table -----
   oWin  := CreateDataWin(2)                                             // параметры окна
   oUse  := CreateDateDbf(GetStartUpFolder(),'_Ua1251.dbf',"UA_LIST","UA1251",.T.)
   oIndx := CreateDateIndex(2,oUse,GetStartUpFolder())                   // создать индексы
   oMenu := CreateDateMenu2( {99,2,4} )  // события                      // меню-кнопки окна
   oTsbW := CreateDateTsb2(oUse,oUse:cCodePage,"Checkpoint (2) !",oWin)  // параметры ТСБ

   aEvent := {}                                               // события на окне, вызов функций
   AAdd( aEvent, {99, {|ow,ky,cn| myExit(ow,ky,cn)     }} )   // выход
   AAdd( aEvent, { 2, {|ow,ky,cn| myBtn2(ow,ky,cn)     }} )   // кнопка 2
   AAdd( aEvent, { 4, {|ow,ky,cn| myBtn4Tbl2(ow,ky,cn) }} )   // кнопка 4

   // переключиться на 1 ордер индекса
   oIndx:nSetOrder := 4
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsbW, aEvent)   // окно с таблицей

   ? ProcNL(), "--- END ------"

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDataWin(nScreen)
   LOCAL oWin

   oWin := oHmgData()
   oWin:lWait      := .F.
   oWin:nPosY      := 0
   oWin:nPosX      := 0
   oWin:nPosW      := System.ClientWidth
   oWin:nPosH      := System.ClientHeight/2
   oWin:aBackcolor := {183,221,232}
   oWin:cTitle     := ""
   oWin:lCenter    := .F.
   oWin:lTopmost   := .F.
   oWin:bOnInit    := Nil
   oWin:bOnRelease := {||Nil}
   oWin:bIAClose   := {||Nil}

   IF nScreen == 2
      oWin:lWait := .T.
      oWin:nPosY := System.ClientHeight/2 
      oWin:nPosH := System.ClientHeight/2 - 1
      oWin:aBackcolor := {250,236,158}
   ENDIF

RETURN oWin


///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateDbf(cPath, cFile, cAls, cCodePage, lShared)
   LOCAL oUse, cMsg, oError, cUse := cPath + "\" + cFile

   oUse := oHmgData()
   oUse:cFullPath := cUse
   oUse:cPath     := cPath + "\"
   oUse:cFile     := cFile
   oUse:cCodePage := cCodePage
   oUse:lShared   := lShared
   oUse:cError    := ""

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
         //AlertStop( cMsg, "ERROR")
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
FUNCTION CreateDateIndex(nMode,oUse,cPathTemp)              // создать индексы
   LOCAL oIndx, cIndex, cAls, cMsg, nI, cOn, cFor, cTag

   oIndx  := oHmgData()
   cIndex := ChangeFileExt( oUse:cFullPath, '.cdx' )
   cIndex := cPathTemp + "\" + cFileNoPath( cIndex )
   DeleteFile(cIndex)   // обязательно удалить
   cAls   := oUse:cAlias

   oIndx:cIndex    := cIndex
   // (1) показ всех записей
   // (2) показ НЕ удалённых записей
   // (3) показ удалённых записей
   // (4) показ записей по условию
   IF nMode == 1
   oIndx:aFor      := { ""             , "!Deleted()"   , "Deleted()"    , "KCITY>0"       }
   oIndx:aTag      := { "KSTREET"      , "NO_DEL"       , "DEL"          , "ONLY_CITY"     }
   ELSE
   oIndx:aFor      := { ""             , "!Deleted()"   , "Deleted()"    , "KCITY=3"       }
   oIndx:aTag      := { "KSTREET"      , "NO_DEL"       , "DEL"          , "KIEV"          }
   ENDIF
   oIndx:aIndxOn   := { "UPPER(STREET)", "UPPER(STREET)", "UPPER(STREET)", "UPPER(STREET)" }
   oIndx:cError    := ""
   oIndx:nSetOrder := 0

   IF LEN(cAls) > 0  // если база открыта

      IF !ISDIRECTORY( cPathTemp )
         cMsg := "Couldn't create indexes !; There is no such path for files - "
         cMsg += cPathTemp + ";" + ProcNL()
         //AlertStop(cMsg, "ERROR")
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
         oIndx:nSetOrder := 1  //DbSetOrder(1)
      ENDIF

   ELSE
      cMsg := "Couldn't create indexes !; Database is not open!;"  + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oIndx:cError  := cMsg
   ENDIF

RETURN oIndx

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateMenu(aPost)             // меню-кнопки окна
   LOCAL oMenu

   oMenu := oHmgData()
   oMenu:lDebug   := .T.       // отладка, показ ошибок
   oMenu:nPosWin  := 1         // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign  := DT_LEFT   // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign  := DT_TOP    // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   //oMenu:aCaption := { "Выход" , "1"  , "2"  , "3"  , "4"  , "5"   }
   oMenu:aBtnPost := aPost     // _wPost(Х) - номер события на кнопке
   oMenu:aCaption := { CHR(255)    , CHR(396), CHR(397), CHR(398), CHR(340) }
   oMenu:aBColor  := { {189,30,73} , GRAY    , GRAY    , GRAY    , BLUE     }
   oMenu:lBtnIco  := .F.       // F-кнопки без иконок
   oMenu:aIcon    := {}        // иконок здесь нет
   oMenu:nIcoSize := 32
   oMenu:aFont    := { "Wingdings", 28, .F., .F. , 32, "увеличение фонта кнопки" }
   oMenu:aFClr    := { BLACK , YELLOW }
   oMenu:aHelp    := {}
   oMenu:nIndent  := 0                  // отступ первой кнопки  - резерв
   oMenu:nHBtn    := 44                 // высота кнопки
   oMenu:nWBtn    := 66 //44            // ширина кнопки
   oMenu:nGaps    := 5                  // отступ кнопки от края окна
   oMenu:nGapsBtn := 10                 // между кнопками по ширине/высоте

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
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsbW, cTmp, cTyp
   LOCAL aBColor, nBColor, aGradient, nGrad, aStru, aVal

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsbW := oHmgData()
   oTsbW:nWGaps       := GetBorderWidth()        // отступ по ширине формы
   oTsbW:nHGaps       := GetBorderHeight()       // отступ по высоте формы
   oTsbW:cSupHd1Title := c1Title
   oTsbW:cSupHd2Title := c2Title
   oTsbW:cError       := ""
   // настройки таблицы
   oTsbW:lSelector    := .T.         // F-убрать в таблице вирт.колонку SELECTOR
   oTsbW:lColNumber   := .T.         // F-убрать в таблице вирт.колонку ORDKEYNO
   oTsbW:aColNumber   := { 1, 60 }   // вирт.колонка с номерами
   oTsbW:lSuperHead   := .T.         // F-убрать в таблице суперхидер
   oTsbW:lSpecHd      := .T.         // F-убрать в таблице нумератор

   // цвета таблицы
   nGrad              := RGB(48,29,26)
   oTsbW:aBrush       := aBColor                         // под таблицей
   oTsbW:nClrNoDbf    := GetSysColor( COLOR_BTNFACE )    // селектора/нумератора/вирт.колонки
   aGradient          := { RGB(242,163,167), nGrad }
   oTsbW:nClrNoEdit   := aGradient                       // шапка/подвал колонок типа "+=^"
   oTsbW:nClrBackDel  := RGB(50, 50, 50)                 // фона удалённых записей
   oTsbW:nClrForeDel  := CLR_GRAY                        // текст удалённых записей
   oTsbW:nClr1Fore    := CLR_BLUE                        // 1 , текст в ячейках таблицы
   oTsbW:nClr2Back    := nBColor       //CLR_WHITE       // 2 , фон   в ячейках таблицы
   oTsbW:nClr3Fore    := CLR_YELLOW                      // 3 , текста шапки таблицы
   aGradient          := { RGB(40,122,237), nGrad }
   oTsbW:nClr4Back    := aGradient                       // 4 , фона шапки таблицы
   oTsbW:nClr9Fore    := CLR_YELLOW                      // 9 , текста подвала таблицы
   oTsbW:nClr10Back   := aGradient                       // 10, фона подвала таблицы
   aGradient          := { RGB(96,255,255), nGrad }
   oTsbW:nClr16Back   := aGradient                       // 16, фона суперхидера
   oTsbW:nClr17Fore   := CLR_WHITE                       // 17, текста суперхидера
   oTsbW:n1Clr16Back  := aGradient                       // 16, фона суперхидера колонка 1
   oTsbW:n1Clr17Fore  := CLR_RED                         // 17, текста суперхидера колонка 1
   // цвета курсора
   oTsbW:nClrFocus1   := -RGB(1,1,1)       // черная окантовка
   oTsbW:nClrFocus2   := -CLR_HRED
   oTsbW:nClrSeleF    := GetSysColor( COLOR_WINDOWTEXT )
   oTsbW:nClrNoFocus1 := -CLR_BLUE
   oTsbW:nClrNoFocus2 := -RGB( 128, 225, 225 )
   // добавка к цветам
   oTsbW:lShowZebra   := .F.                   // показ чётная\нечётная строка
   oTsbW:nClr22Bck    := CLR_WHITE             // цвет чётная\нечётная row
   // работа с колонками - цифры это кол-во знаков, а не пикселей 
   oTsbW:aWidthCol    := { {"STREET", +10}, {"CITY", -5}, {"DOM", -5} }

   cAls := oUse:cAlias
   cErr := ""
   aDbf := {}
   //  подготовленный список полей базы для показа в таблице
   AADD( aDbf, { "NN"            , "N",   6, 0, "№№"                , .F. } )
   AADD( aDbf, { "DATENACH"      , "D",   8, 0, "Дата;расчёта"      , .T. } )
   AADD( aDbf, { "KCITY"         , "N",   6, 0, "Not-show"          , .T. } )
   AADD( aDbf, { "KSTREET"       , "N",   6, 0, "Not-show"          , .T. } )
   AADD( aDbf, { "CITY"          , "C",  25, 0, "Город"             , .T. } )
   AADD( aDbf, { "STREET"        , "C",  30, 0, "Улица"             , .T. } )
   AADD( aDbf, { "DOM"           , "C",  12, 0, "Дом"               , .T. } )
   AADD( aDbf, { "KORPUS"        , "C",  12, 0, "Корпус"            , .T. } )
   AADD( aDbf, { "STROEN"        , "C",  12, 0, "Строение"          , .T. } )
   AADD( aDbf, { "PODEZD"        , "C",  12, 0, "Подъезд"           , .T. } )
   AADD( aDbf, { "PRIX2019"      , "N",  12, 2, "Приход;2019"       , .F. } )
   AADD( aDbf, { "NACH2019"      , "N",  12, 2, "Начислено;2019"    , .F. } )
   AADD( aDbf, { "DOLG2019"      , "N",  12, 2, "Долг;2019"         , .F. } )
   AADD( aDbf, { "PRIX2020"      , "N",  12, 2, "Приход;2020"       , .T. } )
   AADD( aDbf, { "NACH2020"      , "N",  12, 2, "Начислено;2020"    , .T. } )
   AADD( aDbf, { "DOLG2020"      , "N",  12, 2, "Долг;2020"         , .T. } )
   AADD( aDbf, { "ITOGPRIX"      , "N",  12, 2, "ИТОГО;ПРИХОД"      , .T. } )
   AADD( aDbf, { "MONTH1"        , "C",  10, 0, "Месяц с:"          , .T. } )
   AADD( aDbf, { "YEAR1"         , "N",   5, 0, "Год с:"            , .T. } )
   AADD( aDbf, { "M1PRIX"        , "N",  12, 2, "Приход;с месяца"   , .T. } )
   AADD( aDbf, { "M1NACH"        , "N",  12, 2, "Начислено;с месяца", .T. } )
   AADD( aDbf, { "M1DOLG"        , "N",  12, 2, "Долг;с месяца"     , .T. } )
   AADD( aDbf, { "PERCENT1"      , "N",   5, 0, "Процент;с месяца"  , .T. } )
   AADD( aDbf, { "MODEDIT"       , "@",   8, 0, "Правка"            , .T. } )
   AADD( aDbf, { "ID"            , "+",   4, 0, "Служебное"         , .T. } )
   AADD( aDbf, { "MODVER"        , "^",   8, 0, "Служебное"         , .T. } )
   AADD( aDbf, { "DT"            , "=",   8, 0, "Служебное"         , .T. } )
   // режим открытия списка полей базы
   oTsbW:lUseDbfListField := .F.  // T-открыть структуру базы dbStruct()
                                  // F-считать из ранее подготовленного списка

   oTsbW:lNameFieldNumba  := .F.  // T-числа вместо имени полей

   aStru := ( cAls )->( dbStruct() )
   IF oTsbW:lUseDbfListField
      aDbf := {}
      FOR nJ := 1 TO LEN(aStru)
         aVal := aStru[nJ]
         IF oTsbW:lNameFieldNumba // T-числа вместо имени полей
            AADD( aVal, HB_NtoS(nJ) )
            AADD( aVal, .T. )
         ELSE
            AADD( aVal, aVal[1] )
            AADD( aVal, .T. )
         ENDIF
         AADD( aDbf, aVal )
      NEXT
   ENDIF

   nK      := LEN(aDbf)
   aHead   := {}  // список шапки колонок таблицы
   aFoot   := {}  // ширина колонок таблицы
   aPict   := {}  // список подвала колонок таблицы
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
         oTsbW:cError := cMsg + cErr
      ENDIF

      oTsbW:aHead   := aHead
      oTsbW:aFoot   := aFoot
      oTsbW:aPict   := aPict
      oTsbW:aName   := aName
      oTsbW:aAlign  := aAlign
      oTsbW:aField  := aField
      oTsbW:aFSize  := aFSize
      oTsbW:aFAlign := aFAlign
      oTsbW:aEdit   := aEdit
      //oTsb:aEdit  := .F.     // запрет правки всех ячеек, игнорирует aEdit

   ENDIF

RETURN oTsbW

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateMenu2(aPost)             // меню-кнопки окна
   LOCAL oMenu

   oMenu := oHmgData()
   oMenu:lDebug    := .T.       // отладка, показ ошибок
   oMenu:nPosWin   := 3         // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := DT_LEFT   // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := DT_TOP    // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := { "Выход"     , "База"  , "Авторы"    }
   oMenu:aBtnPost  := aPost     // _wPost(Х) - номер события на кнопке
   oMenu:aBColor   := { {189,30,73} , GRAY    , {94,162,38} }
   oMenu:lBtnIco   := .T.       // F-кнопки без иконок
   oMenu:aIcon     := { {"ExitDr64x1","ExitDr64x2"} , {"DbInfo64x1","DbInfo64x2"}, {"About64x1","About64x2"} }
   oMenu:nIcoSize  := 64
   oMenu:lTextVert := .T. // вертикальный текст для кнопок
   oMenu:lTextLeft := .F. // слева текст для кнопок
   oMenu:aFont     := { "Comic Sans MS", 11, .F., .F. , 13, "увеличение фонта кнопки" }
   oMenu:aFClr     := { BLACK , YELLOW }
   oMenu:aHelp     := {}
   oMenu:nIndent   := 0                  // отступ первой кнопки  - резерв
   oMenu:nHBtn     := 110                // высота кнопки
   oMenu:nWBtn     := 72                 // ширина кнопки
   oMenu:nGaps     := 10                 // отступ кнопки от края окна
   oMenu:nGapsBtn  := 10                 // между кнопками по ширине/высоте

   IF oMenu:nPosWin == 1 .OR. oMenu:nPosWin == 2
      // для 1-TopWindow, 2-BottomWindow
      oMenu:nHMenu   := oMenu:nHBtn + oMenu:nGaps * 2      // высота всего меню
   ELSE
      // для  3-LeftWindow, 4-RightWindow
      oMenu:nHMenu   := oMenu:nWBtn + oMenu:nGaps * 2      // ширина всего меню
   ENDIF

RETURN oMenu

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateTsb2(oUse, c1Title, c2Title, oWin)      // параметры ТСБ
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsbW, cTmp, cTyp
   LOCAL aBColor, nBColor, aGradient, nGrad, aStru, aVal

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsbW := oHmgData()
   oTsbW:nWGaps       := GetBorderWidth()*2        // отступ по ширине формы
   oTsbW:nHGaps       := GetBorderHeight()*2       // отступ по высоте формы
   oTsbW:cSupHd1Title := c1Title
   oTsbW:cSupHd2Title := c2Title
   oTsbW:cError       := ""
   // настройки таблицы
   oTsbW:lSelector    := .F.         // F-убрать в таблице вирт.колонку SELECTOR
   oTsbW:lColNumber   := .T.         // F-убрать в таблице вирт.колонку ORDKEYNO
   oTsbW:aColNumber   := { 1, 30 }   // вирт.колонка с номерами
   oTsbW:nHeightHead  := 30          // новое значение, но можно не ставить
   oTsbW:lSuperHead   := .F.         // F-убрать в таблице суперхидер
   oTsbW:lSpecHd      := .F.         // F-убрать в таблице нумератор
   oTsbW:lFooting     := .F.         // F-убрать в таблице подвал
   oTsbW:lDrawDelAll  := .F.         // T-убрать всё: шапку/подвал/суперхидер/нумератор - мой ключ
   oTsbW:lAdjust      := .F.         // F-убрать добавление пробелов в колонки для растяжки ширины в экран

   // цвета таблицы
   nGrad              := RGB(48,29,26)
   oTsbW:aBrush       := aBColor                         // под таблицей
   oTsbW:nClrNoDbf    := GetSysColor( COLOR_BTNFACE )    // селектора/нумератора/вирт.колонки
   aGradient          := { RGB(242,163,167), nGrad }
   oTsbW:nClrNoEdit   := aGradient                       // шапка/подвал колонок типа "+=^"
   oTsbW:nClrBackDel  := RGB(50, 50, 50)                 // фона удалённых записей
   oTsbW:nClrForeDel  := CLR_GRAY                        // текст удалённых записей
   oTsbW:nClr1Fore    := CLR_BLUE                        // 1 , текст в ячейках таблицы
   oTsbW:nClr2Back    := nBColor       //CLR_WHITE       // 2 , фон   в ячейках таблицы
   oTsbW:nClr3Fore    := CLR_RED //YELLOW                      // 3 , текста шапки таблицы
   aGradient          := { RGB(40,122,237), nGrad }
   //oTsbW:nClr4Back  := aGradient                       // 4 , фона шапки таблицы
   oTsbW:nClr4Back    := GetSysColor( COLOR_BTNFACE )    // 4 , фона шапки таблицы
   oTsbW:nClr9Fore    := CLR_YELLOW                      // 9 , текста подвала таблицы
   oTsbW:nClr10Back   := aGradient                       // 10, фона подвала таблицы
   aGradient          := { RGB(96,255,255), nGrad }
   oTsbW:nClr16Back   := aGradient                       // 16, фона суперхидера
   oTsbW:nClr17Fore   := CLR_WHITE                       // 17, текста суперхидера
   oTsbW:n1Clr16Back  := aGradient                       // 16, фона суперхидера колонка 1
   oTsbW:n1Clr17Fore  := CLR_RED                         // 17, текста суперхидера колонка 1
   // цвета курсора
   oTsbW:nClrFocus1   := -RGB(1,1,1)       // черная окантовка
   oTsbW:nClrFocus2   := -CLR_HRED
   oTsbW:nClrSeleF    := GetSysColor( COLOR_WINDOWTEXT )
   oTsbW:nClrNoFocus1 := -CLR_BLUE
   oTsbW:nClrNoFocus2 := -RGB( 128, 225, 225 )
   // добавка к цветам
   oTsbW:lShowZebra   := .T.                   // показ чётная\нечётная строка
   oTsbW:nClr22Bck    := CLR_WHITE             // цвет чётная\нечётная row
   // работа с колонками - цифры это кол-во знаков, а не пикселей  
   oTsbW:aWidthCol    := { {"KSTREET", +5}, {"KCITY", +5}, {"LSHOW", +5}, {"KVIEW", +5}, {"ID", +5} }

   cAls := oUse:cAlias
   cErr := ""
   aDbf := {}
   //  подготовленный список полей базы для показа в таблице
   //  в этом примере нет подготовленного списка

   // режим открытия списка полей базы
   oTsbW:lUseDbfListField := .T.  // T-открыть структуру базы dbStruct()
                                  // F-считать из ранее подготовленного списка
   oTsbW:lNameFieldNumba  := .T.  // T-числа вместо имени полей

   aStru := ( cAls )->( dbStruct() )
   IF oTsbW:lUseDbfListField
      aDbf := {}
      FOR nJ := 1 TO LEN(aStru)
         aVal := aStru[nJ]
         IF oTsbW:lNameFieldNumba // T-числа вместо имени полей
            AADD( aVal, HB_NtoS(nJ) )
            AADD( aVal, .T. )
         ELSE
            AADD( aVal, aVal[1] )
            AADD( aVal, .T. )
         ENDIF
         AADD( aDbf, aVal )
      NEXT
   ENDIF

   nK      := LEN(aDbf)
   aHead   := {}  // список шапки колонок таблицы
   aFoot   := {}  // ширина колонок таблицы
   aPict   := {}  // список подвала колонок таблицы
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
         oTsbW:cError := cMsg + cErr
      ENDIF

      oTsbW:aHead   := aHead
      oTsbW:aFoot   := aFoot
      oTsbW:aPict   := aPict
      oTsbW:aName   := aName
      oTsbW:aAlign  := aAlign
      oTsbW:aField  := aField
      oTsbW:aFSize  := aFSize
      oTsbW:aFAlign := aFAlign
      oTsbW:aEdit   := aEdit
      //oTsb:aEdit  := .F.     // запрет правки всех ячеек, игнорирует aEdit

   ENDIF

RETURN oTsbW

///////////////////////////////////////////////////////////////////////////////
FUNCTION myExit()
   // не работает _wPost(99) // закрыть главное окно
   //DbCloseAll()
   ReleaseAllWindows()
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myBtn4(ow,ky,cn)
   LOCAL cForm, cObj  := cn
   LOCAL cMsg, cTxt, cLog, oWnd, oCtl, a2Dim, oCargo, nI, oBrw

   cLog := _SetGetLogFile()
   fErase( cLog )
   ?  "--------- " + ProcNL() + "(", ow,ky,cn,")"
   ?  "Доступные параметры по этой кнопке !"
   ?  ow:Name, ow:Type, cn
   oWnd  := ThisWindow.Object  // объект окна
   oCtl  := This.Object        // объект контола кнопки
   cForm := oWnd:Name
   ? oWnd:Name, oCtl:Name, oCtl:Type
   ? Repl("-",60)
   ? "Берём данные с кнопки - This.Cargo"
   oCargo  := This.Cargo
   a2Dim   := oCargo:GetAll(.F.)   // получить с ключём массив
   FOR nI := 1 TO Len(a2Dim)       // это список ВСЕХ ключей и их значений
      ? "  oCargo:", nI, "Key =", a2Dim[nI][1], "Val =", a2Dim[nI][2]
      IF VALTYPE(a2Dim[nI][2]) == "A"
         ?? HB_ValToExp(a2Dim[nI][2])
      ENDIF
   NEXT
   ? Repl("-",60)
   oBrw := oWnd:Cargo:oBrw       // получить данные из объекта
   ?  "oBrw = ",oBrw, oBrw:cAlias
   ? Repl("-",60)
   oCargo := oWnd:Cargo
   ? "Берём данные с формы - oWnd:Cargo"
   a2Dim  := oCargo:GetAll(.F.)   // получить с ключём массив
   FOR nI := 1 TO Len(a2Dim)
      ? "  oCargo:", nI, "Key =", a2Dim[nI][1], "Val =", a2Dim[nI][2]
      IF VALTYPE(a2Dim[nI][2]) == "A"
         ?? HB_ValToExp(a2Dim[nI][2])
      ENDIF
   NEXT
   ? Repl("-",60)
   oCargo := oWnd:Cargo:oTsb
   cTxt   := ""
   ? "Берём данные с формы - oWnd:Cargo:oTsb"
   a2Dim  := oCargo:GetAll(.F.)   // получить с ключём массив
   FOR nI := 1 TO Len(a2Dim)
      ? "  oCargo:", nI, "Key =", a2Dim[nI][1], "Val =", a2Dim[nI][2]
      IF VALTYPE(a2Dim[nI][2]) == "A"
         ?? HB_ValToExp(a2Dim[nI][2])
      ENDIF
   NEXT
   ? Repl("-",60)

   cMsg := hb_MemoRead(cLog)
   AlertInfo(cMsg)
   SetProperty(cForm, cObj, "Enabled", .T. )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myBtn1(ow,ky,cn)
   LOCAL cForm, cObj, oWnd, oCtl

   oWnd  := ThisWindow.Object  // объект окна
   oCtl  := This.Object        // объект контола кнопки
   cForm := oWnd:Name
   cObj  := cn

   ? ProcNL(), cForm, cObj, "ky=",ky, ow:Name
   BtnTsbMenu(1,cObj)       // смотреть TsbViewer.prg
   SetProperty(cForm, cObj, "Enabled", .T. )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myBtn2(ow,ky,cn)
   LOCAL cForm, cObj, oWnd, oCtl

   oWnd  := ThisWindow.Object  // объект окна
   oCtl  := This.Object        // объект контола кнопки
   cForm := oWnd:Name
   cObj  := cn

   ? ProcNL(), cForm, cObj, "ky=", ky, ow:Name
   BtnTsbMenu(2,cObj)       // смотреть TsbViewer.prg
   
   SetProperty(cForm, cObj, "Enabled", .T. )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myBtn3(ow,ky,cn)
   LOCAL cForm, cObj, oWnd, oCtl

   oWnd  := ThisWindow.Object  // объект окна
   oCtl  := This.Object        // объект контола кнопки
   cForm := oWnd:Name
   cObj  := cn

   MsgDebug(ow:Name, cForm,cObj,oCtl:Name, "ky=",ky)

   SetProperty(cForm, cObj, "Enabled", .T. )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myBtn4Tbl2(ow,ky,cn)
   LOCAL cForm, cObj, oWnd, cTitle, cMsg, nEv := ky

   ? ow:Name, ky, cn
   oWnd   := ThisWindow.Object
   cForm  := oWnd:Name
   cObj   := cn
   cTitle := "About the program - " + ow:Name
   cMsg   := SHOW_TITLE + SHOW_VERS + ";;"
   cMsg   += "(c) 2021 Verchenko Andrey <verchenkoag@gmail.com>;"
   cMsg   += "(c) 2021 Sergej Kiselev <bilance@bilance.lv>;;"
   cMsg   += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";"
   cMsg   += "(c) Grigory Filatov http://www.hmgextended.com;;"
   cMsg   += PadC( "This program is Freeware!", 60 ) + ";"
   cMsg   += PadC( "Copying is allowed!", 60 ) + ";"

   SET MSGALERT BACKCOLOR TO { 203, 236, 176 }    // for HMG_Alert()

   AlertInfo( cMsg, cTitle, App.Cargo:cIco64, 64, {RED} )

   SET MSGALERT BACKCOLOR TO { 183, 221, 232 }    // for HMG_Alert()

   SetProperty(cForm, cObj, "Enabled", .T. )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
//#define WM_PAINT  15                     // перересовка объектов на форме
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

