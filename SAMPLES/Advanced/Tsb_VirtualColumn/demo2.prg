/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Виртуальные колонки в Tsbrowse для dbf-файла
 * Объединение 2-х dbf в одну таблицу
 * Цвет текста и фона ячеек записываем во временную БД
 * Фильтр по виртуальным колонкам, цвета по виртуальным колонкам
 * Создать Dbf-файл в памяти через HB_MEMIO
 * Virtual columns in Tsbrowse for dbf file
 * Combining 2 dbf into one table
 * The color of the text and background of the cells is written to the temporary database
 * Filter by virtual columns, colors by virtual columns
 * Create Dbf file in memory via HB_MEMIO
*/

#define _HMG_OUTLOG
#define SHOW_TITLE  "Virtual columns in Tsbrowse / Combining 2 dbf into one table ( " + cFileNoPath(App.ExeName) + " )"
#define VIRT_COLUMN_1      1
#define VIRT_COLUMN_2      2
#define VIRT_COLUMN_3      3
#define VIRT_COLUMN_4      4
#define VIRT_COLUMN_5      5
#define VIRT_COLUMN_6      6
#define VIRT_COLUMN_END    6
#define VIRT_COLUMN_MAX    ( VIRT_COLUMN_END + 1 )
#define NAME_FILE          1
#define NAME_ALIAS         2
#define NAME_CDP           3
#define NAME_VIA           4
#define NAME_TEMP          5
#define NAME_TEMP_ALIAS    6


#include "minigui.ch"
#include "TSBrowse.ch"

REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST DBFNTX, DBFCDX, DBFFPT
REQUEST HB_MEMIO

MEMVAR oPubApp    // лучше так делать ( для всех модулей можно в *.ch файле располагать )
//////////////////////////////////////////////////////////////////////
PROCEDURE Main( cFile, cTmpPath )
   LOCAL cFile1, cAls1, cCdp1, cVia1, cFile2, cAls2, cCdp2, cVia2
   LOCAL oBrw1, oBrw2, nY, nX, nW, nH, nC, nWPrt, cFileTmp1, cFileTmp2
   LOCAL hFont1, hFont2, hFont3, cPthStart, cPthTmp
   LOCAL cAlsTmp1, cAlsTmp2, lOpen1, lOpen2, aSupHd1, aSupHd2
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL lTmpErase  := .T.              // удалять tmp файлы, если они не в mem:
   Default cTmpPath := '.\', cFile := "demo.dbf"

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   RddSetDefault( 'DBFCDX' )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   ON                                         // автоматически открывать индексные файлы
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET MSGALERT BACKCOLOR TO { 141, 179, 226 }               // for HMG_Alert()
   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono" SIZE 12  // for HMG_Alert()
   SET MENUSTYLE EXTENDED                                    // switch menu style to advanced
   SetMenuBitmapHeight( 20 )                                 // set icon size 20x20

   SET DEFAULT ICON TO "1MAIN_ICO"
   SET FONT TO cFont, nSize

   // в качестве примера для создания объектов.  !!! после команды SET OOP ON
   PUBLIC oPubApp                                    // назвал покороче, удобнее писать
   oPubApp := oKeyData()                             // создать объект (контейнер) для PUBLIC переменной
   oPubApp:cCurDir  := GetStartUpFolder() + "\"
   oPubApp:cDbfDir  := oPubApp:cCurDir + "DBASE" + "\"
   oPubApp:cLogFile := oPubApp:cCurDir + "_Msg.log"
   oPubApp:aBColor  := SILVER                        // Color window
   oPubApp:cTmpPath := cTmpPath
   oPubApp:cFile    := cFile

   _SetGetLogFile( oPubApp:cLogFile ) ; fErase( _SetGetLogFile() )

   cPthTmp    := oPubApp:cTmpPath      // создать/открыть в текущей папке или mem:
   cPthStart  := oPubApp:cCurDir       // тек. каталог

   // создать объект (контейнер) для приложения (App.) и вернуть оттуда переменные
   WITH OBJECT (App.Object):Cargo := myAppCargoInit( cPthStart, cPthTmp, cFile, lTmpErase )
      cFile1     := :aFile1[ NAME_FILE  ]
      cFile2     := :aFile2[ NAME_FILE  ]
      cAls1      := :aFile1[ NAME_ALIAS ]
      cAls2      := :aFile2[ NAME_ALIAS ]
      cCdp1      := :aFile1[ NAME_CDP   ]
      cCdp2      := :aFile2[ NAME_CDP   ]
      cVia1      := :aFile1[ NAME_VIA   ]
      cVia2      := :aFile2[ NAME_VIA   ]
      cFileTmp1  := :aFile1[ NAME_TEMP  ]
      cFileTmp2  := :aFile2[ NAME_TEMP  ]
      cAlsTmp1   := :aFile1[ NAME_TEMP_ALIAS ]
      cAlsTmp2   := :aFile2[ NAME_TEMP_ALIAS ]
      aSupHd1    := :aSupHd1 // суперхидер таблицы-1
      aSupHd2    := :aSupHd2 // суперхидер таблицы-2
   END WITH

   // создать временную базу по существующей базе
   lOpen1 := CreateMemTmp( cFile1, cAls1, cCdp1, cVia1, cAlsTmp1, cFileTmp1 )
   lOpen2 := CreateMemTmp( cFile2, cAls2, cCdp2, cVia2, cAlsTmp2, cFileTmp2 )

   IF !lOpen1 .or. !lOpen2 ; QUIT
   ENDIF

   myFont( .T., nSize )  // загрузить свои фонты для таблицы

   hFont1  := GetFontHandle( "TsbNorm"   )
   hFont2  := GetFontHandle( "TsbBold"   )
   hFont3  := GetFontHandle( "TsbSuperH" )

   nY := nX := 0

   DEFINE WINDOW Form_Main                             ;
      TITLE          SHOW_TITLE                        ;
      BACKCOLOR      oPubApp:aBColor                   ;
      MAIN TOPMOST   NOMAXIMIZE NOSIZE                 ;
      ON INIT {|| This.Topmost := .F., _wPost(5) }     ;         // _wPost исп. для завершения ON INIT
      ON RELEASE {|| CloseMemTmp(cFileTmp1, cAlsTmp1), ;         // нельзя убирать, т.к. если mem:,
                     CloseMemTmp(cFileTmp2, cAlsTmp2), ;         // то надо обязательно деать
                     DbCloseAll(), myFont() }                    // dbDrop(cTmp, cTmp, 'DBFCDX')

      (This.Object):Cargo := oKeyData()         // создать объект (контейнер) для окна Form_Main
      (This.Object):Cargo:oBrwFocus := Nil

      (This.Object):Event(1, {|| myVirtColumColorSaveCell(1) })  // заполнение табл. 1
      (This.Object):Event(2, {|| myVirtColumColorSaveCell(2) })  // заполнение табл. 2
      (This.Object):Event(3, {|| myColorsInitTempDbf(1) })       // запись цвета в tempDBF 1
      (This.Object):Event(4, {|| myColorsInitTempDbf(2) })       // запись цвета в tempDBF 2
      (This.Object):Event(5, {|| _wSend(3), _wSend(4) } )

      nW := This.ClientWidth       // ширина окна

      DEFINE MAIN MENU
         POPUP "Test tbrowse" FONT hFont3
            ITEM "Put color in virtual columns tbrowse-1" ACTION _wPost(1) FONT hFont1
            ITEM "Put color in virtual columns tbrowse-2" ACTION _wPost(2) FONT hFont1
            SEPARATOR
            ITEM "F3: ListColumn tbrowse-1"  ACTION  myListColumn(oBrw1) FONT hFont1
            ITEM "F3: ListColumn tbrowse-2"  ACTION  myListColumn(oBrw2) FONT hFont1
            SEPARATOR
            ITEM "What filter is tbrowse-1"  ACTION  myFilterTsb(oBrw1) FONT hFont1
            ITEM "What filter is tbrowse-2"  ACTION  myFilterTsb(oBrw2) FONT hFont1
            SEPARATOR
            ITEM "Exit"                      ACTION  _wPost(99)         FONT hFont3
         END POPUP
         POPUP "About"        FONT hFont3
            ITEM "Program Info"                 ACTION MsgAbout()          FONT hFont2
            ITEM "Virtual table columns"        ACTION MsgVirtColunm()     FONT hFont2
            ITEM "Table virtual column header"  ACTION MsgVirtHeadColunm() FONT hFont2
            ITEM "Table column header"          ACTION MsgInfoHeader()     FONT hFont2
         END POPUP
         POPUP "Right/left mouse click on the table header"  FONT hFont2
            ITEM "Mouse click on the table header"                  ACTION MsgInfoHeader()     FONT hFont2
            ITEM "Mouse click on the header of the virtual columns" ACTION MsgVirtHeadColunm() FONT hFont2
         END POPUP
      END MENU

      nWPrt := ( nW - 10 ) / 6
      DEFINE STATUSBAR
         STATUSITEM ""                  WIDTH 10
         STATUSITEM "Recno: 0/0"        WIDTH nWPrt FONTCOLOR PURPLE ACTION  Nil
         STATUSITEM "Column: 0/0"       WIDTH nWPrt FONTCOLOR PURPLE ACTION  Nil
         STATUSITEM "Mode: Dbf"         WIDTH nWPrt FONTCOLOR RED
         STATUSITEM "ALIAS1 - " + cAls1 WIDTH nWPrt FONTCOLOR GRAY
         STATUSITEM "ALIAS2 - " + cAls2 WIDTH nWPrt FONTCOLOR GRAY
         STATUSITEM "DELETE OFF"        WIDTH nWPrt FONTCOLOR LGREEN
      END STATUSBAR

      //////////// первая таблица ///////////////////
      nC := This.ClientHeight - This.StatusBar.Height - nY
      nH := nC * 0.5

      oBrw1 := myBrw( nY, nX, nW, nH, 1, cAlsTmp1, cAls1, aSupHd1 )

      /////////////// вторая таблица ///////////////////
      nY += nH
      nH := nC - nH

      oBrw2 := myBrw( nY, nX, nW, nH, 2, cAlsTmp2, cAls2, aSupHd2 )

      ON KEY ESCAPE ACTION {|| iif( oBrw2:IsEdit, oBrw2:SetFocus(), ;
                               iif( oBrw1:IsEdit, oBrw1:SetFocus() , _wPost(99) ) ) }

      WITH OBJECT This.Object
        :Cargo:oBrw1 := oBrw1               // на окне запомнили, объект tsb уже готовый
        :Cargo:oBrw2 := oBrw2               // на окне запомнили, объект tsb уже готовый
        :Event( 99, {|ow| ow:Release() } )  // выход по ESC
        :Event(500, {|  | NIL })            // реал. блок ставится в myVirtHeadClick(...)
      END WITH

      This.Minimize ;  This.Restore ; DO EVENTS

      oBrw1:SetFocus()  // фокус на таблицу 1

   END WINDOW

   ACTIVATE WINDOW Form_Main

RETURN

////////////////////////////////////////////////////////////////////////
// создать в контейнере приложения свои переменные с именами
STATIC FUNCTION myAppCargoInit( cPath, cPthTmp, cFile, lTmpErase )
   LOCAL o, aTmp, nTmp, cTmp, nSize, cFile1, cFile2
   Default cPath := ".\", cPthTmp := ".\"
   Default cFile := "demo.dbf"
   Default lTmpErase := .T.

   WITH OBJECT o := oKeyData()               // создать контейнер
      :aFile1    := { cPath+cFile, "ONE", "RU866", RddSetDefault(), cPthTmp+"tmpONE.DBF", "MEMOONE"  }
      :aFile2    := { cPath+cFile, "TWO", "RU866", RddSetDefault(), cPthTmp+"tmpTWO.DBF", "MEMOTWO"  }
      :lTmpErase := lTmpErase                // удалять tmp файлы, если они не в mem:
      :nTmp2memSize :=  50                   // если LastRec > 50 Мб, то работа на TmpFile
      :nWaitWndMax  := 1000                  // если в БД больше 1000 записей
      :nWaitWndCnt  := 250                   // счетчик записей для показа индикации режим Color
      :nWaitWndCrt  :=  50                   // счетчик записей для показа индикации режим Create
      :nWaitWndSave :=  50                   // счетчик записей для показа индикации режим Save
      // путь создания tmpXXX.DBF
      nSize := FILESIZE( :aFile1[ NAME_FILE ] ) / 1024 / 1024
      IF nSize == 0
         // пропуск, нет такого файла
      ELSEIF nSize < :nTmp2memSize  // Мб
         cPthTmp    := "mem:"       // создать/открыть в памяти через HB_MEMIO
         //cPthTmp    := ".\"       // тестировка
         :aFile1[ NAME_TEMP ] := cPthTmp + "tmpONE.DBF"
         :aFile2[ NAME_TEMP ] := cPthTmp + "tmpTWO.DBF"
      ELSE
         cPthTmp    := GetUserTempFolder() + "\"
         cFile1     := cPthTmp + "tmpONE.DBF"
         cFile2     := cPthTmp + "tmpTWO.DBF"
         // для запуска нескольких копий программы
         cFile1     := GetFileNameMaskNum(cFile1)    // получить новое имя файла
         cFile2     := GetFileNameMaskNum(cFile2)    // получить новое имя файла
         :aFile1[ NAME_TEMP ] := cFile1
         :aFile2[ NAME_TEMP ] := cFile2
      ENDIF

      :aTsbFonts := { "TsbNorm", "TsbBold", "TsbBold", "TsbSpecH", "TsbSuperH", "TsbEdit" }
      :aTmpStru  := {                        ;
                     {"VIRT_1", "N",  7, 0}, ;  // виртуальные колонки
                     {"VIRT_2", "N",  7, 0}, ;
                     {"VIRT_3", "N",  7, 0}, ;
                     {"VIRT_4", "N",  7, 0}, ;
                     {"VIRT_5", "N",  7, 0}, ;
                     {"VIRT_6", "N",  7, 0}, ;
                     {"RECID" , "N",  7, 0}  ;  // RecNo() присоединяемой базы "9,999,999"
                    }
      :aTmpView  := {}                            // поля показа Field
      :aTmpHead  := {}                            // поля показа Head
      :aColVirt  := {}                            // список вирт. колонок массивом
      :cColVirt  := ","                           // список вирт. колонок строкой ",VIRT_1,...,VIRT_6,"
      FOR nTmp := 1 TO Len( :aTmpStru )
          aTmp := :aTmpStru[ nTmp ]
          IF     "VIRT" $ aTmp[1]
             cTmp := "("+hb_ntos(nTmp)+")"
             AADD( :aColVirt, aTmp[1] )
             :cColVirt += aTmp[1]+","
          ELSEIF "REC"  $ aTmp[1]
             cTmp := "ID"
          ENDIF
          AADD( :aTmpView, aTmp[1] )
          AADD( :aTmpHead, cTmp )
      NEXT
      // суперхидер таблицы
      :aSupHd1 := { :aFile1[ NAME_FILE  ] + ' [ Alias: ' + ;
                    :aFile1[ NAME_ALIAS ] + "/"  + ;
                    :aFile1[ NAME_CDP   ] + '/'  + ;
                    :aFile1[ NAME_VIA   ] + ' ]' + ' + TempDbf - ' + ;
                    :aFile1[ NAME_TEMP  ] }

      :aSupHd2 := { :aFile2[ NAME_FILE  ] + ' [ Alias: ' + ;
                    :aFile2[ NAME_ALIAS ] + "/"  + ;
                    :aFile2[ NAME_CDP   ] + '/'  + ;
                    :aFile2[ NAME_VIA   ] + ' ]' + ' + TempDbf - ' + ;
                    :aFile2[ NAME_TEMP  ] }

   END WITH

RETURN o

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrw( nY, nX, nW, nH, nBrw, cAlsTmp, cAlias, aSupHd )
   LOCAL cBrw   := "oBrw"+hb_ntos(nBrw)
   LOCAL AOC    := (App.Object):Cargo // использовать из контейнера приложения свои переменные
   LOCAL aFont  := AOC:aTsbFonts
   LOCAL aHead  := AOC:aTmpHead
   LOCAL aField := AOC:aTmpView
   LOCAL nNum   := Len( aField )
   LOCAL aFld   := {}
   LOCAL aNam   := {}
   LOCAL aHdr   := {}
   LOCAL lEdit  := .T.     // разрешить редактировать ячейки
   LOCAL oBrw, nFld, cFld

   FOR nFld := 1 TO (cAlias)->( FCount() )
      cFld := (cAlias)->( FieldName( nFld ) )
      AAdd( aFld, cFld )
      AAdd( aNam, cFld )
      AAdd( aHdr, cFld )
   NEXT

   DEFINE TBROWSE &cBrw OBJ oBrw ALIAS cAlsTmp CELL ;
          AT nY, nX WIDTH nW HEIGHT nH              ;
          FONT       aFont                          ;
          BRUSH      YELLOW                         ;
          HEADERS    aHead                          ;
          COLSIZES   NIL                            ;
          PICTURE    NIL                            ;
          JUSTIFY    NIL                            ;
          COLUMNS    aField                         ;
          COLNAMES   aField                         ;
          FOOTERS    .T.                            ;
          FIXED      COLSEMPTY                      ;
          LOADFIELDS GOTFOCUSSELECT                 ;
          COLNUMBER  { nNum, 40 }                   ;
          ENUMERATOR LOCK EDIT                      ;
          ON INIT    {|ob| ob:Cargo := oKeyData() }

          :DelColumn( ATail( aField ) )                   // удалить колонку "RECID" из таблицы

          myVirtSetTsb( oBrw )                            // настройки виртуальных столбцов

          :LoadFields( lEdit, aFld, cAlias, aNam, aHdr )  // загрузить все поля осн. базы в таблицу

          myBrwInit( oBrw, nBrw )      // init TBrowse and Cargo
          myColumnInit( oBrw )         // инициализация колонок таблицы для фильтра/итого по вирт.колонкам
          myColorsInit( oBrw )         // инициализация цветов в Cargo
          mySetTsb( oBrw )             // настройки таблицы
          //myPartWidthTsb( oBrw )      // поправить ширину колонок
          myColorTsb( oBrw )           // цвета на таблицу
          myColorTsbElect( oBrw )      // цвета избранные/показ из tempDBF
          mySupHdTsb( oBrw, aSupHd )   // SuperHeader
          myEnumTsb( oBrw )            // ENUMERATOR по порядку
          mySet2Tsb( oBrw )            // настройки таблицы дополнительные
          mySetEditTsb( oBrw )         // настройки редактирования
          mySetHeadClick( oBrw )       // настройка для шапки таблицы

          :nFreeze     := :nColumn("ORDKEYNO")
          :nCell       := :nFreeze + 1
          :lLockFreeze := .T.

   END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:Refresh() }   // это нуно после уст.цветов

RETURN oBrw

///////////////////////////////////////////////////////////
// Инициализация TsBrowse и Cargo таблицы
STATIC FUNCTION myBrwInit( oBrw, nBrw )

   WITH OBJECT oBrw

      :Cargo:nBrowse  := nBrw                  // номер таблицы - запомнили
      :Cargo:cFilter  := '""'                  // выражение тек. фильтра вирт. колонок
      :Cargo:nFilter  := 0                     // нет фильтра вирт. колонок
      :Cargo:aFilter  := {"DELETED()", "CITY", "STREET", "YEAR2", "DOLG2014", "DOLG2015"}

      // снятие фильтра, если нет записей по фильтру - разблокировка шапки колонок
      :bEvents := {|obr,nmsg|
                    If nmsg == WM_LBUTTONUP .and. obr:nLen == 0
                       obr:FilterData()
                       myClrVirtHead( obr, obr:Cargo:nClr4 ) // очистка цвета фона шапки таблицы виртуальных колонок
                       mySumVirtFoot( obr, .F. )             // вывод подвала виртуальных колонок
                       obr:DrawHeaders(.T.)
                       obr:SetFocus()
                       DO EVENTS
                    EndIf
                    Return Nil
                   }
      :bGotFocus := {|ob| myGotFocusTsb(ob)     }
      :bOnDraw   := {|ob| SayStatusBar(ob)      }   // показ StatusBar - Recno/Column

      :UserKeys(VK_F3, {|ob| myListColumn(ob)   })  // инфо по списку колонок
      /*
      // блок отладки вывода - для тестирования
      :bTSDrawCell := {|ob,ocel,ocol|
                        If ocel:nDrawType == 0 .and. ob:Cargo:nFilter > 0      // Line
                           If ocol:cName == "VIRT1"
                              ocel:nClrBack := ocol:Cargo:oBack:Get(ob:nAtPos, oCol:Cargo:nBackDef)
                              ocel:nClrFore := ocol:Cargo:oFore:Get(ob:nAtPos, oCol:Cargo:nForeDef)
                              //? ob:nAt, ob:nAtPos, ocol:cName, ocel:nClrBack
                           EndIf
                        EndIf
                        Return Nil
                       }
      */
   END WITH

RETURN Nil

/////////////////////////////////////////////////////////////////////////
// инициализация колонок таблицы для фильтра/итого по вирт.колонкам
STATIC FUNCTION myColumnInit( oBrw )
   LOCAL oCol

   WITH OBJECT oBrw
      FOR EACH oCol IN :aColumns            // Init Cargo в колонке
         oCol:Cargo := oKeyData()
         oCol:Cargo:nSum  := 0
         oCol:Cargo:aVirt := oKeyData()
         //oCol:Cargo:aVirt := Array((:cAlias)->( LastRec() ))
         //AFill(oCol:Cargo:aVirt, 0)
         // убрать пустую дату и 0 в колонках
         oCol:lEmptyValToChar := .T.
      NEXT
   END WITH

RETURN Nil

///////////////////////////////////////////////////////////
// Инициализация цветов таблицы в Cargo
STATIC FUNCTION myColorsInit( oBrw )

   WITH OBJECT oBrw:Cargo
      // мои переменные цвета (фон/текст) для вывода каждой ячейки при инициализации
      :nBackDef   := CLR_WHITE
      :nForeDef   := CLR_BLACK
      :nBackKeyNo := CLR_RED
      :nForeKeyNo := CLR_WHITE
      // 0. строки создание переменных
      :nBtnText   :=  GetSysColor( COLOR_BTNTEXT )     // nClrSpecHeadFore
      :nBtnFace   :=  GetSysColor( COLOR_BTNFACE )     // nClrSpecHeadBack
      :nBClrSpH   :=  GetSysColor( COLOR_BTNFACE )     // nClrSpecHeadBack
      // 1. переменные цветов из #define CLR_... и RGB(...), меняя правую часть меняем цвета тсб
      :nHRED      :=  CLR_HRED
      :n_HRED     := -CLR_HRED
      :nRED       :=  CLR_RED
      :nBLUE      :=  CLR_BLUE
      :n_BLUE     := -CLR_BLUE
      :n_HBLUE    := -RGB(128,225,225)
      :nHBLUE     :=  RGB(128,225,225)
      :nHBLUE2    :=  RGB(  0,176,240)   //CLR_HBLUE
      :nHGRAY     :=  CLR_HGRAY
      :nGRAY      :=  CLR_GRAY
      :nBLACK     :=  CLR_BLACK
      :nYELLOW    :=  CLR_YELLOW
      :nGREEN     :=  CLR_GREEN
      :nGREEN2    :=  RGB(  0,255,  0)
      :nGREEN3    :=  RGB( 94,162, 38)
      :nORANGE    :=  CLR_ORANGE
      :nWHITE     :=  CLR_WHITE
      :nPURPLE2   := RGB(206,59,255)
      :nBCDelRec  := RGB( 65, 65, 65 )
      :nFCDelRec  := RGB( 251, 250, 174 )     // желтый осветл.
      :nBCYear    := RGB( 178, 227, 137 )     // зеленый осветл. 60%
      :nBCYear2   := :nGREEN3                 // зеленый осветл. 25%
      :nFCYear    := RGB( 63, 108, 25 )       // текст зеленый осветл. 50%

      // 2. переменные RGB( ... ) для использования
      :nRgb0      :=  RGB(  0,  0,  0)
      :nRgb1      :=  RGB(180,180,180)
      :nRgb2      :=  RGB(255,255,240)
      :nRgb3      := -RGB(128,225,225)

      // 3. переменные (aColors items number) от номера позиции в :SetColor( {...}, ... ) из TsBrowse.ch
      :nClrLine   :=  :nRgb1
      :nClr1      :=  :nRgb0                  // #define CLR_         1   // text
      :nClr2      :=  :nRgb2                  // #define CLR_PANE     2   // back
      :nClr3      :=  :nWHITE                 // #define CLR_HEADF    3   // header text
      :nClr4      := {:nHGRAY, :nGRAY}        // #define CLR_HEADB    4   // header back
      :nClr5      :=  :nRgb0                  // #define CLR_FOCUSF   5   // focused text
      :nClr6_1    :=  :n_BLUE                 // #define CLR_FOCUSB   6 1 // focused back
      :nClr6_2    :=  :nRgb3                  // #define CLR_FOCUSB   6 2 // focused back
      :nClr9      :=  :nWHITE                 // #define CLR_FOOTF    9   // footer text
      :nClr10     := {:nGRAY, :nHGRAY}        // #define CLR_FOOTB   10   // footer back
      :nClr11     :=  :nRgb0                  // #define CLR_SELEF   11   // focused inactive (or selected) text
      :nClr12_1   :=  :n_BLUE                 // #define CLR_SELEB   12 1 // focused inactive (or selected) back
      :nClr12_2   :=  :nRgb3                  // #define CLR_SELEB   12 2 // focused inactive (or selected) back
      :nClr16     := {RGB(0,176,240),RGB(60,60,60)}    // 16, фона спецхидер
      :nClr17     :=  :nYELLOW                         // 17, текста спецхидер
      :aClrVirt   := { :nBCDelRec, 0, :nHBLUE2, :nFCYear, :nHRED, :nPURPLE2 }
      :aClrBrw    := { :nGREEN2 , :nYELLOW }
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGotFocusTsb( oBrw )
   LOCAL oCargo := GetProperty(oBrw:cParentWnd, "Cargo")

   oCargo:oBrwFocus   := oBrw                 // на окне запомнили, какой tsb в фокусе
   oBrw:Cargo:nClr6_1 := oBrw:Cargo:n_HRED

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION SayStatusBar( oBrw )      // показ StatusBar - Recno/Column
   LOCAL nCell  := oBrw:nCell
   LOCAL nRecno := (oBrw:cAlias)->( Recno() )
   LOCAL cForm  := oBrw:cParentWnd
   LOCAL cSt1, cSt2, cRec, lDel, cDel, cVal

   cVal := "Column: "+hb_ntos(nCell - VIRT_COLUMN_MAX)+" / "
   cVal += hb_ntos(oBrw:nColCount() - VIRT_COLUMN_MAX)
   SetProperty( cForm, "StatusBar" , "Item" , 3, cVal )

   cSt1 := hb_NtoS(nRecno)
   cSt2 := hb_NtoS((oBrw:cAlias)->( LastRec() ))
   lDel := (oBrw:cAlias)->( Deleted() )
   cDel := iif( (oBrw:cAlias)->( Deleted() ), "Deleted", "" )
   cRec := iif( lDel, "*", " " )+"Recno: "
   cVal := cRec+cSt1+" / "+cSt2+" "+cDel
   SetProperty( cForm, "StatusBar" , "Item" , 2, cVal )

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cMsg := ''

   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cSize := HB_NtoS( INT( oBrw:GetColSizes()[nCol] ) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += " :nWidth=" + HB_NtoS(oCol:nWidth)
      cMsg  += '  ( "' + oCol:cFieldTyp + '" ' + HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT

   AlertInfo(cMsg + REPL(";",30))

RETURN Nil

//////////////////////////////////////////
// настройки виртуальных столбцов
STATIC FUNCTION myVirtSetTsb( oBrw )
   LOCAL oCol, AOC := (App.Object):Cargo
   LOCAL nKolvo := (oBrw:cAlias)->( Lastrec() )
   LOCAL nLen   := Len( hb_ntos(nKolvo) ), nWidth

   nLen   := iif( nLen > 2, nLen, nLen + 1 )           // если < 3х, то +1 к nLen
   nWidth := GetFontWidth( AOC:aTsbFonts[ 1 ], nLen )  // Font имя для cell

   WITH OBJECT oBrw

      FOR EACH oCol IN :aColumns
          oCol:nWidth  := nWidth     // любая колонка может иметь итог как в колонке #
          IF "KEYNO" $ oCol:cName
             oCol:nFieldLen := nLen
             LOOP
          ENDIF
          oCol:bDecode := {|xx| iif( Empty(xx), "", hb_ntos(xx) ) }
          oCol:nAlign  := DT_CENTER
          oCol:nHAlign := DT_CENTER
          oCol:nFAlign := DT_CENTER
      NEXT

   END WITH

RETURN Nil

//////////////////////////////////////////
STATIC FUNCTION mySetTsb( oBrw )
   WITH OBJECT oBrw
      :nColOrder     := 0           // убрать значок сортировки по полю
      :lNoChangeOrd  := .T.         // убрать сортировку по полю
      :nWheelLines   := 1           // прокрутка колесом мыши
      :lNoGrayBar    := .F.         // показывать неактивный курсор в таблице
      :lNoLiteBar    := .F.         // при переключении фокуса на другое окно не убирать "легкий" Bar
                                    // строка фокусная, при установленных цветах, прорисовывается,
                                    // при .T. прорисовки фокусной строки нет, т.е. все строки
                                    // одинаковы на фоне тсб (по установленным цветам), т.е.
                                    // нет работы :DrawSelect()
      :lNoResetPos   := .F.         // предотвращает сброс позиции записи на gotfocus
      :lNoPopUp      := .T.         // избегает всплывающее меню при щелчке правой кнопкой мыши по заголовку столбца
      :lNoHScroll    := .T.         // отключаем показ HScroll для коротких по ширине тсб (все колонки входят в показ)
      :nHeightCell   += 2           // высота ячеек таблицы добавит 2 пиксела
      //:nCellMarginLR := 1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nStatusItem   :=  0
      :lNoKeyChar    := .T.         // method :KeyChar disabled
      :lCheckBoxAllReturn := .T.    // Enter modify value oCol:lCheckBox
      :lPickerMode        := .F.    // формат даты нормальный
   END WITH

RETURN Nil

//////////////////////////////////////////
STATIC FUNCTION mySet2Tsb( oBrw )
   LOCAL nLen, cBrw, nTsb

   WITH OBJECT oBrw
      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth   // ширины внутри тсб
      nLen := :GetAllColsWidth() - 1     // ширина всех колонок видимых
      IF nLen > nTsb                     // колоноки не входят в показ -> HScroll
         :lAdjColumn  := .T.             // выравнивать последнюю колонку при прорисовке
         :lNoHScroll  := .F.             // добавить\вкл. ползунок горизонтальный
         :lMoreFields := ( :nColCount() > 30 ) // если колонок больше, то вкл.
                                               // метод работы, что бы не
                                               // зависала прорисовка тсб
      ELSE
         :AdjColumns()  // колонки входят в окно тсб, уберем вертикальную "дырку"
                        // распределив ее значение по колонкам, растянув
      ENDIF
   END WITH

RETURN Nil
#if 0
////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myPartWidthTsb( oBrw )         // поправить ширину колонок
   LOCAL nW, oCol, cType, hFont := oBrw:hFont  // 1-cells font
   LOCAL cCol, cNam, aColVirt, lColVirt
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   // из-за виртуальных колонок ширина остальных колонок нарушается
   // + к этому влияет фонт "DejaVu Sans Mono", т.к. он моноширинный
   // нужно самостоятельно расставить ширину колонок

   aColVirt := AClone( AOC:aColVirt )  // { "VIRT_1", "VIRT_2", ... } // список вирт. колонок массивом
   AADD( aColVirt , "ORDKEYNO" )       // без aClone() в AOC:aColVirt будут лишние "ORDKEYNO"

   WITH OBJECT oBrw
      FOR EACH oCol IN :aColumns
         cCol     := oCol:cName
         cType    := oCol:cFieldTyp
         lColVirt := .F.
         FOR EACH cNam IN aColVirt
            IF cCol == cNam
               lColVirt := .T.
               EXIT
            ENDIF
         NEXT
         IF !lColVirt

            IF cType $ "=@T"
               oCol:nWidth := GetTextWidth( Nil, REPL("9",24), hFont ) // 24 знака
            ELSEIF cType $ "+^" // Type: [+] [^]
               oCol:nWidth := GetTextWidth( Nil, REPL("9",6), hFont )  // 6 знака
            ELSEIF cType == "D"
               oCol:nWidth := GetTextWidth( Nil, REPL("9",11), hFont )
            ELSEIF cType == "C"
               // увеличим ширину колонок
               oCol:nWidth := GetTextWidth( Nil, REPL("H", oCol:nFieldLen), hFont )
               // увеличим ширину колонки для компенсации :nCellMarginLR := 1
               // это нужно не всегда ! зависит от разрешений экрана и фонта
               nW := GetTextWidth( Nil, REPL("H",1), hFont )
               oCol:nWidth +=  nW + nW/2

            ELSEIF cType == "N"
               oCol:nWidth := GetTextWidth( Nil, REPL("0", oCol:nFieldLen), hFont ) * 0.8
               IF oCol:nFieldLen < VIRT_COLUMN_MAX
                  oCol:nWidth := GetTextWidth( Nil, REPL("0", oCol:nFieldLen), hFont )
               ENDIF
            ENDIF

            // увеличим ширину колонки для длинных названий полей
            nW := GetTextWidth( Nil, "H" + oCol:cName, hFont )
            IF nW > oCol:nWidth
               oCol:nWidth := nW
            ENDIF

         ENDIF  // !lColVirt

      NEXT

   END WITH

RETURN Nil
#endif
////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsb( oBrw )
   LOCAL O := oBrw:Cargo

   WITH OBJECT oBrw
      :nClrLine := O:nClrLine   // создать в контейнере tsb свои переменные с именами
      :SetColor( { 1}, { O:nClr1  } )  // 1 , текста в ячейках таблицы
      :SetColor( { 2}, { O:nClr2  } )  // 2 , фона в ячейках таблицы
      :Setcolor( { 3}, { O:nClr3  } )  // 3 , текста шапки таблицы
      :SetColor( { 4}, { O:nClr4  } )  // 4 , фона шапка таблицы   // !!! тут лишний блок кода, массива достаточно
      :SetColor( { 5}, { O:nClr5  } )  // 5 , текста курсора, текст в ячейках с фокусом
      :SetColor( { 6}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr6_1 , c:nClr6_2  ) } } )  // 6 , фона курсора
      :SetColor( { 9}, { O:nClr9  } )  // 9 , текста подвала таблицы
      :SetColor( {10}, { O:nClr10 } )  // 10, фона подвала таблицы // !!! тут лишний блок кода, массива достаточно
      :SetColor( {11}, { O:nClr11 } )  // 11, текста неактивного курсора (selected cell no focused)
      :SetColor( {12}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr12_1, c:nClr12_2 ) } } )  // 12, фона неактивного курсора (selected cell no focused)
      :hBrush   := CreateSolidBrush( 255, 255, 230 )   // цвет фона под таблицей
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorsInitTempDbf( nBrw )  // запись цвета в tempDBF
   LOCAL nRecno, i, cField, cAlsIsx, nRec, lDel, nVal, nFldYear
   LOCAL AOC   := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL aFile := { AOC:aFile1, AOC:aFile2 }
   LOCAL cBrw  := "oBrw"+hb_ntos(nBrw)
   LOCAL oBrw  := This.&(cBrw).Object
   LOCAL cAls  := oBrw:cAlias
   LOCAL O := oBrw:Cargo          // использовать из контейнера таблицы свои переменные
   LOCAL aVFore, aVBack, aFCell, aBCell, lWaitWnd
   LOCAL nWaitWndMax := AOC:nWaitWndMax
   LOCAL nWaitWndCnt := AOC:nWaitWndCnt
   LOCAL nWaitWnd := 0
   LOCAL cMsg := "Wait, color is being written to dbf file. "+cBrw+" - "

   nRecno   := (cAls)->( RecNo() )
   lWaitWnd := (cAls)->( LastRec() ) > nWaitWndMax        // если в БД больше 1000 записей
   cAlsIsx  := aFile[ nBrw ][ NAME_ALIAS ]
   // проверка на поле
   SELECT(cAlsIsx)
   nFldYear := FIELDNUM("YEAR2")
   SELECT(cAls)

   IF lWaitWnd
      WaitWindow( cMsg + repl(".", 7), .T. )
   ENDIF

   aVFore := Array( VIRT_COLUMN_END )
   aVBack := Array( VIRT_COLUMN_END )
   FOR i := 1 TO VIRT_COLUMN_END
       cField := "VFORE_" + hb_ntos( i )
       aVFore[ i ] := (cAls)->( FieldPos( cField ) )
       cField := "VBACK_" + hb_ntos( i )
       aVBack[ i ] := (cAls)->( FieldPos( cField ) )
   NEXT

   aFCell := Array( LEN(AOC:aFClrCell) )
   aBCell := Array( LEN(AOC:aFClrCell) )
   FOR i := 1 TO LEN(AOC:aFClrCell)
       cField := AOC:aFClrCell[ i ]
       aFCell[ i ] := (cAls)->( FieldPos( cField ) )
       cField := AOC:aBClrCell[ i ]
       aBCell[ i ] := (cAls)->( FieldPos( cField ) )
   NEXT

   (cAls)->( dbGotop() )

   DO WHILE (cAls)->( !EOF() )
      nRec  := (cAls)->( RecNo() )
      DO EVENTS
      FOR i := 1 TO VIRT_COLUMN_END  // LEN(AOC:aBClrCellVirt)
         (cAls)->( FieldPut(aVFore[ i ], O:nBLACK   ) )  // цвет текста ячеек
         (cAls)->( FieldPut(aVBack[ i ], O:nBClrSpH ) )  // цвет фона ячеек
      NEXT

      IF ( lDel := (cAls)->( DELETED() ) )                    // для удалённых записей
         FOR i := 1 TO LEN(AOC:aFClrCell)
             (cAls)->( FieldPut(aFCell[ i ], O:nHGRAY    ) )  // цвет текста ячеек
             (cAls)->( FieldPut(aBCell[ i ], O:nBCDelRec ) )  // цвет фона ячеек
         NEXT
      ENDIF

      IF nFldYear > 0                            // если такое поле есть в базе
         nVal := (cAlsIsx)->YEAR2                // в качестве примера
         IF nVal > 2020
            FOR i := 1 TO LEN(AOC:aFClrCell)
               cField := AOC:aFClrCell[ i ]      // цвет текста ячеек
               //(cAls)->&cField := O:nHBLUE     // новый цвет
               cField := AOC:aBClrCell[ i ]      // цвет фона ячеек
               (cAls)->&cField := O:nBCYear      // новый фон
            NEXT
         ENDIF
      ENDIF

      IF lWaitWnd
         nWaitWnd++
         IF nWaitWnd >= nWaitWndCnt
            nWaitWnd := 0
            WaitWindow( cMsg+hb_ntos(nRec), .T. )
         ENDIF
      ENDIF

      (cAls)->( dbSkip() )
   ENDDO

   IF lWaitWnd
       nRec := (cAls)->( LastRec() )
       WaitWindow( cMsg+hb_ntos(nRec), .T. )
       InkeyGui(1000)
       WaitWindow()
   ENDIF

   (cAls)->( dbGoto( nRecno ) )
   oBrw:Refresh()

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsbElect( oBrw )
   LOCAL aColVirt, lVirtual, nCol, cFld, oCol, cCol, cNam
   LOCAL nBrowse, nAt := oBrw:nAt, aCol := oBrw:aColumns
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   nBrowse  := O:nBrowse     // номер таблицы
   aColVirt := AOC:aColVirt  //{ "VIRT_1", "VIRT_2", ... } // список вирт. колонок массивом

   FOR nCol := 1 TO Len(aCol)
      oCol := aCol[ nCol ]
      cCol := oCol:cName
      lVirtual := .F.
      FOR EACH cNam IN aColVirt
         IF cCol == cNam
            lVirtual := .T.
            EXIT
         ENDIF
      NEXT
      IF lVirtual
         // --------- вывод цвета для каждой ячейки таблицы -----------
         oCol:nClrBack := {|at,nc,br| myClrBackVirt(at,nc,br) } // цвет фона в ячейках таблицы
         oCol:nClrFore := {|at,nc,br| myClrForeVirt(at,nc,br) } // цвет текста в ячейках таблицы
      ELSE
         IF cCol == "ORDKEYNO"
            oBrw:GetColumn(cCol):nClrFore := O:nBLACK
            oBrw:GetColumn(cCol):nClrBack := O:nBClrSpH  // как у нумератора таблицы
         ELSE
            // --------- вывод цвета для каждой ячейки таблицы -----------
            oCol:nClrBack := {|at,nc,br| myClrBack(at,nc,br) } // цвет фона в ячейках таблицы
            oCol:nClrFore := {|at,nc,br| myClrFore(at,nc,br) } // цвет текста в ячейках таблицы
            // --- цвет текста для активной строки таблицы, цвет текста :SetColor({5}...) - одно условие
            oCol:nClrFocuFore := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->( DELETED() ), O:nWHITE, O:nClr1 ) }

         ENDIF  // cCol == "ORDKEYNO"
      ENDIF   // lVirtual
   NEXT

   // цвет фона шапки таблицы для добавочного списка колонок
   FOR EACH cFld IN { "ID", "TS", "VM", "IM", "DT", "TT" }
      IF cFld $ "IM,DT,TT"
         oBrw:GetColumn(cFld):nClrHeadBack := {|| oBrw:Cargo:nORANGE }  // цвет фона шапка таблицы
         oBrw:GetColumn(cFld):nClrFootBack := {|| oBrw:Cargo:nORANGE }  // цвет фона подвала таблицы
      ELSE
         oBrw:GetColumn(cFld):nClrHeadBack := {|| oBrw:Cargo:nRED }  // цвет фона шапка таблицы
         oBrw:GetColumn(cFld):nClrFootBack := {|| oBrw:Cargo:nRED }  // цвет фона подвала таблицы
      ENDIF
   NEXT

   /*FOR EACH cFld IN { "ID", "TS", "VM", "IM", "DT", "TT" }
       oCol              := oBrw:GetColumn(cFld)
       // oCol:nClrBack  := { |a,n,b| myTsbColorBack(a,n,b)   }  // цвет фона в ячейках таблицы
       oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }  // цвет фона подвала таблицы
       oCol:nClrFootBack := { |n,b  | myTsbColorBackHead(n,b) }  // цвет фона шапка таблицы
       // Это историческая неточность (параметры надо было {|b,n,a| ... } )
       // для блока кода подвала - передается два параметра
       // для строки(ячеек) - передается три параметра
   NEXT*/

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetTmpDbfColor( cAls, cFld, nAt, lDel )
   LOCAL nRec, nColor

   SELECT(cAls)
   nRec := RecNo()
   DbGoto(nAt)
   lDel   := (cAls)->( Deleted() )
   nColor := (cAls)->&cFld    // новый цвет
   DbGoto(nRec)

RETURN nColor

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myClrBackVirt( nAt, nCol, oBrw )
   LOCAL oCol := oBrw:aColumns[ nCol ]
   LOCAL nColor, cFld, cAls
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   cFld := AOC:aBClrCellVirt[ nCol ]  // цвет фона виртуальных ячеек из базы
   cAls := oBrw:cAlias
   nAt  := oBrw:nAtPos

   nColor := myGetTmpDbfColor( cAls, cFld, nAt )  // новый цвет фона ячейки
   IF nColor == 0
      nColor := O:nBClrSpH                        // как у нумератора таблицы
   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myClrForeVirt( nAt, nCol, oBrw )
   LOCAL oCol := oBrw:aColumns[ nCol ]
   LOCAL nColor, cFld, cAls
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   cFld := AOC:aFClrCellVirt[ nCol ]  // цвет текста виртуальных ячеек
   cAls := oBrw:cAlias
   nAt  := oBrw:nAtPos

   nColor := myGetTmpDbfColor( cAls, cFld, nAt )    // новый цвет текста ячейки
   IF nColor == 0
      nColor := O:nClr1                             // цвет текста таблицы
   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myClrBack( nAt, nCol, oBrw )
   LOCAL oCol := oBrw:aColumns[ nCol ]
   LOCAL nColor, cFld, cAls, nI
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   nI   := nCol - VIRT_COLUMN_MAX
   cFld := AOC:aBClrCell[ nI ]       // цвет фона виртуальных ячеек из базы
   cAls := oBrw:cAlias
   nAt  := oBrw:nAtPos

   nColor := myGetTmpDbfColor( cAls, cFld, nAt )  // новый цвет фона ячейки
   IF nColor == 0
      nColor := O:nClr2                           // цвет фона таблицы
   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myClrFore( nAt, nCol, oBrw )
   LOCAL oCol := oBrw:aColumns[ nCol ]
   LOCAL nColor, cFld, cAls, nI, lDel, cTyp
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   nI   := nCol - VIRT_COLUMN_MAX
   cFld := AOC:aFClrCell[ nI ]  // цвет текста виртуальных ячеек
   cAls := oBrw:cAlias
   nAt  := oBrw:nAtPos
   cTyp := oCol:cFieldTyp
   lDel := .F.

   nColor := myGetTmpDbfColor( cAls, cFld, nAt, @lDel )   // новый цвет текста ячейки

   IF nColor == 0
      nColor := O:nClr1           // цвет текста таблицы

      IF cTyp == "N" .AND. !lDel
         nColor := O:nGREEN       // новый цвет текста ячейки
      ENDIF

   ENDIF

RETURN nColor

//////////////////////////////////////////////////////////////////
// суперхидер
STATIC FUNCTION mySupHdTsb( oBrw, aSupHd )
   LOCAL O := oBrw:Cargo             // использовать из контейнера свои переменные

   WITH OBJECT oBrw
   :AddSuperHead( 1, :nColCount(), aSupHd[1] )

   // задать цвета суперхидеру
   :SetColor( {16}, { O:nClr16 } ) // 16, фона спецхидер
   :SetColor( {17}, { O:nClr17 } ) // 17, текста спецхидер

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// ENUMERATOR по порядку сделаем свой
STATIC FUNCTION myEnumTsb( oBrw )
   LOCAL nOneCol, oCol, nI := 0, nCnt := 0

   nOneCol := oBrw:nColumn("ORDKEYNO")

   FOR EACH oCol IN oBrw:aColumns
      nI++
      oCol:cSpcHeading := NIL
      oCol:cSpcHeading := IIF( nI == nOneCol, "#" , "+" )
      IF nI > nOneCol
         IF oCol:lVisible
            oCol:cSpcHeading := hb_ntos( ++nCnt )
         ENDIF
      ENDIF
   NEXT

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// настройки редактирования
STATIC FUNCTION mySetEditTsb( oBrw )
   LOCAL i, oCol, cTyp, cColVirt, nBrowse
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL O   := oBrw:Cargo          // использовать из контейнера таблицы свои переменные

   nBrowse  := O:nBrowse     // номер таблицы
   cColVirt := AOC:cColVirt  // "VIRT_1,VIRT_2,..."    // список вирт. колонок строкой

   WITH OBJECT oBrw

      // удаление/восстановление записи разрешена
      // кнопка для удаления, будет работать и на восстановление
      :SetDeleteMode( .T., .F., {|| AlertYesNo(iif((oBrw:cAlias)->(Deleted()), "Восстановить", "Удалить") + ;
                                                  " запись в таблице ?", "Подтверждение") } )

      :SetAppendMode( .F. )      // запрещена вставка записи в конце базы стрелкой вниз

      AEval( :aColumns, {|oc|                   // в списке удаленных edit запрещена
                          If oc:lEdit
                             oc:bPrevEdit := {|xv,ob| xv := ! (ob:cAlias)->(Deleted()) }
                          EndIf
                          Return Nil
                        } )

      FOR i := 1 TO Len(:aColumns)

         oCol := :aColumns[ i ]
         cTyp := oCol:cFieldTyp
         // edit колонок
         IF cTyp $ "+=^"   // Type: [+] [=] [^]
            oCol:bPrevEdit := {|| AlertStop("It is forbidden to edit this type of field !") , FALSE }
         ENDIF

         IF oCol:cName $ cColVirt
            // edit виртуальных ячеек таблицы - в качестве примера
            oCol:bLClicked := {|nrp,ncp,nat,obr| myVirtCellClick(1,obr,nrp,ncp,nat) }
            oCol:bRClicked := {|nrp,ncp,nat,obr| myVirtCellClick(2,obr,nrp,ncp,nat) }
         ENDIF

      NEXT

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// настройка для шапки таблицы
STATIC FUNCTION mySetHeadClick( oBrw )
   LOCAL i, oCol

   WITH OBJECT oBrw

      FOR i := 1 TO Len(:aColumns)
         oCol := :aColumns[ i ]
         oCol:bHLClicked := {|nrp,ncp,nat,obr| myAllHeadClick(1,obr,nrp,ncp,nat) }
         oCol:bHRClicked := {|nrp,ncp,nat,obr| myAllHeadClick(2,obr,nrp,ncp,nat) }
      NEXT

   END WIDTH

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myVirtCellClick( nClick, oBrw, nRowPix, nColPix )
   LOCAL nRow, nRow2, cNam, cForm, nCol, cCel, cMs, cColVirt
   LOCAL cMsg, cTyp, xVal, oCol, nY, nX, nWCel, nHCel
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   cColVirt := AOC:cColVirt         // список вирт. колонок строкой

   cNam  := {'Left mouse', 'Right mouse'}[ nClick ]
   cForm := oBrw:cParentWnd
   nRow  := oBrw:GetTxtRow(nRowPix)             // номер строки курсора в таблице
   nCol  := Max(oBrw:nAtCol(nColPix, .T.), 1)   // номер колонки курсора в таблице
   nRow2 := oBrw:nAt                            // номер строки в таблице
   cMs   := cNam + ", y/x: " + hb_ntos(nRowPix) + "/" + hb_ntos(nColPix) + ";;"
   xVal  := oBrw:GetValue(nCol)
   cTyp  := ValType(xVal)
   cCel  := "Cell position row/column: " + hb_ntos(nRow2) + '/' + hb_ntos(nCol) + ";"
   cCel  += "Get Cell value: [" + cValToChar(xVal) + "]    "
   cCel  += "Type Cell: " + cTyp + ";"
   oCol  := oBrw:aColumns[ nCol ]
   cCel  += "Column: " + hb_ntos(nCol) + " [" + oCol:cName + "];;"
   nWCel := oBrw:aColumns[ nCol ]:nWidth       // ширина текущей ячейки
   nHCel := oBrw:nHeightCell                   // высота текущей ячейки

   nY := (nRow2 - 1) * nHCel + oBrw:nTop + oBrw:nHeightHead + oBrw:nHeightSuper
   nY += IIF( oBrw:lDrawSpecHd, oBrw:nHeightSpecHd, 0 )
   nX := oCol:oCell:nCol
   IF _IsControlDefined("Lbl_0", cForm)
      DoMethod(cForm, "Lbl_0", "SetFocus")
   ELSE
      @ nY,nX GETBOX Lbl_0 OF &cForm WIDTH nWCel HEIGHT nHCel ;
        BACKCOLOR YELLOW VALUE "[ "+HB_NtoS(xVal)+" ]" READONLY NOTABSTOP
   ENDIF
   InkeyGui(1000)

   cMsg := 'Only for column: '+cColVirt+' !;;'
   cMsg += 'for more details see the "About" menu,; then the "Virtual table columns" menu'

   AlertInfo( cMs + cCel + cMsg + CRLF, ProcName()+"()" )

   IF _IsControlDefined("Lbl_0", cForm)
      DoMethod(cForm, "Lbl_0", "Release")
   ENDIF

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myAllHeadClick( nClick, oBrw, nRowPix, nColPix, nAt )
   LOCAL cForm, nRow, nCell, cNam, cName, nCol, nIsHS, nLine, oCol
   LOCAL nY, nX, cMsg1, cMsg2, cMsg3, aMsg, nCol0, nEvnt, cVirt
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   cVirt := AOC:cColVirt            // список вирт. колонок строкой
   cForm := oBrw:cParentWnd
   nRow  := oBrw:GetTxtRow(nRowPix)                 // номер строки курсора в таблице
   nCol  := Max(oBrw:nAtColActual( nColPix ), 1 )   // номер активной колонки курсора в таблице
   nCell := oBrw:nCell                              // номер ячейки в таблице
   nIsHS := iif(nRowPix > oBrw:nHeightSuper, 1, 2)
   cNam  := {'Left mouse', 'Right mouse'}[ nClick ]
   oCol  := oBrw:aColumns[ nCol ]
   cName := oCol:cName
   nLine := nAt

   nY    := GetProperty(cForm, "Row") + GetTitleHeight()
   nX    := GetProperty(cForm, "Col") + GetBorderWidth() - 4
   // возмём координаты от шапки таблицы
   nY    += GetMenuBarHeight() + oBrw:nTop + 2
   nY    += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper, 0 )
   nY    += IIF( oBrw:lDrawHeaders, oBrw:nHeightHead , 0 )
   nX    += oCol:oCell:nCol

   If nIsHS == 2        // нажат SuperHider
      oBrw:SetFocus()
      RETURN NIL
   Else
      // нажат Header
   Endif

   If nClick == 1
      // ваша обработка левой клавиши мышки
      IF cName $ cVirt
         nEvnt := Val(right(cName, 1))
         IF oBrw:Cargo:nFilter == nEvnt
            nEvnt := 99                     // снимаем фиьтр вирт. колонки
         ENDIF
      ENDIF
   Else
      // ваша обработка правой клавиши мышки
   Endif

   cMsg1 := cNam + ", y:x " + hb_ntos(nRowPix) + ":" + hb_ntos(nColPix)
   cMsg2 := "Head position y/x: " + hb_ntos(nY) + '/' + hb_ntos(INT(nX))
   cMsg3 := "Column header: " + hb_ntos(nCol)
   cMsg3 += "-" + hb_ntos( VIRT_COLUMN_MAX ) + "="
   nCol0 := nCol - VIRT_COLUMN_MAX
   cMsg3 += hb_ntos(nCol0) + "  [" + cName + "]"
   aMsg  := { cMsg1, cMsg2, cMsg3 }

   IF cName $ cVirt+'ORDKEYNO,'
      // сделаем отдельное сообщение
      cMsg3 := "Column header: " + hb_ntos(nCol) + " [" + cName + "]"
      aMsg  := { cMsg1, cMsg2, cMsg3 }
      // меню шапки виртуальных колонок
      myVirtHeadClick(oBrw, nY, nX, aMsg, nEvnt )
   ELSE
      // меню шапки обычных колонок
      myHeadClick(oBrw, nY, nX, aMsg )
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myVirtHeadClick( oBrw, nY, nX, aMsg, nEvnt )
   LOCAL cForm, hFont1, hFont2, hFont3

   cForm  := oBrw:cParentWnd
   hFont1 := GetFontHandle( "TsbEdit"   )
   hFont2 := GetFontHandle( "TsbSuperH" )
   hFont3 := GetFontHandle( "TsbBold"   )

   SET WINDOW THIS TO cForm
   // назначим новое событие
   (This.Object):Event( 500, {|ow,ky,np| ky := ow, myFilter(np, oBrw) })
   SET WINDOW THIS TO

   IF nEvnt == NIL

      DEFINE CONTEXT MENU OF &cForm
         MENUITEM  "Show virtual columns"         ACTION  {|| myShowHideColumn(1,oBrw) } FONT hFont2
         MENUITEM  "Hide virtual columns"         ACTION  {|| myShowHideColumn(2,oBrw) } FONT hFont2
         SEPARATOR
         Popup 'Filter by virtual column ???'  FONT hFont3
            /*
            MENUITEM  "Filter by virtual column (1)"  ACTION  {|| myFilter(1,oBrw) }  FONT hFont2
            MENUITEM  "Filter by virtual column (2)"  ACTION  {|| myFilter(2,oBrw) }  FONT hFont2
            MENUITEM  "Filter by virtual column (3)"  ACTION  {|| myFilter(3,oBrw) }  FONT hFont2
            MENUITEM  "Filter by virtual column (4)"  ACTION  {|| myFilter(4,oBrw) }  FONT hFont2
            MENUITEM  "Filter by virtual column (5)"  ACTION  {|| myFilter(5,oBrw) }  FONT hFont2
            MENUITEM  "Filter by virtual column (6)"  ACTION  {|| myFilter(6,oBrw) }  FONT hFont2
            */
            MENUITEM  "Filter by virtual column (1)"  ACTION  _wPost(500, cForm, 1)   FONT hFont2
            MENUITEM  "Filter by virtual column (2)"  ACTION  _wPost(500, cForm, 2)   FONT hFont2
            MENUITEM  "Filter by virtual column (3)"  ACTION  _wPost(500, cForm, 3)   FONT hFont2
            MENUITEM  "Filter by virtual column (4)"  ACTION  _wPost(500, cForm, 4)   FONT hFont2
            MENUITEM  "Filter by virtual column (5)"  ACTION  _wPost(500, cForm, 5)   FONT hFont2
            MENUITEM  "Filter by virtual column (6)"  ACTION  _wPost(500, cForm, 6)   FONT hFont2
         End Popup
         /*
         MENUITEM  "Filter by all virtual column"  ACTION  {|| myFilter(0,oBrw)  }  FONT hFont3
         MENUITEM  "Clear table filter"            ACTION  {|| myFilter(99,oBrw) }  FONT hFont3
         */
         MENUITEM  "Filter by all virtual column"  ACTION  _wPost(500, cForm,  0)  FONT hFont3
         MENUITEM  "Clear table filter"            ACTION  _wPost(500, cForm, 99)  FONT hFont3
         SEPARATOR
         MENUITEM  "Exit"                          ACTION  {|| oBrw:SetFocus() } FONT hFont3
         SEPARATOR
         MENUITEM  aMsg[1] DISABLED  FONT hFont1
         MENUITEM  aMsg[2] DISABLED  FONT hFont1
         MENUITEM  aMsg[3] DISABLED  FONT hFont1
      END MENU

      _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
      InkeyGui(100)

      DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
      END MENU

   ELSE

      DO EVENTS ; _wPost(500, cForm, nEvnt)

   ENDIF

   oBrw:SetFocus()
   oBrw:DrawSelect()
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION myHeadClick( oBrw, nY, nX, aMsg )
   LOCAL cForm, hFont1, hFont2, hFont3

   cForm  := oBrw:cParentWnd
   hFont1 := GetFontHandle( "TsbEdit"   )
   hFont2 := GetFontHandle( "TsbSuperH" )
   hFont3 := GetFontHandle( "TsbBold"   )

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  aMsg[3]  ACTION  {|| MsgDebug(aMsg[3]) } FONT hFont2
       MENUITEM  "Exit"   ACTION  {|| oBrw:SetFocus() } FONT hFont3
       SEPARATOR
       MENUITEM  aMsg[1] DISABLED  FONT hFont1
       MENUITEM  aMsg[2] DISABLED  FONT hFont1
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   oBrw:SetFocus()
   oBrw:DrawSelect()

RETURN Nil

//////////////////////////////////////////////////////////////////
// показать/скрыть колонки из отображения
STATIC FUNCTION myShowHideColumn( nShowHide, oBrw )
   LOCAL oCol, cCol, cListCol, nCol, aDimCol := {}
   LOCAL aCol := oBrw:aColumns
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   cListCol := AOC:cColVirt         // список вирт. колонок строкой - ",VIRT1,VIRT2,..."

   FOR nCol := 1 TO Len(aCol)
      oCol := aCol[ nCol ]
      cCol := oCol:cName
      IF ","+cCol+"," $ cListCol
         AADD( aDimCol , nCol )
      ENDIF
   NEXT

   IF nShowHide == 1
      oBrw:HideColumns( aDimCol ,.f.)   // показать колонки
   ELSE
      oBrw:HideColumns( aDimCol ,.t.)   // скрыть колонки
   ENDIF

RETURN Nil

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateMemTmp( cFile, cAlias, cCdp, cVia, cAlsTmp, cFileTmp )
   LOCAL cFil := hb_FNameExtSet( cFile, "" )
   LOCAL cInd := hb_FNameExtSet( cFile, ".CDX" )
   LOCAL AOC  := (App.Object):Cargo
   LOCAL aStru := AClone( AOC:aTmpStru )
   LOCAL cFld  := ATail( aStru )[1]  // по RECID делаем TAG, уст. RELATION
   LOCAL nWaitWndMax := AOC:nWaitWndMax
   LOCAL nWaitWndCnt := AOC:nWaitWndCrt
   LOCAL nWaitWnd := 0, lWaitWnd, nRec
   LOCAL cMsg := 'Wait, creating a temporary base '
   LOCAL i, k, lErr, nErr := 0

   AOC:aFClrCellVirt := {}    // список виртуальных колонок для цвета текста ячеек
   AOC:aBClrCellVirt := {}    // список виртуальных колонок для цвета фона ячеек

   FOR i := 1 TO VIRT_COLUMN_END
      // эти поля нужны для показа цвета ячеек виртуальных колонок в таблице
      AADD( aStru, { "VFORE_" + hb_ntos( i ), "N",  8, 0 } )  // цвет текста ячеек
      AADD( aStru, { "VBACK_" + hb_ntos( i ), "N",  8, 0 } )  // цвет фона ячеек
      AADD( AOC:aFClrCellVirt, "VFORE_" + hb_ntos( i )  )
      AADD( AOC:aBClrCellVirt, "VBACK_" + hb_ntos( i )  )
   NEXT

   cVia := iif( "mem:" $ cFil, "DBFCDX", cVia )

   IF ! hb_FileExists( cFile )
      MsgStop('File Dbf not found !' + CRLF + cFile  + CRLF + ProcNL() , "ERROR")
      RETURN .F.
   ENDIF

   IF ! hb_FileExists( cInd )
      USE &(cFile) ALIAS (cAlias) NEW    // open EXCLUSIVE
      INDEX ON RecNo() TAG &cFld         // TAG для RELATION
      USE
   ENDIF

   IF Empty(cCdp) ; USE &(cFile) ALIAS (cAlias) SHARED NEW
   ELSE           ; USE &(cFile) ALIAS (cAlias) SHARED NEW CODEPAGE cCdp
   ENDIF

   SET WINDOW MAIN OFF
   WaitWindow( cMsg+repl('.', 7), .T. )

   AOC:aFClrCell := {}    // список колонок для цвета текста ячеек
   AOC:aBClrCell := {}    // список колонок для цвета фона ячеек

   SET ORDER TO 1         // Set AutOpen ON
   GO TOP
   // область cAlias
   FOR i := 1 TO FCount()
      // эти поля нужны для показа цвета ячеек в таблице
      AADD( aStru, { "FORE_" + hb_ntos( i ), "N",  8, 0 } )  // цвет текста ячеек
      AADD( aStru, { "BACK_" + hb_ntos( i ), "N",  8, 0 } )  // цвет фона ячеек
      AADD( AOC:aFClrCell, "FORE_" + hb_ntos( i )  )
      AADD( AOC:aBClrCell, "BACK_" + hb_ntos( i )  )
   NEXT

   CloseMemTmp( cFileTmp, cAlsTmp )

   DBCREATE( cFileTmp, aStru, cVia, .T., cAlsTmp )
   // область cAlsTmp
   lWaitWnd := (cAlias)->( LastRec() ) > nWaitWndMax        // если в БД больше 1000 записей
   k := FieldPos( cFld )                                    // выборка и заполнение поля ключа
   (cAlias)->( dbGotop() )
   DO WHILE (cAlias)->( !Eof() )
      lErr := .T.
      BEGIN SEQUENCE WITH { |e|break(e) }
         (cAlsTmp)->( dbAppend() )
         lErr := (cAlsTmp)->( NetErr() )
      END SEQUENCE
      IF lErr
         nErr ++
         ? "DB:",cFileTmp, cAlsTmp, "Append blank error", (cAlias)->( RecNo() ), (cAlsTmp)->( RecNo() )
         IF nErr > 2   // наверно, кончилась память
            EXIT
         ENDIF
      ELSE
         (cAlsTmp)->( FieldPut( k, (cAlias)->( RecNo() ) ) )
         IF (cAlias)->( Deleted() )
            (cAlsTmp)->( dbDelete() )
         ENDIF
      ENDIF
      nRec := (cAlias)->( RecNo() )
      DO EVENTS
      IF lWaitWnd
         nWaitWnd++
         IF nWaitWnd >= nWaitWndCnt
            nWaitWnd := 0
            WaitWindow( cMsg+hb_ntos(nRec), .T. )
         ENDIF
      ENDIF
      (cAlias)->( dbSkip() )
   ENDDO
   (cAlias)->( dbGotop() )

   SELECT(cAlsTmp)
   GO TOP
   INDEX ON &cFld TAG &cFld    // надо для колонки #
                               // при фильтре будет норм. нумерация, в ID будет RecNo
   DbCommit()

   SET ORDER TO 1
   SET RELATION TO &cFld INTO &cAlias
   GO TOP

   IF lWaitWnd
       nRec := (cAlias)->( LastRec() )
       WaitWindow( cMsg+hb_ntos(nRec), .T. )
       InkeyGui(1000)
   ENDIF

   WaitWindow()            // close the wait window

   IF nErr > 0
      AlertStop(cAlsTmp+": "+"Append blank error !"+CRLF+cFileTmp)
   ENDIF

   SET WINDOW MAIN ON

RETURN .T.

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CloseMemTmp( cFile, cAlias )
   LOCAL cFil := hb_FNameExtSet( cFile, "" )
   LOCAL cInd := hb_FNameExtSet( cFile, ".CDX" )
   LOCAL AOC  := (App.Object):Cargo
   LOCAL lDel := AOC:lTmpErase

   cFile := hb_FNameExtSet( cFile, ".DBF" )

   IF cAlias != NIL .and. Select(cAlias) > 0
      (cAlias)->( dbCloseArea() )
   ENDIF
   IF "mem:" $ cFil ; dbDrop(cFil, cFil, "DBFCDX")
   ELSEIF lDel      ; fErase(cInd ) ; fErase(cFile)
   ENDIF

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
FUNCTION myVirtColumColorSaveCell(nBrw)
   LOCAL nRecno, nBrowse, nKolvo, cName, nRec, aItogo
   LOCAL nClr2VCol, oCol, nFlg, nFClr, nBClr, lDel, cAlsReal
   LOCAL xVal, cFiels, nJ, nI, aLineBClr, aLineFClr, aFClrCel, aBClrCel
   LOCAL cBrw  := "oBrw"+hb_ntos(nBrw)
   LOCAL oBrw  := This.&(cBrw).Object
   LOCAL AOC   := (App.Object):Cargo  // использовать из контейнера приложения свои переменные
   LOCAL aFile := { AOC:aFile1, AOC:aFile2 }
   LOCAL nWaitWndMax := AOC:nWaitWndMax
   LOCAL nWaitWndCnt := AOC:nWaitWndSave
   LOCAL nWaitWnd := 0, lWaitWnd
   LOCAL cMsg := "Wait, checking in progress. "+cBrw+" - "
   LOCAL cAls := oBrw:cAlias
   LOCAL O := oBrw:Cargo             // использовать из контейнера таблицы свои переменные
   LOCAL nKeyNo := oBrw:nColumn("ORDKEYNO")
   LOCAL aOVirt := {}

   lWaitWnd := (cAls)->( LastRec() ) > nWaitWndMax        // если в БД больше 1000 записей

   WaitWindow( cMsg + " ... ", .T. )

   aFClrCel := AOC:aFClrCell         // список колонок цвета текста ячеек
   aBClrCel := AOC:aBClrCell         // список колонок цвета фона ячеек
   nBrowse  := nBrw                  // oBrw:Cargo:nBrowse    // номер таблицы
   nRecno   := (cAls)->( RecNo() )
   nKolvo   := LastRec()
   cAlsReal := aFile[ nBrw ][ NAME_ALIAS ] // cAlsReal := IIF( nBrowse == 1, "ONE", "TWO" )
   aItogo   := Array( VIRT_COLUMN_END )
   AFILL( aItogo, 0 )

   // берем цвет из второго вирт.столбца
   IF nBrowse == 1 .OR. nBrowse == 2
      nClr2VCol := O:aClrBrw[nBrowse]
   ELSE
      nClr2VCol := O:nWHITE
   ENDIF

   (cAls)->( dbGotop() )
   oBrw:GoTop()

   // виртуальные колонки и обычные колонки - итоги
   FOR EACH oCol IN oBrw:aColumns
      oCol:Cargo:nSum  := 0
      oCol:Cargo:aVirt := oKeyData()
      IF "VIRT" $ oCol:cName
         AADD( aOVirt, oCol:Cargo )      // запомнили cargo объекты для вирт. колонок
      ENDIF
   NEXT
   DO EVENTS
   hb_gcAll()                            // мусор собираем
   DO EVENTS

   DO WHILE (cAls)->( !EOF() )

      nRec := (cAls)->( RECNO() )
      lDel := (cAls)->( DELETED() )

      aLineBClr := myGetColorBackLine( oBrw )   // массив фона каждой строки таблицы
      aLineFClr := myGetColorForeLine( oBrw )   // цвет текста каждой строки таблицы

      FOR nI := 1 TO LEN(oBrw:aColumns)

         oCol  := oBrw:GetColumn( nI )
         //xVal  := oBrw:GetValue( nI )   // лишние чтения данных колонки не нужны !
         cName := oCol:cName
         nBClr := aLineBClr[ nI ]   // цвет фона каждой ячейки
         nFClr := aLineFClr[ nI ]   // цвет текста каждой ячейки

         // виртуальные колонки
         IF cName == "VIRT_1"
            nFlg           := iif( lDel, 1, 0 )    // удалённые записи
            (cAls)->( FieldPut(FieldPos(cName), nFlg) )
            //(cAls)->&cName := nFlg // замедляет работу, т.к. идет вызов macro генератора, а потом команда выше !
            aItogo[1] += nFlg
            IF nFlg > 0
               (cAls)->VFORE_1 := O:nFCDelRec      // ставим нужный цвет текcта
               (cAls)->VBACK_1 := O:nBCDelRec      // ставим нужный цвет фона
               oCol:Cargo:aVirt:Set(nRec, { O:nBCDelRec, O:nFCDelRec })  // { Back, Fore }
            ENDIF

         ELSEIF cName == "CITY"
            xVal := oBrw:GetValue( nI )
            nFlg := iif( "DMITROV" $ UPPER(xVal), 1, 0 )
            nJ   := nI - nKeyNo   // oBrw:nColumn("ORDKEYNO")
            IF nFlg > 0
               cFiels := aFClrCel[ nJ ]
               (cAls)->( FieldPut(FieldPos(cFiels), O:nBLUE) )
               //(cAls)->&cFiels := O:nBLUE
               cFiels := aBClrCel[ nJ ]
               (cAls)->( FieldPut(FieldPos(cFiels), nClr2VCol) )
               //(cAls)->&cFiels := nClr2VCol   // ставим нужный цвет фона - см. выше

               (cAls)->VIRT_2 := 1
               aItogo[2]      += 1
               oCol:Cargo:aVirt:Set(nRec, { nClr2VCol, O:nBLUE })  // { Back, Fore }
                aOVirt[2]:aVirt:Set(nRec, { nClr2VCol, O:nBLUE })  // { Back, Fore }

               (cAls)->VFORE_2 := O:nBLUE     // ставим нужный цвет текcта
               (cAls)->VBACK_2 := nClr2VCol   // ставим нужный цвет фона  - см. выше
            ENDIF

         ELSEIF cName == "STREET"
            xVal := oBrw:GetValue( nI )
            nFlg := iif( "GAGARIN" $ UPPER(xVal), 1, 0 )
            nJ   := nI - nKeyNo   // oBrw:nColumn("ORDKEYNO")
            IF nFlg > 0
               cFiels          := aFClrCel[ nJ ]
               (cAls)->( FieldPut(FieldPos(cFiels), O:nBLACK) )
               //(cAls)->&cFiels := O:nBLACK
               cFiels := aBClrCel[ nJ ]
               (cAls)->( FieldPut(FieldPos(cFiels), O:nHBLUE2) )
               //(cAls)->&cFiels := O:nHBLUE2   // ставим нужный цвет фона - см. выше

               (cAls)->VIRT_3 := 1
               aItogo[3]      += 1
               oCol:Cargo:aVirt:Set(nRec, { O:nHBLUE2, O:nBLACK })  // { Back, Fore }
                aOVirt[3]:aVirt:Set(nRec, { O:nHBLUE2, O:nBLACK })  // { Back, Fore }

               (cAls)->VFORE_3 := O:nBLACK    // ставим нужный цвет текcта
               (cAls)->VBACK_3 := O:nHBLUE2   // ставим нужный цвет фона  - см. выше
            ENDIF

         ELSEIF cName == "YEAR2"
            nJ   := nI - nKeyNo   // oBrw:nColumn("ORDKEYNO")
            xVal := oBrw:GetValue( nI )
            IF VALTYPE(xVal) != "N"           // обработка ошибочной ситуации
               (cAls)->VBACK_4 := O:nBLACK
               aOVirt[4]:aVirt:Set(nRec, { O:nBLACK, 0 })  // { Back, Fore }
            ELSE
               nFlg := iif( xVal > 2020, 1, 0 )
               IF nFlg > 0
                  cFiels          := aFClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), nFClr) )
                  //(cAls)->&cFiels := nFClr
                  cFiels := aBClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), O:nGREEN3) )
                  //(cAls)->&cFiels := O:nGREEN3   // ставим нужный цвет фона - см. выше

                  (cAls)->VIRT_4 := 1
                  aItogo[4]      += 1
                  oCol:Cargo:aVirt:Set(nRec, { O:nGREEN3, nFClr })  // { Back, Fore }
                   aOVirt[4]:aVirt:Set(nRec, { O:nGREEN3, nFClr })  // { Back, Fore }

                  (cAls)->VFORE_4 := nFClr       // ставим нужный цвет текcта
                  (cAls)->VBACK_4 := O:nGREEN3   // ставим нужный цвет фона  - см. выше
               ENDIF
            ENDIF

         ELSEIF cName == "DOLG2014"
            nJ   := nI - nKeyNo   // oBrw:nColumn("ORDKEYNO")
            xVal := oBrw:GetValue( nI )
            IF VALTYPE(xVal) != "N"           // обработка ошибочной ситуации
               (cAls)->VBACK_5 := O:nBLACK
               aOVirt[5]:aVirt:Set(nRec, { O:nBLACK, 0 })  // { Back, Fore }
            ELSE
               nFlg := iif( xVal < 0, 1, 0 )
               IF nFlg > 0
                  cFiels          := aFClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), O:nGREEN) )
                  //(cAls)->&cFiels := O:nGREEN
                  cFiels := aBClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), O:nHRED) )
                  //(cAls)->&cFiels := O:nHRED     // ставим нужный цвет фона - см. выше

                  (cAls)->VIRT_5 := 1
                  aItogo[5]      += 1
                  oCol:Cargo:aVirt:Set(nRec, { O:nHRED, O:nGREEN })  // { Back, Fore }
                   aOVirt[5]:aVirt:Set(nRec, { O:nHRED, O:nGREEN })  // { Back, Fore }

                  (cAls)->VFORE_5 := O:nGREEN    // ставим нужный цвет текcта
                  (cAls)->VBACK_5 := O:nHRED     // ставим нужный цвет фона  - см. выше
               ENDIF
            ENDIF

         ELSEIF cName == "DOLG2015"
            nJ   := nI - nKeyNo   // oBrw:nColumn("ORDKEYNO")
            xVal := oBrw:GetValue( nI )
            IF VALTYPE(xVal) != "N"              // обработка ошибочной ситуации
               (cAls)->VBACK_6 := O:nBLACK
               aOVirt[6]:aVirt:Set(nRec, { O:nBLACK, 0 })  // { Back, Fore }
            ELSE
               nFlg := iif( xVal < 0, 1, 0 )
               IF nFlg > 0
                  cFiels          := aFClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), O:nGREEN) )
                  //(cAls)->&cFiels := O:nGREEN
                  cFiels := aBClrCel[ nJ ]
                  (cAls)->( FieldPut(FieldPos(cFiels), O:nPURPLE2) )
                  //(cAls)->&cFiels := O:nPURPLE2   // ставим нужный цвет фона - см. выше

                  (cAls)->VIRT_6 := 1
                  aItogo[6]      += 1
                  oCol:Cargo:aVirt:Set(nRec, { O:nPURPLE2, O:nGREEN })  // { Back, Fore }
                   aOVirt[6]:aVirt:Set(nRec, { O:nPURPLE2, O:nGREEN })  // { Back, Fore }

                  (cAls)->VFORE_6 := O:nGREEN     // ставим нужный цвет текcта
                  (cAls)->VBACK_6 := O:nPURPLE2   // ставим нужный цвет фона  - см. выше
               ENDIF
            ENDIF

         ENDIF

      NEXT
      DO EVENTS

      IF lWaitWnd
         nWaitWnd++
         IF nWaitWnd >= nWaitWndCnt
            nWaitWnd := 0
            WaitWindow( cMsg+hb_ntos(nRec), .T. )
         ENDIF
      ENDIF

      SELECT(cAls)
      (cAls)->( dbSkip())
   ENDDO
   (cAls)->( dbGoto(nRecno) )
   oBrw:GoTop()

   // запись итого в виртуальные колонки
   FOR nI := 1 TO LEN(oBrw:aColumns)
      oCol  := oBrw:GetColumn( nI )
      cName := oCol:cName
      IF cName == "ORDKEYNO"
         EXIT
      ENDIF
      oCol:Cargo:nSum := aItogo[nI]
   NEXT

   mySumVirtFoot( oBrw, .T. )   // вывод подвала виртуальных колонок

   IF lWaitWnd
       nRec := (cAls)->( LastRec() )
       WaitWindow( cMsg+hb_ntos(nRec), .T. )
       InkeyGui(1000)
   ENDIF

   WaitWindow()                 // закрыть окно сообщения

   oBrw:Refresh() ; InkeyGui(500)

   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myGetColorBackLine(oBrw)
   LOCAL nCol, nColor, aClr := {}

   For nCol := 1 TO Len( oBrw:aColumns )
      nColor := oBrw:aColumns[ nCol ]:nClrBack
      If Valtype( nColor ) == "B"
         nColor := Eval( oBrw:aColumns[ nCol ]:nClrBack, oBrw:nAt, nCol, oBrw )
      EndIf
      AADD( aClr, nColor )
   Next

RETURN aClr

///////////////////////////////////////////////////////////////////
FUNCTION myGetColorForeLine(oBrw)
   LOCAL nCol, nColor, aClr := {}

   For nCol := 1 TO Len( oBrw:aColumns )
      nColor := oBrw:aColumns[ nCol ]:nClrFore
      If Valtype( nColor ) == "B"
         nColor := Eval( oBrw:aColumns[ nCol ]:nClrFore, oBrw:nAt, nCol, oBrw )
      EndIf
      AADD( aClr, nColor )
   Next

RETURN aClr

//////////////////////////////////////////////////////////////////
// Back цвет Header вирт. колонок таблицы
STATIC FUNCTION myClrVirtHead( oBrw, nClr, lDraw )
   Local i
   Default nClr := oBrw:Cargo:nClr4
   FOR i := 1 TO oBrw:nColumn("ORDKEYNO")
      oBrw:aColumns[ i ]:nClrHeadBack := nClr
   NEXT
   IF ISLOGICAL(lDraw)
      oBrw:DrawHeaders(lDraw)
   ENDIF
RETURN Nil

//////////////////////////////////////////////////////////////////
// вывод подвала виртуальных колонок
STATIC FUNCTION mySumVirtFoot( oBrw, lDraw, aSum )
   Local i, oCol
   IF Empty(aSum)
      aSum := Array(VIRT_COLUMN_END)
      FOR i := 1 TO VIRT_COLUMN_END
          oCol := oBrw:aColumns[ i ]
          aSum[ i ] := oCol:Cargo:nSum
      NEXT
   ENDIF
   FOR i := 1 TO VIRT_COLUMN_END
       oCol := oBrw:aColumns[ i ]
       oCol:cFooting := iif( Empty(aSum[ i ]), "", hb_ntos(aSum[ i ]) )
   NEXT
   IF !Empty(lDraw)
      oBrw:DrawFooters()
   ENDIF
RETURN Nil

//////////////////////////////////////////////////////////////////
// посчет сумм итогов виртуальных колонок
STATIC FUNCTION mySumVirtCalc( oBrw )
   LOCAL cAls, aSum, nVal, nRec, cFld, i, aColVirt
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   aColVirt := AOC:aColVirt  //{ "VIRT_1", "VIRT_2", ... } // список вирт. колонок массивом
   cAls     := oBrw:cAlias
   aSum     := Array( LEN(aColVirt) )
   AFill(aSum, 0)

   DO WHILE (cAls)->( !EOF() )
      nRec := (cAls)->( RecNo() )
      FOR i := 1 TO LEN(aColVirt)
         cFld := aColVirt[ i ]
         nVal := (cAls)->&cFld
         IF nVal > 0
            aSum[ i ] += 1
         ENDIF
      NEXT
      (cAls)->( dbSkip() )
   ENDDO
   (cAls)->( dbGotop() )

RETURN aSum

//////////////////////////////////////////////////////////////////
// фильтр по таблице
STATIC FUNCTION myFilter(nFilter,oBrw)
   LOCAL cFilt, cFltr := "["+oBrw:cParentWnd+"], ["+oBrw:cControlName+"]"
   LOCAL aSum, aColVirt, nI
   LOCAL AOC := (App.Object):Cargo  // использовать из контейнера приложения свои переменные

   aColVirt := AOC:aColVirt         // список вирт. колонок массивом

   myClrVirtHead( oBrw, oBrw:Cargo:nClr4, .F. )      // очистка цвета фона шапки таблицы виртуальных колонок

   IF     nFilter == 99                              // очистить фильтр
      oBrw:Cargo:nFilter := 0                        // нет фильтра по таблице
      oBrw:Cargo:cFilter := '""'                     // очистить условие фильтра
   ELSEIF nFilter == 0                               // фильтр по всем полям
      cFilt := ""
      FOR nI := 1 TO LEN(aColVirt)
         cFilt += aColVirt[nI] + " > 0 "
         cFilt += IIF( nI == LEN(aColVirt), "", ".OR." )
      NEXT
      oBrw:Cargo:nFilter := 100                      // номер фильтра по таблице
      oBrw:Cargo:cFilter := cFilt //"ALL VIRTUAL COLUMNS OF THE TABLE" // по всем вирт.колонкам
      myClrVirtHead( oBrw, oBrw:Cargo:nORANGE )      // цвет фона шапка таблицы виртуальных колонок по фильтру

   ELSE
      cFilt := aColVirt[nFilter] + " > 0 "
      oBrw:Cargo:nFilter := nFilter                  // номер колонки для фильтра по таблице
      oBrw:Cargo:cFilter := cFilt //oBrw:Cargo:aFilter[nFilter]
      oBrw:aColumns[ nFilter ]:nClrHeadBack := oBrw:Cargo:nORANGE
   ENDIF

   oBrw:FilterData(cFilt)

   IF !Empty(cFilt) ; aSum := mySumVirtCalc( oBrw )
   ENDIF

   mySumVirtFoot( oBrw, .F., aSum )   // вывод подвала виртуальных колонок

   oBrw:DrawHeaders(.T.)
   oBrw:SetFocus()
   DO EVENTS

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myFont( lCreate, nFSDef )
   LOCAL aFont := {}, cFont
   DEFAULT nFSDef := _HMG_DefaultFontSize

   // создаем массив имен фонтов для режимов или убираем их
   AAdd( aFont, "TsbNorm"   )
   AAdd( aFont, "TsbBold"   )
   AAdd( aFont, "TsbSpecH"  )
   AAdd( aFont, "TsbSuperH" )
   AAdd( aFont, "TsbEdit"   )

   IF empty(lCreate)
      FOR EACH cFont IN aFont ; _ReleaseFont( cFont )
      NEXT
   ELSE
      DEFINE FONT TsbNorm   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef
      DEFINE FONT TsbBold   FONTNAME "Tahoma"             SIZE nFSDef BOLD
      DEFINE FONT TsbSpecH  FONTNAME _HMG_DefaultFontName SIZE nFSDef BOLD
      DEFINE FONT TsbSuperH FONTNAME "Comic Sans MS"      SIZE nFSDef + 2 BOLD
      DEFINE FONT TsbEdit   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef BOLD
   ENDIF

RETURN .T.

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "(c) 2020 Verchenko Andrey <verchenkoag@gmail.com>;"
   cMsg += "(c) 2020 Sergej Kiselev <bilance@bilance.lv>;;"
   cMsg += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";"
   cMsg += "(c) Grigory Filatov http://www.hmgextended.com;;"
   cMsg += PadC( "This program is Freeware!", 60 ) + ";"
   cMsg += PadC( "Copying is allowed!", 60 ) + ";"

   AlertInfo( cMsg, "About this demo", , , {RED} , , )

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgVirtColunm()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Описание виртуальных колонок таблицы, в качестве примера:;"
   cMsg += "(1) - удаленная запись ;"
   cMsg += "(2) - в колонке CITY содержится слово DMITROV ;"
   cMsg += "(3) - в колонке STREET содержится слово GAGARIN ;"
   cMsg += "(4) - YEAR2 > 2020;"
   cMsg += "(5) - DOLG2014 < 0;"
   cMsg += "(6) - DOLG2015 < 0;;"
   cMsg += "Description of virtual table columns, as an example:;"
   cMsg += "(1) - deleted record;"
   cMsg += "(2) - the CITY column contains the word DMITROV;"
   cMsg += "(3) - the STREET column contains the word GAGARIN;"
   cMsg += "(4) - YEAR2 > 2020;"
   cMsg += "(5) - DOLG2014 < 0;"
   cMsg += "(6) - DOLG2015 < 0"

   AlertInfo( cMsg, "About virtual table columns", , , {RED} , , )

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgVirtHeadColunm()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Кликнуть правой кнопкой мыши по шапке виртуальных столбцов таблицы,;"
   cMsg += "будет включен фильтр по этой колонке,;"
   cMsg += "Кликнуть левой кнопкой мыши - будет показ контекстного меню:;"
   cMsg += "1) Показать/скрыть виртуальные колонки;"
   cMsg += "2) Фильтр по виртуальным колонкам;;"
   cMsg += "Right-click on the header of the virtual table columns;"
   cMsg += "filter will be enabled for this column;"
   cMsg += "Left-click - the context menu will be shown:;"
   cMsg += "1) Show / hide virtual columns;"
   cMsg += "2) Filter by virtual columns"

   AlertInfo( cMsg, "About virtual table columns", , , {RED} , , )

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgInfoHeader()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Кликнуть правой/левой кнопкой мыши по шапке колонок таблицы:;"
   cMsg += "Показ контекстного меню для колонок шапки таблицы;;"
   cMsg += "Click with the right/left mouse button on the header of the table columns:;"
   cMsg += "Show context menu for table header columns"

   AlertInfo( cMsg, "About virtual table columns", , , {RED} , , )

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION myFilterTsb(oBrw)
   LOCAL cMsg, cFlt := ( oBrw:cAlias )->( dbFilter() )

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Фильтр по базе = "
   cMsg += IIF( LEN(cFlt) == 0, 'нет', 'есть' ) + ";"
   cMsg += "Filter by base = "
   cMsg += IIF( LEN(cFlt) == 0, 'no', 'there is' ) + ";"
   cMsg += "(" + oBrw:cAlias + ")->( dbFilter() ) = " + IIF( LEN(cFlt) == 0, '""', cFlt )
   cMsg += ";;"
   cMsg += "Номер фильтра по таблице = " + HB_NtoS(oBrw:Cargo:nFilter) + ";"
   cMsg += "Условие фильтра по колонке таблицы = " + oBrw:Cargo:cFilter + ";;"
   cMsg += "Filter number for the table = " + HB_NtoS(oBrw:Cargo:nFilter) + ";"
   cMsg += "Filter condition by table column = " + oBrw:Cargo:cFilter + ";;"

   AlertInfo( cMsg, "About virtual table columns", , , {LGREEN} , , )

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN "Called from " + ProcName( nVal + 1 ) + "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

///////////////////////////////////////////////////////////////////
// При наличии файла добавить число версии в имя
FUNCTION GetFileNameMaskNum( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)
