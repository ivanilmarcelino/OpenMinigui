/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Таблица по dbf и массиву. Настройки таблицы.
 * Использование типов полей в базе '^', '+', '=', '@', 'T'
 * Использование вспомогательного класса TSBcell для быстрого экспорта данных.
 * Экспорт таблицы Excel, Word, Open Office в файлы: xls/xml/doc/ods
 * Поддержка формул в таблице для рассчёта в самом Excel (текст в формулу).
 * Table on dbf and array. Table settings.
 * Using field types in the database '^', '+', '=', '@', 'T'
 * Using the auxiliary TSBcell class for quick data export.
 * Export Excel, Word, Open Office spreadsheets to files: xls/xml/doc/ods
 * Support for formulas in the table for calculation in Excel itself (text to formula).
*/

#define _HMG_OUTLOG
#define SHOW_TITLE   "Tsbrowse Dbf/Array for Report Demo / Export table to Excel, Word, Open Office"
#define SHOW_VERSION "  Version 1.2 from 10.10.2024"

#include "minigui.ch"
#include "TSBrowse.ch"

PROCEDURE MAIN(cParam)
   LOCAL lArray, aParam, nFSDef
   DEFAULT cParam := ""

   If empty(cParam)
      lArray := .F.
   Else
      aParam := &cParam
      lArray := aParam[1]
   EndIf

   SET DECIMALS TO 4
   SET DATE     TO GERMAN
   SET EPOCH    TO 2000
   SET CENTURY  ON
   SET EXACT    ON
   SET DATE FORMAT "DD.MM.YY"
   SET MSGALERT BACKCOLOR TO { 141, 179, 226 }      // for HMG_Alert()
   SET MENUSTYLE EXTENDED                           // switch the menu style to advanced
   SET MULTIPLE OFF WARNING

   SET OOP ON

   SET FONT TO "Comic Sans MS", 11
   nFSDef := _HMG_DefaultFontSize
   // фонты для таблицы
   DEFINE FONT Norm   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef
   DEFINE FONT Bold   FONTNAME _HMG_DefaultFontName SIZE nFSDef BOLD
   DEFINE FONT SpecH  FONTNAME "Arial"              SIZE nFSDef BOLD
   DEFINE FONT SuperH FONTNAME "Comic Sans MS"      SIZE nFSDef + 2 BOLD
   DEFINE FONT Edit   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef BOLD
   // фонт для таблицы добавочный
   DEFINE FONT Itog   FONTNAME "Arial Black"        SIZE nFSDef
   // фонт для HMG_Alert()
   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono" SIZE nFSDef

   _SetGetLogFile( GetStartUpFolder() + "\_MsgLog.txt" ) // путь+имя лог-файла
   fErase( _SetGetLogFile() )                            // очистить лог-файл

   FORM_MAIN(lArray)

   // удаление фонтов
   RELEASE FONT Norm
   RELEASE FONT Bold
   RELEASE FONT SpecH
   RELEASE FONT SuperH
   RELEASE FONT Edit
   RELEASE FONT Itog

RETURN


PROCEDURE FORM_MAIN(lArray)
   LOCAL aDatos, aArray, aHead, aSize, aFoot, aPict, aAlign
   LOCAL aFAlign, aKfcWidth, aFldCenter, aField, aName
   LOCAL oBrw, nY, nX, nW, nH, nI, cName, aSupHd, aFont
   LOCAL hFont1, hFont2, hFont3, hFont4

   hFont1 := GetFontHandle( "Norm"   )
   hFont2 := GetFontHandle( "Bold"   )
   hFont3 := GetFontHandle( "SuperH" )
   hFont4 := GetFontHandle( "Itog" )

   DEFINE WINDOW test  ;
      TITLE SHOW_TITLE + SHOW_VERSION ;
      ICON "1MAIN_ICO" ;
      MAIN TOPMOST     ;
      ON INIT    {|| This.Topmost := .F., oBrw:SetFocus() } ;
      ON RELEASE {|| iif( ISCHAR(aArray), (aArray)->(dbCloseArea()), ) } ;
      NOMAXIMIZE NOSIZE

      DEFINE MAIN MENU
         POPUP "Mode tbrowse"   FONT hFont4
            ITEM "Mode tbrowse-dbf"    ACTION myRestart(1) FONT hFont3
            ITEM "Mode tbrowse-array"  ACTION myRestart(2) FONT hFont3
            SEPARATOR
            ITEM "Exit"                ACTION test.Release   FONT hFont4
         END POPUP
         POPUP "Data access"   FONT hFont4
            ITEM "Enum, SpcHd, Header, Footer  - F8 (Mode: array+dbf)" ACTION myF8(oBrw) FONT hFont1
            ITEM "Enum, SpcHd, Header, Footer  - F9 (Mode: only dbf)"  ACTION myF9(oBrw) FONT hFont1
            ITEM "Access only footer data      - F5 "                  ACTION myF5(oBrw) FONT hFont1
            ITEM "Access only ENUMERATOR data  - F4 "                  ACTION myF4(oBrw) FONT hFont1
            ITEM "Show selected cell in window - F3 "                  ACTION myF3(oBrw) FONT hFont1
            ITEM "Read table data via function - F2 "                  ACTION myF2(oBrw) FONT hFont1
         END POPUP
         POPUP "Data input"   FONT hFont4
            ITEM "Column 31 and 32 entry"  ACTION myCol3132(oBrw) FONT hFont1
         END POPUP
         POPUP "Export"         FONT hFont4
            ITEM "Microsoft Office"                   DISABLED   FONT hFont3
            ITEM "   Export to Ole-Excel/white (xls)" ACTION ToExcel7(oBrw,1) FONT hFont1
            ITEM "   Export to Ole-Excel/color (xls)" ACTION ToExcel7(oBrw,2) FONT hFont1
            ITEM "   Export to Ole-Excel/Formula    " ACTION ToExcel7(oBrw,3) FONT hFont1
            ITEM "   Export to Ole-Word/white  (doc)" ACTION ToWord7(oBrw,1)  FONT hFont1
            ITEM "   Export to Ole-Word/color  (doc)" ACTION ToWord7(oBrw,2)  FONT hFont1
            SEPARATOR
            ITEM "XML document format"                DISABLED   FONT hFont3
            ITEM "   Export to XML/white    (xml)"    ACTION ToXml7(oBrw,1)  FONT hFont1
            ITEM "   Export to XML/color    (xml)"    ACTION ToXml7(oBrw,2)  FONT hFont1
            SEPARATOR
            ITEM "Open Office"                        DISABLED   FONT hFont3
            ITEM "   Export to Ole-Calc/white  (ods)" ACTION ToCalc7(oBrw,1)  FONT hFont1
            ITEM "   Export to Ole-Calc/color  (ods)" ACTION ToCalc7(oBrw,2)  FONT hFont1
         END POPUP
         POPUP "About"      FONT hFont4
            ITEM "Program Info"  ACTION MsgAbout()   FONT hFont2
         END POPUP
      END MENU

      DEFINE STATUSBAR
         STATUSITEM "0"                 // WIDTH 0 FONTCOLOR BLACK
         STATUSITEM "Mode: " + IIF(lArray,"Tsb-Array", "Tsb-Dbf") WIDTH 320 FONTCOLOR RED
         STATUSITEM "Item 2" WIDTH 230  FONTCOLOR GRAY
         STATUSITEM "Item 3" WIDTH 230  FONTCOLOR GRAY
      END STATUSBAR

      nY := 1 + iif( IsVistaOrLater(), GetBorderWidth ()/2, 0 )
      nX := 1 + iif( IsVistaOrLater(), GetBorderHeight()/2, 0 )
      nW := test.WIDTH  - 2 * GetBorderWidth()
      nH := test.HEIGHT - 2 * GetBorderHeight() -    ;
            GetTitleHeight() - test.StatusBar.Height - GetMenuBarHeight()

      aDatos     := CreateDatos( lArray )   // загрузка массивов из базы
      aArray     := aDatos[ 1 ]
      aHead      := aDatos[ 2 ]
      aSize      := aDatos[ 3 ]
      aFoot      := aDatos[ 4 ]
      aPict      := aDatos[ 5 ]
      aAlign     := aDatos[ 6 ]
      aName      := aDatos[ 7 ]
      aField     := aDatos[ 8 ]
      aSupHd     := aDatos[ 9 ]
      aFAlign    := aDatos[ 10 ]               // Footer align
      aKfcWidth  := aDatos[ 11 ]               // изменить ширину колонок
      aFldCenter := aDatos[ 12 ]               // центрирование строковых колонок таблицы
      // фонты для таблицы - можно указать все сразу
      aFont      := { "Norm", "Bold", "Bold", "SpecH", "SuperH", "Edit", "Itog" }
      aFoot      := .T.  // создаем пустые значения для подвала, расчёт будет в mySumTsb()

      IF ISCHAR( aArray ) ; dbSelectArea( aArray )
      ENDIF

      DEFINE TBROWSE oBrw ;
             AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ;
             FONT       aFont                               ;
             BRUSH      { 255, 255, 230 }                   ;
             HEADERS    aHead                               ;
             COLSIZES   aSize                               ;
             PICTURE    aPict                               ;
             JUSTIFY    aAlign                              ;
             COLUMNS    aField                              ;
             COLNAMES   aName                               ;
             COLNUMBER  { 1, 50 }                           ;
             FOOTERS    aFoot                               ;
             FIXED      COLSEMPTY                           ;
             LOADFIELDS                                     ;
             ENUMERATOR /*EDIT GOTFOCUSSELECT*/

             mySetTsb( oBrw )                      // настройки таблицы

             // подвал отбивка
             AEval(aName, {|cn,nn| :GetColumn(cn):nFAlign := aFAlign[nn] })
             // изменить (уменьшить) ширину колонок
             AEval(aKfcWidth, {|aw| :GetColumn(aw[1]):nWidth *= aw[2] })
             // центрирование строковых колонок таблицы
             For EACH cName IN aFldCenter
                 :GetColumn( cName ):bDecode := {|cv| Alltrim(cv) }
             Next
             // убрать 0 и 0.0 и пустую дату из таблицы, чтобы не было при экспорте
             For nI := :nColumn( "NN" ) TO Len( :aColumns )
                IF HB_ISNUMERIC( :GetValue(nI) ) .or. HB_ISDATE(:GetValue(nI))
             :GetColumn( nI ):bDecode := {|nv| iif( empty(nv), "", nv ) }
                ENDIF
             Next

             // ставим фонты в :Cargo запоминая и передаём список колонок для изменений
             myFontTsb( aFont, , oBrw, { "NN", "M1DOLG", "M2DOLG", "COL16" } )

             myColorTsb( oBrw )          // цвета на таблицу
             myColorTsbElect( oBrw )     // цвета избранные
             mySumTsb ( oBrw )           // суммирование колонок таблицы
             myDelColumnTsb( oBrw )      // убрать колонки из отображения
             mySupHdTsb( oBrw, aSupHd )  // SuperHeader
             myEnumTsb( oBrw )           // ENUMERATOR по порядку
             mySet2Tsb( oBrw )           // настройки таблицы дополнительные

             :nFreeze     := :nColumn("PODEZD") // заморозить таблицу до этого столбца
             :lLockFreeze := .F.                // избегать прорисовки курсора на замороженных столбцах

             :UserKeys(VK_F2, {|ob| myF2(ob) })  // инфо по массиву ячеек таблицы
             :UserKeys(VK_F3, {|ob| myF3(ob) })  // инфо по текущей ячейке таблицы
             :UserKeys(VK_F4, {|ob| myF4(ob) })  // инфо по ENUMERATOR
             :UserKeys(VK_F5, {|ob| myF5(ob) })  // инфо по подвалу
             :UserKeys(VK_F8, {|ob| myF8(ob) })  // инфо по таблице Mode:Array + Mode:Dbf
             :UserKeys(VK_F9, {|ob| myF9(ob) })  // инфо по таблице Mode:Dbf

             :bOnEscape := {|ob| DoMethod(ob:cParentWnd, "Release") }  // выход по ESC

      END TBROWSE ON END {|ob| ob:SetNoHoles() }

      This.Minimize ;  This.Restore ; DO EVENTS

   END WINDOW

   test.Activate

RETURN


STATIC FUNCTION myFontTsb( nAt, nCol, oBrw, aName )
   LOCAL hFont, cNam, O

   IF HB_ISARRAY( nAt )                       // установка handle фонтов в :Cargo

      WITH OBJECT oBrw:Cargo
      IF HB_ISARRAY(aName)
         cNam := ","
         AEval( aName, {|cn| cNam += cn + ',' } )
         :cColNameList := cNam              // создать в контейнере :cColNameList
                                            // для списка колонок для фонта [3]
      ENDIF
      FOR EACH cNam IN nAt
          :Set( upper('hFont_'+cNam), GetFontHandle(cNam) )
      NEXT
      END WITH

      FOR EACH O IN oBrw:aColumns
          O:hFont     := {|nr,nc,ob| myFontTsb(nr, nc, ob     )}  // фонты для строк   таблицы
          O:hFontFoot := {|nr,nc,ob| myFontTsb(nr, nc, ob, .T.)}  // фонты для подвала таблицы
      NEXT

   ELSEIF aName == NIL                       // работа в блоке кода строки тсб

      Default nAt := 0

      cNam := ',' + oBrw:aColumns[ nCol ]:cName + ','

      IF cNam $ oBrw:Cargo:cColNameList ; hFont := oBrw:Cargo:hFont_Itog
      ELSE                              ; hFont := oBrw:Cargo:hFont_Norm
      ENDIF

   ELSEIF HB_ISLOGICAL(aName)

      Default nAt := 0

      IF aName                               // Footer

         cNam := ',' + oBrw:aColumns[ nCol ]:cName + ','

         IF cNam $ oBrw:Cargo:cColNameList ; hFont := oBrw:Cargo:hFont_Itog
         ELSE                              ; hFont := oBrw:Cargo:hFont_Bold
         ENDIF

      ELSE                                   // Header

      ENDIF

   ENDIF

RETURN hFont


STATIC FUNCTION mySetTsb( oBrw )
   WITH OBJECT oBrw
      :Cargo         := oKeyData()  // создает объект без переменных (условно пустой) используем ниже по коду
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
      :nCellMarginLR := 1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   END WITH
RETURN Nil


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


STATIC FUNCTION myColorTsb( oBrw )
   LOCAL O

   WITH OBJECT oBrw:Cargo
      // 0. строки ( demo ) создание переменных, как пример, в тексте prg не используется
      :nText      :=  GetSysColor( COLOR_WINDOWTEXT )
      :nPane      :=  GetSysColor( COLOR_WINDOW )
      :nFocuFore  :=  GetSysColor( COLOR_HIGHLIGHTTEXT )
      :nFocuBack  :=  GetSysColor( COLOR_HIGHLIGHT )
      :nSeleFore  :=  CLR_HGRAY                        // nClrSeleFore NO focused
      :nSeleBack  :=  CLR_GRAY                         // nClrSeleBack NO focused
      :nBtnText   :=  GetSysColor( COLOR_BTNTEXT )
      :nBtnFace   :=  GetSysColor( COLOR_BTNFACE )     // end demo

      // 1. переменные цветов из #define CLR_... и RGB(...), меняя правую часть меняем цвета тсб
      :nHRED      :=  CLR_HRED
      :n_HRED     := -CLR_HRED
      :n_HBLUE    := -RGB(128,225,225)
      :nHBLUE     :=  RGB(128,225,225)
      :nHBLUE2    :=  RGB(  0,176,240)   //CLR_HBLUE
      :nHGRAY     :=  CLR_HGRAY
      :nGRAY      :=  CLR_GRAY
      :nBLACK     :=  CLR_BLACK
      :nYELLOW    :=  CLR_YELLOW
      :nGREEN     :=  CLR_GREEN
      :nGREEN2    :=  RGB(  0,255,  0)
      :nORANGE    :=  CLR_ORANGE
      :nRED       :=  CLR_RED
      :nWHITE     :=  CLR_WHITE
      :nBLUE      :=  CLR_BLUE
      :n_BLUE     := -CLR_BLUE

      // 2. переменные RGB( ... ) для использования
      :nRgb0      :=  RGB(  0,  0,  0)
      :nRgb1      :=  RGB(180,180,180)
      :nRgb2      :=  RGB(255,255,240)
      :nRgb3      := -RGB(128,225,225)

      // 3. переменные (aColors items number) от номера позиции в :SetColor( {...}, ... ) из TsBrowse.ch
      :nClrLine   :=  :nRgb1
      :nClr2      :=  :nRgb2                  // #define CLR_PANE     2   // back
      :nClr3      :=  :nWHITE                 // #define CLR_HEADF    3   // header text
      :nClr4      := {:nGRAY,:nBLACK}         // #define CLR_HEADB    4   // header back
      :nClr5      :=  :nRgb0                  // #define CLR_FOCUSF   5   // focused text
      :nClr6_1    :=  :n_BLUE                 // #define CLR_FOCUSB   6 1 // focused back
      :nClr6_2    :=  :nRgb3                  // #define CLR_FOCUSB   6 2 // focused back
      :nClr9      :=  :nBLUE                  // #define CLR_FOOTF    9   // footer text
      :nClr10     := {:nHGRAY,:nGRAY}         // #define CLR_FOOTB   10   // footer back
      :nClr11     :=  :nRgb0                  // #define CLR_SELEF   11   // focused inactive (or selected) text
      :nClr12_1   :=  :n_BLUE                 // #define CLR_SELEB   12 1 // focused inactive (or selected) back
      :nClr12_2   :=  :nRgb3                  // #define CLR_SELEB   12 2 // focused inactive (or selected) back

   END WITH

   WITH OBJECT oBrw
      O := :Cargo
      :nClrLine := O:nClrLine   // создать в контейнере свои переменные с именами
      :SetColor( { 2}, { O:nClr2  } )  // 2 , фона в ячейках таблицы
      :SetColor( { 5}, { O:nClr5  } )  // 5 , текста курсора, текст в ячейках с фокусом
      :SetColor( { 6}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr6_1 , c:nClr6_2  ) } } )  // 6 , фона курсора
      :SetColor( {11}, { O:nClr11 } )  // 11, текста неактивного курсора (selected cell no focused)
      :SetColor( {12}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr12_1, c:nClr12_2 ) } } )  // 12, фона неактивного курсора (selected cell no focused)
      :Setcolor( { 3}, { O:nClr3  } )    // 3 , текста шапки таблицы
      :SetColor( { 4}, { O:nClr4  } )    // 4 , фона шапка таблицы   // !!! тут лишний блок кода, массива достаточно
      :SetColor( { 9}, { O:nClr9  } )    // 9 , текста подвала таблицы
      :SetColor( {10}, { O:nClr10 } )    // 10, фона подвала таблицы // !!! тут лишний блок кода, массива достаточно
   END WITH

RETURN Nil


STATIC FUNCTION myColorTsbElect( oBrw )
   LOCAL cFld, oCol, cNam

   // цвет фона для списка колонок
   FOR EACH cNam IN { "ITOGPRCNT", "PERCENT1", "PERCENT2", "NOT_FIELD" }
       oCol := oBrw:GetColumn(cNam)    // объект колонки по cNam из списка колонок
       cFld := oCol:cField             // имя поля в объекте колонки
       IF oCol:cName == cNam
          oCol:nClrBack     := { |a,n,b| myTsbColorBack(a,n,b)   }  // цвет фона в ячейках таблицы
          oCol:nClrFootBack := { |n,b  | myTsbColorBackFoot(n,b) }  // цвет фона подвала таблицы
          oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }  // цвет фона шапка таблицы
          // Это историческая неточность (параметры надо было {|b,n,a| ... } )
          // для блока кода подвала - передается два параметра
          // для строки(ячеек) - передается три параметра
       ENDIF
   NEXT

   // цвет фона в ячейках таблицы для колонки COL16
   oBrw:GetColumn("COL16"):nClrBack     := { |a,n,b| myTsbColorBack16(a,n,b) }

   // цвет фона шапки таблицы для добавочного списка колонок
   FOR EACH cFld IN { "ITOGPRIX", "ITOGNACH", "ITOGDOLG", "M1DOLG", "M2DOLG", "COL16" }
       oCol              := oBrw:GetColumn(cFld)
       oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }
   NEXT

   // цвет текста для всех ячеек колонок типа "N"
   AEval(oBrw:aColumns, {|oCol| oCol:nClrFore := { |nr,nc,ob| myTsbColorFore(nr,nc,ob) } } )

RETURN Nil


STATIC FUNCTION myTsbColorBack( nAt, nCol, oBrw )  // это ячейки
   LOCAL nVal, n2N, nColor, nRzv := nAt
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   nVal := oBrw:GetValue(nCol)
   n2N  := oBrw:GetValue("NN")   // первая колонка NN
   IF HB_ISCHAR(n2N)  ; n2N  := Val(n2N)
   ENDIF
   IF HB_ISCHAR(nVal) ; nVal := Val(nVal)
   ENDIF

   //? procname(), procname(1), procname(2), nAt, nCol, oBrw, n2N, nVal
   //? "Y,X =",nAt, nCol, "NN =", n2N, VALTYPE(n2N), VALTYPE(nVal), "nVal =", nVal

   IF VALTYPE(n2N) != "N"                  // обработка ошибочной ситуации
      nColor := O:nBLACK  // CLR_BLACK

   ELSEIF VALTYPE(nVal) != "N"             // обработка ошибочной ситуации
      nColor := O:nBLACK  // CLR_BLACK

   ELSEIF n2N == 0
      // значит это разделительная строка между адресами
      nColor := O:nClr2

   ELSE
      IF nVal < 51
         nColor := O:nHRED   // CLR_HRED
      ELSEIF nVal >= 51 .AND. nVal <= 76
         nColor := O:nHBLUE2 // CLR_HBLUE
      ELSEIF nVal >= 76 .AND. nVal <= 91
         nColor := O:nYELLOW // CLR_YELLOW
      ELSEIF nVal > 91
         nColor := O:nGREEN // CLR_GREEN
      ELSE
         nColor := O:nWHITE // CLR_WHITE
      ENDIF
   ENDIF
   //?? "|ret =", nColor
RETURN nColor


STATIC FUNCTION myTsbColorFore( nAt, nCol, oBrw )
   LOCAL nVal, nColor, nRzv := nAt
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   nVal := oBrw:GetValue(nCol)
   IF HB_ISCHAR(nVal) ; nVal := Val(nVal)
   ENDIF

   IF VALTYPE(nVal) = "N"

      IF nVal < 0
         nColor := O:nHRED   // CLR_HRED
      ELSE
         nColor := O:nBLACK
      ENDIF

   ELSE
      nColor := O:nBLACK    // CLR_BLACK
   ENDIF

RETURN nColor


STATIC FUNCTION myTsbColorBack16( nAt, nCol, oBrw )  // это ячейки колонки 16
   LOCAL nVal, n2N, nColor, nRzv := nAt
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   nVal := oBrw:GetValue(nCol)
   n2N  := oBrw:GetValue("NN")   // первая колонка NN
   IF HB_ISCHAR(n2N)  ; n2N  := Val(n2N)
   ENDIF
   IF HB_ISCHAR(nVal) ; nVal := Val(nVal)
   ENDIF

   IF VALTYPE(n2N) != "N"                  // обработка ошибочной ситуации
      nColor := O:nBLACK  // CLR_BLACK
   ELSEIF VALTYPE(nVal) != "N"             // обработка ошибочной ситуации
      nColor := O:nBLACK  // CLR_BLACK
   ELSEIF n2N == 0
      // значит это разделительная строка между адресами
      nColor := O:nClr2
   ELSE
      IF nVal < 0
         nColor := O:nORANGE   // CLR_ORANGE
      ELSE
         nColor := O:nGREEN2   // RGB(0,255,0)
      ENDIF
   ENDIF

RETURN nColor


STATIC FUNCTION myTsbColorBackFoot( nCol, oBrw )
   LOCAL O := oBrw:Cargo     // использовать из контейнера свои переменные
   LOCAL nVal, nColor := O:nWHITE

   // ? "ColorBackFoot(nCol=",nCol, pCount()
   nVal := VAL( cValToChar(oBrw:aColumns[nCol]:cFooting) )

   //? "nCol =", nCol, VALTYPE(nVal), "nVal =", nVal
   IF VALTYPE(nVal) != "N"             // обработка ошибочной ситуации
      nColor := O:nBLACK  // CLR_BLACK

   ELSEIF nVal < 51
      nColor := O:nHRED   // CLR_HRED

   ELSEIF nVal >= 51 .AND. nVal <= 76
      nColor := O:nHBLUE2 // CLR_HBLUE

   ELSEIF nVal >= 76 .AND. nVal <= 91
      nColor := O:nYELLOW // CLR_YELLOW

   ELSEIF nVal > 91
      nColor := O:nGREEN // CLR_GREEN

   ELSE

      nColor := O:nWHITE // CLR_WHITE
   ENDIF
   //?? "|ret =", nColor
RETURN nColor


STATIC FUNCTION myTsbColorBackHead( nCol, oBrw )
   LOCAL O := oBrw:Cargo             // использовать из контейнера свои переменные
   LOCAL nVal, cName, nColor := O:nBLACK

   nVal  := VAL( oBrw:aColumns[nCol]:cHeading )
   cName := oBrw:aColumns[nCol]:cName

   IF cName == "COL16"
      nColor := O:nORANGE
   ELSE
      nColor := O:nRED
   ENDIF

RETURN nColor


STATIC FUNCTION mySumTsb( oBrw )
   LOCAL nOldRec, nOldArea := select()
   LOCAL aCol := oBrw:aColumns
   LOCAL aSum := array( Len(aCol) )
   LOCAL nCnt := 0, oCol, nCol, cCol, nSum
   LOCAL oFld := oBrw:GetColumn("CITY")
   LOCAL cFld := oFld:cField
   LOCAL nVal1, nVal2, cVal, aRec, xVal

   IF oBrw:lIsDbf                           // это для tsbrowse-Dbf

      dbSelectArea( oBrw:cAlias )
      nOldRec := RecNo()
      nVal1   := oBrw:nColumn("ORDKEYNO")   // номер колонки нумерации для dbf

      // SET SCOPE  TO ...
      SET FILTER TO &( "! empty("+cFld+")" )
      GO TOP
      DO WHILE !EOF()
         nCnt ++
         FOR nCol := oBrw:nColumn("NN") TO Len(aCol)
             oCol := aCol[ nCol ]
             cCol := oCol:cField
             IF ! '(' $ cCol .and. oCol:cFieldTyp == 'N'
                nSum := iif( aSum[ nCol ] == NIL, 0, aSum[ nCol ] )
                nSum += FieldGet( FieldPos(cCol) )
                aSum[ nCol ] := nSum
             ENDIF
         NEXT
         SKIP
      ENDDO
      // SET SCOPE  TO
      SET FILTER TO
      GO TOP
      GOTO nOldRec
      dbSelectArea( nOldArea )

   ELSEIF oBrw:lIsArr                       // это для tsbrowse-Array

      WITH OBJECT oBrw
      nVal1 := :nColumn("ARRAYNO")          // номер колонки нумерации для Array
      FOR EACH aRec IN :aArray
          IF empty( aRec[ ( :nColumn("CITY") - nVal1 ) ] ) ; LOOP               // SET FILTER TO ! empty(CITY)
          ENDIF
          nCnt ++
          FOR nCol := ( :nColumn("NN") - nVal1 ) TO Len(aRec)
              xVal := aRec[ nCol ]                                              // real value
              IF ! HB_ISNUMERIC(xVal) ; LOOP
              ENDIF
              nSum := iif( aSum[ nCol + nVal1 ] == NIL, 0, aSum[ nCol + nVal1 ] )
              aSum[ nCol + nVal1 ] := nSum + xVal
          NEXT
      NEXT
      END WITH

   ENDIF

   FOR nCol := 1 TO Len(aSum)
       nSum := aSum[ nCol ]
       IF nSum == NIL ; LOOP
       ENDIF
       oCol := aCol[ nCol ]
       oCol:cFooting := iif( empty(nSum), "", hb_ntos(nSum) )
   NEXT

   oBrw:GetColumn("NN"       ):cFooting  :=  HB_NToS(nCnt)  // кол-во строк
   oBrw:GetColumn("CITY"     ):cFooting  :=  'Total records:' + HB_NToS(nCnt)
   oBrw:GetColumn("CITY"     ):nFAlign   :=  DT_CENTER

   // расчёт процентов в подвале
   nVal1 := VAL( cValToChar(oBrw:GetColumn("ITOGPRIX"):cFooting) )
   nVal2 := VAL( cValToChar(oBrw:GetColumn("ITOGNACH"):cFooting) )
   //nVal1 := aSum[ oBrw:nColumn("ITOGPRIX") ] // можно и так
   //nVal2 := aSum[ oBrw:nColumn("ITOGNACH") ] // можно и так
   IF empty(nVal2) ; cVal := "Error 0/ ?"
   ELSE            ; cVal := hb_ntos( int( nVal1 / nVal2 * 100 ) )
   ENDIF
   oBrw:GetColumn("ITOGPRCNT"):cFooting  := cVal

   nVal1 := VAL( cValToChar(oBrw:GetColumn("M1PRIX"):cFooting) )
   nVal2 := VAL( cValToChar(oBrw:GetColumn("m1NACH"):cFooting) )
   IF empty(nVal2) ; cVal := "Error 0/ ?"
   ELSE            ; cVal := hb_ntos( int( nVal1 / nVal2 * 100 ) )
   ENDIF
   oBrw:GetColumn("PERCENT1"):cFooting  := cVal

   nVal1 := VAL( cValToChar(oBrw:GetColumn("M2PRIX"):cFooting) )
   nVal2 := VAL( cValToChar(oBrw:GetColumn("m2NACH"):cFooting) )
   IF empty(nVal2) ; cVal := "Error 0/ ?"
   ELSE            ; cVal := hb_ntos( int( nVal1 / nVal2 * 100 ) )
   ENDIF
   oBrw:GetColumn("PERCENT2"):cFooting  :=  cVal

   // ненужные итоговые суммы по годам в подвале
   oBrw:GetColumn("YEAR1"):cFooting :=  " "
   oBrw:GetColumn("YEAR2"):cFooting :=  " "

   oBrw:DrawFooters()

RETURN Nil


STATIC FUNCTION myDelColumnTsb( oBrw )
   LOCAL nCol, aHideCol := {}
   LOCAL aCol := oBrw:aColumns
   LOCAL cDelCol, oCol, cCol, cType

   // список удаляемых колонок
   cDelCol := ",KCITY,KSTREET,KORPUS,STROEN,NDOG,NNDOG,"

   // уберем колонки
   FOR nCol := 1 TO Len(aCol)

      oCol := aCol[ nCol ]

      IF oBrw:lIsDbf  // для dbf
         cCol  := oCol:cField
         cType := oCol:cFieldTyp
      ELSE
         cType := oCol:cDataType
      ENDIF
      cCol := oCol:cName // для всех вариантов
      IF '20' $ cCol .and. cType  == 'N'
         // уберем колонки 20xx годом
         AADD( aHideCol , nCol )
      ELSEIF 'DATE' $ cCol
         // уберем колонки DATExxx
         AADD( aHideCol , nCol )
      ELSEIF ","+cCol+"," $ cDelCol
         AADD( aHideCol , nCol )
      ENDIF
   NEXT

   oBrw:HideColumns( aHideCol ,.t.)   // скрыть колонки

RETURN Nil


STATIC FUNCTION mySupHdTsb( oBrw, aSupHd )
   LOCAL nMax := 0, nI, oCol, nFirst, nLast, aSup

   WITH OBJECT oBrw
   FOR nI := 1 TO Len( :aColumns )
      oCol := :aColumns[ nI ]
      IF oCol:lVisible ; nMax := Max( nMax, nI )
      ENDIF
   NEXT
   IF nMax > 0
     FOR nI := nMax TO 1 STEP -1
        oCol := :aColumns[ nI ]
        IF ! oCol:lVisible ; :DelColumn( nI )
        ENDIF
     NEXT
   ENDIF

   // суперхидер
   nFirst := nLast := 0
   FOR EACH aSup IN aSupHd
      IF   empty(aSup[1])       ; nFirst := nLast + 1
      ELSEIF     aSup[1] == "+" ; nFirst := nLast + 1
      ELSEIF val(aSup[1]) >  0  ; nFirst :=      val(aSup[1])
      ELSE                      ; nFirst := :nColumn(aSup[1])
      ENDIF
      IF         aSup[2] == "*" ; nLast := :nColCount()
      ELSEIF val(aSup[2]) >  0  ; nLast :=      val(aSup[2])
      ELSE                      ; nLast := :nColumn(aSup[2])
      ENDIF
      IF nFirst > 0 .and. nLast > 0 .and. nLast >= nFirst
         :AddSuperHead( nFirst, nLast, aSup[3] )
      ENDIF
   NEXT
   // задать цвета суперхидеру
   :SetColor( {16}, { { || { CLR_HGRAY, CLR_GRAY   } } } ) // 16, фона спецхидер
   :SetColor( {17}, { CLR_RED                          } ) // 17, текста спецхидер

   END WIDTH

RETURN NIL


// ENUMERATOR по порядку сделаем свой
STATIC FUNCTION myEnumTsb( oBrw )
   LOCAL oCol, nCnt := 0

   FOR EACH oCol IN oBrw:aColumns
       oCol:cSpcHeading := NIL
       IF oCol:lVisible
          oCol:cSpcHeading := hb_ntos( ++nCnt )
       ENDIF
   NEXT

RETURN NIL


STATIC FUNCTION myF8( oBrw )
   LOCAL nAt, nCol, oCol, oCel, hFnt, aSup, cVal, nFrom, nTo, cStr

   CursorWait()
   WaitWindow( "Wait, reading a table", .T. )

   _SetGetLogFile("_MsgLog.txt")
   fErase( _SetGetLogFile() )

   WITH OBJECT oBrw
   :GoTop()
   :lDrawLine := .F.
? '======= общее решение для массива и для dbf =========== F8'
? '"быстрый" доступ к данным тсб через объекты класса TSBcell'
? 'oCol:oCellHead, oCol:oCellEnum, oCol:oCell, oCol:oCellFoot'
? ':lDrawLine =', :lDrawLine, "Log file =", _SetGetLogFile()
   :GoTop()
?
? "~", "DrawSuper/суперхидер таблицы"
   aSup := :DrawSuper( .F. )
   FOR EACH oCel IN aSup
       nCol  := hb_enumindex(oCel)
       nFrom := -1
       nTo := -1
       IF nCol <= Len(oBrw:aSuperHead)
          nFrom := oBrw:aSuperHead[ nCol ][1]
          nTo   := oBrw:aSuperHead[ nCol ][2]
       ENDIF
       hFnt := oCel:hFont
       ? "~"+str(oCel:nCell,3), oCel:nDrawType,;
       TR0(oCel:nCol,7), TR0(oCel:nWidth,7), oCel:nAlign, ;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),15),;
       oCel:cValue, nFrom, nTo
   NEXT
?
   :DrawHeaders()
? "@", "DrawHeader/шапка таблицы"
   FOR nCol := 1 TO :nColCount()
       oCol := :aColumns[ nCol ]
       oCel := oCol:oCellHead
       hFnt := oCel:hFont
       cVal := oCel:cValue
       IF oCel:lMultiLine
          cVal := StrTran(cVal, CRLF, " ")
       ENDIF
? "@"+str(nCol,3), oCel:nDrawType, oCol:lVisible,;
       TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign, ;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),15)
       ?? cVal
   NEXT
?
   IF :lDrawSpecHd
? "=", "DrawSpecHeader/спецхидер таблицы/нумератор колонок"
      FOR nCol := 1 TO :nColCount()
          oCol := :aColumns[ nCol ]
          oCel := oCol:oCellEnum
          hFnt :=oCel:hFont
? "="+str(nCol,3), oCel:nDrawType, oCol:lVisible,;
          TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign, ;
          TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),15),;
          TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),15),;
          TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),15),;
          oCel:cValue
      NEXT
?
   ENDIF

? ".", "DrawLine/сама таблица"
   FOR nAt := 1 TO :nLen
      :DrawLine()
      FOR nCol := 1 TO :nColCount()
         oCol := :aColumns[ nCol ]
         oCel := oCol:oCell
         hFnt := oCel:hFont
? "."+TR0(nAt,4)+TR0(nCol,3), oCel:nDrawType, oCol:lVisible, ;
           TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign,;
           TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),15), ;
           TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),15), ;
           TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),15), ;
           oCol:cFieldTyp, Valtype(oCel:uValue), Valtype(oCel:cValue),;
           oCel:uValue, oCel:cValue, oCol:cPicture
       NEXT
?
       :GoDown()
   NEXT
?
? "#", "DrawFooter/подвал таблицы"
   FOR nCol := 1 TO :nColCount()
       oCol := :aColumns[ nCol ]
       oCel := oCol:oCellFoot
       hFnt :=oCel:hFont
? "#"+str(nCol,3), oCel:nDrawType, oCol:lVisible,;
       TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign, ;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),15),;
       TR0(HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),15),;
       oCel:cValue
   NEXT
?
   :lDrawLine := .T.
   :Reset()
    AEval( :aColumns ,{|oc| oc:oCell := NIL, ;
                            oc:oCellHead := NIL, ;
                            oc:oCellEnum := NIL, ;
                            oc:oCellFoot := NIL } )
   END WITH

   cStr := hb_memoread(_SetGetLogFile())
   DO EVENTS

   WaitWindow()
   CursorArrow()

   AlertInfo( cStr, "INFO", , 0, , , ;
              {|ao,cn|
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Width  := System.ClientWidth
                  This.(cn).Width := This.ClientWidth - This.(cn).Col - 20
                  This.Center
               EndIf
               Return Nil
              } )

RETURN Nil

STATIC FUNCTION myF9( oBrw )
   LOCAL nCol, oCol, oCel, aSup
   LOCAL nRec := RecNo(), hFnt

   IF ! oBrw:lIsDbf
      AlertStop( "Switch to Mode tbrowse-dbf !", "INFO" )
      RETURN Nil
   ENDIF

   CursorWait()
   WaitWindow( "Wait, reading a table", .T. )

   _SetGetLogFile("_MsgLog.txt")
   fErase( _SetGetLogFile() )

   WITH OBJECT oBrw
? '==== решение только для dbf =========================== F9'
? '"быстрый" доступ к данным тсб через объекты класса TSBcell'
? 'oCol:oCellHead, oCol:oCellEnum, oCol:oCell, oCol:oCellFoot'
   dbGoTop()
? ':lDrawLine =', :lDrawLine, "Log file =", _SetGetLogFile()
?
? "~", "DrawSuper/суперхидер таблицы"
   aSup := :DrawSuper( .F. )
   FOR EACH oCel IN aSup
       hFnt :=oCel:hFont
? "~"+str(oCel:nCell,3), oCel:nDrawType, TR0(oCel:nCol,7), TR0(oCel:nWidth,7), oCel:nAlign, ;
       HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),;
       oCel:cValue
   NEXT
?
   :DrawHeaders( , .F.)
? "@", "DrawHeader/шапка таблицы"
   FOR nCol := 1 TO :nColCount()
       oCol := :aColumns[ nCol ]
       oCel := oCol:oCellHead
       hFnt := oCel:hFont
? "@"+str(nCol,3), oCel:nDrawType, oCol:lVisible ,;
       TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign,;
       HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),;
       oCel:cValue
   NEXT
?
   IF :lDrawSpecHd
? "=", "DrawSpecHeader/спецхидер таблицы/нумератор колонок"
      FOR nCol := 1 TO :nColCount()
          oCol := :aColumns[ nCol ]
          oCel := oCol:oCellEnum
          hFnt :=oCel:hFont
? "="+str(nCol,3), oCel:nDrawType, oCol:lVisible,;
          TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign, ;
          HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),;
          oCel:cValue
      NEXT
?
   ENDIF
? ".", "DrawLine/сама таблица"
   DO WHILE ! EOF()
      :DrawLine( , .F.)
      FOR nCol := 1 TO :nColCount()
          oCol := :aColumns[ nCol ]
          oCel := oCol:oCell
          hFnt :=oCel:hFont
? "."+TR0(RecNo(),4)+TR0(nCol,3), oCel:nDrawType, oCol:lVisible ,;
          TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign,;
          HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),;
          oCol:cFieldTyp, Valtype(oCel:uValue), Valtype(oCel:cValue),;
          oCel:uValue, oCel:cValue, oCol:cPicture
      NEXT
?
      dbSkip()
   ENDDO

? "#", "DrawFooter/подвал таблицы"
   FOR nCol := 1 TO :nColCount()
       oCol := :aColumns[ nCol ]
       oCel := oCol:oCellFoot
       hFnt :=oCel:hFont
? "#"+str(nCol,3), oCel:nDrawType, oCol:lVisible ,;
       TR0(oCel:nCol,7), TR0(oCol:cName,10), oCel:nAlign, ;
       HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
       HB_ValToExp(HMG_n2RGB(oCel:nClrTo  )),;
       oCel:cValue
   NEXT
?
   dbGoto(nRec)
   AEval( :aColumns ,{|oc| oc:oCell := NIL, ;
                           oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, ;
                           oc:oCellFoot := NIL } )
   END WITH

   WaitWindow()
   CursorArrow()

   AlertInfo( hb_memoread(_SetGetLogFile()), "INFO", , 0, , , ;
              {|ao,cn|
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Width  := System.ClientWidth
                  This.(cn).Row := 10
                  This.(cn).Col := 10
                  This.(cn).Width  := This.ClientWidth - 10 * 2
                  This.(cn).Height := This.(cn).Height + 10 * 2 + 10
                  This.Center
               EndIf
               Return Nil
              } , .T. )     // .T. - lNoPlay

RETURN Nil


STATIC FUNCTION myF5( oBrw )
   LOCAL nCol, oCol, oCel, hFnt

   _SetGetLogFile("_MsgLog.txt")
   fErase( _SetGetLogFile() )

// !!!  не надо эти строки, если доступ только к Header, SpcHd, Footer
// !!!  oBrw:lDrawLine := .F.
// !!!  oBrw:GoTop()
? '======= общее решение для массива и для dbf =========== F5'
? '"быстрый" доступ к данным тсб через объекты класса TSBcell'
? 'Доступ только к данным Footer'
? ':lDrawLine =', oBrw:lDrawLine, "Log file =", _SetGetLogFile()
?
? "#", "DrawFooter/подвал таблицы"
   // !!! если доступ только к Header, SpcHd, Footer достаточно перменной параметра
   oBrw:DrawHeaders( , .F.)           // он создает для Header, SpcHd, Footer
   FOR nCol := 1 TO oBrw:nColCount()
       oCol := oBrw:aColumns[ nCol ]
       oCel := oCol:oCellFoot
       hFnt := oCel:hFont
? "#"+str(nCol,3), oCel:nDrawType, oCol:lVisible, TR0(oCel:nCol,7),;
          TR0(oCol:cName,12), oCel:nAlign ,;
          HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
          oCel:cValue
   NEXT
?
   // !!!  не надо эти строки, если доступ только к Header, SpcHd, Footer
   // !!!   oBrw:lDrawLine := .T.
   // !!!   oBrw:Reset()
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, ;
                            oc:oCellHead := NIL, ;
                            oc:oCellEnum := NIL, ;
                            oc:oCellFoot := NIL } )

   AlertInfo( hb_memoread(_SetGetLogFile()), "INFO", , , , , ;
              {|ao,cn|
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Width  := test.ClientWidth  * 0.95
                  This.(cn).Width := This.ClientWidth - This.(cn).Col - 20
                  This.Center
               EndIf
               Return Nil
              } )

RETURN Nil


STATIC FUNCTION myF4( oBrw )           // Доступ только к данным ENUMERATOR
   LOCAL nCol, oCol, oCel, hFnt

   _SetGetLogFile("_MsgLog.txt")
   fErase( _SetGetLogFile() )

? '======= общее решение для массива и для dbf =========== F4'
? '"быстрый" доступ к данным тсб через объекты класса TSBcell'
? 'Вывод только ENUMERATOR'
? ':lDrawLine =', oBrw:lDrawLine, "Log file =", _SetGetLogFile()
? "~~~~~1,2,3,4,5,...", "DrawSpecHeader - show/hide:", oBrw:lDrawSpecHd

   oBrw:DrawHeaders( , .F.)        // он создает для Header, SpcHd, Footer, Enum
   IF oBrw:lDrawSpecHd
      FOR nCol := 1 TO oBrw:nColCount()
          oCol := oBrw:aColumns[ nCol ]
          oCel := oCol:oCellEnum
          hFnt := oCel:hFont
? "="+str(nCol,3), oCel:nDrawType, oCol:lVisible, TR0(oCel:nCol,7),;
          TR0(oCol:cName,14), oCel:nAlign,;
          HB_ValToExp(HMG_n2RGB(oCel:nClrFore)),;
          HB_ValToExp(HMG_n2RGB(oCel:nClrBack)),;
          " ", oCel:cValue
      NEXT
   ENDIF
? ; ? "-[End]-"
   // освобождаем переменные (память), можно не делать освобится после работы тсб
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, ;
                            oc:oCellHead := NIL, ;
                            oc:oCellEnum := NIL, ;
                            oc:oCellFoot := NIL } )

   AlertInfo( hb_memoread(_SetGetLogFile()), "INFO", , , , , ;
              {|ao,cn|
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Width  := test.ClientWidth  * 0.95
                  This.(cn).Width := This.ClientWidth - This.(cn).Col - 20
                  This.Center
               EndIf
               Return Nil
              } )

RETURN Nil


STATIC FUNCTION myF3( oBrw, nCol )
   LOCAL oCol
   LOCAL cValH, cValC, cValF
   LOCAL oHead, oFoot, oCell
   LOCAL nForeH, nBackH, nClrToH
   LOCAL nForeC, nBackC, nClrToC
   LOCAL nForeF, nBackF, nClrToF
   LOCAL aForeH, aBackH, aClrToH
   LOCAL aForeC, aBackC, aClrToC
   LOCAL aForeF, aBackF, aClrToF
   LOCAL cForm := "TestTsbCell"
   LOCAL nGaps := 20, nY, nX, cN
   LOCAL nLen  := 50, w, h, cName

   Default nCol := oBrw:nCell

   WITH OBJECT oBrw

   :DrawHeaders(, .F.) ; :DrawLine(, .F.)

    oCol  := :aColumns[ nCol ]
    oHead := oCol:oCellHead
    oCell := oCol:oCell
    oFoot := oCol:oCellFoot

    cName   := oCol:cName
    cValH   := oHead:cValue
    cValC   := oCell:cValue
    cValF   := oFoot:cValue
    nForeH  := oHead:nClrFore
    nBackH  := oHead:nClrBack
    nClrToH := oHead:nClrTo
    nForeC  := oCell:nClrFore
    nBackC  := oCell:nClrBack
    nClrToC := oCell:nClrTo
    nForeF  := oFoot:nClrFore
    nBackF  := oFoot:nClrBack
    nClrToF := oFoot:nClrTo

    aForeH  := HMG_n2RGB( nForeH  )
    aBackH  := HMG_n2RGB( nBackH  )
    aClrToH := HMG_n2RGB( nClrToH )
    aForeC  := HMG_n2RGB( nForeC  )
    aBackC  := HMG_n2RGB( nBackC  )
    aClrToC := HMG_n2RGB( nClrToC )
    aForeF  := HMG_n2RGB( nForeF  )
    aBackF  := HMG_n2RGB( nBackF  )
    aClrToF := HMG_n2RGB( nClrToF )

    w := nGaps + GetFontWidth( _HMG_FontName( oCell:hFont ), 50) + nGaps
    h := nGaps + oHead:nHeightCell +       ;
         nGaps + oCell:nHeightCell +       ;
         nGaps + oFoot:nHeightCell + nGaps

    nY := nX := nGaps

   END WITH

   DEFINE WINDOW &cForm  CLIENTAREA w,h  TITLE oBrw:cParentWnd+'.'+oBrw:cControlName+'.'+oCol:cName+' ( '+hb_ntos(nCol)+' )' ;
                                         MODAL  NOSIZE
      cN := cName+'_Head'
      @ nY, nX LABEL &cN VALUE cValH WIDTH oCol:nWidth HEIGHT oHead:nHeightCell FONT _HMG_FontName( oHead:hFont ) ;
               BACKCOLOR aClrToH FONTCOLOR aForeH   BORDER    ON INIT {|| nY += This.Height + nGaps }

      cN := cName+'_Cell'
      @ nY, nX LABEL &cN VALUE cValC WIDTH oCol:nWidth HEIGHT oCell:nHeightCell FONT _HMG_FontName( oCell:hFont ) ;
               BACKCOLOR aClrToC  FONTCOLOR aForeC  BORDER    ON INIT {|| nY += This.Height + nGaps }

      cN := cName+'_Foot'
      @ nY, nX LABEL &cN VALUE cValF WIDTH oCol:nWidth HEIGHT oFoot:nHeightCell FONT _HMG_FontName( oFoot:hFont ) ;
               BACKCOLOR aClrToF  FONTCOLOR aForeF  BORDER    ON INIT {|| nY += This.Height + nGaps }

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN Nil


STATIC FUNCTION myF2( oBrw )
   LOCAL nI, nJ, aRet, aLine, cStr

   _SetGetLogFile("_F2Log.txt")
   fErase( _SetGetLogFile() )

? '======= Функция чтения всех данных таблицы =========== F2'
? "массив фонт/цвет_текст/цвет_фона/значение/тип/формат/имя_поля ячеек таблицы" ; ?

   // массив фонт/цвет_текст/цвет_фона/значение/тип/формат/имя_поля ячеек таблицы
   aRet := myGetTsbCell(oBrw)  // в модуле TsbExport.prg
   FOR nI := 1 TO LEN(aRet)
      ? "Recno:",nI
      aLine := aRet[nI]
      FOR nJ := 1 TO LEN(aLine)
        ? "Col:" + HB_NtoS(nJ) + " [", aLine[nJ,7], aLine[nJ,4] , aLine[nJ,5] , aLine[nJ,6], "]"
      NEXT
      DO EVENTS
      ?
   NEXT

   cStr := hb_memoread(_SetGetLogFile())
   DO EVENTS

   AlertInfo( cStr, "INFO", , 0, , , ;
              {|ao,cn|
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Width  := test.ClientWidth  * 0.8
                  This.(cn).Width := This.ClientWidth - This.(cn).Col - 20
                  This.Center
               EndIf
               Return Nil
              } )

   fErase( _SetGetLogFile() )
   _SetGetLogFile("_MsgLog.txt")

RETURN Nil

STATIC FUNCTION myCol3132( oBrw )
   LOCAL cFrm1, cFrm2, cAls, nRecno

   IF ! oBrw:lIsDbf
      AlertStop( "Switch to Mode tbrowse-dbf !", "INFO" )
      RETURN Nil
   ENDIF

   cFrm1  := '=RC[-12] - RC[-11]'
   cFrm2  := '=RC[-11] * 1 / 100'
   cAls   := oBrw:cAlias
   nRecno := (oBrw:cAlias)->( RecNo() )

   SELECT(cAls)
   GOTO TOP
   DO WHILE !EOF()
      IF (cAls)->NN == 0
         (cAls)->F1 := Space(10)
         (cAls)->F2 := Space(10)
      ELSE
         (cAls)->F1 := cFrm1
         (cAls)->F2 := cFrm2
      ENDIF

      DO EVENTS
      SKIP
   ENDDO

   AlertInfo( "Filled in columns 31 and 32 with Excel formulas !", "INFO" )

   oBrw:Refresh(.T.)
   oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

RETURN Nil


STATIC FUNCTION myRestart(nI)
   LOCAL aParam[1], cParam

   test.Hide
   WaitWindow( 'Restart the program ...', .T. )
   INKEYGUI(100)

   IF nI == 1
     aParam[1] := .F. // тсб-Dbf
   ELSEIF nI == 2
     aParam[1] := .T. // тсб-SetArrayTo
   ENDIF

   cParam := '"' + HB_ValToExp(aParam) + '"'
   ? "  ."
   ? "  ShellExecute( , 'open', '" + Application.ExeName + "' , " + cParam + " , 3 )"
   ? "  ."
   ShellExecute( , 'open', Application.ExeName, cParam, , 2 )

   ReleaseAllWindows()  // закрыть программу

RETURN NIL

* ======================================================================
STATIC FUNCTION CreateDatos( lArray )
   LOCAL aDatos, aHead, aFSize, aFoot, aPict, aAlign, aName, aField
   LOCAL aFAlign, aKfcWidth, aFldCenter, cAlias, aVal, cPicture, n1, n2
   LOCAL i, k, cSH1, cSH2, cSH3, cSH4, cSH5, cSH6, cSH7, cSH8, aSupHd
   DEFAULT lArray := .T.

   IF hb_FileExists( "Total.dbf" )
      USE Total ALIAS "TOTAL" /*CODEPAGE "RU866"*/ NEW
      cAlias := ALIAS()
      k      := fCount()
   ELSE
      MsgStop('File not found !' + CRLF + 'Total.dbf', "ERROR")
      ReleaseAllWindows()  // закрыть программу
   ENDIF

   aHead        := array(k)
   aFoot        := array(k)
   aPict        := array(k)
   aName        := array(k)
   aAlign       := array(k)
   aField       := array(k)
   aFSize       := array(k)
   aFAlign      := array(k)    // Footer align

   FOR i := 1 TO k
      aHead [ i ] := myHeadName( FieldName( i ) )
      aFoot [ i ] := hb_ntos( i )
      aName [ i ] := FieldName( i ) // 'FLD'+hb_ntos( i ) - не надо так
      aField[ i ] := FieldName( i )
      aFSize[ i ] := myHeadSize( aHead[ i ] , i , FieldType( i ) )
      aAlign[ i ] := DT_LEFT
      n1          := FieldLen( i )
      n2          := FieldDec( i )
      cPicture    := REPL("9", n1) + IIF( n2 > 0, "."+REPL("9", n2), "" )
      switch FieldType( i )
         case 'M' ; aAlign[ i ] := DT_LEFT   ; aPict[i] := REPL("X",250)         ; exit
         case 'C' ; aAlign[ i ] := DT_LEFT   ; aPict[i] := "@K " + REPL("X",n1)  ; exit
         case 'N' ; aAlign[ i ] := DT_RIGHT  ; aPict[i] := cPicture              ; exit
         case 'D' ; aAlign[ i ] := DT_CENTER ; aPict[i] := "99.99.99"            ; exit
         case 'L' ; aAlign[ i ] := DT_CENTER ; aPict[i] := "X"                   ; exit
         case '=' ; aAlign[ i ] := DT_CENTER ; aPict[i] := REPL("X",21)          ; exit
         case '@' ; aAlign[ i ] := DT_CENTER ; aPict[i] := REPL("X",21)          ; exit
         case 'T' ; aAlign[ i ] := DT_CENTER ; aPict[i] := REPL("X",21)          ; exit
         case '^' ; aAlign[ i ] := DT_RIGHT  ; aPict[i] := REPL("9", 8)          ; exit
         case '+' ; aAlign[ i ] := DT_RIGHT  ; aPict[i] := REPL("9", 8)          ; exit
      end switch
      IF ","+aName[ i ]+"," $ ",NN,DOM,PODEZD,YEAR1,YEAR2,MONTH1,MONTH2,"
         aAlign[ i ] := DT_CENTER
      ENDIF
      aFAlign[ i ] := DT_CENTER
   NEXT
   // суперхидер
   DBGOTO(2)
   cSH1   := "Adres"
   cSH2   := DTOC((cAlias)->DATEFROM) + " - " + DTOC((cAlias)->DATEBY)
   cSH3   := ALLTRIM((cAlias)->MONTH1) + " " + HB_NtoS((cAlias)->YEAR1)
   cSH4   := ALLTRIM((cAlias)->MONTH2) + " " + HB_NtoS((cAlias)->YEAR2)
   cSH5   := "21-15"
   cSH6   := " -o- "
   cSH7   := "New type field"
   cSH8   := "Formula Excel"
   aSupHd := {}
   AADD( aSupHd, { '1' , '1'          , " "      } )
   AADD( aSupHd, { '+' , 'PODEZD'     , cSH1     } )
   AADD( aSupHd, { '+' , 'ITOGPRCNT'  , cSH2     } )
   AADD( aSupHd, { '+' , 'PERCENT1'   , cSH3     } )
   AADD( aSupHd, { '+' , 'PERCENT2'   , cSH4     } )
   AADD( aSupHd, { '+' , 'COL16'      , cSH5     } )
   AADD( aSupHd, { '+' , 'DCALC'      , cSH6     } )
   AADD( aSupHd, { '+' , 'DT'         , cSH7     } )
   AADD( aSupHd, { '+' , 'F2'         , cSH8     } )

   // ширина колонок по именам
   aKfcWidth := { {"DOM"   , 0.7}, ;
                  {"PODEZD", 0.9}, ;
                  {"CITY"  , 0.7}, ;
                  {"STREET", 0.8}, ;
                  {"MONTH1", 0.8}, ;
                  {"MONTH2", 0.7}  ;
                }

   // центрирование текстовых колонок по именам
   aFldCenter :=  {"DOM", "PODEZD", "MONTH1", "MONTH2"}

   // построение таблицы в МиниГуи можно делать
   // двумя вариантами: Tsbrowse-Dbf и Tsbrowse-SetArrayTo()
   // какую таблицу выбрать можно определить в этом массиве
   // aArrаy := ALIAS() и задаем aField := {...} будет в тсб-Dbf
   // aArrаy := aDim и задаем aField := NIL будет в тсб-SetArrayTo

   IF lArray
      // второй вариант
      aField := NIL

      // загрузка базы в массив
      aDatos := {}
      GO TOP
      DO WHILE !EOF()
         aVal := {}
         FOR i := 1 TO k
            AADD( aVal, FIELDGET( i ) )
         NEXT
         AADD( aDatos, aVal )
         DO EVENTS
         SKIP
      ENDDO
      GO TOP

      // можно закрыть базу
      (cAlias)->(dbCloseArea())

   ELSE

     // первый вариант
     aDatos := ALIAS()

   ENDIF

RETURN { aDatos, aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aSupHd, aFAlign, aKfcWidth, aFldCenter }

* ======================================================================
STATIC FUNCTION myHeadName( cName )
   LOCAL nI, a2Dim := {}

   AADD( a2Dim, { "NN"       , "№№                                   " } )
   AADD( a2Dim, { "CITY"     , "City                                 " } )
   AADD( a2Dim, { "STREET"   , "Street                               " } )
   AADD( a2Dim, { "DOM"      , "House;number                         " } )
   AADD( a2Dim, { "PODEZD"   , "Room;number                          " } )
   AADD( a2Dim, { "ITOGPRIX" , "amount of;receipt for;entire period  " } )
   AADD( a2Dim, { "ITOGNACH" , "accrual;amount for;entire period     " } )
   AADD( a2Dim, { "ITOGDOLG" , "total;debt for;entire period         " } )
   AADD( a2Dim, { "ITOGPRCNT", "% ratio;shortfall for; entire period " } )
   AADD( a2Dim, { "MONTH1"   , "month;arrival                        " } )
   AADD( a2Dim, { "YEAR1"    , "year;arrival                         " } )
   AADD( a2Dim, { "M1PRIX"   , "amount;income                        " } )
   AADD( a2Dim, { "M1NACH"   , "amount;accruals                      " } )
   AADD( a2Dim, { "M1DOLG"   , "amount;debt                          " } )
   AADD( a2Dim, { "PERCENT1" , "% ratio;shortage                     " } )
   AADD( a2Dim, { "MONTH2"   , "month;arrival                        " } )
   AADD( a2Dim, { "YEAR2"    , "year;arrival                         " } )
   AADD( a2Dim, { "M2PRIX"   , "amount;income                        " } )
   AADD( a2Dim, { "M2NACH"   , "amount;accruals                      " } )
   AADD( a2Dim, { "M2DOLG"   , "amount;debt                          " } )
   AADD( a2Dim, { "PERCENT2" , "% ratio;shortage                     " } )
   AADD( a2Dim, { "COL16"    , "change;payments for;two months       " } )
   AADD( a2Dim, { "DCALC"    , "Date;calc                            " } )
   AADD( a2Dim, { "LOG"      , "Type;[L]                             " } )
   AADD( a2Dim, { "ID"       , "Type;[+]                             " } )
   AADD( a2Dim, { "TS"       , "Type;[=]                             " } )
   AADD( a2Dim, { "VM"       , "Type;[^]                             " } )
   AADD( a2Dim, { "IM"       , "Type;[@]                             " } )
   AADD( a2Dim, { "DT"       , "Type;[T]                             " } )
   AADD( a2Dim, { "F1"       , "Formula(1);My type;[#]               " } )
   AADD( a2Dim, { "F2"       , "Formula(2);My type;[#]               " } )

   // подстановка заданных полей
   FOR nI := 1 TO LEN(a2Dim)
      IF cName == a2Dim[nI,1]
         cName := ALLTRIM(a2Dim[nI,2])
         cName := ATREPL( ";", cName, CRLF )
         EXIT
      ENDIF
   NEXT

RETURN cName

* ======================================================================
// при работе с dbf надо считать размеры колонок по шапке таблицы
STATIC FUNCTION myHeadSize( cName, nI, cType )
   LOCAL nWCol, aDim, nJ, nWText, nWFld, nLenDbf, cText

   // Т.к. фонты уже регистрированы, то ширину колонки вычислить просто
   nWCol   := 0
   IF CRLF $ cName
       aDim  := HB_ATokens(cName,CRLF,.F.,.F.)
       FOR nJ := 1 TO LEN(aDim)
          nWText := GetFontWidth( "Bold", Len(aDim[nJ]) )
          nWCol  := MAX( nWCol, nWText )
       NEXT
    ELSE
       nWCol := GetFontWidth( "Bold", Len(cName) )
    ENDIF
    // получили max ширину шапки таблицы

    // считаем ширину размера поля базы
    nLenDbf := FIELDLEN(nI)
    IF cType == "C"
       cText := REPL("x", nLenDbf )
    ELSEIF cType == "N" .OR. cType == "^"
       cText := REPL("9", nLenDbf )
    ELSEIF cType == "D"
       cText := "99099099990"
    ELSEIF cType == "T" .OR. cType == "@" .OR. cType == "="
       cText := REPL("9", 22 )
    ELSEIF cType == "L"
       cText := "XXXX"
    ELSEIF cType == "+"
       cText := REPL("9", 8 )
    ELSE
       cText := REPL("0", 6 )
    ENDIF
    nWFld := GetFontWidth( "Bold", Len(cText) )
    IF nWFld > nWCol
       nWCol := nWFld
    ENDIF
    nWCol += 2 // отступ для красоты

RETURN nWCol


FUNCTION TR0( cTxt, nLen, cSim )
   IF HB_ISNUMERIC(cTxt) ; cTxt := hb_ntos(cTxt)
   ENDIF
   Default nLen := Len(cTxt)
   If cSim == Nil; cSim := " "
   EndIf
RETURN PadL(AllTrim(cTxt), nLen, cSim)


FUNCTION MsgAbout()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";" + SHOW_VERSION + ";;"
   cMsg += "Tables in MiniGui can be done in two ways:;"
   cMsg += "1) Tsbrowse-Dbf mode, opening a dbf file;"
   cMsg += "2) Tsbrowse-Array mode, array creation;"
   cMsg += "to change the mode use the menu Mode tbrowse;;"
   cMsg += "(c) 2020 Verchenko Andrey <verchenkoag@gmail.com>;"
   cMsg += "(c) 2020 Sergej Kiselev <bilance@bilance.lv>;;"
   cMsg += "Export to Microsoft Office;"
   cMsg += "(c) 2018-2020 Sidorov Aleksandr <aksidorov@mail.ru>;;"
   cMsg += "Export to XML;"
   cMsg += "(c) 2018 Igor Nazarov;"
   cMsg += "(c) 2020 Sidorov Aleksandr <aksidorov@mail.ru>;;"
   cMsg += "Export to Open Office;"
   cMsg += "(c) 2018 Pavel Tsarenko <tpe2@mail.ru>;"
   cMsg += "(c) 2020 Sidorov Aleksandr <aksidorov@mail.ru>;;"
   cMsg += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";"
   cMsg += "(c) Grigory Filatov http://www.hmgextended.com;;"
   cMsg += PadC( "This program is Freeware!", 60 ) + ";"
   cMsg += PadC( "Copying is allowed!", 60 ) + ";"

   AlertInfo( cMsg, "About this demo", , , {RED} , , ;
              {|ao,cn|
               Local cBtnName := "Btn_01"
               Local cForm    := ThisWindow.Name
               ao := (This.Object):GetObj4Type("EDITBOX")
               If HB_ISARRAY(ao) .and. Len(ao) == 1
                  cn := ao[1]:Name
                  This.Height      := test.ClientHeight  * 0.8
                  This.(cn).Height := This.ClientHeight - This.(cn).Row - 70
                  This.Center
                  This.&(cBtnName).Row := GetProperty(cForm,"ClientHeight") - 60
                  This.&(cBtnName).Col := This.&(cBtnName).Col - 10
                  This.&(cBtnName).FontColor := YELLOW
                  //This.&(cBtnName).FontBold  := .T.
               EndIf
               Return Nil
              } )

RETURN NIL


STATIC FUNCTION _HMG_FontName( FontHandle )
   LOCAL FontName, i

   IF ( i := AScan( _HMG_aControlHandles, FontHandle ) ) > 0
      IF _HMG_aControlType [ i ] == "FONT"
         FontName := _HMG_aControlNames [ i ]
      ENDIF
   ENDIF

RETURN FontName
