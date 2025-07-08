/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Тестирование колонок в Tsbrowse для dbf файла 
 * Копировать / вставить в буфер обмена содержимое ячейки
 * Поддержка вставки буфера обмена всех типов переменных таблицы
 * Testing columns in Tsbrowse for a dbf file
 * Copy / paste the contents of a cell to the clipboard
 * Support for pasting clipboard of all types of table variables
*/

#define _HMG_OUTLOG
#define SHOW_TITLE  "Testing columns in Tsbrowse for a dbf file"

#include "minigui.ch"
#include "TSBrowse.ch"

REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST DBFNTX, DBFCDX, DBFFPT

//////////////////////////////////////////////////////////////////////
PROCEDURE Main()
   LOCAL cFile1, cAls1, cCdp1, cVia1, cFile2, cAls2, cCdp2, cVia2
   LOCAL oBrw1, oBrw2, nY, nX, nW, nH, nC, nWPrt, aDatos1, aDatos2
   LOCAL cFont, nSize, aBackColor, aTsbFont, hFont1, hFont2, hFont3

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   RddSetDefault( 'DBFCDX' )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   OFF
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET MSGALERT BACKCOLOR TO { 141, 179, 226 }               // for HMG_Alert()
   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono" SIZE 12  // for HMG_Alert()

   SET MENUSTYLE EXTENDED      // switch menu style to advanced
   SetMenuBitmapHeight( 20 )   // set icon size 20x20

   _SetGetLogFile( GetStartUpFolder() + "\_Msg.log" )
   fErase( _SetGetLogFile() )

   cFont      := "Arial"
   nSize      := 12
   aBackColor := SILVER
   cFile1     := cFile2 := GetStartUpFolder() + "\demo.DBF"
   cAls1      := "ONE"
   cAls2      := "TWO"
   cCdp1      := cCdp2  := "RU866"
   cVia1      := cVia2  := "DBFCDX"
   aTsbFont   := { "TsbNorm", "TsbBold", "TsbBold", "TsbSpecH", "TsbSuperH", "TsbEdit" }

   nY := nX := 0

   myFont( .T., nSize )  // загрузить свои фонты для таблицы
   aDatos1 := CreateDatos1( cFile1, cAls1, cCdp1, cVia1 )
   aDatos2 := CreateDatos2( cFile2, cAls2, cCdp2, cVia2 )

   SET DEFAULT ICON TO "1MAIN_ICO"
   SET FONT TO cFont, nSize
   hFont1  := GetFontHandle( "TsbNorm"   )
   hFont2  := GetFontHandle( "TsbBold"   )
   hFont3  := GetFontHandle( "TsbSuperH" )

   DEFINE WINDOW Form_Main                 ;
      TITLE SHOW_TITLE ICON "1MAIN_ICO"    ;
      BACKCOLOR aBackColor                 ;
      MAIN TOPMOST                         ;
      ON INIT    {|| This.Topmost := .F. } ;
      ON RELEASE {|| DbCloseAll(), myFont() } ;
      NOMAXIMIZE NOSIZE

      nW := This.ClientWidth       // ширина окна

      (This.Object):Cargo           := oKeyData()
      (This.Object):Cargo:oBrwFocus := Nil

      DEFINE MAIN MENU
         POPUP "Test tbrowse" FONT hFont3
            ITEM "F3: ListColumn tbrowse-1"  ACTION  myListColumn( oBrw1 ) FONT hFont2
            ITEM "F3: ListColumn tbrowse-2"  ACTION  myListColumn( oBrw2 ) FONT hFont2
            SEPARATOR
            ITEM "Exit"                      ACTION Form_Main.Release FONT hFont3
         END POPUP
         POPUP "About"        FONT hFont3
            ITEM "Program Info"              ACTION MsgAbout()     FONT hFont2
            ITEM "Mode tbrowse-dbf"          ACTION MsgInfoTsb()   FONT hFont2
         END POPUP
         POPUP "Right-click on cell" FONT hFont3
            ITEM "Right click info"          ACTION MsgInfoCell()  FONT hFont2
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

      oBrw1 := myBrw1( nY, nX, nW, nH, aDatos1, aTsbFont, 1 )
      (This.Object):Cargo:oBrw1 := oBrw1         // на окне запомнили, объект tsb уже готовый

      /////////////// вторая таблица ///////////////////
      nY += nH
      nH := nC - nH

      oBrw2 := myBrw2( nY, nX, nW, nH, aDatos2, aTsbFont, 2 )
      (This.Object):Cargo:oBrw2 := oBrw2         // на окне запомнили, объект tsb уже готовый

      ON KEY ESCAPE ACTION {|| iif( oBrw2:IsEdit, oBrw2:SetFocus(), ;
                               iif( oBrw1:IsEdit, oBrw1:SetFocus() , _wPost(99) ) ) }  // выход по ESC

      WITH OBJECT This.Object
         :Event(99, {|ow| ow:Release() } )
      END WITH

      This.Minimize ;  This.Restore ; DO EVENTS

      oBrw1:SetFocus()

   END WINDOW

   ACTIVATE WINDOW Form_Main

RETURN

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrw1( nY, nX, nW, nH, aDatos, aFont, nBrw )
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aArray, aFAlign
   LOCAL oBrw1, aName, aSupHd, cAlias, aField

   cAlias     := aDatos[  1 ]
   aArray     := aDatos[  1 ]
   aHead      := aDatos[  2 ]
   aSize      := aDatos[  3 ]
   aFoot      := aDatos[  4 ]
   aPict      := aDatos[  5 ]
   aAlign     := aDatos[  6 ]
   aName      := aDatos[  7 ]
   aField     := aDatos[  8 ]
   aFAlign    := aDatos[  9 ]         // Footer align
   aSupHd     := aDatos[ 10 ]
   aFoot      := .T.                  // создаем пустые значения для подвала
/*
//---------- отладка ---------------
? "aArray =" , aArray               ; ?
? "aHead ="  , aHead   ; ?v aHead   ; ?
? "aSize ="  , aSize   //; ?v aSize ; ?
//? "aFoot ="  , aFoot   ; ?v aFoot ; ?
? "aPict ="  , aPict   ; ?v aPict   ; ?
? "aAlign =" , aAlign  ; ?v aAlign  ; ?
? "aName ="  , aName   ; ?v aName   ; ?
? "aField =" , aField  ; ?v aField  ; ?
? "aSupHd =" , aSupHd  ; ?v aSupHd  ; ?
*/

   DEFINE TBROWSE oBrw1                                  ;
          AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ;
          FONT       aFont                               ;
          BRUSH      YELLOW                              ;
          HEADERS    aHead                               ;
          COLSIZES   aSize                               ;
          PICTURE    aPict                               ;
          JUSTIFY    aAlign                              ;
          COLUMNS    aField                              ;
          COLNAMES   aName                               ;
          FOOTERS    aFoot                               ;
          FIXED      COLSEMPTY                           ;
          LOADFIELDS                                     ;
          /*COLNUMBER  { 1, 40 } */                      ;
          ENUMERATOR LOCK EDIT

          :Cargo          := oKeyData()            // создает объект без переменных (условно пустой) используем ниже по коду
          :Cargo:nBrowse  := nBrw                  // номер таблицы - запомнили

          mySetTsb( oBrw1 )            // настройки таблицы
          //myPartWidthTsb( oBrw1 )      // поправить ширину колонок
          myColorTsb( oBrw1 )          // цвета на таблицу
          myColorTsbElect( oBrw1 )     // цвета избранные
          mySupHdTsb( oBrw1, aSupHd )  // SuperHeader
          //myEnumTsb( oBrw1 , 1 )     // ENUMERATOR по порядку - здесь не нужен
          mySet2Tsb( oBrw1 )           // настройки таблицы дополнительные
          mySetEditTsb( oBrw1 )        // настройки редактирования

          :bGotFocus := {|ob| myGotFocusTsb(ob)     }
          :bOnDraw   := {|ob| SayStatusBar(ob)      }   // показ StatusBar - Recno/Column

          :UserKeys(VK_F3, {|ob| myListColumn(ob)   })  // инфо по списку колонок

   END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:oPhant:nClrHeadBack := ob:Cargo:nClr4, ;
                                             ob:oPhant:nClrFootBack := ob:Cargo:nClr10,;
                                             ob:Refresh() }

RETURN oBrw1

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrw2( nY, nX, nW, nH, aDatos, aFont, nBrw )
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aArray, aFAlign
   LOCAL oBrw2, aName, aSupHd, cAlias, aField

   cAlias     := aDatos[  1 ]
   aArray     := aDatos[  1 ]
   aHead      := aDatos[  2 ]
   aSize      := aDatos[  3 ]
   aFoot      := aDatos[  4 ]
   aPict      := aDatos[  5 ]
   aAlign     := aDatos[  6 ]
   aName      := aDatos[  7 ]
   aField     := aDatos[  8 ]
   aFAlign    := aDatos[  9 ]         // Footer align
   aSupHd     := aDatos[ 10 ]
   aFoot      := .T.                  // создаем пустые значения для подвала

   DEFINE TBROWSE oBrw2                                  ;
          AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ;
          FONT       aFont                               ;
          BRUSH      YELLOW                              ;
          HEADERS    aHead                               ;
          COLSIZES   aSize                               ;
          PICTURE    aPict                               ;
          JUSTIFY    aAlign                              ;
          COLUMNS    aField                              ;
          COLNAMES   aName                               ;
          FOOTERS    aFoot                               ;
          FIXED      COLSEMPTY                           ;
          LOADFIELDS GOTFOCUSSELECT                      ;
          COLNUMBER  { 1, 40 }                           ;
          ENUMERATOR LOCK  EDIT  SELECTOR .T.

          :Cargo          := oKeyData()            // создает объект без переменных (условно пустой) используем ниже по коду
          :Cargo:nBrowse  := nBrw                  // номер таблицы - запомнили

          mySetTsb( oBrw2 )            // настройки таблицы
          myPartWidthTsb( oBrw2 )      // поправить ширину колонок
          myColorTsb( oBrw2 )          // цвета на таблицу
          myColorTsbElect( oBrw2 )     // цвета избранные
          mySupHdTsb( oBrw2, aSupHd )  // SuperHeader
          myEnumTsb( oBrw2 , 1)        // ENUMERATOR по порядку
          mySet2Tsb( oBrw2 )           // настройки таблицы дополнительные
          mySetEditTsb( oBrw2 )        // настройки редактирования

          :bGotFocus := {|ob| myGotFocusTsb(ob)     }
          :bOnDraw   := {|ob| SayStatusBar(ob)      }   // показ StatusBar - Recno/Column

          :UserKeys(VK_F3, {|ob| myListColumn(ob)   })  // инфо по списку колонок

   END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:oPhant:nClrHeadBack := ob:Cargo:nClr4, ;
                                             ob:oPhant:nClrFootBack := ob:Cargo:nClr10,;
                                             ob:Refresh() }

RETURN oBrw2

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

   cVal := "Column: "+hb_ntos(nCell-7)+" / "+hb_ntos(oBrw:nColCount()-7)
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
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "' + oCol:cFieldTyp + '" ' + HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT

   AlertInfo(cMsg + REPL(";",30))

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
      :nCellMarginLR := 1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
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

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myPartWidthTsb( oBrw )          // поправить ширину колонок
   LOCAL oCol, cType, nW, hFont := oBrw:hFont   // 1-cells font

   WITH OBJECT oBrw
      FOR EACH oCol IN :aColumns
         cType := oCol:cFieldTyp
         //? "oCol:xxxx = ", cType, oCol:cName, oCol:nWidth, oCol:cPicture,
         //?? oCol:nFieldLen, oCol:nFieldDec
         IF cType $ "=@T"
            oCol:nWidth := (App.Object):W(2.5)  // можно сделать так
            //oCol:nWidth := GetTextWidth( Nil, REPL("9",24), hFont ) // 24 знака
         ELSEIF cType $ "+^" // Type: [+] [^]
            // если в базе будет 1 000 000 записей, то нужно 7 знаков
            oCol:nWidth := GetTextWidth( Nil, REPL("9",7), hFont )  // 7 знака
         ELSEIF cType == "D"
            oCol:nWidth := GetTextWidth( Nil, REPL("9",11), hFont )
         //ELSEIF cType == "N"
            // не нужно это делать. оставил в качестве примера.
            // увеличим ширину колонки для коротких названий полей
            //IF LEN(oCol:cName) < 5
            //   oCol:nWidth := GetTextWidth( Nil, REPL("H", oCol:nFieldLen), hFont ) * 0.8
            //ENDIF
         ELSEIF cType == "C"
            // увеличим ширину колонки для компенсации :nCellMarginLR := 1
            // это нужно не всегда ! зависит от разрешений экрана и фонта
            // можете фонт "DejaVu Sans Mono" заменить на "Arial"
            nW := GetTextWidth( Nil, REPL("H",1), hFont )
            oCol:nWidth +=  nW + nW/2
         ELSE
         ENDIF
      NEXT
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsb( oBrw )
   LOCAL O

   WITH OBJECT oBrw:Cargo

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
      :nORANGE    :=  CLR_ORANGE
      :nWHITE     :=  CLR_WHITE
      :nPURPLE2   := RGB(206,59,255)
      :nBCDelRec  := RGB( 65, 65, 65 )
      :nFCDelRec  := RGB( 251, 250, 174 )   // желтый осветл.
      //:nFCDelRec  := RGB( 248, 209, 211 )   // красный осветл.
      :nBCYear    := RGB( 251, 213, 181 )   // оранжевый осветл. 40%
      :nFCYear    := RGB( 109,  15, 20  )   // красный

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
      :aClrVirt   := { :nBCDelRec, 0, :nHBLUE2, :nBCYear, :nHRED, :nPURPLE2 }
      :aClrBrw    := { :nGREEN2 , :nYELLOW }

   END WITH

   WITH OBJECT oBrw
      O := :Cargo
      :nClrLine := O:nClrLine   // создать в контейнере свои переменные с именами
      :SetColor( { 1}, { O:nClr1  } )  // 1 , текста в ячейках таблицы
      :SetColor( { 2}, { O:nClr2  } )  // 2 , фона в ячейках таблицы
      :SetColor( { 5}, { O:nClr5  } )  // 5 , текста курсора, текст в ячейках с фокусом
      :SetColor( { 6}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr6_1 , c:nClr6_2  ) } } )  // 6 , фона курсора
      :SetColor( {11}, { O:nClr11 } )  // 11, текста неактивного курсора (selected cell no focused)
      :SetColor( {12}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr12_1, c:nClr12_2 ) } } )  // 12, фона неактивного курсора (selected cell no focused)
      :Setcolor( { 3}, { O:nClr3  } )    // 3 , текста шапки таблицы
      :SetColor( { 4}, { O:nClr4  } )    // 4 , фона шапка таблицы   // !!! тут лишний блок кода, массива достаточно
      :SetColor( { 9}, { O:nClr9  } )    // 9 , текста подвала таблицы
      :SetColor( {10}, { O:nClr10 } )    // 10, фона подвала таблицы // !!! тут лишний блок кода, массива достаточно
      :hBrush   := CreateSolidBrush( 255, 255, 230 )   // цвет фона под таблицей
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsbElect( oBrw )
   LOCAL lVirtual, nCol, cFld, oCol, aCol := oBrw:aColumns
   LOCAL cCol, O := oBrw:Cargo  // использовать из контейнера свои переменные
   LOCAL nAt := oBrw:nAt

   oBrw:GetColumn("ORDKEYNO"):nClrBack  := O:nBClrSpH

   FOR nCol := 1 TO Len(aCol)
      oCol := aCol[ nCol ]
      cCol := oCol:cName
      lVirtual := .F.
      IF cCol == "ORDKEYNO"
         lVirtual := .T.
      ENDIF
      IF !lVirtual
         // ----- первое условие для строки таблицы --------- цвет не будет по нему ---------
         //oCol:nClrBack := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->( Deleted() ), O:nBCDelRec, O:nClr2 ) }
         //oCol:nClrFore := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->( Deleted() ), O:nFCDelRec, O:nClr1 ) }
         // ----- доп.условие для строки таблицы ------- цвет будет по нему ----------
         //oCol:nClrBack := { |nr,nc,ob| nr:=nc, iif( Eval(ob:GetColumn("YEAR2"):bData) > 2020 , O:nBCYear, O:nClr2 ) }
         //oCol:nClrFore := { |nr,nc,ob| nr:=nc, iif( Eval(ob:GetColumn("YEAR2"):bData) > 2020 , O:nFCYear, O:nClr1 ) }
         // или можно так
         //oCol:nClrBack := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->YEAR2 > 2020 , O:nBCYear, O:nClr2 ) }
         //oCol:nClrFore := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->YEAR2 > 2020 , O:nFCYear, O:nClr1 ) }

         // цвет фона для всех ячеек строки таблицы  - несколько условий
         oCol:nClrBack := { |a,n,b| myTsbColorBackLine(a,n,b)   }
         // цвет текста для всех ячеек строки таблицы - несколько условий
         oCol:nClrFore := { |a,n,b| myTsbColorForeLine(a,n,b)   }
      ENDIF
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
       oCol:nClrBack     := { |a,n,b| myTsbColorBack(a,n,b)   }  // цвет фона в ячейках таблицы
       oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }  // цвет фона подвала таблицы
       oCol:nClrFootBack := { |n,b  | myTsbColorBackHead(n,b) }  // цвет фона шапка таблицы
       // Это историческая неточность (параметры надо было {|b,n,a| ... } )
       // для блока кода подвала - передается два параметра
       // для строки(ячеек) - передается три параметра
   NEXT*/

RETURN Nil

////////////////////////////////////////////////////////////////////////
// цвет фона для всех ячеек строки таблицы  - несколько условий
STATIC FUNCTION myTsbColorBackLine( nAt, nCol, oBrw )
   LOCAL nColor, nVal, lDel, nI, nRezerv := nAt := nCol
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   lDel := (oBrw:cAlias)->( Deleted() )
   // ---- второе условие ----
   // или так
   //nVal := Eval(oBrw:GetColumn("YEAR2"):bData)
   // или так
   //nVal := (oBrw:cAlias)->YEAR2
   // или так
   nI   := oBrw:GetColumn("YEAR2")
   nVal := oBrw:GetValue(nI)

   IF VALTYPE(nVal) != "N"     // обработка ошибочной ситуации
      nColor := O:nBLACK
   ELSEIF lDel
      nColor := O:nBCDelRec    // новый цвет фона таблицы
   ELSEIF nVal > 2020
      nColor := O:nBCYear      // новый цвет фона таблицы
   ELSE
      nColor := O:nClr2        // цвет фона таблицы
   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////
// цвет текста для всех ячеек строки таблицы - несколько условий
STATIC FUNCTION myTsbColorForeLine( nAt, nCol, oBrw )
   LOCAL nColor, nVal, lDel, nI, nRezerv := nAt := nCol
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   lDel := (oBrw:cAlias)->( Deleted() )
   // ---- второе условие ----
   // или так
   //nVal := Eval(oBrw:GetColumn("YEAR2"):bData)
   // или так
   //nVal := (oBrw:cAlias)->YEAR2
   // или так
   nI   := oBrw:GetColumn("YEAR2")
   nVal := oBrw:GetValue(nI)

   IF VALTYPE(nVal) != "N"     // обработка ошибочной ситуации
      nColor := O:nBLACK
   ELSEIF lDel
      nColor := O:nFCDelRec    // новый цвет текста таблицы
   ELSEIF nVal > 2020
      nColor := O:nFCYear      // новый цвет текста таблицы
   ELSE
      nColor := O:nClr1        // цвет текста таблицы
   ENDIF

RETURN nColor

//////////////////////////////////////////////////////////////////
STATIC FUNCTION mySupHdTsb( oBrw, aSupHd )
   LOCAL O := oBrw:Cargo             // использовать из контейнера свои переменные

   WITH OBJECT oBrw
   // суперхидер
   :AddSuperHead( 1, :nColCount(), aSupHd[1] )

   // задать цвета суперхидеру
   :SetColor( {16}, { O:nClr16  } ) // 16, фона спецхидер
   :SetColor( {17}, { O:nClr17  } ) // 17, текста спецхидер

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// ENUMERATOR по порядку сделаем свой
STATIC FUNCTION myEnumTsb( oBrw , nOneCol )
   LOCAL oCol, nI := 0, nCnt := 0
   DEFAULT nOneCol := 1

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
   LOCAL i, oCol, cTyp

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
         IF cTyp $ "+=^"   // Type: [+] [=] [^]
            oCol:bPrevEdit := {|| AlertStop("It is forbidden to edit this type of field !") , FALSE }
         ENDIF
         // edit колонок
         oCol:bRClicked := {|nrp,ncp,nat,obr| myCellClick(2,obr,nrp,ncp,nat) }
      NEXT

   END WIDTH

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myCellClick( nClick, oBrw, nRowPix, nColPix )
   LOCAL cNam := {'Left mouse', 'Right mouse'}[ nClick ]
   LOCAL nRow, oCol, nCol, nLine, cTyp, cType, xVal, cMsg, cMsg4
   LOCAL nY, nX, cForm, hFont1, hFont2, hFont3, cMsg1, cMsg2, cMsg3
   LOCAL nHMenu, nWMenu

   DO EVENTS

   WITH OBJECT oBrw
      :DrawSelect() ; DO EVENTS
      cForm := :cParentWnd
      nLine := :nAt                           // table line number
      nRow  := :GetTxtRow(nRowPix)            // table row cursor number
      nCol  := Max( :nAtCol(nColPix), 1 )     // cursor column number in table
      xVal  := :GetValue(nCol)                // real value
      cTyp  := ValType( xVal )                // real valtype
      oCol  := :aColumns[ nCol ]
      cType := oCol:cFieldTyp                 // valtype field
   END WITH

   //nX := _HMG_MouseCol
   //nY := _HMG_MouseRow
   // возмём координаты от ячеек таблицы
   nY := INT( oCol:oCell:nRow * oBrw:nHeightCell ) + oBrw:nTop + GetTitleHeight()
   nY += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper, 0 )
   nY += IIF( oBrw:lDrawSpecHd, oBrw:nHeightSpecHd, 0 )
   nY += IIF( oBrw:lDrawHeaders, oBrw:nHeightHead, 0 )
   nX := INT( oCol:oCell:nCol + int(oCol:oCell:nWidth / 2 ) )

   hFont1 := GetFontHandle( "TsbEdit"   )
   hFont2 := GetFontHandle( "TsbSuperH" )
   hFont3 := GetFontHandle( "TsbBold"   )

   nHMenu := 20*8 + 20*5 + 20  // примерная высота контекстного меню

   IF nY + nHMenu >= System.ClientHeight
      nY := System.ClientHeight - nHMenu
   ENDIF

   cMsg1  := "Cell position row/column: " + hb_ntos(nLine) + '/' + hb_ntos(nCol)
   cMsg2  := "Get Cell value: [" + cValToChar(xVal) + "]"
   cMsg3  := 'Type Cell: "' + cType + '" ( "' + cTyp + '" )'
   cMsg   := cMsg1 + ";" + cMsg2 + ";" + cMsg3
   cMsg4  := oBrw:cControlName+": "+hb_ntos(nRow)+"/"+hb_ntos(nCol)
   cMsg4  += "  y/x= " + HB_NtoS(nY) + "/" + HB_NtoS(nX)
   cMsg4  += " oCol:oCell:nRow = " + HB_NtoS(oCol:oCell:nRow)

   // примерная ширина контекстного меню
   nWMenu := 20 + GetTextWidth( nil, cMsg4, hFont1 )   + 20
   // или можно так
   nWMenu := 20 + GetFontWidth("TsbEdit", LEN(cMsg4) ) + 20
   IF nX + nWMenu >= System.ClientWidth
      nX := nX - nWMenu - 40
   ENDIF

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  "Copy cell to clipboard"       ACTION  {|| System.Clipboard := cValToChar(oBrw:GetValue(nCol)) , oBrw:SetFocus() } FONT hFont2
       SEPARATOR
       MENUITEM  "Copy to cell from clipboard"  ACTION  {|| Copy_Cell_Clipboard(1,oBrw,cType,nCol) }  FONT hFont2
       SEPARATOR
       MENUITEM  "Clear cell"                   ACTION  {|| Copy_Cell_Clipboard(0,oBrw,cType,nCol) }  FONT hFont2
       SEPARATOR
       MENUITEM  "Exit"                         ACTION  {|| oBrw:SetFocus() } FONT hFont3
       SEPARATOR
       MENUITEM  cMsg4    DISABLED  DEFAULT     FONT hFont1
       SEPARATOR
       MENUITEM  cMsg1    DISABLED  DEFAULT     FONT hFont1
       MENUITEM  cMsg2    DISABLED  DEFAULT     FONT hFont1
       MENUITEM  cMsg3    DISABLED  DEFAULT     FONT hFont1
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
STATIC FUNCTION Copy_Cell_Clipboard(nMenu,oBrw,cType,nCol)
   LOCAL xWrt

   IF nMenu == 1
      xWrt := System.Clipboard
   ELSE
      xWrt := ""
   ENDIF

   IF cType == VALTYPE(xWrt)
   ELSEIF cType == "C" .AND. VALTYPE(xWrt) == "N"
      xWrt := HB_NtoS(xWrt)
   ELSEIF cType == "N" .AND. VALTYPE(xWrt) == "C"
      xWrt := VAL(xWrt)
   ELSEIF cType == "D"
      xWrt := CTOD(xWrt)
   ELSEIF cType == "T" .OR. cType == "@"
      xWrt := hb_CToT(xWrt)
   ELSEIF cType == "L"
      xWrt := IIF( nMenu == 1, .T., .F. )
   ENDIF

   //?? xWrt, cType, VALTYPE(xWrt)

   IF cType $ "+=^"   // Type field: [+] [=] [^]
      AlertStop("It is forbidden to edit this type of field !")
   ELSE
      IF (oBrw:cAlias)->( RLock() )
          oBrw:SetValue(nCol, xWrt)
         (oBrw:cAlias)->( DbUnlock() )
         (oBrw:cAlias)->( DbCommit() )
      ELSE
         AlertStop("Recording is locked!")
      ENDIF
   ENDIF

   oBrw:DrawSelect()
   oBrw:SetFocus()

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateDatos1( cFile, cAlias, cCdp, cVia )
   LOCAL aDatos, aSupHd

   aDatos := CreateDatos(cFile, cAlias, cCdp)

   IF Empty( aDatos ) ; RETURN aDatos   // File not found !
   ENDIF

   // суперхидер
   aSupHd     := { cFile }

   IF ! empty(cCdp) ; aSupHd[1] += '   [ ' + cCdp + ' ] '
   ENDIF

   IF ! empty(cVia) ; aSupHd[1] += '   [ ' + cVia + ' ] '
   ENDIF

   IF ! empty(cAlias) ; aSupHd[1] += '   [ Alias: ' + cAlias + ' ]'
   ENDIF

   AAdd( aDatos, aSupHd )  // добавим к aDatos массив aSupHd

RETURN aDatos

//////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateDatos2( cFile, cAlias, cCdp, cVia )
   LOCAL aDatos, aSupHd

   aDatos := CreateDatos(cFile, cAlias, cCdp)

   IF Empty( aDatos ) ; RETURN aDatos   // File not found !
   ENDIF

   // суперхидер
   aSupHd     := { cFile }

   IF ! empty(cCdp) ; aSupHd[1] += '   [ ' + cCdp + ' ] '
   ENDIF

   IF ! empty(cVia) ; aSupHd[1] += '   [ ' + cVia + ' ] '
   ENDIF

   IF ! empty(cAlias) ; aSupHd[1] += '   [ Alias: ' + cAlias + ' ]'
   ENDIF

   AAdd( aDatos, aSupHd )  // добавим к aDatos массив aSupHd

RETURN aDatos

/////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateDatos( cFile, cAlias, cCdp )
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aName, aField, aFAlign
   LOCAL aDatos, i, k

   IF hb_FileExists( cFile )

      IF Empty(cCdp) ; USE &(cFile) ALIAS (cAlias) SHARED NEW
      ELSE           ; USE &(cFile) ALIAS (cAlias) SHARED NEW CODEPAGE cCdp
      ENDIF

   ELSE

      MsgStop('File Dbf not found !' + CRLF + cFile  + CRLF + ProcName() , "ERROR")
      ReleaseAllWindows()
      RETURN NIL

   ENDIF

   k       := fCount()
   aHead   := array(k)
   aFoot   := array(k)
   aPict   := array(k)
   aName   := array(k)
   aAlign  := array(k)
   aField  := array(k)
   aSize   := array(k)
   aFAlign := array(k)       // Footer align

   FOR i := 1 TO k
       aHead  [ i ] := FieldName( i )
       aFoot  [ i ] := hb_ntos  ( i )
       aName  [ i ] := FieldName( i )
       aField [ i ] := FieldName( i )
       aFAlign[ i ] := DT_CENTER
       aAlign [ i ] := DT_CENTER
       switch FieldType( i )
          case 'C' ; aAlign[ i ] := DT_LEFT   ; exit
          case 'M' ; aAlign[ i ] := DT_LEFT   ; exit
          case 'N' ; aAlign[ i ] := DT_RIGHT  ; exit
       end switch
   NEXT

   aDatos := ALIAS()
   aSize  := NIL // array(k) - размер ширин колонок построит сам tsbrowse

RETURN { aDatos, aHead, aSize, aFoot, aPict, aAlign, aName, aField, aFAlign }

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
FUNCTION MsgInfoCell()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Right-click on the cell, menu:;"
   cMsg += "Copy / paste the contents of a cell to the clipboard;"
   cMsg += "or clear cell;"
   cMsg += "Support for pasting clipboard of all types of table variables;;"
   cMsg += "На ячейке кликнуть правой кнопкой мышки, меню:;"
   cMsg += "Копировать / вставить в буфер обмена содержимое ячейки;"
   cMsg += "или очистить ячейку;"
   cMsg += "Поддержка вставки буфера обмена всех типов переменных таблицы;"

   AlertInfo( cMsg, "Right-click on the cell Info", , , {RED} , , )

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgInfoTsb()
   LOCAL cMsg

   cMsg := SHOW_TITLE + ";;"
   cMsg += "Table display mode: tsbrowse-dbf;"
   cMsg += "Table colors as an example :;"
   cMsg += "deleted records and field YEAR2> 2020;;"
   cMsg += "Режим показа таблицы: tsbrowse-dbf;"
   cMsg += "Цвета таблицы в качестве примера:;"
   cMsg += "удалённые записи и поле YEAR2 > 2020"

   AlertInfo( cMsg, "Mode tsbrowse", , , {RED} , , )

RETURN NIL

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


