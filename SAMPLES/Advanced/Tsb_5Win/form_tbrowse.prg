/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Универсальный модуль обработки нескольких таблиц
*/

//#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"

MEMVAR oMain
#define BRW_BMP_CONTEX_MENU    28
//////////////////////////////////////////////////////////////////////////////////////////
FUNCTION TBrowse_Create(nTable, cTabl, cForm, cAls, c2Title, nYBrw, nXBrw, nWBrw, nHBrw)
   LOCAL aBackColor, aUse3Dim, cSupHdr, aTsbFont, nBrwBC, nBrwBC2, nBrwBC3, aParam
   LOCAL xSelector, aBrush, aEdit, aNumer, aSupHdr, aBrwColors, nHead1, nHead2
   LOCAL aHeader, aFSize, aFooter, aPict, aAlign, aNames, aField, aFAlign, aCardTitle
   LOCAL oBrw   // для каждой таблицы будет свой oBrw

   ? "====== " + ProcNL(), cForm, cTabl, nTable, cAls, c2Title

   // данные по форме -> ListTables.prg
   aBackColor := myTableBackColor(nTable)          // цвет формы
   // данные по таблице -> ListTables.prg
   aUse3Dim   := myTableUse(nTable)                // массив базы dbf/alias/codepage

   aTsbFont   := myTsbFont()                       // загрузить фонты для таблицы
   aHeader    := myTableDatos(nTable,1)            // список шапки колонок таблицы
   aCardTitle := myTableDatos(nTable,1)            // список названий в карточку
   aFSize     := myTableDatos(nTable,2)            // ширина колонок таблицы
   aFooter    := myTableDatos(nTable,3)            // список подвала колонок таблицы
   aPict      := myTableDatos(nTable,4)            // список PICTURE колонок таблицы
   aAlign     := myTableDatos(nTable,5)            // список отбивки колонок таблицы
   aNames     := myTableDatos(nTable,6)            // список наименований колонок таблицы
   aField     := myTableDatos(nTable,7)            // список полей базы колонок таблицы
   aFAlign    := myTableDatos(nTable,8)            // список отбивки подвала колонок таблицы
   aEdit      := myTableDatos(nTable,9)            // список правки колонок таблицы

   cSupHdr    := "[Alias: " + aUse3Dim[2] + " , CodePage: " + aUse3Dim[3] + "]"
   cSupHdr    += SPACE(5) + c2Title
   aSupHdr    := { cSupHdr }                       // список суперхидера - для примера 1
   xSelector  := .T.                               // первая колонка - селектор
   aNumer     := { 1, 70 }                         // виртуальная колонка с нумерацией
   aBrush     := SILVER                            // цвет фона под таблицей
   nBrwBC     := ToRGB( aBackColor        )        // цвет фона таблицы
   nBrwBC2    := ToRGB( { 255, 255, 255 } )        // цвет фона через одну линию
   nBrwBC3    := ToRGB( {  50,  50,  50 } )        // цвет фона удалённых записей
   nHead1     := ToRGB( {  40, 122, 237 } )        // цвет фона шапки и подвала: голубой цвет
   nHead2     := ToRGB( {  48,  29,  26 } )        // цвет фона шапки и подвала: серо-черный фон
               //    1       2       3       4        5       6       7
   aParam     := { nTable, aEdit, nBrwBC, nBrwBC2, nBrwBC3, nHead1, nHead2 } // передать в Cargo
   aBrwColors := myBrwGetColor(nTable,nBrwBC,nBrwBC2,nBrwBC3,nHead1,nHead2)  // цвета задаём перед Tsbrowse, замена :SetColor(...)

   DEFINE TBROWSE &cTabl OBJ oBrw CELL ;
      AT nYBrw, nXBrw ALIAS cAls WIDTH nWBrw HEIGHT nHBrw ;
      FONT aTsbFont                    ;   // все фонты для таблицы
      BRUSH aBrush                     ;   // цвет фона под таблицей
      COLORS  aBrwColors               ;   // все цвета таблицы
      BACKCOLOR aBackColor             ;   // фон таблицы - совпадает с фоном окна
      HEADERS aHeader                  ;   // список шапки колонок таблицы
      JUSTIFY aAlign                   ;   // список отбивки колонок таблицы
      COLUMNS aField                   ;   // список наименований колонок таблицы
      NAMES   aNames                   ;   // список полей базы колонок таблицы
      EDITCOLS aEdit                   ;   // массив данных для редактирования колонок .T.\.F.\Nil>\.T\.F.\NIL
      FOOTERS aFooter                  ;   // список подвала колонок таблицы
      SIZES aFSize                     ;   // ширина колонок таблицы
      LOADFIELDS                       ;   // автоматическое создание столбцов по полям активной базы данных
      GOTFOCUSSELECT                   ;
      EMPTYVALUE                       ;
      FIXED                            ;   // активирует функцию двойного курсора на закрепленных столбцах
      COLNUMBER aNumer                 ;   // виртуальная колонка с нумерацией
      ENUMERATOR                       ;   // нумерация колонок
      LOCK                             ;   // автоматическая блокировка записи при вводе в базу данных
      SELECTOR xSelector               ;   // первая колонка - селектор записей
      ON INIT {|ob| myBrwInit( ob ) }      // настройки таблицы - смотреть ниже

      myBrwInit(oBrw, aParam)              // мои ДОнастройки таблицы
      myBrwSetting(oBrw,aTsbFont)          // настройки таблицы
      myBrwDelColumn(oBrw)                 // убрать колонки из отображения
      myBrwColumnWidth(oBrw)               // изменение показа ширины колонок
      myBrwMaskBmp(oBrw)                   // маска показа картинок
      myBrwEnum(oBrw)                      // ENUMERATOR по порядку
      myBrwColorChange(oBrw,nTable)        // цвета изменить
      myBrwHeaderFooterSpcHd(oBrw,aFAlign) // обработка шапки, подвала и спецхидера
      myBrwSuperHeader(oBrw,aSupHdr)       // создать СуперХидер таблицы
      RecnoDeleteRecover(oBrw, .T.)        // init for :DeleteRow()

      IF ! :lSelector   // если нет селектора
         :AdjColumns()
      ENDIF

   END TBROWSE ON END {|ob| mySetNoHoles(ob) ,;  // смотреть ниже
                            iif( ob:lSelector, ob:AdjColumns(), Nil ), ;
                            ob:SetFocus() }
        // AdjColumns() - добавить пробелы для растяжки колонок по всей длине Tsbrowse

   // !!! Колонка SELECTOR есть !!!
   // До END TBROWSE колонки SELECTOR нет физически, все обращения к колонкам по номеру,
   // без учета SELECTOR, а после END TBROWSE колонка SELECTOR есть. :AdjColumns() до
   // END TBROWSE растягивает на всю ширину без колонки SELECTOR, а вынесенное в блок
   // ON END растягивает на всю ширину с учетом колонки SELECTOR

   // Две колонки таблицы SELECTOR и ORDKEYNO (COLNUMBER aNumer) - виртуальные,
   // т.е. их нет в Dbf файле

   oBrw:nFreeze     := oBrw:nColumn("ORDKEYNO") // заморозить таблицу до этого столбца
   oBrw:lLockFreeze := .T.                      // избегать прорисовки курсора на замороженных столбцах
   oBrw:nCell       := oBrw:nFreeze + 1         // передвинуть курсор на колонку номер

   // изменим цвет колонки
   oBrw:GetColumn("SELECTOR"):nClrBack := GetSysColor( COLOR_BTNFACE )

   // создать - контекстное меню ТСБ / TSB context menu
   myBrwContextMenu(oBrw)

   // включить/отключить контекстное меню ТСБ
   //SET CONTEXT MENU CONTROL &(oBrw:cControlName) OF &cForm ON
   //SET CONTEXT MENU CONTROL &(oBrw:cControlName) OF &cForm OFF

   // сохранить для карточки
   oBrw:Cargo:aHeader := aCardTitle  // список названий полей в карточку
   oBrw:Cargo:aField  := aField      // список наименований колонок таблицы
   oBrw:Cargo:aEdit   := aEdit       // массив данных для редактирования колонок

RETURN oBrw  // Внимание ! вернуть для внешнего доступа к таблице

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwInit( oBrw, aPrm )  // !!! установки, настройки тсб  !!!

   WITH OBJECT oBrw
      IF Empty(aPrm)                // !!! только эти установки в ON INIT !!!
         :nColOrder     :=  0       // убрать значок сортировки по полю
         :lNoChangeOrd  := .F.      // убрать сортировку по полю
                                    // что бы работал bLDblClick, иначе работает ::SetOrder(...)
         :nWheelLines   :=  1       // прокрутка колесом мыши с шагом ...
         :lNoGrayBar    := .F.      // показывать неактивный курсор в таблице
         :lNoLiteBar    := .F.      // при переключении фокуса на другое окно не убирать "легкий" Bar
         :lNoResetPos   := .F.      // предотвращает сброс позиции записи на gotfocus
         :lNoPopUp      := .T.      // избегает всплывающее меню при щелчке правой кнопкой мыши по заголовку столбца
         :nStatusItem   :=  0       // в 1-й Item StatusBar не выводить автоматом из тсб
         :lPickerMode   := .F.      // формат даты нормальный
         :nMemoHV       :=  1       // показ одной строки мемо-поля
         //:lCellBrw    := .F.      // маркер на всю таблицу
         //:lNoHScroll  := .T.      // НЕТ-показа горизонтального скролинга
         //:lNoVScroll  := .T.      // НЕТ-показа вертикального скролинга
         //:lFooting    := .T.      // использовать подвал
         //:lDrawFooters:= .T.      // рисовать  подвалы
         :lNoMoveCols   := .T.      // .T. - НЕЛЬЗЯ юзерам изменять размер или перемещать столбцы
         :nCellMarginLR :=  1       // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
         :lNoKeyChar    := .T.      // запрет на ввод символов и цифр в ячейке
         :lCheckBoxAllReturn := .F. // Enter modify value oCol:lCheckBox
         // --------- заменяем колонку CHECKBOX на свои картинки ---------
         :aCheck   := { LoadImage("CheckT24"), LoadImage("CheckF24") }
         // --------- хранилище картинок, удаляется после закрытия объекта автоматом ------
         :aBitMaps := { LoadImage("Empty16" ), LoadImage("No16") ,;
                        LoadImage("Arrow_down")    ,; // картинка стрелка_вниз  30x30
                        LoadImage("Arrow_up")      ,; // картинка стрелка_вверх 30x30
                        LoadImage("ArrowDown20")   ,; // картинка стрелка_вниз  20x20
                        LoadImage("ArrowUp20")     ,; // картинка стрелка_вверх 20x20
                        LoadImage("bFltrAdd20")    ,; // картинка фильтр 20x20
                        LoadImage("bSupHd40")      ,; // картинка 40x140
                        LoadImage("ArrDown40Blue") ,; // картинка стрелка_вниз 40x40 - PNG
                      }
                      // картинки PNG с прозрачностью не надо делать для
                      // :nBmpMaskXXXX := 0x00CC0020    // SRCCOPY

         :Cargo := oHmgData()                 // init Cargo как THmgData объект-контейнер
      ELSE
         :Cargo:nHMain     := 0               // высота главного окна ставиться в -> form_table.prg
         :Cargo:nTable     := aPrm[1]         // номер таблицы
         :Cargo:aEdit      := aPrm[2]         // массив данных для редактирования колонок
         :Cargo:hArrDown   := :aBitMaps[3]    // картинка стрелка_вниз  30x30
         :Cargo:hArrUp     := :aBitMaps[4]    // картинка стрелка_вверх 30x30
         :Cargo:hArrDown20 := :aBitMaps[5]    // картинка стрелка_вниз  20x20
         :Cargo:hArrUp20   := :aBitMaps[6]    // картинка стрелка_вверх 20x20
         :Cargo:hFltrAdd20 := :aBitMaps[7]    // картинка фильтр 20x20
         :Cargo:bSupHd32   := :aBitMaps[8]    // картинка 32x118
         :Cargo:bArrDown32 := :aBitMaps[9]    // картинка стрелка_вниз 36x36
         // запомнить цвет и другие переменные в THmgData объект-контейнер
         :Cargo:nClr_2     := aPrm[3]         // цвет фона таблицы
         :Cargo:nClr_2_1   := aPrm[4]         // цвет чётная\нечётная row
         :Cargo:nClr_2_2   := aPrm[3]         // цвет чётная\нечётная row
         :Cargo:nClr_2_Del := aPrm[5]         // цвет удалённых записей
         :Cargo:nClr_4_1   := aPrm[6]         // цвет фона шапки таблицы: градиент
         :Cargo:nClr_4_2   := aPrm[7]         // цвет фона шапки таблицы: градиент
         :Cargo:nClr_10_1  := aPrm[6]         // цвет фона подвала таблицы: градиент
         :Cargo:nClr_10_2  := aPrm[7]         // цвет фона подвала таблицы: градиент

         // ДОПОЛНИТЕЛЬНЫЕ ЦВЕТА в таблицу
         IF :Cargo:nTable == 2
          :Cargo:nClr_1      := CLR_RED         // цвет ячеек таблицы
         ELSE
          :Cargo:nClr_1      := CLR_BLACK       // цвет ячеек таблицы
         ENDIF
         :Cargo:nClr_2Col1  := GetSysColor( COLOR_BTNFACE )   // цвет фона первой колонки
         :Cargo:nClr_2xC10  := CLR_WHITE       // цвет фона шапки 1-таблицы:10-колонки - поле COUNTRY
         :Cargo:nClr_2xBlck := CLR_ORANGE      // цвет фона шапки для полей типа [+] [=] [^]

         :Cargo:nClr_Fltr   := CLR_YELLOW      // цвет фона колонки таблицы с фильтром
         :Cargo:aColFilter  := {}              // колонки таблицы с фильтром
         :Cargo:aColNumFltr := {}              // номера колонок таблицы с фильтром
         :Cargo:cBrwFilter  := ""              // строка фильтра по всем колонки таблицы
         :Cargo:cSuperHead  := ""              // титул суперхидера
         :Cargo:nHSuperHead := 0               // высота титула суперхидера

      ENDIF
   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwSetting( oBrw, aTsbFont )   // настройки таблицы
   LOCAL nCol, cCol, lRet, cFontHead := aTsbFont[2]
   LOCAL cFontFoot := aTsbFont[3]

   WITH OBJECT oBrw

      :nHeightCell    += 6              // добавим пикселей к высоте ячеек
      :nHeightHead    := GetFontHeight(cFontHead) * 2       // высота шапки
      :nHeightFoot    := GetFontHeight(cFontFoot)           // высота подвала
      :nHeightSpecHd  := 24    // высота картинки           // высота спецхидера ENUMERATOR

      :aColumns[1]:hFont := GetFontHandle(cFontHead  )      // 1-ю колонку ставим Bold-фонт
      :aColumns[2]:hFont := GetFontHandle("TsbOneCol")      // 2-ю колонку ставим мой фонт
      IF oBrw:Cargo:nTable > 3
         FOR nCol := 1 TO Len(:aColumns)
            cCol := :aColumns[ nCol ]:cName
            IF cCol == "CENA_ALL" .OR. cCol == "CENAMAST"
              :aColumns[nCol]:hFont := GetFontHandle("TsbOneCol")   // ставим мой фонт на колонку
            ENDIF
         NEXT
      ENDIF

      // Изменить фонт: 1 = Cells, 2 = Headers, 3 = Footers, 4 = SuperHeaders
      //:ChangeFont( cFontHead, 5 , 1 )     // меняем фонт ячеек таблицы 5-го столбца на aStaticFont[2]
      //:ChangeFont( aTsbFont[ 3 ], , 2 )   // меняем фонт шапки таблицы на ...
      FOR nCol := 1 TO Len(:aColumns)
         cCol := :aColumns[ nCol ]:cName
         IF     cCol == "SELECTOR"
         ELSEIF cCol == "ORDKEYNO"
         ELSE
            :ChangeFont( aTsbFont[ 6 ], nCol, 3 )   // меняем фонт подвала таблицы на TsbBoldMini
         ENDIF
      NEXT

      // обработка нажатий клавиш для обработки - здесь не использую
      //:bKeyDown := { |nKey,nFalgs,ob| myKeyAction(nKey, 0, nFalgs, ob) }

      // Двойной клик мышки везде
      //:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :bLDblClick := {|p1,p2,p3,ob|
                       Local nRow := ob:GetTxtRow( p1 )
                       Local nCol := ob:nAtColActual( p2 )
                       p3 := p1 > ob:nHeightSuper
                       ? "=>", nRow, nCol, p3, p1, ob:nHeightSuper
                       DO EVENTS
                       IF     nRow > 0      // Cell
                          ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 )
                       ELSEIF nRow ==  0
                          IF p3             // Header
                             _wPost(801, ob:cParentWnd, {ob, nRow, nCol, p1, p2, p3})
                          ELSE              // Super Header
                             _wPost(801, ob:cParentWnd, {ob, nRow, nCol, p1, p2, p3})
                          ENDIF
                       ELSEIF nRow == -1    // Footer
                          _wPost(801, ob:cParentWnd, {ob, nRow, nCol, p1, p2, p3})
                       ELSEIF nRow == -2    // SpecHd
                          _wPost(801, ob:cParentWnd, {ob, nRow, nCol, p1, p2, p3})
                       ENDIF
                       Return Nil
                     }

      (ThisWindow.Object):Event( 801, {|ow,ky,ap|
                                       Local oBrw := ap[1]   // доступен объект таблицы
                                       Local nRow := ap[2]
                                       Local nCol := ap[3]
                                       Local nPixelRow, nPixelCol, cWnd, cBrw, lHeader, cMsg
                                       nPixelRow := ap[4]
                                       nPixelCol := ap[5]
                                       lHeader   := ap[6]
                                       cMsg := hb_ntos(ky)+" bLDblClick: "+hb_ntos(nRow)+", "+hb_ntos(nCol)
                                       cWnd := ow:Name
                                       cBrw := oBrw:cControlName
                                       IF nRow == 0
                                          IF lHeader ; cMsg += " Header "
                                             // вешаем обработку шапки таблицы
                                             //MsgBox(cMsg, "INFO: "+cBrw)
                                             myBrwHeadClick(3,oBrw,nPixelRow, nPixelCol,oBrw:nAt)
                                          ELSE       ; cMsg += " Super Head"
                                             // вешаем обработку СуперХидера таблицы
                                             //MG_Debug(nPixelRow, nPixelCol, cWnd, cBrw, lHeader, cMsg)
                                             myBrwHeadClick(3,oBrw,nPixelRow, nPixelCol,oBrw:nAt)
                                          ENDIF
                                       ENDIF
                                       Return Nil
                                      } )

      // обработка клавиши ESC
      :UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F.  })
      // обработка клавиши ENTER
      :UserKeys(VK_RETURN, {|ob,nky,cky| lRet := myRecnoEnter(ob,nky,cky), lRet })

      // редактировать ячейку таблицы - выборочно
      :UserKeys(VK_F4 ,    {|ob,nky,cky| lRet := myRecnoEnter(ob,nky,cky), lRet })
      :nFireKey := VK_F4   // KeyDown default Edit

      // проверка фонтов по клавише F12
      :UserKeys(VK_F12,    {|ob| myBrwInfoFont( ob )   })
      // инфо по списку колонок
      :UserKeys(VK_F2 ,    {|ob| myBrwListColumn( ob ) })
      // инфо по текущй записи
      :UserKeys(VK_F3 ,    {|ob| myBrwInfoRecno( ob ) })

      :SetAppendMode( .F. )    // запрещена вставка записи в конце базы стрелкой вниз
      //:SetDeleteMode( .F. )  // удаление записи запрещено

      // кнопка для удаления, будет работать и на восстановление
      :SetDeleteMode( .T., .F., {|| MG_YesNo( "ВНИМАНИЕ !;;" + iif((oBrw:cAlias)->(Deleted()) ,;
                                              "Восстановить", "Удалить") + ;
                                              " запись в таблице ?;", "Подтверждение") } )

      // удалить/добавить запись через событие - резерв
      //:UserKeys(VK_INSERT, {|ob| _wPost(71, ob, ob), .F. }) // кнопка Insert
      //:UserKeys(VK_DELETE, {|ob| _wPost(72, ob, ob), .F. }) // кнопка Delete
      //(ThisWindow.Object):Event( 71, {|ob,ob| RecnoInsert(ob)        } )
      //(ThisWindow.Object):Event( 72, {|ob,ob| RecnoDeleteRecover(ob) } )

      // удалить/добавить запись - работает
      :UserKeys(VK_INSERT, {|ob| RecnoInsert(ob)       , .F. })
      :UserKeys(VK_DELETE, {|ob| RecnoDeleteRecover(ob), .F. })

      //:bGotFocus := {|ob| myGotFocusTsb(ob)     }   // резерв
      //:bOnDraw   := {|ob| SayStatusBar(ob)      }   // показ StatusBar - Recno/Column

   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwDelColumn(oBrw)      // убрать колонки из отображения
   LOCAL nCol, aHideCol := {}
   LOCAL aCol := oBrw:aColumns
   LOCAL cDelCol, oCol, cCol

   // удаляемая колонка
   cDelCol := LOWER("Not-show")        // -> ListTables.prg

   // уберем колонки
   FOR nCol := 1 TO Len(aCol)
      oCol := aCol[ nCol ]
      cCol := LOWER( oCol:cHeading )
      IF cCol == cDelCol
         AADD( aHideCol , nCol )
      ENDIF
   NEXT

   IF Len(aHideCol) > 0
      oBrw:HideColumns( aHideCol ,.t.)   // скрыть колонки
   ENDIF

RETURN Nil

//////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwColumnWidth( oBrw )  // изменение показа ширины колонок
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

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwMaskBmp(oBrw)                   // маска показа картинок
   LOCAL oCol, cCol

   // изменение маски показа картинок в таблице
   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      //IF oCol:lVisible
         //oCol:nBmpMaskHead := 0x00CC0020    // SRCCOPY - резерв
         //oCol:nBmpMaskFoot := 0x00CC0020    // SRCCOPY - резерв
         oCol:nBmpMaskHead   := 0x00BB0226    // MERGEPAINT
         oCol:nBmpMaskFoot   := 0x00BB0226    // MERGEPAINT
         oCol:nBmpMaskSpcHd  := 0x00CC0020    // SRCCOPY
         //oCol:nBmpMaskCell := 0x00CC0020    // SRCCOPY - ячейки таблицы пропустить
         //oCol:nBmpMaskCell := 0x00BB0226    // MERGEPAINT - ячейки таблицы
      //ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
// ENUMERATOR по порядку сделаем свой
STATIC FUNCTION myBrwEnum( oBrw, nOneCol )
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

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwColorChange( oBrw, nTable )          // цвета изменить
   LOCAL o, oCol, nBC1Col, cCol, cTyp

   o        := oBrw:Cargo          // получить данные из объекта
   nBC1Col  := o:nClr_2Col1        // цвет фона колонки 1-2

   IF nTable == 3 .OR. nTable == 4
      //oBrw:GetColumn("ORDKEYNO"):nClrBack := nBC1Col
   ENDIF
   oCol := oBrw:GetColumn("ORDKEYNO")
   oCol:nClrBack := nBC1Col

   // изменение картинки для удалённых записей в колонке ORDKEYNO
   oCol:uBmpCell := {|nc,ob| nc:=nil, iif( (ob:cAlias)->(Deleted()), ob:aBitMaps[2], ob:aBitMaps[1] ) }

   //oCol := oBrw:GetColumn("SELECTOR")  // это не будет работать, т.к.
   //oCol:nClrBack := nBC1Col            // колонки SELECTOR ещё нет

   // изменение цвета спецхидера - ENUMERATOR (нумерация колонок)
   FOR EACH oCol IN oBrw:aColumns
      oCol:nClrSpcHdBack := nBC1Col     // ::aColorsBack[ 18 ]
      oCol:nClrSpcHdFore := CLR_BLACK   // ::aColorsBack[ 19 ]
   NEXT

   // Левый край TBROWSE
   oBrw:nClrHeadBack := oBrw:Cargo:nClr_2_2

   // заменяем на блок кода вызова функции
   oBrw:SetColor( {1}, { { |nr,nc,ob| BrwColorForeCell(nr,nc,ob) } } ) // 1 , текста в ячейках таблицы
   oBrw:SetColor( {2}, { { |nr,nc,ob| BrwColorBackCell(nr,nc,ob) } } ) // 2 , фона в ячейках таблицы

   // цвет фона шапки таблицы + подвала для добавочного списка колонок
   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      cTyp := oCol:cFieldTyp
      IF cCol == "COUNTRY"
         oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }
         oCol:nClrFootBack := { |n,b  | myTsbColorBackHead(n,b) }
      ENDIF
      IF cTyp $ "+=^"   // Type: [+] [=] [^]
         oCol:nClrHeadBack := { |n,b  | myTsbColorBackHead(n,b) }
         oCol:nClrFootBack := { |n,b  | myTsbColorBackHead(n,b) }
      ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
// обработка шапки и подвала и спецхидера(нумератора) таблицы
STATIC FUNCTION myBrwHeaderFooterSpcHd(oBrw,aFAlign)
   LOCAL nI, oCol, cCol

   /* смотреть \SOURCE\TsBrowse\TSCOLUMN.PRG
   // Click Event - клик мышкой
   DATA bFLClicked   // Блок для оценки в нижнем колонтитуле, щелкнув левой кнопкой мыши
   DATA bFRClicked   // Блок для оценки в нижнем колонтитуле, щелкнув правой кнопкой мыши
   DATA bHLClicked   // Блок для оценки по заголовку(шапка-таблицы), щелкнув левой кнопкой мыши
   DATA bHRClicked   // Блок для оценки в заголовке(шапка-таблицы), щелкнув правой кнопкой мыши
   DATA bSLClicked   // Блок для оценки в специальном заголовке(нумератор), щелкнув левой кнопкой мыши
   DATA bSRClicked   // Блок для оценки в специальном заголовке(нумератор), щелкнув правой кнопкой мыши
   DATA bLClicked    // Блок для оценки при щелчке левой кнопкой мыши по ячейке
   */
   FOR nI := 1 TO Len( oBrw:aColumns )
      oCol := oBrw:aColumns[ nI ]
      cCol := oCol:cName
      IF ISARRAY(aFAlign)
         oCol:nFAlign := DT_CENTER //aFAlign[ nI ]
      ENDIF
      IF cCol == "ORDKEYNO" .OR. cCol == "SELECTOR"
      ELSE
         // картинка в шапке колонок таблицы - стрелка_вниз  20x20
         // {|| hArrDown } - так нельзя
         oCol:uBmpHead  := {|nc,ob| nc := ob:Cargo, nc:hArrDown20 }
         oCol:nHAlign   := nMakeLong( DT_CENTER, DT_RIGHT  )
         // картинка в подвале колонок таблицы - стрелка_вверх 20x20
         oCol:uBmpFoot  := {|nc,ob| nc := ob:Cargo, nc:hArrUp20  }
         oCol:nFAlign   := nMakeLong( DT_CENTER, DT_RIGHT  )
         // картинка в нумераторе колонок таблицы - стрелка_вниз  20x20
         oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hArrDown20   }
         oCol:nSAlign   := nMakeLong( DT_CENTER, DT_RIGHT  )
      ENDIF
      // настройка для шапки и СуперХидера таблицы
      oCol:bHLClicked := {|nrp,ncp,nat,obr| myBrwHeadClick(1,obr,nrp,ncp,nat) }
      oCol:bHRClicked := {|nrp,ncp,nat,obr| myBrwHeadClick(2,obr,nrp,ncp,nat) }
      // настройка для подвала таблицы
      oCol:bFLClicked := {|nRowPix,nColPix,nAt,oBrw| myBrwFootClick(1,nRowPix,nColPix,nAt,oBrw) }
      oCol:bFRClicked := {|nRowPix,nColPix,nAt,oBrw| myBrwFootClick(2,nRowPix,nColPix,nAt,oBrw) }
      // настройка для SpecHd таблицы
      oCol:bSLClicked := {|nrp,ncp,nat,obr| myBrwSpcHdClick(1,nrp,ncp,nat,obr) }
      oCol:bSRClicked := {|nrp,ncp,nat,obr| myBrwSpcHdClick(2,nrp,ncp,nat,obr) }
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwSuperHeader(oBrw,aSupHdr)   // создать СуперХидер таблицы
   LOCAL nI, nFSz, aFont, hFont

   hFont  := GetFontHandle("TsbSuperH")
   aFont  := GetFontParam( hFont )
   nFSz   := aFont[2]

   WITH OBJECT oBrw
      /*
      //---------- команды для создания суперхидера -------------
      Add Super Header To oBrw From Column 1 To Column 1 Title ""

      Add Super Header To oBrw From Column 2 To Column 2 Title "" ;
           BITMAP :Cargo:bArrDown32 HORZ DT_CENTER VERT DT_CENTER

      Add Super Header To oBrw From Column 2 To Column :nColCount() ;
          Title aSupHdr[1] BITMAP :Cargo:bSupHd32 HORZ DT_CENTER VERT DT_CENTER
      */
      // заголовок 1,2,3 таблицы - СуперХидер
      :AddSuperHead( 1, 1, "",,, .F.,,, .F., .F., .F.,, )
      :AddSuperHead( 2, 2, "",,, .F.,, :Cargo:bArrDown32, .F., .F., .F., DT_CENTER, DT_CENTER )
      :AddSuperHead( 2, :nColCount(), aSupHdr[1],,, .F.,, :Cargo:bSupHd32, .F., .F., .F., DT_CENTER, DT_CENTER )

      // менять высоты в таблице, после END TBROWSE ... НЕЖЕЛАТЕЛЬНО,
      // т.к. они фактически "зашиваются" в сетку отрисовки
      // В данном случае задаем высоту суперхидера в 2 строки
      //:nHeightSuper := nFSz * 2 + 10                  // высота заголовка (СуперХидера)

      // Переделано на высоту картинки 40x140 - "bSupHd40"
      :nHeightSuper   := 40 + 2*2                     // высота заголовка (СуперХидера)

      //:nHeightSuper := 0                            // скрыть СуперХидер - если надо

      :Cargo:cSuperHead  := aSupHdr[1]                // сохраним титул суперхидера
      :Cargo:nHSuperHead := :nHeightSuper             // сохраним высоту титула суперхидера

      //SuperHeader oBrw:aSuperHead[ nI, 15 ] - это nBitmapMask для ячейки SuperHead
      // изменение маски показа картинок в суперхидере таблицы
      FOR nI := 1 TO Len( :aSuperHead )
         IF !Empty( :aSuperHead[ nI ][8] )           // uBitMap задан ?
            :aSuperHead[ nI ][15]   := 0x00BB0226    // MERGEPAINT
            //:aSuperHead[ nI ][15] := 0x00CC0020    // SRCCOPY
         ENDIF
      NEXT

      // Высоту картинки можно задавать = высоте суперхидера
      //? "  Высота суперхидера=", :nHeightSuper, ProcNL()

   END WITH

RETURN Nil
///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myRecnoEnter( oBrw, nKey, cKey )  // редактировать запись
   LOCAL o, nTable, lDel, aEdit, cMsg, nCell, nRow, nDop, cCol, nCol, oCol
   LOCAL lEdit, cTyp, lVal, lRet, cVal, oCell, nY, nX, nW, nH, nHMain

   ? PROCNL()
   o      := oBrw:Cargo                    // получить данные из объекта
   nTable := o:nTable                      // номер таблицы
   nHMain := o:nHMain                      // высота главного окна
   lDel   := (oBrw:cAlias)->( DELETED() )  // удалена ли запись ?
   aEdit  := o:aEdit                       // массив данных для редактирования колонок
   nCell  := oBrw:nCell                    // номер ячейки/колонки в таблице
   nRow   := oBrw:nAt                      // номер строки в таблице
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oCell:nRow + oBrw:nHeightHead + 4 + nHMain
   nX     := oCell:nCol
   nW     := oCell:nWidth
   nH     := oCell:nHeight

   nDop := 0
   FOR nCol := 1 TO 3
      cCol := oBrw:aColumns[ nCol ]:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         nDop ++
      ENDIF
   NEXT

   IF lDel
      cMsg := "Запрещено редактировать удаленные записи !;;"
      cMsg += ProcNL()
      MG_Stop(cMsg)
      Return .F.
   ENDIF

   oCol := oBrw:aColumns[ nCell ]
   cTyp := oCol:cFieldTyp
   cCol := oCol:cName
   IF cTyp $ "+=^"   // Type: [+] [=] [^]
      cMsg := "Запрещено редактировать поле [" + cCol + "] этого типа !;;"
      cMsg += ProcNL()
      MG_Stop(cMsg)
      Return .F.
   ENDIF

   IF nCell <= 2
      cMsg := "Запрещено редактировать колонку [" + cCol + "] в таблице !;;"
      cMsg += ProcNL()
      MG_Stop(cMsg)
      Return .F.
   ENDIF

   lEdit := aEdit[nCell - nDop]  // с учетом доп.колонок в таблице
   IF !lEdit
      cMsg := "Запрещено редактировать поле [" + cCol + "] в массиве aEdit[] !;;"
      cMsg += ProcNL()
      MG_Stop(cMsg)
      Return .F.
   ENDIF

   IF nTable == 5
      cMsg := "Таблицу номер 5 - запрещено редактировать .....;;"
      cMsg += ProcNL()
      MG_Stop(cMsg)
      Return .F.
   ENDIF

   If cTyp == "L"
      lVal := oBrw:GetValue(cCol)
      IF !oBrw:lCheckBoxAllReturn         // если .F. в настройках myBrwInit()
         IF (oBrw:cAlias)->(RLock())      // то делать самому
            oBrw:SetValue( cCol, !lVal )
            (oBrw:cAlias)->(dbUnLock())
            oBrw:DrawSelect()             // перерисовать текущую ячейку таблицы
         ENDIF
      ENDIF
   EndIf

   // lRet := myTableEnterGeneral(oBrw)

   IF nTable == 1 .AND. cCol == "COUNTRY"
      IF nKey == VK_F4
         // можно редактировать
         cVal := "F4 - можно редактировать"
      ELSE
         cVal := myMenuListCountry(oBrw)     // -> menu_list.prg
         IF LEN(cVal) > 0
            IF (oBrw:cAlias)->(RLock())      // то делать самому
               oBrw:SetValue( cCol, cVal )
               (oBrw:cAlias)->(dbUnLock())
               oBrw:DrawSelect()             // перерисовать текущую ячейку таблицы
            ENDIF
         ENDIF
         Return .F.
      ENDIF
   ENDIF

#ifdef _HMG_OUTLOG
   MG_Debug( nKey, cKey,"Номер таблицы=", nTable, "Alias=",oBrw:cAlias,;
            "Deleted()=",lDel,"nCell=",nCell,"nRow=",nRow, "lEdit=",lEdit, cVal,;
             "Координаты ячейки:", nY, nX, nW, nH )
#else
   cKey := NIL
#endif
   lRet := .T.

   //oBrw:aColumns[nCell]:lEdit := .T.

RETURN lRet

///////////////////////////////////////////////////////////////////////////////
// задать цвета таблицы, цвета задаём перед Tsbrowse, замена :SetColor(...)
STATIC FUNCTION myBrwGetColor(nTable,nPane,nPane2,nPane3,nHead1,nHead2)
   LOCAL aColors, nBCSpH
   DEFAULT nHead1 := 0 , nHead2 := 0

   // nPane  // цвет фона таблицы
   // nPane2 // цвет фона таблицы через одну строку
   // nPane3 // цвет фона таблицы удалённых записей

   nBCSpH := GetSysColor( COLOR_BTNFACE )     // цвет фона спецхидера таблицы
   IF nHead1 == 0
      nHead1 := ToRGB( { 40, 122, 237 } )     // голубой цвет
   ENDIF
   IF nHead2 == 0
      nHead2 := ToRGB( { 48,  29,  26 } )     // серо-черный фон
   ENDIF

   aColors := {}
   AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK          } } )            // 1 , текста в ячейках таблицы
   AAdd( aColors, { CLR_PANE  , {|| nPane              } } )            // 2 , фона в ячейках таблицы

   // ---- так не работает, нужно делать через блок кода
   //AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), nPane3 ,;
   //                                         iif( ob:nAt % 2 == 0, nPane2, nPane ) )   } } )

   // ---- заменяем на блок кода вызова функции - так тоже не работает, не определено oBrw:Cargo
   //AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| BrwColorForeCell(nr,nc,ob) } } ) // 1 , текста в ячейках таблицы
   //AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| BrwColorBackCell(nr,nc,ob) } } ) // 2 , фона в ячейках таблицы
   nPane2 := nPane3  // уберём ошибку компиляции

   AAdd( aColors, { CLR_HEADF , {|| ToRGB( YELLOW )    } } )            // 3 , текста шапки таблицы
   AAdd( aColors, { CLR_HEADB , {|| { nHead1, nHead2 } } } )            // 4 , фона шапки таблицы
   AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLACK } } )                     // 5 , текста курсора, текст в ячейках с фокусом

   //AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, ; // 6 , фона курсора
   //                         CLR_HRED, { RGB( 163, 163, 163 ), RGB( 127, 127, 127 ) } ) } } )
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, ;
                                          -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора

   AAdd( aColors, { CLR_EDITF , {|| CLR_RED    } } )                   // 7 , текста редактируемого поля
   AAdd( aColors, { CLR_EDITB , {|| CLR_YELLOW } } )                   // 8 , фона редактируемого поля
   AAdd( aColors, { CLR_FOOTF , {|| ToRGB( YELLOW )       } } )        // 9 , текста подвала таблицы
   AAdd( aColors, { CLR_FOOTB , {|| { nHead1, nHead2 }    } } )        // 10, фона подвала таблицы
   AAdd( aColors, { CLR_SELEF , {|| CLR_GRAY   } } )                   // 11, текста неактивного курсора (selected cell no focused)
   AAdd( aColors, { CLR_SELEB , {|| { RGB(255,255,74), ;               // 12, фона неактивного курсора (selected cell no focused)
                                         RGB(240,240, 0) } } } )
   AAdd( aColors, { CLR_ORDF  , {|| CLR_WHITE  } } )                   // 13, текста шапки выбранного индекса
   AAdd( aColors, { CLR_ORDB  , {|| CLR_RED    } } )                   // 14, фона шапки выбранного индекса
   AAdd( aColors, { CLR_LINE  , {|| CLR_GRAY   } } )                   // 15, линий между ячейками таблицы
   AAdd( aColors, { CLR_SUPF  , {|| nBCSpH     } } )                   // 16, фона спецхидер
   //AAdd( aColors, { CLR_SUPF  , {|| { CLR_WHITE, nHead1 }  } } )     // 16, фона спецхидер
   AAdd( aColors, { CLR_SUPB  , {|| CLR_RED    } } )                   // 17, текста спецхидер

   IF nTable == 1  // можно менять цвета в зависимости от таблицы
   ENDIF

RETURN aColors

///////////////////////////////////////////////////////////////////
// 1 , текст в ячейках таблицы
// пример для раскраски таблицы по колонке CENAMAST
STATIC FUNCTION BrwColorForeCell( nAt, nCol, oBrw )
   LOCAL nColor, nSum, o, nTable, nText, lDel, cCol, nBC1Col
   Default nAt := 0 , nCol := 0

   o       := oBrw:Cargo                      // получить данные из
   nTable  := o:nTable                        // номер таблицы
   nText   := o:nClr_1                        // цвет ячеек таблицы
   nBC1Col := o:nClr_2Col1                    // цвет фона колонки 1-2
   lDel    := (oBrw:cAlias)->( DELETED() )    // удалена ли запись ?

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nText
   ELSE

      IF nTable > 3  // это в качестве примера как можно делать

         If Len(oBrw:aColumns) > 0
            nSum := Eval( oBrw:GetColumn('CENAMAST'):bData )
         EndIf

         // обработка ошибочной ситуации
         IF VALTYPE(nSum) != "N"
            RETURN CLR_HGRAY
         ENDIF

         cCol := oBrw:aColumns[ nCol ]:cName
         IF cCol == 'CENAMAST'
            IF nSum <= -1500
               nColor := CLR_HRED
            ELSEIF nSum < 0
               nColor := CLR_RED
            ELSEIF nSum <= 100
               nColor := CLR_GREEN
            ELSEIF nSum > 100
               nColor := CLR_BLUE
               nColor := CLR_BLUE
            ENDIF
         ELSE
            nColor := nText
         ENDIF

      ELSE
         // для nTable = 1 и 2
         nColor := nText
      ENDIF

      // это правило действует всегда
      IF lDel // удалена ли запись ?
         nColor := CLR_HGRAY
      ENDIF

   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////////////////
// 2 , фона в ячейках таблицы
STATIC FUNCTION BrwColorBackCell( nAt, nCol, oBrw )
   LOCAL nColor, lDel, o, nTable, nTBC, nTBC1, nTBC2, nTBCdel, nBC1Col, cCol
   LOCAL nBCFltr, aColFltr, nJ
   Default nAt := 0, nCol := 0

   o        := oBrw:Cargo                      // получить данные из
   nTable   := o:nTable                        // номер таблицы
   nTBC     := o:nClr_2                        // цвет фона таблицы
   nTBC1    := o:nClr_2_1                      // цвет чётная\нечётная row
   nTBC2    := o:nClr_2_2                      // цвет чётная\нечётная row
   nTBCdel  := o:nClr_2_Del                    // цвет удалённых записей
   nBC1Col  := o:nClr_2Col1                    // цвет фона колонки 1-2
   nBCFltr  := o:nClr_Fltr                     // цвет фона колонки таблицы с фильтром
   aColFltr := o:aColNumFltr                   // номера колонок таблицы с фильтром
   lDel     := (oBrw:cAlias)->( DELETED() )    // удалена ли запись ?

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nBC1Col
   ELSE

      IF nTable == 4 .OR. nTable == 5  // это в качестве примера как можно делать
         IF oBrw:nAt % 2 == 0
            nColor := nTBC2
         ELSE
            nColor := nTBC1
         ENDIF
      ELSE
         nColor := nTBC
      ENDIF

      // если есть фильтр на колонке
      IF LEN(aColFltr) > 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nCol
               nColor := nBCFltr
            ENDIF
         NEXT
      ENDIF

      // это правило действует всегда
      IF lDel                 // удалена ли запись ?
         nColor := nTBCdel
      ENDIF

   ENDIF

RETURN nColor

///////////////////////////////////////////////////////////////////////////////
// 4 + 10 , фона шапки/подвала в таблице
STATIC FUNCTION myTsbColorBackHead( nCol, oBrw )
   LOCAL o, cName, nColor, cType, nBCxC10, nBCxBlck, nClr_4_1, nClr_4_2

   o        := oBrw:Cargo              // получить данные из контейнера
   nBCxC10  := o:nClr_2xC10            // цвет фона шапки 1-таблицы:10-колонки - поле COUNTRY
   nBCxBlck := o:nClr_2xBlck           // цвет фона шапки для полей типа [+] [=] [^]
   nClr_4_1 := o:nClr_4_1              // цвет фона шапки таблицы: градиент
   nClr_4_2 := o:nClr_4_2              // цвет фона шапки таблицы: градиент
   cName    := oBrw:aColumns[nCol]:cName
   cType    := oBrw:aColumns[nCol]:cFieldTyp

   IF cName == "COUNTRY"
      nColor := { nBCxC10, nClr_4_2 }
   ELSE
      nColor := { nClr_4_1, nClr_4_2 }
   ENDIF

   IF cType $ "+=^"   // Type: [+] [=] [^]
      nColor := { nBCxBlck, nClr_4_2 }
   ENDIF

RETURN nColor

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySetNoHoles(oBrw)
   LOCAL nHole, nProc
   /*
   ? ProcNL() , "Дырка в конце таблицы "
   ? "nHeightSuper =", oBrw:nHeightSuper
   ? "nHeightHead  =", oBrw:nHeightHead
   ? "nHeightSpecHd=", oBrw:nHeightSpecHd
   ? "nHeightFoot  =", oBrw:nHeightFoot
   ? "nHeightCell  =", oBrw:nHeightCell
   */
   nHole := oBrw:SetNoHoles(1, .F.)  // расчёт высоты дырки, без изменений высот
   //!!!  nHole это то, что надо разделить между
   //!!!  заголовками :nHeightSuper, :nHeightHead, :nHeightSpcHd, :nHeightFoot

   // В данном случае, для запрета изменения высоты :nHeightSuper и :nHeightSpcHd
   // так как там используются картинки, то nHole делю на 2 элемента
   nProc := 0.5                      // 2 элемента
   //? "nHole=", nHole, "nProc=", nProc
   oBrw:nHeightHead   += INT(nHole * nProc)   // добавить в высоту шапки
   oBrw:nHeightFoot   += INT(nHole * nProc)   // добавить в высоту подвала
   //oBrw:nHeightSuper  +=
   //oBrw:nHeightSpecHd +=
   /*
   ? "    После добавления пикселов в высоту шапки + подвала:", INT(nHole * nProc)
   ? "nHeightSuper =", oBrw:nHeightSuper
   ? "nHeightHead  =", oBrw:nHeightHead
   ? "nHeightSpecHd=", oBrw:nHeightSpecHd
   ? "nHeightFoot  =", oBrw:nHeightFoot
   ? "nHeightCell  =", oBrw:nHeightCell
   */
   nHole := oBrw:SetNoHoles(1, .F.)  // расчёт высоты дырки, без изменений высот
   //? "nHole=", nHole
   IF nHole >= 1
      oBrw:nHeightFoot += nHole  // добавим остаток в подвал
      //? "  добавим остаток в высоту подвала - :nHeightFoot  =", oBrw:nHeightFoot
   ENDIF

   nHole := oBrw:SetNoHoles(1)  //!!! расчёт высоты дырки производим с учетом изменений (проверка)
   //? "New nHole =", nHole

RETURN nHole

//////////////////////////////////////////////////////////////////////////////
// новая запись в базе добавляется в конец базы и переходим сразу к редактированию
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL nRecno

   IF MG_YesNo( "Вставить запись в таблицу ?", "Добавление записи" )
      // добавить в поле DT_ADD дату+время вставки записи
      oBrw:bAddAfter  := {|ob,ladd|
                           If ladd
                              (ob:cAlias)->( dbSkip(0) )
                              //(ob:cAlias)->DT_ADD := (ob:cAlias)->TS
                           EndIf
                           Return Nil
                         }

      // встроенный метод для добавления записи
      oBrw:AppendRow(.T.)

      oBrw:bAddAfter  := Nil

      IF (oBrw:cAlias)->(RLock())
         //(oBrw:cAlias)->DT_USER := M->nPubUser     // кто вставил запись
         //(oBrw:cAlias)->IM      := hb_DateTime()   // когда изменили запись
         (oBrw:cAlias)->(DBUnlock())
      ENDIF
      (oBrw:cAlias)->(DbCommit())

      nRecno := (oBrw:cAlias)->( RecNo() )
      ? ProcNL(), "Insert=", nRecno

      oBrw:nCell := 3  // в начало колонок для редактирования
      oBrw:Setfocus()
      DO EVENTS

   ENDIF

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION RecnoDeleteRecover(oBrw, lSet)
   LOCAL lChange, cChange, nAt, lDelete, nRecno, nRecnew
   DEFAULT lSet := .F.

   IF lSet

      // Ставятся для блоков, смотри по Eval(...), т.е.
      // oBrw:bDelBefore := {|rec,obr| ... }
      // oBrw:bDelAfter  := {|rec,obr| ... }
      // oBrw:bDelete    := {|rec,obr: ... }
      // oBrw:bPostDel   := {|obr    | ... }
      // На удалении и восстановлении работает блок :bPostDel

      oBrw:Cargo:nRecnoDeleteRecover := 0
      oBrw:bPostDel := {|ob|
                        Local nRec := ob:Cargo:nRecnoDeleteRecover  // запись на которой были до :DeleteRow()
                        Local cAls := ob:cAlias
                        Local nOld := (cAls)->( RecNo() )
                        Local lDel
                        If nRec > 0
                           (cAls)->( dbGoto( nRec ) )
                           lDel := (cAls)->( Deleted() )   // .T. - запись удалена
                           If (cAls)->( RLock() )
                              // если нужна запись в базу даты+время для этих действий
                              //If lDel ; (cAls)->DT_DEL  := hb_DateTime()
                              //Else    ; (cAls)->DT_REST := hb_DateTime()
                              //EndIf
                              (cAls)->( DbUnLock() )
                           EndIf
                           (cAls)->( dbGoto( nOld ) )
                        EndIf
                        Return nil
                       }
      RETURN Nil
   ENDIF

   oBrw:Cargo:nRecnoDeleteRecover := (oBrw:cAlias)->(RecNo())

   nAt     := oBrw:nAt     // для dbf :nAt лучше не использовать
   lDelete := (oBrw:cAlias)->(Deleted())
   nRecno  := (oBrw:cAlias)->(RecNo())

   // удаление/восстановление записи разрешена !!!
   // встроенный метод для удаления текущей записи
   lChange := oBrw:DeleteRow(.F., .T.)

   DO EVENTS

   IF lChange             // изменение было
      nRecnew := (oBrw:cAlias)->(RecNo())
      (oBrw:cAlias)->(dbGoto(nRecno))
      cChange := iif( lDelete, "Recover ", "Delete " )
      ? ProcNL()
      ? "    ...",hb_DateTime(), "cChange=", cChange, "nRecno=",nRecno
      ?? "Deleted=",(oBrw:cAlias)->(Deleted())
      (oBrw:cAlias)->(dbGoto(nRecnew))
   ENDIF

   oBrw:Cargo:nRecnoDeleteRecover := 0
   oBrw:DrawLine()     // перерисовать текущую строку таблицы
   oBrw:SetFocus()
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////////
// контекстное меню ТСБ / TSB context menu
STATIC FUNCTION myBrwContextMenu(oBrw)
   LOCAL nI, nJ, aTsbMnu, nTsbMnu, cTsbMnu, hFont1, hFont2, hWnd, cMenu, cImage

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )
   SetThemes(1)               // тема "Office 2000 theme"
   //SetThemes(2)             // тема SILVER

   hFont1 := GetFontHandle( "TsbNorm" )
   hFont2 := GetFontHandle( "TsbBold" )
   hWnd   := GetFormHandle(oBrw:cParentWnd)

   // в качестве примера - обычное контекстное меню таблицы
   //DEFINE CONTEXT MENU CONTROL &(oBrw:cControlName)
   //   MENUITEM "F2  - Инфо по колонкам таблицы" ACTION {|| myBrwListColumn(oBrw)} NAME 0081 FONT hFont1 IMAGE "Dbg32"
   //   MENUITEM "F3  - Инфо по текущй записи"    ACTION {|| myBrwInfoRecno(oBrw) } NAME 0082 FONT hFont1 IMAGE "Dbg32"
   //   MENUITEM "F12 - Инфо по фонтам таблицы"   ACTION {|| myBrwInfoFont(oBrw)  } NAME 0083 FONT hFont1 IMAGE "Dbg32"
   //   SEPARATOR
   //   MENUITEM "Список открытых БД"     ACTION {|| myGetAllUse()                      } FONT hFont1 NAME 0084 IMAGE "bBase32"
   //   MENUITEM "Текущая база"           ACTION {|| Base_Tek()                         } FONT hFont1 NAME 0085 IMAGE "bBase32"
   //   MENUITEM "Set relation этой базы" ACTION {|| MG_Info( Base_Relation( ALIAS() )) } FONT hFont1 NAME 0086 IMAGE "bBase32"
   //   MENUITEM "DbFilter этой базы"     ACTION {|| Darken2Open(hWnd) ,;
   //                                                MG_Info( "DbFilter() этой базы: " +;
   //                                                  (oBrw:cAlias)->( DbFilter() )) ,;
   //                                                Darken2Close(hWnd)                 } FONT hFont1 NAME 0087 IMAGE "bBase32"
   //   MENUITEM "Фильтр БД (Cargo)"      ACTION {|| Darken2Open(hWnd) ,;
   //                                                MG_Debug("Фильтр строка в Cargo:",;
   //                                                oBrw:Cargo:cBrwFilter    ,;
   //                                                oBrw:Cargo:aColFilter    ,;
   //                                                oBrw:Cargo:aColNumFltr)  ,;
   //                                                Darken2Close(hWnd)                 } FONT hFont1 NAME 0088 IMAGE "bBase32"
   //   SEPARATOR
   //   MENUITEM "Выход"   ACTION Nil  NAME 0089  FONT hFont2
   //END MENU

   nTsbMnu  := 80
   aTsbMnu  := { " F2  - Инфо по колонкам таблицы", " F3  - Инфо по текущй записи",;
                 " F12 - Инфо по фонтам таблицы"  , "SEPARATOR"                   ,;
                 " Список открытых БД"            , " Текущая база"               ,;
                 " Set relation этой базы"        , " Dbfilter этой базы"         ,;
                 " Фильтр БД (Cargo)"         }

   DEFINE CONTEXT MENU CONTROL &(oBrw:cControlName)

      nJ := 1
      FOR nI := 1 TO Len(aTsbMnu)
         cMenu   := aTsbMnu[ nI ]
         IF cMenu == "" .OR. cMenu == "SEPARATOR"
            SEPARATOR
         ELSE
            cTsbMnu := StrZero(nTsbMnu + nJ, 4)
            cImage := IIF( "F" $ cMenu, "Dbg32", "bBase32" )
            MENUITEM cMenu ACTION _wPost(80, ,This.Name) NAME &(cTsbMnu) FONT hFont1 IMAGE cImage
            nJ++
         ENDIF
      NEXT
      (ThisWindow.Object):Event( 80, {|ow,ky,cnam| myTsbContextMnu(ow,ky,cnam) } )
      SEPARATOR
      MENUITEM  "Exit"  ACTION {|| Nil } FONT hFont2 NAME 0089

   END MENU

   oBrw:SetFocus()

RETURN Nil

///////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbContextMnu(ow, ky, cItm)
   LOCAL oBrw  := (ThisWindow.Object):Cargo:oBrw     // получить данные из объекта
   LOCAL hWnd, cForm := ow:Name

   IF ISOBJECT(oBrw)
      hWnd := GetFormHandle(oBrw:cParentWnd)
      ky   := VAL(cItm)
      Darken2Open(hWnd)
      IF     ky == 81
         myBrwListColumn(oBrw)
      ELSEIF ky == 82
         myBrwInfoRecno(oBrw)
      ELSEIF ky == 83
         myBrwInfoFont(oBrw)
      ELSEIF ky == 84
         myGetAllUse()                             // Список открытых БД ->  util_InfoDbf.prg
      ELSEIF ky == 85
         DBSELECTAREA(oBrw:cAlias)
         Base_Tek()                                // Текущая база ->  util_InfoDbf.prg
      ELSEIF ky == 86
         MG_Info( Base_Relation( ALIAS() ) )       // Set relation этой базы ->  util_InfoDbf.prg
      ELSEIF ky == 87
         MG_Info( "DbFilter() этой базы: " + ;
                   (oBrw:cAlias)->( DbFilter() ))  // DbFilter этой базы ->  util_InfoDbf.prg
      ELSEIF ky == 88
#ifdef _HMG_OUTLOG
         MG_Debug( "Фильтр строка в Cargo:"  ,;    // Фильтр БД (Cargo) ->  util_InfoDbf.prg
                   oBrw:Cargo:cBrwFilter     ,;
                   "Массив условий по колонкам:", oBrw:Cargo:aColFilter ,;
                   "Массив колонкок с фильтром:", oBrw:Cargo:aColNumFltr )
#endif
      ENDIF
      Darken2Close(hWnd)
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// обработка шапки и суперхидера таблицы
STATIC FUNCTION myBrwHeadClick( nClick, oBrw, nRowPix, nColPix, nAt )
   LOCAL cForm, nRow, nCell, cNam, cName, nCol, nIsHS, nLine, oCol
   LOCAL nY, nX, cMsg1, cMsg2, cMsg3, aMsg, nVirt, cCol, nV
   LOCAL cVirt, aNam, aMenu, cMenu, cCnr, nCnr

   aNam  := {'Left mouse :OneClick', 'Right mouse :OneClick', 'Left mouse :bLDblClick'}
   aMenu := {'Header - ', 'SuperHeader - '}
   cForm := oBrw:cParentWnd
   nRow  := oBrw:GetTxtRow(nRowPix)                 // номер строки курсора в таблице
   nCol  := Max(oBrw:nAtColActual( nColPix ), 1 )   // номер активной колонки курсора в таблице
   nCell := oBrw:nCell                              // номер ячейки в таблице
   nLine := nAt                                     // строка ячейки в таблице
   oCol  := oBrw:aColumns[ nCol ]
   cName := oCol:cName
   nIsHS := iif(nRowPix > oBrw:nHeightSuper, 1, 2)
   cNam  := aNam[ nClick ]
   cMenu := aMenu[ nIsHS ]
   cVirt := ",ORDKEYNO,SELECTOR,"
   cCnr  := ""
   nCnr  := 0

   nY    := GetProperty(cForm, "Row") + GetTitleHeight()
   nX    := GetProperty(cForm, "Col") + GetBorderWidth() - 4
   // возмём координаты от шапки таблицы
   nY    += GetMenuBarHeight() + oBrw:nTop + 2
   nY    += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nY    += IIF( oBrw:lDrawHeaders, oBrw:nHeightHead  , 0 )
   nY    -= IIF( oBrw:lDrawSpecHd , oBrw:nHeightSpecHd, 0 )
   IF nIsHS == 2  // суперхидер таблицы
      nY -= IIF( oBrw:lDrawHeaders, oBrw:nHeightHead  , 0 )
   ENDIF
   nX    += oCol:oCell:nCol
   nX    += IIF( oBrw:lSelector, oBrw:aColumns[1]:nWidth , 0 )  // если есть селектор
   nX    -= 5

   nVirt := 0
   FOR nV := 1 TO 3
      cCol := oBrw:aColumns[ nV ]:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         nVirt ++
      ENDIF
   NEXT

   cMsg1 := cMenu + cNam + ", y:x " + hb_ntos(nRowPix) + ":" + hb_ntos(nColPix)
   cMsg2 := "Head position y/x: " + hb_ntos(nY) + '/' + hb_ntos(INT(nX))
   // номера колонок не совпадают с номерами полей в базе,
   // т.к. есть скрытые/удалённые колонки из таблицы см.функцию myBrwDelColumn()
   // нумерацию берем из нумератора таблицы
   //IF oBrw:nCell > oBrw:nColumn("ORDKEYNO")
      //cCnr := oBrw:aColumns[ oBrw:nCell ]:cSpcHeading - так нельзя
      cCnr := oBrw:aColumns[ nCol ]:cSpcHeading
      nCnr := Val( cCnr )
   //ENDIF
   cMsg3 := "Column header: " + hb_ntos(nCnr) + "  [" + cName + "]"
   // расчёты без удалённых колонок
   //cMsg3 := "Column header: " + hb_ntos(nCol) + " - " + hb_ntos(nVirt)
   //cMsg3 += " = " + hb_ntos(nCol-nVirt) + "  [" + cName + "]"
   aMsg  := { cMsg1, cMsg2, cMsg3 }

   IF cName $ cVirt
      // сделаем отдельное сообщение
      cMsg3 := "Virtual column: " + hb_ntos(nCol) + " [" + cName + "]"
      aMsg  := { cMsg1, cMsg2, cMsg3 }
      // меню шапки виртуальных колонок - можно сделать отдельное меню
      myMenuHeadClick(oBrw, nY, nX, aMsg, nIsHS)
   ELSE
      // меню шапки обычных колонок
      myMenuHeadClick(oBrw, nY, nX, aMsg, nIsHS)
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN Nil

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myMenuHeadClick( oBrw, nY, nX, aMsg, nIsHS )
   LOCAL cForm, hFont1, hFont2, hFont3, aTsbMnu, nI, cTsbMnu, nTsbMnu, cVal

   cForm   := oBrw:cParentWnd
   hFont1  := GetFontHandle( "TsbEdit"   )
   hFont2  := GetFontHandle( "TsbSuperH" )
   hFont3  := GetFontHandle( "TsbBold"   )
   aTsbMnu := myListConstantsImages()           // -> menu_list.prg
   nTsbMnu := 70

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( BRW_BMP_CONTEX_MENU )   // set image size
   SetThemes(1)                                 // "White theme" в ContextMenu

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  aMsg[1] DISABLED  FONT hFont1
       MENUITEM  aMsg[2] DISABLED  FONT hFont1
       IF nIsHS == 2  // SuperHeader
          SEPARATOR
          FOR nI := 1 TO Len(aTsbMnu)
             cTsbMnu := StrZero(nTsbMnu + nI, 4)
             cVal    := aTsbMnu[ nI,1 ] //+ " - константа показа картинок в Суперхидере"
             MENUITEM cVal ACTION _wPost(70, ,This.Name) NAME &(cTsbMnu) FONT hFont1 IMAGE "Dbg32"
          NEXT
       ENDIF
       SEPARATOR
       MENUITEM  aMsg[3] ACTION  {|| MG_Debug(aMsg[3]) } FONT hFont2
       MENUITEM  "Exit"  ACTION  {|| oBrw:SetFocus() } FONT hFont3
   END MENU
   (ThisWindow.Object):Event( 70, {|ow,ky,cnam| myConstImageSuperHead(ow,ky,cnam,aTsbMnu) } )

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   oBrw:SetFocus()
   oBrw:DrawSelect()

RETURN Nil

//////////////////////////////////////////////////////////////////////////////////
// обработка нумератора(спецхидера) таблицы
STATIC FUNCTION myBrwSpcHdClick( nClick, nRowPix, nColPix, nAt, oBrw )
   LOCAL cForm, nRPos, nAtCol, cNam, cName, cMsg, cCnr, nCnr
   LOCAL oCol, nY, nX, cMsg1, cMsg2, cMsg3, cMsg4, aMsg, nVirt, cCol, nCol
   LOCAL nClickRow := oBrw:GetTxtRow( nRowPix )

   cForm  := oBrw:cParentWnd
   nRPos  := oBrw:nRowPos
   nAtCol := Max( oBrw:nAtCol( nColPix ), 1 )  // номер колонки
   oCol   := oBrw:aColumns[ nAtCol ]
   cName  := oCol:cName
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nX     := GetProperty(cForm, "Col") + GetBorderWidth() - 4
   // возмём координаты от шапки таблицы
   nY     += GetMenuBarHeight() + oBrw:nTop + 2
   nY     += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nY     += IIF( oBrw:lDrawHeaders, oBrw:nHeightHead  , 0 )
   nY     -= 1   //IIF( oBrw:lDrawSpecHd , oBrw:nHeightSpecHd, 0 )
   nX     += oCol:oCell:nCol
   nX     += IIF( oBrw:lSelector, oBrw:aColumns[1]:nWidth , 0 )  // если есть селектор
   nX     -= 5
   nVirt  := 0
   cCnr   := ""
   nCnr   := 0

   FOR nCol := 1 TO 3
      cCol := oBrw:aColumns[ nCol ]:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         nVirt ++
      ENDIF
   NEXT

   cMsg  := "Special Header - "
   cNam  := {'Left mouse :OneClick', 'Right mouse :OneClick'}[ nClick ]
   cMsg1 := cMsg + cNam + ", y:x " + hb_ntos(nRowPix) + ":" + hb_ntos(nColPix)
   cMsg2 := "Head position y/x: " + hb_ntos(nY) + '/' + hb_ntos(INT(nX))
   // номера колонок не совпадают с номерами полей в базе,
   // т.к. есть скрытые/удалённые колонки из таблицы см.функцию myBrwDelColumn()
   // нумерацию берем из нумератора таблицы
   //cCnr := oBrw:aColumns[ oBrw:nCell ]:cSpcHeading - так нельзя
   cCnr := oBrw:aColumns[ nAtCol ]:cSpcHeading
   nCnr := Val( cCnr )

   cMsg3 := "Column header: " + hb_ntos(nCnr) + "  [" + cName + "]"
   cMsg4 := "nAt=" + hb_ntos(nAt) + ", nAtCol=" + hb_ntos(nAtCol)
   cMsg4 += ", nClickRow=" + hb_ntos(nClickRow)
   aMsg  := { cMsg1, cMsg2, cMsg3, cMsg4 }

   IF     cName == "SELECTOR"
   ELSEIF cName == "ORDKEYNO"
      myMenuSpcHdClick( oBrw, nY, nX, aMsg, nCnr, nAtCol, 1 )
   ELSE
      myMenuSpcHdClick( oBrw, nY, nX, aMsg, nCnr, nAtCol, 99 )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myMenuSpcHdClick( oBrw, nY, nX, aMsg, nCol, nAtCol, nMode )
   LOCAL cForm, hFont1, hFont2, hFont3, cMenu1, cMenu2, oCol, nJ, nDel
   LOCAL o, c2Fltr, aFltr, cName, cSpcHd, nMenu, aColFltr, lFilter, c1Fltr
   LOCAL lChange, cMenu0, cMenu3, cMenu4, cMenu5, nCnt, cCol, cFilter, a2Fltr
   LOCAL nRowPos, nCell

   o        := oBrw:Cargo                 // получить данные из объекта
   aFltr    := o:aColFilter               // колонки таблицы с фильтром
   aColFltr := o:aColNumFltr              // номера колонок таблицы с фильтром
   cForm    := oBrw:cParentWnd
   hFont1   := GetFontHandle( "TsbEdit"   )
   hFont2   := GetFontHandle( "TsbSuperH" )
   hFont3   := GetFontHandle( "TsbBold"   )
   cMenu0   := 'Удалить ВСЕ фильтры по столбцам'
   cMenu1   := 'Удалить фильтр из столбца "' + hb_ntos(nCol) + '"'
   cMenu2   := 'Поставить фильтр на столбец "' + hb_ntos(nCol) + '"'
   cMenu3   := 'Сортировать по возрастанию'
   cMenu4   := 'Сортировать по убыванию'
   cMenu5   := 'Без сортировки'
   oCol     := oBrw:aColumns[ nAtCol ]
   cName    := oCol:cName
   cSpcHd   := oCol:cSpcHeading
   nMenu    := 0
   lFilter  := .F.
   lChange  := .F.
   cFilter  := ""
   nRowPos  := oBrw:nRowPos
   nCell    := oBrw:nCell

   IF LEN(aColFltr) > 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nAtCol
            lFilter  := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( BRW_BMP_CONTEX_MENU )   // set image size
   SetThemes(0)                                 // "White theme" в ContextMenu

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM aMsg[1] DISABLED FONT hFont2
       SEPARATOR
       IF nMode == 1
          nMenu := 0
          MENUITEM cMenu0 ACTION {|| MG_Debug(cMenu0,cName,nAtCol) } FONT hFont2 IMAGE "bFltrDel28"
       ELSE
          MENUITEM cMenu3 ACTION {|| nMenu := 3, c2Fltr := MG_Debug(cMenu3,cName,nAtCol,"Резерв") } FONT hFont2 IMAGE "bSortA28"
          MENUITEM cMenu4 ACTION {|| nMenu := 4, c2Fltr := MG_Debug(cMenu4,cName,nAtCol,"Резерв") } FONT hFont2 IMAGE "bSortZ28"
          MENUITEM cMenu5 ACTION {|| nMenu := 5, c2Fltr := MG_Debug(cMenu5,cName,nAtCol,"Резерв") } FONT hFont2
          SEPARATOR
          IF lFilter
             MENUITEM cMenu1 ACTION {|| nMenu := 1 } FONT hFont2 IMAGE "bFltrDel28"
          ELSE
             MENUITEM cMenu1 DISABLED FONT hFont2 IMAGE "bFltrDel28"
          ENDIF
          MENUITEM cMenu2  ACTION {|| nMenu := 2, a2Fltr := MenuFltr(oBrw,cMenu2,cName,cSpcHd,nAtCol) } FONT hFont2 IMAGE "bFltrAdd28"
       ENDIF
       SEPARATOR
       MENUITEM aMsg[3] ACTION  {|| MG_Debug(aMsg)  } FONT hFont2
       MENUITEM "Exit"  ACTION  {|| oBrw:SetFocus() } FONT hFont3
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu == 0                  // удалить ВСЕ фильтры по столбцам
      lChange  := .T.
      aFltr    := {}
      aColFltr := {}
      nCnt     := 0
      FOR EACH oCol IN oBrw:aColumns
         cCol := oCol:cName
         IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
            // пропуск нумерации
         ELSE
            IF oCol:lVisible
               oCol:cSpcHeading := hb_ntos( ++nCnt )
               // картинка в нумераторе колонок таблицы - стрелка_вниз 20x20
               oCol:uBmpSpcHd   := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
            ENDIF
         ENDIF
      NEXT
      // сделать очистку фильтра по таблице
      cFilter := ""

   ELSEIF nMenu == 1              // меню - убрать фильтр
      lChange := .T.
      nDel    := 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nAtCol
            nDel := nJ
            EXIT
         ENDIF
      NEXT
      IF nDel > 0
         // удалить условие фильтра для колонки
         ADel( aFltr   , nDel, .T. )
         ADel( aColFltr, nDel, .T. )
      ENDIF
      IF AT( "[", cSpcHd ) > 0
         cSpcHd := SUBSTR( cSpcHd, 1, AT("[",cSpcHd) - 2 )
      ENDIF
      oCol:cSpcHeading := cSpcHd
      // картинка в нумераторе колонок таблицы - стрелка_вниз 20x20
      oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
      // сделать очистку фильтра по таблице
      IF LEN(aFltr) == 0
         cFilter := ""
      ELSE
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 2  // меню - поставить фильтр
      IF LEN(a2Fltr) > 0  // поставили фильтр по таблице
         lChange := .T.
         nDel    := 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nAtCol
               nDel := nJ
               EXIT
            ENDIF
         NEXT
         c1Fltr := a2Fltr[1]  // строка фильтра
         c2Fltr := a2Fltr[2]  // строка фильтра, резерв
         IF nDel == 0
            // новое условие фильтра для колонки
            AADD( aFltr   , c1Fltr )
            AADD( aColFltr, nAtCol )
            cSpcHd += "  [" + hb_ntos(LEN(aFltr)) + "]"
         ELSE
            //  фильтр уже есть
            aFltr[nDel] := c1Fltr
         ENDIF
         oCol:cSpcHeading := cSpcHd
         // картинка в нумераторе колонок таблицы - фильтр  20x20
         oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hFltrAdd20   }
         // добавить фильтр по другим колонкам, если есть
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 3  // меню - Сортировать по возрастанию
   ELSEIF nMenu == 4  // меню - Сортировать по убыванию
   ELSEIF nMenu == 5  // меню - Без сортировки

   ENDIF

   nRowPos := oBrw:nRowPos
   nCell   := oBrw:nCell
   // перечитать шапку и нумератор
   oBrw:DrawHeaders()
   IF lChange
      // перезаписать значения в контейнер-объект
      o:aColFilter  := aFltr                // колонки таблицы с фильтром
      o:aColNumFltr := aColFltr             // номера колонок таблицы с фильтром
      o:cBrwFilter  := cFilter              // строка фильтра по всем колонки таблицы

      // oBrw:Reset() - это не надо, уже есть в oBrw:FilterData()
      IF LEN(cFilter) == 0
         oBrw:FilterData()
      ELSE
         oBrw:FilterData( cFilter )         // установка фильтра на базу
      ENDIF
      mySuperHeaderChange( oBrw, cFilter )  // изменить суперхидер таблицы

      // для управления перестановок колонок (за пределами окна тсб)
      DO EVENTS
      nCell := nCell - 1
      oBrw:GoPos( nRowPos, nCell )          // восстановить курсор в таблице на строке/столбце
      oBrw:GoRight()

   ENDIF
   DO EVENTS
   oBrw:SetFocus()

RETURN Nil

//////////////////////////////////////////////////////////////////////////////////
// изменить суперхидер таблицы
STATIC FUNCTION mySuperHeaderChange( oBrw, cFilter )
   LOCAL cText, nH, aFltr

   nH    := oBrw:Cargo:nHSuperHead     // высота титула суперхидера - не использую
   cText := oBrw:Cargo:cSuperHead      // титул суперхидера
   aFltr := oBrw:Cargo:aColFilter      // колонки таблицы с фильтром

   IF LEN(aFltr) > 0
      cText += CRLF + cFilter
   ENDIF

   oBrw:aSuperHead[3,3] := cText   // поменяли СуперХидер
   oBrw:DrawHeaders()              // перечитать суперхидер/шапку/нумератор
   DO EVENTS

RETURN Nil

//////////////////////////////////////////////////////////////////////////////////
// обработка подвала таблицы
STATIC FUNCTION myBrwFootClick( nClick, nRowPix, nColPix, nAt, oBrw )
   LOCAL cForm, nRPos, nAtCol, nHrow, nLine, nHcell, cNam, cName, cMsg
   LOCAL oCol, nY, nX, cMsg1, cMsg2, cMsg3, cMsg4, aMsg, nVirt, cCol, nCol
   LOCAL nClickRow := oBrw:GetTxtRow( nRowPix )

   cForm  := oBrw:cParentWnd
   nRPos  := oBrw:nRowPos        // номер колонки
   nAtCol := Max( oBrw:nAtCol( nColPix ), 1 )
   oCol   := oBrw:aColumns[ nAtCol ]
   cName  := oCol:cName
   nHCell := oBrw:nHeightCell    // высота одной ячейки
   nLine  := oBrw:nRowCount()    // кол-во строк в таблице
   nHrow  := nLine * nHcell      // высота строк в таблице
   cMsg   := "Footer - "
   nVirt  := 0
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nX     := GetProperty(cForm, "Col") + GetBorderWidth() - 4
   // возмём координаты от шапки таблицы
   nY     += GetMenuBarHeight() + oBrw:nTop + 2
   nY     += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nY     += IIF( oBrw:lDrawHeaders, oBrw:nHeightHead  , 0 )
   nY     += IIF( oBrw:lDrawSpecHd , oBrw:nHeightSpecHd, 0 )
   nY     += nHrow                                    // общая высота перед подвалом таблицы
   nY     -= 22
   nX     += oCol:oCell:nCol
   nX     += IIF( oBrw:lSelector, oBrw:aColumns[1]:nWidth , 0 )  // если есть селектор
   nX     -= 5

   FOR nCol := 1 TO 3
      cCol := oBrw:aColumns[ nCol ]:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         nVirt ++
      ENDIF
   NEXT

   cNam  := {'Left mouse :OneClick', 'Right mouse :OneClick'}[ nClick ]
   cMsg1 := cMsg + cNam + ", y:x " + hb_ntos(nRowPix) + ":" + hb_ntos(nColPix)
   cMsg2 := "Foot position y/x: " + hb_ntos(nY) + '/' + hb_ntos(INT(nX))
   cMsg3 := "Column: " + hb_ntos(nAtCol) + " - " + hb_ntos(nVirt)
   cMsg3 += " = " + hb_ntos(nAtCol-nVirt) + "  [" + cName + "]"
   cMsg4 := "nAt=" + hb_ntos(nAt) + ", nAtCol=" + hb_ntos(nAtCol)
   cMsg4 += ", nClickRow=" + hb_ntos(nClickRow)
   aMsg  := { cMsg1, cMsg2, cMsg3, cMsg4 }

   myMenuFootClick( oBrw, nY, nX, aMsg )

   IF nAtCol > 2
      // поместить в подвал строку
      oBrw:aColumns[nAtCol]:cFooting := "[ "+HB_NtoS(nAtCol-nVirt) + " ]"
      oBrw:DrawFooters()
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION myMenuFootClick( oBrw, nY, nX, aMsg )
   LOCAL cForm, hFont1, hFont2, hFont3

   cForm  := oBrw:cParentWnd
   hFont1 := GetFontHandle( "TsbEdit"   )
   hFont2 := GetFontHandle( "TsbSuperH" )
   hFont3 := GetFontHandle( "TsbBold"   )

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( BRW_BMP_CONTEX_MENU )   // set image size
   SetThemes(1)                                 // "White theme" в ContextMenu

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  aMsg[1] DISABLED FONT hFont1
       MENUITEM  aMsg[2] DISABLED FONT hFont1
       MENUITEM  aMsg[4] DISABLED FONT hFont1
       SEPARATOR
       MENUITEM  aMsg[3] ACTION  {|| MG_Debug(aMsg[3]) } FONT hFont2
       MENUITEM  "Exit"  ACTION  {|| oBrw:SetFocus()   } FONT hFont3
   END MENU

   //nY -= BRW_BMP_CONTEX_MENU * 6        // 6 строк меню

   _ShowContextMenu(cForm, nY, nX, .f. )  // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm          // delete menu after exiting
   END MENU

   DO EVENTS

   oBrw:SetFocus()

RETURN Nil

///////////////////////////////////////////////////////////////////////////////////
// сменить картинку в суперхидере - изменить контстанту показа
STATIC FUNCTION myConstImageSuperHead(ow, ky, cItm, aTsbMnu)
   LOCAL oBrw := (ThisWindow.Object):Cargo:oBrw        // получить данные из объекта
   LOCAL nI, nMsk, cForm := ow:Name

   ky   := VAL(cItm)
   nI   := ky - 70
   nMsk := aTsbMnu[nI,2]

   IF ISOBJECT(oBrw)
      // изменение маски показа картинок в суперхидере таблицы
      FOR nI := 1 TO Len( oBrw:aSuperHead )
         IF !Empty( oBrw:aSuperHead[ nI ][8] )      // uBitMap задан ?
            oBrw:aSuperHead[ nI ][15] := nMsk       // SRCCOPY
         ENDIF
      NEXT
      oBrw:DrawHeaders()     // перечитать суперхидер/шапку/нумератор
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwInfoFont( oBrw )
   LOCAL cMsg

   cMsg := "Table alias: " + oBrw:cAlias + ";;"
   cMsg += "1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg += "   2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg += "   3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg += "  4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg += "   5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg += "6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";"

   MG_Info(cMsg,"Инфо о фонтах таблицы")

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cMsg := ''

   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "' + oCol:cFieldTyp + '" ' + HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT

   MG_Info(cMsg + REPL(";",30))

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myBrwInfoRecno( oBrw )            // Инфо по текущй записи
   LOCAL nLine, cAls, nCell, cMsg, oCol, cTyp, cCol, xVal
   LOCAL oCell, nY, nX, nW, nH, aVal

   cMsg  := ""
   cAls  := oBrw:cAlias
   nLine := oBrw:nAt
   nCell := oBrw:nCell                     // номер ячейки/колонки в таблице
   oCol  := oBrw:aColumns[ nCell ]
   cTyp  := oCol:cFieldTyp
   cCol  := oCol:cName
   xVal  := oBrw:GetValue(cCol)
   //xVal := oBrw:GetValue(nCell)          // можно так
   oCell := oBrw:GetCellInfo(oBrw:nRowPos)
   nY    := oCell:nRow + oBrw:nHeightHead + 4
   nX    := oCell:nCol
   nW    := oCell:nWidth
   nH    := oCell:nHeight
   aVal  := { nY, nX, nW, nH }

   cMsg += "  База: " + cAls + ";"
   cMsg += "Запись: " + HB_NtoS( (cAls)->( RECNO() ) ) + ";;"
   cMsg += " Номер строки в таблице: " + HB_NtoS( nLine ) + ";"
   cMsg += "Номер колонки в таблице: " + HB_NtoS( nCell ) + ";"
   cMsg += "Имя поля базы: " + cCol + "  [" + cTyp + "];;"
   cMsg += "Значение ячейки: [" + cValToChar(xVal) + "];;"
   cMsg += "Координаты ячейки: " + HB_ValToExp( aVal )

   MG_Info( cMsg + REPL(";",20), "Инфо по текущй записи" )

RETURN Nil

