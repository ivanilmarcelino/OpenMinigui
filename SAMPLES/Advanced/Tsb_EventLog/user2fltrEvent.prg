/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * 23.09.20
*/
#include "minigui.ch"
#include "TSBrowse.ch"
////////////////////////////////////////////////////////////////////////
FUNCTION my3SpravDbf(cPath, cFile, a2Dim, cTitle)
   LOCAL nTsbLine, aTsbColumn, aRet, cAls, lDelFile

   lDelFile   := .F.           // не удалять файл Dbf
   aRet       := my3SpravDbfNew(cPath, cFile, a2Dim, lDelFile)
   cAls       := aRet[1]       // алиас временной базы
   aTsbColumn := aRet[2]       // кол-во символов в колонках
   SELECT(cAls)
   DbSetOrder(0)
   nTsbLine   := LASTREC()     // кол-во строк в таблице

   SELECT(cAls)
   nTsbLine   := LASTREC()     // кол-во строк в таблице

   DbSetOrder(1)
   Goto Top
   aRet := my3SelectDbf(cAls, cTitle, aTsbColumn, nTsbLine)
   IF LEN(aRet) == 0
      aRet := { {} , {}, {"нет события"} }
   ENDIF

   (cAls)->(DbCloseArea())

RETURN aRet

////////////////////////////////////////////////////////////////////////////////
// справочник событий в программе
STATIC FUNCTION my3SelectDbf(cAls, cTitleTsb, aTblColumn, nTblLine)
   LOCAL cTitle, cIcon, cFont, cFontBtn, cText, nWTxt, cFind, aBackColor
   LOCAL nI, oBrw, nFontSize, aRet, nY, nX, nW, nH, nLR, nG, aBColorTxt
   LOCAL nTsbHeight, nTsbWidth, nLetters, nHTxt, oDlu, nWTsb, nHTsb, oTsb
   LOCAL cObjBtn, cBtnCapt, aBtnIcon, aBtnGrad, aBtnClr, aFntClr, aBtnFnt
   LOCAL nYBtn, nXBtn, nwPost, nWBtn, nHBtn, aLang

#ifdef KEY_ENG
   cTitle := "Event Directory"
   aLang := { "Event Search:", "Select", "Cancel" }
#else
   cTitle := "Справочник событий"
   aLang  := { "Поиск события:", "Выбор", "Отмена" }
#endif

   cIcon      := "iDbC48x1"
   cFont      := "DejaVu Sans Mono"
   cFontBtn   := "Comic Sans MS"
   aRet       := {}
   cFind      := ''
   aBackColor := {178,227,137}
   aBColorTxt := {255,255,240}
   nFontSize  := mySizeFontMenuResolutionScreen()
   nHTxt      := nFontSize*2                               // ширина GET'ов
   nLetters   := 0
   FOR nI := 1 TO LEN(aTblColumn)
      nLetters += aTblColumn[nI]                           // кол-во букв в колонках
   NEXT
   nTsbWidth  := GetTxtWidth( REPL("x",nLetters), nFontSize, cFont )
   nTsbWidth  += GetVScrollBarWidth()                      // примерная ширина таблицы
   nTblLine   := nTblLine + 1 + 2 + 1     + 3              // кол-во строк в таблице
   nTsbHeight := nTblLine * GetTxtHeight( "A", nFontSize, cFont ) // примерная высота таблицы
   nX := nLR  := 20
   //?  "примерная высота таблицы, nTsbHeight := ", nTsbHeight

   // возвращает объект с данными размеров от размера фонта от dlu в pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top*2

   IF GetDesktopHeight() <= 600
     nG := 10 - IIF( Large2Fonts(), 4, 0 )
   ELSEIF GetDesktopHeight() >= 768 .AND. GetDesktopHeight() <= 864
     nG := 16 - IIF( Large2Fonts(), 8, 0 )
   ELSEIF GetDesktopHeight() > 864  //   == 1080
     nG := 22 - IIF( Large2Fonts(), 9, 0 )
   ENDIF

   nY         := nG                                        // отступ сверху и снизу
   nW         := nTsbWidth + nLR*2 + GetBorderWidth()      // размеры окна
   nH         := nTsbHeight + 90 + nG*2
   IF nH > System.ClientHeight * 0.95
      nH         := System.ClientHeight * 0.95
      nTsbHeight := nH - ( nG/2 + nHTxt + nHTxt + nG/2 + nG/2 )
      nTsbHeight -= ( GetTitleHeight() + GetBorderHeight() )
   ENDIF

   SET FONT TO cFont, nFontSize   // фонты для таблицы
   SELECT(cAls)

   DEFINE WINDOW Form_SprDbf AT nY, nX WIDTH nW HEIGHT nH ;
      ICON cIcon TITLE cTitle BACKCOLOR aBackColor  ;
      MODAL NOSIZE                                  ;
      FONT cFont SIZE nFontSize                     ;
      ON INIT {|| oBrw:Setfocus() }

      nW       := This.ClientWidth
      nH       := This.ClientHeight

      (This.Object):Cargo           := oKeyData()   // создает объект без переменных (условно пустой) используем ниже по коду
      (This.Object):Cargo:nFontSize := nFontSize    // размер фонта таблицы
      (This.Object):Cargo:hWin      := This.Handle  // потом проще добывать handle окна
      (This.Object):Cargo:cWin      := This.Name    // ...
      (This.Object):Cargo:cFilter   := ""           // фильтр по базе
      // можно так писать и еще что то перенести с This. ... сюда, если надо
      //This.Cargo:hWin := This.Handle           // потом проще добывать handle окна
      //This.Cargo:cWin := This.Name             // ...
      //Узнать есть ли переменная в объекте можно так
      //IF ( oBrw:Cargo:Pos(upper("<имя переменной>")) ) > 0
      //ENDIF

      @ 0, 0 LABEL Label_1 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      nY    := nG/2
      cText := aLang[1]  //"Поиск по наименованию:"
      nWTxt := GetTxtWidth( cText, nFontSize, cFont )
      @ nY, nX LABEL Label_2 WIDTH nWTxt HEIGHT nHTxt VALUE cText ;
        FONTCOLOR BLACK TRANSPARENT /*CENTERALIGN*/ VCENTERALIGN
      nY += This.Label_2.Height

      @ nY, nX TEXTBOX Textbox_Find VALUE cFind WIDTH nWTxt HEIGHT nHTxt ;
        FONTCOLOR BLACK BACKCOLOR aBColorTxt                             ;
        ON GOTFOCUS  {|| This.Button_Enter.Enabled := .F.    } ;
        ON LOSTFOCUS {|| This.Button_Enter.Enabled := .T.    } ;
        ON CHANGE    {|| oBrw:PostMsg(WM_KEYDOWN, VK_F20, 0), This.Textbox_Find.Setfocus }

        //ON CHANGE    ( oMyBase():PostMsg(WM_KEYDOWN, VK_F20, 0) ) ;
        //ON CHANGE {|| myTsbRefresh(oBrw), This.Textbox_Find.Setfocus }
        //ON CHANGE {|| cFind := This.Textbox_Find.Value ,;
        //              myTsbRefresh(oBrw)               ,;
        //              This.Textbox_Find.Setfocus }

      nY   += This.Label_2.Height + nG/2
      nX   := nWTxt + nLR*2

      // ширина для 2x кнопок
      nWBtn    := ( nW - nX - nLR*2 ) / 2
      nHBtn    := nY - nG

      cObjBtn  := "Button_Enter"
      cBtnCapt := aLang[2]   //"Выбор"
      aBtnIcon := { "iFloppy48x1", "iFloppy48x2" }
      aBtnGrad := { CLR_GREEN, CLR_WHITE }
      aBtnClr  := NIL
      aFntClr  := NIL
      aBtnFnt  := { cFontBtn, nFontSize + 2, .T. }
      nYBtn    := nG/2
      nXBtn    := nX
      nwPost   := 1      // событие XX на форме
      my2BUTTON(nYBtn, nXBtn, nWBtn, nHBtn, cObjBtn, cBtnCapt, aBtnGrad, aBtnClr, aBtnIcon, aFntClr, aBtnFnt, nwPost)
      // переназначим здесь на номер nPost
      This.&(cObjBtn).Action := {|| _wPost(This.Cargo:nPost, , This.Name) }

      cObjBtn  := "Button_Exit"
      cBtnCapt := aLang[3]   //"Отказ"
      aBtnIcon := { "iReturn48x1", "iReturn48x2" }
      aBtnGrad := { CLR_HRED, CLR_WHITE }
      aBtnClr  := NIL
      aFntClr  := NIL
      aBtnFnt  := { cFontBtn, nFontSize + 2, .T. }
      nYBtn    := nG/2
      nXBtn    := nX + nWBtn + nLR
      nwPost   := 99      // событие XX на форме
      my2BUTTON(nYBtn, nXBtn, nWBtn, nHBtn, cObjBtn, cBtnCapt, aBtnGrad, aBtnClr, aBtnIcon, aFntClr, aBtnFnt, nwPost)
      // переназначим здесь на номер nPost
      This.&(cObjBtn).Action := {|| _wPost(This.Cargo:nPost, , This.Name) }

      nX    := nLR
      nWTsb := nTsbWidth              // примерная ширина таблицы
      nHTsb := nTsbHeight             // примерная высота таблицы

      //////////////// Table 2 ///////////////////////
      //@ nY, nX LABEL Lbl_2 WIDTH nWTsb HEIGHT nHTsb VALUE '- Table -' BACKCOLOR GRAY

      // массивы для таблицы
      oTsb := CreateDatos(nWTsb, aTblColumn, cTitleTsb)
      IF ISCHAR( oTsb:aArray ) ; dbSelectArea( oTsb:aArray )
      ENDIF
/*
// ---------- отладка ---- не убирать ------------
? "oTsb:aArray    =" , oTsb:aArray                          ; ?
? "oTsb:aHead     =" , oTsb:aHead      ; ?v oTsb:aHead      ; ?
? "oTsb:aFSize    =" , oTsb:aFSize     ; ?v oTsb:aFSize     ; ?
? "oTsb:aFoot     =" , oTsb:aFoot      ;                    ; ?
? "oTsb:aFPict    =" , oTsb:aFPict     ; ?v oTsb:aFPict     ; ?
? "oTsb:aAlign    =" , oTsb:aAlign     ; ?v oTsb:aAlign     ; ?
? "oTsb:aName     =" , oTsb:aName      ; ?v oTsb:aName      ; ?
? "oTsb:aField    =" , oTsb:aField     ; ?v oTsb:aField     ; ?
? "oTsb:aSupHd    =" , oTsb:aSupHd     ; ?v oTsb:aSupHd     ; ?
*/
      DEFINE TBROWSE oBrw AT nY, nX        ;
             ALIAS oTsb:aArray             ;
             WIDTH nWTsb HEIGHT nHTsb CELL ;
             FONT       oTsb:aTsbFont      ;
             BRUSH      oTsb:aBrush        ;
             HEADERS    oTsb:aHead         ;
             COLSIZES   oTsb:aFSize        ;
             PICTURE    oTsb:aFPict        ;
             JUSTIFY    oTsb:aAlign        ;
             COLUMNS    oTsb:aField        ;
             COLNAMES   oTsb:aName         ;
             COLNUMBER  oTsb:aNumber       ;
             FOOTERS    oTsb:aFoot         ;
             FIXED                         ;  // активирует функцию двойного курсора на заблокированных столбцах
             COLSEMPTY                     ;  // вместо - AEval( oBrw:aColumns, {|oCol| oCol:lEmptyValToChar := .T. } )
             LOADFIELDS /*SELECTOR .T. ENUMERATOR EDIT GOTFOCUSSELECT*/

             mySuperHeader( oBrw, oTsb:aSupHd )    // SuperHeader
             mySetTsb( oBrw )                      // настройки таблицы
             myColorTsb( oBrw )                    // цвета на таблицу
             mySet2Tsb( oBrw )                     // настройки таблицы дополнительные
             mySetEditTsb( oBrw )                  // настройки редактирования
             mySetHeadFoot( oBrw )                 // настройки обработки шапки/подвала

             :bLDblClick   := {|up1,up2,nfl,ob| up1:=up2:=nfl:=Nil, ;
                                            ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

             :UserKeys( VK_RETURN, {|ob| _wPost(1, ob, ob), .F. } )

             //:UserKeys(VK_F2, {|| MsgDebug( "Строка поиска=;" + ThisWindow.Cargo:cFilter) })

             // назначить свою обработку нажатий клавиш
             :bUserKeys   := {|nKy,nFl,ob| myKeyUserEdit(nKy, nFl, ob) }

             :bOnEscape   := {|ob| aRet := {}, DoMethod(ob:cParentWnd, "Release") }  // выход по ESC

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:oPhant:nClrHeadBack := ob:Cargo:nClr4a, ;  // !!!
                                                ob:oPhant:nClrFootBack := ob:Cargo:nClr4a,;
                                                ob:Refresh() }

      (This.Object):Cargo:oBrw := oBrw     // на окне запомнили, объект tsb уже готовый
      // можно так писать
      //This.Cargo:oBrw := oBrw

      WITH OBJECT This.Object
      :Event( 1, {|  | aRet := Dbf2GetAllLine(oBrw) , _wSend(98) } )
      :Event(98, {|ow| ow:Release()              } )
      :Event(99, {|ow| aRet := {} , ow:Release() } )
      END WITH

      ON KEY ESCAPE OF Form_SprDbf ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW Form_SprDbf
   ACTIVATE WINDOW Form_SprDbf

RETURN aRet

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Dbf2GetAllLine(oBrw)
   LOCAL lVal, nVal, cVal, aRet1, aRet2, aRet3
   LOCAL cAlias := oBrw:cAlias

   oBrw:FilterData()
   // oBrw:Reset() - это не надо, уже есть в oBrw:FilterData()
   DO EVENTS
   oBrw:GoTop()

   aRet1 := {} ; aRet2 := {} ; aRet3 := {}
   SELECT(cAlias)
   DbSetOrder(0)
   GOTO TOP
   DO WHILE !EOF()
      lVal  := (cAlias)->CHECK
      nVal  := (cAlias)->NCODE
      cVal  := ALLTRIM( (cAlias)->CCODE )
      IF lVal
         AADD( aRet1, nVal )
         AADD( aRet2, cVal )
         AADD( aRet3, ""  )
      ENDIF
      SKIP
   ENDDO
   GOTO TOP

RETURN { aRet1, aRet2, aRet3 }

//////////////////////////////////////////////////////////////////////////////////
// Функция обработки нажатия клавиш в таблице
// Функция должна возвращать: .T. или .F.
// .T. - продолжить обработку клавиши в тсб
// .F. - в блоке все обработали и продолжать обработку клавиши в тсб не надо
STATIC FUNCTION myKeyUserEdit( nKey, nFlg, oBrw )
   LOCAL oCargo, cFilter, lRet
   LOCAL cForm  := oBrw:cParentWnd
   Default nFlg := Nil, oBrw := Nil

   SET WINDOW THIS TO oBrw
   oCargo  := This.Cargo
   cFilter := oCargo:cFilter      // фильтр по базе
   SET WINDOW THIS TO

   DO CASE
      CASE nKey == VK_DOWN .OR. nKey == VK_UP       // 38 + 40
         lRet := .T.
      CASE nKey == VK_PRIOR .OR. nKey == VK_NEXT    // PgUp + PgDn / 33 + 34
         lRet := .T.
      CASE nKey == VK_SPACE
      CASE nKey == VK_F5
         //Table_Print(oBrw)
      //CASE nKey == VK_RETURN
      //   _wPost(1)
      //   lRet := .F.
      CASE nKey == 16 .OR. nKey == 17  // Shift+Alt  Shift+Ctrl  "RUS/LAT"
         lRet := .F.
      CASE nKey == VK_F20
         cFilter := GetProperty(cForm, "Textbox_Find", "Value" )
         oCargo:cFilter := cFilter
         If ! empty(cFilter)
            SetProperty(cForm, "Textbox_Find", "Value", cFilter )
            oCargo:cFilter := cFilter
            myTsbRefresh(oBrw)
         EndIf
         lRet := .F.

      CASE nKey == VK_BACK             // Backspace
         IF LEN(cFilter) > 0
            cFilter := LEFT(cFilter,LEN(cFilter)-1)
            SetProperty(cForm, "Textbox_Find", "Value", cFilter )
            oCargo:cFilter := cFilter
         ENDIF
         myTsbRefresh(oBrw)
         lRet := .F.
      CASE ( nKey > 47 .AND. nKey < 58 ) .OR. ( nKey > 63 .AND. nKey < 91 )
         // цифры и латинские буквы
         cFilter += CHR(nKey)
         SetProperty(cForm, "Textbox_Find", "Value", cFilter )

         SET WINDOW THIS TO oBrw
         oCargo  := This.Cargo
         oCargo:cFilter := cFilter      // сохранить фильтр по базе
         SET WINDOW THIS TO

         myTsbRefresh(oBrw)
         lRet := .F.
      OTHERWISE
         lRet := .T.
   ENDCASE

RETURN lRet

////////////////////////////////////////////////////////////
// фильтр по таблице и рефреш базы
STATIC FUNCTION myTsbRefresh(oBrw)
   LOCAL oCargo, cFilter, cFltr

   // Вот так лучше
   SET WINDOW THIS TO oBrw
   oCargo  := This.Cargo
   cFilter := oCargo:cFilter      // фильтр по базе
   SET WINDOW THIS TO

   IF LEN(cFilter) == 0
      oBrw:FilterData()
   ELSE
      cFltr := "'" + UPPER(cFilter) + "' $ UPPER(CCODE)"
      oBrw:FilterData( cFltr )
   ENDIF

   // oBrw:Reset() - это не надо, уже есть в oBrw:FilterData()
   DO EVENTS
   oBrw:GoTop()
   oBrw:SetFocus()

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySetTsb( oBrw, nTable )
   DEFAULT nTable := 1

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
      //:lNoVScroll  := .T.         // отключаем показ VScroll - НЕЛЬЗЯ
      :nCellMarginLR := 1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      // --------- заменяем в зависимости от монитора ---------
      //IF GetDesktopHeight() < 800
         :nHeightCell  := 26     // высота ячеек таблицы - высота картинки 20 + 2*2 отступы сверху и снизу
         //:nHeightSuper := 12   // высота заголовка ( спецхидер )
         :nHeightFoot  := 18     // высота подвала таблицы
         :aCheck       := { LoadImage("CheckT20"), LoadImage("CheckF20") }  // заменяем на свои картинки
      //ELSE
      //   :nHeightCell  := 38     // высота ячеек таблицы - высота картинки 32 + 3*2 отступы сверху и снизу
      //   :nHeightSuper := 40     // высота заголовка ( спецхидер )
      //   :nHeightFoot  := 38     // высота подвала таблицы
      //   :aCheck       := { LoadImage("CheckT32"), LoadImage("CheckF32") }  // заменяем на свои картинки
      //ENDIF

   END WITH

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
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
         :AdjColumns()   // колонки входят в окно тсб, уберем вертикальную "дырку"
                         // распределив ее значение по избранным колонкам, растянув
      ENDIF

      //:ResetVScroll( .T. )          // включаем показ VScroll
      //:oVScroll:SetRange( 0, 0 )    //
      /*или*/
      IF :nLen > :nRowCount()
         :ResetVScroll( .T. )
         :oHScroll:SetRange( 0, 0 )
      ENDIF

      :nHeightCell  += 4                // добавить

   END WITH

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsb( oBrw )
   LOCAL O

   WITH OBJECT oBrw:Cargo
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
      :nGREEN5    :=  RGB(178,227,137)
      :nORANGE    :=  CLR_ORANGE
      :nORANGE5   :=  RGB(250,195,143)
      :nRED       :=  CLR_RED
      :nWHITE     :=  CLR_WHITE
      :nBLUE      :=  CLR_BLUE
      :n_BLUE     := -CLR_BLUE
      :n_HBLUE    := -CLR_HBLUE
      :n_BLACK    := -RGB(1,1,1)
      :n_TsbBC    := RGB(174,255,255)
      :n_TsbBC2   := RGB(229,245,215)

      // 2. переменные RGB( ... ) для использования
      :nRgb0      :=  RGB(  0,  0,  0)
      :nRgb1      :=  RGB(180,180,180)
      :nRgb2      :=  RGB(255,255,255)
      :nRgb3      := -RGB(128,225,225)

      // 3. переменные (aColors items number) от номера позиции в :SetColor( {...}, ... ) из TsBrowse.ch
      :nClrLine   :=  :nRgb1
      :nClr2      :=  :nRgb2                  // #define CLR_PANE     2   // back
      :nClr3      :=  :nWHITE                 // #define CLR_HEADF    3   // header text
      :nClr4      :=  {RGB(60,60,60),RGB(0,176,240)}     // мой цвет
      //:nClr4      :=  {RGB(0,128,128),RGB(0,176,240)}   // мой цвет
      //:nClr4      := {:nBLACK, :nGRAY}        // #define CLR_HEADB    4   // header back
      :nClr4a     :=  {RGB(60,60,60),RGB(34,116,70)}     // мой цвет
      :nClr5      :=  :nRgb0                  // #define CLR_FOCUSF   5   // focused text
      :nClr5a     :=  :nWHITE                 // #define CLR_FOCUSF   5   // focused text
      :nClr6_1    :=  :n_HRED                 // #define CLR_FOCUSB   6 1 // focused back
      :nClr6_2    :=  :n_BLACK                // #define CLR_FOCUSB   6 2 // focused back
      :nClr6a1    :=  :nHRED                  // #define CLR_FOCUSB   6 1 // focused back
      :nClr6a2    :=  :nBLACK                 // #define CLR_FOCUSB   6 2 // focused back
      :nClr9      :=  :nWHITE                          // мой цвет
      :nClr10     := {RGB(0,176,240),RGB(60,60,60)}    // мой цвет
      //:nClr9      :=  :nBLUE                  // #define CLR_FOOTF    9   // footer text
      //:nClr10     := {:nGRAY, :nHGRAY}        // #define CLR_FOOTB   10   // footer back
      :nClr11     :=  :nRgb0                  // #define CLR_SELEF   11   // focused inactive (or selected) text
      :nClr12_1   :=  :n_BLUE                 // #define CLR_SELEB   12 1 // focused inactive (or selected) back
      :nClr12_2   :=  :nRgb3                  // #define CLR_SELEB   12 2 // focused inactive (or selected) back
      :nClr16     := {RGB(60,60,60),RGB(0,176,240)}  // 16, фона спецхидер
      :nClr17     :=  :nYELLOW                       // 17, текста спецхидер
      :nClrCol2_5 := RGB(204,255,255)                // цвет колонок 2-5

   END WITH

   WITH OBJECT oBrw
      O := :Cargo
      :nClrLine := O:nClrLine   // создать в контейнере свои переменные с именами

      //:SetColor( { 1}, { O:nBLACK   } )  // 1 , текста в ячейках таблицы
      :SetColor( { 2}, { O:n_TsbBC2 } )    // 2 , фона в ячейках таблицы
      :SetColor( { 5}, { O:nClr5   } )     // 5 , текста курсора, текст в ячейках с фокусом
      :SetColor( { 6}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr6_1 , c:nClr6_2  ) } } )  // 6 , фона курсора
      :SetColor( {11}, { O:nClr11  } )     // 11, текста неактивного курсора (selected cell no focused)
      :SetColor( {12}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr12_1, c:nClr12_2 ) } } )  // 12, фона неактивного курсора (selected cell no focused)
      :Setcolor( { 3}, { O:nClr3   } )  // 3 , текста шапки таблицы
      :SetColor( { 4}, { O:nClr4a  } )  // 4 , фона шапка таблицы   // !!! тут лишний блок кода, массива достаточно
      :SetColor( { 9}, { O:nClr9   } )  // 9 , текста подвала таблицы
      :SetColor( {10}, { O:nClr4a  } )  // 10, фона подвала таблицы // !!! тут лишний блок кода, массива достаточно
      :SetColor( {13}, { O:nBLACK  } )  // 13, текста шапки выбранного индекса
      :SetColor( {14}, { O:nHBLUE2 } )  // 14, фона шапки выбранного индекса
      //:SetColor( {16}, { { || { CLR_HGRAY, CLR_GRAY   } } } ) // 16, фона спецхидер
      //:SetColor( {17}, { CLR_RED                          } ) // 17, текста спецхидер
      :SetColor( {16}, { O:nClr16  } )    // 16, фона суперхидер
      :SetColor( {17}, { O:nClr17  } )    // 17, текста суперхидер

      :hBrush   := CreateSolidBrush( 229,245,215 )   // цвет фона под таблицей
/*
? O:n_TsbBC2, hb_valtoexp( HMG_n2RGB( O:n_TsbBC2 ) )
? O:nClr5, hb_valtoexp( HMG_n2RGB( O:nClr5 ) )
? O:nClr6_1, O:nClr6_2 , hb_valtoexp( HMG_n2RGB( O:nClr6_1 ) ), hb_valtoexp( HMG_n2RGB( O:nClr6_2 ) )
? O:nClr11, hb_valtoexp( HMG_n2RGB( O:nClr11 ) )
? O:nClr12_1, O:nClr12_2 , hb_valtoexp( HMG_n2RGB( O:nClr12_1 ) ), hb_valtoexp( HMG_n2RGB( O:nClr12_2 ) )
*/
    //ENDIF

   END WITH


RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySuperHeader( oBrw, aSupHd )
   LOCAL oCargo, nFontSize

   SET WINDOW THIS TO oBrw
      oCargo    := This.Cargo
      nFontSize := oCargo:nFontSize  // размер фонта таблицы
   SET WINDOW THIS TO

   WITH OBJECT oBrw

   // суперхидер
   :AddSuperHead( 1  , :nColCount() , aSupHd[1] )

   :nHeightSuper := 0 //nFontSize*2      // высота заголовка ( суперхидер )

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// настройки редактирования
STATIC FUNCTION mySetEditTsb( oBrw )
   LOCAL nI

   WITH OBJECT oBrw

      nI := :nColumn( 'CHECK' )
      :aColumns[nI]:lEdit     := .T.
      :aColumns[nI]:nEditMove := 0
      //:aColumns[nI]:bPostEdit := {|| mySumTsbDbf( oBrw ) }  // обработка после ввода

      nI := :nColumn( 'NCODE' )
      :aColumns[nI]:lEdit     := .F.
      :aColumns[nI]:nEditMove := 0

      nI := :nColumn( 'CCODE' )
      :aColumns[nI]:lEdit     := .F.
      :aColumns[nI]:nEditMove := 0

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// настройки обработки шапки/подвала
STATIC FUNCTION mySetHeadFoot( oBrw )
   LOCAL nI, oCol

   WITH OBJECT oBrw
      // ---------- назначаем на шапку и подвал отдельную функцию ----------------
      For nI := 1 To :nColCount()
         // левая и правая кнопка мышки для шапки и подвала таблицы
         oCol := :aColumns[ nI ]
         oCol:bHLClicked := {|nrp,ncp,nat,obr| HeadClick(1,obr,nrp,ncp,nat) }
         oCol:bHRClicked := {|nrp,ncp,nat,obr| HeadClick(2,obr,nrp,ncp,nat) }
         oCol:bFLClicked := {|nrp,ncp,nat,obr| FootClick(1,obr,nrp,ncp,nat) }
         oCol:bFRClicked := {|nrp,ncp,nat,obr| FootClick(2,obr,nrp,ncp,nat) }
      Next

   END WIDTH

RETURN NIL


//////////////////////////////////////////////////////////////////
STATIC FUNCTION HeadClick( nClick, oBrw, nRowPix, nColPix, nAt )
   LOCAL nRow := oBrw:GetTxtRow(nRowPix)        // номер строки курсора в таблице
   LOCAL nCol := Max(oBrw:nAtCol(nColPix), 1)   // номер колонки курсора в таблице
   LOCAL nCell:= oBrw:nCell                     // номер ячейки в таблице
   LOCAL cNam := {'Left mouse', 'Right mouse'}[ nClick ]
   LOCAL nIsHS := iif(nRowPix > oBrw:nHeightSuper, 1, 2)
   LOCAL cObj, nSH, xVal

   xVal := nAt // резерв - чтобы убрать ошибку при компиляции

   cObj := iif(nIsHS == 1, 'Header', 'SuperHider')
   IF nIsHS == 1  // 'Header'
      cObj += "_" + HB_NtoS(nCol)
   ELSE
      If nCol <= oBrw:nColumn('Name_5')
         cObj := "SuperHider_1"
         nSH  := 1
      ElseIf nCol > oBrw:nColumn('Name_5')
         cObj := "SuperHider_2"
         nSH  := 2
      EndIf
   ENDIF

   //IF nCol == 3
      MyShowCntMenu( oBrw, nClick, nSH, {nRowPix,nColPix}, {nRow,nCol} )
   //ENDIF

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION FootClick( nClick, oBrw, nRowPix, nColPix, nAt )
   LOCAL nRow  := oBrw:GetTxtRow(nRowPix)       // номер строки курсора в таблице
   LOCAL nCol  := Max(oBrw:nAtCol(nColPix), 1)  // номер колонки курсора в таблице
   LOCAL nCell := oBrw:nCell                     // номер ячейки в таблице
   LOCAL cNam  := {'Left mouse', 'Right mouse'}[ nClick ]
   LOCAL cObj  := "Foot_" + HB_NtoS(nCol), xVal

   xVal := nAt // резерв - чтобы убрать ошибку при компиляции
   //IF nCol == 3
      MyShowCntMenu( oBrw, nClick, 0, {nRowPix,nColPix}, {nRow,nCol} )
   //ENDIF

RETURN Nil

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION MyShowCntMenu(oBrw, nClick, nSupHid, aMouse, aRowCol)
   LOCAL Font1, Font2, lRefresh := .F., aRez := aRowCol
   LOCAL nY, nX, nMetka := 0, cForm := oBrw:cParentWnd

   If nClick == 1   // ваша обработка
   Endif
   If nSupHid == 1  // ваша обработка
   Endif

   nY := aMouse[1]
   nX := aMouse[2]
   nY += GetProperty(cForm, "Row") + GetTitleHeight() + GetProperty(cForm, oBrw:cControlName, "Row")
   nX += GetProperty(cForm, "Col") + GetBorderWidth() + GetProperty(cForm, oBrw:cControlName, "Col")

   Font1 := GetFontHandle( "Font_1F7" )  // фонты из users2filter.prg
   Font2 := GetFontHandle( "Font_2F7" )  // фонты из users2filter.prg

   SET MENUSTYLE EXTENDED     // switch the menu style to advanced
   SetMenuBitmapHeight( 28 )  // set icon size 18x18

   DEFINE CONTEXT MENU OF &cForm
#ifdef KEY_ENG
       MENUITEM  'Mark all columns'   ACTION nMetka := 1 FONT Font1 IMAGE "CheckT20"
       SEPARATOR
       MENUITEM  'Unmark everywhere'  ACTION nMetka := 2 FONT Font1 IMAGE "CheckF20"
       SEPARATOR
       MENUITEM  "Exit"               ACTION Nil FONT Font2
#else
       MENUITEM  'Метка на все графы' ACTION nMetka := 1 FONT Font1  IMAGE "CheckT20"
       SEPARATOR
       MENUITEM  'Снять метку везде'  ACTION nMetka := 2 FONT Font1  IMAGE "CheckF20"
       SEPARATOR
       MENUITEM  "Выход"              ACTION Nil         FONT Font2
#endif
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f.) // DISPLAYING THE MENU
   InkeyGui(10)

   DEFINE CONTEXT MENU OF &cForm  // deleting menu after exiting
   END MENU

   IF nMetka # 0           // менялся ли в меню nMetka
      CheckFieldAll(oBrw,nMetka)
   ENDIF

   oBrw:GoTop()
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////
STATIC FUNCTION CheckFieldAll(oBrw, nLog)
   LOCAL lVal, cAlias := oBrw:cAlias
   DEFAULT nLog := 1

   DbSetOrder(0)
   GOTO TOP
   lVal  := IIF(nLog==1, .T., .F.)
   DO WHILE !EOF()
      (cAlias)->( FIELDPUT(FIELDNUM("CHECK"), lVal ) )
      SKIP
   ENDDO
   GOTO TOP

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateDatos(nWTsb, aTsbColumn, cTitleTsb)
   LOCAL oDatos, i, k, aProc, nColTsb, cNam, w

   WITH OBJECT ( oDatos := oKeyData() ) // создает объект без переменных (условно пустой)
                                        // используем далее по коду
   // колонки таблицы
   //              1     2       3
#ifdef KEY_ENG
   :aHead := { "-v-","code","name of events" }
#else
   :aHead := { "-v-","код","наименование событий" }
#endif

   nColTsb  := Len(:aHead)
   :aFoot   := Array(nColTsb)
   :aFPict  := Array(nColTsb)
   :aName   := Array(nColTsb)
   :aAlign  := Array(nColTsb)
   :aField  := Array(nColTsb)
   :aFSize  := Array(nColTsb)
   :aFAlign := Array(nColTsb)

   FOR i := 1 TO nColTsb
      cNam := :aHead[ i ]
      IF AT(";", cNam) > 0
         :aHead[ i ] := ATREPL( ";", cNam, CRLF )
      ENDIF
      :aFoot [ i ] := "" //hb_ntos( i )
      :aName [ i ] := FieldName( i ) //"NAME_" + hb_ntos( i + 1 )   // +1 Виртуальная колонка
      :aField[ i ] := FieldName( i )
      //aFSize[ i ] := myHeadSize( aHead[ i ] , i , FieldType( i ) )
      :aAlign[ i ] := DT_LEFT
      switch FieldType( i )
         case 'N' ; :aAlign[ i ] := DT_RIGHT  ; exit
         case 'D' ; :aAlign[ i ] := DT_CENTER ; exit
         case 'L' ; :aAlign[ i ] := DT_CENTER ; exit
      end switch
      :aFAlign[ i ] := DT_CENTER
   NEXT

   // суперхидер
   :aSupHd := {}
   AADD( :aSupHd, cTitleTsb )  // здесь использ. ОДИН суперхидер

   // построение таблицы в процентном соотношении
   // nWTsb = 100%
   w := 0
   FOR i := 1 TO LEN(aTsbColumn)
      w += aTsbColumn[i]
   NEXT
   aProc := {}
   FOR i := 1 TO LEN(aTsbColumn)
      AADD( aProc , INT( 96 / w * aTsbColumn[i] ) ) // 96%
   NEXT
   //? "aProc=", HB_ValtoExp(aProc)
   // ручная доводка ширины колонок
   aProc := {7, 7, 84}

   IF LEN(aProc) # nColTsb
      MsgDebug("Массив [%] # кол-ву колонок!",aProc,nColTsb)
   ENDIF
   k := 0
   FOR i := 1 TO LEN(aProc)
      k += aProc[i]
   NEXT
   IF k > 100
      MsgDebug("100% > сумме % колонок!",k,aProc)
   ENDIF
   //? "%-",k, HB_ValToExp(aProc)

   // назначить ширину колонок
   k := 0
   FOR i := 1 TO LEN(aProc)
      :aFSize[ i ] := nWTsb/100 * aProc[i]
      k += aProc[i]
   NEXT
   IF k > 100
      MsgDebug("100% > сумме % колонок!",k,aProc)
   ENDIF
   // ручная доводка
   :aFSize[ 2 ] += 15
   :aFSize[ 3 ] -= 15

   // первый вариант
   :aArray   := ALIAS()
   // фонты для таблицы
   :aTsbFont := NIL
   //:aBrush   := { 255, 255, 230 }
   //:aNumber  := { 1, 40 }

   END WITH

RETURN oDatos  // возврат контейнера с данными

////////////////////////////////////////////////////////////////////////
// создадим базу со структурой
STATIC FUNCTION my3SpravDbfNew(cPath, cFile, a2Dim, lDbfDelFile)
   LOCAL aDbf, cFileDbf, cFileIndx, cAlias, nI, cField
   LOCAL aTsbColumn, nMax, nKolvo, lNew
   DEFAULT lDbfDelFile := .F.

   nMax := 0
   FOR nI := 1 TO LEN(a2Dim)
      nMax := MAX(nMax, LEN(ALLTRIM(a2Dim[nI,2])))
   NEXT
   nMax += 2

   aDbf := {}
   AAdd( aDbf, {"Check"  , "L",      1, 0 } )
   AAdd( aDbf, {"nCode"  , "N",      6, 0 } )
   AAdd( aDbf, {"cCode"  , "C",   nMax, 0 } )
   aTsbColumn := { 3, 6, nMax }      // кол-во символов в колонках

   cFileDbf  := cPath + cFile
   cFileIndx := cFileDbf + ".cdx"
   cAlias    := cFileNoExt( "TMP_" + cFileDbf )
   lNew      := .F.

   IF lDbfDelFile
      DeleteFile(cFileDbf) // удалить
   ENDIF

   IF !FILE(cFileDbf)
      lNew := .T.
   ELSE
      Use (cFileDbf) Via "DBFCDX" Alias (cAlias) Exclusive New CODEPAGE "RU1251"
      nKolvo := Lastrec()
      IF Len(a2Dim) # nKolvo
         lNew := .T.
         (cAlias)->(DbCloseArea())
      ENDIF
   ENDIF

   IF lNew
      DbCreate( cFileDbf, aDbf )
      Use (cFileDbf) Via "DBFCDX" Alias (cAlias) Exclusive New CODEPAGE "RU1251"
      DeleteFile(cFileIndx) // удалить индекс, обязательно
      cField := "nCode"
      INDEX ON &cField TAG NCODE TO (cFileIndx)

      FOR nI := 1 TO Len(a2Dim)
         APPEND BLANK
         (cAlias)->nCode := a2Dim[nI,1]
         (cAlias)->cCode := a2Dim[nI,2]
      NEXT
   ENDIF

   DbSetOrder(0)

RETURN { ALIAS(), aTsbColumn }

//////////////////////////////////////////////////////////////////////////////
// расчёт высоты фонта в зависимости от разрешения экрана  для менюшек
FUNCTION mySizeFontMenuResolutionScreen()
   LOCAL nFontSize

   IF GetDesktopHeight() <= 600
      nFontSize := 12
   ELSEIF GetDesktopHeight() >= 768 .OR. GetDesktopHeight() <= 864
      nFontSize := 14
   ELSEIF GetDesktopHeight() > 864  //   == 1080
      nFontSize := 16
   ELSEIF GetDesktopHeight() == 1080
      nFontSize := 16
   ENDIF

   nFontSize  := IIF( Large2Fonts(), nFontSize-2, nFontSize )

   IF GetDesktopHeight() == 1080
      nFontSize := 16
   ENDIF

RETURN nFontSize

