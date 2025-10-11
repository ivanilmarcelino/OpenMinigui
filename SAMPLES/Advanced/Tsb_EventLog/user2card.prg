/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2020-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Карточка/Card: 18.09.20 - 09.09.25
*/

#include "minigui.ch"
#include "TSBrowse.ch"

#define PATH_TEMP  App.Cargo:cPathTemp
//////////////////////////////////////////////////////////////////////////////
FUNCTION User2LogCard(aWin,oBrw,aTsbCard,cIcon)
   LOCAL i, y, x, w, h, cObj, aCap, aIco, aClr, nPost, cCapt, aFont, aFntClr
   LOCAL oCard, nY, nX, nW, nH, nWL, nWTbl, nHTbl, cFName, nFSize, aTsbFnt
   LOCAL oCargo, nGaps, cLang, cTitle

   ? ProcNL(), "Card !", aWin, oBrw:cAlias, aTsbCard, cIcon

   //SET WINDOW THIS TO oBrw
   oCargo := oBrw:Cargo
   //SET WINDOW THIS TO

   cFName  := App.Cargo:cFontName
   nFSize  := 14 //App.Cargo:nFontSize
   aTsbFnt := oCargo:aTsbFont
   y       := 10
   x       := 10
   w       := 140
   h       := 60
   nGaps   := 10
   //aFont := { "Tahona", 10 , .T. , .F. }
   aFont   := { App.Cargo:cFontName2, nFSize , .T. , .F. }
   aFntClr := {  BLACK, YELLOW }

#ifdef KEY_ENG
   aCap    := { "Help", "First;entry", "Next;entry", "Previous;entry", "Last;entry", "View;entry", "Exit;card" }
   cLang   := "Records: "
   cTitle  := "Event Card"
#else
   aCap    := { "Помощь", "Первая;запись", "Следущая;запись", "Предыдущая;запись",;
                "Последняя;запись", "Просмотр;записи", "Выход из;карточки" }
   cLang   := "Записи: "
   cTitle  := "Карточка события"
#endif

   aIco    := { {"iHelp48x1"   , "iHelp48x2"   } , {"iRecFrst48x1", "iRecFrst48x2"} ,;
                {"iRecNext48x1", "iRecNext48x2"} , {"iRecPrev48x1", "iRecPrev48x2"} ,;
                {"iRecLast48x1", "iRecLast48x2"} , {"iLogfile48"  , "iLogfile48x2"} ,;
                {"iReturn48x1" , "iReturn48x2" }    }

   aClr    := { CLR_FB, CLR_GRAY, CLR_GRAY, CLR_GRAY, CLR_GRAY, CLR_VIBER, CLR_HRED }

   SET FONT TO cFName, nFSize

   DEFINE WINDOW Form_2LogCard At aWin[1], aWin[2] WIDTH aWin[3] HEIGHT aWin[4] ;
      TITLE cTitle ICON cIcon                                ;
      MODAL NOSIZE                                           ;
      BACKCOLOR aWin[5]                                      ;
      ON INIT     {|| This.Topmost := .F. , _wSend(20), oCard:Setfocus()  }
      //ON RELEASE  {|| MsgDebug(This.Title)   }

      nW := This.ClientWidth        // ширина окна
      nH := This.ClientHeight       // высота окна

      (This.Object):Cargo           := oKeyData()

      FOR i := 1 TO Len(aIco)
         cObj  := "Btn_"+StrZero(i,2)
         nPost := IIF( i == Len(aIco), 98, i )   // цифры - событие
         cCapt := ATREPL( ";", aCap[i], CRLF )
         my2BUTTON(y, x, w, h, cObj, cCapt, {aClr[i],WHITE}, , aIco[i], aFntClr, aFont, nPost )
         // переназначим здесь на номер nPost
         This.&(cObj).Action := {|| _wPost(This.Cargo:nPost, , This.Name) }
         x += This.&( cObj ).Width + nGaps
      NEXT

      nWL := nW - x

      @ y, x LABEL Lbl_1 WIDTH nWL HEIGHT h VALUE cLang + " 100 / 100" ;
        FONTCOLOR WHITE SIZE nFSize + 6 CENTERALIGN VCENTERALIGN TRANSPARENT

      //////////////////// таблица ///////////////////
      nY    := y + h + y
      nX    := nGaps
      nWTbl := nW - nGaps*2
      nHTbl := nH - nY - nGaps
      oCard := myCardTable( nY, nX, nWTbl, nHTbl, aTsbFnt, aTsbCard )
      oCard:Cargo:cRecno := cLang

      (This.Object):Cargo:oCard := oCard   // на окне запомнили, объект tsb уже готовый
      //@ nY, nX LABEL Lbl_Table WIDTH nWTbl HEIGHT nHTbl VALUE "Таблица" ;
      //  BACKCOLOR SILVER CENTERALIGN VCENTERALIGN

      ON KEY ESCAPE ACTION {|| _wPost(98) }  // выход по ESC

      WITH OBJECT This.Object
         :Event(  0, {|  | This.Topmost := .F. , oCard:Setfocus()               } )
         :Event(  1, {|  | AlertInfo("Help will be here !") , oCard:Setfocus()  } )
         :Event(  2, {|  | oBrw:GoTop(),     DoEvents(), _wSend(20)             } )  // GoFirst
         :Event(  3, {|  | oBrw:Skip(1),     DoEvents(), _wSend(20)             } )  // GoNext
         :Event(  4, {|  | oBrw:Skip(-1),    DoEvents(), _wSend(20)             } )  // GoPrev
         :Event(  5, {|  | oBrw:GoBottom(),  DoEvents(), _wSend(20)             } )  // GoLast
         :Event(  6, {|  | myRecnoPrint(oCard)                                  } )  // Print
         :Event( 20, {|  | myRecnoRead(oBrw,oCard)                              } )  // Refresh
         :Event( 98, {|ow| ow:Release() } )
      END WITH

   END WINDOW

   ACTIVATE WINDOW Form_2LogCard // ON INIT {|| This.Topmost := .T. }

RETURN NIL

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myCardTable( nY, nX, nW, nH, aFont, aTsbCard )
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aField, aName, aFAlign, aArray
   LOCAL oCard, aSupHd, nWColumn1, nWColumn3, cName, nImg, nMax, aDatos, nI

   nMax   := 0
   aDatos := {}
   FOR nI := 1 TO LEN(aTsbCard)
      cName := ATREPL( ";", aTsbCard[nI,1], " " ) + ":"
      nImg  := IIF( LEN(aTsbCard[nI,6]) > 0 , 1 , 2 )
      AADD( aDatos, { cName , nImg, "", aTsbCard[nI,2], aTsbCard[nI,6] } )
      nMax := MAX( nMax, LEN(cName) )
   NEXT

   nWColumn1  := GetTextWidth( Nil, REPL("H",nMax+2), GetFontHandle("TsbNorm") ) + 20
   nWColumn3  := nW - nWColumn1 - 40 - 32 - 100 - 100 - GetVScrollBarWidth() - 1
   aArray     := aDatos

#ifdef KEY_ENG
   aHead      := { "Name"        , " "   , "Value"    , "DB Field", "Function"}
#else
   aHead      := { "Наименование", " "   , "Значение" , "Поле БД" , "Функция"}
#endif

   aSize      := { nWColumn1     ,  32   , nWColumn3  , 100      , 100      }
   aFoot      := .T.     // создаем пустые значения для подвала
   aPict      := NIL
   aAlign     := {      2        ,  1    , 0          , 0        , 0        }
   aField     := NIL     // второй вариант - Tsbrowse-SetArrayTo()
   aName      := { "tNAM"        , "tIMG", "tVAL"     , "tFLD"   , "tFUNC"  }
   aFAlign    := NIL     // Footer align
   aSupHd     := {}

/*
? "aHead ="  , aHead   ; ?v aHead   ; ?
? "aSize ="  , aSize   ; ?v aSize   ; ?
? "aAlign =" , aAlign  ; ?v aAlign  ; ?
*/
   IF LEN(aHead) == 0 .OR. LEN(aName) == 0
      MsgDebug("Error ! aHead=0 , aName=0, aField=0 ???" )
   ENDIF

   DEFINE TBROWSE oCard                                  ;
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
          COLNUMBER  { 1, 40 } EDIT
          // ENUMERATOR LOCK EDIT SELECTOR .T.

          :Cargo  := oKeyData()       // создает объект без переменных (условно пустой) используем ниже по коду

          mySupHdTsb( oCard, aSupHd )  // SuperHeader
          mySetTsb( oCard )            // настройки таблицы
          myColorTsb( oCard )          // цвета на таблицу
          myColorTsbElect( oCard )     // цвета избранные
          myDelColumnTsb( oCard )      // убрать колонки из таблицы
          mySet2Tsb( oCard )           // настройки таблицы дополнительные
          mySetImageTsb( oCard )       // картинки в таблице
          mySetEditTsb( oCard )        // настройки редактирования


          //:bGotFocus := {|ob| myGotFocusTsb(ob)     }
          //:bOnDraw   := {|ob| SayStatusBar(ob)    }   // показ StatusBar - Recno/Column

          /*:UserKeys( VK_F3, {|ob| myListColumn(ob)   })  // инфо по списку колонок
          :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3:=Nil, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
          :UserKeys( VK_RETURN, {|ob| _wPost(24, ob:cParentWnd), .F. } )
          */

   //END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }
   END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:oPhant:nClrHeadBack := ob:Cargo:nClr4, ;
                                             ob:oPhant:nClrFootBack := ob:Cargo:nClr10,;
                                             ob:Refresh() }


RETURN oCard

//////////////////////////////////////////////////////////////////
// суперхидер
STATIC FUNCTION mySupHdTsb( oBrw, aSupHd )
   LOCAL O := oBrw:Cargo  // использовать из контейнера свои переменные

   WITH OBJECT oBrw
     IF LEN(aSupHd) == 0
       :AddSuperHead( 1, :nColCount(), "" )
       :nHeightSuper := 0        // высота заголовка
     ELSE
       :AddSuperHead( 1, :nColCount(), aSupHd[1] )
     ENDIF
   END WIDTH

RETURN NIL

//////////////////////////////////////////////////////////////////////
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
      :nHeightCell   += 6           // высота ячеек таблицы добавит 2 пиксела
      :nCellMarginLR := 0           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nStatusItem   :=  0
      :lNoKeyChar    := .T.         // method :KeyChar disabled
      :lCheckBoxAllReturn := .T.    // Enter modify value oCol:lCheckBox
      :lPickerMode        := .F.    // формат даты нормальный
      :nHeightHead  := :nHeightCell * 1.4   // высота шапки таблицы
      :nHeightFoot  := :nHeightCell + 1.4   // высота строки подвала
   END WITH
RETURN Nil

//////////////////////////////////////////
STATIC FUNCTION mySet2Tsb( oBrw )
   LOCAL nLen, cBrw, nTsb

   WITH OBJECT oBrw
      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth   // ширины внутри тсб
      nLen := :GetAllColsWidth() - 1     // ширина всех колонок видимых
      /*IF nLen > nTsb                     // колоноки не входят в показ -> HScroll
         :lAdjColumn  := .T.             // выравнивать последнюю колонку при прорисовке
         :lNoHScroll  := .F.             // добавить\вкл. ползунок горизонтальный
         :lMoreFields := ( :nColCount() > 30 ) // если колонок больше, то вкл.
                                               // метод работы, что бы не
                                               // зависала прорисовка тсб
      ELSE
         :AdjColumns()  // колонки входят в окно тсб, уберем вертикальную "дырку"
                        // распределив ее значение по колонкам, растянув
      ENDIF */

      //IF :nLen > :nRowCount()
         :ResetVScroll( .T. )
         :oHScroll:SetRange( 0, 0 )
      //ENDIF

      :nFreeze     := :nColumn("tIMG") // заморозить таблицу до этого столбца
      :lLockFreeze := .T.              // избегать прорисовки курсора на замороженных столбцах

      :GoPos(1, :nFreeze + 1 )

      :AdjColumns("tVAL") // добавим пробелы в эту колонку, уберем вертикальную "дырку"

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
      :n_BLUE     := -CLR_BLUE
      :n_HBLUE    := -CLR_HBLUE
      :n_BLACK    := -RGB(1,1,1)
      :n_HRED     := -CLR_HRED
      :n_RED      :=  CLR_RED
      :nRED       :=  CLR_RED
      :nBLUE      :=  CLR_BLUE
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
      //:nFCDelRec  := RGB( 248, 209, 211 ) // красный осветл.
      :nBCYear    := RGB( 251, 213, 181 )   // оранжевый осветл. 40%
      :nFCYear    := RGB( 109,  15, 20  )   // красный
      :nBCError   := CLR_HRED
      :nFCError   := CLR_BLUE
      :nBLUESKYPE := RGB(  0,176,240 )          // голубой, как SKYPE
      :nBLUEBLUE  := RGB(  9, 77,181 )          // сине-голубой
      :nBLUE4     := RGB( 84,141,212 )          // синий-осветл. ?0%
      :nBLUE5     := CLR_FB                     // голубой, как FB
      :nBLUE40    := RGB(198,217,240 )          // синий-осветл. 40%
      :nBLUEDARK  := RGB(  0,155,173 )          // темно-голубой
      :nBLACKLigh := RGB( 60, 60,60  )          // черный-осветлённый

      // 2. переменные RGB( ... ) для использования
      :nRgb0      :=  RGB(  0,  0,  0)
      :nRgb1      :=  RGB(180,180,180)
      :nRgb2      :=  RGB(255,255,240)
      :nRgb3      := -RGB(128,225,225)
      :nMy2       :=  RGB(255,255,240)          // белый

      // 3. переменные (aColors items number) от номера позиции в :SetColor( {...}, ... ) из TsBrowse.ch
      :nClrLine   :=  :nRgb1
      :nClr1      :=  :nRgb0         // #define CLR_         1   // text
      :nClr2      :=  :nMy2   //:nRgb2                  // #define CLR_PANE     2   // back
      :nClr3      :=  :nWHITE                   // #define CLR_HEADF    3   // header text
//      :nClr4    := {:nBLUEDARK, :nBtnFace}    // #define CLR_HEADB    4   // header back
      :nClr4      := {:nBLUE4,:nBLACKLigh}      // #define CLR_FOOTB   10   // footer back
      :nClr5      :=  :nRgb0                    // #define CLR_FOCUSF   5   // focused text
      :nClr6_1    :=  :n_HRED                   // #define CLR_FOCUSB   6 1 // focused back
      :nClr6_2    :=  :n_BLACK                  // #define CLR_FOCUSB   6 2 // focused back
      :nClr9      :=  :nWHITE                   // #define CLR_FOOTF    9   // footer text
//      :nClr10   := {:nBLUEDARK, :nBtnFace}    // #define CLR_FOOTB   10   // footer back
      :nClr10     := {:nBLUE4,:nBLACKLigh}      // #define CLR_FOOTB   10   // footer back
      :nClr11     :=  :nRgb0                    // #define CLR_SELEF   11   // focused inactive (or selected) text
      :nClr12_1   :=  :n_BLUE                   // #define CLR_SELEB   12 1 // focused inactive (or selected) back
      :nClr12_2   :=  :nRgb3                    // #define CLR_SELEB   12 2 // focused inactive (or selected) back
      :nClr16     := {:nBLUE4,:nBLACKLigh}      // 16, фона спецхидер
      :nClr17     :=  :nYELLOW                  // 17, текста спецхидер
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
      :SetColor( {16}, { O:nClr16  } )   // 16, фон суперхидера
      :SetColor( {17}, { O:nClr17  } )   // 17, текст суперхидера
      :hBrush   := CreateSolidBrush( 139, 160, 228 )   // цвет фона под таблицей
   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myColorTsbElect( oBrw )
   LOCAL lVirtual, nCol, oCol, aCol := oBrw:aColumns
   LOCAL cCol, O := oBrw:Cargo  // использовать из контейнера свои переменные
   LOCAL nAt := oBrw:nAt

   oBrw:GetColumn("ORDKEYNO"):nClrBack  := O:nBLUE40
   oBrw:GetColumn("tNAM"):nClrBack      := O:nBLUE40
   oBrw:GetColumn("tFLD"):nClrBack      := O:nBLUE40
   oBrw:GetColumn("tFUNC"):nClrBack     := O:nBLUE40

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
         //oCol:nClrBack := { |nr,nc,ob| nr:=nc, iif( Eval(ob:GetColumn("NUSER"):bData) > 2020 , O:nBCYear, O:nClr2 ) }
         //oCol:nClrFore := { |nr,nc,ob| nr:=nc, iif( Eval(ob:GetColumn("NUSER"):bData) > 2020 , O:nFCYear, O:nClr1 ) }
         // или можно так
         //oCol:nClrBack := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->NUSER > 2020 , O:nBCYear, O:nClr2 ) }
         //oCol:nClrFore := { |nr,nc,ob| nr:=nc, iif( (ob:cAlias)->NUSER > 2020 , O:nFCYear, O:nClr1 ) }

         // цвет фона для всех ячеек строки таблицы  - несколько условий
         //oCol:nClrBack := { |a,n,b| myTsbColorBackLine(a,n,b)   }
         // цвет текста для всех ячеек строки таблицы - несколько условий
         //oCol:nClrFore := { |a,n,b| myTsbColorForeLine(a,n,b)   }
      ENDIF
   NEXT

   // цвет фона шапки таблицы для добавочного списка колонок
   /*FOR EACH cFld IN { "ID", "TS", "VM", "IM", "DT", "TT" }
      IF cFld $ "IM,DT,TT"
         oBrw:GetColumn(cFld):nClrHeadBack := {|| oBrw:Cargo:nORANGE }  // цвет фона шапка таблицы
         oBrw:GetColumn(cFld):nClrFootBack := {|| oBrw:Cargo:nORANGE }  // цвет фона подвала таблицы
      ELSE
         oBrw:GetColumn(cFld):nClrHeadBack := {|| oBrw:Cargo:nRED }  // цвет фона шапка таблицы
         oBrw:GetColumn(cFld):nClrFootBack := {|| oBrw:Cargo:nRED }  // цвет фона подвала таблицы
      ENDIF
   NEXT */

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

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myDelColumnTsb( oBrw )
   LOCAL nCol, cNam, aHideCol := {}
   LOCAL aCol := oBrw:aColumns
   LOCAL cDelCol, oCol, cCol, cType, aDelCol

   aDelCol := { "tFLD", "tFUNC" }
   // список всех колонок таблицы
   //? "------- список всех колонок таблицы -------"
   FOR nCol := 1 TO Len(aCol)
      cNam := oBrw:aColumns[nCol]:cName
      //? nCol, cNam
   NEXT
   // список удаляемых колонок
   cDelCol := ","
   FOR nCol := 1 TO Len(aDelCol)
      cNam := UPPER(aDelCol[nCol])    // поле базы
      //IF aDelCol[nCol,5] == 0       // показ в таблице
         cDelCol += cNam + ","
      //ENDIF
   NEXT
   // уберем колонки
   FOR nCol := 1 TO Len(aCol)
      oCol  := aCol[ nCol ]
      cType := oCol:cDataType
      cCol  := UPPER(oCol:cName) // для всех вариантов
      IF ","+cCol+"," $ cDelCol
         AADD( aHideCol , nCol )
      ENDIF
   NEXT

   oBrw:HideColumns( aHideCol ,.t.)   // скрыть колонки

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
// картинки в таблице
STATIC FUNCTION mySetImageTsb( oBrw )
   LOCAL oCol

   WITH OBJECT oBrw

      :aBitMaps := { LoadImage("bSearch32"), LoadImage("bNil32") }
      oCol := :GetColumn("tIMG")
      //oCol:nClrBack := CLR_WHITE
      //oCol:nClrFore := CLR_WHITE
      //oCol:hFont    := oCol:hFontHead
      oCol:bData    :=  {||Nil}
      oCol:cData    := '{||Nil}'
      oCol:nAlign   := nMakeLong( DT_CENTER, DT_CENTER )
      oCol:nHAlign  := nMakeLong( DT_CENTER, DT_CENTER )
                                               // колонка 2, а не 3 колонка !!!
      oCol:uBmpCell := {|nc,ob| nc := ob:aArray[ ob:nAt ][2], ob:aBitMaps[ nc ] }
      //oCol:uBmpCell := {|nc,ob| nc := ob:aArray[ ob:nAt ][2], ;
      //                           _LogFile(.T.,nc,ob:aArray[ ob:nAt ][1]) } - test

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// настройки редактирования
STATIC FUNCTION mySetEditTsb( oBrw )
   LOCAL i, oCol, cTyp, cNam

   WITH OBJECT oBrw

      // удаление/восстановление записи разрешена
      // кнопка для удаления, будет работать и на восстановление
      //:SetDeleteMode( .T., .F., {|| AlertYesNo(iif((oBrw:cAlias)->(Deleted()), "Восстановить", "Удалить") + ;
      //                                            " запись в таблице ?", "Подтверждение") } )

      :SetAppendMode( .F. )      // запрещена вставка записи в конце базы стрелкой вниз

      AEval( :aColumns, {|oc|                   // в списке удаленных edit запрещена
                          If oc:lEdit
                             oc:bPrevEdit := {|xv,ob| xv := ! (ob:cAlias)->(Deleted()) }
                          EndIf
                          Return Nil
                        } )

      FOR i := 1 TO Len(:aColumns)
         oCol := :aColumns[ i ]
         cNam := UPPER(oCol:cName)
         cTyp := oCol:cFieldTyp
         // edit колонок
         IF cTyp $ "+=^"   // Type: [+] [=] [^]
            oCol:bPrevEdit := {|| AlertStop("Disable editing of field type !") , FALSE }
         ENDIF
         IF cNam ==  UPPER("tVal")
            oCol:lEdit := .T.   // редактирования
            oCol:bPrevEdit := {|| myEditCard( oBrw ) , FALSE }
         ELSE
            oCol:lEdit := .F.   // запрет редактирования
         ENDIF
         //? ProcNL(), i, cNam, cTyp, oCol:lEdit

      NEXT

   END WIDTH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// показ сканов документоа
STATIC FUNCTION myEditCard( oCard )
   LOCAL cVal, cLog, cTxt, cMsg

#ifdef KEY_ENG // for this project demo-en.hbp
   cMsg := "Do you want to open the editor with this cell value ?;"
#else
   cMsg := "Вы хотите открыть редактор с этим значением ячейки ?"
#endif

   cVal := ALLTRIM( oCard:GetValue("tFUNC") )
   IF LEN(cVal) == 0
   ELSE
      cTxt := myGetPathLog(oCard)
      IF LEN(cTxt) > 0
         IF AlertYesNo( cMsg, "Open file", .T., "iQuest64", 64, { LGREEN, RED }, .T. )
            cLog := PATH_TEMP + "\error-log.log"
            HB_MemoWrit( cLog, cTxt )
            DO EVENTS ; wApi_Sleep(100)
            ShellExecute( , 'open', cLog, , , SW_SHOWNORMAL)
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetPathLog(oCard)
   LOCAL nRecno, nI, cFind, cPath := ""
   LOCAL nCell := oCard:nCell  // номер текущего столбца в таблице
   LOCAL nRPos := oCard:nAt    // номер текущей строки в таблице

   nRecno := (oCard:cAlias)->(RecNo())
   oCard:GoTop()  // переход на начало таблицы
   FOR nI := 1 TO oCard:nLen
      cFind := oCard:GetValue("tFLD")
      IF cFind ==  "REM" //"SCANPATH"
         cPath := ALLTRIM( oCard:GetValue("tVAL") )
         EXIT
      ENDIF
      oCard:Skip(1)
      DO EVENTS
   NEXT

   oCard:Reset()
   //oCard:GoToRec( nRecno )
   oCard:GoPos(nRPos,nCell)   // передвинуть на строку где первоначально стоял МАРКЕР
   oCard:SetFocus()
   DO EVENTS

RETURN cPath

/////////////////////////////////////////////////////////////////////////////////////
// событие 20 - Refresh
STATIC FUNCTION myRecnoRead(oBrw,oCard)
   LOCAL cStr, cAls := oBrw:cAlias
   LOCAL nI, cField, xVal, cType

   cStr := HB_NtoS(oBrw:nAt) + " / " + HB_NtoS(oBrw:nLen)
   Form_2LogCard.Lbl_1.Value := oCard:Cargo:cRecno + cStr

   oCard:GoTop()    // переход на начало таблицы / go to the beginning of the table
   FOR nI := 1 TO oCard:nLen
      cField := oCard:GetValue("tFLD")
      xVal   := (cAls)->&cField
      cType  := VALTYPE(xVal)
      IF cType # "C"
         xVal := cValToChar(xVal)
      ENDIF
      oCard:SetValue("tVAL", " " + xVal)
      oCard:Skip(1)
      DO EVENTS
   NEXT

   oCard:Reset()
   oCard:GoTop()  // переход на начало таблицы / go to the beginning of the table
   oCard:SetFocus()
   DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// событие 6 - Print
STATIC FUNCTION myRecnoPrint(oCard)
   LOCAL nMax, nI, cNam, cVal, aDim := {}, cStr := ""
   LOCAL cPath := PATH_TEMP
   LOCAL cFile := "Сведения_по_событию.txt"

   nMax := 0
   oCard:GoTop()  // переход на начало таблицы
   FOR nI := 1 TO oCard:nLen
      cNam := oCard:GetValue("tNAM")
      cVal := oCard:GetValue("tVAL")
      AADD( aDim, { ALLTRIM(cNam), cVal } )
      nMax := MAX( nMax, LEN(cNam) )
      //cStr += cNam + cVal + CRLF
      oCard:Skip(1)
      DO EVENTS
   NEXT
   nMax += 1

   FOR nI := 1 TO LEN(aDim)
      cStr += PADL(aDim[nI,1],nMax) + aDim[nI,2] + CRLF
   NEXT

   HB_MemoWrit( cPath + cFile, cStr )
   ShellExecute( 0, "Open", cPath + cFile,,, 1 )

   oCard:Reset()
   oCard:GoTop()  // переход на начало таблицы
   oCard:SetFocus()
   DO EVENTS

RETURN NIL
