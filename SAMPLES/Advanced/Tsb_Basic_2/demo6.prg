/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2019 Sergej Kiselev <bilance@bilance.lv>
 *
 * Таблица по условному индексу и карточка полей БД 
 * Многопользовательская работа с базой 
 * Table on conditional index and database field card
 * Multiuser work with the database
*/
#include "hmg.ch"
#include "TSBrowse.ch"
#include "Dbinfo.ch"

REQUEST DBFCDX

STATIC nStaticTime, aStaticTableColor, cStaticUser, lStaticEditCard

DECLARE WINDOW Form_0
/////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL oBr, aAlias, aHWin, cUser, nOrder
   LOCAL nY, nX, nW, nH, nG, nBtnW, nBtnH
   LOCAL cForm  := 'Form_0'
   LOCAL cTitle := "(6) TsBrowse + Conditional index"                   

   RddSetDefault( 'DBFCDX' )

   SET DATE FORMAT 'DD.MM.YYYY'
   SET DELETED ON
   SET AUTOPEN OFF
   SET OOP ON

   SET DIALOGBOX CENTER OF PARENT
   DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 12  // for HMG_Alert()
   SET CENTERWINDOW RELATIVE PARENT                // for HMG_Alert()

   aHWin  := OnlyOneInstance(cTitle)  // сколько программ запущено
   aAlias := UseOpenBase()            // создать/открыть тестовую базу 
   Index2Create(aHWin)                // создать условный индекс для каждой программы
   nOrder      := INDEXORD()             
   nStaticTime := SECONDS()           // включить время для показа таймера
   cUser       := "  " + HB_NtoS( LEN(aHWin) + 1 ) + "-User"  
   cStaticUser := ALLTRIM(cUser)

   DEFINE WINDOW  &cForm AT 20,10 WIDTH 590 HEIGHT 600               ;
      TITLE       cTitle + cUser                                     ;
      ICON        "MG_ICO"                                           ;
      MAIN        NOMAXIMIZE NOSIZE                                  ;
      ON INIT     ( ChangeWinBrw(oBr,aHWin), This.Topmost := .F. )   ;
      ON RELEASE  AEval( aAlias, {|ca| (ca)->( dbCloseArea(ca) ) } )    // закрыть все базы при выходе

      DEFINE STATUSBAR
         STATUSITEM "" WIDTH 10
         STATUSITEM cTitle + " - network opening of the database !"  WIDTH 390 FONTCOLOR RED
         STATUSITEM "00:00:00" 
         STATUSITEM "Order: " + HB_NtoS(nOrder) 
         STATUSITEM cUser  
      END STATUSBAR
      
      nY := nX := nG := 5 
   
      nBtnH := 30  ; nBtnW := ( This.ClientWidth - nG * 7 ) / 6 

      @ nY, nX BUTTONEX Button_10 WIDTH nBtnW HEIGHT nBtnH ;
        CAPTION "10 Recno" SIZE 10 BOLD BACKCOLOR SILVER   ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                ;
        ACTION RecnoCreateCondition(oBr,10)
        nX += nBtnW + nG

      @ nY, nX BUTTONEX Button_20 WIDTH nBtnW HEIGHT nBtnH ;
        CAPTION "20 Recno" SIZE 10 BOLD BACKCOLOR SILVER   ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                ;
        ACTION RecnoCreateCondition(oBr,20)
        nX += nBtnW + nG

      @ nY, nX BUTTONEX Button_Refresh WIDTH nBtnW HEIGHT nBtnH ;
        CAPTION "Refresh" SIZE 10 BOLD BACKCOLOR SILVER         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                     ;
        ACTION RecnoRefresh(oBr, .t.)     // с обновлением времени
        nX += nBtnW + nG

      @ nY, nX BUTTONEX Button_BaseInfo WIDTH nBtnW HEIGHT nBtnH ;
        CAPTION "(i) Base" SIZE 10 BOLD BACKCOLOR SILVER         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                      ;
        ACTION {|| InfoDbase() , oBr:SetFocus() }
        nX += nBtnW + nG

      @ nY, nX BUTTONEX Button_Help WIDTH nBtnW HEIGHT nBtnH  ;
        CAPTION "(i) Help" SIZE 10 BOLD BACKCOLOR SILVER      ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                   ;
        ACTION {|| MsgAbout() , oBr:SetFocus() }
        nX += nBtnW + nG

      @ nY, nX BUTTONEX Button_Exit WIDTH nBtnW HEIGHT nBtnH ;
        CAPTION "Exit" SIZE 10 BOLD BACKCOLOR SILVER         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                  ;
        ACTION ThisWindow.Release()

      nY  := This.Button_10.Row + This.Button_10.Height + 5
      nX  := This.Button_10.Col
      nW  := This.ClientWidth  - nX * 2
      nH  := This.ClientHeight - This.StatusBar.Height - nY - 5

      oBr := CreateBrowse(nY, nX, nW, nH)      // создать таблицу

      // включить таймер 1 раз в полминуты вызов функции
      DEFINE TIMER Timer_1 INTERVAL 30 * 1000 ACTION RecnoRefresh(oBr, .t.)
      // включить таймер 2 для отображения времени Timer_1 каждую секунду
      DEFINE TIMER Timer_2 INTERVAL 1000 ACTION Timer1Show()

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm ON INIT ( This.Topmost := .T., oBr:SetFocus() )

RETURN

/////////////////////////////////////////////////////////////////////
FUNCTION CreateBrowse( nY, nX, nW, nH )
   LOCAL oBrw, cAls := ALIAS()
   
   DEFINE TBROWSE oBrw OBJ oBrw AT nY, nX WIDTH nW HEIGHT nH ALIAS cAls GRID ;
          COLORS    { CLR_BLACK, CLR_BLUE }     ;
          FONT      "Tahoma"                    ;
          SIZE      12                          ;
          COLUMNS   { "F2", "F1", "F3", "CODE" }

   :SetAppendMode( .F. )      // вставка записи запрещена (в конце базы стрелкой вниз)
   :SetDeleteMode( .T., .T. ) // удаление записи разрешено

   :lNoHScroll  := .T.        // показ горизонтального скролинга
   :lCellBrw    := .F.
   :lInsertMode := .T.        // флаг для переключения режима Вставки при редактировании
   :lPickerMode := .F.        // ввод формата колонки типа ДАТА сделать через цифры

    ADD COLUMN TO TBROWSE oBrw DATA {|| (oBrw:cAlias)->( OrdKeyNo() ) } ;  
        HEADER CRLF + "NN" SIZE 60 ;
        COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
        NAME NN                             

   :LoadFields(.F.)

   // Set columns width
   :SetColSize( oBrw:nColumn( "F1"   ), 100  )
   :SetColSize( oBrw:nColumn( "F2"   ), 200  )
   :SetColSize( oBrw:nColumn( "F3"   ),  80  )
   :SetColSize( oBrw:nColumn( "CODE" ),  90  )

   // Set names for the table header
   :aColumns[1]:cHeading := "NN"      
   :aColumns[2]:cHeading := "Text"      
   :aColumns[3]:cHeading := "Date"      
   :aColumns[4]:cHeading := "Number"      
   :aColumns[5]:cHeading := "CodeList" 

   :GetColumn('F1'):cPicture := Nil       // пустые поля отображать как пробел
   :GetColumn('F1'  ):nAlign := DT_CENTER
   :GetColumn('CODE'):nAlign := DT_CENTER

   :lPickerMode  := .F.                 // ввод формата колонки типа ДАТА сделать через цифры
   :lNoKeyChar   := .T.                 // отключить ВСЕ колонки: edit от нажатия клавиш цифр\букв 
   :lNoGrayBar   := .F.                 // показывать неактивный курсор
   :nWheelLines  := 1                   // прокрутка колесом мыши
   :nClrLine     := COLOR_GRID          // цвет линий между ячейками таблицы
   :lNoChangeOrd := TRUE                // убрать сортировку по полю
   :nColOrder    := 0                   // убрать значок сортировки по полю
   :lCellBrw     := TRUE                // celled browse flag
   :lNoVScroll   := TRUE                // отключить показ горизонтального скролинга таблицы
   :hBrush       := CreateSolidBrush( 242, 245, 204 )   // цвет фона под таблицей

   // prepare for showing of Double cursor
   AEval( :aColumns, {| oCol | oCol:lFixLite := oCol:lEdit := TRUE, ;
                               oCol:lOnGotFocusSelect := .T.,       ;
                               oCol:lEmptyValToChar   := .T. } )
          // oCol:lOnGotFocusSelect := .T. - включат засинение данных при получении фокуса 
          //   GetBox-ом и сбрасывает, очищает поле при нажатии первого символа 
          // oCol:lEmptyValToChar := .T. - при .T. переводит empty(...) значение поля в ""

   :nHeightCell += 10        // к высоте ячеек таблицы добавим
   :nHeightHead += 5         // к высоте шапки таблицы добавим

   // GetBox встраиваем в ячейку, задаем отступы
   :aEditCellAdjust[1] += 4  // cell_Y + :aEditCellAdjust[1]
   :aEditCellAdjust[2] += 2  // cell_X + :aEditCellAdjust[2]
   :aEditCellAdjust[3] -= 5  // cell_W + :aEditCellAdjust[3]
   :aEditCellAdjust[4] -= 8  // cell_H + :aEditCellAdjust[4]

   :SetColor( { 1 }, { RGB( 0, 12, 120 )    } )
   :SetColor( { 2 }, { RGB( 242, 245, 204 ) } )
   :SetColor( { 3 }, { CLR_RED              } )
   :SetColor( { 4 }, { RGB( 231,178, 30 )   } )
   :SetColor( { 5 }, { RGB( 0, 0, 0 )       } )

   :SetColor( { 6 }, { { | a, b, oBr | a:=nil, IF( oBr:nCell == b, { RGB( 66, 255, 236 ), RGB( 111, 183, 155 ) }, ;
                          { CLR_HRED, CLR_HCYAN } ) } } )  // cursor backcolor

   :SetColor( { 9  }, { CLR_RED              } )
   :SetColor( { 10 }, { RGB( 231,178, 30 )   } )
   :SetColor( { 11 }, { CLR_YELLOW           } ) 
   :SetColor( { 12 }, { CLR_BLACK            } ) 

   :lFooting     := .T.  // использовать подвал таблицы
   :lDrawFooters := .T.  // рисовать подвал таблицы
   :nHeightFoot  := 6    // высота строки подвала таблицы

   :nFreeze      := 1     // Заморозить столбец
   :lLockFreeze  := .T.   // Избегать прорисовки курсора на замороженных столбцах

   // Двойной клик мышки на МАРКЕРЕ  
   :bLDblClick  := {|up1,up2,nfl,obr| up1:=up2:=nfl:=Nil, ;
                                 obr:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   // ------------------------------------------------------
   // 1) Обработка клавиш в таблице. Сначала работает 
   // :UserKeys( nKey, bKey, lCtrl, lShift ) 
   // и если блок при выполнении вернет: 
   // .T. - продолжить обработку клавиши в тсб
   // .F. - в блоке все обработали и продолжать обработку клавиши в тсб не надо 
   // ------- пример ---------------------------------------
   // :UserKeys( VK_RETURN, {|ob| Table_Enter_Card(ob), .F. } )
   // :UserKeys( VK_F5, {|ob| Table_Print(ob), .F. } )

   // Если предыдущий БЛОК-КОДА вернёт .T. то будет работать дальше
   // 2) :bUserKeys := {|nKy,nFl,oBr| MyKeyUserEdit(nKy, nFl, oBr) }
   // Т.е. клавиши в таблице можно обработать ДВА раза
   // ------------------------------------------------------

   // назначить свою обработку нажатий клавиш
   :bUserKeys   := {|nKy,nFl,oBr| MyKeyUserEdit(nKy, nFl, oBr) } 

   :ResetVScroll( .T. )       // показывать вертикальный скролинг таблицы
   :oHScroll:SetRange(0,0)
   :AdjColumns()              // растянуть колонки до заполнения пустоты в бровсе справа

   END TBROWSE  ON END ( oBrw:SetNoHoles(), ;  // убрать дырку внизу таблицы
                         oBrw:GoPos( 5,3 ) )   // МАРКЕР на 5 строку и 3 колонку

RETURN oBrw

//////////////////////////////////////////////////////////////////////////////////
// Функция обработки нажатия клавиш в таблице 
// Функция должна возвращать: .T. или .F.
// .T. - продолжить обработку клавиши в тсб
// .F. - в блоке все обработали и продолжать обработку клавиши в тсб не надо 
STATIC FUNCTION MyKeyUserEdit( nKey, nFlg, oBrw )
   LOCAL lRet, cForm := oBrw:cParentWnd
   Default nFlg := Nil, oBrw := Nil

   DO CASE
      CASE nKey == VK_DOWN .OR. nKey == VK_UP       // 38 + 40 
         lRet := .T.
      CASE nKey == VK_PRIOR .OR. nKey == VK_NEXT    // PgUp + PgDn / 33 + 34  
         lRet := .T.
      CASE nKey == VK_SPACE
      CASE nKey == VK_F5
         //Table_Print(oBrw)
      CASE nKey == VK_RETURN  
         Table_Enter_Card(oBrw)
         lRet := .F. 
      CASE nKey == 16 .OR. nKey == 17  // Shift+Alt  Shift+Ctrl  "RUS/LAT"
         lRet := .F. 
      OTHERWISE
         //? ProcName(0), " nKey == ",nKey
         lRet := .T. 
   ENDCASE
   
RETURN lRet

/////////////////////////////////////////////////////////////////////
FUNCTION Table_Enter_Card(oBrw)
   LOCAL cAls := oBrw:cAlias
   LOCAL nRecno, aDim

   // здесь можно сделать загрузку полей карточки из ини-файла
   aDim := CardGetStruct()       // поля ВСЕЙ карточки в функции

   nRecno := (cAls)->(RecNo())   // номер записи который редактируем
                                 // НУЖЕН, т.к. есть таймер-перечитать таблицу

   Show_Card(oBrw, aDim, .T. , nRecno)  // показ карточки в режиме редактирования  

   oBrw:SetFocus() 
   DO EVENTS 

   RETURN NIL

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION CardGetStruct()
   LOCAL aDim, aFld, aType, aSpr

   // сделано через поля базы, потому что в таблице 4 столбца, 
   // а в карточке может быть много полей БД (резерв на будущее)
   // можно сделать загрузку структур через ини-файлы
   aDim := {}              ; aFld := {}          ; aType := {}          ; aSpr := {}
   AADD( aDim, "Date"    ) ; AADD( aFld, "F1"  ) ; AADD( aType, "D"   ) ; AADD( aSpr, {}             )
   AADD( aDim, "Text"    ) ; AADD( aFld, "F2"  ) ; AADD( aType, "C"   ) ; AADD( aSpr, {}             )
   AADD( aDim, "Number"  ) ; AADD( aFld, "F3"  ) ; AADD( aType, "N"   ) ; AADD( aSpr, {}             )
   AADD( aDim, "CodeList") ; AADD( aFld, "CODE") ; AADD( aType, "S"   ) ; AADD( aSpr, SpravCode()    )
   AADD( aDim, "Handbook") ; AADD( aFld, "F4"  ) ; AADD( aType, "S"   ) ; AADD( aSpr, SpravCode2()   )
   AADD( aDim, "Recno"   ) ; AADD( aFld, ""    ) ; AADD( aType, "FUN" ) ; AADD( aSpr, 'MyRetRecno()' )
   // aType - тип обработки поля, S-справочник, FUN-функция

RETURN {aDim,aFld,aType,aSpr}

/////////////////////////////////////////////////////////////////////
// пример простой функции
FUNCTION MyRetRecno()
   RETURN (ALIAS())->(RECNO()) 

/////////////////////////////////////////////////////////////////////
// пример простого справочника. Можно сделат загрузку из dbf-файла
FUNCTION SpravCode()
   LOCAL nI, aDim := {}

   FOR nI := 1 TO 5
      AADD( aDim, { nI, PadR( " ( Code = " + HB_NtoS(nI) + " )", 20 ) } )
   NEXT

   RETURN aDim

/////////////////////////////////////////////////////////////////////
// пример простого справочника. Можно сделат загрузку из dbf-файла
FUNCTION SpravCode2()
   LOCAL aDim := {}

   AADD( aDim, { 1, PadR( " one"   , 10 ) } )
   AADD( aDim, { 2, PadR( " two"   , 10 ) } )
   AADD( aDim, { 3, PadR( " three" , 10 ) } )
   AADD( aDim, { 4, PadR( " four"  , 10 ) } )
   AADD( aDim, { 5, PadR( " five"  , 10 ) } )
   AADD( aDim, { 6, PadR( " six"   , 10 ) } )

   RETURN aDim

/////////////////////////////////////////////////////////////////////
// получить значение из справочника
FUNCTION SpravGetCode( nVal, aDim )
   LOCAL nI, lRet := .F., cRet := " no handbook array"

   IF nVal == NIL
      nVal := -0.1
   ENDIF

   IF LEN(aDim) > 0
      FOR nI := 1 TO LEN(aDim)
         IF nVal > 0 .AND. nVal < LEN(aDim) + 1
            IF nVal == aDim[nI,1]
               cRet := aDim[nI,2]
               lRet := .T.
               EXIT
            ENDIF
         ENDIF
      NEXT
      IF ! lRet
         cRet := " no data (" + HB_NtoS(nVal)+ ")"
      ENDIF
   ENDIF

   RETURN cRet

/////////////////////////////////////////////////////////////////////
FUNCTION Show_Card(oBrw,aDim,lEditCard,nRecnoEdit)  
   LOCAL cForm := oBrw:cParentWnd , cAls := oBrw:cAlias
   LOCAL aCargo, actpos := {0,0,0,0}
   LOCAL nCol, nRow, nWidth, nHeight, cMsgIndx
   LOCAL cMsg, aBackColor := { 242, 245, 204 }
   LOCAL cFont := "Tahoma", nFontSize := 14
   LOCAL nI, nX, nY, nWLbl, nHF, nWGbx, cN, cN2
   LOCAL aCardName, aCardFld, aCardType, aCardFSpr, cRun
   LOCAL nG, nK, cVal, xVal, aObjGBox, nColButt, cNButt
   
   aBackColor := aStaticTableColor   // взять новый цвет  

   GetWindowRect( GetFormHandle( cForm ), actpos )  // координаты основного окна
   nCol    := actpos[1]              // Form_0.Col
   nRow    := actpos[2]              // Form_0.Row   
   nWidth  := actpos[3] - actpos[1]  // Form_0.Width 
   nHeight := actpos[4] - actpos[2]  // Form_0.Height

   SELECT(cAls)
   cMsgIndx := "Index condition: [ " + OrdFor() + " ]"

   aCardName := aDim[1]   // наименование полей
   aCardFld  := aDim[2]   // поля базы
   aCardType := aDim[3]   // тип обработки поля
   aCardFSpr := aDim[4]   // сами функции/справочники 2х-массив

   lStaticEditCard := .F.    // не было редактирование полей карточки

   nK := 0  // 100 - для теста, сместить карточку вправо 
   DEFINE WINDOW Form_Card               ;
      At nRow, nCol + 70 + nK            ;
      WIDTH nWidth - 70 HEIGHT nHeight   ;
      TITLE "Card test box"              ;
      MODAL                              ;
      BACKCOLOR aBackColor               ;
      NOSIZE                             ;
      FONT cFont SIZE nFontSize          ;
      ON INIT {|| This.Topmost := .F. , CardRecnoInfo(oBrw), DoEvents(), MyFocus() }  

      nWidth  := This.ClientWidth 
      nHeight := This.ClientHeight
      cForm   := ThisWindow.Name

      SetProperty(cForm, 'Cargo', aDim)    // передаём на объект все поля карточки
      
      @ 0, 0 LABEL buff WIDTH nWidth HEIGHT 40 VALUE cMsgIndx ;
        BACKCOLOR SILVER CENTERALIGN VCENTERALIGN 

      @ 60, 20 LABEL Lbl_Rec WIDTH nWidth-170 HEIGHT nFontSize*2*3 VALUE "" ;
        FONTCOLOR BLACK  TRANSPARENT

      @ 40 + 20, nWidth-130-20 BUTTONEX Button_Help WIDTH 130 HEIGHT 50 ;
         CAPTION "(?) Help" BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP    ;
         FONTCOLOR WHITE BACKCOLOR { 0,176,240 }                        ;
         ACTION {|| MsgAbout(), Form_Card.buff.Setfocus }

      nG    := 5   // расстояние между строками карточки
      nY    := Form_Card.Lbl_Rec.Row + Form_Card.Lbl_Rec.Height + 20
      nX    := 30
      nWLbl := 90
      nHF   := nFontSize*2
      nWGbx := 60

      aObjGBox := {}  // массив перепоказа значений карточки
      FOR nI := 1 TO Len(aCardName)

         cN := 'Lbl_Card_' + StrZero(nI, 2)
         @ nY, nX LABEL &cN WIDTH nWLbl HEIGHT nHF VALUE aCardName[nI] + ":" ;
           FONTCOLOR BLUE TRANSPARENT RIGHTALIGN

         cN2   := 'GetBox_Card_' + StrZero(nI, 2)
         AADD( aObjGBox, cN2 )  // массив перепоказа значений карточки

         IF aCardType[nI]  == "S" // тип поля справочник
            aDim  := aCardFSpr[nI] 
            cVal  := aDim[1][2] 
            nWGbx := GetTxtWidth( REPL("A", LEN(cVal)), nFontSize, cFont ) + 20
            nWGbx := IIF( nWGbx > nWidth - (nX+nWLbl+10)-nX, nWidth - (nX+nWLbl+10)-nX, nWGbx )
            xVal  := FIELDGET( FIELDNUM( aCardFld[nI] ) ) // значение поля в бд
            cVal  := SpravGetCode( xVal, aCardFSpr[nI] )   // значение из справочника

            @ nY-2, nX+nWLbl+10 GETBOX &cN2 WIDTH nWGbx HEIGHT nHF ;
               VALUE cVal READONLY //ON CHANGE {|| "не нужно сюда" } 

            aCargo := { aCardType[nI], aCardFld[nI], xVal, cVal }
            //SetProperty(cForm, cN2, 'Cargo', aCargo )   // передаём на объект
            This.&(cN2).Cargo := aCargo

            nColButt := nX+nWLbl+10 + nWGbx + 10
            cNButt   := "Button_" + cN2 
            @ nY-2, nColButt BUTTONEX &cNButt WIDTH nHF HEIGHT nHF   ;
               CAPTION "?" FLAT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
               BOLD FONTCOLOR BLACK BACKCOLOR SILVER                 ;
               ACTION {|| lStaticEditCard := .T.  /* было изменение */      ,;  
                          SetProperty(cForm, 'Button_Down', 'Enabled', .F.) ,; 
                          SetProperty(cForm, 'Button_Up'  , 'Enabled', .F.) ,; 
                          EditTypeSprav( This.Cargo ) ,  MyFocus() }      
            // номер кнопки в массиве - это для чтения из aDim[]
            //SetProperty(cForm, cNButt, 'Cargo', { nI, cN2, cNButt } ) 
            This.&(cNButt).Cargo := { nI, cN2, cNButt }

            IF !lEditCard  // если запись блокирована (нет редактирования)
               SetProperty(cForm, cNButt, 'Enabled', .F.)  // блокировать редактирование
            ENDIF

         ELSEIF aCardType[nI]  == "FUN" // тип поля функция
            cRun  := aCardFSpr[nI]
            xVal  := &cRun
            cVal  := cValToChar(xVal)
            nWGbx := GetTxtWidth( REPL("A", LEN(cVal)), nFontSize, cFont ) + 20
            nWGbx := IIF( nWGbx > nWidth - (nX+nWLbl+10)-nX, nWidth - (nX+nWLbl+10)-nX, nWGbx )

            @ nY, nX+nWLbl+10 LABEL &cN2 WIDTH nWGbx HEIGHT nHF VALUE xVal BOLD TRANSPARENT 

            aCargo := { aCardType[nI], aCardFld[nI], 0, "резерв" }
            //SetProperty(cForm, cN2, 'Cargo', aCargo )   // передаём на объект
            This.&(cN2).Cargo := aCargo

         ELSE   
            // тип обработки поля N C D
            xVal  := FIELDGET( FIELDNUM( aCardFld[nI] ) )
            cVal  := cValToChar(xVal)
            nWGbx := GetTxtWidth( REPL("A", LEN(cVal)), nFontSize, cFont ) + 20
            nWGbx := IIF( nWGbx > nWidth - (nX+nWLbl+10)-nX, nWidth - (nX+nWLbl+10)-nX, nWGbx )

            @ nY-2, nX+nWLbl+10 GETBOX &cN2 WIDTH nWGbx HEIGHT nHF ;
               VALUE xVal ON CHANGE {|| lStaticEditCard := .T.  /* было изменение */      ,;
                                        SetProperty(cForm, 'Button_Down', 'Enabled', .F.) ,; 
                                        SetProperty(cForm, 'Button_Up'  , 'Enabled', .F.)    } 

            aCargo := { aCardType[nI], aCardFld[nI], 0, "резерв" }
            //SetProperty(cForm, cN2, 'Cargo', aCargo )   // передаём на объект
            This.&(cN2).Cargo := aCargo

            IF !lEditCard  // если запись блокирована (нет редактирования)
               SetProperty(cForm, cN2, 'Readonly', .T.)  // блокировать редактирование
            ENDIF

         ENDIF

         nY += nHF + nG
      NEXT

      cMsg := "Вводим значение в базу CODE = 2, далее нажимаем кнопку [Save record]"+CRLF
      cMsg += 'В условном индексе эта запись должна "исчезнуть" и из таблицы удалиться тоже !'+CRLF
      cMsg += "Enter the value in the database CODE = 2, then press the button [Save record]" + CRLF
      cMsg += 'In the conditional index, this entry should "disappear" and also be removed from the table !'

      @ nY, nX LABEL Lbl_Info WIDTH nWidth-nX*2 HEIGHT 100 VALUE cMsg ;
        SIZE 10 FONTCOLOR RED TRANSPARENT 

      cMsg := "Запись заблокирована другим пользователем. Можно только смотреть" 
      @ nHeight-50-50, 0 LABEL Lbl_RLock WIDTH nWidth HEIGHT 20 VALUE cMsg ;
        SIZE 11 FONTCOLOR LGREEN TRANSPARENT CENTERALIGN VCENTERALIGN INVISIBLE

      @ nHeight-50-20, 20 BUTTONEX Button_Down WIDTH 100 HEIGHT 50            ;
         CAPTION "Down to"+CRLF+"record" FLAT NOXPSTYLE HANDCURSOR NOTABSTOP  ;
         FONTCOLOR WHITE BACKCOLOR GRAY                                       ;
         ACTION {|| nRecnoEdit := CardDownUp(oBrw, 1, aObjGBox)  ,;
                    MyFocus() }

      @ nHeight-50-20, 20+100+20 BUTTONEX Button_Up WIDTH 100 HEIGHT 50      ;
         CAPTION "Up to"+CRLF+"record"  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP  ;
         FONTCOLOR WHITE BACKCOLOR GRAY                                      ;
         ACTION {|| nRecnoEdit := CardDownUp(oBrw, 2, aObjGBox)  ,;
                    MyFocus() }

      @ nHeight-50-20, 20+200+40+10 BUTTONEX Button_Save WIDTH 100 HEIGHT 50   ;
         CAPTION "Save"+CRLF+"record" FLAT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
         FONTCOLOR WHITE BACKCOLOR LGREEN                                      ;
         ACTION {|lWrite| lWrite := CardSave(oBrw, nRecnoEdit, aObjGBox) ,;
                    nRecnoEdit := CardDownUp(oBrw, 0, aObjGBox) ,;
                    MyFocus() }

      @ nHeight-50-20, nWidth-100-20 BUTTONEX Button_Exit WIDTH 100 HEIGHT 50 ;
         CAPTION "Exit" BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP              ;
         FONTCOLOR WHITE BACKCOLOR MAROON                                     ;
         ACTION {|| CardModify(oBrw, nRecnoEdit, aObjGBox) ,;
                    RecnoRefresh(oBrw, .t.), ThisWindow.Release }  

      // RecnoRefresh(oBrw, .t.)  // перечитать таблицу всегда, т.к. другая программа
                                  // может удалить/добавить запись для этой таблицы

      IF !lEditCard // если запись блокирована (нет редактирования)
         CardRecnoGetSay(.F., aObjGBox)  // поменять атрибуты объектов на форме
      ENDIF

      ON KEY ESCAPE OF Form_Card ACTION ThisWindow.Release

      ON KEY F3     OF Form_Card ACTION MsgDebug( GetProperty(cForm, 'Cargo') )

   END WINDOW

   //CENTER WINDOW Form_Card
   ACTIVATE WINDOW Form_Card ON INIT {|| This.Topmost := .T. }  

RETURN NIL 

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION MyFocus()
    Form_Card.buff.Setfocus
RETURN NIL

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION CardRecnoInfo(oBrw)
   LOCAL cMsg, cAls := oBrw:cAlias

   // Запись на которой стоит МАРКЕР 
   cMsg := HB_NtoS( (cAls)->(OrdKeyNo()) ) + " / " + HB_NToS( (cAls)->(OrdKeyCount()) ) + CRLF
   cMsg += "Recno() = " + HB_NtoS( (cAls)->(RecNo()) ) + CRLF
   cMsg += "oBrw:nRowPos = " + HB_NtoS( oBrw:nRowPos ) + CRLF
   cMsg += "oBrw:nAt = " + HB_NtoS( oBrw:nAt ) + CRLF

   Form_Card.Lbl_Rec.Value := cMsg

RETURN NIL

/////////////////////////////////////////////////////////////////////
// считать новую запись и показать объекты
// поменять атрибуты объектов на форме
STATIC FUNCTION CardRecnoGetSay(lVal, aObjGBox)
   LOCAL xVal, cVal, nI, cObj, aCargo, cForm := ThisWindow.Name
   LOCAL aDimCard, aCardName, aCardType, aCardFld, aCardFSpr, cRun

   Form_Card.Lbl_RLock.Visible   := !lVal  // показать надпись
   Form_Card.Lbl_Info.Visible    := lVal
   Form_Card.Button_Save.Visible := lVal

   aDimCard := GetProperty(cForm, 'Cargo') // считать весь массив карточки

   aCardName := aDimCard[1]   // наименование полей
   aCardFld  := aDimCard[2]   // поля базы
   aCardType := aDimCard[3]   // тип обработки поля
   aCardFSpr := aDimCard[4]   // сами функции/справочники 2х-массив

   FOR nI := 1 TO Len(aObjGBox)

      cObj   := aObjGBox[nI]
      IF aCardType[nI]  == "S" // тип поля справочник
         SetProperty(cForm, cObj, 'Enabled' , lVal)
         // блокировать редактирование полей ВСЕГДА, т.к. это просто показ значения
         SetProperty(cForm, cObj, 'Readonly', .T. )  
         xVal := FIELDGET( FIELDNUM( aCardFld[nI] ) )
         cVal := SpravGetCode( xVal, aCardFSpr[nI] )        // значение из справочника
         SetProperty(cForm, cObj, "Value" , cVal)           // показ на карточке
         aCargo    := GetProperty(cForm, cObj, 'Cargo' )    // считали этот объект
         aCargo[3] := xVal
         aCargo[4] := cVal
         SetProperty(cForm, cObj, 'Cargo', aCargo )         // изменили этот объект
      ELSEIF aCardType[nI] == "FUN" // тип поля функция
         cRun  := aCardFSpr[nI]
         xVal  := &cRun
         cVal  := cValToChar(xVal)
         SetProperty(cForm, cObj, "Value" , cVal)  // показ на карточке
      ELSE
         // блокировать/разблокировать редактирование полей
         SetProperty(cForm, cObj, 'Readonly', !lVal)  
         xVal := FIELDGET( FIELDNUM( aCardFld[nI] ) )
         SetProperty(cForm, cObj, "Value" , xVal)  // показ на карточке
      ENDIF

   NEXT

   SetProperty( cForm, 'Button_Down', 'Enabled', .T. )  
   SetProperty( cForm, 'Button_Up'  , 'Enabled', .T. )     

RETURN NIL

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION CardDownUp(oBrw, nArrow, aObjGBox)
   LOCAL cAls := oBrw:cAlias, cForm := ThisWindow.Name
   LOCAL nRecno

   IF nArrow == 1
      oBrw:GoDown()
      IF oBrw:lHitBottom
         TONE(600)
      ENDIF
   ELSEIF nArrow == 2
      oBrw:GoUp()
      IF oBrw:lHitTop
         TONE(600)
      ENDIF
   ELSEIF nArrow == 0
      // перечитать значение полей карточки
   ENDIF

   RecnoRefresh(oBrw, .f.)  // перечитать таблицу всегда, т.к. другая программа может 
                            // удалить/добавить запись для этой таблицы (условия индекса) 

   nRecno  := (cAls)->(RecNo())

   CardRecnoGetSay(.T., aObjGBox)  // считать новую запись и показать объекты

   CardRecnoInfo(oBrw)

   lStaticEditCard := .F.  // перечитали новую запись и поставили для неё флаг 

RETURN nRecno

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CardSave(oBrw, nRecnoEdit, aObjGBox)
   LOCAL cForm := ThisWindow.Name
   LOCAL nRecno, nKolvo1, nKolvo2, nRowPos, aCargo
   LOCAL cAls := oBrw:cAlias, nRecGo, nCell, lRecnoWrite
   LOCAL cCardType, cCardFld, nI, cObj, xVal, nRow := 0

   nRowPos := oBrw:nRowPos
   nCell   := oBrw:nCell 
   nRecno  := (cAls)->(RecNo())
   (cAls)->( dbSkip(1) )
   If (cAls)->( EOF() )
      (cAls)->( dbGoto(nRecno) )
      (cAls)->( dbSkip(-1) )
      nRow := -1
   EndIf
   nRecGo   := (cAls)->(RecNo())         // куда идем (следующая запись в базе)
   (cAls)->( dbGoto(nRecno) )            // востаннавливаем номер позиции записи в базе
   nKolvo1  := ORDKEYCOUNT()             // всего позиций в индексе до сохранения полей

   // Если запись пропала из таблицы, значит nRecnoEdit не равен nRecno
   // и нужно записывать запись по номеру nRecnoEdit
   IF nRecnoEdit # nRecno
      dbGoto(nRecnoEdit)
   ENDIF

   SELECT(cAls)
   lRecnoWrite := .F.
   // сетевой захват записи 
   IF (cAls)->(RLock())    

      FOR nI := 1 TO Len(aObjGBox)

          cObj   := aObjGBox[nI]
          aCargo := GetProperty(cForm, cObj, 'Cargo')   // считаю данные по объекту GetBox
          //  тип обработки, имя поля, значение поля в бд, значение из справочника
          cCardType := aCargo[1]
          cCardFld  := aCargo[2]
          IF cCardType  == "S" // тип поля справочник
             xVal := aCargo[3]
             FIELDPUT( FIELDNUM( cCardFld ) , xVal )  // запись в поле
          ELSEIF cCardType == "FUN" // тип поля функция
             // пропуск записи
          ELSE
             xVal := GetProperty( cForm, cObj, "Value" )
             FIELDPUT( FIELDNUM( cCardFld ) , xVal )  // запись в поле
          ENDIF

      NEXT

      (cAls)->(DBUnlock())
      lRecnoWrite := .T.
   ELSE
      MsgStop("Record " + HB_NtoS(RECNO()) + " locked!")
   ENDIF
   (cAls)->(DbCommit())  // скидываем на диск
   DO EVENTS

   SetProperty(cForm, 'Button_Down', 'Enabled', .T.)  
   SetProperty(cForm, 'Button_Up'  , 'Enabled', .T.)  

   IF lRecnoWrite
      lStaticEditCard := .F.   // обнуляем для другой записи 
   ENDIF

   nKolvo2 := ORDKEYCOUNT()    // всего записей индекса после сохранения полей 

   WITH OBJECT oBrw                        
   IF nKolvo1 # nKolvo2
      :Reset()       
      If :nRowCount() >= :nLen
         :GoPos(nRowPos + nRow, nCell)
      ElseIf nRowPos == :nRowCount()
         :GoBottom()
         :nCell := nCell
      Else
         :GotoRec(nRecGo, nRowPos) 
         :nCell := nCell
         :UpStable()
      EndIf
   ELSE
      :Refresh()        
   ENDIF

   :SetFocus() 
   END WITH
   DO EVENTS 

RETURN lRecnoWrite

/////////////////////////////////////////////////////////////////////
// Обработка записи по кнопки - справочник S показ/запись
STATIC FUNCTION EditTypeSprav(aThisCargo) 
   LOCAL cForm := ThisWindow.Name
   LOCAL nI, nKey, cN2, nRet, aDim, cVal, aClr, aButt, cMsg, aCargo
   LOCAL aDimCard, aCardName, aCardType, aCardFld, aCardFSpr

   aDimCard := GetProperty(cForm, 'Cargo') // считать весь массив карточки

   aCardName := aDimCard[1]   // наименование полей
   aCardFld  := aDimCard[2]   // поля базы
   aCardType := aDimCard[3]   // тип обработки поля
   aCardFSpr := aDimCard[4]   // сами функции/справочники 2х-массив

   SetProperty( cForm, 'Button_Exit', 'Enabled', .F. )  
   SetProperty( cForm, 'Button_Save', 'Enabled', .F. )     

   nKey   := aThisCargo[1]       // номер нажатой кнопки 
   cN2    := aThisCargo[2]       // имя объекта GetBox
   aDim   := aCardFSpr[nKey]     // весь справочник по этой кнопке

   // простой справочник на 3 значения, здесь нужно делать свой справочник
   aClr  := { YELLOW, RED, GREEN }
   aButt := {}
   FOR nI := 1 TO 3
      AADD(aButt, ALLTRIM(aDim[nI,2]) )
   NEXT
   cMsg  := "Select the value you need from the handbook ?" + CRLF
   cMsg  += "Выберите нужное значение из справочника ?"

   nRet  := HMG_Alert( cMsg, aButt, "Attention!", NIL, NIL, NIL, aClr, NIL )  

   IF nRet == 0  // допускаем, что можно отказаться от выбора из справочника
      lStaticEditCard := .F.  // пометили, что НЕ изменили это поле карточки
   ELSE

      aDim   := aCardFSpr[nKey]                       // весь справочник 
      cVal   := SpravGetCode( nRet, aDim )            // наименование из справочника
      SetProperty(cForm, cN2, 'Value', cVal)          // изменили это поле на карточе

      aCargo    := GetProperty(cForm, cN2, 'Cargo')   // считаем объект GetBox
      aCargo[3] := nRet
      aCargo[4] := cVal
      //           тип обработки, поле-бд    , значение поля в бд
      //aCargo := { aCardType[nI], aCardFld[nI], nRet, cVal }
      SetProperty(cForm, cN2, 'Cargo', aCargo)        // изменили этот объект GetBox

   ENDIF

   SetProperty( cForm, 'Button_Exit', 'Enabled', .T. )  
   SetProperty( cForm, 'Button_Save', 'Enabled', .T. )     

RETURN NIL

/////////////////////////////////////////////////////////////////////
// Проверка модификации карточки и запись значений в базу 
STATIC FUNCTION CardModify(oBrw, nRecnoEdit, aObjGBox)
   LOCAL cMsg, lRecnoSave

   IF lStaticEditCard  // была модификация поля карточки

     cMsg := "Запись карточки была изменена !" + CRLF
     cMsg += "Вы хотите записать изменённый данные в карточке ?" + CRLF
     cMsg += CRLF + "Record card has been changed!" + CRLF
     cMsg += "Do you want to write the changed data in the card ?"

     IF MsgYesNo( cMsg, "Save record" /*"Сохранить запись"*/, .T. )
        lRecnoSave := CardSave(oBrw, nRecnoEdit, aObjGBox)
        IF lRecnoSave
           lStaticEditCard := .F.   // обнуляем для другой записи
        ENDIF
     ENDIF

   ENDIF

RETURN NIL

/////////////////////////////////////////////////////////////////////
// Создать записи для показа в таблице 
STATIC FUNCTION RecnoCreateCondition(oBrw,nRecno) 
   LOCAL nOrder, cVal, lWrite, nI := 0
   LOCAL cForm  := oBrw:cParentWnd
   LOCAL cAlias := oBrw:cAlias

   IF GetControlIndex("Timer_1", cForm ) > 0
      SetProperty( cForm, "Timer_1" , "Enabled" , .F. )                 // отключить таймер
   ENDIF
   cVal := GetProperty( cForm, "StatusBar" , "Item" , 2 )               // считать что есть
   SetProperty( cForm, "StatusBar" , "Item" , 2, "Re-read database!" )  // показ обновления

   oBrw:Enabled( .F. )  // блокировка таблицы с закраской
   InkeyGui(600)

   lWrite := .T.
   SELECT(cAlias)
   nOrder := INDEXORD()             
   DbSetOrder(0)
   GOTO TOP
   DO WHILE ! EOF()
      IF ! DELETED()
         IF (cAlias)->(RLock())    
            (cAlias)->CODE := 0  // обнуляем
            (cAlias)->(DBUnlock())
         ENDIF
         IF lWrite
            IF RECNO() % 5 == 0
               // сетевой захват записи
               IF (cAlias)->(RLock())    
                  (cAlias)->CODE := 1  // помечаем для показа по условному индексу
                  nI ++
                  (cAlias)->(DBUnlock())
               ELSE
                  MsgStop("Record " + HB_NtoS(RECNO()) + " locked!")
               ENDIF
               IF nRecno == nI
                  lWrite := .F.  // прекращаем запись
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      SKIP
   ENDDO

   (cAlias)->(DbCommit())  // скидываем на диск

   DbSetOrder(nOrder)

   oBrw:Enabled( .T. )         // разблокировка таблицы с закраской

   oBrw:Reset() 

   // обязательно перечитать состояние вертикального скролинга
   oBrw:ResetVScroll( .T. ) 
   oBrw:oHScroll:SetRange( 0, 0 ) 

   SysRefresh() 
   oBrw:nLen := ( oBrw:cAlias )->( Eval( oBrw:bLogicLen ) ) 
   oBrw:Upstable() 
   oBrw:Refresh(.T., .T.) 
   oBrw:SetFocus() 
   DO EVENTS

   InkeyGui(500) // для теста

   SetProperty( cForm, "StatusBar" , "Item" , 2, cVal )  // возврат сообщения

   IF GetControlIndex("Timer_1", cForm ) > 0
      SetProperty( cForm, "Timer_1" , "Enabled" , .F. )  // включить таймер
      nStaticTime := SECONDS()                           // обновить время
   ENDIF

RETURN Nil

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION RecnoRefresh(oBrw, ltimer)
   LOCAL cVal, cForm := oBrw:cParentWnd
   Default ltimer := .f.

   // если нет редактирования записи юзером то перечитаем базу
   // if there is no editing of record by the user that we will re-read the database
   If empty( oBrw:aColumns[ oBrw:nCell ]:oEdit )

      Index2Reindex(oBrw:cAlias,2)  // переиндексировать индекс

      cVal := GetProperty( cForm, "StatusBar" , "Item" , 2 )               // считать что есть
      SetProperty( cForm, "StatusBar" , "Item" , 2, "Re-read database!" )  // показ обновления
      // это перепоказ таблицу
      SysRefresh() 
      oBrw:nLen := ( oBrw:cAlias )->( Eval( oBrw:bLogicLen ) ) 
      oBrw:Upstable() 
      oBrw:Refresh(.T., .T.) 
      oBrw:SetFocus() 
      DO EVENTS
      SetProperty( cForm, "StatusBar" , "Item" , 2, cVal )  // возврат сообщения
   EndIf

   If ltimer
      nStaticTime := SECONDS()  // обновить время
   EndIf

RETURN Nil

/////////////////////////////////////////////////////////////////////
FUNCTION Timer1Show()  // показ таймера на форме 
    LOCAL cTime

    cTime := " " + SECTOTIME( GetProperty( ThisWindow.Name, "Timer_1", "Value" ) / 1000 - (SECONDS() - nStaticTime) )
    SetProperty ( ThisWindow.Name, "StatusBar" , "Item" , 3, cTime )

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION UseOpenBase()
   LOCAL aStr   := {} 
   LOCAL cDbf   := GetStartUpFolder() + "\test6" 
   LOCAL cIndx  := cDbf 
   LOCAL lDbfNo 
   LOCAL aAlias := {} 
   LOCAL i, j, nn  := 1 
  
   IF ( lDbfNo := ! File( cDbf+'.dbf' ) ) 
      AAdd( aStr, { 'F1'  , 'D',  8, 0 } ) 
      AAdd( aStr, { 'F2'  , 'C', 60, 0 } ) 
      AAdd( aStr, { 'F3'  , 'N', 10, 2 } ) 
      AAdd( aStr, { 'CODE', 'N',  4, 0 } ) 
      AAdd( aStr, { 'F4'  , 'N',  2, 0 } ) 
      dbCreate( cDbf, aStr ) 
   ENDIF 
  
   IF lDbfNo .OR. !File( cIndx+'.cdx' )
      USE ( cDbf ) ALIAS TEST EXCLUSIVE NEW 
  
      i := 0
      j := 0
      WHILE TEST->( RecCount() ) < 200 
         TEST->( dbAppend() ) 
         TEST->F1   := Date() + nn++
         TEST->F2   := "Recno = " + HB_NtoS( RECNO() )
         TEST->F3   := RECNO() 
         TEST->CODE := IIF( i == 1, 0, i + nn ) 
         TEST->F4   := j  
         IF ( i % 3 ) == 0
            DbDelete()
         ENDIF
         i++
         j++
         j := IIF( j > 3, 0, j ) 
      END 
  
      INDEX ON RECNO() TAG ALL TO (cIndx)           
      USE 
   ENDIF 

   SET AUTOPEN ON
  
   USE ( cDbf ) ALIAS TEST SHARED NEW 
   OrdSetFocus('ALL') 
   Dbsetorder(1)
   GO TOP 

   SET AUTOPEN OFF
  
   AADD( aAlias, ALIAS() )

RETURN aAlias

/////////////////////////////////////////////////////////////////////
FUNCTION Index2Create(aHWindows)
   LOCAL cFilter, cIndx, cMaska

   cMaska := HB_NtoS( LEN(aHWindows) + 1 ) + "-User"
   // условный индекс нужно делать для каждого юзера отдельно
   // чтобы не было конфликтов при использовании этого файла
   cIndx := GetStartUpFolder() + "\test6." + cMaska + '.cdx'
   cFilter := "CODE==1 .AND. !Deleted()"
   DELETEFILE(cIndx) // обязательно
   SELECT TEST
   INDEX ON RECNO() TAG CODE1 TO (cIndx) FOR &cFilter ADDITIVE 
   OrdSetFocus('CODE1') 
   Dbsetorder(2)
   GO TOP 

RETURN NIL

/////////////////////////////////////////////////////////////////////
// переиндексировать индекс 
STATIC FUNCTION Index2Reindex( cAls, nOrder )
   LOCAL aMemIndex, cTag, nTekOrder, cFilter, cOrdKey, cFileIndex
   LOCAL nKolvo, nRecno

   SELECT(cAls)
   nTekOrder  := INDEXORD()             
   cFileIndex := DBORDERINFO(DBOI_FULLPATH)
   cFilter    := OrdFor() 
   cOrdKey    := OrdKey()
   cTag       := OrdName()                 
   nRecno     := OrdKeyNo()

   IF nOrder == 2     // перестроить индекс 

      // Запомнить все открытые индексы
      aMemIndex := Index2OpenSave()
      DBCLEARINDEX()

      Index2OpenRestore( aMemIndex, -1 ) // Восстановить индексные файлов базы, кроме последнего

      INDEX ON &cOrdKey TAG &cTag TO (cFileIndex) FOR &cFilter ADDITIVE 
      OrdSetFocus(cTag) 
      DBSetOrder( nTekOrder )             // Восстановить ордер 
      nKolvo := OrdKeyCount()             // всего записей индекса  
      nRecno := IIF( nRecno > nKolvo, nKolvo, nRecno )
      nRecno := IIF( nRecno == 0, nKolvo, nRecno )
      ORDKEYGOTO(nRecno)                  // восстановить текущую запись

   ENDIF

RETURN NIL

/////////////////////////////////////////////////////////////////
// Возрат массива индексных файлов открытой базы 
FUNCTION Index2OpenSave()
LOCAL aDim := {}, nI, cPath

   FOR nI := 1 TO 100
      IF LEN(ORDNAME(nI)) == 0
          EXIT
      ELSE
         DBSetOrder(nI)
         cPath := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) ) 
         IF cPath == ""
            EXIT
         ELSE
            AADD( aDim, { ALIAS(), cPath } )
         ENDIF
      ENDIF
   NEXT

RETURN aDim

/////////////////////////////////////////////////////////////////
// Подключить к открытой базе ранее открытые индексные файлы 
FUNCTION Index2OpenRestore(aDim,nFile)
LOCAL nI, cBase, nSel, cPathIndex 
DEFAULT aDim := {}, nFile := 0

IF LEN(aDim) == 0
   MsgDebug("Нет открытых индексов для базы !"+SPACE(40)+"Текущий алиас: "+ALIAS()+" !")
ELSE
   cBase  := aDim[1,1]
   nSel   := SELECT(cBase)
   IF nSel > 0
      SELECT(cBase)
      FOR nI := 1 TO LEN(aDim) + nFile   // Восстановить открытые индексы
         cPathIndex := aDim[nI,2]        // -1 можно подключать не все 
         ORDLISTADD( cPathIndex  )
         DBSetOrder(nI)
      NEXT
   ELSE
     MsgDebug("Нет открытой базы: "+cBase+" ! Индексы не восстановил...")
   ENDIF
ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
#define GW_HWNDFIRST	0
#define GW_HWNDLAST	1
#define GW_HWNDNEXT	2
#define GW_HWNDPREV	3
#define GW_OWNER	4
#define GW_CHILD	5

// Check to run the second/third copy of the program
FUNCTION OnlyOneInstance(cAppTitle)
   LOCAL cTitle, hWnd, aHWindows := {} 
 
   hWnd := GetWindow( GetForegroundWindow(), GW_HWNDFIRST )
   WHILE hWnd != 0  // Loop through all the windows
      cTitle := GetWindowText( hWnd )
      IF GetWindow( hWnd, GW_OWNER ) = 0 .AND. cAppTitle $ cTitle 
         AADD( aHWindows, { hWnd, cTitle, IsWindowVisible( hWnd ) } )
      ENDIF
      hWnd := GetWindow( hWnd, GW_HWNDNEXT )  // Get the next window
      DO EVENTS
   ENDDO

RETURN aHWindows

///////////////////////////////////////////////////////////////////////////
FUNCTION ChangeWinBrw(oBrw,aHWindows)
   LOCAL nH := Form_0.Height , nW := Form_0.Width
   LOCAL nK, nI, hWnd
 
   IF LEN(aHWindows) == 0
      // skipping
      aStaticTableColor := { 242, 245, 204 } // цвет для карточки

   ELSEIF LEN(aHWindows) == 1

      Form_0.Row := 0 
      Form_0.Col := 0
      aStaticTableColor := { 255,178,178 }  // цвет для карточки

      oBrw:SetColor( { 2 }, { RGB( 255,178,178 ) } )
      oBrw:hBrush := CreateSolidBrush( 255,178,178 )
      RecnoRefresh(oBrw)

      FOR nI := 1 TO LEN(aHWindows)
          hWnd := aHWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT
   ELSEIF LEN(aHWindows) == 2

      Form_0.Row := 0   
      Form_0.Col := GetDesktopWidth() - nW
      aStaticTableColor := { 159,191,236 }  // цвет для карточки

      oBrw:SetColor( { 2 }, { RGB( 159,191,236 ) } )
      oBrw:hBrush := CreateSolidBrush( 159,191,236 )
      RecnoRefresh(oBrw)

      FOR nI := 1 TO LEN(aHWindows)
          hWnd := aHWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT
   ELSE
      nK := LEN(aHWindows)

      Form_0.Row := GetDesktopHeight() - 20 * nK - nH
      Form_0.Col := 0 + 20 * nK
      aStaticTableColor := { 159-10 * nK,191,236 }  // цвет для карточки

      oBrw:SetColor( { 2 }, { RGB( 159 - 10 * nK,191,236 ) } )
      oBrw:hBrush := CreateSolidBrush( 159 - 10 * nK,191,236 )
      RecnoRefresh(oBrw)

      FOR nI := 1 TO LEN(aHWindows)
          hWnd := aHWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT

   ENDIF

RETURN Nil

///////////////////////////////////////////////////////////////////////////
#define COPYRIGHT  "Author by Andrey Verchenko. Dmitrov, 2019."
#define PRG_NAME   "TsBrowse - network opening of the database !"
#define PRG_VERS   "Version 1.0"
#define PRG_RU01   "Таблица по условному индексу и карточка полей БД"
#define PRG_RU02   "Простой сценарий работы с карточкой, " + CRLF + ;
                   SPACE(10) + 'кто последний правил карточку тот и прав !' 
#define PRG_EN01   "Table on conditional index and database field card"
#define PRG_EN02   "A simple script for working with a card," + CRLF + ;
                   SPACE(10) + "who is the last to rule the card is right !"
#define PRG_INFO1  "Tips and tricks programmers from our forum http://clipper.borda.ru"
#define PRG_INFO2  "SergKis, Grigory Filatov and other..."
 * Таблица по условному индексу и карточка полей БД 
 * Table on conditional index and database field card

/////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   RETURN MsgInfo( PadC( PRG_NAME , 70 ) + CRLF +  ;
                   PadC( PRG_VERS , 70 ) + CRLF +  ;
                   PadC( COPYRIGHT, 70 ) + CRLF + CRLF + ;
                   PadC( PRG_RU01 , 70 ) + CRLF + ;
                   PRG_RU02 + CRLF + CRLF + ;
                   PadC( PRG_EN01 , 70 )  + CRLF + ;
                   PRG_EN02 + CRLF + CRLF + ;
                   PadC( PRG_INFO1, 70 ) + CRLF + ;
                   PadC( PRG_INFO2, 70 ) + CRLF + CRLF + ;
                   hb_compiler() + CRLF + ;
                   Version() + CRLF + ;
                   MiniGuiVersion() + CRLF + CRLF + ;
                   PadC( "This program is Freeware!", 70 ) + CRLF + ;
                   PadC( "Copying is allowed!", 70 ), "About", "ZZZ_B_ALERT", .F. )

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName )  // получить Width текста
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize       // из MiniGUI.Init()

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize)
   nWidth := GetTextWidth(0, cText, hFont)        // ширина текста 
   DeleteObject (hFont)                    

   RETURN nWidth

//////////////////////////////////////////////////////////////////////////////
FUNCTION InfoDbase()
RETURN MsgInfo( Base_Current(), "Open databases" )

//////////////////////////////////////////////////////////////////////////////
FUNCTION Base_Current(cPar)
   LOCAL cMsg, nI, nSel, nOrder, cAlias, cIndx, aIndx := {}
   DEFAULT cPar := ""

   cAlias := ALIAS()
   nSel := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !" + CRLF 
      RETURN cMsg
   ENDIF

   nOrder := INDEXORD()  
   cMsg   := "Open Database - alias: " + cAlias + "   RddName: " + RddName() + CRLF
   cMsg   += "Path to the database - " + DBINFO(DBI_FULLPATH) + CRLF + CRLF
   cMsg   += "Open indexes: "

   IF nOrder == 0
      cMsg += " (no indexes) !" + CRLF 
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' )' + CRLF + CRLF
      FOR nI := 1 TO 100
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += HB_NtoS(nI) + ') - Index file: '  + CRLF + DBORDERINFO(DBOI_FULLPATH) + CRLF
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ")" + CRLF
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '"' + CRLF
            cMsg += '       FOR index: "' + OrdFor() + '"' + CRLF
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT)) + ' )' + CRLF + CRLF
            AADD( aIndx, STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) 
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS()
   ENDIF
   cMsg += "          Number of records = " + HB_NtoS(ORDKEYCOUNT()) + CRLF

   RETURN cMsg
