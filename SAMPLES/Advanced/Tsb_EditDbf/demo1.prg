/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Разное редактирование ячеек таблицы (для массивов) из DBf-файла
 * Замена целиком массива показа строк в таблице на массив показа строк юзера
 * Хранение сортировки показа строк таблицы юзера в ini-файле
 * _TBrowse() Miscellaneous editing of table cells (for arrays) from DBf-file
 * Replace the entire array of table row display with the array of user row display
 * Store the sorting of user table row display in the ini file
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

REQUEST DBFNTX, DBFCDX, DBFFPT
#define PROGRAM  "MiniGui: _TBrowse(). Miscellaneous editing of table cells (for arrays) from DBf-file."
#define PROGVER  "   Version 1.01 (21.05.2025)"
#define PROGINF  "Display test:"

FUNCTION Main()
   LOCAL nY, nX, nG := 20, aLng, nH, nW, aBClr := {255,178,178}
   LOCAL owc, oTbl, oTsb, aXdim, nWTsb, nHTsb, aRet, aName, aHead, aXDfct, nF
   LOCAL cVal := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()

#ifdef KEY_ENG // for this project demo1-en.hbp
   aLng := { "... Wait for the preparation to complete ...", "Show all table columns (12)" ,;
             "F5 - place window on current cell (fill cell) + F2, F3, F4, F7",;
             "3 column in the table - hide the display of the row (0) or set the row sorting by number" ,;
             "Do you want to save changes to the DB " , "Changed ", " positions in the table" ,;
             "Writing data to the DB-" , "(right click on the cell of this column)" }
#else
   aLng := { "... Дождитесь завершения подготовки ...", "Показать все столбцы таблицы (12)" ,;
             "F5 - поместить окно на текущую ячейку (заполнить ячейку) + F2, F3, F4, F7",;
             "3 столбец в таблице - скрыть отображение строки (0) или задать сортировку строк по номеру" ,;
             "Сохранить изменения в БД " , "Изменено ", " позиций в таблице" ,;
             "Запись данных в БД-", "(правый клик мышки на ячейке этой колонки)" }
#endif

   WaitWindow( {aLng[1] , App.ExeName }, .T., 600, 16, NIL, RED, aBClr )
   App.Cargo:nMemoChar := CalcMemoLine()   // for function MEMOLINE(xxxx,App.Cargo:nMemoChar,1)
                                           // the sizes change after reboot
   IF !UseDbf(@aXDfct)                     // открыть базы и считать справочник в массив
       QUIT                                // open databases and read the directory into an array
   ENDIF
   App.Cargo:aSprDfct := aXDfct            // массив dbf-неисправностей / dbf fault array
   aRet  := ArrayLoadDim()                 // считать массивы / count arrays
   aXdim := aRet[1]                        // данные таблицы / table data
   aName := aRet[2]                        // имена полей таблицы / table field names
   aHead := aRet[3]                        // шапка таблицы / table header
   aXdim := ArrayDbfLoad(aXdim,"Defect")   // загрузить данные в таблицу из базы
   WaitWindow()

   // фонты для программы задаются в Sets_ENV() / fonts for the program are set in Sets_ENV()
   SET FONT TO _GetSysFont(), App.Cargo:nFontSize  // ->    o:nFontSize := ??

   // размеры экрана программы задаются в Sets_ENV() / The program screen sizes are set in Sets_ENV()
   // можно протестировать любое разрешение экрана / you can test any screen resolution
   // o:aDisplayMode   := { 1440, 800 }
   nH := 800
   nW := App.Cargo:aDisplayMode[1] * 0.9

   IF nH > Sys.ClientHeight ; nH := Sys.ClientHeight
   ENDIF
   IF nW > Sys.ClientWidth  ; nW := Sys.ClientWidth
   ENDIF

   DEFINE WINDOW wMain CLIENTAREA nW, nH TITLE App.Cargo:cTitle  ;
          MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR aBClr         ;
          ON INIT    ( This.Topmost := .F., _wPost( 0) )         ;  // выполняется после инициализации окна
          ON RELEASE ( This.Hide, _wSend(90) )                      // выполняется перед разрушением окна
          //ON INTERACTIVECLOSE {|lRet| lRet := myQuit(.F.) }       // НЕТ выхода, пока есть ещё окна - резерв
          This.Cargo := oHmgData() ; owc := This.Cargo              // создадим контейнер для этого окна

      owc:cForm   := This.Name
      owc:aBClr   := This.Backcolor
      owc:cAls    := ALIAS()             // ВАЖНО ! используется далее в коде
      owc:nRecno  := RECNO()             // IMPORTANT! used further in the code
      nW          := This.ClientWidth
      nH          := This.ClientHeight

      nY := 5
      DRAW ICON IN WINDOW wMain AT nY, nW-96-10 PICTURE "1MG" WIDTH 96 HEIGHT 96 COLOR aBClr

      @ nY, 5 LABEL Buff VALUE cVal WIDTH nW-195-nY*2 HEIGHT nH - nY*2 ;
        SIZE 14 FONTCOLOR WHITE TRANSPARENT RIGHTALIGN

      @ nY+30, nW-96-nY*2-80 BUTTONEX Btn_Test WIDTH 80 HEIGHT 45 CAPTION "Test" ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost("_BtnTest", , This.Name) }

      cVal := aLng[2]                        // кроме колонки массива и объекта
      nX   := nY := nG                       // except array column and object
      @ nY, nX CHECKLABEL Chk_1 WIDTH nW-500 HEIGHT 35                ;
               VALUE cVal LEFTCHECK IMAGE { 'CheckT28', 'CheckF28' }  ;
               SIZE 15 FONTCOLOR RED BACKCOLOR aBClr                  ;
               ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )            ;
               ON INIT {|| This.Checked := .F.          }             ;
               ACTION  {|| This.Checked := ! This.Checked, _wPost(22,, This.Checked) }

      nY += This.Chk_1.Height - 2

      cVal := aLng[3]
      @ nY, nX LABEL Lbl_0 VALUE cVal WIDTH nW-nG*2-96 HEIGHT nF SIZE 15 FONTCOLOR RED TRANSPARENT

      nY := 5 + 96 + 5  ; nX := nG

      nWTsb := nW - nG * 2 ; nHTsb := nH - nY - nG

      cVal := aLng[4]
      nF   := 16 * 2
      @ nY - nF, nX LABEL Lbl_1 VALUE cVal WIDTH nW-nG*2-96 HEIGHT nF SIZE 15 FONTCOLOR RED TRANSPARENT

      /////////////////////// Table ///////////////////////////////////////////////////////
      // назначаем параметры таблицы / assign table parameters
      oTsb := TableParam( owc:cForm, aXdim, "cTable", aName, aHead, nWTsb)
      // function in library \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oTbl := _TBrowse( oTsb, aXdim, "cTable", nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы / here we do some fine-tuning of the table
      oTbl:Cargo:nCellFoc := 3                            // фокусная ячейка / focal cell
      oTbl:Cargo:nModify  := 0                            // счётчик изменений / change counter
      oTbl:Cargo:cMsg_9   := aLng[9]                      // текст сообщения / message text
      oTbl:Cargo:dZDate   := (owc:cAls)->DateZa           // Дата заявки ! ВАЖНО ! Используется далее в коде
                                                          // Date of application ! IMPORTANT ! Used further in the code
      // объект положим на окно / we put the object on the window
      This.Cargo:oTbl     := oTbl                         // oWnd:Cargo:oTbl
      This.Cargo:cTbl     := oTbl:cControlName            // "cTable"
      This.Cargo:aIsxTbl  := oTbl:aArray                  // ИСХОДНЫЙ массив ВАЖНО / ORIGINAL array IMPORTANT !!!
      CalcMemoLine(oTbl)  // размеры меняются после перезагрузки / the sizes change after reboot
                          // for function MEMOLINE(xxxx,App.Cargo:nMemoChar,1)

      // обработка событий окна и др. объектов окна / handling window events and other window objects
      WITH OBJECT This.Object
        :Event( 0, {|ow|  // executed after window initialization
                     Local ob := ow:Cargo:oTbl      // получить объект таблицы через cargo окна
                     Local op := ob:Cargo:oParam    // получить параметры (oTsb) таблицы через cargo таблицы
                     ob:Show()
                     //MsgDebug(ob:Cargo)           // показ переменной
                     DO EVENTS
                     IF App.Cargo:lDebug
                        _SetThisFormInfo(ow)
                        WaitWindow( "...  W  A  I  T  ...", .T. )
                        ? _o2log(op, 27, ProcNL() + "  oTsb => ", .T. ) // проверка / examination
                        WaitWindow()
                        _SetThisFormInfo()
                     ENDIF
                     ob:Setfocus()
                     _wSend(1)
                     DO EVENTS
                     Return Nil
                     } )

        :Event( 1, {|ow| // показ массива по сортировке юзера, через клон массива
                         // display array by user sorting, via array clone
                         Local obr   := ow:Cargo:oTbl     // исходный массив таблицы / source table array
                         Local aIsx  := ow:Cargo:aIsxTbl
                         Local aDim  := ACLONE(aIsx)
                         Local i, aVal, aUser := {}
                         If IsArray(aDim)
                            obr:Hide()
                            aDim := ASORT( aDim,,, { |x, y| x[ADIM_SORT] < y[ADIM_SORT] } )
                            For i := 1 TO Len(aDim)
                               aVal := aDim[i]
                               If aVal[ADIM_SORT] # 0
                                  AADD( aUser, aVal )
                               Endif
                            Next
                            If Len(aUser) == 0
                               aVal    := ACLONE(aDim[1])
                               aVal[1] := "No lines to show!"
                               aVal[2] := "No lines to show!"
                               AADD( aUser, aVal )
                               AADD( aUser, aVal )
                            Endif
                            // далее заместить массив ТСБ на новый !!!
                            // then replace the TSB array with a new one!!!
                            obr:aArray := aUser
                            obr:nLen   := LEN(aUser)
                            obr:Reset()
                            obr:Refresh()
                            obr:Show()
                            obr:Setfocus()
                         Endif
                         DO EVENTS
                         Return Nil
                         } )

        // в качестве резерва / as a reserve
        :Event( 2, {|ow,ky,cn| _SetThisFormInfo(ow) , MsgDebug(ow:Name,ky,cn) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event({3,"_BtnTest"}, {|ow,ky,cn| // кнопка Тест / Test button
                                 _SetThisFormInfo(ow)
                                 TestChess(ow:Name,cn,ky)  // -> tsb_Checkerboard.prg
                                 _SetThisFormInfo()
                                 This.Buff.Setfocus
                                 //ow:Setfocus("Buff")
                                 DO EVENTS
                                 Return Nil
                                 })

        :Event( 4, {|ow| // закрасить активную ячейку / paint the active cell - example
                         _SetThisFormInfo(ow)
                         Cell_Window_Test(ow)
                         _SetThisFormInfo()
                         _wPost(5)  // перерисовать текущую ячейку таблицы
                         DO EVENTS
                         Return Nil
                         })

        :Event(5, {|ow,ky,lr,ob| // redraw the current table cell
                                 // lr == .T. - :refresh()
                     ob := ow:Cargo:oTbl
                     IF !Empty(lr) ; ob:Refresh()
                     ENDIF
                     ob:DrawSelect()
                     ob:SetFocus()
                     DO EVENTS
                     ky := lr
                     Return Nil
                     })

        :Event(10, {|ow,ky,cn| // просмотр текущей строки массива / view current row of array
                     Local ob := ThisWindow.Cargo:oTbl
                     ? ProcNL(), ":Event(", ky,")", ow:Name, cn
                     ? "      [nAt]=", ob:nAt, ":aArray[nAt]", ob:aArray[ob:nAt]
                     //?v ob:aArray[ob:nAt]
                     //?v ob:aDefValue[ob:nAt]
                     _SetThisFormInfo(ow)
                     MsgDebug("[nAt]=", ob:nAt,":aArray=",ob:aArray[ob:nAt], ">> - - - End - - -<<" )
                     _SetThisFormInfo()
                     Return Nil
                     })

        :Event( 19, {|ow| ow:Cargo:oTbl:DrawFooters() })  // при смене курсора таблицы - перерисуем подвал
                                                          // when changing the table cursor - redraw the footer
        :Event( 20, {|ow| // прорисовка таблицы после CHECKLABEL / drawing the table after CHECKLABEL
                     Local obr  := ow:Cargo:oTbl
                     Local nAlg := iif( This.Chk_1.Checked, DT_RIGHT, DT_CENTER )
                     obr:nCell  := obr:Cargo:nCellFoc
                     obr:oHScroll := NIL
                     obr:Refresh()
                     obr:DrawSelect()
                     obr:SetFocus()
                     obr:Show()
                     DO EVENTS
                     obr:nAlignSupHdSet(1,, nAlg)
                     obr:SendMsg(WM_KEYDOWN, VK_RIGHT, 0) // _PushKey(VK_RIGHT)
                     DO EVENTS
                     Return Nil
                     } )

        :Event(22, {|ow,ky,lc| // переключать видимость столбцов / toggle column visibility
                     Local aIsx  := ow:Cargo:aIsxTbl    // исходный массив таблицы / source table array
                     LOCAL cName, oCol, nCol, nI
                     Local obr   := ow:Cargo:oTbl       // <<<--- важно / important
                     Local aHide := {"F_NN","F_PROCES", "F_BASE", "F_READ", "F_WRITE", "F_WINDOWS", "F_ACCESS","V_CALC", "V_SPR","V_AADD"}
                     //                 3         4         5        6           7             8        9         10        11      12
                     Default lc := .T.
                     obr:Hide()   // скрыть таблицу для дальнейшей прорисовки / hide table for further drawing
                     obr:aArray := aIsx        // заместить массив ТСБ на ИСХОДНЫЙ массив / replace the TSB array with the ORIGINAL array
                     obr:nLen   := LEN(aIsx)
                     obr:Reset()
                     //
                     IF lc
                        // "---- показ всех колонок ----"
                        FOR EACH oCol IN obr:aColumns
                           nCol := hb_EnumIndex(oCol)
                           oCol:lVisible := lc
                        NEXT
                        //obr:lDrawSpecHd   := .T.  - нельзя, ломает показ ТСБ
                     ELSE
                        //obr:lDrawSpecHd   := .F.  - нельзя, ломает показ ТСБ
                        // ---- скрыть показ колонок / hide column display ----
                        FOR EACH oCol IN obr:aColumns
                           nCol  := hb_EnumIndex(oCol)
                           cName := oCol:cName
                           IF "_" $ cName             // вариант-2 / option-2
                              //oCol:lVisible := .F.
                           ENDIF
                           FOR nI := 1 TO LEN(aHide)
                              IF cName == aHide[nI]
                                 oCol:lVisible := .F.
                              ENDIF
                           NEXT
                        NEXT
                        _wSend(1)  // сортировка юзера / sort user
                        DO EVENTS
                     ENDIF
                     _wSend(20)  // перерисовать таблицу / redraw the table
                     ky := lc
                     Return Nil
                     } )

        :Event(23, {|ow,ky,xc| // переключать CHECKLABEL Chk_1 из подвала таблицы, колонка (2)
                               // toggle CHECKLABEL Chk_1 from table footer, column (2)
                               //  ob:aColumns[2]:bFLClicked
                     ? ProcNL(), ":Event(", ky,")", ow:Name,xc
                     ky := ! This.Chk_1.Checked
                     This.Chk_1.Checked := ky
                     _wSend(22, ow, ky)
                     DO EVENTS
                     Return Nil
                     } )

        //  ob:aColumns[3]:bFLClicked - меню сортировки пользователя / user sort menu
        :Event(24, {|ow,ky,cN| _SetThisFormInfo(ow) , myContexMenuSort(ow,ky,cN) ,;    // tsb_EditWindows.prg
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        // назначим клавиши в таблице, см. ниже / assign keys in the table, see below
        // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_RETURN   , MsgDebug(...)
        //             _wPost(  32  ,  33  , 34   , 35-"_3Save" , 36

        :Event({50,"_TsbRClick"}, {|ow,ky,xc| // Right click on the cursor in the table
                                    Local cm, ob, p1, p2, p3, nAt
                                    DO EVENTS
                                    cm := ProcNL()
                                    ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                                    ?? "Правый клик мышки на курсоре в таблице"
                                    ?  Repl(".", Len(cm)), ow:Name, "{p1,p2,p3,ob}=", xc
                                    p1 := xc[1] ; p2 := xc[2] ; p3 := nAt := xc[3] ; ob := xc[4]
                                    ?? "{", p1, p2, p3, ob, "}"
                                    // p1, p2 - координаты клика, p3=nAt, ob = объект таблица
                                    // p1, p2 - click coordinates, p3=nAt, ob = table object
                                    ? "   выбор [nAt]=", nAt, ":aArray[nAt]=", ob:aArray[nAt]
                                    //?v ob:aArray[nAt]
                                    //? "------------------"
                                    _SetThisFormInfo(ow)
                                    Tsb_RClickContexMenu(ob, ob:aArray[nAt], nAt)  // -> tsb_EditWindows.prg
                                    _SetThisFormInfo()
                                    DO EVENTS
                                    Return Nil
                                    })

        :Event(90, {|ow,ky| // ON Release windows - executed before window destruction
                     Local cm, nMdf := ow:Cargo:oTbl:Cargo:nModify  // счётчик изменений
                     DO EVENTS                                      // change counter
                     cm := ProcNL()
                     ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                     ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                     ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                     IF nMdf > 0
                        SET LANGUAGE TO ENGLISH
                        cm := aLng[5] + " DB-" + ow:Cargo:cAls + "?;"
                        cm += aLng[6] + HB_NtoS(nMdf) + aLng[7]
                        IF AlertYesNo(cm)
                           WaitWindow( {aLng[8] + ow:Cargo:cAls, App.ExeName }, .T., 400, 16, NIL, WHITE, RED )
                           DbSelectArea(ow:Cargo:cAls)                // ВАЖНО !
                           DbGoto(ow:Cargo:nRecno)                    // IMPORTANT !
                           // запись из массива в базу / write from array to database
                           ArrayDbfSave(ow:Cargo:oTbl:aArray)
                           (ow:Cargo:cAls)->( DbCloseArea() )
                           // запись сортировки показа / record sorting display
                           IniSave(ow:Cargo:aIsxTbl)
                           WaitWindow()
                        ENDIF
                     ENDIF
                     DbCloseAll()   // закрыть все базы / close all bases
                     DO EVENTS
                     Return Nil
                     })

        :Event(99, {|ow| ow:Release() })
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY F5     ACTION _wPost(4)
      ON KEY F7     ACTION _wPost(10)
      ON KEY ESCAPE ACTION IIF( oTbl:IsEdit, oTbl:SetFocus(), _wPost(99) )

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TableParam(cForm,aXDim,cBrw,aName,aHead,nWTsb)
   LOCAL oTsb, nClr1, nClr2, a, nI, aWSize, cT, nHCell

   cT := This.Title
   cT := Subs(cT, At(".", cT) + 1)
   cT := Left(cT, At(".", cT) - 1)
   cT := space(3) + Alltrim(cT) + space(3)
   //
   oTsb := oHmgData()                // создадим контейнер для таблицы
                                     // let's create a container for the table
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // обязательно так / definitely so !!!
   oTsb:cFormName      := cForm      // или так / or like this
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35       // высота ячеек  / cell height
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 4) }   // колонка нумерации и её ширина / numbering column and its width
   oTsb:nHeightCell    := nHCell                                  // the supplement depends on the screen size
   oTsb:nHeightHead    := 1                                       // высота шапки таблицы  / table header height
   oTsb:nHeightFoot    := nHCell                                  // высота подвала таблицы / table footer height
   oTsb:lFooting       := .T.                                     // ставить в таблице подвал / put a footer in the table
   oTsb:lSpecHd        := .T.                                     // поставить в таблице нумератор колонок / put a column numberer in the table
   oTsb:lSuperHd       := .T.                                     // поставить в таблице суперхидер / put superheader in the table
   oTsb:cSuperHd       := cT                                      // текст суперхидера / superheader text
   oTsb:nHeightSuperHd := nHCell - 4                              // высота суперхидера / superheader height
   oTsb:nCellMarginLR  := 0                                       // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
                                                                  // indent from the cell line when pressing left, right by the number of spaces
   //oTsb:uSelector    := 20                                      // селестор слева таблицы / selestor left of table
   oTsb:lNoPicture     := .T.
   ? ProcNL(), "######## nHCell=", nHCell

   oTsb:aName := aName
   oTsb:aHead := aHead
   // можно задать просто цифры в шапке таблицы / you can simply specify numbers in the table header
   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead  := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF
   //              1  2  3  4  5  6  7  8  9 10 11 12 13
   oTsb:aHead    := {"","","","","","","","","","","",""} // убираем шапку таблицы - используем 11 колонок массива
   oTsb:aHideCol := { 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidths(aXDim,2,nWTsb)   // подсчёт ширины колонок - показ только 2 колонок, остальные не нужны
   oTsb:aSize    := aWSize                                // назначим ширину колонок / let's assign the width of the columns

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.
      oTsb:aFoot       := .T.
   ENDIF

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize
   ENDIF

   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      //oTsb:nHeightSuperHd := 20
   ENDIF

   nClr1               := HMG_RGB2n(This.Backcolor)         // цвет фона шапка+подвал / background color header+footer
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон / grey and black background
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера / color: superheader text and background
   oTsb:aBrush         := {240,240,240}                     // цвет фона под таблицей / background color under table

   // цвета в таблицу
   oTsb:lZebra    := .F.                                    // вкл.\откл. механизм zebra / on/off zebra mechanism
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // серый цвет / gray
   oTsb:aZebra    := { {251,227,227}, {207,152,149} }
   oTsb:aBZebra   := {RGB(251,227,227), RGB(255,178,178), ;
                      CLR_RED, CLR_MAGENTA, CLR_YELLOW }   // тут можно больше цветов / More colors are possible here
   oTsb:nBZebraNo := 3
   oTsb:aFZebra   := {CLR_BLACK, CLR_HBLUE, CLR_GREEN, CLR_RED, CLR_MAGENTA}  // для текста ячеек / for cell text
   oTsb:nBZebraNo := 4

   a := {}
   //AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, {|nr,nc,ob| // меняем цвет на свою зебру - вариант-3
                        Local nClr := ob:nClrPane, cTyp, aClr, oTsb, oCol, nCol
                        IF !IsArray(ob:aArray) ; Return nClr
                        ENDIF
                        //? ProcNL(), nr,nc,ob, "ob:nLen=", ob:nLen, ob:aColumns
                        nc   := IIF( nc > LEN(ob:aColumns), LEN(ob:aColumns), nc )
                        //?? nc
                        oCol := ob:aColumns[nc]
                        oTsb := ob:Cargo:oParam
                        nCol := ob:nColumn("F_PROCES")
                        nCol += IIF( ob:nColumn("ARRAYNO", .T.) > 0, -1, 0)
                        nCol += IIF( ob:nColumn("SELECTOR", .T.) > 0, -1, 0)
                        aClr := oTsb:aFZebra         // цвета текста
                        nClr := aClr[1]
                        cTyp := ob:aArray[nr][nCol]
                        IF cTyp $ "LNCDT"           ; nClr := aClr[1]
                        ELSEIF cTyp == "SPR_A"      ; nClr := aClr[2]
                        ELSEIF cTyp == "SPR_S"      ; nClr := aClr[3]
                        ELSEIF cTyp == "SPR_J"      ; nClr := aClr[4]
                        ELSEIF cTyp == "M"          ; nClr := CLR_MAGENTA
                        ELSEIF cTyp == "CALC"       ; nClr := CLR_BLUE
                        ELSEIF cTyp == ""           ; nClr := CLR_HRED
                        ELSEIF "LINE" $ cTyp        ; nClr := CLR_WHITE
                        ELSE                        ; nClr := CLR_GRAY
                        ENDIF
                        Return nClr
                        }})

   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   //AAdd(a, { CLR_PANE, {|nr,nc,ob| // меняем цвет на свою зебру - вариант-2
   //                                Local nClr := ob:nClrPane, cVal
   //                                Local aClr := {RGB(251,227,227), ;
   //                                               RGB(207,152,149), ;
   //                                               CLR_HRED}
   //                                IF !IsArray(ob:aArray) ; Return nClr
   //                                ENDIF
   //                                nc := nr % 2
   //                                nClr := aClr[ nc + 1 ]
   //                                cVal := ob:aArray[nr][3]
   //                                IF cVal == "LINE1" ; nClr := aClr[3]
   //                                ENDIF
   //                                Return nClr
   //                                } } )

   AAdd(a, { CLR_PANE, {|nr,nc,ob| // меняем цвет на свою зебру - вариант-3
                        Local nClr := ob:nClrPane, cVal, aClr, oTsb, oCol, nKey
                        IF !IsArray(ob:aArray) ; Return nClr
                        ENDIF
                        oCol := ob:aColumns[nc]
                        oTsb := ob:Cargo:oParam
                        nKey := ob:nColumn("F_PROCES")
                        nKey += IIF( ob:nColumn("ARRAYNO", .T.) > 0, -1, 0)
                        nKey += IIF( ob:nColumn("SELECTOR", .T.) > 0, -1, 0)
                        aClr := oTsb:aBZebra
                        nClr := aClr[ nr % 2 + 1 ]
                        cVal := ob:aArray[nr][nKey]
                        IF cVal == "LINE1"               ; nClr := aClr[3]
                        ELSEIF cVal == "LINE2"           ; nClr := aClr[4]
                        // колонку в др. цвет при механизме zebra
                        ELSEIF oCol:cName == "F_PROCES"  ; nClr := aClr[5]
                        ELSEIF oCol:cName == "F_BASE"    ; nClr := CLR_ORANGE
                        ELSEIF oCol:cName == "F_READ"    ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_WRITE"   ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_WINDOWS" ; nClr := RGB(194,194,194)
                        ELSEIF oCol:cName == "F_ACCESS"  ; nClr := RGB(92,242,212)
                        ELSEIF oCol:cName == "F_NN"      ; nClr := RGB(120,242,137)
                        ENDIF
                        Return nClr
                        }})

   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы / table header text
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы  / table header background
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора / cursor background
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля / editable field text
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля / editable field background
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы / table footer text
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы / table footer background

   // --- не надо так, сделано выше    oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }
   //AAdd(a, { CLR_SUPF , CLR_WHITE                })  // 16, фона суперхидера / superheader background
   //AAdd(a, { CLR_SUPB , CLR_RED                  })  // 17, текста суперхидера / superheader text

   //AAdd(a, { CLR_SPCF , CLR_YELLOW             })  // 18, specheader text - нумератор
   //AAdd(a, { CLR_SPCB , { nClr1, nClr2 }       })  // 19, specheader back - нумератор
   AAdd(a, { CLR_SPCF , CLR_RED                  })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , RGB(240,240,240)         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a

   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // code blocks for _TBrowse(...) - the names of the variables bInit,body,bEnd,After cannot be changed
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб / tsb settings
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб / other tsb settings
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) }                     // блок кода после END TBROWSE
   //  блок кода oTsb:bEnd после END TBROWSE - НЕ использовать без необходимости - все делать в oTsb:bAfter !!!
   //  code block oTsb:bEnd after END TBROWSE - DO NOT use unless necessary - do everything in oTsb:bAfter !!!

   oTsb:bInit := {|ob,op| // настройки тсб
                   ob:Hide()                                      // скрыть таблицу для дальнейшей прорисовки
                   ob:HideColumns( op:aHideCol ,.t.)              // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO") + 1    // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   // --------- хранилище картинок, удаляется после закрытия объекта автоматом ------
                   ob:aBitMaps    := { LoadImage("bGear24"), LoadImage("bEye24"), LoadImage("bGear20") ,;
                                       LoadImage("bGear16"), LoadImage("bEye16"), LoadImage("bAttach24") ,;
                                       LoadImage("bAttach24x2"), LoadImage("bAttach24x3")  }
                   // редактирование ячеек таблицы -> см. ниже
                   myTsbEdit(ob,op)
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lPickerMode := .F.
                   oc := ob:aColumns[3]
                   oc:lEdit  := .T.
                   oc:cPicture := Nil
                   oc:lCheckBox := .T.
                   oc:nAlign := DT_LEFT
                   oc:nEditMove := 0    // перечитать ячейку
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll    := NIL
                   IF ob:nHeightHead < 3 ; ob:GetColumn(1):cHeading := " "
                   ENDIF
                   IF ob:nHeightFoot < 3 ; ob:GetColumn(1):cFooting := " "
                   ENDIF
                   oc := ob:aColumns[2]
                   oc:cFooting     := "customizing table rows"
                   oc:uBmpFoot     := ob:aBitMaps[3]  // [3] картинка в подвале колонок таблицы
                   oc:nFAlign      := nMakeLong( DT_LEFT, DT_LEFT  )
                   oc:nBmpMaskFoot := 0x00BB0226    // MERGEPAINT
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(23, ob:cParentWnd, ob) }
                   oc := ob:aColumns[4]
                   oc:uBmpFoot     := ob:aBitMaps[2]  // [2] картинка в подвале колонок таблицы
                   oc:nFAlign      := nMakeLong( DT_CENTER, DT_CENTER )
                   oc:nBmpMaskFoot := 0x00BB0226  //0x00CC0020    // SRCCOPY
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(24, ob:cParentWnd, ob:aColumns[4]:cName) }
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         //oc:nWidth    := GetTextWidth( Nil, "0000", hFont )   // кол-во знаков - здесь не будет работать
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   // при смене курсора таблицы / when changing the table cursor
                   ob:bChange := {|ob|  _wPost(19, ob:cParentWnd, ob) }
                   // поставить картинки на строки таблицы / put pictures on table rows
                   oc := ob:GetColumn("REDIT")
                   oc:uBmpCell := {|nc,ob|
                                     Local hBmp/*, oc := ob:aColumns[nc]*/, nn := 0
                                     Local aRec := ob:aArray[ ob:nAt ]
                                     Local cKey := aRec[5]
                                     //? ProcNL(), nc, HB_ValToExp(aRec), cKey
                                     IF IsString(cKey)
                                        cKey := upper(cKey)
                                        IF     cKey == upper("aDefect")  ; nn := 6  // "bAttach24" см.выше
                                        ELSEIF cKey == upper("MOb4orud") ; nn := 7  // "bAttach24x2" см.выше
                                        ELSEIF cKey == upper("MAktVip")  ; nn := 8  // "bAttach24x3" см.выше
                                        ENDIF
                                     ENDIF
                                     IF nn > 0 ; hBmp := ob:aBitMaps[ nn ]
                                     ENDIF
                                     nn := nc
                                     Return hBmp
                                     }
                   oc:nAlign := DT_LEFT
                   // смена курсора таблицы / change table cursor
                   // cFooting := Eval( oColumn:cFooting, nCol, oBrw )
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   Local cc := ob:Cargo:cMsg_9
                                   cc := Iif( IsString(cc), cc, "" )
                                   nc := ""
                                   If ob:nLen > 0
                                      nc := hb_ntos(na)+ "/" + hb_ntos(nl)
                                      nc += " - " + ob:aArray[na][4]
                                      nc += " , " + cValToChar( ob:aArray[na][5] )
                                      nc += " , " + cValToChar( ob:aArray[na][6] )
                                      nc += Space(5) + cc
                                   EndIf
                                   Return nc
                                   }
                   // пример трассировки / example of tracing
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nn, nw := 0
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       // НЕ НАДО МЕНЯТЬ РАЗМЕРЫ КОЛОНОК, потом будет трудно их получить через :GetCellSize()
                       // DO NOT CHANGE COLUMN SIZES, it will be difficult to get them later via :GetCellSize()
                       //IF oc:cName == "ARRAYNO"
                          nn := oc:nWidth
                          // вот здесь делаем уменьшение ширины колонки
                       //   oc:nWidth := GetTextWidth( Nil, "0000", oc:hFont )
                       //   nn -= oc:nWidth
                       //ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                    NEXT
                    IF !Empty(nn)
                       //oc := ATail(ob:aColumns)
                       //oc := ob:GetColumn("RNAME")
                       //oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    // можно так
                    //ob:UserKeys(VK_F2 ,  {|ob| myTsbListColumn( ob ), ob:Setfocus() })  // инфо по списку колонок
                    //ob:UserKeys(VK_F3 ,  {|ob| myTsbListFont( ob )  , ob:Setfocus() })  // инфо по фонтам таблицы
                    //ob:UserKeys(VK_F4 ,  {|ob| myTsbArrayLine( ob ) , ob:Setfocus() })  // инфо по строке таблицы
                    ob:SetNoHoles()
                    ob:Refresh()
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице - лучше так
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }  ;
                     }
   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по списку колонок
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по фонтам таблицы
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по строке таблицы
        {50, {|ow,ky,ob| _wPost("_TsbRClick",ow) , ky:=ow:=ob              } }  ;   // правый клик мышки
                     }

   // Двойной клик мышки на курсоре в таблице - уже есть
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // Вот так !!!
   // Правый клик мышки на курсоре в таблице
   oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(50, ob:cParentWnd, {p1,p2,p3,ob}) }
   // Левый клик мышки на курсоре в таблице
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок
FUNCTION CalculatColumnWidths(aXDim,nCol,nWTsb)
   LOCAL aDim, v, a, i, hFont, nW, aWSize

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle("Normal")
   aWSize := Array(Len(aDim[1]))
   aFill(aWSize, 0)

   FOR EACH a IN aDim
       FOR EACH v IN a
           i := hb_enumindex(v)
           // показ только 2 колонок
           //IF i > nCol ; LOOP
           //ENDIF
           // делаем расчёт на все колонки, т.к. будет показ других колонок
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           v  += "HH"  // добавка
           nW := GetTextWidth( Nil, v, hFont )
           IF i > 2
              nW := IIF( nW > 400, 400, nW )
           ENDIF
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT

   //oTsb:aNumber := { 1, 30 }                // колонка нумерации и её ширина
   // для колонки 2 делаем всю ширину экрана показа, кроме колонки 1
   aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ширина показа других колонок в зависимости от ширины текста
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb
   ?? "Колонка:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   //? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // колонки "V_CALC", "V_SPR", "V_AADD"
      IF "V_" $ cCol
         oCol:lEdit     := .T.
         oCol:nEditMove := 0  // откл. перемещение курсора после :Edit()
         oCol:bPrevEdit := {|xv,ob| // просмотр в окне
                                   Local nc, oc, cVal
                                   nc   := ob:nCell
                                   oc   := ob:GetColumn( nc )
                                   cVal := ob:GetValue(nc)
                                   SET WINDOW THIS TO ob
                                   //AlertInfo(cVal)    // можно и так
                                   AlertInfo(xv)
                                   SET WINDOW THIS TO
                                   Return .F. // запрет попадания в get-ячейки
                            }
      ELSEIF cCol == "F_NN"
         oCol:lEdit     := .T.
         oCol:nEditMove := 0  // откл. перемещение курсора после :Edit()
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> см.ниже
      ELSE
         //oCol:lEdit := .T. // задано выше oc:lEdit  := .T.
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> см.ниже
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> см.ниже
      ENDIF
      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются - для массива не работает
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, cFunc, cVal, nVal, cCol5
   LOCAL cTyp, cMsg, lWrt, cStr, aVal, aDim14, aRet, xDop15, a2Dim, aCode
   LOCAL cAccess, aDim, aVal13, nAt, aText, nJ, nI, aFld2, cErr, cFld, lYes
   LOCAL cMemo, cText, a3Dim

   WITH OBJECT ob
      aVal    := :aArray[:nAt]         // вся строка массива
      nCol    := :nCell
      oCol    := :aColumns[ nCol ]
      oCol:Cargo := oHmgData()         // создадим контейнер на колонку
      cAls    := :cAlias
      cTyp    := oCol:cFieldTyp        // тип обработки колонки
      cNam    := oCol:cName
      cJTyp   := aVal[ACOL_4]          // тип обработки строки
      Default cJTyp := "+"
      cFunc   := aVal[ACOL_8]          // функция получения массива значений
      aVal13  := aVal[ACOL_13]         // здесь будет массив значений для дальнейшей правки
      aDim14  := aVal[ACOL_14]
      xDop15  := aVal[ACOL_15]
      cAccess := aVal[ACOL_9]
      IF cAccess == "R"
         cJTyp := "0"
      ENDIF
      IF cNam == "F_NN"  // только это поле
         cJTyp := "N"
      ENDIF
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   //cStr += 'Column array type: "' + cTyp + '" ;'
   cStr += 'Column array cJTyp: "' + cJTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .F.                    // не записывать поле
   lRet := .F.                    // не давать редактировать поле в :get

   // изменим ввод числа только для этой колонки
   IF cNam == "REDIT" .AND. Valtype(uVal) == "N"
      ob:aEditCellAdjust[3] := 150 - oCol:nWidth
   ENDIF

   IF LEN(cJTyp) == 0             // нет обработки
   ELSEIF cJTyp == "0"            // нет обработки
   ELSEIF cJTyp $ "NLCD"
      lRet := .T.                 // редактировать поле в :get
      lWrt := .T.                 // записывать в ячейку
   ELSEIF cJTyp == "M"
      aRet := CellEditMemo(uVal, ob)   // -->> tsb_memo_cell.prg
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                // записывать в ячейку
      ENDIF
   ELSEIF cJTyp $ "DMN"
      cTyp := "D"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                 // записывать в ячейку
      ENDIF
   ELSEIF cJTyp $ "DT"
      cTyp := "T"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                 // записывать в ячейку
      ENDIF

   ELSEIF cJTyp $ "A"                // массив полей базы
      aRet := CellEdit_A(ob,aVal13,aDim14,xDop15)
      IF LEN(aRet) > 0
         ob:aArray[ob:nAt][ACOL_13] := aRet[1]
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(aRet[1])  // преобразование в "C" колонки (13)
         uVal := aRet[2]
         lWrt := .T.                 // записывать в ячейку
      ENDIF

   ELSEIF cJTyp $ "SPR_A"
      aRet := Tsb_ContexMenu(ob,aDim14,xDop15)  // см.ниже
      IF LEN(aRet) > 0
         uVal := aRet[2]
         ob:aArray[ob:nAt][ACOL_2]  := aRet[2]
         ob:aArray[ob:nAt][ACOL_13] := aRet[1]
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(nVal)  // преобразование в "C" колонки (13)
         lWrt := .T.                 // записывать в ячейку
      ENDIF

   ELSEIF cJTyp $ "SPR_S"
      IF UPPER(cFunc) == "SPR_2DBF()"
         a2Dim := Spr_2Dbf(aDim14)
         aDim  := a2Dim[1]             // новый одномерный массив
         aCode := a2Dim[2]             // коды значений
      ENDIF
      aRet := Tsb_ContexMenu(ob,aDim,xDop15)  // см.ниже
      IF LEN(aRet) > 0
         uVal := aRet[2]
         nVal := aRet[1]
         IF nVal == 0   ; nVal := 0           ; cVal := "..."
         ELSE           ; nVal := aCode[nVal] ; cVal := aRet[2]
         ENDIF
         ob:aArray[ob:nAt][ACOL_2]  := cVal
         ob:aArray[ob:nAt][ACOL_13] := nVal
         ob:aArray[ob:nAt][ACOL_10] := myVal2Str(nVal)  // преобразование в "C"
         lWrt := .T.                   // записывать в ячейку
      ENDIF

   ELSEIF cJTyp $ "CALC"
      ? ProcNL(), cJTyp, HB_ValToExp( ob:aArray[ob:nAt][13] )
      SET WINDOW THIS TO ob:cParentWnd
      DO EVENTS
      IF UPPER(cFunc) = "MYWINCALC()"
         aRet := Tsb_myWinCalc(ob,aDim14,xDop15,aVal)        // -> tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal               // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)  // преобразование в "C" колонки (13)
            lWrt := .T.                                      // записывать в ячейку
         ENDIF
      ELSEIF UPPER(cFunc) = "MYWINCALC2()"
         aRet := Tsb_myWinCalc2(ob,aDim14,xDop15,aVal)       // -> tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal               // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)  // преобразование в "C" колонки (13)
            lWrt := .T.                                      // записывать в ячейку
         ENDIF

      ELSEIF UPPER(cFunc) = "MYWINCALC3()"
         aRet := Tsb_myWinCalc3(ob,aDim14,xDop15,aVal)        // tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal                // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)   // преобразование в "C" колонки (13)
            lWrt := .T.                                       // записывать в ячейку
         ENDIF

      ELSEIF UPPER(cFunc) = UPPER("ZaListNeis()")            // -> tsb_EditWindows.prg
         aFld2 := aVal[ACOL_14]  // поля в которые нужно записать значения - искать в ob:aArray[nJ][ACOL_2]
         IF !IsArray(aFld2)
            cErr := "Ошибка ! Колонка [ACOL_14] не массив !;;"
            cErr += ProcNL() + ";" + ProcNL(1)
            AlertStop(cErr,ProcNL(),,64,{RED})
         ENDIF
         ? "###### ", "aFld2=",aFld2, HB_ValToExp(aFld2)
         a2Dim := Tsb_ZaListNeis(ob,aDim14,xDop15,aVal)
         IF LEN(a2Dim) > 0
            aCode := a2Dim[1]
            aText := a2Dim[2]
            // исправления в ТСБ из Tsb_ZaListNeis()
            uVal += "."                                       // делам для перезаписи всего ТСБ
            ob:aArray[ob:nAt][ACOL_13] := aCode               // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aCode)  // преобразование в "C" колонки (13)
            ? "###### [ACOL_13] = ", HB_ValToExp(aCode)
            //MsgDebug("Здесь сделать запись в 10 позиций вниз ТСБ или найти их по метке !",LEN(a2Dim),a2Dim)
            nAt  := ob:nAt    // запомнить откуда ушёл
            cErr := ""
            FOR nI := 1 TO 10
               cFld  := UPPER(aFld2[nI])
               ? "###### ", nI, cFld, "##"
               lYes  := .F.
               FOR nJ := 1 TO LEN(ob:aArray)
                  cCol5 := ob:aArray[nJ][ACOL_5]
                  //?? nJ, cCol5, VALTYPE(cCol5)
                  IF !IsString(cCol5)
                     cCol5 := "это не поле !" + cValToChar(cCol5)
                  ENDIF
                  IF cFld == UPPER(cCol5)
                     cVal := HB_NtoS(aCode[nI])              // код значения справочника в текст
                     ?? nJ, cCol5, "OLD:", ob:aArray[nJ][ACOL_2], "NEW:", aText[nI]
                     ob:aArray[nJ][ACOL_2]  := aText[nI]
                     ob:aArray[nJ][ACOL_13] := cVal          // (13) - значение исправленного поля {} для типа CALC
                     ob:aArray[nJ][ACOL_10] := cVal          // преобразование в "C" колонки (13)
                     lYes := .T.
                  ENDIF
               NEXT
               IF !lYes
                  cErr += "Не найдено поле в колонке (5): " + cFld + ";"
               ENDIF
            NEXT
            IF LEN(cErr) > 0
               cErr += ";; Смотрите log-ошибок, строка: " + ProcNL() + ";"
               AlertStop(cErr,ProcNL(),,64,{RED})
            ENDIF
            ob:nAt := nAt                               // вернуться откуда ушёл
            ob:Refresh()
            lWrt  := .T.                                // записывать в ячейку
         ENDIF
         ? "###### " + ProcNL(), "-> в Tsb_ZaListNeis()", HB_ValToExp(aCode), aText

      ELSEIF UPPER(cFunc) = UPPER("Tovar_HMG()")            // -> tsb_Tovar.prg
         // поле тип C - нужно записать значения: "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;"
         aVal13 := aVal[ACOL_13]   // здесь  массив значений для дальнейшей правки
         IF !IsString(aVal13)
            cErr := "Ошибка ! Колонка [ACOL_13] не строка !;;"
            cErr += ProcNL() + ";" + ProcNL(1)
            AlertStop(cErr,ProcNL(),,64,{RED})
         ENDIF
         a3Dim := Tsb_Tovar_HMG(ob,aVal13,aVal)
         IF LEN(a3Dim) > 0
            cMemo := a3Dim[1]
            cText := a3Dim[2]   // ->  [ACOL_13]
            a2Dim := a3Dim[3]   // ->  { {"SumVsego","SumWObor","SumMaster"} , {nSum_All,nSumObor,nSumMast} } }
            uVal  := cMemo
            ob:aArray[ob:nAt][ACOL_2]  := cMemo
            ob:aArray[ob:nAt][ACOL_13] := cText              // (13) - значение исправленного поля {} для типа CALC
            ob:aArray[ob:nAt][ACOL_14] := a2Dim              // показ колонки (10)
            ob:aArray[ob:nAt][ACOL_15] := ""                 // резерв
            ob:aArray[ob:nAt][ACOL_10] := cText              // преобразование в "C" колонки (13)
            ob:aArray[ob:nAt][ACOL_11] := HB_ValToExp(a2Dim) // преобразование в "C" колонки (14)
            ob:aArray[ob:nAt][ACOL_12] := ""                 // преобразование в "C" колонки (15)
            lWrt  := .T.                                     // записывать в ячейку
         ENDIF
         ? "###### " + ProcNL(), "-> в Tsb_ZaListNeis()", HB_ValToExp(aCode), aText

      ELSEIF UPPER(cFunc) = UPPER("Win_Fld64()")            // -> demo1_line64.prg
         //  ( 1) - [C]   ** Список квартир (шахматка)
         //  ( 2) - [C]   выбрана квартира:     (правая кнопка мышки - меню создания квартир)
         //  ( 3) - [N]   64
         //  ( 4) - [C]   CALC
         //  ( 5) - [C]   MAKTVIP
         //  ( 6) - [C]   Get_Fld64()
         //  ( 7) - [C]   Set_Fld64()
         //  ( 8) - [C]   Win_Fld64()
         //  ( 9) - [C]   W
         //  (10) - [C]   {"", "10, 9, 8/1, 7, 6, 5, 1 "}
         //  (11) - [C]   {"NAKTVIP", "MAKTVIP"}
         //  (12) - [C]   Run_Line64()
         //  (13) - [A]   {"", "10, 9, 8/1, 7, 6, 5, 1 "} - правим эти данные, только [1], [2] - не правим
         //  (14) - [A]   {"NAKTVIP", "MAKTVIP"}          - запись в поля базы, здесь не используем
         //  (15) - [C]   Run_Line64()
         aVal13 := aVal[ACOL_13]   // здесь  массив значений для дальнейшей правки
         IF !IsArray(aVal13)
            cErr := "Ошибка ! Колонка [ACOL_13] не массив !;;"
            cErr += ProcNL() + ";" + ProcNL(1)
            AlertStop(cErr,ProcNL(),,64,{RED})
         ENDIF
         aDim := Tsb_Checkerboard(ob,aVal13[2],aVal)     // -> tsb_Checkerboard.prg
         DO EVENTS
         IF LEN(aDim) > 0
            cVal  := aDim[1]      // вернём одно значение
            IF LEN(ALLTRIM(cVal)) == 0
               // пропуск
            ELSE
               cText := ALLTRIM(aVal[ACOL_2])
               cMsg  := ALLTRIM(SUBSTR(cText, AT("(",cText)-1))
               cText := ALLTRIM(SUBSTR(cText,1, AT(":",cText)))
               cMemo := cText + " " + cVal + SPACE(5) + cMsg
               uVal  := cMemo
               ob:aArray[ob:nAt][ACOL_2]  := cMemo
               ob:aArray[ob:nAt][ACOL_13] := {cVal, aVal13[2]}  // (13) - значение исправленного поля {} для типа CALC
               ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(a2Dim) // преобразование в "C" колонки (13)
               lWrt  := .T.                                     // записывать в ячейку
            ENDIF
         ENDIF
         ? "###### " + ProcNL(), "-> в Tsb_Checkerboard() uVal=", uVal

      ELSE
         MsgDebug("ERROR! Нет обработки функции !",cFunc)
      ENDIF
      ? "###### " + ProcNL(), cJTyp,"[13]=" ,HB_ValToExp( ob:aArray[ob:nAt][ACOL_13] )
      ? "###### " + ProcNL(), cJTyp,"[10]=" ,HB_ValToExp( ob:aArray[ob:nAt][ACOL_10] )
      SET WINDOW THIS TO

   ELSE
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                           // записывать ячейку
      //ob:Cargo:nModify ++          // счётчик-изменения в таблице
      //ob:SetValue(nCol,uVal)       // <== только при lRet == .F., в :bPostEdit ТСБ пишет сам, без твоего участия
   ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   //    ob:Cargo:nModify ++          // счётчик-изменения в таблице
   //    это делается в :bPostEdit при lRet == .T., используя значение в колонке
   //    IF oCol:xOldEditValue != uVal  <== :bPostEdit !!!
   //       ob:Cargo:nModify ++
   //    ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   // Вот этот случай нужно делать !!! Иначе нет сохранения в базу для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   IF lWrt .AND. !lRet
      ob:Cargo:nModify ++          // счётчик-изменения в таблице
      ob:SetValue(nCol,uVal)       // <== только при lRet == .F., в :bPostEdit ТСБ пишет сам, без твоего участия
      ob:DrawSelect()              // перерисовать текущую ячейку таблицы
      ob:SetFocus()
   ENDIF
   ? "###### " + ProcNL(), lWrt,lRet, cJTyp, "[2]=", HB_ValToExp( ob:aArray[ob:nAt][ACOL_2] )

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
// Срабатывает после ввода в get
STATIC FUNCTION myTsbEditPost( uVal, ob )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod, cJTyp
   LOCAL oWnd  := _WindowObj(ob:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT ob
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp               // тип обработки колонки
      uOld := oCol:xOldEditValue           // old value
      lMod := ! uVal == uOld               // .T. - modify value
      cAls := :cAlias
      cJTyp := ob:aArray[ob:nAt][ACOL_4]   // тип обработки строки
      Default cJTyp := "+"
   END WITH

   IF Valtype(uVal) == "N"                           // !!!!!!!!!!!!!!!!!!!!!
      ob:aEditCellAdjust[3] := 0
   ENDIF

   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   //cStr += ';Column processing type: "' + cTyp + '" ;'
   cStr += ';Column array cJTyp: "' + cJTyp + '" ;'

   IF cJTyp $ "CNDLM"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF
   //    это делается в :bPostEdit при lRet == .T., используя значение в колонке
   IF oCol:xOldEditValue != uVal  // <== :bPostEdit !!!
      ob:Cargo:nModify ++         // счётчик-изменения в таблице
   ENDIF

   ? "###### " + ProcNL(), cJTyp, "[2]=", HB_ValToExp( ob:aArray[ob:nAt][ACOL_2] )
   ob:DrawSelect()    // перерисовать текущую ячейку таблицы
   ob:SetFocus()

   DO EVENTS

RETURN .T.

///////////////////////////////////////////////////////////////////////////
// закрасить активную ячейку
STATIC FUNCTION Cell_Window_Test(oWnd)
   Local ob := oWnd:Cargo:oTbl, ocel
   Local op := ob:Cargo:oParam
   Local nc := ob:nCell, cfrm
   Local nr := ob:nRowPos, y, x, w, h, n
   Local oc := ob:aColumns[ nc ]
   Local xv := ob:GetValue(nc)
   Local cv := cValToChar(xv)
   Local hFont, nWCell, cChar, nW, nLenChar, cVal

   IF Empty( This.Chk_1.Checked ) .and. oc:cName == "REDIT"
      cfrm := "w" + oc:cName
      ocel := ob:GetCellSize(nr, nc)
      y := ocel:nRow   ; x := ocel:nCol
      w := ocel:nWidth ; h := ocel:nHeight

      IF ValType(xv) == "D"
         n := GetFontWidth(op:aFont[1], Len(cv)) * 0.85  // "Normal"
         w -= n
         x += n
      ELSE
         n := GetFontWidth(op:aFont[1], 1) * 0.7   // "Normal" ~ width 1 знака
         n := int( w / n )                         // ~ кол-во символов в колонке
         IF Len( cv ) > n
            cv := left( cv, n ) + "@"
         ENDIF
      ENDIF

      hFont  := oc:hFont
      nWCell := oc:nWidth
      nWCell -= GetVScrollBarWidth()
      nWCell -= GetTextWidth( Nil, "A", hFont )   // :nCellMarginLR
      nWCell -= 24                                // oCol:uBmpCell
      nWCell -= 12                                // добавка
      cChar  := "000" + REPL("A", 20)
      n := GetFontWidth(op:aFont[1], 1)           // "Normal" 1 знак
      nLenChar := int( nWCell / n )               // ~ кол-во символов в колонке
      cv    := HB_NtoS(nLenChar)
      cChar := REPL("A", nLenChar - LEN(cv) - 1 ) + "|"
      cVal  := cv + cChar
      nW    := GetTextWidth( Nil, cVal, hFont )

      DEFINE WINDOW &cfrm AT y, x WIDTH w HEIGHT h  ;
             MODAL NOSYSMENU NOCAPTION NOSIZE       ;
             BACKCOLOR YELLOW                       ;
             ON INIT ( wApi_Sleep(3000), _wPost(99) )

         w := This.ClientWidth
         h := This.ClientHeight

         @ 0, 0 LABEL Buff VALUE cVal WIDTH w HEIGHT h ;
                           FONTCOLOR RED TRANSPARENT

         (This.Object):Event(99, {|ow| ow:Release() })

      END WINDOW

      ACTIVATE WINDOW &cfrm

   ENDIF

RETURN Nil

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )


   rddSetDefault( "DBFCDX" )

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
   SET DATE FORMAT TO "DD.MM.YY"

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   // Проверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start." + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое
   SET WINDOW MAIN OFF

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   // для отладки - потом убрать
   cLog             := o:cLogFile
   //o:cLogFile       := cFilePath( cLog ) + "\"
   //o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:tStart         := hb_DateTime()       // start time
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := {  5 , 191, 255 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM  + " ! " + PROGVER + " !   " + PROGINF
   o:cVersion       := PROGVER
   o:cTsbVersion    := PROGVER              // это для записи в ини-файл сортировки строк таблицы
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2025 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\DBF\"
   o:cPathStart     := GetStartUpFolder() + "\"
   o:cIniSortUser   := o:cPathTemp + 'tmp_SortUser.ini'
   o:cIniMemoLine   := o:cPathTemp + 'tmp_MemoLine.ini'
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   o:aDisplayMode   := { 1280, 800 }
   // далее везде при построении окон и проверок ставить так:
   // then everywhere when building windows and checks set it like this:
   // App.Cargo:aDisplayMode[1] ...  App.Cargo:aDisplayMode[2]
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:nMemoChar      := 0
   o:cTitle         += " " + o:cDisplayMode + "  FontSize: " + HB_NtoS(o:nFontSize)
   //o:lPosWinOrTsb := .T.   // позиция окна по родит.окну / window position by parent window
   o:lPosWinOrTsb   := .F.   // позиция окна по ТСБ / window position according to TSB

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName , o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName , o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", o:cFontName , o:nFontSize-2, .T., .T. )
   _DefineFont("SpecHdr" , o:cFontName , o:nFontSize-5, .T., .T. )
   _DefineFont("SuperHd" , o:cFontName2, o:nFontSize+2, .F., .F. )
   _DefineFont("TsbEdit" , "Arial"     , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS" , o:cFontName2 , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MnNormal" , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MenuBtn"  , o:cFontName  , o:nFontSize   , .T., .F. )         // фонт кнопок верхнего меню
   _DefineFont("WinBtn"   , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт кнопок окон
   // Menu* font - demo1-ru.hbp
   _DefineFont("FntBtn_1" , "Segoe Script", o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("FntCnMn1" , "Bodoni MT"   , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )             // фонт окна Alert*
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   SetMenuBitmapHeight( 32 )          // set menu icons size to 32x32
   SET WINDOW MODAL PARENT HANDLE ON  // окна Modal получают родителя - активное окно MiniGui

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? "Free Open Software:", Version()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", MiniGuiVersion()
   ? ATREPL( "!", App.Cargo:cTitle, CRLF ) ; ?

RETURN
