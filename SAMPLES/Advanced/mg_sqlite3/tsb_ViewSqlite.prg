/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Окно с таблицей / Window with table
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
#include "hbsqlit3.ch"
#include "dbinfo.ch"

///////////////////////////////////////////////////////////////////
FUNCTION Tsb_ViewSqlite(oWnd, nPos, cWIco, cTable, lWin1)
   LOCAL cForm, hForm, aBColor, cTitle, cMsg, cSelect
   LOCAL oBrw, cBrw, oTsb, nY, nX, nH, nW, nG, o, owc, ao
   LOCAL nH1, nW2, a4Clr, nDelta, cFont, nFSize
   LOCAL nTotalRecno, cAlias
   DEFAULT lWin1 := .T.

   ? ProcNL(), oWnd, nPos, cWIco, cTable, lWin1

   ao       := App.Cargo
   nY       := ao:nHMain                // высота окна главной формы
   nX       := 0  ; nG := 20
   nW       := ao:aDisplayMode[1]       // размеры окна определены в 0main.prg
   nH       := ao:aDisplayMode[2] - nY  // размеры окна определены в 0main.prg
   nG       := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   //nW     := Sys.ClientWidth
   //nH     := Sys.ClientHeight - nY
   cTitle   := HB_NtoS(nPos) + ":" + cTable
   cForm    := "Form_Tsb" + HB_NtoS(nPos)
   a4Clr    := App.Cargo:a4Clr             // цвета для окон таблицы
   aBColor  := a4Clr[1]
   cBrw     := "Tsb_" + HB_NtoS(nPos)
   cFont    := ao:cFontName2
   nFSize   := ao:nDlgSize
   nDelta   := 40

   IF !lWin1   // для всех окон
      nY += (nPos - 1) * nDelta
      nX += (nPos - 1) * nDelta
   ENDIF

   If MGVersNumba() == 231206
   Elseif MGVersNumba() > 240700
   Else
      cMsg := IIF( ao:cLang == "RU", "ОШИБКА ПОКАЗА БД !;", "ERROR DISPLAYING DB !;" )
      cMsg += IIF( ao:cLang == "RU", "Компиляция только на версии МиниГуи 24.08 и выше;",;
                          "Compilation only on MiniGui version 24.08 and higher;" )
      cMsg += IIF( ao:cLang == "RU", 'Программа может "упасть"...', 'The program may "crash"...' )
      cMsg += ";;" + MiniGuiVersion() + ";;" + ProcNL()
      AlertStop( cMsg , , "ZZZ_B_STOP64", 64 )
   Endif

   IF _IsWindowDefined(cForm)
      hForm := GetFormHandle(cForm)
      IF hForm != 0
         IF IsIconic( hForm ) ; _Restore( hForm )
         ENDIF
         DoMethod(cForm, "SetFocus")
      ENDIF
      RETURN "" // пусто, эта форма уже открыта
   ENDIF

   // запись открытого окна таблицы
   AADD( App.Cargo:aWinOpen, cForm )

   //cTable := "Country Official Languages"
   IF AT(" ", cTable ) > 0
      cAlias := ATREPL( " ", cTable, "_" )
      cTable := "'" + cTable + "'"
   ELSE
      cAlias := cTable
   ENDIF
   cSelect := "SELECT * FROM " + cTable
   //dbUseArea( TRUE,, cSelect, cTable,,, "UTF8" )
   dbUseArea( TRUE,, cSelect, cAlias,,, "UTF8" )

   cMsg := IIF( ao:cLang == "RU", "Открываю таблицу: ", "Open the table:" ) + cTable
   WaitWindow( {cMsg,oWnd:Cargo:cFile}, .T., 800, 13, NIL, BLACK, App.Cargo:aDlgBColor, 14, BLUE, 4 )
   nTotalRecno := SQLITE_RECCOUNT(cTable, oWnd:Cargo:cFile)
   WaitWindow()

   ? Alias(), "cTable=", cTable, cSelect, RddName(), "nTotalRecno=", nTotalRecno
   ? oWnd:Cargo:cFile

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE cTitle ICON cWIco                        ;
      MINWIDTH 500 MINHEIGHT 500                     ; // блокировка уменьшения размеров окна
      WINDOWTYPE STANDARD TOPMOST                    ;
      ON MAXIMIZE ( ResizeForm( This.Object ) )      ;
      ON SIZE     ( ResizeForm( This.Object ) )      ;
      BACKCOLOR aBColor                              ;
      FONT cFont SIZE nFSize                         ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor      // цвет окна
      owc:oMainCrg := oWnd:Cargo          // Cargo родительское окно
      owc:nG       := nG
      owc:cAls     := ALIAS()             // алиас в этом окне
      owc:ahIcoDel := {}                  // для удаления хендлов иконок с формы
      owc:cTable   := cTable
      owc:cFile    := oWnd:Cargo:cFile    // путь к файлу
      owc:cRequest := cSelect
      owc:aIcoFltr := { "iAFilter32x1", "iAFilter32x3" }    // смена иконок             - резерв
      owc:aMn2FClr := { WHITE, RED    }                     // смена цвета фонта кнопки - резерв
      owc:aMn2BClr := { owc:aBColor, YELLOW }               // смена цвета фона кнопки  - резерв
      owc:nFilter  := 1                                     //                          - резерв
      // верхнее меню окна с кнопками
      TopMenuViewButtons(owc)          // -> tsb_ViewMenu.prg
      //owc:aMn2Text                   // смена меню { "Фильтр" , "Очистка" } -> tsb_ViewMenu.prg

      nY  := owc:nHTBar //+ nG
      nX  := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nH1 := 5
      nW2 := owc:nWEndTB    // конец кнопок

      owc:cRus  := "F2-инфо,   Ins-новая запись, Del-удалить/восстановить запись"
      owc:cEng  := "F2-info,   Ins-new recno, Del-delete/restore recno"
      owc:cMsg1 := IIF( ao:cLang == "RU", owc:cRus, owc:cEng)
      @ nH1, nW2 + nG LABEL Lbl_1 VALUE owc:cMsg1 AUTOSIZE FONTCOLOR WHITE TRANSPARENT
      nH1 += This.Lbl_1.Height + 1

      owc:cRus  := "" //F9-удалить список"
      owc:cEng  := "" //F9-Delete list"
      owc:cMsg2 := IIF( ao:cLang == "RU", owc:cRus, owc:cEng)
      @ nH1, nW2 + nG LABEL Lbl_2 VALUE owc:cMsg2 AUTOSIZE FONTCOLOR WHITE TRANSPARENT

      This.MinWidth  := owc:nWEndTB + nG + GetBorderWidth()*2  // блокировка уменьшения размеров окна
      //This.MinHeight := owc:nHBtnEnd + GetBorderHeight()*2   // блокировка уменьшения размеров окна

      /////////////////////// таблица ///////////////////////////////////////////////////
      oTsb := oHmgData()
      oTsb:cForm  := cForm
      oTsb:cAls   := ALIAS()
      oTsb:nTotal := nTotalRecno   // всего записей в таблице
      // координаты таблицы
      oTsb:nY     := nY
      oTsb:nX     := nG
      oTsb:nW     := nW - oTsb:nX * 2
      oTsb:nH     := nH - oTsb:nY - nG

      @ oTsb:nY, oTsb:nX LABEL Label_Table PARENT &cForm WIDTH oTsb:nW HEIGHT oTsb:nH ;
        VALUE '' SIZE 20 CENTERALIGN BACKCOLOR WHITE INVISIBLE
      owc:cLabel := 'Label_Table'

      oBrw := Draw_Table( oTsb, cBrw, oWnd, owc )         // таблица
      IF IsObject(oBrw)
         oBrw:Cargo:owc := owc                  // запомнили на таблице
         owc:oBrw       := oBrw                 // запомнили на окне
         owc:cBrw       := cBrw                 // запомнили на окне
         //_o2log(owc , 15, ProcNL()+" -------------- Параметры объекта : => owc", .T.)
         //_o2log(oTsb, 15, ProcNL()+" Параметры объекта : => oTsb", .T.)
         //ON KEY ESCAPE ACTION ( iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) ) ) - если надо
         owc:lSayTable := .T.             // это таблица
      ELSE
         IF IsString(oBrw) ;  cMsg := oBrw
         ENDIF             ;  cMsg := 'Table'
         This.Lbl_2.Value       := ""
         This.Label_Table.Value := cMsg
         This.Label_Table.Show
         owc:lSayTable := .F.             // это НЕ таблица
      ENDIF

      ON KEY F1     ACTION NIL

      o := This.Object
      o:Event( 0, {|ow| // запуск после построения окна
                        This.Topmost := .F.
                        ? ProcNL(),">>> Start window: "+ow:Name
                        IF ! owc:lSayTable          // это НЕ таблица
                           This.&("_ATable").Enabled  := .F.
                           This.&("_AExport").Enabled := .F.
                        ELSE
                           ow:Cargo:oBrw:SetFocus()
                        ENDIF
                        DO EVENTS
                        Return Nil
                        })
      // имя объекта + имя события    aObj  := { "_ATable" , "_AExport", "_AExit"  }
      //            VVVV
      o:Event({10,"_ATable" }, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                              _SetThisFormInfo(ow)       ,;
                                              myTableStruct(ow,ky,cn,ob) ,;  // -> tsb_ViewMenu.prg
                                              _SetThisFormInfo()         ,;
                                              This.&(cn).Enabled := .T.  ,;
                                              ky:=cn , ob:Setfocus()  } )

      o:Event({11,"_AFilter"}, {|ow,ky,cn,ob| // фильтр на таблицу
                                             Local cFltr, owc := ow:Cargo
                                             Local cCapt, cIco, aFClr, aBClr
                                             This.&(cn).Enabled := .F.
                                             ob    := ow:Cargo:oBrw
                                             cIco  := owc:aIcoFltr[1]
                                             cCapt := owc:aMn2Text[1]
                                             aFClr := owc:aMn2FClr[1]
                                             aBClr := owc:aMn2BClr[1]
                                             ? "  ###", ProcNL(), ky,cn, ob:cAlias, "!!! Фильтр !!!"
                                             IF owc:nFilter == 1               // надпись Фильтр
                                                _SetThisFormInfo(ow)
                                                Form4Filter(ow,ky,cn,ob)       // -> tsb_ViewFilter.prg - новая таблица
                                                _SetThisFormInfo()
                                                DbSelectArea(ob:cAlias)        // на всякий случай
                                                cFltr := App.Cargo:cRetFilter  // вернуть строку фильтра
                                                If LEN(cFltr) > 0
                                                   ob:FilterData( cFltr )      // установка фильтра на базу
                                                   mySuperHdFilter(ob, cFltr)  // показ фильтра в суперхидере
                                                   owc:nFilter := 2
                                                   cIco  := owc:aIcoFltr[2]
                                                   cCapt := owc:aMn2Text[2]
                                                   aFClr := owc:aMn2FClr[2]
                                                   aBClr := owc:aMn2BClr[2]
                                                Endif
                                             Else                               // надпись Очистить
                                                ob:FilterData()                 // очистка фильтра на базу
                                                mySuperHdFilter(ob, "")         // показ фильтра в суперхидере
                                                owc:nFilter := 1
                                             Endif
                                             This.&(cn).Enabled   := .T.
                                             This.&(cn).Caption   := cCapt
                                             This.&(cn).Fontcolor := aFClr
                                             This.&(cn).Backcolor := aBClr
                                             This.&(cn).Icon      := cIco
                                             ob:SetFocus()
                                             DO EVENTS
                                             Return Nil
                                             } )

      o:Event({12,"_AExport"}, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                             _SetThisFormInfo(ow)         ,;
                                             TableToExport(ow,ky,cn,ob)   ,;     // -> tsb_export.prg
                                             _SetThisFormInfo()           ,;
                                             This.&(cn).Enabled := .T.    ,;
                                             ob:Setfocus()  } )

      o:Event({89,"_AExit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

      o:Event(90, {|ow,ky,ah,i| // ON Release
                              ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                              ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                              IF LEN(ow:Cargo:cAls) > 0
                                 (ow:Cargo:cAls)->( dbCloseArea() )    // ОБЯЗАТЕЛЬНО !!!
                              ENDIF
                              ah := ow:Cargo:ahIcoDel
                              ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                              ?? ah, HB_ValToExp(ah)
                              IF IsArray(ah)
                                 AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                              Endif
                              // удалить открытое окно таблицы из App.Cargo:aWinOpen
                              If Len(App.Cargo:aWinOpen) > 0
                                 ? Repl(".", 10),"App.Cargo:aWinOpen =" + HB_ValtoExp(App.Cargo:aWinOpen)
                                 For i := 1 TO Len(App.Cargo:aWinOpen)
                                     If UPPER(App.Cargo:aWinOpen[i]) == UPPER(ow:Name)
                                        ? Repl(".", 10),"Delete " + ow:Name + " from App.Cargo:aWinOpen"
                                        hb_ADel(App.Cargo:aWinOpen, i, .T.)
                                     Endif
                                 Next
                              Endif
                              DO EVENTS
                              Return Nil
                              })

      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

   //CENTER WINDOW &cForm
   IF lWin1   // для одного окна
      ACTIVATE WINDOW &cForm
   ENDIF

RETURN cForm

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm( oWnd )
   LOCAL nG, owc, nTsbY, nTsbX, cBrw, nH, nW, nHTBar, oBrw, cObj
   DEFAULT oWnd := _WindowObj( GetActiveWindow() )

   nW     := This.ClientWidth
   nH     := This.ClientHeight
   owc    := oWnd:Cargo
   nG     := owc:nG
   nHTBar := owc:nHTBar      // конец кнопок по высоте
   oBrw   := oWnd:Cargo:oBrw   // считали с окна

   ? ProcNL(), oBrw, oBrw:cAlias, oBrw:cControlName
   IF ISOBJECT(oBrw)
      // объект Tbrowse изменить
      nTsbY  := owc:nTsbY
      nTsbX  := owc:nTsbX
      cBrw   := owc:cBrw

      //cBrw   := oBrw:cControlName
      This.&(cBrw).Enabled := .F. // блокировать область таблицы (Строки не отображаются)

      // По методу Move() запускается ReSize() - описание параметров см. TControl.prg
      oBrw:Move( oBrw:nLeft, oBrw:nTop, This.ClientWidth - oBrw:nLeft - nG, This.ClientHeight - oBrw:nTop - nG, .T. )

      This.&(cBrw).Enabled := .T. // разблокировать область таблицы (Строки отображаются)

      oBrw:Paint()
      oBrw:Refresh(.T.)
      oBrw:SetNoHoles()
      oBrw:SetFocus()

   ELSE
      // объект Label изменить
      cObj := owc:cLabel
      This.&(cObj).Width  := nW - nG*2
      This.&(cObj).Height := nH - nG - nHTBar
   ENDIF

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION mySuperHdFilter(oBrw, cFilter)   // показ фильтра в суперхидере
   LOCAL cText, nSupHd1, nSupHd2, aClr16, nClr17
   DEFAULT cFilter := ""

   // то что ранее запомнили
   cText   := oBrw:Cargo:TitleSupHd
   aClr16  := oBrw:Cargo:Clr16SupHd
   nClr17  := oBrw:Cargo:Clr17SupHd

   IF LEN(cFilter) > 0
      //oBrw:aSuperHead[2,3] := cText1    // поменяли СуперХидер
      oBrw:aSuperHead[1,3] := "  FILTER: " + cFilter
      nSupHd1 := CLR_YELLOW
      nSupHd2 := CLR_RED
      aClr16 := { nSupHd1 , nSupHd2 }
      nClr17  := CLR_BLACK
   ELSE
      oBrw:aSuperHead[1,3] := cText
   ENDIF
   // задать цвета суперхидеру
   oBrw:SetColor( {16}, { aClr16  } )   // 16, фона суперхидеру
   oBrw:SetColor( {17}, { nClr17  } )   // 17, текста суперхидеру

   oBrw:DrawHeaders()                  // перечитать суперхидер/шапку/нумератор
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_Table( oTsb, cBrw, oWnd, owc )
   LOCAL oBrw, cForm, a4Clr, cSuperHd, nG

   nG               := owc:nG
   cSuperHd         := "FILE: " + cFileNoPath(oWnd:Cargo:cFile)
   cSuperHd         += "   TABLE: " + owc:cTable
   cSuperHd         += "   SCHEME: " + owc:cRequest
   //                      cell     Head    foot     SpecHider   SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }
   oTsb:uSelector   := 20
   oTsb:lSpecHd     := .T.    // поставить в таблице нумератор
   oTsb:lFooting    := .T.    // поставить в таблице подвал
   oTsb:aFoot       := .T.
   oTsb:aEdit       := .T.    // редактировать колонки
   a4Clr            := App.Cargo:a4Clr                // цвета для окон таблицы
   oTsb:a4Clr       := a4Clr                          // сохраним 4 цвета таблицы
   oTsb:aBrush      := a4Clr[3]                       // цвет фона под таблицей
   oTsb:aColor      := Color_Tsb(a4Clr,oTsb)          // цвета таблицы: 2(шапка+подвал),3(строка %1),4(строка %2)
   oTsb:cTtlSupHead := cSuperHd
   cForm            := oTsb:cForm                     // определено выше
   oTsb:aNumber     := { 1, 60 }

   // строим таблицу по заданным массивам
   DEFINE TBROWSE &cBrw OBJ oBrw OF &cForm ALIAS oTsb:cAls     ;
      AT oTsb:nY, oTsb:nX  WIDTH oTsb:nW HEIGHT oTsb:nH CELL   ;
      FONT   oTsb:aFont                                        ; // все фонты для таблицы
      COLORS oTsb:aColor                                       ; // цвета таблицы
      BRUSH  oTsb:aClrBrush                                    ; // цвет фона под таблицей
      COLNUMBER oTsb:aNumber                                   ; // слева таблицы виртуальная колонка с нумерацией
      ENUMERATOR                                               ; // нумерация колонок
      EDITABLE                                                 ; // редактировать таблицу
      SELECTOR .T.                                             ; // первая колонка - селектор
      AUTOCOLS                                                 ; // авто расчет размеров колонки по width
      ON CHANGE oBrw:Refresh(.f.,.f.)                          ; // убрать разрезание строк в таблице
      ON INIT  {|ob| ob:Cargo := oHmgData(), ;
                 ob:lNoChangeOrd  := .T., ;     // отключить сортировку
                 ob:nColOrder     :=  0 , ;     // убрать значок сортировки по колонке
                 ob:lNoGrayBar    := .F., ;     // T-НЕ показывать неактивный курсор в таблице
                 ob:lNoLiteBar    := .F., ;     // при переключении фокуса на другое окно не убирать "легкий" Bar
                 ob:lNoResetPos   := .F., ;     // предотвращает сброс позиции записи на gotfocus
                 ob:lPickerMode   := .F., ;     // формат даты нормальный через цифры
                 ob:nStatusItem   :=  0 , ;     // в 1-й Item StatusBar не выводить автоматом из тсб
                 ob:lNoKeyChar    := .T., ;     // .T. - откл. метод KeyChar(...) - ввод от букв, цифр
                 ob:nWheelLines   :=  1 , ;     // прокрутка колесом мыши
                 ob:nCellMarginLR :=  1 , ;     // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
                 oB:aEditCellAdjust[1] := -3,; // correction of cell row
                 ob:lMoveCols     := .F., ;
                 ob:nMemoHV       :=  1 , ;     // показ одной строки мемо-поля
                 ob:nLineStyle := LINES_ALL ,;
                 ob:nClrLine   := COLOR_GRID,;
                 ob:lCheckBoxAllReturn := .T. }

      :Cargo:nModify := 0                           // изменения в таблице
      :Cargo:aFont   := oTsb:aFont                  // запомним фонты
      :Cargo:aSupHd  := oTsb:aSupHd                 // суперхидер таблицы
      :Cargo:aIconDel:= oTsb:aIconDel               // удалить значение
      :Cargo:lRecINS := .F.                         // блокировка клавиши INS
      :Cargo:lRecDEL := .F.                         // блокировка клавиши DEL
      :Cargo:aStruct := {}                          // структура базы для экспорта
      :Cargo:cTable  := owc:cTable                  // имя таблицы
      :Cargo:cFile   := owc:cFile                   // путь к файлу
      :Cargo:cCdPg   := oWnd:Cargo:cCdPg            // положим на ТСБ CodePage файла
      :Cargo:nTotalRecno := oTsb:nTotal             // запомнили на таблице - всего записей

      myTsbInit(oBrw,oTsb)            // настройки таблицы
      myTsbFont(oBrw,oTsb)            // фонты в таблице
      myTsbEnum(oBrw)                 // ENUMERATOR по порядку
      myTsbSuperHd(oBrw,oTsb)         // SuperHeader
      myTsbKeyFX(oBrw,oTsb)           // обработка клавиш
      myTsbEdit(oBrw,oTsb)            // настройки редактирования

   END TBROWSE
   //END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }   // убрать дырку внизу таблицы - это для array и dbf

   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   myTsbEnd(oBrw,oTsb)             // последние действия с ТСБ

RETURN oBrw

//////////////////////////////////////////////////////////////////
STATIC FUNCTION Color_Tsb(aClr,oTsb)             // цвета таблицы
   LOCAL aColors, nPane2, nPane3, nPane, nHead1, nHead2, nBCSpH
   //                     1           2           3             4
   // aClr[4] цвета:  фона окна| шапка+подвал | строка %1 | строка %2 и под таблицей

   nPane   := HMG_RGB2n(aClr[3])  // цвет фона таблицы
   nPane2  := HMG_RGB2n(aClr[4])  // строка % 2
   nPane3  := CLR_BLUE            // удалённая запись
   nHead1  := HMG_RGB2n(aClr[2])  // цвет фона шапка+подвал
   nHead2  := RGB( 48, 29,26)     // серо-черный фон
   nBCSpH  := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы
   aColors := {}
   //AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK             } } )      // 1 , текста в ячейках таблицы
   //AAdd( aColors, { CLR_PANE  , {|| RGB(247,239,221)      } } )      // 2 , фона в ячейках таблицы
   // включаем условия показа
   //AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_GRAY, CLR_BLACK ) } } ) // 1
   AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK             } } )      // 1 , текста в ячейках таблицы
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), nPane3 ,;
                                            iif( ob:nAt % 2 == 0, nPane2, nPane ) )   } } )    // 2 , фона в ячейках таблицы
   oTsb:aClr1  := CLR_BLACK
   oTsb:aClr16 := { nHead1, nHead2 }
   oTsb:aClr17 := CLR_WHITE

   AAdd( aColors, { CLR_HEADF , {|| CLR_YELLOW            } } )        // 3 , текста шапки таблицы
   AAdd( aColors, { CLR_HEADB , {|| { nHead2, nHead1 }    } } )        // 4 , фона шапки таблицы
   AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLUE              } } )        // 5 , текста курсора в ячейках с фокусом
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   //AAdd( aColors, { CLR_FOCUSF, {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_WHITE, CLR_BLACK ) } } )  // 5 , текста курсора в ячейках с фокусом
   //AAdd( aColors, { CLR_FOCUSB, {|nr,nc,ob| myFocusB(nr,nc,ob,-CLR_HRED,-CLR_BLUE,-CLR_YELLOW) } } ) // 6 , фона курсора

   AAdd( aColors, { CLR_EDITF , {|| CLR_ORANGE            } } )        // 7 , текста редактируемого поля
   AAdd( aColors, { CLR_EDITB , {|| CLR_GREEN             } } )        // 8 , фона редактируемого поля

   AAdd( aColors, { CLR_FOOTF , {|| CLR_YELLOW            } } )        // 9 , текста подвала таблицы
   AAdd( aColors, { CLR_FOOTB , {|| { nHead1, nHead2 }    } } )        // 10, фона подвала таблицы
   AAdd( aColors, { CLR_SELEF , {|| CLR_GRAY   }            } )        // 11, текста неактивного курсора (selected cell no focused)
   AAdd( aColors, { CLR_SELEB , {|| { RGB(255,255,74), ;               // 12, фона неактивного курсора (selected cell no focused)
                                         RGB(240,240, 0) } } } )

   AAdd( aColors, { CLR_ORDF  , {|| CLR_WHITE  }             } )       // 13, текста шапки выбранного индекса
   AAdd( aColors, { CLR_ORDB  , {|| CLR_RED    }             } )       // 14, фона шапки выбранного индекса
   AAdd( aColors, { CLR_LINE  , {|| CLR_WHITE  }             } )       // 15, линий между ячейками таблицы
   AAdd( aColors, { CLR_SUPF  , {|| { nHead1, nHead2 }     } } )       // 16, фона спецхидер
   AAdd( aColors, { CLR_SUPB  , {|| CLR_HRED   }             } )       // 17, текста спецхидер
   AAdd( aColors, { CLR_SPCF  , {|| CLR_RED    }             } )       // 18, specheader text
   AAdd( aColors, { CLR_SPCB  , {|| nBCSpH     }             } )       // 19, specheader back
   AAdd( aColors, { CLR_SPCA  , {|| CLR_GREEN  }             } )       // 20, active specheader back

RETURN aColors

/*/////////////////////////////////////////////////////////////////
STATIC FUNCTION myFocusB(nAt, nCol, oBrw, nFoc, nClr, nDel)
   HB_SYMBOL_UNUSED(nAt)          // or Default nAt  := oBrw:nAtPos
   Default nFoc := -CLR_HRED
   Default nClr := -CLR_BLUE
   Default nDel := -CLR_YELLOW

   IF oBrw:nCell == nCol
      nClr := nFoc
   ELSEIF (oBrw:cAlias)->( Deleted() )
      nClr := nDel
   ENDIF

RETURN nClr*/

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // настройки таблицы
   LOCAL nHImg, nI, oCol, cCol, n, oDlu, cVal, aFont, cFont, nFSize
   LOCAL hFont, cHead, nWCol

   ? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName
   //_o2log(oTsb , 15, ProcNL()+" -------------- Параметры объекта : => oTsb", .T.)

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // подгоним размеры колонок по фонту
   oDlu := _Font2oDlu( oTsb:aFont[1] )
   n    := oDlu:nSize

   //!!! варианты задания размера
   ? SPACE(5) + _HMG_DefaultFontName, _HMG_DefaultFontSize, "n=", n, oTsb:aFont[1]
   ? SPACE(5) + "!!!",n," oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   nHImg := oDlu:H1 + 6              // высота картинки = высота строк в ТСБ
   //                ^^^ - константа
   nHImg := oDlu:H(1.25)             // так правильнее, от размера фонта высота
   //              ^^^^  - пропорция от размера фонта

   WITH OBJECT oBrw
      :Cargo:nModify := 0     // изменения в таблице

      :lNoKeyChar    := .F.          // НЕТ ввода в ячейки от букв, цифр
      :nHeightCell   := nHImg        // высота ячеек = высоте картинки
      :nHeightHead   := nHImg * 1.2  // высота шапки
      :nHeightFoot   := nHImg + 4    // высота подвала
      :nHeightSpecHd := n + n/2      // высота спецхидера ENUMERATOR
      :lFooting      := .T.          // использовать подвал
      :lDrawFooters  := .T.          // рисовать  подвалы
      //:nFreeze     := 2            // Заморозить столбец
      //:nCell       := :nFreeze + 1
      :lLockFreeze   := .T.          // Избегать прорисовки курсора на замороженных столбцах
      :nCellMarginLR :=  1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nMemoHV       :=  1           // показ одной строки мемо-поля
      :lNoKeyChar    := .F.          // нет ввода в ячейки от букв, цифр

      // --------- хранилище картинок, удаляется после закрытия объекта автоматом ------
      :aBitMaps      := { Nil, LoadImage("bRecDel16") }

      :nHeightCell := :nHeightCell + 1
      :nHeightHead := :nHeightCell
   END WITH

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         cVal := oCol:cFieldTyp + '('
         cVal += HB_NtoS(oCol:nFieldLen) + ','
         cVal += HB_NtoS(oCol:nFieldDec) + ')'
         oCol:cFooting := cVal
         oCol:nFAlign  := DT_CENTER
         AADD( oBrw:Cargo:aStruct , {cCol, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldLen} )
      ENDIF
      // для фонта MONO - DejaVu Sans Mono делаем добавку
      IF oCol:cFieldTyp == "C"
         //oCol:cPicture := Nil
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
      ELSEIF oCol:cFieldTyp $ "N"
         n := 2 + 1  // :nCellMarginLR :=  1
         oCol:nWidth   += oCol:ToWidth(n)
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         IF LEN( SET( _SET_DATEFORMAT ) ) > 8
            oCol:nWidth := oCol:ToWidth(10+2)   // "01.01.2024"
         ELSE
            oCol:nWidth := oCol:ToWidth(10)   // "01.01.24"
         ENDIF
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99" // 23 символа
         //oCol:bDecode  := {|tval| iif( tval == hb_CToT(""), "", hb_TtoS(tval) ) }
         //oCol:bDecode:= {|tval| hb_TtoS(tval) }
         //oCol:nAlign   := DT_LEFT
         // лучше так
         oCol:cPicture := NIL
         IF nFSize > 14  ; oCol:nAlign   := DT_LEFT
         ELSE            ; oCol:nAlign   := DT_CENTER
         ENDIF
         oCol:nWidth   := oCol:ToWidth(25)
         //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - это не работает, если задан oCol:cPicture, он в приоритете
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp == "M"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
      ENDIF
      // выровним ширину колонки
      hFont := oCol:hFontHead                  // какой фонт в колонке шапки
      cHead := oCol:cHeading + "H"
      nWCol := GetTextWidth( Nil, cHead, hFont )
      IF oCol:nWidth < nWCol
         oCol:nWidth := nWCol
      ENDIF
   NEXT

   // в момент постройки этих колонок НЕТ, строяться только после
   // блок кода после END TBROWSE
   ? ProcNL()
   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   ? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)
   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont, oCol, aFont

   hFont := oBrw:aColumns[1]:hFontSpcHd  // 4-special header font
   aFont := GetFontParam(hFont)
   ? ProcNL(), hFont , HB_ValToExp(aFont)

   // установить фонт для 1 колонки таблицы
   oBrw:aColumns[1]:hFont     := hFont     // 1-cells font
   oBrw:aColumns[1]:hFontFoot := hFont     // 3-footer font

   // установить фонт для всех колонок таблицы
   // спецхидера - ENUMERATOR (нумерация колонок)
   FOR EACH oCol IN oBrw:aColumns
      oCol:hFontSpcHd := hFont
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
// ENUMERATOR по порядку сделаем свой
STATIC FUNCTION myTsbEnum( oBrw, nOneCol )
   LOCAL oCol, nBCSpH, nI := 0, nCnt := 0
   DEFAULT nOneCol := 1

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы

   FOR EACH oCol IN oBrw:aColumns
      nI++
      oCol:cSpcHeading := NIL
      oCol:cSpcHeading := IIF( nI == nOneCol, "#" , "+" )
      IF nI > nOneCol
         IF oCol:lVisible
            oCol:cSpcHeading := hb_ntos( ++nCnt )
         ENDIF
      ENDIF
      // изменение цвета спецхидера - ENUMERATOR (нумерация колонок)
      oCol:nClrSpcHdBack := nBCSpH      // ::aColorsBack[ 18 ]
      oCol:nClrSpcHdFore := CLR_RED     // ::aColorsBack[ 19 ]
   NEXT

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw, oTsb )
   LOCAL hFont, nHFont, aSupHd, cSprHd, nClr16, nClr17, O

   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )
   aSupHd := oTsb:aSupHd
   O      := oBrw:Cargo
   cSprHd := oTsb:cTtlSupHead
   nClr16 := oTsb:aClr16
   nClr17 := oTsb:aClr17

   WITH OBJECT oBrw
      // Создаём СУПЕРХИДЕР в таблице размером 0
      :AddSuperHead( 1, :nColCount()+1, "Super_Header_Table" ) //,,, .F.,,, .F., .F., .F., 0, )
      :aSuperhead[ 1, 3 ] := cSprHd
      :nHeightSuper := nHFont * 1.5    // 1 строка
      // задать цвета суперхидеру
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, фона спецхидер
      :SetColor( { 17 }, { nClr17           } ) // 17, текста спецхидер
   END WIDTH

   o:TitleSupHd := oBrw:aSuperhead[ 1, 3 ]    // запомнить
   o:Clr16SupHd := nClr16                     // 16, фона
   o:Clr17SupHd := nClr17                     // 17, текста

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// другие настройки тсб
STATIC FUNCTION myTsbKeyFX( oBrw, oTsb )
   LOCAL o := oBrw:Cargo      // использовать из контейнера свои переменные
   LOCAL nLen, cBrw, nTsb

   ? ProcNL(), oBrw:cAlias, oTsb

   WITH OBJECT oBrw
      // обработка клавиш
      /*
      :UserKeys(VK_SPACE, {|ob|
                           Local lRet := .T., lval, cval
                           ob:Cargo:nModify ++  // была модификация таблицы
                           IF ob:nCell == 2
                              lval := ob:GetValue( ob:nCell )
                              cval := ob:GetValue( ob:nCell + 1 )
                              IF ! "---" $ cval
                                 ob:SetValue( ob:nCell, ! lval )
                                 ob:DrawSelect()
                                 DO EVENTS
                                 lRet := .F.
                              ENDIF
                           ENDIF
                           Return lRet
                           })
      :UserKeys(VK_RETURN, {|ob|
                            Local lRet := .T.
                            ob:Cargo:nModify ++  // была модификация таблицы
                            IF ob:nCell == 2
                               DO EVENTS
                               ob:PostMsg( WM_KEYDOWN, VK_SPACE, 0 )
                               lRet := .F.
                            ENDIF
                            Return lRet
                            })

      // колонка с нестандартным чекбоксом
      // т.к. колонка 2 это не CheckBox, выражение логическое, то тсб меняет лог.значение
      //  на текст из массива oBrw:aMsg, там языковые значения {"Да", "Нет" ...}
      IF hb_IsArray( :aMsg ) .and. Len( :aMsg ) > 1
         :aMsg[1] := ""
         :aMsg[2] := ""
      ENDIF
      */

      // обработка мышки
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :SetAppendMode( .F. )            // запрещена вставка записи в конце базы стрелкой вниз
      //oBrw:SetDeleteMode( .T., .F. )
      //oBrw:SetDeleteMode( .T., .T. ) // стандартный запрос на удаление
      :SetDeleteMode( .T., .F., {|| // меню для удаления/восстановления
                                    Local lDel, cDel, cIns, cMsg, cTtl
                                    Local lRet, aClrs := { {45,223,70} , ORANGE }
                                    Local aTmp, aBClr, aFClr
                                    If App.Cargo:cLang == "RU"
                                       cDel := "ВНИМАНИЕ !;Удалить запись в таблице ?"
                                       cIns := "ВНИМАНИЕ !;Восстановить запись в таблице ?"
                                       cTtl := "Подтверждение"
                                    Else
                                       cDel := "ATTENTION !;Delete a record in a table ?"
                                       cIns := "ATTENTION !;Restore a record in a table ?"
                                       cTtl := "Confirmation"
                                    Endif
                                    lDel  := (oBrw:cAlias)->(Deleted())
                                    cMsg  := iif(lDel, cIns, cDel)
                                    aBClr := {248,209,211}      // светло-красный
                                    aFClr := MAROON
                                    aTmp  := _SetMsgAlertColors(aBClr,aFClr)  // новые цвета
                                    lRet  := AlertYesNo( cMsg, cTtl, ,"ZZZ_B_STOP64", 64, aClrs )
                                    _SetMsgAlertColors(aTmp[1],aTmp[2])       // восстановить цвета
                                    Return lRet
                                } )
      // обработка клавиши ESC и других
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      :UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      :UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })

      // клавиши FXX
      :UserKeys(VK_F2    , {|ob| myTsbListColumn( ob ), ob:Setfocus() })  // инфо по списку колонок
      :UserKeys(VK_F3    , {|ob| myTsbListFont( ob )  , ob:Setfocus() })  // инфо по фонтам таблицы
      :UserKeys(VK_F9    , {|ob| ListDelete( ob )     , ob:Setfocus() })  // удалить список

      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth
      nLen := :GetAllColsWidth() - 1
      IF nLen > nTsb
         :lAdjColumn  := .T.
         :lNoHScroll  := .F.
         :lMoreFields := ( :nColCount() > 45 )
      ELSE
         :AdjColumns()
      ENDIF

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, nI, cCol

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      //? "    .",nI, cCol
      oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> tsb_view_func.prg
      oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> tsb_view_func.prg
      oCol:lEdit     := .T.
      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ДЕЛАЕМ ПОСЛЕ END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы
   a4Clr  := oTsb:a4Clr                     // считаем 4 цвета таблицы
   nTest  := HMG_RGB2n(a4Clr[1])            // цвет фона окна

   ? ProcNL(), MGVersNumba()
   ? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   ? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   oBrw:lClrSelectorHdBack := .F. // background OFF
   // виртуальная колонка / virtual column
   IF oBrw:lSelector
      oCol := oBrw:GetColumn("SELECTOR")
      oCol:nClrBack        := nBCSpH
      oCol:nClrFore        := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      //oCol:nClrSpcHdBack := nBCSpH
      oCol:SaveColor()                       // сохранить цвета колонки
      oBrw:nClrSelectorHdBack := nBCSpH      // Footer для "SELECTOR"
   ENDIF
   // Левый край TBROWSE
   oBrw:nClrHeadBack := nBCSpH

   // изменение виртуальной колонки
   nTest := oBrw:Cargo:nTotalRecno
   nLen  := LEN(HB_NtoS(nTest))
   nCol  := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // кол-во знаков + 2 знака
      oCol:nWidth := nWCol                                       // новая ширина
      oCol:cFooting := HB_NtoS(nTest)                            // RecCount
      // вариант показа - цвет
      oCol:nClrBack      := nBCSpH
      oCol:nClrFore      := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      oCol:nClrFootFore  := CLR_WHITE
      oCol:SaveColor()             // сохранить цвета колонки
      // замена фонта для колонки таблицы
      hFont := oBrw:aColumns[2]:hFontSpcHd       // 4-special header font
      oBrw:aColumns[nCol]:hFont     := hFont     // 1-cells font
      oBrw:aColumns[nCol]:hFontFoot := hFont     // 3-footer font
   ENDIF

   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// новая запись в базе добавляется в конец базы и переходим сразу к редактированию
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL nRecno, cMsg, aTmp, aBColor, aFColor, aColors, cTitle //, oRst

   ? " -Ins- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTitle := 'Добавление записи'
      cMsg   := "ВНИМАНИЕ !;Вставить запись в таблицу ?;"
   ELSE
      cTitle := 'Adding recno'
      cMsg   := "ATTENTION!;Insert a record into the table ?;"
   ENDIF

   aColors := { {45,223,70} , ORANGE }
   aBColor := { 238, 249, 142 }   // светло-жёлтый
   aFColor := BLACK
   aTmp    := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   IF AlertYesNo( cMsg, cTitle, , , 64, aColors )
      // срабатывает сразу при добавлении записи
      oBrw:bAddAfter := {|ob,ladd|
                          Local nRecno := (ob:cAlias)->( RecNo() )
                          LOCAL cTable  := ATREPL( "_", ob:cAlias, " " )
                          LOCAL cInsertQuery := "INSERT INTO " + cTable + " DEFAULT VALUES"

                          If ladd
                             ? "+++ :bAddAfter",ProcNL()
                             ?? "RecNo()= ", nRecno
                             IF rddInfo( RDDI_EXECUTE, cInsertQuery )
                             EndIf
                          EndIf
                          Return Nil
                        }

      // oBrw:bAddAfter  := Nil  // это если не нужен код заполнения полей при создании новой записи

      // встроенный метод для добавления записи
      oBrw:AppendRow()

      nRecno := (oBrw:cAlias)->( RecNo() )
      ? "+++ " + ProcNL(), hb_DateTime(), "Insert!", "RecNo()=", nRecno

      oBrw:nCell := 3  // в начало колонок для редактирования
      oBrw:Reset()
      oBrw:GoBottom()     // всегда на новую запись, если нет индекса
      DO EVENTS

   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])      // восстановить цвета

RETURN Nil

//////////////////////////////////////////////////////////////////////////
// удалённые записи в таблице можно восстановить
//                        только сразу не выходя из этой таблицы !!!
// В дальнейшем удаленная запись в самом файле-sqlite помечатется как
//  свободная запись и НЕ ПОДЛЕЖИТ восстановлению !!!
// только через спец.утилиты можно и не всегда восстанавить содержимое
STATIC FUNCTION RecnoDelete(oBrw)
   LOCAL lChange, nAt, lDelete, nRecno, nCell, nMetod, nRec

   ? " -Del- "+ProcNL(), oBrw:ClassName
   ?? ":nLen=", oBrw:nLen //,":lIsXXX=", oBrw:lIsDbf, oBrw:lIsArr
   ?? ":nRowPos=", oBrw:nRowPos

   // срабатывает сразу при удалении записи
   oBrw:bDelAfter := {|nr,ob|
                             Local cAls := ob:cAlias
                             Local nOld := (cAls)->( RecNo() )
                             LOCAL cTable  := ATREPL( "_", ob:cAlias, " " )
                             LOCAL cDeleteQuery := "DELETE FROM " + cTable
                             LOCAL cWhere := " WHERE "
                             LOCAL aStruct := oB:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы

                             cWhere += aStruct[ 1 ][ 1 ] + "="
                             cWhere += ClipValue2SQL( (cAls)->( FieldGet( 1 ) ) )
                             cDeleteQuery += cWhere
                             ? " -Del-  :bDelAfter" + ProcNL(), "nRecno=", nOld
                             ?? "cDeleteQuery=", cDeleteQuery
                             IF rddInfo( RDDI_EXECUTE, cDeleteQuery )
                             EndIf

                             Return nr
                            }

   lDelete := (oBrw:cAlias)->( Deleted() )
   nRecno  := (oBrw:cAlias)->( RecNo() )
   nCell   := oBrw:nCell    // маркер на колонке таблицы
   nAt     := oBrw:nAt      // для массива - строка курсора на экране
   nAt     := oBrw:nRowPos  // для dbf     - строка курсора на экране
   ? " -Del-  lDelete=", lDelete, "nRecno=",nRecno

   nMetod  := 0
   IF oBrw:lIsArr                 //  для массива
      ? " -Del- :nLen == :nAt", oBrw:nLen, oBrw:nAt
      IF oBrw:nLen == oBrw:nAt
         nMetod := 1  // это последняя запись
      ENDIF
   ELSEIF oBrw:lIsDbf            //  для dbf
      ? " -Del- ordKeyNo() == ordKeyCount()"
      ?? ordKeyNo(), ordKeyCount()
      IF ordKeyNo() == ordKeyCount()
         nMetod := 1  // это последняя запись
      ENDIF
      ?? ":nRowPos=", oBrw:nRowPos
   ENDIF
   ?? "nMetod=",nMetod

   // удаление/восстановление записи разрешена !!!
   // встроенный метод для удаления текущей записи
   lChange := oBrw:DeleteRow(.F., .T.)

   IF lChange                              // изменение было
      ? " -Del- " + ProcNL(), "lChange="+cValToChar(lChange), "переход! новая запись!"
      ?? "-> nMetod=" + HB_NtoS(nMetod)
      IF nMetod == 1        // это последняя запись в базе и таблице
         IF oBrw:lIsArr                   // для массива
            oBrw:Refresh(.T., .T.)
            nRec := oBrw:nLen
            oBrw:GoPos(nRec, nCell)
            ?? "переход :GoPos(:nLen=", nRec
         ELSEIF oBrw:lIsDbf               // для dbf
            (oBrw:cAlias)->( dbSkip(0) )
            oBrw:Reset()
            oBrw:Refresh(.T., .T.)
            oBrw:GoBottom()               // на последнюю запись
            nRec   := oBrw:nRowPos        // номер записи в таблице
            nRecno := (oBrw:cAlias)->( RecNo() )
            oBrw:GoToRec( nRecno )
            DO EVENTS
            ?? "переход :GoToRec()=", nRecno, ":nRowPos=",nRec
         ENDIF
      ELSE
         IF nAt == 1
            oBrw:Reset()
            oBrw:Refresh()
            nRecno += 1
         ENDIF
         oBrw:GoToRec( nRecno )
         ?? "GoToRec()=", nRecno
      ENDIF

      oBrw:DrawFooters()   // перересуем подвал
      DO EVENTS
      //запись в журнал-действий-пользователей-программы
      //write to the program-user-actions-log
   ELSE
      ?? "отмена удаления", lChange
   ENDIF

   DO EVENTS
   ? " -Del-  .end"

RETURN Nil

//////////////////////////////////////////////////////////////////////////
// удалённые записи в таблице можно восстановить
//                        только сразу не выходя из этой таблицы !!!
// В дальнейшем удаленная запись в самом файле-sqlite помечатется как
//  свободная запись и НЕ ПОДЛЕЖИТ восстановлению !!!
// только через спец.утилиты можно и не всегда восстанавить содержимое
STATIC FUNCTION ListDelete(oBrw)
   LOCAL owc, cTtl, cMsg, cLng

   ? " -DelList- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTtl := 'Удаление списка записей'
      cMsg := "ВНИМАНИЕ !;Удалить текущий СПИСОК записей в таблице ?;;"
      cLng := "ОТКАЗ !;Удалять можно ТОЛЬКО записи по фильтру !;;"
   ELSE
      cTtl := 'Delete list of records'
      cMsg := "ATTENTION!;Delete the current LIST of records in the table?;;"
      cLng := "REFUSAL!;You can delete ONLY records by filter!;;"
   ENDIF

   owc := oBrw:Cargo:owc             // Cargo окна
   IF owc:nFilter == 1               // надпись Фильтр
      AlertStop( cLng, , , 64, {RED} )
      RETURN NIL
   ENDIF

   IF AlertYesNo(cMsg, cTtl, , "iQuestion64", 64, {ORANGE,RED} )

   ENDIF

   DO EVENTS
   ? " -DelList-  .end"

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, aRet
   LOCAL cTyp, cMsg, xRet, lWrt, cStr

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // тип обработки колонки
      cNam  := oCol:cName
   END WITH
   oCol:nClrEditBack := CLR_GRAY
   ? SPACE(5) + ProcNL(), nCol, cTyp

   uOld := uVal
   lWrt := .T.       // записать ячейку
   lRet := .T.       // давать редактировать поле в :get
   aRet := {uVal}    // поместим в массив - то что пришло
   cMsg := ''
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += 'Тип поля колонки: "' + cTyp + '" ;'
      cStr += 'НЕТ обработки для этого поля !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   ? ProcNL(), cTyp, uVal, VALTYPE(uVal)
   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_RED
      oCol:nClrEditBack := CLR_GRAY
   ELSEIF cTyp $ "CMV"
      // доп.проверка
      IF !IsString(uVal)
         uVal := cValToChar(uVal)
      ENDIF
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // пример для своей функции
      IF AT(CRLF,uVal) > 0           // если в поле "C" есть CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // не давать редактировать поле в :get
      ELSEIF cTyp $ "MV"
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                // не давать редактировать поле в :get
      ENDIF
   ELSEIF cTyp $ "=@T" .OR. cTyp $ "D"
      aRet := CellEdit_DT(oBrw, cTyp, uVal)
      lRet := .F.             // не давать редактировать поле в :get
   ELSE
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_RED
      ? SPACE(5) + ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      //cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      //AlertInfo(cMsg + cStr)
      //lWrt := .F.             // не записывать в ячейку
      //lRet := .F.             // не давать редактировать поле в :get
   ENDIF

   IF lWrt                                         // записывать ячейку
      //IF (oBrw:cAlias)->(RLock())                  // делать самому
         // !!! всегда массив, если пустой, то это ОТКАЗ от ввода
         IF LEN(aRet) > 0
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // счётчик-изменения в таблице
            xRet := aRet[1]
            oBrw:SetValue(nCol,xRet)
            // !!! oBrw:cAlias - SQLMIX/0 Операция не поддерживается
            //(oBrw:cAlias)->KOPERAT  := 555       // кто правил запись
            //(oBrw:cAlias)->DATEVVOD := DATE()    // дата правки
            //(oBrw:cAlias)->TIMEVVOD := 9999      // время правки
            //(oBrw:cAlias)->( DbUnlock() )
            //(oBrw:cAlias)->( DbCommit() )
         ENDIF
      //ELSE
      //   cMsg := "Recording is locked !; Recno="
      //   cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
      //   AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      //ENDIF
   ENDIF
   oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы
   oBrw:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, oBrw )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod
   LOCAL oWnd  := _WindowObj(oBrw:cParentWnd)
   LOCAL aItog := oWnd:Cargo:aItogo
   LOCAL cTyp, cMsg, cStr
   LOCAL cTable  := ATREPL( "_", oBrw:cAlias, " " )
   LOCAL cUpdateQuery := "UPDATE " + cTable + " SET "
   LOCAL i, ni, cWhere := " WHERE "
   LOCAL aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec}

   WITH OBJECT oBrw
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // тип обработки колонки
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
   END WITH

   ? SPACE(5) + ProcNL(), nCol, cTyp
   cStr := 'oCol:bEditPost !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += 'Тип поля колонки: "' + cTyp + '" ;'
      cStr += 'НЕТ обработки для этого поля !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   IF cTyp $ "CNDLIV"
      // стандартная обработка
      FOR i := 1 TO Len( aStruct )
         IF lMod
            cUpdateQuery += aStruct[ i ][ 1 ] + "=" + ClipValue2SQL( FieldGet( i ) ) + ","
         ENDIF
      NEXT
      // no Change
      IF Right( cUpdateQuery, 4 ) == "SET "
         RETURN .F.
      ENDIF
      // remove last comma
      cUpdateQuery := Left( cUpdateQuery, Len( cUpdateQuery ) - 1 )
         FOR nI := 1 TO 1
            cWhere += aStruct[ nI ][ 1 ] + "="
            // use original value
            cWhere += ClipValue2SQL( (cAls)->( FieldGet( nI ) ) )
            cWhere += " AND "
         NEXT
         // remove last " AND "
         cWhere := Left( cWhere, Len( cWhere ) - 5 )
         cUpdateQuery += cWhere
                             ? " -Upd-  :bPostEdit" + ProcNL()
                             ?? "cUpdateQuery=", cUpdateQuery
                             IF rddInfo( RDDI_EXECUTE, cUpdateQuery )
                             EndIf
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN .T.

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle //, aStruct

   //aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
   IF App.Cargo:cLang == "RU"
      cTitle := 'Инфо по списку колонок'
   ELSE
      cTitle := 'Info on the list of columns'
   ENDIF
   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      cMsg  += HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT
   cMsg += ";"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := cValToChar( oCol:cPicture )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = "
      cMsg  += ' "'+ cFld + '"  ;'
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight - 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH

   ? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet)

   oJWnd := oBrw:Cargo:ObjWnd        // текущее окно
   cForm := oJWnd:Name
   nY    += oJWnd:Row
   nX    += oJWnd:Col + 7
   IF oBrw:lDrawSpecHd
      nY -= oBrw:nHeightSpecHd    // высота спецхидера ENUMERATOR
   ENDIF

   nY     += IIF( App.Cargo:aDisplayMode[2] <= 720, 8, 4 )
   nHCell += IIF( App.Cargo:aDisplayMode[2] <= 720, 3, 0 )
   //nY     += IIF( Sys.ClientHeight <= 720, 8, 4 )
   //nHCell += IIF( Sys.ClientHeight <= 720, 3, 0 )

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]

   nHObj  := nHCell - 7 //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4       // две кнопки
   nW     := nWDate + nWBtn
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода

   // выход за границы экрана/прижимаем к правому концу ячейки
   IF nX + nW > App.Cargo:aDisplayMode[2] //Sys.ClientWidth
      nX := (nWCell + nX) - nW
   ENDIF
   nH := nHCell

   // новое окно в ячейку таблицы
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH  ;
      MODAL NOCAPTION                              ;
      FONT cFont SIZE nFSize                       ;
      ON LOSTFOCUS {|| oWnd:Release() }            ;
      ON INIT      {|| DoEvents() }

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle

      IF cType == "D"

         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := { This.Date_1.Value } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         //tDTime := HB_STRTOTS( xGet )
         tDTime := xGet
         IF tDTime == hb_CToT("")
            tDTime := hb_DateTime()
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value  ,;
                      aRet   := { tDTime } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ENDIF

       DRAW LINE IN WINDOW Cell AT 2, 2 TO 2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT nH-2, 2 TO nH-2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, 2 TO nH, 2 PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, nW-2 TO nH, nW-2 PENCOLOR RED PENWIDTH 4

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } )
   Cell.Activate

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////
