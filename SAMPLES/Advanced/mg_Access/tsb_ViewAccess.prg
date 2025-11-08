/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Окно с таблицей / Window with table
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"

///////////////////////////////////////////////////////////////////
FUNCTION Tsb_ViewAccess(oWnd, nPos, cWIco, cTtl, lWin1)
   LOCAL cForm, hForm, aBColor, cTitle, cMsg, ao
   LOCAL oBrw, cBrw, oTsb, nY, nX, nH, nW, nG, o, owc
   LOCAL nH1, nW2, a4Clr, nDelta

   ? ProcNL(), oWnd, nPos, cWIco, cTtl, lWin1

   ao       := App.Cargo
   nY       := App.Cargo:nHMain      // высота окна главной формы
   nX       := 0  ; nG := 20
   nW       := Sys.ClientWidth
   nH       := Sys.ClientHeight - nY
   cTitle   := HB_NtoS(nPos) + ":" + cTtl
   cForm    := "Form_Tsb" + HB_NtoS(nPos)
   a4Clr    := App.Cargo:a4Clr             // цвета для окон таблицы
   aBColor  := a4Clr[1]
   cBrw     := "Tsb_" + HB_NtoS(nPos)
   nDelta   := 40

   IF !lWin1   // для всех окон
      nY += (nPos - 1) * nDelta
      nX += (nPos - 1) * nDelta
   ENDIF

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

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE cTitle ICON cWIco                        ;
      MINWIDTH 500 MINHEIGHT 500                     ; // блокировка уменьшения размеров окна
      WINDOWTYPE STANDARD TOPMOST                    ;
      ON MAXIMIZE ( ResizeForm( This.Object ) )      ;
      ON SIZE     ( ResizeForm( This.Object ) )      ;
      BACKCOLOR aBColor                              ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor   // цвет окна
      owc:oMainCrg := oWnd:Cargo       // Cargo родительское окно
      owc:nG       := nG
      owc:cAls     := ALIAS()          // алиас в этом окне
      owc:ahIcoDel := {}               // для удаления хендлов иконок с формы
      owc:cFile    := ""

      // верхнее меню окна с кнопками
      TopMenuViewButtons(owc)          // -> tsb_ViewMenu.prg
      ? ProcNL(), "owc:ahIcoDel=",owc:ahIcoDel, HB_ValToExp(owc:ahIcoDel)
      nY  := owc:nHTBar //+ nG
      nX  := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nH1 := 5
      nW2 := owc:nWEndTB    // конец кнопок

      @ nH1, nW2 + nG LABEL Lbl_1 VALUE owc:cAls AUTOSIZE FONTCOLOR WHITE TRANSPARENT
      nH1 += This.Lbl_1.Height + 1

      owc:cRus := "F2-инфо,   Ins-новая запись, Del-удалить запись"
      owc:cEng := "F2-info,   Ins-new recno, Del-delete recno"
      owc:cMsg := IIF( ao:cLang == "RU", owc:cRus, owc:cEng)
      @ nH1, nW2 + nG LABEL Lbl_2 VALUE owc:cMsg AUTOSIZE FONTCOLOR WHITE TRANSPARENT

      This.MinWidth  := owc:nWEndTB + nG + GetBorderWidth()*2  // блокировка уменьшения размеров окна
      //This.MinHeight := owc:nHBtnEnd + GetBorderHeight()*2   // блокировка уменьшения размеров окна

      /////////////////////// таблица ///////////////////////////////////////////////////
      oTsb := oHmgData()
      oTsb:cForm := cForm
      // координаты таблицы
      oTsb:nY    := nY
      oTsb:nX    := nG
      oTsb:nW    := nW - oTsb:nX * 2
      oTsb:nH    := nH - oTsb:nY - nG

      @ oTsb:nY, oTsb:nX LABEL Label_Table PARENT &cForm WIDTH oTsb:nW HEIGHT oTsb:nH ;
        VALUE '' SIZE 20 CENTERALIGN BACKCOLOR WHITE INVISIBLE
      owc:cLabel := 'Label_Table'

      oBrw := Draw_RECORDSET( oTsb, cBrw, oWnd, owc, nPos )         // таблица
      IF IsObject(oBrw)
         oBrw:Cargo:aFont := oTsb:aFont  // запомнили на окне
         owc:oBrw  := oBrw               // запомнили на окне
         owc:cBrw  := cBrw               // запомнили на окне
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
                                             _SetThisFormInfo(ow)      ,;
                                             ob:PostMsg( WM_KEYDOWN, VK_F2, 0 )  ,; // инфо по списку колонок
                                             _SetThisFormInfo()        ,;
                                             This.&(cn).Enabled := .T. ,;
                                             ky:=cn , ob:Setfocus()  } )

      o:Event({11,"_AExport"}, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
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
                                 ow:Cargo:cAls:Close()    // ОБЯЗАТЕЛЬНО !!!
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

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_RECORDSET( oTsb, cBrw, oWnd, owc, nPos )
   LOCAL oBrw, cForm, a4Clr, aTable, cTable, cSuperHd, oRSet, aRSet, nG
   LOCAL cMsg, oErr, cErr, cLng

   nG         := owc:nG
   //--------------- работа с таблицей Access  ------------
   aTable     := oWnd:Cargo:aTable    // запомнили ранее/remembered earlier
   cTable     := aTable[nPos]
   aRSet      := oWnd:Cargo:aRSet     // запомнили ранее/remembered earlier
   oRSet      := aRSet[nPos]

   BEGIN SEQUENCE WITH { |e|break(e) }
       oRSet:Open()
       oRSet:Sort := oRSet:Fields(0):Name
       owc:cAls   := oRSet                          // ОБЯЗАТЕЛЬНО запомним
   RECOVER USING oErr
      // обработка возникшей ошибки, данные о ней в oErr
      cLng := IIF( App.Cargo:cLang == "RU", "Это может быть не TABLE а VIEW-формы показа !",;
                     "This may not be a TABLE but a VIEW display form !")
      ? ProcNL(), oErr
      ? REPL(".",5), oErr:description, oErr:operation, oErr:genCode
      cErr := cTable + ';;' + oErr:description
      cErr += if(!Empty(oErr:operation),';'+oErr:operation,'')
      cErr += ' (' + HB_NtoS(oErr:genCode) + ');;' + cLng
      //AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      ? cErr
      cErr     := ATREPL( ";", cErr, CRLF )
      owc:cAls := ""                                // ОБЯЗАТЕЛЬНО запомним
      RETURN cErr
   END SEQUENCE
   owc:cFile  := oWnd:Cargo:cFile + "-" + cTable    // запомним для экспорта

   IF oRSet:RecordCount() == 0
      cMsg := "Table: " + cTable
      cMsg += ";There are no recno in the table !;"
      cMsg += "oRSet:RecordCount() = 0"
      //AlertStop(cMsg, , , 64, {RED})
      ? cMsg
      cMsg := ATREPL( ";", cMsg, CRLF )
      RETURN cMsg
   ENDIF

   cSuperHd   := "FILE: " + cFileNoPath(oWnd:Cargo:cFile)
   cSuperHd   += "   TABLE: " + cTable
   //                      cell     Head   foot    SpecHider  SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Bold", "SpecHdr" , "ItalBold", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }
   oTsb:uSelector   := 20
   oTsb:lSpecHd     := .T.    // поставить в таблице нумератор
   oTsb:lFooting    := .T.    // поставить в таблице подвал
   oTsb:aFoot       := .T.
   oTsb:nHeightFoot := 25     // высота подвала
   oTsb:nHeightHead := 25     // высота шапки
   oTsb:aEdit       := .T.    // редактировать колонки
   a4Clr            := App.Cargo:a4Clr                // цвета для окон таблицы
   oTsb:a4Clr       := a4Clr                          // сохраним 4 цвета таблицы
   oTsb:aBrush      := a4Clr[3]                       // цвет фона под таблицей
   oTsb:aColor      := Color_Tsb(a4Clr,oTsb)          // цвета таблицы: 2(шапка+подвал),3(строка %1),4(строка %2)
   oTsb:cTtlSupHead := cSuperHd
   cForm            := oTsb:cForm                     // определено выше
   /*
    // COLNUMBER oTsb:aNumber                         ; // слева таблицы виртуальная колонка с нумерацией
    Виртуальной колонки COLNUMBER в этом TBROWSE - НЕТ !
    RECORDSET oRSet работает по своей выборке, причем
    имеет свой доступ к oRSet, который прописывается в методе
    METHOD SetRecordSet( oRSet ) CLASS TSBrowse
    При этом надо учитывать, что выборка ВСЯ в памяти,
    т.е. есть ограничения на память PC конкретного, т.е. с sql
    серверами по запросам SELECT... работают небольшими порциями, т.е. это означает
    для ТСБ, что нет VScroll-а (нужно отключать его)
   */
   // строим таблицу по заданным массивам
   // COLNUMBER oTsb:aNumber - НЕТ для RECORDSET !!!           ; // слева таблицы виртуальная колонка с нумерацией
   DEFINE TBROWSE &cBrw OBJ oBrw OF &cForm RECORDSET oRSet     ;
      AT oTsb:nY, oTsb:nX  WIDTH oTsb:nW HEIGHT oTsb:nH CELL   ;
      FONT   oTsb:aFont                                        ; // все фонты для таблицы
      COLORS oTsb:aColor                                       ; // цвета таблицы
      BRUSH  oTsb:aClrBrush                                    ; // цвет фона под таблицей
      EDITABLE                                                 ; // редактировать таблицу
      ENUMERATOR                                               ; // нумерация колонок
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
                 oB:aEditCellAdjust[1] := -3, ;  // correction of cell row
                 ob:lMoveCols     := .F., ;
                 ob:nMemoHV       :=  1 , ;     // показ одной строки мемо-поля
                 ob:nLineStyle := LINES_ALL ,;
                 ob:nClrLine   := COLOR_GRID,;
                 ob:lCheckBoxAllReturn := .T. }

      :Cargo:cTable  := cTable
      :Cargo:nModify := 0                           // изменения в таблице
      :Cargo:aFont   := oTsb:aFont                  // запомним фонты
      :Cargo:aSupHd  := oTsb:aSupHd                 // суперхидер таблицы
      :Cargo:aIconDel:= oTsb:aIconDel               // удалить значение
      :Cargo:lRecINS := .F.                         // блокировка клавиши INS
      :Cargo:lRecDEL := .F.                         // блокировка клавиши DEL
      :Cargo:aStruct := {}                          // структура базы для экспорта
      :Cargo:oRSet   := oWnd:Cargo:aRSet[nPos]      // данные этой таблицы

      myTsbInit(oBrw,oTsb,oRSet)      // настройки таблицы
      myTsbFont(oBrw,oTsb)            // фонты в таблице
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
FUNCTION Color_Tsb(aClr,oTsb)             // цвета таблицы
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
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( ob:oRSet:AbsolutePosition % 2 == 0, nPane2, nPane )  } } )    // 2 , фона в ячейках таблицы

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

//////////////////////////////////////////////////////////////////
FUNCTION myTsbInit( oBrw, oTsb, oRSet )  // настройки таблицы
   Local nHImg, nI, cCol, oCol, n, oDlu, nLen0, nLen2, cVal
   Local oFld, cFld, nTyp, cTyp, nLen, nDec, xVal, cFldEng

   ? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName

   // подгоним размеры колонок по фонту
   oDlu := _Font2oDlu( oTsb:aFont[1] )
   n    := oDlu:nSize

   //!!! варианты задания размера
   ? SPACE(5) + _HMG_DefaultFontName, _HMG_DefaultFontSize, "n=", n, oTsb:aFont[1]
   ? SPACE(5) + "!!!",n," oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   _o2log(oRSet , 15, ProcNL()+" -------------- Параметры объекта : => oRSet", .T.)

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      //oCol:cHeading := aRusHead[nI]                    // если нужно присвоим своё название колонки
      ? nI

      // структура для ТСБ
      oFld := oRSet:Fields( nI-1 )
      ?? "oFld=", oFld

      cFld := oFld:Name
      nTyp := oFld:Type
      //cTyp := o:Do( 'cHbType', nTyp )
      cTyp := ClipperFieldType( nTyp )
      xVal := oFld:Value
      nLen := oFld:DefinedSize
      nDec := 0
      ?? cFld, "nTyp=",nTyp, "cTyp=", cTyp, nLen, nDec, '[',xVal,']'
      nLen := IIF( cTyp == "N", nLen + 2, nLen )

      // присвоим структуру колонки, т.к. в RECORDSET нет этого
      oCol:cField     := cFld
      oCol:cFieldTyp  := cTyp
      //oCol:cFieldLen  := nLen                     // нет такой переменной в RECORDSET
      //oCol:cFieldDec  := nDec                     // нет такой переменной в RECORDSET

      oCol:cName := oCol:cHeading                   // присвоим имя колонки, т.к. в RECORDSET нет этого
      cCol       := oCol:cName
      oCol:lEdit := .T.                             // не работает EDITABLE при создании таблицы

      cFldEng := myField10(UPPER(cFld))             // привести к англ.буквам для экспорта в dbf
      AADD( oBrw:Cargo:aStruct , {cFldEng, cTyp, nLen, nDec} )  // структура базы для экспорта
      ?? oCol:cField, oCol:cFieldTyp, "{", cFldEng, cTyp, nLen, nDec, "}"

      IF cCol == "SELECTOR"
         oCol:cFooting := "->"                      // присвоим подвал колонки - не работает
      ELSE
         nLen0 := nLen + 2                          // +2 учитываем добавку для отступа
         nLen0 := iif( nLen0 > 40, 40, nLen0 )
         nLen2 := LEN(oCol:cHeading) + 2
         ?? nLen0, nLen2
         nLen0 := MAX(nLen0,nLen2)
         oCol:nWidth := oDlu:TextWidth(Repl("9",nLen0))
         ?? "=", oCol:nWidth
         IF LEN(cFld) > 10   // больше 10 знаков
            cFld := SUBSTR( cFld, 1, 10 )
         ENDIF
         oCol:cFooting := cFldEng        // присвоим подвал колонки - поля БД для экспорта dbf
         cVal := cFldEng + CRLF + oCol:cFieldTyp + '('
         //cVal += HB_NtoS(oCol:nFieldLen) + ','
         //cVal += HB_NtoS(oCol:nFieldDec) + ')'
         //oCol:cFooting := cVal
         oCol:nFAlign  := DT_CENTER
      ENDIF
   NEXT

   nHImg := oDlu:H1 + 6              // высота картинки = высота строк в ТСБ
   //                ^^^ - константа
   nHImg := oDlu:H(1.25)             // так правильнее, от размера фонта высота
   //              ^^^^  - пропорция от размера фонта

   WITH OBJECT oBrw
      :Cargo:nModify := 0     // изменения в таблице

      :lNoKeyChar    := .F.          // НЕТ ввода в ячейки от букв, цифр
      :nHeightCell   := nHImg        // высота ячеек = высоте картинки
      :nHeightHead   := nHImg * 1.2  // высота шапки
      :nHeightFoot   := nHImg        // высота подвала
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
      // для фонта MONO - DejaVu Sans Mono делаем добавку
      IF oCol:cFieldTyp == "C"
         //oCol:cPicture := Nil
         //oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99"
         //oCol:bDecode  := {|tval| hb_TtoS(tval) }
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(25)
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(10)
      ELSEIF oCol:cFieldTyp == "M"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
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
FUNCTION myTsbFont( oBrw )
   LOCAL hFont, oCol

   //hFont := oBrw:aColumns[1]:hFontSpcHd     // 4-special header font
   hFont := GetFontHandle("SpecHdr")
   // установить фонт для 1 колонки таблицы виртуальная колонка - здесь нет такой
   //oBrw:aColumns[1]:hFont     := hFont      // 1-cells font
   //oBrw:aColumns[1]:hFontFoot := hFont      // 3-footer font

   // установить фонт для всех колонок таблицы
   // спецхидера - ENUMERATOR (нумерация колонок)
   FOR EACH oCol IN oBrw:aColumns
      oCol:hFontSpcHd := hFont
   NEXT

RETURN Nil

//////////////////////////////////////////////////////////////////
FUNCTION myTsbSuperHd( oBrw, oTsb )
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
   o:ColorSupHd := nClr16                     // 16, фона спецхидер

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// другие настройки тсб
FUNCTION myTsbKeyFX( oBrw, oTsb )
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
      :UserKeys(VK_F8    , {|ob| myTsbSelectorNew(ob) , ob:Setfocus() })  //
      :UserKeys(VK_F9    , {|ob| myTsbSelectorOld(ob) , ob:Setfocus() })  //

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
FUNCTION myTsbEdit( oBrw )
   LOCAL oCol

   FOR EACH oCol IN oBrw:aColumns
      IF oCol:cName == "SELECTOR" .OR. oCol:cName == "ORDKEYNO"  ; LOOP
      ENDIF
      IF oCol:cFieldTyp $ "+=@T"
         oCol:lEdit := .F.
      ENDIF
      //IF "NAME" $ oCol:cName
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> tsb_view_func.prg
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> tsb_view_func.prg
      //ENDIF
      //? hb_enumindex(oCol), oCol:cName, oCol:bPrevEdit, oCol:bPostEdit, oCol:lIndexCol, oCol:cOrder
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// блок-кода который ДЕЛАЕМ ПОСЛЕ END TBROWSE
FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, oCol, a4Clr, nTest, nLen, hFont, nWCol, nCol

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

   // изменение виртуальной колонки - здесь НЕТ такой колонки
   nLen  := LEN(HB_NtoS(oBrw:nLen))
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
   ENDIF
     
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// новая запись в базе добавляется в конец базы и переходим сразу к редактированию
FUNCTION RecnoInsert(oBrw)
   LOCAL cMsg, cMsg2, aTmp, aBColor, aFColor, aColors, cTitle, oRst

   ? " -Ins- "+ProcNL(), oBrw:ClassName

   // сделано ранее :Cargo:oRSet := oWnd:Cargo:aRSet[nPos]
   oRst := oBrw:Cargo:oRSet          // берем ранее положенный oRSet

   IF App.Cargo:cLang == "RU"
      cTitle := 'Добавление записи'
      cMsg   := "ВНИМАНИЕ !;Вставить запись в таблицу ?"
      cMsg2  := "Запись добавлена в таблицу !;;"
      cMsg2  += "Неоходимо закрыть таблицу и открыть её заново !;;"
      cMsg2  += "Ограничение метода AppendRow() Tsbrowse для работы с RECORDSET;"
   ELSE
      cTitle := 'Adding recno'
      cMsg   := "ATTENTION!;Insert a record into the table ? "
      cMsg2  := "Record added to table!;;"
      cMsg2  += "Need to close table and reopen it!;;"
      cMsg2  += "Limitation of Tsbrowse AppendRow() method for working with RECORDSET;"
   ENDIF

   aColors := { {45,223,70} , ORANGE }
   aBColor := { 238, 249, 142 }   // светло-жёлтый
   aFColor := BLACK
   aTmp    := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   IF AlertYesNo( cMsg, cTitle, , , 64, aColors )
      // срабатывает сразу при добавлении записи
      // добавить в поле дату+время вставки записи
      oBrw:bAddAfter := {|ob|
                          Local oConn := App.Cargo:oConx
                          Local cTable := ob:Cargo:cTable
                          LOCAL cInsertQuery := "INSERT INTO " + cTable + " DEFAULT VALUES"
                             ? "+++ cInsertQuery", cInsertQuery
                          //If ladd
                             ? "+++ :bAddAfter",ProcNL(), cTable
                             oConn:Execute( cInsertQuery )
                          //EndIf
                          Return Nil
                         }

      // oBrw:bAddAfter  := Nil  // это если не нужен код заполнения полей при создании новой записи

      // встроенный метод для добавления записи
      oBrw:AppendRow()

      //nRecno := (oBrw:cAlias)->( RecNo() )
      ? "+++ " + ProcNL(), hb_DateTime(), "Insert!" //, "RecNo()=", nRecno
/*
         oRSt := TOleAuto():New( "ADODB.RecordSet" )
         With Object oRSt
            :CursorLocation   := adUseClient
            :CursorType       := adOpenDynamic
            :LockType         := adLockOptimistic
            :ActiveConnection := App.Cargo:oConx
            :Source           := "SELECT * FROM " + oBrw:Cargo:cTable
         End With

      oBrw:SetRecordset(oRSt)
*/
      AlertInfo( cMsg2, cTitle, "iAccess64", 64, { ORANGE } )

      oBrw:nCell := 1 // в начало колонок для редактирования
      oBrw:Reset()
      oBrw:GoBottom()     // всегда на новую запись, если нет индекса
      oBrw:Setfocus()
      DO EVENTS

   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])      // восстановить цвета

RETURN Nil

////////////////////////////////////////////////////////////////////////////
FUNCTION RecnoDelete(oBrw)
   LOCAL lChange, nAt, lDelete, nRecno, nCell, nMetod, nRec, cMsg2, cTitle

   ? " -Del- "+ProcNL(), oBrw:ClassName
   ?? ":nLen=", oBrw:nLen //,":lIsXXX=", oBrw:lIsDbf, oBrw:lIsArr
   ?? ":nRowPos=", oBrw:nRowPos

   // срабатывает сразу при удалении записи
   oBrw:bDelAfter := {|nr,ob| // for Access
                              Local oConn := App.Cargo:oConx
                              Local cTable := ob:Cargo:cTable
                              LOCAL cDeleteQuery := "DELETE FROM " + cTable
                              LOCAL oRSet, cWhere := " WHERE "

                              oRSet  := oBrw:Cargo:oRSet          // берем ранее положенный oRSet
                              cWhere += oRSet:Fields(0):Name + "="
                              // use original value
                              cWhere += ClipValue2SQL( oRSet:Fields(0):Value )

                              cDeleteQuery += cWhere
                              ? " -Del-  :bDelAfter" + ProcNL()
                              ?? "cDeleteQuery=", cDeleteQuery
                              oConn:Execute( cDeleteQuery )
                              Return nr
                              }

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

   IF App.Cargo:cLang == "RU"
      cTitle := 'Удаление записи'
      cMsg2  := "Запись удалена из таблицы !;;"
      cMsg2  += "Неоходимо закрыть таблицу и открыть её заново !;;"
      cMsg2  += "Ограничение метода DeleteRow() Tsbrowse для работы с RECORDSET;"
   ELSE
      cTitle := 'Record deletion'
      cMsg2  := "Record deleted from table!;;"
      cMsg2  += "Need to close table and reopen it!;;"
      cMsg2  += "Limitation of DeleteRow() Tsbrowse method for working with RECORDSET;"
   ENDIF

   IF lChange                              // изменение было
      AlertInfo( cMsg2, cTitle, ,"iAccess64", 64, { ORANGE } )
   ENDIF
 
   oBrw:Reset()
   oBrw:Setfocus()
   DO EVENTS
   ? " -Del-  .end"

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle, aStruct

   aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
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
      //cMsg  += HB_NtoS(oCol:nFieldLen)                  // нет такой переменной в RECORDSET
      //cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'   // нет такой переменной в RECORDSET
      IF nCol > 1
         cMsg  += HB_NtoS(aStruct[nCol-1,3]) + ','
         cMsg  += HB_NtoS(aStruct[nCol-1,4]) + ' ) ;'
      ELSE
         cMsg  += ' ) ;'
      ENDIF
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

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListFont( oBrw )
   LOCAL cMsg, cTitle, cMsg1, cMsg2, cFnt, hFnt, aFnt, aFonts, n

   IF App.Cargo:cLang == "RU"
      cTitle := 'Инфо по фонтам'
      cMsg1  := 'Фонты в таблице'
      cMsg2  := 'Все фонты в программе'
   ELSE
      cTitle := 'Info on fonts'
      cMsg1  := 'Fonts in table'
      cMsg2  := 'All fonts in the program'
   ENDIF
   cMsg   := ";" + cMsg1 + ";;"
   cMsg   += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg   += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg   += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg   += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg   += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg   += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";;"

   cMsg   += ";" + cMsg2 + ";;"
   aFonts := {}
   FOR n := 1 TO Len( _HMG_aControlNames )
      IF _HMG_aControlType[ n ] == "FONT"
         AAdd( aFonts, { _HMG_aControlNames[ n ], _HMG_aControlHandles[ n ] } )
      ENDIF
   NEXT

   FOR EACH aFnt IN aFonts
       cFnt := aFnt[1]
       hFnt := aFnt[2]
       cMsg += "  " + hb_NtoS(hb_enumindex(aFnt)) + ". " + ;       /*+ cFnt + " : "*/
               hb_valtoexp( GetFontParam( hFnt ) ) + CRLF
   NEXT
   cMsg += REPL("; ",10)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbSelectorNew( oBrw )
   LOCAL cMsg, cTitle, nTest, oCol

   IF App.Cargo:cLang == "RU"
      cTitle := ''
      cMsg   := 'Смена цвета фона колонки SELECTOR !;;'
      cMsg   += "Успех операции !;;" + ProcNL()
   ELSE
      cTitle := ''
      cMsg   := 'Change the background color of the SELECTOR column!;;'
      cMsg   += "Operation success!;;" + ProcNL()
   ENDIF

   oBrw:GetColumn("SELECTOR"):SaveColor()

   // другой вариант раскрасски
   nTest := CLR_YELLOW
   oBrw:nClrSelectorHdBack := nTest           // Footer для "SELECTOR"

   oCol := oBrw:GetColumn("SELECTOR")
   oCol:nClrHeadBack  := nTest
   oCol:nClrBack      := nTest
   oCol:nClrFootBack  := nTest
   oCol:nClrSpcHdBack := nTest

   oBrw:RefResh()
   DO EVENTS

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbSelectorOld( oBrw )
   LOCAL cMsg, cTitle, oCol

   IF App.Cargo:cLang == "RU"
      cTitle := ''
      cMsg   := 'Смена цвета фона колонки SELECTOR !;;'
      cMsg   += "Успех операции !;;" + ProcNL()
   ELSE
      cTitle := ''
      cMsg   := 'Change the background color of the SELECTOR column!;;'
      cMsg   += "Operation success!;;" + ProcNL()
   ENDIF

   oCol := oBrw:GetColumn("SELECTOR")
   oCol:RestColor()

   oBrw:RefResh()
   DO EVENTS

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet
   LOCAL cTyp, cMsg, cRet, lWrtUDT, cStr, oRst

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // тип обработки колонки
      cNam  := oCol:cName
      // сделано ранее :Cargo:oRSet := oWnd:Cargo:aRSet[nPos]
      oRst  := :Cargo:oRSet          // берем ранее положенный oRSet
   END WITH
   uOld := uVal
   ? SPACE(5) + ProcNL(), nCol, cTyp  //, (cAls)->(IndexOrd()), (cAls)->(OrdSetFocus()), oBrw:uLastTag
   lWrtUDT := .F.                        // не записывать User+Date+Time
   lRet    := .T.                        // давать редактировать поле в :get
   cStr    := 'oCol:bPrevEdit !;'
   cStr    += 'Тип обработки колонки/Column processing type: "' + cTyp + '" '
   cStr    += 'oCol:cName = ' + cNam
   ? cStr
   IF cTyp $ "NDL"
      IF Valtype(uOld) == "U"
         IF cTyp == "N"
            lRet := {0, 0}
         ELSEIF cTyp == "D"
            lRet := {ctod(''), ctod('')}
         ELSEIF cTyp == "L"
            lRet := {.F., .F.}
         ENDIF
      ENDIF
      // стандартная обработка
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF cTyp $ "CM"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      cRet := ""
      IF Valtype(uVal) == "U"
            uVal := cRet
            lRet := {uVal, Space(20)}
      ENDIF
      IF AT(CRLF,uVal) > 0
         cRet    := CellEditMemo(uVal, oBrw)
         lWrtUDT := .T.     // записывать User+Date+Time
         lRet    := .F.     // не давать редактировать поле в :get
      ELSEIF cTyp == "M"
         cRet    := CellEditMemo(uVal, oBrw)
         lWrtUDT := .T.     // записывать User+Date+Time
         lRet    := .F.     // не давать редактировать поле в :get
      ENDIF
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      lRet    := .F.     // не давать редактировать поле в :get
   ENDIF

   IF lWrtUDT                                 // записывать User+Date+Time
      // Записи блокируются автоматом при пессимистической блокировке при начале редактирования,
      // при оптимистической при обновлении.
      //IF (oBrw:cAlias)->(RLock())             // делать самому

         IF LEN(cRet) > 0   // для ("C" + CRLF) и ("M")
            oBrw:SetValue(nCol,cRet)
         ENDIF
         //IF FIELDNUM("KOPERAT") > 0              // если есть такое поле
            //(oBrw:cAlias)->KOPERAT  := 555       // кто правил запись
            //(oBrw:cAlias)->DATEVVOD := DATE()    // дата правки
            //(oBrw:cAlias)->TIMEVVOD := 9999      // время правки
         //ENDIF
         //(oBrw:cAlias)->( DbUnlock() )
         //(oBrw:cAlias)->( DbCommit() )
         IF oBrw:nColumn("KOPERAT", .T.) > 0       // если есть такое поле
            oRst:Fileds("KOPERAT"):Value = 555     // кто правил запись
         ENDIF

      //ELSE
      //   cMsg := "Recording is locked !; Recno="
      //   cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
      //   AlertStop( cMsg )
      //ENDIF
   ENDIF
   oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbEditPost( uVal, oBrw )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod
   LOCAL oWnd  := _WindowObj(oBrw:cParentWnd)
   LOCAL aItog := oWnd:Cargo:aItogo
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT oBrw
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // тип обработки колонки
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
   END WITH

   ? SPACE(5) + ProcNL(), nCol, cTyp  //, (cAls)->(IndexOrd()), (cAls)->(OrdSetFocus()), oBrw:uLastTag
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'Тип обработки колонки/Column processing type: "' + cTyp + '" ;'
   cStr += 'oCol:cName = ' + cNam

   IF cTyp $ "CNDL"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      RETURN .F.
   ENDIF
   /*
   IF LEN(cRun) > 0
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
   ENDIF

   // для всех колонок итогов
   lSay := .F.
   ? "   uVal - oCol:xOldEditValue = ", uVal , oCol:xOldEditValue
   IF oCol:Cargo:lTotal .and. oCol:xOldEditValue != uVal
      ?? "oCol:Cargo:nTotal=",oCol:Cargo:nTotal
      oCol:Cargo:nTotal += uVal - oCol:xOldEditValue
      lSay := .T.
      ?? "=>", oCol:Cargo:nTotal
   ENDIF

   IF lSay ; _wPost("_ItogSay", oBrw:cParentWnd)
   ENDIF */
   DO EVENTS

RETURN .T.

//////////////////////////////////////////////////////////////////////////////////////////
// Преобразование имени поля
STATIC FUNCTION myField10(cStr)

    IF LEN(cStr) > 10
       cStr := SUBSTR(cStr,1,10)
    ENDIF

    IF IsRusChar(cStr)
       cStr := TranRusLat(cStr)
    ENDIF

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////////////
// Преобразование клавиши к верхнему регистру латинского алфавита
STATIC FUNCTION TranRusLat(cStr)
   cStr := UPPER(cStr)
   cStr := CharRepl('АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЬЭЮЯ ',cStr,;
                    "ABVGDEJZIIKLMNOPRSTUFXCCHHY_EUI_")
   cStr := CHARREM(",;.-",cStr)
Return cStr

//////////////////////////////////////////////////////////////////////////////////////////
// проверка на русские буквы
FUNCTION IsRusChar(cStr)
   LOCAL lRet := .F., nI, cBukva

   FOR nI := 1 TO LEN(cStr)
      cBukva := SUBSTR(cStr,nI,1)
      IF ASC(cBukva) > 127
         lRet := .T.
         EXIT
      ENDIF
   NEXT

RETURN lRet

//////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ClipperFieldType( nType )
   LOCAL cType := nType

   SWITCH nType
   CASE 129             // adChar
   CASE 130             // adWChar
   CASE 200             // adVarChar
   CASE 202             // adVarWChar
      cType := "C"
      EXIT
   CASE   2             // adSmallInt
   CASE   3             // adInteger
   CASE   4             // adSingle
   CASE   5             // adDouble
   CASE   6             // adCurrency
   CASE  14             // adDecimal
   CASE  17             // adUnsignedTinyInt
   CASE  16             // adTinyInt
   CASE  18             // adUnsignedSmallInt
   CASE  19             // adUnsignedInt
   CASE  20             // adBigInt
   CASE  21             // adUnsignedBigInt
   CASE 131             // adNumeric
   CASE 139             // adVarNumeric
   CASE 205             // я добавил ????????
      cType := "N"
      EXIT
   CASE  11             //
      cType := "L"
      EXIT
   CASE   7             // adDate
      cType := "T"
      EXIT
   CASE 133             // adDBDate
      cType := "D"
      EXIT
   CASE 203             // adLongVarWChar
      cType := "M"
      EXIT
   ENDSWITCH

RETURN cType

// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
FUNCTION ClipValue2SQL( Value )

   SWITCH ValType( Value )
   CASE "N"
      RETURN hb_ntos( Value )

   CASE "D"
      IF Empty( Value )
         RETURN "''"
      ELSE
         /* SQL dates are like YYYY-MM-DD */
         RETURN "'" + StrZero( Year( Value ), 4 ) + "-" + StrZero( Month( Value ), 2 ) + "-" + StrZero( Day( Value ), 2 ) + "'"
      ENDIF

   CASE "C"
   CASE "M"
      IF Empty( Value )
         RETURN "''"
      ELSE
         RETURN "'" + value + "'"
      ENDIF

   CASE "L"
      RETURN iif( Value, "1", "0" )

   CASE "U"
      RETURN "NULL"

   ENDSWITCH

   RETURN "''"       // NOTE: Here we lose values we cannot convert
