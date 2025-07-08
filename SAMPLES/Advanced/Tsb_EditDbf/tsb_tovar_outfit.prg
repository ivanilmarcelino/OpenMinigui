/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * _TBrowse() Разные функции для редактирований ячеек таблицы
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Tovar( nY, nX, nGBtn, nW, nWBtn, nHIco )
   LOCAL lIco, nHBtn, aBtnCap, nBtnLen, aFont2, nHBtn2, oBtn := oHmgData()
   LOCAL lRow := .T.  // кнопки по горизонтали
   DEFAULT nWBtn := 0, nHIco := 0

#ifdef KEY_ENG // for this project demo1-en.hbp
   aBtnCap := { "Save", "Cancel" }
#else
   aBtnCap := { "Сохранить", "Отменить" }
#endif

   IF nHIco == 0
      nHIco  := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана
      nHIco  := 48
   ENDIF
   lIco      := .T.  // растягивать размер иконки
   nHBtn     := nHIco + 15
   oBtn:aCap := aBtnCap
   oBtn:aObj := { "_3Save", "_3Cancel"  }
   nBtnLen   := LEN(aBtnCap)
   IF nWBtn == 0
      nWBtn  := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ширина кнопки
   ENDIF
   oBtn:aWBtn:= { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr := { { 35,179, 15} , {254, 73, 83} }
   oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(Х) - номер события на кнопке
   oBtn:aObj := { "_3Save", "_3Cancel"  }
   oBtn:aIco := { {"iFloppy48x1"   , "iFloppy48x2"  , lIco, nHIco} ,;
                  {"iReturn48x1"   , "iReturn48x2"  , lIco, nHIco}  }
   aFont2    := GetFontParam(GetFontHandle("ComSanMS"))
   oBtn:aFnt := { aFont2[1], aFont2[2], aFont2[3] }  // фонты для всех кнопок
   nHBtn2    := aFont2[2] * 1                        // 1 строка текста на кнопках
   nHBtn     := MAX(nHBtn,nHBtn2)                    // скорректируем высоту кнопки

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_Tovar_Outfit(oBrw1,cForm)
   LOCAL cPrnt, hWnd, aRet, nWBtn, nHIco, nHText, cIcon, cTitle, oBrw, oTsb
   LOCAL nY, nX, nW, nH, nG, aBClr, cFont, nFSize, nHUp, owc, nWText
   LOCAL cMsg, nY2, nX2, nW2, nH2, nWTsb, nHTsb, oGet, oWnd, cRetAls
   DEFAULT cForm := "Tsb_Win_Outfit"

   ? "======",ProcNL(),"oBrw1=",oBrw1:cAlias,oBrw1:cParentWnd,"cForm=",cForm
   ////////// позиция окна ТСБ
   cPrnt  := oBrw1:cParentWnd      // родительское окно
   oWnd   := _WindowObj(cPrnt)     // !!!
   hWnd   := GetFormHandle(cPrnt)
   nY     := GetWindowRow(hWnd)
   nX     := GetWindowCol(hWnd)
   nW     := GetWindowWidth(hWnd)
   nH     := GetWindowHeight(hWnd)
   nG     := 15   // между объектами
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   nHIco  := 48
   nWBtn  := 260      // 2 кнопки
   aBClr  := { 84,183,128}
   cIcon  := "gear48x1"
   cFont  := App.Cargo:cFontName
   nFSize := App.Cargo:nFontSize
   nHText := nFSize * 2

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := "Directory of equipment and works on request"
   cMsg   := "Find:"
#else
   cTitle := "Справочник оборудования и работ по заявке"
   cMsg   := "Поиск:"
#endif
   cTitle  += SPACE(5) + ProcFile()

   cRetAls := ALIAS()
   DbSelectArea("Ob4orud")
   OrdSetFocus(3)            // "KVIEW == 1 .AND. !Deleted()"
   //OrdSetFocus("KVIEW")    // ставим сразу здесь !!!
   DbGotop()

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // новое окно в координаты таблицы
   nW2 := nW * 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE FONT cFont SIZE nFSize BACKCOLOR aBClr            ;
      ON INIT    {|| oBrw:Setfocus(), DoEvents()  }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr   := This.Backcolor
      owc:cAls    := ALIAS()
      owc:cRetAls := cRetAls
      nY  := nX := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // задать и вывести кнопки над таблицей / set and display buttons above the table
      nX     := nW - nG - nWBtn*2 - nG
      nHUp   := Draw_BtnEx_Tovar( nY, nX, nG, nW, nWBtn, nHIco)

      nY     := nHUp - nHText - nG
      nX     := nG
      nWText := GetTxtWidth( cMsg, nFSize, cFont, .T. ) + 5
      @ nY, nX LABEL Lbl_Find VALUE cMsg WIDTH nWText HEIGHT nHText ;
        FONTCOLOR WHITE TRANSPARENT BOLD VCENTERALIGN //RIGHTALIGN

      nX += This.Lbl_Find.Width + 2
      // GetBox для поиска в таблицы
      @ nY, nX GETBOX GB_Find OBJ oGet WIDTH 120 HEIGHT nHText VALUE " " ;
        PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
        ACTION       {|| This.Value := "" }   ;
        IMAGE        {"bDelRed24","" }        ;
        ON GOTFOCUS  {|ob| ob := ThisWindow.Cargo:oBrw, ob:nCell := 3, ob:DrawSelect() } ;
        ON CHANGE    {|| Search_TsbDbf( ThisWindow.Object ) } ;
        ON INIT      {|| This.Cargo := .T. }
        //ON LOSTFOCUS {|| This.Cargo := .F., This.Value := space(30) } ;
      This.Cargo:oGet := oGet
      This.Cargo:cGet := "GB_Find"    // запомнить для дальнейшего использования

      nY    := nHUp         ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// таблица ///////////////////////////////////////////////////////
      oTsb := TablePatam( cForm, owc:cAls, "cTable", nWTsb, cTitle )
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, owc:cAls, "cTable", nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы
      oBrw:Cargo:nModify  := 0                            // счётчик изменений
      // объект положим на окно
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cSpTable"
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents() } )
         // имя объекта + имя события //   кнопки верхнего меню
         //            VVVV           //   oBtn:aObj := { "_3Save","_3Cancel"}
         :Event({10,"_3Save"}, {|ow,ky,cn,ob| // сохранить
                                              ob := ow:Cargo:oBrw
                                              If !IsString(cn)  // это когда срабатывает по клавише ENTER
                                                 cn := "_3Save"
                                              Endif
                                              _SetThisFormInfo(ow)
                                              //MsgDebug(ow:Name,ky,cn,ob:cAlias)
                                              aRet := myTsbCloseLine( ob )
                                              _SetThisFormInfo()
                                              This.&(cn).Enabled := .T.   // разблокировать кнопку
                                              // ob:SetFocus(), ow:Setfocus('Lbl_0')
                                              _wPost(99,ow)
                                              ky := cn
                                              Return Nil
                                              })

         // назначим клавиши в таблице, см. ниже
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_RETURN   , MsgDebug(...)
         //             _wPost(  32  ,  33  , 34   , 35-"_3Save" , 36

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DbSelectArea(ow:Cargo:cRetAls)
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_3Cancel"}, {|ow| aRet := {}, ow:Release() } )
         :Event({99,"_3Releas"}, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

   END WINDOW

   ACTIVATE WINDOW &cForm

   //IF _IsWindowDefined(cPrnt)               // можно так / you can do it like this
   //   DoMethod(cPrnt, "SetFocus")
   //ENDIF

   IF _IsWindowDefined(oWnd:Name)             // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_TsbDbf(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw
   LOCAL cGet  := oWnd:Cargo:cGet             // это "GB_Find"
   LOCAL cVal, lSwap := .F.
   //-----------------------------------------------------------------
   // НЕЛЬЗЯ ИСПОЛЬЗОВАТЬ для этого фильтра - oTsb:aNumber := {1, 30}
   //-----------------------------------------------------------------
   SET WINDOW THIS TO oWnd
   cVal := Trim( This.&(cGet).Value )
   cVal := upper(cVal)
   SET WINDOW THIS TO

   IF     Len( cVal ) == 0
      oBrw:FilterFTS()       // очистка фильтра
      lSwap := .T.
   ELSEIF Len( cVal ) > 2    // от 3-х символов поиск
      oBrw:FilterFTS( cVal, .T. )   // Empty(cVal) обработка внутри метода
      lSwap := .T.
   ENDIF

   IF lSwap
      oBrw:nCell := 3
      oBrw:Reset()
      DO EVENTS
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCloseLine( oBrw )
   LOCAL aRet, cAls, nCol, cNam, xVal

   aRet  := {}
   cAls  := oBrw:cAlias
   FOR nCol := 1 TO oBrw:nColCount()
       cNam := oBrw:aColumns[ nCol ]:cName
       IF cNam == "ORDKEYNO"  ; LOOP
       ELSEIF cNam == "SELECTOR"  ; LOOP
       ELSE
          xVal := oBrw:GetValue(cNam)
          IF IsString(xVal)
             xVal := ALLTRIM(xVal)
          ENDIF
          AADD( aRet, xVal )
       ENDIF
   NEXT
RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatam(cForm,cAls,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr, nHCell
   //
   oTsb := oHmgData()
   oTsb:cAls           := cAls
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   //---------------------------------------------------------------------------------------------------
   //oTsb:aNumber      := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // колонка нумерации и её ширина / numbering column and its width
   //oTsb:aNumber      := { 1, 30 }                // колонка нумерации и её ширина - НЕЛЬЗЯ ДЕЛАТЬ !!!
                                                   // т.к. будем использовать oBrw:FilterFTS()
   //---------------------------------------------------------------------------------------------------
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   oTsb:nHeightCell    := nHCell                   // высота ячеек
   oTsb:lDrawHeaders   := .F.                      // НЕ ставить в таблице шапку !!! Нет такого параметра
   oTsb:nHeightHead    := nHCell                   // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := nHCell                   // высота подвала
   oTsb:lFooting       := .T.                      // ставить в таблице подвал
   oTsb:lSpecHd        := .T.                      // поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.                      // поставить в таблице суперхидер
   oTsb:cSuperHd       := cTitle                   // текст суперхидера
   oTsb:nHeightSuperHd := nHCell                   // высота суперхидера
   oTsb:nCellMarginLR  := 0                        // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   oTsb:uSelector      := 20                       // селестор слева таблицы
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "С_Ob4orud", "С_CENA"  , "C_Obor"  , "C_Mast"  , "С_CODE"  , "С_OB1"   , "С_OB2"    }  // имена полей колонок
   oTsb:aField         := { "Ob4orud"  , "CENA_ALL", "CenaObor", "CenaMast", "KOb4orud", "KOB1ZAIV", "KOB2WORK" }  // имена полей базы
   ? ProcNL(), "######## nHCell=", nHCell

#ifdef KEY_ENG // for this project demo1-en.hbp
   //                        1                               2             3         4        5               6                  7
   oTsb:aHead := { "Name;of equipment or work"          , "Price", "Equipment", "Master'y", "Codes"      , "Bid;Group"    , "Work;Group"    }
#else
   oTsb:aHead := { "Наименование;оборудования или работ", "Цена" , "Оборуд."  , "Мастеру" , "Коды;оборуд.", "Группа;заявки", "Группа;работ" }
#endif

   oTsb:aHideCol := {}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := DbfCalculatColumnWidths(cAls,oTsb:aField,2,nWTsb)   // подсчёт ширины колонок - добавка во 2 колонку
   oTsb:aSize    := aWSize                                              // назначим ширину колонок для ТСБ

   // высоту можно задать и так
   nHFnt              := App.Cargo:nFontSize * 1.8
   //oTsb:nHeightHead := nHFnt                          // высота шапки
   //oTsb:nHeightFoot := nHFnt                          // высота подвала

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
      oTsb:aFoot       := .T.                            // заполнить подвал
   ENDIF

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // высота нумератора
   ENDIF

   aBClr               := This.Backcolor
   nClr1               := HMG_RGB2n(aBClr)                  // цвет фона шапка+подвал
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := aBClr                             // цвет фона под таблицей
   // цвета в таблицу
   oTsb:lZebra    := .T.                                    // это вкл.\откл. механизм zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // серый цвет
   oTsb:aZebra    := { aBClr, {190,244,214} }
   a := {}
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_RED                  })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , RGB(240,240,240)         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a

   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) } // блок кода после END TBROWSE НЕ использовать
                                               // без необходимости, работает DEFAULT значение
                                               // !!! все делать в oTsb:bAfter !!!
   // Проверка
   //  ?v oTsb:aName   //
   //  ?v oTsb:aHead   //
   //  ?v oTsb:aSize   //
   //  ?v oTsb:aField  //

   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)            // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   //ob:nFreeze     := ob:nColumn("ORDKEYNO")     // Заморозить столбцы
                   //ob:lLockFreeze := .T.                        // Избегать прорисовки курсора на замороженных столбцах
                   //ob:lNoKeyChar  := .F.                        // ввод в ячейки от букв, цифр
                   //myTsbEditDbf(ob,op)                          // редактирование ячеек таблицы
                   ob:nCell        := 1                           // передвинуть курсор
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         // здесь не работает уменьшение ширины колонки
                         //oc:nWidth := GetTextWidth( Nil, "00000", hFont )   // кол-во знаков
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   ob:lNoHScroll := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll   := NIL
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nw := 0, nn, i
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       i := hb_enumindex(oc)
                       IF oc:cName == "ORDKEYNO"
                          nn := oc:nWidth
                          // вот здесь делаем уменьшение ширины колонки
                          oc:nWidth := GetTextWidth( Nil, "00000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                       //? i, oc:cName, oc:nWidth
                    NEXT
                    IF !Empty(nn)
                       oc := ATail(ob:aColumns)
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_RETURN, {|ob| _wPost(35, ob:cParentWnd, ob) } }  ;
                     }

   // назначить события на окно  --> tsb_util.prg
   oTsb:aEvents   := { ;                                       //!!!
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow     } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow     } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow     } }, ;
        {35, {|ow,ky,ob| _wPost("_3Save",ow)  , ky:=ow:=ob                } }, ;
        {36, {|ow,ky,ap| MsgDebug(ap[1],ap[2],ap[3],ap[4]:cAlias), ky:=ow } }  ;
                     }

   // Двойной клик мышки на курсоре в таблице
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // Вот так !!!
   // Правый клик мышки на курсоре в таблице
   oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(36, ob:cParentWnd, {p1,p2,p3,ob}) }
   // Левый клик мышки на курсоре в таблице
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок
STATIC FUNCTION DbfCalculatColumnWidths(cAls,aFld,nCol,nWTsb)
   LOCAL v, i, j, hFont, nW, aWSize, cFld

   hFont  := GetFontHandle("Normal")
   aWSize := Array(Len(aFld))
   aFill(aWSize, 0)

   DBSELECTAREA(cAls)
   FOR j := 1 TO  ORDKEYCOUNT()
       ORDKEYGOTO(j)
       FOR EACH cFld IN aFld
           i := hb_enumindex(cFld)
           // делаем расчёт на все колонки
           v := (cAls)->&cFld
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           v  := ALLTRIM(v)
           v  += "HH"  // добавка
           nW := GetTextWidth( Nil, v, hFont )
           nW := IIF( nW > 400, 400, nW )
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT
   DbGotop()

   //oTsb:aNumber := { 1, 30 }                // колонка нумерации и её ширина
   // для колонки 2 делаем всю ширину экрана показа, кроме колонки 1
   //aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ширина показа других колонок в зависимости от ширины текста
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb,"nCol=",nCol
   //?? "Колонка:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
/*STATIC FUNCTION myTsbEditDbf( oBrw )
   LOCAL oCol, cCol, nI

   //? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // oCol:lEdit := .T. // задано выше oc:lEdit  := .T.
      // oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> здесь не нужно
      // oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> здесь не нужно
      //IF "F_CHK" $ cCol
      //   oCol:lEdit     := .T.   // сделано выше
      //   oCol:nEditMove := 0     // откл. перемещение курсора после :Edit()
      //   oCol:bPrevEdit := {|xv,ob| xv = ob, DoEvents() , ob:Cargo:nModify++ }         // счётчик изменений
      //   oCol:bPostEdit := {|val, brw| brw:Cargo:nChk += IIF( val, 1, -1 ) ,;          // счётчик кол-ва чекбокса
      //                                 brw:aColumns[2]:cFooting := HB_NtoS(brw:Cargo:nChk) ,;
      //                                 brw:DrawFooters()  }
      //ENDIF

      //IF "F_NAME" $ cCol
      //   oCol:lEdit     := .F.
      //   oCol:nEditMove := 0  // откл. перемещение курсора после :Edit()
      //   oCol:bPrevEdit := {|xv,ob| ob:nCell := 2, ob:DrawSelect(), xv = ob, DoEvents() , ;
      //                              ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0), .F. }
      //ENDIF
      //oCol:bPrevEdit := {|xv,ob| ob:nCell := 3, ob:DrawSelect(), xv = ob, DoEvents() , ;
      //                           ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0), .F. }
                                                                    // вернуть .F. чтобы не попастьв get-ячейки
      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются - для массива не работает
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL */

