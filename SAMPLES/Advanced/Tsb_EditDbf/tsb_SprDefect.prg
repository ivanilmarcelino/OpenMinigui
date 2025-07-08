/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 15.04.25
 *
 * _TBrowse() Разные функции для редактирований ячеек таблицы
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_ZaListNeis(oBrw1,aDim,oDop,aLine,cForm)
   LOCAL oWnd, aRet, nWBtn, nHBtn, nHIco, aBtnCap, nHText, cIcon, cTitle
   LOCAL cFont, nFSize, aFont, aFont2, cBtnFont, nBtnFSize, aXDfct, cMsg1
   LOCAL nY, nX, nW, nH, nG, aBClr, aBtnFClr, aBtnBClr, nWText, aFldDbf
   LOCAL owc, aIco1, aIco2, aIco3, aColor, aGrOver, aGrFill, aValDbf, aChk
   LOCAL cMsg, nY2, nX2, nW2, nH2, nWTsb, nHTsb, oBrw, oTsb, oGet, aCode, cErr
   DEFAULT cForm := "Tsb_Win_Defect"

   //MsgDebug(aDim,"oDop=",oDop,"aLine=",aLine,cForm)
   ? "======",ProcNL(),aDim,"oDop=",oDop,"aLine=",aLine,cForm
   //?v  aLine
   aFldDbf := aDim  // нельзя использовать - есть вероятность того что запись в БД ушла на другую позицую
                    // нужно делать доп. контроль
   aValDbf := oDop  // поле aDefect - тоже нельзя использовать - резерв
   aCode   := aLine[ACOL_13]  //  (13) - значение исправленного поля {} для типа CALC
   IF "DEMO3.EXE" $ UPPER(cFileNoPath(App.ExeName))
      // позиция окна по окну / window position by window
      oWnd   := _WindowObj( oBrw1:cParentWnd )    // parent window
      nY     := oWnd:Row
      nX     := oWnd:Col
      nW     := oWnd:Width
      nH     := oWnd:Height
   ELSE
      // позиция окна по ТСБ / window position according to TSB
      oWnd   := _WindowObj( oBrw1:cParentWnd )   // родительское окно
      nY     := GetWindowRow(oBrw1:hWnd)
      nX     := GetWindowCol(oBrw1:hWnd)
      nW     := GetWindowWidth(oBrw1:hWnd)
      nH     := GetWindowHeight(oBrw1:hWnd)
   ENDIF
   nG     := 10   // между объектами
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   nHIco  := 48
   nWBtn  := 220      // 3 кнопки
   nHBtn  := nHIco + 10
   aBClr  := { 84,183,128}
   cIcon  := "gear48x1"

#ifdef KEY_ENG // for this project demo1-en.hbp
   aFont   := GetFontParam("DlgFont")
   aFont2  := GetFontParam("ComSanMS")
   aBtnCap := { "Select", "Cleaning", "Cancel" }
   cTitle  := "Malfunction on request"
   cMsg    := "Find:"
   cMsg1   := "ATTENTION!; The number of selected items is MORE than 10!; I am leaving only 10 items!"
   cErr    := "Error! No array of dbf faults!;Array oWnd:Cargo:aXDfct not defined!;;"
#else
   aFont   := GetFontParam("FntCnMn1")
   aFont2  := GetFontParam("FntCnMn2")
   aBtnCap := { "Отобрать", "Очистка", "Отменить" }
   cTitle  := "Неисправность по заявке"
   cMsg    := "Поиск:"
   cMsg1   := "ВНИМАНИЕ !; Количество выбранных позиций БОЛЬШЕ 10 !; Оставляю только 10 позиций !"
   cErr    := "Ошибка ! Нет массива dbf-неисправностей !;Массив oWnd:Cargo:aXDfct не определён !;;"
#endif
   cTitle  += SPACE(5) + ProcFile()

   aXDfct  := App.Cargo:aSprDfct         // массив dbf-неисправностей из Cargo окна // -> demo1_util.prg
   IF !IsArray(aXDfct)
      cErr += ProcNL() + ";" + ProcNL(1) + ";;"
      AlertStop(cErr, "", , 64, {RED})
      ? ATREPL( ";", cErr, CRLF )
   ENDIF
   aXDfct := mySortDim(aXDfct,aCode,@aChk)    // поставить чекбоксы в массив + aChk-массив с чекбоксом,
                                              // может быть пустым {}
   cFont     := aFont[1]
   nFSize    := aFont[2]
   nHText    := nFSize * 2
   // кнопки на форме
   cBtnFont  := aFont2[1]
   nBtnFSize := aFont2[2]
   aBtnFClr  := { BLUE , YELLOW }
   aColor    := GRAY
   aGrOver   := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
   aGrFill   := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
   aBtnBClr  := { {225,225,225}, GRAY }
   aBtnFClr  := { RED, YELLOW }

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // новое окно в координаты таблицы
   nW2 := nW //* 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2, nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE FONT cFont SIZE nFSize BACKCOLOR aBClr             ;
      ON INIT    {|| _wSend(0), DoEvents()     }                      ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90) }   // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr := This.Backcolor
      nY  := nX := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      @ 0, 0 LABEL Label_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      nY     := nHBtn + nG*2 - nHText - nG
      nWText := GetTxtWidth( cMsg, nFSize, cFont, .T. ) + 5
      @ nY, nX LABEL Lbl_Find VALUE cMsg WIDTH nWText HEIGHT nHText ;
        FONTCOLOR WHITE TRANSPARENT BOLD VCENTERALIGN //RIGHTALIGN

      nX += This.Lbl_Find.Width + 2
      // GetBox для поиска в таблицы
      @ nY, nX GETBOX GB_Find OBJ oGet WIDTH 180 HEIGHT nHText VALUE " " ;
        PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
        ACTION       {|| ThisWindow.Cargo:nGB_Find := 1, This.Value := "" }   ;
        ACTION2      {|| ThisWindow.Cargo:nGB_Find := 2, This.Value := "" }   ;
        IMAGE        {"bTofix24", "bDelRed24" } ; // tobegin, tofix, collect
        BUTTONWIDTH  nHText                     ;
        ON GOTFOCUS  {|ob| ob := ThisWindow.Cargo:oBrw, ob:nCell := 3, ob:DrawSelect() } ;
        ON CHANGE    {|| Search_ATSB( ThisWindow.Object ) } ;
        ON INIT      {|| This.Cargo := .T. }
        //ON LOSTFOCUS {|| This.Cargo := .F., This.Value := space(30) } ;
        //ON CHANGE    {|| iif( Empty( This.Cargo ), NIL, Search_ATSB( ThisWindow.Object, .T. ) ) } ;

      This.Cargo:oGet := oGet
      This.Cargo:cGet := "GB_Find"    // запомнить для дальнейшего использования
      This.Cargo:nGB_Find := 0

      nY    := nHBtn + nG*2 ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// таблица ///////////////////////////////////////////////////////
      oTsb := TablePatamDfct( cForm, aXDfct, "cTableDfc", nWTsb, aLine[1])
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDfct, "cTableDfc", nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы
      oBrw:Cargo:nCellFoc := 2                            // фокусная ячейка
      oBrw:Cargo:nModify  := 0                            // счётчик изменений
      oBrw:Cargo:nChk     := LEN(aChk)                    // счётчик кол-ва чекбокса
      oBrw:Cargo:aIsx     := oBrw:aArray                  // ИСХОДНЫЙ массив ВАЖНО !!!
      oBrw:Cargo:nArray   := 1                            // коррект.массива 1-oBrw:aArray, 2-owc:aIsx
      // объект положим на окно
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cSpTable"
      This.Cargo:aChk     := aChk                         // массив с чекбоксом .T. строки
      //
      /////////////////////// кнопки на форме ////////////////////////////////
      nY    := nG
      nX    := nW - nWBtn * 3 - nG * 3

      aIco1 := { "iMg_Ok48x1"    , "iMg_Ok48x2"     }
      aIco2 := { "iCircle48x1"   , "iCircle48x2"    }
      aIco3 := { "iMg_Cancel48x1", "iMg_Cancel48x2" }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[1] ;
               ICON aIco1[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco1[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco1[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(10 ,, This.Name) }

      nX := nW - nWBtn * 2 - nG * 2

      @ nY, nX BUTTONEX Btn_Cls WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[2] ;
               ICON aIco2[1] NOXPSTYLE HANDCURSOR NOTABSTOP            ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                  ;
               ON MOUSEHOVER ( This.Icon := aIco2[2] ,        ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco2[1] ,        ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(20 ,, This.Name) }

      nX := nW - nWBtn  - nG

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION aBtnCap[3] ;
               ICON aIco3[1] NOXPSTYLE HANDCURSOR NOTABSTOP                ;
               FONTCOLOR aBtnFClr[1] FONT cBtnFont SIZE nBtnFSize BOLD     ;
               BACKCOLOR aGrOver GRADIENTFILL aGrFill                      ;
               ON MOUSEHOVER ( This.Icon := aIco3[2],         ;
                               This.Fontcolor := aBtnFClr[2], ;
                               This.GradientFill := aGrFill ) ;
               ON MOUSELEAVE ( This.Icon := aIco3[1],         ;
                               This.Fontcolor := aBtnFClr[1], ;
                               This.GradientOver := aGrOver ) ;
               ACTION {|| _wPost(98 ,, This.Name) }

      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents(), _wSend(5,ow) } )

         :Event(  5, {|ow| // счётчик кол-ва чекбокса в подвале таблицы
                           Local ob   := ow:Cargo:oBrw
                           Local nChk := ob:Cargo:nChk
                           ob:aColumns[2]:cFooting := HB_NtoS(nChk)
                           ob:DrawFooters()
                           DO EVENTS
                           Return Nil
                           } )

         :Event( 10, {|ow,ky,cn|  // сохранить данные
                       Local a, i, obr, owc, aTxt, aChk
                       owc  := ow:Cargo
                       obr  := owc:oBrw
                       a    := obr:Cargo:aIsx
                       aChk := {}  ; aTxt := {}
                       For i := 1 TO LEN(a)
                          If a[i,1]
                             AADD(aChk, a[i,3]) // коды по чекбоксу
                             AADD(aTxt, a[i,2]) // наименование
                          Endif
                       Next
                       IF LEN(aChk) > 10
                          AlertStop(cMsg1,,,64,{RED})
                       ELSEIF LEN(aChk) == 0
                          aChk := ARRAY(10)
                          AFILL( aChk, 0 )
                          aTxt := ARRAY(10)
                          AFILL( aTxt, "" )
                       ELSE
                          For i := LEN(aChk) TO 10
                             AADD(aChk, 0 ) // коды по чекбоксу
                             AADD(aTxt, "") // наименование
                          Next
                       ENDIF
                       //MsgDebug( owc:oBrw:Cargo:nModify, owc:oBrw:Cargo:nChk )
                       // счётчик изменений   // счётчик кол-ва чекбокса
                       ? "### Save Tsb Check:", ProcNL(), owc:oBrw:Cargo:nChk
                       ?? "aChk=", Len(aChk), HB_ValToExp(aChk)
                       aRet := { aChk, aTxt }
                       _wPost(99,ow)
                       ky := cn
                       Return Nil
                       } )

         :Event( 20, {|ow,ky,cn|  // очистить чекбокс
                       Local obr, a, i, owc := ow:Cargo
                       owc:oBrw:Cargo:nModify := 0
                       owc:oBrw:Cargo:nChk    := 0
                       obr := owc:oBrw
                       a   := obr:aArray
                       For i := 1 TO LEN(a)
                          a[i,1] := .F.
                       Next
                       obr:aColumns[2]:cFooting := ""
                       obr:DrawFooters()
                       obr:Refresh()
                       DO EVENTS
                       ky := cn
                       Return Nil
                       } )

         // назначим клавиши в таблице, см. ниже
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4
         //             _wPost(  32  ,  33  , 34

         :Event(90, {|ow,ky| // ON Release windows
                       Local cm
                       cm := ProcNL()
                       ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                       ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                       DO EVENTS
                       Return Nil
                       })

         :Event( 98, {|ow| aRet := {}, _wPost(99,ow) } )

         :Event( 99, {|ow| ow:Release() } )
      END WITH

      ON KEY F1   ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

   END WINDOW

   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)  // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_ATSB(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw, aDim := {}, a
   LOCAL cGet  := oWnd:Cargo:cGet             // это "GB_Find"
   LOCAL cVal  := trim( This.&(cGet).Value )
   LOCAL nBtn  := oWnd:Cargo:Get("n"+cGet, 0)
   LOCAL lSwap := .F.
   LOCAL nLen  := Len( cVal )

   IF     nLen == 0
      ? "~~~~~~~>>>", cGet, nBtn, cVal
      IF nBtn == 1       // нажали кнопку "строки c V к началу"
         // сортировка исходного массива для сбора галочек вверху
         oBrw:aArray := ASORT( oBrw:aArray,,, { |x, y| x[1] > y[1] } )
      ELSE               // нажали кнопку "очистить\снять фильтр"
         // очистить\снять фильтр и показать исходный массив
         oBrw:aArray := ASORT( oBrw:aArray,,, { |x, y| x[2] < y[2] } )
      ENDIF
      oBrw:Cargo:nArray := 1             // коррект.массива 1-oBrw:aArray, 2-oBrw:Cargo:aIsx
      oBrw:aArray := oBrw:Cargo:aIsx
      lSwap := .T.
   ELSEIF nLen > 2    // от 3-х символов поиск
     cVal := upper(cVal)
     FOR EACH a IN oBrw:Cargo:aIsx
         IF cVal $ upper(a[2]) ; AAdd( aDim, AClone( a ) )
         ENDIF
     NEXT
     IF Len(aDim) = 0
        a := array( Len(oBrw:aArray[1]) )
        a[2] := 'Нет такой строки: "' + cVal + '" ....'
        AAdd( aDim, a )
     ENDIF
     oBrw:Cargo:nArray := 2              // коррект.массива 1-oBrw:aArray, 2-owc:aIsx
     oBrw:aArray := aDim
     lSwap := .T.

   ENDIF

   IF lSwap
      oBrw:nCell := 3
      oBrw:Reset()
      DO EVENTS
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySortDim(aXDfct,aCode,aChk)          // поставить чекбоксы в массив
   LOCAL nI, nJ, nCode, aRet := {}

   aChk := {}
   IF !IsArray(aCode)
      RETURN aXDfct
   ENDIF
   IF LEN(aCode) == 0
      RETURN aXDfct
   ENDIF
   FOR nI := 1 TO LEN(aCode)
      nCode := aCode[nI]
      IF nCode > 0
         AADD( aChk, nCode )          // есть чекбокс по коду
         FOR nJ := 1 TO LEN(aXDfct)
            IF nCode == aXDfct[nJ,3]  // коды неисправностей
               aXDfct[nJ,1] := .T.
            ENDIF
         NEXT
      ENDIF
   NEXT
   aRet := ASORT( aXDfct,,, { |x, y| x[1] > y[1] } )

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamDfct(cForm,aXDim,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   oTsb:aNumber        := { 1, 30 }                // колонка нумерации и её ширина
   oTsb:nHeightCell    := 28                       // высота ячеек = высоте картинки чекбокса
   oTsb:lDrawHeaders   := .F.                      // НЕ ставить в таблице шапку !!! Нет такого параметра
   oTsb:nHeightHead    := oTsb:nHeightCell         // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := oTsb:nHeightCell         // высота подвала
   oTsb:lFooting       := .T.                      // ставить в таблице подвал
   oTsb:lSpecHd        := .T.                      // поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.                      // поставить в таблице суперхидер
   oTsb:cSuperHd       := cTitle                   // текст суперхидера
   oTsb:nHeightSuperHd := 24                       // высота суперхидера
   oTsb:nCellMarginLR  := 0                        // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   //oTsb:uSelector    := 20                       // селестор слева таблицы
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "F_CHK", "F_NAME", "F_CODE", "F_GRP", "F_VID", "F_SRK" }

#ifdef KEY_ENG // for this project demo1-en.hbp
   //               1         2                          3              4                        5                    6
   oTsb:aHead := { "*", "Fault name"            , "Codes; faulty." , "Fault;group"        , "Fault type"       , "Urgency type"   }
#else
   oTsb:aHead := { "*", "Название неисправности", "Коды;неиспр.", "Группа;неисправности", "Вид;неисправности", "Тип;срочности" }
#endif

   oTsb:aHideCol := {}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidthsDfc(aXDim,2,nWTsb)   // подсчёт ширины колонок - добавка во 2 колонку
   oTsb:aSize    := aWSize                                  // назначим ширину колонок для ТСБ

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
   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)            // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO")        // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   myTsbEditDfct(ob,op)                             // редактирование ячеек таблицы
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lPickerMode := .F.
                   oc := ob:aColumns[2]
                   oc:lEdit     := .T.
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nAlign    := DT_LEFT
                   oc:nEditMove := 0    // перечитать ячейку
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         // здесь не работает уменьшение ширины колонки
                         //oc:nWidth := GetTextWidth( Nil, "0000", hFont )   // кол-во знаков
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
                    Local oc, nw := 0, nn, nc := ob:nColumn("F_NAME")
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       IF oc:cName == "ARRAYNO"
                          nn := oc:nWidth
                          // вот здесь делаем уменьшение ширины колонки
                          oc:nWidth := GetTextWidth( Nil, "00000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                       // запрет перескакивания с колонки 2 - F_CHK
                       IF hb_enumindex(oc) > nc
                          oc:bGotFocus := {|nold,ncel,ob|
                                           nold := ob:nColumn("F_CHK")
                                           IF ncel != nold
                                              ob:nCell := nold
                                              ob:DrawSelect()
                                              DO EVENTS
                                           ENDIF
                                           Return Nil
                                           }
                       ENDIF
                    NEXT
                    IF !Empty(nn)
                       oc := ATail(ob:aColumns)
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице --> tsb_util.prg
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }  ;
                     }

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }  ;
                     }
RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок
STATIC FUNCTION CalculatColumnWidthsDfc(aXDim,nCol,nWTsb)
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
           nW := IIF( nW > 400, 400, nW )
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT

   //oTsb:aNumber := { 1, 30 }                // колонка нумерации и её ширина
   // для колонки 2 делаем всю ширину экрана показа, кроме колонки 1
   //aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ширина показа других колонок в зависимости от ширины текста
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb,"nCol=",nCol
   //?? "Колонка:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEditDfct( oBrw )
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
      IF "F_CHK" $ cCol
         oCol:lEdit     := .T.   // сделано выше
         oCol:nEditMove := 0     // откл. перемещение курсора после :Edit()
         oCol:bPrevEdit := {|xv,ob| xv = ob, DoEvents() , ob:Cargo:nModify++ }         // счётчик изменений
// ---------- пример массива ----------
//      1              2              3         4               5           6      7
// 1  {.T., "постоянный гул панели", 108, "АДС подъезда", "электроника", "1-день", 35}
// 2  {.F., "резерв", 240, "ТО -> плановые работы", "металл", "1-день", 1}
// 3  {.F., "выезд мастера", 221, "ПЗ -> АУ", "электроника", "3-дня", 2}
// 4  {.F., "дверь заблокирована", 101, "АДС подъезда", "электроника", "текущий день (до 17:30)", 3}
         oCol:bPostEdit := {|val, brw| // после ввода
                                       Local nArr := brw:Cargo:nArray          // коррект.массива 1-oBrw:aArray, 2-oBrw:Cargo:aIsx
                                       Local lChk, id, a, b, i, j := 0
                                       // :nAt НЕ совпадает с порядковым номером в массиве из-за сортировки массива
                                       brw:Cargo:nChk += IIF( val, 1, -1 )     // счётчик кол-ва чекбокса
                                       lChk := val
                                       b    := brw:Cargo:aIsx
                                       If nArr == 1                     // правка массива ТСБ исходного-неисправленного
                                          a  := brw:aArray[brw:nAt]     // текущая строка
                                          id := a[7]                    // id в обоих массивах
                                          ? ")-~-~->", nArr, ":nAt=", brw:nAt, val, "id=", id, a[2]
                                          For i := 1 To LEN(b)
                                             //? i, ")", id, "==", b[i][7], b[i][2]
                                             If id == b[i][7]
                                                j := i
                                                Exit
                                             Endif
                                          Next
                                          ?? "j=",j
                                          If j > 0
                                             brw:Cargo:aIsx[j][1] := lChk
                                             ?? brw:Cargo:aIsx[j][2] , brw:Cargo:aIsx[j][7]
                                          Endif
                                       Else
                                          // включён фильтр, массив в ТСБ уже другой - сокращёный
                                          a  := brw:aArray[brw:nAt]     // текущая строка
                                          id := a[7]                    // id в обоих массивах
                                          ? "]=~=~=>", nArr, ":nAt=", brw:nAt, val, "id=", id, a[2]
                                          For i := 1 To LEN(b)
                                             //? i, "==", b[i][7] , b[i][2]
                                             If id == b[i][7]
                                                j := i
                                                Exit
                                             Endif
                                          Next
                                          ?? "j=",j
                                          If j > 0
                                             brw:Cargo:aIsx[j][1] := lChk
                                             ?? brw:Cargo:aIsx[j][2] , brw:Cargo:aIsx[j][7]
                                          Endif
                                       Endif
                                       _wSend(5,brw:cParentWnd)
                                       Return Nil
                                       }
      ENDIF

      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются - для массива не работает
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

