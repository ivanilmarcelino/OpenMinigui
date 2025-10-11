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
   LOCAL nTsbLine, aCharColumn, aRet, cAls, lDelFile

   // создадим базу справочника / let's create a directory database
   lDelFile    := .F.           // не удалять файл Dbf
   aRet        := my3SpravDbfNew(cPath, cFile, a2Dim, lDelFile)
   cAls        := aRet[1]       // алиас временной базы
   aCharColumn := aRet[2]       // кол-во символов в колонках
   SELECT(cAls)
   DbSetOrder(0)
   nTsbLine   := LASTREC()     // кол-во строк в таблице
   DbSetOrder(1)
   Goto Top
   aRet := my3SelectDbf(cAls, cTitle, aCharColumn, nTsbLine)
   IF LEN(aRet) == 0
      aRet := { {} , {}, {"нет событий в справочнике/no events in the directory"} }
   ENDIF

   (cAls)->(DbCloseArea())

RETURN aRet

////////////////////////////////////////////////////////////////////////////////
// справочник событий в программе / directory of events in the program
STATIC FUNCTION my3SelectDbf(cAls, cTitleTsb, aCharColumn, nTsbLine)
   LOCAL cForm, cTitle, cIcon, cFont, cFontBtn, cText, nWTxt, cFind, aBClr
   LOCAL nI, oBrw, nFontSize, aRet, nY, nX, nW, nH, nLR, nG, aBClrTxt, aXY
   LOCAL nTsbHeight, nTsbWidth, nHTxt, oDlu, nWTsb, nHTsb, oTsb, oGet
   //LOCAL cObjBtn, cBtnCapt, aBtnIcon, aBtnGrad, aBtnClr, aFntClr, aBtnFnt
   //LOCAL nYBtn, nXBtn, nwPost, nWBtn, nHBtn
   LOCAL aFont, hFont, nHCell, nHSpecHd, aWSize, nWSys, aLang, oWnd, cTbl, owc

   ? "======",ProcNL(),cAls, cTitleTsb, aCharColumn, nTsbLine

#ifdef KEY_ENG
   cTitle := "Event Directory"
   aLang := { "Event Search:", "Select", "Cancel" }
#else
   cTitle := "Справочник событий"
   aLang  := { "Поиск события:", "Выбор", "Отмена" }
#endif

   oWnd       := _WindowObj( GetActiveWindow() )   // родительское окно
   cForm      := "Form_SprDbf"
   cIcon      := "iView48x1"
   hFont      := GetFontHandle("Normal")
   aFont      := GetFontParam(hFont)
   cFont      := aFont[1]
   nFontSize  := aFont[2]
   nHTxt      := nFontSize*2                               // ширина GET'ов
   cFontBtn   := "Comic Sans MS"
   aRet       := {}
   cFind      := ''
   aBClr      := {  6,175,143}
   aBClrTxt   := {240,240,240}
   nTsbWidth  := 0
   // aCharColumn := { 3, 6, nMax }                        // кол-во символов в колонках
   aWSize     := CalculatColumnWidths(aCharColumn,hFont)   // подсчёт ширины колонок
   FOR nI := 1 TO LEN(aCharColumn)
      nTsbWidth += aWSize[nI]
   NEXT
   nTsbWidth  += GetVScrollBarWidth()                      // примерная ширина таблицы
   nHCell     := 28                                        // высота картинки чекбокса
   nHSpecHd   := nFontSize                                 // высота нумератора  таблицы
   nTsbHeight := ( nTsbLine + 1 + 1) * nHCell + nHSpecHd   // примерная высота таблицы
   ?  "approximate height of the table, nTsbHeight = ", nTsbHeight, "nTsbLine=",nTsbLine
   ?  "approximate table width, nTsbWidth = ", nTsbWidth

   // возвращает объект с данными размеров от размера фонта от dlu в pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top*2
   //
   nX    := nLR  := 20
   nWSys := System.ClientHeight * 0.95
   nY    := nG                                        // отступ сверху и снизу
   nW    := nTsbWidth + nLR*2 + GetBorderWidth()      // размеры окна
   nH    := nTsbHeight + 90 + nG*2
   IF nH > nWSys
      nH         := nWSys
      nTsbHeight := nH - ( nG/2 + nHTxt + nHTxt + nG/2 + nG/2 )
      nTsbHeight -= ( GetTitleHeight() + GetBorderHeight() )
   ENDIF

   SET FONT TO cFont, nFontSize
   SELECT(cAls)

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   DEFINE WINDOW &cForm                           ;
      AT nY, nX WIDTH nW HEIGHT nH                ;
      ICON cIcon TITLE cTitle BACKCOLOR aBClr     ;
      MODAL NOSIZE                                ;
      FONT cFont SIZE nFontSize                   ;
      ON INIT    {|| _wSend(0), DoEvents()     }  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90) }     // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; owc := This.Cargo   // создает объект без переменных (условно пустой) используем ниже по коду

      owc:aBClr   := This.Backcolor
      owc:nFSize  := nFontSize          // размер фонта таблицы
      owc:hWin    := This.Handle        // потом проще добывать handle окна
      owc:cWin    := This.Name          // ...
      owc:cAls    := cAls               // база
      //Узнать есть ли переменная в объекте можно так
      //IF ( owc:Pos(upper("<имя переменной>")) ) > 0
      //ENDIF
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      nY    := nG/2
      cText := aLang[1]  //"Поиск по наименованию:"
      nWTxt := GetTxtWidth( cText, nFontSize, cFont )
      @ nY, nX LABEL Label_2 WIDTH nWTxt HEIGHT nHTxt VALUE cText ;
        FONTCOLOR BLACK TRANSPARENT /*CENTERALIGN*/ VCENTERALIGN
      nY += This.Label_2.Height

      // GetBox вверху окна
      @ nY, nX GETBOX GB_Find OBJ oGet WIDTH nWTxt HEIGHT nHTxt VALUE space(30)  ;
        PICTURE "@K" NOTABSTOP FONTCOLOR BLACK BACKCOLOR aBClrTxt                ;
        ON LOSTFOCUS {|| This.Cargo := .F., This.Value := space(30) }            ;
        ON CHANGE    {|| iif( Empty( This.Cargo ), NIL, Search_TSB( ThisWindow.Object, .T. ) ) } ;
        ON INIT      {|| This.Cargo := .T. }

      This.Cargo:oGet := oGet
      This.Cargo:cGet := "GB_Find"    // запомнить для дальнейшего использования

      nY += This.GB_Find.Height + nLR/2
      nX += This.GB_Find.Width + nLR/2

      // задать и вывести кнопки над таблицей
      aXY   := Draw_BtnEx_Filtr( nLR/2, nX, nLR/2, nW-nX, { aLang[2], aLang[3] } )
      // координаты таблицы
      nX    := nLR
      nWTsb := nTsbWidth              // примерная ширина таблицы
      nHTsb := nH - nY - nG/2         // примерная высота таблицы

      ///////////////////////////////// Table //////////////////////////////////////////
      //@ nY, nX LABEL Lbl_2 WIDTH nWTsb HEIGHT nHTsb VALUE '- Table -' BACKCOLOR GRAY
      cTbl := "cTableChk"
      oTsb := TablePatamDbf( cForm, cAls, cTbl, nWTsb, cTitle, hFont, nHCell, nHSpecHd, aCharColumn)
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, cAls, cTbl, nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы
      oBrw:Cargo:nModify := 0                            // счётчик изменений
      // объект положим на окно
      This.Cargo:oBrw    := oBrw                         // owc:oBrw

      WITH OBJECT This.Object
         :Event(  0,             {|ow| /*ow:Cargo:oBrw:SetFocus(),*/ ow:Setfocus("GB_Find") , DoEvents()  } )
         :Event({10,"_FSelect"}, {|  | aRet := Dbf2GetAllLine(oBrw) , _wSend(99) } )

         :Event( 90, {|ow,ky| // ON Release windows
                              Local cm
                              cm := ProcNL()
                              ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                              ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                              DO EVENTS
                              Return Nil
                              })

         :Event({18,"_FExit"}, {|ow,ky,cn| // выход
                                            _LogFile(.T., "  -->> Button: ",cn, ow:Name, ky)
                                            aRet := {}
                                            _wSend(99,ow:Name)
                                            Return Nil
                                            } )

         //:Event({98,"_FExit2"}, {|ow,ky,cn| aRet := {} , ky := cn , ow:Release() } )

         :Event(99, {|ow| ow:Release()    } )
      END WITH

      ON KEY F1     OF &cForm ACTION NIL
      ON KEY ESCAPE OF &cForm ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)    // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0     // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet

///////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Filtr( nY, nX, nGBtn, nW, aLang )
   LOCAL nHIco, lIco, nWBtn, nHBtn, nBtnLen, aFont2, oBtn, cText
   LOCAL nWText, aYX, lRow := .T.  // кнопки по горизонтали
   DEFAULT nW := 0

   oBtn       := oHmgData()
   nHIco      := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана / icon height from screen
   nHIco      += 5*2
   nHIco      := 48                                           // задаём вручную размер иконки
   lIco       := .T.                                          // растягивать размер иконки  / stretch the icon size
   aFont2     := GetFontParam(GetFontHandle("FntBtnMain"))    // Фонт кнопок главной формы  / Main form button font
   //                    1          2
   oBtn:aCap  := { aLang[1]  , aLang[2]    }
   oBtn:aObj  := { "_FSelect", "_FExit"    }  // метка события / event label
   oBtn:aClr  := { CLR_GREEN , {189,30,73} }
   oBtn:aPst  := { 10, 99 }                    // _wPost(Х) - не использую / I don't use it
   oBtn:aIco  := { {"iFloppy48x1", "iFloppy48x2"   , lIco, nHIco } ,;
                   {"iReturn48x1" , "iReturn48x2"  , lIco, nHIco }     }

   nBtnLen    := LEN(oBtn:aCap)
   cText      := oBtn:aCap[1]
   nWText     := GetTxtWidth( cText, aFont2[2], aFont2[1] )
   IF nW == 0
      nWBtn   := nWText                                       // ширина кнопки / button width
   ELSE
      nWBtn   := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ширина кнопки / button width
   ENDIF
   oBtn:aWBtn := { nWBtn, nWBtn }                                  // задать ширину кнопки
   oBtn:lVert := .F.                                               // вертикальный текст на кнопке
   oBtn:aFnt  := { aFont2[1], aFont2[2], aFont2[3], oBtn:lVert }   // фонты для всех кнопок

   oBtn:aFntClr := { BLACK, YELLOW }

   aYX := Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN { aYX[1], aYX[2] }

///////////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamDbf(cForm,cAls,cBrw,nWTsb,cTitle,hFont,nHCell,nHSpecHd,aCharColumn)
   LOCAL oTsb, nClr1, nClr2, a, aWSize, aBClr
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   oTsb:cAls           := cAls
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   //oTsb:uSelector    := 20                       // селестор слева таблицы
   //oTsb:aNumber      := { 1, 30 }                // колонка нумерации и её ширина
   //nHCell            := 28                       // высота картинки чекбокса
   oTsb:nHeightCell    := nHCell                   // высота ячеек
   oTsb:nHeightHead    := nHCell                   // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := nHCell                   // высота подвала
   oTsb:nHeightSpecHd  := nHSpecHd                 // высота нумератора
   oTsb:nHeightSuperHd := 0                        // высота суперхидера
   oTsb:lDrawHeaders   := .F.                      // НЕ ставить в таблице шапку !!! Нет такого параметра
   oTsb:lFooting       := .T.                      // ставить в таблице подвал
   oTsb:aFoot          := .T.
   oTsb:lSpecHd        := .T.                      // поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .F.                      // поставить в таблице суперхидер
   oTsb:cSuperHd       := cTitle                   // текст суперхидера
   oTsb:nCellMarginLR  := 0                        // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   oTsb:lNoPicture     := .T.

   // колонки таблицы / table columns
   //                   1     2        3
#ifdef KEY_ENG
   oTsb:aHead    := { "-v-","code","name of events" }
#else
   oTsb:aHead    := { "-v-","код","наименование событий" }
#endif

   oTsb:aName    := { "F_CHK", "F_CODE", "F_NAME" }
   oTsb:aHideCol := {}                                             // нет скрых колонок, учитывать SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidths(aCharColumn,nWTsb,hFont)  // подсчёт ширины колонок
   oTsb:aSize    := aWSize                                         // назначим ширину колонок для ТСБ
   // цвета в таблице / colors in the table
   aBClr               := This.Backcolor
   nClr1               := HMG_RGB2n(aBClr)                  // цвет фона шапка+подвал
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := aBClr                             // цвет фона под таблицей
   oTsb:lZebra         := .T.                               // это вкл.\откл. механизм zebra
   //oTsb:aZebra       := { {230,230,230}, SILVER }         // серый цвет
   oTsb:aZebra         := { {113,244,218}, {9,209,169} }
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
                   //ob:Hide()                                     // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)             // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   //ob:nFreeze     := ob:nColumn("ARRAYNO")        // Заморозить столбцы
                   //ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   //ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                            // ввод в ячейки от букв, цифр
                   //myTsbEditDbf(ob,op)                            // редактирование ячеек таблицы
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   ob:lNoHScroll := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll   := NIL
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nw := 0, nc := ob:nColumn("F_NAME")
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                       IF oc:lCheckBox
                          oc:lEdit     := .T.
                          oc:cPicture  := Nil
                          oc:nAlign    := DT_CENTER
                          oc:nEditMove := 0
                          oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
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
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице --> tsb_util.prg
   //oTsb:aUserKeys := { ;
   //     {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
   //     {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
   //     {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }  ;
   //                  }

   // назначить события на окно
   //oTsb:aEvents   := { ;
   //     {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;
   //     {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;
   //     {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }  ;
   //                  }

RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок
STATIC FUNCTION CalculatColumnWidths(aChar,hFont)
   LOCAL v, a, i, nW, aWSize, nWChar

   nWChar := 0
   aWSize := Array(Len(aChar))
   aFill(aWSize, 0)

   FOR EACH a IN aChar
      i  := hb_enumindex(a)
      v  := REPL("H", a ) + "HH"   // добавка
      nW := GetTextWidth( Nil, v, hFont )
      aWSize[ i ] := nW
      nWChar += nW
   NEXT

   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWChar=",nWChar

RETURN aWSize

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_TSB(oWnd, aWait)          // поиск по базе
   LOCAL oBrw, cVal, cGet
   Default oWnd  := ThisWindow.Object
   Default aWait := .F.

   oBrw := oWnd:Cargo:oBrw
   cGet := oWnd:Cargo:cGet             // это "GB_Find"

   // нельзя срабатывает LOSGFOCUS getbox
   IF !Empty(aWait)
      IF HB_ISLOGICAL(aWait)
         aWait := "Calculation RESULTS..."
         //WaitWindow( aWait, .T. , 600, 16, NIL, BLUE, App.Cargo:aBClrMain )
      ENDIF
   ENDIF

   SET WINDOW THIS TO oWnd
   cVal := Trim( This.&(cGet).Value )
   SET WINDOW THIS TO

   oBrw:FilterFTS( cVal, .T. )         // Empty(cVal) обработка внутри метода

   // нельзя срабатывает LOSGFOCUS getbox
   //IF !Empty(aWait) ; WaitWindow()
   //ENDIF

RETURN .T.

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


////////////////////////////////////////////////////////////////////////
// создадим базу со структурой
STATIC FUNCTION my3SpravDbfNew(cPath, cFile, a2Dim, lDbfDelFile)
   LOCAL aDbf, cFileDbf, cFileIndx, cAlias, nI, cField
   LOCAL aCharColumn, nMax, nKolvo, lNew
   DEFAULT lDbfDelFile := .F.

   nMax := 0
   FOR nI := 1 TO LEN(a2Dim)
      nMax := MAX(nMax, LEN(ALLTRIM(a2Dim[nI,2])))
   NEXT
   nMax += 2

   aDbf := {}
   AAdd( aDbf, {"Check"  , "L",      1, 0 } )
   AAdd( aDbf, {"nCode"  , "N",      5, 0 } )
   AAdd( aDbf, {"cCode"  , "C",   nMax, 0 } )
   aCharColumn := { 2, 5, nMax }      // кол-во символов в колонках

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

RETURN { ALIAS(), aCharColumn }
