/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
///////////////////////////////////////////////////////////////////
STATIC FUNCTION Menu_Tab(aBColor)
   LOCAL hFont, aFont, oMenu := oHmgData()
   DEFAULT aBColor := GRAY

   hFont := GetFontHandle('ItalBold')
   //aFont := GetFontParam(hFont)
   aFont := { App.Cargo:cFontName2 , App.Cargo:nBtnFontSize, .T. } // "Comic Sans MS" nSize + 4
   // имя объекта + имя события
   oMenu:aObj  := { "_3Tab1"  , "_3Tab2" , "_3Tab3" , "_3Tab4" , "_3Tab5" , "_3Tab6"  }
   oMenu:aImg  := { {NIL,NIL} , {NIL,NIL}, {NIL,NIL}, {NIL,NIL}, {NIL,NIL}, {NIL,NIL} }
                //      1                 2                     3              4                 5            6
   oMenu:aMnRu  := { "Tab-1"        , "Tab-2"        , "Tab-3"        , "Tab-4"        , "Tab-5"        , "Tab-6"         }
   oMenu:aHelp  := { "Data on tab-1", "Data on tab-2", "Data on tab-3", "Data on tab-4", "Data on tab-5", "Data on tab-6" }
   oMenu:a1BClr := { {159,191,236}  , {251, 230, 148}, {255, 178, 178}, {214,166,242}   , {195,224,133} , {249,193,71}    }
   //oMenu:aMnEn := { "Help"  , {"Search;user"    ,"Clear;search"  }, {"Print" ,"Notepad" }, "Exit", "Exit", "Exit" }
   oMenu:aCapt    := oMenu:aMnRu //IIF( App.Cargo:cLang == "RU", oMenu:aMnRu, oMenu:aMnEn )
   oMenu:nHIco    := 30    //IIF( App.Cargo:aDisplayMode[2] <= 900, 48, 64 )  // высота-ширина иконки на кнопке
   oMenu:nHG2     := 5     // добавочная высота к тексту кнопки
   oMenu:aBtnFClr := { BLACK, RED, BLACK, YELLOW     }           // цвет фонта кнопки + 2/3/4-цвет инвертный
   //oMenu:aBtnBClr := { {66,92,251} , WHITE, YELLOW, GRAY }     // цвет фона кнопки + 2/3/4-цвет инвертный
   oMenu:aBtnBClr := { aBColor , WHITE, YELLOW, GRAY }            // цвет фона кнопки + цвет инвертный
   oMenu:aBtnFont := { aFont[1], aFont[2], aFont[3]  }            // фонт на кнопках
   //oMenu:aBtnFont  := { "Tahoma", aFont[2]+2, .T.  }  // фонт на кнопках - можно и так задать
   oMenu:nX        := 0
   oMenu:nY        := 0
   oMenu:lAutoSize := .T.        // T - автоматический расчёт высоты и ширины кнопки от высоты иконки
   oMenu:nWBtn     := 0          // ручное задание ширины кнопки
   oMenu:nHBtn     := 0          // ручное задание высоты кнопки
   oMenu:lVert     := .F.        // НЕ вертикальный текст на кнопке
   oMenu:nClientW  := 0          // ширина окна
   //oMenu:lAutoSize := .F.      // F - ручное задание
   //oMenu:nWBtn   := 120        // ручное задание ширины кнопки
   //oMenu:nHBtn   := 100        // ручное задание высоты кнопки
   oMenu:lGradient := .T.        // градиент на кнопках - включить
   oMenu:lGradient := .F.        // НЕТ градиента на кнопках

RETURN oMenu

//////////////////////////////////////////////////////////////////////////
FUNCTION Card_6Tsb(oWnd,aWinBClr,aAdr,cTtl,lZaEdit)
   LOCAL nH, nW, nG, nX, nY, oMenu, lNoSave, cVal, cObj, nHBtn, aBClr
   LOCAL cForm, owc, nCapt, cPost, aTbl, nI, aObj, aRet, aXdim, aSupHd
   LOCAL cTitle, aName, aHead, nYTsb, nXTsb, nWTsb, nHTsb, oTsb, oXBrw
   LOCAL cIni, cAls, aUser, aVal, nJ, aDim
   DEFAULT lZaEdit := .T.

   ? "-->>", ProcNL(), oWnd, aWinBClr, aAdr
   ? ProcNL(), Za_VipZa()
   //? "    >" ; ?v aAdr
   cForm  := oWnd:Name
   owc    := oWnd:Cargo
   cAls   := owc:cAls
   aTbl   := owc:aTbl           // координаты таблицы
   nG     := owc:nG
   nY     := aTbl[1]
   nX     := aTbl[2]
   nW     := aTbl[3]
   nH     := aTbl[4]
   /////////////////////// Кнопки под адресом абонента /////////////////////////////////////
   oMenu          := Menu_Tab(owc:aBClr)      // menu-tab options
   oMenu:nY       := nY
   oMenu:nX       := nX                       // координаты начала кнопок
   oMenu:nHBtn    := 40                       // ручное задание высоты кнопки
   oMenu:nG       := 1                        // ручное задание
   oMenu:nClientW := nW                       // ширина окна
   lNoSave        := .F.                      // нет записи массива кнопок на форме,
                                              // иначе будет переприсвоение ранее выведенному массиву
   cPost          := "_3Tab0"                 // одно событие на всех кнопках
   aObj           := oMenu:aObj               // { "_3Tab1", "_3Tab2", "_3Tab3", "_3Tab4", "_3Tab5", "_3Tab6" }

   cObj := aObj[1]
   IF _IsControlDefined(cObj, cForm)
      FOR nI := 1 TO LEN(aObj)
         DoMethod(cForm, aObj[nI], "Show")
         ? "    >", nI, cForm, aObj[nI], "Show"
      NEXT
   ELSE
      FOR nI := 1 TO LEN(aObj)
         AADD( owc:aObjHide, aObj[nI] )   // для сокрытия потом
      NEXT
      // вывод меню
      MenuTopIconButtons( owc, oMenu,, lNoSave, oMenu:nG, cPost )  // -> menu_topButton.prg (owc:nG)
      DO EVENTS
   ENDIF

   // выводим таблицы
   nHBtn   := owc:nHBtn2
   nCapt   := LEN(oMenu:aMnRu)
   aSupHd  := oMenu:aHelp
   // таблицы строятся только здесь
   owc:aTsbDel := {}       // обнулим массив
   owc:aTsbIni := {}       // настройки сортировки строк в таблице
   owc:aWrtIni := {}       // настройки сортировки строк в таблице для записи
   owc:aBrw    := {}
   owc:nBrw    := 0        // нет тсб в фокусе (кнопкм не нажаты)

   FOR nI := 1 TO nCapt

      cObj   := "Label_Table" + HB_NtoS(nI)
      cVal   := CRLF + "Здесь таблица - " + oMenu:aMnRu[nI] + CRLF
      cVal   += "Номер таблицы = " + HB_NtoS(nI)
      aBClr  := oMenu:a1BClr[nI]
      cTitle := cTtl + aSupHd[nI]

      //@ aTbl[1] + nHBtn, aTbl[2] LABEL &cObj PARENT &cForm WIDTH aTbl[3] HEIGHT aTbl[4] - nHBtn ;
      //  VALUE cVal SIZE owc:nFSize*2 BACKCOLOR aBClr CENTERALIGN //VCENTERALIGN

      cObj  := "cTable_" + HB_NtoS(nI)
      cIni  := App.Cargo:cPathTemp + "6" + cObj + ".ini"  // настройки сортировки строк в таблице
      aRet  := LoadDimZaivCard(nI)                // считать массивы   -> card_array_zaiv.prg
      aXdim := aRet[1]                            // данные таблицы
      aName := aRet[2]                            // имена полей таблицы
      aHead := aRet[3]                            // шапка таблицы
      aXdim := ArrayDbfLoad(aXdim,cAls,cIni,owc:aWrtIni,nI)  // загрузить данные в таблицу из базы
      /////////////////////// таблица //////////////////////////////////////////////////////////////
      nYTsb := aTbl[1] + nHBtn
      nXTsb := aTbl[2]
      nWTsb := aTbl[3]
      nHTsb := aTbl[4] - nHBtn
      oTsb  := TableParam2( cForm, aXdim, cObj, aName, aHead, nWTsb, aBClr, cTitle)
      oTsb:lZaEdit := lZaEdit                      // ВАЖНО !!!
      // ? _o2log(oTsb, 27, ProcNL() + "  oTsb => ", .T. ) // проверка
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oXBrw := _TBrowse( oTsb, aXdim, cObj, nYTsb, nXTsb, nWTsb, nHTsb )
      oXBrw:Cargo:nBrw     := nI
      oXBrw:Cargo:nCellFoc := 3                            // фокусная ячейка
      oXBrw:Cargo:nModify  := 0                            // счётчик изменений
      oXBrw:Cargo:lChkTsb  := .F.                          // правка настроек строк таблицы
      oXBrw:Cargo:dZDate   := (cAls)->DateZa               // Дата заявки ! ВАЖНО ! Используется далее в коде
      oXBrw:Cargo:aIsxTbl  := oXBrw:aArray                 // ИСХОДНЫЙ массив ВАЖНО !!!
      //
      AADD( owc:aBrw   , oXBrw )  // массив объектов таблицы
      AADD( owc:aTsbDel, cObj  )  // для удаления 5-таблиц - первоначальный вариант
      AADD( owc:aTsbIni, cIni  )  // настройки сортировки строк в таблице

      // включить сортировку юзера
      //oXBrw:Hide()
      aUser := {}
      aDim  := oXBrw:aArray
      aDim := ASORT( aDim,,, { |x, y| x[ADIM_SORT] < y[ADIM_SORT] } )
      FOR nJ := 1 TO LEN(aDim)
         aVal := aDim[nJ]
         IF aVal[ADIM_SORT] # 0
            AADD( aUser, aVal )
         ENDIF
      NEXT
      If Len(aUser) == 0
         aVal    := ACLONE(aDim[1])
         aVal[1] := "Нет строк таблицы для показа !"
         aVal[2] := "Исправьте сортировку показа через (3) столбец !"
         AADD( aUser, aVal )
         AADD( aUser, aVal )
      Endif
      // далее заместить массив ТСБ на новый !!!
      oXBrw:aArray := aUser
      oXBrw:nLen   := LEN(aUser)
      oXBrw:Reset()
      oXBrw:Refresh()
      //oXBrw:Show()
      oXBrw:Setfocus()

      //IF nI > 1
      //   DoMethod(cForm, cObj, "Hide")
      //   ? nI, cObj, "Hide"
      //ENDIF
      DO EVENTS
   NEXT
   This.Cargo:nBrw := 1  // (тек. тсб в фокусе)

   oXBrw := owc:aBrw[1]
   oXBrw:Show()

   CalcMemoLine(oXBrw)  // размеры меняются после перезагрузки / the sizes change after reboot
                        // for function MEMOLINE(xxxx,App.Cargo:nMemoChar,1)

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TableParam2(cForm,aXDim,cBrw,aName,aHead,nWTsb,aBClr,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nI, aWSize, cT, nHCell

   cT := space(3) + Alltrim(cTitle) + space(3)
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // колонка нумерации и её ширина
   oTsb:nHeightCell    := nHCell                   // высота ячеек
   oTsb:lDrawHeaders   := .F.                      // !!! Нет такого параметра
   oTsb:nHeightHead    := 1                        // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := nHCell                   // высота подвала
   oTsb:lFooting       := .T.                      // ставить в таблице подвал
   oTsb:lSpecHd        := .F.                      // НЕ поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.                      // поставить в таблице суперхидер
   oTsb:cSuperHd       := cT                       // текст суперхидера
   oTsb:nHeightSuperHd := nHCell - 6               // высота суперхидера
   oTsb:nCellMarginLR  := 0                        // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   //oTsb:uSelector    := 20                       // селестор слева таблицы
   oTsb:lNoPicture     := .T.
   ? ProcNL(), "######## nHCell=", nHCell
   oTsb:aName := aName
   oTsb:aHead := aHead
   // можно задать просто цифры в шапке таблицы
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
   oTsb:aSize    := aWSize                                // назначим ширину колонок для ТСБ

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

   nClr1               := HMG_RGB2n(aBClr)                  // цвет фона шапка+подвал
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   //oTsb:aBrush       := {240,240,240}                     // цвет фона под таблицей
   oTsb:aBrush         := aBClr                             // цвет фона под таблицей

   // цвета в таблицу
   oTsb:lZebra    := .F.                                    // это вкл.\откл. механизм zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // серый цвет
   oTsb:aZebra    := { {251,227,227}, {207,152,149} }
   oTsb:aBZebra   := {RGB(251,227,227), nClr1 /*RGB(255,178,178)*/ ,;
                      CLR_HRED, CLR_MAGENTA, CLR_YELLOW }   // тут можно больше цветов
   oTsb:nBZebraNo := 3
   oTsb:aFZebra   := {CLR_BLACK, CLR_HBLUE, CLR_BLUE, CLR_RED, CLR_MAGENTA}  // для текста ячеек
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
                        ELSEIF cTyp == "M"          ; nClr := CLR_MAGENTA
                        ELSEIF cTyp == "CALC"       ; nClr := CLR_BLUE
                        ELSEIF cTyp == ""           ; nClr := CLR_HRED
                        ELSEIF "LINE" $ cTyp        ; nClr := CLR_WHITE
                        ELSE                        ; nClr := CLR_BLACK
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

   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы

   // --- не надо так, сделано выше    oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }
   //AAdd(a, { CLR_SUPF , CLR_WHITE              })  // 16, фона суперхидера
   //AAdd(a, { CLR_SUPB , CLR_RED                })  // 17, текста суперхидера

   //AAdd(a, { CLR_SPCF , CLR_YELLOW             })  // 18, specheader text - нумератор
   //AAdd(a, { CLR_SPCB , { nClr1, nClr2 }       })  // 19, specheader back - нумератор
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
                                       LoadImage("bAttach24x2")  }
                   // редактирование ячеек таблицы -> см. ниже
                   //MsgDebug(op:lZaEdit, ob:Cargo:oParam:lZaEdit)
                   IF op:lZaEdit
                      myTsbEdit(ob,op)
                   ENDIF
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lPickerMode := .F.
                   oc := ob:aColumns[3]
                   oc:lEdit     := op:lZaEdit
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nAlign    := DT_LEFT
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
                   oc:cFooting     := "настройка строк таблицы"
                   oc:uBmpFoot     := ob:aBitMaps[3]  // [3] картинка в подвале колонок таблицы
                   oc:nFAlign      := nMakeLong( DT_LEFT, DT_LEFT  )
                   oc:nBmpMaskFoot := 0x00BB0226    // MERGEPAINT
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(42, ob:cParentWnd, ob) }
                   oc := ob:aColumns[4]
                   oc:uBmpFoot     := ob:aBitMaps[2]  // [2] картинка в подвале колонок таблицы
                   oc:nFAlign      := nMakeLong( DT_CENTER, DT_CENTER )
                   oc:nBmpMaskFoot := 0x00BB0226  //0x00CC0020    // SRCCOPY
                   oc:bFLClicked   := {|p1,p2,nr,ob| p1:=p2:=nr, _wPost(43, ob:cParentWnd, ob:aColumns[4]:cName) }
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
                   // поставить картинки на строки таблицы
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
                                        ENDIF
                                     ENDIF
                                     IF nn > 0 ; hBmp := ob:aBitMaps[ nn ]
                                     ENDIF
                                     nn := nc
                                     Return hBmp
                                     }
                   oc:nAlign := DT_LEFT
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nw := 0, nn
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       IF oc:cName == "ARRAYNO"
                          nn := oc:nWidth
                          // вот здесь делаем уменьшение ширины колонки
                          oc:nWidth := GetTextWidth( Nil, "0000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                    NEXT
                    IF !Empty(nn)
                       //oc := ATail(ob:aColumns)
                       oc := ob:GetColumn("F_NAME")
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    // можно так
                    //ob:UserKeys(VK_F2 ,  {|ob| myTsbListColumn( ob ), ob:Setfocus() })  // инфо по списку колонок
                    //ob:UserKeys(VK_F3 ,  {|ob| myTsbListFont( ob )  , ob:Setfocus() })  // инфо по фонтам таблицы
                    //ob:UserKeys(VK_F4 ,  {|ob| myTsbArrayLine( ob ) , ob:Setfocus() })  // инфо по строке таблицы
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
           IF i > 3
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

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, cFunc, cVal, nVal
   LOCAL cTyp, cMsg, lWrt, cStr, aVal, aDim14, aRet, xDop15, a2Dim, aCode
   LOCAL cAccess, aDim, aVal13, aText, cMemo, a3Dim, cErr, nAt, nI, cFld
   LOCAL lYes, cText, nJ, cCol5, aFld2, hWin

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
      aRet := Tsb_ContexMenu(ob,aDim,xDop15)  // tsb_EditWindows.prg
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
      hWin := _WindowObj(ob:cParentWnd):Handle
      SET WINDOW THIS TO ob:cParentWnd
      //Darken2Open(hWin)              // затенение окна поставить
      ? ProcNL(), cJTyp, HB_ValToExp( ob:aArray[ob:nAt][13] )
      DO EVENTS
      IF UPPER(cFunc) = "MYWINCALC()"
         aRet := Tsb_myWinCalc(ob,aDim14,xDop15,aVal)       // tsb_EditWindows.prg
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
         aRet := Tsb_myWinCalc2(ob,aDim14,xDop15,aVal)        // tsb_EditWindows.prg
         IF LEN(aRet) > 0
            aVal := aRet[1]
            cVal := aRet[2]
            uVal := cVal
            ob:aArray[ob:nAt][ACOL_2]  := cVal
            ob:aArray[ob:nAt][ACOL_13] := aVal                // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S
            ob:aArray[ob:nAt][ACOL_10] := HB_ValToExp(aVal)   // преобразование в "C" колонки (13)
            lWrt := .T.                                       // записывать в ячейку
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

      ELSEIF UPPER(cFunc) = UPPER("ZaListNeis()")             // -> tsb_EditWindows.prg
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
            cErr := "Ошибка ! Колонка [ACOL_13] не строка !;"
            cErr += "[" + VALTYPE(aVal13) + "];;"
            ? ProcNL() , ATREPL( ";", cErr, CRLF )
            ?? VALTYPE(aVal13), aVal13
            ? HB_ValTOExp(aVal13)
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

      ELSE
         MsgDebug("ERROR! Нет обработки функции !",cFunc)
      ENDIF
      ? "###### " + ProcNL(), cJTyp,"[13]=" ,HB_ValToExp( ob:aArray[ob:nAt][ACOL_13] )
      //Darken2Close(hWin)        // затенение окна убрать
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

