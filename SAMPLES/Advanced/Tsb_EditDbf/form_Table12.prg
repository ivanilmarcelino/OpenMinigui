/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
//#include "metrocolor.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION FormTable12(nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, cSuperHd, oClr, oMenu, oColnm)
   LOCAL nH, nW, nG, nX, nY, cTitle, cFont, nFSize, aBackColor
   LOCAL aHide, aForm, cFormCurr, cFormMain, lVsbl, nI, hWnd
   LOCAL oTsb, oBrw, aRet, owc, cBrw

   ? "-->> #### " + ProcNL(), nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, oMenu
   cFont      := App.Cargo:cDefFontName
   nFSize     := App.Cargo:nDefFontSize
   cFormMain  := App.Cargo:cWinMain         // имя окна MAIN формы
   cFormCurr  := ThisWindow.Name            // текущая форма - родительское окно
   aBackColor := oClr:aBClr                 // Цвет фона всей формы
   cTitle     := cTtl + '  (' + App.Cargo:cDisplayMode + ')'
   cTitle     += SPACE(5) + ProcFile()
   nW         := App.Cargo:aDisplayMode[1]  // System.ClientWidth
   nH         := App.Cargo:aDisplayMode[2]  // System.ClientHeight
   //nH       -= GetTaskBarHeight()         // высота Панели задач Desktop
   nG         := IIF(App.Cargo:aDisplayMode[1]<=1440, 10, 15)  // отступ
   nY         := nX := 0
   nY         := _WindowCargo(cFormCurr):nHMain     // высота родительской формы

   // скрыть все окна кроме текущего, пропуская скрытые окна - пример
   aHide := {}
   aForm := HMG_GetForms()
   nI := lVsbl
   FOR nI := 1 TO Len(aForm)
      lVsbl := IsWindowVisible( GetFormHandle( aForm[nI] ) )
      hWnd  := GetFormHandle( aForm[nI] )
      //? nI, aForm[nI], hWnd, lVsbl, cFormMain
      IF aForm[nI] == cFormMain     ; LOOP
      ELSEIF aForm[nI] == cFormCurr ; LOOP
      ELSEIF !lVsbl                 ; LOOP
      ENDIF
      //DoMethod(aForm[nI], "Hide")
      //AADD( aHide, aForm[nI] )
   NEXT

   // проверка на уже созданное окно
   ? ProcNL(), "["+cForm+"]", VALTYPE(cForm)
   If _IsWindowDefined( cForm )
      DO EVENTS
      If IsIconic( GetFormHandle(cForm) )
         _Restore( GetFormHandle(cForm) )
      Else
         DoMethod( cForm, "SetFocus" )
      EndIf
      aRet := { .T., "есть окно " + cForm }
      ? "-->> Return", ProcNL(), "aRet=", HB_ValToExp(aRet)
      RETURN aRet
   EndIf

   aRet := { .F., "нет окна " + cForm }

   DbSelectArea(cAls)
   // при создании TBrowse надо ставить правильный тэг индекса
   // TBrowse его удерживает, если не использовать привязку тэгов к колонкам
   //OrdSetFocus("DOCDTV")   // ставим сразу здесь !!!

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW-nX HEIGHT nH-nY       ;
      TITLE cTitle ICON cIco                                    ;
      WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE             ;
      BACKCOLOR aBackColor  FONT cFont SIZE nFSize              ;
      ON GOTFOCUS  {|| App.Cargo:cFormGotFocus := This.Name }   ; // возврат фокуса на форму
      ON INIT     {|| _wPost(0)  }                              ; // выполняется после инициализации окна
      ON RELEASE  {|| _wSend(90) }                              ; // выполняется перед разрушением окна
      //ON LOSTFOCUS {|| myLangRecoverLost(This.Cargo:aCurrLang) }     // снятие фокуса с формы

      This.Cargo    := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor   := This.BackColor                  // цвет окна
      owc:hWin      := This.Handle                     // хендл этого окна
      owc:Name      := This.Name
      owc:oWin      := This.Object
      owc:ahIcoDel  := {}                              // для удаления хендлов иконок с формы
      owc:nG        := nG                              // отступ от края окна
      owc:cFormCurr := cFormCurr                       // родительское окно
      owc:cBtnEnabled := cBtnEnabled                   // разблокировать кнопку

      nY := 0
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff WIDTH 10 HEIGHT 10 VALUE "" CENTERALIGN VCENTERALIGN INVISIBLE

      /////////////////////// Кнопки вверху формы /////////////////////////////////////
      oMenu:nY       := nY + nG
      oMenu:nX       := nX + nG                   // координаты начала кнопок
      oMenu:nClientW := nW                        // ширина окна куда нужно вместить кнопки
      MenuTopIconButtons( owc, oMenu )            // -> menu_topButton.prg (owc:nG)
      // owc:nHTBar                               // высота кнопок + отступы

      owc:nTsbY := owc:nHTBar
      owc:nTsbX := nG
      owc:nTsbW := nW - nG*2
      owc:nTsbH := nH - owc:nTsbY - nG

      //@ owc:nTsbY, owc:nTsbX  LABEL Label_Tsb WIDTH owc:nTsbW HEIGHT owc:nTsbH VALUE cTitle ;
      //  BACKCOLOR GRAY CENTERALIGN VCENTERALIGN
      /////////////////////// таблица //////////////////////////////////////////////////////////
      cBrw := "Tsb_" + cForm
      oTsb := TsbPatamDbf( nTable, cForm, cAls, cBrw, cSuperHd, owc:nTsbW, oClr, oColnm )
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, cAls, cBrw, owc:nTsbY, owc:nTsbX, owc:nTsbW, owc:nTsbH )
      //
      oBrw:Cargo:nModify := 0         // счётчик изменений - не использую
      This.Cargo:oBrw    := oBrw      // положить на форму

      // Установка событий на это окно
      IF nTable == 1
         Sets_Event2Zaivka()             // -> demo3_1Base.prg
      ELSEIF nTable == 2
         //Sets_Event2Dogovor()          // -> demo3_2Base.prg
      ENDIF

      ON KEY ESCAPE OF &cForm ACTION _wPost(99,cForm)
      ON KEY F1     OF &cForm ACTION NIL

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   ?  ProcNL(), "-->> End " + cForm

   // восстановить скрытые окна / restore hidden windows
   FOR nI := 1 TO Len(aHide)
      //IF _IsWindowDefined(aHide[nI])
      //   DoMethod(aHide[nI], "Show")
      //ENDIF
   NEXT

   ? ProcNL(), "-->> Return","aRet=", HB_ValToExp(aRet)

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatamDbf(nTable, cForm, cAls, cBrw, cSuperHd, nTsbW, oColor, oColnm )
   LOCAL oTsb, nClr1, nClr2, a, nI, nHCell

   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cAls           := cAls
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   oTsb:nTable         := nTable     // номер таблицы - обязательно !!!
   oTsb:nMemoHV        :=  1         // показ одной строки мемо-поля - НЕ РАБОТАЕТ !!!
   oTsb:lNoPicture     := .T.
   oTsb:lFooting       := .T.        // ставить в таблице подвал
   oTsb:lNoPicture     := .T.
   oTsb:lSpecHd        := .F.        // НЕ поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.        // поставить в таблице суперхидер
   oTsb:cSuperHd       := cSuperHd
   oTsb:aFont          := oColnm:aFont
   nI := nTsbW // резерв

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
      oTsb:aFoot       := .T.                            // заполнить подвал
   ENDIF

   //oTsb:uSelector    := 20                                      // не использую
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // колонка нумерации и её ширина
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35
   nHCell              := IIF( nHCell < 32, 32, nHCell )
   oTsb:nHeightCell    := nHCell                            // высота ячеек
   oTsb:nHeightHead    := nHCell                            // высота шапки
   oTsb:nHeightFoot    := nHCell                            // высота подвала

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                               // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := GetFontHeight(oTsb:aFont[4])    // высота нумератора
   ENDIF

   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      oTsb:nHeightSuperHd := nHCell                         // высота суперхидера
   ENDIF

   nClr1 := HMG_RGB2n(oColor:aBClr)                         // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)                                 // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := oColor:aBrush   //{240,240,240}   // цвет фона под таблицей

   // цвета в таблицу
   a := {}
   // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a
   oTsb:lZebra    := oColor:lZebra
   oTsb:aZebra    := oColor:aZebra

   // Назначение всех колонок таблицы / Assigning all columns of a table
   oTsb:aHideCol := {} //{ 4, 5, 6, 7, 8}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   oTsb:aField   := oColnm:aField           // ОБЯЗАТЕЛЬНО для dbf !!!
   oTsb:aHead    := oColnm:aHead
   oTsb:aName    := oColnm:aName
   oTsb:aSize    := oColnm:aSize            // назначим ширину колонок для ТСБ
   oTsb:aPict    := oColnm:aPict            // формат полей, если нужно

   ThisCheckFieldDbf(oTsb:aField)           // проверка полей базы
   // Проверка колонок таблицы
   //  ? "oTsb:aField=", oTsb:aField ; ?v oTsb:aField
   //  ? "oTsb:aName= ", oTsb:aName  ; ?v oTsb:aName
   //  ? "oTsb:aPict=" , oTsb:aPict  ; ?v oTsb:aPict
   //  ? "oTsb:aHead= ", oTsb:aHead  ; ?v oTsb:aHead
   //  ? "oTsb:aFoot= ", oTsb:aFoot  ; ? IIF( IsArray(oTsb:aFoot), HB_ValToExp(oTsb:aFoot), oTsb:aFoot )
   //  ? "oTsb:aSize= ", oTsb:aSize  ; ?v oTsb:aSize
   //  ? "oTsb:aAlign=", oTsb:aAlign //; ?v oTsb:aAlign
   //

   // мои доп. данные по колонкам берем из demo3_1Base.prg
   oTsb:aColPrc  := oColnm:aColPrc     // тип обработки колонки
   oTsb:aFunc1   := oColnm:aFunc1      // функция-1 :bPrevEdit для обработки колонки таблицы
   oTsb:aFunc2   := oColnm:aFunc2      // функция-2 :bPostEdit для обработки колонки таблицы
   oTsb:aBlock   := oColnm:aBlock      // кодовый блок на составные поля и функции
   oTsb:aDecode  := oColnm:aDecode     // для колонки oCol:bDecode
   oTsb:aCol     := oColnm:aCol        // массив колонок таблицы - сохраним ОБЯЗАТЕЛЬНО !!!
   oTsb:aTable   := oColnm:aTable      // положить весь массив таблицы в cargo окна, на всякий случай

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
                   Local oTsb
                   ob:HideColumns( op:aHideCol ,.t.)              // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ORDKEYNO")       // Заморозить столбцы
                   //ob:nFreeze   := ob:nColumn("ADRESPRN")       // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   ob:nMemoHV     :=  1                           // показ одной строки мемо-поля
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nCell       := 3                            // передвинуть курсор
                   WITH OBJECT ob
                     oTsb := op
                     // мои доп. данные по колонкам
                     :Cargo:aColPrc := oTsb:aColPrc     // тип обработки колонки
                     :Cargo:aFunc1  := oTsb:aFunc1      // функция-1 :bPrevEdit для обработки колонки таблицы
                     :Cargo:aFunc2  := oTsb:aFunc2      // функция-2 :bPostEdit для обработки колонки таблицы
                     :Cargo:aTable  := oTsb:aTable      // положить весь массив таблицы в cargo окна, на всякий случай
                     :Cargo:aBlock  := oTsb:aBlock      // кодовый блок на составные поля и функции
                     :Cargo:aDecode := oTsb:aDecode     // для колонки oCol:bDecode
                     :Cargo:lRecINS := .F.              // блокировка клавиши INS
                     :Cargo:lRecDEL := .F.              // блокировка клавиши DEL
                     :Cargo:nTable  := oTsb:nTable      // номер таблицы - обязательно !!!
                   END WITH
                   Column_Init( ob, op )   // меняем поля на блок кода
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   Local oc, i := 0, aBmp, aMsg, bBmpCell, hImg, cImg, cField
                   Local cMsg := "", nHImg := 32
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll    := NIL
                   // замена первой колонки и спецхидера
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         oc:nFAlign     := DT_LEFT
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   // вторая колонка (2) всегда чекбокс
                   oc := ob:aColumns[2]   // "MARK"
                   oc:lEdit     := .T.
                   oc:cPicture  := Nil
                   oc:lCheckBox := .T.
                   oc:nEditMove := 0    // перечитать ячейку
                   IF ob:nHeightCell > 40
                      oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   ELSE
                      oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   ENDIF
                   // третья колонка (3) для поля - смена иконки
                   IF ob:Cargo:nTable == 1        // -> demo3_1Base.prg
                      cField := "KZBID"
                      aBmp := {"bFWord32","bFExcel32","bFCalc32","bFText32","bFCSV32","bFZero32"}
                      aMsg := {"File MS Word", "File MS Excel", "File OO Calc", "File *.txt",;
                                      "File *.csv", "Delete value" }
                   ELSEIF ob:Cargo:nTable == 2    // -> demo3_2Base.prg
                   ENDIF
                   cField := "KZBID"
                   nI := ob:nColumn(cField, .T.)
                   IF nI > 0
                      oc := ob:aColumns[nI]
                      oc:Cargo := oHmgData()         // создадим контейнер на колонку
                      oc:Cargo:cField := cField
                      oc:Cargo:aBmp   := aBmp
                      oc:Cargo:aMsg   := aMsg
                      oc:nClrBack  := CLR_WHITE
                      oc:lEdit     := .T.
                      oc:nEditMove := 0              // перечитать ячейку
                      oc:lBitMap   := .T.            // убрать показ значений поля из колонки
                      //oc:nWidth := nHImg           // ширина колонки как у картинки - задано в oTsb
                      oc:aBitMaps := {}
                      For i := 1 To Len(aBmp)
                         cImg := aBmp[i]
                         hImg := LoadImage(cImg,,nHImg,nHImg)
                         AAdd( oc:aBitMaps, hImg )
                         If hImg == 0
                            cMsg += "No image ["+cImg+"] in resources;"
                         Endif
                      Next
                      If Len(cMsg) > 0
                         cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
                         AlertStop(cMsg,,,64,{RED})
                      Endif
                      bBmpCell := {|nc,ob| // показ картинки в зависимости от поля "K????"
                                    Local ocol  := ob:aColumns[nc]
                                    Local ni    := 0                      // bFZero32
                                    Local nMax  := LEN(ocol:aBitMaps)     // bFZero32
                                    Local nCode := ob:GetValue(ob:Cargo:cField)   // колонка коды типа файлов
                                    //? ProcName(), nCode, ocol:cName, ocol:cField
                                    //nCode := FIELDGET(FIELDNUM(ob:Cargo:cField))  // можно и так
                                    IF !IsNumeric(nCode)
                                       nCode := 0
                                    ENDIF
                                    IF nCode <= 0 .OR. nCode >= nMax
                                       ni := nMax
                                    ELSE
                                       ni := nCode
                                    ENDIF
                                    Return ocol:aBitMaps[ni]  // картинку с позиции массива
                                    }

                      oc:uBmpCell := bBmpCell  // блок-код смены картинок
                      oc:nAlign   := nMakeLong( DT_CENTER, DT_CENTER )
                      oc:nHAlign  := DT_CENTER
                      //oc:bData  :=  {||Nil}
                      //oc:cData  := '{||Nil}'
                      // редактирование колонки (3)
                      oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }  // -> см.ниже
                   ENDIF
                   //
                   // поставим в подвал
                   // смена курсора таблицы / change table cursor
                   // cFooting := Eval( oColumn:cFooting, nCol, oBrw )
                   oc := ob:GetColumn("ADRESPRN")
                   oc:nFAlign  := DT_LEFT
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   nc := ""
                                   If ob:nLen > 0
                                      nc := hb_ntos(na)+ "/" + hb_ntos(nl)
                                      nc += Space(5) + " [!]"
                                   EndIf
                                   Return nc
                                   }
                   ob:bChange := {|ob|  _wPost(19, ob:cParentWnd, ob) } // при смене курсора таблицы
                   //
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   // назначим клавиши в таблице
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_RETURN, {|ob|
                      Local oc := ob:aColumns[ ob:nCell ]
                      Local xval, lRet
                      IF oc:cName == "MARK"
                      ELSEIF oc:cName == "KZBID"
                         //oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }
                         xval := ob:GetValue(ob:nCell)
                         lRet := EVal(oc:bPrevEdit, xval, ob )
                      ELSE
                        _wPost(40, ob:cParentWnd, ob)
                      ENDIF
                      Return Nil
                      } } }

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по списку колонок
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по фонтам таблицы
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по строке таблицы
        {40, {|ow,ky,ob| _wPost(40,ow)  , ky:=ow:=ob                       } }, ;   // карточка
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

//////////////////////////////////////////////////////////////////////////
// меняем поля на блок кода по колонкам
STATIC FUNCTION Column_Init( ob, op )
   Local oCol, aDim, nI, nJ, nO, nS, bBlock, aBlock, cStr, nMax
   Local aColPrc, aFunc1, aFunc2, aDecode, bDecode, cMsg

   ? "### oTsb:bInit - "+ProcNL()
   // моя добавочная обработка
   aDim    := op:aCol     // весь массив таблицы
   aBlock  := op:aBlock   // блок кода
   aColPrc := op:aColPrc  // тип обработки колонки: "BMP", "K", "S", "C", "N", "D"
   aFunc1  := op:aFunc1   // функция-1 :bPrevEdit для обработки колонки таблицы
   aFunc2  := op:aFunc2   // функция-2 :bPostEdit для обработки колонки таблицы
   aDecode := op:aDecode  // для колонки oCol:bDecode
   IF !IsArray(aDim)
      cMsg := "Error! No table column array !;;"
      cMsg += "oCol:aCol := aCol"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN NIL
   ENDIF

   nMax := 0
   nO   := IIF( ob:nColumn("ORDKEYNO", .T.) > 0, 1, 0) // проверка поля, если нет, то будет 0
   nS   := IIF( ob:lSelector, 1, 0 )   // если есть/нет селектор
   nJ   := nO + nS
   ?? "ORDKEYNO:", nO, "lSelector:", nS

   FOR EACH oCol IN ob:aColumns
      cStr := ATREPL( CRLF, oCol:cHeading, "|" )
      nMax := MAX( nMax, LEN(cStr) )
   NEXT

   // меняем поля на блок кода и делаем bDecode
   FOR EACH oCol IN ob:aColumns
       nI := hb_EnumIndex(oCol)
       ? nI, oCol:cName, PADR( ATREPL( CRLF, oCol:cHeading, "|" ), nMax ) + ","
       ?? oCol:cField, "FieldPos()=", FieldPos(oCol:cName), "nW=",oCol:nWidth
       oCol:Cargo := oHmgData()
       oCol:Cargo:cName  := oCol:cName
       oCol:Cargo:lTotal := .F.
       oCol:Cargo:nTotal :=  0                 // итог по колонке
       IF nI <= ob:nColumn("ORDKEYNO") ; LOOP
       ENDIF
       nJ := nI - nO // учитываем колонку ORDKEYNO
       IF nJ > 0
          bBlock := aBlock[nJ]
          ?? "nJ=", nJ
          IF ISBLOCK(bBlock)
            ?? "bBlock=" , bBlock
            ?? aDim[nJ,8]
            //oCol:bData  := &(bBlock)
            oCol:bData    := bBlock
            oCol:nAlign   := DT_LEFT
            //nLen        := LEN(aDim[nJ,6])  // ширина поля - не использую
            //oCol:nWidth := oCol:ToWidth( REPL("a",nLen) ) - уже сделано
            //oCol:cPicture := aDim[nJ,7]       // формат поля
            //oCol:bDecode  := {|cv| Alltrim(cv) }
            //oCol:l3DLook   := .T.
            oCol:l3DTextCell := .T.
            //oCol:nClr3DLCell := CLR_RED
          ENDIF
          //?? VALTYPE(bBlock), bBlock
          bDecode := aDecode[nJ]
          IF ISBLOCK(bDecode)
             ?? "bDecode=", bDecode
             oCol:bDecode := bDecode
          ENDIF
          //oCol:cFooting := ""
       ENDIF
   NEXT

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ColumnEditPrev_Two(xVal, oBrw)
   LOCAL nCol, oCol, cFld, cAls, cTyp, cNam, lIco, aRet, xOld, cMsg, lModify
   LOCAL aBmp, aMsg, nTime

   nCol  := oBrw:nCell
   oCol  := oBrw:aColumns[ nCol ]
   cAls  := oBrw:cAlias
   cTyp  := oCol:cFieldTyp
   cFld  := oCol:cField
   cNam  := oCol:cName
   xOld  := xVal                 // сохраним предыдущее значение
   nTime := VAL( SUBSTR(TIME(),1,2) + SUBSTR(TIME(),4,2) )
   // прочитаем из контейнера колонки
   aBmp  := oCol:Cargo:aBmp
   aMsg  := oCol:Cargo:aMsg
   IF !IsArray(aBmp) .OR. !IsArray(aMsg)
      cMsg := "Error! NO arrays in column container!;;"
      cMsg += "oCol:Cargo:aBmp = ???;"
      cMsg += "oCol:Cargo:aMsg = ???"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN .F.
   ENDIF

   //MsgDebug(nCol, oCol, cFld, cAls, cTyp, cNam)
   lIco := .F.   // BMP type
   aRet := Tsb_ContexMenuTwo(oBrw,aMsg,aBmp,lIco)  // -> см.ниже
   IF LEN(aRet) > 0
      xVal := aRet[1]
      //(cAls)->&cFld := xVal               // код в базу
      IF (cAls)->(RLock())                  // делать самому
         oBrw:SetValue(cNam, xVal)          // запись в поле
         //(cAls)->KOPERAT   := M->nOperat  // кто правил запись
         (cAls)->DATEVVOD  := DATE()        // дата правки
         (cAls)->TIMEVVOD  := nTime         // 9999 время правки
         IF ( lModify := xOld != xVal )     // modify value
            oBrw:Cargo:nModify ++           // была модификация таблицы
            //запись в журнал-действий-пользователей-программы
            //write to the program-user-actions-log
         ENDIF
      ELSE
         cMsg := "Запись заблокирована !;"
         cMsg += "Recno blocked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop(cMsg,,,64,{RED})
      ENDIF
      (cAls)->(dbUnLock())
      oBrw:Skip(0)
      oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы

   ENDIF

RETURN .F.

//////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisCheckFieldDbf(aNames)
   LOCAL cTmp, n, nE, cMsg, cAls, nCols

   cAls  := ALIAS()
   cTmp  := ""

   IF aNames == NIL
      cMsg := "Error! No list of DB fields: " + cAls + ";;"
      cMsg += "aNames == NIL"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN NIL
   ENDIF

   nCols := Len( aNames )

   FOR n := 1 TO nCols
      nE := ( cAls )->( FieldPos( aNames[ n ] ) )
      IF nE == 0
         cTmp += HB_NtoS(n) + ". " + aNames[ n ] + ";"
      ENDIF
   NEXT
   IF LEN(cTmp) > 0
      cMsg := "Error! No fields in DB: " + cAls + ";;" + cTmp
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_ContexMenuTwo(oBrw, aDim, aImg, lIcon, lDopMenu)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, aRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, nMenu, aMsg
   LOCAL aFont, nFSize, cName, nWCell, nHCell, oCell, cImg
   DEFAULT lIcon := .T.   // иконки в меню, иначе BMP
   DEFAULT lDopMenu := .F.

   cForm  := oBrw:cParentWnd
   oWnd   := _WindowObj(oBrw:cParentWnd)
   // координаты ячейки в которой Edit
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oWnd:Row + oCell:nRow - 2 //+ GetTitleHeight() /*+ GetMenuBarHeight()*/
   //nY   += oBrw:nTop + IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   //nY   += oBrw:nHeightHead
   nX     := oWnd:Col + oCell:nCol + 2
   nWCell := oCell:nWidth - 2
   nHCell := oCell:nHeight - 2

#ifdef KEY_ENG // for this project demo1-en.hbp
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("ComSanMS")
   aMsg   := { "Delete value", "Exit" }
#else
   hFont1 := GetFontHandle("Normal")     // Фонт колонок таблицы
   hFont2 := GetFontHandle("FntBtn_1")   // Фонт-1 кнопок других форм
   aMsg   := { "Удалить значение", "Выход" }
#endif

   aFont  := GetFontParam(hFont1)
   nFSize := aFont[2]
   nMenu  := 0

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cMenu   := aDim[nI]
         cImg    := aImg[nI]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         hFont   := IIF( lDis, hFont2, hFont1 )

         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      IF lDopMenu
         SEPARATOR
         MENUITEM  aMsg[1] ACTION  {|| nMenu := -1 } FONT hFont2  ICON "iDelVal32"
         SEPARATOR
         MENUITEM  aMsg[2]  ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
      ENDIF
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      aRet := { nMenu, aDim[nMenu] }
   ELSEIF nMenu == -1
      aRet := { 0, "-.-" }
   ELSE
      aRet := {}
   ENDIF

   DO EVENTS

RETURN aRet

