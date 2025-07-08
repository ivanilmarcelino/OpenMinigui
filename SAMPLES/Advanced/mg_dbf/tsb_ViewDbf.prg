/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Создание/показ TBrowse (ТСБ) / Creation/display of TBrowse (TSB)
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
///////////////////////////////////////////////////////////////////////////
FUNCTION Tsb_ViewDbf(aUse,oWnd)
   LOCAL owc, cForm, oTsb, aColor, oBrw, cBrw, cSuperHd, cAls, cMsg, ao

   ? ProcNL(), aUse, oWnd, "|", HB_ValToExp(aUse)
   cForm := oWnd:Name
   owc   := oWnd:Cargo
   oTsb  := owc:oTsb            // координаты и цвета таблицы
   cAls  := ALIAS()
   cBrw  := "Tsb1_" + cForm
   ao    := App.Cargo
   ? "   cBrw=", cBrw, cForm, "cAls=",cAls

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

   aColor := oTsb:a4Clr    // все цвета таблицы  - сделана в главном модуле
   /////////////////////// таблица ///////////////////////////////////////////////////
   //oTsb := oHmgData() - сделана в главном модуле
   //                      cell     Head   foot      SpecHider  SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }
   oTsb:uSelector   := 20
   oTsb:lSpecHd     := .T.           // поставить в таблице нумератор
   oTsb:lFooting    := .T.           // поставить в таблице подвал
   oTsb:aFoot       := .T.
   oTsb:nHeightFoot := 25            // высота подвала
   oTsb:nHeightHead := 25            // высота шапки
   oTsb:aEdit       := .T.           // редактировать колонки
   oTsb:aBrush      := aColor[3]                       // цвет фона под таблицей
   oTsb:aColor      := Color_Tsb(aColor,oTsb)          // цвета таблицы: 2(шапка+подвал),3(строка %1),4(строка %2)
   oTsb:cTtlSupHead := SuperHider()

   // блоки кода для _TBrowse(...) - менять нельзя // (op=oTsb)
   oTsb:bInit  := {|ob,op| myTsbInit(ob,op), myTsbFont(ob,op), myTsbSuperHd(ob,op) }  // настройки тсб  -> см.ниже
   oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)  }  // другие настройки тсб  -> см.ниже
   //oTsb:bEnd := {|ob,op| myTsbEnd(ob,op)                      }  // блок кода после END TBROWSE -> см.ниже

   _o2log(oTsb, 15, ProcNL()+" -------------- Параметры объекта : => oTsb", .T.)

   // ------ вариант 1
   /* SET WINDOW THIS TO oWnd  // переключиться на главное окно с ТСБ
   // ОБЯЗАТЕЛЬНО здесь это делаем !!!
   // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
   oBrw := _TBrowse( oTsb, cAls, cBrw, oTsb:nY, oTsb:nX, oTsb:nW, oTsb:nH )
   SET WINDOW THIS TO oWnd */

   // ------ вариант 2
   oTsb:cForm     := oWnd:Name    // <--- обязательно так !!!
   oTsb:cFormName := oWnd:Name    // или так
   ? ProcNL(), cAls, ALIAS(), "cBrw=",cBrw
   // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
   oBrw := _TBrowse( oTsb, cAls, cBrw, oTsb:nY, oTsb:nX, oTsb:nW, oTsb:nH )
   // последние действия с ТСБ
   myTsbEnd(oBrw,oTsb)                  // <<----- так правильней

   // здесь можно запоминать переменные для oBrw:Cargo:XXXX и потом их использовать в ТСБ
   oBrw:Cargo:aFont  := oTsb:aFont
   oBrw:Cargo:cCdPg  := oWnd:Cargo:cCdPg    // положим на ТСБ CodePage файла
   //App.Cargo:oBrw  := oBrw                // запомнили для внешних функций - здесь НЕ НАДО
   oWnd:Cargo:oBrw   := oBrw                // запомнили на окне
   oWnd:Cargo:cBrw   := cBrw                // запомнили на окне
   oBrw:Cargo:ObjWnd := oWnd                // текущее окно
   oBrw:Cargo:cSprHd := cSuperHd

RETURN oBrw

//////////////////////////////////////////////////////////////////
STATIC FUNCTION SuperHider(cCodePage)                   // Super Hider
   LOCAL cSuperHd, nOrder, cOrder, lModeUse, cFile
   LOCAL cRddDbf, cCdPg, cPsw, cSetDel, aUse
   DEFAULT cCodePage := ""

   aUse     := App.Cargo:aUse
   cFile    := aUse[1]
   lModeUse := aUse[2]
   cRddDbf  := aUse[3]
   cCdPg    := IIF( LEN(cCodePage)==0, aUse[4], cCodePage )
   cPsw     := aUse[5]
   cSetDel  := aUse[6]
   cSuperHd := "HB_LANGSELECT()= " + HB_LANGSELECT() + SPACE(5)
   cSuperHd += "VIA: " + cRddDbf + ", CODEPAGE: " + cCdPg

   IF cRddDbf == "DBFNSX"
      cSuperHd += ", PASSWORD: [" + cPsw + "]"
   ENDIF
   cSuperHd  += ", SET_DELETED: " + IIF( Set(_SET_DELETED), "OFF", "ON" )
   cSuperHd  += ",  USE_MODE: " + my_UseMode() + ",  ALIAS: " + ALIAS()
   cSuperHd  += ",  ORDER: ["
   IF OrdCount() > 0
      nOrder := INDEXORD()
      cOrder := HB_NtoS(nOrder) + "/" + OrdName(nOrder)
      cSuperHd  += cOrder + "]"
   ELSE
      cSuperHd  += "0]"
   ENDIF

RETURN cSuperHd

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
   AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_GRAY, CLR_BLACK ) } } ) // 1
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), nPane3 ,;
                                            iif( ob:nAt % 2 == 0, nPane2, nPane ) )   } } )    // 2 , фона в ячейках таблицы
   oTsb:aClr1  := CLR_BLACK
   oTsb:aClr16 := { nHead1, nHead2 }
   oTsb:aClr17 := CLR_WHITE

   AAdd( aColors, { CLR_HEADF , {|| CLR_YELLOW            } } )        // 3 , текста шапки таблицы
   AAdd( aColors, { CLR_HEADB , {|| { nHead2, nHead1 }    } } )        // 4 , фона шапки таблицы
   //AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLACK } } )                  // 5 , текста курсора в ячейках с фокусом
   AAdd( aColors, { CLR_FOCUSF, {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_WHITE, CLR_BLACK ) } } )  // 5 , текста курсора в ячейках с фокусом
   //AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd( aColors, { CLR_FOCUSB, {|nr,nc,ob| myFocusB(nr,nc,ob,-CLR_HRED,-CLR_BLUE,-CLR_YELLOW) } } ) // 6 , фона курсора

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

//////////////////////////////////////////////////////////////////
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

RETURN nClr

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // настройки
   Local nHCell, nI, cCol, oCol, nDlu, oDlu, cVal, n
   LOCAL aFont, cFont, nFSize, nHHead, nHFoot
   LOCAL hFont, cHead, nWCol

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // подгоним размеры колонок по фонту
   oDlu := _Font2oDlu( aFont[1] )
   nDlu := oDlu:nSize
   // --- варианты задания размера ---
   //?  _HMG_DefaultFontName, _HMG_DefaultFontSize, "nDlu=", nDlu, oTsb:aTsbFont[1]
   //?  "oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   IF cFont == "DejaVu Sans Mono"
      // !!! для фонта MONO - DejaVu Sans Mono делаем добавку
      IF nFSize <= 15                       ; n := 20
      ELSEIF nFSize > 15 .AND. nFSize <= 17 ; n := 70
      ELSEIF nFSize > 17 .AND. nFSize <= 19 ; n := 75
      ELSE                                  ; n := 50
      ENDIF
      //n := iif( nFSize < 15, 20, iif( nFSize < 20, 30, 40 ) )
      oDlu:nPixWidth    += n
      oDlu:nPixWidthDT  += n
      oDlu:nPixWidthDT1 += n
      oDlu:nPixWidthDT2 += n
   ENDIF
   //
   nHCell := oDlu:H1 + IIF(nFSize <= 15,6,12)     // высота строк в ТСБ
   //                ^^^ - константа
   nHCell := oDlu:H(IIF(nFSize <= 15,1.25,1.5))   // так правильнее, от размера фонта высота
   //               ^^^^  - пропорция от размера фонта
   // можно взять и эти размеры
   nHHead := oTsb:nHeightHead       // высота шапки
   nHFoot := oTsb:nHeightFoot       // высота подвала

   WITH OBJECT oBrw

      :lNoKeyChar    := .F.           // НЕТ ввода в ячейки от букв, цифр
      :nHeightCell   := nHCell        // высота ячеек = высоте картинки
      :nHeightHead   := nHCell * 1.2  // высота шапки
      :nHeightFoot   := nHCell + 4    // высота подвала
      :nHeightSpecHd := 12            // высота спецхидера ENUMERATOR
      :lFooting      := .T.           // использовать подвал
      :lDrawFooters  := .T.           // рисовать  подвалы
      //:nFreeze     := 2             // Заморозить столбец
      //:nCell       := :nFreeze + 1
      :lLockFreeze   := .T.           // Избегать прорисовки курсора на замороженных столбцах
      :nCellMarginLR :=  1            // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nMemoHV       :=  1            // показ 2 строки мемо-поля

      // --------- хранилище картинок, удаляется после закрытия объекта автоматом ------
      :aBitMaps      := { Nil, LoadImage("bRecDel16") }

      :GetColumn("ORDKEYNO"):nWidth   := nHCell + 10   // ширина колонки
      // изменение картинки для удалённых записей в колонке ORDKEYNO
      :GetColumn("ORDKEYNO"):aBitMaps := :aBitMaps //oTsb:aBmp1[2]
      :GetColumn("ORDKEYNO"):uBmpCell := {|nc,ob| nc:=nil, iif( (ob:cAlias)->(Deleted()), ob:aBitMaps[2], ob:aBitMaps[1] ) }

      :Cargo:nModify := 0     // изменения в таблице

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

   // в момент постройки колонок SELECTOR, ORDKEYNO - НЕТ
   // строяться только после END TBROWSE
   //? ProcNL()
   //? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)
   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont //, nI, oCol

   hFont := oBrw:aColumns[1]:hFontSpcHd  // 4-special header font
   // установить фонт для 1 колонки таблицы
   oBrw:aColumns[1]:hFont     := hFont     // 1-cells font
   oBrw:aColumns[1]:hFontFoot := hFont     // 3-footer font

    // фонты для колонок 3-4 таблицы, остальные не надо
   /*For nI := 2 To oBrw:nColCount()
      oCol       := oBrw:aColumns[ nI ]
      oCol:hFont := {|nr,nc,ob| // фонты для строк таблицы
                      Local nGet, xv
                      nGet := ob:GetValue("PRIXOD") // колонка сумма
                      xv   := ob:GetValue(nc)
                      //? "**** ob:aColumns["+HB_NtoS(nc)+"]", nr, nc, xv, nGet
                      //!!! nr := ob:aColumns[ nc ]:hFont   // GetFontHandle( "Normal" )
                      nr := ob:hFont
                      IF nGet < 0   // минусовая сумма
                         nr := oBrw:aColumns[nc]:hFontHead
                      ENDIF
                      //IF "---" $ cval
                      //    nr := ob:Cargo:hTsbBold4 // GetFontHandle( "Bold" )
                      //ENDIF
                      Return nr
                      }
   Next */

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw, oTsb )
   LOCAL hFont, nHFont, aSupHd, cSprHd, nClr16, nClr17, nEnd, O

   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )
   aSupHd := oTsb:aSupHd
   O      := oBrw:Cargo
   cSprHd := oTsb:cTtlSupHead
   nClr16 := oTsb:aClr16
   nClr17 := oTsb:aClr17

   WITH OBJECT oBrw  // Шаг-1 см. ниже // Шаг-2
      nEnd := :nColCount() //+ IIF( :lSelector, 1, 0 ) // ВСЕГДА ТАК !
      // Создаём СУПЕРХИДЕР в таблице размером 0
      :AddSuperHead( 1, nEnd, "Super_Header_Table" ) //,,, .F.,,, .F., .F., .F., 0, )
      :aSuperhead[ 1, 3 ] := cSprHd
      :nHeightSuper := nHFont * 1.5    // 1 строка
      // задать цвета суперхидеру
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, фона
      :SetColor( { 17 }, { nClr17           } ) // 17, текста
      // методы доступа
      //nHAlign := :nAlignSupHdGet(nCol)   // что установлено
      //:nAlignSupHdGet(nCol, nHAlign)
      //:aSuperHead[1,12] := DT_LEFT       // изменить отбивку текста
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
   LOCAL nHHead := oTsb:nHeightHead   // высота шапки - в качестве примера

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
      :UserKeys(VK_F2    , {|ob| myTsbCodePage( ob, 1 ), ob:Setfocus() })  // смена кодовой страницы
      :UserKeys(VK_F3    , {|ob| myTsbCodePage( ob, 2 ), ob:Setfocus() })  // смена кодовой страницы
      :UserKeys(VK_F4    , {|ob| myTsbListColumn( ob ) , ob:Setfocus() })  // инфо по списку колонок
      :UserKeys(VK_F5    , {|ob| myTsbListFont( ob )   , ob:Setfocus() })  // инфо по фонтам таблицы
      :UserKeys(VK_F8    , {|ob| myTsbSelectorNew(ob)  , ob:Setfocus() })  //
      :UserKeys(VK_F9    , {|ob| myTsbSelectorOld(ob)  , ob:Setfocus() })  //

      cBrw := :cControlName
      nLen := nTsb := 0
      //nTsb := This.&(cBrw).ClientWidth   // This. НЕЛЬЗЯ ИСПОЛЬЗОВАТЬ,
      //nLen := :GetAllColsWidth() - 1     // если построение ТСБ идёт в другом окне
      IF nLen > nTsb
         //:lAdjColumn  := .T.
         //:lNoHScroll  := .F.
         //:lMoreFields := ( :nColCount() > 45 )
      ELSE
         //:AdjColumns()
      ENDIF

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   //? "***", ProcNL(), Alias()
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
// блок-кода который ДЕЛАЕМ ПОСЛЕ END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, nI, cCol, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы
   a4Clr  := oTsb:a4Clr                     // считаем 4 цвета таблицы
   nTest  := HMG_RGB2n(a4Clr[1])            // цвет фона окна

   //? ProcNL(), MGVersNumba()
   //? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   // вариант показа - 1
   // замена строки нумератора колонок на свой цвет, кроме SELECTOR
   oBrw:lClrSelectorHdBack := .T. // background OFF
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      oCol:nClrSpcHdFore := CLR_RED
      oCol:nClrSpcHdBack := nBCSpH
   NEXT

   // вариант показа - 2
   // изменим цвет колонки - SELECTOR
   /*oBrw:lClrSelectorHdBack := .F. // background OFF
   oCol := oBrw:GetColumn("SELECTOR")
   oCol:nClrBack      := nBCSpH
   oCol:nClrFore      := CLR_RED
   oCol:nClrFootBack  := nBCSpH
   oCol:nClrSpcHdBack := nBCSpH
   oCol:SaveColor()                       // сохранить цвета колонки
   oBrw:nClrSelectorHdBack := nBCSpH      // Footer для "SELECTOR"
   */

   // изменение виртуальной колонки
   nLen := LEN(HB_NtoS(oBrw:nLen))
   nCol := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // кол-во знаков + 2 знака
      oCol:nWidth := nWCol                                       // новая ширина
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

   // изменение ширины поля типа "+"
   nLen := LEN(HB_NtoS(oBrw:nLen))
   FOR EACH oCol IN oBrw:aColumns
      nCol := hb_EnumIndex(oCol)
      IF oCol:cFieldTyp == "+"
         hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
         nWCol := GetTextWidth( Nil, REPL("0", nLen + 6), hFont )   // кол-во знаков + 2 знака + 2 знак отступ
         oCol:nWidth := nWCol                                       // новая ширина
      ENDIF
   NEXT

   IF oBrw:lDrawSuperHd  // Шаг-2
      // увеличить суперхидер до конца колонок
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF

   oBrw:DrawHeaders()   // перечитать суперхидер/шапку/нумератор
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// новая запись в базе добавляется в конец базы и переходим сразу к редактированию
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL nRecno, cMsg, aTmp, aBColor, aFColor, aColors, cTitle

   ? " -Ins- "+ProcNL(), oBrw:ClassName

   IF App.Cargo:cLang == "RU"
      cTitle := 'Добавление записи'
      cMsg   := "ВНИМАНИЕ !;Вставить запись в таблицу ?"
   ELSE
      cTitle := 'Adding recno'
      cMsg   := "ATTENTION!;Insert a record into the table ? "
   ENDIF

   aColors := { {45,223,70} , ORANGE }
   aBColor := { 238, 249, 142 }   // светло-жёлтый
   aFColor := BLACK
   aTmp    := _SetMsgAlertColors(aBColor,aFColor)  // новые цвета

   IF AlertYesNo( cMsg, cTitle, , , 64, aColors )
      // срабатывает сразу при добавлении записи
      // добавить в поле дату+время вставки записи
      oBrw:bAddAfter := {|ob,ladd|
                          Local cRecno := HB_NtoS( (ob:cAlias)->( RecNo() ) )
                          If ladd
                             ? "+++ :bAddAfter",ProcNL(), "INDEXORD()=", INDEXORD()
                             ?? "RecNo()= " + cRecno
                             /*(ob:cAlias)->KOPERAT   := M->nOperat   // кто изменил запись
                             (ob:cAlias)->DATEVVOD  := DATE()       // Дата/время правки
                             (ob:cAlias)->TIMEVVOD  := nTime*/
                             (ob:cAlias)->( dbSkip(0) )
                          EndIf
                          Return Nil
                        }

      // oBrw:bAddAfter  := Nil  // это если не нужен код заполнения полей при создании новой записи

      // встроенный метод для добавления записи
      oBrw:AppendRow(.T.)

      nRecno := (oBrw:cAlias)->( RecNo() )
      IF (oBrw:cAlias)->(RLock())
         // если нужна запись в базу даты+время для этих действий (сделано выше)
         //(oBrw:cAlias)->IM        := hb_DateTime()    // когда изменили запись
         //(oBrw:cAlias)->KOPERAT   := M->nOperat       // кто изменил запись - резерв
         //(oBrw:cAlias)->DATEVVOD  := DATE()
         //(oBrw:cAlias)->TIMEVVOD  := nTime
         (oBrw:cAlias)->(DbCommit())
         (oBrw:cAlias)->(DBUnlock())
         ? "+++ " + ProcNL(), "INDEXORD()=", INDEXORD(), "RecNo()=", nRecno
      ENDIF

      nRecno := (oBrw:cAlias)->( RecNo() )
      ? "+++ " + ProcNL(), hb_DateTime(), "Insert!", "RecNo()=", nRecno

      oBrw:nCell := 1 // oBrw:nColumn("PRIDAT", .T.)  // в начало колонок для редактирования
      oBrw:Reset()
      //oBrw:Refresh(.T.,.T.)
      oBrw:GoBottom()     // всегда на новую запись, если нет индекса
      DO EVENTS

   ENDIF

   _SetMsgAlertColors(aTmp[1],aTmp[2])      // восстановить цвета

RETURN Nil

//////////////////////////////////////////////////////////////////////////
STATIC FUNCTION RecnoDelete(oBrw)
   LOCAL lChange, nAt, lDelete, nRecno, nCell, nMetod, nRec

   ? " -Del- "+ProcNL(), oBrw:ClassName
   ?? ":nLen=", oBrw:nLen //,":lIsXXX=", oBrw:lIsDbf, oBrw:lIsArr
   ?? ":nRowPos=", oBrw:nRowPos

   // срабатывает сразу при удалении записи
   oBrw:bDelAfter := {|nr,ob|
                             Local cAls := ob:cAlias
                             Local nOld := (cAls)->( RecNo() )
                             //If (cAls)->( deleted() )
                             ? " -Del-  :bDelAfter" + ProcNL(), "nRecno=", nOld
                             ?? "INDEXORD()=", INDEXORD()
                             If (cAls)->( RLock() )
                                // если нужна запись в базу даты+время для этих действий
                                //If lDel ; (cAls)->DT_DEL  := hb_DateTime()
                                //Else    ; (cAls)->DT_REST := hb_DateTime()
                                //EndIf
                                //(cAls)->KOPERAT  := M->nOperat  // кто удалил запись
                                //(cAls)->DATEVVOD := DATE()      // когда удалили запись
                                //(cAls)->TIMEVVOD := Time()
                                (cAls)->( DbUnLock() )
                                ?? "Write field: ", DATE(), Time()
                                (cAls)->( dbSkip(0) )
                             EndIf
                             //EndIf
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
   ? SPACE(5) + ProcNL(), nCol, cTyp

   uOld := uVal
   lWrt := .T.       // записать ячейку
   lRet := .T.       // давать редактировать поле в :get
   aRet := {uVal}    // поместим в массив - то что пришло

   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   IF App.Cargo:cLang == "RU"
      cStr += 'Тип поля колонки: "' + cTyp + '" ;'
      cStr += 'НЕТ обработки для этого поля !;'
   ELSE
      cStr += 'Column field type: "' + cTyp + '" ;'
      cStr += 'NO processing for this field!;'
   ENDIF

   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF cTyp $ "CM"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // пример для своей функции
      IF AT(CRLF,uVal) > 0           // если в поле "C" есть CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // не давать редактировать поле в :get
      ELSEIF cTyp == "M"
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
      IF (oBrw:cAlias)->(RLock())                  // делать самому
         // !!! всегда массив, если пустой, то это ОТКАЗ от ввода
         IF LEN(aRet) > 0
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // счётчик-изменения в таблице
            xRet := aRet[1]
            oBrw:SetValue(nCol,xRet)
            //(oBrw:cAlias)->KOPERAT  := 555       // кто правил запись
            //(oBrw:cAlias)->DATEVVOD := DATE()    // дата правки
            //(oBrw:cAlias)->TIMEVVOD := 9999      // время правки
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
         ENDIF
      ELSE
         cMsg := "Recording is locked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
      ENDIF
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

   IF cTyp $ "CNDL"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      RETURN .F.
   ENDIF

   oBrw:SetFocus()
   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCodePage( oBrw, nMenu )             // смена кодовой страницы
   LOCAL aYX, aRet, cCdpg, cTitle

   ? ProcNL(), oBrw, nMenu
   aYX := App.Cargo:aLbl3  // { nH1, nW1 + nG }   // координаты вывода

   IF nMenu == 1
      SET WINDOW THIS TO oBrw:cParentWnd         // ОБЯЗАТЕЛЬНО !!!
      aRet := myCodePagePart(aYX) // { 2 , "RU866", cMsg + "RU866","iFlag_Ru32" }
      SET WINDOW THIS TO
   ELSE
      SET WINDOW THIS TO oBrw:cParentWnd         // ОБЯЗАТЕЛЬНО !!!
      aRet := myCodePageDbf(aYX)
      SET WINDOW THIS TO
   ENDIF
   IF LEN(aRet) > 0
      cCdpg     := aRet[2]
      ReopenDbase(oBrw,cCdpg)
      DO EVENTS
      // перепоказ базы
      cTitle := SuperHider(cCdpg)
      oBrw:aSuperhead[1,3] := cTitle
      oBrw:DrawHeaders()             // перечитать суперхидер/шапку/нумератор
      oBrw:Reset()
      oBrw:GoTop()
      oBrw:SetFocus()
   ENDIF
   DO EVENTS
RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION ReopenDbase(oBrw, cCodePage)
   LOCAL cDbf, cFltr, nI, cIndex, aIndx, cFile, nOrd, cAls
   LOCAL lNew, nJ, lOpen, cOld, oError, cMsg

   BEGIN SEQUENCE  WITH { |e|break( e ) }
      cOld := hb_cdpSelect(cCodePage)
      IF hb_cdpSelect() == cCodePage
         // есть такая кодовая страница
         // there is such a code page
      ENDIF
      hb_cdpSelect(cOld)
      lOpen := .T.
   RECOVER USING oError
      cMsg := "Code page error!;"
      cMsg += "No driver for CodePage: "
      cMsg += cCodePage + ";" + ProcNL()
      AlertStop( cMsg, "ERROR", "ZZZ_B_STOP64", 64)
      lOpen := .F.
   END SEQUENCE

   IF lOpen

      cAls  := oBrw:cAlias
      DbSelectArea(cAls)
      nOrd  := ORDNAME()
      aIndx := {}
      cDbf  := DBINFO( DBI_FULLPATH )
      cFltr := (cAls)->( DbFilter() )
      FOR nI := 1 TO 500
         IF LEN(ORDNAME(nI)) == 0
            EXIT
         ELSE
            DBSetOrder(nI)
            cIndex := DBORDERINFO( DBOI_FULLPATH,,ORDNAME(nI) )
            IF LEN(aIndx) == 0
               AADD(aIndx, cIndex )
            ELSE
               FOR nJ := 1 TO LEN(aIndx)
                  lNew := .T.
                  IF cIndex == aIndx[nJ]
                     lNew := .F.
                     EXIT
                  ENDIF
                  IF lNew
                    AADD(aIndx, cIndex )
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
      (cAls)->( dbCloseArea() )
      DO EVENTS
      INKEYGUI(100)

      USE (cDbf) ALIAS (cAls) CODEPAGE cCodePage NEW SHARED
      DO EVENTS
      IF LEN(aIndx) > 0
         FOR nI := 1 TO LEN(aIndx)
            cFile := aIndx[nI]
            ORDLISTADD( cFile )
         NEXT
         DBSetOrder(nOrd)
      ENDIF

   ENDIF  // lOpen

RETURN NIL

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
