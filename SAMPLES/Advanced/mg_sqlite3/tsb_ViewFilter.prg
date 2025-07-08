/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Форма фильтра на таблицу / Filter form for table
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
////////////////////////////////////////////////////////////////////////////////////
FUNCTION Form4Filter(oWnd, ky, cn, oBrw)
   LOCAL cIcon, cFont, nFontSize, aBackColor, cFontBtn, nHIco, cTxt, aBtn, bAct
   LOCAL nY, nX, nW, nH, nG, nLR, nWTxt, nHTxt, nHBtn, nWBtn, nYBtn, nXBtn, cMsg
   LOCAL ao, owc, oDlu, cBrw, oTsb, ahIco, cForm, cTitle, cAls, a4Clr, cMd5
   LOCAL aBtnFont, aBtnFClr, nFBtnSize, cDbfCnfg, nHBtn2, nWBtn2, nWFrm, oCol
   LOCAL cFilter, cWTtl, cAlsParent, n3Btn, nHFrm, lExeIni, cCdPg, nY2, cField

   ?  "  ###", ProcNL(), oWnd:Name,ky,cn, oBrw:cAlias, This.&(cn).Caption
   App.Cargo:cRetFilter := ""         // вернуть строку фильтра
   cAlsParent := oBrw:cAlias          // база с родительского окна
   cCdPg      := oBrw:Cargo:cCdPg     // читаем на окне ТСБ CodePage файла
   ao         := (App.Cargo)
   cFont      := ao:cFontName                     // DejaVu Sans Mono
   nFontSize  := ao:nFontSize
   cFontBtn   := ao:cFontName2                    // Comic Sans MS
   nFBtnSize  := ao:nDlgSize                      // ao:nFontSize + 2
   aBtnFont   := { cFontBtn, nFBtnSize, .T. }     // фонт для кнопок
   // цвета таблицы: [1]-окно, [2]-(шапка+подвал), [3]-(строка %1), [4]-(строка %2)
   IF IsArray(App.Cargo:a4ClrFilter)
      a4Clr   := App.Cargo:a4ClrFilter
   ELSE
      a4Clr   := { {148,85,185}, {142, 25,142}, {238,130,238} , {232,212,244} }  // фиолетовый 1
   ENDIF
   aBackColor := a4Clr[1]                           // цвет окна формы
   lExeIni    := hb_FileExists(App.Cargo:cIniFile)  // если есть ини-файл к программе
   cForm      := "Form_Filter"
   cIcon      := "iFilter48"
   cWTtl      := IIF( ao:cLang == "RU", "Окно фильтра", "Filter window")
   cTitle     := IIF( ao:cLang == "RU", "Настройка фильтра для таблицы", "Setting up a filter for a table")
   aBtnFClr   := { BLACK, YELLOW }     // цвет фонта кнопки / цвет фонта кнопки при нажатии мышкой
   cBrw       := "Tsb_Filter"
   cMsg       := IIF( ao:cLang == "RU", "Не смог создать базу для фильтра !", "Could not create base for filter !")
   cFilter    := SPACE(480)
   cMd5       := HB_MD5(App.Exename) + cAlsParent              // уник.метка, откуда стартовал ЕХЕ
   cDbfCnfg   := ao:cPathTemp + "mg_Filter_" + cMd5 + ".dbf"   // путь, где база настроек фильтра
   // открыть новую базу для таблицы фильтра
   cAls       := CreateConfigFilter(cDbfCnfg,oBrw)
   IF LEN(cAls) == 0
      cMsg += ";" + cDbfCnfg + ";;" + ProcNL()
      AlertStop( cMsg , , "ZZZ_B_STOP64", 64 )
      RETURN .F.
   ENDIF
   DbSelectArea(cAls)
   // доступ к ини-файлу везде в программе - App.Cargo:oIni

   // возвращает объект с данными размеров от размера фонта от dlu в pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top  // можно так
   nW   := ao:aDisplayMode[1] * IIF( App.Cargo:aDisplayMode[2] <= 720, 0.96, 0.8) // размеры окна
   nH   := ao:aDisplayMode[2] * 0.92       // размеры окна
   nG   := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   nX   := nLR := nG                       // отступ слева и право
   nY   := nG                              // отступ сверху и снизу

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH   ;
      ICON cIcon TITLE cWTtl BACKCOLOR aBackColor      ;
      MODAL NOSIZE                                     ;
      FONT cFont SIZE nFontSize                        ;
      ON INIT    _wPost( 0)                            ;
      ON RELEASE _wSend(98)

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor    := This.BackColor   // цвет окна
      owc:ahIcoDel   := {}               // для удаления хендлов иконок с формы
      owc:oWnd       := oWnd             // запомнить ВЕСЬ объект Cargo -> родительского окна  - вариант 3
      owc:cAlsParent := cAlsParent       // база с родительского окна
      owc:cAls       := cAls             // новая база таблицы фильтра

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nHBtn := IIF( App.Cargo:aDisplayMode[2] <= 720, 38, 64 ) // высота кнопок внизу окна

      nWTxt := nW - nLR * 2
      nHTxt := nFontSize * 2.5
      nHFrm := nHTxt + nG + nFontSize + 5

      @ 0, 0 LABEL Label_0 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      //@ nY, nX LABEL Label_1 WIDTH nW-nG*2 HEIGHT nH-nHBtn-nG*3 VALUE 'Table' BACKCOLOR WHITE
      oTsb := TsbPatam(cBrw,a4Clr,cAls,cTitle,a4Clr)
      // блоки кода для _TBrowse(...) - менять нельзя // (op=oTsb)
      oTsb:bInit  := {|ob,op| myTsbInit(ob,op), myTsbFont(ob,op), myTsbSuperHd(ob,op) }  // настройки тсб  -> см.ниже
      oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)  }  // другие настройки тсб  -> см.ниже
      //oTsb:bEnd := {|ob,op| myTsbEnd(ob,op)                      }  // блок кода после END TBROWSE -> см.ниже
      oTsb:nW := nW-nG*2
      oTsb:nH := nH-nHBtn-nG*3 - nHFrm
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, cAls, cBrw, nY, nX, oTsb:nW, oTsb:nH )
      // последние действия с ТСБ
      myTsbEnd(oBrw,oTsb)         // <<----- так правильней
      //App.Cargo:oBrw  := oBrw   // запомнили для внешних функций - здесь НЕ НАДО
      owc:oBrw := oBrw            // запомнили на окне
      owc:cBrw := cBrw            // запомнили на окне
      oBrw:Cargo:ObjWnd := This.Object   // текущее окно

      /////////////////////// Ниже таблицы ////////////////////////////
      nY := oTsb:nH + nG + nG/2

      oCol  := oBrw:GetColumn("FVAL1")
      nWFrm := GetTxtWidth( REPL("X",20), nFontSize, cFont )

      cMsg := IIF( ao:cLang == "RU", " Поиск поля ", " Search field " )
      @ nY, nX FRAME Frame_1 CAPTION cMsg WIDTH nWFrm HEIGHT nHFrm BACKCOLOR aBackColor //OPAQUE

      nY2 := nY + nFontSize + IIF( App.Cargo:aDisplayMode[2] <= 720, 7, nFontSize )
      cField := SPACE(15)
      @ nY2, nX + nG GETBOX GB_Find VALUE cField WIDTH nWFrm-nG*2 HEIGHT nHTxt FONTCOLOR BLACK BACKCOLOR WHITE ;
        PICTURE REPL('x',20) ON CHANGE {|| cField := This.Value, DoEvents(), _wPost(10) }
      owc:cGBFind := "GB_Find"       // запомнили на окне

      nX += This.GB_Find.Width + nG*3

      cMsg := IIF( ao:cLang == "RU", " Ручной фильтр на таблицу ", " Manual filter on table " )
      @ nY, nX FRAME Frame_2 CAPTION cMsg WIDTH nWTxt-nX+nG HEIGHT nHFrm BACKCOLOR aBackColor //OPAQUE

      nY += nFontSize + IIF( App.Cargo:aDisplayMode[2] <= 720, 7, nFontSize )
      /////////////////////// Button ////////////////////////////
      cMsg := IIF( ao:cLang == "RU", " Список готовых фильтр на таблицу ", " List of ready filters for the table " )
      nHIco  := nHTxt - 5
      cTxt   := ""
      aBtn   := { "Button_List", cTxt, "iFiltr64x1", "iFiltr64x2", nHIco, aBtnFClr, aBtnFont, cMsg }
      nYBtn  := nY
      nXBtn  := nX + nG
      nHBtn2 := nHTxt
      nWBtn2 := nHTxt
      bAct   := {|| /*MsgDebug(This.Cargo),*/ _wPost(4) }  // событие на форме
      ahIco  := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn2, nHBtn2, aBtn, bAct, GRAY)
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF
      IF !lExeIni
         This.Button_List.Hide
      ENDIF

      nX := This.Button_List.Col + This.Button_List.Width + nG/2
      @ nY, nX GETBOX GB_Fltr VALUE cFilter WIDTH nWTxt - nX HEIGHT nHTxt FONTCOLOR BLACK BACKCOLOR WHITE ;
        PICTURE REPL('x',480) ON CHANGE {|| cFilter := This.GB_Fltr.Value }
      owc:cGetBox := "GB_Fltr"       // запомнили на окне

      /////////////////////// Кнопки внизу формы ////////////////////////////
      nWBtn := 300
      n3Btn := nWBtn*3 + nG*4
      IF n3Btn > nW
         nWBtn := ( nW - nG*4 ) / 3
      ENDIF
      nYBtn := nH - nG - nHBtn
      nHIco := nHBtn - nG/2
      cTxt  := IIF( ao:cLang == "RU", "Помощь", "Help" )
      nXBtn := nW - nWBtn*3 - nG*3
      aBtn  := { "Button_Help", cTxt, "iQuestion64", "iQuestion64x2", nHIco, aBtnFClr, aBtnFont, "" }
      bAct  := {|| This.Button_Help.Enabled := .F. ,  _wPost(80,cForm) }   // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, ORANGE )
      // присвоим явно кнопку здесь
      This.Button_Help.Action := bAct
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "Установить фильтр", "Set filter" )
      nXBtn := nW - nWBtn*2 - nG*2
      aBtn  := { "Button_Filtr", cTxt, "iFiltr64x1", "iFiltr64x2", nHIco, aBtnFClr, aBtnFont, "" }
      bAct  := {|| This.Button_Filtr.Enabled := .F. ,  _wPost(90,cForm) }   // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, {60,230,84} )
      // присвоим явно кнопку здесь
      This.Button_Filtr.Action := bAct
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "Отмена", "Cancel" )
      nXBtn := nW - nWBtn - nG
      aBtn  := { "Button_Exit", cTxt, "iReturn64x1", "iReturn64x2", nHIco, aBtnFClr, aBtnFont, "" }
      //bAct:= {|| This.Button_Exit.Enabled := .F.,  _wPost(101,cForm) }   // не работает - вылет
                // вернуть строку фильтра
      bAct  := {|| App.Cargo:cRetFilter := "" ,  _wPost(99,cForm) }   // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, CLR_HRED )
      // присвоим явно кнопку здесь
      This.Button_Exit.Action := bAct
      IF LEN(ahIco) > 0 // для удаления хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      //_o2log(owc, 15, ProcNL()+" -------------- Параметры объекта : => owc", .T.)

      // Установка событий на это окно программы
      Sets_Event2ThisWindow()

      ON KEY ESCAPE OF &cForm ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL       // вернуть строку фильтра   App.Cargo:cRetFilter

//////////////////////////////////////////////////////////////////////////////////////////////////////
// Установка событий на это окно программы
STATIC FUNCTION Sets_Event2ThisWindow()

   WITH OBJECT This.Object
     :Event( 0, {|ow| // запуск после построения окна
                       This.Topmost := .F.
                       ? ProcNL(),">>> Start window: "+ow:Name
                       //ow:Setfocus('Label_0')
                       ow:Cargo:oBrw:Setfocus()
                       DO EVENTS
                       Return Nil
                       })

     :Event( 4, {|ow| // кнопка - Список готовых фильтр на таблицу
                      Local cDownFltr
                      SET WINDOW THIS TO ow:Name         // ОБЯЗАТЕЛЬНО !!!
                      cDownFltr := FilterListIni(ow)
                      SET WINDOW THIS TO
                      If LEN(cDownFltr) > 0
                         This.GB_Fltr.Value := cDownFltr
                         ow:SetFocus(ow:Cargo:cGetBox)   // или ow:SetFocus('GB_Fltr')
                      Else
                         ow:Cargo:oBrw:Setfocus()
                      Endif
                      DO EVENTS
                      Return Nil
                      } )

     :Event(10, {|ow| Refresh_Tsb(ow) } )   // фильтр на ТСБ

     :Event(80, {|ow| // Помощь
                      SET WINDOW THIS TO ow:Name
                      myHelpFilter()      // см.ниже
                      SET WINDOW THIS TO
                      This.Button_Help.Enabled := .T.
                      //ow:SetFocus('Label_0')
                      ow:Cargo:oBrw:Setfocus()
                      Return Nil
                      } )

     :Event(90, {|ow| // Фильтр и закрыть окно
                      Local owc := ow:Cargo
                      Local cFilter, ao  := App.Cargo
                      Local cTtl    := "ОШИБКА !;Фильтр установлен неправильно !;Значение фильтра"
                      Local cTtl2   := "ERROR !;The filter is installed incorrectly !;Filter value"
                      Local cGetBox := ow:Cargo:cGetBox
                      cTtl := IIF(ao:cLang == "RU", cTtl, cTtl2 )
                      SET WINDOW THIS TO ow:Name     // ОБЯЗАТЕЛЬНО !!!
                      cFilter := myFilter2Get(owc)   // выбор из 2х фильтров на форме
                      SET WINDOW THIS TO
                      If LEN(cFilter) > 0
                         IF LEN(ALLTRIM(This.&(cGetBox).Value)) > 0
                            FilterSaveIni(ow,cFilter)   // Записать фильтр в ини-файл
                         ENDIF                          // только ручной фильтр
                         // вернуть строку фильтра
                         App.Cargo:cRetFilter := cFilter
                         _wPost(99,ow:Name)
                      Else
                         // без показа ошибки
                         //SET WINDOW THIS TO ow:Name     // ОБЯЗАТЕЛЬНО !!!
                         //AlertStop( cTtl, , "ZZZ_B_STOP64", 64, {RED} )
                         //SET WINDOW THIS TO
                         App.Cargo:cRetFilter := ""
                         ow:Cargo:oBrw:Setfocus()
                      Endif
                      This.Button_Filtr.Enabled := .T.
                      Return Nil
                      } )

     :Event(98, {|ow| // ON Release
                      Local ah := ow:Cargo:ahIcoDel
                      ?  ProcNL()
                      ?? ">>> Exit button pressed! Window: "+ow:Name
                      ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                      ?? ah, HB_ValToExp(ah)
                      IF IsArray(ah)
                         AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                      ENDIF
                      IF LEN(ow:Cargo:cAls) > 0
                         (ow:Cargo:cAls)->( dbCloseArea() )
                      ENDIF
                      DbSelectArea(ow:Cargo:cAlsParent)  // база с родительского окна
                      DO EVENTS
                      Return Nil
                      } )

     :Event(99, {|ow| ow:Release() } )
   END WITH

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam(cBrw, a4Clr, cAls, cTitle)
   LOCAL oTsb

   oTsb := oHmgData()
   oTsb:cAls        := cAls
   oTsb:cBrw        := cBrw
   //                      cell     Head   foot    SpecHider  SuperHider   Edit
   oTsb:aFont       := { "Normal", "Bold", "Bold", "SpecHdr" , "SuperHdr", "TsbEdit" }
   oTsb:aNumber     := { 1, 40 }                      // ставим нумерацию строк таблицы
   //oTsb:uSelector := 20                             // не ставим SELECTOR
   oTsb:lSpecHd     := .T.                            // поставить в таблице нумератор на колонках
   oTsb:lFooting    := .T.                            // поставить в таблице подвал
   oTsb:aFoot       := .T.                            // заполнить подвал
   oTsb:nHeightFoot := 25                             // высота подвала
   oTsb:nHeightHead := 25                             // высота шапки
   oTsb:nHeightHead := 25                             // высота шапки
   oTsb:nHeightSpecHd := 12                           // высота спецхидера ENUMERATOR
   oTsb:aEdit       := .T.                            // редактировать колонки
   oTsb:a4Clr       := a4Clr                          // сохраним 4 цвета таблицы
   oTsb:aBrush      := a4Clr[3]                       // цвет фона под таблицей
   oTsb:aColor      := Color_Tsb(a4Clr,oTsb)          // цвета таблицы: 1(окно),2(шапка+подвал),3(строка %1),4(строка %2)
   oTsb:cTtlSupHead := cTitle
   oTsb:a4Clr       := a4Clr
   //                    1           2      3       4         5          6        7         8          9        10       11       12       13
   oTsb:aHead       := {"*"    ,"Имя поля","Знак" ,"Значение" ,"Условие","Знак" ,"Значение" ,"Условие","Знак" ,"Значение","-x-"  }
   oTsb:aField      := {"FTYPE","FNAME"   ,"FZNK1","FVAL1"    ,"ORAND1" ,"FZNK2","FVAL2"    ,"ORAND2" ,"FZNK3","FVAL3"   ,"FDEL" }
    // редактировать колонки
   oTsb:aEdit       := { .F.   , .F.      , .T.  , .T.   , .T.       , .T.     , .T.   , .T.       , .T.     , .T.   , .T.       , .T.     , .T.  , .T.    }
   oTsb:aName       := oTsb:aField
   //oTsb:nHCell    := App.Cargo:nTsbHCell                            // высота ячейки
   oTsb:nHCell      := App.Cargo:nFontSize + (App.Cargo:nFontSize/2)  // высота ячейки

   IF App.Cargo:cLang == "EN"
      //              1          2      3     4        5         6         7        8         9         10      11      12     13
      oTsb:aHead := {"*" ,"Field name","Sign" ,"Value" ,"Condition","Sign" ,"Value" ,"Condition","Sign" ,"Value","-x-" }
   ENDIF

   IF App.Cargo:cLang == "RU"
      //              1       2       3     4    5     6       7    8     9       10    11   12     13
      oTsb:aFoot := {"*" ,"Пример:" , ">=" ,"3" ,".И."  ,"<=" ,"5" ,".ИЛИ."  , "==", "-1", "" }
   ELSE
      oTsb:aFoot := {"*" ,"Example:",">=" ,"3" ,".AND.","<=" ,"5" ,".OR.", "==", "-1", "" }
   ENDIF

RETURN oTsb

///////////////////////////////////////////////////////////
STATIC FUNCTION CreateConfigFilter(cDbf,oBrw)
   LOCAL aStr, cAls, cCdp, cTyp, cCol, oCol, aFld, nI
   LOCAL lRet, cRdd, lShared, cCdPg, lUtf8

   // список полей бд
   aFld := {}
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         cTyp := oCol:cFieldTyp
         AADD( aFld, { cTyp, cCol } )
      ENDIF
   NEXT

   lUtf8 := .F.
   cCdPg := oBrw:Cargo:cCdPg     // читаем на окне ТСБ CodePage файла
   //IF cCdPg == "UTF8"
   //   cCdp  := "UTF8"
   //   lUtf8 := .T.
   //ELSE
   //   cCdp := hb_SetCodepage()
   //ENDIF
   cCdp := hb_SetCodepage()
   cRdd := "DBFCDX"
   cAls := "FILTER"
   lRet := .T.
   aStr := {}
   AAdd( aStr, { 'FTYPE' , 'C',  5, 0 } )
   AAdd( aStr, { 'FNAME' , 'C', 15, 0 } )
   AAdd( aStr, { 'FZNK1' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL1' , 'C', 23, 0 } )    //"YYYY-MM-DD hh:mm:ss.fff"
   AAdd( aStr, { 'ORAND1', 'C',  5, 0 } )
   AAdd( aStr, { 'FZNK2' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL2' , 'C', 23, 0 } )
   AAdd( aStr, { 'ORAND2', 'C',  5, 0 } )
   AAdd( aStr, { 'FZNK3' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL3' , 'C', 23, 0 } )
   AAdd( aStr, { 'FDEL'  , 'C',  3, 0 } )

   //IF lUtf8
   //   FOR nI := 1 TO LEN(aStr)
   //      IF aStr[nI,2] == "C"
   //         aStr[nI,3] := aStr[nI,3] * 2
   //      ENDIF
   //   NEXT
   //ENDIF

   ? "  ###", ProcNL(), cRdd, cAls, cCdp
   ? "  ###", cDbf
   //cDbf := App.Cargo:cPathTemp + "test_Filter.dbf"
   //? "  ###", cDbf
   lShared := .F.
   IF !FILE(cDbf)
      dbCreate(cDbf, aStr, cRdd)
      wApi_Sleep(100)
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
         lRet := ! NetErr() .and. Used()
      END SEQUENCE
      IF !lRet
         RETURN ""
      ENDIF

      FOR nI := 1 TO LEN(aFld)
         APPEND BLANK
         (cAls)->FTYPE := aFld[nI,1]
         (cAls)->FNAME := aFld[nI,2]
         (cAls)->FDEL  := "[x]"
      NEXT

      wApi_Sleep(100)
      (cAls)->( DbCloseArea() )

   ENDIF

   BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
      DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
      lRet := ! NetErr() .and. Used()
   END SEQUENCE

   DbSelectArea(cAls)
   IF LastRec() < LEN(aFld)
      (cAls)->( DbCloseArea() )
      wApi_Sleep(100)
      HB_FileDelete( cDbf )
   ENDIF

RETURN ALIAS()

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
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
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

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // настройки
   Local nHImg, oDlu, n

   //? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName
   // подгоним размеры колонок по фонту
   oDlu := _Font2oDlu( oTsb:aFont[1] )
   n    := oDlu:nSize
   //!!! варианты задания размера
   //? SPACE(5) + _HMG_DefaultFontName, _HMG_DefaultFontSize, "n=", n, oTsb:aFont[1]
   //? SPACE(5) + "!!!",n," oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   nHImg := oDlu:H1 + 6              // высота картинки = высота строк в ТСБ
   //                ^^^ - константа
   nHImg := oDlu:H(1.25)             // так правильнее, от размера фонта высота
   //              ^^^^  - пропорция от размера фонта
   WITH OBJECT oBrw

      :lNoKeyChar    := .F.          // НЕТ ввода в ячейки от букв, цифр
      :nHeightCell   := nHImg        // высота ячеек = высоте картинки - здесь нет картинки
      :nHeightHead   := nHImg * 1.2  // высота шапки
      :nHeightFoot   := nHImg + 4    // высота подвала
      :nHeightSpecHd := 12           // высота спецхидера ENUMERATOR
      :lFooting      := .T.          // использовать подвал
      :lDrawFooters  := .T.          // рисовать  подвалы
      :nFreeze       := 3            // Заморозить столбец
      :nCell         := :nFreeze + 1
      :lLockFreeze   := .T.          // Избегать прорисовки курсора на замороженных столбцах
      :nCellMarginLR :=  1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nMemoHV       :=  1           // показ 2 строки мемо-поля

      :Cargo:nModify := 0     // изменения в таблице

   END WITH

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
      :SetColor( { 16 }, { { ||  nClr16  }  } ) // 16, фона спецхидер
      :SetColor( { 17 }, { nClr17           } ) // 17, текста спецхидер
      // методы доступа
      //nHAlign := :nAlignSupHdGet(nCol)   // что установлено
      //:nAlignSupHdGet(nCol, nHAlign)
      //:aSuperHead[1,12] := DT_LEFT       // изменить отбивку текста
   END WIDTH

   o:TitleSupHd := oBrw:aSuperhead[ 1, 3 ]    // запомнить
   o:ColorSupHd := nClr16                     // 16, фона спецхидер

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////
// другие настройки тсб
STATIC FUNCTION myTsbKeyFX( oBrw, oTsb )
   LOCAL o := oBrw:Cargo      // использовать из контейнера свои переменные
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
      :SetDeleteMode( .F. )
      // обработка клавиши ESC и других
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      //:UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      //:UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })
      // клавиши FXX
      :UserKeys(VK_F4    , {|ob| myTsbListColumn( ob ) , ob:Setfocus() })  // инфо по списку колонок
      :UserKeys(VK_F5    , {|ob| myTsbListFont( ob )   , ob:Setfocus() })  // инфо по фонтам таблицы

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol

   FOR EACH oCol IN oBrw:aColumns
      IF oCol:cName == "SELECTOR" .OR. oCol:cName == "ORDKEYNO"  ; LOOP
      ENDIF
      IF oCol:cFieldTyp $ "+=@T"
         oCol:lEdit := .F.
      ENDIF
      //IF "NAME" $ oCol:cName
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }
      //ENDIF
   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
// ДЕЛАЕМ ПОСЛЕ END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw, oTsb )
   LOCAL nBCSpH, nI, cCol, oCol, a4Clr, nTest, nLen, nCol, hFont, nWCol, nW3Col
   LOCAL cBrw, nTsb, nW1Col, nW2Col, nWDel, nWZnk, nWAnd, aCol, nWTsb, nWSum

   nBCSpH := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы
   a4Clr  := oTsb:a4Clr                     // считаем 4 цвета таблицы
   nTest  := HMG_RGB2n(a4Clr[1])            // цвет фона окна

   //? ProcNL(), MGVersNumba()
   //? SPACE(5) + "SELECTOR =", oBrw:nColumn("SELECTOR", .T.), oBrw:lSelector
   //? SPACE(5) + "ORDKEYNO =", oBrw:nColumn("ORDKEYNO", .T.), IIF( oBrw:nColumn("ORDKEYNO", .T.) > 0, .T., .F.)

   IF oBrw:lDrawSuperHd  // Шаг-2: что-то не работает
      // увеличить суперхидер до конца колонок
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF
   oBrw:DrawHeaders()   // перечитать суперхидер/шапку/нумератор

   // замена строки нумератора колонок на свой цвет, кроме SELECTOR
   oBrw:lClrSelectorHdBack := .T. // background OFF
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      oCol:nClrSpcHdFore := CLR_RED
      oCol:nClrSpcHdBack := nBCSpH
   NEXT

   // изменение виртуальной колонки
   nLen := LEN(HB_NtoS(oBrw:nLen))
   nCol := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol  := oBrw:GetColumn("ORDKEYNO")
      hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // кол-во знаков + 2 знака
      oCol:nWidth := nWCol                                       // новая ширина
      // изменим цвет
      oCol := oBrw:GetColumn("ORDKEYNO")
      oCol:nClrBack      := nBCSpH
      oCol:nClrFore      := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      oCol:nClrFootFore  := CLR_WHITE
      oCol:SaveColor()                      // сохранить цвета колонки
      nW1Col := nWCol
   ENDIF

   nLen := LEN(HB_NtoS(oBrw:nLen))
   FOR EACH oCol IN oBrw:aColumns
      nCol := hb_EnumIndex(oCol)
      // изменение ширины поля типа "+"
      //IF oCol:cFieldTyp == "+"
      //   hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
      //   nWCol := GetTextWidth( Nil, REPL("0", nLen + 4), hFont )   // кол-во знаков + 2 знака + 2 знак отступ
      //   oCol:nWidth := nWCol                                       // новая ширина
      //ENDIF
      IF oCol:cFieldTyp == "C"
         oCol:bDecode := {|val| ALLTRIM(val) }
         oCol:nAlign  := DT_CENTER
         oCol:nFAlign := DT_CENTER
      ENDIF
   NEXT

   // ручная разбивка ширины колонок
   oCol := oBrw:GetColumn("FTYPE")
   hFont  := oBrw:aColumns[nCol]:hFont                        // какой фонт в колонке
   nW2Col := GetTextWidth( Nil, REPL("X", 5), hFont )         // кол-во знаков
   oCol:nWidth := nW2Col

   oCol := oBrw:GetColumn("FNAME")
   hFont  := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
   nW3Col := GetTextWidth( Nil, REPL("X", 15), hFont )         // кол-во знаков
   oCol:nWidth := nW3Col
   oCol:nAlign := DT_RIGHT

   oCol  := oBrw:GetColumn("FZNK1")
   hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
   nWZnk := GetTextWidth( Nil, REPL("X", 5), hFont )          // кол-во знаков " >= "
   oCol:nWidth := nWZnk                                       // новая ширина

   oCol  := oBrw:GetColumn("FZNK2")
   oCol:nWidth := nWZnk

   oCol  := oBrw:GetColumn("FZNK3")
   oCol:nWidth := nWZnk

   oCol  := oBrw:GetColumn("ORAND1")
   nWAnd := oCol:nWidth

   oCol  := oBrw:GetColumn("ORAND2")
   oCol:nWidth := nWAnd

   cBrw := oBrw:cControlName
   nTsb := This.&(cBrw).ClientWidth
   nLen := oBrw:GetAllColsWidth() - 1
   ? ProcNL(), "cBrw=", cBrw, "ClientWidth=", nTsb, nLen
   IF nLen > nTsb
      //oBrw:lAdjColumn  := .T.
      //oBrw:lNoHScroll  := .F.
      //oBrw:lMoreFields := ( oBrw:nColCount() > 45 )
   ELSE
      //oBrw:AdjColumns()
   ENDIF

   /*
   AAdd( aStr, { 'FTYPE' , 'C',  5, 0 } )
   AAdd( aStr, { 'FNAME' , 'C', 15, 0 } )
   AAdd( aStr, { 'FZNK1' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL1' , 'C', 23, 0 } )    //"YYYY-MM-DD hh:mm:ss.fff"
   AAdd( aStr, { 'ORAND1', 'C',  5, 0 } )
   AAdd( aStr, { 'FZNK2' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL2' , 'C', 23, 0 } )
   AAdd( aStr, { 'ORAND2', 'C',  5, 0 } )
   AAdd( aStr, { 'FZNK3' , 'C',  2, 0 } )
   AAdd( aStr, { 'FVAL3' , 'C', 23, 0 } )
   AAdd( aStr, { 'FDEL'  , 'C',  3, 0 } )
   */

   oCol  := oBrw:GetColumn("FDEL")
   hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
   nWDel := GetTextWidth( Nil, REPL("X", 5), hFont )          // кол-во знаков "x[x]x"

   // ручная разбивка ширины колонок
   aCol  := {"FVAL1","FVAL2","FVAL3"}
   nWTsb := oBrw:GetAllColsWidth() - 1
   nWSum := GetVScrollBarWidth() + nW1Col + nW2Col + nW3Col + nWAnd*2 + nWDel + nWZnk*3
   nWCol := ( nTsb - nWSum ) / 3

   FOR nI := 1 TO LEN(aCol)
      oCol := oBrw:GetColumn(aCol[nI])
      oCol:nWidth := nWCol
   NEXT

   //oCol  := oBrw:GetColumn("FDEL")
   //oCol:nWidth := nWDel                                       // новая ширина

   oBrw:AdjColumns({"FDEL"})

   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, c1Typ
   LOCAL cTyp, cMsg, cRet, lWrtUDT, cStr, xGet

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // тип обработки колонки
      cNam  := oCol:cName
      c1Typ := :GetValue('FTYPE')
   END WITH
   oCol:nClrEditBack := CLR_GRAY
   uOld := uVal

   ? SPACE(5) + ProcNL(), nCol, cTyp, c1Typ
   lWrtUDT := .T.                        // записывать изменения
   lRet    := .T.                        // давать редактировать поле в :get
   cStr    := 'oCol:bPrevEdit !;'
   cStr    += 'oCol:cName = ' + cNam + ';'
   cStr    += 'NO PROCESSING of field type: ' + c1Typ

   IF c1Typ $ "NL^IB+YZQ" .OR. c1Typ == "Q:U" .OR. c1Typ == "Q:B"
      // стандартная обработка
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF c1Typ $ "CMQV" .OR. c1Typ == "C:U" .OR. c1Typ == "C:B" .OR. ;
          c1Typ == "M:U" .OR. c1Typ == "W" .OR. c1Typ == "P"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      //IF AT(CRLF,uVal) > 0
      //   cRet := CellEditMemo(uVal, oBrw)
      //ELSEIF cTyp == "M"
      //   cRet := CellEditMemo(uVal, oBrw)
      //ENDIF
   ELSEIF c1Typ $ "=@T"
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      lRet    := .F.     // не давать редактировать поле в :get
      lWrtUDT := .F.     // не редактировать вообще
   ENDIF

   IF lWrtUDT                                 // записывать User+Date+Time
      IF (oBrw:cAlias)->(RLock())             // делать самому
         xGet := oBrw:GetValue(nCol)          // значение тек.поля
         cRet := myFieldVal(cNam,oBrw,c1Typ,xGet)
         lRet := .F.           // не давать редактировать поле в :get
         IF 'FVAL' $ cNam      // значение - FVAL1, FVAL2, FVAL3
            IF c1Typ $ 'CMN'
               lRet := .T.     // давать редактировать поле в :get
            ENDIF
         ENDIF
         IF LEN(cRet) > 0   // для ("C" + CRLF) и ("M")
            oBrw:SetValue(nCol,cRet)
         ENDIF
         (oBrw:cAlias)->( DbUnlock() )
         (oBrw:cAlias)->( DbCommit() )
      ELSE
         cMsg := "Recording is locked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop( cMsg )
      ENDIF
   ENDIF
   oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы

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

   ? SPACE(5) + ProcNL(), nCol, cTyp  //, (cAls)->(IndexOrd()), (cAls)->(OrdSetFocus()), oBrw:uLastTag
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'Column processing type: "' + cTyp + '" ;'
   cStr += 'oCol:cName = ' + cNam

   IF cTyp $ "CNDLI"
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

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myFieldVal(cName,oBrw,c1Typ,xGet)
   LOCAL cRet := ""

   //IF cName == 'FSK1' .OR. cName == 'FSK2'
   //   cRet := myGetSkoba(oBrw,cName)
   IF 'ORAND' $ cName
      cRet := myGetOrAnd(oBrw,cName)
   ELSEIF 'FZNK' $ cName
      cRet := myGetZnak(oBrw,cName)
   ELSEIF 'FDEL' $ cName
      cRet := myLineDelFilter(oBrw)
   ELSE
      IF c1Typ $ 'DL=@T'
         cRet := CELL_AllExceptCMN(oBrw,c1Typ,xGet)
      ENDIF
   ENDIF

RETURN cRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLineDelFilter(oBrw)

   oBrw:SetValue( 'FZNK1' ,"")
   oBrw:SetValue( 'FVAL1' ,"")
   oBrw:SetValue( 'ORAND1',"")
   oBrw:SetValue( 'FZNK2' ,"")
   oBrw:SetValue( 'FVAL2' ,"")
   oBrw:SetValue( 'ORAND2',"")
   oBrw:SetValue( 'FZNK3' ,"")
   oBrw:SetValue( 'FVAL3' ,"")
   oBrw:DrawSelect()            // одна ячейка
   oBrw:DrawLine()              // перечитывает текущую строку
   //oBrw:Refresh()             // перечитывает данные в таблице

RETURN ""

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CELL_AllExceptCMN(oBrw,c1Typ,xGet)
   LOCAL oCell := oBrw:GetCellInfo(oBrw:nRowPos)
   LOCAL nY := oCell:nRow + oBrw:nHeightHead
   LOCAL nX := oCell:nCol
   LOCAL nW := oCell:nWidth - 2
   LOCAL nH := oCell:nHeight - 2
   LOCAL oWnd, hWnd, oJWnd, cRet, cForm, nWBtn, nHObj, nHIco, ao
   LOCAL cFont, nFSize, cText, nWDate, dDate1, tDTime

   oJWnd := oBrw:Cargo:ObjWnd        // текущее окно
   cForm := oJWnd:Name
   nY    += oJWnd:Row
   nX    += oJWnd:Col + 3
   //IF oBrw:lDrawSuperHd
   //    nY -= oBrw:nHeightSuper
   //ENDIF
   IF oBrw:lDrawSpecHd
      nY -= oBrw:nHeightSpecHd    // высота спецхидера ENUMERATOR
   ENDIF

   IF c1Typ == "L"
      cRet := myGetLogik(cForm, nY, nX)
      RETURN cRet
   ENDIF

   nY     += IIF( App.Cargo:aDisplayMode[2] <= 720, 4, -4)  // ручная правка
   nH     += IIF( App.Cargo:aDisplayMode[2] <= 720, 3, 0)   // ручная правка
   ao     := (App.Cargo)
   cFont  := ao:cFontName
   nFSize := ao:nFontSize
   cRet   := ""
   nHObj  := nH //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120ДЕКАБРЯ020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 70
   IF c1Typ $ "=@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 50
   ENDIF
   nWBtn  := nH + nH + 4  // две кнопки
   nW     := nWDate + nWBtn
   dDate1 := DATE()
   tDTime := hb_DateTime()

   // новое окно в ячейку таблицы
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH MODAL NOCAPTION ;
      FONT cFont SIZE nFSize                                      ;
      ON LOSTFOCUS oWnd:Release()                                 ;
      ON INIT {|| DoEvents() }

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle

      //@ nY+1, nX+1 GETBOX GB_Focus VALUE "" WIDTH nW - 3 HEIGHT nH - 3 FONTCOLOR BLACK BACKCOLOR WHITE
        //PICTURE REPL('x',480) ON CHANGE {|| cFilter := This.GB_Fltr.Value }
      IF c1Typ == "D"
         dDate1 := CTOD(xGet)
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 0, 0 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE ON CHANGE {|| cRet := This.Date_1.Value }
         nX := This.Date_1.Width + 2

         //@ 0, nX GETBOX Button_2 VALUE '' WIDTH nWBtn HEIGHT nHObj BUTTONWIDTH nHObj ;
         //  ON GOTFOCUS {|| SendMessage(This.Handle, 177 /*EM_SETSEL*/, 0, Len( This.Value )) } ;
         //  ON INIT {|| _SetAlign ( This.Name, ThisWindow.Name, "CENTER" ) } ;
         //  IMAGE { "MINIGUI_EDIT_OK","MINIGUI_EDIT_CANCEL" }                ;
         //  ACTION  ( cRet := DTOC(This.Date_1.Value) , oWnd:Release() )     ;
         //  ACTION2 ( cRet := "  " , oWnd:Release() )

         @ 0, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  cRet := "  " , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

         nX += This.Btn_Esc.Width + 2

         @ 0, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| cRet := DTOC(This.Date_1.Value) , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

      ELSEIF c1Typ $ "=@T"

         @ 0, 0 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), 00, 00, 00 }
         nX := This.Date_2.Width + 2

         @ 0, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  cRet := "  " , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

         nX += This.Btn_Esc.Width + 2

         @ 0, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value ,;
                      cRet := hb_TToC( tDTime, "YYYY-MM-DD", "hh:mm:ss.fff" ) , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

      ENDIF

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } )
   Cell.Activate

RETURN cRet

/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myGetLogik(cForm, nY, nX)
   LOCAL aMenu, nBmpSize, nFSize, nChoice, nPos, lExit
   LOCAL xRet, aFntExt, aMsg, aRus, aEng

   aRus := { " ДА (.T.)" , " НЕТ (.F.)" , "удалить" }
   aEng := { " YES (.T.)", " NO (.F.)"  , "delete"  }
   aMsg := IIF( App.Cargo:cLang == "RU", aRus, aEng )

   aMenu := {}
   AADD( aMenu, { ""         , aMsg[1] , .F. , "-", 1 } )
   AADD( aMenu, { ""         , aMsg[2] , .F. , "-", 2 } )
   AADD( aMenu, {                                     } )
   AADD( aMenu, { "iDelete32", aMsg[3] , .F. , "-", 3 } )

   xRet     := ""
   nPos     := { nY, nX }
   nBmpSize := IIF( App.Cargo:aDisplayMode[2] <= 720, 26, 32 )
   nFSize   := IIF( App.Cargo:aDisplayMode[2] <= 720, 12, 16 )
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos := aMenu[nChoice,5]
      IF nPos == 1
         xRet := ".T."
      ELSEIF nPos == 2
         xRet := ".F."
      ELSEIF nPos == 3
         xRet := "  "
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetZnak(oBrw,cName)
   LOCAL aMenu, nBmpSize, nFSize, nChoice, nPos, lExit, oCell, aUsl, aZnk
   LOCAL xRet, cForm, aFntExt, cMsg, nY, nX, nW, nH, oJWnd, nI

   ? ProcNL(), cName
   cForm := oBrw:cParentWnd
   oJWnd := oBrw:Cargo:ObjWnd        // текущее окно
   oCell := oBrw:GetCellInfo(oBrw:nRowPos)
   nY    := oCell:nRow + oJWnd:Row + oBrw:nHeightHead + 4
   nX    := oCell:nCol + oJWnd:Col
   nW    := oCell:nWidth
   nH    := oCell:nHeight

   aUsl := {}
   IF App.Cargo:cLang == "RU"
      AADD(aUsl,"равно (==)         ")
      AADD(aUsl,"не равен (#)       ")
      AADD(aUsl,"больше и равно (>=)")
      AADD(aUsl,"меньше и равно (<=)")
      AADD(aUsl,"больше (>)         ")
      AADD(aUsl,"меньше (<)         ")
      AADD(aUsl,"содержит ($)       ")
      cMsg := "удалить"
   ELSE
      AADD(aUsl,"equal (==) ")
      AADD(aUsl,"not equal (#) ")
      AADD(aUsl,"greater than and equal (>=)")
      AADD(aUsl,"less than and equal (<=)")
      AADD(aUsl,"greater than (>) ")
      AADD(aUsl,"less than (<) ")
      AADD(aUsl,"contains ($) ")
      cMsg := "delete"
   ENDIF

   aZnk := {}
   AADD(aZnk,"==")
   AADD(aZnk,"#" )
   AADD(aZnk,">=")
   AADD(aZnk,"<=")
   AADD(aZnk,">" )
   AADD(aZnk,"<" )
   AADD(aZnk,"$" )
   AADD(aZnk," " )  // удалить

   aMenu := {}
   FOR nI := 1 TO LEN(aUsl)
       AADD( aMenu, { "", aUsl[nI] , .F. , "-", nI } )
   NEXT
   AADD( aMenu, {                                       } )
   AADD( aMenu, { "iDelete32", cMsg   , .F. , "-", -1   } )

   xRet     := ""
   nPos     := { nY, nX }
   nBmpSize := IIF( App.Cargo:aDisplayMode[2] <= 720, 26, 32 )
   nFSize   := IIF( App.Cargo:aDisplayMode[2] <= 720, 12, 16 )
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos := aMenu[nChoice,5]
      IF nPos == -1
         xRet := "   "
      ELSE
         xRet := aZnk[nPos]
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetOrAnd(oBrw,cName)
   LOCAL aMenu, nBmpSize, nFSize, nChoice, nPos, lExit, oCell, aUsl
   LOCAL xRet, cForm, aFntExt, cMsg, nY, nX, nW, nH, oJWnd, nI

   ? ProcNL(), cName
   cForm := oBrw:cParentWnd
   oJWnd := oBrw:Cargo:ObjWnd        // текущее окно
   oCell := oBrw:GetCellInfo(oBrw:nRowPos)
   nY    := oCell:nRow + oJWnd:Row + oBrw:nHeightHead + 4
   nX    := oCell:nCol + oJWnd:Col
   nW    := oCell:nWidth
   nH    := oCell:nHeight

   aUsl := {}
   IF App.Cargo:cLang == "RU"
      AADD(aUsl,"  .И.  (.AND.)")
      AADD(aUsl," .ИЛИ. (.OR.) ")
      cMsg := "удалить"
   ELSE
      AADD(aUsl," .AND. ")
      AADD(aUsl," .OR.  ")
      cMsg := "delete"
   ENDIF

   aMenu := {}
   FOR nI := 1 TO LEN(aUsl)
       AADD( aMenu, { "", aUsl[nI] , .F. , "-", nI } )
   NEXT
   AADD( aMenu, {                                       } )
   AADD( aMenu, { "iDelete32", cMsg   , .F. , "-", -1   } )

   xRet     := ""
   nPos     := { nY, nX }
   nBmpSize := IIF( App.Cargo:aDisplayMode[2] <= 720, 26, 32 )
   nFSize   := IIF( App.Cargo:aDisplayMode[2] <= 720, 12, 16 )
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos := aMenu[nChoice,5]
      IF nPos == -1
         xRet := "   "
      ELSEIF nPos == 1
         xRet := ".AND."  //ALLTRIM(aUsl[nPos])
      ELSEIF nPos == 2
         xRet := ".OR."   //ALLTRIM(aUsl[nPos])
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myFilter2Get(owc)   // выбор из 2х фильтров на форме
   LOCAL cObjGb, cVal, cRet
   cObjGb := owc:cGetBox   // "GB_Fltr"
   cVal   := ALLTRIM( This.&(cObjGb).Value )
   IF LEN(cVal) > 0
      cRet := cVal
      cRet := FilterCheck(owc,cRet)     // проверка фильтра
   ELSE
      cRet := FilterDbf(owc)            // создание фильтра таблицы
      cRet := FilterCheck(owc,cRet)     // проверка фильтра
   ENDIF
RETURN cRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION FilterDbf(owc)   // создание фильтра
   LOCAL cAls, aDim, cMsg, aRows, lAdd, aFld, a2Fld, cRet, nI, cTyp, cNam
   LOCAL cZnk1, cZnk2, cZnk3, cVal1, cVal2, cVal3, cAnd1, cAnd2, aVal
   LOCAL cStr, nStr, cTmp, aLine, cRus, cEng, cValue

   ? ProcNL(), "-----", ALIAS()
   cRus := "ОШИБКА ! Фильтр пустой !;Не заполнены данные в таблице фильтра"
   cEng := "ERROR! Filter is empty!; Data in filter table is not filled"
   aDim := {}
   aFld := {}
   cAls := owc:cAls            // база фильтра
   DbSelectArea(cAls)
   GOTO TOP
   DO WHILE !EOF()
      lAdd  := .F.
      cZnk1 := ALLTRIM((cAls)->FZNK1)
      cZnk2 := ALLTRIM((cAls)->FZNK2)
      cZnk3 := ALLTRIM((cAls)->FZNK3)
      cVal1 := ALLTRIM((cAls)->FVAL1)
      cVal2 := ALLTRIM((cAls)->FVAL2)
      cVal3 := ALLTRIM((cAls)->FVAL3)
      cAnd1 := ALLTRIM((cAls)->ORAND1)
      cAnd2 := ALLTRIM((cAls)->ORAND2)
      IF LEN(cZnk1) > 0 .OR. LEN(cZnk2) > 0 .OR. LEN(cZnk3) > 0
         lAdd := .T.
      ENDIF
      IF LEN(cVal1) > 0 .OR. LEN(cVal2) > 0 .OR. LEN(cVal3) > 0
         lAdd := .T.
      ENDIF
      IF LEN(cAnd1) > 0 .OR. LEN(cAnd2) > 0
         lAdd := .T.
      ENDIF
      IF lAdd
         //           1      2     3       4      5      6      7      8
         aRows := { cZnk1, cVal1, cAnd1, cZnk2 , cVal2, cAnd2, cZnk3, cVal3 }
         a2Fld := { ALLTRIM((cAls)->FNAME) , (cAls)->FTYPE }
         AADD( aDim, aRows )
         AADD( aFld, a2Fld )
      ENDIF
      SKIP
   ENDDO

   cRet := ""
   IF LEN(aDim) > 0
      aLine := {}
      FOR nI := 1 TO LEN(aDim)
         cNam := aFld[nI,1]
         cTyp := UPPER(ALLTRIM(aFld[nI,2]))
         aVal := aDim[nI]
         cStr := "" ; nStr := 1
         //? "      .", nI, "["+cTyp+"]", cNam, HB_ValToExp(aVal)
         IF cTyp $ "CMQV" .OR. cTyp == "C:U" .OR. cTyp == "C:B" .OR. cTyp == "M:U" .OR. ;
            cTyp == "W" .OR. cTyp == "P"
            //aAdd( aType, {"C:U",         "C"  , "nChar"                      } )
            //aAdd( aType, {"C:B",         "C"  , "Raw"                        } )
            //aAdd( aType, {"M:U",         "M"  , "nMemo"                      } )
            cZnk1 := aVal[1]
            IF cZnk1 == "$" // содержит
               cStr += " '" + UPPER(aVal[2]) + "' " + cZnk1 + " UPPER(" + cNam + ")"
            ELSE
               cStr += UPPER(cNam) + " " + cZnk1 + " '" + UPPER(aVal[2]) + "' "
            ENDIF
            cZnk2 := aVal[4]
            IF LEN(cZnk2) > 0
               IF cZnk2 == "$" // содержит
                  cStr += " " + aVal[3] + " '" + UPPER(aVal[5]) + "' " + cZnk2 + " UPPER(" + cNam + ")"
               ELSE
                  cStr += " " + aVal[3] + cNam + " " + cZnk2 + " '" + aVal[5] + "' "
               ENDIF
               nStr++
            ENDIF
            cZnk3 := aVal[7]
            IF LEN(cZnk3) > 0
               IF cZnk3 == "$" // содержит
                  cStr += " " + aVal[6] + " '" + UPPER(aVal[8]) + "' " + cZnk2 + " UPPER(" + cNam + ")"
               ELSE
                  cStr += " " + aVal[6] + cNam + " " + cZnk2 + " '" + aVal[8] + "' "
               ENDIF
               nStr++
            ENDIF
            IF nStr > 1
               cTmp := cStr
               cStr := "( " + cTmp + " )"
            ENDIF
         ELSEIF cTyp $ "NL^IB+YZQ" .OR. cTyp == "Q:U" .OR. cTyp == "Q:B"
            //aAdd( aType, {"I",           "N"  , "Integer, ShortInt, LongInt" } )
            //aAdd( aType, {"B",           "N"  , "Double"                     } )
            //aAdd( aType, {"+",           "N"  , "Autoinc"                    } )
            //aAdd( aType, {"N",           "N"  , "Numeric"                    } )
            //aAdd( aType, {"Y",           "N"  , "Money"                      } )
            //aAdd( aType, {"Z",           "N"  , "Curdouble"                  } )
            //aAdd( aType, {"Q",           "N"  , "VarCharFox"                 } )
            //aAdd( aType, {"Q:U",         "N"  , "nVarChar"                   } )
            //aAdd( aType, {"Q:B",         "N"  , "VarBinaryFox"               } )
            cStr += cNam + " " + aVal[1] + " " + aVal[2] + " "
            IF LEN(aVal[4]) > 0
               nStr++
               cStr += " " + aVal[3] + cNam + " " + aVal[4] + " " + aVal[5] + " "
            ENDIF
            IF LEN(aVal[7]) > 0
               nStr++
               cStr += " " + aVal[6] + cNam + " " + aVal[7] + " " + aVal[8] + " "
            ENDIF
            IF nStr > 1
               cTmp := cStr
               cStr := "( " + cTmp + " )"
            ENDIF
         ELSEIF cTyp == "D"
            cStr += cNam + " " + aVal[1] + " CTOD('" + aVal[2] + "') "
            IF LEN(aVal[4]) > 0
               nStr++
               cStr += " " + aVal[3] + cNam + " " + aVal[4] + " CTOD('" + aVal[5] + "') "
            ENDIF
            IF LEN(aVal[7]) > 0
               nStr++
               cStr += " " + aVal[6] + cNam + " " + aVal[7] + " CTOD('" + aVal[8] + "') "
            ENDIF
            IF nStr > 1
               cTmp := cStr
               cStr := "( " + cTmp + " )"
            ENDIF
         ELSEIF cTyp $ "=@T"
            cValue := aVal[2]    // "YYYY-MM-DD hh:mm:ss.fff"
            cStr += cNam + ' ' + aVal[1] + ' t"' + cValue + '" '
            IF LEN(aVal[5]) > 0
               cValue := aVal[6]
               nStr++
               cStr += ' ' + aVal[3] + cNam + ' ' + aVal[5] + ' t"' + cValue + '" '
            ENDIF
            IF LEN(aVal[7]) > 0
               cValue := aVal[8]
               nStr++
               cStr += ' ' + aVal[6] + cNam + ' ' + aVal[7] + ' t"' + cValue + '" '
            ENDIF
            IF nStr > 1
               cTmp := cStr
               cStr := "( " + cTmp + " )"
            ENDIF
         ENDIF
         IF LEN(cStr) > 0
            AADD( aLine, cStr )
         ENDIF
         ?? cStr
      NEXT
      //? "aLine=", aLine ; ?v aLine
      // строка фильтра
      IF LEN(aLine) == 1
         cRet += aLine[1]
      ELSE
         FOR nI := 1 TO LEN(aLine)
            cRet += aLine[nI]
            cRet += IIF( nI == LEN(aLine), "" , " .AND. " )
         NEXT
      ENDIF
   ENDIF

   IF LEN(cRet) == 0
      cMsg := IIF( App.Cargo:cLang == "RU", cRus, cEng )
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
   ENDIF

RETURN cRet

//////////////////////////////////////////////////////////////////////////////
// Проверка правильности условия фильтра
STATIC FUNCTION FilterCheck(owc,cFltr)
   LOCAL cMsg, xVal, bOldError, oError, lFiltr, cRet, aDim, lFnd, nI
   LOCAL cAls, cAlsParent, cRus, cEng

   cRus := "ОШИБКА ! Фильтр НЕПРАВИЛЬНЫЙ !;Нет знаков сравнения в таблице фильтра"
   cEng := "ERROR! Filter is INCORRECT!;No comparison signs in filter table"
   cMsg := IIF( App.Cargo:cLang == "RU", cRus, cEng )
   aDim := { ">", "<", "=", "#", "$", "()" }
   lFnd := .F.
   FOR nI := 1 TO LEN(aDim)
      IF AT(aDim[nI],cFltr) > 0
         lFnd := .T.
         EXIT
      ENDIF
   NEXT
   IF !lFnd
      AlertStop( cMsg, , "ZZZ_B_STOP64", 64, {RED} )
      cRet := ""
      RETURN cRet
   ENDIF

   cAlsParent := owc:cAlsParent      // база с родительского окна
   cAls       := owc:cAls            // база фильтра
   lFiltr     := .T.

   DbSelectArea(cAlsParent)
   bOldError := ErrorBlock( { |e|break( e ) } )
   BEGIN SEQUENCE
      xVal := &( cFltr )
   RECOVER USING oError
      lFiltr := .F.
   END SEQUENCE
   ErrorBlock( bOldError )

   IF ! lFiltr
      cRus := "Установлен фильтр:;[ "+cFltr+" ] ;;"
      cRus += "Не правильно установлены параметры фильтра;"
      cRus += "Установите правильно параметры и повторите заново !;"
      cEng := "Filter installed:;[ "+cFltr+" ] ;;"
      cEng += "The filter parameters are not set correctly;"
      cEng += "Set the parameters correctly and try again!;"
      cMsg := IIF( App.Cargo:cLang == "RU", cRus, cEng )
      AlertStop( cMsg , , "ZZZ_B_STOP64", 64 )
      cRet := ""
   ELSE
      cRet := cFltr
   ENDIF

   DbSelectArea(cAls)

RETURN cRet

//////////////////////////////////////////////////////////////////////////////
// Проверка правильности условия фильтра
STATIC FUNCTION FilterListIni(ow)
   LOCAL aMenu, nBmpSize, nFSize, nChoice, nPos, lExit, cIcon, nI
   LOCAL cRet, cForm, aFntExt, aList

   cForm := ow:Name
   cIcon := "iFiltr64x2"
   aList := myCheckFileIni()   // считать список фильтров из ини-файла
   aMenu := {}
   FOR nI := 1 TO LEN(aList)
       AADD( aMenu, { cIcon, aList[nI] , .F. , "-", "MsgDebug", "Stroka1" , nI } )
   NEXT

   cRet     := ""
   nPos     := 3   // по мышке
   nBmpSize := IIF( App.Cargo:aDisplayMode[2] <= 720, 26, 32 )
   nFSize   := IIF( App.Cargo:aDisplayMode[2] <= 720, 12, 16 )
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos := aMenu[nChoice,5]
      cRet := aMenu[nChoice,2]
   ENDIF
   DO EVENTS

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myCheckFileIni()
   LOCAL nI, aList, cKey, cRet, oIni, oSec

   oIni := App.Cargo:oIni    // берем адрес объекта oIni и от него работаем

   //MsgDebug("1", oIni:Get("Filter"),Empty( oSec := oIni:Get("Filter") ) )

   IF Empty( oSec := oIni:Get("Filter") )
      //MsgDebug(Empty( oSec := oIni:Get("Filter") ))
      oIni:Set("Filter", oHmgData())           // секцию добавили
      oIni:Filter:Set("List_01", "Deleted()")
   ENDIF
   oSec := oIni:Get("Filter")

   // MsgDebug("2", oSec)

   aList := {}
   FOR nI := 1 TO 99
      cKey := "List_" + STRZERO(nI,2)
      cRet := oSec:Get(cKey)
      IF cRet == NIL
      ELSE
         AADD( aList, cRet )
      ENDIF
   NEXT

RETURN aList

///////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION FilterSaveIni(ow,cFilter)   // Записать фильтр в ини-файл
   LOCAL cForm, aList, cObjGB, lFind, cKey, nI, oIni, oSec

   IF  hb_FileExists(App.Cargo:cIniFile)  // если есть ини-файл к программе

      cForm  := ow:Name
      cObjGB := ow:Cargo:cGetBox   // "GB_Fltr"
      aList  := myCheckFileIni()   // считать список фильтров из ини-файла
      lFind  := .F.
      cKey   := "List_" + STRZERO( LEN(aList)+1,2)

      FOR nI := 1 TO LEN(aList)
         IF ALLTRIM(UPPER(aList[nI])) == ALLTRIM(UPPER(cFilter))
            lFind := .T.
            EXIT
         ENDIF
      NEXT

      IF !lFind
         oIni := App.Cargo:oIni       // берем адрес объекта oIni и от него работаем
         // запись в ини-файл
         //oIni:FILTER:Set(cKey, cFilter) - что-то не работает
         oSec := oIni:Get("FILTER")
         oSec:Set(cKey, cFilter)
         //
         oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
         oIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе
         DO EVENTS
      ENDIF

   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Refresh_Tsb(ow)                 // поиск по базе
   LOCAL oBrw  := ow:Cargo:oBrw
   LOCAL cObj  := ow:Cargo:cGBFind
   LOCAL cFind := This.&(cObj).Value

   cFind := Alltrim(cFind)
   IF Empty(cFind) ; cFind := Nil
   ENDIF

   IF ISOBJECT(oBrw)
      oBrw:FilterFTS( cFind, .T. )
      oBrw:Refresh()
      oBrw:SetFocus()
   ENDIF
   DO EVENTS
   ow:Setfocus(cObj)

RETURN Nil

////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myHelpFilter()
   LOCAL cMsg, cTtl

   IF App.Cargo:cLang == "RU"
      cTtl := "Помощь"
      cMsg := "Установка фильтра для таблицы;;"
      cMsg += "Можно поставить единичный фильтр, допустим: KCity == 1 (покажет только с кодом 1);"
      cMsg += "Или двойной фильтр: KCity == 1 .AND. KCity == 5 (покажет с кодом 1 и 5);"
      cMsg += "Другой вариант фильтра: KCity >= 1 .AND. KCity <= 5 (покажет с кодом 1,2,3,4,5);"
      cMsg += "Или тройной фильтр: KCity >= 1 .AND. KCity <= 3 .OR. KCity == 60 (покажет с кодом 1,3,60);"
      cMsg += "Далее можно установить фильтр по нескольким колонкам таблицы одновременно.;"
      cMsg += "Для текстовых полей *(С,M) ставьте знак: $-содержит !;;"
      cMsg += "Очистить строку от условий - нажмите [x] в конце строки фильтра;;"
      cMsg += "ВНИМАНИЕ ! Ручной фильтр показывает в ПЕРВУЮ очередь, то что набрано в таблице - игнорируется"
   ELSE
      cTtl := "Help"
      cMsg := "Setting a filter for the table;;"
      cMsg += "You can set a single filter, for example: KCity == 1 (will show only with code 1);"
      cMsg += "Or a double filter: KCity == 1 .AND. KCity == 5 (will show with code 1 and 5);"
      cMsg += "Another filter option: KCity >= 1 .AND. KCity <= 5 (will show with code 1,2,3,4,5);"
      cMsg += "Or a triple filter: KCity >= 1 .AND. KCity <= 3 .OR. KCity == 60 (will show with code 1,3,60);"
      cMsg += "Then you can set a filter for several table columns at the same time.;"
      cMsg += "For text fields *(C,M) put the sign: $-contains !;;"
      cMsg += "Clear line from conditions - press [x] at the end of the filter line;;"
      cMsg += "ATTENTION! The manual filter shows FIRST, what is entered in the table is ignored"
   ENDIF

   AlertInfo( cMsg, cTtl, "iFiltr64x1", 64, {RED}, .T. , , .F. )

RETURN NIL

