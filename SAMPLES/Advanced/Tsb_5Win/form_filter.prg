/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Меню для фильтра / Filter menu
*/

#include "minigui.ch"

//////////////////////////////////////////////////////////////////////////
FUNCTION MenuFltr(oBrw,cMenu2,cName)
   LOCAL o, cIco, cTitle, cFont, nFSize, nW, nH, aBColor, nHBtn, nWBtn
   LOCAL aBtnFnt, aFntClr, aRet, nGRow, nGCol, nHLine, nY, nX, nX2, nX3
   LOCAL cCapt, aBtnGrd, aGrOverEx, aGrFillEx, aGrOverOk, aGrFillOk
   LOCAL a3Dim, aValCmb, aValDbf, nWUsl, nWTxt, aUsl, aZn, nTable, nWR
   LOCAL nUsl1, nUsl2, cZnak1, cZnak2, nValDb1, nValDb2, nAndOr, nI
   LOCAL nWUsl2, cText, aGrOver, aGrFill, cValIs1, cValIs2, cValTyp

   o       := oBrw:Cargo                 // получить данные из объекта
   nTable  := o:nTable                   // номер таблицы
   aBColor := HMG_n2RGB( o:nClr_2 )      // цвет фона таблицы
   nW      := 690
   nH      := 430
   cIco    := "iSearch48x1"
   cTitle  := "Пользовательский фильтр"
   cFont   := "Tahoma"
   nFSize  := ModeSizeFont() + 2
   aBtnFnt := { "Comic Sans MS", nFSize + 2 }
   aFntClr := { BLACK , YELLOW }
   nWBtn   := 120                        // ширина кнопки
   nHBtn   := 55                         // высота кнопки
   nGRow   := 20                         // отступ сверху/снизу
   nGCol   := 30                         // отступ слева/справа
   nHLine  := nFSize * 2                 // высота строки на форме
   aRet    := {}                         // вернуть фильтр для таблицы
   a3Dim   := ListOneColumn(oBrw,cName)  // значение колонки из базы
   aValCmb := a3Dim[1]
   aValDbf := a3Dim[2]
   cValTyp := a3Dim[3]                   // тип поля
   nValDb1 := nValDb2 := 0
   cValIs1 := cValIs2 := ""
   nAndOr  := 1

   aUsl    := {}
   aZn     := {}
   AADD(aUsl,"                     ")  ;  AADD(aZn,"    ")
   AADD(aUsl," равно (==)          ")  ;  AADD(aZn," == ")
   AADD(aUsl," не равен (#)        ")  ;  AADD(aZn," #  ")
   AADD(aUsl," больше (>)          ")  ;  AADD(aZn," >  ")
   AADD(aUsl," меньше (<)          ")  ;  AADD(aZn," <  ")
   AADD(aUsl," больше и равно (>=) ")  ;  AADD(aZn," >= ")
   AADD(aUsl," меньше и равно (<=) ")  ;  AADD(aZn," <= ")
   IF cValTyp $ "CM"
      AADD(aUsl," содержит ($)     ")  ;  AADD(aZn," $ ")
   ELSEIF cValTyp == "L"
      aUsl := {}
      aZn  := {}
      AADD(aUsl,"            ")  ;  AADD(aZn,"    ")
      AADD(aUsl," равно (==) ")  ;  AADD(aZn," == ")
   ENDIF

   nWUsl := 0
   FOR nI := 1 TO LEN(aUsl)
     nWTxt := GetTxtWidth( aUsl[nI], nFSize, cFont, .F. )  // получить Width текста
     nWUsl := MAX( nWUsl, nWTxt )
   NEXT
   nWUsl   += 20
   nUsl1   := nUsl2  := 0
   cZnak1  := cZnak2 := ""
   aBtnGrd := { HMG_RGB2n( GRAY ), CLR_WHITE }  // градиент кнопки
   aGrOver := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
   aGrFill := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }


   DEFINE WINDOW Form_Fltr                 ;
      AT 0, 0 WIDTH nW HEIGHT nH           ;
      TITLE cTitle ICON cIco               ;
      MODAL NOSIZE                         ;
      BACKCOLOR aBColor                    ;
      FONT cFont SIZE nFSize               ;
      ON INIT    {|| ThisOnInit(cValTyp) } ;
      ON RELEASE {|| Nil  }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nGRow
      nX := nGCol

      @ nY, nX LABEL Label_1 WIDTH nW-nGCol*2 HEIGHT nHLine VALUE cMenu2 ;
        FONTCOLOR BLACK TRANSPARENT CENTERALIGN VCENTERALIGN
      // Функция заменит на максимальный размер фонта
      SetFontSizeTextMax(ThisWindow.Name, "Label_1")
      nY += This.Label_1.Height + nGRow

      // ------------------ условие 1 --------------------
      @ nY, nX GETBOX GB_Usl1 VALUE "" WIDTH nWUsl HEIGHT nHLine ;
            FONTCOLOR BLACK BACKCOLOR WHITE READONLY

      @ nY, nX COMBOBOXEX Combo_Usl1 WIDTH nWUsl HEIGHT 320  ;
        ITEMS aUsl VALUE nUsl1 IMAGE {} BACKCOLOR SILVER     ;
        ON LISTCLOSE This.Combo_Usl1.Hide  INVISIBLE         ;
        ON CHANGE { || nUsl1 := This.Combo_Usl1.Value    ,;
                       cZnak1 := aZn[nUsl1]              ,;
                       This.GB_Usl1.Value := aUsl[nUsl1] ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWUsl BUTTONEX Btn_Usl1 WIDTH nHLine HEIGHT nHLine                  ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Usl1.Enabled := .F.  ,;
                   This.Combo_Usl1.Show          ,;
                   SetFocus(GetControlHandle("Combo_Usl1", "Form_Fltr")) ,;
                   This.Btn_Usl1.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ выбор значения 1 --------------------
      nX2    := nX + This.Combo_Usl1.Width + nHLine + nGCol
      nWUsl2 := nW - nX - This.Combo_Usl1.Width - nGCol - nGCol - nHLine

      @ nY, nX2 GETBOX GB_ValIs1 VALUE cValIs1 WIDTH nWUsl2-nHLine HEIGHT nHLine ;
        PICTURE REPL("X", 30) FONTCOLOR BLACK BACKCOLOR WHITE                    ;
        ON CHANGE {|| cValIs1 := This.GB_ValIs1.Value }

      @ nY, nX2 COMBOBOXEX Combo_Dbf1 WIDTH nWUsl2 HEIGHT 520 ;
        ITEMS aValCmb VALUE nValDb1 IMAGE {} BACKCOLOR SILVER ;
        ON LISTCLOSE This.Combo_Dbf1.Hide  INVISIBLE          ;
        ON CHANGE { || nValDb1 := This.Combo_Dbf1.Value ,;
                       cValIs1 := aValCmb[nValDb1]      ,;
                       This.GB_ValIs1.Value := cValIs1  ,;
                       This.Label_1.Setfocus }

      @ nY, nX2 + nWUsl2 - nHLine BUTTONEX Btn_Dbf1 WIDTH nHLine HEIGHT nHLine       ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Dbf1.Enabled := .F.  ,;
                   This.Combo_Dbf1.Show          ,;
                   SetFocus(GetControlHandle("Combo_Dbf1", "Form_Fltr")) ,;
                   This.Btn_Dbf1.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ---------------- выбор значения И / ИЛИ -----------------
      nY  += nHLine + nGRow
      nX3 := nX + nGCol * 2
      nWR := GetTxtWidth( 'ИЛИ', aBtnFnt[2], aBtnFnt[1], .T. ) + 20

      @ nY, nX3 RADIOGROUP Radio_1  OPTIONS { '&И', 'И&ЛИ' }   ;
        VALUE nAndOr WIDTH nWR SPACING 5   HORIZONTAL          ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD BACKCOLOR aBColor ;
        ON CHANGE ( nAndOr := This.Radio_1.Value )

      nY  += nHLine + nGRow

      // ------------------ условие 2 --------------------
      @ nY, nX GETBOX GB_Usl2 VALUE cValIs2 WIDTH nWUsl HEIGHT nHLine ;
        FONTCOLOR BLACK BACKCOLOR WHITE READONLY

      @ nY, nX COMBOBOXEX Combo_Usl2 WIDTH nWUsl HEIGHT 320  ;
        ITEMS aUsl VALUE nUsl2 IMAGE {} BACKCOLOR SILVER     ;
        ON LISTCLOSE This.Combo_Usl2.Hide  INVISIBLE         ;
        ON CHANGE { || nUsl2 := This.Combo_Usl2.Value    ,;
                       cZnak2 := aZn[nUsl2]              ,;
                       This.GB_Usl2.Value := aUsl[nUsl2] ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWUsl BUTTONEX Btn_Usl2 WIDTH nHLine HEIGHT nHLine                  ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Usl2.Enabled := .F.  ,;
                   This.Combo_Usl2.Show          ,;
                   SetFocus(GetControlHandle("Combo_Usl2", "Form_Fltr")) ,;
                   This.Btn_Usl2.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ выбор значения 2 --------------------
      //nX2    := nX + This.Combo_Usl2.Width + nHLine + nGCol
      //nWUsl2 := nW - nX - This.Combo_Usl1.Width - nGCol - nGCol - nHLine

      @ nY, nX2 GETBOX GB_ValIs2 VALUE cValIs2 WIDTH nWUsl2-nHLine HEIGHT nHLine ;
        PICTURE REPL("X", 30) FONTCOLOR BLACK BACKCOLOR WHITE                    ;
        ON CHANGE {|| cValIs2 := This.GB_ValIs2.Value }

      @ nY, nX2 COMBOBOXEX Combo_Dbf2 WIDTH nWUsl2 HEIGHT 520 ;
        ITEMS aValCmb VALUE nValDb2 IMAGE {} BACKCOLOR SILVER ;
        ON LISTCLOSE This.Combo_Dbf2.Hide  INVISIBLE          ;
        ON CHANGE { || nValDb2 := This.Combo_Dbf2.Value ,;
                       cValIs2 := aValCmb[nValDb2]      ,;
                       This.GB_ValIs2.Value := cValIs2  ,;
                       This.Label_1.Setfocus }

      @ nY, nX2 + nWUsl2 - nHLine BUTTONEX Btn_Dbf2 WIDTH nHLine HEIGHT nHLine       ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Dbf2.Enabled := .F.  ,;
                   This.Combo_Dbf2.Show          ,;
                   SetFocus(GetControlHandle("Combo_Dbf2", "Form_Fltr")) ,;
                   This.Btn_Dbf2.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      // ------------------ подсказка --------------------
      nY    += nHLine + nGRow*2
      cText := "Необходимо заполнить хотя бы одну строку для фильтра"
      @ nY, nX LABEL Label_2 WIDTH nW-nGCol HEIGHT nHLine VALUE cText ;
        FONTCOLOR BLUE TRANSPARENT VCENTERALIGN

      nY        := nH - nGRow - nHBtn
      nX        := nW - nGCol - nWBtn
      cCapt     := "Отмена"
      aBtnGrd   := { HMG_RGB2n( {189,30,73} ), CLR_WHITE }  // градиент кнопки
      aGrOverEx := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillEx := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Exit WIDTH nWBtn HEIGHT nHBtn                              ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverEx GRADIENTFILL aGrFillEx                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillEx ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverEx ) ;
        ACTION {|| This.Enabled := .F., Form_Fltr.Release() }

      nX        := nW - nGCol*2 - nWBtn*2
      cCapt     := "Ok"
      aBtnGrd   := { HMG_RGB2n( LGREEN ), CLR_WHITE }  // градиент кнопки
      aGrOverOk := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFillOk := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }

      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn                                ;
        CAPTION cCapt ICON Nil                                                         ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                                   ;
        FONT aBtnFnt[1] SIZE aBtnFnt[2] BOLD FONTCOLOR aFntClr[1]                      ;
        BACKCOLOR aGrOverOk GRADIENTFILL aGrFillOk                                     ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFillOk ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOverOk ) ;
        ACTION {|| This.Enabled := .F. ,;
                   aRet := CollectFilter(cName,cValTyp,cValIs1,cValIs2,cZnak1,cZnak2,nAndOr) ,;  //  поставили фильтр
                   IIF( LEN(aRet)==0, This.Label_1.Setfocus , Form_Fltr.Release() ) ,;
                   This.Btn_Ok.Enabled := .T.  }

   END WINDOW

   CENTER WINDOW   Form_Fltr
   ACTIVATE WINDOW Form_Fltr ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                         This.Restore , DoEvents() }

RETURN aRet

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisOnInit(cValType)

    IF cValType == "L"
       This.GB_Usl2.Hide
       This.Btn_Usl2.Hide
       This.Radio_1.Hide
       This.GB_ValIs2.Hide
       This.Btn_Dbf2.Hide
    ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
//  поставили/собрали фильтр с формы
STATIC FUNCTION CollectFilter(cName,cValType,cValIs1,cValIs2,cZnak1,cZnak2,nAndOr)
   LOCAL cAndOr, lErr, cFilter, cFunc, aRet, cErr

   cValIs1  := ALLTRIM(cValIs1)
   cValIs2  := ALLTRIM(cValIs2)
   cAndOr   := { ".AND.", ".OR." }[nAndOr]
   aRet     := {}
   lErr     := .F.
   cErr     := ""

   IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cZnak2)) == 0 .AND. ;
      LEN(ALLTRIM(cValIs1)) == 0 .AND. LEN(ALLTRIM(cValIs2)) == 0
      // просто выход
   ELSE
      IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
         lErr := .T.  // ошибка
         cErr := "Нет знака условия в первой строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) == 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
         lErr := .T.  // ошибка
         cErr := "Нет знака условия во второй строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0 .AND. ;
             LEN(ALLTRIM(cZnak1)) == 0
         lErr := .T.  // ошибка
         cErr := "Не заполнена первая строка фильтра !"
      ELSE
         cFilter  := ""
         IF cValType $ "CM"
            cFunc := ""
            cName := "ALLTRIM(" + cName + ")"
         ELSEIF cValType $ "=@T"
            cFunc := "CtoT("
         ELSEIF cValType $ "+^N"
            cFunc := "VAL("
         ELSEIF cValType == "D"
            cFunc := "CtoD("
         ELSEIF cValType == "L" .AND. UPPER(cValIs1) == "T"
            cFunc := "!EMPTY("
         ELSEIF cValType == "L" .AND. UPPER(cValIs1) == "F"
            cFunc := "EMPTY("
         ENDIF
         IF ALLTRIM(cZnak1) == "$"
            IF LEN(ALLTRIM(cZnak1)) > 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
               cFilter += "'" + ALLTRIM(cValIs1) + "' $ " + cName
            ENDIF
         ELSE
            IF LEN(ALLTRIM(cZnak1)) > 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
               cFilter += cName + cZnak1 + cFunc + "'" + cValIs1 + "'"
               cFilter += IIF(LEN(cFunc)>0,")","")
            ELSE
               IF cValType $ "CM"
                  cFilter += "LEN( " + cName + " )" + cZnak1 + "0"
               ELSEIF cValType $ "=@T"
                  cFilter += cName + cZnak1 + "CtoT('')"
               ELSEIF cValType $ "+^N"
                  cFilter += cName + cZnak1 + "VAL('0')"
               ELSEIF cValType == "D"
                  cFilter += cName + cZnak1 + "CtoD('')"
               ENDIF
            ENDIF
         ENDIF
         // ------- второе условие ----------
         IF LEN(ALLTRIM(cZnak2)) > 0
            IF ALLTRIM(cZnak2) == "$"
               IF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
                  cFilter += "'" + ALLTRIM(cValIs2) + "' $ " + cName
               ENDIF
            ELSE
               IF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
                  cFilter += cAndOr
                  cFilter += cName + cZnak2 + cFunc + "'" + cValIs2 + "'"
                  cFilter += IIF(LEN(cFunc)>0,")","")
               ELSE
                  cFilter += cAndOr
                  //cFilter += "LEN( " + cName + " )" + cZnak2 + "0"
                  IF cValType $ "CM"
                     cFilter += "LEN( " + cName + " )" + cZnak2 + "0"
                  ELSEIF cValType $ "=@T"
                     cFilter += cName + cZnak2 + "CtoT('')"
                  ELSEIF cValType $ "+^N"
                     cFilter += cName + cZnak2 + "VAL('0')"
                  ELSEIF cValType == "D"
                     cFilter += cName + cZnak2 + "CtoD('')"
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
         aRet := { cFilter , "резерв" }
      ENDIF
   ENDIF

  IF lErr  // ошибка
     MG_Stop("Ошибка в строке фильтра !;;" + cErr, Form_Fltr.Title )
  ENDIF

  ? PROCNL() , HB_ValToExp(aRet)

RETURN aRet

////////////////////////////////////////////////////////////////////////////
FUNCTION ListOneColumn(oBrw,cName)
   LOCAL a2Dim, cAls, cType, xVal, lFind, nOrd, nRec, nI, aDim1, aDim2

   a2Dim := {}
   aDim1 := {}
   aDim2 := {}
   cAls  := oBrw:cAlias

   SELECT(cAls)
   nRec := RecNo()
   nOrd := IndexOrd()
   OrdSetFocus(0)
   dbGotop()
   xVal := FIELDGET( FIELDNUM(cName) )
   AADD( a2Dim, { cValToCHAR(xVal), xVal } )
   cType := FIELDTYPE( FIELDNUM(cName) )
   DO WHILE !EOF()
      xVal  := FIELDGET( FIELDNUM(cName) )
      IF cType $ "CM"
      ELSEIF cType $ "=@T"
      ELSEIF cType $ "+^"
      ELSEIF cType == "D"
      ELSEIF cType == "N"
      ELSEIF cType == "L"
      ENDIF
      lFind := .F.
      FOR nI := 1 TO LEN(a2Dim)
         IF xVal == a2Dim[nI,2]
            lFind := .T.
            EXIT
         ENDIF
      NEXT
      IF !lFind
         AADD( a2Dim, { cValToCHAR(xVal), xVal } )
      ENDIF
      SKIP
      DO EVENTS
   ENDDO
   OrdSetFocus(nOrd)
   dbGoto(nRec)

   a2Dim := ASORT( a2Dim,,, { |x, y| x[2] < y[2] } )
   FOR nI := 1 TO LEN(a2Dim)
      AADD( aDim1 , ALLTRIM(a2Dim[nI,1]) )
      AADD( aDim2 , a2Dim[nI,2] )
   NEXT

RETURN { aDim1, aDim2, cType }
