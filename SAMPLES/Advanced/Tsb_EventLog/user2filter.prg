/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 23.09.20 - 10.09.25
*/

#include "minigui.ch"
//#include "metrocolor.ch"
/////////////////////////////////////////////////////////////////////////
// Меню фильтра для журнала-действий-пользователей
FUNCTION myUser2Filter()
   LOCAL aRet, lMenuStyle, nMenuBitmap

   lMenuStyle   := IsExtendedMenuStyleActive()     // стиль меню EXTENDED/STANDARD
   nMenuBitmap  := GetMenuBitmapHeight()           // высота bmp в меню

   aRet := Form_User2Filter()

   // высота bmp в меню
   SetMenuBitmapHeight(nMenuBitmap)    // вернуть как было
   IF lMenuStyle                       // стиль меню EXTENDED/STANDARD
      SET MENUSTYLE EXTENDED
   ELSE
      SET MENUSTYLE STANDARD
   ENDIF

RETURN aRet

/////////////////////////////////////////////////////////////////////////
FUNCTION Form_User2Filter()
   LOCAL cIco, cIcoBig, c2Title, cFont, nFontSize, aBackColor, aBackUpColor
   LOCAL hWnd, nMaxHeight, nMaxWidth, nRow, nCol, aLblColor, nWlbl
   LOCAL cIco3x1, cIco3x2, cIco2x1, cIco2x2, cButtCapt, nWBth
   LOCAL nHButt, nRowButt, nBFSize, nBFont, cText, nWButt, nWDate
   LOCAL cFileMemo, cFileIni2, cMetkaIni, nGRow, aRetPrn := {}
   LOCAL aGrOverB2, aGrFillB2, aGrOverBEx, aGrFillBEx
   LOCAL aGBBackColor := { WHITE, SILVER, YELLOW }
   LOCAL aGBFontColor := { BLACK, YELLOW, BLUE   }
   LOCAL c1Title, dDate1, dDate2, cUslEvn1, cUslEvn2, cFntTitle
   LOCAL a3Oper, cRcAbon, aColor, nSortTsb, cFClsTtl
   LOCAL nCol2GetObj, a3Event, a1Sort, aLang, aBtnLang, owc

   cIco         := "iLogfile48"
   cFont        := "Tahoma"
   nFontSize    := App.Cargo:nDefFontSize
   //nFontSize  := ModeSizeFont() + 2
   nBFont       := "Comic Sans MS"
   nBFSize      := nFontSize
   aBackColor   := {141,179,226}          // Цвет фона всей формы - как форма таблицы
   aBackUpColor := {  0,176,240}          // Цвет верха фона формы - голубой, как SKYPE
   aLblColor    := BLUE                   // Цвет Label_*
   nGRow        := 20                     // отступ сверху (начало граф)
   cFntTitle    := "Comic Sans MS"
   cFClsTtl     := NAVY
   cIcoBig      := 'iUsers48x1'
   cFileMemo    := App.Cargo:cPathTemp + "Seek_User2Log.txt"
   cFileIni2    := ChangeFileExt( cFileMemo, ".ini"  )
   cMetkaIni    := "22.09.20"  // чтобы при добавлении нового параметра была смена без ошибки
   a3Oper       := { 0 , "", "operator" }
   dDate1       := dDate2   := CTOD("")
   cUslEvn1     := cUslEvn2 := ""
   cRcAbon      := ""
   a3Event      := { {} , {}, {"no event"} }
   nSortTsb     := 1  // 1-'по возрастанию', 2-'по убыванию'

#ifdef KEY_ENG
   c1Title  := 'Log search'
   c2Title  := 'user action events'
   a1Sort   := { 'ascending', 'descending' }
   aLang    := { "1) Event date:", "2) Operator: = ", "3) Subscriber account: = ", "3) Event codes:", "Sorting: " }
   aBtnLang := { "Search", "Cancel" }
#else
   c1Title  := 'Поиск по журналу'
   c2Title  := 'события действий пользователей'
   a1Sort   := { 'по возрастанию', 'по убыванию' }
   aLang    := { "1) Дата события:", "2) Оператор: = ", "3) Л/счет абонента: = ", "3) Коды события:", "Сортировка: "}
   aBtnLang := { "Поиск", "Отмена" }
#endif

   // считать введённые ранее данные
   IniLoadFileForm_UserLog( cFileIni2, cMetkaIni, @a3Oper, @dDate1, @dDate2 ,;
                              @cUslEvn1, @cUslEvn2, @cRcAbon, @nSortTsb, @a3Event )

   DEFINE WINDOW Form_UserLog                        ;
      At 0, 0 WIDTH 820 HEIGHT 660                   ;
      TITLE "" ICON cIco                             ;
      MODAL NOSIZE                                   ;
      FONT cFont SIZE nFontSize BACKCOLOR aBackColor ;
      ON INIT    {|| _wPost( 0) }                    ;
      ON RELEASE {|| _wSend(90) }
      This.Cargo := oHmgData() ; owc := This.Cargo

      hWnd := GetFormHandle('Form_UserLog')
      nMaxWidth  := This.ClientWidth
      nMaxHeight := This.ClientHeight

      @ 0 , 110 LABEL Label_0 WIDTH nMaxWidth HEIGHT 110 ;
        VALUE '' BACKCOLOR aBackUpColor

      DRAW ICON IN WINDOW Form_UserLog AT 0, 0 PICTURE cIcoBig ;
         WIDTH 110 HEIGHT 110 COLOR aBackUpColor

      @ 0 , 110 LABEL Label_01 WIDTH nMaxWidth-110 HEIGHT 60   ;
        VALUE c1Title FONT cFntTitle SIZE nFontSize + 6 BOLD ;
        FONTCOLOR cFClsTtl BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      @ 55 , 110 LABEL Label_02 WIDTH nMaxWidth-110 HEIGHT 50   ;
        VALUE c2Title FONT cFntTitle SIZE nFontSize + 6 BOLD ;
        FONTCOLOR cFClsTtl BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      nRow := This.Label_0.Height + nGRow  // отступ сверху (начало граф)
      cText := aLang[1]                    // "1) Дата события:"
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .F. )
      @ nRow, 20 LABEL Label_Date1 VALUE cText WIDTH nWlbl HEIGHT nFontSize*2  ;
         FONTCOLOR BLACK VCENTERALIGN TRANSPARENT

      cText := " >= "
      nWBth := GetTxtWidth( cText, nFontSize, cFont, .T. )
      nCol2GetObj := 20 + nWlbl + 10
      @ nRow+2, 20 + nWlbl + 10 BUTTONEX Button_Date1 WIDTH nWBth HEIGHT nFontSize*2  ;
        CAPTION "?" FONTCOLOR BLACK BOLD  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP ;
        ACTION {|| Button2ZnakUslov(), cUslEvn1 := This.Button_Date1.Caption, BookFocus() }  // 1
        This.Button_Date1.Caption := cUslEvn1

      nCol   := Form_UserLog.Button_Date1.Col + Form_UserLog.Button_Date1.Width + 10
      nWDate := GetTxtWidth( "99.99.99", nFontSize, cFont, .T. )
      @ nRow+2, nCol GETBOX GB_Date1 VALUE dDate1 WIDTH nWDate HEIGHT nFontSize*2 ;
        PICTURE "@D" BACKCOLOR aGBBackColor FONTCOLOR aGBFontColor        ;
        ON CHANGE {|| dDate1 := This.GB_Date1.Value  }

      /* ---------- не обнуляется дата -------------
      @ nRow, nCol DATEPICKER Date_Nach ;
        VALUE dDate1 WIDTH 200 HEIGHT nFontSize*2 ;
        ON CHANGE { || dDate1 := This.Date_Nach.Value }
      */
      nCol  := This.GB_Date1.Col + This.GB_Date1.Width + 10
      @ nRow+2, nCol BUTTONEX Button_DelDate1          ;
        WIDTH 44 HEIGHT nFontSize*2  PICTURE "bDel24"  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
        ACTION {|| dDate1 := CTOD("")               ,;        // 2
                   This.GB_Date1.Value := dDate1    ,;
                   This.Button_Date1.Caption := ""  ,;
                   cUslEvn1 := ""                   ,;
                   BookFocus() }

      cText := " и "
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .T. ) + 10
      nCol  := This.Button_DelDate1.Col + This.Button_DelDate1.Width + 10
      @ nRow, nCol LABEL Label_Date2 VALUE cText WIDTH nWlbl HEIGHT nFontSize*2  ;
         FONTCOLOR BLACK VCENTERALIGN TRANSPARENT

      cText := " >= "
      nWBth := GetTxtWidth( cText, nFontSize, cFont, .T. )
      nCol  := This.Label_Date2.Col + This.Label_Date2.Width
      @ nRow+2, nCol BUTTONEX Button_Date2 WIDTH nWBth HEIGHT nFontSize*2  ;
        CAPTION "?" FONTCOLOR BLACK BOLD  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP ;
        ACTION {|| Button2ZnakUslov(), cUslEvn2 := This.Button_Date2.Caption, BookFocus() }  // 3
        This.Button_Date2.Caption := cUslEvn2

      nCol   := Form_UserLog.Button_Date2.Col + Form_UserLog.Button_Date2.Width + 10
      nWDate := GetTxtWidth( "99.99.99", nFontSize, cFont, .T. )
      @ nRow+2, nCol GETBOX GB_Date2 VALUE dDate2 WIDTH nWDate HEIGHT nFontSize*2 ;
        PICTURE "@D" BACKCOLOR aGBBackColor FONTCOLOR aGBFontColor        ;
        ON CHANGE {|| dDate2 := This.GB_Date2.Value  }

      nCol  := This.GB_Date2.Col + This.GB_Date2.Width + 10
      @ nRow+2, nCol BUTTONEX Button_DelDate2          ;
        WIDTH 44 HEIGHT nFontSize*2  PICTURE "bDel24"  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
        ACTION {|| dDate2 := CTOD("")               ,;          // 4
                   This.GB_Date2.Value := dDate2    ,;
                   This.Button_Date2.Caption := ""  ,;
                   cUslEvn2 := ""                   ,;
                   BookFocus() }

      // ------- 2) -------------
      nRow  := This.Label_Date1.Row + This.Label_Date1.Height + 20
      cText := aLang[2]                  // "2) Оператор: = "
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .T. )
      @ nRow, 20 LABEL Label_Who VALUE cText WIDTH nWlbl + 20 HEIGHT nFontSize*2 ;
        SIZE nFontSize FONTCOLOR BLACK TRANSPARENT VCENTERALIGN

      nCol  := nCol2GetObj //20 + nWlbl + 10
      cText := "ФИО оператора - кто делал ?"
      nWBth := GetTxtWidth( cText, nFontSize, cFont, .T. )
      @ nRow+2, nCol BUTTONEX Button_Who WIDTH nWBth HEIGHT nFontSize*2 ;
        CAPTION "???" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP         ;
        ACTION {|| a3Oper := UserList2Dim() /*DbfSelectOperat()*/ ,;          // 5
                   cText := IIF( LEN(a3Oper) > 0, a3Oper[2], "" ) ,;
                   This.Button_Who.Caption  := cText            ,;
                   BookFocus() }
        This.Button_Who.Caption := a3Oper[2]

      nCol := This.Button_Who.Col + This.Button_Who.Width + 10
      @ nRow+2, nCol BUTTONEX Button_DelWho            ;
        WIDTH 44 HEIGHT nFontSize*2  PICTURE "bDel24"  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
        ACTION {|| a3Oper[1] := 0 , a3Oper[2]   := "" ,;   // 6
                   This.Button_Who.Caption := ""    ,;
                   BookFocus() }

      cText := aLang[3]      // "3) Л/счет абонента: = "
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .F. )
      nRow  := This.Label_Who.Row + This.Label_Who.Height + 20
      @ nRow, 20 LABEL Label_LC VALUE cText WIDTH nWlbl HEIGHT nFontSize*2  ;
         FONTCOLOR BLACK VCENTERALIGN TRANSPARENT
      This.Label_LC.Hide

      nCol  := This.Label_LC.Col + This.Label_LC.Width + 10
      nWlbl := GetTxtWidth( "9999999999", nFontSize, cFont, .T. )
      @ nRow+2, nCol GETBOX GBox_LC VALUE cRcAbon WIDTH nWlbl HEIGHT nFontSize*2 ;
        PICTURE "99999999" BACKCOLOR aGBBackColor FONTCOLOR aGBFontColor        ;
        ON CHANGE {|| cRcAbon := This.GBox_LC.Value  }
      This.GBox_LC.Hide

      nCol := This.GBox_LC.Col + This.GBox_LC.Width + 10
      @ nRow+2, nCol BUTTONEX Button_DelLC             ;
        WIDTH 44 HEIGHT nFontSize*2  PICTURE "bDel24"  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
        ACTION {|| cRcAbon := ""             ,;             // 7
                   This.GBox_LC.Value := ""  ,;
                   BookFocus() }
      This.Button_DelLC.Hide

      nRow  := This.Label_LC.Row + This.Label_LC.Height + 20
      cText := aLang[4]   // "3) Коды события:"
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .F. )
      @ nRow, 20 LABEL Label_Evn VALUE cText WIDTH nWlbl HEIGHT nFontSize*2 ;
        SIZE nFontSize  FONTCOLOR BLACK TRANSPARENT

      nCol  := This.Label_Evn.Col + This.Label_Evn.Width + 10
      nWBth := nMaxWidth - nCol - 20 - 44 - 10
      @ nRow+2, nCol BUTTONEX Button_Evn WIDTH nWBth HEIGHT nFontSize*2 ;
        CAPTION "???" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP         ;
        ACTION {|| _wSend(8) }
        //ACTION {|| a3Event := EventList2Dim()                                  ,;       //8
        //           cText := IIF(LEN(a3Event[1])>0, HB_ValToExp(a3Event[1]),"") ,;
        //           This.Button_Evn.Caption := cText                            ,;
        //           BookFocus() }
        This.Button_Evn.Caption := IIF( LEN(a3Event[1]) > 0, HB_ValToExp(a3Event[1]), "" )

      nCol := This.Button_Evn.Col + This.Button_Evn.Width + 10
      @ nRow+2, nCol BUTTONEX Button_DelEvn            ;
        WIDTH 44 HEIGHT nFontSize*2  PICTURE "bDel24"  ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP      ;
        ACTION {|| a3Event := { {} , {}, {"no event"} } ,;       // 9
                   This.Button_Evn.Caption := ""        ,;
                   BookFocus() }

      // ------- Sort -------------
      nRow  := This.Label_Evn.Row + This.Label_Evn.Height + 40
      cText := aLang[5]      //"Сортировка: "
      nWlbl := GetTxtWidth( cText, nFontSize, cFont, .T. )
      @ nRow, 20 LABEL Label_Sort VALUE cText WIDTH nWlbl + 20 HEIGHT nFontSize*2 ;
        SIZE nFontSize BOLD FONTCOLOR BLACK TRANSPARENT VCENTERALIGN

      nCol  := nCol2GetObj //20 + nWlbl + 10
      cText := "00по возрастанию00"
      nWBth := GetTxtWidth( cText, nFontSize, cFont, .T. )
      @ nRow+2, nCol BUTTONEX Button_Sort WIDTH nWBth HEIGHT nFontSize*2 ;
        CAPTION "???" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP          ;
        ACTION {|cC,nS,nR| cC := This.Button_Sort.Caption, nS := nSortTsb  ,;   // 10
                           nR := SortList1Dim(a1Sort)                      ,;
                           cText := IIF( nR > 0, a1Sort[nR], cC )          ,;
                           nSortTsb := IIF( nR > 0, nR, nS )               ,;
                           This.Button_Sort.Caption  := cText              ,;
                           BookFocus() }
        This.Button_Sort.Caption := a1Sort[nSortTsb]

      /////////////////////// Button ////////////////////////////
      nWButt := 260  // ширина кнопок внизу
      nHButt := 86   // высота кнопок внизу
      nRowButt := nMaxHeight - nHButt - 20 // начало кнопок на форме

      nCol := ( nMaxWidth  - nWButt*2 )/2 - 30
      cButtCapt := aBtnLang[1]
      cIco2x1   := "iFindTsb64x1"  ;  cIco2x2 := "iFindTsb64x2"
      aColor    := {0,176,240}
      aGrOverB2 := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
      aGrFillB2 := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }

      @ nRowButt, nCol  BUTTONEX BUTTON_Save WIDTH nWButt HEIGHT nHButt                ;
         CAPTION cButtCapt ICON cIco2x1 FONTCOLOR BLACK                                ;
         FONT nBFont SIZE nBFSize BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP             ;
         BACKCOLOR aGrOverB2  GRADIENTFILL aGrFillB2                                   ;
         ON MOUSEHOVER ( This.Fontcolor := YELLOW, This.Icon := cIco2x2, This.GradientFill := aGrFillB2 ) ;
         ON MOUSELEAVE ( This.Fontcolor := BLACK , This.Icon := cIco2x1, This.GradientOver := aGrOverB2 ) ;
         ACTION {|| SetProperty(ThisWindow.Name, This.Name, "Enabled", .F.)          ,;
                    aRetPrn := Ret2Filter(a3Oper, dDate1, dDate2, cUslEvn1, cUslEvn2 ,;
                                                         cRcAbon, nSortTsb, a3Event) ,;
                    IniSaveFileForm_UserLog(cFileIni2,cMetkaIni,a3Oper,dDate1,dDate2 ,;
                                     cUslEvn1, cUslEvn2, cRcAbon, nSortTsb, a3Event) ,;
                    ThisWindow.Release }

      nCol := ( nMaxWidth  - nWButt*2 )/2 + nWButt + 30
      cButtCapt  := aBtnLang[2]
      cIco3x1    := "Return64x2"   ;  cIco3x2 := "Return64x1"
      aGrOverBEx := { { 0.5, CLR_WHITE, CLR_HRED  }, { 0.5, CLR_HRED , CLR_WHITE } }
      aGrFillBEx := { { 0.5, CLR_HRED , CLR_WHITE }, { 0.5, CLR_WHITE, CLR_HRED  } }

      @ nRowButt, nCol  BUTTONEX BUTTON_Exit WIDTH nWButt HEIGHT nHButt                ;
         CAPTION cButtCapt ICON cIco3x1 FONTCOLOR BLACK                                ;
         FONT nBFont SIZE nBFSize BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP             ;
         BACKCOLOR aGrOverBEx  GRADIENTFILL aGrFillBEx                                 ;
         ON MOUSEHOVER ( This.Fontcolor := YELLOW, This.Icon := cIco3x2, This.GradientFill := aGrFillBEx ) ;
         ON MOUSELEAVE ( This.Fontcolor := BLACK , This.Icon := cIco3x1, This.GradientOver := aGrOverBEx ) ;
         ACTION {|| SetProperty(ThisWindow.Name, This.Name, "Enabled", .F.)  ,;
                    INKEYGUI(200), aRetPrn := {}  , ThisWindow.Release }

      ON KEY ESCAPE OF Form_UserLog ACTION {|| aRetPrn := {} , ThisWindow.Release }

      WITH OBJECT This.Object
        :Event( 0, {|ow| ow:Setfocus("Label_0"), DoEvents() })

        :Event( 1, {|ow,ky,cn| _SetThisFormInfo(ow) , MsgDebug(ow:Name,ky,cn) , _SetThisFormInfo(), ;
                               This.&(cn).Enabled := .T. , ow:Setfocus("Buff"), DoEvents() })

        :Event( 8, {|ow,ky,cn| _SetThisFormInfo(ow) , a3Event := EventList2Dim() ,;
                               _SetThisFormInfo(), ;
                               ky := IIF(LEN(a3Event[1])>0, HB_ValToExp(a3Event[1]),"") ,;
                               This.Button_Evn.Caption := ky  , BookFocus() ,;
                               ky := cn, DoEvents() })

        :Event(90, {|ow,ky| // ON Release windows
                            Local cm
                            cm := ProcNL()
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                            DO EVENTS
                            Return Nil
                            })

        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   CENTER WINDOW Form_UserLog
   ACTIVATE WINDOW Form_UserLog

RETURN aRetPrn

/////////////////////////////////////////////////////////////////////////
STATIC FUNCTION BookFocus()
   Form_UserLog.Label_0.Setfocus
RETURN NIL

///////////////////////////////////////////////////////////////////////
// считать данные с ини-файла
Static Function IniLoadFileForm_UserLog(cFileIni,cMetkaIni, a3Oper, dDate1, dDate2  ,;
                                      cUslEvn1, cUslEvn2, cRcAbon, nSortTsb, a3Event )
   LOCAL cStr, aRet
   IF !FILE(cFileIni)
      IniSaveFileForm_UserLog(cFileIni,cMetkaIni, a3Oper, dDate1, dDate2 ,;
                                   cUslEvn1, cUslEvn2, cRcAbon, nSortTsb, a3Event )
   ENDIF

   cStr := ALLTRIM( hb_MemoRead(cFileIni) )
   IF LEN(cStr) == 0
     // нет данных
   ELSE
      // чтобы при добавлении нового параметра была смена без ошибки
      IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0 .AND. AT( cMetkaIni, cStr ) > 0
         aRet      := &cStr
         cMetkaIni := aRet[1]  // чтобы при добавлении нового параметра была смена без ошибки
         a3Oper     := aRet[2]
         dDate1    := aRet[3]
         dDate2    := aRet[4]
         cUslEvn1  := aRet[5]
         cUslEvn2  := aRet[6]
         cRcAbon   := aRet[7]
         nSortTsb  := aRet[8]
         a3Event   := aRet[9]
      ELSE
        // нет данных
      ENDIF
   ENDIF

Return Nil

///////////////////////////////////////////////////////////////////////////////////
Static Function IniSaveFileForm_UserLog(cFileIni,cMetkaIni,a3Oper,dDate1,dDate2 ,;
                                         cUslEvn1, cUslEvn2, cRcAbon, nSortTsb, a3Event )
   LOCAL aSave
   // значения первоначальные
   aSave := {cMetkaIni,a3Oper,dDate1,dDate2,cUslEvn1, cUslEvn2, cRcAbon, nSortTsb, a3Event}
   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )

Return Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Ret2Filter(a3Oper,dDate1,dDate2,cUslEvn1,cUslEvn2 ,;
                                            cRcAbon, nSortTsb, a3Event)
   LOCAL nI, aRet, cFilter, cStr, aDim, aDim2, cSort, cNSrt, cUser
   LOCAL a1Sort, cEvnt, cEvn2, cMsg1, cMsg2, aParams := hb_aParams()

#ifdef KEY_ENG
    cMsg1 := "The Event Date condition sign (first in the column) is UNDEFINED!;"
    cMsg1 += "Skipping this condition!"
    cMsg2 := "The Event Date condition sign (second in the column) is UNDEFINED!;"
    cMsg2 += "Skipping this condition!"
    cEvnt := "Event date: "
    cUser := "Operator: "
    cEvn2 := "Event: ("
    a1Sort := { 'ascending', 'descending' }
#else
    cMsg1 := "Знак условия Дата события (первая в графе) НЕОПРЕДЕЛЁН !;"
    cMsg1 += "Пропуск этого условия !"
    cMsg2 := "Знак условия Дата события (вторая в графе) НЕОПРЕДЕЛЁН !;"
    cMsg2 += "Пропуск этого условия !"
    cEvnt := " Дата события: "
    cUser := "Оператор: "
    cEvn2 := "Событие: ("
    a1Sort := { 'по возрастанию', 'по убыванию' }
#endif

   cFilter := ""
   cStr    := ""

   IF dDate1 > CTOD("")
      IF LEN(cUslEvn1) == 0
         AlertStop(cMsg1,,,64,{RED})
      ELSE
         cFilter += " DEVENT " + cUslEvn1 + "CTOD('" + DTOC(dDate1)+"')"
         cStr    +=  cEvnt + cUslEvn1 + " " + DTOC(dDate1)
      ENDIF
   ENDIF
   IF dDate2 > CTOD("")
      IF LEN(cFilter) > 0
         cFilter += " .AND. "
         cStr    += " и "
      ENDIF
      IF LEN(cUslEvn2) == 0
         AlertStop(cMsg2,,,64,{RED})
      ELSE
         cFilter += " DEVENT " + cUslEvn2 + "CTOD('" + DTOC(dDate2)+"')"
         cStr    += cEvnt + cUslEvn2 + " " + DTOC(dDate2)
      ENDIF
   ENDIF

   IF a3Oper[1] > 0
      IF LEN(cFilter) > 0
         cFilter += " .AND. "
         cStr    += " и "
      ENDIF
      cFilter += "NUSER == "  + HB_NtoS(a3Oper[1])
      cStr    += cUser + ALLTRIM(a3Oper[2])
   ENDIF

   IF LEN(ALLTRIM(cRcAbon)) > 0
      IF LEN(cFilter) > 0
         cFilter += " .AND. "
         cStr    += " и "
      ENDIF
      cFilter += "( RC=='"+ALLTRIM(cRcAbon)+"' .OR."
      cFilter += " RC0=='"+ALLTRIM(cRcAbon)+"' .OR."
      cFilter += " RC3=='"+ALLTRIM(cRcAbon)+"' .OR."
      cFilter += " RC4=='"+ALLTRIM(cRcAbon)+"' )"
      cStr    += " Л/счёт/все графы = " + ALLTRIM(cRcAbon)
   ENDIF

   IF LEN(a3Event[1]) > 0
      IF LEN(cFilter) > 0
         cFilter += " .AND. "
         cStr    += " и "
      ENDIF
      aDim  := a3Event[1]
      aDim2 := a3Event[2]
      cFilter += "( "
      cStr    += cEvn2
      FOR nI := 1 TO LEN(aDim)
          cFilter += "NEVENT=="+ HB_NtoS(aDim[nI])
          cFilter += IIF( nI == LEN(aDim), "", " .OR." )
          cStr    += ALLTRIM(aDim2[nI])
          cStr    += IIF( nI == LEN(aDim), "", ", " )
      NEXT
      cFilter += ") "
      cStr    += ") "
   ENDIF

   //IF LEN(cFilter) > 0
   //   cFilter += " .AND. "
   //   cStr    += " "
   //ENDIF
   //cFilter += "!DELETED()"
   //cStr    += ""

   IF SUBSTR(cStr, LEN(cStr), 1) == ","
      cStr := SUBSTR(cStr, 1, LEN(cStr) - 1)
   ENDIF

   //a1Sort   := { 'по возрастанию', 'по убыванию' }
   IF nSortTsb == 1
      cSort := "DTOS(DEVENT) + TEVENT + STR(IDEVENT)"
   ELSE
      cSort := "DESCEND( DTOS(DEVENT) + TEVENT + STR(IDEVENT) )"
   ENDIF
   cNSrt := a1Sort[nSortTsb]

   aRet := { cStr, cFilter, cSort, cNSrt }

RETURN aRet

///////////////////////////////////////////////////////////////////////////////////
/*Static Function DbfSelectOperat()
   LOCAL aDim, nChoice, aRet := { 0, "" }

   // Возвращает 2х мерный массив из базы {код поля, наименивание поля}
   aDim := Get_Sel_Dim2("Operat", "KOperat", "Operat", "Operat2", "" )
   nChoice := CreateContexMenu(aDim)
   IF nChoice > 0
      aRet := { aDim[nChoice,1] , aDim[nChoice,2]  }
   ENDIF

   RETURN aRet
   */
/////////////////////////////////////////////////////////////////////////////////
Function UserList4x(lCalc)
   LOCAL aUser := {}, nGrp, nUser, cUser, cAls, nFld
   DEFAULT lCalc := .F.   // .T. - выбирать только по полю lCalc

   cAls := ALIAS()
   SELECT OPERAT
   DbSetOrder(1)
   GOTO TOP
   IF lCalc
      nFld := FIELDNUM("LREPORT")
      IF nFld == 0
         MsgDebug("Error ! Нет поля LREPORT в БД:", ALIAS() )
         lCalc := .F.
      ENDIF
   ENDIF

   DO WHILE !EOF()
      IF !DELETED()
         IF  OPERAT->KOPERAT == 0
         ELSEIF OPERAT->KGROUP >= 90
            // пропуск
         ELSE
            IF lCalc
               IF OPERAT->LREPORT
                  nUser := OPERAT->KOPERAT
                  nGrp  := OPERAT->KGROUP
                  cUser := ALLTRIM(OPERAT->OPERAT)
                  AADD( aUser, { nUser, nGrp, cUser, cUser + "  (" + HB_NtoS(nUser) + ")" } )
               ENDIF
            ELSE
               nUser := OPERAT->KOPERAT
               nGrp  := OPERAT->KGROUP
               cUser := ALLTRIM(OPERAT->OPERAT)
               AADD( aUser, { nUser, nGrp, cUser, cUser + "  (" + HB_NtoS(nUser) + ")" } )
               //AADD( aList, STR(nUser,3) + " [ " + HB_NtoS(nGrp) + " ] " + cUser )
            ENDIF
         ENDIF
      ENDIF
      SKIP
   ENDDO
   aUser := ASORT( aUser,,, { | x, y | x[ 3 ] < y[ 3 ] } )

   IF LEN(cAls) > 0
      DBSELECTAREA(cAls)
   ENDIF

RETURN aUser

/////////////////////////////////////////////////////////////////////////////////
Function UserList2Dim(lCalc)
   LOCAL aUser, nGrp, nUser, cUser
   LOCAL nPos, cForm := ThisWindow.Name, aDim, lExit := .F.
   LOCAL nBmpSize, nFSize, nChoice, nI, aRet
   DEFAULT lCalc := .F.   // .T. - выбирать только по полю lCalc

   aUser := UserList4x(lCalc)

   aDim := {}
   FOR nI := 1 TO LEN(aUser)
      IF aUser[nI,1] >= 100
         //AADD( aDim, {"bUserAdmin64", " " + aList[nI], "MsgDebug", "Stroka" , nI } )
         AADD( aDim, {"bUserAdm32", aUser[nI,4], .F., "MsgDebug", "Stroka" , nI } )
      ELSEIF aUser[nI,1] < 10
           //AADD( aDim, {"bUserAdmin64", " " + aList[nI], "MsgDebug", "Stroka" , nI } )
           AADD( aDim, {"bUserM32", aUser[nI,4], .F., "MsgDebug", "Stroka" , nI } )
      ELSE
           //AADD( aDim, {"bUser64"     , " " + aList[nI], "MsgDebug", "Stroka" , nI } )
           AADD( aDim, {"bUserW32", aUser[nI,4], .F., "MsgDebug", "Stroka" , nI } )
      ENDIF
   NEXT

   nPos := 1
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col
   nBmpSize := 32
   nFSize   := App.Cargo:nDefFontSize + 2
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit )

   aRet := {}
   IF nChoice > 0
      nUser := aUser[nChoice,1]
      nGrp  := aUser[nChoice,2]
      cUser := aUser[nChoice,3]
      //M->nOperat   := nUser
      //M->cOperator := cUser
      //M->aPubUserAccessPswrd := AccessLoadPasswrd( M->nOperat, @nGrp)
      //IF LEN(M->aPubUserAccessPswrd) # 500
      //   MsgDebug("ОШИБКА ! Массив aPubUserAccessPswrd=",LEN(M->aPubUserAccessPswrd),M->aPubUserAccessPswrd)
      //ENDIF
      //M->nPubAccessGroup := nGrp  // ГРУППА
      //MG_Debug(nUser,cUser,nGrp)
      aRet := { nUser , cUser }
   ENDIF

RETURN aRet

/////////////////////////////////////////////////////////////////////////////////
Function SortList1Dim(a1Sort)
   LOCAL nPos, cForm := ThisWindow.Name, aDim, lExit := .F.
   LOCAL nBmpSize, nFSize, nChoice, nRet

   aDim := {}
   AADD( aDim, {"bSortN19", a1Sort[1], .F. , "MsgDebug", "Stroka" , 1 } )
   AADD( aDim, {                                                         } )
   AADD( aDim, {"bSortN91", a1Sort[2], .F. , "MsgDebug", "Stroka" , 2 } )

   nPos := 1
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col
   nBmpSize := 48
   nFSize   := App.Cargo:nDefFontSize + 10
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit )

   nRet := 0
   IF nChoice > 0
      nRet := aDim[nChoice,6]
   ENDIF

RETURN nRet

/////////////////////////////////////////////////////////////////////////////////
Function EventList2Dim()
   LOCAL a2Dim, cPath, cFile, cTitle, aRet

   // события в журнал-действий-пользователей-программы
   a2Dim  := EVENTS_Dim()  // -> users2log.prg
   cTitle := "Справочник событий в программе"
   cPath  := M->SetTemp
   cFile  := "EventS1.dbf"
   aRet   := my3SpravDbf(cPath, cFile, a2Dim, cTitle)  // -> users2fltrEvent.prg

RETURN aRet

