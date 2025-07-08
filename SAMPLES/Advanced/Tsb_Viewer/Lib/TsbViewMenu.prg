/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Меню для TsbViewer - Опции/свойства по базе.
 * Menu for TsbViewer - Options/properties by base.
*/

#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbGetAllUse(cTitle)
   LOCAL nI, cMsg := "", aAlias := {}, aSelect := {}, aRdd := {}

   hb_waEval( {|| AADD(aAlias, Alias())   } )
   hb_waEval( {|| AADD(aSelect, Select()) } )
   hb_waEval( {|| AADD(aRdd, RddName())   } )

   FOR nI := 1 TO LEN(aAlias)
       cMsg += "Select: " + HB_NtoS(aSelect[nI])
       cMsg += ",  Alias: " + aAlias[nI] + " ,  RddName: " + aRdd[nI] + CRLF
   NEXT
   cMsg += REPL("; ",30)

   AlertInfo( cMsg, cTitle, , , {RED} )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexesThis(cTitle,cParRet)
   LOCAL nI, nSel, nOrder, cAlias, cIndx, aIndx, xRet, cMsg, cVal, aOrdN
   LOCAL xSc2, xSc1, cZn
   DEFAULT cTitle := "Info index", cParRet := "SAY"

   cAlias := ALIAS()
   nSel   := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop( cMsg, cTitle )
      RETURN NIL
   ENDIF

   nOrder := INDEXORD()
   cMsg   := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + ";"
   cMsg   += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cMsg   += "Open indexes: "
   aIndx  := {}
   aOrdN  := {}

   IF nOrder == 0
      cMsg += " (no) !;;"
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' );;'
      FOR nI := 1 TO 200
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += STR(nI,3) + ')  Index file: ' + DBORDERINFO(DBOI_FULLPATH) + ";"
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ");"
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '" ;'
            cMsg += '       FOR index: "' + OrdFor() + '" ' + SPACE(3)
            //cMsg += 'DBOI_ISCOND: "' + cValToChar(DBORDERINFO(DBOI_ISCOND)) + '" ' + SPACE(5)
            cMsg += 'DESCENDing: ' + cValToChar(DBORDERINFO(DBOI_ISDESC)) + SPACE(3)
            cMsg += 'UNIQUE: ' + cValToChar(DBORDERINFO(DBOI_UNIQUE)) + SPACE(3)+ ';'
            xSc2 := DbOrderInfo( DBOI_SCOPEBOTTOM )
            xSc1 := DbOrderInfo( DBOI_SCOPETOP )
            cZn  := '"'
            IF VALTYPE(xSc1) # "C"
               xSc1 := cValToChar(xSc1)
               xSc2 := cValToChar(xSc2)
               cZn  := ''
            ENDIF
            IF LEN(xSc1) > 0 .OR. LEN(xSc2) > 0
               cMsg += '    SET SCOPE TO: ' + cZn +  xSc1 + cZn + ' , ' + cZn + xSc2 + cZn + ';'
            ENDIF
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT)) + ' );;'
            //cVal := STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI)
            cVal := "(" + HB_NtoS(nI) + "), OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]"
            cVal += " , DESCENDing: " + cValToChar(DBORDERINFO(DBOI_ISDESC))
            cVal += " , UNIQUE: " + cValToChar(DBORDERINFO(DBOI_UNIQUE))
            IF LEN(xSc1) > 0 .OR. LEN(xSc2) > 0
               cVal += '    SET SCOPE TO: ' + cZn +  xSc1 + cZn + ' , ' + cZn + xSc2 + cZn + ';'
            ENDIF
            AADD( aIndx, cVal )
            AADD( aOrdN, OrdName(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) // переключить на основной индекс
      cMsg += REPL("-",60) + ";"
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS() + ";"
   ENDIF
   cMsg += "Number of records by index DBOI_KEYCOUNT(?) = " + HB_NtoS(ORDKEYCOUNT()) + ";"
   cMsg += REPL("; ",30)

   IF cParRet == "ADIM"
      xRet := { aIndx, aOrdN }
   ELSEIF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSEIF cParRet == "LOG"
      xRet := HB_NtoS(nOrder)+" , OrdName: " + OrdName(nI) + ",  [" + OrdFor()+"]  "
      xRet += ", DESCENDing: " + cValToChar(DBORDERINFO(DBOI_ISDESC))
      xRet += ", UNIQUE: " + cValToChar(DBORDERINFO(DBOI_UNIQUE))
      xRet += ", DBOI_KEYCOUNT() = " + HB_NtoS(ORDKEYCOUNT())
   ELSE
      xRet := cMsg
   ENDIF

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexChange(cTitle, oBrw)
   LOCAL aRet, nIndx, cOrd

   aRet := Tbrowse_MenuIndex(cTitle, oBrw)
   IF LEN(aRet) > 0
      nIndx := aRet[1]
      cOrd  := aRet[2]
      DbSelectArea(oBrw:cAlias)
      DbSetOrder(nIndx)
      oBrw:uLastTag := (oBrw:cAlias)->( OrdName(nIndx) )  // без этого индекс слетает
      oBrw:Reset()
      oBrw:Refresh(.T.)
      oBrw:GoTop()
      DO EVENTS
   ENDIF
   oBrw:Setfocus()

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbRelation(cTitle,cParRet)
   LOCAL aDim := {}, nR, aVal, cMsg, cVal
   DEFAULT cParRet := "SAY"

   cMsg := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + ";"
   cMsg += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cVal := ""

   FOR nR := 1 TO 130
      aVal := Relation( nR )
      IF LEN(aVal[1]) > 0
         AADD(aDim, aVal)
         cVal += hb_ValToExp(aVal) + ";"
      ENDIF
      IF LEN(aVal[1]) == 0
         EXIT
      ENDIF
   NEXT
   IF LEN(aDim) == 0
      AADD(aDim, {} )
      cMsg += "No Set relation !;" + CRLF
      cVal := "No Set relation !"
   ENDIF
   cMsg += cVal + REPL("; ",30)

   IF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSE
      cMsg := '"' + ATREPL( ";", cVal , CRLF ) + '"'
   ENDIF

RETURN cMsg

////////////////////////////////////////////////////////////////
STATIC FUNCTION Relation( nRelation )
RETURN { DBRELATION(nRelation), ALIAS(DBRSELECT(nRelation)) }

////////////////////////////////////////////////////////////////
FUNCTION myDbFilter(cTitle,cParRet)
   LOCAL cMsg, cAls := Alias()
   DEFAULT cParRet := "SAY"

   cMsg := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg += "Path to the database - " + DBINFO( DBI_FULLPATH ) + ";;"
   cMsg += 'DbFilter(): "' + (cAls)->( DbFilter() ) + '";;'
   cMsg += REPL("; ",30)

   IF cParRet == "SAY"
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSE
      cMsg := '"' + (cAls)->( DbFilter() ) + '"'
   ENDIF

RETURN cMsg

////////////////////////////////////////////////////////////////
FUNCTION myDbStructure(cTitle)        // Структура этой базы
   LOCAL cMsg, cFile, cFTxt, cAls, aStru, nI, aVal, cTxt

   cAls  := Alias()
   aStru := DbStruct()
   cFile := DBINFO( DBI_FULLPATH )
   cFTxt := ChangeFileExt( cFile, '.txt' )
   cMsg  := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg  += "Path to the database - " + cFile + ";"
   cMsg  += "File with base structure - " + cFTxt + ";;"
   cMsg  += "     FILE: " + cFileNoPath( cFile ) + ";;"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += "   " + HB_NtoS(nI) + ". "
      cMsg += IIF( nI < 10, " ", "" )
      cMsg += PADR(aVal[1],13)
      cMsg += aVal[2] + PADL( HB_NtoS(aVal[3]), 5 )
      cMsg += " " + PADL( HB_NtoS(aVal[4]), 3 ) + ";"
   NEXT

   cMsg  += ";;{"
   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cMsg += IIF( nI == 1 , "", " " )
      cMsg += HB_ValToExp(aVal)
      cMsg += IIF( nI == LEN(aStru) , " ", "," ) + " | ;"
   NEXT
   cMsg += "}; "

   cMsg += CRLF
   cMsg += REPL("--",40) + ";"
   cMsg += SPACE(3) + "aDbf := {};"

   FOR nI := 1 TO LEN(aStru)
      aVal := aStru[nI]
      cTxt := aVal[1] + '"'
      cMsg += SPACE(3) + 'AADD( aDbf, { "' + PADR(cTxt, 15)
      cMsg += ', "' + aVal[2] + '",' + STR(aVal[3],4)
      cMsg += ',' + STR(aVal[4],2) + ', "Not-show" , .T. } );'
   NEXT
   cMsg += CRLF

   AlertInfo( cMsg, cTitle, , , {RED} )

   cMsg := AtRepl( ";", cMsg, CRLF )
   cMsg := AtRepl( "|", cMsg, ";"  )
   cMsg += CRLF + MiniGuiVersion() + CRLF + ">>"
   HB_MemoWrit(cFTxt, cMsg)

   cMsg := "File created successfully !;"
   cMsg += cFTxt + ";;"
   cMsg += "Open this file ?;"
   IF AlertYesNo(cMsg, , , , , {LGREEN,RED} )
      ShellExecute( 0, "Open", cFTxt,,, 3 )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cTitle)       // Выгрузить CSV
   LOCAL cMsg, cFile, cFCsv, cAls

   cAls  := Alias()
   cFile := DBINFO( DBI_FULLPATH )
   cFCsv := ChangeFileExt( cFile, '.csv' )
   cMsg  := "Open Database - alias: " + cAls + "    RddName: " + RddName() + ";"
   cMsg  += "Path to the database - " + cFile + ";"
   cMsg  += "Upload file created - " + cFCsv + ";;"
   cMsg  += "Total records in the database = " + HB_NtoS( LASTREC() ) + ";"

   WaitWindow( 'Converting DBF to CSV', .T. )
   GOTO TOP
   COPY TO (cFCsv) DELIMITED // запись в файл
   WaitWindow()

   AlertInfo( cMsg, cTitle, , , {RED} )

   cMsg := "File created successfully !;"
   cMsg += cFCsv + ";;"
   cMsg += "Open this file ?;"
   IF AlertYesNo(cMsg, , , , , {LGREEN,RED} )
      ShellExecute( 0, "Open", cFCsv,,, 3 )
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION myGetLang(cTitle,cParRet)      // Какой язык в окне ?
   LOCAL cMsg, nI
   DEFAULT cParRet := "SAY"

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   cMsg += "hb_langName()   = " + hb_langName() + ";"
   cMsg += "hb_langMessage()= " + hb_langMessage() + ";"
   // #define EG_ARG          1
   cMsg += "hb_langErrMsg(1)= " + hb_langErrMsg(1) + ";;"

   FOR nI := 1 TO 12
      cMsg += HB_NtoS(nI) +  ") " + LOWER( NTOCMONTH( nI ) ) + ";"
   NEXT
   cMsg +=  ";"
   FOR nI := 1 TO 7
      cMsg += HB_NtoS(nI) +  ") " + LOWER( NTOCDOW( nI ) ) + ";"
   NEXT

   IF cParRet == "SAY"
      cMsg += REPL("; ",20)
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSEIF cParRet == "DEBUG"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgInfo( cMsg, cTitle )
   ELSE
      cMsg := AtRepl( ";", cMsg, CRLF )
   ENDIF

RETURN cMsg

///////////////////////////////////////////////////////////////////////////////
FUNCTION FontsListAll(cTitle)
   LOCAL cFnt, hFnt, aFnt, cMsg := "", aFonts := {}, n

   FOR n := 1 TO Len( _HMG_aControlNames )
      IF _HMG_aControlType[ n ] == "FONT"
         AAdd( aFonts, { _HMG_aControlNames[ n ], _HMG_aControlHandles[ n ] } )
      ENDIF
   NEXT

   FOR EACH aFnt IN aFonts
       cFnt := aFnt[1]
       hFnt := aFnt[2]
       cMsg += strzero(hb_enumindex(aFnt), 2) + ". " + ;       /*+ cFnt + " : "*/
                 hb_valtoexp( GetFontParam( hFnt ) ) + CRLF
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo( cMsg, cTitle, , , {RED})

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION FontsTbrowse( cTitle, oBrw )
   LOCAL cMsg

   cMsg := "Table alias: " + oBrw:cAlias + ";;"
   cMsg += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";"
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout_TsbViewer(cTitle)
   LOCAL cMsg, cIco, aBClr, a2Clr, aFClr
   DEFAULT cTitle := "About the program"

   cMsg  := MsgAboutThis() + ";;"
   cMsg  += "(c) 2021-2023 Verchenko Andrey <verchenkoag@gmail.com>;"
   cMsg  += "(c) 2021-2023 Sergej Kiselev <bilance@bilance.lv>;;"
   cMsg  += hb_compiler() + ";" + Version() + ";" + MiniGuiVersion() + ";"
   cMsg  += "(c) Grigory Filatov http://www.hmgextended.com;;"
   cMsg  += PadC( "This program is Freeware!", 60 ) + ";"
   cMsg  += PadC( "Copying is allowed!", 60 ) + ";"
   cIco  := Icon64TempCreate()      // -> TsbViewMisc.prg
   aBClr := { 242, 163, 167 }
   aFClr := { 82,  0, 141 }
   a2Clr := _SetMsgAlertColors(aBClr,aFClr)

   SET MSGALERT BACKCOLOR TO aBClr                // новый цвет для HMG_Alert()
   SET MSGALERT FONTCOLOR TO aFClr                // новый цвет для HMG_Alert()
   //cMsg += HB_ValToExp(a2Clr)                   // проверка

   AlertInfo( cMsg, cTitle  , cIco, 64, {RED} )

   SET MSGALERT BACKCOLOR TO a2Clr[1]             // восстановить цвет HMG_Alert()
   SET MSGALERT FONTCOLOR TO BLACK                // восстановить цвет HMG_Alert()

RETURN NIL

//////////////////////////////////////////////////////////////////////////
// Меню для фильтра / Filter menu
FUNCTION Tbrowse_MenuFltr(oBrw,cMenu2,cName)
   LOCAL cIco, cTitle, cFont, nFSize, nW, nH, aBColor, nHBtn, nWBtn
   LOCAL aBtnFnt, aFntClr, aRet, nGRow, nGCol, nHLine, nY, nX, nX2, nX3
   LOCAL cCapt, aBtnGrd, aGrOverEx, aGrFillEx, aGrOverOk, aGrFillOk
   LOCAL a3Dim, aValCmb, aValDbf, nWUsl, nWTxt, aUsl, aZn, nWR, aAndOr
   LOCAL nUsl1, nUsl2, cZnak1, cZnak2, nValDb1, nValDb2, nAndOr, nI
   LOCAL nWUsl2, cHelp, aGrOver, aGrFill, cValIs1, cValIs2, cValTyp
   LOCAL hFont, aFont, aSayTxt, hOld, oTsb

   oTsb    := oBrw:Cargo:oParam             // получить данные из объекта
   hFont   := GetFontHandle(oTsb:aFont[2])
   aFont   := GetFontParam(hFont)
   aSayTxt := myLangeRes(5)
   cTitle  := aSayTxt[1]   // Пользовательский фильтр
   cHelp   := aSayTxt[2]   // Необходимо заполнить хотя бы одну строку для фильтра
   aBColor := SILVER       // цвет фона таблицы
   nW      := 690
   nH      := 430
   cIco    := "iSearch48x1"
   cFont   := aFont[1]
   nFSize  := aFont[2] + 2
   aBtnFnt := { "Comic Sans MS", nFSize + 2 }
   aFntClr := { BLACK , YELLOW }
   nWBtn   := 170                        // ширина кнопки
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
   // перевод на другие языки
   aSayTxt := myLangeRes(6)
   aUsl[2] := aSayTxt[1]
   aUsl[3] := aSayTxt[2]
   aUsl[4] := aSayTxt[3]
   aUsl[5] := aSayTxt[4]
   aUsl[6] := aSayTxt[5]
   aUsl[7] := aSayTxt[6]

   IF cValTyp $ "CM"
      AADD(aUsl," содержит ($)     ")  ;  AADD(aZn," $ ")
      aUsl[8] := aSayTxt[7]
   ELSEIF cValTyp == "L"
      aUsl := {}
      aZn  := {}
      AADD(aUsl,"            ")  ;  AADD(aZn,"    ")
      AADD(aUsl," равно (==) ")  ;  AADD(aZn," == ")
      aUsl[2] := aSayTxt[1]
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

   //  для окна типа Modal
   hOld := _HMG_InplaceParentHandle
   _HMG_InplaceParentHandle := ThisWindow.Handle

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
        FONTCOLOR BLACK SIZE nFSize + 4 TRANSPARENT CENTERALIGN VCENTERALIGN
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
      nY     += nHLine + nGRow
      nX3    := nX + nGCol * 2
      aAndOr := { 'И', 'ИЛИ' }
      aAndOr := myLangeRes(7)   // перевод на другие языки
      nWR    := GetTxtWidth( aAndOr[2], aBtnFnt[2], aBtnFnt[1], .T. ) + 50

      @ nY, nX3 RADIOGROUP Radio_1  OPTIONS aAndOr             ;
        VALUE nAndOr WIDTH nWR SPACING 5 HORIZONTAL            ;
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
      @ nY, nX LABEL Label_2 WIDTH nW-nGCol HEIGHT nHLine VALUE cHelp ;
        FONTCOLOR BLUE TRANSPARENT VCENTERALIGN

      nY        := nH - nGRow - nHBtn
      nX        := nW - nGCol - nWBtn
      cCapt     := myLangeRes(8)   // "Отмена"
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

   //  для окна типа Modal
   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

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
   LOCAL cAndOr, lErr, cFilter, cFunc, aRet, cErr, aLang

   cValIs1  := ALLTRIM(cValIs1)
   cValIs2  := ALLTRIM(cValIs2)
   cAndOr   := { ".AND.", ".OR." }[nAndOr]
   aRet     := {}
   lErr     := .F.
   cErr     := ""
   aLang    := myLangeRes(9)   // перевод на другие языки

   IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cZnak2)) == 0 .AND. ;
      LEN(ALLTRIM(cValIs1)) == 0 .AND. LEN(ALLTRIM(cValIs2)) == 0
      // просто выход
   ELSE
      IF LEN(ALLTRIM(cZnak1)) == 0 .AND. LEN(ALLTRIM(cValIs1)) > 0
         lErr := .T.  // ошибка
         cErr := aLang[1] //"Нет знака условия в первой строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) == 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
         lErr := .T.  // ошибка
         cErr := aLang[2] //"Нет знака условия во второй строке фильтра !"
      ELSEIF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0 .AND. ;
             LEN(ALLTRIM(cZnak1)) == 0
         lErr := .T.  // ошибка
         cErr := aLang[3] //"Не заполнена первая строка фильтра !"
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
               cFilter += cName + cZnak1 + cFunc + "[" + cValIs1 + "]"
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
                  cFilter += cAndOr
                  cFilter += "'" + ALLTRIM(cValIs2) + "' $ " + cName
               ENDIF
            ELSE
               IF LEN(ALLTRIM(cZnak2)) > 0 .AND. LEN(ALLTRIM(cValIs2)) > 0
                  cFilter += cAndOr
                  cFilter += cName + cZnak2 + cFunc + "[" + cValIs2 + "]"
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

  IF lErr
     //AlertStop("Ошибка в строке фильтра !;;" + cErr, Form_Fltr.Title )
     AlertStop(aLang[4] + ";;" + cErr, Form_Fltr.Title )
  ENDIF

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

//////////////////////////////////////////////////////////////////////////
// Меню для смены индекса / Menu for changing the index
FUNCTION Tbrowse_MenuIndex(cMenu2, oBrw)
   LOCAL cIco, cTitle, cFont, nFSize, nW, nH, aBColor, nHBtn, nWBtn
   LOCAL aBtnFnt, aFntClr, aRet, nGRow, nGCol, nHLine, nY, nX, cVal
   LOCAL cCapt, aBtnGrd, aGrOverEx, aGrFillEx, aGrOverOk, aGrFillOk
   LOCAL nIndex, cHelp, aGrOver, aGrFill, nWCbox, aIndx, aClrStop
   LOCAL hFont, aFont, aSayTxt, hOld, oTsb, aOrdName
   DEFAULT cMenu2 := "Переключить индекс базы"

   oTsb     := oBrw:Cargo:oParam             // получить данные из объекта
   hFont    := GetFontHandle(oTsb:aFont[2])
   aFont    := GetFontParam(hFont)
   aSayTxt  := myLangeRes(10)
   cTitle   := aSayTxt[1]   // Пользовательское меню
   cHelp    := aSayTxt[2]   // Нет открытых индексов по этой базе
   aBColor  := SILVER       // цвет фона таблицы
   nW       := 760
   nH       := 350
   cIco     := "iSearch48x1"
   cFont    := aFont[1]
   nFSize   := aFont[2] + 2
   aBtnFnt  := { "Comic Sans MS", nFSize + 2 }
   aFntClr  := { BLACK , YELLOW }
   nWBtn    := 170                          // ширина кнопки
   nHBtn    := 55                           // высота кнопки
   nGRow    := 20                           // отступ сверху/снизу
   nGCol    := 30                           // отступ слева/справа
   nHLine   := nFSize * 2                   // высота строки на форме
   aRet     := {}                           // вернуть фильтр для таблицы
   aRet     := myDbIndexesThis("","ADIM")   // массив индексов
   aIndx    := aRet[1]
   aOrdName := aRet[2]
   cVal     := ""
   aBtnGrd  := { HMG_RGB2n( GRAY ), CLR_WHITE }  // градиент кнопки
   aGrOver  := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
   aGrFill  := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }
   aClrStop := HMG_n2RGB( CLR_HRED )
   nIndex   := 0

   //  для окна типа Modal
   hOld := _HMG_InplaceParentHandle
   _HMG_InplaceParentHandle := ThisWindow.Handle

   DEFINE WINDOW Form_Index       ;
      AT 0, 0 WIDTH nW HEIGHT nH  ;
      TITLE cTitle ICON cIco      ;
      MODAL NOSIZE                ;
      BACKCOLOR aBColor           ;
      FONT cFont SIZE nFSize

      nW     := This.ClientWidth
      nH     := This.ClientHeight
      nY     := nGRow
      nX     := nGCol
      nWCbox := nW - nGCol * 2 - nHLine

      @ nY, nX LABEL Label_1 WIDTH nW-nGCol*2 HEIGHT nHLine VALUE cMenu2 ;
        FONTCOLOR BLACK SIZE nFSize + 4 TRANSPARENT CENTERALIGN VCENTERALIGN
      nY += This.Label_1.Height + nGRow

      @ nY, nX GETBOX GB_Indx VALUE cVal WIDTH nWCbox HEIGHT nHLine    ;
        PICTURE REPL("X", 90) FONTCOLOR BLACK BACKCOLOR WHITE READONLY ;
        ON CHANGE {|| cVal := This.GB_Indx.Value }

      @ nY, nX COMBOBOXEX Combo_Indx WIDTH nWCbox HEIGHT 360  ;
        ITEMS aIndx VALUE nIndex IMAGE {} BACKCOLOR SILVER    ;
        ON LISTCLOSE This.Combo_Indx.Hide  INVISIBLE          ;
        ON CHANGE { || nIndex := This.Combo_Indx.Value ,;
                       cVal   := aIndx[nIndex]         ,;
                       This.GB_Indx.Value := cVal      ,;
                       This.Label_1.Setfocus }

      @ nY, nX + nWCbox BUTTONEX Btn_Indx WIDTH nHLine HEIGHT nHLine                 ;
        CAPTION CHR(218) ICON Nil  FLAT NOXPSTYLE HANDCURSOR NOTABSTOP               ;
        FONT "Wingdings" SIZE aBtnFnt[2] FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                       ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.GradientFill := aGrFill ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.GradientOver := aGrOver ) ;
        ACTION {|| This.Btn_Indx.Enabled := .F.  ,;
                   This.Combo_Indx.Show          ,;
                   SetFocus(GetControlHandle("Combo_Indx", "Form_Index")) ,;
                   This.Btn_Indx.Enabled := .T.  ,;
                   _PushKey ( VK_F4 ) }

      //  подсказка
      nY  += nHLine + nGRow*2
      @ nY, nX LABEL Label_2 WIDTH 46 HEIGHT 46 VALUE CHR(74) FONT "Wingdings" ;
        SIZE 40 FONTCOLOR aClrStop TRANSPARENT VCENTERALIGN
      nX += This.Label_2.Width + 10

      @ nY, nX LABEL Label_3 WIDTH nW-nGCol*2 HEIGHT 40 VALUE cHelp ;
        FONTCOLOR aClrStop TRANSPARENT VCENTERALIGN

      IF LEN(aIndx) == 0
         This.GB_Indx.Enabled    := .F.
         This.Combo_Indx.Enabled := .F.
         This.Btn_Indx.Enabled   := .F.
      ELSE
         This.Label_2.Hide
         This.Label_3.Hide
      ENDIF

      nY        := nH - nGRow - nHBtn
      nX        := nW - nGCol - nWBtn
      cCapt     := myLangeRes(8)   // "Отмена"
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
        ACTION {|| This.Enabled := .F., aRet := {}, Form_Index.Release() }

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
                   IIF( nIndex == 0, aRet := {}, aRet := { nIndex, aOrdName[nIndex] } ) ,;
                   IIF( LEN(aRet) == 0, This.Label_1.Setfocus, Form_Index.Release() ) ,;
                   Form_Index.Btn_Ok.Enabled := .T.  }

   END WINDOW

   CENTER WINDOW   Form_Index
   ACTIVATE WINDOW Form_Index //ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                              //           This.Restore , DoEvents() }
   //  для окна типа Modal
   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

RETURN aRet

//////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_Zero(oBrw)
   LOCAL oTsb, cTxt, cFont, nRow, nCol, hOld, oWnd, hWnd, aBColor
   LOCAL nH, nW, hFont, aFont, nFSize, cForm, oc1, oc2, nHDown, nHMain

   cForm   := oBrw:cParentWnd
   oTsb    := oBrw:Cargo:oParam             // получить данные из объекта
   hFont   := GetFontHandle(oTsb:aFont[2])
   aFont   := GetFontParam(hFont)
   cFont   := aFont[1]
   cFont   := "Comic Sans MS"
   nFSize  := aFont[2] + 2
   aBColor := { 253,234,218 }              // цвет фона окна
   nHMain  := Getproperty( cForm, "Height" ) - GetBorderHeight() - GetTitleHeight()
   // расчёт по таблице
   oc1     := oBrw:GetCellSize(1, 1)
   oc2     := oBrw:GetCellSize(oBrw:nRowCount(), 1)
   nRow    := oc1:nRow
   nCol    := oc1:nCol + 1
   nW      := GetWindowWidth(oBrw:hWnd) - GetVScrollBarWidth() + 2 // и справа
   nH      := GetWindowHeight(oBrw:hWnd)
   nHDown  := nHMain - nH
   nH      -= ( oBrw:nHeightSuper + oBrw:nHeightHead +  oBrw:nHeightSpecHd )
   nH      -= ( oBrw:nHeightFoot + GetHScrollBarHeight() + nHDown )
   // инфо по таблице
   cTxt    := CRLF + "DbFilter of this base: " + myDbFilter(,"LOG") + CRLF + CRLF
   cTxt    += "Set relation of this base: " + myDbRelation(,"LOG") + CRLF + CRLF
   cTxt    += "Indexes of this base: " + myDbIndexesThis(,"LOG")

   // вызов только с пользовательских окон, нельзя вызывать с контекстного меню
   //hOld  := _HMG_InplaceParentHandle
   //_HMG_InplaceParentHandle := ThisWindow.Handle
   // Внимание окно CHILD не желательно использовать !!!

   DEFINE WINDOW Forma_Zero AT nRow, nCol CLIENTAREA nW, nH MODAL NOCAPTION ;
      BACKCOLOR aBColor ON LOSTFOCUS oWnd:Release()

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle
      nW   := This.ClientWidth
      nH   := This.ClientHeight

      @ 10, 10 LABEL Label_Zero WIDTH nW-20 HEIGHT nH-20 VALUE cTxt ;
        FONT cFont SIZE nFSize TRANSPARENT

      @ nH - 28, 20 LABEL Label_Info AUTOSIZE VALUE "ESC / ENTER - exit" ;
        FONT "Tahoma" SIZE 18 BOLD TRANSPARENT CENTERALIGN VCENTERALIGN

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey( "Forma_Zero" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey( "Forma_Zero" , 0 , VK_RETURN , {|| oWnd:Release() } )

   Forma_Zero.Activate

   IF ! ISNIL(hOld) ; _HMG_InplaceParentHandle := hOld
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ReopenDbase(oBrw, cCodePage)
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

////////////////////////////////////////////////////////////////////////////////
FUNCTION TsbButtonMenu(oMenu,nWGaps,nHGaps,oThis)   // меню кнопок для таблицы
                             // nWGaps,nHGaps - отступы начала ТСБ
   LOCAL aPos, lDebug, nPosWin, lShow, cErr, nHAlign, nVAlign
   LOCAL nHMenu, aObj, nD1, nD2, nD3, nD4

   aPos   := { 0, 0, 0, 0 }
   lDebug := .F.
   lShow  := .T.
   cErr   := ""
   nD1    := nD2 := nD3 := 0

   IF hb_IsLogical(oMenu:lDebug)
      lDebug := oMenu:lDebug   // отладка, показ ошибок
   ENDIF

   IF hb_IsNumeric(oMenu:nPosWin)
      nPosWin := oMenu:nPosWin
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nPosWin := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsNumeric(oMenu:nHAlign)
      nHAlign := oMenu:nHAlign
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nHAlign := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsNumeric(oMenu:nVAlign)
      nVAlign := oMenu:nVAlign
   ELSE
      cErr += "Error ! IsNumeric() ! Not - oMenu:nVAlign := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aBtnPost)
      nD1 := LEN(oMenu:aBtnPost)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aBtnPost := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aCaption)
      nD2 := LEN(oMenu:aCaption)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aCaption := ??? ;"
      lShow := .F.
   ENDIF

   IF hb_IsArray(oMenu:aBColor)
      nD3 := LEN(oMenu:aBColor)
   ELSE
      cErr += "Error ! IsArray() ! Not - oMenu:aBColor := ??? ;"
      lShow := .F.
   ENDIF

   IF nD1 # nD2 .OR. nD1 # nD3
      cErr += "Error ! Arrays are not equal ! ;;"
      cErr += "        Len(oMenu:aBtnPost) = " + HB_NtoS(nD1) + ";"
      cErr += "        Len(oMenu:aCaption) = " + HB_NtoS(nD2) + ";"
      cErr += "        Len(oMenu:aBColor)  = " + HB_NtoS(nD3) + ";"
      lShow := .F.
   ENDIF

   IF hb_IsLogical(oMenu:lBtnIco)
      IF oMenu:lBtnIco
         IF hb_IsArray(oMenu:aIcon)
            nD4 := LEN(oMenu:aIcon)
            IF nD1 # nD4
               cErr  += "Error ! Len(oMenu:aIcon)  = " + HB_NtoS(nD4) + ";"
               lShow := .F.
            ENDIF
         ELSE
            cErr  += "Error ! IsArray() ! Not - oMenu:aIcon := ??? ;"
            lShow := .F.
         ENDIF
      ENDIF
   ENDIF

   // показ меню без ошибок
   IF lShow
      nHMenu  := IIF( hb_IsNumeric(oMenu:nHMenu), oMenu:nHMenu, 45)  // высота всего меню
      nPosWin := IIF( nPosWin == 0, 1, nPosWin )
      nPosWin := IIF( nPosWin > 4 , 1, nPosWin )
      nPosWin := IIF( nPosWin < 1 , 1, nPosWin )
      IF nPosWin == 1          // TopWindow
         aPos[1] := nHMenu - nHGaps
         aPos[4] := -nHMenu + nHGaps
      ELSEIF nPosWin == 2      // BottomWindow
         aPos[4] := -nHMenu + nHGaps
      ELSEIF nPosWin == 3      // LeftWindow
         aPos[2] := nHMenu - nWGaps
         aPos[3] := -nHMenu + nWGaps
      ELSEIF nPosWin == 4      // RightWindow
         aPos[3] := -nHMenu + nWGaps
      ENDIF

      aObj := Show_Button(oMenu,nWGaps,nHGaps,nPosWin)
      oThis:Cargo:aObjBtn := aObj

   ELSE
      IF lDebug
         cErr += ";;" + ProcNL()
         AlertStop(cErr, "Error !")
      ENDIF
   ENDIF

RETURN aPos

////////////////////////////////////////////////////////////////////////
FUNCTION Show_Button(oMenu,nY,nX,nPosWin)
   LOCAL cForm, nW, nH, aBtnCap, aBtnIco, aBtnClr, aBtnPst, cCapt
   LOCAL cN, cFont, nFSize, lFBold, nJ, aBtnGrd, aGrOver, aGrFill
   LOCAL aFntClr, nwPost, aBtnObj, aFont, aColor, lItalic, nF2Size
   LOCAL lBtnIco, nGBtn, nWBtn, nHBtn, nIndent, nIcoSize, nOnePos
   LOCAL nHAlign, nVAlign, nY2, nX2, nLenBtn, nWBthAll, nHBthAll
   LOCAL lTextVert, lTextLeft, aIco

   aBtnObj := {}
   cForm   := ThisWindow.Name
   nW      := This.ClientWidth
   nH      := This.ClientHeight
   nY2     := nY
   nX2     := nX
   // кнопки
   // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   nHAlign  := IIF( hb_IsNumeric(oMenu:nHAlign), oMenu:nHAlign, DT_LEFT )
   // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   nVAlign  := IIF( hb_IsNumeric(oMenu:nVAlign), oMenu:nVAlign, DT_TOP )
   // Отступ первой кнопки - резерв
   nIndent  := IIF( hb_IsNumeric(oMenu:nIndent), oMenu:nIndent, 0 )
   nWBtn    := oMenu:nWBtn         // ширина кнопки
   nHBtn    := oMenu:nHBtn         // высота кнопки
   nOnePos  := oMenu:nGaps         // отступ кнопки от края окна
   nGBtn    := oMenu:nGapsBtn      // между кнопками по ширине/высоте
   // подсчёт для DT_CENTER
   aBtnCap  := oMenu:aCaption
   nLenBtn  := LEN(aBtnCap)
   nWBthAll := ( nW - nWBtn * nLenBtn - nGBtn * ( nLenBtn - 1 ) ) / 2
   nHBthAll := ( nH - nHBtn * nLenBtn - nGBtn * ( nLenBtn - 1 ) ) / 2

   IF nPosWin = 1                  // TopWindow
      nY := nOnePos
      IF nHAlign == DT_LEFT        // горизонтальные кнопки
      ELSEIF nHAlign == DT_CENTER
         nX := nWBthAll
      ELSEIF nHAlign == DT_RIGHT
         nX := nW - nX2 - nWBtn
      ENDIF
   ELSEIF nPosWin = 2              // BottomWindow
      nY := nH - nHBtn - nOnePos
      IF nHAlign == DT_LEFT        // горизонтальные кнопки
      ELSEIF nHAlign == DT_CENTER
         nX := nWBthAll
      ELSEIF nHAlign == DT_RIGHT
         nX := nW - nX2 - nWBtn
      ENDIF
   ELSEIF nPosWin == 3             // LeftWindow
      nX := nOnePos
      IF nVAlign == DT_TOP         // вертикальные кнопки
      ELSEIF nVAlign == DT_CENTER
         nY := nHBthAll
      ELSEIF nVAlign == DT_BOTTOM
         nY := nH - nY2 - nHBtn
      ENDIF
   ELSEIF nPosWin == 4             // RightWindow
      nX := nW - nWBtn - nOnePos
      IF nVAlign == DT_TOP         // вертикальные кнопки
      ELSEIF nVAlign == DT_CENTER
         nY := nHBthAll
      ELSEIF nVAlign == DT_BOTTOM
         nY := nH - nY2 - nHBtn
      ENDIF
   ENDIF

   IF hb_IsLogical(oMenu:lBtnIco)
      lBtnIco := oMenu:lBtnIco
   ELSE
      lBtnIco := .F.       // F-кнопки без иконок
   ENDIF
   nIcoSize  := oMenu:nIcoSize
   aIco      := IIF( hb_IsArray(oMenu:aIcon), oMenu:aIcon, {} )
   aBtnCap   := oMenu:aCaption
   aBtnIco   := oMenu:aIcon
   aBtnClr   := oMenu:aBColor
   aBtnPst   := oMenu:aBtnPost
   // фонты  для кнопок
   aFntClr   := oMenu:aFClr  //{ BLACK , OLIVE }
   aFont     := oMenu:aFont
   cFont     := aFont[1]     //"Comic Sans MS"
   nFSize    := aFont[2]     //16
   lFBold    := aFont[3]     //.T.
   lItalic   := aFont[4]     //.T.
   nF2Size   := aFont[5]     //32
   // текст для кнопок
   lTextVert := IIF( hb_IsLogical(oMenu:lTextVert), oMenu:lTextVert, .F. )
   lTextLeft := IIF( hb_IsLogical(oMenu:lTextLeft), oMenu:lTextLeft, .F. )

   FOR nJ := 1 TO LEN(aBtnCap)
      cCapt   := StrTran( aBtnCap[nJ], ";" , CRLF )
      aColor  := aBtnClr[nJ]
      aBtnGrd := { HMG_RGB2n( aColor ), CLR_WHITE }  // градиент кнопки
      aGrOver := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFill := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }
      nwPost  := aBtnPst[nJ]
      cN      := 'Btn_' + HB_NtoS(nwPost) //StrZero(nJ, 2)
      AADD( aBtnObj, { cN, aBtnCap[nJ], aColor, nwPost } )

      IF lBtnIco .AND. LEN(aIco) > 0  // кнопки с иконками
         aIco := aBtnIco[nJ]
      ENDIF

      // alt_syntax
      DEFINE BUTTONEX &cN
         ROW           nY
         COL           nX
         WIDTH         nWBtn
         HEIGHT        nHBtn
         CAPTION       cCapt
         ICON          IIF( lBtnIco, aIco[1], Nil )
         FONTNAME      cFont
         FONTSIZE      nFSize
         FONTCOLOR     aFntClr[1]
         FONTBOLD      .F.
         BACKCOLOR     aGrOver
         GRADIENTFILL  aGrFill
         LEFTTEXT      lTextLeft
         VERTICAL      lTextVert
         FLAT          .T.
         NOHOTLIGHT    .F.
         NOXPSTYLE     .T.
         HANDCURSOR    .T.
         NOTABSTOP     .T.
         ONMOUSEHOVER ( myMouseHL(2) )
         ONMOUSELEAVE ( myMouseHL(1) )
         ACTION ( This.Enabled := .F., _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) )  // 29.06.23
         //ACTION ( This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) )
         /*ONINIT {|o|   // можно и так делать
                    This.Cargo := oHmgData()  // создать объект (контейнер) для этой кнопки
                    o := This.Cargo
                    // положим на кнопку нужные данные
                    o:nBtn     := nJ
                    o:nPost    := nwPost
                    o:cCapt    := cCapt
                    o:aIco     := aIco
                    o:aBClr    := aColor
                    o:cObj     := cN
                    o:aGrFill  := aGrFill
                    o:aGrOver  := aGrOver
                    o:aFntClr1 := aFntClr[1]
                    o:aFntClr2 := aFntClr[2]
                    o:lBold2   := .T.
                    o:lBold1   := .F.
                    o:nFSize2  := nF2Size       // увеличенный фонт кнопки
                    o:nFSize   := nFSize        // фонт кнопки
                    o:nIcoSize := nIcoSize      // размер иконки
                    o:aIco     := aIco          // 2 иконки кнопки
                    o:lBtnIco  := lBtnIco       // есть/нет иконка на кнопке
                    Return Nil
                   }          // ON INIT надо задавать только блоком кода */
      END BUTTONEX

      This.&(cN).Cargo := oHmgData()
      WITH OBJECT This.&(cN).Cargo
         :nBtn     := nJ
         :nPost    := nwPost
         :cCapt    := cCapt
         :aIco     := aIco
         :aBClr    := aColor
         :cObj     := cN
         :aGrFill  := aGrFill
         :aGrOver  := aGrOver
         :aFntClr1 := aFntClr[1]
         :aFntClr2 := aFntClr[2]
         :lBold2   := .T.
         :lBold1   := .F.
         :nFSize2  := nF2Size       // увеличенный фонт кнопки
         :nFSize   := nFSize        // фонт кнопки
         :nIcoSize := nIcoSize      // размер иконки
         :aIco     := aIco          // 2 иконки кнопки
         :lBtnIco  := lBtnIco       // есть/нет иконка на кнопке
      END WITH

      // сдвиг кнопки
      IF nPosWin = 1 .OR. nPosWin = 2   // TopWindow-BottomWindow
         // горизонтальные кнопки
         IF nHAlign == DT_LEFT .OR. nHAlign == DT_CENTER
            nX += nWBtn + nGBtn
         ELSE
            nX -= ( nWBtn + nGBtn )
         ENDIF
      ELSE                             // LeftWindow-RightWindow
         // вертикальные кнопки
         IF nVAlign == DT_TOP .OR. nVAlign == DT_CENTER
            nY += nHBtn + nGBtn
         ELSE
            nY -= ( nHBtn + nGBtn )
         ENDIF
      ENDIF

      IF lBtnIco .AND. LEN(aIco) > 0  // кнопки с иконками
         // при первом построении изменить размеры иконки
         This.&(cN).ImageWidth  := nIcoSize
         This.&(cN).ImageHeight := nIcoSize
         This.&(cN).Icon        := LoadIconByName( aIco[1], nIcoSize, nIcoSize )
      ENDIF

   NEXT

RETURN aBtnObj

///////////////////////////////////////////////////////////////////
FUNCTION myMouseHL(n)
   LOCAL hIco, o := This.Cargo

   IF n == 2
      This.FontColor    := o:aFntClr2
      This.FontSize     := o:nFSize2
      This.FontBold     := o:lBold2
      This.GradientFill := o:aGrFill
   ELSE
      This.FontColor    := o:aFntClr1
      This.FontSize     := o:nFSize
      This.FontBold     := o:lBold1
      This.GradientOver := o:aGrOver
   ENDIF

   IF o:lBtnIco      // кнопки с иконками
      hIco := LoadIconByName( o:aIco[n], o:nIcoSize, o:nIcoSize )
      This.Icon := hIco
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
// получить Width текста
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth

