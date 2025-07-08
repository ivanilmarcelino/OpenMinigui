/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Разное редактирование ячеек таблицы (для массивов) из DBf-файла
 * _TBrowse() Miscellaneous editing of table cells (for arrays) from DBf-file
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "ord.ch"

FIELD KMaster, Master, KTIPZA, TIPZA, KWORKS, WORKS, KNNEISPR, NNEISPR
FIELD KNeisGrp, NeisGrp, Knopka0, Naim, KSrokZa, SrokZa, KVIPZA, VIPZA
FIELD KOperat, Operat, KOB1ZAIV, OB1ZAIV, KOB2WORK, OB2WORK, KOb4orud, Ob4orud
//////////////////////////////////////////////////////////////////////
FUNCTION UseDbf(aSpr)
   LOCAL cDbf,cAls, cPath, cMask, cAttr, aDirs, nJ
   LOCAL cFor, xVal, aDim, cRet
   LOCAL aDbf := { "zDefect.dbf", "Master.dbf", "Operat.dbf" }

   cPath := App.Cargo:cPathDbf
   cDbf  := cPath + aDbf[1]
   cAls  := "Defect"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   //? ProcNL(), "проверка",HB_ValToExp( Zaiv_PropDbf(120) )

   cDbf := cPath + aDbf[2]
   cAls := "MASTER"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KMaster       TAG CODE
      INDEX ON UPPER(Master) TAG NAME
   EndIf
   OrdSetFocus(1)
   DbGotop()

   cDbf := cPath + aDbf[3]
   cAls := "OPERAT"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KOperat       TAG CODE
      INDEX ON UPPER(Operat) TAG NAME
   EndIf
   OrdSetFocus(1)
   DbGotop()

   cDbf := cPath + "TIPZA.DBF"
   cAls := "TIPZA"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KTIPZA       TAG CODE
      INDEX ON UPPER(TIPZA) TAG NAME
   EndIf
   OrdSetFocus(1)
   DbGotop()

   cDbf := cPath + "WORKS.DBF"
   cAls := "WORKS"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KWORKS       TAG CODE
      INDEX ON UPPER(WORKS) TAG NAME
   EndIf
   OrdSetFocus(1)
   DbGotop()

   //cPath := App.Cargo:cPathStart
   cMask := "*.*"
   cAttr := "D"
   aDirs := hb_Directory(cPath + cMask, cAttr)

   cDbf := cPath + "NeisGrp.dbf"
   cAls := "NeisGrp"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KNeisGrp       TAG CODE
      INDEX ON UPPER(NeisGrp) TAG NAME
   EndIf

   cDbf := cPath + "Neispr.dbf"
   cAls := "Neispr"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON Knopka0     TAG CODE
      INDEX ON UPPER(Naim) TAG NAME
   EndIf

   cDbf := cPath + "N_SrokZa.dbf"
   cAls := "SrokZa"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KSrokZa     TAG CODE
      INDEX ON UPPER(SrokZa) TAG NAME
   EndIf

   // подготовим массив из базы
#ifdef KEY_ENG // for this project demo1-en.hbp
   cDbf := cPath + "NNEISPR.dbf"
#else
   cDbf := cPath + "NNEISPR-ru.dbf"
#endif
   cAls := "Sprav"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   cFor := "KVIEW == 1 .AND. !Deleted()"
   If OrdCount() < 1
      INDEX ON KNNEISPR       TAG CODE
      INDEX ON UPPER(NNEISPR) TAG NAME FOR &cFor
   EndIf
   OrdSetFocus(2)
   DbGotop()
   aSpr := {}
   FOR nJ := 1 TO  ORDKEYCOUNT()
      ORDKEYGOTO(nJ)
      aDim := {}
      AADD( aDim, .F. )
      AADD( aDim, ALLTRIM( (cAls)->NNEISPR ) )
      AADD( aDim, (cAls)->KNNEISPR )
      xVal := (cAls)->KNEISGRP
      cRet := Alltrim( SAY_SEL_DIM(xVal,'NeisGrp','NeisGrp'  ) )
      AADD( aDim, cRet )
      xVal := (cAls)->Knopka0
      cRet := Alltrim( SAY_SEL_DIM(xVal,'Neispr','Naim'  ) )
      AADD( aDim, cRet )
      xVal := (cAls)->KSrokZa
      cRet := Alltrim( SAY_SEL_DIM(xVal,'SrokZa','SrokZa'  ) )
      AADD( aDim, cRet )
      AADD( aDim, nJ   )
      // итоговый массив
      AADD( aSpr, aDim )
   NEXT
   App.Cargo:aSprDfct := aSpr // массив dbf-неисправностей / dbf fault array

   // ---------- "zOB1ZAIV.DBF", "zOB2WORK.DBF", "zOb4orud.dbf" }
   cDbf := cPath + "zOB1ZAIV.dbf"
   cAls := "OB1ZAIV"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KOB1ZAIV       TAG CODE
      INDEX ON UPPER(OB1ZAIV) TAG NAME
   EndIf

   cDbf := cPath + "zOB2WORK.dbf"
   cAls := "OB2WORK"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KOB2WORK       TAG CODE
      INDEX ON UPPER(OB2WORK) TAG NAME
   EndIf

   cDbf := cPath + "zOb4orud.dbf"
   cAls := "Ob4orud"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED  CODEPAGE "RU866"
   cFor := "KVIEW == 1 .AND. !Deleted()"
   If OrdCount() < 1
      INDEX ON KOb4orud       TAG CODE
      INDEX ON UPPER(Ob4orud) TAG NAME
      INDEX ON UPPER(Ob4orud) TAG KVIEW FOR &cFor
   EndIf
   OrdSetFocus(3)
   DbGotop()

   cDbf := cPath + "zVIPZA.DBF"
   cAls := "VIPZA"
   USE ( cDbf ) ALIAS (cAls) NEW SHARED CODEPAGE "RU866"
   If OrdCount() < 1
      INDEX ON KVIPZA       TAG CODE
      INDEX ON UPPER(VIPZA) TAG NAME
   EndIf
   OrdSetFocus(1)
   DbGotop()


RETURN .T.


//////////////////////////////////////////////////////////////////////
//  ВыбоР Элемента массива    *
//  1-код поля, 2-имя базы, 3-наим.поля -> возврат его
FUNCTION Say_Sel_Dim(nKod, NAME_dbf, cPOLE_KOD, lAnsiOem)
   LOCAL nSel:=SELECT(), nRec:=RECNO(), nS, xKod , nRet, cType, name_ntx1, nOrder
   DEFAULT lAnsiOem := .F. // перевод строки OEM -> ANSI

   nS := SELECT( name_dbf )
   IF nS == 0
      RETURN "Ошибка! Нет алиаса: " + name_dbf
   ENDIF

   SELECT( SELECT( name_dbf ) )
   nOrder := INDEXORD()  // Результат: NUMBA

   IF VALTYPE(nKod) == "N"
      nRet := FIELDNUM(ALLTRIM(cPOLE_KOD))
      cType :=  FIELDTYPE(nRet)
      IF nRet == 0
         xKod:= 'Нет поля "'+cPOLE_KOD+'" в БД "'+NAME_dbf+'" по алиасу "'+ALIAS()+'" !'
         IF lAnsiOem
            xKod := HB_OEMTOANSI(xKod)
         ENDIF
      ELSE
         name_ntx1 := DBORDERINFO(DBOI_INDEXNAME,, ALIAS(NAME_dbf) )
         DBSETORDER(1)
         GOTO TOP
         SEEK nKod
         IF FOUND()
            xKod := FIELDGET(nRet)
            IF lAnsiOem
               xKod := HB_OEMTOANSI(xKod)
            ENDIF
            IF VALTYPE(xKod) ==  "С"    //     Символьный
               xKod := ALLTRIM(xKod)
            ENDIF
         ELSE
            xKod:= "нет данных"
            IF lAnsiOem
               xKod := HB_OEMTOANSI(xKod)
            ENDIF
            DO CASE
               CASE cType ==  "С"    //     Символьный
                  xKod:="нет данных"
                  IF lAnsiOem
                     xKod := HB_OEMTOANSI(xKod)
                  ENDIF
                  xKod := ALLTRIM(xKod)
               CASE cType ==  "N"    //     Числовой
                  xKod:= -1
               CASE cType ==  "D"    //     Date
                  xKod:=CTOD("00.00.00")
               CASE cType ==  "L"    //     Логический
                  xKod:= "нет данных"
                  IF lAnsiOem
                     xKod := HB_OEMTOANSI(xKod)
                  ENDIF
            ENDCASE
         ENDIF
      ENDIF

   ELSE
      xKod:= 'Поле = "'+VALTYPE(nKod)+'"'
      IF lAnsiOem
         xKod := HB_OEMTOANSI(xKod)
      ENDIF
   ENDIF

   DBSETORDER(nOrder)  // Возврат текущего ордера базы справочника
   SELECT( nSel )

RETURN xKod

///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

///////////////////////////////////////////////////////////////////////////////
// получить Width текста
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// (5) - function for reading field (4) and writing to column (2)
FUNCTION GetNumZa()
   LOCAL cD, cDat, cRet, cAls := ALIAS()
   cD   := DTOS((cAls)->Dateza) + " "
   cDat := SUBSTR(cD,7,2)+"."+SUBSTR(cD,5,2)+"."+SUBSTR(cD,1,4)
   cRet := ALLTRIM( TRANSFORM((cAls)->NNza, "@R 999999/99")) + " "
   cRet += cDat + " " + TRANSFORM((cAls)->TimeZa, "@R 99:99") + " "
RETURN cRet

// {"Order Type/Order Type", SPACE(20) , "CALC", "" , "Za_TpVid()" , "FuncWrtNum()", nil, "W", {} } )
FUNCTION Za_TipVid(aParam)    // это показ из нескольких полей базы разных типов
   LOCAL aDim := aParam       // можно не использовать
   LOCAL cRet, aRet, cAls := ALIAS()
   // взято из Zaiv_Type_Vid()
   //cRet := Alltrim( SAY_SEL_DIM((cAls)->Ktipza,'tipza','tipza'  ) ) + " , "
   //cRet += Alltrim( SAY_SEL_DIM((cAls)->KWorks,'Works','Works') )
   cRet := "Sample: "
   cRet += "Ktipza=" + HB_NtoS((cAls)->Ktipza) + " , "
   cRet += "KWorks=" + HB_NtoS((cAls)->KWorks) + " , "
   cRet += "DatePere=" + DTOC((cAls)->DatePere) + " , "
   //cRet += "SumPlata=" + HB_NtoS((cAls)->SumPlata) + " , "
   //cRet += "Dispetch=" + (cAls)->Dispetch + " , "
   cRet += "KAdres1=" + HB_NtoS((cAls)->KAdres1) + " , "
   cRet += "mAktVip=" + (cAls)->mAktVip + " !"
   aRet := { (cAls)->Ktipza, (cAls)->KWorks, (cAls)->DatePere, (cAls)->SumPlata, (cAls)->Dispetch, (cAls)->KAdres1, (cAls)->mAktVip }
RETURN { cRet, aRet }

FUNCTION Za_TipVid2(aParam)    // это показ из нескольких полей базы разных типов
   LOCAL aDim := aParam        // можно не использовать
   LOCAL cRet, aRet, cAls := ALIAS()
   //cRet := "Ktipza=" + HB_NtoS((cAls)->Ktipza) + " , "
   //cRet += "KWorks=" + HB_NtoS((cAls)->KWorks) + " !"
   cRet := Alltrim( SAY_SEL_DIM((cAls)->Ktipza,'tipza','tipza'  ) ) + " , "
   cRet += Alltrim( SAY_SEL_DIM((cAls)->KWorks,'Works','Works') )
   aRet := { (cAls)->Ktipza, (cAls)->KWorks }
RETURN { cRet, aRet }

FUNCTION Za_SrokZa()   // тип CALC
   LOCAL aRet, cRet := "", cAls := Alias()
   //cRet := ALLTRIM( Say_Sel_Dim((cAlias)->Ksrokza, "srokza", "srokza") )
   cRet := "Sample: Ksrokza = " + HB_NtoS( (cAls)->Ksrokza )
   cRet += ", Completion date: " + DTOC((cAls)->DateSrok) + " - "
   cRet += CDOW((cAls)->DateSrok)
   aRet := {(cAls)->Ksrokza, (cAls)->DateSrok }
RETURN { cRet, aRet }

///////////////////////////////////////////////////////////////////////////////////////////////////
// Просроченость заявки Za_ProSrok()
FUNCTION Za_ProSrok()
   LOCAL cRet := " ", nRet, dDate, cAls := Alias()

   IF (cAls)->KVipZa == 3 // Заявка: не выполнена

      IF (cAls)->Ksrokza == 0  // справочник "Срок выполнения" заявки
         cRet += 'ERROR! The "Urgency type" column is not filled in!'
         nRet := 2 // не заполнено поле
      ELSE
         IF (cAls)->DateSrok == CTOD("")
            cRet += 'ERROR! The "Due date" column is not filled in!'
            nRet := 3  // не заполнено поле
         ELSE
            dDate := (cAls)->DateSrok
            IF DATE() > dDate
               cRet += ' BID IS EXPIRED!!! Today: '+DTOC(DATE())+">"+DTOC(dDate)
               nRet := 1   // просрочена заявка
            ELSEIF DATE() == dDate
               cRet += '!!! Last day of APPLICATION execution!!! Today: '+DTOC(DATE())+' !'
            ENDIF
         ENDIF
      ENDIF

   ENDIF

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION Za_VipZa()   // тип CALC
   LOCAL aRet, cRet := "", cAls := Alias()
   cRet := ALLTRIM( Say_Sel_Dim((cAls)->KVipZa, "VipZa", "VipZa") )
   cRet += " , " + DTOC((cAls)->DATEVip) + " , "
   cRet += TRANSFORM( (cAls)->TimeVip , "@Z 99:99" )
   aRet := { (cAls)->KVipZa, (cAls)->DATEVip, (cAls)->TimeVip, (cAls)->INET }
RETURN { cRet, aRet }

//////////////////////////////////////////////////////////////////////////
FUNCTION Zaiv_PropDbf(aPar)           // Свойства заявки
   LOCAL cRet := "", aDim, nFld, cVal, aRet, cAls := Alias()
   DEFAULT aPar := {}

   nFld := (cAls)->KZ_PKV              // 1
   IF nFld > 0
      aDim := { "entrance" , "apartment" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Repair type: "+ cVal + ", "
   ENDIF

   nFld := (cAls)->KTipUst              // 2
   IF nFld > 0
      aDim := { "regular" , "at the company's expense" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Installation type: "+ cVal + ", "
   ENDIF

   nFld := (cAls)->KodTruba            // 3
   IF nFld > 0
      aDim := { "disable" , "enable" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Disconnecting the handset: "+ cVal + ", "
   ENDIF

   nFld := (cAls)->KTexOsm            // 4
   IF nFld > 0
      aDim := { "to do" , "not to do" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Tech. inspection: "+ cVal + ", "
   ENDIF

   nFld := (cAls)->KZvonok          // 5
   IF nFld > 0
      aDim := { "yes" , "no" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Pre-dial: "+ cVal + ", "
   ENDIF

   nFld := (cAls)->KPrint         // 6
   IF nFld > 0
      aDim := { "yes" , "no" }
      IF nFld > LEN(aDim)
         cVal := "???"
      ELSE
         cVal := aDim[nFld]
      ENDIF
      cRet += "Application printed: "+ cVal + ", "
   ENDIF

   cRet := ALLTRIM(cRet)
   IF SUBSTR(cRet, LEN(cRet), 1) == ","
      cRet := SUBSTR(cRet, 1, LEN(cRet)-1)
   ENDIF

   aRet := {(cAls)->KZ_PKV, (cAls)->KTipUst, (cAls)->KodTruba, (cAls)->KTexOsm, (cAls)->KZvonok, (cAls)->KPrint }

RETURN { cRet, aRet }

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION ZaivSay(aPar)
   LOCAL cStr, cRet, aRet, cAls := ALIAS()
   DEFAULT aPar := {}

#ifdef KEY_ENG // for this project demo1-en.hbp
   cRet := " [[[ editing bottom list ...... ]]] "
#else
   cRet := " [[[ редактирование нижнего списка ...... ]]] "
#endif

   aRet := ALLTRIM((cAls)->aDefect)  // поле записи в базу для совместимости со старой программой
   //MsgDebug(VALTYPE(aRet), aRet)
   cStr := "++++++++ " + ProcNL() + " DBF:" + cAls + " +++++++++"
   //? cStr, "aRet=",VALTYPE(aRet), aRet
   IF IsString(aRet)
      IF AT( "{", aRet ) > 0 .AND. AT( "}", aRet ) > 0
         // это массив
         aRet := &aRet
      ELSE
        aRet := {}
      ENDIF
   ELSE
      aRet := {}
   ENDIF

RETURN { cRet, aRet }


///////////////////////////////////////////////////////////////////////////////////////////////////
// функция записи в базу
FUNCTION ZaWrtDfc(aPar)
   LOCAL cStr, cRet, aRet, cAls := ALIAS()
   DEFAULT aPar := {}

   cStr := "++++++++ " + ProcNL() + " DBF:" + cAls + " +++++++++"
   //? cStr, "aPar=",aPar ; ?v aPar
   aRet := aPar[4]
   //? "++++++++ [" + VALTYPE(aRet) + "] " , aRet
   cRet := "возврат из функции: ZaWrtDfc()"
   IF IsArray(aRet)
      cRet := HB_ValtoExp(aRet)
   ENDIF
   //MsgDebug(cStr,"[", aPar, "]", cRet)
RETURN cRet

/////////////////////////////////////////////////////////////////////////
FUNCTION myMemo(...)
   LOCAL cRet, cFld, nLen, nLine, cAls := ALIAS(), aParams := hb_aParams()

   cFld  := aParams[1]
   nLen  := App.Cargo:nMemoChar //aParams[2]
   nLine := aParams[3]
   cRet  := MemoLine( (cAls)->&cFld, nLen, nLine )

RETURN cRet


/////////////////////////////////////////////////////////////////////////
FUNCTION ZA_MOb4Err()
   LOCAL cRet, aRet, nLenRet := App.Cargo:nMemoChar, cAls := ALIAS()

   // выполнена заявка          Выпол.работы/Оборудование
   //IF (cAls)->Kvipza == 1 .AND. LEN(ALLTRIM((cAls)->MOb4orud)) == 0
   IF LEN(ALLTRIM((cAls)->MOb4orud)) == 0
      //cRet := " ОШИБКА ! Не заполнена эта графа !"
      cRet := "fill this field if necessary..."
   ELSE
      cRet := MemoLine( (cAls)->MOb4orud,nLenRet,1 )
   ENDIF
   //cCmp := "MKob4or"    // поле записи в базу кодов оборудования из старой программы
   aRet := (cAls)->MKob4or

RETURN { cRet, aRet }

////////////////////////////////////////////////////////////////////////////
// запись массива полей (10) и значений (8) в базу
FUNCTION Set_ZAMOb4(aVal)     // aPara := { cRType, cField, x13Col, x14Col, x15Col, nI, aLine }
   LOCAL nI, nFld, cFld, cMsg, cErr, xVal, cFType, cXType, cAls, lWrite, aLine
   LOCAL c4Cod, c4Fld, a5Dim, a5Fld, a5Val, cVal, cRet
   //? "==============================" + ProcNL()
   // aVal :=
   // [1]  М,проч.обор.,K-DOM 750,2.00 шт., цена 1 650.00 стоимостью 3 300.00; М,аудио,А/У DP-20H,1.00 шт., цена 700.00 стоимостью 700.00; Т/О,замена/уст,БВД Элтис DP 400-TD,1.00 шт.;
   // [2]  CALC
   // [3]  MOb4orud
   // [4]  2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;
   // [5]  {{"SumVsego", "SumWObor", "SumMaster"}, {4000.0000, 0.0000, 530.0000}}
   // [6]  ''
   // [7]  59
   // [8]  {"(*) Выполнены работы/Оборудование", "М,проч.обор.,K-DOM 750,2.00 шт., цена 1 650.00 стоимостью 3 300.00; М,аудио,А/У DP-20H,1.00 шт., цена 700.00 стоимостью 700.00; Т/О,замена/уст,БВД Элтис DP 400-TD,1.00 шт.; ", 58, "CALC", "MOb4orud", "ZA_MOb4Err()", "Set_ZAMOb4()", "Tovar_HMG()", "W", "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;", '{{"SumVsego", "SumWObor", "SumMaster"}, {4000.0000, 0.0000, 530.0000}}', "", "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;", {{"SumVsego", "SumWObor", "SumMaster"}, {4000.0000, 0.0000, 530.0000}}, ""}
   //
   //cCmp := "MKob4or"    // поле записи в базу кодов оборудования из старой программы
   cRet  := aVal[1]
   c4Cod := aVal[4]
   c4Fld := "MKob4or"
   a5Dim := aVal[5]
   //? "a5Dim=", a5Dim, VALTYPE(a5Dim)
   a5Fld := a5Dim[1]   // поля базы
   a5Val := a5Dim[2]   // значение для записи в базу
   aLine := aVal[8]
   // 1  (*) Выполнены работы/Оборудование
   // 2  М,проч.обор.,K-DOM 750,2.00 шт., цена 1 650.00 стоимостью 3 300.00; М,аудио,А/У DP-20H,1.00 шт., цена 700.00 стоимостью 700.00; Т/О,замена/уст,БВД Элтис DP 400-TD,1.00 шт.;
   // 3  58
   // 4  CALC
   // 5  MOb4orud
   // 6  ZA_MOb4Err()
   // 7  Set_ZAMOb4()
   // 8  Tovar_HMG()
   // 9  W
   // 10 2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;
   // 11 {{"SumVsego", "SumWObor", "SumMaster"}, {4000.0000, 0.0000, 530.0000}}
   // 12 ''
   // 13 2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;
   // 14 {{"SumVsego", "SumWObor", "SumMaster"}, {4000.0000, 0.0000, 530.0000}}
   // 15 ''
   //
   cMsg  := "=.=.=.=.=.= Error! Line: " + HB_NtoS(aVal[6]) + ";"
   aLine[10] := ATREPL( ";", aLine[10], "|" )
   aLine[13] := ATREPL( ";", aLine[13], "|" )
   cMsg  += HB_ValToExp(aLine) + ";;"
   cAls  := ALIAS()
   cErr  := ""

   IF (cAls)->( RLock() )
      (cAls)->&c4Fld := c4Cod
      (cAls)->( DbUnlock() )
      (cAls)->( DbCommit() )
   ELSE
      cErr += "WRITE ERROR ! Write " + c4Fld + " -> "
      cErr += "[" + c4Cod + "] !  "
      cErr += HB_NtoS(RECNO()) + " blocked !;"
   ENDIF

   FOR nI := 1 TO LEN(a5Fld)
      cFld  := a5Fld[nI]
      xVal  := a5Val[nI]
      nFld  := FIELDNUM( cFld )
      IF nFld == 0
         cErr += "No such field ["+cFld+"] in DB-"+cAls+";"
      ELSE
         cFType := FieldType( FIELDNUM( cFld ) )
         cXType := VALTYPE(xVal)
         lWrite := .T.
         IF cFType # cXType
            IF !IsChar(xVal) ; xVal := cValToChar(xVal)
            ENDIF
            IF cFType == "M" .AND. cXType == "C"
               // пропуск ошибки
            ELSE
               cErr += "Different database field types ["+cFType+"] and record values ["+cXType+"];"
               cErr += "xVal=" + xVal + ";;"
               lWrite := .F.
            ENDIF
         ENDIF
         IF lWrite
            IF (cAls)->( RLock() )
               (cAls)->&cFld := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ELSE
               cErr += "WRITE ERROR ! Write " + cFld + " -> "
               IF IsArray(xVal)
                   cVal := HB_ValToExp(xVal)
               ENDIF
               IF !IsString(xVal)
                   cVal := cValToChar(xVal)
               ENDIF
               cVal := ALLTRIM(cVal)
               cErr += "[" + cVal + "] !  "
               cErr += HB_NtoS(RECNO()) + " blocked !;"
            ENDIF
         ENDIF
      ENDIF
   NEXT
   IF LEN(cErr) > 0
      cMsg += cErr + ";"
      cMsg += ProcNL() + ";" + ProcNL(1)
      cMsg += ";" + ProcNL(2) + ";" + ProcNL(3)
      AlertStop(cMsg, "", , 64, {RED})
      cMsg += ";" + REPL("=.",40) + ";"
      ? "==============================" + ProcNL()
      ? ATREPL( ";", cMsg, CRLF )
      ? "==============================" ; ? "."
   ENDIF
   //? "  RETURN=", cRet
   //? "==============================" + ProcNL()
   //? "."
RETURN cRet

//////////////////////////////////////////////////////////////////////////////////////
FUNCTION CalcMemoLine(oBrw)     // для функции MEMOLINE(xxxx,App.Cargo:nMemoChar,1)
   LOCAL nLenChar, oCol, nWCell, hFont, aRet, nWChar, cStr
   LOCAL cIni := App.Cargo:cIniMemoLine
   //Local oPar := oBrw:Cargo:oParam
   DEFAULT oBrw := "нет объекта таблицы"

   nLenChar := 35
   ? "#####" + ProcNL(), "nLenChar=",nLenChar
   IF !FILE(cIni)
      aRet := { nLenChar, "MEMOLINE()", App.ExeName }
      HB_MemoWrit( cIni, HB_ValToExp(aRet) )
   ELSE
      cStr := ALLTRIM( hb_MemoRead(cIni) )
      IF LEN(cStr) > 0
         IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0
            aRet      := &cStr
            nLenChar  := aRet[1]
         ENDIF
      ENDIF
      IF !IsNumeric(nLenChar)
         nLenChar := 35
      ENDIF
      nLenChar := IIF(nLenChar < 15, 35, nLenChar)
   ENDIF
   ? "#####", cIni, "nLenChar=",nLenChar

   IF ISOBJECT(oBrw)
      oCol     := oBrw:aColumns[3]
      hFont    := oCol:hFont
      nWCell   := oCol:nWidth
      nWCell   -= GetVScrollBarWidth()
      nWCell   -= GetTextWidth( Nil, "A", hFont )   // :nCellMarginLR
      nWCell   -= 24                                // oCol:uBmpCell
      nWCell   -= 12                                // добавка
      nWChar   := GetFontWidth("Normal", 1)         // "Normal" 1 знак
      nLenChar := int( nWCell / nWChar )            // ~ кол-во символов в колонке
      // запись в ини-файл
      aRet := { nLenChar, "MEMOLINE()", nWCell, App.ExeName }
      HB_MemoWrit( cIni, HB_ValToExp(aRet) )
   ENDIF
   ? "###########" + ProcNL(), "nLenChar=",nLenChar

RETURN nLenChar

