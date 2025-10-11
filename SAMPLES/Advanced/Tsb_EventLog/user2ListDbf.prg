/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"

///////////////////////////////////////////////////////////////
FUNCTION UserListDbf(cAls)
   LOCAL cIco, nIco, cTtl, aRet, aArray, oTsb, aBtn, oWin
   LOCAL bInitForm, cMsg, nReport, cLang

   ? ProcNL(), "Operators Directory:", cAls

   SELECT OPERAT
   //DbSetOrder(3)
   OrdSetFocus("KGROUP")    // !!! ставим сразу здесь / we put it right here

   nReport := FieldNum("LREPORT")
   aArray  := ALIAS()
   cIco    := "iUsers48x1"
   nIco    := 96

   oWin := oHmgData()
   oWin:aBColor   := { 210, 166, 236 }                // цвет всей формы

#ifdef KEY_ENG
   cTtl       := "Program Operators Directory"
   oWin:cHelp := "To display the operator in the report, you must check the box in the [Reports] column !" + CRLF
   oWin:cHelp += "If you don't need the operator in the reports, uncheck the box !" + CRLF
   cMsg       := "Error! No field for Reports!; "+ALIAS()+"->LREPORT;;Contact the program developer"
   cLang      := "Exit"
#else
   cTtl       := "Справочник операторов программы"
   oWin:cHelp := "Для показа оператора в отчёте необходимо поставить галочку в колонке [Отчёты] !" + CRLF
   oWin:cHelp += "  Если не нужен оператор в отчётах, то уберите галочку" + CRLF
   cMsg       := "Ошибка ! Нет поля для Отчётов !;"+ALIAS()+"->LREPORT;;Обратится к разработчику программы"
   cLang      := "Выход"
#endif

   IF nReport == 0
      AlertStop( cMsg, , , 64, {RED} )
      RETURN NIL
   ENDIF

   oWin:aFntHelp  := { "Arial", 16, .T., .T. }
   oWin:aHelpFClr := MAROON
   oWin:aBtnFClr  := { WHITE  }                       // цвет фонта кнопки
   oWin:aBtnBClr  := { MAROON }                       // цвет фона кнопки
   oWin:aBtnFClr2 := WHITE                            // инвертный цвет фонта кнопки (фокус на кнопке)
   oWin:aBtnBClr2 := BLUE                             // инвертный цвет фона кнопки  (фокус на кнопке)
   aBtn           := { cLang }                        // кнопки на форме
   bInitForm      := Nil                              // блок кода после показа формы

   oTsb := ParamTsb7()
   aRet := AlertTSB7(cIco,nIco,aArray,cTtl,aBtn,oWin,oTsb,bInitForm)
   DO EVENTS

   ? ProcNL(), "END! ###", ALIAS(), "->", cAls, "|"
   DbSelectArea(cAls)
   ?? ALIAS()

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION ParamTsb7()
   LOCAL oTsb

   oTsb := oHmgData()
   //oTsb:aNumber   := {}                         // не задавать колонку нумерации, по умолчанию есть колонка
   //                            1           2          3         4             5                    6
   //oTsb:aHead     := { "UserCode"     , "User"  , "Group" , "Label"   } //, "EditedDate/Time"  , "Print"  }
   oTsb:aFoot       := { "(1)"          , "(2)"   , "(3)"   , "(4)"     } //, "(5)"              , "(6)"    }
   oTsb:aEdit       := { .F.            , .T.     , .F.     , .T.       } //, .T.                , .T.      }  // редактировать колонки
   oTsb:aField      := {"KOPERAT"       ,"OPERAT" , "KGROUP", "LREPORT" } //, "KGROUP", "TS"               , "lPRN"   }
   oTsb:aName       := oTsb:aField
   //oTsb:nHeightHead := 1                        // высота шапки   - убрать шапку таблицы
   //oTsb:nHeightFoot := 1                        // высота подвала - убрать подвал таблицы
   //oTsb:lFooting    := .F.                      // НЕ ставить в таблице подвал
   oTsb:lSpecHd     := .F.                        // поставить в таблице нумератор колонок
   oTsb:lSuperHd    := .T.                        // поставить в таблице суперхидер

#ifdef KEY_ENG
   oTsb:aHead    := { "Operator;code", "Operator", "Group", "Reports" } //, "Group", "Edit date/time", "Print" }
   oTsb:cSuperHd := "List of operators to display in reports"
#else
   oTsb:aHead    := { "Код;оператора", "Оператор" , "Группа" , "Отчёты" } //, "Группа", "Дата/время правки", "Печать" }
   oTsb:cSuperHd := "Список операторов для показа в отчётах"
#endif

   //oTsb:aHideCol  := { 4, 5, 6 }                // скрыть колонки - резерв
   //oTsb:aSize     := aSize                      // назначим ширину колонок для ТСБ - резерв

   // ВНИМАНИЕ ! Блок-кода использовать НЕЛЬЗЯ ! он уже используется в alert_tsb.prg !
   // oTsb:bInit := {|ob,op| // настройки тсб

   oTsb:bBody_2 := {|ob,op| // другие настройки тсб для Dbf - добавочные
                     Local oc := ob:GetColumn("OPERAT")
                     oc:nFAlign  := DT_CENTER
                     /*oc:nFAlign  := DT_LEFT
                     oc:cFooting := {|nc,ob|
                                     Local na := ob:nAt, nl := ob:nLen
                                     //DbSelectArea(ob:cAlias) - не нужно
                                     nc := "ORDER: ["
                                     If (ob:cAlias)->(OrdCount()) > 0
                                        nl := (ob:cAlias)->(INDEXORD())
                                        nc += hb_ntos(nl) + " | "
                                        nc += (ob:cAlias)->(OrdName(nl)) + "]"
                                     Else
                                        nc += "0]"
                                     EndIf
                                     na := nc
                                     Return nc
                                     } */
                     // редактирование ячеек таблицы -> alert_tsb.prg
                     myAlertTsbEdit7(ob,op)
                     DO EVENTS
                     Return Nil
                     }

RETURN oTsb

