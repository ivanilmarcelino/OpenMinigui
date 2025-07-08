/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Создание массива для таблицы
 * _TBrowse() Create array for table
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////
FUNCTION ArrayLoadDim()
   LOCAL aDim := {}, nI, nK, aVal, aName, aHead, cErr, aSpr, aFld, cSay
   LOCAL oVipZa, oSrkZa, oTipVid, oProper, cMsg, oTipVd, aSFld

   //                1            2               3                  4                     5                6                  7                  8                   9           10           11             12              13           14           15
   aName := { "RNAME"        , "REDIT"      , "F_NN"           ,  "F_PROCES"          , "F_BASE"    , "F_READ"          , "F_WRITE"         , "F_WINDOWS"      , "F_ACCESS"  , "V_CALC"    , "V_SPR"    ,  "V_AADD"      , "F_CALC"  , "F_SPR"     ,  "F_AADD"    }
   aHead := { "Наименование" , "Значение"   , "Порядок;показа" , "Функция;обработки"  , "Поле;базы" , "Функция;чтения"  , "Функция;записи"  , "Функция;окна"   , "Write;Read", "Показ CALC", "Показ;SPR", "Показ;доп.дан", "Для CALC", "Данные;SPR", "Доп.данные" }
   aHead := { "Name"         , "Value"      , "Order;show"     , "Processing;function", "Base;field", "Reading;function", "Writing;function", "Window;function", "Write;Read", "Show;CALC" , "Show;SPR" , "Show;add.data", "For CALC", "SPR;data"  , "Add.data"   }
   // (1)  - колонка показа
   // (2)  - колонка показа, правка значения
   // ----- далее колонки скрыть ---------------
   // (3)  - порядок показа строк в таблице
   // (4)  - тип обработки ячеек таблицы
   // (5)  - поле базы в dbf
   // (6)  - функция чтение поля (4) и запись в колонку (2)
   // (7)  - функция записи в поле (4) из колонки (2) или (8)-где NIL
   // (8)  - функция для окна редактирования переменных для типа CALC,SPR_A,SPR_J,SPR_S
   // (9)  - доступ редактирования ячеек Write/Read
   // (10) - преобразование в "C" колонки (12)
   // (11) - преобразование в "C" колонки (13)
   // (12) - преобразование в "C" колонки (14)
   // (13) - значение исправленного поля {} для типа CALC,SPR_A,SPR_J,SPR_S из (3), в остальных случаях NIL
   // (14) - доп.данные для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   // (15) - доп.данные разные
   // (1) - display column
   // (2) - display column, value editing
   // -------- hide further columns ---------------------
   // (3) - the order of displaying rows in the table
   // (4)  - table cell processing type
   // (5) - base field in dbf
   // (6) - function reading field (4) and writing to column (2)
   // (7) - function writing to field (4) from column (2) or (8) - where NIL
   // (8) - function for variable editing window for type CALC,SPR_A,SPR_J,SPR_S
   // (9) - Write/Read cell editing access
   // (10)- conversion to "C" of column (12)
   // (11) - conversion to "C" of column (13)
   // (12) - conversion to "C" of column (14)
   // (13) - value of corrected field {} for type CALC,SPR_A,SPR_J,SPR_S from (3), in other cases NIL
   // (14) - additional data for type (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   // (15) - additional data different
   //                      1                         2            3     4        5               6                 7              8                9   10  11   12  13  14   15
   AADD( aDim, {"Variable checks"                  , ""        ,  1, "LINE1" , ""         , ""               , ""              , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type C field test"             , SPACE(20) ,  2, "C"     , "FIO"      , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type L field test"             , .F.       ,  3, "L"     , "lPrint"   , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type M (memo) field test"      , SPACE(90) ,  4, "M"     , "MREM"     , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type N"          , 0         ,  5, "N"     , "NNZa"     , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type D"          , CTOD("")  ,  6, "D"     , "DateDog"  , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type D calendar" , CTOD("")  ,  7, "DMN"   , "DatePlan" , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field of type T"       , ""        ,  8, "DT"    , "IMZ"      , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"Checks of composite variables"    , ""        ,  9, "LINE2" , ""         , ""               , ""              , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Subscriber's apartment N + C"  , SPACE(20) , 10, "A"     , {"NKvar","CKvar"}, ""         , ""              , ""             , "W", "", "", "", nil, {"@Z 99999","xxxxx", "/" } , {"Number:","Letter:"} } )
   AADD( aDim, {"   Phone (home/mobile/work)"      , SPACE(50) , 11, "A"     , {"TelFIO","TelFIO3","TelFIO2"}, ""    , ""      , ""             , "W", "", "", "", nil, {"@R (999) 999-99-99","@R (999) 999-99-99","@R (999) 999-99-99", " , "} , "" } )
   AADD( aDim, {"   Select from array - type SPR_A", SPACE(20) , 12, "SPR_A" , "KDispJil" , "GetDim2Str()"   , ""              , ""             , "W", "", "", "", nil, {"Tenant","Dispatch","Rostelecom","ODS (obshch.disp.service)","City 77"}, "iVOTelefon48" } ) // 15-icon
   AADD( aDim, {"   Select from array in this line", SPACE(20) , 13, "SPR_A" , "KOnline"  , "GetDim2Str()"   , ""              , ""             , "W", "", "", "", nil, {"Portal" ,"other", "https://www.hmgextended.com/" } , "iUser32"} ) // 15-icon

   // заполняется для вложенного справочника "CALC", для других не надо
   oTipVd := oHmgData()
   oTipVd:aType := { "S"       , "S"        }
   oTipVd:aName := { "Bid type", "Bid kind" }
   oTipVd:aTFor := { {"tipza","Ktipza","tipza",2,""} , {"Works","KWorks","Works",2,""} }
   oTipVd:aFld  := {"Ktipza","KWorks"}     // поля записи в базу
   oTipVd:aBClr := GREEN   // цвет внутри
   oTipVd:aFClr := BLUE                                                                     // V--- кол-во = oTipVid:aFld                    //  9   10  11   12  13       14        15
   AADD( aDim, {"   Bid: type / kind"              , SPACE(20) , 13, "CALC"  , ""         , "Za_TipVid2()"   , "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oTipVd:aFld, oTipVd } )

   // заполняется для вложенного справочника "CALC", для других не надо
   oTipVid := oHmgData()
   oTipVid:aType := { "S"               , "S"         , "D"                   , "N"             , "C"       , "A"                , "M"          }
   oTipVid:aName := { "Application type", "Condition" , "Application deadline", "Payment amount", "Comment" , "Print application", "Memo-field" }
   oTipVid:aTFor := { {"tipza","Ktipza","tipza",2} , {"Works","KWorks","Works",2} , {"D"}, {"N"}, {"C"} , {"Master","Manager","Warehouse"}, {"M"} }
   oTipVid:aFld  := {"Ktipza","KWorks","DatePere","SumPlata","Dispetch","KAdres1" ,"mAktVip"}   // поля записи в базу
   oTipVid:aIcon := {""      ,""      ,""        ,""        ,""        ,"iPrint32", ""      }   // иконки в контекстное меню
   oTipVid:aBClr := GREEN   // цвет внутри
   oTipVid:aFClr := BLUE                                                                    // V--- кол-во = oTipVid:aFld                //  8    9  10  11   12    13         14     15
   AADD( aDim, {"   Application Type/Miscellaneous" , SPACE(20) , 14, "CALC"  , ""         , "Za_TipVid()"    , "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oTipVid:aFld, oTipVid } )
   // ---------------------
   // заполняется для вложенного справочника "CALC", для других не надо
   oSrkZa := oHmgData()
   oSrkZa:aName := {"Type of urgency", "Deadline" }
   oSrkZa:aType := {"S","D"}
   oSrkZa:aTFor := { {"srokza","Ksrokza","srokza",2} , {"D"} }
   oSrkZa:aCod1 := { 1, 2, 3, 4, 7, 5, 6, 10, 20, 30 }
   oSrkZa:aVal1 := { "1-day", "2-days", "3-days", "4-days", "7-days", "out of turn (4 hours)", "current day (until 17:30)", "repair (10-days)", "repair (20-days)", "end of current month" }
   oSrkZa:aXArr := { {oSrkZa:aCod1,oSrkZa:aVal1} , {}  }        // все значения из одной Dbf + дата
   oSrkZa:aBClr := BLUE      // цвет внутри
   oSrkZa:aFClr := MAROON    // PINK  PURPLE
   oSrkZa:aFld  := {"Ksrokza","DateSrok" }  // поля записи в базу                                                                         //     9  10  11   12  13        14        15
   AADD( aDim, {"   (*) Request urgency type"       , SPACE(20) , 15, "CALC"  , ""         , "Za_Srokza()"  , "SetDim2Wrt()" , "myWinCalc2()" , "W", "", "", "", nil, oSrkZa:aFld , oSrkZa } )
   // ---------------------
   AADD( aDim, {"   Request overdue"                , SPACE(20) , 16, "CALC"  , ""         , "Za_ProSrok()" , ""             , ""             , "R", "", "", "", nil, {} , nil } )
   AADD( aDim, {"   (*) Electronics Master (dbf)"   , SPACE(20) , 17, "SPR_S" , "KMaster"  , "Spr_1Dbf()"   , ""             , "Spr_2Dbf()"   , "W", "", "", "", nil, {"Master","KMaster","Master",2,"KFIRMA==1"}, "iMaster48" } ) // 15-icon
   AADD( aDim, {"   (*) Gate Master (dbf)"          , SPACE(20) , 18, "SPR_S" , "KMaster0" , "Spr_1Dbf()"   , ""             , "Spr_2Dbf()"   , "W", "", "", "", nil, {"Master","KMaster","Master",2,"KFIRMA==2"}, "iMaster48" } ) // 15-icon

   oProper := oHmgData()
   oProper:aType := { "A"          , "A"            , "A"                , "A"         , "A"               , "A"             }
   oProper:aName := { "Type of repair", "Type of installation", "Disconnecting the handset", "Technical inspection", "Pre-dial", "Print request" }
   oProper:aTFor := { {"entrance","apartment"} , {"regular", "at the company's expense"} , {"disable","enable"} , {" yes","no"} , {" yes","no"} , {" yes","no"} }
   oProper:aFld  := {"KZ_PKV","KTipUst","KodTruba","KTexOsm","KZvonok","KPrint"}                  // поля записи в базу
   oProper:aIcon := {"iMaster48","iVOTelefon48","iUser32","iMg_Ok48x2","iMg_Ok48x1","iPrint32"}   // иконки в контекстное меню
   oProper:aBClr := GREEN   // цвет внутри
   oProper:aFClr := BLUE                                                                                                                     //   9   10  11  12  13         14        15
   AADD( aDim, {"   Application properties"         , SPACE(20) , 20, "CALC"  , ""         ,"Zaiv_PropDbf()", "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oProper:aFld, oProper } )
   // ---------------------------------------------------------------
   oVipZa := oHmgData()
   oVipZa:aName := {"Type of execution", "Date of execution", "Time of execution", "Submit to site"}
   oVipZa:aType := {"S","D","N","N" }
   oVipZa:aPict := { Repl("x", 22), "@D", "@Z 99:99", "@Z 999" }
   oVipZa:aTFor := { {"VipZa","KVipZa","VipZa",2,""} , {}, {}, {} }
   oVipZa:aBClr := BLUE      // цвет внутри
   oVipZa:aFClr := MAROON    // PINK  PURPLE
   oVipZa:aFld  := {"Ksrokza", "DATEVip", "TimeVip", "INET" }  // поля записи в базу - "INET" Флаг для передачи на сайт
   AADD( aDim, {"   (*) Request execution type"     , SPACE(20) , 21, "CALC"  , ""         , "Za_VipZa()"     , "SetDim2Wrt()"  , "myWinCalc3()" , "W", "", "", "", nil, oVipZa:aFld , oVipZa  } )
   // ---------------------
   AADD( aDim, {"Example of a database card:"       , ""        , 22, "LINE1" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"Bid: number, date, time"           , ""        , 23, ""      , ""         , "GetNumZa()"   , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"1.1.The lock is faulty"            , .F.       , 24, "L"     , "Nele6"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Adjusting the door closer"     , .F.       , 25, "L"     , "Nele7"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Magnet does not hold the door" , .F.       , 26, "L"     , "Nele10"   , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"2.1.Panel - E2"                    , .F.       , 27, "L"     , "Nele1"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Panel - Error"                 , .F.       , 28, "L"     , "Nele2"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Panel - Other"                 , .F.       , 29, "L"     , "Nele9"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"3.1.Intercom: not working"         , .F.       , 30, "L"     , "Nele8"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Intercom: other"               , SPACE(32) , 31, "C"     , "NeleRem3" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"4.1.Keys: do not open the entrance", .F.       , 32, "L"     , "Nele3"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Keys: recoding"                , .F.       , 33, "L"     , "Nele5"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Keys: order (pieces)"          , SPACE(10) , 34, "C"     , "cNele4"   , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"5.Codes do not open the entrance"  , .F.       , 35, "L"     , "Nele4"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"6.1.Subscriber device: not working", .F.       , 36, "L"     , "NeleAbo3" , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Subscriber device: entrance"   , SPACE(23) , 37, "C"     , "NeleAbo2" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Subscriber device: apartment"  , SPACE(23) , 38, "C"     , "NeleAbo1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  4.Subscriber device: other"      , SPACE(23) , 39, "C"     , "NeleAbo"  , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"7.Additional tube installation:"   , SPACE(33) , 40, "C"     , "NeleDou1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"8.Create code for apartment"       , SPACE(4)  , 41, "C"     , "cNele11"  , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"9.Cost"                            , SPACE(22) , 42, "C"     , "NeleDou3" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"10.1.Other"                        , SPACE(36) , 43, "C"     , "NeleRem1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   2.Other"                        , SPACE(36) , 44, "C"     , "NeleRem2" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   3.Other"                        , SPACE(36) , 45, "C"     , "NeleRem4" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   // выбор из справочника
   aSpr := {"Sprav","KNNEISPR","NNEISPR",2,"KVIEW==1"}
   aFld := {"KDfc1","KDfc2","KDfc3","KDfc4","KDfc5","KDfc6","KDfc7","KDfc8","KDfc9","KDfc10" }  // поля записи в базу
   AADD( aDim, {"Table-4: List of faults"           , ""        , 46, "LINE1" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""   } )
   AADD( aDim, {"Troubleshooting guide"             , SPACE(60) , 47, "CALC"  , "aDefect"  , "ZaivSay()"    , "ZaWrtDfc()"   , "ZaListNeis()" , "W", "", "", "", nil, aFld, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 48, "K"     , "KDfc1"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 49, "K"     , "KDfc2"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 50, "K"     , "KDfc3"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 51, "K"     , "KDfc4"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 52, "K"     , "KDfc5"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 53, "K"     , "KDfc6"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 54, "K"     , "KDfc7"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 55, "K"     , "KDfc8"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 56, "K"     , "KDfc9"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 57, "K"     , "KDfc10"   , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {"Malfunctions note"                 , SPACE(90) , 58, "M"     , "MDefect"  , ""             , ""             , ""             , "W", "", "", "", nil, {}  , ""   } )
   AADD( aDim, {"Table-5: Equipment list"           , ""        , 59, "LINE2" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""   } )
   // ---------------------
   // "MKob4or"  - [2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;]  // поле записи в базу кодов оборудования из старой программы
   aSFld := { {"SumVsego","SumWObor","SumMaster"} , { 0,0,0} }                                                                              //   9   10  11  12  13    14    15
   AADD( aDim, {"(*) Works completed/Equipment"     , SPACE(60) , 60, "CALC"  , "MOb4orud" , "ZA_MOb4Err()" , "Set_ZAMOb4()" , "Tovar_HMG()"  , "W", "", "", "", nil, aSFld, ""   } )
   AADD( aDim, {""                                  , ""        , 61, ""      , ""         , "myMemo['MOb4orud',99,2]", ""   , ""             , "R", "", "", "", nil, {}   , ""   } )
   AADD( aDim, {""                                  , ""        , 62, ""      , ""         , "myMemo['MOb4orud',99,3]", ""   , ""             , "R", "", "", "", nil, {}   , ""   } )
   // ---------------------
#ifdef KEY_ENG // for this project demo1-en.hbp
   cSay := "  ** List apartments (checkerboard)"
#else
   cSay := "  ** Список квартир (шахматка)"
#endif
   aFld := {"NAKTVIP","MAKTVIP"}  // {"C","M"} поля записи в базу / database entry fields
   AADD( aDim, {"Table-6: Launching functions"      , ""        , 63, "LINE2" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""             } )
   AADD( aDim, {cSay                                , SPACE(20) , 64, "CALC"  , "MAKTVIP"  , "Get_Fld64()"  , "Set_Fld64()"  , "Win_Fld64()"  , "W", "", "", "", nil, aFld, "Run_Line64()" } )

   cErr := ""
   FOR nI := 1 TO LEN(aDim)
      aVal := aDim[nI]
      nK   := LEN(aVal)
      IF nK # 15
         cErr += HB_NtoS(nI) + ") LEN()="
         cErr += HB_NtoS(LEN(aVal))
         cErr += " aDim=" + HB_ValToExp(aVal)
         cErr += CRLF
      ENDIF
   NEXT
   IF LEN(cErr) > 0
      cMsg := "Ошибка при создании массива !" + CRLF
      cMsg += cErr + CRLF + CRLF
      cMsg += ProcNL() + CRLF + ProcNL(1)
      AlertStop(cMsg, "ERROR!", , 64, {RED})
      ? cMsg
   ENDIF
   // приводим все строки массива к 14 элементам и ошибок не будет
   FOR nI := 1 TO LEN(aDim)
       IF Len(aDim[nI]) < 11 ; ASize(aDim[nI], 11)
       ENDIF
       IF Len(aDim[nI]) < 12 ; ASize(aDim[nI], 12)
       ENDIF
       IF Len(aDim[nI]) < 13 ; ASize(aDim[nI], 13)
       ENDIF
       IF Len(aDim[nI]) < 14 ; ASize(aDim[nI], 14)
       ENDIF
       IF Len(aDim[nI]) < 15 ; ASize(aDim[nI], 15)
       ENDIF
   NEXT

RETURN { aDim, aName, aHead }
