/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с меню Таблицы / Working with the Tables menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "dbinfo.ch"
/////////////////////////////////////////////////////////////////////////////
FUNCTION Menu3Table(ow,ky,cn)
   LOCAL aMenu, nBmpSize, nFSize, nChoice, nPos, lExit, nI, aBtnObj
   LOCAL xRet, cForm, aFntExt, a2Table, cLang, cLang2, cIcon, aBtn3
   LOCAL nLen, cMsg, nY, nX, cIndx, aTable, cFile

   ? ProcNL(), ow:Name,ky,cn
   cForm := ow:Name
   cFile := ow:Cargo:cFile
   cIcon := "iSqlite32"

   // список таблиц из файла Sqlite
   aTable  := ow:Cargo:aTable  // { "Таблица 1" , "Таблица 2" , "Таблица 3" , "Таблица 4" }
   a2Table := ow:Cargo:a2Table
   IF LEN(aTable) == 0
      cMsg := IIF( App.Cargo:cLang == "RU", "Нет таблиц в этом файле !",;
                      "There are no tables in this file !")
      cMsg += ";" + cFile
      AlertStop( cMsg, "Error", , 64, {RED} )
      RETURN .F.
   ENDIF

   cLang   := IIF( App.Cargo:cLang == "RU", "Открыть все таблицы", "Open all tables" )
   cLang2  := IIF( App.Cargo:cLang == "RU", "Закрыть все таблицы", "Close all tables")
   cMsg    := IIF( App.Cargo:cLang == "RU", "Таблица: ", "Tables: ")

   aMenu  := {}
   FOR nI := 1 TO LEN(a2Table)
      cIndx := IIF( LEN(a2Table[nI,2]) > 0, "  (" + a2Table[nI,2] + ")" , "" )
      AADD( aMenu, { cIcon, cMsg + a2Table[nI,1] + cIndx , .F.  , "msgDebug"  , "Str"+HB_NtoS(nI) , nI } )
   NEXT
   AADD( aMenu, {                                                       } )
   AADD( aMenu, { "iWinOpen32" , cLang  , .F.  , "msgDebug"  , "Str999" , 999  } )
   AADD( aMenu, { "iWinClose32", cLang2 , .F.  , "msgDebug"  , "Str998" , 998  } )

   nLen     := LEN(aMenu)
   ?? "LEN(aMenu)=", nLen
   xRet     := "-"
   nPos     := 3
   nBmpSize := IIF( nLen <= 25,32, IIF(nLen > 35, 16, 24) )
   nFSize   := IIF( nLen <= 25,16, IIF(nLen > 35, 10, 12) )
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   IF !IsString(cn)
      cn := "_Table"
   ENDIF
   aBtnObj  := ow:Cargo:aBtnObj     // массив кнопок на форме
   aBtn3    := aBtnObj[3]
   ? SPACE(3) + HB_ValToExp(aBtn3)
   //  1      2          3             4      5   6   7   8      9         10
   //{ 3, "_Table", "-имя объекта", "Tables", 0, 174, 77, 69, "_Table", "-событие"}
   nY       := ow:Row + aBtn3[5] + aBtn3[8]
   nX       := ow:Col + aBtn3[6] + 5
   nPos     := {nY, nX}

   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos  := aMenu[nChoice,6]
      IF nPos == 999
        xRet  := OpenTableAll(ow,cIcon)
      ELSEIF nPos == 998
        cIcon := aMenu[nChoice,1]
        xRet  := CloseTableAll(ow,cIcon)
      ELSE
        xRet := OpenTableOne(ow,nPos,aMenu[nChoice],aTable[nChoice])
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION OpenTableOne(ow,nPos, aMenu, cTableName)
    LOCAL cIco, cTtl, lWin1, cForm

    cTtl  := aMenu[2]
    cIco  := aMenu[1]
    lWin1 := .T.       // одно окно и остаться на окне
    cForm := Tsb_ViewSqlite(ow, nPos, cIco, cTableName, lWin1)  // -> tsb_ViewSqlite.prg

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION OpenTableAll(ow,cIcon)
   LOCAL aTable, nI, cTable, lWin1, cForm, aForm, hForm

   aTable := ow:Cargo:aTable    // { "Таблица 1" , "Таблица 2" , "Таблица 3" , "Таблица 4" }
   lWin1  := .F.                // все окна
   aForm  := {}

   FOR nI := 1 TO LEN(aTable)
      cTable := aTable[nI]
      cForm  := Tsb_ViewSqlite(ow, nI, cIcon, cTable, lWin1)  // -> tsb_ViewAccess.prg
      IF LEN(cForm) > 0
         AADD( aForm , cForm )
      ENDIF
   NEXT

   IF LEN(aForm) > 0
      // активировать все окна
      _ActivateWindow( aForm, .F., , )
   ENDIF

   hForm := nI := cForm
   /*
   IF LEN(aForm) > 0
      FOR nI := 1 TO LEN(aForm)
         cForm := aForm[nI]
         ? ProcNL(), nI, cForm
         hForm := GetFormHandle(cForm)
         IF hForm != 0
            IF IsIconic( hForm ) ; _Restore( hForm )
            ENDIF
            DoMethod(cForm, "SetFocus")
         ENDIF
         DO EVENTS
      NEXT
   ENDIF */

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION CloseTableAll(ow,cIcon)

   ? ProcNL(), ow,cIcon

   //_wPost(99, "wMain")
   //_wPost(91, "wMain")
   //_wPost(92, "wMain")
   //_wPost(93, "wMain")
   _wPost(94, "wMain")

RETURN NIL
