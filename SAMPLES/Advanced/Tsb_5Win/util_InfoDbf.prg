/*
 * Copyright 2015-2018, Verchenko Andrey <verchenkoag@gmail.com>
 * Tips and tricks programmers from our forum http://clipper.borda.ru
*/

#include "minigui.ch"
#include "Dbinfo.ch"

/*POPUP 'DBASE!' FONT cFontNorm
   ITEM 'Текущая база'           FONT cFontNorm ACTION Base_Tek()
   ITEM 'Set relation этой базы' FONT cFontNorm ACTION MsgDebug( Base_Relation( ALIAS() )
   ITEM 'Список открытых БД'     FONT cFontNorm ACTION MyGetAllUse()
END POPUP */

//////////////////////////////////////////////////////////////////////////////
// Список открытых БД / List of open databases
FUNCTION myGetAllUse()
   LOCAL nI, cMsg := "", aAlias := {}, aSelect := {}, aRdd := {}

   hb_waEval( {|| AADD(aAlias, Alias())} )
   hb_waEval( {|| AADD(aSelect, Select())} )
   hb_waEval( {|| AADD(aRdd, RddName())} )

   FOR nI := 1 TO LEN(aAlias)
       cMsg += "select: " + HB_NtoS(aSelect[nI])
       cMsg += ",  alias: " + aAlias[nI] + " ,  RddName: " + aRdd[nI] + CRLF
   NEXT

   //MsgInfo( cMsg, "Open databases" )
   MG_Exclam( cMsg, "Open databases", "iDebug72", 72 )

   RETURN NIL

//////////////////////////////////////////////////////////////////////////////
// Текущая база / Current base
FUNCTION BASE_TEK(cPar)
   LOCAL nI, nSel, nOrder, cAlias, cIndx, aIndx := {}
   LOCAL cMsg := "Calling from:: " + ProcName( 1 ) + "(" + hb_ntos( ProcLine( 1 ) ) + ") --> " + ProcFile( 1 ) + CRLF + CRLF
   DEFAULT cPar := ""

   cAlias := ALIAS()
   nSel   := SELECT(cAlias)
   IF nSel == 0
      cMsg := "       No open BASE !" + CRLF + CRLF + cMsg
      MsgStop( cMsg, "Open databases" )
      RETURN NIL
   ENDIF

   nOrder := INDEXORD()  // Результат: NUMBA
   cMsg := "Open Database - alias: " + Alias() + "    RddName: " + RddName() + CRLF
   cMsg += "Path to the database - " + DBINFO(DBI_FULLPATH) + CRLF + CRLF
   cMsg += "Open indexes: "

   IF nOrder == 0
      cMsg += " (no) !" + CRLF + CRLF
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' )' + CRLF + CRLF
      FOR nI := 1 TO 100
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += STR(nI,3) + ') - Index file: ' + DBORDERINFO(DBOI_FULLPATH) + CRLF
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ")" + CRLF
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '"' + CRLF
            cMsg += '       FOR index: "' + OrdFor() + '" ' + SPACE(5)
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT  )) + ' )' + CRLF + CRLF
            AADD( aIndx, STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) // переключить на основной индекс
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS()
   ENDIF
   cMsg += "          Number of records = " + HB_NtoS(ORDKEYCOUNT()) + CRLF

   IF cPar == "STRING"
   ELSEIF cPar == "STRING2"
      cMsg := Alias() + ",  Open indexes: " + HB_NtoS(LEN(aIndx)) + ",  Current index = "+HB_NtoS(nOrder)
   ELSE
      //MsgInfo( cMsg, "Open databases" )
      MG_Exclam( cMsg, "Open databases", "iDebug72", 72 )
   ENDIF

RETURN cMsg

//////////////////////////////////////////////////////////////////////////////
// Set relation этой базы
FUNCTION Base_Relation( cAls )
   LOCAL aDim := {}, nR, aVal, cMsg

   cMsg := "Open Database - alias: " + Alias() + CRLF

   DBSELECTAREA(cAls)
   FOR nR := 1 TO 130
      aVal := Relation( nR )
      IF LEN(aVal[1]) > 0
         AADD(aDim, aVal)
         cMsg += hb_ValToExp(aVal) + CRLF
      ENDIF
      IF LEN(aVal[1]) == 0
         EXIT
      ENDIF
   NEXT
   IF LEN(aDim) == 0
      AADD(aDim, {} )
      cMsg += "No Set relation !" + CRLF
   ENDIF

RETURN cMsg

////////////////////////////////////////////////////////////////
STATIC FUNCTION Relation( nRelation )
RETURN { DBRELATION(nRelation), ALIAS(DBRSELECT(nRelation)) }

////////////////////////////////////////////////////////////////
FUNCTION Ukr_Check(cAls)
   LOCAL aName, aCode, nI, nVal, cVal

   IF cAls == "STREET_UA"
      aName := {"Невідомо", "Київ","Борисполь","Українка","Ірпінь","Бориспіль","Обухів","Буча","Гостоміль","Васильків","Вишневе","Нові Пітрівці","Бортничи","Вишгород","Бровари","Moskow","Київ" }
      aCode := {4           , 3     ,5          , 6        , 7      , 8         , 9      , 10   , 11        , 12        , 13      , 14            , 15       , 16       , 20      , 1      , 0     }
      GOTO TOP
      DO WHILE !EOF()
         nVal := (cAls)->KCity
         cVal := ""
         FOR nI := 1 TO LEN(aName)
            IF aCode[nI] == nVal
               cVal := aName[nI]
               EXIT
            ENDIF
         NEXT
         IF LEN(cVal) > 0
            IF (cAls)->(RLock())
                  (cAls)->City := cVal
               (cAls)->(DBUnlock())
            ENDIF
            (cAls)->(DbCommit())
         ENDIF
         SKIP
      ENDDO
   ENDIF

RETURN NIL
