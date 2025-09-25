/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Работа с базой / Work with the base
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "dbinfo.ch"
#include "set.ch"

// Пример использования функций - Sergej Kiselev
/////////////////////////////////////////////////////////////////////
FUNCTION my_Use(cFile, cIndx, cAls, cKey, lDelete, cCdp, cRdd)
   LOCAL nAlsOld := Select()
   LOCAL lRet := .F., lIndex
   DEFAULT cIndx := "", lDelete := .T.
   DEFAULT cKey := "", cRdd := RddSetDefault()

   // задан режим удаления и заданы ключи создания индекса
   lIndex  := !Empty(cIndx) .and. !Empty(cKey)
   lDelete := lDelete .and. lIndex

   IF lDelete ; DeleteFile( cIndx )
   ENDIF

   SELECT 0 // NEW Area
   // для повторного открытия уже открытого ранее dbf !!! НЕ УБИРАТЬ !!!
   IF  Empty(cAls) ; cAls := "_XYZ_"+hb_ntos(Select())
   ELSEIF Select(cAls) > 0 ; cAls += "_"+hb_ntos(Select())
   ENDIF

   cAls := upper(alltrim(cAls))

   NetErr( .F. )  // сбросили флаг в .F.

   BEGIN SEQUENCE WITH {|e| break( e ) }
      USE ( cFile ) Alias &cAls CODEPAGE cCdp VIA cRdd SHARED
      lRet := ! NetErr() .and. Used()
   END SEQUENCE

   //? "File =", cFile
   //? "Open =", lRet, Alias(), cAls ; ?
   IF LEN(cIndx) == 0
      lIndex := .F.    // не нужен индекс
   ENDIF

   IF lRet .and. lIndex
      DO EVENTS
      IF hb_FileExists(cIndx)
         ORDLISTADD( cIndx )
      ELSE
         INDEX ON &cKey TO (cIndx) ADDITIVE
      ENDIF
      //? cIndx, INDEXORD(), OrdName(), OrdKey() ; ?
   ENDIF

   IF lRet                             // открыли
      IF OrdCount() > 0 ; OrdSetFocus(1)
      ENDIF
      dbGoTop()
   ELSEIF nAlsOld > 0                     // нет
      dbSelectArea(nAlsOld)               // вернулись где были
   ENDIF

   DO EVENTS

RETURN lRet

/////////////////////////////////////////////////////////////////////
FUNCTION my_Close( cAls, cIndx, uAlsOld )
   LOCAL nAls := 0, nAlsOld := 0, lUse := .F.
   LOCAL lDel := hb_IsChar(cIndx) .and. !Empty(cIndx)
   DEFAULT cAls := Alias(), uAlsOld := 0

   IF hb_IsNumeric (cAls) ; nAls := cAls
   ELSEIF hb_IsChar(cAls) ; nAls := Select(cAls)
   ENDIF

   IF hb_IsNumeric (uAlsOld) ; nAlsOld := uAlsOld
   ELSEIF hb_IsChar(uAlsOld) ; nAlsOld := Select(uAlsOld)
   ENDIF

   IF nAls > 0 .and. (nAls)->( Used() )
      (nAls)->( dbCloseArea() )
   ENDIF

   IF lDel .and. hb_FileExists(cIndx) ; DeleteFile(cIndx)
   ENDIF

   IF nAlsOld > 0 ; dbSelectArea(nAlsOld)
   ENDIF

   DO EVENTS

RETURN lUse

////////////////////////////////////////////////////////////////////////
FUNCTION my_Use2(cFile, cIndx, cAls, cKey, lDelete, cCdp, lShared, cRdd)
   LOCAL nAlsOld := Select()
   LOCAL lRet := .F., lIndex, aIndex
   LOCAL aInd, cTag, cBag, lUnq, cFor, bFor, bKey
   DEFAULT lDelete := .T., lShared := .T.
   DEFAULT cRdd := RddSetDefault()

   // задан режим удаления и заданы ключи создания индекса
   lIndex  := !Empty(cIndx) .and. !Empty(cKey)
   lDelete := lDelete .and. lIndex

   IF lDelete ; DeleteFile( cIndx )
   ENDIF

   SELECT 0 // NEW Area
   // для повторного открытия уже открытого ранее dbf !!! НЕ УБИРАТЬ !!!
   IF  Empty(cAls) ; cAls := "_XYZ_"+hb_ntos(Select())
   ELSEIF Select(cAls) > 0 ; cAls += "_"+hb_ntos(Select())
   ENDIF

   cAls := upper(alltrim(cAls))

   NetErr( .F. )  // сбросили флаг в .F.

   BEGIN SEQUENCE WITH {|e| break( e ) }
      //USE ( cFile ) Alias &cAls CODEPAGE cCdp VIA cRdd SHARED
      DbUseArea(.F., cRdd, cFile, cAls, lShared, .F., cCdp)
      lRet := ! NetErr() .and. Used()
   END SEQUENCE

   ? "File =", cFile
   ? "Open =", lRet, Alias(), cAls
   ?

   IF lRet .and. lIndex
      DO EVENTS
      IF hb_FileExists(cIndx)                             // подключаем индекс
         ORDLISTADD( cIndx )
      ELSEIF hb_IsArray(cKey)                             // создаем тэги
         aIndex := AClone(cKey)
         FOR EACH aInd IN aIndex
             aInd := ASize(aInd, 5)
             cTag := aInd[1]  // Tag name
             cKey := aInd[2]  // Tag Key character
             lUnq := aInd[3]  // Tag Unique
             cFor := aInd[4]  // FOR ...
             cBag := aInd[5]  // FileName index or NIL
             bKey := &( "{|| "+cKey+" }")
             IF !Empty(cFor) .and. hb_IsChar(cFor)
                bFor := &( "{|| "+cFor+" }" )
                ordCondSet( cFor, bFor )
             ENDIF
             OrdCreate( cBag, cTag, cKey, bKey, !Empty(lUnq) )
             DO EVENTS
         NEXT
      ELSE                                                // создаем индекс
         INDEX ON &cKey TO (cIndx) ADDITIVE
      ENDIF
      ? cIndx, INDEXORD(), OrdName(), OrdKey(), OrdCount()
      ?
   ENDIF

   IF lRet                             // открыли
      IF OrdCount() > 0 ; OrdSetFocus(1)
      ENDIF
      dbGoTop()
   ELSEIF nAlsOld > 0                     // нет
      dbSelectArea(nAlsOld)               // вернулись где были
   ENDIF

   DO EVENTS

RETURN lRet

*-----------------------------------------------------------------------------------------*
FUNCTION myUseArea( cFile, cAlias, lShared, cRdd, nWhl ) // Открыть таблицу или DBF
*-----------------------------------------------------------------------------------------*
   LOCAL cAls, w := 10, lRet := .F.
   Default lShared := .T., cRdd := RddSetDefault()

   IF hb_IsNumeric(nWhl) .and. nWhl > 0; w := nWhl
   ENDIF

   select 0

   IF empty(cAlias) .or. ! hb_IsChar(cAlias)
      cAls := "_XYZ_"+hb_ntos(select())
   ELSE
      cAls := upper(cAlias)
      IF select(cAlias) > 0 ; cAls += '_' +hb_ntos(select())
      ENDIF
   ENDIF

   NetErr( .F. )

   DO WHILE w-- > 0
      lRet := .F.
      BEGIN SEQUENCE WITH {|e| break( e ) }
         DbUseArea(.F., cRdd, cFile, cAls, lShared, .F.)
         lRet := !Empty( Alias() )
      END SEQUENCE
      IF lRet .AND. ! NetErr()
         lRet := Used()
         EXIT
      ENDIF
      wApi_Sleep(100)
   ENDDO

   IF lRet ; GO TOP
   ENDIF
   DO EVENTS

RETURN lRet

*-----------------------------------------------------------------------------------------*
FUNCTION myOrdArea( aIndex )
*-----------------------------------------------------------------------------------------*
   LOCAL aO, bO, lU, cI, cT, cO
   LOCAL lRet := .F., nRet := 0

   IF Empty(aIndex) .or. ! hb_IsArray(aIndex) ; RETURN lRet
   ENDIF

   FOR EACH aO IN aIndex
       DO EVENTS
       IF Len(aO) < 4 ; ASize(aO, 4)
       ENDIF
       cT := aO[1]  // Tag name
       cO := aO[2]  // Tag Key character
       lU := aO[3]  // Tag Unique
       cI := aO[4]  // FileName index
       lU := !Empty(lU)
       bO := &( "{|| "+cO+" }")
       OrdCreate( cI, cT, cO, bO, lU )
       nRet++
       DO EVENTS
   NEXT

   IF ( lRet := (nRet == Len(aIndex)) )
      IF OrdCount() > 0 ; OrdSetFocus(1)
      ENDIF
      dbGoTop()
      DO EVENTS
   ENDIF

RETURN lRet

*-----------------------------------------------------------------------------*
FUNCTION myRLock( nWhl )
*-----------------------------------------------------------------------------*
LOCAL lRet := .F.

 Default nWhl := 10

 IF ! dbInfo(DBI_SHARED); RETURN .T.
 ENDIF

 WHILE nWhl > 0
    If ( lRet := Rlock() ); EXIT
    ENDIF
    wApi_Sleep( 100 )
    nWhl -= 1
 END

RETURN lRet

*-----------------------------------------------------------------------------*
FUNCTION IsFileFree( cFile, nWhl ) // File txt or file dbf open shared ?
*-----------------------------------------------------------------------------*
   LOCAL hFile, lRet := .T.
   Default nWhl := 10

   If hb_FileExists(cFile)
      lRet := .F.
      WHILE ( nWhl-- ) > 0    // ждем\проверяем в цикле - освободился ?
         hFile := FOpen(cFile, 0)
         IF ( lRet := hFile > 0 ) ; FClose(hFile) ; EXIT
         ENDIF
         wApi_Sleep( 50 )        // Max 500 мск, можно nWhl := 20 -> 1 сек.
      END
   EndIf

RETURN lRet

/////////////////////////////////////////////////////////////////
// Возрат массива открытой базы АЛИАС и индексных файлов базы
FUNCTION myIndexOpen(cPath)
   LOCAL aMemIndex := {}, nI, cTmp
   DEFAULT cPath := ""

   FOR nI := 1 TO 900
        IF LEN(ORDNAME(nI)) == 0
           EXIT
        ELSE
           DBSetOrder(nI)
           ORDSETFOCUS(ORDBAGNAME(nI))
           cTmp  := ALLTRIM( UPPER( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) ) )
           IF cTmp == ""
              EXIT
           ELSE
              AADD(aMemIndex, { ALIAS(), cTmp, cPath } )
           ENDIF
        ENDIF
   NEXT

RETURN aMemIndex

/////////////////////////////////////////////////////////////////
// Возрат массива открытой базы АЛИАС и индексных файлов базы
FUNCTION myIndexRestore(aDim,lSay)
   LOCAL nI, cAlias, cIndex
   DEFAULT aDim := {}, lSay := .T.

   IF LEN(aDim) == 0
      IF lSay
         AlertStop("There are no open indexes for the database !;Current alias: "+ALIAS()+" !")
      ENDIF
   ELSE
      FOR nI := 1 TO LEN(aDim)   // Восстановить открытые индексы
         cAlias := aDim[nI,1]
         cIndex := aDim[nI,2]
         DBSELECTAREA(cAlias)
         IF LEN( ALLTRIM( cIndex ) ) > 0
            ORDLISTADD( cIndex )
         ENDIF
         DBSetOrder(nI)
      NEXT
   ENDIF

RETURN NIL

