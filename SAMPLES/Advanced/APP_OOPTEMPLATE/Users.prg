/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/
#include "minigui.ch"
#include "Dbinfo.ch"

/////////////////////////////////////////////////////////////////////////
FUNCTION IsDbfUsers()
   LOCAL nI, aDbf, cDbf, cPath, cStr, cAls, lShared, bOldError

   cDbf  := "Users.dbf"
   cPath := App.Cargo:cPathDbf
   aDbf  := {}
   AADD( aDbf , {"NN"       ,"N",  4, 0 } )
   AADD( aDbf , {"USER"     ,"C", 30, 0 } )
   AADD( aDbf , {"KUSER"    ,"N", 10, 0 } )
   AADD( aDbf , {"FTYPE"    ,"C",  2, 0 } )
   AADD( aDbf , {"DELET"    ,"L",  1, 0 } )
   AADD( aDbf , {"VISIB"    ,"L",  1, 0 } )
   AADD( aDbf , {"TITLE"    ,"C", 90, 0 } )
   AADD( aDbf , {"DT_MODIFY","T",  8, 0 } )

   IF !FILE(cPath + cDbf)
      bOldError := ERRORBLOCK( {|var| BREAK(var)} )
      BEGIN SEQUENCE
        DbCreate( cPath + cDbf, aDbf , "DBFCDX", .T. )
        FOR nI := 1 TO 50
            APPEND BLANK
        NEXT
      RECOVER
         cStr := "Database: ;" + cPath + cDbf + ";"
         cStr += "occupied by another process (operator) !"
         AlertStop( cStr )
      END SEQUENCE
      // Возвратиться к блоку обработки ошибок по умолчанию
      ERRORBLOCK( bOldError )
   ELSE
      cAls    := "TEMP"
      lShared := .T.
      IF myUseArea( cPath + cDbf, cAls, lShared )
         GOTO TOP
         DO WHILE !EOF()
           IF (cAls)->(RLock())
              (cAls)->NN    := 0
              (cAls)->USER  := ""
              (cAls)->KUSER := 0
              (cAls)->FTYPE := ""
              (cAls)->TITLE := ""
              (cAls)->DT_MODIFY := hb_DateTime()  // когда изменили запись
              (cAls)->(DBUnlock())
            ENDIF
            SKIP
         ENDDO
         (cAls)->(DbCommit())
         (cAls)->(dbCloseArea())
      ENDIF
   ENDIF

Return Nil

///////////////////////////////////////////////////////////
// Открыть таблицу или DBF - 03.08.23
// Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
STATIC FUNCTION myUseArea( cDbf, cAls, lShared, cRdd, cCdp, nWhl )
   LOCAL lRet := .T., cPth, cFil, cExt
   Default cAls := ""
   Default lShared := .T.
   Default cRdd := RddSetDefault()
   Default cCdp := "RU866"
   DEFAULT nWhl := 10

   If !hb_FileExists(cDbf)
      AlertStop("Нет файла базы !;" + cDbf + ";;" + ProcNL() + ";" + ProcNL(1) )
      RETURN .F.
   ENDIF

   NetErr( .F. )

   hb_FNameSplit(cDbf, @cPth, @cFil, @cExt)
   cDbf := hb_FNameMerge(cPth, cFil, "")

   IF LEN(cAls) == 0
      cAls := cFileNoPath(cDbf)
      cAls := SUBSTR(cAls,1,AT(".",cAls)-1)
   ENDIF

   IF SELECT(cAls) > 0
      AlertStop("Алиас такой уже есть !;" + ;
                 "ALIAS()=" + cAls + ";"  + ;
                 cDbf + ";;" + ProcNL() + ";" + ProcNL(1) )
      RETURN .F.
   ENDIF

   SELECT 0
   If     empty (cAls)    ; cAls := '_XYZ_'+hb_ntos(select())
   ElseIf select(cAls) > 0; cAls += '_'    +hb_ntos(select())
   EndIf
   //!!!
   DO WHILE nWhl-- > 0
      lRet := .F.
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
         lRet := ! NetErr() .and. Used()
      END SEQUENCE
      IF lRet; EXIT
      ENDIF
      wApi_Sleep(100)
   ENDDO
   // ^^^ - цикл главное, т.к. из за сбоя сети пробуем несколько раз открыть
   IF lRet
      dbGoTop()
   ENDIF
   ? ProcNL(), "Открытие БД " + cDbf, "Кол-во попыток (10--) =", nWhl

RETURN lRet

//////////////////////////////////////////////////////////////////////////
FUNCTION DimGetForms()
   LOCAL nI, cForm, aFrm, aList := {}, cMsg := ""

   aFrm := HMG_GetForms()
   cMsg += "Number of open windows: " + HB_NtoS(LEN(aFrm)) + CRLF

   FOR nI := 1 TO LEN(aFrm)
      cForm := aFrm[nI]
      cMsg  += SPACE(5) + HB_NtoS(nI) + ") "
      cMsg  += cForm + ',  Type: "'+_HMG_aFormType[nI]+'" '
      cMsg  += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cMsg  += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cMsg  += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cMsg  += ', Title: ' + GetProperty( cForm, "Title" ) + CRLF
      AADD(aList, { cForm, _HMG_aFormType[nI], _HMG_aFormHandles[nI], _HMG_aFormDeleted[nI] ,;
                    IsWindowVisible( GetFormHandle( cForm ) ) ,;
                    GetProperty( cForm, "Title" ) } )
   NEXT

RETURN aList

/////////////////////////////////////////////////////////////////////////
FUNCTION LogUsers(cLen)
   LOCAL nI, cDbf, cPath, cAls, lShared, cRet, aForm, aUser

   // Form: WMAININIT, Type: "A" , Handle: 1575198, Deleted: F, Visible: F
   //         , Title: Template of the finished program on MiniGui
   cLen    := "0"
   aForm   := DimGetForms()
   aUser   := {}
   cRet    := "(???)"
   cDbf    := "Users.dbf"
   cPath   := App.Cargo:cPathDbf
   cAls    := "USERS"
   lShared := .T.
   IF myUseArea( cPath + cDbf, cAls, lShared )
      nI := 1
      DBSELECTAREA(cAls)
      GOTO TOP
      DO WHILE !EOF()
         IF LEN(ALLTRIM((cAls)->USER)) == 0
            IF (cAls)->(RLock())
               (cAls)->NN    := RECNO()
               (cAls)->USER  := aForm[nI,1]
               (cAls)->KUSER := aForm[nI,3]
               (cAls)->FTYPE := aForm[nI,2]
               (cAls)->DELET := aForm[nI,4]
               (cAls)->VISIB := aForm[nI,5]
               (cAls)->TITLE := aForm[nI,6]
               (cAls)->DT_MODIFY := hb_DateTime()  // когда изменили запись
               (cAls)->(DBUnlock())
                nI++
            ENDIF
            IF nI > LEN(aForm)
               EXIT
            ENDIF
         ENDIF
         SKIP
      ENDDO
      (cAls)->(DbCommit())

      GOTO TOP
      DO WHILE !EOF()
         IF LEN(ALLTRIM((cAls)->USER)) == 0
            EXIT
         ELSE
            AADD(aUser, ALLTRIM((cAls)->USER))
         ENDIF
         SKIP
      ENDDO
      (cAls)->(dbCloseArea())
   ENDIF

   IF LEN(aUser) > 0
      cLen := HB_NtoS(LEN(aUser))
      cRet := HB_ValToExp(aUser)
      cRet := CHARREM('{}', cRet)
      cRet := ALLTRIM(cRet)
   ENDIF

RETURN cRet
