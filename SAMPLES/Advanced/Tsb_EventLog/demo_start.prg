/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Работа с базами данных / Working with databases
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "dbinfo.ch"

///////////////////////////////////////////////////////////////////////////////
FUNCTION Set_Start_Init()
   LOCAL cMsg, oac := App.Cargo, cDir := oac:cPathDbf

   SET AUTOPEN   ON  // !!! всегда так / always like this

   oac:nUser := hb_RandomInt( 10 )             // program user start
   oac:cUser := "User_" + HB_NtoS(oac:nUser)

   PUBLIC nOperat, cOperator
   M->nOperat   := oac:nUser
   M->cOperator := oac:cUser

   cDir := SUBSTR(cDir,1,RAT("\",cDir)-1)
   IF !ISDIRECTORY( cDir )
      CreateFolder( cDir )
      IF !ISDIRECTORY( cDir )
         cMsg := "I can't create a folder for the database !;;"
         cMsg += cDir + ";;"
         cMsg += "ERROR ! DOS(" + HB_NtoS(DosError()) + ");;"
         cMsg += "Exit the program !;;"
         cMsg += ProcNL() + ";" + ProcNL(1)
         AlertStop(cMsg,,,64,{RED})
         ? ProcNL(), cMsg
         QUIT
      ENDIF
   ENDIF

   oac:bIndex1 := {|| // creating indexes of the main database
     Local lRet := .F.
     BEGIN SEQUENCE WITH {|e| break( e ) }
     INDEX ON &("IDEVENT")                     TAG IDEVENT
     DO EVENTS
     INDEX ON &("NUSER")                       TAG NUSER     FOR !Deleted() UNIQUE
     DO EVENTS
     INDEX ON &("DTOS(DEVENT) + TEVENT + STR(IDEVENT)") TAG DATEIDEV  FOR !Deleted()
     DO EVENTS
     INDEX ON &("DEVENT")                      TAG DATE_EV   FOR !Deleted()
     DO EVENTS
     INDEX ON &("DTOS(DEVENT) + STR(IDEVENT)") TAG DATE_UNI  FOR !Deleted() UNIQUE
     DO EVENTS
     lRet := .T.
     END SEQUENCE
     Return lRet
     }

   oac:bIndex2 := {|| // creating indexes of the operator directory
     Local lRet := .F.
     BEGIN SEQUENCE WITH {|e| break( e ) }
     INDEX ON &("KOPERAT") TAG KOPERAT
     DO EVENTS
     INDEX ON &("OPERAT")  TAG OPERAT
     DO EVENTS
     INDEX ON &("UPPER(OPERAT)")  TAG KGROUP FOR &('KOPERAT > 0 .AND. KGROUP < 90 .AND. !DELETED()')
     DO EVENTS
     lRet := .T.
     END SEQUENCE
     Return lRet
     }

   oac:cStruct1 := "Struct_User2Log"                 // user2log.prg

   oac:aStruct2 := {;
                    {"KOPERAT"   , "N",  5, 0 } ,;
                    {"OPERAT"    , "C", 30, 0 } ,;
                    {"KGROUP"    , "N",  3, 0 } ,;
                    {"TS"        , "=",  8, 0 } ,;
                    {"LREPORT"   , "L",  1, 0 } ,;
                    {"D_11_02_25", "C",  1, 0 } ;
                   }

   oac:aFileDbf := { {"User2Log" , "User2Log", .T., oac:bIndex1, oac:cStruct1 } ,;
                     {"Operat"   , "Operat"  , .T., oac:bIndex2, oac:aStruct2 }    }

   Sets_User2Index()  // запись данных по базам / recording data in databases
                      // -> user2Index.prg

   IF !Set_DataBase_Create()
      QUIT
   ENDIF

   IF !Set_DataBase_Open()
      QUIT
   ENDIF

   // проверка версии базы по последнему полю / Checking the database version by the last field
   ChkDbfUser2Log(cDir + "\" + oac:aFileDbf[1,1],oac:aFileDbf[1,2])  // user2log.prg

   // entry in the program Event Log
   User2Login()    // -> user2log.prg

   Set_Add_User()  // add users to Operat.dbf

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION Set_DataBase_Create()
   LOCAL cDbf, cAls, lRet, lErr, aUse := {}, a, cErr, bBlk, cFil
   LOCAL cRun, aSru, cPth := App.Cargo:cPathDbf
   LOCAL cInd, lInd, aDbf := App.Cargo:aFileDbf

   cErr := ""
   FOR EACH a IN aDbf
       cDbf := a[1]
       cAls := a[2]
       bBlk := a[4]
       aSru := a[5]
       cFil := cPth + cDbf + ".dbf"
       cInd := cPth + cDbf + ".cdx"
       IF !FILE(cFil)
          IF IsArray(aSru)
             DBCreate( cFil, aSru, "DBFCDX")
          ELSEIF IsString(aSru)
             cRun := aSru + "('" + cFil + "','" + cAls + "')"
             lRet := &cRun
          ELSE
             cErr += "Error! Incorrect parameters aSru = "
             MsgDebug("Error! Incorrect parameters aSru = ",aSru)
          ENDIF
       ENDIF
       lErr := .T.
       IF !FILE(cInd)
          BEGIN SEQUENCE WITH {|e| break( e ) }
             USE ( cPth + cDbf ) ALIAS ( cAls ) NEW EXCLUSIVE
             IF Used() ; AAdd( aUse, .T. ) ; lErr := .F.
             ENDIF
          END SEQUENCE
          IF lErr
             cErr += "Not used EXCLUSIVE ! " + HB_NtoS(hb_enumindex(a))
             cErr += " - " + cAls + " " + cPth + cDbf + ";"
          ELSE
             lInd := EVal(bBlk, cInd)   // создание cdx нормально
             dbCloseArea()
          ENDIF
       ENDIF
   NEXT
   lRet := .T.
   IF LEN(cErr) > 0
      cErr += ";" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error create", , 64, {RED} )
      lRet := .F.
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////////////
FUNCTION Set_DataBase_Open()
   LOCAL cDbf, cAls, lRet, lErr, aUse := {}, a, cErr
   LOCAL cPth := App.Cargo:cPathDbf
   LOCAL aDbf := App.Cargo:aFileDbf

   cErr := ""
   FOR EACH a IN aDbf
       cDbf := a[1]
       cAls := a[2]
       lErr := .T.
       BEGIN SEQUENCE WITH {|e| break( e ) }
          USE ( cPth + cDbf ) ALIAS ( cAls ) NEW SHARED
          IF Used() ; AAdd( aUse, .T. ) ; lErr := .F.
          ENDIF
          //? ProcNL(), RECNO(), myGetIndexUse()
       END SEQUENCE
       IF lErr
          cErr += "Not used SHARED ! " + HB_NtoS(hb_enumindex(a))
          cErr += " - " + cAls + " " + cPth + cDbf + ";"
       ELSE
       ENDIF
   NEXT
   lRet := Len(aUse) == Len(aDbf)
   IF !lRet
      cErr += ";" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Error open", , 64, {RED} )
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////////////
FUNCTION Set_Add_User()
   LOCAL nI, nCode, cName, aDim

   SELECT User2Log
   OrdSetFocus("NUSER")      // список юзеров
   dbGoTop()

   aDim := {}
   FOR nI := 1 TO ORDKEYCOUNT()
      ORDKEYGOTO(nI)
      nCode := User2Log->NUSER
      cName := User2Log->USER
      AADD( aDim, { nCode, cName } )
   NEXT

   SELECT OPERAT
   DbSetOrder(1)
   //? ProcNL(), RECNO(), myGetIndexUse()

   FOR nI := 1 TO LEN(aDim)
      nCode := aDim[nI,1]
      cName := aDim[nI,2]
      GOTO TOP
      SEEK(nCode)
      IF !FOUND()
         APPEND BLANK
         IF OPERAT->(RLock())
            OPERAT->KOPERAT := nCode
            OPERAT->OPERAT  := cName
            OPERAT->KGROUP  := 2
            OPERAT->( DbUnlock() )
            OPERAT->( DbCommit() )
         ENDIF
      ENDIF
      DO EVENTS
   NEXT

RETURN .T.

/////////////////////////////////////////////////////////// 03.08.23
// Открыть таблицу или DBF
FUNCTION myUseArea( cDbf, cAls, lShared, cRdd, cCdp, nWhl, cErr2 )
   LOCAL lRet := .T., cPth, cFil, cExt
   Default cAls := ""
   Default lShared := .T.
   Default cRdd := RddSetDefault()
   Default cCdp := "RU866"
   DEFAULT nWhl := 10

   If !hb_FileExists(cDbf)
      cErr2 += "Нет файла базы !;" + cDbf + ";"
      //AlertStop(cErr + ";" + ProcNL() + ";" + ProcNL(1) )
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

   //SELECT 0
   //If     empty (cAls)    ; cAls := '_XYZ_'+hb_ntos(select())
   //ElseIf select(cAls) > 0; cAls += '_'    +hb_ntos(select())
   //EndIf
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
   //? SPACE(2)+ProcNL(), "Открытие " + cDbf, lRet, "Кол-во попыток (10--) =", nWhl

RETURN lRet


/////////////////////////////////////////////////////////////////
FUNCTION BASE_TEK(cPar)
   LOCAL nI, cText, nSel, nOrder, cAlias, cIndx, aIndx := {}
   LOCAL cZn, xSc1, xSc2, cVal, cMsg := ProcNL( 1 )
   DEFAULT cPar := ""

   cAlias := ALIAS()
   nSel   := SELECT(cAlias)
   IF nSel == 0
      cText := "Нет открытых БАЗ !;"
      cText += 'Alias()= "' + cAlias + '" , SELECT(cAlias) -> 0 ;;'
      cText += ProcNL() + ";" + ProcNL(1)
      AlertStop( cText, "Открытые БД", , 64, {RED} )
      RETURN cText
   ENDIF

   nOrder := INDEXORD()  // Результат: NUMBA
   cText := "Открыта БД - алиас: "+Alias()+"()    RddName: " + RddName() + CRLF
   cText += "Путь к базе - " + DBINFO(DBI_FULLPATH) + CRLF
   cText += 'DbFilter()= [' + (cAlias)->( DbFilter() ) + ']' + CRLF + CRLF
   cText += "Открытые индексы: "
   IF RddName() == "LETO"
      IF OrdCount() == 0
         cText += " (нет) !" + CRLF + CRLF
      ELSE
         cText += CRLF + CRLF
         FOR nI := 1 TO OrdCount()
             cText += STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI)
             cText += "   OrdFor: " + OrdFor() + CRLF
             //cText += "   DBOI_UNIQUE: " +  XTOC(dbInfo(DBOI_UNIQUE)) + CRLF
            AADD( aIndx, STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI) )
         NEXT
      ENDIF
   ELSE
      IF nOrder == 0
         cText += " (нет) !" + CRLF + CRLF
      ELSE
         cText += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' )' + CRLF + CRLF
         //cText += CRLF + CRLF
         FOR nI := 1 TO 100
            cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
            IF cIndx == ""
               EXIT
            ELSE
               DBSetOrder( nI )
               cText += STR(nI,3) + ') - Файл индекса: ' + DBORDERINFO(DBOI_FULLPATH) + CRLF
               cText += '   Фокус индекса: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ")" + CRLF
               cText += '    Ключ индекса: "' + DBORDERINFO( DBOI_EXPRESSION ) + '"' + CRLF
               cText += '     FOR индекса: "' + OrdFor() + '" ' + SPACE(3)
               cText += 'DESCENDing: ' + cValToChar(DBORDERINFO(DBOI_ISDESC)) + SPACE(3)
               cText += 'UNIQUE: ' + cValToChar(DBORDERINFO(DBOI_UNIQUE)) + SPACE(3)+ CRLF
               xSc2 := DbOrderInfo( DBOI_SCOPEBOTTOM )
               xSc1 := DbOrderInfo( DBOI_SCOPETOP )
               cZn  := '"'
               IF VALTYPE(xSc1) # "C"
                  xSc1 := cValToChar(xSc1)
                  xSc2 := cValToChar(xSc2)
                  cZn  := ''
               ENDIF
               IF LEN(xSc1) > 0 .OR. LEN(xSc2) > 0
                  cText += '    SET SCOPE TO: ' + cZn +  xSc1 + cZn + ' , ' + cZn + xSc2 + cZn + CRLF
               ENDIF
               cText += '   KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT)) + ' )' + ';'+ CRLF

               cVal := STR(nI,3) + ", OrdName: " + OrdName(nI) + ", OrdKey: " + OrdKey(nI)
               cVal += ", DESCENDing: " + cValToChar(DBORDERINFO(DBOI_ISDESC))
               cVal += ", UNIQUE: " + cValToChar(DBORDERINFO(DBOI_UNIQUE))
               cVal += ", KEYCOUNT() = " + HB_NtoS(ORDKEYCOUNT()) + CRLF
               AADD( aIndx, cVal )

            ENDIF
         NEXT
         DBSetOrder( nOrder ) // переключить на основной индекс
         cText += "Текущий индекс = "+HB_NtoS(nOrder)+" , Фокус индекса: " + ORDSETFOCUS()
      ENDIF
      IF OrdCount() == 0
         cText += "          Кол-во записей = " + HB_NtoS(LastRec()) + CRLF
      ELSE
         cText += "          Кол-во записей = " + HB_NtoS(ORDKEYCOUNT()) + CRLF
      ENDIF
   ENDIF
   cText += CRLF

   IF cPar == "STRING"
   ELSEIF cPar == "STRING2"
      cText := Alias() + ",  Открытые индексы: " + HB_NtoS(LEN(aIndx)) + ",  Текущий индекс = "+HB_NtoS(nOrder)
      cText += " , OrdName: " + OrdName() + " , OrdKey: " + OrdKey() + CRLF
      cText += ' FOR: "' + OrdFor() +'"'+ CRLF
   ELSE
      AlertInfo( cText, "Открытые БД", , 64, {RED} )
   ENDIF
   DbSelectArea(cAlias)

RETURN cText

/////////////////////////////////////////////////////////////////
// Возрат массива открытой базы АЛИАС и индексных файлов базы
FUNCTION myIndexOpen(cPath)
   LOCAL aMemIndex := {}, nI, cTmp, nOrder
   DEFAULT cPath := ""

   nOrder := INDEXORD()
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
   DbSetOrder(nOrder)

RETURN aMemIndex

/////////////////////////////////////////////////////////////////
// Возрат массива открытой базы АЛИАС и индексных файлов базы
FUNCTION myIndexRestore(aDim,lSay)
   LOCAL nI, cAlias, cIndex
   DEFAULT aDim := {}, lSay := .T.

   IF LEN(aDim) == 0
      IF lSay
         AlertStop("Нет открытых индексов для базы !;Текущий алиас: "+ALIAS()+" !")
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

/////////////////////////////////////////////////////////////////
FUNCTION MYINDEXOPENSAVE(cPath)
   RETURN myIndexOpen(cPath)

FUNCTION MYINDEXOPENRESTORE(aDim)
   RETURN myIndexRestore(aDim)

//////////////////////////////////////////////////////////////////////////////
// Список открытых индексов / List of open indexes
FUNCTION myGetIndexUse()
   LOCAL nI, nTags, cOrd, cFor, nOrder, cOrder, cMsg

   cMsg := "Список открытых индексов / List of open indexes:" + CRLF
   cMsg += "Alias: " + ALIAS()  + CRLF

   ? ProcNL(), ALIAS()
   IF OrdCount() == 0
      cMsg += "Index: no open indexes" + CRLF + CRLF
      RETURN cMsg
   ENDIF

   cMsg   += " Path: " + (ALIAS())->( DBINFO(DBI_FULLPATH) ) + CRLF

   nOrder := INDEXORD()
   cOrder := ( ALIAS() )->( ordKey( nOrder ) )
   nTags  := ( ALIAS() )->( ordCount() )
   FOR nI := 1 TO nTags
      cOrd := ( ALIAS() )->( ordName( nI ) )
      cFor := ( ALIAS() )->( ordKey( nI ) )
      cMsg += SPACE(5) + "DbSetOrder(" + HB_NtoS(nI) + ") " + cOrd + " - " + cFor + CRLF
   NEXT
   cMsg += CRLF
   cMsg += "Current index: " + HB_NtoS(nOrder) + " - " + cOrder + CRLF
   DbSetOrder(nOrder)

RETURN cMsg

