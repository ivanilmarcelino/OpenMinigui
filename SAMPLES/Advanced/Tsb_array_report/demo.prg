/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ¬ыборка из базы 100,000 записей за период и расчЄт отчЄта по услови€м
 * –абота с базой операторами SCOPE, FILTER, INDEX + условна€ индексаци€
 * ќтложенна€ активаци€ окон отчЄта в _TBrowse()
 * ѕоказ лога программы после выхода из программы
 * Selection from the database of 100,000 records for the period and calculation of the report according to the conditions
 * Working with the database using the SCOPE, FILTER, INDEX operators + conditional indexing
 * Delayed activation of report windows in _TBrowse()
 * Show program log after exiting the program
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "dbinfo.ch"
#include "set.ch"

REQUEST DBFCDX
#define PROGVER    "Version 0.6 (06.09.2025)"
#define RECNO_DBF  100000 
/////////////////////////////////////////////////////////////////////////
Function Main
   LOCAL cLog, nW, nH, tTime, cLang, aTtl, cTitle, aCode, aNVip, owc
                                      
   SET DATE  TO GERMAN
   SET EPOCH TO ( Year(Date()) - 50 )

   SET NAVIGATION EXTENDED
   SET MULTIPLE OFF WARNING
   SET FONT TO "DejaVu Sans Mono", 16
   SET OOP ON
   RDDSETDEFAULT('DBFCDX')

   SET DELETED ON

   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }               // for HMG_Alert()
   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono" SIZE 14  // for HMG_Alert()
   SET ShowRedAlert ON

   cLog := _SetGetLogFile( "_msg.log" )
   DELETEFILE(cLog)
   SET LOGFILE TO &cLog // установить им€ файла дл€ вывода отладки

   tTime  := HB_DATETIME()
   cLang  := Upper( Left( SET ( _SET_LANGUAGE ), 2 ) )
   aTtl   := { 'ѕрименение операторов: SCOPE,FILTER,INDEX','Using operators: SCOPE, FILTER, INDEX'}
   cTitle := aTtl[iif(cLang == "EN", 2, 1)]
   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ;  ? cTitle ; ?

   // calculation by the base on the field KVIPZA and the reference book of this field 
   aCode := {1,2,3,4,5,6}  // KVIPZA == aCode[?] 
   aNVip := {"completed","cancelled","not completed","rescheduled: waiting for call","no contact","rejected for non-payment"}

   DEFINE WINDOW Form_Main                    ;
      AT 0,0 WIDTH Sys.ClientWidth HEIGHT 80  ;
      TITLE cTitle                            ;
      MAIN NOSHOW NOMAXIMIZE NOSIZE           ;
      BACKCOLOR  { 0,64,80 }                  ;        
      ON INIT    {|| DoEvents(), _wPost(0) }  ;   // выполн€етс€ после инициализации окна / executed after window initialization
      ON RELEASE {|| _wSend(91)  }            ;   // выполн€етс€ перед разрушением окна / performed before the window is destroyed
      ON INTERACTIVECLOSE {|| NIL }               // закрытие окна по [x] / close window by [x]

      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:hWin     := This.Handle               
      owc:aTtl     := aTtl
      owc:a2Master := {}                        // список мастеров из базы - заполн€ем в Test_Use()
      owc:aVipCode := aCode                     // положим на окно дл€ удобства / Let's put it on the window for convenience
      owc:aVipName := aNVip                     // положим на окно дл€ удобства / Let's put it on the window for convenience

      nW := This.ClientWidth
      nH := This.ClientHeight

      WITH OBJECT This.Object
      :Event( 0, {|ow|
                  Local aSnd := {40, 1, 2, 3, 4}, nI, a
                  Local aPar := {"Open/create database", "SCOPE + FILTER", "Calculation - SCOPE",;
                                 "Conditional indexation", "DoWhile -> Array"}
                  Local cWnd := ow:Name, cWin, aLbl, cLbl, cMsg, a4Dim := {}
                  Local owc  := ow:Cargo
                  Local cCap := "... Windows: " + cWnd + " ... "
                  Local aSay := { cCap, owc:aTtl[iif(cLang == "EN", 2, 1)], GetExeFileName(), "...." }
                  Local cForm, aForm
                  cWin := WaitWindow(aSay, .T., 900, 16, NIL, YELLOW, ORANGE )
                  aLbl := HMG_GetFormControls(cWin, "LABEL")  // list of objects
                  cLbl := ATail(aLbl)
                  For nI := 1 To Len(aSnd)
                     cMsg := hb_ntos(nI) + "/" 
                     cMsg += hb_ntos(Len(aPar)) + Space(10) + aPar[nI]
                     SetProperty( cWin, cLbl, "Value", cMsg )
                     DO EVENTS
                     a := { aSnd[nI], aPar[nI], cWin, cLbl }
                     _wSend(aSnd[nI], cWnd, a )
                     DO EVENTS
                     If aSnd[nI] >= 1 .AND. aSnd[nI] <= 4
                        Aadd( a4Dim, ow:Cargo:aRet )  // сохраним результат / save the result
                     Endif
                  Next
                  WaitWindow()
                  DO EVENTS
                  aForm := {}
                  For nI := 1 To Len(a4Dim)
                     _wSend(50, ow:Name, a4Dim[nI] )
                     cForm := ow:Cargo:cRetForm           // -> demo_report.prg
                     //cForm := Table_Rprt(ow,a4Dim[nI])  // opening of bases - you can do it this way too
                     IF LEN(cForm) > 0
                        AADD(aForm, cForm)
                     ENDIF
                     DO EVENTS
                  Next
                  wApi_Sleep(100)
                  // активировать все окна / activate all windows
                  _ActivateWindow( aForm, .F., , )
                  DO EVENTS
                  wApi_Sleep(500)
                  a := HMG_GetForms("S")
                  If Len(a) > 0 // standard window
                     _wSend(90, ow:Name, a ) 
                  Else
                     _wSend(99, ow:Name ) 
                  Endif
                  DO EVENTS
                  Return Nil
                  } )

      :Event( 1, {|ow,ky,xv| ky := Test_Scope_Filter(ow,xv)     , ow:Cargo:aRet := ky } )
      :Event( 2, {|ow,ky,xv| ky := Test_Calc_Scope(ow,xv)       , ow:Cargo:aRet := ky } )
      :Event( 3, {|ow,ky,xv| ky := Test_Conditional_index(ow,xv), ow:Cargo:aRet := ky } )
      :Event( 4, {|ow,ky,xv| ky := Test_DoWhile_Array(ow,xv)    , ow:Cargo:aRet := ky } )
      :Event(40, {|ow,ky,xv| ky := Test_Use(ow,xv)    /* opening of bases */          } )
      :Event(50, {|ow,ky,xv| ky := Table_Rprt(ow,xv)  /* reports window */            } )

      /* можно писать и так / you can write it like this too
      :Event( 0, {|  | _wSend(1), _wSend(2), _wSend(3), _wSend(99) } )
      :Event( 1, {|  |  Test_Use()  } )
      :Event( 2, {|  |  Test_XXXX() } )
       */
      :Event(89, {|ow,ky  | _LogFile(.T., ">>> Open Log: "+ow:Name,ky) , DoEvents(), ShellExecute(0,"Open",cLog,,,1) } )           
      :Event(90, {|ow,ky,a| AlertInfo("Close the report windows !;;" + ATREPL(",",HB_ValToExp(a),";"), ky:=ow:Name), ow:Show() } )
      :Event(91, {|ow,ky  | _LogFile(.T., ">>> End of program <<<  "+HMG_TimeMS(tTime),ow:Name,ky), DoEvents(), _wSend(89,ow)  } )
      :Event(99, {|ow     | ow:Release()  } )
      END WITH

   END WINDOW

   //CENTER WINDOW Form_Main
   ACTIVATE WINDOW Form_Main

Return Nil

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_DoWhile_Array(oWnd, xPar)
   LOCAL tTime, a2Mast, aCode, aNVip, dDate1, dDate2, nMast, cAls, nSum, a
   LOCAL i, j, nExp, k, n, z, aTable, aRpt, cTxt, aRet
   LOCAL aRecno, nK
   DEFAULT oWnd := ThisWindow.Object                

   ? ProcNL(), oWnd, xPar, HB_ValToExp(xPar)
   aTable := { xPar[1], xPar[2] }
   a2Mast := oWnd:Cargo:a2Master   // справочник мастеров / handbook of Masters
   aCode  := oWnd:Cargo:aVipCode   // справочник кодов пол€ KVIPZA / KVIPZA field code directory
   aNVip  := oWnd:Cargo:aVipName   // наименование кодов KVIPZA / KVIPZA code name
   tTime  := HB_DATETIME()
   dDate1 := CTOD("01.01.25")
   dDate2 := DATE()
   nSum   := 0
   cAls   := "ZAIVKA"
   aRpt   := {}
   aRecno := {}

   SET SOFTSEEK  ON   // включает подвод SEEK до ближайшего большего ключа
                      // turns on SEEK advance to the nearest larger key
   DBSELECTAREA(cAls)
   OrdSetFocus("DATEZA")
   ? SPACE(5) + "Order =", OrdSetFocus(), dDate1, "-", dDate2
   GO TOP
   SEEK(dDate1)
   DO WHILE !EOF() .and. ZAIVKA->DATEZA >= dDate1 .AND. ZAIVKA->DATEZA <= dDate2
      DO EVENTS
      IF !Deleted()
         AADD( aRecno, RECNO() )
      ENDIF
      SKIP
   ENDDO
   DbSetOrder(0)
   GO TOP
   ?? "aRecno=", aRecno
   
   FOR i := 1 TO Len(a2Mast)
       DO EVENTS
       nMast := a2Mast[ i, 1 ]
       ? SPACE(5) + "Master: " + a2Mast[ i, 2 ]
       cTxt := "Array[ recno() ] + KMASTER == "+ hb_ntos(nMast) 
       AADD( aRpt, { "Master: " + a2Mast[ i, 2 ], 0, SPACE(5) + cTxt } )
       k    := 0
       a    := ARRAY(Len(aCode))
       AFILL(a, 0)
       z    := 0                             // undefined
       nExp := 0                             // expired
       FOR nK := 1 TO LEN(aRecno)
          GOTO(aRecno[nK])
          DO EVENTS
          IF (cAls)->KMASTER == nMast
             k++
             n := (cAls)->KVIPZA
             IF n > 0 .AND. n <= Len(a) 
                a[ n ]++
             ELSE
                z++
             ENDIF
             IF n == 3          // unfulfilled
                IF DATE() > (cAls)->DateSrok
                   nExp ++      // expired
                ENDIF
             ENDIF
          ENDIF
       NEXT
       nSum += k
       ?? " Qty: ", k, HB_ValToExp(a), "undefined=",z, "expired=",nExp
       //
       FOR j := 1 TO LEN(aNVip)
          cTxt := aNVip[j] + ":"
          AADD( aRpt, { cTxt , a[j], "" } )
       NEXT
       AADD( aRpt, { "expired="  ,nExp, "" } )
       AADD( aRpt, { "undefined=",z   , "" } )
       AADD( aRpt, { "", 0, "" } )
   NEXT
   cTxt := "time spent " + HMG_TimeMS(tTime)
   ? SPACE(5) + "Total requests=",nSum
   ? SPACE(10)+".", xPar[1], xPar[2], cTxt
   ? CRLF, REPL("-",90)
   AADD( aRpt, { "", 0, "" } )
   AADD( aRpt, { "Total requests=", nSum , ""} )
   AADD( aRpt, { "time spent " + HMG_TimeMS(tTime), 0 , ""} )

   aRet := {aRpt, aTable, cTxt, ProcName() }

RETURN aRet 

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Scope_Filter(oWnd, xPar)
   LOCAL tTime, a2Mast, aCode, aNVip, dDate1, dDate2, nMast, cAls, nSum, a
   LOCAL oZa, i, j, nExp, k, n, z, cDtZ1, cDtZ2, aTable, aRpt, cTxt, aRet
   DEFAULT oWnd := ThisWindow.Object                

   ? ProcNL(), oWnd, xPar, HB_ValToExp(xPar)
   aTable := { xPar[1], xPar[2] }
   a2Mast := oWnd:Cargo:a2Master   // справочник мастеров / handbook of Masters
   aCode  := oWnd:Cargo:aVipCode   // справочник кодов пол€ KVIPZA / KVIPZA field code directory
   aNVip  := oWnd:Cargo:aVipName   // наименование кодов KVIPZA / KVIPZA code name
   tTime  := HB_DATETIME()
   dDate1 := CTOD("01.01.25")
   dDate2 := DATE()
   nSum   := 0
   cAls   := "ZAIVKA"
   DBSELECTAREA(cAls)
   OrdSetFocus("DATZA")
   cDtZ1 := DtoS(dDate1)
   cDtZ2 := DtoS(dDate2)
   aRpt  := {}

   ? SPACE(5) + "Order =", OrdSetFocus(), cDtZ1, "-", cDtZ2

   SET SCOPE TO cDtZ1, cDtZ2
   oZa := oHmgData()
   FOR i := 1 TO Len(a2Mast)
       nMast := a2Mast[ i, 1 ]
       ? SPACE(5) + "Master: " + a2Mast[ i, 2 ]
       cTxt := "SCOPE: " + cDtZ1 + " - " + cDtZ2 + " + FILTER: KMASTER == "+ hb_ntos(nMast)
       AADD( aRpt, { "Master: " + a2Mast[ i, 2 ], 0, SPACE(5) + cTxt } )
       k    := 0
       a    := ARRAY(Len(aCode))
       AFILL(a, 0)
       z    := 0                             // undefined
       nExp := 0                             // expired
       SET FILTER TO &("!Deleted() .and. KMASTER == "+ hb_ntos(nMast))
       GO TOP
       DO WHILE !EOF()
          DO EVENTS
          k++
          n := (cAls)->KVIPZA
          IF n > 0 .AND. n <= Len(a) 
             a[ n ]++
          ELSE
             z++
          ENDIF
          IF n == 3          // unfulfilled
             IF DATE() > (cAls)->DateSrok
                nExp ++      // expired
             ENDIF
          ENDIF
          SKIP
       ENDDO
       nSum += k
       IF k > 0 ; oZa:Set(nMast, a)
       ENDIF
       ?? " Qty: ", k, HB_ValToExp(a), "undefined=",z, "expired=",nExp
       //
       FOR j := 1 TO LEN(aNVip)
          cTxt := aNVip[j] + ":"
          AADD( aRpt, { cTxt , a[j], "" } )
       NEXT
       AADD( aRpt, { "expired="  ,nExp, "" } )
       AADD( aRpt, { "undefined=",z   , "" } )
       AADD( aRpt, { "", 0, "" } )
   NEXT
   SET FILTER TO
   SET SCOPE  TO
   GO TOP
   cTxt := "time spent " + HMG_TimeMS(tTime)
   ? SPACE(5) + "Total requests=",nSum
   ? SPACE(10)+".", xPar[1], xPar[2], cTxt
   ? CRLF, REPL("-",90)
   AADD( aRpt, { "", 0, "" } )
   AADD( aRpt, { "Total requests=", nSum , ""} )
   AADD( aRpt, { "time spent " + HMG_TimeMS(tTime), 0 , ""} )

   //_o2log(oZa, 10, "1 >>>", .T.) ; ?
   //? 2, oZa:GetAll() ; ?v oZa:GetAll() ; ?

   aRet := {aRpt, aTable, cTxt, ProcName() }

RETURN aRet 

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Calc_Scope(oWnd, xPar)
   LOCAL tTime, a2Mast, aCode, aNVip, dDate1, dDate2, nMast, cMast, cVal
   LOCAL cDat, cEnd, nI, nJ, cAls, nSum, aSum, nVal, nZer, nSrk, aRpt
   LOCAL nRec, aTable, cTxt, aRet
   DEFAULT oWnd := ThisWindow.Object                
                                                   
   ? ProcNL(), oWnd, xPar, HB_ValToExp(xPar)
   aTable := { xPar[1], xPar[2] }
   a2Mast := oWnd:Cargo:a2Master   // справочник мастеров / handbook of Masters
   aCode  := oWnd:Cargo:aVipCode   // справочник кодов пол€ KVIPZA / KVIPZA field code directory
   aNVip  := oWnd:Cargo:aVipName   // наименование кодов KVIPZA / KVIPZA code name

   tTime  := HB_DATETIME()
   dDate1 := CTOD("01.01.25")
   dDate2 := DATE()
   cAls   := "ZAIVKA"
   DBSELECTAREA(cAls)
   OrdSetFocus("MAST_ZA")
   ? SPACE(5) + "Order =", OrdSetFocus(), DtoS(dDate1), "-", DtoS(dDate2)

   aRpt := {}
   nSum := 0
   FOR nI := 1 TO LEN(a2Mast)

      nMast := a2Mast[nI,1]
      cMast := a2Mast[nI,2]
      ? SPACE(5) + "Master: " + cMast
      cDat := STR(nMast,3) + DtoS(dDate1)      // N (3,0)
      cEnd := STR(nMast,3) + DtoS(dDate2) 
      AADD( aRpt, { "Master: " + cMast , 0, SPACE(5) + "SCOPE: " + cDat + " - " + cEnd } )
      ?? "SCOPE:",cDat, cEnd
      //?? "LEN=", LEN(cDat)
      SET SCOPE TO cDat, cEnd
      GOTO TOP
      nRec := 0
      nZer := 0                                // undefined
      nSrk := 0                                // expired
      aSum := ARRAY(LEN(aCode))                // amount of requests for execution
      AFILL(aSum,0)
      DO WHILE !EOF()  
         DO EVENTS
         nRec++
         nVal := ZAIVKA->KVIPZA
         IF nVal > 0 .AND. nVal <= LEN(aCode)
            aSum[nVal]++
         ELSE
            nZer++
         ENDIF
         IF nVal == 3       // unfulfilled
            IF DATE() > (cAls)->DateSrok
               nSrk ++      // expired
            ENDIF
         ENDIF
         SKIP
      ENDDO
      nSum += nRec

      FOR nJ := 1 TO LEN(aNVip)
         cVal := aNVip[nJ] + ":"
         AADD( aRpt, { cVal , aSum[nJ], "" } )
      NEXT
      AADD( aRpt, { "expired="  ,nSrk, "" } )
      AADD( aRpt, { "undefined=",nZer, "" } )
      AADD( aRpt, { "", 0, "" } )

      ?? " Qty: ", nRec, HB_ValToExp(aSum), "undefined=",nZer, "expired=",nSrk
      SET SCOPE TO

   NEXT

   cTxt := "time spent " + HMG_TimeMS(tTime)
   ? SPACE(5) + "Total requests=",nSum
   ? SPACE(10)+".", xPar[1], xPar[2], cTxt
   ? CRLF, REPL("-",90)
   AADD( aRpt, { "", 0, "" } )
   AADD( aRpt, { "Total requests=", nSum , ""} )
   AADD( aRpt, { "time spent " + HMG_TimeMS(tTime), 0 , ""} )

   aRet := {aRpt, aTable, cTxt, ProcName() }

RETURN aRet 

///////////////////////////////////////////////////////////////////////////
// условна€ индексаци€ базы / conditional indexation of the base 
STATIC FUNCTION Test_Conditional_index(oWnd, xPar)
   LOCAL tTime, a2Mast, aCode, aNVip, dDate1, dDate2, nMast, cMast, cVal
   LOCAL nI, nJ, cAls, nSum, aSum, nVal, nZer, nSrk, aRpt, cTag, nG
   LOCAL nRec, nOrd, aOpen, cFilter, cPath, cIndex, aTable, cTxt, aRet
   DEFAULT oWnd := ThisWindow.Object                
                                                   
   ? ProcNL(), oWnd, xPar, HB_ValToExp(xPar)
   aTable := { xPar[1], xPar[2] }
   a2Mast := oWnd:Cargo:a2Master   // справочник мастеров / handbook of Masters
   aCode  := oWnd:Cargo:aVipCode   // справочник кодов пол€ KVIPZA / KVIPZA field code directory
   aNVip  := oWnd:Cargo:aVipName   // наименование кодов KVIPZA / KVIPZA code name
   cPath  := GetUserTempFolder() + "\" 
   tTime  := HB_DATETIME()
   dDate1 := CTOD("01.01.25")
   dDate2 := DATE()
   cAls   := "ZAIVKA"
   DbSelectArea(cAls)
   nOrd   := IndexOrd()
   aOpen  := myIndexOpen()  // список открытых индексных файлов этой базы / list of open index files of this database
   OrdSetFocus("DateZa")
   ? SPACE(5) + "Order =", OrdSetFocus(), dDate1, "-", dDate2

   aRpt := {}
   nSum := 0
   
   FOR nI := 1 TO LEN(a2Mast)

      nMast := a2Mast[nI,1]
      cMast := a2Mast[nI,2]
      ? SPACE(5) + "Master: " + cMast

      cFilter := "( DateZa >= CTOD('" + DtoC(dDate1) + "') .AND. DateZa <= CTOD('" + DtoC(dDate2) + "') ) "     
      cFilter += " .AND. KMaster == " + HB_NtoS(nMast) + " .AND. !Deleted()" 
      cIndex  := cPath + cAls + "_tmp" + HB_NtoS(nI) + ".cdx"
      cTag    := "DZA_MAST_" + HB_NtoS(nI)
      //?? "check:", &cFilter 
      DELETEFILE(cIndex)
      GOTO TOP
      SEEK(dDate1)
      INDEX ON &('DateZa') TAG &(cTag) TO (cIndex) EVAL myProgress() FOR &cFilter ADDITIVE
      DO EVENTS
      OrdSetFocus(cTag)
      AADD( aRpt, { "Master: " + cMast ,  0, SPACE(5) + cFilter } )

      nRec := ORDKEYCOUNT()
      ?? "INDEX:",OrdSetFocus()
      nZer := 0                                // undefined
      nSrk := 0                                // expired
      aSum := ARRAY(LEN(aCode))                // amount of requests for execution
      AFILL(aSum,0)
      FOR nG := 1 TO ORDKEYCOUNT()
         ORDKEYGOTO( nG )
         nVal := ZAIVKA->KVIPZA
         IF nVal > 0 .AND. nVal <= LEN(aCode)
            aSum[nVal]++
         ELSE
            nZer++
         ENDIF
         IF nVal == 3       // unfulfilled
            IF DATE() > (cAls)->DateSrok
               nSrk ++      // expired
            ENDIF
         ENDIF
         DO EVENTS
      NEXT
      nSum += nRec

      FOR nJ := 1 TO LEN(aNVip)
         cVal := aNVip[nJ] + ":"
         AADD( aRpt, { cVal , aSum[nJ] , "" } )
      NEXT
      AADD( aRpt, { "expired="  ,nSrk, ""  } )
      AADD( aRpt, { "undefined=",nZer, ""  } )
      AADD( aRpt, { "", 0, "" } )

      ?? " Qty: ", nRec, HB_ValToExp(aSum), "undefined=",nZer, "expired=",nSrk
      SET SCOPE TO

   NEXT

   cTxt := "time spent " + HMG_TimeMS(tTime)
   ? SPACE(5) + "Total requests=",nSum
   ? SPACE(10)+".", xPar[1], xPar[2], cTxt
   ? CRLF, REPL("-",90)
   AADD( aRpt, { "", "", "" } )
   AADD( aRpt, { "Total requests=", nSum, ""  } )
   AADD( aRpt, { "time spent " + cTxt, 0, ""  } )

   DbSelectArea(cAls)
   DBCLEARINDEX()         // закрыть все индексы этой базы / close all indexes of this database
   myIndexRestore(aOpen)  // восстановить открытые индексы этой базы / Restore open indexes of this database
   DbSetOrder(nOrd)

   aRet := {aRpt, aTable, cTxt, ProcName() }

RETURN aRet 

////////////////////////////////////////////////////////////////////////////////////
//  ак правильно написать функцию, чтобы колЄсико прелодера не замирало ?
// How to write a function correctly so that the preloader wheel does not freeze?
STATIC FUNCTION myProgress()
DO EVENTS
RETURN .T.

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Use(oWnd, xPar)
   LOCAL cPath, cFDbf, cIndx, cCdp, cAls, cKey, tTime, nRec, aCode
   LOCAL aList, nCode, cName, cErr, cWin, cLbl, cMsg, lDelete, aNVip
   LOCAL lModeDel := Set( _SET_DELETED )

   ? ProcNL(), oWnd, xPar, HB_ValToExp(xPar)
   aCode   := oWnd:Cargo:aVipCode   // справочник кодов пол€ KVIPZA / KVIPZA field code directory
   aNVip   := oWnd:Cargo:aVipName   // наименование кодов KVIPZA / KVIPZA code name
   tTime   := HB_DATETIME()
   cPath   := GetStartupFolder() + "\"
   cCdp    := NIL
   aList   := {} 
   cErr    := ""
   lDelete := .T.
   cWin    := xPar[3]
   cLbl    := xPar[4]
   //SET DELETED OFF

   cFDbf := cPath + "Master.dbf"
   cAls  := "MASTER"
   cIndx := ""  // не нужен индекс / no index needed 
   cKey  := ""  // не нужен ключ дл€ базы / no key needed for base
   IF my_Use(cFDbf, cIndx, cAls, cKey, lDelete, cCdp)
      ? SPACE(5) + dbInfo(DBI_FULLPATH), "lUse=", Used(), Alias()

      DbSelectArea(cAls)
      GOTO TOP
      DO WHILE !EOF()
         IF !DELETED()
            IF MASTER->KMASTER # 0
               IF MASTER->KDEL == 1  // работает
                  nCode := MASTER->KMASTER
                  cName := ALLTRIM(MASTER->MASTER)
                  AADD( aList, { nCode, cName } )
               ENDIF
            ENDIF
         ENDIF
         SKIP
      ENDDO
      aList := ASORT( aList,,, { |x, y| x[2] < y[2] } )
      ? 
   ELSE
      cErr += cFDbf + ";"
   ENDIF

   oWnd:Cargo:a2Master := aList  // запомним на окне / let's remember on the window

   cFDbf   := cPath + "zaivka.dbf"
   cAls    := "ZAIVKA"
   cIndx   := cPath + cAls + "_temp.cdx"

   IF !FILE(cFDbf)
      nRec := RECNO_DBF
      cMsg := "Creating a database - "
      cMsg += TRANSFORM(nRec,"9 999 999")
      SetProperty( cWin, cLbl, "Value", cMsg )
      DO EVENTS
      CreateDbf(cFDbf, cAls, nRec, aList, aCode)  // -> util_zaiv.prg
   ENDIF

   IF my_Use(cFDbf, cIndx, cAls, cKey, lDelete, cCdp)
      ? SPACE(5) + dbInfo(DBI_FULLPATH), "lUse=", Used(), Alias()
      ? SPACE(5) + "number of records in the database = "
      nRec := LastRec()
      ?? TRANSFORM(nRec, "9 999 999") ; ?
      IF RECNO_DBF # nRec
         cErr := "Error ! Records set: " + HB_NtoS(RECNO_DBF)
         cErr += " and in the database: " + HB_NtoS(nRec) + CRLF
         cErr += "You need to delete the database to eliminate this error!"
         MsgStop(cErr)
         ? cErr
         QUIT
      ENDIF
      cMsg := "Create index - " + cFileNoPath(cIndx)
      SetProperty( cWin, cLbl, "Value", cMsg )
      DO EVENTS
      IF !FILE(cIndx)
         INDEX ON &("DTOS(DATEZA)")                  TAG DATZA   TO (cIndx) 
         INDEX ON &("STR(KMASTER,3) + DTOS(DateZa)") TAG MAST_ZA TO (cIndx) FOR !Deleted() 
         INDEX ON &("NNZA")                          TAG KODZA   TO (cIndx) 
         INDEX ON &("STR(KMASTER)")                  TAG MASTER  TO (cIndx) FOR !Deleted()  UNIQUE
         INDEX ON &("DATEZA")                        TAG DATEZA  TO (cIndx) 
      ELSE
         ORDLISTADD( cIndx )
      ENDIF
      OrdSetFocus(1) 
      DbGotop() 
   ELSE
      cErr += cFDbf + ";"
   ENDIF

   IF LEN(cErr) > 0
      AlertStop("Error opening Databases !;;" + cErr,,,64,{RED} )
      _wSend(99, oWnd:Name )
   ENDIF
   ? SPACE(10)+".", xPar[1], xPar[2], "time spent " + HMG_TimeMS(tTime)
   ? CRLF, REPL("-",90)

   Set( _SET_DELETED, lModeDel )

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN " >>> " + ProcName(nVal+1) + "(" + ;
          hb_ntos( ProcLine(nVal+1) ) + ") --> " + ProcFile(nVal+1)
