/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * вызов моей функции при ошибке завершения программы
 * call my function on program termination error
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
/////////////////////////////////////////////////////////////////////////////////////
Function my_ErrorExit(cMsg,oError,cText,cError)
   LOCAL cErrLog := _GetErrorlogFile()
   LOCAL cErrDir := hb_FNameName(cErrLog)
   LOCAL cNewDir := App.Cargo:cPathErrLog
   LOCAL oErr, nErr, cBuf, cLine, cTmp, cForm, aForm, nI, cTxt
   LOCAL cVers, cUser, cUsr := hb_UserName()
   LOCAL nEvtn, cEvtn, cRem, cTime, nRes
   LOCAL cLog := hb_FNameDir(App.ExeName) + "Error_" + ;
                 iif( " " $ cUsr, StrTran(cUsr, " ", "_"), cUsr )+".log"
   STATIC o_ErrorLog

   cForm := GetFormNameByIndex( GetFormIndexByHandle( GetActiveWindow() ) )
   aForm := HMG_GetForms()
   cUser := App.Cargo:cUser
   IF !IsString(cUser)
      cUser := "no user specified"
   ENDIF
   cVers := App.Cargo:cVersion
   IF !IsString(cVers)
      cVers := "program version not specified"
   ENDIF

   // эти строки можно менять под себя / These lines can be changed to suit your needs
   cTxt := Repl("#-",50) + CRLF
   cTxt += '  Date program error: ' + DTOC(DATE()) + '  Time: ' + TIME() + CRLF
   cTxt += '         Application: ' + GetExeFileName() + " / " + cVers + CRLF
   cTxt += '       Computer/User: ' + NetName()+"/"+hb_UserName()+"/" + cUser + CRLF
   cTxt += '        Current base: ' + ALIAS() + ', Recno: ' + HB_NtoS(RecNo()) + '/' + HB_NtoS(LastRec()) + CRLF
   cTxt += '     Time from start: ' + TimeFromStart() + CRLF
   cTxt += '   Screen resolution: ' + HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight()) + CRLF
   cTxt += 'Focus current window: ' + cForm + CRLF
   cTxt += 'List of open windows: ' + HB_NtoS(LEN(aForm)) + CRLF

   FOR nI := 1 TO LEN(aForm)
      cForm := UPPER(aForm[nI])
      cTxt  += SPACE(5) + HB_NtoS(nI) + ") "
      cTxt  += ' Form: ' + cForm + ', Type: "'+_HMG_aFormType[nI]+'" '
      cTxt  += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cTxt  += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cTxt  += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cTxt  += ', Title: ' + GetProperty( cForm, "Title" ) + CRLF
   NEXT
   cTxt += Repl("#-",50) + CRLF

   IF pCount() == 0
      SET MSGALERT BACKCOLOR TO YELLOW
      SET MSGALERT FONTCOLOR TO BLACK

      cTmp := "Вы хотите просмотреть лог-ошибки программы ?;;" + ;
              "Do you want to view the program error logs ?"
      oErr := o_ErrorLog
      cLog := oErr:cLogFile
      ?
      ? "oErr:GetAll()", oErr:GetAll() ; ?v oErr:GetAll()
      ?
      ? Repl("=",80)
      _o2log(App.Cargo,,">>> App.Cargo:", .T.)
      ? Repl("=",80)
      ?
      cTxt := HB_MemoRead(cLog)  // ~ 11000 byte
      cTxt := SUBSTR(cTxt,3)
      // запись ошибки в журнал-событий-программы
      // write error to program-events-log
      nEvtn := 990
      cEvtn := FindEventsDim(nEvtn)
      cRem  := cTxt
      cTime := HMG_TimeMS( App.Cargo:tStart )
      User2LogWrite(nEvtn, cEvtn, cRem, cTime)
      //
      IF AlertYesNo(cTmp, "Open debug file", .T., "iQuest64", 64, { LGREEN, RED }, .T.)
         ShellExecute( , 'open', cLog, , , SW_SHOWNORMAL) ; DO EVENTS
      ENDIF
      //
      DO EVENTS ; wApi_Sleep(100)
      DbCloseAll()
      hb_FileDelete ( cLog )  // удалить если нужно / delete if necessary

      RETURN cMsg

   ELSE
      hb_FileDelete ( cLog )
      _SetGetLogFile( cLog )
      ? ProcNL(), cMsg, pCount(), oError

      IF cText == NIL .OR. cError == NIL
         ? ProcNL() ; ? "Эта версия МиниГуи не поддерживает обработку ошибок (..,oErr,cTxt,cErr) !"
         ?? "This version of MiniGuy does not support error handling (..,oErr,cTxt,cErr) !"
         ? "Используйте версию / Use version: 23.12.8, 25.09 and above"
      ENDIF
      ? cTxt
      ? cError ; ?

   ENDIF

   IF !hb_DirExists( cNewDir )
      DirMake(cNewDir)
      IF !hb_DirExists( cNewDir )
         cMsg := "I can't create a folder for error logs !;;"
         AlertStop( cMsg + cNewDir, , , 64, {RED} )
      ENDIF
   ENDIF

   o_ErrorLog := oHmgData() ; oErr := o_ErrorLog

   oErr:cUser       := cUsr
   oErr:cLogFile    := cLog
   oErr:cErrorLog   := cErrLog
   oErr:dDate       := Date()
   oErr:cTime       := Time()
   oErr:tDateTime   := NIL
   oErr:cFileName   := ""
   oErr:cErrorMsg   := "" // 1
   oErr:aStackTrace := {} // 2
   oErr:aSystemInfo := {} // 3
   oErr:aEnvInfo    := {} // 4
   oErr:aWorkArea   := {} // 5
   oErr:aErrorInfo  := {} // 6
   oErr:aMemoryVar  := {} // 7

   IF hb_FileExists(cErrLog)
      cBuf := hb_memoread(cErrLog)
      cBuf := StrTran(cBuf, "<BR>", "")
      FOR EACH cLine IN hb_ATokens(cBuf, CRLF)
          IF Empty(cLine) ; LOOP
          ENDIF
          cLine := Alltrim(cLine)
          IF "Date:" $ cLine .and.  [class="date"] $ cLine
             nErr  := 0
             cLine := subs(cLine, At(">", cLine)+1)
             cTmp  := left(cLine, At("<", cLine)-1)
             cTmp  := alltrim(cTmp)
             oErr:dDate := CtoD(cTmp)
             cTmp  := "Time:"
             cLine := subs(cLine, At(cTmp, cLine)+Len(cTmp))
             cLine := subs(cLine, At(">", cLine)+1)
             cTmp  := left(cLine, At("<", cLine)-1)
             oErr:cTime := Alltrim(cTmp)
             cTmp := hb_ATokens(oErr:cTime, ":")
             oErr:tDateTime := hb_DateTime( year(oErr:dDate), ;
                                           Month(oErr:dDate), ;
                                             Day(oErr:dDate), ;
                                                Val(cTmp[1]), ; // nHour
                                                Val(cTmp[2]), ; // nMinute
                                                Val(cTmp[3]) )  // nSecond
             LOOP
          ELSEIF [class="error"] $ cLine
             cLine := subs(cLine, At(">", cLine)+1)
             cLine := left(cLine, At("<", cLine)-1)
             oErr:cErrorMsg := cLine
             nErr++                                       // 1
             LOOP
          ELSEIF left(cLine, 5) == Repl("-", 5)
             IF     "Stack Trace"        $ cLine ; nErr++ // 2
             ELSEIF "System Information" $ cLine ; nErr++ // 3
             ELSEIF "Environmental Info" $ cLine ; nErr++ // 4
             ELSEIF "Detailed Work Area" $ cLine ; nErr++ // 5
             ELSEIF "Internal Error Han" $ cLine ; nErr++ // 6
             ELSEIF "Available Memory V" $ cLine ; nErr++ // 7
             ELSE                                ; nErr++ // 8 skip
             ENDIF
             LOOP
          ENDIF
          IF "<" $ cLine .and. ">" $ cLine  // skip
          ELSEIF nErr == 2                  // 2
             AAdd(oErr:aStackTrace, cLine)
          ELSEIF nErr == 3                  // 3
             AAdd(oErr:aSystemInfo, cLine)
          ELSEIF nErr == 4                  // 4
             AAdd(oErr:aEnvInfo, cLine)
          ELSEIF nErr == 5                  // 5
             AAdd(oErr:aWorkArea, cLine)
          ELSEIF nErr == 6                  // 6
             AAdd(oErr:aErrorInfo, cLine)
          ELSEIF nErr == 7                  // 7
             AAdd(oErr:aMemoryVar, cLine)
          ENDIF
      NEXT
   ENDIF

   cTmp := DtoS(oErr:dDate)+"_"+StrTran(oErr:cTime, ":", "")+"_"+cUsr+"_"

   oErr:cFileName := cNewDir + cTmp + ;
                     hb_FNameNameExt(cErrLog)

   nRes := hb_vfRename(cErrLog, oErr:cFileName)
   IF nRes # 0
      cTmp := " DOS: " + HB_NtoS(FError())
      cMsg := "Error transferring file !" + cTmp + ";;"
      cMsg += cErrLog + ";=>;" + oErr:cFileName
      AlertStop( cMsg, , , 64, {RED} )
   ENDIF

   ? ; _o2log(oErr,, ">>> o_ErrorLog", .F.) ; ?

   ? "oErr:aStackTrace:", oErr:aStackTrace
   ? "*** ERROR ***", oErr:cErrorMsg       ; ?v oErr:aStackTrace
   ? "oErr:aSystemInfo:", oErr:aSystemInfo ; ?v oErr:aSystemInfo
   ? "oErr:aEnvInfo   :", oErr:aEnvInfo    ; ?v oErr:aEnvInfo
   ? "oErr:aWorkArea  :", oErr:aWorkArea   ; ?v oErr:aWorkArea
   ? "oErr:aErrorInfo :", oErr:aErrorInfo  ; ?v oErr:aErrorInfo
   ? "oErr:aMemoryVar:" , oErr:aMemoryVar  ; ?v oErr:aMemoryVar
   ?

RETURN cMsg
