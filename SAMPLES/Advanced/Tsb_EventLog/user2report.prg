/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Статистика выполнения(события программы) по операторам за периоды времени - кнопка "F5 Отчёты".
 * Execution statistics (program events) by operator over time periods - press the "F5 Reports" button.
 *
*/

#include "hmg.ch"
#include "TSBrowse.ch"

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myPathExportDoc()
   LOCAL cPath, cDir, cMsg

#ifdef KEY_ENG
   cDir := "User Logs"
   cMsg := "Cannot create folder for documents!;;"
#else
   cDir := "Журналы пользователей"
   cMsg := "Не могу создать папку для документов !;;"
#endif

   // Создание папки программы "Мои документы\"
   cPath := GetMyDocumentsFolder()    //System.MyDocumentsFolder
   cPath += "\" + cDir
   IF !hb_DirExists( cPath )
      CreateFolder( cPath )
      IF !hb_DirExists( cPath )
         AlertStop( cMsg + cPath, , , 64, {RED} )
      ENDIF
   ENDIF

RETURN cPath += "\"

////////////////////////////////////////////////////////////////////////////
// Меню отчётов для журнала-действий-пользователей
FUNCTION myTsbReport(oBrw,oWnd, ky, cn)
   LOCAL cForm, aDim, nY, nX, nPos, nBmpSize, nFSize, lExit, aFntExt
   LOCAL nPar, nVal, nChoice, cFunc, cPath, cTitle, cTag, cAls

   cPath := myPathExportDoc()   // путь записи файлов отчётов
   oBrw:Cargo:cPathExport := cPath
   cForm := oBrw:cParentWnd
   cForm := oWnd:Name
   cAls  := oBrw:cAlias
   cTag  := oBrw:Cargo:cIndxTag        // текущий тэг индекса
   nPos  := ky                         // убрать ошибку компиляции
   aDim  := {}
   // координаты вывода окна
   nY    := GetProperty(cForm, "Row") + GetTitleHeight()
   nY    += GetProperty(cForm, cn, "Row") + GetProperty(cForm, cn, "Height")
   nX    := GetProperty(cForm, "Col") + GetBorderWidth()
   nX    += GetProperty(cForm, cn, "Col") - 5 //+ GetProperty(cForm, cn, "Width")

#ifdef KEY_ENG
   cTitle := "User Action Event Log"                                // T-no choice, F-there is a choice
   AADD( aDim, { "bUser32" , " Operator List"                       , .F., "UserListDbf", "1" , 1 } )
   AADD( aDim, {                                                                                  } )
   AADD( aDim, { "none"    , "Reports by operators"                 , .T., "MsgDebug"   , "0" , 0 } )
   AADD( aDim, { "bExcel48", " Work for the period by operator"     , .F., "myReport1"  , "1" , 2 } )
   AADD( aDim, { "bExcel48", " Work for the period by all operators", .F., "myReport1"  , "2" , 3 } )
   AADD( aDim, {                                                                                  } )
   AADD( aDim, { "bDir48"  , " Folder with reports ..."             , .F., "myDirReport", "0" , 4 } )
#else
   cTitle := "Журнал событий действий пользователей"                // T-нет выбора, F-есть выбор
   AADD( aDim, { "bUser32" , " Список операторов"                   , .F., "UserListDbf", "1" , 1 } )
   AADD( aDim, {                                                                                  } )
   AADD( aDim, { "none"    , "Отчёты по операторам"                 , .T., "MsgDebug"   , "0" , 0 } )
   AADD( aDim, { "bExcel48", "  Работа за период по оператору"      , .F., "myReport1"  , "1" , 2 } )
   AADD( aDim, { "bExcel48", "  Работа за период по всем операторам", .F., "myReport1"  , "2" , 3 } )
   AADD( aDim, {                                                                                  } )
   AADD( aDim, { "bDir48"  , "  Папка с отчётами ..."               , .F., "myDirReport", "0" , 4 } )
#endif

   oBrw:Cargo:cMaskaFile := "UserLog"     // маска имени файла
   oBrw:Cargo:cNameForma := cTitle
   SetThemes(1)    // тема "Office 2003 theme" в ContextMenu
   //SetThemes(2)  // тема "Office 2000 theme" в ContextMenu
   //SetThemes(3)  // тема "Dark theme" в ContextMenu
   nPos     := { nY, nX }
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize + 2
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .T.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFntExt )
   IF nChoice > 0
      //cFunc  := aDim[nChoice,4] + "(" + HB_ValToExp(aDim[nChoice]) + ")"
      cFunc  := aDim[nChoice,4] + "( '" + cForm + "', " + HB_NtoS(aDim[nChoice,6]) + " )"
      oBrw:Cargo:cNameTitle := ALLTRIM(aDim[nChoice,2])
      //xRet := EVal( hb_MacroBlock( cFunc ) )
      //IF MyIsFunNoRun(cFunc)
      //   xRet := EVal( hb_MacroBlock( cFunc ), nChoice, aDim[nChoice] )
      //ELSE
      //   xRet := NIL
      //ENDIF
      nVal := aDim[nChoice,6]
      nPar := VAL(aDim[nChoice,5])
      //? ProcNL(), "====== ALIAS():", ALIAS()
      //MsgDebug(nChoice,nVal,"nPar=",nPar,ALIAS())
      //? ProcNL(), "====== ALIAS():", ALIAS()
      DbSelectArea(cAls)             // !!! иначе потеря алиаса / otherwise loss of alias
      IF nVal == 1
         UserListDbf(oBrw:cAlias)      // user2ListDbf.prg
      ELSEIF nVal == 2
         myReport1(oBrw , 1)
      ELSEIF nVal == 3
         myReport1(oBrw , 2)
      ELSEIF nVal == 4
         ShellExecute(, 'open',cPath, "", "" , 1 )
      ELSE
         AlertInfo("Ещё не сделано !;Not done yet !")
      ENDIF
   ENDIF

   DbSelectArea(cAls)
   //oBrw:uLastTag := cTag      // без этого индекс слетает
   OrdSetFocus(cTag)
   //oBrw:Reset()
   //oBrw:Display()
   //oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////////////////
// Оператор работает в ночную смену с 20:00 по 08:00 - нужно учитывать следующий день смены
// The operator works the night shift from 20:00 to 08:00 - the next day of the shift must be taken into account
FUNCTION myReport1(oBrw, nPar)
   LOCAL aDate, cAls, nUser, tTime, dDate1, dDate2, nSum, aRecno, cDat, cEnd
   LOCAL aNext, aIdS, nK, nI, nJ, a2Dim, nCode, nEve, cTime, aTime0, aTime2
   LOCAL aSumm, aRpt, aTime, nS1, nS2, nS3, cUser, xTime0, xTime2, aVal
   LOCAL nIUsr, a4User, nErr, cMaska, aTable, cWinTtl, aRet, cForm, aUser
   LOCAL aHead, aLang, cVal, aClr1, aClr2, lCalc, nReport, cMsg

   ? ProcNL(), oBrw:cAlias, nPar
   cAls   := oBrw:cAlias
   aDate  := { CTOD("06.09.25") , CTOD("07.09.25") }
   aUser  := { 101, "User 101" }
   tTime  := HB_DATETIME()
   dDate1 := aDate[1]
   dDate2 := aDate[2]
   nSum   := 0
   aRecno := {}
   aRpt   := {}
   a2Dim  := EVENTS_Dim(1)     // список событий, кроме 1,2 и 900, 990
                               // list of events except 1,2 and 900, 990
#ifdef KEY_ENG
   cMsg := "Error! No field for Reports!;OPERAT->LREPORT;;Contact the program developer"
#else
   cMsg := "Ошибка ! Нет поля для Отчётов !;OPERAT->LREPORT;;Обратится к разработчику программы"
#endif
   SELECT OPERAT
   nReport := FieldNum("LREPORT")
   IF nReport == 0
      AlertStop( cMsg, , , 64, {RED} )
      ? ProcNL(), cMsg
      RETURN NIL
   ENDIF
   lCalc   := .T.               // выбирать только по полю lCalc
   a4User  := UserList4x(lCalc) // -> user2filter.prg

   /////////////////////////////
   cForm := oBrw:cParentWnd
   aClr1 := oBrw:Cargo:aBClrForm  // цвет формы
   aClr2 := {  0,176,240}
   aRet  := Form_UserDate(cForm,nPar, aClr1, aClr2)
   IF LEN(aRet) == 0
      RETURN Nil
   ENDIF
   aUser := aRet[2]
   aDate := aRet[1]  ; dDate1 := aDate[1] ; dDate2 := aDate[2]

   IF nPar == 1        // по одному оператору
      //a4User := { {101, 11, "User 101", "---" } }
      a4User := { { aUser[1], 0, aUser[2], "---" } }
      cMaska := " " + a4User[1,3] + "  c: " + DTOC(dDate1) + " по: " + DTOC(dDate2)
   ELSE                // по всем операторам
      cMaska := "  c: " + DTOC(dDate1) + " по: " + DTOC(dDate2)
   ENDIF

   //SET SOFTSEEK  ON   // включает подвод SEEK до ближайшего большего ключа
   //                   // turns on SEEK advance to the nearest larger key

   // выборка из базы за период
   DBSELECTAREA(cAls)
   // уникальный список сессий программы за этот период
   // INDEX ON &("DTOS(DEVENT) + STR(IDEVENT)")  TAG DATE_UNI   FOR !Deleted() UNIQUE
   OrdSetFocus("DATE_UNI")
   aIdS := {}
   cDat := DTOS(dDate1)
   cEnd := DTOS(dDate2)
   ? "     SCOPE:",cDat, cEnd
   SET SCOPE TO cDat, cEnd
   DO WHILE !EOF()
      DO EVENTS
      //IF (cAls)->NUSER == nUser
         AADD( aIdS, (cAls)->IDEVENT )
      //ENDIF
      SKIP
   ENDDO
   ?? "aIdS=",aIdS
   //
   //INDEX ON &("DTOS(DEVENT) + STR(IDEVENT)") TAG DATEIDEV  FOR !Deleted()
   // INDEX ON &("DEVENT") TAG DATE_EV   FOR !Deleted()
   OrdSetFocus("DATE_EV")
   ? SPACE(5) + "Order =", OrdSetFocus(), dDate1, "-", dDate2
   GO TOP
   SEEK(dDate1)
   DO WHILE !EOF() .and. (cAls)->DEVENT >= dDate1 .AND. (cAls)->DEVENT <= dDate2
      DO EVENTS
      IF !Deleted()
         //IF (cAls)->NUSER == nUser
            AADD( aRecno, RECNO() )
         //ENDIF
      ENDIF
      SKIP
   ENDDO
   ? "     aRecno=", aRecno
   /*FOR nK := 1 TO LEN(aRecno)
      GOTO(aRecno[nK])
      DO EVENTS
      ? SPACE(5)+".", nK, (cAls)->IDEVENT, (cAls)->DEVENT, (cAls)->TEVENT
      ?? (cAls)->NEVENT          // код события            / event code
      ?? ALLTRIM((cAls)->CEVENT) // наименование события   / event name
      ?? (cAls)->CTIME           // время работы программы / program running time
   NEXT */

   // ищем закрытие смены юзера - следующий день
   // We are looking for the user change to close - the next day
   aNext := {}
   GO TOP
   SEEK(dDate2+1)
   DO WHILE !EOF() .and. (cAls)->DEVENT == dDate2+1
      DO EVENTS
      IF !Deleted()
         //IF (cAls)->NUSER == nUser
            AADD( aNext, RECNO() )
            AADD( aRecno, RECNO() )  // добавим к общему списку
         //ENDIF
      ENDIF
      SKIP
   ENDDO
   ? "     aNext=", aNext
   /*FOR nK := 1 TO LEN(aNext)
      GOTO(aNext[nK])
      ? SPACE(5)+".", nK, (cAls)->IDEVENT, (cAls)->DEVENT, (cAls)->TEVENT
      ?? (cAls)->NEVENT           // код события            / event code
      ?? ALLTRIM((cAls)->CEVENT)  // наименование события   / event name
      ?? (cAls)->CTIME            // время работы программы / program running time
   NEXT */

   FOR nIUsr := 1 TO LEN(a4User)
      aVal  := a4User[nIUsr]
      nUser := aVal[1]
      cUser := aVal[3]
      nErr  := 0                  // кол-во ошибок
      ? "      nI=", nIUsr, "User:", nUser, cUser
      //
      // создание отчёта
      aTime0 := {}                // время работы программы
      aTime2 := {}                // время работы юзера
      aSumm  := ARRAY(LEN(a2Dim))
      AFILL(aSumm, 0 )

      FOR nK := 1 TO LEN(aRecno)
         GOTO(aRecno[nK])
         IF (cAls)->NUSER == nUser

            nEve := (cAls)->NEVENT            // код события / event code
            IF nEve >= 900
              cTime := ALLTRIM((cAls)->CTIME)
              IF LEN(cTime) > 0               // время работы программы / program running time
                 AADD(aTime0, cTime )
              ENDIF
              cTime := ALLTRIM((cAls)->CTIME)
              IF LEN(cTime) > 0               // время работы юзера / user work time
                 AADD(aTime2, cTime )
              ENDIF
            ENDIF
            IF nEve == 990   // "Error exit !"
               nErr++        // кол-во ошибок
            ENDIF
            //
            nJ := 0
            FOR nI := 1 TO LEN(a2Dim)
               nCode := a2Dim[nI,1]
               IF nCode == nEve
                  nJ := nI
                  EXIT
               ENDIF
            NEXT
            IF nJ > 0 .AND. nJ <= LEN(aSumm)
               aSumm[nJ] ++
            ENDIF
            //? SPACE(5)+".", nK, aRecno[nK]
            //?? nEve                    // код события            / event code
            //?? ALLTRIM((cAls)->CEVENT) // наименование события   / event name
            //?? (cAls)->CTIME           // время работы программы / program running time

         ENDIF
         DO EVENTS
      NEXT
      //
      // расчёт времени
      nS1 := nS2 := nS3 := 0
      FOR nK := 1 TO LEN(aTime0)
         cTime := SUBSTR(aTime0[nK],1,8)
         aTime := HB_ATokens(cTime, ":")
         //? nK, cTime, HB_ValToExp(aTime)
         IF LEN(aTime) >= 3
            nS1 += Val(aTime[1])
            nS2 += Val(aTime[2])
            nS3 += Val(aTime[3])
         ENDIF
      NEXT
      //? "     aTime0 Время:", nS1, nS2, nS3
      xTime0 := CalcTime(nS1, nS2, nS3)
      //
      nS1 := nS2 := nS3 := 0
      FOR nK := 1 TO LEN(aTime2)
         cTime := SUBSTR(aTime2[nK],1,8)
         aTime := HB_ATokens(cTime, ":")
         //? nK, cTime, HB_ValToExp(aTime)
         IF LEN(aTime) >= 3
            nS1 += Val(aTime[1])
            nS2 += Val(aTime[2])
            nS3 += Val(aTime[3])
         ENDIF
      NEXT
      //? "     aTime2 Время:", nS1, nS2, nS3  // "0:26:987"
      xTime2 := CalcTime(nS1, nS2, nS3)
      //
      // создание массива просчитанного отчёта
      aVal := { cUser, xTime0, xTime2  }
      FOR nI := 1 TO LEN(aSumm)
         AADD( aVal, aSumm[nI] )
      NEXT
      AADD( aVal, nErr )   // кол-во ошибок
      //
      AADD( aRpt, aVal )
      //? "     aSumm=" , aSumm  ; ?v aSumm
      //? "     aTime0=", aTime0 ; ?v aTime0
      //? "     aTime2=", aTime2 ; ?v aTime2

   NEXT
   //? "     aRpt="  , aRpt   ; ?v aRpt
   //
#ifdef KEY_ENG
   aLang := { "Operators", "Program;working;time", "Operator;working;time"}
#else
   aLang := { "Операторы", "Время;работы;программы",  "Время;работы;оператора"}
#endif
   // шапка отчёта
   aHead := aLang
   FOR nK := 1 TO LEN(a2Dim)
      cVal := a2Dim[nK,2]
      cVal := ATREPL( " ", cVal, ";" )
      AADD(aHead, cVal)
   NEXT
   AADD(aHead, "###" )   // кол-во ошибок
   //? "     aHead="  , aHead   ; ?v aHead

   cForm   := oBrw:cParentWnd
   //           1/2   Работа за период по .....
   aTable  := { nPar, oBrw:Cargo:cNameTitle + " " + cMaska }
   cWinTtl := oBrw:Cargo:cNameForma // Журнал событий
   aRet    := {aRpt, aTable, cWinTtl, HB_NtoS(nPar) + ProcName() }
   cForm   := Table_Rprt(cForm,aRet,aHead,oBrw)     // -> user2report_tsb.prg
   //MsgDebug(aRpt)

RETURN Nil

///////////////////////////////////////////////////////////////
FUNCTION CalcTime(nS1, nS2, nS3)
   LOCAL nTotal, nHours, nMinutes, nSeconds, cResult

   //? "Time:", nS1, nS2, nS3  // "0:26:987"
   nTotal := nS1 * 3600 + nS2 * 60 + nS3
   //? "Секунд всего: ", nTotal

   nHours := Floor(nTotal / 3600)
   nTotal := nTotal % 3600

   nMinutes := Floor(nTotal / 60)
   nTotal := nTotal % 60

   nSeconds := Floor(nTotal)

   //? nHours, nMinutes, nSeconds
   cResult := PadL(nHours, 2, "0") + ":"
   cResult += PadL(nMinutes, 2, "0") + ":"
   cResult += PadL(nSeconds, 2, "0")

RETURN cResult

/////////////////////////////////////////////////////////////////////////
FUNCTION Form_UserDate(cParentWin, nFlOper, aClr1, aClr2 )
   LOCAL cIco, cIcoBig, c2Title, cFont, nFSize, aBackColor, aBackUpColor
   LOCAL hWnd, nH, nW, nG, nRow, nCol, aLblColor, nWlbl, nCol2
   LOCAL cIco3x1, cIco3x2, cIco2x1, cIco2x2, cButtCapt, nWBth
   LOCAL nHButt, nRowButt, nBFSize, nBFont, cText, nWButt, nWDate
   LOCAL cFileMemo, cFileIni2, cMetkaIni, aRetPrn := {}
   LOCAL aGrOverB2, aGrFillB2, aGrOverBEx, aGrFillBEx
   LOCAL aGBBackColor := { WHITE, SILVER, YELLOW }
   LOCAL aGBFontColor := { BLACK, YELLOW, BLUE   }
   LOCAL c1Title, dDate1, dDate2, cFntTitle
   LOCAL a3Oper, aColor, cFClsTtl, nHLbl, aLang, aBtnLang, owc

   cIco         := "iLogfile48"
   cFont        := "Tahoma"
   nFSize    := App.Cargo:nDefFontSize + 2
   nBFont       := "Comic Sans MS"
   nBFSize      := nFSize + 2
   aBackColor   := aClr1 //{141,179,226}  // Цвет фона всей формы - как форма таблицы
   aBackUpColor := aClr2 //{  0,176,240}  // Цвет верха фона формы - голубой, как SKYPE
   aLblColor    := BLUE                   // Цвет Label_*
   cFntTitle    := "Comic Sans MS"
   cFClsTtl     := NAVY
   cIcoBig      := 'iUsers48x1'
   cFileMemo    := App.Cargo:cPathTemp + "Seek_UserDate.txt"
   cFileIni2    := ChangeFileExt( cFileMemo, ".ini"  )
   cMetkaIni    := "12.09.25"  // для добавлении нового параметра - изменить метку
                               // to add a new parameter - change the label
   a3Oper       := { 0 , "", "???" }
   dDate1       := dDate2 := CTOD("")
   nHLbl        := nFSize*2
   nG           := 20          // отступы по краям окна / margins at the edges of the window

#ifdef KEY_ENG
   c1Title := 'Report parameters for'
   c2Title := 'user event log'
   aLang := { "Operator:", "Calculation date from:", "Calculation date to:" }
   aBtnLang := { "Select", "Cancel" }
#else
   c1Title  := 'Параметры отчёта для'
   c2Title  := 'журнала событий пользователей'
   aLang    := { "Оператор:", "Дата расчёта с:", "Дата расчёта по:" }
   aBtnLang := { "Выбрать", "Отмена" }
#endif

   // считать введённые ранее данные
   IniLoadFileForm_UserDate( cFileIni2, cMetkaIni, @a3Oper, @dDate1, @dDate2 )
   IF !IsArray(a3Oper) .OR. LEN(a3Oper) == 0
      a3Oper := { 0 , "", "???" }
   ENDIF

   // Calculate window width
   cText  := "Calculation date from:HH"
   nWlbl  := GetTxtWidth( cText, nFSize, cFont, .F. )
   nWDate := GetTxtWidth( "HHdd'.'MMMM'H'yyyyHHH", nFSize, cFont, .T. )
   cText  := "Operator's full name - who did it?"
   nWBth  := GetTxtWidth( cText, nFSize, cFont, .T. )
   IF nWBth > nWDate
      nWDate := nWBth
   ENDIF
   nW    := nG*2 + nWlbl + nG/2 + nWDate + GetBorderWidth()*2

   DEFINE WINDOW Form_UserDate                       ;
      At 0, 0 WIDTH nW HEIGHT 500                    ;
      TITLE "" ICON cIco                             ;
      MODAL NOSIZE                                   ;
      FONT cFont SIZE nFSize BACKCOLOR aBackColor    ;
      ON INIT    {|| _wPost( 0) }                    ;
      ON RELEASE {|| _wSend(90) }
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:cPrWin := cParentWin
      hWnd := GetFormHandle('Form_UserDate')
      nW   := This.ClientWidth
      nH   := This.ClientHeight

      @ 0 , 110 LABEL Label_0 WIDTH nW HEIGHT 110 ;
        VALUE '' BACKCOLOR aBackUpColor

      DRAW ICON IN WINDOW Form_UserDate AT 0, 0 PICTURE cIcoBig ;
         WIDTH 110 HEIGHT 110 COLOR aBackUpColor

      @ 0 , 110 LABEL Label_01 WIDTH nW-110 HEIGHT 60   ;
        VALUE c1Title FONT cFntTitle SIZE nFSize + 6 BOLD ;
        FONTCOLOR cFClsTtl BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      @ 55 , 110 LABEL Label_02 WIDTH nW-110 HEIGHT 50   ;
        VALUE c2Title FONT cFntTitle SIZE nFSize + 6 BOLD ;
        FONTCOLOR cFClsTtl BACKCOLOR aBackUpColor CENTERALIGN VCENTERALIGN

      nCol  := nG := 20
      nCol2 := nCol + nWlbl + nG/2
      nRow  := This.Label_0.Height + nG  // отступ сверху (начало граф)

      // ------- 1) Оператор: = -------------
      @ nRow, nCol LABEL Label_Who VALUE aLang[1] WIDTH nWlbl HEIGHT nHLbl ;
        FONTCOLOR BLACK TRANSPARENT VCENTERALIGN RIGHTALIGN

      @ nRow, nCol2 BUTTONEX Button_Who WIDTH nWBth HEIGHT nHLbl ;
        CAPTION "???" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP    ;
        ACTION {|| a3Oper := UserList2Dim(.T.) /*DbfSelectOperat()*/ ,;
                   cText := IIF( LEN(a3Oper) > 0, a3Oper[2], "" ) ,;
                   This.Button_Who.Caption  := cText            ,;
                   This.Label_0.Setfocus  }
        This.Button_Who.Caption := a3Oper[2]
      // все операторы / all operators
      IF nFlOper > 1
         This.Label_Who.Hide
         This.Button_Who.Hide
      ENDIF

      // ------- 2)  Дата расчёта с: -------------
      nRow  += This.Label_Who.Height + nG
      @ nRow, nCol LABEL Label_Date1 VALUE aLang[2] WIDTH nWlbl HEIGHT nHLbl  ;
        FONTCOLOR BLACK TRANSPARENT VCENTERALIGN RIGHTALIGN

      @ nRow, nCol2 DATEPICKER GB_Date1 VALUE dDate1 WIDTH nWDate HEIGHT nHLbl ;
                 DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE BOLD             ;
                 ON CHANGE {|| dDate1 := This.GB_Date1.Value }

      // -------- 3) Дата расчёта по: --------------------
      nRow += This.Label_Date1.Height + nG
      @ nRow, nCol LABEL Label_Date2 VALUE aLang[3] WIDTH nWlbl HEIGHT nHLbl  ;
        FONTCOLOR BLACK TRANSPARENT VCENTERALIGN RIGHTALIGN

      @ nRow, nCol2 DATEPICKER GB_Date2 VALUE dDate2 WIDTH nWDate HEIGHT nHLbl ;
                 DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE BOLD             ;
                 ON CHANGE {|| dDate2 := This.GB_Date2.Value }

      /////////////////////// Button ////////////////////////////
      nWButt := 260  // ширина кнопок внизу
      nHButt := 86   // высота кнопок внизу
      nRowButt := nH - nHButt - 20 // начало кнопок на форме

      nCol := ( nW  - nWButt*2 )/2 - 30
      cButtCapt := aBtnLang[1]
      cIco2x1   := "iFindTsb64x1"  ;  cIco2x2 := "iFindTsb64x2"
      aColor    := {0,176,240}
      aGrOverB2 := { { 0.5, CLR_WHITE, aColor    }, { 0.5, aColor   , CLR_WHITE } }
      aGrFillB2 := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }

      @ nRowButt, nCol  BUTTONEX BUTTON_Save WIDTH nWButt HEIGHT nHButt                ;
         CAPTION cButtCapt ICON cIco2x1 FONTCOLOR BLACK                                ;
         FONT nBFont SIZE nBFSize BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP             ;
         BACKCOLOR aGrOverB2  GRADIENTFILL aGrFillB2                                   ;
         ON MOUSEHOVER ( This.Fontcolor := YELLOW, This.Icon := cIco2x2, This.GradientFill := aGrFillB2 ) ;
         ON MOUSELEAVE ( This.Fontcolor := BLACK , This.Icon := cIco2x1, This.GradientOver := aGrOverB2 ) ;
         ACTION {|| SetProperty(ThisWindow.Name, This.Name, "Enabled", .F.)            ,;
                    aRetPrn := Ret2UserDate(a3Oper, dDate1, dDate2, aLang)             ,;
                    IniSaveFileForm_UserDate(cFileIni2,cMetkaIni,a3Oper,dDate1,dDate2) ,;
                    SetProperty(ThisWindow.Name, This.Name, "Enabled", .T.)            ,;
                    IIF( LEN(aRetPrn)==0, Nil, ThisWindow.Release )         }

      nCol := ( nW  - nWButt*2 )/2 + nWButt + 30
      cButtCapt  := aBtnLang[2]
      cIco3x1    := "Return64x2"   ;  cIco3x2 := "Return64x1"
      aGrOverBEx := { { 0.5, CLR_WHITE, CLR_HRED  }, { 0.5, CLR_HRED , CLR_WHITE } }
      aGrFillBEx := { { 0.5, CLR_HRED , CLR_WHITE }, { 0.5, CLR_WHITE, CLR_HRED  } }

      @ nRowButt, nCol  BUTTONEX BUTTON_Exit WIDTH nWButt HEIGHT nHButt                ;
         CAPTION cButtCapt ICON cIco3x1 FONTCOLOR BLACK                                ;
         FONT nBFont SIZE nBFSize BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP             ;
         BACKCOLOR aGrOverBEx  GRADIENTFILL aGrFillBEx                                 ;
         ON MOUSEHOVER ( This.Fontcolor := YELLOW, This.Icon := cIco3x2, This.GradientFill := aGrFillBEx ) ;
         ON MOUSELEAVE ( This.Fontcolor := BLACK , This.Icon := cIco3x1, This.GradientOver := aGrOverBEx ) ;
         ACTION {|| SetProperty(ThisWindow.Name, This.Name, "Enabled", .F.)  ,;
                    INKEYGUI(200), aRetPrn := {}  , ThisWindow.Release }

      ON KEY ESCAPE OF Form_UserDate ACTION {|| aRetPrn := {} , ThisWindow.Release }

      WITH OBJECT This.Object
        :Event( 0, {|ow| ow:Setfocus("Label_0"), DoEvents() })

        :Event( 1, {|ow,ky,cn| _SetThisFormInfo(ow) , MsgDebug(ow:Name,ky,cn) , _SetThisFormInfo(), ;
                               This.&(cn).Enabled := .T. , ow:Setfocus("Buff"), DoEvents() })

        :Event(90, {|ow,ky| // ON Release windows
                            Local cm
                            cm := ProcNL()
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                            DO EVENTS
                            Return Nil
                            })

        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   CENTER WINDOW Form_UserDate
   ACTIVATE WINDOW Form_UserDate

RETURN aRetPrn

/////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Ret2UserDate( a3Oper, dDate1, dDate2, aLang )
   LOCAL d1, d2, aMsg, cMsg

#ifdef KEY_ENG
   aMsg := { "ERROR !;", "No date entered - ",;
             "You must enter a date for calculation!;" }
#else
   aMsg := { "ОШИБКА !;", "Не введена дата - ",;
             "Необходимо ввести дату для расчёта!;" }
#endif

   cMsg := ""
   d1   := dDate1 //This.GB_Date1.Value
   d2   := dDate2 //This.GB_Date2.Value

   IF d1 == CTOD("")
      cMsg += aMsg[2] + aLang[2] + ";;"
   ENDIF

   IF d2 == CTOD("")
      cMsg += aMsg[2] + aLang[3] + ";;"
   ENDIF

   IF LEN(cMsg) > 0
      cMsg += aMsg[3]
      AlertStop( aMsg[1] + cMsg, , , 64, {RED} )
      RETURN {}
   ENDIF

RETURN { {d1,d2} , a3Oper}

///////////////////////////////////////////////////////////////////////
// считать данные с ини-файла
Static Function IniLoadFileForm_UserDate(cFileIni,cMetkaIni, a3Oper, dDate1, dDate2)
   LOCAL cStr, aRet
   IF !FILE(cFileIni)
      IniSaveFileForm_UserDate(cFileIni,cMetkaIni, a3Oper, dDate1, dDate2)
   ENDIF

   cStr := ALLTRIM( hb_MemoRead(cFileIni) )
   IF LEN(cStr) == 0
     // нет данных
   ELSE
      // чтобы при добавлении нового параметра была смена без ошибки
      IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0 .AND. AT( cMetkaIni, cStr ) > 0
         aRet      := &cStr
         cMetkaIni := aRet[1]  // чтобы при добавлении нового параметра была смена без ошибки
         a3Oper    := aRet[2]
         dDate1    := aRet[3]
         dDate2    := aRet[4]
      ELSE
        // нет данных
      ENDIF
   ENDIF

Return Nil

///////////////////////////////////////////////////////////////////////////////////
Static Function IniSaveFileForm_UserDate(cFileIni,cMetkaIni,a3Oper,dDate1,dDate2)
   LOCAL aSave
   // значения первоначальные
   aSave := {cMetkaIni,a3Oper,dDate1,dDate2}
   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )

Return Nil

