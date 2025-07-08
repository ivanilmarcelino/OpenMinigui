/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * The idea of 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/
#define _HMG_OUTLOG

#include "hmg.ch"

////////////////////////////////////////////////////////////
// Проверка запуска программы на ВТОРУЮ копию программы
// Check the start of the program on the second copy of the program
FUNCTION OnlyOneInstance( cAppTitle, lMsg , nDataId )
   LOCAL hWnd, lRet, cMsg, cData
   DEFAULT cAppTitle := "Not set - cAppTitle", lMsg := .T.

   cMsg := App.ExeName + ";;"
   cMsg += "Trying to run a second copy of the program !;Start denied !;"
   cMsg += "Opening an already running program !;;"
   cMsg += "Попытка запуска второй копии программы !;Отказано в запуске !;"
   cMsg += "Открываю уже запущенную программу!"
   hWnd := FindWindowEx( ,,, cAppTitle )
   lRet := !Empty( hWnd )

   ? "   #### " + ProcNL(), cAppTitle, hWnd, IsWindowVisible(hWnd)
   IF lRet
      IF !Empty(lMsg)
         AlertStop(cMsg, cAppTitle )
         // отправить сообщение ранее запущенной программы
         cData := cFileNoPath(App.ExeName) + " | "
         cData += HB_TSTOSTR( HB_DATETIME() )
         cData += " - Message WM_SetFocus !"
         cData += "(no display in tray)"
         // Transfer data to window -> APP_TITLE
         SendMessageData( hWnd, cData, nDataId )
         ? "   #### " + ProcNL(), cAppTitle, "=> отправлено сообщение: ", cData
      ENDIF
      //IF IsIconic( hWnd ) ; _Restore( hWnd )
      //ENDIF
      SetForeGroundWindow( hWnd )
      ? "   #### hWnd =", hWnd, "ExitProcess( 0 )" + CRLF + CRLF + CRLF
      ExitProcess( 0 )
   ENDIF

Return NIL

//////////////////////////////////////////////////////////////////////////
FUNCTION myExitWithoutUserWork(nMinutes)     // Выход без работы юзера
   LOCAL cMsg, cExeRun, cParam, nLenSpc := App.Cargo:nLenSpc
   LOCAL cVal, cTime := HB_TSTOSTR( HB_DATETIME() )
   LOCAL cFileTxt, cLogEvents := App.Cargo:cLogEvents

   cMsg := "Закрытие из-за бездействия пользователя!"
   cVal := cTime + " | " + PADR(cMsg,nLenSpc) + " | " + ProcNL()
   STRFILE( cVal + CRLF, cLogEvents, .T. ) ; ? cVal ; ? REPL(".",20)

   cExeRun := App.Cargo:cExeAlert
   IF FILE(cExeRun)
      cMsg   := "ВНИМАНИЕ!;Программа закрыта из-за бездействия пользователя;"
      cMsg   += "в течении " + HB_NtoS(nMinutes) + " минут !;"
      cMsg   += "ATTENTION!;The program is closed due to user inaction;"
      cMsg   += "within " + HB_NtoS(nMinutes) + " minutes !;"
      cMsg   += "Выход/Exit - " + cTime
      cParam := '-info "' + cMsg + '"'
      cMsg   := "ShellExecute()=" + cFileNoPath(cExeRun) + "," + cParam
      ? Space(Len(cTime)) + " | " + PADR(cMsg,nLenSpc) + " | " + ProcNL()
      ShellExecute( , 'open', cExeRun, cParam, , SW_SHOWNORMAL)
   ELSE
      cFileTxt := ChangeFileExt( ExeName(), '.txt' )
      cMsg     := AtRepl( ";", cMsg, CRLF )
      HB_MemoWrit( cFileTxt , cMsg + CRLF)
      wApi_Sleep(100)
      ShellExecute(0,"Open",cFileTxt,,,SW_SHOWNORMAL)
   ENDIF
   wApi_Sleep(100)

RETURN NIL

//////////////////////////////////////////////////////////////////////////
FUNCTION myEventTreatment(cData)
   LOCAL cMsg, cType, aParts, cForm, hForm, lForm, cMemo, cLog, cGetMemo, ow
   //!!! тут не должны создаваться окна, т.к. меняется СРЕДА This, для
   //!!! работающих сейчас окон. Будут валится из за неопределённости
   //!!! появления WM_COPYDATA. Здесь надо сохранить данные в базе/файле и
   //!!! поставить метку, что обновление по WM_COPYDATA было и потом
   //!!! что то с этим сделать
   cForm    := App.Cargo:cCopyData_Wnd           //"Form_ListCD"
   cMemo    := App.Cargo:cCopyData_Memo          //"Edit_Memo"
   cLog     := App.Cargo:cCopyDataLog            // _copydata.log
   cGetMemo := ""
   STRFILE( cData + CRLF, cLog, .T. )           // записать в журнал
   IF ( lForm := _IsWindowDefined( cForm ) )
      hForm := GetFormHandle(cForm)             //!!!
      IF GetControlIndex(cMemo, cForm ) > 0
         cGetMemo := GetProperty(cForm, cMemo, "Value")
      ENDIF
   ENDIF

   aParts := HB_ATokens(cData, "|")
   IF LEN(aParts) == 9
      cMsg := "ERROR! Wrong message !;"
      cMsg += "No separator sign '|';"
      cMsg += "cData= " + cData + CRLF

      IF lForm
         IF GetControlIndex(cMemo, cForm ) > 0
            SetProperty(cForm, cMemo, "Value", cGetMemo + cMsg)
         ENDIF
      ENDIF
      //AlertStop(cMsg,ProcNL())
      STRFILE( cMsg + CRLF, cLog, .T. )        // записать в журнал
      RETURN NIL
   ENDIF

   IF lForm
      IF _IsControlDefined(cMemo, cForm)
         SetProperty(cForm, cMemo, "Value", cGetMemo + cData + CRLF)
      ENDIF
   ENDIF

   DO EVENTS
   wApi_Sleep(100)

   cType := ALLTRIM( UPPER(aParts[1]) )
   cMsg  := aParts[2]

   ow := App.Cargo:oWinMain  // объект MAIN окна
   // :Event( 51, {|ow,ky,as| ....  // icon ky =  1 -> NIIF_INFO        // -> main.prg
                                    // as := {"Message","Title"}
   // отправить сообщение MAIN окну
   //_wPost(51, ow, { 1, cMsg, "Processing " + cType } ) // меняет This среду
   _pPost(51, 0, { 1, cMsg, "Processing " + cType } )    // НЕ меняет This среду

   IF cType == UPPER("Notepad.exe")
      ShellExecute(0,"Open",cLog,,,SW_SHOWNORMAL)
   ELSEIF cType == UPPER("WinWord.exe")
   ELSEIF cType == UPPER(cFileNoPath(App.ExeName))     // Demo_timer.exe
      IF UPPER("WM_SetFocus") $ UPPER(cMsg)
         MMP2Foreground()  // окно на передний план
      ELSEIF UPPER("Button_Sample3") $ UPPER(cMsg)     // Demo_timer.exe
         my_Standard3(,,"TSB3_ALS",)                   // -> table3.prg
      ENDIF
   ELSE
      ? ProcNL(), "cForm=",cForm,IsWindowVisible( hForm )
      IF lForm .and. IsWindowVisible( hForm ) //GetProperty( cForm, "Visible" )
         IF IsIconic( hForm ) ; _Restore( hForm )
         ENDIF
      ENDIF
   ENDIF
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////
FUNCTION MMP2Foreground()
   LOCAL oMMP := App.Cargo:oMainMenuProg    // объект окна главного меню программы
   LOCAL cMMP := App.Cargo:cMainMenuProg    // имя окна главного меню программы Forma_Main
   LOCAL oFrm, oDlg, cFrm := cMMP, hFrm, cNam, cFocu, cTyp

   FOR EACH oFrm IN HMG_GetForms(, .T.)
       IF "oDlg" $ oFrm:Name
          oDlg := oFrm
       ENDIF
   NEXT

   ? repl("^",50) ; _o2log("Form*") ; ?
   ? "App.Cargo:cFormGotFocus =", App.Cargo:cFormGotFocus

   IF     !Empty(oDlg)
      cFrm := oDlg:Name
   ELSEIF !Empty(App.Cargo:cFormGotFocus)
      cFrm := App.Cargo:cFormGotFocus
   ENDIF

   DoMethod(cFrm, "Minimize") ; DO EVENTS
   DoMethod(cFrm, "Restore")  ; DO EVENTS

   hFrm := GetFormHandle(cFrm)
   SetForegroundWindow( hFrm )

   SetProperty(cFrm, "Topmost", .T.)
   DoMethod(cFrm, "SetFocus") ; DO EVENTS

   FOR EACH cNam IN HMG_GetFormControls(cFrm)
       ? hb_enumindex(cNam), cNam
       cTyp := GetControlType(cNam, cFrm)
       IF !Empty(cFocu)
       ELSEIF "TBROWSE" $ cTyp ; cFocu := cNam
       ELSEIF "BUTT" $ cTyp    ; cFocu := cNam
       ENDIF
       ?? cTyp, cFocu
   NEXT

   IF !Empty(cFocu)
      DoMethod(cFrm, cFocu, "SetFocus") ; DO EVENTS
   ENDIF

   SetProperty(cFrm, "Topmost", .F.) ; DO EVENTS

   ? repl("^",50) ; ?

RETURN NIL
////////////////////////////////////////////////////////////
// закрыть все открытые окна и заново запустить Form_Menu_Main(ow)
FUNCTION RestartMainMenu()
   LOCAL oMMP := App.Cargo:oMainMenuProg     // объект окна главного меню программы
   LOCAL cMMP := App.Cargo:cMainMenuProg     // имя окна главного меню программы
   LOCAL aForm, nI, cMsg

   ? ProcNL(), cMMP, "= OnInterActiveClose=Nil, OnRelease=Nil"

   cMsg := "ВНИМАНИЕ !; Сейчас будет изменено разрешение экранных форм;"
   cMsg += "ТОЛЬКО этой программы на " + App.Cargo:cDisplayMode
   cMsg += ";Окно MAIN формы: " + App.Cargo:cWinMain + " не меняется !;"
   cMsg += ";При открытии других окон в программе необходимо заменить:;"
   cMsg += "System.ClientWidth  на App.Cargo:aDisplayMode[1];"
   cMsg += "System.ClientHeight на App.Cargo:aDisplayMode[2];"
   cMsg += ";ATTENTION!; The resolution of screen forms will now be changed;"
   cMsg += "ONLY this program on " + App.Cargo:cDisplayMode
   cMsg += ";Window MAIN form: " + App.Cargo:cWinMain + " does not change!;"
   cMsg += ";When opening other windows in the program, you must replace:;"
   cMsg += "System.ClientWidth  on App.Cargo:aDisplayMode[1];"
   cMsg += "System.ClientHeight on App.Cargo:aDisplayMode[2]"
   AlertInfo(cMsg)

   // определяем новые обработчики окна
   SetProperty(cMMP, "OnRelease"         , {||Nil})
   SetProperty(cMMP, "OnInterActiveClose", {||Nil})

   aForm := HMG_GetForms()
   ? "     aForm=", aForm, HB_ValToExp(aForm)
   ? "     Release form:"

   FOR nI := 1 TO LEN(aForm)
      IF _HMG_aFormType[nI] # "A" .and. _IsWindowDefined( aForm[nI] )
         DoMethod( aForm[nI], "Release" )
         DO EVENTS
         wApi_Sleep(200)
         ?? aForm[nI]+","
      ENDIF
   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetDisplayModeScreen()
   RETURN { "640x480"  ,  "768x560",  "800x600", "1024x768", "1280x720" ,;
            "1280x768" , "1280x800", "1280x960","1280x1024", "1360x768" ,;
            "1366x768" ,"1440x1080", "1600x900","1600x1024", "1680x1054",;
            "1940x1080","1920x1440","2560x1440","2580x1080", "3440x1440",;
            "3840x2160" }

///////////////////////////////////////////////////////////////////////////////
FUNCTION ChangeClientModeDisplay()
   LOCAL nI, nPos, nBmpSize, nFSize, nChoice, lExit, cRet, aDim
   LOCAL aMode := GetDisplayModeScreen()
   LOCAL cFile := ChangeFileExt( App.ExeName, ".display" )
   LOCAL cForm := ThisWindow.Name

   aDim := {}
   FOR nI := 1 TO LEN(aMode)                        // .T.- no choice .F.- there is a choice
      AADD( aDim, {"Image.bmp", "Client window resolution - " + aMode[nI], .F., "MsgDebug()", nI } )
   NEXT

   nPos     := 3  // window position type
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col
   nBmpSize := 32
   nFSize   := 16
   lExit    := .F.   // additional menu in Context Menu - exit
   //nFSize   := ModeSizeFont() + 4

   //SetThemes(2)  // theme "Office 2000 theme" в ContextMenu
   SetThemes(3)    // theme "Dark theme" в ContextMenu

   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit )

   IF nChoice > 0
      cRet := aMode[nChoice]
      aDim := { VAL(SUBSTR(cRet,1,AT("x",cRet)-1)) ,;
                VAL(SUBSTR(cRet,AT("x",cRet)+1)) }
      App.Cargo:aDisplayMode := aDim
      App.Cargo:cDisplayMode := cRet
      HB_MemoWrit( cFile, HB_ValToExp( App.Cargo:aDisplayMode ) )
   ELSE
      cRet := App.Cargo:cDisplayMode
   ENDIF

RETURN cRet

////////////////////////////////////////////////////////////////////////
FUNCTION oGetForms(lChar)
   LOCAL aForms := {}, cRet := "", ow, aw, lVsbl, nI, lDel
   Default lChar := .T.

   FOR EACH ow IN HMG_GetForms( , .T. )
      nI    := hb_enumIndex()
      lVsbl := IsWindowVisible( GetFormHandle( ow:Name ) )
      lDel  := _HMG_aFormDeleted[nI]
      AAdd(aForms, {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title} )
   NEXT
   IF lChar
      FOR EACH aw IN aForms
          cRet += hb_valtoexp(aw) + CRLF
      NEXT
      RETURN cRet
   ENDIF

RETURN aForms

/////////////////////////////////////////////////////////////////////////////
FUNCTION PromptGetForms()
   LOCAL nH, nW, nG, nWBtn, nHBtn, cIco, cTitle, nY, nX, nK, nL, cTxt, nI
   LOCAL cFont, nFSize, cBFont, nBFSize, aBackColor, aBtn, cForm, cObj
   LOCAL aFntClr, aBtnBClr, aList, o

   ? "-->>", ProcNL(), ThisWindow.Name

   cIco       := "i_About32"
   cTitle     := 'Список окон этой программы / '
   cTitle     += 'List of windows of this program'
   cFont      := App.Cargo:cDefFontName
   nFSize     := App.Cargo:nDefFontSize
   cBFont     := "DejaVu Sans Mono"             // шрифт для кнопок
   nBFSize    := App.Cargo:nBtnFontSize + 2
   aBackColor := GREY                           // цвет фона всей формы
   aFntClr    := { WHITE, YELLOW      }         // цвет фонта кнопок
   aBtnBClr   := { {94,59,185}, BLACK }         // цвет фона кнопок
   nW         := App.Cargo:aDisplayMode[1]      // ширина окна - потом уменьшим
   nH         := App.Cargo:aDisplayMode[2]      // высота окна - потом уменьшим
   nG         := 20                             // отступ
   cForm      := "Form_ListWin" 
   aBtn       := oGetForms(.F.)

   nWBtn := nHBtn := nL := 0
   nK    := Len(aBtn)
   aList := {}
   FOR nI := 1 TO nK
      nWBtn := MAX(LEN(aBtn[nI,7]), nWBtn)
      nHBtn := MAX(LEN(aBtn[nI,3]), nHBtn)
   NEXT
   nWBtn := IIF( nWBtn > 70, 70, nWBtn)
   // можно выкинуть лишние строки из aBtn с lDel == .T. и lVsbl := .F., например
   FOR nI := 1 TO nK
      //AAdd(aForms, {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title} )
      cTxt := STR(nI,2) + ") " + PADR(aBtn[nI,3], nHBtn + 1)
      cTxt += ", " + PADR(aBtn[nI,7], nWBtn + 1)
      cTxt += ", [" + aBtn[nI,2] + "] "
      cTxt += CVALTOCHAR(aBtn[nI,4]) + " "
      cTxt += CVALTOCHAR(aBtn[nI,5]) + " "
      cTxt += HB_NtoS(aBtn[nI,6]) + " "
      nL   := MAX(Len(cTxt),nL)
      AADD( aList, cTxt )
      ? nI, nL, Len(cTxt)
   NEXT
   // добавим кнопку 
   AADD( aList, PADC("EXIT",nL) )
   AADD(aBtn, {Len(aBtn) + 1, "", "", .F., .F., 0, ""})

   cTxt  := REPL("a",nL)
   nWBtn := GetTxtWidth( cTxt, nBFSize+2, cBFont, .F. ) + nG  // получить Width текста
   nW    := nWBtn + nG*2 + 10 + GetBorderWidth()*2            // ширина окна

   DEFINE WINDOW &cForm At nY, nX WIDTH nW HEIGHT nH                   ;
      TITLE cTitle ICON cIco                                           ;
      MODAL NOSIZE                                                     ;
      BACKCOLOR aBackColor                                             ;
      FONT cFont SIZE nFSize                                           ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name }           ;
      ON INIT     {|| This.Topmost := .F., DoEvents(), _wPost(0) }     ;
      ON RELEASE  {|| AEval({91,92,93}, {|n| _wSend(n), DoEvents()}) } ; // executed before destroying the window

      This.Cargo := oHmgData()
      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nY    := nX := nG
      nHBtn := 40
      nWBtn := nW - nG * 2

      @ 0,0 Label Buff Value "" AUTOSIZE

      nK := Len(aList)
      FOR nI := 1 TO nK

         cObj := "Btn_" + StrZero(nI, 2)
         cTxt := aList[ nI ]

         @ nY, nX BUTTONEX &cObj WIDTH nWBtn HEIGHT nHBtn CAPTION cTxt  ;
           ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                 ;
           FONTCOLOR aFntClr[1] BACKCOLOR aBtnBClr[1]                   ; 
           FONT cBFont SIZE nBFSize                                     ;
           ACTION  {|| This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) }       ;
           ON MOUSEHOVER ( This.Fontcolor := This.Cargo:aFClr[2], This.FontSize := This.Cargo:nFSize+2, This.Backcolor := This.Cargo:aBClr[2] ) ;
           ON MOUSELEAVE ( This.Fontcolor := This.Cargo:aFClr[1], This.FontSize := This.Cargo:nFSize  , This.Backcolor := This.Cargo:aBClr[1] ) ;
           ON INIT {|| 
                    This.Cargo := oHmgData() 
                    This.Cargo:cObj := This.Name
                    This.Cargo:oCtl := This.Object       
                    Return Nil
                    }

         o := This.&(cObj).Cargo  // берем адрес объекта для быстроты доступа
         o:nBtn   := nI
         o:aBtn   := aBtn[ nI ]
         o:nPost  := 1  
         o:aBClr  := aBtnBClr
         o:aFClr  := aFntClr
         o:nFSize := nBFSize
         o:cFont := (App.Cargo):cBtnFontName
         o:oWnd  := This.Object       

         IF nI == nK
            o:aBClr := { MAROON , BLACK }
            This.&(cObj).Caption   := "Exit from this window"
            This.&(cObj).Backcolor := This.&(cObj).Cargo:aBClr[1]
            This.&(cObj).Action    := {|| _wPost(99) }
            This.&(cObj).FontName  := (App.Cargo):cBtnFontName
            This.&(cObj).FontBold  := .T.
            This.&(cObj).FontSize  := nBFSize 
         ENDIF
         nY += nHBtn + nG/2
      NEXT

      ThisWindow.Height  := nY + nG + nG/2 + GetBorderHeight()   // установим внешнюю высоту окна

      WITH OBJECT This.Object
         :Event( 0, {|    | 
                     This.Topmost := .F.
                     Return Nil
                     })
         :Event( 1, {|obtn|
                     Local ow := obtn:Window
                     Local a  := obtn:Cargo:aBtn
                     Local cForm := a[3]
                     Local lVsbl := a[4]
                     Local lDel  := a[5]
                     //  1     2        3       4      5       6         7
                     // {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title}
                     cForm := a[3]
                     IF _IsWindowDefined(cForm) .and. lVsbl .and. !lDel
                        DoMethod(cForm, "Minimize") ; DO EVENTS
                        DoMethod(cForm, "Restore" ) ; DO EVENTS
                        IF _IsControlDefined("Buff", cForm)
                           DoMethod(cForm, "Buff", "SetFocus" )
                        ENDIF
                        DO EVENTS
                     ENDIF
                     ow:Enabler(obtn:Name, .T.)
                     _wPost(99)
                     Return Nil
                     })
         :Event(99, {|ow  | ow:Release })
      END WITH
      
   END WINDOW

   CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   LOCAL cMsg

   cMsg := App.Cargo:cProgTtlRu + ";" + App.Cargo:cProgTtlEn + ";;"
   cMsg += App.Cargo:cCopyright + ";" + App.Cargo:cEmail + ";"
   cMsg += App.Cargo:cProgVersion + ";;"
   cMsg += App.Cargo:cPrgInfo1 + ";" + App.Cargo:cPrgInfo2 + ";"
   cMsg += App.Cargo:cPrgInfo3 + ";;"
   cMsg += "Special BIG THANK YOU - Sergej Kiselev !;;"
   cMsg += "Operating System: " + Os() + ";"
   cMsg += "Developed in : " +  MiniGUIVersion() + ";"
   cMsg += "xBase Compiler: " + Version() + ";"
   cMsg += "C Compiler: " + Hb_Ccompiler() + ";;"
   cMsg += PadC( "This program is Freeware!", 70 ) + ";"
   cMsg += PadC( "Copying is allowed!", 70 )  + ";"

   AlertInfo( cMsg, "About", "iOkey64x1", 64 )

RETURN NIL

