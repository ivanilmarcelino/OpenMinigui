/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 *
 * Работа с окнами MAIN, STANDARD, MODAL и таблицами TsBrowse.
 * Как убрать "подвисание/зависание" программы (потерю фокуса между окнами MODAL)
 * Правильная установка режима работы вложенных MODAL окон
 * Завершение программы по наличию семафорного файла demo.stop
 * Запуск отдельного потока за контролем семафорного файла demo.stop
 * Правильное закрытие программы по наличию семафорного файла demo.stop
 * Запрет входа в программу при наличии семафорного файла demo.stop
 * Working with windows MAIN, STANDARD, MODAL and tables TsBrowse.
 * How to remove the "freezing/hanging" of the program (loss of focus between MODAL windows)
 * Correct setting of the operating mode of nested MODAL windows
 * Use button events (:Event) instead of functions!
 * Terminate the program when the semaphore file demo.stop is present
 * Start a separate thread to control the semaphore file demo.stop
 * Correct program closing with the presence of the demo.stop semaphore file
 * Disable entry into the program if there is a semaphore file demo.stop
*/
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "hbthread.ch"
//#include "i_winuser.ch"

REQUEST DBFCDX
#define PROGVER  "Version 1.3 (30.08.2025)"
#define WM_WND_LAUNCH   (WM_USER+1044)

FUNCTION Main()
   LOCAL cForm := "wMain", aMsg, cMsg, i, nMax, cFStop, a, aRun, cLbl, aLbl
   LOCAL nY, nX, nH, nW, nG, owc, lRet, cWin, oac := App.Cargo
   LOCAL cTitle := oac:cTitle, aBClr := oac:aBClrMain
   LOCAL nHkfc := oac:nWndKfcH   // window height from Desktop (value from 0 to 1)
   LOCAL nWkfc := oac:nWndKfcW   // window width  from Desktop (value from 0 to 1)
   Default aBClr := {0, 191, 255}
   //
   cFStop := ChangeFileExt( App.ExeName, ".stop" )  // program stop semaphore file
   SET WINDOW MAIN OFF
   /*IF FILE( cFStop )
      // option 1 ----
      aMsg := { "A T T I N G !", "The program is closed for work!" ,;
                "Wait a while and try to start again" }
      WaitWindow(aMsg, .T., 600, 16, NIL, BLUE, WHITE )
      nMax := 15          // 15 seconds
      FOR i := 1 TO nMax ; wApi_Sleep(1000) ; DO EVENTS
      NEXT
      WaitWindow()
      QUIT
   ENDIF*/
   //
   IF FILE( cFStop )
      // option 2 ---
      cMsg := "A T T I N G !;;The program is closed for work!;"
      cMsg += "Wait a while and try to start again;;"
      cMsg += REPL("=",30) + ";"
      cMsg += HB_MemoRead( cFStop )
      WaitWinStop( App.Cargo:cTitle, App.Cargo:cIcoDef, 128, cMsg, WHITE, BLUE, 15 /*seconds*/ )
      QUIT
      aMsg := i := nMax := cMsg
   ENDIF
   //
   // list of program initialization functions
   aRun := { ;
               { "Opening databases" , {|| Set_DataBase_Tsb_Open()  } }, ;
               { "Tsb parameters"    , {|| Set_Default_Tsb_Param()  } }, ;
               { "Tsb reports"       , {|| Set_Default_Tsb_Report() } }, ;
               { "Waiting"           , {|| myInkeyGui(3000)         } }  ;
             }
   //
   // option 1 ----
   aMsg := {"Initialization of the program", App.Exename, "...."}
   cWin := WaitWindow(aMsg, .T., 600, 16, NIL, RED, aBClr )
   aLbl := HMG_GetFormControls(cWin, "LABEL")  // list of objects
   cLbl := ATail(aLbl)
   lRet := .T.
   FOR EACH a IN aRun
      cMsg := hb_ntos( hb_EnumIndex(a) ) + "/" 
      cMsg += hb_ntos( Len(aRun) ) + Space(10) + a[1]
      SetProperty( cWin, cLbl, "Value", cMsg )
      DO EVENTS
      //wApi_Sleep( 1000 )
      //lRet := EVal(a[2]) ; Default lRet := .F.   // !!! function start
      //? hb_EnumIndex(a), a[1], lRet
      If !lRet    
         cMsg := "ERROR !;" + a[1]  
         AlertStop(cMsg,,,64,{RED})
         ? ProcNL(), cMsg
         QUIT
      Endif
   NEXT
   WaitWindow()
   // option 2 ----
   _SplashWindow( "Form_Splash",,,,, "DEMO", aRun )  // Program logo display -> demo_splash.prg
   //
   SET WINDOW MAIN ON
   //
   nY := nX := 0
   nW := Sys.DesktopWidth
   nH := App.Object:H2 + App.Object:nMargHeight * 2 + ;
         GetTitleHeight() + GetBorderHeight()
   nG := App.Object:nMargWidth

   App.Cargo:aOpenWin := {}   // create a list of names of open windows in the program

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH        ;
          TITLE cTitle + " " + MiniGuiVersion()             ;
          MAIN NOSIZE  TOPMOST BACKCOLOR aBClr              ;
          ON GOTFOCUS  _wSend(20,,.T.)                      ;
          ON LOSTFOCUS _wSend(20,,.F.)                      ;
          ON INIT    ( This.Topmost := .F., _wPost(0) )     ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:cFocus   := "Buff"        // remember the focus of the restore on the form !!! IMPORTANT
      owc:cFocWnd  := ""            // window name standard
      owc:cLastBtn := ""            // button name standard window
      owc:ahIcoDel := {}            // to remove icon handles from a form

      @ 0,0 LABEL &(owc:cFocus) VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      ON KEY F1 ACTION NIL

      ButtonBar({{"Test 1", "Test window STANDARD"}, ;
                 {"Test 2", "Test window STANDARD"}, ;
                 {"Test 3", "Test window MODAL"   }, ;
                 {"Test 4", "Test window MODAL"   }, ;
                 {"Test 5", "Test window STANDARD"}, ;
                 {"Test 6", "Test window STANDARD"}, ;
                 {"Stop 7", "Terminate the program when the semaphore file demo.stop is present"}, ;
                  "Exit"} , "Btn_")

      owc:nHIco := 128
      owc:hIcon := LoadIconByName( App.Cargo:cIcoDef, owc:nHIco, owc:nHIco )
      owc:aIcon := { 128, nG, owc:nHIco, owc:nHIco }

      DRAW ICON IN WINDOW &cForm AT owc:aIcon[1], owc:aIcon[2] HICON owc:hIcon ;
                    WIDTH owc:aIcon[3] HEIGHT owc:aIcon[4] COLOR aBClr
      AADD(owc:ahIcoDel, owc:hIcon)  // then we will remove it from the form

      owc:nX := nG*2 + owc:nHIco
      @ owc:aIcon[1], owc:nX LABEL Label_Info VALUE App.Cargo:cInfo WIDTH nW - owc:nX HEIGHT 260 TRANSPARENT

      IF nHkfc > 0.4 .and. nHkfc < 1
         owc:nRowEnd := This.Height
         This.Height := int( Sys.ClientHeight * nHkfc )
      ENDIF
      IF nWkfc > 0.4 .and. nWkfc < 1
         This.Width := int( Sys.ClientWidth * nWkfc )
      ENDIF
      IF nHkfc > 0.4 .and. nWkfc > 0.4
         This.Col := int( ( Sys.ClientWidth - This.Width ) / 2 )
      ENDIF

      WITH OBJECT This.Object
       :Event( 0, {|ow,ky   |   // ON INIT
                    Local owc := ow:Cargo, nMsg := 220, nSek := 2
                    Local cFile := ChangeFileExt( App.ExeName, ".stop" )
                    ? ProcNL(), "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                    ? Repl(".", 5), "=> ON INIT <=", ow:Name, FILE(cFile), cFile
                    owc:cFStop := cFile
                    // start a separate thread to control the semaphore file demo.stop
                    owc:hThr := myThr_Start(cFile, ow:Handle, nMsg, nSek)
                    ow:SetFocus(owc:cFocus)
                    Return Nil
                    })  // -> demo_thread.prg
       :Event( 1, {|ow,ky,cn|                   // button 1
                    _wSend("_Enable",,{cn, .F.})
                    ow:Cargo:cLastBtn := cn
                    SET WINDOW THIS TO ow:Name
                    This.&(cn).Enabled := .T.
                    ky := Table_One(ow, cn)
                    SET WINDOW THIS TO
                    _wSend("_Enable",,{cn, .T.})
                    Return Nil
                    })
       :Event( 2, {|ow,ky,cn|                   // button 2
                    _wSend("_Enable",,{cn, .F.})
                    ow:Cargo:cLastBtn := cn
                    SET WINDOW THIS TO ow:Name
                    This.&(cn).Enabled := .T.
                    ky := Table_Two(ow, cn)
                    SET WINDOW THIS TO
                    _wSend("_Enable",,{cn, .T.})
                    Return Nil
                    })
       :Event( 3, {|ow,ky,cn|                   // button 3
                    Local lIsModal := _HMG_IsModalActive
                    _wSend("_Enable",,{cn, .F.})
                    SET WINDOW THIS TO ow:Name
                    _HMG_IsModalActive := .T.
                    ky := Table_Four(ow, cn, .T.)
                    _HMG_IsModalActive := lIsModal
                    SET WINDOW THIS TO
                    AEval(HMG_GetForms(,.T.), {|oo| EnableWindow(oo:Handle) })
                    _wSend("_Enable",ow,{cn, .T.})
                    ow:SetFocus(owc:cFocus)
                    _wSend(21)
                    Return Nil
                    })
       :Event( 4, {|ow,ky,cn|                   // button 4
                    Local lIsModal := _HMG_IsModalActive
                    _wSend("_Enable",,{cn, .F.})
                    SET WINDOW THIS TO ow:Name
                    _HMG_IsModalActive := .T.
                    ky := Table_Four(ow, cn, .T.)
                    _HMG_IsModalActive := lIsModal
                    SET WINDOW THIS TO
                    AEval(HMG_GetForms(,.T.), {|oo| EnableWindow(oo:Handle) })
                    _wSend("_Enable",ow,{cn, .T.})
                    ow:SetFocus(owc:cFocus)
                    _wSend(21)
                    Return Nil
                    })
       :Event( 5, {|ow,ky,cn|                   // button 5
                    _wSend("_Enable",,{cn, .F.})
                    ow:Cargo:cLastBtn := cn
                    SET WINDOW THIS TO ow:Name
                    This.&(cn).Enabled := .T.
                    ky := Table_One(ow, cn, 7)
                    SET WINDOW THIS TO
                    _wSend("_Enable",,{cn, .T.})
                    Return Nil
                    })
       :Event( 6, {|ow,ky,cn|                   // button 6
                    _wSend("_Enable",,{cn, .F.})
                    ow:Cargo:cLastBtn := cn
                    SET WINDOW THIS TO ow:Name
                    This.&(cn).Enabled := .T.
                    ky := Table_Two(ow, cn, 8)
                    SET WINDOW THIS TO
                    _wSend("_Enable",,{cn, .T.})
                    Return Nil
                    })
       :Event( 7, {|ow,ky,cn|                   // button 7
                    Local cm
                    _wSend("_Enable",,{cn, .F.})
                    ow:Cargo:cLastBtn := cn
                    SET WINDOW THIS TO ow:Name
                    This.&(cn).Enabled := .T.
                    cm := "Now the demo.stop semaphore file will be created !;"
                    cm += "Press the button to continue and the program will close"
                    AlertStop( cm, App.Exename, , 64, {RED} )
                    cm := "Databases are busy with INDEXING !;"
                    cm += "Approximate processing time is 5 minutes;"
                    cm += "After this time you can enter the program;"
                    cm += "Indexing is done by the computer: ;"
                    cm += GetComputerName() + "\" + GetUserName()
                    cm += "  " + HB_TTOC( HB_DATETIME() )
                    ky := ChangeFileExt( App.ExeName, ".stop" )
                    HB_MemoWrit( ky, cm )
                    SET WINDOW THIS TO
                    ? ProcNL(), HB_DATETIME(), FILE(ky), ky
                    _wSend("_Enable",,{cn, .T.})
                    Return Nil
                    })
       :Event({10,"_Enable"}, {|ow,ky,ab|      // enable\disable button
                    Local abt, lbt
                    lbt := !Empty(ab[2])
                    IF IsArray(ab[1]) ; abt :=  ab[1]
                    ELSE              ; abt := {ab[1]}
                    ENDIF
                    IF Len( HMG_GetForms("S") ) == 0 // not standard window
                       AAdd(abt, ow:Cargo:cBtn_Exit)
                       ow:Cargo:cLastBtn := ""
                       ow:SetFocus(ow:Cargo:cFocus)
                    ENDIF
                    FOR EACH ky IN abt ; ow:Enabler(ky, lbt)
                    NEXT
                    Return Nil
                    })
       :Event(20, {|ow,ky,lf|                   // got\lost focus window
                    Local pen := 3, cwnd, ownc
                    Local y := pen, x := pen
                    Local w := ow:ClientWidth  - y * 2
                    Local h := ow:ClientHeight - x * 2
                    Local clr := iif( Empty(lf), This.BackColor, RED )
                    ky := 2
                    DrawRR( clr, pen, y, x, h, w, , ky )
                    DO EVENTS
                    IF !Empty(lf) .and. !Empty(ow:Cargo:cFocWnd)
                       cwnd := ow:Cargo:cFocWnd
                       ownc := GetProperty(cwnd, "Cargo")
                       DoMethod(cwnd, "SetFocus")
                       DoMethod(cwnd, ownc:cFocus, "SetFocus")
                       DO EVENTS
                    ENDIF
                    ow:Cargo:cFocWnd := ""
                    Return Nil
                    })
       :Event(21, {|ow,ky|                      // focus window standard
                    Local owc := ow:Cargo
                    Local cnm := owc:cLastBtn
                    IF !Empty(cnm)
                       ky := This.&(cnm).Cargo
                       _wPost(ky, , cnm)
                    ENDIF
                    Return Nil
                    })
       :Event(90, {|ow,ky|                      // ON RELEASE
                    Local ah := ow:Cargo:ahIcoDel
                    ? ProcNL(), "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                    ? Repl(".", 5), "=> RELEASE WINDOW <=", ow:Name
                    dbCloseAll()
                    myThr_Stop(ow:Cargo:hThr)  // complete function in the stream
                    ? Repl(".", 5), "Thread close()"
                    ? Repl(".", 5),"Delete handle icon - ow:Cargo:ahIcoDel="
                    ?? ah, HB_ValToExp(ah)
                    IF IsArray(ah)
                       AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                    ENDIF
                    //myThreadClose()   // complete function in the stream
                    Return Nil
                    })
       :Event( 99, {|ow| ow:Release() })
       :Event(220, {|ow,ky|  // Wait + Stop
                     Local oac := App.Cargo, cMsg, aWin, i
                     Local cForm, owc := ow:Cargo
                     ? "==>> " + ProcName() + "(" + HB_NtoS(ProcLine()) + ")"
                     ?? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                     cMsg := "A T T I N G !;;The program is closed for work!;"
                     cMsg += REPL("=",30) + ";"
                     cMsg += HB_MemoRead( owc:cFStop )
                     // if the program is on the taskbar
                     If IsIconic( ow:Handle )
                        _Restore( ow:Handle )
                        //SendMessage( ow:Handle, WM_PAINT, 0, 0 ) // supplement, try
                     EndIf
                     DoMethod( ow:Name, "SetFocus" )
                     DO EVENTS
                     WaitWinStop( oac:cTitle, oac:cIcoDef, 128, cMsg, WHITE, ORANGE, 15 /*seconds*/ )
                     // RELEASE ALL - does not work
                     aWin := App.Cargo:aOpenWin      // list of names of open windows in the program
                     For i := LEN(aWin) To 1 STEP -1
                        cForm := aWin[i]
                        If _IsWindowDefined(cForm)
                            ? Repl(".", 5), i, cForm, "Release"
                            DoMethod(cForm, "Release" )
                        Endif
                        Doevents()
                     Next
                     _wSend(99, ow, .T.)
                     Return Nil
                     })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

STATIC FUNCTION Set_Default_Tsb_Param()
   LOCAL oac := App.Cargo, o
   // oTsb default
   Default oac:oTsb := oHmgData() ; o := oac:oTsb

   o:lZebra      := .T.
   o:lSpecHd     := .T.
   o:aFoot       := .T.
   o:aEdit       := .T.
   o:uSelector   := 20
   o:aNumber     := { 1, App.Object:W(0.5) }
   o:nHeightCell := App.Object:H(1.1)
   o:nHeightHead := App.Object:H(1.2)
   o:bAfter      := {|ob|
                     Local oc := ob:aColumns[1]
                     oc:nClrBack := {|na,nc,obr|
                                     Local ocol := obr:aColumns[nc]
                                     Local nclr := ocol:nClrHeadBack
                                     IF (obr:cAlias)->( Deleted() )
                                        nclr := CLR_HGRAY
                                        na := nc
                                     ENDIF
                                     Return nclr
                                     }
                     Return Nil
                     }

RETURN .T.

STATIC FUNCTION Set_Default_Tsb_Report()
   LOCAL oac := App.Cargo, o
   // Calculation default
   Default oac:oCounting := oHmgData() ; o := oac:oCounting

   o:bReport_1 := {|ow,ky,cn|
                   Local ct := 'Waiting ... Calculation in progress - '
                   Local ob := ow:Cargo:oBrw, cww, alb, clb
                   Local hm := _HMG_MainHandle
                   SET WINDOW THIS TO ow:Name
                   ct += ob:cAlias + ";" + App.ExeName + ";"
                   _HMG_MainHandle := ow:Handle
                   cww := WaitWindow( ct, .T., 400, 16, NIL, YELLOW, BLUE )
                   _HMG_MainHandle := hm
                   alb := HMG_GetFormControls(cww, "LABEL")
                   clb := ATail(alb)
                   SetProperty(cww, clb, "Alignment", "Center")
                   FOR ky := 1 TO 5
                       SetProperty(cww, clb, "Value", hb_ntos(ky))
                       wApi_Sleep(1000)
                   NEXT
                   SetProperty(cww, clb, "Value", "")
                   wApi_Sleep(300)
                   WaitWindow()
                   SET WINDOW THIS TO
                   IF ow:Type == "M" ; EnableWindow( ky := ow:Handle ) // !!! remove LOSS OF FOCUS for windows
                   ENDIF
                   ow:Enabler(cn, .T.)
                   ow:SetFocus(ow:Cargo:cFocus)
                   DO EVENTS
                   Return Nil
                   }

   o:bReport_2 := {|ow,ky,cn|
                   Local ct := 'Waiting ... Calculation in progress - '
                   Local ob := ow:Cargo:oBrw, cww, alb, clb
                   Local hm := _HMG_MainHandle
                   SET WINDOW THIS TO ow:Name
                   ct += ob:cAlias + ";" + App.ExeName + ";"
                   _HMG_MainHandle := ow:Handle
                   cww := WaitWindow( ct, .T., 400, 16, NIL, WHITE, PURPLE )
                   _HMG_MainHandle := hm
                   alb := HMG_GetFormControls(cww, "LABEL")
                   clb := ATail(alb)
                   SetProperty(cww, clb, "Alignment", "Center")
                   FOR ky := 1 TO 5
                       SetProperty(cww, clb, "Value", hb_ntos(ky))
                       wApi_Sleep(1000)
                   NEXT
                   SetProperty(cww, clb, "Value", "")
                   wApi_Sleep(300)
                   WaitWindow()
                   SET WINDOW THIS TO
                   IF ow:Type == "M" ; EnableWindow( ky := ow:Handle ) // !!! remove LOSS OF FOCUS for windows
                   ENDIF
                   ow:Enabler(cn, .T.)
                   ow:SetFocus(ow:Cargo:cFocus)
                   DO EVENTS
                   Return Nil
                   }

RETURN .T.

FUNCTION Set_DataBase_Tsb_Open() 
   LOCAL cDbf, cAls, lRet, lErr, aUse := {}, a 
   LOCAL cPth := App.Cargo:cPathDbf 
   LOCAL aDbf := App.Cargo:aFileDbf 
  
   FOR EACH a IN aDbf 
       cDbf := a[1] 
       cAls := a[2] 
       lErr := .T. 
       BEGIN SEQUENCE WITH {|e| break( e ) } 
          USE ( cPth + cDbf ) ALIAS ( cAls ) NEW SHARED 
          IF Used() ; AAdd( aUse, .T. ) ; lErr := .F. 
          ENDIF 
       END SEQUENCE 
       IF lErr ; ? hb_enumindex(a), cAls, cPth + cDbf, "Not used !" 
       ENDIF 
   NEXT 
   lRet := Len(aUse) == Len(aDbf) 
RETURN lRet 

FUNCTION Table_One(oWnd, cBtn, nTbl)
   LOCAL cFocus := "Buff", oac := App.Cargo
   LOCAL oTsb, owc, cNam := "Tst_S_", nY, nX, nH, nW
   LOCAL nDbf  := iif( Empty(nTbl), 1, nTbl )         //"CUST1" or "CUST7"
   LOCAL cForm := "w" + hb_ntos(nDbf) + "_" + cBtn
   LOCAL cAls  := oac:aFileDbf[ nDbf ][2]
   LOCAL aBClr := oac:aBClrDbf[ nDbf ]
   Default aBClr := {127, 255, 212}

   IF _IsWindowDefined( cForm )
      oWnd:Cargo:cFocWnd := cForm
      IF IsIconic( nH := GetFormHandle(cForm) ) ; _Restore( nH )
      ENDIF
      DoMethod(cForm, "SetFocus")
      RETURN NIL
   ENDIF

   IF Empty(oWnd:Cargo:nRowEnd) ; nY := oWnd:Row + oWnd:Height
   ELSE                         ; nY := oWnd:Cargo:nRowEnd
   ENDIF
   nX := oWnd:Col
   nW := oWnd:Width
   nH := Sys.ClientHeight - nY

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH                 ;
          TITLE "Demo TBrowse. STANDARD " + cForm + " => "+cAls      ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE              ;
          BACKCOLOR aBClr                                            ;
          ON INIT    ( This.Topmost := .F., _wPost( 0) )             ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:oParent := oWnd
      owc:cFocus  := cFocus
      owc:cButton := cBtn
      IF AScan(App.Cargo:aOpenWin, cForm) == 0
         AADD( App.Cargo:aOpenWin, cForm )   // save the name of the open window in the list
      ENDIF

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      ButtonBar({"Modal", "Calculation", "Wait", "Exit"}, cNam)

      nY := App.Object:H2 + App.Object:nMargWidth * 2
      nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight - nY

      oTsb := App.Cargo:oTsb:Clone()

      This.Cargo:oBrw := _TBrowse( oTsb, cAls, , nY, nX, nW, nH )

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | ow:SetFocus(ow:Cargo:cFocus) })
         :Event( 1, {|ow,ky,cn|
                      Local lIsModal := _HMG_IsModalActive
                      SET WINDOW THIS TO ow:Name
                      _HMG_IsModalActive := .T.
                      Table_Three(ow, cn, .T.)
                      _HMG_IsModalActive := lIsModal
                      AEval(HMG_GetForms(,.T.), {|oo| EnableWindow(oo:Handle) }) // !!! remove LOSS OF FOCUS for windows
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, App.Cargo:oCounting:bReport_1 )
         :Event( 3, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      ky := Table_W(ow, cn)
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      _wPost(10, , ky)
                      Return Nil
                      })
         :Event(10, {|ow,ky,arr|
                      Local cMsg := "Selected: ;"
                      Local lEsc := !Empty(ow:Cargo:lEscape)
                      Local aRez := ow:Cargo:aRezult
                      ky := iif( IsArray(arr), Len(arr), 0 )
                      cMsg += "1 - records: "+cValToChar(ky) + ";"
                      cMsg += "2 - records: "+cValToChar(Len(aRez))
                      IF lEsc ; cMsg += ";3 - press key ESCAPE !"
                      ENDIF
                      SET WINDOW THIS TO ow:Name
                      AlertInfo(cMsg)
                      SET WINDOW THIS TO
                      ow:SetFocus(ow:Cargo:cFocus)
                      Return Nil
                      })
         :Event(90, {|ow|
                      Local awnd := HMG_GetForms("S")
                      Local i, om := ow:Cargo:oParent
                      Local nwnd := Len(awnd)
                      IF nwnd > 1
                         FOR i := nwnd TO 1 STEP -1
                             IF ow:Name != awnd[ i ]
                                om:Cargo:cFocWnd := awnd[ i ]
                                EXIT
                             ENDIF
                         NEXT
                      ENDIF
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION Table_Two(oWnd, cBtn, nTbl)
   LOCAL cFocus := "Buff", oac := App.Cargo
   LOCAL oTsb, owc, cNam := "Run_S_", nY, nX, nH, nW
   LOCAL nDbf  := iif( Empty(nTbl), 2, nTbl )          //"CUST2" or "CUST8"
   LOCAL cForm := "w" + hb_ntos(nDbf) + "_" + cBtn
   LOCAL cAls  := oac:aFileDbf[ nDbf ][2]
   LOCAL aBClr := oac:aBClrDbf[ nDbf ]
   Default aBClr := {0, 255, 255}

   IF _IsWindowDefined( cForm )
      oWnd:Cargo:cFocWnd := cForm
      IF IsIconic( nH := GetFormHandle(cForm) ) ; _Restore( nH )
      ENDIF
      DoMethod(cForm, "SetFocus")
      RETURN NIL
   ENDIF

   IF Empty(oWnd:Cargo:nRowEnd) ; nY := oWnd:Row + oWnd:Height
   ELSE                         ; nY := oWnd:Cargo:nRowEnd
   ENDIF
   nX := oWnd:Col
   nW := oWnd:Width
   nH := Sys.ClientHeight - nY

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH                 ;
          TITLE "Demo TBrowse. STANDARD " + cForm + " => "+cAls      ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE              ;
          BACKCOLOR aBClr                                            ;
          ON INIT    ( This.Topmost := .F., _wPost( 0) )             ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:oParent := oWnd
      owc:cFocus  := cFocus
      owc:cButton := cBtn
      IF AScan(App.Cargo:aOpenWin, cForm) == 0
         AADD( App.Cargo:aOpenWin, cForm )   // save the name of the open window in the list
      ENDIF

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      ButtonBar({"Modal", "Calculation", "Wait", "Exit"}, cNam)

      nY := App.Object:H2 + App.Object:nMargWidth * 2
      nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight - nY

      oTsb := App.Cargo:oTsb:Clone()

      This.Cargo:oBrw := _TBrowse( oTsb, cAls, , nY, nX, nW, nH )

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | ow:SetFocus(ow:Cargo:cFocus) })
         :Event( 1, {|ow,ky,cn|
                      Local lIsModal := _HMG_IsModalActive
                      SET WINDOW THIS TO ow:Name
                      _HMG_IsModalActive := .T.
                      Table_Four(ow, cn, .T.)
                      _HMG_IsModalActive := lIsModal
                      AEval(HMG_GetForms(,.T.), {|oo| EnableWindow(oo:Handle) }) // !!! remove LOSS OF FOCUS for windows
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, App.Cargo:oCounting:bReport_2 )
         :Event( 3, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      ky := Table_W(ow, cn)
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      _wPost(10,, ky)
                      Return Nil
                      })
         :Event(10, {|ow,ky,arr|
                      Local cMsg := "Selected: ;"
                      Local lEsc := !Empty(ow:Cargo:lEscape)
                      Local aRez := ow:Cargo:aRezult
                      ky := iif( IsArray(arr), Len(arr), 0 )
                      cMsg += "1 - records: "+cValToChar(ky) + ";"
                      cMsg += "2 - records: "+cValToChar(Len(aRez))
                      IF lEsc ; cMsg += ";3 - press key ESCAPE !"
                      ENDIF
                      SET WINDOW THIS TO ow:Name
                      AlertInfo(cMsg)
                      SET WINDOW THIS TO
                      ow:SetFocus(ow:Cargo:cFocus)
                      Return Nil
                      })
         :Event(90, {|ow|
                      Local awnd := HMG_GetForms("S")
                      Local i, om := ow:Cargo:oParent
                      Local nwnd := Len(awnd)
                      IF nwnd > 1
                         FOR i := nwnd TO 1 STEP -1
                             IF ow:Name != awnd[ i ]
                                om:Cargo:cFocWnd := awnd[ i ]
                                EXIT
                             ENDIF
                         NEXT
                      ENDIF
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION Table_Three(oWnd, cBtn, lAls)
   LOCAL cFocus := "Buff", oac := App.Cargo
   LOCAL oTsb, owc, cNam := "Tst_M_", nY, nX, nH, nW
   LOCAL nDbf  := iif( Empty(lAls), 3, 5 )  // "CUST3" or "CUST5"
   LOCAL cForm := "w" + hb_ntos(nDbf) + "_" + cBtn
   LOCAL cAls  := oac:aFileDbf[ nDbf ][2]
   LOCAL aBClr := oac:aBClrDbf[ nDbf ]
   Default aBClr := {216, 191, 216}

   nY := oWnd:Row
   nX := oWnd:Col
   nW := oWnd:Width
   nH := Sys.ClientHeight - nY

   _HMG_InplaceParentHandle := oWnd:Handle

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH           ;
          TITLE "Demo TBrowse. MODAL " + cForm + " => " + cAls ;
          MODAL NOSIZE BACKCOLOR aBClr                         ;
          ON INIT    ( _wPost( 0) )                            ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:oParent := oWnd
      owc:cFocus  := cFocus
      owc:lAlias  := !Empty( lAls )
      IF AScan(App.Cargo:aOpenWin, cForm) == 0
         AADD( App.Cargo:aOpenWin, cForm )   // save the name of the open window in the list
      ENDIF

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      IF owc:lAlias
         ButtonBar({"Modal", "Exit"}, cNam, , , App.Object:W2)
      ELSE
         ButtonBar({"Info" , "Calculation", "Wait", "Exit"}, cNam)
      ENDIF

      nY := App.Object:H2 + App.Object:nMargWidth * 2
      nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight - nY

      oTsb := App.Cargo:oTsb:Clone()
      oTsb:uAlias := cAls
      oTsb:nY     := nY
      oTsb:nX     := nX
      oTsb:nW     := nW
      oTsb:nH     := nH

      This.Cargo:oBrw := _TBrowse( oTsb )

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | ow:SetFocus(ow:Cargo:cFocus) })
         :Event( 1, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      IF ow:Cargo:lAlias
                         ky := Table_Four(ow, cn)
                      ELSE
                         AlertInfo("Press button "+ow:Name+"."+cn)
                      ENDIF
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, App.Cargo:oCounting:bReport_1 )
         :Event( 3, {|ow,ky,cn|
                      Local cMsg, lEsc
                      SET WINDOW THIS TO ow:Name
                      ky := Table_W(ow, cn)
                      lEsc := !Empty(ow:Cargo:lEscape)
                      cMsg := "Selected: ;" + ;
                           "1 - records: "+cValToChar(Len(ky)) + ";" + ;
                           "2 - records: "+cValToChar(Len(ow:Cargo:aRezult))
                      IF lEsc ; cMsg += ";3 - press key ESCAPE !"
                      ENDIF
                      AlertInfo(cMsg)
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_InplaceParentHandle := 0

RETURN NIL

FUNCTION Table_Four(oWnd, cBtn, lAls)
   LOCAL cFocus := "Buff", oac := App.Cargo
   LOCAL oTsb, owc, cNam := "Run_M_", nY, nX, nH, nW
   LOCAL nDbf  := iif(  Empty(lAls), 4, 6 )  // "CUST4" or "CUST6"
   LOCAL cForm := "w" + hb_ntos(nDbf) + "_" + cBtn
   LOCAL cAls  := oac:aFileDbf[ nDbf ][2]
   LOCAL aBClr := oac:aBClrDbf[ nDbf ]
   Default aBClr := {255, 239, 213}

   nY := oWnd:Row
   nX := oWnd:Col
   nW := oWnd:Width
   nH := Sys.ClientHeight - nY

   _HMG_InplaceParentHandle := oWnd:Handle

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH           ;
          TITLE "Demo TBrowse. MODAL " + cForm + " => " + cAls ;
          MODAL NOSIZE  BACKCOLOR aBClr                        ;
          ON INIT    ( _wPost( 0) )                            ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:oParent := oWnd
      owc:cFocus  := cFocus
      owc:lAlias  := !Empty( lAls )
      IF AScan(App.Cargo:aOpenWin, cForm) == 0
         AADD( App.Cargo:aOpenWin, cForm )   // save the name of the open window in the list
      ENDIF

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      IF owc:lAlias
         ButtonBar({"Modal", "Exit"}, cNam, , , App.Object:W2)
      ELSE
         ButtonBar({"Info" , "Calculation", "Wait", "Exit"}, cNam)
      ENDIF

      nY := App.Object:H2 + App.Object:nMargWidth * 2
      nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight - nY

      oTsb := App.Cargo:oTsb:Clone()
      oTsb:uAlias := cAls
      oTsb:nY     := nY
      oTsb:nX     := nX
      oTsb:nW     := nW
      oTsb:nH     := nH

      This.Cargo:oBrw := _TBrowse( oTsb )

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | ow:SetFocus(ow:Cargo:cFocus) })
         :Event( 1, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      IF ow:Cargo:lAlias
                         ky := Table_Three(ow, cn)
                      ELSE
                         AlertInfo("Press button "+ow:Name+"."+cn)
                      ENDIF
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, App.Cargo:oCounting:bReport_2 )
         :Event( 3, {|ow,ky,cn|
                      Local cMsg, lEsc
                      SET WINDOW THIS TO ow:Name
                      ky := Table_W(ow, cn)
                      lEsc := !Empty(ow:Cargo:lEscape)
                      cMsg := "Selected: ;" + ;
                           "1 - records: "+cValToChar(Len(ky)) + ";" + ;
                           "2 - records: "+cValToChar(Len(ow:Cargo:aRezult))
                      IF lEsc ; cMsg += ";3 - press key ESCAPE !"
                      ENDIF
                      AlertInfo(cMsg)
                      SET WINDOW THIS TO
                      ow:Enabler(cn, .T.)
                      ow:SetFocus(ow:Cargo:cFocus)
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_InplaceParentHandle := 0

RETURN NIL

FUNCTION Table_W(oWnd, cBtn, bSelect)
   LOCAL cForm := "w" + "W_" + cBtn
   LOCAL nY, nX, nH, nW, cN
   LOCAL owc, cNam := "Wait_M_"
   LOCAL aBClr := {255, 239, 213}, cFocus := "Buff"

   nY := oWnd:Row
   nX := oWnd:Col
   nW := App.Object:W(3.5)  //oWnd:Width  * 0.35
   nH := App.Object:H(7.5)  //oWnd:Height * 0.25 //Sys.ClientHeight - nY

   _HMG_InplaceParentHandle := oWnd:Handle

   oWnd:Cargo:lEscape := .F.
   oWnd:Cargo:aRezult := {}

   Default bSelect    := {|ow|
                           Local owc := ow:Cargo
                           Local oParent := owc:oParent      // oWnd
                           Local oBrw := oParent:Cargo:oBrw
                           Local cAls := oBrw:cAlias, nRec
                           Local nOld := Select(), nOldRec := RecNo()
                           Local aRez := oParent:Cargo:aRezult
                           dbSelectArea(cAls)
                           nRec := RecNo()
                           GO TOP
                           DO WHILE !EOF()
                              DO EVENTS
                              IF owc:lBreak ; EXIT
                              ENDIF
                              IF RecNo() %2 == 0 .and. !Deleted()
                                 AAdd(aRez, RecNo())
                              ENDIF
                              SKIP
                              InkeyGui(50)
                           ENDDO
                           dbGoto(nRec)
                           DO EVENTS
                           dbSelectArea(nOld)
                           dbGoto(nOldRec)
                           DO EVENTS
                           InkeyGui(50)
                           owc:lBreak := .T.
                           Return Nil
                           }

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH           ;
          TITLE "Demo TBrowse. MODAL " + cForm + " => WAIT"    ;
          MODAL NOSIZE /*NOSYSMENU NOCAPTION*/ BACKCOLOR aBClr ;
          ON INTERACTIVECLOSE This.Cargo:lBreak                ;
          ON INIT    _wPost( 0)                                ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData() ; owc := This.Cargo

      IF AScan(App.Cargo:aOpenWin, cForm) == 0
         AADD( App.Cargo:aOpenWin, cForm )   // save the name of the open window in the list
      ENDIF

      DEFINE STATUSBAR BOLD
         STATUSITEM ""
      END STATUSBAR

      owc:oParent := oWnd
      owc:cFocus  := cFocus
      owc:nMaxCnt := 15        // maximum operating time
      owc:cTimer  := "_TM_"
      owc:lBreak  := .F.
      owc:aIcoH   := {}
      owc:nIcoS   := 64
      owc:nIcoY   := Int( ( This.ClientHeight - owc:nIcoS -     ;
                                     This.StatusBar.Height) / 2 )
      owc:nIcoX   := Int( ( This.ClientWidth  - owc:nIcoS ) / 2 )
      owc:cIcoTm  := "_TI_"

      FOR EACH cN IN App.Cargo:aWaitIcoN
          AAdd(owc:aIcoH, LoadIconByName(cN, owc:nIcoS, owc:nIcoS))
      NEXT

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      DEFINE TIMER &(owc:cTimer) INTERVAL 1000 ;
             ON INIT {|| This.Enabled := .F., This.Cargo := 0 } ;
             ACTION  {|| This.Enabled := .F., _wPost(10,, This.Name) }

      DEFINE TIMER &(owc:cIcoTm) INTERVAL  150 ;
             ON INIT {|| This.Enabled := .F., This.Cargo := 0 } ;
             ACTION  {||
                       Local owc := ThisWindow.Cargo
                       IF This.&(owc:cTimer).Enabled .and. !owc:lBreak
                          This.Enabled := .F.
                          _wPost(11,, This.Name)
                       ENDIF
                       Return Nil
                       }

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|| ThisWindow.Cargo:oParent:Cargo:lEscape := .T., ;
                               ThisWindow.Cargo:lBreak := .T. }

      WITH OBJECT This.Object
         :Event( 0, {|ow| ow:SetFocus(ow:Cargo:cFocus), _wSend(10), ;
                          _wSend(11), _wPost(1) })
         :Event( 1, bSelect )
         :Event(10, {|ow,ky,cn|
                     Local owc := ow:Cargo
                     Default cn := owc:cTimer
                     ow:Enabler(cn, .F.)
                     ky := This.&(cn).Cargo + 1
                     ow:StatusBar:Say("... W A I T ..." + str(ky, 5))
                     DO EVENTS
                     This.&(cn).Cargo := ky
                     IF ky > owc:nMaxCnt .or. owc:lBreak
                        ow:Enabler(owc:cIcoTm, .F.)
                        _wSend(99)
                     ELSE
                        ow:Enabler(cn, .T.)
                     ENDIF
                     Return Nil
                     })
         :Event(11, {|ow,ky,cn|
                     Local owc := ow:Cargo
                     Default cn := owc:cIcoTm
                     ow:Enabler(cn, .F.)
                     ky := This.&(cn).Cargo + 1
                     ky := iif( ky > Len(owc:aIcoH), 1, ky )
                     This.&(cn).Cargo := ky
                     DO EVENTS
                     DRAW ICON IN WINDOW &(ow:Name)    ;
                               AT owc:nIcoY, owc:nIcoX ;
                               PICTURE owc:aIcoH[ ky ] ;
                               WIDTH owc:aIcoS HEIGHT owc:aIcoS TRANSPARENT
                     DO EVENTS
                     ow:Enabler(cn, .T.)
                     Return Nil
                     })
         :Event(90, {|ow| AEval(ow:Cargo:aIcoH, {|hi| DeleteObject(hi) }) })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   _HMG_InplaceParentHandle := 0

RETURN oWnd:Cargo:aRezult


FUNCTION WaitWinStop(cTitle, cIcon, nISize, cText, aFClr, aBClr, nTime)
   LOCAL cForm, hFont, aFont, cFont, nFSize, aWHTxt, cMsg, cFocus, owc
   LOCAL nY, nX, nH, nW, nG, cInfo

   cForm  := "MG_Wait_Stop"
   hFont  := GetFontHandle("DlgFont")
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   aWHTxt := TextSizeWH( cText, cFont, nFSize )
   cMsg   := aWHTxt[3]
   nG     := 20
   nW     := nG + nISize + nG + aWHTxt[1] + nG
   nH     := nG + aWHTxt[2] + nG + nFSize*2 + nG
   cInfo  := "Closing window via " // seconds
   cFocus := "Buff"

   _HMG_InplaceParentHandle:=GetActiveWindow()

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH    ;
          TITLE cTitle BACKCOLOR aBClr                  ;
          MODAL NOSIZE /*NOCAPTION*/                    ;
          ON INIT    _wPost( 0)                         ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:nTime   := nTime    // number of seconds counted
      owc:nFor    := 1
      owc:cInfo   := cInfo
      owc:cFocus  := cFocus
      owc:cTimer  := "_TM_"
      owc:lEscape := .F.

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      DRAW ICON IN WINDOW &cForm AT nG, nG PICTURE cIcon WIDTH nISize HEIGHT nISize COLOR aBClr

      nY := nG
      nX := nG + nISize + nG

      @ nY, nX LABEL Label_1 WIDTH aWHTxt[1] HEIGHT aWHTxt[2] VALUE cMsg ;
        FONTCOLOR aFClr CENTERALIGN TRANSPARENT

      nY += This.Label_1.Height //+ nG

      @ nY, 0 LABEL Label_2 WIDTH nW HEIGHT nFSize*2 VALUE cInfo ;
        FONTCOLOR aFClr CENTERALIGN VCENTERALIGN TRANSPARENT

      DEFINE TIMER &(owc:cTimer) INTERVAL 1000 ;
             ON INIT {|| This.Enabled := .F., This.Cargo := 0 } ;
             ACTION  {|| This.Enabled := .F., _wPost(1,, This.Name) }

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|| ThisWindow.Cargo:lEscape := .T. }

      WITH OBJECT This.Object
         :Event( 0, {|ow,ky| ow:SetFocus(ow:Cargo:cFocus) , This.Topmost := .T. ,;
                             BringWindowToTop( This.Handle )  ,;
                             _LogFile(.T., ">>> ON INIT WINDOW: "+ow:Name+" - Event:",ky) ,;
                             _wSend(1) } )

         :Event( 1, {|ow,ky,cn|
                     Local cv, owc := ow:Cargo
                     Default cn := owc:cTimer
                     ow:Enabler(cn, .F.)
                     owc:nFor ++
                     cv := HB_NtoS( owc:nTime - owc:nFor )
                     ? ProcName(), ow,ky,cn
                     This.Label_2.Value := owc:cInfo + cv + " sec."
                     DO EVENTS
                     ?? owc:nFor, ">", owc:nTime
                     IF owc:nFor > owc:nTime .or. owc:lEscape
                        ow:Enabler(cn, .T.)
                        owc:lEscape := .T.
                        _wSend(99,ow)
                        ?? "_wSend(99,ow)"
                     ELSE
                        ow:Enabler(cn, .T.)
                     ENDIF
                     //DO EVENTS  - NO! OTHERWISE the program will remain in memory !!!
                     Return Nil
                     })
         :Event(90, {|ow,ky| _LogFile(.T., ">>> ON RELEASE WINDOW: "+ow:Name+" - Event:",ky) })
         :Event(99, {|ow   | ow:Release() })
      END WITH

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   _HMG_InplaceParentHandle := 0

   ? "===>>>", cForm, _IsWindowDefined(cForm)

RETURN NIL


FUNCTION TextSizeWH( cMsg, cFont, nFSize )
   LOCAL aText, nLenText, nLenOneStr, nMaxLineNmb, nWTxt
   LOCAL cText, nHText, nWText, nI, cRetMsg
   // высчитываем размерность текста
   cMsg   := AtRepl( CHR(0), cMsg, ";" )
   cMsg   := AtRepl( ";", cMsg, CRLF )
   // поиск максимальной строки в тексте
   aText       := HB_ATokens(cMsg, CRLF, .F., .F.)
   nLenText    := LEN(aText)
   nLenOneStr  := 0
   nMaxLineNmb := 0
   nWTxt       := 0
   cRetMsg     := ""
   FOR nI := 1 TO nLenText
      IF LEN(aText[nI]) > nLenOneStr
         nLenOneStr  := LEN(aText[nI])
         nMaxLineNmb := nI
      ENDIF
      cText   := aText[nI]
      nWText  := GetTxtWidth( cText, nFSize, cFont ) // получить Width текста
      nWTxt   := MAX(nWTxt,nWText)
      cRetMsg += cText + CRLF
   NEXT

   cText    := aText[nMaxLineNmb]                                // максимальная строка текста в массиве
   cText    := IIF( LEN(cText) < 50, REPL("X",50), cText )       // проверка наличия текста
   nLenText := IIF(nLenText < 1, 1, nLenText )                   // проверка наличия текста
   nWText   := nWTxt + 20
   nHText   := GetTxtHeight( "B", nFSize, cFont )*nLenText       // получить Height текста

RETURN { nWText, nHText, cRetMsg }

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )  // получить Width текста
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

   RETURN nWidth

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtHeight( cText, nFontSize, cFontName, lBold )  // получить Height текста
   LOCAL hFont, nHeight
   DEFAULT cText     := "B"                 ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   hFont := InitFont( cFontName, nFontSize, lBold )
   nHeight := GetTextHeight( 0, cText , hFont )    // высота шрифта
   DeleteObject( hFont )

   RETURN nHeight

*----------------------------------------------------------------------------*
FUNCTION DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
*----------------------------------------------------------------------------*
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 5
   DEFAULT nPen  := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE                ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
        ROUNDWIDTH  nCurve ROUNDHEIGHT nCurve   ;
        PENCOLOR  aColor PENWIDTH   nPen

RETURN Nil

FUNCTION ButtonBar(aBtn, cPref, nY, nX, nW, nH, nG, l99)
   LOCAL cTxt, nBtn, cBtn, aNam := {}, cTool, cCapt
   Default cPref := "Btn_", l99 := .T.

   Default nG := App.Object:nMargWidth, aBtn := {}, nBtn := 0
   Default nY := nG, nX := nG , ;
           nW := App.Object:W1, ;
           nH := App.Object:H2

   FOR EACH cTxt IN aBtn
       nBtn  := hb_enumindex(cTxt)
       IF IsArray(cTxt) ; cTool := cTxt[2] ; cCapt := cTxt[1]
       ELSE             ; cTool := NIL     ; cCapt := cTxt
       ENDIF
       cBtn := cPref + hb_ntos( nBtn )
       @ nY, nX BUTTONEX &cBtn WIDTH nW HEIGHT nH CAPTION cCapt ;
                TOOLTIP  cTool                                  ;
                NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
                ACTION ( This.Enabled := .F., _wPost(This.Cargo,, This.Name) )
       This.&(cBtn).Cargo := iif( l99 .and. nBtn == Len(aBtn), 99, nBtn )
       nX += This.&(cBtn).Width + nG
       This.Cargo:cBtn_Exit := cBtn
       AAdd(aNam, cBtn)
   NEXT

RETURN aNam

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12, nBmp := 24, nIco := 24, oac

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   ON
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET TOOLTIPSTYLE BALLOON

   SET MULTIPLE QUIT WARNING
   SET WINDOW MAIN OFF
   SET NAVIGATION EXTENDED
   SET WINDOW MODAL PARENT HANDLE ON
   SET ShowRedAlert ON

   App.Cargo := oHmgData() ; oac := App.Cargo

   oac:cTitle    := "Demo TBrowse. Test of operation of windows of type MAIN,STANDARD,MODAL. "
   oac:cIcoDef   := "1MG"
   oac:lLogDel   := .T.
   oac:cLogFile  := hb_FNameExtSet( App.ExeName, ".log" )
   oac:cIniFile  := hb_FNameExtSet( App.ExeName, ".ini" )
   oac:lFontSize := .T.
   oac:nWndKfcH  := 0   // window height from Desktop (value from 0 to 1)
   oac:nWndKfcW  := 0   // window width  from Desktop (value from 0 to 1)
   oac:aWaitIcoN := { "zmk01", "zmk02", "zmk03", "zmk04", ;
                      "zmk05", "zmk06", "zmk07", "zmk08" }
   oac:cPathDbf  := ".\"
   oac:aFileDbf  := {{"CUSTOMER" , "CUST1"}, {"CUSTOMER2", "CUST2"}, ;
                     {"CUSTOMER" , "CUST3"}, {"CUSTOMER2", "CUST4"}, ;
                     {"CUSTOMER" , "CUST5"}, {"CUSTOMER2", "CUST6"}, ;
                     {"CUSTOMER2", "CUST7"}, {"CUSTOMER" , "CUST8"} }
   oac:aBClrDbf  := {{127, 255, 212}      , {  0, 255, 255}, ;
                     {216, 191, 216}      , {255, 239, 213}, ;
                     {216, 191, 216}      , {255, 239, 213}, ;
                     {216, 191, 216}      , {255, 239, 213}}
   oac:aBClrMain := {0, 191, 255}

   oac:cInfo     := "Working with windows MAIN, STANDARD, MODAL and tables TsBrowse" + CRLF
   oac:cInfo     += 'How to remove the "freezing/hanging" of the program (loss of focus between MODAL windows)' + CRLF
   oac:cInfo     += "Correct setting of the operating mode of nested MODAL windows" + CRLF
   oac:cInfo     += "Use button events (:Event) instead of functions !" + CRLF + CRLF
   oac:cInfo     += "!!! Terminate the program when the semaphore file demo.stop is present" + CRLF
   oac:cInfo     += "    Start a separate thread to control the semaphore file demo.stop" + CRLF
   oac:cInfo     += "    Correct program closing with the presence of the demo.stop semaphore file" + CRLF
   oac:cInfo     += CRLF + PROGVER

   IF hb_FileExists( oac:cIniFile )
      oac:oIni := TIniData():New(oac:cIniFile, .T.):Read()
      Default oac:oIni:COM := oHmgData()
      Default oac:oIni:COM:cTitle    := oac:cTitle
      Default oac:oIni:COM:cFontName := cFont
      Default oac:oIni:COM:nFontSize := nSize
      Default oac:oIni:COM:lFontSize := oac:lFontSize
      Default oac:oIni:COM:cPathDbf  := oac:cPathDbf
      Default oac:oIni:COM:aFileDbf  := oac:aFileDbf
      Default oac:oIni:COM:nWndKfcH  := oac:nWndKfcH
      Default oac:oIni:COM:nWndKfcW  := oac:nWndKfcW
      Default oac:oIni:COM:aBClrDbf  := oac:aBClrDbf
      Default oac:oIni:COM:aBClrMain := oac:aBClrMain
      cFont := oac:oIni:COM:cFontName
      nSize := oac:oIni:COM:nFontSize
      oac:cPathDbf  := oac:oIni:COM:cPathDbf
      oac:aFileDbf  := oac:oIni:COM:aFileDbf
      oac:nWndKfcH  := oac:oIni:COM:nWndKfcH
      oac:nWndKfcW  := oac:oIni:COM:nWndKfcW
      oac:cTitle    := oac:oIni:COM:cTitle
      oac:aBClrMain := oac:oIni:COM:aBClrMain
      oac:aBClrDbf  := oac:oIni:COM:aBClrDbf
   ENDIF

   IF oac:lFontSize
      IF     Sys.DesktopWidth >= 1920 ; nSize += 4 ; nBmp := 32 ; nIco := 64
      ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2 ; nIco := 32
      ENDIF
   ENDIF

   oac:nMenuBmpH := nBmp
   oac:nIconSize := nIco

   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )

   _SetGetLogFile( oac:cLogFile )

   IF oac:lLogDel ; hb_FileDelete( oac:cLogFile )
   ENDIF

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( oac:nMenuBmpH )
   //
   SET DEFAULT ICON TO oac:cIcoDef

   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ; ? Version() ; ? hb_Ccompiler()
   ? "Computer/User: " + NetName() + "/" + hb_UserName()

RETURN

FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

FUNCTION myInkeyGui(nVal)   
   InkeyGui(nVal)
RETURN .T.

////////////////////////////////////////////////////////////////////////////
//          Streaming functions in MiniGui
FUNCTION myThr_Start(cFile, hWnd, nMsg, nSek)
   LOCAL cMsg

   IF ! hb_mtvm()
      cMsg := "No multithreading support !;"
      cMsg += "Program compilation key -mt !;"
      AlertStop( cMsg, App.ExeName , , 64, {RED} )
      ? cMsg
      RETURN 0
   ENDIF

   Default nMsg := 220, nSek := 2

RETURN hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @myThr_File(), cFile, hWnd, nMsg, nSek )

FUNCTION myThr_Stop(hThr)

   IF !Empty(hThr) ; hb_threadDetach( hThr )
   ENDIF

RETURN NIL

FUNCTION myThr_File(cFile, hWnd, nMsg, nSek)
   LOCAL lFile, lPost := .T.

   ? ProcNL(), HB_DATETIME(), "<<<", cFile, hWnd, nMsg, nSek, lPost
   ?? "IsIconic()=", IsIconic(hWnd)

   DO WHILE .T.
      IF lPost .and. ( lFile := hb_FileExists(cFile) )
         DO EVENTS       //     v--- "i_winuser.ch"
         PostMessage( hWnd, WM_WND_LAUNCH, nMsg, 0 )
         lPost := .F.
      ENDIF
      // wait nSek секунд
      DO EVENTS ; wApi_Sleep(1000 * nSek ) ; DO EVENTS

      ? ProcNL(), "<<<", HB_DATETIME(), cFile, lFile, lPost
   ENDDO

RETURN NIL

