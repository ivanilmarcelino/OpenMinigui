/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 *
 * Работа с окнами MAIN, STANDARD, MODAL и таблицами TsBrowse.
 * Как убрать "подвисание/зависание" программы (потерю фокуса между окнами MODAL)
 * Правильная установка режима работы вложенных MODAL окон
 * Working with windows MAIN, STANDARD, MODAL and tables TsBrowse.
 * How to remove the "freezing/hanging" of the program (loss of focus between MODAL windows)
 * Correct setting of the operating mode of nested MODAL windows
 * Use button events (:Event) instead of functions!
 */
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX
#define PROGVER  "Version 0.6 (16.07.2025)"

FUNCTION Main()
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW, nG, owc, oac := App.Cargo
   LOCAL cTitle := oac:cTitle, aBClr := oac:aBClrMain
   LOCAL nHkfc := oac:nWndKfcH   // window height from Desktop (value from 0 to 1)
   LOCAL nWkfc := oac:nWndKfcW   // window width  from Desktop (value from 0 to 1)
   Default aBClr := {0, 191, 255}
   //
   IF !Set_DataBase_Tsb_Open()
      AlertStop("The database is not open !")
      QUIT
   ENDIF
   //
   Set_Default_Tsb_Param()
   Set_Default_Tsb_Report()
   //
   nY := nX := 0
   nW := Sys.DesktopWidth
   nH := App.Object:H2 + App.Object:nMargHeight * 2 + ;
         GetTitleHeight() + GetBorderHeight()
   nG := App.Object:nMargWidth

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
                  "Exit"}, "Btn_")

      owc:nHIco := 128
      owc:hIcon := LoadIconByName( App.Cargo:cIcoDef, owc:nHIco, owc:nHIco )
      owc:aIcon := { 128, nG, owc:nHIco, owc:nHIco }

      DRAW ICON IN WINDOW &cForm AT owc:aIcon[1], owc:aIcon[2] HICON owc:hIcon ;
                    WIDTH owc:aIcon[3] HEIGHT owc:aIcon[4] COLOR aBClr
      AADD(owc:ahIcoDel, owc:hIcon)  // then we will remove it from the form

      owc:nX := nG*2 + owc:nHIco
      @ owc:aIcon[1], owc:nX LABEL Label_Info VALUE App.Cargo:cInfo WIDTH nW - owc:nX HEIGHT 150 TRANSPARENT

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
       :Event( 0, {|ow      | ow:SetFocus(ow:Cargo:cFocus) })
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
                    dbCloseAll() 
                    ? "==>> " + ProcName() + "(" + HB_NtoS(ProcLine()) + ")"
                    ?? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                    ? Repl(".", 5), "=> RELEASE WINDOW <=", ow:Name
                    ? Repl(".", 5),"Delete handle icon - ow:Cargo:ahIcoDel="
                    ?? ah, HB_ValToExp(ah)
                    IF IsArray(ah)
                       AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                    ENDIF
                    Return Nil
                    })
       :Event(99, {|ow| ow:Release() })
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

RETURN NIL

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

RETURN NIL

STATIC FUNCTION Set_DataBase_Tsb_Open()
   LOCAL cDbf, cAls, lRet, lErr, aUse := {}, a
   LOCAL cPth := App.Cargo:cPathDbf
   LOCAL aDbf := App.Cargo:aFileDbf
   
   WaitWindow( "... Wait for the preparation to complete ...;"+App.ExeName, .T. )
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
   WaitWindow()

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
   oac:cInfo     += "Use button events (:Event) instead of functions !" + CRLF + CRLF + PROGVER

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

RETURN

