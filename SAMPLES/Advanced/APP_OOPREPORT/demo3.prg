/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

#translate o2Log( <x,...> ) => _o2Log( <x> )

REQUEST DBFCDX

FIELD FIRST, LAST, AGE, STATE, CITY, INCOMING, OUTLAY

STATIC s_lCloseMain := .T.
*-----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*-----------------------------------------------------------------------------*
   LOCAL cFont := 'Arial', nSize := 11
   LOCAL cFDlg := "DejaVu Sans Mono"
   LOCAL nFDlg := nSize + 2
   LOCAL cFLog := '_Msg.log'
   LOCAL cIcoDef := "hmg_ico"
/*
   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN
*/
   RddSetDefault("DBFCDX")

   SET CENTURY    ON
   SET DATE       GERMAN
   SET DELETED    ON
   SET EXCLUSIVE  ON
   SET EPOCH TO   2000
   SET AUTOPEN    ON
   SET EXACT      ON
   SET SOFTSEEK   ON
   *--------------------------------
   SET OOP ON
   *--------------------------------
   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO cIcoDef
   SET TOOLTIP ON
   SET TOOLTIP BALLOON ON
   SET WINDOW MODAL PARENT HANDLE ON  

   SET FONT TO cFont, nSize

   DEFINE FONT Normal   FONTNAME cFont SIZE nSize
   DEFINE FONT FontBold FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT FontNorm FONTNAME "Courier New" SIZE nSize

   DEFINE FONT DlgFont FONTNAME cFDlg SIZE nFDlg

   _SetGetLogFile(cFLog)  ; fErase(cFLog)

   IF (App.Object):IsError   // ошибка создания обработчика сообыти App.Object
      MsgStop( (App.Object):cError, "ERROR" )
      QUIT
   ENDIF

   WITH OBJECT ( App.Cargo := oHmgData() )
    :cWndMain    := "wMain"
    :cFontName   := cFont
    :nFontSize   := nSize
    :cFDlgName   := cFDlg
    :nFDlfSize   := nFDlg
    :cFileLog    := cFLog
    :cTitle      := 'MiniGUI Demo for TBrowse report. ' + MiniGUIVersion()
    :nTimer1     := 3          // сек. interval for timers
    :nTimer1Nn   := 0          // count for Timer_1
    :cTimer1Log  := "_timer_1.log"
    :nTimer2     := 5          // сек. interval for timers
    :nTimer2Nn   := 0          // count for Timer_2
    :cTimer2Log  := "_timer_2.log"
    :bInitTsbDef := {|ob|      // default code block для всех TsBrowse
                      ob:Cargo := oHmgData() 
                      ob:nColOrder    :=  0 
                      ob:lNoVScroll   := .F.
                      ob:lNoHScroll   := .T.
                      ob:lNoGrayBar   := .F.
                      ob:lNoLiteBar   := .F.
                      ob:lNoResetPos  := .F.
                      ob:lPickerMode  := .F.
                      ob:lNoChangeOrd := .T.
                      ob:nStatusItem  :=  0 
                      ob:lNoKeyChar   := .T.
                      ob:nWheelLines  :=  1 
                      ob:nCellMarginLR := 1
                      ob:nLineStyle := LINES_ALL
                      ob:nClrLine := COLOR_GRID
                      ob:lCheckBoxAllReturn := .T.
                      Return Nil
                      }
   END WITH

   WITH OBJECT App.Object
    // set events App\Program begin
    :Event({ 1, "Timer_1"}, {|oa,ky,nn,cnam|   // таймер 1
           Local oMain := App.Cargo:oMain, cAls, oRec
           Local oThis, cFunc, xRet, cKey, nKey
           oMain:Enabler(cnam, .F.)            // выкл.timer
                                               // --- для примера
           cKey  := oa:oEvents:Get(ky, "???")  // имя события от кода его
           nKey  := oa:oEvents:Get(cKey, 0)    // код события от его имени
                                               // ----
           cFunc := "my" + cnam                // имя ф-ии обработчика
           ? "#", "O:"+oa:ClassName,ky,nn,cnam, cKey, nKey, cFunc, oMain:Name
           oThis := _oThis()                   // снимок this среды
           oThis:cFunc := cFunc
           oThis:cKey  := cKey
           oThis:nKey  := nKey
           oThis:cLog  := App.Cargo:cFileLog
           IF HB_ISOBJECT(oMain:Cargo:oBrw)
              cAls := (oMain:Cargo:oBrw:cAlias)
              oRec := (cAls)->( oRecGet() )
              oRec:nRecNo := (cAls)->( RecNo() )
              oThis:oRec := oRec
           ENDIF
           xRet := hb_ExecFromArray( cFunc, {oThis} )
           oMain:Enabler(cnam, .T.) // вкл. timer
           nn := ky
           oThis := Nil
           Return Nil
           })

    :Event({ 2, "Timer_2"}, {|oa,ky,nn,cnam|   // таймер 2
           Local oMain := App.Cargo:oMain
           Local oThis, cFunc, xRet, cKey, nKey
           oMain:Enabler(cnam, .F.)            // выкл.timer
                                               // --- для примера
           cKey  := oa:oEvents:Get(ky, "???")  // имя события от кода его
           nKey  := oa:oEvents:Get(cKey, 0)    // код события от его имени
                                               // ----
           cFunc := "my" + cnam                // имя ф-ии обработчика
           ? "#", "O:"+oa:ClassName,ky,nn,cnam, cKey, nKey, cFunc, oMain:Name
           oThis := _oThis()                   // снимок this среды
           oThis:cFunc := cFunc
           oThis:cKey  := cKey
           oThis:nKey  := nKey
           oThis:cLog  := App.Cargo:cFileLog
           xRet := hb_ExecFromArray( cFunc, {oThis} )
           oMain:Enabler(cnam, .T.) // вкл. timer
           nn := ky
           oThis := Nil
           Return Nil
           })
    // set events App\Program end
   END WITH

   fErase( App.Cargo:cTimer1Log )
   fErase( App.Cargo:cTimer2Log )

RETURN

*-----------------------------------------------------------------------------*
FUNCTION Main()
*-----------------------------------------------------------------------------*
   LOCAL cN, nY, nX, nW, nH, hSpl, oTabl, cAlias
   LOCAL cWnd := App.Cargo:cWndMain
   LOCAL cDbf := "Employee"
   LOCAL cAls := "BASE"

   USE ( cDbf )  ALIAS ( cAls ) SHARED  NEW
   IF hb_FieldLen("FIRST") <= 15
      StruMod(cDbf, cAls, dbStruct())
   ENDIF

   cAlias := Alias()

   //Set_bEvents("App_OnEvents")              // NO static function
   Set_bEvents( {|...| App_OnEvents(...) } )  // static function

   DEFINE WINDOW &cWnd AT 0,0 WIDTH 950 HEIGHT 650 ;
      TITLE App.Cargo:cTitle ;
      MAIN TOPMOST NOMAXIMIZE  NOSIZE ;
      ON INIT    {|| _wPost(0) }  ;
      ON RELEASE {|| dbCloseAll() } ;
      ON INTERACTIVECLOSE s_lCloseMain
      This.Cargo := oHmgData()

      App.Cargo:oMain := This.Object

      // Right Click Menu items
      This.Cargo:aTxt := {'Column report FIRST', ;
                          'Column report LAST ', ;
                          'Column report AGE  ', ;
                          'Column report STATE', ;
                          'Column report CITY ', ;
                          'Column report STATE + Left(LAST, 1)', ;
                          'Column report CITY + Left(LAST, 1) ' }
      // array keys code block to summarize the report
      This.Cargo:aKeys2Sum := { {|| Alltrim( FIRST ) }, ;
                                {|| Alltrim( LAST )  }, ;
                                {|| hb_ntos( AGE )  }, ;
                                {|| Alltrim( STATE ) }, ;
                                {|| Alltrim( CITY )  }, ;
                                {|| STATE + ', ' + LEFT( LAST, 1 ) + '...' }, ;
                                {|| CITY  + ', ' + LEFT( LAST, 1 ) + '...' } }

        DEFINE STATUSBAR BOLD
         STATUSITEM '' ;
                       ACTION Nil
         STATUSITEM '' WIDTH This.ClientWidth * 0.25 ;
                       ACTION {|| MsgBox("Press 1 - "+This.Name, ThisWindow.Name) } ;
                       TOOLTIP "Click my" 
         STATUSITEM '' WIDTH This.ClientWidth * 0.25 ;
                       ACTION {|| MsgBox("Press 2 - "+This.Name, ThisWindow.Name) } ;
                       TOOLTIP "Click my" 
         STATUSITEM '' WIDTH This.ClientWidth * 0.25 ;
                       ACTION Nil
      END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION "REPORT" BUTTONSIZE 52,32 FLAT

         BUTTON E0  CAPTION ' '      PICTURE 'cabinet' ACTION Nil ;
                                      SEPARATOR
         BUTTON 01  CAPTION 'First' PICTURE 'n1'    ;
                    TOOLTIP This.Cargo:aTxt[1]+'  Ctrl+1, Shift+1'  ;
                    ACTION  _wPost(/*1*/'отчет First') SEPARATOR
         BUTTON 02  CAPTION 'Last'   PICTURE 'n2'   ;
                    TOOLTIP This.Cargo:aTxt[2]+'  Ctrl+2, Shift+2'  ;
                    ACTION  _wPost(/*2*/'отчет Last') SEPARATOR
         BUTTON 03  CAPTION 'Age'     PICTURE 'n3'  ;
                    TOOLTIP This.Cargo:aTxt[3]+'  Ctrl+3, Shift+3'  ;
                    ACTION  _wPost(/*3*/'отчет Age') SEPARATOR
         BUTTON 04  CAPTION 'State' PICTURE 'n4'    ;
                    TOOLTIP This.Cargo:aTxt[4]+'  Ctrl+4, Shift+4'  ;
                    ACTION  _wPost(4) SEPARATOR
         BUTTON 05  CAPTION 'City'   PICTURE 'n5'   ;
                    TOOLTIP This.Cargo:aTxt[5]+'  Ctrl+5, Shift+5'  ;
                    ACTION  _wPost(5) SEPARATOR
         BUTTON 06  CAPTION 'State ?' PICTURE 'n6'                  ;
                    TOOLTIP This.Cargo:aTxt[6]+'  Ctrl+6, Shift+6'  ;
                    ACTION  _wPost(6) SEPARATOR
         BUTTON 07  CAPTION 'City ?'  PICTURE 'n7'                  ;
                    TOOLTIP This.Cargo:aTxt[7]+'  Ctrl+7, Shift+7'  ;
                    ACTION  _wPost(7) SEPARATOR
      END TOOLBAR

      DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 99  CAPTION 'Exit'  PICTURE 'exit'  ACTION _wPost(/*99*/'Exit')
      END TOOLBAR
      END SPLITBOX

      nY := GetWindowHeight(hSpl)
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - This.StatusBar.Height - nY

      DEFINE TBROWSE oTabl OBJ oTabl  AT nY, nX  ALIAS cAlias CELL ;
             WIDTH nW HEIGHT nH TOOLTIP 'Right click - context menu'                              ;
             FONT    {"Normal", "FontBold", "FontBold"}                        ;
             FOOTERS .T. EDIT                                                  ;
             COLUMNS {"FIRST", "LAST", "AGE", "STATE", "CITY", "INCOMING", "OUTLAY"} ;
             COLNUMBER {1, 40} LOADFIELDS COLSEMPTY ;
             ON INIT {|ob| Eval( App.Cargo:bInitTsbDef, ob ) }
             
         AEval(:aColumns, {|oc| oc:nWidth *= iif( oc:cFieldTyp == "C", 0.5, 1 ), ;
                                oc:lFixLite := .T. })
         // Right Click Menu items
         :Cargo:aTxt := ThisWindow.Cargo:aTxt
         // array keys code block to summarize the report
         :Cargo:aKeys2Sum := ThisWindow.Cargo:aKeys2Sum

         :SetAppendMode( .F. )
         :SetDeleteMode( .F. )

         :nHeightCell  += 2
         :nHeightHead  := :nHeightCell + 2
         :nHeightFoot  := :nHeightCell + 2

         :bChange   := {|ob| ob:DrawFooters() }
         :bRClicked := {|p1,p2,p3,ob| p1:=p2:=p3, ;
                         _wPost(/*90*/"RClickMenu", ob:cParentWnd, ob) }

         :SetColor( { CLR_FOCUSB }, { { |a,b,c| a:=b, If( c:nCell == b, {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                                                                        {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } )

         :aColumns[ 1 ]:cFooting := { |nc,ob| nc := ob:nAtPos, iif( Empty(nc), '', hb_ntos(nc) ) }
         :aColumns[ 2 ]:cFooting := hb_ntos( (cAlias)->( LastRec() ) )

         :UserKeys( VK_1, {|ob| _wPost('отчет First'  , ob:cParentWnd) } ) 
         :UserKeys( VK_2, {|ob| _wPost('отчет Last'   , ob:cParentWnd) } ) 
         :UserKeys( VK_3, {|ob| _wPost('отчет Age'    , ob:cParentWnd) } ) 
         :UserKeys( VK_4, {|ob| _wPost('отчет State'  , ob:cParentWnd) } ) 
         :UserKeys( VK_5, {|ob| _wPost('отчет City'   , ob:cParentWnd) } ) 
         :UserKeys( VK_6, {|ob| _wPost('отчет State ?', ob:cParentWnd) } ) 
         :UserKeys( VK_7, {|ob| _wPost('отчет City ?' , ob:cParentWnd) } ) 
                                                                  
         :ResetVScroll( .T. )
         :oHScroll:SetRange( 0, 0 )
         :AdjColumns()

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      ON KEY SHIFT+1 ACTION _wPost('отчет First'  ) // 1
      ON KEY SHIFT+2 ACTION _wPost('отчет Last'   ) // 2
      ON KEY SHIFT+3 ACTION _wPost('отчет Age'    ) // 3
      ON KEY SHIFT+4 ACTION _wPost('отчет State'  ) // 4
      ON KEY SHIFT+5 ACTION _wPost('отчет City'   ) // 5
      ON KEY SHIFT+6 ACTION _wPost('отчет State ?') // 6
      ON KEY SHIFT+7 ACTION _wPost('отчет City ?' ) // 7
      ON KEY ESCAPE  ACTION {|| iif( oTabl:IsEdit, oTabl:SetFocus(), _wPost(99) ) }  // выход по ESC

      cN := "Timer_1"
      DEFINE TIMER &(cN) INTERVAL App.Cargo:nTimer1 * 1000  ACTION {|| NIL } 
              This.&(cN).Enabled := .F.     // отключить таймер до ON INIT
              This.&(cN).Cargo   := {1, cN} // nMsg for App.Object

      cN := "Timer_2"
      DEFINE TIMER &(cN) INTERVAL App.Cargo:nTimer2 * 1000 ACTION {|| NIL } 
              This.&(cN).Enabled := .F.     // отключить таймер до ON INIT
              This.&(cN).Cargo   := {2, cN} // nMsg for App.Object

      WITH OBJECT This.Object          // ---- Window events
      // object TsBrowse
      :Cargo:oBrw := oTabl            
      // StatusBar
      :StatusBar:Say(" Timer - 1", 2)
      :StatusBar:Say(" Timer - 2", 3)
      // ToolBar 1
      :Event(  0, {|ow| 
                   Local lTm_1, cStb1 := ow:StatusBar:Get(2)
                   Local lTm_2, cStb2 := ow:StatusBar:Get(3)
                   This.Topmost := .F.
                   ow:Enabler("Timer_1", .T.)      // timer on
                   ow:Enabler("Timer_2", .T.)      // timer on
                   lTm_1 := This.Timer_1.Enabled
                   lTm_2 := This.Timer_2.Enabled
                   ow:StatusBar:Say(cStb1 + ": "+ iif( lTm_1, "ON", "OFF" ), 2)
                   ow:StatusBar:Say(cStb2 + ": "+ iif( lTm_2, "ON", "OFF" ), 3)
                   This.oTabl.SetFocus 
                   Return Nil
                   } )
      :Event({ 1, 'отчет First'  }, {|ow,ky| Report(ow, ky) } )
      :Event({ 2, 'отчет Last'   }, {|ow,ky| Report(ow, ky) } )
      :Event({ 3, 'отчет Age'    }, {|ow,ky| Report(ow, ky) } )
      :Event({ 4, 'отчет State'  }, {|ow,ky| Report(ow, ky) } )
      :Event({ 5, 'отчет City'   }, {|ow,ky| Report(ow, ky) } )
      :Event({ 6, 'отчет State ?'}, {|ow,ky| Report(ow, ky) } )
      :Event({ 7, 'отчет City ?' }, {|ow,ky| Report(ow, ky) } )
      // ToolBar 2
      :Event({99, 'Exit'}, {|ow  | ow:Release() } )
      // Tsb. Right click - context menu
      :Event({90, "RClickMenu"}, {|ow,ky,ob| ky := ob:Cargo:aTxt, ;
                                             MenuReport(ow, ky) } )
      // StatusBar
      :Event( 91, {|ow| ow:StatusBar:Say('... W A I T ...') } )
      :Event( 92, {|ow| ow:StatusBar:Say('')               } )
      END WITH                         // ---- Window events

   END WINDOW

     CENTER WINDOW &cWnd
   ACTIVATE WINDOW &cWnd

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION _ShowFormContextMenu( cForm, nRow, nCol, lCenter )
*-----------------------------------------------------------------------------*
   LOCAL xContextMenuParentHandle := 0, hWnd, aRow

   DEFAULT nRow := -1, nCol := -1, lCenter := .F.

   If .Not. _IsWindowDefined(cForm)
      xContextMenuParentHandle := _HMG_xContextMenuParentHandle
   Else
      xContextMenuParentHandle := GetFormHandle(cForm )
   Endif

   If xContextMenuParentHandle == 0
      MsgMiniGuiError("Context Menu is not defined. Program terminated")
   EndIf

   lCenter := lCenter .or. ( nRow == 0 .or. nCol == 0 )
   hWnd    := GetFormHandle(cForm)

   If lCenter
      If nCol == 0
         nCol := int( GetWindowWidth (hWnd) / 2 )
      EndIf
      If nRow == 0
         nRow := int( GetWindowHeight(hWnd) / 2 )
      EndIf
   ElseIf nRow < 0 .or. nCol < 0
      aRow := GetCursorPos()
      nRow := aRow[1]
      nCol := aRow[2]
   EndIf

   TrackPopupMenu ( _HMG_xContextMenuHandle , nCol , nRow , xContextMenuParentHandle )

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION MenuReport( oWnd, aTxt, lPost, nRow, nCol, lCenter, nZeroLen )
*-----------------------------------------------------------------------------*
   LOCAL cWnd := oWnd:Name
   LOCAL nItm := 0, cNam, cImg, i
   LOCAL lDis := .F.
   LOCAL bAct := {|| nItm := Val(This.Name) }

   IF Empty(aTxt) ; RETURN nItm
   ENDIF

   Default nZeroLen := 4, lPost := .T.

   DEFINE CONTEXT MENU OF &cWnd
      FOR i := 1 TO len(aTxt)
          cNam := StrZero(i, nZeroLen)
          If  i > 9 ; cImg := Nil
          Else      ; cImg := 'n' + hb_ntos(i)
          EndIf
          _DefineMenuItem( aTxt[ i ], bAct, cNam, cImg, .F., lDis, , , , .F., .F.)
      NEXT
      SEPARATOR
      MENUITEM 'Exit' ACTION NIL
   END MENU

   _ShowFormContextMenu(cWnd, nRow, nCol, lCenter )

   DEFINE CONTEXT MENU OF &cWnd
   END MENU
   DO EVENTS

   If nItm > 0 .and. lPost ; _wPost(nItm, oWnd)
   EndIf

RETURN nItm

*-----------------------------------------------------------------------------*
STATIC FUNCTION Report( oWnd, nEvent )
*-----------------------------------------------------------------------------*
   LOCAL nOld := Select(), cKey, aRpt
   LOCAL oBrw := (This.oTabl.Object):Tsb
   LOCAL cAls := oBrw:cAlias
   LOCAL nRec := (cAls)->( RecNo() )
   LOCAL bKeys, o := oKeyData()
   LOCAL cNam := oBrw:aColumns[ nEvent + 1 ]:cHeading

   oBrw:lEnabled := .F.
   oWnd:StatusBar:Say('... W A I T ...')
   This.&(StrZero(nEvent, 2)).Enabled := .F.
   This.E0.Caption := hb_ntos(nEvent)
   DO EVENTS

   // keys to summarize the report
   bKeys := oBrw:Cargo:aKeys2Sum

   wApi_Sleep(200)           // specially delay for the test

   dbSelectArea( cAls )

   GO TOP                    // create report in container object
   DO WHILE ! EOF()
      DO EVENTS
      cKey := Eval( bKeys[ nEvent ] )
      o:Sum( cKey, { 1, cKey, 1, INCOMING, OUTLAY, INCOMING - OUTLAY } )
      SKIP
   ENDDO
   GOTO nRec
                             // report from the container object to the array
   aRpt := o:Values()        // array value {{...}, {...}, ...}

   dbSelectArea( nOld )

   wApi_Sleep(200)           // specially delay for the test

   oWnd:StatusBar:Say('')
   DO EVENTS

   TsbReport( oWnd, nEvent, aRpt, cNam )

   (This.oTabl.Object):Tsb:lEnabled := .T.      // oBrw:lEnabled := .T.
   (This.oTabl.Object):SetFocus()               // oBrw:SetFocus()
   This.&(StrZero(nEvent, 2)).Enabled := .T.
   This.E0.Caption := ''
   DO EVENTS

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION TsbReport( oWnd, nEvent, aArray, cColName )
*-----------------------------------------------------------------------------*
   LOCAL aCap, oRpt, hSpl, nY, nX, nW, nH
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aName, aFontHF
   LOCAL a, i, o := oKeyData()
   LOCAL oBrw := oWnd:Cargo:oBrw

   // calculate the results
   FOR EACH a IN aArray
       For i := 1 To Len(a)
           If i < 3 ; o:Sum(i, 1)        // quantity
           Else     ; o:Sum(i, a[ i ])   // amount
           EndIf
       Next
   NEXT

   a := o:Values()              // array {{value}, ...}
   i := Len( a )

   aAlign := array( i )
   aSize  := array( i )
   aPict  := array( i )
   aFoot  := array( i )
   aSize [ 1 ] := 50
   aPict [ 1 ] := '9999'
   aAlign[ 1 ] := DT_CENTER

   AEval(a, {|ns,nn| aFoot[ nn ] := iif( nn == 1, '', hb_ntos(ns) ) })

   // report title Report
   aCap := oBrw:Cargo:aTxt
   
   If   nEvent == 6
      cColName := 'State, Last ? ...'
   ElseIf nEvent == 7
      cColName := 'City, Last ? ...'
   ElseIf nEvent == 3 .or. nEvent == 4    // Age, State
      aSize [ 2 ] := 80
      aAlign[ 2 ] := DT_CENTER
   EndIf

   // report column headers
   aHead := { "#", cColName, "Quantity", "Incoming", "Outlay", "Balance" }

   aFontHF := GetFontHandle("FontBold")

   nY := nX := 0 ; nW := 800 ; nH := 450+GetTitleHeight()+GetBorderHeight()

   DEFINE WINDOW Report AT nY,nX WIDTH nW HEIGHT nH TITLE aCap[ nEvent ] ;
          MODAL NOSIZE 

      This.Cargo := oHmgData()

      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 01  CAPTION 'Print'  PICTURE 'printer'  ;
                       TOOLTIP 'Report printing   F5'  ;
                       ACTION  _wPost(1)  SEPARATOR
         BUTTON 02  CAPTION 'Excel'  PICTURE 'excel'   ;
                       TOOLTIP 'Export to MS Excel   F6' ;
                       ACTION  _wPost(2)  SEPARATOR
      END TOOLBAR

      DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 99  CAPTION 'Exit'  PICTURE 'exit'  ACTION _wPost(99)
      END TOOLBAR
      END SPLITBOX

      nY := GetWindowHeight(hSpl)
      nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight - nY

      DEFINE TBROWSE oRpt OBJ oRpt AT nY,nX WIDTH nW HEIGHT nH CELL ;
             TOOLTIP 'Double click on title - sorting' ;
             ON INIT {|ob| Eval( App.Cargo:bInitTsbDef, ob ) }

      :SetArrayTo( aArray, aFontHF, aHead, aSize, aFoot, aPict, aAlign, aName )

      :nHeightCell  += 5
      :nHeightHead  := :nHeightCell + 2
      :nHeightFoot  := :nHeightCell + 2
      :lDrawFooters := .T.
      :lFooting     := .T.

      :SetColor( { CLR_FOCUSB }, { { |a,b,c| a:=b,  If( c:nCell == b, {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                                                                      {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } )
      :aColumns[ 1 ]:bData := {|| oRpt:nAt }

      If nEvent == 6 .or. nEvent == 7
         :aColumns[ 2 ]:hFont  := GetFontHandle('FontNorm')
         :aColumns[ 2 ]:nWidth += 70
      EndIf

      :UserKeys( VK_F5, {|ob| _wPost(/*1*/"Print", ob) } )
      :UserKeys( VK_F6, {|ob| _wPost(/*2*/"Excel", ob) } )

      :lNoChangeOrd := .F.
      AEval( :aColumns, {|oc,nc| oc:lFixLite := .T., ;
                                 oc:lIndexCol := nc > 1 })

      :AdjColumns({3, 4, 5, 6})   // :AdjColumns()

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(99)

      This.Cargo:oBrw := oRpt
      // ToolBar 1
      (This.Object):Event({ 1, "Print"}, {|ow| oWnd:StatusBar:Say('... W A I T ...'), ;
           AlertInfo('P r i n t i n g.  This.Name = ' + This.Name, ow:Name), ;
                                     oWnd:StatusBar:Say('') } )
      (This.Object):Event({ 2, "Excel"}, {|ow| oWnd:PostMsg(91),                      ;
           MsgBox('Export to MS Excel. This.Name = ' + This.Name, ow:Name), ;
                                     oWnd:PostMsg(92) } )
      // ToolBar 2
      (This.Object):Event( 99, {|ow| ow:Release()  } )

   END WINDOW

     CENTER WINDOW Report
   ACTIVATE WINDOW Report

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION StruMod( cDbf, cAls, aStru  )
*-----------------------------------------------------------------------------*
    LOCAL i

    FOR i := 1 TO Len(aStru)
        IF aStru[ i ][2] == "C" ; aStru[ i ][3] *= 2
        ENDIF
    NEXT

    USE

    dbCreate( cDbf+"__", aStru, , .T., "_TMP_" )
    APPEND FROM ( cDbf )
    dbCloseArea()

    fErase(cDbf+".dbf")
    fErase(cDbf+".cdx")
    fRename(cDbf+"__"+".dbf", cDbf+".dbf")

    USE ( cDbf )  ALIAS ( cAls ) SHARED  NEW

RETURN .T.

STATIC FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )
   LOCAL cMsg, nRet := 0, ky, cn, cm, ap, nn, i

   IF hWnd == _HMG_MainHandle   // только для MAIN window
      SWITCH nMsg
      **********************************************************************
      CASE WM_TIMER
      **********************************************************************
        cMsg := "Event arrived - WM_TIMER"
   
        IF ( i := AScan ( _HMG_aControlIds, wParam ) ) > 0
           cn := _HMG_aControlNames    [ i ]       // timer name
           ap := _HMG_aControlMiscData2[ i ]       // Cargo
           IF ( nn := RAt("_", cn) ) > 0
              nn := Val( subs(cn, nn + 1) )
           ENDIF
           ? "$", ProcName(), cMsg, hb_datetime(), nn, cn, hb_valtoexp(ap)
           IF HB_ISARRAY( ap ) .and. Len( ap ) > 0 // работаем
              ky := ap[1]                          // nPost msg
              cm := ap[2]                          // cPost msg
              // Timer работа не меняет среду This
            //_pPost(ky, nn, cn)          // nPost msg -> App.Object
              _pPost(cm, ky, cn)          // cPost msg -> App.Object
           ENDIF
           i := lParam
        ENDIF                               

        nRet := 1           
        EXIT
      
      END SWITCH
   ENDIF

   IF nRet > 0 ; RETURN 1   // nMsg выполненно
   ENDIF

RETURN 0                    // nMsg НЕ выполненно

*-----------------------------------------------------------------------------*
FUNCTION myTimer_1( oThis )
*-----------------------------------------------------------------------------*
   LOCAL o  := App.Cargo
   LOCAL ow := o:oMain
   LOCAL nItm := 2, cStb 
   LOCAL nCnt := o:nTimer1Nn + 1
   LOCAL cLog := App.Cargo:cTimer1Log
   
   IF Len( hb_ntos( nCnt ) ) > 12 ; nCnt := 1
   ENDIF
   o:nTimer1Nn := nCnt
   cStb := ow:StatusBar:Get(nItm)    // StatusBar item 2
   cStb := Left(cStb, At(":", cStb))

   ow:StatusBar:Say(cStb + " " + hb_ntos( nCnt ), nItm) ; DO EVENTS

   _LogFile({.T., cLog}, "@@@ Timer_1 - 1", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //_LogFile({.F., cLog}, ProcLine(), "***")
   //_LogFile(cLog, "@@@ Timer_1 - 2", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //_LogFile(cLog)
   //_LogFile({cLog, .T.}, "@@@ Timer_1 - 3", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //? "@@@ Timer_1", HB_DATETIME(), ProcName(), nCnt
   //o2Log(oThis, 19, "*** oThis .F. =>", .F., cLog)
   //?
   _o2Log(oThis, 19, "*** oThis .T. =>", .T., cLog)
   ?
   To2Log(cLog)
   ?

RETURN .T.

*-----------------------------------------------------------------------------*
FUNCTION myTimer_2( oThis )
*-----------------------------------------------------------------------------*
   LOCAL o  := App.Cargo
   LOCAL ow := o:oMain
   LOCAL nItm := 3, cStb 
   LOCAL nCnt := o:nTimer2Nn + 1
   LOCAL cLog := App.Cargo:cTimer2Log

   IF Len( hb_ntos( nCnt ) ) > 12 ; nCnt := 1
   ENDIF
   o:nTimer2Nn := nCnt
   cStb := ow:StatusBar:Get(nItm)    // StatusBar item 3
   cStb := Left(cStb, At(":", cStb))

   ow:StatusBar:Say(cStb + " " + hb_ntos( nCnt ), nItm) ; DO EVENTS

   _LogFile({.T., cLog}, "@@@ Timer_2 - 1", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //_LogFile({.F., cLog}, ProcLine(), "***")
   //_LogFile(cLog, "@@@ Timer_2 - 2", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //_LogFile(cLog)
   //_LogFile({cLog, .T.}, "@@@ Timer_2 - 3", HB_DATETIME(), ProcName(), ProcLine(), nCnt)
   //? "@@@ Timer_2", HB_DATETIME(), ProcName(), nCnt
   o2Log(oThis, 19, "*** oThis 1 =>", .F., cLog)
   ?
   //o2Log(oThis, 19, "*** oThis 2 =>", .T., cLog)
   //?
   To2Log(cLog)
   ?

RETURN .T.

FUNCTION To2Log(cLog)
   //o2Log(_oThis(), 19, "*** _oThis() .F. =>", .F., cLog)
   //?
   o2Log(_oThis(), 19, "### _oThis() .T. =>", .T., cLog)
   ?
   //o2Log(_oThis():FormCargo, 19, "*** _oThis():FormCargo .T. =>", .T., cLog)
   //?
   //o2Log(_HMG_aEventInfo,12, "*** _HMG_aEventInfo =>", , cLog)
   o2Log("Forms*" , , , , cLog)
   //o2Log("Forms*" , , , .T., cLog)
   o2Log("procNL*", , , , cLog)
   //o2Log(App.Cargo, 19, "*** App.Cargo =>", , , , cLog)
   ?
RETURN Nil
