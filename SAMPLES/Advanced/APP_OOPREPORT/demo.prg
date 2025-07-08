/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

FIELD FIRST, LAST, AGE, STATE, CITY, INCOMING, OUTLAY

*-----------------------------------------------------------------------------*
FUNCTION Main()
*-----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, hSpl, oTabl, cAlias
   LOCAL cWnd := 'wMain'
   LOCAL cDbf := "Employee"
   LOCAL cAls := "BASE"

   RddSetDefault("DBFCDX")

   SET CENTURY    ON
   SET DATE       GERMAN
   SET DELETED    ON
   SET EXCLUSIVE  ON
   SET EPOCH TO   2000
   SET AUTOPEN    ON
   SET EXACT      ON
   SET SOFTSEEK   ON

   SET NAVIGATION EXTENDED
   SET FONT       TO "Arial", 11
   SET DEFAULT ICON TO "hmg_ico"

   *--------------------------------
   SET OOP ON
   *--------------------------------

   DEFINE FONT Normal   FONTNAME _HMG_DefaultFontName ;
                            SIZE _HMG_DefaultFontSize
   DEFINE FONT FontBold FONTNAME _HMG_DefaultFontName ;
                            SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT FontNorm FONTNAME "Courier New"       ;
                            SIZE _HMG_DefaultFontSize

   USE ( cDbf )  ALIAS ( cAls ) SHARED  NEW
   IF hb_FieldLen("FIRST") <= 15
      StruMod(cDbf, cAls, dbStruct())
   ENDIF

   cAlias := Alias()

   DEFINE WINDOW &cWnd AT 0,0 WIDTH 950 HEIGHT 650 ;
      TITLE 'MiniGUI Demo for TBrowse report' ;
      MAIN  NOMAXIMIZE  NOSIZE ;
      ON RELEASE  dbCloseAll() ;
      ON INTERACTIVECLOSE (This.Object):Action

        DEFINE STATUSBAR BOLD
         STATUSITEM ''          ACTION Nil
         STATUSITEM '' WIDTH  80 ACTION Nil
         STATUSITEM '' WIDTH 430 ACTION Nil
      END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION "REPORT" BUTTONSIZE 52,32 FLAT

         BUTTON E0  CAPTION ' '      PICTURE 'cabinet' ACTION Nil ;
                                      SEPARATOR
         BUTTON 01  CAPTION 'First' PICTURE 'n1'    ;
                    TOOLTIP 'Column report FIRST   Ctrl+1, Shift+1'  ;
                    ACTION  _wPost(1) SEPARATOR
         BUTTON 02  CAPTION 'Last'   PICTURE 'n2'   ;
                    TOOLTIP 'Column report LAST Ctrl+2, Shift+2'  ;
                    ACTION  _wPost(2) SEPARATOR
         BUTTON 03  CAPTION 'Age'     PICTURE 'n3'  ;
                    TOOLTIP 'Column report AGE  Ctrl+3, Shift+3'   ;
                    ACTION  _wPost(3) SEPARATOR
         BUTTON 04  CAPTION 'State' PICTURE 'n4'    ;
                    TOOLTIP 'Column report STATE   Ctrl+4, Shift+4'  ;
                    ACTION  _wPost(4) SEPARATOR
         BUTTON 05  CAPTION 'City'   PICTURE 'n5'   ;
                    TOOLTIP 'Column report CITY Ctrl+5, Shift+5'  ;
                    ACTION  _wPost(5) SEPARATOR
         BUTTON 06  CAPTION 'State ?' PICTURE 'n6'                  ;
                    TOOLTIP 'Column report STATE + Left(LAST, 1)  Ctrl+6, Shift+6'  ;
                    ACTION  _wPost(6) SEPARATOR
         BUTTON 07  CAPTION 'City ?'  PICTURE 'n7'                  ;
                    TOOLTIP 'Column report CITY + Left(LAST, 1)   Ctrl+7, Shift+7'  ;
                    ACTION  _wPost(7) SEPARATOR
      END TOOLBAR

      DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 99  CAPTION 'Exit'  PICTURE 'exit'  ACTION _wPost(99)
      END TOOLBAR
      END SPLITBOX

      WITH OBJECT This.Object          // ---- Window events
      // StatusBar
      :StatusBar:Say(MiniGUIVersion(), 3)
      // ToolBar 1
      :Event(  1, {|ow,ky| Report(ow, ky) } )
      :Event(  2, {|ow,ky| Report(ow, ky) } )
      :Event(  3, {|ow,ky| Report(ow, ky) } )
      :Event(  4, {|ow,ky| Report(ow, ky) } )
      :Event(  5, {|ow,ky| Report(ow, ky) } )
      :Event(  6, {|ow,ky| Report(ow, ky) } )
      :Event(  7, {|ow,ky| Report(ow, ky) } )
      // ToolBar 2
      :Event( 99, {|ow  | ow:Release() } )
      // Tsb. Right click - context menu
      :Event( 90, {|ow  | MenuReport(ow) } )
      // StatusBar
      :Event( 91, {|ow  | ow:StatusBar:Say('... W A I T ...') } )
      :Event( 92, {|ow  | ow:StatusBar:Say('')               } )
      END WITH                         // ---- Window events

      nY := GetWindowHeight(hSpl)
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - This.StatusBar.Height - nY

      DEFINE TBROWSE oTabl  AT nY, nX  ALIAS cAlias  WIDTH nW  HEIGHT nH  CELL ;
             TOOLTIP 'Right click - context menu'                              ;
             FONT    {"Normal", "FontBold", "FontBold"}                        ;
             FOOTERS .T. EDIT                                                  ;
             COLUMNS {"FIRST", "LAST", "AGE", "STATE", "CITY", "INCOMING", "OUTLAY"} ;
             COLNUMBER {1, 40} LOADFIELDS COLSEMPTY

         AEval(:aColumns, {|oc| oc:nWidth *= iif( oc:cFieldTyp == "C", 0.5, 1 ) })

         :SetAppendMode( .F. )
         :SetDeleteMode( .F. )

         :lNoGrayBar := .T.
         :nWheelLines  := 1
         :nClrLine     := COLOR_GRID
         :nHeightCell  += 2
         :nHeightHead  := :nHeightCell + 2
         :nHeightFoot  := :nHeightCell + 2
         :lNoVScroll := .F.
         :lNoHScroll := .T.
         :bChange    := {|ob| ob:DrawFooters() }
         :bRClicked   := {|p1,p2,p3,ob| p1:=p2:=p3:=Nil, _wPost(90, ob:cParentWnd) }

         :SetColor( { CLR_FOCUSB }, { { |a,b,c| If( c:nCell == b, {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                                                                  {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } )

         :aColumns[ 1 ]:cFooting := { |nc,ob| nc := ob:nAtPos, iif( Empty(nc), '', hb_ntos(nc) ) }
         :aColumns[ 2 ]:cFooting := hb_ntos( (cAlias)->( LastRec() ) )

         :UserKeys( VK_1, {|ob| _wPost(1, ob:cParentWnd) }, .T. )
         :UserKeys( VK_2, {|ob| _wPost(2, ob:cParentWnd) }, .T. )
         :UserKeys( VK_3, {|ob| _wPost(3, ob:cParentWnd) }, .T. )
         :UserKeys( VK_4, {|ob| _wPost(4, ob:cParentWnd) }, .T. )
         :UserKeys( VK_5, {|ob| _wPost(5, ob:cParentWnd) }, .T. )
         :UserKeys( VK_6, {|ob| _wPost(6, ob:cParentWnd) }, .T. )
         :UserKeys( VK_7, {|ob| _wPost(7, ob:cParentWnd) }, .T. )

         :ResetVScroll( .T. )
         :oHScroll:SetRange( 0, 0 )
         :AdjColumns()

      END TBROWSE

      oTabl:SetNoHoles()
      oTabl:SetFocus()

      ON KEY SHIFT+1 ACTION _wPost(1)
      ON KEY SHIFT+2 ACTION _wPost(2)
      ON KEY SHIFT+3 ACTION _wPost(3)
      ON KEY SHIFT+4 ACTION _wPost(4)
      ON KEY SHIFT+5 ACTION _wPost(5)
      ON KEY SHIFT+6 ACTION _wPost(6)
      ON KEY SHIFT+7 ACTION _wPost(7)
      ON KEY ESCAPE  ACTION {|| iif( oTabl:IsEdit, oTabl:SetFocus(), _wPost(99) ) }  // выход по ESC

   END WINDOW

   CENTER   WINDOW &cWnd
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
   hWnd   := GetFormHandle(cForm)

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
STATIC FUNC MenuReport( oWnd, aTxt, lPost, nRow, nCol, lCenter, nZeroLen )
*-----------------------------------------------------------------------------*
   LOCAL cWnd := oWnd:Name
   LOCAL nItm := 0, cNam, cImg, i
   LOCAL lDis := .F.
   LOCAL bAct := {|| nItm := Val(This.Name) }

   Default nZeroLen := 4, lPost := .T.
   Default aTxt := { ;
                     'Column report FIRST', ;
                     'Column report LAST ', ;
                     'Column report AGE  ', ;
                     'Column report STATE', ;
                     'Column report CITY ', ;
                     'Column report STATE + Left(LAST, 1)', ;
                     'Column report CITY + Left(LAST, 1) '  ;
                   }

   DEFINE CONTEXT MENU OF &cWnd
      For i := 1 To len(aTxt)
          cNam := StrZero(i, nZeroLen)
          If  i > 9
             cImg := Nil
          Else
             cImg := 'n' + hb_ntos(i)
          EndIf
          _DefineMenuItem( aTxt[ i ], bAct, cNam, cImg, .F., lDis, , , , .F., .F.)
      NEXT
      SEPARATOR
      MENUITEM 'Exit'  ACTION NIL
   END MENU

   _ShowFormContextMenu(cWnd, nRow, nCol, lCenter )

   DEFINE CONTEXT MENU OF &cWnd
   END MENU
   DO EVENTS

   If nItm > 0 .and. lPost
      oWnd:PostMsg(nItm)
   EndIf

RETURN nItm

*-----------------------------------------------------------------------------*
STATIC FUNC Report( oWnd, nEvent )
*-----------------------------------------------------------------------------*
   LOCAL nOld := Select(), cKey, aRpt
   LOCAL oBrw := (This.oTabl.Object):Tsb
   LOCAL cAls := oBrw:cAlias
   LOCAL nRec := (cAls)->( RecNo() )
   LOCAL b, o := oKeyData()
   LOCAL cNam := oBrw:aColumns[ nEvent + 1 ]:cHeading

   oWnd:Action := .F.
   oBrw:lEnabled := .F.
   oWnd:StatusBar:Say('... W A I T ...')
   This.&(StrZero(nEvent, 2)).Enabled := .F.
   This.E0.Caption := hb_ntos(nEvent)
   DO EVENTS

   // keys to summarize the report
   b := { {|| Alltrim( FIRST ) }, ;
          {|| Alltrim( LAST )  }, ;
          {|| hb_ntos( AGE )  }, ;
          {|| Alltrim( STATE ) }, ;
          {|| Alltrim( CITY )  }, ;
          {||         STATE   + ', ' + LEFT( LAST, 1 ) + '...' }, ;
          {||         CITY  + ', ' + LEFT( LAST, 1 ) + '...' }  ;
        }

   wApi_Sleep(500)           // specially delay for the test

   dbSelectArea( cAls )

   GO TOP                    // create report in container object
   DO WHILE ! EOF()
      DO EVENTS
      cKey := Eval( b[ nEvent ] )
      o:Sum( cKey, { 1, cKey, 1, INCOMING, OUTLAY, INCOMING - OUTLAY } )
      SKIP
   ENDDO
   GOTO nRec
                             // report from the container object to the array
   aRpt := o:Eval(.T.)      // array value {{...}, {...}, ...}

   dbSelectArea( nOld )

   wApi_Sleep(500)           // specially delay for the test

   oWnd:StatusBar:Say('')
   DO EVENTS
   oWnd:Action := .T.

   TsbReport( oWnd, nEvent, aRpt, cNam )

   (This.oTabl.Object):Tsb:lEnabled := .T.      // oBrw:lEnabled := .T.
   (This.oTabl.Object):SetFocus()               // oBrw:SetFocus()
   This.&(StrZero(nEvent, 2)).Enabled := .T.
   This.E0.Caption := ''
   DO EVENTS

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC TsbReport( oWnd, nEvent, aArray, cColName )
*-----------------------------------------------------------------------------*
   LOCAL aCap, oRpt, nY, nX, hSpl
   LOCAL aHead, aSize, aFoot, aPict, aAlign, aName, aFontHF
   LOCAL a, i, o := oKeyData()

   // calculate the results
   FOR EACH a IN aArray
       For i := 1 To Len(a)
           If i < 3
              o:Sum(i, 1)        // quantity
           Else
              o:Sum(i, a[ i ])    // amount
           EndIf
       Next
   NEXT

   a := o:Eval(.T.)             // array {{value}, ...}

   aAlign := array(Len(a))
   aSize  := array(Len(a))
   aPict  := array(Len(a))
   aFoot  := array(Len(a))
   aSize [ 1 ] := 50
   aPict [ 1 ] := '9999'
   aAlign[ 1 ] := DT_CENTER

   AEVal(a, {|ns,nn| aFoot[ nn ] := iif( nn == 1, '', hb_ntos(ns) ) })

   // report title Report
   aCap := { 'Column report FIRST', ;
             'Column report LAST' , ;
             'Column report AGE'  , ;
             'Column report STATE', ;
             'Column report CITY' , ;
             'Column report STATE, LAST ?...', ;
             'Column report CITY,  LAST ?...'  ;
            }

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

   DEFINE WINDOW Report ;
      AT   0, 0 ;
      WIDTH  700  ;
      HEIGHT 450 + GetTitleHeight() + GetBorderHeight() ;
      TITLE aCap[ nEvent ] ;
      MODAL NOSIZE ;
      ON RELEASE Nil

      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION "" BUTTONSIZE 42,32 FLAT

      BUTTON 01  CAPTION 'Print'  PICTURE 'printer'  ;
                    TOOLTIP 'Report printing   F5'  ;
                    ACTION  wPost()  SEPARATOR
      BUTTON 02  CAPTION 'Excel'  PICTURE 'excel'   ;
                    TOOLTIP 'Export to MS Excel   F6' ;
                    ACTION  wPost()  SEPARATOR
      END TOOLBAR

      DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 99  CAPTION 'Exit'  PICTURE 'exit'  ACTION wPost()
      END TOOLBAR
      END SPLITBOX

      // ToolBar 1
      (This.Object):Event(  1, {|ow| oWnd:StatusBar:Say('... W A I T ...'), ;
           MsgBox('P r i n t i n g.  This.Name = ' + This.Name, ow:Name), ;
                                     oWnd:StatusBar:Say('') } )
      (This.Object):Event(  2, {|ow| oWnd:PostMsg(91),                      ;
           MsgBox('Export to MS Excel. This.Name = ' + This.Name, ow:Name), ;
                                     oWnd:PostMsg(92) } )
      // ToolBar 2
      (This.Object):Event( 99, {|ow| ow:Release()  } )

      nY := GetWindowHeight(hSpl)
      DEFINE TBROWSE oRpt  AT nY, nX  WIDTH  This.ClientWidth            ;
                                      HEIGHT This.ClientHeight - nY  CELL ;
                                      TOOLTIP 'Double click on title - sorting'

      :SetArrayTo( aArray, aFontHF, aHead, aSize, aFoot, aPict, aAlign, aName )

      :lNoGrayBar := .T.
      :nWheelLines    := 1
      :nClrLine       := COLOR_GRID
      :nHeightCell    += 5
      :nHeightHead    := :nHeightCell + 2
      :nHeightFoot    := :nHeightCell + 2
      :lDrawFooters := .T.
      :lFooting     := .T.
      :lNoVScroll := .F.
      :lNoHScroll := .T.

      :SetColor( { CLR_FOCUSB }, { { |a,b,c| If( c:nCell == b, {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                                                               {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } )
      :aColumns[ 1 ]:bData   := {|| oRpt:nAt }
      :aColumns[ 1 ]:lIndexCol := .F.

      If nEvent == 6 .or. nEvent == 7
         :aColumns[ 2 ]:hFont  := GetFontHandle('FontNorm')
         :aColumns[ 2 ]:nWidth += 70
      EndIf

      :UserKeys( VK_F5, {|ob| wPost(1, ob) } )
      :UserKeys( VK_F6, {|ob| wPost(2, ob) } )

      :AdjColumns({3, 4, 5, 6})   // :AdjColumns()

      END TBROWSE

      oRpt:SetNoHoles()
      oRpt:SetFocus()

      ON KEY ESCAPE ACTION wPost(99)

   END WINDOW

   CENTER   WINDOW Report
   ACTIVATE WINDOW Report

RETURN Nil

*-----------------------------------------------------------------------------*
FUNC wPost( nEvent, nIndex, xParam )
*-----------------------------------------------------------------------------*
   LOCAL oWnd

   If HB_ISOBJECT(nIndex)

      oWnd := _WindowObj(nIndex:cParentWnd)
      oWnd:SetProp(nEvent, xParam)
      oWnd:PostMsg(nEvent)

   Else

      DEFAULT nEvent := val( This.Name )

      If nEvent > 0
         oWnd := (ThisWindow.Object)
         oWnd:SetProp(nEvent, xParam)
         oWnd:PostMsg(nEvent, nIndex)
      EndIf

   EndIf

RETURN Nil

*-----------------------------------------------------------------------------*
FUNC wSend( nEvent, nIndex, xParam )
*-----------------------------------------------------------------------------*
   LOCAL oWnd

   If HB_ISOBJECT(nIndex)

      oWnd := _WindowObj(nIndex:cParentWnd)
      oWnd:SetProp(nEvent, xParam)
      oWnd:SendMsg(nEvent)

   Else

      DEFAULT nEvent := val( This.Name )

      If nEvent > 0
         oWnd := (ThisWindow.Object)
         oWnd:SetProp(nEvent, xParam)
         oWnd:SendMsg(nEvent, nIndex)
      EndIf

   EndIf

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
