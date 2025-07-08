/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch" 

REQUEST DBFCDX 

*-----------------------------------------------------------------------------*
FUNCTION Main()
*-----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, hSpl, oBrw
   LOCAL cWnd := 'wMain'

   RddSetDefault("DBFCDX") 

   SET CENTURY      ON 
   SET DATE         GERMAN 
   SET DELETED      ON 
   SET EXCLUSIVE    ON 
   SET EPOCH TO     2000 
   SET AUTOPEN      ON 
   SET EXACT        ON 
   SET SOFTSEEK     ON 

   SET NAVIGATION   EXTENDED 
   SET FONT         TO "Arial", 12
   SET DEFAULT ICON TO "hmg_ico"

   SET DIALOGBOX CENTER  OF  PARENT
   SET CENTERWINDOW RELATIVE PARENT
   *--------------------------------
   SET OOP ON
   *--------------------------------

   DEFINE FONT FontBold FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT AgeCard  FONTNAME "Verdana"            SIZE  12  BOLD    
   DEFINE FONT DlgFont  FONTNAME "Tahoma"             SIZE  12  

   SET GETBOX FOCUS BACKCOLOR TO {{255,255,255},{255,255,200},{200,255,255}}
   SET GETBOX FOCUS FONTCOLOR TO {{  0,  0,  0},{255,255,200},{0  ,0  ,255}}

	DEFINE WINDOW &cWnd AT 0,0 WIDTH 980 HEIGHT 650 ;
		TITLE 'MiniGUI Demo for TBrowse report AGE'  ;
		MAIN   NOMAXIMIZE   NOSIZE                   ; 
		ON INTERACTIVECLOSE (This.Object):Action

  		DEFINE STATUSBAR BOLD
			STATUSITEM ''           ACTION Nil
			STATUSITEM '' WIDTH  80 ACTION Nil
			STATUSITEM '' WIDTH 460 ACTION Nil
		END STATUSBAR

		DEFINE SPLITBOX HANDLE hSpl
		DEFINE TOOLBAR ToolBar_1 CAPTION "REPORT  AGE:" BUTTONSIZE 64,32 FLAT

			BUTTON E0  CAPTION ' '       PICTURE 'cabinet'             ;
                    ACTION  NIL                             SEPARATOR 
			BUTTON 01  CAPTION '1-25'    PICTURE 'n1'                  ;
                    ACTION  _wPost(1, This.Index, { 1, 25}) SEPARATOR
			BUTTON 02  CAPTION '26-40'   PICTURE 'n2'                  ;
                    ACTION  _wPost(1, This.Index, {26, 40}) SEPARATOR
			BUTTON 03  CAPTION '41-60'   PICTURE 'n3'                  ;
                    ACTION  _wPost(1, This.Index, {41, 60}) SEPARATOR
			BUTTON 04  CAPTION '61-80'   PICTURE 'n4'                  ;
                    ACTION  _wPost(1, This.Index, {61, 80}) SEPARATOR
			BUTTON 05  CAPTION '81-100'  PICTURE 'n5'                  ;
                    ACTION  _wPost(1, This.Index, {81,100}) SEPARATOR
			BUTTON 06  CAPTION 'All'     PICTURE 'n6'                  ;
                    ACTION  _wPost(1, This.Index, {  ,   }) SEPARATOR
		END TOOLBAR

		DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
			BUTTON 99  CAPTION 'Exit'   PICTURE 'exit'  ACTION _wPost(99) 
		END TOOLBAR
		END SPLITBOX

		This.E0.Cargo := 'AgeReport'        // TsBrowse name

      WITH OBJECT This.Object
      :StatusBar:Say(MiniGUIVersion(), 3)
      :Event(  1, {|ow,ky,ap| AgeReport(ow, ky, ap) } )
      :Event(  2, {|oc      | AgeCard(oc:Window, oc:Tsb, oc) } )
      :Event( 91, {|oc      | Brw_Age_Init(oc:Tsb)  } )
      :Event( 92, {|oc      | Brw_Age_End (oc:Tsb)  } )
      :Event( 93, {|oc      | Brw_Age_Body(oc:Tsb)  } )
      :Event( 99, {|ow      | ow:Release()          } )
      END WITH                

      nY   := GetWindowHeight(hSpl)
      nX   := 1
      nW   := This.ClientWidth  - nX * 2
      nH   := This.ClientHeight - This.StatusBar.Height - nY

      oBrw := Brw_Age(nY, nX, nW, nH)

      oBrw:SetFocus()            

		ON KEY ESCAPE  ACTION _wPost(99) 

	END WINDOW

	CENTER   WINDOW &cWnd
	ACTIVATE WINDOW &cWnd

RETURN Nil

STATIC FUNC AgeCard( oWnd, oBrw, oCnl )
   LOCAL nRet  
   LOCAL bInit := {|| bAgeCard(oWnd, oBrw, oCnl) }
   LOCAL aClr  := { GRAY , GRAY, GREEN,  RED     }
   LOCAL aButt := { "&Down","&Up" ,"&Save","&Cancel" }
   LOCAL cTitl := "VIEW  CARD"
   LOCAL cMsg  := "View the selected card !" + CRLF + ' '
   LOCAL cIco  := "EditIco32"
   LOCAL cFont := 'AgeCard'

   If ! empty( oBrw:GetValue('AGE') ) .or. ! empty( oBrw:GetValue('FIRST') )
      HMG_Alert( cMsg, aButt, cTitl, , cIco, , aClr, bInit, .T., cFont )
   EndIf

RETURN Nil

STATIC FUNC bAgeCard( oWnd, oBrw, oCnl )
   LOCAL nGaps := 10             // для примера:
   LOCAL nInd := oCnl:Index      // Index TsBrowse
   LOCAL cBrw := oCnl:Name       // Name  TsBrowse
   LOCAL cTxt, oCol, nCol
   LOCAL nY, nX, nW, nH, nGw, nGh
   LOCAL d, h, y, x, o, a := {}
   LOCAL nLblW := 100, nGetW := 0

  _HMG_ModalDialogReturn := 1                         // Down
   This.Cargo            := .F.                       // no modify card

   nGw := This.ClientWidth - This.Btn_04.Width  - This.Btn_04.Col - 4  // gaps width
   nGh := nGw + 2                                                      // gaps height
   nY  := This.Say_02.Row  + This.Say_02.Height + nGh
   nX  := This.Btn_01.Col
   nH  := This.Say_01.Height
   d   := Int( This.Btn_01.Height * 0.3 )

   WITH OBJECT oBrw

   FOR EACH oCol IN :aColumns
       nCol := :nColumn(oCol:cName)
       cTxt := oCol:cHeading
       If CRLF $ cTxt; cTxt := StrTran(cTxt, CRLF, ' ')
       EndIf
       o            := oKeyData()
       o:nCol       := nCol
       o:oCol       := oCol
       o:LblName    := oCol:cName + '_'
       o:LblText    := cTxt
       o:GetName    := oCol:cName
       o:GetValue   := :GetValue(oCol:cName)
       o:GetWidth   := oCol:ToWidth(iif( oCol:nFieldLen > 30, 30, ))
       o:GetPict    := :cPictureGet( , nCol )
       o:GetToolTip := Nil
       nGetW        := Max( nGetW, o:GetWidth )
       o:GetROnly   := ','+oCol:cName+',' $ ',AGE,'
       o:GetCenter  := ','+oCol:cName+',' $ ',AGE,' .or. oCol:cFieldTyp $ 'D'
       o:GetButton  := ','+oCol:cName+',' $ ',STREET,STATE,'
       o:BtnName    := o:LblName + 'Btn'
       o:BtnAction  := '_wPost(11, , "' + o:GetName + '")'
       If o:GetButton
          o:GetToolTip := 'Press F5 or DoubleClick'
       EndIf
       If ! o:GetROnly
          o:GetPict := '@K'
       EndIf
       AAdd( a, o )
   NEXT
   y := nY
   FOR EACH o IN a
            x := nX
       @ y, x LABEL &( o:LblName )      ;
              VALUE    o:LblText        ;
              WIDTH nLblW HEIGHT nH     ;
              VCENTERALIGN              
            x += nLblW + nGw
       @ y, x GETBOX   &( o:GetName )                  ;
              VALUE       o:GetValue                   ;
              WIDTH       o:GetWidth                   ;
              HEIGHT      nH                           ;
              PICTURE     o:GetPict                    ;
              ON CHANGE ( ThisWindow.Cargo    := .T.,  ;
                          ThisWindow.Closable := .F.,  ;
                          This.Btn_01.Enabled := .F.,  ;
                          This.Btn_02.Enabled := .F.,  ;
                          This.Btn_03.Enabled := .T. ) ;
              TOOLTIP     o:GetToolTip
         If o:GetROnly
            This.&(o:GetName).ReadOnly  := .T.
         EndIf
         If o:GetCenter
            This.&(o:GetName).Alignment := 'CENTER'
         EndIf
         If o:GetButton
            x += o:GetWidth + int( nGw * 0.5 )
            y -= 1
       @ y, x BUTTON   &( o:BtnName ) ;
              PICTURE     'Table24'   ;
              WIDTH       nH + 2      ;
              HEIGHT      nH + 2      ;
              NOTABSTOP
              This.&(o:BtnName).Action := hb_MacroBlock( o:BtnAction )
             (This.&(o:GetName).Object):SetKeyEvent( VK_F5, hb_MacroBlock( o:BtnAction ) )
             (This.&(o:GetName).Object):SetDoubleClick( hb_MacroBlock( o:BtnAction ) )
         EndIf
         h := y
         y += nH + nGh
   NEXT
   h := h - nY + nGh     // new delta height

   END WITH

   (This.Object):Event( 11, {|ow,ky,cp| Spr_View(ow, cp, ky) } )

   This.Height := h + This.Height - d
   This.Center

   AEval( HMG_GetFormControls( This.Name, "OBUTTON" ), ;
          {|c| This.&(c).Height := (This.&(c).Height) - d, ;
               This.&(c).Row := h + (This.&(c).Row) } )

   This.Btn_03.Enabled := .F.                              // No Save
   This.AGE.TabStop    := .F.

   This.Btn_01.Action  := {|| Age_CardSkip( oBrw, .T. ) }  // Down
   This.Btn_02.Action  := {|| Age_CardSkip( oBrw, .F. ) }  // Up
   This.Btn_03.Action  := {|| Age_CardSave( oBrw, .T. ) }  // Save
   This.Btn_04.Action  := {|| Age_CardSave( oBrw, .F. ) }  // Cancel

   ON KEY ESCAPE ACTION Age_CardSave( oBrw, .F. )

   ON KEY CONTROL+D ACTION iif( This.Btn_01.Enabled, This.Btn_01.Action, )
   ON KEY CONTROL+U ACTION iif( This.Btn_02.Enabled, This.Btn_02.Action, )
   ON KEY CONTROL+S ACTION iif( This.Btn_03.Enabled, This.Btn_03.Action, )
   ON KEY CONTROL+C ACTION This.Btn_04.Action

RETURN NIL

STATIC FUNC Spr_View( oWnd, cGet, nEvent )
   LOCAL aThis := _ThisInfo()
   LOCAL cMsg  := '', lRet := .F.
   LOCAL cWnd  := oWnd:Name
   LOCAL oGet  := This.&(cGet).Object
   LOCAL xVal  := This.&(cGet).Value
   LOCAL nVal  := 0
   LOCAL aVal  := {"10244 NATIONAL CITY CENTER", "UT" }
   LOCAL cFocu := This.FocusedControl, cNam

   If cFocu == cGet

      cNam := cGet + '_Btn'
      This.&(cNam).SetFocus ; DO EVENTS
      _PushKey( VK_SPACE )

      RETURN lRet

   EndIf

   If     cGet == 'STREET'
      cMsg := 'Selected new STREET value !;' + aVal[1]
      nVal := 1
   ElseIf cGet == 'STATE'
      cMsg := 'Selected new STATE  value !;' + aVal[2]
      nVal := 2
   EndIf

   If ! empty(cMsg)

      lRet := AlertYesNo( cMsg, 'SAVE '+cWnd+'.'+cGet)

      _ThisInfo(aThis)

      If lRet
         This.&(cGet).SetFocus
         DO EVENTS
         _PushKey(VK_SPACE)
         DO EVENTS
         This.&(cGet).Value := aVal[ nVal ]
      EndIf

      This.&(cGet).SetFocus

   EndIf

RETURN lRet

STATIC FUNC Age_CardSave( oBrw, lSave )
   LOCAL oWnd := ThisWindow.Object
   LOCAL oGet, cAls, lMsg := .F., nSel
   LOCAL aRec := oBrw:aArray[ oBrw:nAt ]
   LOCAL nRec := ATail(aRec)
   LOCAL cFocu := This.FocusedControl

   If empty( lSave ) .and. ThisWindow.Cargo
      If ! empty( cFocu ) .and. ! 'BUTT' $ This.&(cFocu).Type
         This.Btn_04.SetFocus
         DO EVENTS
      EndIf
      lMsg := AlertYesNo('Save card - '+cValToChar(This.AGE.Value  )+CRLF+ ;
                                        cValToChar(This.FIRST.Value)+CRLF+ ;
                                        cValToChar(This.LAST.Value ),      ;
                     'NR. '+hb_ntos(oBrw:nAt)+ ' RECNO ' + cValToChar(nRec))
   EndIf

   If ! empty( lSave ) .or. lMsg
      nSel := Select()
      If nRec > 0 .and. AgeTableUse()
         cAls := ALIAS()
         (cAls)->( OrdSetFocus(0) )
         (cAls)->( dbGoto(nRec) )
         IF (cAls)->( RecNo() ) == nRec .and. (cAls)->( RLock() )
            FOR EACH oGet IN oWnd:GetObj4Type('GETBOX')
                (cAls)->( FieldPut(FieldPos(oGet:Name), oGet:Value) )
                oBrw:SetValue(oGet:Name, oGet:Value)
            NEXT
            (cAls)->( dbCommit() )
            (cAls)->( dbUnLock() )
            oBrw:DrawSelect()        
         ENDIF
         (cAls)->( dbCloseArea() )
         dbSelectArea(nSel)
      EndIf
   EndIf

   ThisWindow.Cargo     := .F.
   ThisWindow.Closable  := .T.
   This.Btn_01.Enabled  := .T.
   This.Btn_02.Enabled  := .T.
   This.Btn_03.Enabled  := .F.

   If empty( lSave ) .or. lMsg
      oWnd:Release()
      RETURN Nil
   EndIf

   This.Btn_01.SetFocus()    

RETURN Nil

STATIC FUNC Age_CardSkip( oBrw, lDown )
   LOCAL oWnd := ThisWindow.Object
   LOCAL oGet

   If empty(lDown); oBrw:GoUp()
   Else           ; oBrw:GoDown()
   EndIf

   FOR EACH oGet IN oWnd:GetObj4Type('GETBOX')
       oGet:Value := oBrw:GetValue(oGet:Name)
   NEXT

   This.Btn_03.Enabled := .F.
   ThisWindow.Cargo    := .F.
   ThisWindow.Closable := .T.

   If     oBrw:lHitTop
      TONE(3600)
      This.Btn_02.Enabled := .F.
      This.Btn_01.Enabled := .T.
      This.Btn_01.SetFocus()    
   ElseIf oBrw:lHitBottom
      TONE(3600)
      This.Btn_01.Enabled := .F.
      This.Btn_02.Enabled := .T.
      This.Btn_02.SetFocus()    
   Else
      This.Btn_01.Enabled := .T.
      This.Btn_02.Enabled := .T.
   ENDIF

    DO EVENTS

RETURN Nil

*-----------------------------------------------------------------------------*
STAT FUNC Brw_Age( nY, nX, nW, nH )
*-----------------------------------------------------------------------------*
   LOCAL oBrw
   LOCAL cBrw   := This.E0.Cargo
   LOCAL aFont  := { GetFontHandle('FontBold'), GetFontHandle('FontBold') }
   LOCAL aDatos := AgeSelect(.T.)                                    // Init value
   LOCAL aClr   := {}

   AAdd( aClr, { CLR_FOCUSB, {|a,b,c| iif( c:nCell == b,      ;     // 6
                             {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                             {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } ) 

   AAdd( aClr, { CLR_SELEF ,  CLR_BLACK } )             // 11, focused inactive fore
   AAdd( aClr, { CLR_SELEB ,  CLR_HGRAY } )             // 12, focused inactive back

   oBrw := Brw2Arr(cBrw, nY, nX, nW, nH, aDatos, aClr, aFont)

RETURN oBrw

*-----------------------------------------------------------------------------*
STATIC FUNC Brw_Age_Init( oBrw )
*-----------------------------------------------------------------------------*
   DEFAULT oBrw := (This.Object):Tsb

   WITH OBJECT oBrw

   :lNoGrayBar   := .F.
   :nWheelLines  :=  1         
   :nStatusItem  :=  0         
   :lNoVScroll   := .F.       
   :lNoHScroll   := .T.       
   :lNoKeyChar   := .T.       
   :lDrawFooters := .T.       
   :lFooting     := .T.       
   :nFireKey     :=  0    
   :nClrLine     := COLOR_GRID

   :lNoResetPos  := .T.
   :lNoMoveCols  := .T.
   :lNoKeyChar   := .T.
   :lNoChangeOrd := .T.

   END WITH

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC Brw_Age_End( oBrw )
*-----------------------------------------------------------------------------*
   DEFAULT oBrw := (This.Object):Tsb

   WITH OBJECT oBrw

   :GetColumn('AGE'):nFAlign := DT_CENTER

   :SetNoHoles()

   END WITH

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC Brw_Age_Body( oBrw )
*-----------------------------------------------------------------------------*

   WITH OBJECT oBrw

    ADD SUPER HEADER TO oBrw FROM 1 TO :nColCount() TITLE This.ToolBar_1.Caption

   :nHeightCell  += 5
   :nHeightHead  := :nHeightCell + 2
   :nHeightFoot  := :nHeightCell + 2
   :nHeightSuper := :nHeightHead

   :bLDblClick   := {|up1,up2,nfl,ob| up1:=up2:=nfl:=Nil, ;
                                  ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

   :UserKeys( VK_RETURN, {|ob| _wPost(2, ob, ob), .F. } )

   END WITH

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION Brw2Arr( cBrw, nY, nX, nW, nH, aDatos, aColor, aFont, lAdj )
*-----------------------------------------------------------------------------*
   LOCAL aArray, aHead, aSize, aFoot, aPict, aAlign, aName, oBrw

   DEFAULT lAdj := .T.

   aArray   := aDatos[ 1 ]
   aHead    := aDatos[ 2 ]
   aSize    := aDatos[ 3 ]
   aFoot    := aDatos[ 4 ]
   aPict    := aDatos[ 5 ]
   aAlign   := aDatos[ 6 ]
   aName    := aDatos[ 7 ]

   DEFINE TBROWSE &cBrw OBJ oBrw AT nY, nX WIDTH nW HEIGHT nH CELL ;
          COLORS aColor ON INIT _wPost( 91, This.Index )
          DO EVENTS   

      :SetArrayTo(aArray, aFont, aHead, aSize, aFoot, aPict, aAlign, aName)
      
       AEval(:aColumns, {|oc,nc| oc:lEmptyValToChar := .T., ; 
                                 oc:lFixLite        := .T. })
                                 
       _wSend( 93, GetControlIndex( :cControlName, :cParentWnd ) )

         If lAdj
      :AdjColumns()
         EndIf

      :ResetVScroll( .T. )
      :oHScroll:SetRange(0,0)
                     
   END TBROWSE      ON END _wPost( 92, This.Index )
       DO EVENTS
   
RETURN oBrw                         

*-----------------------------------------------------------------------------*
STATIC FUNC AgeReport( oWnd, nEvent, aSelect )
*-----------------------------------------------------------------------------*
   LOCAL aDatos, aArray, aSize, aTyp, aLen
   LOCAL cCapt := 'All'
   LOCAL cBrw  := This.E0.Cargo               // TsBrowse name
   LOCAL nRec, nPos, nCol, cBtnC
   
   nEvent      := Val( This.Name )            // Button name
   oWnd:Action := .F.
   oWnd:StatusBar:Say('W A I T')

   If aSelect[1] != Nil
      cCapt := hb_ntos(aSelect[1])+'-'+hb_ntos(aSelect[2])
   EndIf

   cBtnC := This.E0.Caption
   This.E0.Caption := cCapt ; DO EVENTS

   aDatos := AgeSelect( aSelect[1], aSelect[2] )

   aArray := aDatos[ 1 ]
   aSize  := aDatos[ 3 ]
   aTyp   := aDatos[ 8 ]
   aLen   := aDatos[ 9 ]

   WITH OBJECT (This.&(cBrw).Object):Tsb      // oWnd:GetObj(cBrw):Tsb 

   :Hide()
    nRec := :nAt
    nPos := :nRowPos
    nCol := :nCell
    AEval(:aColumns, {|oc,nc| oc:nWidth    := aSize[ nc ] })
    AEval(:aColumns, {|oc,nc| oc:cFieldTyp := aTyp[nc],    ;
                              oc:nFieldLen := aLen[nc]+2  })
   :HideColumns( 'STREET', ! 'All' $ cCapt )
   :cTextSupHdSet( 1, This.ToolBar_1.Caption + '  ' + cCapt )
   :Display()
   :AdjColumns()
    DO EVENTS
   :SetArray(aArray, .T.)
   :Reset()
   :GetColumn('AGE'):cFooting := hb_ntos(:nLen)
   :ResetVScroll( .T. )
   :oHScroll:SetRange(0,0)
    DO EVENTS
    If cCapt == cBtnC          // нажали ту же кнопку, удерживаем курсор как был
       If nPos <= :nRowCount() .and. :nLen <= :nRowCount()
         :GoPos( nPos, nCol )
       Else                    // :nLen > :nRowCount()
          :Skip(nRec-nPos)
          :nCell := nCol
          If nPos > 1
             WHILE nPos-- > 1
                :GoDown()
             ENDDO
          EndIf
       EndIf
    EndIf
   :Show()
    DO EVENTS
   :SetFocus()

   END WITH

   oWnd:StatusBar:Say('')
   oWnd:Action := .T.

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC AgeSelect( nAge1, nAge2 )
*-----------------------------------------------------------------------------*
   LOCAL aField := {  "AGE", "FIRST", "LAST", "STATE", "CITY", "STREET"    }
   LOCAL aHead  := {  "Age", "First", "Last", "State", "City", "Street"    }
   LOCAL aFoot  := {     "",      "",     "",      "",     "",       ""    }
   LOCAL aDefV  := {      0,      "",     "",      "",     "",       "", 0 }
   LOCAL aSize  := {     50,     110,    110,      60,    150,      220    }
   LOCAL aPict  := { '9999',        ,       ,        ,       ,             }
   LOCAL aAlign := {      1,       0,      0,       1,      0,        0    }
   LOCAL aName  := aField
   LOCAL aAge   := {}, aVal
   LOCAL aLen   := {}
   LOCAL aTyp   := {}

   If AgeTableUse()

      SET  ORDER TO TAG AGE
      SET  SCOPE TO nAge1, nAge2

      IF HB_ISLOGICAL(nAge1)
         GO BOTTOM
      ELSE
         GO TOP 
         DO WHILE ! EOF()
            DO EVENTS
            aVal := {}
            AEval(aField, {|cp| AAdd( aVal, FieldGet(FieldPos(cp)) ) })
            AAdd( aVal, RecNo() )
            AAdd( aAge, aVal )
            DO EVENTS
            SKIP
         ENDDO
      ENDIF

      AEval(aField, {|cp| AAdd( aLen, FieldLen (FieldPos(cp)) ), ;
                          AAdd( aTyp, FieldType(FieldPos(cp)) )})
      USE

   EndIf

   If Len(aAge) == 0
      AAdd(aAge, aDefV )
   EndIf

RETURN { aAge, aHead, aSize, aFoot, aPict, aAlign, aName, aTyp, aLen }

*-----------------------------------------------------------------------------*
STATIC FUNC AgeTableUse()
*-----------------------------------------------------------------------------*
   LOCAL cFile  := GetStartUpFolder() + "\" + "Employee"
   LOCAL cAlias := "BASE_AGE"
   LOCAL lUsed

   If ! file(cFile+IndexExt())
      USE &cFile  ALIAS &cAlias NEW
      INDEX ON _FIELD->AGE TAG AGE
      USE
   EndIf

   USE &cFile ALIAS &cAlias  SHARED  NEW

   If ! ( lUsed := Used() )
      MsgStop('Table not used !'+CRLF+cFile+'.dbf', 'ERROR')
   EndIf

RETURN lUsed

*-----------------------------------------------------------------------------*
FUNCTION _ThisInfo( aThis )
*-----------------------------------------------------------------------------*

   IF HB_ISARRAY( aThis )

      _HMG_ThisFormIndex   := aThis [1]
      _HMG_ThisEventType   := aThis [2]
      _HMG_ThisType        := aThis [3]
      _HMG_ThisIndex       := aThis [4]
      _HMG_ThisFormName    := aThis [5]
      _HMG_ThisControlName := aThis [6]

      RETURN NIL

   ENDIF

RETURN { _HMG_ThisFormIndex, _HMG_ThisEventType, _HMG_ThisType, _HMG_ThisIndex, _HMG_ThisFormName, _HMG_ThisControlName }
