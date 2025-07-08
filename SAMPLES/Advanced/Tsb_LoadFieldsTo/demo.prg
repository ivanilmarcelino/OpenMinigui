/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch" 
#include "rddleto.ch"

REQUEST DBFCDX, HB_MEMIO, LETO

FIELD FIRST, LAST, AGE, STATE, CITY, STREET, NAME

STATIC a_Alias

#define TBL_STATE     "State"
#define TBL_EMPLOYEE  "Employee"

#define ALS_STATE     a_Alias[ 1 ]  
#define ALS_EMPLOYEE  a_Alias[ 2 ]  

*-----------------------------------------------------------------------------*
FUNCTION Main( cPath )
*-----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, hSpl, oBrw, f, i, k
   LOCAL cWnd := 'wMain', cAlias, aStru, cAlsC
   LOCAL cOut := 'OUT'
   LOCAL cTmp := 'mem:out'
   LOCAL cDir := 'Demo'      // LetoDbf Directory

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

   If ! empty(cPath)
      If '*' $ cPath
         cPath := "127.0.0.1:2812"
      EndIf
      If ( i := AT(".", cPath) ) > 0 .and. ( k := AT(":", cPath) ) > 0 .and. k > i
         cPath := '//' + cPath + '/'
      EndIf
      If left(cPath, 2) == '//'
         if ! Leto_DirExist( cPath+cDir )
            Leto_DirMake( cPath+cDir )
         EndIf
         cDir += '/'
         RddSetDefault("LETO") 
         If LETO_CONNECT( cPath ) < 0
            MsgStop( "Can't connect to server LETO"+CRLF+cPath, 'ERROR')
            QUIT
         EndIf
         f := TableUse(1)+'.dbf'
         If ! leto_file(cDir + f)
            leto_fCopyToSrv( f, cDir + f )
         EndIf
         f := TableUse(2)+'.dbf'
         If ! leto_file(cDir + f)
            leto_fCopyToSrv( f, cDir + f )
         EndIf
         cPath := cDir
      EndIf
   EndIf

   DEFINE FONT FontBold FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT AgeCard  FONTNAME "Verdana"            SIZE  12  BOLD
   DEFINE FONT DlgFont  FONTNAME "Tahoma"             SIZE  12

   SET GETBOX FOCUS BACKCOLOR TO {{255,255,255},{255,255,200},{200,255,255}}
   SET GETBOX FOCUS FONTCOLOR TO {{  0,  0,  0},{255,255,200},{0  ,0  ,255}}

   cPath := iif( Empty(cPath), '', cPath )
   aStru := {                         ;
             {"REC"    , "N",  7, 0}, ;
             {"FIRST"  , "C", 15, 0}, ;
             {"LAST"   , "C", 15, 0}, ;
             {"STATE"  , "C",  2, 0}, ;
             {"ZIP"    , "C", 10, 0}, ;
             {"AGE"    , "N",  2, 0}, ;
             {"MARRIED", "L",  1, 0}  ;
            }

   If 'mem:' $ cTmp; dbDrop(cTmp, cTmp, 'DBFCDX')
   EndIf

   dbCreate(cTmp, aStru, 'DBFCDX', .T., cOut) ; ZAP

   If Empty( a_Alias := TableUse( cPath ) )
      TableClose(cTmp)
      QUIT
   EndIf

   cAlsC := ALS_STATE

   SELECT &cAlsC
   SET ORDER TO TAG KDS
   GO TOP

   cAlias := ALS_EMPLOYEE

   SELECT &cAlias
   SET ORDER TO TAG IDN
   GO TOP

   SELECT &cOut

   nW := System.DesktopWidth  * 0.98
   nH := System.DesktopHeight * 0.82

	DEFINE WINDOW &cWnd AT 0,0 WIDTH nW HEIGHT nH      ;
		TITLE 'MiniGUI Demo for TBrowse report to temp' ;
		MAIN   NOMAXIMIZE    NOSIZE   TOPMOST           ; 
		ON INIT              This.Topmost := .F.        ;
		ON INTERACTIVECLOSE (This.Object):Action        ;
      ON RELEASE           TableClose(cTmp)

  		DEFINE STATUSBAR BOLD
			STATUSITEM ''  
			STATUSITEM '' WIDTH This.ClientWidth * 0.25 
			STATUSITEM '' WIDTH This.ClientWidth * 0.4 
		END STATUSBAR

		DEFINE SPLITBOX HANDLE hSpl
		DEFINE TOOLBAR ToolBar_1 CAPTION "REPORT : " BUTTONSIZE 170,32   FLAT
			BUTTON E0  CAPTION ' '       PICTURE 'cabinet'    SEPARATOR 
		END TOOLBAR

		DEFINE TOOLBAR ToolBar_2 CAPTION ""       BUTTONSIZE  84,32   FLAT
			BUTTON BAge  CAPTION 'Age'   PICTURE 'n1' SEPARATOR WHOLEDROPDOWN
         DEFINE DROPDOWN MENU BUTTON BAge
            ITEM "Age  1-25 (.T.)"   IMAGE 'n1' ;
                 ACTION _wPost(1, , { 1, 25,This.Caption,1}) // MARRIED .T.
            ITEM "Age 26-40 (.F.)"  IMAGE 'n2' ;
                 ACTION _wPost(1, , {26, 40,This.Caption,0}) // MARRIED .F.
            ITEM "Age 41-60"        IMAGE 'n3' ;
                 ACTION _wPost(1, , {41, 60,This.Caption, })
            ITEM "Age 61-80 (.T.)"  IMAGE 'n4' ;
                 ACTION _wPost(1, , {61, 80,This.Caption,1}) // MARRIED .T.
            ITEM "Age 81-100 (.F.)" IMAGE 'n5' ;
                 ACTION _wPost(1, , {81,100,This.Caption,0}) // MARRIED .F.
            ITEM "Age  0-0"         IMAGE 'n6' ;
                 ACTION _wPost(1, , { 0,  0,This.Caption, })
            ITEM "Age All"          IMAGE 'n7' ;
                 ACTION _wPost(1, , {  ,   ,This.Caption, })
         END MENU
			BUTTON BFio  CAPTION 'First' PICTURE 'n2' SEPARATOR WHOLEDROPDOWN
         DEFINE DROPDOWN MENU BUTTON BFio
            ITEM "First PAUL"           IMAGE 'n1' ;
                 ACTION _wPost(2, , {"PAUL" ,"PAUL" ,This.Caption, })
            ITEM "First PETE"           IMAGE 'n2' ;
                 ACTION _wPost(2, , {"PETE" ,"PETE" ,This.Caption, })
            ITEM "First ABE-AL (.T.)"   IMAGE 'n3' ;
                 ACTION _wPost(2, , {"ABE"  ,"AL"   ,This.Caption,1}) // MARRIED .T.
            ITEM "First BRUNO"          IMAGE 'n4' ;
                 ACTION _wPost(2, , {"BRUNO","BRUNO",This.Caption, })
            ITEM "First RYAN"           IMAGE 'n5' ;
                 ACTION _wPost(2, , {"RYAN" ,"RYIAN",This.Caption, })
            ITEM "First TROY-YAO (.F.)" IMAGE 'n6' ;
                 ACTION _wPost(2, , {"TROY" ,"YAO"  ,This.Caption,0}) // MARRIED .F.
            ITEM "First All"            IMAGE 'n7' ;
                 ACTION _wPost(2, , {       ,       ,This.Caption, })
         END MENU
			BUTTON BAdr  CAPTION 'State' PICTURE 'n3'           WHOLEDROPDOWN
         DEFINE DROPDOWN MENU BUTTON BAdr
            ITEM "State AR"           IMAGE 'n1' ;
                 ACTION _wPost(3, , {"AR","AR",This.Caption, })
            ITEM "State WY"           IMAGE 'n2' ;
                 ACTION _wPost(3, , {"WY","WY",This.Caption, })
            ITEM "State SD"           IMAGE 'n3' ;
                 ACTION _wPost(3, , {"SD","SD",This.Caption, })
            ITEM "State LA"           IMAGE 'n4' ;
                 ACTION _wPost(3, , {"LA","LA",This.Caption, })
            ITEM "State AK-AR (.T.)"  IMAGE 'n5' ;
                 ACTION _wPost(3, , {"AK","AR",This.Caption,1}) // MARRIED .T.
            ITEM "State WA-WV (.F.)"  IMAGE 'n6' ;
                 ACTION _wPost(3, , {"WA","WV",This.Caption,0}) // MARRIED .F.
         END MENU
		END TOOLBAR
		
		DEFINE TOOLBAR ToolBar_3 CAPTION space(150) BUTTONSIZE 42,32   FLAT
			BUTTON E3  CAPTION ' '      PICTURE 'br_emp'       SEPARATOR 
			BUTTON 99  CAPTION 'Exit'   PICTURE 'exit'  ACTION _wPost(99) 
		END TOOLBAR
		END SPLITBOX

		This.E0.Cargo := 'TblReport'        // TsBrowse name

      WITH OBJECT This.Object
      :StatusBar:Say(MiniGUIVersion(), 3)
      :StatusBar:Icon('hmg_ico'      , 3)
      :StatusBar:Say(RddSetDefault()+': '+cPath+TableUse(2)+', '+TableUse(1), 2) 
      :Event(  1, {|ow,ky,ap| TblReport(ow, ky, ap, cAlias, cOut) } )
      :Event(  2, {|ow,ky,ap| TblReport(ow, ky, ap, cAlias, cOut) } )
      :Event(  3, {|ow,ky,ap| TblReport(ow, ky, ap, cAlias, cOut) } )
      :Event( 99, {|ow      | ow:Release()          } )
      END WITH

      nY   := GetWindowHeight(hSpl)
      nX   := 1
      nW   := This.ClientWidth  - nX * 2
      nH   := This.ClientHeight - This.StatusBar.Height - nY

      oBrw := Brw2Fld( nY, nX, nW, nH, cAlias, cOut )

      oBrw:SetNoHoles()
      oBrw:SetFocus()

		ON KEY ESCAPE  ACTION iif( oBrw:IsEdit, oBrw:PostMsg( WM_KEYDOWN, VK_ESCAPE ), _wPost(99) ) 

	END WINDOW

	CENTER   WINDOW &cWnd
	ACTIVATE WINDOW &cWnd

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION Brw2Fld( nY, nX, nW, nH, cAls, cOut )
*-----------------------------------------------------------------------------*
   LOCAL oBrw, nI
   LOCAL cBrw    := This.E0.Cargo
   LOCAL aColSel := {"REC", "STATE" }
   LOCAL aNamSel := {"IDN", "STATE2"}
   LOCAL aHdrSel := {"Id" , "State2"}
   LOCAL aClr    := {}
   LOCAL cAlsS   := ALS_STATE

   AAdd( aClr, { CLR_FOCUSB, {|a,b,c| iif( c:nCell == b,      ;     // 6
                             {RGB( 66, 255, 236), RGB(209, 227, 248)}, ;
                             {RGB(220, 220, 220), RGB(220, 220, 220)} ) } } ) 

   AAdd( aClr, { CLR_SELEF ,  CLR_BLACK } )             // 11, focused inactive fore
   AAdd( aClr, { CLR_SELEB ,  CLR_HGRAY } )             // 12, focused inactive back

   DEFINE TBROWSE &cBrw OBJ oBrw AT nY, nX WIDTH nW HEIGHT nH ALIAS cOut ;
             CELL COLORS aClr 

      :hFontHead    := GetFontHandle('FontBold')
      :hFontFoot    := GetFontHandle('FontBold')

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
      :lRecLockArea := .T.

       ADD COLUMN TO TBROWSE oBrw DATA {|| OrdKeyNo() } ;  
           HEADER "NN"            SIZE 60               ;
           FOOTER hb_ntos( (cOut)->( OrdKeyCount() ) )  ;
           ALIGN DT_CENTER                              ;
           NAME NN                             

      :LoadFields(.F., {"REC"  }, cOut, {"IDN"}, {"Id"})
      :LoadFields(.F., {"STATE"}, cAls )
      :LoadFields(.F., {"NAME" }, cAlsS,       , {"State name"})
      :LoadFields(.T., {"CITY", "STREET", "ZIP", "FIRST", "LAST", "AGE", "MARRIED" }, cAls)

       AEval(:aColumns, {|oc,nc| oc:lEmptyValToChar := .T., ; 
                                 oc:lFixLite        := .T. })

      :GetColumn("MARRIED" ):bEncode   := {|lx| ! lx }
      :GetColumn("MARRIED" ):nEditMove := DT_DONT_MOVE

      :GetColumn("NN"    ):cAlias  := cOut
      :GetColumn("NN"    ):nAlign  := DT_CENTER
      :GetColumn("NN"    ):nFAlign := DT_CENTER
      :GetColumn("IDN"   ):nAlign  := DT_CENTER
      :GetColumn("AGE"   ):nAlign  := DT_CENTER
      :GetColumn("ZIP"   ):nAlign  := DT_CENTER
      :GetColumn("STATE" ):nAlign  := DT_CENTER

      :GetColumn("NAME"  ):nWidth  -= 20
      :GetColumn("FIRST" ):nWidth  -= 30
      :GetColumn("LAST"  ):nWidth  -= 30
      :GetColumn("CITY"  ):nWidth  -= 50
      :GetColumn("STREET"):nWidth  -= 40
      :GetColumn("ZIP"   ):nWidth  += 10
      :GetColumn("AGE"   ):nWidth  += 10

       ADD SUPER HEADER TO oBrw FROM 1 TO :nColCount() TITLE This.ToolBar_1.Caption

      :nHeightCell  += 5
      :nHeightHead  := :nHeightCell + 2
      :nHeightFoot  := :nHeightCell + 2
      :nHeightSuper := :nHeightHead

      :nFreeze      := :nColumn('IDN')
      :lLockFreeze  := .T.

      :AdjColumns()

      :ResetVScroll( .T. )
      :oHScroll:SetRange(0,0)

   END TBROWSE

RETURN oBrw                         

*-----------------------------------------------------------------------------*
STATIC FUNC TblReport( oWnd, nEvent, aParam, cAls, cOut )
*-----------------------------------------------------------------------------*
   LOCAL cBrw  := This.E0.Cargo               // TsBrowse name
   LOCAL oCnl  := This.&(cBrw).Object         // Control  object tsb
   LOCAL oBrw  := oCnl:Tsb                    // TsBrowse object
   LOCAL aTag  := { "AGE", "FIO", "ADR" }
   LOCAL cTag  := aTag[ nEvent ]
   LOCAL Scop1 := aParam[ 1 ]
   LOCAL Scop2 := aParam[ 2 ]
   LOCAL cCapt := aParam[ 3 ]
   LOCAL xMarr := aParam[ 4 ]
   LOCAL cBtnC := This.E0.Caption
   LOCAL cAlsS := ALS_STATE
   LOCAL cFltr

   oWnd:Action := .F.
   oWnd:StatusBar:Say('W A I T')

   This.E0.Caption := cCapt ; DO EVENTS

   SELECT &cOut
   SET RELATION TO
   GO TOP
   ZAP
   GO TOP

   SELECT &cAls
   SET ORDER TO TAG &cTag
   SET SCOPE TO Scop1, Scop2
   SET RELATION TO STATE INTO &cAlsS
   IF xMarr != NIL
      cFltr := 'MARRIED'
      IF Empty(xMarr)
         cFltr := '!'+cFltr
      ENDIF
      SET FILTER TO &(cFltr)
   ENDIF
   GO TOP
   DO WHILE ! EOF()
      DO EVENTS
      (cOut)->( dbAppend() ) ; (cOut)->REC := RecNo()
      SKIP
   ENDDO
   SET SCOPE  TO 
   SET FILTER TO
   SET ORDER  TO TAG IDN
   GO TOP
   DO EVENTS

   SELECT &cOut
   GO TOP
   SET RELATION TO FIELD->REC INTO &cAls 
   GO TOP
   DO EVENTS

   oBrw:cTextSupHdSet( 1, This.ToolBar_1.Caption + '  ' + cCapt )
   oBrw:GetColumn("NN"):cFooting := hb_ntos( OrdKeyCount() ) 
   oBrw:Reset()
   DO EVENTS
   oBrw:SetFocus()

   oWnd:StatusBar:Say('')
   oWnd:Action := .T.

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC TableClose( cTmp )
*-----------------------------------------------------------------------------*

   dbCloseAll()

   If 'mem:' $ cTmp ; dbDrop(cTmp, cTmp, 'DBFCDX')
   EndIf

   If RddSetDefault() == 'LETO' ; LETO_DISCONNECT()
   EndIf

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNC TableUse( cPath )
*-----------------------------------------------------------------------------*
   LOCAL cAlsS, cAlsE, aRet
   LOCAL aFil := { TBL_STATE, TBL_EMPLOYEE }

   If HB_ISNUMERIC( cPath )
      RETURN aFil[ cPath ]
   Else
      If UseState   ( cPath, aFil[1] ) ; cAlsS := ALIAS()
      EndIf
      If UseEmployee( cPath, aFil[2] ) ; cAlsE := ALIAS()
      Endif
      If Empty(cAlsS) .or. Empty(cAlsE)
         dbCloseAll()
      Else
         aRet := { cAlsS, cAlsE }
      EndIf
   EndIf

RETURN aRet

*-----------------------------------------------------------------------------*
STATIC FUNC UseEmployee( cPath, cName )
*-----------------------------------------------------------------------------*
   LOCAL cFile, cAlias, lTind
   LOCAL nWhl   := 10, nCnt
   LOCAL lRet   := .T.
   LOCAL lUsed  := .F.
   LOCAL cRdd   := RddSetDefault()

   STATIC c_Path

   DEFAULT cName := TBL_EMPLOYEE

   If c_Path == NIL ; c_Path := GetStartUpFolder() + "\"
   EndIf

   If HB_ISCHAR(cPath) 
      If left(cPath, 2) == '//'
         cPath := ''
      EndIf
      c_Path := cPath
   EndIf

   cFile  := c_Path + cName
   cAlias := upper(cName)

   SELECT 0

   If select(cAlias) > 0
      cAlias += '_'+hb_ntos(select())
   EndIf

   If cRdd == 'LETO' ; lTind := leto_file(cFile+IndexExt())
   Else              ; lTind := file(cFile+IndexExt())
   EndIf
   
   If ! lTind
      nCnt := nWhl
      DO WHILE nCnt-- > 0
         lRet := .F.
         BEGIN SEQUENCE WITH { |e|break(e) }
            USE &cFile ALIAS &cAlias 
            lRet := .T.
         END SEQUENCE
         IF lRet; EXIT
         ENDIF
         hb_IdleSleep(0.1)
      ENDDO
      If lRet
         INDEX ON AGE               TAG AGE  FOR !Deleted()
         INDEX ON FIRST+LAST        TAG FIO  FOR !Deleted()
         INDEX ON STATE+CITY+STREET TAG ADR  FOR !Deleted()
         INDEX ON RecNo()           TAG IDN  FOR !Deleted()
         INDEX ON RecNo()           TAG DEL  FOR  Deleted()
         USE
      EndIf
   EndIf

   If lRet
      nCnt := nWhl
      DO WHILE nCnt-- > 0
         lRet := .F.
         BEGIN SEQUENCE WITH { |e|break(e) }
            USE &cFile ALIAS &cAlias SHARED 
            lRet := .T.
         END SEQUENCE
         IF lRet; EXIT
         ENDIF
         hb_IdleSleep(0.1)
      ENDDO
   EndIf

   nCnt := 0

   If ( lUsed := Used() ); nCnt := OrdCount()
   EndIf

   If ! lUsed 
      MsgStop('Table not used !'+CRLF+cFile+'.dbf'+CRLF+ ;
              'Alias() = '+cAlias, 'ERROR')
   ElseIf nCnt == 0
      USE
      lUsed := .F.
      MsgStop('Table not indexed !'+CRLF+cFile+indexExt()+CRLF+ ;
              'Alias() = '+cAlias, 'ERROR')
   EndIf
   
RETURN lUsed

*-----------------------------------------------------------------------------*
STATIC FUNC UseState( cPath, cName )
*-----------------------------------------------------------------------------*
   LOCAL cFile, cAlias, lTind
   LOCAL nWhl   := 10, nCnt
   LOCAL lRet   := .T.
   LOCAL lUsed  := .F.
   LOCAL cRdd   := RddSetDefault()

   STATIC c_Path

   DEFAULT cName := TBL_STATE

   If c_Path == NIL ; c_Path := GetStartUpFolder() + "\"
   EndIf

   If HB_ISCHAR(cPath) 
      If left(cPath, 2) == '//'
         cPath := ''
      EndIf
      c_Path := cPath
   EndIf

   cFile  := c_Path + cName
   cAlias := upper(cName)

   SELECT 0

   If select(cAlias) > 0
      cAlias += '_'+hb_ntos(select())
   EndIf

   If cRdd == 'LETO' ; lTind := leto_file(cFile+IndexExt())
   Else              ; lTind := file(cFile+IndexExt())
   EndIf
   
   If ! lTind
      nCnt := nWhl
      DO WHILE nCnt-- > 0
         lRet := .F.
         BEGIN SEQUENCE WITH { |e|break(e) }
            USE &cFile ALIAS &cAlias 
            lRet := .T.
         END SEQUENCE
         IF lRet; EXIT
         ENDIF
         hb_IdleSleep(0.1)
      ENDDO
      If lRet
         INDEX ON STATE     TAG KDS  FOR !Deleted()
         INDEX ON NAME      TAG NAM  FOR !Deleted()
         INDEX ON RecNo()   TAG DEL  FOR  Deleted()
         USE
      EndIf
   EndIf

   If lRet
      nCnt := nWhl
      DO WHILE nCnt-- > 0
         lRet := .F.
         BEGIN SEQUENCE WITH { |e|break(e) }
            USE &cFile ALIAS &cAlias SHARED 
            lRet := .T.
         END SEQUENCE
         IF lRet; EXIT
         ENDIF
         hb_IdleSleep(0.1)
      ENDDO
   EndIf

   nCnt := 0

   If ( lUsed := Used() ); nCnt := OrdCount()
   EndIf

   If ! lUsed 
      MsgStop('Table not used !'+CRLF+cFile+'.dbf'+CRLF+ ;
              'Alias() = '+cAlias, 'ERROR')
   ElseIf nCnt == 0
      USE
      lUsed := .F.
      MsgStop('Table not indexed !'+CRLF+cFile+indexExt()+CRLF+ ;
              'Alias() = '+cAlias, 'ERROR')
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
