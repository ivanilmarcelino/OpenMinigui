/* 
 * MINIGUI - Harbour Win32 GUI library Demo 
 * 
 */ 
  
#include "demo.ch" 

REQUEST DBFCDX

SET PROCEDURE TO demo_misc

*-----------------------------------------------------------------------------*
FUNCTION Main()
*-----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, hSpl

   SetsEnv()
   CreateIndex()

   /*                    // default PUBLIC  variable for base columns
   sColsPrivate()        // create  PRIVATE variable for base columns
   */

   InitBaseCols()        // Сolumn objects create

   nW := 800
   nH := 120
   nY := 0
   nX := Int( ( System.ClientWidth - nW ) / 2 )

   DEFINE WINDOW wMain AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE "MiniGUI TsBrowse Demo"       ;
		MAIN   NOMAXIMIZE   NOSIZE ; 
		ON RELEASE  dbCloseAll()   ;
		ON INTERACTIVECLOSE (This.Object):Action
		
  		DEFINE STATUSBAR BOLD
			STATUSITEM ''                      ACTION Nil
			STATUSITEM '' WIDTH Int(nW * 0.2)  ACTION Nil
			STATUSITEM '' WIDTH Int(nW / 3  )  ACTION Nil
		END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl
		DEFINE TOOLBAR ToolBar_1 CAPTION ""              BUTTONSIZE 72,32 FLAT
			BUTTON 01  CAPTION 'Base 1'  PICTURE 'n1'     ACTION wPost()   SEPARATOR                 
			BUTTON 02  CAPTION 'Base 2'  PICTURE 'n2'     ACTION wPost()   SEPARATOR 
		END TOOLBAR
		
		DEFINE TOOLBAR ToolBar_2 CAPTION ""              BUTTONSIZE 42,32 FLAT
			BUTTON 99  CAPTION 'Exit'    PICTURE 'exit'   ACTION wPost() 
		END TOOLBAR
		END SPLITBOX

		This.Height := This.StatusBar.Height + GetWindowHeight(hSpl) + ;
                     GetTitleBarHeight()   + GetBorderHeight()

      WITH OBJECT This.Object                           // ---- Window events
      // ToolBar 1
      :Event( 01, {|ow| Base_Customer(ow) } )
      :Event( 02, {|ow| Base_Country (ow) } )
      // ToolBar 2
      :Event( 99, {|ow| ow:Release()      } )
      END WITH                                          // ---- Window events

   END WINDOW

   wMain.Activate

RETURN Nil
   
*----------------------------------------------------------------------------*
FUNC Base_Customer( oParent )
*----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, oCol
   LOCAL cAlsBase, cAlsCust, hFontBold
   LOCAL cWnd := 'wBase1', aCols := {}
   LOCAL cBrw := 'oBase1', oBrw, aColors

   oParent:Action := .F.     // Lock work message parent window

   // Init TBROWSE columns
   AAdd( aCols, gCols( OrdKeyNo     ) )
   AAdd( aCols, gCols( Cust.CUSTNO  ) )
   AAdd( aCols, gCols( Cust.COMPANY ) )
   AAdd( aCols, gCols( Cust.COUNTRY ) )
   AAdd( aCols, gCols( Cust.CITY    ) )

   hFontBold := GetFontHandle('FontBold')

   MyUse('Customer', 'CUST');   cAlsCust := Alias()
   MyUse('Base'    , 'BASE');   cAlsBase := Alias()

   nH := 700
   nW := GetVScrollBarWidth() + GetBorderWidth() * 2
   
   FOR EACH oCol IN aCols
       WITH OBJECT oCol
       // real alias
       If     :cAlias $ cAlsCust ; :cAlias := cAlsCust
       ElseIf :cAlias $ cAlsBase ; :cAlias := cAlsBase
       ElseIf '____'  $ :cAlias  ; :cAlias := cAlsBase
       EndIf
       // other
       If :cField == 'CUSTNO'
          :cAlias  := cAlsBase
          :bSeek   := {|ob,nc| ( cAlsCust )->( dbSeek((ob:cAlias)->CUSTNO, .F.) ) }
       EndIf
       // edit mode
       If ( :lEdit   := :cName != 'ORDKEYNO' )
          :bPrevEdit := {|uv,ob| wPost( VK_RETURN+ob:nCell, ob, {ob,.T.,uv} ), .F. }
       Endif
       // window width
       nW += :nWidth
       END WITN
   NEXT

   nY := oParent:Row + oParent:Height + GetBorderHeight()
   nX := 0

   If nW >  System.ClientWidth
      nW := System.ClientWidth
   Else
      nX := Int( ( System.ClientWidth - nW ) / 2 )
   EndIf

   If nH > ( System.ClientHeight - nY )
      nH :=  System.ClientHeight - nY
   EndIf

   nY += 2

   DEFINE WINDOW &cWnd AT nY, nX WIDTH nW HEIGHT nH           ;
		TITLE 'MiniGUI Demo TsBrowse: BASE.DBF'       ;
		CHILD        TOPMOST  NOMAXIMIZE   NOSIZE     ; 
		ON INIT               This.Topmost := .F.     ;
		ON RELEASE            wSend(98)               ;
		ON INTERACTIVECLOSE ( This.Object ):Action

      nY := 1
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2

      dbSelectArea( cAlsBase )

      oBrw := Tsb_Create(cBrw, nY, nX, nW, nH, aCols, aColors, hFontBold)

      WITH OBJECT oBrw
      :nFreeze     := :nColumn('ORDKEYNO')
      :lLockFreeze := .T.
      :nCell       := :nFreeze + 1

      :bLDblClick  := {|p1,p2,p3,ob| wPost( VK_RETURN+ob:nCell, ob, {ob,.F.,p1,p2,p3} ) }

      :SetNoHoles()
      :SetFocus()
      END WITH

      WITH OBJECT This.Object                           // ---- Window events
      // Events oBrw:nCell
      :Event( VK_RETURN+2, {|ow,ky,or| or := TsbBoxCust(ow, ky), CustSave(ow, ky, or) } )
      :Event( VK_RETURN+3, {|ow,ky,or| or := TsbBoxCust(ow, ky), CustSave(ow, ky, or) } )
      :Event( VK_RETURN+4, {|ow,ky,or| or := TsbBoxCust(ow, ky), CustSave(ow, ky, or) } )
      :Event( VK_RETURN+5, {|ow,ky,or| or := TsbBoxCust(ow, ky), CustSave(ow, ky, or) } )
      // Events
      :Event( 98         , {|  | (cAlsBase)->( dbCloseArea() ), ;
                                 (cAlsCust)->( dbCloseArea() ), ;
                                  oParent:Action := .T. } ) // UnLock work message parent window
      :Event( 99         , {|ow| ow:Release()           } )
      END WITH                                          // ---- Window events
      
   END WINDOW

   ACTIVATE WINDOW &cWnd
  
RETURN Nil

*----------------------------------------------------------------------------*
FUNC CustSave ( oWnd, nEvent, oRec )
*----------------------------------------------------------------------------*
   LOCAL nID
   LOCAL aPar := oWnd:GetProp(nEvent)
   LOCAL oBrw := aPar[1]

   If Empty(oRec) .or. ! HB_ISOBJECT(oRec)
      RETURN Nil
   EndIf

   nID := oRec:Get('CUSTNO')

// !!! Здесь заносим nID в CUSTNO BASE.DBF
   ( oBrw:cAlias )->( RLock() )
   ( oBrw:cAlias )->CUSTNO := nID
   ( oBrw:cAlias )->( dbUnlock() )
   oBrw:Refresh()

   ? ProcName(), oRec:GetAll(), oBrw:cControlName, oWnd:Name

   AEval(oRec:GetAll(), {|av,nv| _LogFile(.T., nv, av[1], av[2]) })

RETURN Nil

*----------------------------------------------------------------------------*
FUNC LandSave ( oWnd, nEvent, oRec )
*----------------------------------------------------------------------------*
   LOCAL nID
   LOCAL aPar := oWnd:GetProp(nEvent)
   LOCAL oBrw := aPar[1]

   If Empty(oRec) .or. ! HB_ISOBJECT(oRec)
      RETURN Nil
   EndIf

   nID := oRec:Get('COUNTRYNO')
   
// !!! Здесь заносим nID в COUNTRYNO BASE2.DBF
   ( oBrw:cAlias )->( RLock() )
   ( oBrw:cAlias )->COUNTRYNO := nID
   ( oBrw:cAlias )->( dbUnlock() )
   oBrw:Refresh()

   ? ProcName(), oRec:GetAll(), oBrw:cControlName, oWnd:Name

   AEval(oRec:GetAll(), {|av,nv| _LogFile(.T., nv, av[1], av[2]) })

RETURN Nil

*----------------------------------------------------------------------------*
FUNC Base_Country ( oParent )
*----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, oCol
   LOCAL cAlsBase, cAlsLand, hFontBold
   LOCAL cWnd := 'wBase2', aCols := {}
   LOCAL cBrw := 'oBase2', oBrw, aColors

   oParent:Action := .F.

   // Init TBROWSE columns
   AAdd( aCols, gCols( OrdKeyNo       ) )
   AAdd( aCols, gCols( Land.COUNTRYNO ) )
   AAdd( aCols, gCols( Land.KOD       ) )
   AAdd( aCols, gCols( Land.NAME      ) )
   AAdd( aCols, gCols( Land.ISES      ) )

   hFontBold := GetFontHandle('FontBold')

   MyUse('Country' , 'LAND');   cAlsLand := Alias()
   MyUse('Base2'   , 'BASE');   cAlsBase := Alias()

   nH := 700
   nW := GetVScrollBarWidth() + GetBorderWidth() * 2
   
   FOR EACH oCol IN aCols
       WITH OBJECT oCol
       // real alias
       If     :cAlias $ cAlsLand ; :cAlias := cAlsLand
       ElseIf :cAlias $ cAlsBase ; :cAlias := cAlsBase
       ElseIf '____'  $ :cAlias  ; :cAlias := cAlsBase
       EndIf
       // other
       If :cName == 'COUNTRYNO'
          :cAlias  := cAlsBase
          :bSeek   := {|ob,nc| ( cAlsLand )->( dbSeek(ob:GetValue(nc), .F.) ) }
       EndIf
       // edit mode
       If ( :lEdit   := :cName != 'ORDKEYNO' )
          :bPrevEdit := {|uv,ob| wPost( VK_RETURN + ob:nCell, ob, {ob,.T.,uv} ), .F. }
       Endif
       // window width
       nW += :nWidth
       END WITN
   NEXT

   nY := oParent:Row + oParent:Height + GetBorderHeight()
   nX := 0

   If nW >  System.ClientWidth
      nW := System.ClientWidth
   Else
      nX := Int( ( System.ClientWidth - nW ) / 2 )
   EndIf

   If nH > ( System.ClientHeight - nY )
      nH :=  System.ClientHeight - nY
   EndIf

   nY += 2

   DEFINE WINDOW &cWnd AT nY, nX WIDTH nW HEIGHT nH          ;
		TITLE 'MiniGUI Demo TsBrowse: BASE2.DBF'     ;
		CHILD        TOPMOST  NOMAXIMIZE   NOSIZE    ; 
		ON INIT               This.Topmost := .F.    ;
		ON RELEASE            wSend(98)              ;
		ON INTERACTIVECLOSE ( This.Object ):Action

      nY := 1
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2
      
      dbSelectArea( cAlsBase )

      oBrw := Tsb_Create(cBrw, nY, nX, nW, nH, aCols, aColors, hFontBold)

      WITH OBJECT oBrw
      :nFreeze     := :nColumn('ORDKEYNO')
      :lLockFreeze := .T.
      :nCell       := :nFreeze + 1

      :bLDblClick  := {|p1,p2,p3,ob| wPost( VK_RETURN + ob:nCell, ob, {ob,.F.,p1,p2,p3} ) }
      :SetNoHoles()
      :SetFocus()
      END WITH

      WITH OBJECT This.Object                           // ---- Window events
      // Events oBrw:nCell
      :Event( VK_RETURN+2, {|ow,ky,or| or := TsbBoxLand(ow, ky), LandSave(ow, ky, or) } )
      :Event( VK_RETURN+3, {|ow,ky,or| or := TsbBoxLand(ow, ky), LandSave(ow, ky, or) } )
      :Event( VK_RETURN+4, {|ow,ky,or| or := TsbBoxLand(ow, ky), LandSave(ow, ky, or) } )
      // Events
      :Event( 98         , {|  | (cAlsBase)->( dbCloseArea() ), ;
                                 (cAlsLand)->( dbCloseArea() ), ;
                                  oParent:Action := .T.       } )
      :Event( 99         , {|ow| ow:Release()                 } )
      END WITH                                          // ---- Window events
      
   END WINDOW

   ACTIVATE WINDOW &cWnd
  
RETURN Nil

*----------------------------------------------------------------------------*
FUNC TsbBoxCust ( oParent, nEvent )
*----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, nD, oCol, oBrw
   LOCAL cAlsCust, hFontBold
   LOCAL cWnd := 'wCust', aCols := {}
   LOCAL cBrw := 'oCust', aColors
   LOCAL aPar := oParent:GetProp(nEvent)
   LOCAL oTsb := aPar[1]
   LOCAL nCel  := oTsb:nCell
   LOCAL nRow  := oTsb:nRowPos
   LOCAL oCel  := oTsb:GetCellinfo(nRow, nCel, .F.)
   LOCAL nHgt  := oTsb:nHeightCell
   LOCAL nLine := 10
   LOCAL lM    := _HMG_IsModalActive
   LOCAL hM    := _HMG_ActiveModalHandle
   LOCAL oRet  

   If oParent:Type == 'M'            // modal
      _HMG_IsModalActive     := .F.
      _HMG_ActiveModalHandle := 0
   EndIf

   // Init TBROWSE columns
   AAdd( aCols, gCols( Cust.COMPANY ) )
   AAdd( aCols, gCols( Cust.COUNTRY ) )
   AAdd( aCols, gCols( Cust.CITY    ) )

   hFontBold := GetFontHandle('FontNorm')

   MyUse('Customer', 'CUST');   cAlsCust := Alias()

   nW := GetVScrollBarWidth() + GetBorderWidth() * 2 + 10
   
   FOR EACH oCol IN aCols
       WITH OBJECT oCol
       // real alias
       :cAlias := cAlsCust
       // window width
       nW += :nWidth
       END WITN
   NEXT
   
   nD := GetTitleBarHeight() + GetBorderHeight() + 4
   nY := oParent:Row + oCel:nRow + nHgt + nD
   nX := oParent:Col + oCel:nCol 
   nH := ( nLine + 1 ) * nHgt + nHgt * 2

   If ( nY + nH + nD + GetBorderHeight() ) > System.ClientHeight
      nY -= ( nH + nHgt + nD + GetBorderHeight() )
   EndIf
   
   If ( nX + nW + GetBorderWidth() ) > System.ClientWidth
      nX := nX + oCel:nWidth - nW
   EndIf

   DEFINE WINDOW &cWnd AT nY, nX CLIENTAREA nW, nH             ;
		TITLE ProcName()                               ;
		CHILD        TOPMOST  NOMAXIMIZE   NOSIZE      ;
		ON RELEASE   wSend(98)                         ;
		ON LOSTFOCUS do_Obj( This.Handle, {|ow| ow:Release() } )

      nY := 1
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2
      
      dbSelectArea( cAlsCust )

      oBrw := Tsb_Create(cBrw, nY, nX, nW, nH, aCols, aColors, hFontBold)

      WITH OBJECT oBrw
      :UserKeys    ( VK_RETURN, {|ob| oRet := (ob:cAlias)->( RecGet() ), ;
                                                   _PushKey(VK_ESCAPE) } )
      :bLDblClick := {|p1,p2,p3,ob| ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

      :SetNoHoles()
      :SetFocus()
      END WITH

      WITH OBJECT This.Object                           // ---- Window events
      // Events
      :Event( 98, {|  | (cAlsCust)->( dbCloseArea() ) } )
      :Event( 99, {|ow| ow:Release()                  } )
      END WITH                                          // ---- Window events

      ON KEY ESCAPE ACTION wPost(99)
      
   END WINDOW

   ACTIVATE WINDOW &cWnd

   If oParent:Type == 'M'                // modal
      _HMG_IsModalActive     := lM
      _HMG_ActiveModalHandle := hM
   EndIf

   oParent:SetFocus(oTsb:hWnd)

RETURN oRet

*----------------------------------------------------------------------------*
FUNC TsbBoxLand ( oParent, nEvent )
*----------------------------------------------------------------------------*
   LOCAL nY, nX, nW, nH, nD, oCol, oBrw
   LOCAL cAlsLand, hFontBold
   LOCAL cWnd  := 'wLand', aCols := {}
   LOCAL cBrw  := 'oLand', aColors
   LOCAL aPar  := oParent:GetProp(nEvent)
   LOCAL oTsb  := aPar[1]
   LOCAL nCel  := oTsb:nCell
   LOCAL nRow  := oTsb:nRowPos
   LOCAL oCel  := oTsb:GetCellinfo(nRow, nCel, .F.)
   LOCAL nHgt  := oTsb:nHeightCell
   LOCAL nLine := 10
   LOCAL lM    := _HMG_IsModalActive
   LOCAL hM    := _HMG_ActiveModalHandle
   LOCAL oRet  

   If oParent:Type == 'M'            // modal
      _HMG_IsModalActive     := .F.
      _HMG_ActiveModalHandle := 0
   EndIf

   // Init TBROWSE columns
   AAdd( aCols, gCols( Land.KOD       ) )
   AAdd( aCols, gCols( Land.NAME      ) )
   AAdd( aCols, gCols( Land.ISES      ) )

   hFontBold := GetFontHandle('FontNorm')

   MyUse('Country', 'LAND');   cAlsLand := Alias()

   nW := GetVScrollBarWidth() + GetBorderWidth() * 2 + 10
   
   FOR EACH oCol IN aCols
       WITH OBJECT oCol
       // real alias
       :cAlias := cAlsLand
       // window width
       nW += :nWidth
       END WITN
   NEXT
   
   nD := GetTitleBarHeight() + GetBorderHeight() + 4
   nY := oParent:Row + oCel:nRow + nHgt + nD
   nX := oParent:Col + oCel:nCol 
   nH := ( nLine + 1 ) * nHgt + nHgt * 2

   If ( nY + nH + nD + GetBorderHeight() ) > System.ClientHeight
      nY -= ( nH + nHgt + nD + GetBorderHeight() )
   EndIf
   
   If ( nX + nW + GetBorderWidth() ) > System.ClientWidth
      nX := nX + oCel:nWidth - nW
   EndIf

   DEFINE WINDOW &cWnd AT nY, nX CLIENTAREA nW, nH             ;
		TITLE ProcName()                               ;
		CHILD        TOPMOST  NOMAXIMIZE   NOSIZE      ;
		ON RELEASE   wSend(98)                         ;
		ON LOSTFOCUS do_Obj( This.Handle, {|ow| ow:Release() } )

      nY := 1
      nX := 1
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2
      
      dbSelectArea( cAlsLand )

      oBrw := Tsb_Create(cBrw, nY, nX, nW, nH, aCols, aColors, hFontBold)

      WITH OBJECT oBrw
      :UserKeys    ( VK_RETURN, {|ob| oRet := (ob:cAlias)->( RecGet() ), ;
                                                   _PushKey(VK_ESCAPE) } )
      :bLDblClick := {|p1,p2,p3,ob| ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

      :SetNoHoles()
      :SetFocus()
      END WITH

      WITH OBJECT This.Object                           // ---- Window events
      // Events
      :Event( 98, {|  | (cAlsLand)->( dbCloseArea() ) } )
      :Event( 99, {|ow| ow:Release()                  } )
      END WITH                                          // ---- Window events

      ON KEY ESCAPE ACTION wPost(99)
      
   END WINDOW

   ACTIVATE WINDOW &cWnd

   If oParent:Type == 'M'                // modal
      _HMG_IsModalActive     := lM
      _HMG_ActiveModalHandle := hM
   EndIf

   oParent:SetFocus(oTsb:hWnd)

RETURN oRet

