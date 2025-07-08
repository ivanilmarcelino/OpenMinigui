#include "minigui.ch"
#include "hbclass.ch"
#include "TSBrowse.ch"


#define CBS_NOINTEGRALHEIGHT  0x0400

#define COMBO_BASE       320

#define CB_SETEDITSEL    ( COMBO_BASE +  2 )
#define CB_SHOWDROPDOWN  ( COMBO_BASE + 15 )
#define CB_ERR           -1
#define CBN_CLOSEUP      8


* ============================================================================
* CLASS TComboBox  Driver for ComboBox  TSBrowse 7.0
* ============================================================================

CLASS TComboBox FROM TControl

   CLASSDATA lRegistered AS LOGICAL
   
   DATA Atx
   DATA lAppend
   DATA nAt
   DATA aItems   AS ARRAY                     // Combo array
   DATA bCloseUp                              // Block to be evaluated on Close Combo

   METHOD New( nRow, nCol, bSetGet, aGetData, nWidth, nHeight, oWnd, bChanged,;
             nClrFore, nClrBack, hFont, cMsg, cControl, cWnd )
   METHOD Default()
   METHOD GetDlgCode( nLastKey, nFlags )
   METHOD HandleEvent( nMsg, nWParam, nLParam )
   METHOD KeyDown( nKey, nFlags )
   METHOD KeyChar( nKey, nFlags )
   METHOD LButtonDown( nRow, nCol )
   METHOD LostFocus()

ENDCLASS

* ============================================================================
* METHOD TComboBox:New() Version 7.0
* ============================================================================

METHOD New( nRow, nCol, bSetGet, aGetData, nWidth, nHeight, oWnd, bChanged,;
             nClrFore, nClrBack, hFont, cMsg, cControl, cWnd ) CLASS TComboBox

   LOCAL invisible     := .F.
   LOCAL sort          := .F.
   LOCAL displaychange := .F.
   LOCAL notabstop     := .F.
   LOCAL ParentHandle

   DEFAULT nClrFore  := GetSysColor( COLOR_WINDOWTEXT ),;
           nClrBack  := GetSysColor( COLOR_WINDOW ),;
           nHeight   := 12

   ::nTop         := nRow
   ::nLeft        := nCol
   ::nBottom      := ::nTop + nHeight - 1
   ::nRight       := ::nLeft + nWidth - 1
   
   if oWnd == Nil
       oWnd       := Self
       oWnd:hWnd  := GetFormHandle( cWnd )                  //JP
   endif
   
   ::oWnd         := oWnd
   ParentHandle   := oWnd:hWnd
   
   if _HMG_BeginWindowMDIActive
        ParentHandle :=  GetActiveMdiHandle()
        cWnd         := _GetWindowProperty ( ParentHandle, "PROP_FORMNAME" )
   endif
   
   ::nId          := ::GetNewId()
   ::cControlName := cControl
   ::cParentWnd   := cWnd
   ::nStyle       := nOR( WS_CHILD, WS_VISIBLE, WS_TABSTOP, ;
                          WS_VSCROLL, WS_BORDER, CBS_DROPDOWN, CBS_NOINTEGRALHEIGHT )

   ::bSetGet      := bSetGet
   ::aItems       := aGetData
   ::lCaptured    := .F.
   ::hFont        := hFont
   ::cMsg         := cMsg
   ::bChange      := bChanged
   ::lFocused     := .F.
   ::lAppend      := .F.
   ::nLastKey     := 0
   ::Atx          := 0

   ::SetColor( nClrFore, nClrBack )

   if ! Empty( ParentHandle )

      ::hWnd := InitComboBox( ParentHandle, 0, nCol, nRow, nWidth , '', 0 , nHeight, invisible, notabstop, sort , displaychange , _HMG_IsXP )

      ::AddVars( ::hWnd )
      ::Default()

      if GetObjectType( hFont ) == OBJ_FONT
         _SetFontHandle( ::hWnd, hFont )
         ::hFont := hFont
      endif

      oWnd:AddControl( ::hWnd )

   endif

return Self

* ============================================================================
* METHOD TComboBox:Default() Version 7.0 Jul/15/2004
* ============================================================================

METHOD Default() CLASS TComboBox

   LOCAL i

   For i = 1 To Len( ::aItems )
      ComboAddString( ::hWnd, ::aItems[i] )
      If i == Eval( ::bSetGet )
         ComboSetCurSel( ::hWnd, i )
      Endif
   Next

Return NIL

* ============================================================================
* METHOD TComboBox:GetDlgCode() Version 7.0 Jul/15/2004
* ============================================================================

METHOD GetDlgCode( nLastKey, nFlags ) CLASS TComboBox

   HB_SYMBOL_UNUSED( nFlags )
   ::nLastKey := nLastKey

Return DLGC_WANTALLKEYS

* ============================================================================
* METHOD TComboBox:HandleEvent() Version 7.0 Jul/15/2004
* ============================================================================

METHOD HandleEvent( nMsg, nWParam, nLParam ) CLASS TComboBox

   If HiWord( nWParam ) == CBN_CLOSEUP
      if ::bCloseUp <> NIL
         If( ValType( ::bCloseUp ) == "B", Eval( ::bCloseUp, Self ), ::bCloseUp( Self ) )
         Return 0
      endif
   Endif

Return ::Super:HandleEvent( nMsg, nWParam, nLParam )

* ============================================================================
* METHOD TComboBox:KeyDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD KeyDown( nKey, nFlags ) CLASS TComboBox

   LOCAL nAt := ::SendMsg( CB_GETCURSEL )

   If nAt != CB_ERR
      ::nAt := nAt + 1
   Endif

   ::nLastKey := nKey
   If nKey == VK_TAB .or. nKey == VK_RETURN .or. nKey == VK_ESCAPE
      ::bLostFocus := Nil
      Eval( ::bKeyDown, nKey, nFlags, .T. )
      Return 0
   Endif

Return ::Super:KeyDown( nKey, nFlags )

* ============================================================================
* METHOD TComboBox:LostFocus() Version 7.0 Jul/15/2004
* ============================================================================

METHOD LostFocus() CLASS TComboBox

   LOCAL nAt

   DEFAULT ::lAppend := .F.

   If ::nLastKey == Nil .and. ::lAppend
      ::SetFocus()
      ::nLastKey := 0
      Return 0
   EndIf

   nAt := ::SendMsg( CB_GETCURSEL )

   If nAt != CB_ERR
      ::nAt = nAt + 1
      If ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, nAt + 1 )
      Else
         Eval( ::bSetGet, ::aItems[ nAt + 1 ] )
      Endif
   Else
      Eval( ::bSetGet, GetWindowText( ::hWnd ) )
   Endif

   ::lFocused := .F.

   If ::bLostFocus != Nil
      Eval( ::bLostFocus, ::nLastKey )
   EndIf

Return 0

* ============================================================================
* METHOD TComboBox:KeyChar() Version 7.0 Jul/15/2004
* ============================================================================

METHOD KeyChar( nKey, nFlags ) CLASS TComboBox

   Do Case
      Case nKey == VK_TAB
           Return 0            // We don't want API default behavior

      Case nKey == VK_ESCAPE
           Return 0

      Case nKey == VK_RETURN
           Return 0

      Otherwise
         Return ::Super:Keychar( nKey, nFlags )
   Endcase

Return 0

* ============================================================================
* METHOD TComboBox:LButtonDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD LButtonDown( nRow, nCol ) CLASS TComboBox

   LOCAL nShow := 1
   
   HB_SYMBOL_UNUSED( nRow )
   HB_SYMBOL_UNUSED( nCol )

   If ::nLastKey != Nil .and. ::nLastKey == 9999
      nShow := 0
      ::nLastKey := 0
   Else
      ::nLastKey := 9999
   EndIf

   ::PostMsg( CB_SHOWDROPDOWN, nShow, 0 )

Return 0
