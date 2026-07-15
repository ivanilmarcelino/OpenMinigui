#include "minigui.ch"
#include "hbclass.ch"
#include "TSBrowse.ch"

#define EM_GETSEL     176
#define EN_CHANGE     768  // 0x0300
#define EN_UPDATE     1024 // 0x0400

* ============================================================================
* CLASS TSMulti  Version 7.0 Jul/15/2004
* ============================================================================

CLASS TSMulti FROM TControl

   CLASSDATA lRegistered AS LOGICAL

   DATA Atx, lAppend, lChanged
   DATA nPos

   METHOD New( nRow, nCol, bSetGet, oWnd, nWidth, nHeight, hFont, ;
               nClrFore, nClrBack, cControl, cWnd )

   METHOD Default()
   METHOD HandleEvent( nMsg, nWParam, nLParam )
   METHOD GetDlgCode( nLastKey, nFlags )
   METHOD KeyChar( nKey, nFlags )
   METHOD KeyDown( nKey, nFlags )
   METHOD LostFocus( hCtlFocus )
   METHOD lValid()
   METHOD Command( nWParam, nLParam )
   METHOD Save()

ENDCLASS

* ============================================================================
* METHOD TSMulti:New() Version 7.0 Jul/15/2004
* ============================================================================

METHOD New( nRow, nCol, bSetGet, oWnd, nWidth, nHeight, hFont, ;
            nClrFore, nClrBack, cControl, cWnd ) CLASS TSMulti

   DEFAULT nClrFore := GetSysColor( COLOR_WINDOWTEXT ), ;
           nClrBack := GetSysColor( COLOR_WINDOW ), ;
            nHeight := 12

   ::nTop    := nRow
   ::nLeft   := nCol
   ::nBottom := ::nTop + nHeight - 1
   ::nRight  := ::nLeft + nWidth - 1

   IF oWnd == NIL
      oWnd := Self
      oWnd:hWnd := GetFormHandle ( cWnd )
   ENDIF
   ::oWnd := oWnd

   IF _HMG_BeginWindowMDIActive
      cWnd := _GetWindowProperty ( GetActiveMdiHandle(), "PROP_FORMNAME" )
   ENDIF

   ::nId := ::GetNewId()
   ::nStyle := nOR( ES_MULTILINE, ES_WANTRETURN, WS_CHILD, WS_BORDER, ;
               WS_THICKFRAME, WS_VSCROLL, WS_HSCROLL )

   ::cControlName := cControl
   ::cParentWnd   := cWnd
   ::hWndParent   := oWnd:hWnd
   ::bSetGet      := bSetGet
   ::lCaptured    := .F.
   ::hFont        := hFont
   ::lFocused     := .F.
   ::lAppend      := .F.
   ::nLastKey     := 0
   ::lChanged     := .F.

   //::SetColor( nClrFore, nClrBack )

   ::Atx := 0

   if ! Empty( ::oWnd:hWnd )

      ::Create( "EDIT" )
      ::AddVars( ::hWnd )
      ::Default()

      IF hFont != NIL
         ::hFont := hFont
      ENDIF
      oWnd:AddControl( ::hWnd )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSMulti:Default() Version 7.0 Jul/15/2004
* ============================================================================

METHOD Default() CLASS TSMulti

   LOCAL Value := Eval( ::bSetGet )

   If ! Empty( Value )
      SetWindowText( ::hWnd, Value )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSMulti:HandleEvent() Version 7.0 Jul/15/2004
* ============================================================================

METHOD HandleEvent( nMsg, nWParam, nLParam ) CLASS TSMulti

RETURN ::Super:HandleEvent( nMsg, nWParam, nLParam )

* ============================================================================
* METHOD TSMulti:GetDlgCode() Version 7.0 Jul/15/2004
* ============================================================================

METHOD GetDlgCode( nLastKey, nFlags ) CLASS TSMulti

   HB_SYMBOL_UNUSED( nFlags )
   ::nLastKey := nLastKey

RETURN DLGC_WANTALLKEYS + DLGC_WANTCHARS

* ============================================================================
* METHOD TSMulti:KeyChar() Version 7.0 Jul/15/2004
* ============================================================================

METHOD KeyChar( nKey, nFlags ) CLASS TSMulti

   HB_SYMBOL_UNUSED( nFlags )

   IF _GetKeyState( VK_CONTROL )
      nKey := If( Upper( Chr( nKey ) ) == "W" .OR. nKey == VK_RETURN, VK_TAB, nKey )
   ENDIF

   IF nKey == VK_TAB .OR. nKey == VK_ESCAPE
      RETURN 0
   ENDIF

RETURN ::Super:KeyChar( nKey, nFlags )

* ============================================================================
* METHOD TSMulti:Save() Version 7.0 Jul/15/2004
* ============================================================================

METHOD Save() CLASS TSMulti

   LOCAL cText

   IF ::bSetGet != NIL
      cText := ::GetText()
      IF Right( cText, 2 ) == CRLF
         cText := SubStr( cText, 1, Len( cText ) - 2 )
      ENDIF

      IF ::oCol:lEditBox .and. ! Empty( cText ) .and. CRLF $ cText
         IF Len( ::oCol:cEditBoxSep ) > 0
            cText := StrTran( cText, CRLF, ::oCol:cEditBoxSep )
         ELSEIF ::oCol:nEditBoxWrap > 0
            cText := StrTran( cText, CRLF, " " )
         ENDIF
      ENDIF

      Eval( ::bSetGet, cText )
      IF Empty( ::oCol:bEditEnd )
         ::oBrw:PostEdit( cText, ::nCol )
      ENDIF
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSMulti:KeyDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD KeyDown( nKey, nFlags ) CLASS TSMulti

   IF _GetKeyState( VK_CONTROL )
      nKey := If( Upper( Chr( nKey ) ) == "W" .OR. nKey == VK_RETURN, VK_TAB, nKey )
   ENDIF

   ::nLastKey := nKey

   IF nKey == VK_TAB .OR. nKey == VK_ESCAPE

      IF ::lValid()

         IF nKey != VK_ESCAPE
            ::Save()
         ENDIF

         ::bLostFocus := NIL
         Eval( ::bKeyDown, nKey, nFlags, .T. )

      ENDIF

   ENDIF

RETURN 0

* ============================================================================
* METHOD TSMulti:lValid() Version 7.0 Jul/15/2004
* ============================================================================

METHOD lValid() CLASS TSMulti

   LOCAL lRet := .T.

   IF ValType( ::bValid ) == "B"
      lRet := Eval( ::bValid, ::GetText() )
   ENDIF

RETURN lRet

* ============================================================================
* METHOD TSMulti:LostFocus() Version 7.0 Jul/15/2004
* ============================================================================

METHOD LostFocus( hCtlFocus ) CLASS TSMulti

   ::lFocused := .F.

   ::nPos := LoWord( ::SendMsg( EM_GETSEL ) )
   IF ::bLostFocus != NIL
      Eval( ::bLostFocus, ::nLastKey, hCtlFocus )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSMulti:Command() Version 7.0 Jul/15/2004
* ============================================================================

METHOD Command( nWParam, nLParam ) CLASS TSMulti

   LOCAL nNotifyCode, hWndCtl

   nNotifyCode := HiWord( nWParam )
// nID   := LoWord( nWParam )
   hWndCtl := nLParam

   DO CASE
   CASE hWndCtl == 0

      * Enter ..........................................
      IF HiWord( nWParam ) == 0 .AND. LoWord( nWParam ) == 1
         ::KeyDown( VK_RETURN, 0 )
      ENDIF

      * Escape .........................................
      IF HiWord( nwParam ) == 0 .AND. LoWord( nwParam ) == 2
         ::KeyDown( VK_ESCAPE, 0 )
      ENDIF

   CASE hWndCtl != 0

      DO CASE
      CASE nNotifyCode == EN_CHANGE
         ::lChanged := .T.

      CASE nNotifyCode == EN_KILLFOCUS
         ::LostFocus()

      CASE nNotifyCode == EN_UPDATE
         IF _GetKeyState( VK_ESCAPE )
            ::KeyDown( VK_ESCAPE, 0 )
         ENDIF
         IF _GetKeyState( VK_CONTROL )
            IF GetKeyState( VK_RETURN ) == -127 .OR. _GetKeyState( VK_RETURN )
               ::KeyDown( VK_RETURN, 0 )
            ENDIF
         ENDIF
      ENDCASE

   ENDCASE

RETURN NIL
