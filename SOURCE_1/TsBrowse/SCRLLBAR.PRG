// ==============================================================================
// SCRLLBAR.PRG Version 5.0 27/Feb/2002
// Is an adaptation of FiveTech TScrollBar Class to be used with TSBrowse
// ==============================================================================

#ifdef __XHARBOUR__
#define __SYSDATA__

#endif
#include "minigui.ch"
#include "hbclass.ch"
#include "TSBrowse.ch"

#define SB_HORZ         0
#define SB_VERT         1
#define SB_CTL          2

#define COLOR_SCROLLBAR 0


CLASS TSBScrlBar FROM TControl

   DATA lVertical, lReDraw, lIsChild, nMin, nMax, nPgStep
   DATA bGoUp, bGoDown, bGoTop, bGoBottom, bPageUp, bPageDown, bPos
   DATA bTrack
   DATA l32Bit
   DATA lShowDisabled, hWnd, oWnd
   DATA lUpdate AS LOGICAL // TControl
   DATA bWhen // TWindow
   DATA bValid // TWindow

   CLASSDATA aProperties INIT { "cVarName", "nMin", "nMax", ;
      "nPgStep", "nTop", "nLeft", "Cargo" }

   METHOD New( nRow, nCol, nMin, nMax, nPgStep, lVertical, oWnd, nWidth, nHeight, ;
      bUpAct, bDownAct, bPgUp, bPgDown, bPos, lPixel, nClrText, ;
      nClrBack, cMsg, lUpdate, bWhen, bValid, lDesign ) CONSTRUCTOR

   METHOD WinNew( nMin, nMax, nPgStep, lVertical, oWnd, bUpAction, ;
      bDownAction, bPgUp, bPgDown, bPos, nClrText, nClrBack, ;
      lUpdate, bWhen, bValid ) CONSTRUCTOR

   METHOD GetPos() INLINE GetScrollPos( If( ::lIsChild, ::oWnd:hWnd, ::hWnd ), ;
      If( ::lIsChild, If( ::lVertical, SB_VERT, SB_HORZ ), SB_CTL ) )

   METHOD GetRange() INLINE GetScrlRange( If( ::lIsChild, ::oWnd:hWnd, ::hWnd ), ;
      If( ::lIsChild, If( ::lVertical, SB_VERT, SB_HORZ ), SB_CTL ) )

   METHOD HandleEvent( nMsg, nWParam, nLParam )

   // These two have to be BLOCK

   METHOD GoUp() BLOCK {| Self, nPos | nPos := ::GetPos(), ;
      if( nPos > ::GetRange()[ 1 ], ;
      ::SetPos( --nPos ), ), ;
      If( ::bGoUp != NIL, Eval( ::bGoUp ), ) }

   METHOD GoDown() BLOCK {| Self, nPos | nPos := ::GetPos(), ;
      if( nPos < ::nMax, ;
      ::SetPos( ++nPos ), ), ;
      If( ::bGoDown != NIL, Eval( ::bGoDown ), ) }


   METHOD GoTop() INLINE ::SetPos( ::nMin ), ;
      If( ::bGoTop != NIL, Eval( ::bGoTop ), )

   METHOD GoBottom() INLINE ::SetPos( ::nMax ), ;
      If( ::bGoBottom != NIL, Eval( ::bGoBottom ), )

   METHOD PageUp() INLINE If( ::bPageUp != NIL, Eval( ::bPageUp ), ), ;
      ::SetPos( ::GetPos() - ::nPgStep )

   METHOD PageDown() INLINE If( ::bPageDown != NIL, Eval( ::bPageDown ), ), ;
      ::SetPos( ::GetPos() + ::nPgStep )

   METHOD SetMode( l32Bit )

   METHOD SetPage( nSize, lReDraw )

   METHOD SetPos( nPos ) INLINE SetScrollPos( If( ::lIsChild, ::oWnd:hWnd, ;
      ::hWnd ), If( ::lIsChild, If( ::lVertical, ;
      SB_VERT, SB_HORZ ), SB_CTL ), ;
      nPos, ::lReDraw, ::lShowDisabled, ;
      ::l32Bit )

   METHOD SetRange( nMin, nMax ) INLINE ;
      ::nMin := nMin, ::nMax := nMax, ;
      SetScrollRange( if( ::lIsChild, ::oWnd:hWnd, ::hWnd ), ;
      If( ::lIsChild, If( ::lVertical, SB_VERT, SB_HORZ ), SB_CTL ), ;
      nMin, nMax, ::lReDraw, ::lShowDisabled, ::l32Bit ) // JP 74

   METHOD ThumbPos( nPos ) INLINE If( ::bPos != NIL, Eval( ::bPos, nPos ), )

   METHOD MouseMove( nRow, nCol, nKeyFlags )

   METHOD ThumbTrack( nPos ) INLINE If( ::bTrack != NIL, ;
      Eval( ::bTrack, nPos ), ;
      ::ThumbPos( nPos ) )

ENDCLASS

// ============================================================================
// METHOD TSBScrlBar:New() Version 5.0 27/Feb/2002
// ============================================================================

METHOD New( nRow, nCol, nMin, nMax, nPgStep, lVertical, oWnd, nWidth, nHeight, ;
      bUpAct, bDownAct, bPgUp, bPgDown, bPos, lPixel, nClrText, ;
      nClrBack, cMsg, lUpdate, bWhen, bValid, lDesign ) CLASS TSBScrlBar

   DEFAULT nRow := 0, nCol := 0, ;
      nMin := 0, nMax := 0, nPgStep := 1, ;
      lVertical := .T., nWidth := If( lVertical, 16, 100 ), ;
      nHeight := If( lVertical, 100, 17 ), ;
      lPixel := .F., ;
      nClrText := GetSysColor( COLOR_WINDOW ), ;
      nClrBack := GetSysColor( COLOR_SCROLLBAR ), ;
      lUpdate := .F., lDesign := .F.

   ::cCaption := ""
   ::nTop := nRow * If( lPixel, 1, SCRL_CHARPIX_H ) // 14
   ::nLeft := nCol * If( lPixel, 1, SCRL_CHARPIX_W ) // 8
   ::nBottom := ::nTop + nHeight - 1
   ::nRight := ::nLeft + nWidth - 1
   ::nMin := nMin
   ::nMax := nMax
   ::nPgStep := nPgStep
   ::lVertical := lVertical
   ::lReDraw := .T.
   ::nStyle := nOr( WS_CHILD, WS_VISIBLE, WS_TABSTOP, ;
      iif( lVertical, SBS_VERT, SBS_HORZ ), ;
      iif( lDesign, WS_CLIPSIBLINGS, 0 ) )
   ::bGoUp := bUpAct
   ::bGoDown := bDownAct
   ::bPageUp := bPgUp
   ::bPageDown := bPgDown
   ::bPos := bPos
   ::oWnd := oWnd
   ::lIsChild := .F.
   ::lDrag := lDesign
   ::lCaptured := .F.
   ::cMsg := cMsg
   ::lUpdate := lUpdate
   ::bWhen := bWhen
   ::bValid := bValid

   IF ! Empty( ::oWnd:hWnd )
      ::Create( "SCROLLBAR" )
      ::SetRange( ::nMin, ::nMax )
      ::SetPos( ::nMin )
   ENDIF

   IF lDesign
      ::CheckDots()
   ENDIF

RETURN Self

// ============================================================================
// METHOD TSBScrlBar:WinNew() Version 5.0 27/Feb/2002
// Constructor for non-true ScrollBar Controls
// ( when using WS_VSCROLL, WS_HSCROLL styles in a Window )
// They are NOT controls but we consider them as real Objects!
// ============================================================================

METHOD WinNew( nMin, nMax, nPgStep, lVertical, oWnd, bUpAction, ;
      bDownAction, bPgUp, bPgDown, bPos, nClrText, nClrBack, ;
      lUpdate, bWhen, bValid ) CLASS TSBScrlBar

   DEFAULT nMin := 1, nMax := 2, nPgStep := 1, lVertical := .T., ;
      nClrText := GetSysColor( COLOR_WINDOW ), ;
      nClrBack := GetSysColor( COLOR_SCROLLBAR ), ;
      lUpdate := .F.

   ::oWnd := oWnd
   ::lVertical := lVertical
   ::lReDraw := .T.
   ::lIsChild := .T.
   ::nMin := nMin
   ::nMax := nMax
   ::nPgStep := nPgStep
   ::bGoUp := bUpAction
   ::bGoDown := bDownAction
   ::bPageUp := bPgUp
   ::bPageDown := bPgDown
   ::bPos := bPos
   ::lUpdate := lUpdate
   ::bWhen := bWhen
   ::bValid := bValid
   ::hWnd := 0
   ::lShowDisabled := .F.
   ::l32Bit := ::SetMode()

   ::SetPage( ::nPgStep, .T. )
   ::SetRange( nMin, nMax )
   ::SetPos( nMin )

RETURN Self

// ============================================================================
// METHOD TSBScrlBar:HandleEvent() Version 5.0 27/Feb/2002
// ============================================================================

METHOD HandleEvent( nMsg, nWParam, nLParam ) CLASS TSBScrlBar

   DO CASE
   CASE nMsg == FM_SCROLLUP
      ::GoUp()
      RETURN 0

   CASE nMsg == FM_SCROLLDOWN
      ::GoDown()
      RETURN 0

   CASE nMsg == FM_SCROLLPGUP
      ::PageUp()
      RETURN 0

   CASE nMsg == FM_SCROLLPGDN
      ::PageDown()
      RETURN 0

   CASE nMsg == FM_THUMBPOS
      ::ThumbPos( nWParam )
      RETURN 0

   CASE nMsg == FM_THUMBTRACK
      ::ThumbTrack( nWParam )
      RETURN 0
   ENDCASE

RETURN ::Super:HandleEvent( nMsg, nWParam, nLParam )

// ============================================================================
// METHOD TSBScrlBar:MouseMove() Version 5.0 27/Feb/2002
// ============================================================================

METHOD MouseMove( nRow, nCol, nKeyFlags ) CLASS TSBScrlBar

   LOCAL nResult := ::Super:MouseMove( nRow, nCol, nKeyFlags )

RETURN If( ::lDrag, nResult, Nil ) // We want standard behavior !!!

// ============================================================================
// METHOD TSBScrlBar:SetPage() Version 5.0 27/Feb/2002
// ============================================================================

METHOD SetPage( nSize, lReDraw ) CLASS TSBScrlBar

   LOCAL nFlags

   DEFAULT lRedraw := .F.

   IF ! Empty( ::hWnd )
      nFlags = SB_CTL
   ELSE
      IF ::lVertical
         nFlags = SB_VERT
      ELSE
         nFlags = SB_HORZ
      ENDIF
   ENDIF

   ::nPgStep := nSize

   SetScrollInfo( If( ! Empty( ::hWnd ), ::oWnd:hWnd, ::hWnd ), ;
      nFlags, nSize, lReDraw, ::lShowDisabled )

RETURN NIL

// ============================================================================
// METHOD TSBScrlBar:SetMode() Version 5.0 27/Feb/2002
// ============================================================================

METHOD SetMode( l32Bit ) CLASS TSBScrlBar

   LOCAL lWin32 := .T. // IsWin95()  // W2K also returns TRUE. NT 4 doesn't support SetScrollInfo

   DEFAULT l32Bit := .T.

RETURN iif( l32Bit .AND. ! lWin32, .F., l32Bit )
