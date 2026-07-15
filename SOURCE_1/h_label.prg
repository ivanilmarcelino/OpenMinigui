/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "minigui.ch"
#include "i_winuser.ch"

/* Label layout constants */
#define LABEL_BLINK_INTERVAL_MS     500
#define LABEL_AUTOSIZE_ENABLED      1
#define LABEL_SMALL_FONT_LIMIT      14
#define LABEL_HEIGHT_PADDING_SMALL  12
#define LABEL_HEIGHT_PADDING_LARGE  16

*-----------------------------------------------------------------------------*
FUNCTION _DefineLabel( ControlName, ParentFormName, nCol, nRow, cCaption, nWidth, nHeight, ;
      cFontName, nFontSize, lBold, lBorder, lClientEdge, lHScroll, lVScroll, ;
      lTransparent, aRGB_Bk, aRGB_Font, bProcedure, cToolTip, nHelpId, lInvisible, ;
      lItalic, lUnderline, lStrikeout, lAutoSize, lRightAlign, lCenterAlign, ;
      lBlink, bMouseOver, bMouseLeave, lVCenterAlign, lNoPrefix, nId, bInit, bDblClick, bRClick )
*-----------------------------------------------------------------------------*
   LOCAL hParent, hCtrl, hFont
   LOCAL cVar, nIdx
   LOCAL nStyle, bDlgInit
   LOCAL lDialogInMemory

   /* Apply defaults */
   hb_default( @nWidth, 120 )
   hb_default( @nHeight, 24 )
   hb_default( @lInvisible, .F. )
   hb_default( @lBold, .F. )
   hb_default( @lItalic, .F. )
   hb_default( @lUnderline, .F. )
   hb_default( @lStrikeout, .F. )
   hb_default( @lVCenterAlign, .F. )
   __defaultNIL( @bProcedure, "" )
   __defaultNIL( @bRClick, "" )

   /* Font resolution */
   IF ( hFont := GetFontHandle( cFontName ) ) != 0
      GetFontParamByRef( hFont, @cFontName, @nFontSize, @lBold, @lItalic, @lUnderline, @lStrikeout )
   ENDIF

   /* Context detection for window/dialog/frame */
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
      __defaultNIL( @cFontName, _HMG_ActiveFontName )
      __defaultNIL( @nFontSize, _HMG_ActiveFontSize )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. .NOT. _HMG_ParentWindowActive
      nCol += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      nRow += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /* Validation */
   IF .NOT. _IsWindowDefined( ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError( "Window: " + hb_defaultValue( ParentFormName, "Parent" ) + " is not defined." )
   ENDIF

   IF hb_isChar( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF
   IF _IsControlDefined( ControlName, ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + " Already defined." )
   ENDIF

   /* Caption handling */
   IF hb_isArray( cCaption )
      cCaption := hb_ValToStr( cCaption )
   ELSEIF hb_isBlock( cCaption )
      cCaption := cValToChar( Eval( cCaption ) )
   ELSE
      cCaption := cValToChar( cCaption )
   ENDIF

   /* Color conversion */
   IF aRGB_Bk != NIL .AND. .NOT. IsArrayRGB( aRGB_Bk )
      aRGB_Bk := nRGB2Arr( aRGB_Bk )
   ENDIF
   IF aRGB_Font != NIL .AND. .NOT. IsArrayRGB( aRGB_Font )
      aRGB_Font := nRGB2Arr( aRGB_Font )
   ENDIF

   cVar := "_" + ParentFormName + "_" + ControlName
   nIdx := _GetControlFree()

   /* Control creation */
   IF _HMG_BeginDialogActive
      hParent := _HMG_ActiveDialogHandle
      nStyle := WS_CHILD + SS_NOTIFY
      IF lBorder      ; nStyle += WS_BORDER      ; ENDIF
      IF .NOT. lInvisible ; nStyle += WS_VISIBLE ; ENDIF
      IF lHScroll     ; nStyle += WS_HSCROLL     ; ENDIF
      IF lVScroll     ; nStyle += WS_VSCROLL     ; ENDIF
      IF lRightAlign  ; nStyle += ES_RIGHT       ; ENDIF
      IF lCenterAlign ; nStyle += ES_CENTER      ; ENDIF
      IF lVCenterAlign; nStyle += SS_CENTERIMAGE ; ENDIF

      IF lDialogInMemory
         bDlgInit := {|x,y,z| InitDialogLabel(x,y,z) }
         AAdd( _HMG_aDialogItems, { nId, nIdx, "static", nStyle, 0, nCol, nRow, nWidth, nHeight, ;
               cCaption, nHelpId, cToolTip, cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeout, ;
               bDlgInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )
      ELSE
         hCtrl := GetDialogItemHandle( hParent, nId )
         IF cCaption != NIL
            SetWindowText( hCtrl, cCaption )
         ENDIF
         SetWindowStyle( hCtrl, nStyle, .T. )
      ENDIF
   ELSE
      hParent := GetFormHandle( ParentFormName )
      hCtrl := InitLabel( hParent, cCaption, 0, nCol, nRow, nWidth, nHeight, '', ;
         ( hb_isBlock( bProcedure ) .OR. hb_isBlock( bDblClick ) .OR. hb_isBlock( bRClick ) .OR. hb_isString( cToolTip ) ), ;
         ( hb_isBlock( bMouseOver ) .OR. hb_isBlock( bMouseLeave ) ), ;
         lBorder, lClientEdge, lHScroll, lVScroll, lTransparent, lInvisible, lRightAlign, lCenterAlign, lVCenterAlign, lNoPrefix )
   ENDIF

   /* Post-creation setup */
   IF .NOT. lDialogInMemory
      IF hFont != 0
         _SetFontHandle( hCtrl, hFont )
      ELSE
         __defaultNIL( @cFontName, _HMG_DefaultFontName )
         __defaultNIL( @nFontSize, _HMG_DefaultFontSize )
         hFont := _SetFont( hCtrl, cFontName, nFontSize, lBold, lItalic, lUnderline, lStrikeout )
      ENDIF

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, hCtrl )
      ENDIF
      IF cToolTip != NIL
         SetToolTip( hCtrl, cToolTip, GetFormToolTipHandle( ParentFormName ) )
      ENDIF
   ENDIF

   /* Register control in HMG arrays */
#ifdef _NAMES_LIST_
   _SetNameList( cVar, nIdx )
#else
   Public &cVar. := nIdx
#endif

   _HMG_aControlType[ nIdx ] := "LABEL"
   _HMG_aControlNames[ nIdx ] := ControlName
   _HMG_aControlHandles[ nIdx ] := hCtrl
   _HMG_aControlParenthandles[ nIdx ] := hParent
   _HMG_aControlIds[ nIdx ] := nId
   _HMG_aControlProcedures[ nIdx ] := bProcedure
   _HMG_aControlPageMap[ nIdx ] := {}
   _HMG_aControlValue[ nIdx ] := NIL
   _HMG_aControlInputMask[ nIdx ] := lTransparent
   _HMG_aControllostFocusProcedure[ nIdx ] := bMouseLeave
   _HMG_aControlGotFocusProcedure[ nIdx ] := bMouseOver
   _HMG_aControlChangeProcedure[ nIdx ] := bRClick
   _HMG_aControlDeleted[ nIdx ] := .F.
   _HMG_aControlBkColor[ nIdx ] := aRGB_Bk
   _HMG_aControlFontColor[ nIdx ] := aRGB_Font
   _HMG_aControlDblClick[ nIdx ] := _HMG_ActiveTabButtons
   _HMG_aControlHeadClick[ nIdx ] := bDblClick
   _HMG_aControlRow[ nIdx ] := nRow
   _HMG_aControlCol[ nIdx ] := nCol
   _HMG_aControlWidth[ nIdx ] := nWidth
   _HMG_aControlHeight[ nIdx ] := nHeight
   _HMG_aControlSpacing[ nIdx ] := iif( lAutoSize, LABEL_AUTOSIZE_ENABLED, 0 )
   _HMG_aControlContainerRow[ nIdx ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ nIdx ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ nIdx ] := ""
   _HMG_aControlContainerHandle[ nIdx ] := 0
   _HMG_aControlFontName[ nIdx ] := cFontName
   _HMG_aControlFontSize[ nIdx ] := nFontSize
   _HMG_aControlFontAttributes[ nIdx ] := { lBold, lItalic, lUnderline, lStrikeout }
   _HMG_aControlToolTip[ nIdx ] := cToolTip
   _HMG_aControlRangeMin[ nIdx ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveTabName, '' )
   _HMG_aControlRangeMax[ nIdx ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ], '' )
   _HMG_aControlCaption[ nIdx ] := cCaption
   _HMG_aControlVisible[ nIdx ] := .NOT. lInvisible
   _HMG_aControlHelpId[ nIdx ] := nHelpId
   _HMG_aControlFontHandle[ nIdx ] := hFont
   _HMG_aControlBrushHandle[ nIdx ] := 0
   _HMG_aControlEnabled[ nIdx ] := .T.
   _HMG_aControlMiscData1[ nIdx ] := { 0, lBlink, .T. }
   _HMG_aControlMiscData2[ nIdx ] := ''

   /* Final UI adjustments */
   IF .NOT. lDialogInMemory
      IF lAutoSize
         _LabelApplyAutoSize( ControlName, ParentFormName, nIdx )
      ENDIF
      IF lBlink
         _LabelEnableBlink( ControlName, ParentFormName, nIdx )
      ENDIF
      IF .NOT. _HMG_BeginWindowActive
         _Refresh( nIdx )
      ENDIF
   ENDIF

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nIdx, cVar )
   ENDIF
   Do_ControlEventProcedure( bInit, nIdx )
RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogLabel( ParentFormName, hCtrl, nIdx )
*-----------------------------------------------------------------------------*
   LOCAL cName := _HMG_aControlNames[ nIdx ]
   IF _HMG_aControlSpacing[ nIdx ] == LABEL_AUTOSIZE_ENABLED
      _LabelApplyAutoSize( cName, ParentFormName, nIdx )
      RedrawWindow( hCtrl )
   ENDIF
   IF _HMG_aControlMiscData1[ nIdx ][ 2 ]
      _LabelEnableBlink( cName, ParentFormName, nIdx )
   ENDIF
   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[ 3 ]
      _HMG_aControlDeleted[ nIdx ] := .T.
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION OLABELEVENTS( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*
   LOCAL nIdx := AScan( _HMG_aControlHandles, hWnd )   // direct handle lookup
   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )
   IF nIdx > 0
      SWITCH nMsg
      CASE WM_MOUSEMOVE
         _DoControlEventProcedure( _HMG_aControlGotFocusProcedure[ nIdx ], nIdx )
         EXIT
      CASE WM_MOUSELEAVE
         _DoControlEventProcedure( _HMG_aControlLostFocusProcedure[ nIdx ], nIdx )
         EXIT
      ENDSWITCH
   ENDIF
RETURN 0

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _LabelApplyAutoSize( cName, cParent, nIdx )
*-----------------------------------------------------------------------------*
   LOCAL hFont := _HMG_aControlFontHandle[ nIdx ]
   LOCAL cCap  := _HMG_aControlCaption[ nIdx ]
   LOCAL nSize := _HMG_aControlFontSize[ nIdx ]
   LOCAL aAttr := _HMG_aControlFontAttributes[ nIdx ]
   LOCAL nW, nH

   nW := GetTextWidth( NIL, cCap, hFont )
   IF aAttr[1] .OR. aAttr[2]
      nW += GetTextWidth( NIL, " ", hFont )
   ENDIF
   nH := nSize + iif( nSize < LABEL_SMALL_FONT_LIMIT, LABEL_HEIGHT_PADDING_SMALL, LABEL_HEIGHT_PADDING_LARGE )

   _SetControlWidth( cName, cParent, nW )
   _SetControlHeight( cName, cParent, nH )
RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _LabelEnableBlink( cName, cParent, nIdx )
*-----------------------------------------------------------------------------*
   LOCAL aBlink := _HMG_aControlMiscData1[ nIdx ]
   _DefineTimer( 'BlinkTimer' + hb_ntos( nIdx ), cParent, LABEL_BLINK_INTERVAL_MS, ;
      {|| aBlink[3] := !aBlink[3], iif( aBlink[3], _ShowControl( cName, cParent ), _HideControl( cName, cParent ) ) } )
RETURN