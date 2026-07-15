/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2014-2026 Grigory Filatov <gfilatov@gmail.com>
 */

#include "minigui.ch"

/*----------------------------------------------------------------------*/
/* Internal helpers                                                     */
/*----------------------------------------------------------------------*/

STATIC FUNCTION _RatingImageName( cWindow, cControl, nIndex )
RETURN cWindow + "_" + cControl + "_" + hb_ntos( nIndex )

STATIC FUNCTION _ApplyRatingImage( cWindow, cControl, nIndex, nImagePos )
   LOCAL cImageName := _RatingImageName( cWindow, cControl, nIndex )
   LOCAL aImages := GetProperty( cWindow, cImageName, "Cargo" )
   SetProperty( cWindow, cImageName, "Picture", aImages[ nImagePos ] )
RETURN NIL

STATIC FUNCTION _PaintRating( cWindow, cControl, nSelected )
   LOCAL i
   ClearRating( cWindow, cControl )
   FOR i := 1 TO nSelected
      _ApplyRatingImage( cWindow, cControl, i, 2 )
   NEXT
RETURN NIL

/*----------------------------------------------------------------------*/
/* Rating control definition                                            */
/*----------------------------------------------------------------------*/

FUNCTION _DefineRating( ControlName, ParentForm, x, y, w, h, nValue, ;
      aImages, nCount, nSpace, cToolTip, bOnChange, lBorder, ;
      lResource, lReadOnly, lInvisible, lVertical )

   LOCAL cParentForm, cPublicVar, hControl

   DEFAULT h TO 20
   DEFAULT w TO 100
   DEFAULT nValue TO 0
   DEFAULT aImages TO { "empty.png", "full.png" }
   DEFAULT nCount TO 5
   DEFAULT lBorder TO .F.
   DEFAULT lReadOnly TO .F.
   DEFAULT lInvisible TO .F.
   DEFAULT lVertical TO .F.

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF ! _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF
   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   IF lResource
      aImages := { "_empty", "_full" }
   ENDIF

   cPublicVar := "_" + ParentForm + "_" + ControlName

#ifdef _NAMES_LIST_

   _SetNameList( cPublicVar, Len( _HMG_aControlNames ) + 1 )

#else

   PUBLIC &cPublicVar. := Len( _HMG_aControlNames ) + 1

#endif

   cParentForm := ParentForm
   ParentForm := GetFormHandle( ParentForm )

   hControl := _InitRating( cParentForm, ControlName, x, y, w, h, nValue, ;
      aImages, nCount, nSpace, cToolTip, bOnChange, lBorder, ;
      lReadOnly, lInvisible, lVertical )

   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
   ENDIF

   AAdd( _HMG_aControlType, "RATING" )
   AAdd( _HMG_aControlNames, ControlName )
   AAdd( _HMG_aControlHandles, hControl )
   AAdd( _HMG_aControlParentHandles, ParentForm )
   AAdd( _HMG_aControlIds, nCount )
   AAdd( _HMG_aControlProcedures, "" )
   AAdd( _HMG_aControlPageMap, {} )
   AAdd( _HMG_aControlValue, nValue )
   AAdd( _HMG_aControlInputMask, "" )
   AAdd( _HMG_aControllostFocusProcedure, "" )
   AAdd( _HMG_aControlGotFocusProcedure, "" )
   AAdd( _HMG_aControlChangeProcedure, bOnChange )
   AAdd( _HMG_aControlDeleted, .F. )
   AAdd( _HMG_aControlBkColor, {} )
   AAdd( _HMG_aControlFontColor, {} )
   AAdd( _HMG_aControlDblClick, "" )
   AAdd( _HMG_aControlHeadClick, {} )
   AAdd( _HMG_aControlRow, y )
   AAdd( _HMG_aControlCol, x )
   AAdd( _HMG_aControlWidth, w )
   AAdd( _HMG_aControlHeight, h )
   AAdd( _HMG_aControlSpacing, 0 )
   AAdd( _HMG_aControlContainerRow, iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 ) )
   AAdd( _HMG_aControlContainerCol, iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 ) )
   AAdd( _HMG_aControlPicture, "" )
   AAdd( _HMG_aControlContainerHandle, 0 )
   AAdd( _HMG_aControlFontName, "" )
   AAdd( _HMG_aControlFontSize, 0 )
   AAdd( _HMG_aControlFontAttributes, { .F., .F., .F., .F. } )
   AAdd( _HMG_aControlToolTip, cToolTip )
   AAdd( _HMG_aControlRangeMin, 0 )
   AAdd( _HMG_aControlRangeMax, 0 )
   AAdd( _HMG_aControlCaption, "" )
   AAdd( _HMG_aControlVisible, ! lInvisible )
   AAdd( _HMG_aControlHelpId, 0 )
   AAdd( _HMG_aControlFontHandle, 0 )
   AAdd( _HMG_aControlBrushHandle, 0 )
   AAdd( _HMG_aControlEnabled, .T. )
   AAdd( _HMG_aControlMiscData1, 0 )
   AAdd( _HMG_aControlMiscData2, "" )
RETURN NIL

/*----------------------------------------------------------------------*/

PROCEDURE _ReleaseRating( cWindow, cControl )
   LOCAL i, nCount, cImageName

   IF ! _IsControlDefined( cControl, cWindow )
      RETURN
   ENDIF

   nCount := GetControlId( cControl, cWindow )
   FOR i := 1 TO nCount
      cImageName := _RatingImageName( cWindow, cControl, i )
      DoMethod( cWindow, cImageName, "Release" )
   NEXT

   _ReleaseControl( cControl, cWindow )
   EraseWindow( cWindow )
RETURN

/*----------------------------------------------------------------------*/

FUNCTION _InitRating( ParentForm, ControlName, x, y, w, h, nValue, ;
      aImages, nCount, nSpace, cToolTip, bOnChange, lBorder, ;
      lReadOnly, lInvisible, lVertical )

   LOCAL i, cImageName, nCol := x, nRow := y, nIndex

   DEFAULT nSpace TO 0

   FOR i := 1 TO nCount
      cImageName := _RatingImageName( ParentForm, ControlName, i )

      DEFINE IMAGE ( cImageName )
         PARENT ( ParentForm )
         ROW y
         COL nCol
         WIDTH w
         HEIGHT h
         PICTURE aImages[ 1 ]
         TOOLTIP cToolTip
         ONMOUSEHOVER iif( lReadOnly, NIL, OnHoverRate( ParentForm, ControlName ) )
         ONMOUSELEAVE iif( lReadOnly, NIL, OnLeaveRate( ParentForm, ControlName, bOnChange ) )
         ONCLICK iif( lReadOnly, NIL, ;
            ( nIndex := Val( SubStr( This.NAME, RAt( "_", This.Name ) + 1 ) ), ;
            SetProperty( ParentForm, ControlName, "Value", nIndex ), ;
            OnSelectRate( ParentForm, ControlName, bOnChange ) ) )
         INVISIBLE lInvisible
      END IMAGE

      nIndex := GetControlIndex( cImageName, ParentForm )
      _HMG_aControlIds[ nIndex ] := nCount
      _HMG_aControlMiscData2[ nIndex ] := aImages
      _HMG_aControlChangeProcedure[ nIndex ] := bOnChange
      IF nValue > 0
         _HMG_aControlValue[ nIndex ] := nValue
      ENDIF

      IF lVertical
         y += h + nSpace
      ELSE
         nCol += w + nSpace
      ENDIF
   NEXT

   IF lBorder
      _DrawRatingBorder( ParentForm, lVertical, nRow, x, y, nCol, w, h, nSpace )
   ENDIF

   IF nValue > 0
      OnLeaveRate( ParentForm, ControlName, bOnChange )
   ENDIF
RETURN _GetId()

/*----------------------------------------------------------------------*/

STATIC PROCEDURE _DrawRatingBorder( cWindow, lVertical, nRow, x, y, nCol, w, h, nSpace )
   IF lVertical
      DRAW RECTANGLE IN WINDOW &cWindow AT nRow - 1, x - 1 TO y - nSpace + 1, nCol + w + 1 PENCOLOR { 192, 192, 192 }
   ELSE
      DRAW RECTANGLE IN WINDOW &cWindow AT nRow - 1, x - 1 TO nRow + h + 1, nCol - nSpace + 1 PENCOLOR { 192, 192, 192 }
   ENDIF
RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION OnHoverRate( cWindow, cControl )
   LOCAL nSelected := Val( SubStr( This.NAME, RAt( "_", This.Name ) + 1 ) )
   _PaintRating( cWindow, cControl, nSelected )
RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION OnLeaveRate( cWindow, cControl, bOnChange )
   LOCAL nSelected := GetProperty( cWindow, cControl, "Value" )
   IF nSelected == 0
      ClearRating( cWindow, cControl )
      IF ISBLOCK( bOnChange )
         Eval( bOnChange, nSelected )
      ENDIF
   ELSE
      OnSelectRate( cWindow, cControl, bOnChange )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION OnSelectRate( cWindow, cControl, bOnChange )
   LOCAL nSelected := GetProperty( cWindow, cControl, "Value" )
   IF nSelected <= 0
      RETURN NIL
   ENDIF
   _PaintRating( cWindow, cControl, nSelected )
   IF ISBLOCK( bOnChange )
      Eval( bOnChange, nSelected )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION ClearRating( cWindow, cControl )
   LOCAL i, nCount := GetControlId( cControl, cWindow )
   FOR i := 1 TO nCount
      _ApplyRatingImage( cWindow, cControl, i, 1 )
   NEXT
RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION RefreshRating( ParentForm, ControlName )
   LOCAL bOnChange := _GetControlAction( ControlName, ParentForm, "ONCHANGE" )
RETURN OnLeaveRate( ParentForm, ControlName, bOnChange )

/*----------------------------------------------------------------------*/

PROCEDURE ToggleRatingReadOnly( cWindow, cControl, lReadOnly )
   LOCAL i, k, nCount, cImageName
   k := GetControlIndex( cControl, cWindow )
   nCount := GetControlId( cControl, cWindow )
   FOR i := 1 TO nCount
      cImageName := _RatingImageName( cWindow, cControl, i )
      IF lReadOnly
         IF ! ISARRAY( _HMG_aControlSpacing[ k ] )
            _HMG_aControlSpacing[ k ] := { ;
               GetProperty( cWindow, cImageName, "ONGOTFOCUS" ), ;
               GetProperty( cWindow, cImageName, "ONLOSTFOCUS" ) }
         ENDIF
         SetProperty( cWindow, cImageName, "ONGOTFOCUS", {|| NIL } )
         SetProperty( cWindow, cImageName, "ONLOSTFOCUS", {|| NIL } )
      ELSE
         IF ISARRAY( _HMG_aControlSpacing[ k ] )
            SetProperty( cWindow, cImageName, "ONGOTFOCUS", _HMG_aControlSpacing[ k ][ 1 ] )
            SetProperty( cWindow, cImageName, "ONLOSTFOCUS", _HMG_aControlSpacing[ k ][ 2 ] )
         ENDIF
      ENDIF
   NEXT
RETURN
