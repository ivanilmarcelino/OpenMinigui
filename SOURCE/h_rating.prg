/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2014-2025 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

*-----------------------------------------------------------------------------*
FUNCTION _DefineRating ( ControlName, ParentForm, x, y, w, h, nValue, aImages, nCnt, ;
      nSpace, tooltip, onchangeprocedure, border, resource, readonly, invisible, vertical )
*-----------------------------------------------------------------------------*
   LOCAL cParentForm, mVar, ControlHandle

   DEFAULT h TO 20
   DEFAULT w TO 100
   DEFAULT nValue TO 0
   DEFAULT aImages := { 'empty.png', 'full.png' }
   DEFAULT nCnt TO 5
   DEFAULT border TO FALSE
   DEFAULT readonly TO FALSE
   DEFAULT invisible TO FALSE
   DEFAULT vertical TO FALSE

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF .NOT. _IsWindowDefined ( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined ( ControlName, ParentForm )
      MsgMiniGuiError ( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   IF resource
      aImages [1] := '_empty'
      aImages [2] := '_full'
   ENDIF

   mVar := '_' + ParentForm + '_' + ControlName
#ifdef _NAMES_LIST_
   _SetNameList( mVar , Len( _HMG_aControlNames ) + 1 )
#else
   Public &mVar. := Len( _HMG_aControlNames ) + 1
#endif
   cParentForm := ParentForm

   ParentForm := GetFormHandle ( ParentForm )

   ControlHandle := _InitRating ( cParentForm, ControlName, x, y, w, h, nValue, aImages, nCnt, nSpace, tooltip, onchangeprocedure, border, readonly, invisible, vertical )

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, Controlhandle )
   ENDIF

   AAdd ( _HMG_aControlType, "RATING" )
   AAdd ( _HMG_aControlNames, ControlName )
   AAdd ( _HMG_aControlHandles, ControlHandle )
   AAdd ( _HMG_aControlParentHandles, ParentForm )
   AAdd ( _HMG_aControlIds, nCnt )
   AAdd ( _HMG_aControlProcedures, "" )
   AAdd ( _HMG_aControlPageMap, {} )
   AAdd ( _HMG_aControlValue, nValue )
   AAdd ( _HMG_aControlInputMask, "" )
   AAdd ( _HMG_aControllostFocusProcedure, "" )
   AAdd ( _HMG_aControlGotFocusProcedure, "" )
   AAdd ( _HMG_aControlChangeProcedure, onchangeprocedure )
   AAdd ( _HMG_aControlDeleted, .F. )
   AAdd ( _HMG_aControlBkColor, {} )
   AAdd ( _HMG_aControlFontColor, {} )
   AAdd ( _HMG_aControlDblClick, "" )
   AAdd ( _HMG_aControlHeadClick, {} )
   AAdd ( _HMG_aControlRow, y )
   AAdd ( _HMG_aControlCol, x )
   AAdd ( _HMG_aControlWidth, w )
   AAdd ( _HMG_aControlHeight, h )
   AAdd ( _HMG_aControlSpacing, 0 )
   AAdd ( _HMG_aControlContainerRow, iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlContainerCol, iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlPicture, "" )
   AAdd ( _HMG_aControlContainerHandle, 0 )
   AAdd ( _HMG_aControlFontName, '' )
   AAdd ( _HMG_aControlFontSize, 0 )
   AAdd ( _HMG_aControlFontAttributes, { FALSE, FALSE, FALSE, FALSE } )
   AAdd ( _HMG_aControlToolTip, tooltip )
   AAdd ( _HMG_aControlRangeMin, 0 )
   AAdd ( _HMG_aControlRangeMax, 0 )
   AAdd ( _HMG_aControlCaption, '' )
   AAdd ( _HMG_aControlVisible, if( invisible, FALSE, TRUE ) )
   AAdd ( _HMG_aControlHelpId, 0 )
   AAdd ( _HMG_aControlFontHandle, 0 )
   AAdd ( _HMG_aControlBrushHandle, 0 )
   AAdd ( _HMG_aControlEnabled, .T. )
   AAdd ( _HMG_aControlMiscData1, 0 )
   AAdd ( _HMG_aControlMiscData2, '' )

RETURN NIL

*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseRating ( cWindow, cControl )
*-----------------------------------------------------------------------------*
   LOCAL i, img_name
   LOCAL nCount

   IF _IsControlDefined ( cControl, cWindow )

      nCount := GetControlId ( cControl, cWindow )

      FOR i := 1 TO nCount
         img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
         DoMethod( cWindow, img_name, 'Release' )
      NEXT

      _ReleaseControl ( cControl, cWindow )
      EraseWindow( cWindow )

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _InitRating ( ParentForm, ControlName, x, y, w, h, nValue, aImages, nCnt, ;
      nSpace, tooltip, onchangeprocedure, border, readonly, invisible, vertical )
*-----------------------------------------------------------------------------*
   LOCAL i, img_name, col := x, row := y

   DEFAULT nSpace := 0

   FOR i := 1 TO nCnt
      img_name := ParentForm + "_" + ControlName + "_" + hb_ntos( i )
      DEFINE IMAGE ( img_name )
         PARENT ( ParentForm )
         ROW y
         COL col
         WIDTH w
         HEIGHT h
         PICTURE aImages [1]
         TOOLTIP tooltip
         ONMOUSEHOVER iif( readonly, NIL, OnHoverRate( ParentForm, ControlName ) )
         ONMOUSELEAVE iif( readonly, NIL, OnLeaveRate( ParentForm, ControlName, onchangeprocedure ) )
         ONCLICK iif( readonly, NIL, ( SetProperty( ParentForm, ControlName, 'Value', ;
            Val( SubStr( This.Name, RAt( '_', This.Name ) + 1 ) ) ), ;
            OnSelectRate( ParentForm, ControlName, onchangeprocedure ) ) )
         INVISIBLE invisible
      END IMAGE
      _HMG_aControlIds[ GetControlIndex( img_name, ParentForm ) ] := nCnt
      _HMG_aControlMiscData2[ GetControlIndex( img_name, ParentForm ) ] := aImages
      _HMG_aControlChangeProcedure[ GetControlIndex( img_name, ParentForm ) ] := onchangeprocedure
      IF vertical
         y += h + nSpace
      ELSE
         col += w + nSpace
      ENDIF
      IF nValue > 0
         _HMG_aControlValue[ GetControlIndex( img_name, ParentForm ) ] := nValue
      ENDIF
   NEXT

   IF border
      IF vertical
         DRAW RECTANGLE ;
            IN WINDOW Win_1 ;
            AT row - 1, x - 1 ;
            TO y - nSpace + 1, col + w + 1 ;
            PENCOLOR { 192, 192, 192 }
      ELSE
         DRAW RECTANGLE ;
            IN WINDOW Win_1 ;
            AT row - 1, x - 1 ;
            TO row + h + 1, col - nSpace + 1 ;
            PENCOLOR { 192, 192, 192 }
      ENDIF
   ENDIF

   IF nValue > 0
      OnLeaveRate( ParentForm, ControlName, onchangeprocedure )
   ENDIF

RETURN _GetId()


STATIC FUNCTION OnHoverRate( cWindow, cControl )

   LOCAL i, img_name
   LOCAL select := Val( SubStr( This.Name, RAt( '_', This.Name ) + 1 ) )

   ClearRating( cWindow, cControl )
   FOR i := 1 TO select
      img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
      SetProperty( cWindow, img_name, 'Picture', GetProperty( cWindow, img_name, 'Cargo' ) [2] )
   NEXT

RETURN NIL


STATIC FUNCTION OnLeaveRate( cWindow, cControl, onchange )

   LOCAL pressed := GetProperty( cWindow, cControl, 'Value' )

   IF pressed == 0
      ClearRating( cWindow, cControl )
      IF ISBLOCK( onchange )
         Eval( onchange, pressed )
      ENDIF
   ELSE
      OnSelectRate( cWindow, cControl, onchange )
   ENDIF

RETURN NIL


STATIC FUNCTION OnSelectRate( cWindow, cControl, onchange )

   LOCAL i, img_name
   LOCAL pressed := GetProperty( cWindow, cControl, 'Value' )

   IF pressed > 0
      ClearRating( cWindow, cControl )
      FOR i := 1 TO pressed
         img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
         SetProperty( cWindow, img_name, 'Picture', GetProperty( cWindow, img_name, 'Cargo' ) [2] )
      NEXT
      IF ISBLOCK( onchange )
         Eval( onchange, pressed )
      ENDIF
   ENDIF

RETURN NIL


FUNCTION ClearRating( cWindow, cControl )

   LOCAL i, img_name
   LOCAL nCount := GetControlId ( cControl, cWindow )

   FOR i := 1 TO nCount
      img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
      SetProperty( cWindow, img_name, 'Picture', GetProperty( cWindow, img_name, 'Cargo' ) [1] )
   NEXT

RETURN NIL


FUNCTION RefreshRating( ParentForm, ControlName )

   LOCAL onchangeprocedure := _GetControlAction( ControlName, ParentForm, 'ONCHANGE' )

RETURN OnLeaveRate( ParentForm, ControlName, onchangeprocedure )


PROCEDURE ToggleRatingReadOnly( cWindow, cControl, lReadOnly )

   LOCAL i, img_name
   LOCAL k := GetControlIndex( cControl, cWindow )
   LOCAL nCount := GetControlId( cControl, cWindow )

   IF lReadOnly
      // Disable interactions
      FOR i := 1 TO nCount
         img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
         IF ! ISARRAY( _HMG_aControlSpacing [k] )
            _HMG_aControlSpacing [k] := { GetProperty( cWindow, img_name, 'ONGOTFOCUS' ), GetProperty( cWindow, img_name, 'ONLOSTFOCUS' ) }
         ENDIF
         SetProperty( cWindow, img_name, 'ONGOTFOCUS', {|| NIL } )
         SetProperty( cWindow, img_name, 'ONLOSTFOCUS', {|| NIL } )
      NEXT
   ELSE
      // Restore interactions
      FOR i := 1 TO nCount
         img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
         IF ISARRAY( _HMG_aControlSpacing [k] )
            SetProperty( cWindow, img_name, 'ONGOTFOCUS', _HMG_aControlSpacing [k][1] )
            SetProperty( cWindow, img_name, 'ONLOSTFOCUS', _HMG_aControlSpacing [k][2] )
         ENDIF
      NEXT
   ENDIF

RETURN
