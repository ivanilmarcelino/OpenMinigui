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

*-----------------------------------------------------------------------------*
FUNCTION _DefineImage( ControlName, ParentFormName, x, y, FileName, w, h, ;
      ProcedureName, tooltip, HelpId, invisible, stretch, aBKColor, ;
      transparent, adjustimage, mouseover, mouseleave, ;
      nAlphaLevel, nId, bInit, dblclick, rclick )
*-----------------------------------------------------------------------------*

   LOCAL nParentHandle
   LOCAL nControlHandle := 0
   LOCAL nControl
   LOCAL nStyle
   LOCAL nBkColor := -1

   LOCAL cVarName
   LOCAL lDialogInMemory
   LOCAL lCheckAlpha
   LOCAL lNotify
   LOCAL lMouseTracking
   LOCAL lAction := .F.

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   LOCAL bDlgInit

   LOCAL ow := NIL
   LOCAL oc := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /* Parent resolution */

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, ;
                            _HMG_ActiveDialogName, ;
                            _HMG_ActiveFormName )
   ENDIF

   /* Frame offset handling */

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
      nContainerRow  := _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      nContainerCol  := _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /* Validation */

   IF ! _IsWindowDefined( ParentFormName ) .AND. ! lDialogInMemory
      MsgMiniGuiError( ;
         "Window: " + IFNIL( ParentFormName, "Parent", ParentFormName ) + ;
         " is not defined." )
   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ! lDialogInMemory
      MsgMiniGuiError( ;
         "Control: " + ControlName + ;
         " Of " + ParentFormName + ;
         " Already defined." )
   ENDIF

   /* Defaults */

   hb_default( @w, 0 )
   hb_default( @h, 0 )

   hb_default( @stretch, .F. )
   hb_default( @transparent, .F. )
   hb_default( @adjustimage, .F. )

   __defaultNIL( @rclick, "" )

   w := IFEMPTY( w, -1, w )
   h := IFEMPTY( h, -1, h )

   lCheckAlpha := ISNUMERIC( nAlphaLevel )

   /* Action */

   IF ProcedureName == NIL
      ProcedureName := ""
   ELSE
      lAction := .T.
   ENDIF

   /* Background color */

   IF IsArrayRGB( aBKColor )
      nBkColor := RGB( aBKColor[1], aBKColor[2], aBKColor[3] )
   ENDIF

   /* Alpha validation */

   IF ISNUMERIC( nAlphaLevel ) .AND. ;
         ( nAlphaLevel < 0 .OR. nAlphaLevel > 255 )
      nAlphaLevel := 255
   ENDIF

   /* Notification flags */

   lNotify := ;
      lAction .OR. ;
      ISBLOCK( dblclick ) .OR. ;
      ISBLOCK( rclick ) .OR. ;
      ISSTRING( tooltip )

   lMouseTracking := ;
      ISBLOCK( mouseover ) .OR. ;
      ISBLOCK( mouseleave )

   /* Control allocation */

   cVarName := "_" + ParentFormName + "_" + ControlName
   nControl := _GetControlFree()

   /* Dialog mode */

   IF _HMG_BeginDialogActive

      nParentHandle := _HMG_ActiveDialogHandle
      nStyle := WS_CHILD + SS_BITMAP

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lNotify
         nStyle += SS_NOTIFY
      ENDIF

      IF lDialogInMemory

         bDlgInit := {|a, b, c| InitDialogImage( a, b, c )}

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nControl, ;
               "static", ;
               nStyle, ;
               0, ;
               x, ;
               y, ;
               w, ;
               h, ;
               "", ;
               HelpId, ;
               "", ;
               "", ;
               , ;
               , ;
               , ;
               , ;
               , ;
               bDlgInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         nControlHandle := GetDialogItemHandle( nParentHandle, nId )
         x := GetWindowCol( nControlHandle )
         y := GetWindowRow( nControlHandle )
         w := GetWindowWidth( nControlHandle )
         h := GetWindowHeight( nControlHandle )
         SetWindowStyle( nControlHandle, nStyle, .T. )

      ENDIF

   ELSE

      nParentHandle := GetFormHandle( ParentFormName )

      nControlHandle := InitImage( ;
         nParentHandle, ;
         0, ;
         x, ;
         y, ;
         invisible, ;
         lNotify, ;
         lMouseTracking )

   ENDIF

   /* Runtime initialization */

   IF ! lDialogInMemory

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, nControlHandle )
      ENDIF

      IF tooltip != NIL
         SetToolTip( ;
            nControlHandle, ;
            tooltip, ;
            GetFormToolTipHandle( ParentFormName ) )
      ENDIF

   ENDIF

   /* Register variable */

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /* Identity */

   _HMG_aControlType[ nControl ]          := "IMAGE"
   _HMG_aControlNames[ nControl ]         := ControlName
   _HMG_aControlHandles[ nControl ]       := nControlHandle
   _HMG_aControlParentHandles[ nControl ]  := nParentHandle
   _HMG_aControlIds[ nControl ]           := nId

   /* Events */

   _HMG_aControlProcedures[ nControl ]         := ProcedureName
   _HMG_aControllostFocusProcedure[ nControl ] := mouseleave
   _HMG_aControlGotFocusProcedure[ nControl ]  := mouseover
   _HMG_aControlChangeProcedure[ nControl ]    := rclick

   /*
      MiniGUI internal reuse:
         _HMG_aControlHeadClick -> dblclick
         _HMG_aControlDblClick  -> alpha flag
   */

   _HMG_aControlHeadClick[ nControl ] := dblclick
   _HMG_aControlDblClick[ nControl ]  := lCheckAlpha

   /* State */

   _HMG_aControlPageMap[ nControl ]  := {}
   _HMG_aControlDeleted[ nControl ]   := .F.
   _HMG_aControlEnabled[ nControl ]   := .T.
   _HMG_aControlVisible[ nControl ]   := ! invisible

   /* Geometry */

   _HMG_aControlRow[ nControl ]      := y
   _HMG_aControlCol[ nControl ]      := x
   _HMG_aControlWidth[ nControl ]    := w
   _HMG_aControlHeight[ nControl ]   := h
   _HMG_aControlContainerRow[ nControl ]    := nContainerRow
   _HMG_aControlContainerCol[ nControl ]    := nContainerCol
   _HMG_aControlContainerHandle[ nControl ] := 0

   /* Image configuration */

   _HMG_aControlPicture[ nControl ]   := FileName
   _HMG_aControlValue[ nControl ]     := iif( stretch, 1, 0 )
   _HMG_aControlInputMask[ nControl ] := iif( transparent, 1, 0 )
   _HMG_aControlCaption[ nControl ]   := iif( adjustimage, 1, 0 )
   _HMG_aControlSpacing[ nControl ]   := nBkColor
   _HMG_aControlRangeMin[ nControl ]  := w
   _HMG_aControlRangeMax[ nControl ]  := h

   /* Appearance */

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL
   _HMG_aControlFontName[ nControl ]  := ""
   _HMG_aControlFontSize[ nControl ]  := 0
   _HMG_aControlFontAttributes[ nControl ] := { .F., .F., .F., .F. }
   _HMG_aControlToolTip[ nControl ]   := tooltip
   _HMG_aControlFontHandle[ nControl ] := 0
   _HMG_aControlBrushHandle[ nControl ] := 0

   /* Misc */

   _HMG_aControlHelpId[ nControl ]    := HelpId
   _HMG_aControlMiscData1[ nControl ] := nAlphaLevel
   _HMG_aControlMiscData2[ nControl ] := ""

   /* Runtime image setup */

   IF ! lDialogInMemory
      InitDialogImage( ParentFormName, nControlHandle, nControl )
   ENDIF

   /* OOP integration */

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, nControl, cVarName )

#ifdef _OBJECT_
      ow := _WindowObj( nParentHandle )
      oc := _ControlObj( nControlHandle )
#endif

   ENDIF

   /* Initial event */

   Do_ControlEventProcedure( bInit, nControl, ow, oc )

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION InitDialogImage( ParentName, ControlHandle, nControl )
*-----------------------------------------------------------------------------*

   IF ValType( ParentName ) != "U"

      _HMG_aControlBrushHandle[ nControl ] := ;
         C_SetPicture( ;
            ControlHandle, ;
            _HMG_aControlPicture[ nControl ], ;
            _HMG_aControlWidth[ nControl ], ;
            _HMG_aControlHeight[ nControl ], ;
            _HMG_aControlValue[ nControl ], ;
            _HMG_aControlInputMask[ nControl ], ;
            _HMG_aControlSpacing[ nControl ], ;
            _HMG_aControlCaption[ nControl ], ;
            _HMG_aControlDblClick[ nControl ] .AND. ;
               HasAlpha( _HMG_aControlPicture[ nControl ] ), ;
            _HMG_aControlMiscData1[ nControl ] )

      IF Empty( _HMG_aControlValue[ nControl ] )
         _HMG_aControlWidth[ nControl ]  := GetWindowWidth( ControlHandle )
         _HMG_aControlHeight[ nControl ] := GetWindowHeight( ControlHandle )
      ENDIF

   ENDIF

   /* Modal dialog cleanup */

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[3]
      _HMG_aControlDeleted[ nControl ] := .T.
   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION BmpSize( xBitmap )
*-----------------------------------------------------------------------------*

   LOCAL aRet := { 0, 0, 4 }

   IF ISSTRING( xBitmap )

      aRet := GetBitmapSize( xBitmap )

      IF Empty( aRet[1] ) .AND. Empty( aRet[2] )
         xBitmap := C_GetResPicture( xBitmap )
         aRet := GetBitmapSize( xBitmap )
         DeleteObject( xBitmap )
      ENDIF

   ELSEIF ISNUMERIC( xBitmap )

      aRet := GetBitmapSize( xBitmap )

   ENDIF

RETURN aRet


*-----------------------------------------------------------------------------*
FUNCTION HasAlpha( FileName )
*-----------------------------------------------------------------------------*

   LOCAL hBitmap
   LOCAL lResult := .F.

   hBitmap := C_GetResPicture( FileName )

   IF GetObjectType( hBitmap ) == OBJ_BITMAP

      IF BmpSize( FileName )[3] == 32
         lResult := C_HasAlpha( hBitmap )
      ENDIF

      DeleteObject( hBitmap )

   ENDIF

RETURN lResult


*-----------------------------------------------------------------------------*
FUNCTION HMG_SaveImage( FileName, cOutName, cEncoder, nJpgQuality, aOutSize )
*-----------------------------------------------------------------------------*

   LOCAL hBitmap
   LOCAL lResult := .F.

   hBitmap := iif( ISSTRING( FileName ), ;
                   C_GetResPicture( FileName ), ;
                   FileName )

   IF GetObjectType( hBitmap ) == OBJ_BITMAP

      hb_default( @cEncoder, "BMP" )
      hb_default( @nJpgQuality, 100 )
      __defaultNIL( @aOutSize, BmpSize( hBitmap ) )

      lResult := C_SaveHBitmapToFile( ;
         hBitmap, ;
         cOutName, ;
         aOutSize[1], ;
         aOutSize[2], ;
         "image/" + Lower( cEncoder ), ;
         nJpgQuality )

      IF ISSTRING( FileName )
         DeleteObject( hBitmap )
      ENDIF

   ENDIF

RETURN lResult
