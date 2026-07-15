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
STATIC FUNCTION _DefineFrame( ControlName, ParentFormName, ;
      x, y, w, h, caption, FontName, FontSize, opaque, ;
      bold, italic, underline, strikeout, backcolor, ;
      fontcolor, transparent, invisible, nId, bInit )
*-----------------------------------------------------------------------------*

   LOCAL hParent
   LOCAL hControl
   LOCAL hFont

   LOCAL cVarName

   LOCAL nControl
   LOCAL nStyle

   LOCAL lDialogInMemory

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   LOCAL cRangeMin := ""
   LOCAL cRangeMax := ""

   LOCAL ow := NIL
   LOCAL oc := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /* Resolve predefined font */

   IF ( hFont := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         hFont, ;
         @FontName, ;
         @FontSize, ;
         @bold, ;
         @italic, ;
         @underline, ;
         @strikeout )

   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /* Validation */

   IF !_IsWindowDefined( ParentFormName ) .AND. ;
         ! lDialogInMemory

      MsgMiniGuiError( ;
         "Window: " + ;
         IFNIL( ParentFormName, "Parent", ParentFormName ) + ;
         " is not defined." )

   ENDIF

   IF ISCHAR( ControlName ) .AND. ;
         ControlName == "0"

      ControlName := HMG_GetUniqueName()

   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ;
         ! lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentFormName + ;
         " Already defined." )

   ENDIF

   cVarName := "_" + ParentFormName + "_" + ControlName

   nControl := _GetControlFree()

   /* Frame container metadata */

   IF _HMG_FrameLevel > 0

      nContainerRow := ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      nContainerCol := ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

      cRangeMin := _HMG_ActiveTabName

      cRangeMax := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   /* Dialog mode */

   IF _HMG_BeginDialogActive

      hParent := _HMG_ActiveDialogHandle

      nStyle := WS_CHILD + BS_GROUPBOX + BS_NOTIFY

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lDialogInMemory

         /*
            Dialog item structure:

            { ;
               ID, k/hWnd, class, style, exstyle, ;
               x, y, w, h, caption, helpid, tooltip, ;
               font, size, bold, italic, underline, strikeout, ;
               ..., tabactive, ..., tabpage ;
            }
         */

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nControl, ;
               "button", ;
               nStyle, ;
               0, ;
               x, ;
               y, ;
               w, ;
               h, ;
               caption, ;
               , ;
               , ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout, ;
               , ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

         IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
               _HMG_aDialogTemplate[3]

            _HMG_aControlDeleted[ nControl ] := .T.

            RETURN NIL

         ENDIF

      ELSE

         hControl := ;
            GetDialogItemHandle( hParent, nId )

         x := GetWindowCol( hControl )
         y := GetWindowRow( hControl )

         w := GetWindowWidth( hControl )
         h := GetWindowHeight( hControl )

         IF caption != NIL
            SetWindowText( hControl, caption )
         ENDIF

         SetWindowStyle( hControl, nStyle, .T. )

      ENDIF

   ELSE

      hParent := GetFormHandle( ParentFormName )

      hControl := ;
         InitFrame( ;
            hParent, ;
            0, ;
            x, ;
            y, ;
            w, ;
            h, ;
            caption, ;
            "", ;
            0, ;
            opaque )

   ENDIF

   /* Runtime initialization */

   IF ! lDialogInMemory

      IF hFont != 0

         _SetFontHandle( hControl, hFont )

      ELSE

         __defaultNIL( @FontName, _HMG_DefaultFontName )
         __defaultNIL( @FontSize, _HMG_DefaultFontSize )

         hFont := ;
            _SetFont( ;
               hControl, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout )

      ENDIF

      /* Theme handling */

      IF _HMG_IsThemed .AND. ;
            ( IsArrayRGB( backcolor ) .OR. ;
              IsArrayRGB( fontcolor ) )

         SetWindowTheme( hControl, "", "" )

      ENDIF

      /* Tab integration */

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
      ENDIF

   ENDIF

   /* Register name */

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /* Identity */

   _HMG_aControlType[ nControl ]          := "FRAME"
   _HMG_aControlNames[ nControl ]         := ControlName

   _HMG_aControlHandles[ nControl ]       := hControl
   _HMG_aControlParentHandles[ nControl ] := hParent

   _HMG_aControlIds[ nControl ]           := nId

   /* Events */

   _HMG_aControlProcedures[ nControl ]         := ""
   _HMG_aControlLostFocusProcedure[ nControl ] := ""
   _HMG_aControlGotFocusProcedure[ nControl ]  := ""
   _HMG_aControlChangeProcedure[ nControl ]    := ""

   /* State */

   _HMG_aControlPageMap[ nControl ] := {}
   _HMG_aControlValue[ nControl ]   := NIL

   /*
      InputMask stores FRAME transparency state.
   */

   _HMG_aControlInputMask[ nControl ] := transparent

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.

   /*
      Existing MiniGUI behavior preserved:
      visibility handled later by _HideControl().
   */

   _HMG_aControlVisible[ nControl ] := .T.

   /* Appearance */

   _HMG_aControlBkColor[ nControl ]   := backcolor
   _HMG_aControlFontColor[ nControl ] := fontcolor

   _HMG_aControlPicture[ nControl ] := ""

   _HMG_aControlFontName[ nControl ] := FontName
   _HMG_aControlFontSize[ nControl ] := FontSize

   _HMG_aControlFontAttributes[ nControl ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ nControl ] := ""
   _HMG_aControlCaption[ nControl ] := caption

   _HMG_aControlFontHandle[ nControl ]  := hFont
   _HMG_aControlBrushHandle[ nControl ] := 0

   /* Geometry */

   _HMG_aControlRow[ nControl ]    := y
   _HMG_aControlCol[ nControl ]    := x

   _HMG_aControlWidth[ nControl ]  := w
   _HMG_aControlHeight[ nControl ] := h

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ] := ;
      nContainerRow

   _HMG_aControlContainerCol[ nControl ] := ;
      nContainerCol

   _HMG_aControlContainerHandle[ nControl ] := 0

   /* Internal state */

   /*
      DblClick slot internally reused for:
      - active tab buttons
      - opaque state
   */

   _HMG_aControlDblClick[ nControl ] := ;
      _HMG_ActiveTabButtons .OR. opaque

   _HMG_aControlHeadClick[ nControl ] := {}

   _HMG_aControlRangeMin[ nControl ] := cRangeMin
   _HMG_aControlRangeMax[ nControl ] := cRangeMax

   _HMG_aControlHelpId[ nControl ] := 0

   _HMG_aControlMiscData1[ nControl ] := 0
   _HMG_aControlMiscData2[ nControl ] := ""

   /* Visibility */

   IF invisible
      _HideControl( ControlName, ParentFormName )
   ENDIF

   /* OOP integration */

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, nControl, cVarName )

#ifdef _OBJECT_

      ow := _WindowObj( hParent )
      oc := _ControlObj( hControl )

#endif

   ENDIF

   /* Init callback */

   Do_ControlEventProcedure( bInit, nControl, ow, oc )

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION _BeginFrame( name, parent, row, col, w, h, ;
      caption, FontName, FontSize, opaque, ;
      bold, italic, underline, strikeout, ;
      backcolor, fontcolor, transparent, ;
      invisible, nId, bInit )
*-----------------------------------------------------------------------------*

   /* Active form defaults */

   IF _HMG_BeginWindowActive

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /* Nested frame offsets */

   IF _HMG_FrameLevel > 0 .AND. ;
         !_HMG_ParentWindowActive

      col += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      row += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      parent := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   /* Parent resolution */

   IF parent == NIL

      IF _HMG_BeginWindowActive .OR. ;
            _HMG_BeginDialogActive

         parent := ;
            iif( _HMG_BeginDialogActive, ;
                 _HMG_ActiveDialogName, ;
                 _HMG_ActiveFormName )

      ENDIF

   ENDIF

   /* Defaults */

   hb_default( @caption, "" )

   IF Empty( caption )

      /*
         Invisible caption workaround.
      */

      FontName := "Arial"
      FontSize := 1

   ENDIF

   hb_default( @w, 140 )
   hb_default( @h, 140 )

   /* Create frame */

   _DefineFrame( ;
      name, ;
      parent, ;
      col, ;
      row, ;
      w, ;
      h, ;
      caption, ;
      FontName, ;
      FontSize, ;
      opaque, ;
      bold, ;
      italic, ;
      underline, ;
      strikeout, ;
      backcolor, ;
      fontcolor, ;
      transparent, ;
      invisible, ;
      nId, ;
      bInit )

RETURN NIL