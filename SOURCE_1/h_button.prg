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

#include "i_winuser.ch"
#include "minigui.ch"

*-----------------------------------------------------------------------------*
FUNCTION _DefineButton( ControlName, ParentFormName, x, y, Caption, ;
      ProcedureName, w, h, FontName, FontSize, tooltip, ;
      gotfocus, lostfocus, flat, NoTabStop, HelpId, invisible, ;
      bold, italic, underline, strikeout, multiline, ;
      lDefault, key, nId )
*-----------------------------------------------------------------------------*

   LOCAL nParentHandle
   LOCAL nControlHandle := 0
   LOCAL nFontHandle
   LOCAL nControl
   LOCAL nStyle

   LOCAL cVarName
   LOCAL bDlgInit

   LOCAL lDialogInMemory

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   /* Defaults */

   hb_default( @w, 100 )
   hb_default( @h, 28 )

   hb_default( @flat, .F. )
   hb_default( @NoTabStop, .F. )
   hb_default( @invisible, .F. )

   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   /* Resolve predefined font */

   IF ( nFontHandle := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         nFontHandle, ;
         @FontName, ;
         @FontSize, ;
         @bold, ;
         @italic, ;
         @underline, ;
         @strikeout )

   ENDIF

   /* Active parent */

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := ;
         iif( _HMG_BeginDialogActive, ;
              _HMG_ActiveDialogName, ;
              _HMG_ActiveFormName )

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /* Frame offsets */

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

      nContainerRow := ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      nContainerCol := ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

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

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
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

   /* Allocation */

   cVarName := "_" + ParentFormName + "_" + ControlName
   nControl := _GetControlFree()

   /* Dialog mode */

   IF _HMG_BeginDialogActive

      nParentHandle := _HMG_ActiveDialogHandle

      nStyle := ;
         BS_NOTIFY + ;
         WS_CHILD + ;
         BS_PUSHBUTTON

      IF flat
         nStyle += BS_FLAT
      ENDIF

      IF ! NoTabStop
         nStyle += WS_TABSTOP
      ENDIF

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lDialogInMemory

         bDlgInit := ;
            {|a, b, c| InitDialogButtonImage( a, b, c ) }

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
               Caption, ;
               HelpId, ;
               tooltip, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout, ;
               bDlgInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         nControlHandle := ;
            GetDialogItemHandle( nParentHandle, nId )

         x := GetWindowCol( nControlHandle )
         y := GetWindowRow( nControlHandle )

         w := GetWindowWidth( nControlHandle )
         h := GetWindowHeight( nControlHandle )

         IF nFontHandle != 0

            _SetFontHandle( nControlHandle, nFontHandle )

         ELSE

            __defaultNIL( @FontName, _HMG_DefaultFontName )
            __defaultNIL( @FontSize, _HMG_DefaultFontSize )

            nFontHandle := ;
               _SetFont( ;
                  nControlHandle, ;
                  FontName, ;
                  FontSize, ;
                  bold, ;
                  italic, ;
                  underline, ;
                  strikeout )

         ENDIF

         IF Caption != NIL
            SetWindowText( nControlHandle, Caption )
         ENDIF

         SetWindowStyle( nControlHandle, nStyle, .T. )

      ENDIF

   ELSE

      nParentHandle := GetFormHandle( ParentFormName )

      nControlHandle := ;
         InitButton( ;
            nParentHandle, ;
            Caption, ;
            0, ;
            x, ;
            y, ;
            w, ;
            h, ;
            "", ;
            0, ;
            flat, ;
            NoTabStop, ;
            invisible, ;
            multiline, ;
            lDefault )

   ENDIF

   /* Runtime font */

   IF ! lDialogInMemory

      IF nFontHandle != 0

         _SetFontHandle( nControlHandle, nFontHandle )

      ELSE

         __defaultNIL( @FontName, _HMG_DefaultFontName )
         __defaultNIL( @FontSize, _HMG_DefaultFontSize )

         nFontHandle := ;
            _SetFont( ;
               nControlHandle, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout )

      ENDIF

   ENDIF

   /* Register name */

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /* Identity */

   _HMG_aControlType[ nControl ]          := "BUTTON"
   _HMG_aControlNames[ nControl ]         := ControlName
   _HMG_aControlHandles[ nControl ]       := nControlHandle
   _HMG_aControlParentHandles[ nControl ] := nParentHandle
   _HMG_aControlIds[ nControl ]           := nId

   /* Events */

   _HMG_aControlProcedures[ nControl ]         := ProcedureName
   _HMG_aControllostFocusProcedure[ nControl ] := lostfocus
   _HMG_aControlGotFocusProcedure[ nControl ]  := gotfocus
   _HMG_aControlChangeProcedure[ nControl ]    := ""

   /* State */

   _HMG_aControlPageMap[ nControl ] := {}
   _HMG_aControlValue[ nControl ]   := NIL

   _HMG_aControlInputMask[ nControl ] := ;
      iif( ISCHARACTER( key ), key, "" )

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := ! invisible

   /* Geometry */

   _HMG_aControlRow[ nControl ]    := y
   _HMG_aControlCol[ nControl ]    := x

   _HMG_aControlWidth[ nControl ]  := w
   _HMG_aControlHeight[ nControl ] := h

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ]    := nContainerRow
   _HMG_aControlContainerCol[ nControl ]    := nContainerCol
   _HMG_aControlContainerHandle[ nControl ] := 0

   /* Appearance */

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL

   _HMG_aControlPicture[ nControl ] := ""

   _HMG_aControlFontName[ nControl ] := FontName
   _HMG_aControlFontSize[ nControl ] := FontSize

   _HMG_aControlFontAttributes[ nControl ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ nControl ] := tooltip
   _HMG_aControlCaption[ nControl ] := Caption

   _HMG_aControlFontHandle[ nControl ]  := nFontHandle
   _HMG_aControlBrushHandle[ nControl ] := 0

   /* Misc */

   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0

   _HMG_aControlHelpId[ nControl ] := HelpId

   _HMG_aControlDblClick[ nControl ] := ""
   _HMG_aControlHeadClick[ nControl ] := {}

   _HMG_aControlMiscData1[ nControl ] := 0
   _HMG_aControlMiscData2[ nControl ] := ""

   /* OOP */

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nControl, cVarName )
   ENDIF

   /* Runtime init */

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

      _SetHotKeyByName( ;
         ParentFormName, ;
         key, ;
         ProcedureName )

   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION _DefineImageButton( ControlName, ParentFormName, x, y, Caption, ;
      ProcedureName, w, h, image, tooltip, gotfocus, lostfocus, ;
      flat, notrans, HelpId, invisible, notabstop, lDefault, ;
      icon, extract, nIdx, noxpstyle, key, nId )
*-----------------------------------------------------------------------------*

   LOCAL nParentHandle
   LOCAL nControlHandle := 0
   LOCAL nControl
   LOCAL nStyle
   LOCAL nhImage := 0

   LOCAL aRet
   LOCAL cVarName
   LOCAL cPicture
   LOCAL bDlgInit

   LOCAL lDialogInMemory

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   /* Defaults */

   hb_default( @flat, .F. )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )
   hb_default( @noxpstyle, .F. )
   hb_default( @nIdx, 0 )

   /* Toolbar protection */

   IF _HMG_ToolBarActive
      RETURN NIL
   ENDIF

   /* Active parent */

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := ;
         iif( _HMG_BeginDialogActive, ;
              _HMG_ActiveDialogName, ;
              _HMG_ActiveFormName )

   ENDIF

   /* Frame offsets */

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

      nContainerRow := ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      nContainerCol := ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

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

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ;
         ! lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentFormName + ;
         " Already defined." )

   ENDIF

   IF ! Empty( image ) .AND. ! Empty( icon )

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentFormName + ;
         ". Either bitmap or icon must be specified." )

   ENDIF

   /* Allocation */

   cVarName := "_" + ParentFormName + "_" + ControlName

   nControl := _GetControlFree()

   cPicture := IFEMPTY( icon, image, icon )

   IF ISARRAY( cPicture )
      image := cPicture[1]
   ENDIF

   /* Dialog mode */

   IF _HMG_BeginDialogActive

      nParentHandle := _HMG_ActiveDialogHandle

      nStyle := ;
         BS_NOTIFY + ;
         BS_BITMAP + ;
         WS_CHILD + ;
         BS_PUSHBUTTON

      IF flat
         nStyle += BS_FLAT
      ENDIF

      IF ! notabstop
         nStyle += WS_TABSTOP
      ENDIF

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lDialogInMemory

         bDlgInit := ;
            {|a, b, c| InitDialogButtonImage( a, b, c ) }

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
               Caption, ;
               HelpId, ;
               tooltip, ;
               , ;
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

         nControlHandle := ;
            GetDialogItemHandle( nParentHandle, nId )

         x := GetWindowCol( nControlHandle )
         y := GetWindowRow( nControlHandle )

         w := GetWindowWidth( nControlHandle )
         h := GetWindowHeight( nControlHandle )

         SetWindowStyle( nControlHandle, nStyle, .T. )

         _SetBtnPicture( nControlHandle, image )

      ENDIF

   ELSE

      nParentHandle := GetFormHandle( ParentFormName )

      aRet := ;
         InitImageButton( ;
            nParentHandle, ;
            Caption, ;
            0, ;
            x, ;
            y, ;
            w, ;
            h, ;
            image, ;
            flat, ;
            notrans, ;
            invisible, ;
            notabstop, ;
            lDefault, ;
            icon, ;
            extract, ;
            nIdx, ;
            ( _HMG_IsThemed .AND. ! noxpstyle ) )

      nControlHandle := aRet[1]
      nhImage        := aRet[2]

   ENDIF

   /* Register name */

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /* Identity */

   _HMG_aControlType[ nControl ]          := "BUTTON"
   _HMG_aControlNames[ nControl ]         := ControlName
   _HMG_aControlHandles[ nControl ]       := nControlHandle
   _HMG_aControlParentHandles[ nControl ] := nParentHandle
   _HMG_aControlIds[ nControl ]           := nId

   /* Events */

   _HMG_aControlProcedures[ nControl ]         := ProcedureName
   _HMG_aControllostFocusProcedure[ nControl ] := lostfocus
   _HMG_aControlGotFocusProcedure[ nControl ]  := gotfocus
   _HMG_aControlChangeProcedure[ nControl ]    := ""

   /* State */

   _HMG_aControlPageMap[ nControl ] := {}
   _HMG_aControlValue[ nControl ]   := NIL

   _HMG_aControlInputMask[ nControl ] := ;
      iif( ISCHARACTER( key ), key, "" )

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := ! invisible

   /* Geometry */

   _HMG_aControlRow[ nControl ]    := y
   _HMG_aControlCol[ nControl ]    := x

   _HMG_aControlWidth[ nControl ]  := w
   _HMG_aControlHeight[ nControl ] := h

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ]    := nContainerRow
   _HMG_aControlContainerCol[ nControl ]    := nContainerCol
   _HMG_aControlContainerHandle[ nControl ] := 0

   /* Appearance */

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL

   _HMG_aControlPicture[ nControl ] := cPicture

   _HMG_aControlFontName[ nControl ] := ""
   _HMG_aControlFontSize[ nControl ] := 0

   _HMG_aControlFontAttributes[ nControl ] := ;
      { .F., .F., .F., .F. }

   _HMG_aControlToolTip[ nControl ] := tooltip
   _HMG_aControlCaption[ nControl ] := Caption

   _HMG_aControlFontHandle[ nControl ]  := 0
   _HMG_aControlBrushHandle[ nControl ] := nhImage

   /* Misc */

   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0

   _HMG_aControlHelpId[ nControl ] := HelpId

   _HMG_aControlDblClick[ nControl ] := noxpstyle
   _HMG_aControlHeadClick[ nControl ] := {}

   _HMG_aControlMiscData1[ nControl ] := ;
      IFEMPTY( icon, 0, 1 )

   _HMG_aControlMiscData2[ nControl ] := ""

   /* OOP */

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nControl, cVarName )
   ENDIF

   /* Runtime init */

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

      _SetHotKeyByName( ;
         ParentFormName, ;
         key, ;
         ProcedureName )

   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION InitDialogButtonImage( ParentFormName, ;
      nControlHandle, nControl )
*-----------------------------------------------------------------------------*

   LOCAL image

   image := _HMG_aControlPicture[ nControl ]

   IF ! Empty( image ) .AND. ;
         ValType( ParentFormName ) != "U"

      _SetBtnPicture( nControlHandle, image )

   ENDIF

   /* Modal dialog cleanup */

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
         _HMG_aDialogTemplate[3]

      _HMG_aControlDeleted[ nControl ] := .T.

   ENDIF

RETURN NIL
