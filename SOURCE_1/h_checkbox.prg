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

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   #xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
   #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
#endif

#define DEFAULT_CHECK_WIDTH   100
#define DEFAULT_CHECK_HEIGHT   28

*------------------------------------------------------------------------------*
STATIC FUNCTION _BuildCheckStyle( lNoTabStop, lInvisible, lThreeState, lPushLike, lBitmap )
*------------------------------------------------------------------------------*
   LOCAL nStyle := BS_NOTIFY + WS_CHILD

   IF ! lNoTabStop
      nStyle += WS_TABSTOP
   ENDIF

   IF ! lInvisible
      nStyle += WS_VISIBLE
   ENDIF

   IF lBitmap
      nStyle += BS_BITMAP
   ENDIF

   IF lPushLike
      nStyle += BS_PUSHLIKE
   ENDIF

   IF lThreeState
      nStyle += BS_AUTO3STATE
   ELSE
      nStyle += BS_AUTOCHECKBOX
   ENDIF

RETURN nStyle

*------------------------------------------------------------------------------*
STATIC FUNCTION _PrepareParentForm( ParentFormName, x, y, FontName, FontSize )
*------------------------------------------------------------------------------*

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := iif( ;
         _HMG_BeginDialogActive, ;
         _HMG_ActiveDialogName, ;
         _HMG_ActiveFormName )

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

RETURN ParentFormName

*------------------------------------------------------------------------------*
STATIC PROCEDURE _ValidateControl( ControlName, ParentFormName, lDialogInMemory )
*------------------------------------------------------------------------------*

   IF ! _IsWindowDefined( ParentFormName ) .AND. ! lDialogInMemory
      MsgMiniGuiError( ;
         "Window: " + ;
         IFNIL( ParentFormName, "Parent", ParentFormName ) + ;
         " is not defined." )
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ;
         ! lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ControlName + ;
         " Of " + ParentFormName + ;
         " Already defined." )
   ENDIF

RETURN

*------------------------------------------------------------------------------*
STATIC PROCEDURE _ApplyFont( ;
   ControlHandle, ;
   FontHandle, ;
   FontName, ;
   FontSize, ;
   bold, ;
   italic, ;
   underline, ;
   strikeout )
*------------------------------------------------------------------------------*

   IF FontHandle != 0

      _SetFontHandle( ControlHandle, FontHandle )

   ELSE

      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )

      FontHandle := _SetFont( ;
         ControlHandle, ;
         FontName, ;
         FontSize, ;
         bold, ;
         italic, ;
         underline, ;
         strikeout )

   ENDIF

RETURN

*------------------------------------------------------------------------------*
STATIC PROCEDURE _ApplyToolTip( ;
   ControlHandle, ;
   ParentFormName, ;
   ToolTip )
*------------------------------------------------------------------------------*

   IF ToolTip != NIL
      SetToolTip( ;
         ControlHandle, ;
         ToolTip, ;
         GetFormToolTipHandle( ParentFormName ) )
   ENDIF

RETURN

*------------------------------------------------------------------------------*
FUNCTION _DefineCheckBox( ;
   ControlName, ParentFormName, x, y, Caption, Value, ;
   FontName, FontSize, ToolTip, ChangeProcedure, ;
   w, h, LostFocus, GotFocus, HelpId, ;
   Invisible, NoTabStop, Bold, Italic, ;
   Underline, StrikeOut, Field, BackColor, ;
   FontColor, Transparent, LeftJustify, ;
   ThreeState, Enter, AutoSize, MultiLine, ;
   nId, bInit )
*------------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle := 0
   LOCAL WorkArea
   LOCAL cVarName
   LOCAL nIndex
   LOCAL nStyle
   LOCAL blInit
   LOCAL lDialogInMemory
   LOCAL oc := NIL
   LOCAL ow := NIL
   LOCAL aFormBkColor
   LOCAL nColor

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default( @w, DEFAULT_CHECK_WIDTH )
   hb_default( @h, DEFAULT_CHECK_HEIGHT )

   hb_default( @Invisible,    .F. )
   hb_default( @NoTabStop,    .F. )
   hb_default( @Transparent,  .F. )
   hb_default( @LeftJustify,  .F. )
   hb_default( @MultiLine,    .F. )
   hb_default( @ThreeState,   .F. )
   hb_default( @AutoSize,     .F. )

   __defaultNIL( @LostFocus, "" )
   __defaultNIL( @GotFocus, "" )
   __defaultNIL( @ChangeProcedure, "" )

   IF ! ThreeState
      hb_default( @Value, .F. )
   ENDIF

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         FontHandle, ;
         @FontName, ;
         @FontSize, ;
         @Bold, ;
         @Italic, ;
         @Underline, ;
         @StrikeOut )

   ENDIF

   IF Field != NIL

      IF hb_UAt( ">", Field ) == 0

         MsgMiniGuiError( ;
            "Control: " + ControlName + ;
            " Of " + ParentFormName + ;
            " : You must specify a fully qualified field name." )

      ELSE

         WorkArea := hb_ULeft( ;
            Field, ;
            hb_UAt( ">", Field ) - 2 )

         IF Select( WorkArea ) != 0
            Value := &( Field )
         ENDIF

      ENDIF

   ENDIF

   ParentFormName := ;
      _PrepareParentForm( ;
         ParentFormName, ;
         @x, ;
         @y, ;
         @FontName, ;
         @FontSize )

   lDialogInMemory := _HMG_DialogInMemory

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   _ValidateControl( ;
      ControlName, ;
      ParentFormName, ;
      lDialogInMemory )

   IF Transparent .AND. ;
         _HMG_FrameLevel == 0 .AND. ;
         _HMG_IsThemed

      Transparent := .F.

      aFormBkColor := ;
         _HMG_aFormBkColor[ GetFormIndex( ParentFormName ) ]

      IF BackColor == NIL .AND. ;
            aFormBkColor[1] < 0 .AND. ;
            aFormBkColor[2] < 0 .AND. ;
            aFormBkColor[3] < 0

         nColor := GetSysColor( COLOR_BTNFACE )
         BackColor := nRGB2Arr( nColor )

      ELSE
         BackColor := aFormBkColor
      ENDIF

   ENDIF

   cVarName := "_" + ParentFormName + "_" + ControlName
   nIndex   := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := _BuildCheckStyle( ;
         NoTabStop, ;
         Invisible, ;
         ThreeState, ;
         .F., ;
         .F. )

      IF lDialogInMemory

         blInit := {|x1, y1, z1| ;
            InitDialogCheckButton( x1, y1, z1 ) }

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nIndex, ;
               "button", ;
               nStyle, ;
               0, ;
               x, y, w, h, ;
               Caption, ;
               HelpId, ;
               ToolTip, ;
               FontName, ;
               FontSize, ;
               Bold, ;
               Italic, ;
               Underline, ;
               StrikeOut, ;
               blInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )
         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         IF Caption != NIL
            SetWindowText( ControlHandle, Caption )
         ENDIF

         SetWindowStyle( ControlHandle, nStyle, .T. )

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle( ParentFormName )

      ControlHandle := InitCheckBox( ;
         ParentFormHandle, ;
         Caption, ;
         0, ;
         x, ;
         y, ;
         MultiLine, ;
         ThreeState, ;
         w, ;
         h, ;
         Invisible, ;
         NoTabStop, ;
         LeftJustify, ;
         Transparent )

   ENDIF

   IF ! lDialogInMemory

      _ApplyFont( ;
         ControlHandle, ;
         @FontHandle, ;
         FontName, ;
         FontSize, ;
         Bold, ;
         Italic, ;
         Underline, ;
         StrikeOut )

      IF _HMG_IsThemed .AND. IsArrayRGB( FontColor )
         SetWindowTheme( ControlHandle, "", "" )
      ENDIF

      IF _HMG_BeginTabActive
         AAdd( ;
            _HMG_ActiveTabCurrentPageMap, ;
            ControlHandle )
      ENDIF

      _ApplyToolTip( ;
         ControlHandle, ;
         ParentFormName, ;
         ToolTip )

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nIndex )
#else
   Public &cVarName. := nIndex
#endif

   _HMG_aControlType[nIndex]               := "CHECKBOX"
   _HMG_aControlNames[nIndex]              := ControlName
   _HMG_aControlHandles[nIndex]            := ControlHandle
   _HMG_aControlParentHandles[nIndex]      := ParentFormHandle
   _HMG_aControlIds[nIndex]                := nId
   _HMG_aControlProcedures[nIndex]         := Enter
   _HMG_aControlPageMap[nIndex]            := Field
   _HMG_aControlValue[nIndex]              := Value
   _HMG_aControlInputMask[nIndex]          := Transparent
   _HMG_aControlLostFocusProcedure[nIndex] := LostFocus
   _HMG_aControlGotFocusProcedure[nIndex]  := GotFocus
   _HMG_aControlChangeProcedure[nIndex]    := ChangeProcedure
   _HMG_aControlDeleted[nIndex]            := .F.
   _HMG_aControlBkColor[nIndex]            := BackColor
   _HMG_aControlFontColor[nIndex]          := FontColor
   _HMG_aControlDblClick[nIndex]           := _HMG_ActiveTabButtons
   _HMG_aControlHeadClick[nIndex]          := {}
   _HMG_aControlRow[nIndex]                := y
   _HMG_aControlCol[nIndex]                := x
   _HMG_aControlWidth[nIndex]              := w
   _HMG_aControlHeight[nIndex]             := h
   _HMG_aControlSpacing[nIndex]            := ThreeState

   _HMG_aControlContainerRow[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlContainerCol[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlPicture[nIndex]            := ""
   _HMG_aControlContainerHandle[nIndex]    := 0
   _HMG_aControlFontName[nIndex]           := FontName
   _HMG_aControlFontSize[nIndex]           := FontSize
   _HMG_aControlFontAttributes[nIndex]     := ;
      { Bold, Italic, Underline, StrikeOut }

   _HMG_aControlRangeMin[nIndex]           := ;
      iif ( _HMG_FrameLevel > 0 , _HMG_ActiveTabName , '' )
   _HMG_aControlRangeMax[nIndex]           := ;
      iif ( _HMG_FrameLevel > 0 , _HMG_ActiveFrameParentFormName [_HMG_FrameLevel] , '' )

   _HMG_aControlToolTip[nIndex]            := ToolTip
   _HMG_aControlCaption[nIndex]            := Caption
   _HMG_aControlVisible[nIndex]            := ! Invisible

   _HMG_aControlHelpId[nIndex]             := HelpId
   _HMG_aControlFontHandle[nIndex]         := FontHandle
   _HMG_aControlBrushHandle[nIndex]        := 0
   _HMG_aControlEnabled[nIndex]            := .T.
   _HMG_aControlMiscData1[nIndex]          := 0
   _HMG_aControlMiscData2[nIndex]          := ""

   IF ! lDialogInMemory

      IF ThreeState .AND. Value == NIL

         SendMessage( ;
            ControlHandle, ;
            BM_SETCHECK, ;
            BST_INDETERMINATE, ;
            0 )

      ELSEIF Value

         SendMessage( ;
            ControlHandle, ;
            BM_SETCHECK, ;
            BST_CHECKED, ;
            0 )

      ENDIF

      IF AutoSize

         _SetControlWidth( ;
            ControlName, ;
            ParentFormName, ;
            GetTextWidth( NIL, Caption, FontHandle ) + ;
            iif( Bold .OR. Italic, ;
                  GetTextWidth( NIL, " ", FontHandle ), 0 ) + ;
            20 )

         _SetControlHeight( ;
            ControlName, ;
            ParentFormName, ;
            FontSize + iif( FontSize < 14, 12, 16 ) )

         RedrawWindow( ControlHandle )

      ENDIF

   ENDIF

   IF Field != NIL

      AAdd( ;
         _HMG_aFormBrowseList[ ;
            GetFormIndex( ParentFormName ) ], ;
         nIndex )

   ENDIF

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, nIndex, cVarName )

#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif

   ENDIF

   Do_ControlEventProcedure( bInit, nIndex, ow, oc )

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION _DefineCheckButton( ;
   ControlName, ParentFormName, ;
   x, y, Caption, Value, ;
   FontName, FontSize, ToolTip, ;
   ChangeProcedure, ;
   w, h, LostFocus, GotFocus, ;
   HelpId, Invisible, NoTabStop, ;
   Bold, Italic, Underline, ;
   StrikeOut, nId )
*------------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle := 0
   LOCAL cVarName
   LOCAL nIndex
   LOCAL nStyle
   LOCAL blInit
   LOCAL lDialogInMemory

   hb_default( @Value, .F. )
   hb_default( @w, DEFAULT_CHECK_WIDTH )
   hb_default( @h, DEFAULT_CHECK_HEIGHT )

   hb_default( @Invisible, .F. )
   hb_default( @NoTabStop, .F. )

   __defaultNIL( @LostFocus, "" )
   __defaultNIL( @GotFocus, "" )
   __defaultNIL( @ChangeProcedure, "" )

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         FontHandle, ;
         @FontName, ;
         @FontSize, ;
         @Bold, ;
         @Italic, ;
         @Underline, ;
         @StrikeOut )

   ENDIF

   ParentFormName := ;
      _PrepareParentForm( ;
         ParentFormName, ;
         @x, ;
         @y, ;
         @FontName, ;
         @FontSize )

   lDialogInMemory := _HMG_DialogInMemory

   _ValidateControl( ;
      ControlName, ;
      ParentFormName, ;
      lDialogInMemory )

   cVarName := "_" + ParentFormName + "_" + ControlName
   nIndex   := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := _BuildCheckStyle( ;
         NoTabStop, ;
         Invisible, ;
         .F., ;
         .T., ;
         .F. )

      IF lDialogInMemory

         blInit := {|x1, y1, z1| ;
            InitDialogCheckButton( x1, y1, z1 ) }

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nIndex, ;
               "button", ;
               nStyle, ;
               0, ;
               x, y, w, h, ;
               "", ;
               HelpId, ;
               ToolTip, ;
               FontName, ;
               FontSize, ;
               Bold, ;
               Italic, ;
               Underline, ;
               StrikeOut, ;
               blInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )
         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         SetWindowStyle( ;
            ControlHandle, ;
            nStyle, ;
            .T. )

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle( ParentFormName )

      ControlHandle := InitCheckButton( ;
         ParentFormHandle, ;
         Caption, ;
         0, ;
         x, ;
         y, ;
         "", ;
         0, ;
         w, ;
         h, ;
         Invisible, ;
         NoTabStop )

   ENDIF

   IF ! lDialogInMemory

      _ApplyFont( ;
         ControlHandle, ;
         @FontHandle, ;
         FontName, ;
         FontSize, ;
         Bold, ;
         Italic, ;
         Underline, ;
         StrikeOut )

      IF _HMG_BeginTabActive

         AAdd( ;
            _HMG_ActiveTabCurrentPageMap, ;
            ControlHandle )

      ENDIF

      _ApplyToolTip( ;
         ControlHandle, ;
         ParentFormName, ;
         ToolTip )

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nIndex )
#else
   Public &cVarName. := nIndex
#endif

   _HMG_aControlType[nIndex]               := "CHECKBOX"
   _HMG_aControlNames[nIndex]              := ControlName
   _HMG_aControlHandles[nIndex]            := ControlHandle
   _HMG_aControlParentHandles[nIndex]      := ParentFormHandle
   _HMG_aControlIds[nIndex]                := nId

   _HMG_aControlProcedures[nIndex]         := ""
   _HMG_aControlPageMap[nIndex]            := {}
   _HMG_aControlValue[nIndex]              := Value
   _HMG_aControlInputMask[nIndex]          := ""

   _HMG_aControlLostFocusProcedure[nIndex] := LostFocus
   _HMG_aControlGotFocusProcedure[nIndex]  := GotFocus
   _HMG_aControlChangeProcedure[nIndex]    := ChangeProcedure

   _HMG_aControlDeleted[nIndex]            := .F.

   _HMG_aControlBkColor[nIndex]            := NIL
   _HMG_aControlFontColor[nIndex]          := NIL

   _HMG_aControlDblClick[nIndex]           := ""
   _HMG_aControlHeadClick[nIndex]          := {}

   _HMG_aControlRow[nIndex]                := y
   _HMG_aControlCol[nIndex]                := x
   _HMG_aControlWidth[nIndex]              := w
   _HMG_aControlHeight[nIndex]             := h

   _HMG_aControlSpacing[nIndex]            := .F.

   _HMG_aControlContainerRow[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlContainerCol[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlPicture[nIndex]            := ""
   _HMG_aControlContainerHandle[nIndex]    := 0

   _HMG_aControlFontName[nIndex]           := FontName
   _HMG_aControlFontSize[nIndex]           := FontSize

   _HMG_aControlFontAttributes[nIndex]     := ;
      { Bold, Italic, Underline, StrikeOut }

   _HMG_aControlToolTip[nIndex]            := ToolTip

   _HMG_aControlRangeMin[nIndex]           := 0
   _HMG_aControlRangeMax[nIndex]           := 0

   _HMG_aControlCaption[nIndex]            := Caption

   _HMG_aControlVisible[nIndex]            := ! Invisible

   _HMG_aControlHelpId[nIndex]             := HelpId

   _HMG_aControlFontHandle[nIndex]         := FontHandle
   _HMG_aControlBrushHandle[nIndex]        := 0

   _HMG_aControlEnabled[nIndex]            := .T.

   _HMG_aControlMiscData1[nIndex]          := 2
   _HMG_aControlMiscData2[nIndex]          := ""

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nIndex, cVarName )
   ENDIF

   IF Value .AND. ! lDialogInMemory

      SendMessage( ;
         ControlHandle, ;
         BM_SETCHECK, ;
         BST_CHECKED, ;
         0 )

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION InitDialogCheckButton( ParentName, ControlHandle, nIndex )
*------------------------------------------------------------------------------*

   LOCAL cBitmap    := _HMG_aControlPicture[nIndex]
   LOCAL xValue     := _HMG_aControlValue[nIndex]
   LOCAL lThreeState := _HMG_aControlSpacing[nIndex]

   IF ! Empty( cBitmap ) .AND. ;
         ValType( ParentName ) != "U"

      _SetBtnPicture( ControlHandle, cBitmap )

   ENDIF

   IF lThreeState .AND. xValue == NIL

      SendMessage( ;
         ControlHandle, ;
         BM_SETCHECK, ;
         BST_INDETERMINATE, ;
         0 )

   ELSEIF xValue

      SendMessage( ;
         ControlHandle, ;
         BM_SETCHECK, ;
         BST_CHECKED, ;
         0 )

   ENDIF

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
         _HMG_aDialogTemplate[3]

      _HMG_aControlDeleted[nIndex] := .T.

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION _DefineImageCheckButton( ;
   ControlName, ParentFormName, ;
   x, y, BitMap, Value, ;
   FontName, FontSize, ToolTip, ;
   ChangeProcedure, ;
   w, h, LostFocus, GotFocus, ;
   HelpId, Invisible, NoTabStop, ;
   nId, NoTrans )
*------------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL aInitResult
   LOCAL nImageHandle := 0
   LOCAL cVarName
   LOCAL nIndex
   LOCAL nStyle
   LOCAL blInit
   LOCAL lDialogInMemory

   hb_default( @Value, .F. )
   hb_default( @w, DEFAULT_CHECK_WIDTH )
   hb_default( @h, DEFAULT_CHECK_HEIGHT )

   hb_default( @Invisible, .F. )
   hb_default( @NoTabStop, .F. )
   hb_default( @NoTrans, .F. )

   __defaultNIL( @LostFocus, "" )
   __defaultNIL( @GotFocus, "" )
   __defaultNIL( @ChangeProcedure, "" )

   ParentFormName := ;
      _PrepareParentForm( ;
         ParentFormName, ;
         @x, ;
         @y, ;
         @FontName, ;
         @FontSize )

   lDialogInMemory := _HMG_DialogInMemory

   _ValidateControl( ;
      ControlName, ;
      ParentFormName, ;
      lDialogInMemory )

   cVarName := "_" + ParentFormName + "_" + ControlName
   nIndex   := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := _BuildCheckStyle( ;
         NoTabStop, ;
         Invisible, ;
         .F., ;
         .T., ;
         .T. )

      IF lDialogInMemory

         blInit := {|x1, y1, z1| ;
            InitDialogCheckButton( x1, y1, z1 ) }

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nIndex, ;
               "button", ;
               nStyle, ;
               0, ;
               x, y, w, h, ;
               "", ;
               HelpId, ;
               ToolTip, ;
               FontName, ;
               FontSize, ;
               , ;
               , ;
               , ;
               , ;
               blInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )
         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         SetWindowStyle( ;
            ControlHandle, ;
            nStyle, ;
            .T. )

         _SetBtnPicture( ;
            ControlHandle, ;
            BitMap )

      ENDIF

   ELSE

      ParentFormHandle := ;
         GetFormHandle( ParentFormName )

      aInitResult := InitImageCheckButton( ;
         ParentFormHandle, ;
         "", ;
         0, ;
         x, ;
         y, ;
         "", ;
         NoTrans, ;
         BitMap, ;
         w, ;
         h, ;
         Invisible, ;
         NoTabStop, ;
         _HMG_IsThemed )

      ControlHandle := aInitResult[1]
      nImageHandle  := aInitResult[2]

   ENDIF

   IF ! lDialogInMemory

      IF _HMG_BeginTabActive

         AAdd( ;
            _HMG_ActiveTabCurrentPageMap, ;
            ControlHandle )

      ENDIF

      _ApplyToolTip( ;
         ControlHandle, ;
         ParentFormName, ;
         ToolTip )

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nIndex )
#else
   Public &cVarName. := nIndex
#endif

   _HMG_aControlType[nIndex]               := "CHECKBOX"
   _HMG_aControlNames[nIndex]              := ControlName
   _HMG_aControlHandles[nIndex]            := ControlHandle
   _HMG_aControlParentHandles[nIndex]      := ParentFormHandle
   _HMG_aControlIds[nIndex]                := nId

   _HMG_aControlProcedures[nIndex]         := ""
   _HMG_aControlPageMap[nIndex]            := {}

   _HMG_aControlValue[nIndex]              := Value
   _HMG_aControlInputMask[nIndex]          := ""

   _HMG_aControlLostFocusProcedure[nIndex] := LostFocus
   _HMG_aControlGotFocusProcedure[nIndex]  := GotFocus
   _HMG_aControlChangeProcedure[nIndex]    := ChangeProcedure

   _HMG_aControlDeleted[nIndex]            := .F.

   _HMG_aControlBkColor[nIndex]            := NIL
   _HMG_aControlFontColor[nIndex]          := NIL

   _HMG_aControlDblClick[nIndex]           := ""
   _HMG_aControlHeadClick[nIndex]          := {}

   _HMG_aControlRow[nIndex]                := y
   _HMG_aControlCol[nIndex]                := x
   _HMG_aControlWidth[nIndex]              := w
   _HMG_aControlHeight[nIndex]             := h

   _HMG_aControlSpacing[nIndex]            := NoTrans

   _HMG_aControlContainerRow[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlContainerCol[nIndex]       := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlPicture[nIndex]            := BitMap

   _HMG_aControlContainerHandle[nIndex]    := 0

   _HMG_aControlFontName[nIndex]           := FontName
   _HMG_aControlFontSize[nIndex]           := FontSize

   _HMG_aControlFontAttributes[nIndex]     := ;
      { .F., .F., .F., .F. }

   _HMG_aControlToolTip[nIndex]            := ToolTip

   _HMG_aControlRangeMin[nIndex]           := 0
   _HMG_aControlRangeMax[nIndex]           := 0

   _HMG_aControlCaption[nIndex]            := ""

   _HMG_aControlVisible[nIndex]            := ! Invisible

   _HMG_aControlHelpId[nIndex]             := HelpId

   _HMG_aControlFontHandle[nIndex]         := 0
   _HMG_aControlBrushHandle[nIndex]        := nImageHandle

   _HMG_aControlEnabled[nIndex]            := .T.

   _HMG_aControlMiscData1[nIndex]          := 1
   _HMG_aControlMiscData2[nIndex]          := ""

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nIndex, cVarName )
   ENDIF

   IF Value .AND. ! lDialogInMemory

      SendMessage( ;
         ControlHandle, ;
         BM_SETCHECK, ;
         BST_CHECKED, ;
         0 )

   ENDIF

RETURN NIL
