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

/*---------------------------------------------------------------------------*/
/* Layout constants                                                          */
/*---------------------------------------------------------------------------*/

#define RADIO_AUTOSIZE_EXTRA_W   21
#define RADIO_AUTOSIZE_EXTRA_H    8
#define RADIO_DEFAULT_HEIGHT     28
#define RADIO_DEFAULT_SPACING    25

/*---------------------------------------------------------------------------*/
FUNCTION _DefineRadioGroup( ;
      ControlName, ParentFormName, x, y, aOptions, Value, ;
      FontName, FontSize, ToolTip, Change, Width, Spacing, ;
      HelpId, Invisible, NoTabStop, Bold, Italic, Underline, ;
      StrikeOut, BackColor, FontColor, Transparent, Horizontal, ;
      LeftJustify, aReadOnly, AutoSize, GotFocus, LostFocus, ;
      aId, bInit )
/*---------------------------------------------------------------------------*/

   LOCAL ParentFormHandle
   LOCAL FontHandle
   LOCAL aHandles := {}
   LOCAL cVarName
   LOCAL nIndex
   LOCAL nBackRow, nBackCol, nBackWidth
   LOCAL lDialogInMemory
   LOCAL oControl := NIL, oWindow := NIL

#ifdef _OBJECT_
   oWindow := oDlu2Pixel()
#endif

   /* Apply defaults */
   hb_default( @Width, 120 )
   hb_default( @Invisible, .F. )
   hb_default( @NoTabStop, .F. )
   hb_default( @Horizontal, .F. )
   hb_default( @AutoSize, .F. )
   hb_default( @LeftJustify, .F. )

   __defaultNIL( @Change, "" )
   __defaultNIL( @GotFocus, "" )
   __defaultNIL( @LostFocus, "" )

   hb_default( @Spacing, iif( Horizontal, iif( AutoSize, GetBorderWidth(), 0 ), RADIO_DEFAULT_SPACING ) )

   /* Font handling */
   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @Bold, @Italic, @Underline, @StrikeOut )
   ENDIF

   /* Context from active window/dialog */
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )
   ENDIF

   /* Adjust coordinates inside frames */
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[_HMG_FrameLevel]
      y += _HMG_ActiveFrameRow[_HMG_FrameLevel]
      ParentFormName := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel]
   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /* Validation checks */
   IF ! _IsWindowDefined( ParentFormName ) .AND. ! lDialogInMemory
      MsgMiniGuiError( "Window: " + IFNIL( ParentFormName, "Parent", ParentFormName ) + " is not defined." )
   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ! lDialogInMemory
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + " Already defined." )
   ENDIF

   /* ReadOnly array initialization */
   IF ! ISARRAY( aReadOnly )
      aReadOnly := Array( Len( aOptions ) )
      AFill( aReadOnly, .F. )
   ENDIF

   cVarName := "_" + ParentFormName + "_" + ControlName
   nIndex   := _GetControlFree()

   nBackCol := x
   nBackRow := y

   /* Create the radio buttons (dialog or runtime) */
   IF _HMG_BeginDialogActive
      ParentFormHandle := _HMG_ActiveDialogHandle

      IF lDialogInMemory
         _CreateDialogRadioGroupTemplate( aHandles, aOptions, aId, x, y, Width, Spacing, ;
            Horizontal, Invisible, ToolTip, HelpId, FontName, FontSize, ;
            Bold, Italic, Underline, StrikeOut, nIndex )
      ELSE
         _CreateDialogRadioGroupControls( aHandles, ParentFormHandle, aOptions, aId, ;
            Invisible, NoTabStop, FontHandle, @FontName, @FontSize, ;
            Bold, Italic, Underline, StrikeOut, @x, @y, @Width, @Spacing )
      ENDIF
   ELSE
      ParentFormHandle := GetFormHandle( ParentFormName )

      _CreateRuntimeRadioGroup( aHandles, ParentFormHandle, aOptions, x, y, Width, Spacing, ;
         Horizontal, Invisible, NoTabStop, LeftJustify, AutoSize, FontHandle, ;
         @FontName, @FontSize, Bold, Italic, Underline, StrikeOut, ToolTip, ;
         ParentFormName, @nBackWidth )
   ENDIF

   /* Theme and tooltip handling */
   IF ! lDialogInMemory
      IF _HMG_IsThemed .AND. ( IsArrayRGB( BackColor ) .OR. IsArrayRGB( FontColor ) )
         AEval( aHandles, {|h| SetWindowTheme( h, "", "" ) } )
      ENDIF

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, aHandles )
      ENDIF

      IF ToolTip != NIL
         SetToolTip( aHandles[1], ToolTip, GetFormToolTipHandle( ParentFormName ) )
      ENDIF
   ENDIF

   /* Register public variable and control metadata */
#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nIndex )
#else
   PUBLIC &cVarName. := nIndex
#endif

   /* Store control properties in HMG arrays */
   _HMG_aControlType[nIndex]               := "RADIOGROUP"
   _HMG_aControlNames[nIndex]              := ControlName
   _HMG_aControlHandles[nIndex]            := aHandles
   _HMG_aControlParentHandles[nIndex]      := ParentFormHandle
   _HMG_aControlIds[nIndex]                := aId
   _HMG_aControlValue[nIndex]              := iif( ISNUMERIC( Value ), Value, 0 )
   _HMG_aControlPageMap[nIndex]            := aReadOnly
   _HMG_aControlDeleted[nIndex]            := .F.
   _HMG_aControlBkColor[nIndex]            := BackColor
   _HMG_aControlFontColor[nIndex]          := FontColor
   _HMG_aControlLostFocusProcedure[nIndex] := LostFocus
   _HMG_aControlGotFocusProcedure[nIndex]  := GotFocus
   _HMG_aControlChangeProcedure[nIndex]    := Change
   _HMG_aControlRow[nIndex]                := nBackRow
   _HMG_aControlCol[nIndex]                := nBackCol
   _HMG_aControlWidth[nIndex]              := iif( AutoSize, nBackWidth, Width )
   _HMG_aControlHeight[nIndex]             := iif( Horizontal, RADIO_DEFAULT_HEIGHT, Spacing * Len( aOptions ) + GetBorderHeight() )
   _HMG_aControlSpacing[nIndex]            := Spacing
   _HMG_aControlInputMask[nIndex]          := Transparent
   _HMG_aControlContainerRow[nIndex]       := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1 )
   _HMG_aControlContainerCol[nIndex]       := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1 )
   _HMG_aControlPicture[nIndex]            := ! NoTabStop
   _HMG_aControlContainerHandle[nIndex]    := 0
   _HMG_aControlFontName[nIndex]           := FontName
   _HMG_aControlFontSize[nIndex]           := FontSize
   _HMG_aControlFontAttributes[nIndex]     := { Bold, Italic, Underline, StrikeOut }
   _HMG_aControlToolTip[nIndex]            := ToolTip
   _HMG_aControlCaption[nIndex]            := aOptions
   _HMG_aControlProcedures[nIndex]         := ""
   _HMG_aControlDblClick[nIndex]           := _HMG_ActiveTabButtons
   _HMG_aControlHeadClick[nIndex]          := AutoSize
   _HMG_aControlRangeMin[nIndex]           := iif( _HMG_FrameLevel > 0, _HMG_ActiveTabName, "" )
   _HMG_aControlRangeMax[nIndex]           := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameParentFormName[_HMG_FrameLevel], "" )
   _HMG_aControlBrushHandle[nIndex]        := 0
   _HMG_aControlVisible[nIndex]            := iif( Invisible, FALSE, TRUE )
   _HMG_aControlHelpId[nIndex]             := HelpId
   _HMG_aControlFontHandle[nIndex]         := FontHandle
   _HMG_aControlEnabled[nIndex]            := .T.
   _HMG_aControlMiscData1[nIndex]          := Horizontal
   _HMG_aControlMiscData2[nIndex]          := ""

   /* Initial value and ReadOnly setup */
   IF ! lDialogInMemory
      IF ISNUMERIC( Value ) .AND. Value > 0
         _SetValue( , , Value, nIndex )
      ENDIF
      SetProperty( ParentFormName, ControlName, "ReadOnly", aReadOnly )
   ENDIF

   /* OOP support */
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nIndex, cVarName )
#ifdef _OBJECT_
      oWindow  := _WindowObj( ParentFormHandle )
      oControl := _ControlObj( aHandles[1] )
#endif
   ENDIF

   Do_ControlEventProcedure( bInit, nIndex, oWindow, oControl )

RETURN Nil

/*---------------------------------------------------------------------------*/
STATIC FUNCTION _SetRadioGroupFont( hControl, FontHandle, FontName, FontSize, ;
      Bold, Italic, Underline, StrikeOut )

   IF FontHandle != 0
      _SetFontHandle( hControl, FontHandle )
   ELSE
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )
      FontHandle := _SetFont( hControl, FontName, FontSize, Bold, Italic, Underline, StrikeOut )
   ENDIF

RETURN FontHandle

/*---------------------------------------------------------------------------*/
/* Runtime radio group creation (normal window)                              */
/*---------------------------------------------------------------------------*/

STATIC FUNCTION _CreateRuntimeRadioGroup( ;
      aHandles, ParentFormHandle, aOptions, x, y, Width, Spacing, ;
      Horizontal, Invisible, NoTabStop, LeftJustify, AutoSize, ;
      FontHandle, FontName, FontSize, Bold, Italic, Underline, StrikeOut, ;
      ToolTip, ParentFormName, nBackWidth )

   LOCAL i, hControl

   /* First radio button */
   hControl := InitRadioGroup( ParentFormHandle, aOptions[1], 0, x, y, "", 0, Width, Invisible, NoTabStop, LeftJustify )

   /* Font setup */
   FontHandle := _SetRadioGroupFont( hControl, FontHandle, FontName, FontSize, Bold, Italic, Underline, StrikeOut )

   /* AutoSize adjustment for first button */
   IF AutoSize
      nBackWidth := Width
      Width := GetTextWidth( NIL, aOptions[1], FontHandle ) + RADIO_AUTOSIZE_EXTRA_W
      MoveWindow( hControl, x, y, Width, GetTextHeight( NIL, aOptions[1], FontHandle ) + RADIO_AUTOSIZE_EXTRA_H, .T. )
   ENDIF

   AAdd( aHandles, hControl )

   /* Remaining radio buttons */
   FOR i := 2 TO Len( aOptions )
      IF Horizontal
         x += Width + Spacing
      ELSE
         y += Spacing
      ENDIF

      hControl := InitRadioButton( ParentFormHandle, aOptions[i], 0, x, y, "", 0, Width, Invisible, LeftJustify )

      FontHandle := _SetRadioGroupFont( hControl, FontHandle, FontName, FontSize, Bold, Italic, Underline, StrikeOut )

      /* AutoSize adjustment for current button */
      IF AutoSize
         Width := GetTextWidth( NIL, aOptions[i], FontHandle ) + RADIO_AUTOSIZE_EXTRA_W
         MoveWindow( hControl, x, y, Width, GetTextHeight( NIL, aOptions[i], FontHandle ) + RADIO_AUTOSIZE_EXTRA_H, .T. )
      ENDIF

      AAdd( aHandles, hControl )

      IF ToolTip != NIL
         SetToolTip( aHandles[i], ToolTip, GetFormToolTipHandle( ParentFormName ) )
      ENDIF
   NEXT

RETURN Nil

/*---------------------------------------------------------------------------*/
/* Dialog template creation (for resource dialogs)                           */
/*---------------------------------------------------------------------------*/

STATIC FUNCTION _CreateDialogRadioGroupTemplate( ;
      aHandles, aOptions, aId, x, y, Width, Spacing, Horizontal, Invisible, ;
      ToolTip, HelpId, FontName, FontSize, Bold, Italic, Underline, StrikeOut, nIndex )

   LOCAL n, nStyle, bInit

   nStyle := BS_NOTIFY + WS_CHILD + BS_AUTORADIOBUTTON
   IF ! Invisible
      nStyle += WS_VISIBLE
   ENDIF

   AAdd( aHandles, 0 )

   FOR n := 1 TO Len( aId )
      bInit := iif( n == Len( aId ), {|xParent, hControl, k| InitDialogRadioGroup( xParent, hControl, k ) }, {|| Nil } )

      AAdd( _HMG_aDialogItems, { ;
         aId[n], nIndex, "button", nStyle, 0, x, y, Width, Spacing, ;
         aOptions[n], HelpId, ToolTip, FontName, FontSize, Bold, Italic, Underline, StrikeOut, ;
         bInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )

      IF Horizontal
         x += Width + Spacing
      ELSE
         y += Spacing
      ENDIF
   NEXT

RETURN Nil

/*---------------------------------------------------------------------------*/
/* Dialog runtime control initialization                                     */
/*---------------------------------------------------------------------------*/

STATIC FUNCTION _CreateDialogRadioGroupControls( ;
      aHandles, ParentFormHandle, aOptions, aId, Invisible, NoTabStop, ;
      FontHandle, FontName, FontSize, Bold, Italic, Underline, StrikeOut, ;
      x, y, Width, Spacing )

   LOCAL hControl, n, nStyle

   nStyle := BS_NOTIFY + WS_CHILD + BS_AUTORADIOBUTTON + WS_GROUP
   IF ! NoTabStop
      nStyle += WS_TABSTOP
   ENDIF
   IF ! Invisible
      nStyle += WS_VISIBLE
   ENDIF

   /* First control - get metrics */
   hControl := GetDialogItemHandle( ParentFormHandle, aId[1] )
   SetWindowStyle( hControl, nStyle, .T. )

   x       := GetWindowCol( hControl )
   y       := GetWindowRow( hControl )
   Width   := GetWindowWidth( hControl )
   Spacing := GetWindowHeight( hControl )

   /* Configure all radio buttons */
   FOR n := 1 TO Len( aId )
      hControl := GetDialogItemHandle( ParentFormHandle, aId[n] )

      SetWindowStyle( hControl, BS_NOTIFY + WS_CHILD + BS_AUTORADIOBUTTON, .T. )
      IF ! Invisible
         SetWindowStyle( hControl, WS_VISIBLE, .T. )
      ENDIF

      IF ISARRAY( aOptions ) .AND. n <= Len( aOptions )
         SetWindowText( hControl, aOptions[n] )
      ENDIF

      FontHandle := _SetRadioGroupFont( hControl, FontHandle, FontName, FontSize, Bold, Italic, Underline, StrikeOut )

      AAdd( aHandles, hControl )
   NEXT

RETURN Nil

/*---------------------------------------------------------------------------*/
/* Dialog initialization callback                                            */
/*---------------------------------------------------------------------------*/

FUNCTION InitDialogRadioGroup( ParentName, ControlHandle, nIndex )

   LOCAL aHandles := _HMG_aControlHandles[nIndex]
   LOCAL Value    := _HMG_aControlValue[nIndex]

   IF ISNUMERIC( Value ) .AND. Value > 0 .AND. ControlHandle > 0
      _SetValue( , , Value, nIndex )
   ENDIF

   IF Len( _HMG_aControlIds[nIndex] ) == Len( aHandles ) .AND. ParentName != NIL
      SetProperty( ParentName, _HMG_aControlNames[nIndex], "ReadOnly", _HMG_aControlPageMap[nIndex] )
   ENDIF

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[3]
      _HMG_aControlDeleted[nIndex] := .T.
   ENDIF

RETURN Nil
