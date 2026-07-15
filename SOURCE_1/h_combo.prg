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

#define CBS_UPPERCASE  0x2000
#define CBS_LOWERCASE  0x4000

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   #xtranslate hb_UAt( <c>, <n> )    => At( <c>, <n> )
   #xtranslate hb_ULeft( <c>, <n> )  => Left( <c>, <n> )
   #xtranslate hb_URight( <c>, <n> ) => Right( <c>, <n> )
   #xtranslate hb_ULen( <c> )        => Len( <c> )
#endif

/*----------------------------------------------------------------------*/
STATIC FUNCTION _ApplyComboFont( hControl, hFont, cFontName, nFontSize, ;
                                 lBold, lItalic, lUnderline, lStrikeOut )
/*----------------------------------------------------------------------*/
   IF hFont != 0
      _SetFontHandle( hControl, hFont )
   ELSE
      __defaultNIL( @cFontName, _HMG_DefaultFontName )
      __defaultNIL( @nFontSize, _HMG_DefaultFontSize )

      hFont := _SetFont( hControl, cFontName, nFontSize, ;
                         lBold, lItalic, lUnderline, lStrikeOut )
   ENDIF

RETURN hFont

/*----------------------------------------------------------------------*/
STATIC FUNCTION _BuildComboStyle( lDisplayChange, lNoTabStop, lInvisible, ;
                                  lSort, lUpper, lLower )
/*----------------------------------------------------------------------*/
   LOCAL nStyle

   nStyle := WS_CHILD + WS_VSCROLL + ;
             iif( lDisplayChange, CBS_DROPDOWN, CBS_DROPDOWNLIST )

   IF ! lNoTabStop
      nStyle += WS_TABSTOP
   ENDIF

   IF ! lInvisible
      nStyle += WS_VISIBLE
   ENDIF

   IF lSort
      nStyle += CBS_SORT
   ENDIF

   IF _HMG_IsXPorLater
      nStyle += CBS_NOINTEGRALHEIGHT
   ENDIF

   IF lUpper
      nStyle += CBS_UPPERCASE
   ENDIF

   IF lLower
      nStyle += CBS_LOWERCASE
   ENDIF

RETURN nStyle

/*-----------------------------------------------------------------------------*/
FUNCTION _DefineCombo( ControlName, ParentFormName, x, y, w, rows, value, ;
   FontName, FontSize, tooltip, changeprocedure, h, gotfocus, lostfocus, uEnter, ;
   HelpId, invisible, notabstop, sort, bold, italic, underline, strikeout, ;
   ItemSource, ValueSource, DisplayChange, OnDisplayChangeProcedure, break, ;
   GripperText, ListWidth, nId, OnListDisplayProcedure, OnListCloseProcedure, ;
   backcolor, fontcolor, lUpper, lLower, cuetext, OnCancel, AutoComplete, ;
   lShowDropDown, nItemHeight, bInit )
/*-----------------------------------------------------------------------------*/
   LOCAL ParentFormHandle
   LOCAL ControlHandle
   LOCAL FontHandle
   LOCAL ContainerHandle := 0
   LOCAL WorkArea
   LOCAL cField
   LOCAL mVar
   LOCAL k
   LOCAL i
   LOCAL blInit
   LOCAL Style
   LOCAL lDialogInMemory
   LOCAL oc := NIL
   LOCAL ow := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   hb_default( @w, 120 )
   hb_default( @h, 150 )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )
   hb_default( @sort, .F. )
   hb_default( @GripperText, "" )
   hb_default( @ListWidth, w )
   hb_default( @AutoComplete, .F. )
   hb_default( @lShowDropDown, .F. )

   __defaultNIL( @changeprocedure, "" )
   __defaultNIL( @gotfocus, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @rows, {} )
   __defaultNIL( @uEnter, "" )

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, ;
                         @bold, @italic, @underline, @strikeout )
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, ;
                             _HMG_ActiveDialogName, ;
                             _HMG_ActiveFormName )

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   IF ! _IsWindowDefined( ParentFormName ) .AND. ! lDialogInMemory
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
         "Control: " + ControlName + ;
         " Of " + ParentFormName + ;
         " Already defined." )
   ENDIF

   IF Sort .AND. ( ItemSource != NIL .OR. ValueSource != NIL )
      MsgMiniGuiError( ;
         "Sort clause can't be used with ItemSource or ValueSource." )
   ENDIF

   IF ItemSource != NIL

      IF hb_UAt( ">", ItemSource ) == 0

         MsgMiniGuiError( ;
            "Control: " + ControlName + ;
            " Of " + ParentFormName + ;
            " (ItemSource): You must specify a fully qualified field name." )

      ELSE

         WorkArea := hb_ULeft( ItemSource, ;
                     hb_UAt( ">", ItemSource ) - 2 )

         cField := hb_URight( ItemSource, ;
                    hb_ULen( ItemSource ) - ;
                    hb_UAt( ">", ItemSource ) )

      ENDIF

   ENDIF

   hb_default( @value, 0 )

   mVar := "_" + ParentFormName + "_" + ControlName
   k    := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := _BuildComboStyle( DisplayChange, notabstop, ;
                                 invisible, sort, ;
                                 lUpper, lLower )

      IF lDialogInMemory

         blInit := {|a,b,c| InitDialogComboBox( a, b, c ) }

         AAdd( _HMG_aDialogItems, ;
            { nId, k, "COMBOBOX", Style, 0, x, y, w, h, "", ;
              HelpId, tooltip, FontName, FontSize, ;
              bold, italic, underline, strikeout, ;
              blInit, _HMG_BeginTabActive, .F., ;
              _HMG_ActiveTabPage } )

      ELSE

         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )
         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         FontHandle := _ApplyComboFont( ;
            ControlHandle, FontHandle, ;
            FontName, FontSize, ;
            bold, italic, underline, strikeout )

         SetWindowStyle( ControlHandle, Style, .T. )

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle( ParentFormName )

      IF x == NIL .OR. y == NIL

         _HMG_SplitLastControl := "COMBOBOX"

         i := GetFormIndex( ParentFormName )

         IF i > 0

            ControlHandle := InitComboBox( ;
               _HMG_aFormReBarHandle[i], ;
               0, x, y, w, ;
               lUpper, lLower, h, ;
               invisible, notabstop, sort, ;
               DisplayChange, ;
               _HMG_IsXPorLater )

            FontHandle := _ApplyComboFont( ;
               ControlHandle, FontHandle, ;
               FontName, FontSize, ;
               bold, italic, underline, strikeout )

            AddSplitBoxItem( ;
               ControlHandle, ;
               _HMG_aFormReBarHandle[i], ;
               w, break, GripperText, w, ;
               , _HMG_ActiveSplitBoxInverted )

            ContainerHandle := _HMG_aFormReBarHandle[i]

         ENDIF

      ELSE

         ControlHandle := InitComboBox( ;
            ParentFormHandle, ;
            0, x, y, w, ;
            lUpper, lLower, h, ;
            invisible, notabstop, sort, ;
            DisplayChange, ;
            _HMG_IsXPorLater )

      ENDIF

   ENDIF

   IF ! lDialogInMemory

      FontHandle := _ApplyComboFont( ;
         ControlHandle, FontHandle, ;
         FontName, FontSize, ;
         bold, italic, underline, strikeout )

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
      ENDIF

      IF _HMG_IsThemed .AND. ;
         ( IsArrayRGB( backcolor ) .OR. ;
           IsArrayRGB( fontcolor ) ) .AND. ;
         ! DisplayChange

         SetWindowTheme( ControlHandle, "", "" )
      ENDIF

      IF tooltip != NIL
         SetToolTip( ;
            ControlHandle, ;
            tooltip, ;
            GetFormToolTipHandle( ParentFormName ) )
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType[k]               := "COMBO"
   _HMG_aControlNames[k]              := ControlName
   _HMG_aControlHandles[k]            := ControlHandle
   _HMG_aControlParentHandles[k]      := ParentFormHandle
   _HMG_aControlIds[k]                := nId
   _HMG_aControlProcedures[k]         := OnDisplayChangeProcedure
   _HMG_aControlPageMap[k]            := cField
   _HMG_aControlValue[k]              := Value
   _HMG_aControlInputMask[k]          := OnListDisplayProcedure
   _HMG_aControlLostFocusProcedure[k] := lostfocus
   _HMG_aControlGotFocusProcedure[k]  := gotfocus
   _HMG_aControlChangeProcedure[k]    := changeprocedure
   _HMG_aControlDeleted[k]            := .F.
   _HMG_aControlBkColor[k]            := backcolor
   _HMG_aControlFontColor[k]          := fontcolor
   _HMG_aControlDblClick[k]           := uEnter
   _HMG_aControlHeadClick[k]          := {}
   _HMG_aControlRow[k]                := y
   _HMG_aControlCol[k]                := x
   _HMG_aControlWidth[k]              := w
   _HMG_aControlHeight[k]             := h
   _HMG_aControlSpacing[k]            := WorkArea
   _HMG_aControlContainerRow[k]       := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveFrameRow[_HMG_FrameLevel], -1 )

   _HMG_aControlContainerCol[k]       := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveFrameCol[_HMG_FrameLevel], -1 )

   _HMG_aControlPicture[k]            := OnListCloseProcedure
   _HMG_aControlContainerHandle[k]    := ContainerHandle
   _HMG_aControlFontName[k]           := FontName
   _HMG_aControlFontSize[k]           := FontSize
   _HMG_aControlFontAttributes[k]     := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[k]            := tooltip
   _HMG_aControlRangeMin[k]           := 0
   _HMG_aControlRangeMax[k]           := 0
   _HMG_aControlCaption[k]            := ValueSource
   _HMG_aControlVisible[k]            := ! invisible
   _HMG_aControlHelpId[k]             := HelpId
   _HMG_aControlFontHandle[k]         := FontHandle
   _HMG_aControlBrushHandle[k]        := 0
   _HMG_aControlEnabled[k]            := .T.

   _HMG_aControlMiscData1[k] := ;
      { 0, DisplayChange, ItemSource, rows, ;
        ListWidth, cuetext, AutoComplete, ;
        lShowDropDown, 0, OnCancel, ;
        nItemHeight }

   _HMG_aControlMiscData2[k] := ""

   IF Len( _HMG_aDialogTemplate ) == 0
      InitDialogComboBox( ParentFormName, ControlHandle, k )
   ENDIF

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, k, mVar )

#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif

   ENDIF

   Do_ControlEventProcedure( bInit, k, ow, oc )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogComboBox( ParentName, ControlHandle, k )
*-----------------------------------------------------------------------------*
   LOCAL WorkArea, BackRec, rcount := 0, cset := 0, ItemHeight
   LOCAL Value, rows, DisplayChange, ItemSource, cField, ListWidth, cuetext

   WorkArea := _HMG_aControlSpacing[k]
   cField := _HMG_aControlPageMap[k]
   Value := _HMG_aControlValue[k]
   rows := _HMG_aControlMiscData1[k,4]
   DisplayChange := _HMG_aControlMiscData1[k,2]
   ItemSource := _HMG_aControlMiscData1[k,3]
   ListWidth := _HMG_aControlMiscData1[k,5]
   cuetext := _HMG_aControlMiscData1[k,6]
   ItemHeight := _HMG_aControlMiscData1[k,11]

   IF DisplayChange
      _HMG_aControlRangeMin[k] := FindWindowEx( ControlHandle, 0, "Edit", Nil )
      IF _HMG_aControlToolTip[k] != NIL
         SetToolTip( _HMG_aControlRangeMin[k], _HMG_aControlToolTip[k], GetFormToolTipHandle( ParentName ) )
      ENDIF
      IF !Empty( cuetext ) .AND. IsVistaOrLater()
         value := 0
         SendMessageWideString( _HMG_aControlRangeMin[k], EM_SETCUEBANNER, .T., cuetext )
      ENDIF
   ELSEIF !Empty( cuetext ) .AND. IsVistaOrLater()
      value := 0
      SendMessageWideString( ControlHandle, CB_SETCUEBANNER, .T., cuetext )
   ENDIF

   SetDropDownWidth( ControlHandle, ListWidth )

   IF ValType( WorkArea ) == "C"
      IF Select( WorkArea ) != 0
         BackRec := ( WorkArea )->( RecNo() )
         ( WorkArea )->( dbGoTop() )
         DO WHILE ! ( WorkArea )->( EOF() )
            rcount++
            IF value == ( WorkArea )->( RecNo() )
               cset := rcount
            ENDIF
            ComboAddString( ControlHandle, cValToChar( ( WorkArea )->&( cField ) ) )
            ( WorkArea )->( dbSkip() )
         ENDDO
         ( WorkArea )->( dbGoto( BackRec ) )
         ComboSetCurSel( ControlHandle, cset )
      ENDIF
   ELSE
      IF Len( rows ) > 0
         AEval( rows, { |v| ComboAddString( ControlHandle, v ) } )
      ENDIF
      IF ISNUMBER( value ) .AND. value <> 0
         ComboSetCurSel( ControlHandle, Value )
      ENDIF
   ENDIF

   IF ItemHeight != NIL
      ComboSetItemHeight( ControlHandle, ItemHeight )
   ENDIF

   IF ItemSource != NIL
      AAdd( _HMG_aFormBrowseList[ GetFormIndex( ParentName ) ], k )
   ENDIF

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[3]
      _HMG_aControlDeleted[k] := .T.
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
PROCEDURE _DataComboRefresh( i )
*-----------------------------------------------------------------------------*
   LOCAL BackValue, BackRec, WorkArea, cField, ControlHandle

   IF Empty( _HMG_aControlCaption[i] )
      BackValue := _GetValue( , , i )
   ELSE
      cField := _HMG_aControlCaption[i]
      _HMG_aControlCaption[i] := ""
      BackValue := _GetValue( , , i )
      _HMG_aControlCaption[i] := cField
   ENDIF

   cField := _HMG_aControlPageMap[i]
   ControlHandle := _HMG_aControlHandles[i]
   WorkArea := _HMG_aControlSpacing[i]

   BackRec := ( WorkArea )->( RecNo() )
   ( WorkArea )->( dbGoTop() )
   ComboboxReset( ControlHandle )

   DO WHILE ! ( WorkArea )->( EOF() )
      IF _HMG_aControlMiscData1[i,1] <> 1
         ComboAddString( ControlHandle, cValToChar( ( WorkArea )->&( cField ) ) )
      ELSE
         ComboAddDataStringEx( ControlHandle, cValToChar( ( WorkArea )->&( cField ) ) )
      ENDIF
      ( WorkArea )->( dbSkip() )
   ENDDO

   ( WorkArea )->( dbGoto( BackRec ) )

   IF BackValue > 0 .AND. BackValue <= ( WorkArea )->( LastRec() )
      _SetValue( , , BackValue, i )
   ENDIF

RETURN
