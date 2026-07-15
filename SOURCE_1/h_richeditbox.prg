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

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
  #xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
  #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
#endif

*-----------------------------------------------------------------------------*
FUNCTION _DefineRichEditBox( ControlName, ParentForm, x, y, w, h, value, ;
      fontname, fontsize, tooltip, maxlength, gotfocus, change, lostfocus, readonly, ;
      break, HelpId, invisible, notabstop, bold, italic, underline, strikeout, file, ;
      field, backcolor, fontcolor, plaintext, nohscroll, novscroll, select, vscroll )
*-----------------------------------------------------------------------------*
   LOCAL ControlHandle, FontHandle
   LOCAL cParentForm, mVar, k, ContainerHandle := 0
   LOCAL WorkArea, i

   hb_default( @w, 120 )
   hb_default( @h, 240 )
   hb_default( @file, "" )
   hb_default( @field, "" )
   hb_default( @value, "" )
   hb_default( @maxlength, 64738 )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )
   hb_default( @nohscroll, .F. )
   hb_default( @plaintext, .F. )

   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   IF ( FontHandle := GetFontHandle( fontname ) ) != 0
      GetFontParamByRef( FontHandle, @fontname, @fontsize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF !Empty( field )
      IF hb_UAt( '>', field ) == 0
         MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " : You must specify a fully qualified field name." )
      ELSE
         WorkArea := hb_ULeft( field, hb_UAt( '>', field ) - 2 )
         IF Select( WorkArea ) != 0
            value := &( field )
         ENDIF
      ENDIF
   ENDIF

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
      __defaultNIL( @fontname, _HMG_ActiveFontName )
      __defaultNIL( @fontsize, _HMG_ActiveFontSize )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF ! _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + IFNIL( ParentForm, "Parent", ParentForm ) + " is not defined." )
   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   mVar := '_' + ParentForm + '_' + ControlName
   cParentForm := ParentForm
   ParentForm := GetFormHandle( ParentForm )

   IF x == NIL .OR. y == NIL
      IF _HMG_SplitLastControl == 'TOOLBAR'
         break := .T.
      ENDIF
      _HMG_SplitLastControl := 'RICHEDIT'

      i := GetFormIndex( cParentForm )
      IF i > 0
         ControlHandle := InitRichEditBox( _HMG_aFormReBarHandle[i], 0, x, y, w, h, '', 0, maxlength, ;
            readonly, invisible, notabstop, nohscroll, novscroll )

         IF FontHandle != 0
            _SetFontHandle( ControlHandle, FontHandle )
         ELSE
            __defaultNIL( @fontname, _HMG_DefaultFontName )
            __defaultNIL( @fontsize, _HMG_DefaultFontSize )
            FontHandle := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
         ENDIF

         AddSplitBoxItem( ControlHandle, _HMG_aFormReBarHandle[i], w, break, , , , _HMG_ActiveSplitBoxInverted )
         ContainerHandle := _HMG_aFormReBarHandle[i]
      ENDIF
   ELSE
      ControlHandle := InitRichEditBox( ParentForm, 0, x, y, w, h, '', 0, maxlength, ;
         readonly, invisible, notabstop, nohscroll, novscroll )

      IF FontHandle != 0
         _SetFontHandle( ControlHandle, FontHandle )
      ELSE
         __defaultNIL( @fontname, _HMG_DefaultFontName )
         __defaultNIL( @fontsize, _HMG_DefaultFontSize )
         FontHandle := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
      ENDIF

      SetFontRTF( ControlHandle, IFARRAY( fontcolor, -1, 0 ), fontname, fontsize, bold, italic, ;
         iif( IsArrayRGB( fontcolor ), RGB( fontcolor[1], fontcolor[2], fontcolor[3] ), NIL ), underline, strikeout )
   ENDIF

   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF

   IF tooltip != NIL
      SetToolTip( ControlHandle, tooltip, GetFormToolTipHandle( cParentForm ) )
   ENDIF

   IF IsArrayRGB( backcolor )
      SetBkgndColor( ControlHandle, .T., backcolor[1], backcolor[2], backcolor[3] )
   ENDIF

   IF File( file )
      StreamIn( ControlHandle, file, iif( plaintext, 1, 2 ) )
   ELSE
      file := ""
   ENDIF

   k := _GetControlFree()
#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   Public &mVar. := k
#endif

   _HMG_aControlType[k] := "RICHEDIT"
   _HMG_aControlNames[k] := ControlName
   _HMG_aControlHandles[k] := ControlHandle
   _HMG_aControlParenthandles[k] := ParentForm
   _HMG_aControlIds[k] := 0
   _HMG_aControlProcedures[k] := vscroll
   _HMG_aControlPageMap[k] := field
   _HMG_aControlValue[k] := Nil
   _HMG_aControlInputMask[k] := ""
   _HMG_aControllostFocusProcedure[k] := lostfocus
   _HMG_aControlGotFocusProcedure[k] := gotfocus
   _HMG_aControlChangeProcedure[k] := change
   _HMG_aControlDeleted[k] := .F.
   _HMG_aControlBkColor[k] := backcolor
   _HMG_aControlFontColor[k] := fontcolor
   _HMG_aControlDblClick[k] := select
   _HMG_aControlHeadClick[k] := {}
   _HMG_aControlRow[k] := y
   _HMG_aControlCol[k] := x
   _HMG_aControlWidth[k] := w
   _HMG_aControlHeight[k] := h
   _HMG_aControlSpacing[k] := 0
   _HMG_aControlContainerRow[k] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1 )
   _HMG_aControlContainerCol[k] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1 )
   _HMG_aControlPicture[k] := ""
   _HMG_aControlContainerHandle[k] := ContainerHandle
   _HMG_aControlFontName[k] := fontname
   _HMG_aControlFontSize[k] := fontsize
   _HMG_aControlFontAttributes[k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[k] := tooltip
   _HMG_aControlRangeMin[k] := 0
   _HMG_aControlRangeMax[k] := 0
   _HMG_aControlCaption[k] := file
   _HMG_aControlVisible[k] := !invisible
   _HMG_aControlHelpId[k] := HelpId
   _HMG_aControlFontHandle[k] := FontHandle
   _HMG_aControlBrushHandle[k] := 0
   _HMG_aControlEnabled[k] := .T.
   _HMG_aControlMiscData1[k] := 0
   _HMG_aControlMiscData2[k] := ''

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
   ENDIF

   IF Len( value ) > 0
      _SetValue( , , value, k )
   ENDIF

RETURN Nil


*-----------------------------------------------------------------------------*
PROCEDURE _DataBaseRichEditBoxSave( ControlName, ParentForm, typ )
*-----------------------------------------------------------------------------*
   LOCAL cTempFile := TempFile( GetTempFolder(), 'txt' )
   LOCAL Field, i

   IF Empty( cTempFile )
      RETURN
   ENDIF

   i := GetControlIndex( ControlName, ParentForm )
   Field := _HMG_aControlPageMap[i]

   _DataRichEditBoxSave( ControlName, ParentForm, cTempFile, typ )

   IF _IsFieldExists( Field )
      REPLACE &Field WITH MemoRead( cTempFile )
   ENDIF

   FErase( cTempFile )

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _DataRichEditBoxSetValue( ControlName, ParentForm, cRichValue, typ )
*-----------------------------------------------------------------------------*
   LOCAL cTempFile := TempFile( GetTempFolder(), 'txt' )

   IF !Empty( cTempFile )
      hb_MemoWrit( cTempFile, cRichValue )
      _DataRichEditBoxOpen( ControlName, ParentForm, cTempFile, typ )
      FErase( cTempFile )
   ENDIF

RETURN cRichValue

*-----------------------------------------------------------------------------*
FUNCTION _DataRichEditBoxGetValue( ControlName, ParentForm, typ )
*-----------------------------------------------------------------------------*
   LOCAL cTempFile := TempFile( GetTempFolder(), 'txt' )
   LOCAL cRichValue := ""

   IF !Empty( cTempFile )
      _DataRichEditBoxSave( ControlName, ParentForm, cTempFile, typ )
      cRichValue := MemoRead( cTempFile )
      FErase( cTempFile )
   ENDIF

RETURN cRichValue

*-----------------------------------------------------------------------------*
PROCEDURE _DataRichEditBoxOpen( ControlName, ParentForm, cFile, typ )
*-----------------------------------------------------------------------------*
   LOCAL i := GetControlIndex( ControlName, ParentForm )

   IF i > 0
      _HMG_aControlCaption[i] := cFile
      StreamIn( GetControlHandle( ControlName, ParentForm ), cFile, hb_defaultValue( typ, 2 ) )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _DataRichEditBoxSave( ControlName, ParentForm, cFile, typ )
*-----------------------------------------------------------------------------*
   LOCAL i := GetControlIndex( ControlName, ParentForm )

   IF i > 0
      IF cFile == NIL
         cFile := _HMG_aControlCaption[i]
      ENDIF
      StreamOut( GetControlHandle( ControlName, ParentForm ), cFile, hb_defaultValue( typ, 2 ) )
   ENDIF

RETURN