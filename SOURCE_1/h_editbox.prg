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
   #xtranslate hb_UAt( <c>, <n> )   => At( <c>, <n> )
   #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
#endif

/*----------------------------------------------------------------------*/
/* Helpers                                                              */
/*----------------------------------------------------------------------*/

STATIC FUNCTION ApplyControlFont( hWnd, FontName, FontSize, ;
      bold, italic, underline, strikeout )

   LOCAL FontHandle

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         FontHandle, ;
         @FontName, ;
         @FontSize, ;
         @bold, ;
         @italic, ;
         @underline, ;
         @strikeout )

      _SetFontHandle( hWnd, FontHandle )

   ELSE

      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )

      FontHandle := _SetFont( ;
         hWnd, ;
         FontName, ;
         FontSize, ;
         bold, ;
         italic, ;
         underline, ;
         strikeout )

   ENDIF

RETURN FontHandle

STATIC PROCEDURE ApplyEditValue( hWnd, Value )

   IF ValType( Value ) $ "CM" .AND. !Empty( Value )
      SetWindowText( hWnd, Value )
   ENDIF

RETURN

STATIC FUNCTION BuildEditStyle( noborder, notabstop, invisible, ;
      novscroll, nohscroll )

   LOCAL Style

   Style := ES_MULTILINE + ;
            ES_WANTRETURN + ;
            WS_CHILD + ;
            iif( noborder, 0, WS_BORDER )

   IF !notabstop
      Style += WS_TABSTOP
   ENDIF

   IF !invisible
      Style += WS_VISIBLE
   ENDIF

   IF !novscroll
      Style += WS_VSCROLL
   ELSE
      Style += ES_AUTOVSCROLL
   ENDIF

   IF !nohscroll
      Style += WS_HSCROLL
   ENDIF

RETURN Style

STATIC PROCEDURE ResolveFieldValue( Field, Value )

   LOCAL WorkArea

   IF Field == NIL
      RETURN
   ENDIF

   IF hb_UAt( ">", Field ) == 0

      MsgMiniGuiError( ;
         "You must specify a fully qualified field name." )

      RETURN

   ENDIF

   WorkArea := hb_ULeft( ;
      Field, ;
      hb_UAt( ">", Field ) - 2 )

   IF Select( WorkArea ) != 0
      Value := &( Field )
   ENDIF

RETURN

/*----------------------------------------------------------------------*/
/* Main EditBox Definition                                              */
/*----------------------------------------------------------------------*/

FUNCTION _DefineEditbox( ControlName, ParentFormName, x, y, w, h, value, ;
      fontname, fontsize, tooltip, maxlength, gotfocus, change, ;
      lostfocus, readonly, break, HelpId, invisible, notabstop, ;
      bold, italic, underline, strikeout, field, backcolor, ;
      fontcolor, novscroll, nohscroll, noborder, nId, bInit )

   LOCAL ParentFormHandle
   LOCAL ControlHandle := 0
   LOCAL FontHandle := 0
   LOCAL ContainerHandle := 0

   LOCAL mVar
   LOCAL k
   LOCAL nFormIndex

   LOCAL Style
   LOCAL blInit

   LOCAL lDialogInMemory

   LOCAL oc := NIL
   LOCAL ow := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /* Defaults */

   hb_default( @w, 120 )
   hb_default( @h, 240 )

   hb_default( @value, "" )
   hb_default( @maxlength, 64738 )

   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )

   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   /* Resolve field binding */

   ResolveFieldValue( Field, @Value )

   /* Active form context */

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := ;
         iif( ;
            _HMG_BeginDialogActive, ;
            _HMG_ActiveDialogName, ;
            _HMG_ActiveFormName )

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /* Frame offset */

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /* Validation */

   IF !_IsWindowDefined( ParentFormName ) .AND. !lDialogInMemory

      MsgMiniGuiError( ;
         "Window: " + ;
         IFNIL( ParentFormName, "Parent", ParentFormName ) + ;
         " is not defined." )

   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ;
      !lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentFormName + ;
         " Already defined." )

   ENDIF

   /* Registration setup */

   mVar := "_" + ParentFormName + "_" + ControlName
   k    := _GetControlFree()

   /* Build control */

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      Style := BuildEditStyle( ;
         noborder, ;
         notabstop, ;
         invisible, ;
         novscroll, ;
         nohscroll )

      /* Dialog template mode */

      IF lDialogInMemory

         blInit := {|x, y, z| InitDialogEdit( x, y, z ) }

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               k, ;
               "edit", ;
               Style, ;
               0, ;
               x, y, w, h, ;
               value, ;
               HelpId, ;
               tooltip, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout, ;
               blInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         /* Runtime dialog control */

         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )

         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         ApplyEditValue( ControlHandle, Value )

         SetWindowStyle( ;
            ControlHandle, ;
            Style, ;
            .T. )

      ENDIF

   ELSE

      /* Standard window mode */

      ParentFormHandle := GetFormHandle( ParentFormName )

      IF x == NIL .OR. y == NIL

         IF _HMG_SplitLastControl == "TOOLBAR"
            Break := .T.
         ENDIF

         _HMG_SplitLastControl := "EDIT"

         nFormIndex := GetFormIndex( ParentFormName )

         IF nFormIndex > 0

            ControlHandle := InitEditBox( ;
               ParentFormHandle, ;
               0, ;
               x, y, w, h, ;
               "", ;
               noborder, ;
               maxlength, ;
               readonly, ;
               invisible, ;
               notabstop, ;
               novscroll, ;
               nohscroll )

            FontHandle := ApplyControlFont( ;
               ControlHandle, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout )

            AddSplitBoxItem( ;
               ControlHandle, ;
               _HMG_aFormReBarHandle[ nFormIndex ], ;
               w, ;
               break, ;
               , , , ;
               _HMG_ActiveSplitBoxInverted )

            ContainerHandle := ;
               _HMG_aFormReBarHandle[ nFormIndex ]

            ApplyEditValue( ControlHandle, Value )

         ENDIF

      ELSE

         /* Standard positioned edit */

         ControlHandle := InitEditBox( ;
            ParentFormHandle, ;
            0, ;
            x, y, w, h, ;
            "", ;
            noborder, ;
            maxlength, ;
            readonly, ;
            invisible, ;
            notabstop, ;
            novscroll, ;
            nohscroll )

         ApplyEditValue( ControlHandle, Value )

      ENDIF

   ENDIF

   /* Runtime setup */

   IF !lDialogInMemory

      IF _HMG_BeginTabActive
         AAdd( ;
            _HMG_ActiveTabCurrentPageMap, ;
            ControlHandle )
      ENDIF

      IF FontHandle == 0

         FontHandle := ApplyControlFont( ;
            ControlHandle, ;
            FontName, ;
            FontSize, ;
            bold, ;
            italic, ;
            underline, ;
            strikeout )

      ENDIF

      IF tooltip != NIL

         SetToolTip( ;
            ControlHandle, ;
            tooltip, ;
            GetFormToolTipHandle( ParentFormName ) )

      ENDIF

   ENDIF

   /* Register control */

#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType[ k ]              := "EDIT"
   _HMG_aControlNames[ k ]             := ControlName
   _HMG_aControlHandles[ k ]           := ControlHandle
   _HMG_aControlParenthandles[ k ]     := ParentFormHandle

   _HMG_aControlIds[ k ]               := nId

   _HMG_aControlProcedures[ k ]        := ""
   _HMG_aControlPageMap[ k ]           := Field

   _HMG_aControlValue[ k ]             := NIL
   _HMG_aControlInputMask[ k ]         := ""

   _HMG_aControlLostFocusProcedure[ k ] := lostfocus
   _HMG_aControlGotFocusProcedure[ k ]  := gotfocus
   _HMG_aControlChangeProcedure[ k ]    := change

   _HMG_aControlDeleted[ k ]           := .F.

   _HMG_aControlBkColor[ k ]           := backcolor
   _HMG_aControlFontColor[ k ]         := fontcolor

   _HMG_aControlDblClick[ k ]          := ""
   _HMG_aControlHeadClick[ k ]         := {}

   _HMG_aControlRow[ k ]               := y
   _HMG_aControlCol[ k ]               := x

   _HMG_aControlWidth[ k ]             := w
   _HMG_aControlHeight[ k ]            := h

   _HMG_aControlSpacing[ k ]           := 0

   _HMG_aControlContainerRow[ k ]      := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlContainerCol[ k ]      := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlPicture[ k ]           := ""

   _HMG_aControlContainerHandle[ k ]   := ;
      ContainerHandle

   _HMG_aControlFontName[ k ]          := fontname
   _HMG_aControlFontSize[ k ]          := fontsize

   _HMG_aControlFontAttributes[ k ]    := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ k ]           := tooltip

   _HMG_aControlRangeMin[ k ]          := 0
   _HMG_aControlRangeMax[ k ]          := 0

   _HMG_aControlCaption[ k ]           := ""

   _HMG_aControlVisible[ k ]           := ;
      ! invisible

   _HMG_aControlHelpId[ k ]            := HelpId

   _HMG_aControlFontHandle[ k ]        := FontHandle
   _HMG_aControlBrushHandle[ k ]       := 0

   _HMG_aControlEnabled[ k ]           := .T.

   _HMG_aControlMiscData1[ k ]         := ;
      { 0, maxlength, readonly }

   _HMG_aControlMiscData2[ k ]         := ""

   /* Dialog runtime initialization */

   IF Len( _HMG_aDialogTemplate ) == 0
      InitDialogEdit( ParentFormName, ControlHandle, k )
   ENDIF

   /* Browse binding */

   IF Field != NIL

      AAdd( ;
         _HMG_aFormBrowseList[ ;
            GetFormIndex( ParentFormName ) ;
         ], ;
         k )

   ENDIF

   /* OOP integration */

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, k, mVar )

#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif

   ENDIF

   Do_ControlEventProcedure( bInit, k, ow, oc )

RETURN NIL

/*----------------------------------------------------------------------*/
/* Refresh                                                              */
/*----------------------------------------------------------------------*/

PROCEDURE _DataEditBoxRefresh( i )

   LOCAL Field := _HMG_aControlPageMap[ i ]
   LOCAL nCaretPos

   IF Field != NIL

      _SetValue( , , &Field, i )

   ELSE

      nCaretPos := ;
         HiWord( ;
            SendMessage( ;
               _HMG_aControlHandles[ i ], ;
               EM_GETSEL, ;
               0, ;
               0 ) )

      _SetValue( ;
         , ;
         , ;
         _GetValue( , , i ), ;
         i )

      SendMessage( ;
         _HMG_aControlHandles[ i ], ;
         EM_SETSEL, ;
         nCaretPos, ;
         nCaretPos )

   ENDIF

RETURN

/*----------------------------------------------------------------------*/
/* Dialog Init                                                          */
/*----------------------------------------------------------------------*/

FUNCTION InitDialogEdit( ParentName, ControlHandle, k )

   LOCAL maxlength
   LOCAL readonly

   ParentName := NIL

   maxlength := _HMG_aControlMiscData1[ k, 2 ]
   readonly  := _HMG_aControlMiscData1[ k, 3 ]

   IF ISLOGICAL( readonly )

      SendMessage( ;
         ControlHandle, ;
         EM_SETREADONLY, ;
         iif( readonly, 1, 0 ), ;
         0 )

   ENDIF

   IF maxlength != NIL

      SendMessage( ;
         ControlHandle, ;
         EM_LIMITTEXT, ;
         maxlength, ;
         0 )

   ENDIF

   /* Modal dialog template */

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
      _HMG_aDialogTemplate[ 3 ]

      _HMG_aControlDeleted[ k ] := .T.

   ENDIF

RETURN NIL
