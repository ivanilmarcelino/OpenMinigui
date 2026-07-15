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
FUNCTION _DefineListbox( ControlName, ParentFormName, x, y, w, h, aRows, value, ;
      fontName, fontSize, tooltip, changeProcedure, dblClick, gotFocus, ;
      lostFocus, lBreak, HelpId, invisible, noTabStop, lSort, ;
      bold, italic, underline, strikeout, backColor, fontColor, ;
      multiSelect, dragItems, multiColumn, multiTabs, aWidth, nId )
*-----------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL ControlHandle := 0
   LOCAL FontHandle
   LOCAL mVar
   LOCAL k
   LOCAL nStyle
   LOCAL blInit
   LOCAL rows
   LOCAL i
   LOCAL lDialogInMemory

   hb_default( @w, 120 )
   hb_default( @h, 120 )
   hb_default( @value, 0 )

   __defaultNIL( @gotFocus, "" )
   __defaultNIL( @lostFocus, "" )
   __defaultNIL( @changeProcedure, "" )
   __defaultNIL( @dblClick, "" )

   hb_default( @invisible, .F. )
   hb_default( @noTabStop, .F. )
   hb_default( @lSort, .F. )
   hb_default( @multiColumn, .F. )
   hb_default( @multiTabs, .F. )
   hb_default( @aWidth, {} )

   /*
    * Font normalization
    */
   IF ( FontHandle := GetFontHandle( fontName ) ) != 0
      GetFontParamByRef( FontHandle, ;
         @fontName, @fontSize, ;
         @bold, @italic, @underline, @strikeout )
   ENDIF

   /*
    * Clone rows to avoid side effects
    */
   rows := iif( ISARRAY( aRows ), AClone( aRows ), {} )

   /*
    * Multi-tab preprocessing
    */
   IF multiTabs .AND. Len( rows ) > 0

      IF Len( aWidth ) == 0 .AND. ISARRAY( rows[ 1 ] )

         FOR i := 1 TO Len( rows[ 1 ] )
            AAdd( aWidth, Int( w / Len( rows[ 1 ] ) ) )
         NEXT

      ENDIF

      FOR i := 1 TO Len( rows )

         IF ISARRAY( rows[ i ] )
            rows[ i ] := LB_Array2String( rows[ i ] )
         ENDIF

      NEXT

   ENDIF

   /*
    * Active window/dialog defaults
    */
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := iif( ;
         _HMG_BeginDialogActive, ;
         _HMG_ActiveDialogName, ;
         _HMG_ActiveFormName )

      __defaultNIL( @fontName, _HMG_ActiveFontName )
      __defaultNIL( @fontSize, _HMG_ActiveFontSize )

   ENDIF

   /*
    * Frame offset adjustment
    */
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /*
    * Validation
    */
   IF .NOT. _IsWindowDefined( ParentFormName ) .AND. ;
      .NOT. lDialogInMemory

      MsgMiniGuiError( ;
         "Window: " + ;
         IFNIL( ParentFormName, "Parent", ParentFormName ) + ;
         " is not defined." )

   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. ;
      .NOT. lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ControlName + ;
         " Of " + ParentFormName + ;
         " Already defined." )

   ENDIF

   /*
    * Internal registration index
    */
   mVar := "_" + ParentFormName + "_" + ControlName
   k    := _GetControlFree()

   /*
    * Dialog mode
    */
   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := WS_BORDER + ;
                WS_CHILD + ;
                WS_VSCROLL + ;
                LBS_DISABLENOSCROLL + ;
                LBS_NOTIFY + ;
                LBS_NOINTEGRALHEIGHT

      IF multiSelect
         nStyle += LBS_MULTIPLESEL
      ENDIF

      IF ! noTabStop
         nStyle += WS_TABSTOP
      ENDIF

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lSort
         nStyle += LBS_SORT
      ENDIF

      IF multiColumn
         nStyle += LBS_MULTICOLUMN
      ENDIF

      IF multiTabs
         nStyle += LBS_USETABSTOPS
      ENDIF

      IF lDialogInMemory

         /*
          * Dialog template
          */
         blInit := {|x, y, z| InitDialogListBox( x, y, z ) }

         AAdd( _HMG_aDialogItems, ;
            { ;
               nId, ;
               k, ;
               "LISTBOX", ;
               nStyle, ;
               0, ;
               x, y, w, h, ;
               "", ;
               HelpId, ;
               tooltip, ;
               fontName, ;
               fontSize, ;
               bold, italic, underline, strikeout, ;
               blInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         /*
          * Existing dialog control
          */
         ControlHandle := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         x := GetWindowCol( ControlHandle )
         y := GetWindowRow( ControlHandle )

         w := GetWindowWidth( ControlHandle )
         h := GetWindowHeight( ControlHandle )

         SetWindowStyle( ControlHandle, nStyle, .T. )

      ENDIF

   ELSE

      /*
       * Standard window mode
       */
      ParentFormHandle := GetFormHandle( ParentFormName )

      IF x == NIL .OR. y == NIL

         /*
          * Splitbox mode
          */
         IF _HMG_SplitLastControl == "TOOLBAR"
            lBreak := .T.
         ENDIF

         i := GetFormIndex( ParentFormName )

         IF i > 0

            IF multiSelect

               ControlHandle := InitMultiListBox( ;
                  _HMG_aFormReBarHandle[ i ], ;
                  0, x, y, w, h, ;
                  fontName, fontSize, ;
                  invisible, noTabStop, ;
                  lSort, dragItems, ;
                  multiTabs, multiColumn )

            ELSE

               ControlHandle := InitListBox( ;
                  _HMG_aFormReBarHandle[ i ], ;
                  0, 0, 0, ;
                  w, h, ;
                  "", 0, ;
                  invisible, noTabStop, ;
                  lSort, dragItems, ;
                  multiTabs, multiColumn )

            ENDIF

            AddSplitBoxItem( ;
               ControlHandle, ;
               _HMG_aFormReBarHandle[ i ], ;
               w, ;
               lBreak, ;
               , , , ;
               _HMG_ActiveSplitBoxInverted )

            _HMG_SplitLastControl := "LISTBOX"

         ENDIF

      ELSE

         /*
          * Standard positioned control
          */
         IF multiSelect

            ControlHandle := InitMultiListBox( ;
               ParentFormHandle, ;
               0, x, y, w, h, ;
               fontName, fontSize, ;
               invisible, noTabStop, ;
               lSort, dragItems, ;
               multiTabs, multiColumn )

         ELSE

            ControlHandle := InitListBox( ;
               ParentFormHandle, ;
               0, x, y, w, h, ;
               "", 0, ;
               invisible, noTabStop, ;
               lSort, dragItems, ;
               multiTabs, multiColumn )

         ENDIF

      ENDIF

   ENDIF

   /*
    * Runtime-only initialization
    */
   IF .NOT. lDialogInMemory

      IF FontHandle != 0

         _SetFontHandle( ControlHandle, FontHandle )

      ELSE

         __defaultNIL( @fontName, _HMG_DefaultFontName )
         __defaultNIL( @fontSize, _HMG_DefaultFontSize )

         FontHandle := _SetFont( ;
            ControlHandle, ;
            fontName, ;
            fontSize, ;
            bold, italic, underline, strikeout )

      ENDIF

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
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

   /*
    * Control metadata registration
    */
   _HMG_aControlType[ k ]               := iif( multiSelect, "MULTILIST", "LIST" )
   _HMG_aControlNames[ k ]              := ControlName
   _HMG_aControlHandles[ k ]            := ControlHandle
   _HMG_aControlParentHandles[ k ]      := ParentFormHandle
   _HMG_aControlIds[ k ]                := nId
   _HMG_aControlProcedures[ k ]         := ""
   _HMG_aControlPageMap[ k ]            := {}
   _HMG_aControlValue[ k ]              := value
   _HMG_aControlInputMask[ k ]          := ""
   _HMG_aControlLostFocusProcedure[ k ] := lostFocus
   _HMG_aControlGotFocusProcedure[ k ]  := gotFocus
   _HMG_aControlChangeProcedure[ k ]    := changeProcedure
   _HMG_aControlDeleted[ k ]            := .F.
   _HMG_aControlBkColor[ k ]            := backColor
   _HMG_aControlFontColor[ k ]          := fontColor
   _HMG_aControlDblClick[ k ]           := dblClick
   _HMG_aControlHeadClick[ k ]          := {}

   _HMG_aControlRow[ k ]                := y
   _HMG_aControlCol[ k ]                := x
   _HMG_aControlWidth[ k ]              := w
   _HMG_aControlHeight[ k ]             := h
   _HMG_aControlSpacing[ k ]            := 0

   _HMG_aControlContainerRow[ k ] := ;
      iif( _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )

   _HMG_aControlContainerCol[ k ] := ;
      iif( _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )

   _HMG_aControlPicture[ k ]         := ""
   _HMG_aControlContainerHandle[ k ] := 0

   _HMG_aControlFontName[ k ]       := fontName
   _HMG_aControlFontSize[ k ]       := fontSize

   _HMG_aControlFontAttributes[ k ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ k ]      := tooltip
   _HMG_aControlRangeMin[ k ]     := rows
   _HMG_aControlRangeMax[ k ]     := aWidth
   _HMG_aControlCaption[ k ]      := ""
   _HMG_aControlVisible[ k ]      := ! invisible
   _HMG_aControlHelpId[ k ]       := HelpId
   _HMG_aControlFontHandle[ k ]   := FontHandle
   _HMG_aControlBrushHandle[ k ]  := 0
   _HMG_aControlEnabled[ k ]      := .T.

   _HMG_aControlMiscData1[ k ] := ;
      { multiColumn, multiTabs }

   _HMG_aControlMiscData2[ k ] := ""

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
   ENDIF

   /*
    * Populate items
    */
   IF Len( _HMG_aDialogTemplate ) == 0

      AEval( rows, ;
         {|r| ListboxAddString( ControlHandle, r ) } )

      IF multiSelect

         IF ISARRAY( value )
            LISTBOXSETMULTISEL( ControlHandle, value )
         ENDIF

      ELSE

         IF ISNUMBER( value ) .AND. value <> 0
            ListboxSetCurSel( ControlHandle, value )
         ENDIF

      ENDIF

      IF multiTabs
         LISTBOXSETMULTITAB( ControlHandle, aWidth )
      ENDIF

   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION InitDialogListBox( ParentName, ControlHandle, k )
*-----------------------------------------------------------------------------*

   LOCAL rows
   LOCAL value
   LOCAL aWidth
   LOCAL multiTabs

   HB_SYMBOL_UNUSED( ParentName )

   rows       := _HMG_aControlRangeMin[ k ]
   value      := _HMG_aControlValue[ k ]
   aWidth     := _HMG_aControlRangeMax[ k ]
   multiTabs  := _HMG_aControlMiscData1[ k ][ 2 ]

   AEval( rows, ;
      {|r| ListboxAddString( ControlHandle, r ) } )

   IF _HMG_aControlType[ k ] == "MULTILIST"

      IF ISARRAY( value )
         LISTBOXSETMULTISEL( ControlHandle, value )
      ENDIF

   ELSE

      IF ISNUMBER( value ) .AND. value <> 0
         ListboxSetCurSel( ControlHandle, value )
      ENDIF

   ENDIF

   IF multiTabs
      LISTBOXSETMULTITAB( ControlHandle, aWidth )
   ENDIF

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
      _HMG_aDialogTemplate[ 3 ]

      _HMG_aControlDeleted[ k ] := .T.

   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION LB_Array2String( aData, cSep )
*-----------------------------------------------------------------------------*

   LOCAL cData := ""

   hb_default( @cSep, Chr( 9 ) )

   IF ! Empty( aData )

      cData := aData[ 1 ]

      AEval( aData, ;
         {|x| cData += ( cSep + x ) }, 2 )

   ENDIF

RETURN cData
