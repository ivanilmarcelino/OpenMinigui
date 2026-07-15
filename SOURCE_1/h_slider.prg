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
FUNCTION _DefineSlider( ControlName, ParentFormName, ;
      x, y, w, h, lo, hi, value, ;
      tooltip, scroll, change, vertical, ;
      noticks, both, top, left, ;
      HelpId, invisible, notabstop, ;
      backcolor, nId, enableselrange, ;
      nSelMin, nSelMax, bInit )
*-----------------------------------------------------------------------------*

   LOCAL hParent
   LOCAL hControl

   LOCAL cVarName

   LOCAL nControl
   LOCAL nStyle

   LOCAL bDlgInit

   LOCAL lDialogInMemory

   LOCAL ow := NIL
   LOCAL oc := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /*-----------------------------------------------------------------------*/
   /* Defaults                                                              */
   /*-----------------------------------------------------------------------*/

   hb_default( ;
      @w, ;
      iif( vertical, ;
           35 + iif( both, 5, 0 ), ;
           120 ) )

   hb_default( ;
      @h, ;
      iif( vertical, ;
           120, ;
           35 + iif( both, 5, 0 ) ) )

   hb_default( @lo, 0 )
   hb_default( @hi, 100 )

   hb_default( ;
      @value, ;
      Int( ( hi - lo ) / 2 ) )

   hb_default( @enableselrange, .F. )
   hb_default( @nSelMin, 0 )
   hb_default( @nSelMax, 0 )

   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )

   __defaultNIL( @scroll, "" )
   __defaultNIL( @change, "" )

   /*-----------------------------------------------------------------------*/
   /* Resolve active parent                                                 */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := ;
         iif( _HMG_BeginDialogActive, ;
              _HMG_ActiveDialogName, ;
              _HMG_ActiveFormName )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame offset adjustment                                               */
   /*-----------------------------------------------------------------------*/

   IF _HMG_FrameLevel > 0 .AND. ;
         ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      ParentFormName := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   /*-----------------------------------------------------------------------*/
   /* Validation                                                            */
   /*-----------------------------------------------------------------------*/

   IF .NOT. _IsWindowDefined( ParentFormName ) .AND. ;
         .NOT. lDialogInMemory

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
         .NOT. lDialogInMemory

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentFormName + ;
         " Already defined." )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Control allocation                                                    */
   /*-----------------------------------------------------------------------*/

   cVarName := "_" + ParentFormName + "_" + ControlName

   nControl := _GetControlFree()

   /*-----------------------------------------------------------------------*/
   /* Dialog mode                                                           */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginDialogActive

      hParent := _HMG_ActiveDialogHandle

      nStyle := WS_CHILD

      IF ! notabstop
         nStyle += WS_TABSTOP
      ENDIF

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF vertical
         nStyle += TBS_VERT
      ENDIF

      IF ! noticks
         nStyle += TBS_AUTOTICKS
      ELSE
         nStyle += TBS_NOTICKS
      ENDIF

      IF both
         nStyle += TBS_BOTH
      ENDIF

      IF top
         nStyle += TBS_TOP
      ENDIF

      IF left
         nStyle += TBS_LEFT
      ENDIF

      IF enableselrange
         nStyle += TBS_ENABLESELRANGE
      ENDIF

      /*--------------------------------------------------------------------*/
      /* Dialog template mode                                               */
      /*--------------------------------------------------------------------*/

      IF Len( _HMG_aDialogTemplate ) > 0

         bDlgInit := ;
            {|a, b, c| InitDialogSlider( a, b, c ) }

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nControl, ;
               "msctls_trackbar32", ;
               nStyle, ;
               0, ;
               x, ;
               y, ;
               w, ;
               h, ;
               "", ;
               HelpId, ;
               tooltip, ;
               "", ;
               0, ;
               , ;
               , ;
               , ;
               , ;
               bDlgInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      /*--------------------------------------------------------------------*/
      /* Runtime dialog mode                                                */
      /*--------------------------------------------------------------------*/

      ELSE

         hControl := ;
            GetDialogItemHandle( hParent, nId )

         x := GetWindowCol( hControl )
         y := GetWindowRow( hControl )

         w := GetWindowWidth( hControl )
         h := GetWindowHeight( hControl )

         SetWindowStyle( hControl, nStyle, .T. )

      ENDIF

   /*-----------------------------------------------------------------------*/
   /* Standard form mode                                                    */
   /*-----------------------------------------------------------------------*/

   ELSE

      hParent := GetFormHandle( ParentFormName )

      hControl := ;
         InitSlider( ;
            hParent, ;
            0, ;
            x, ;
            y, ;
            w, ;
            h, ;
            lo, ;
            hi, ;
            vertical, ;
            noticks, ;
            both, ;
            top, ;
            left, ;
            invisible, ;
            notabstop, ;
            enableselrange, ;
            nSelMin, ;
            nSelMax )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Runtime-only initialization                                           */
   /*-----------------------------------------------------------------------*/

   IF .NOT. lDialogInMemory

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
      ENDIF

      SendMessage( hControl, TBM_SETPOS, 1, value )

      IF tooltip != NIL

         SetToolTip( ;
            hControl, ;
            tooltip, ;
            GetFormToolTipHandle( ParentFormName ) )

      ENDIF

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Register control name                                                 */
   /*-----------------------------------------------------------------------*/

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /*-----------------------------------------------------------------------*/
   /* Identity                                                              */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlType[ nControl ]          := "SLIDER"
   _HMG_aControlNames[ nControl ]         := ControlName

   _HMG_aControlHandles[ nControl ]       := hControl
   _HMG_aControlParentHandles[ nControl ] := hParent

   _HMG_aControlIds[ nControl ]           := nId

   /*-----------------------------------------------------------------------*/
   /* Event procedures                                                      */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlProcedures[ nControl ]         := scroll
   _HMG_aControlLostFocusProcedure[ nControl ] := ""
   _HMG_aControlGotFocusProcedure[ nControl ]  := ""
   _HMG_aControlChangeProcedure[ nControl ]    := change

   /*-----------------------------------------------------------------------*/
   /* Runtime state                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlPageMap[ nControl ]   := {}
   _HMG_aControlValue[ nControl ]     := value
   _HMG_aControlInputMask[ nControl ] := ""

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := ! invisible

   /*-----------------------------------------------------------------------*/
   /* Geometry                                                              */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRow[ nControl ]    := y
   _HMG_aControlCol[ nControl ]    := x

   _HMG_aControlWidth[ nControl ]  := w
   _HMG_aControlHeight[ nControl ] := h

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ] := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
           -1 )

   _HMG_aControlContainerCol[ nControl ] := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
           -1 )

   _HMG_aControlContainerHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Appearance                                                            */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlBkColor[ nControl ]   := backcolor
   _HMG_aControlFontColor[ nControl ] := NIL

   _HMG_aControlPicture[ nControl ] := ""

   _HMG_aControlFontName[ nControl ] := ""
   _HMG_aControlFontSize[ nControl ] := 0

   _HMG_aControlFontAttributes[ nControl ] := ;
      { .F., .F., .F., .F. }

   _HMG_aControlToolTip[ nControl ] := tooltip

   _HMG_aControlCaption[ nControl ] := ""

   /*
      FontHandle slot reused internally for:
      - Active tab name tracking
   */

   _HMG_aControlFontHandle[ nControl ] := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveTabName, ;
           "" )

   _HMG_aControlBrushHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Miscellaneous                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRangeMin[ nControl ] := lo
   _HMG_aControlRangeMax[ nControl ] := hi

   _HMG_aControlHelpId[ nControl ] := HelpId

   _HMG_aControlDblClick[ nControl ] := ;
      _HMG_ActiveTabButtons

   _HMG_aControlHeadClick[ nControl ] := {}

   /*
      MiscData1 reused internally for:
      - Parent frame form name
   */

   _HMG_aControlMiscData1[ nControl ] := ;
      iif( _HMG_FrameLevel > 0, ;
           _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ], ;
           "" )

   _HMG_aControlMiscData2[ nControl ] := ""

   /*-----------------------------------------------------------------------*/
   /* OOP integration                                                       */
   /*-----------------------------------------------------------------------*/

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, nControl, cVarName )

#ifdef _OBJECT_
      ow := _WindowObj( hParent )
      oc := _ControlObj( hControl )
#endif

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Init callback                                                         */
   /*-----------------------------------------------------------------------*/

   Do_ControlEventProcedure( bInit, nControl, ow, oc )

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION InitDialogSlider( ParentName, hControl, nControl )
*-----------------------------------------------------------------------------*

   IF ValType( ParentName ) <> "U"

      SendMessage( ;
         hControl, ;
         TBM_SETPOS, ;
         1, ;
         _HMG_aControlValue[ nControl ] )

   ENDIF

   /* Modal dialog cleanup */

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. ;
         _HMG_aDialogTemplate[3]

      _HMG_aControlDeleted[ nControl ] := .T.

   ENDIF

RETURN NIL