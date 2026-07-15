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
FUNCTION _DefineSpinner( ControlName, ParentForm, ;
      x, y, w, value, ;
      FontName, FontSize, ;
      rl, rh, ;
      tooltip, ;
      change, lostfocus, gotfocus, ;
      h, HelpId, ;
      horizontal, invisible, notabstop, ;
      bold, italic, underline, strikeout, ;
      wrap, readonly, ;
      increment, ;
      backcolor, fontcolor, ;
      cuetext, ;
      bInit )
*-----------------------------------------------------------------------------*

   LOCAL aHandles
   LOCAL hControl
   LOCAL hFont
   LOCAL hParent

   LOCAL cVarName
   LOCAL nControl

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   LOCAL ow := NIL
   LOCAL oc := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /*-----------------------------------------------------------------------*/
   /* Defaults                                                              */
   /*-----------------------------------------------------------------------*/

   hb_default( @w, 120 )
   hb_default( @h, 24 )

   __defaultNIL( @value, rl )

   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   hb_default( @horizontal, .F. )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )

   hb_default( @wrap, .F. )
   hb_default( @readonly, .F. )

   hb_default( @increment, 1 )

   /*-----------------------------------------------------------------------*/
   /* Resolve predefined font                                               */
   /*-----------------------------------------------------------------------*/

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

   /*-----------------------------------------------------------------------*/
   /* Resolve active parent                                                 */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginWindowActive

      ParentForm := _HMG_ActiveFormName

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame offset                                                          */
   /*-----------------------------------------------------------------------*/

   IF _HMG_FrameLevel > 0 .AND. ;
      ! _HMG_ParentWindowActive

      nContainerRow := ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      nContainerCol := ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

      x += nContainerCol
      y += nContainerRow

      ParentForm := ;
         _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Validation                                                            */
   /*-----------------------------------------------------------------------*/

   IF .NOT. _IsWindowDefined( ParentForm )

      MsgMiniGuiError( ;
         "Window: " + ;
         IFNIL( ParentForm, "Parent", ParentForm ) + ;
         " is not defined." )

   ENDIF

   IF ISCHAR( ControlName ) .AND. ;
      ControlName == "0"

      ControlName := HMG_GetUniqueName()

   ENDIF

   IF _IsControlDefined( ControlName, ParentForm )

      MsgMiniGuiError( ;
         "Control: " + ;
         ControlName + ;
         " Of " + ;
         ParentForm + ;
         " Already defined." )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Control allocation                                                    */
   /*-----------------------------------------------------------------------*/

   cVarName := "_" + ParentForm + "_" + ControlName

   nControl := _GetControlFree()

   /*-----------------------------------------------------------------------*/
   /* Create control                                                        */
   /*-----------------------------------------------------------------------*/

   hParent := GetFormHandle( ParentForm )

   aHandles := ;
      InitSpinner( ;
         hParent, ;
         0, ;
         x, ;
         y, ;
         w, ;
         "", ;
         0, ;
         rl, ;
         rh, ;
         h, ;
         invisible, ;
         notabstop, ;
         wrap, ;
         readonly, ;
         horizontal )

   hControl := aHandles[1]

   /*-----------------------------------------------------------------------*/
   /* Font initialization                                                   */
   /*-----------------------------------------------------------------------*/

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

   /*-----------------------------------------------------------------------*/
   /* Tab integration                                                       */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, aHandles )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Tooltip                                                               */
   /*-----------------------------------------------------------------------*/

   IF tooltip != NIL

      AEval( ;
         aHandles, ;
         { |hWnd| ;
            SetToolTip( ;
               hWnd, ;
               tooltip, ;
               GetFormToolTipHandle( ParentForm ) ) } )

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

   _HMG_aControlType[ nControl ]          := "SPINNER"
   _HMG_aControlNames[ nControl ]         := ControlName

   _HMG_aControlHandles[ nControl ]       := aHandles
   _HMG_aControlParentHandles[ nControl ] := hParent

   _HMG_aControlIds[ nControl ]           := 0

   /*-----------------------------------------------------------------------*/
   /* Event procedures                                                      */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlProcedures[ nControl ]         := ""

   _HMG_aControlLostFocusProcedure[ nControl ] := lostfocus
   _HMG_aControlGotFocusProcedure[ nControl ]  := gotfocus
   _HMG_aControlChangeProcedure[ nControl ]    := change

   /*-----------------------------------------------------------------------*/
   /* Runtime state                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlPageMap[ nControl ]   := {}
   _HMG_aControlValue[ nControl ]     := 0
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
      nContainerRow

   _HMG_aControlContainerCol[ nControl ] := ;
      nContainerCol

   _HMG_aControlContainerHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Appearance                                                            */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlBkColor[ nControl ]   := backcolor
   _HMG_aControlFontColor[ nControl ] := fontcolor

   /*
      Spinner increment is internally stored
      in the Picture slot.
   */

   _HMG_aControlPicture[ nControl ] := increment

   _HMG_aControlFontName[ nControl ] := FontName
   _HMG_aControlFontSize[ nControl ] := FontSize

   _HMG_aControlFontAttributes[ nControl ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ nControl ] := tooltip

   _HMG_aControlCaption[ nControl ] := ""

   _HMG_aControlFontHandle[ nControl ]  := hFont
   _HMG_aControlBrushHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Range / misc                                                          */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRangeMin[ nControl ] := rl
   _HMG_aControlRangeMax[ nControl ] := rh

   _HMG_aControlHelpId[ nControl ] := HelpId

   _HMG_aControlDblClick[ nControl ] := ""
   _HMG_aControlHeadClick[ nControl ] := {}

   /*
      MiscData1:
         { reserved, readonly }
   */

   _HMG_aControlMiscData1[ nControl ] := ;
      { 0, readonly }

   _HMG_aControlMiscData2[ nControl ] := ""

   /*-----------------------------------------------------------------------*/
   /* Cue banner                                                            */
   /*-----------------------------------------------------------------------*/

   IF ! Empty( cuetext ) .AND. ;
      IsVistaOrLater() .AND. ;
      IsThemed()

      value := ""

      SendMessageWideString( ;
         hControl, ;
         EM_SETCUEBANNER, ;
         .T., ;
         cuetext )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Initial value                                                         */
   /*-----------------------------------------------------------------------*/

   IF ISNUMERIC( value )
      SetSpinnerValue( aHandles[2], value )
   ENDIF

   IF increment <> 1
      SetSpinnerIncrement( aHandles[2], increment )
   ENDIF

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
FUNCTION OSPINEVENTS( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*

   LOCAL hParent
   LOCAL nIndex
   LOCAL nMenuIndex

   SWITCH nMsg

   /*-----------------------------------------------------------------------*/
   /* Keyboard navigation                                                   */
   /*-----------------------------------------------------------------------*/

   CASE WM_GETDLGCODE

      IF _HMG_ExtendedNavigation

         IF wParam == VK_RETURN

            IF _GetKeyState( VK_SHIFT )
               InsertShiftTab()
            ELSE
               InsertTab()
            ENDIF

         ENDIF

      ENDIF

      EXIT

   /*-----------------------------------------------------------------------*/
   /* Context menu                                                          */
   /*-----------------------------------------------------------------------*/

   CASE WM_CONTEXTMENU

      nIndex := ;
         AScan( ;
            _HMG_aControlHandles, ;
            {|x| ;
               iif( ISARRAY( x ), ;
                    AScan( x, hWnd ) > 0, ;
                    x == hWnd ) } )

      IF nIndex == 0
         RETURN 0
      ENDIF

      hParent := ;
         _HMG_aControlParentHandles[ nIndex ]

      nMenuIndex := ;
         AScan( ;
            _HMG_aControlsContextMenu, ;
            {|x| x[1] == hWnd } )

      IF nMenuIndex > 0

         IF _HMG_aControlsContextMenu[ nMenuIndex ][4]

            SetFocus( wParam )

            _HMG_xControlsContextMenuID := ;
               _HMG_aControlsContextMenu[ nMenuIndex ][3]

            TrackPopupMenu( ;
               _HMG_aControlsContextMenu[ nMenuIndex ][2], ;
               LOWORD( lParam ), ;
               HIWORD( lParam ), ;
               hParent )

            RETURN 1

         ENDIF

      ENDIF

      EXIT

   ENDSWITCH

RETURN 0