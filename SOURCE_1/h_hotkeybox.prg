/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

HOTKEYBOX Control Source Code
Copyright 2006 Grigory Filatov <gfilatov@gmail.com>

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

*-----------------------------------------------------------------------------*
FUNCTION _DefineHotKeyBox( ControlName, ParentForm, ;
      x, y, w, h, value, FontName, FontSize, tooltip, ;
      change, HelpId, invisible, notabstop, ;
      bold, italic, underline, strikeout )
*-----------------------------------------------------------------------------*

   LOCAL hControl
   LOCAL hFont
   LOCAL hParent

   LOCAL cParentForm
   LOCAL cVarName

   LOCAL nControl

   /*-----------------------------------------------------------------------*/
   /* Defaults                                                              */
   /*-----------------------------------------------------------------------*/

   hb_default( @w, 120 )
   hb_default( @h, 24 )

   hb_default( @value, 0 )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )

   __defaultNIL( @change, "" )

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
   /* Resolve active form                                                   */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginWindowActive

      ParentForm := _HMG_ActiveFormName

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame offset adjustment                                               */
   /*-----------------------------------------------------------------------*/

   IF _HMG_FrameLevel > 0 .AND. ;
         ! _HMG_ParentWindowActive

      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

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
   /* Create control                                                        */
   /*-----------------------------------------------------------------------*/

   cVarName    := "_" + ParentForm + "_" + ControlName
   cParentForm := ParentForm

   hParent := GetFormHandle( ParentForm )

   hControl := ;
      InitHotKeyBox( ;
         hParent, ;
         x, ;
         y, ;
         w, ;
         h, ;
         "", ;
         0, ;
         invisible, ;
         notabstop )

   /*-----------------------------------------------------------------------*/
   /* Font setup                                                            */
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
      AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Initial value                                                         */
   /*-----------------------------------------------------------------------*/

   SetHotKeyValue( hControl, value )

   /*-----------------------------------------------------------------------*/
   /* Tooltip                                                               */
   /*-----------------------------------------------------------------------*/

   IF tooltip != NIL

      SetToolTip( ;
         hControl, ;
         tooltip, ;
         GetFormToolTipHandle( cParentForm ) )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Register control                                                      */
   /*-----------------------------------------------------------------------*/

   nControl := _GetControlFree()

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, nControl )

#else

   PUBLIC &cVarName. := nControl

#endif

   /*-----------------------------------------------------------------------*/
   /* Identity                                                              */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlType[ nControl ]          := "HOTKEYBOX"
   _HMG_aControlNames[ nControl ]         := ControlName

   _HMG_aControlHandles[ nControl ]       := hControl
   _HMG_aControlParentHandles[ nControl ] := hParent

   _HMG_aControlIds[ nControl ]           := 0

   /*-----------------------------------------------------------------------*/
   /* Event procedures                                                      */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlProcedures[ nControl ]         := ""
   _HMG_aControllostFocusProcedure[ nControl ] := ""
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

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL

   _HMG_aControlPicture[ nControl ] := ""

   _HMG_aControlFontName[ nControl ] := FontName
   _HMG_aControlFontSize[ nControl ] := FontSize

   _HMG_aControlFontAttributes[ nControl ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ nControl ] := tooltip

   _HMG_aControlCaption[ nControl ] := ""

   _HMG_aControlFontHandle[ nControl ]  := hFont
   _HMG_aControlBrushHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Miscellaneous                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0

   _HMG_aControlHelpId[ nControl ] := HelpId

   _HMG_aControlDblClick[ nControl ] := ""
   _HMG_aControlHeadClick[ nControl ] := {}

   _HMG_aControlMiscData1[ nControl ] := 0
   _HMG_aControlMiscData2[ nControl ] := ""

   /*-----------------------------------------------------------------------*/
   /* OOP integration                                                       */
   /*-----------------------------------------------------------------------*/

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nControl, cVarName )
   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION _GetHotKeyName( cControlName, cFormName )
*-----------------------------------------------------------------------------*

   LOCAL cKeyName

   cKeyName := ;
      C_GETHOTKEYNAME( ;
         GetControlHandle( cControlName, cFormName ) )

RETURN ;
   SubStr( ;
      cKeyName, ;
      1, ;
      At( Chr( 0 ), cKeyName ) - 1 )