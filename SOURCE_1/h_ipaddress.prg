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

*-----------------------------------------------------------------------------*
FUNCTION _DefineIPAddress( ControlName, ParentForm, x, y, w, h, aValue, ;
      fontname, fontsize, tooltip, lostfocus, gotfocus, ;
      change, HelpId, invisible, notabstop, bold, italic, underline, strikeout )
*-----------------------------------------------------------------------------*
   LOCAL ControlHandle, FontHandle
   LOCAL nParentHandle, cVarName, nControl
   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   /*-----------------------------------------------------------------------*/
   /* Resolve active form */
   /*-----------------------------------------------------------------------*/
   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
      __defaultNIL( @fontname, _HMG_ActiveFontName )
      __defaultNIL( @fontsize, _HMG_ActiveFontSize )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Validation */
   /*-----------------------------------------------------------------------*/
   IF .NOT. _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Defaults */
   /*-----------------------------------------------------------------------*/
   hb_default( @w, 124 )
   hb_default( @h, 24 )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )
   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   IF ( FontHandle := GetFontHandle( fontname ) ) != 0
      GetFontParamByRef( FontHandle, @fontname, @fontsize, @bold, @italic, @underline, @strikeout )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame container */
   /*-----------------------------------------------------------------------*/
   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
      nContainerRow := _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      nContainerCol := _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Control creation */
   /*-----------------------------------------------------------------------*/
   cVarName := "_" + ParentForm + "_" + ControlName
   nParentHandle := GetFormHandle( ParentForm )

   ControlHandle := InitIPAddress( nParentHandle, 0, x, y, w, h, invisible, notabstop )

   IF FontHandle != 0
      _SetFontHandle( ControlHandle, FontHandle )
   ELSE
      __defaultNIL( @fontname, _HMG_DefaultFontName )
      __defaultNIL( @fontsize, _HMG_DefaultFontSize )
      FontHandle := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
   ENDIF

   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF

   IF ISARRAY( aValue )
      SetIPAddress( ControlHandle, aValue[1], aValue[2], aValue[3], aValue[4] )
   ENDIF

   IF tooltip != NIL
      SetToolTip( ControlHandle, tooltip, GetFormToolTipHandle( ParentForm ) )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Control allocation */
   /*-----------------------------------------------------------------------*/
   nControl := _GetControlFree()

   /*-----------------------------------------------------------------------*/
   /* Register control name */
   /*-----------------------------------------------------------------------*/
#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nControl )
#else
   PUBLIC &cVarName. := nControl
#endif

   /*-----------------------------------------------------------------------*/
   /* Identity */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlType[ nControl ] := "IPADDRESS"
   _HMG_aControlNames[ nControl ] := ControlName
   _HMG_aControlHandles[ nControl ] := ControlHandle
   _HMG_aControlParentHandles[ nControl ] := nParentHandle
   _HMG_aControlIds[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Event procedures */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlProcedures[ nControl ] := ""
   _HMG_aControlLostFocusProcedure[ nControl ] := lostfocus
   _HMG_aControlGotFocusProcedure[ nControl ] := gotfocus
   _HMG_aControlChangeProcedure[ nControl ] := change

   /*-----------------------------------------------------------------------*/
   /* Runtime state */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlPageMap[ nControl ] := {}
   _HMG_aControlValue[ nControl ] := Nil
   _HMG_aControlInputMask[ nControl ] := ""
   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlBkColor[ nControl ] := Nil
   _HMG_aControlFontColor[ nControl ] := Nil
   _HMG_aControlDblClick[ nControl ] := ""
   _HMG_aControlHeadClick[ nControl ] := {}
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := iif( invisible, .F., .T. )

   /*-----------------------------------------------------------------------*/
   /* Geometry */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlRow[ nControl ] := y
   _HMG_aControlCol[ nControl ] := x
   _HMG_aControlWidth[ nControl ] := w
   _HMG_aControlHeight[ nControl ] := h
   _HMG_aControlSpacing[ nControl ] := 0
   _HMG_aControlContainerRow[ nControl ] := nContainerRow
   _HMG_aControlContainerCol[ nControl ] := nContainerCol
   _HMG_aControlContainerHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Appearance */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlPicture[ nControl ] := ""
   _HMG_aControlFontName[ nControl ] := fontname
   _HMG_aControlFontSize[ nControl ] := fontsize
   _HMG_aControlFontAttributes[ nControl ] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[ nControl ] := tooltip
   _HMG_aControlCaption[ nControl ] := ''
   _HMG_aControlFontHandle[ nControl ] := FontHandle
   _HMG_aControlBrushHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Miscellaneous */
   /*-----------------------------------------------------------------------*/
   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0
   _HMG_aControlHelpId[ nControl ] := HelpId
   _HMG_aControlMiscData1[ nControl ] := 0
   _HMG_aControlMiscData2[ nControl ] := ''

RETURN Nil