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
FUNCTION _DefineTimer( ControlName, ParentForm, ;
      Interval, ProcedureName, Once, bInit )
*-----------------------------------------------------------------------------*

   LOCAL nParentHandle
   LOCAL cVarName
   LOCAL nId
   LOCAL nControl

   LOCAL lSuccess

   LOCAL nContainerRow := -1
   LOCAL nContainerCol := -1

   /*-----------------------------------------------------------------------*/
   /* Resolve active form                                                   */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
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

   IF ISCHAR( ControlName ) .AND. ControlName == "0"

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
   /* Defaults                                                              */
   /*-----------------------------------------------------------------------*/

   hb_default( @Interval, 1000 )
   hb_default( @Once, .F. )

   IF _HMG_ProgrammaticChange
      Interval := Max( Interval, 10 )
   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame container                                                       */
   /*-----------------------------------------------------------------------*/

   IF _HMG_FrameLevel > 0

      nContainerRow := ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ]

      nContainerCol := ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ]

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Timer creation                                                        */
   /*-----------------------------------------------------------------------*/

   cVarName := "_" + ParentForm + "_" + ControlName

   nParentHandle := GetFormHandle( ParentForm )

   nId := _GetId()

   lSuccess := ;
      InitTimer( ;
         nParentHandle, ;
         nId, ;
         Interval )

   /*-----------------------------------------------------------------------*/
   /* Control allocation                                                    */
   /*-----------------------------------------------------------------------*/

   nControl := _GetControlFree()

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

   _HMG_aControlType[ nControl ]          := "TIMER"
   _HMG_aControlNames[ nControl ]         := ControlName

   _HMG_aControlHandles[ nControl ]       := 0
   _HMG_aControlParentHandles[ nControl ] := nParentHandle

   _HMG_aControlIds[ nControl ] := nId

   /*-----------------------------------------------------------------------*/
   /* Event procedures                                                      */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlProcedures[ nControl ] := ProcedureName

   _HMG_aControlLostFocusProcedure[ nControl ] := ""
   _HMG_aControlGotFocusProcedure[ nControl ]  := ""
   _HMG_aControlChangeProcedure[ nControl ]    := ""

   /*-----------------------------------------------------------------------*/
   /* Runtime state                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlPageMap[ nControl ]   := {}
   _HMG_aControlValue[ nControl ]     := Interval
   _HMG_aControlInputMask[ nControl ] := ""

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := .T.

   /*-----------------------------------------------------------------------*/
   /* Geometry                                                              */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRow[ nControl ]    := 0
   _HMG_aControlCol[ nControl ]    := 0

   _HMG_aControlWidth[ nControl ]  := 0
   _HMG_aControlHeight[ nControl ] := 0

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ] := ;
      nContainerRow

   _HMG_aControlContainerCol[ nControl ] := ;
      nContainerCol

   _HMG_aControlContainerHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Appearance                                                            */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL

   /*
      Picture slot internally stores
      TIMER ONCE state.
   */

   _HMG_aControlPicture[ nControl ] := Once

   _HMG_aControlFontName[ nControl ] := ""
   _HMG_aControlFontSize[ nControl ] := 0

   _HMG_aControlFontAttributes[ nControl ] := ;
      { .F., .F., .F., .F. }

   _HMG_aControlToolTip[ nControl ] := ""
   _HMG_aControlCaption[ nControl ] := ""

   _HMG_aControlFontHandle[ nControl ]  := 0
   _HMG_aControlBrushHandle[ nControl ] := 0

   /*-----------------------------------------------------------------------*/
   /* Miscellaneous                                                         */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0

   _HMG_aControlHelpId[ nControl ] := 0

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

   /*-----------------------------------------------------------------------*/
   /* Init callback                                                         */
   /*-----------------------------------------------------------------------*/

   Do_ControlEventProcedure( bInit, nControl )

RETURN lSuccess