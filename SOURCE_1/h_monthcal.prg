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
FUNCTION _DefineMonthCal( ;
      ControlName, ParentFormName, ;
      x, y, w, h, value, ;
      FontName, FontSize, tooltip, ;
      notoday, notodaycircle, weeknumbers, ;
      change, HelpId, invisible, notabstop, ;
      bold, italic, underline, strikeout, ;
      backcolor, fontcolor, ;
      titlebkclr, titlefrclr, ;
      background, trlfontclr, ;
      select, gotfocus, lostfocus, ;
      nId, bInit )
*-----------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL aControlHandle := { 0, 0 }

   LOCAL cVarName
   LOCAL nControl

   LOCAL bDialogInit
   LOCAL nStyle
   LOCAL lDialogInMemory

   LOCAL ow := NIL
   LOCAL oc := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   /*-----------------------------------------------------------------------*/
   /* Defaults                                                              */
   /*-----------------------------------------------------------------------*/

   __defaultNIL( @value, Date() )

   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )

   hb_default( @bold, .F. )
   hb_default( @italic, .F. )
   hb_default( @underline, .F. )
   hb_default( @strikeout, .F. )

   /*-----------------------------------------------------------------------*/
   /* Resolve font                                                          */
   /*-----------------------------------------------------------------------*/

   IF ( aControlHandle[ 2 ] := GetFontHandle( FontName ) ) != 0

      GetFontParamByRef( ;
         aControlHandle[ 2 ], ;
         @FontName, ;
         @FontSize, ;
         @bold, ;
         @italic, ;
         @underline, ;
         @strikeout )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Active form defaults                                                  */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive

      ParentFormName := ;
         iif( ;
            _HMG_BeginDialogActive, ;
            _HMG_ActiveDialogName, ;
            _HMG_ActiveFormName )

      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Frame offset adjustment                                               */
   /*-----------------------------------------------------------------------*/

   IF _HMG_FrameLevel > 0 .AND. !_HMG_ParentWindowActive

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

   IF ISCHAR( ControlName ) .AND. ControlName == "0"

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
   /* Control registration setup                                            */
   /*-----------------------------------------------------------------------*/

   cVarName := "_" + ParentFormName + "_" + ControlName
   nControl := _GetControlFree()

   /*-----------------------------------------------------------------------*/
   /* Dialog mode                                                           */
   /*-----------------------------------------------------------------------*/

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := ;
         WS_BORDER + ;
         WS_CHILD + ;
         MCS_DAYSTATE

      IF ! invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF ! notabstop
         nStyle += WS_TABSTOP
      ENDIF

      IF notoday
         nStyle += MCS_NOTODAY
      ENDIF

      IF notodaycircle
         nStyle += MCS_NOTODAYCIRCLE
      ENDIF

      IF weeknumbers
         nStyle += MCS_WEEKNUMBERS
      ENDIF

      /*--------------------------------------------------------------------*/
      /* Dialog template mode                                               */
      /*--------------------------------------------------------------------*/

      IF lDialogInMemory

         InitExCommonControls( 1 )

         bDialogInit := ;
            {|x, y, z| InitDialogMonthCalendar( x, y, z ) }

         /*
            Dialog item structure:

            { ;
               ID, k/hWnd, class, style, exstyle, ;
               x, y, w, h, caption, helpid, tooltip, ;
               font, size, bold, italic, underline, strikeout, ;
               init, tabactive, deleted, tabpage ;
            }
         */

         AAdd( ;
            _HMG_aDialogItems, ;
            { ;
               nId, ;
               nControl, ;
               "SysMonthCal32", ;
               nStyle, ;
               0, ;
               x, ;
               y, ;
               w, ;
               h, ;
               "", ;
               HelpId, ;
               tooltip, ;
               FontName, ;
               FontSize, ;
               bold, ;
               italic, ;
               underline, ;
               strikeout, ;
               bDialogInit, ;
               _HMG_BeginTabActive, ;
               .F., ;
               _HMG_ActiveTabPage ;
            } )

      ELSE

         aControlHandle[ 1 ] := ;
            GetDialogItemHandle( ParentFormHandle, nId )

         SetWindowStyle( ;
            aControlHandle[ 1 ], ;
            nStyle, ;
            .T. )

         IF aControlHandle[ 2 ] != 0

            _SetFontHandle( ;
               aControlHandle[ 1 ], ;
               aControlHandle[ 2 ] )

         ELSE

            __defaultNIL( @FontName, _HMG_DefaultFontName )
            __defaultNIL( @FontSize, _HMG_DefaultFontSize )

            aControlHandle[ 2 ] := ;
               _SetFont( ;
                  aControlHandle[ 1 ], ;
                  FontName, ;
                  FontSize, ;
                  bold, ;
                  italic, ;
                  underline, ;
                  strikeout )

         ENDIF

         x := GetWindowCol( aControlHandle[ 1 ] )
         y := GetWindowRow( aControlHandle[ 1 ] )

      ENDIF

   /*-----------------------------------------------------------------------*/
   /* Standard window mode                                                  */
   /*-----------------------------------------------------------------------*/

   ELSE

      ParentFormHandle := ;
         GetFormHandle( ParentFormName )

      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )

      aControlHandle := ;
         InitMonthCal( ;
            ParentFormHandle, ;
            0, ;
            x, ;
            y, ;
            w, ;
            h, ;
            FontName, ;
            FontSize, ;
            notoday, ;
            notodaycircle, ;
            weeknumbers, ;
            invisible, ;
            notabstop, ;
            bold, ;
            italic, ;
            underline, ;
            strikeout )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Runtime-only initialization                                           */
   /*-----------------------------------------------------------------------*/

   IF .NOT. lDialogInMemory

      w := GetWindowWidth( aControlHandle[ 1 ] )
      h := GetWindowHeight( aControlHandle[ 1 ] )

      /*--------------------------------------------------------------------*/
      /* Tab integration                                                    */
      /*--------------------------------------------------------------------*/

      IF _HMG_BeginTabActive

         AAdd( ;
            _HMG_ActiveTabCurrentPageMap, ;
            aControlHandle[ 1 ] )

      ENDIF

      /*--------------------------------------------------------------------*/
      /* Initial value                                                      */
      /*--------------------------------------------------------------------*/

      SetMonthCalValue( ;
         aControlHandle[ 1 ], ;
         Year( value ), ;
         Month( value ), ;
         Day( value ) )

      /*--------------------------------------------------------------------*/
      /* Tooltip                                                            */
      /*--------------------------------------------------------------------*/

      IF tooltip != NIL

         SetToolTip( ;
            aControlHandle[ 1 ], ;
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
   /* Register control metadata                                             */
   /*-----------------------------------------------------------------------*/

   _HMG_aControlType[ nControl ]               := "MONTHCAL"
   _HMG_aControlNames[ nControl ]              := ControlName

   _HMG_aControlHandles[ nControl ]            := aControlHandle[ 1 ]
   _HMG_aControlParentHandles[ nControl ]      := ParentFormHandle

   _HMG_aControlIds[ nControl ]                := nId

   _HMG_aControlProcedures[ nControl ]         := ""

   _HMG_aControlPageMap[ nControl ]            := {}

   _HMG_aControlValue[ nControl ]              := value
   _HMG_aControlInputMask[ nControl ]          := ""

   _HMG_aControlLostFocusProcedure[ nControl ] := lostfocus
   _HMG_aControlGotFocusProcedure[ nControl ]  := gotfocus
   _HMG_aControlChangeProcedure[ nControl ]    := change

   _HMG_aControlDeleted[ nControl ]            := .F.

   _HMG_aControlBkColor[ nControl ]            := backcolor
   _HMG_aControlFontColor[ nControl ]          := fontcolor

   /* Vista+ select event callback */
   _HMG_aControlDblClick[ nControl ] := ;
      iif( IsVistaOrLater(), select, "" )

   _HMG_aControlHeadClick[ nControl ]          := {}

   _HMG_aControlRow[ nControl ]                := y
   _HMG_aControlCol[ nControl ]                := x

   _HMG_aControlWidth[ nControl ]              := w
   _HMG_aControlHeight[ nControl ]             := h

   _HMG_aControlSpacing[ nControl ]            := 0

   _HMG_aControlContainerRow[ nControl ] := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameRow[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlContainerCol[ nControl ] := ;
      iif( ;
         _HMG_FrameLevel > 0, ;
         _HMG_ActiveFrameCol[ _HMG_FrameLevel ], ;
         -1 )

   _HMG_aControlPicture[ nControl ]            := ""

   _HMG_aControlContainerHandle[ nControl ]    := 0

   _HMG_aControlFontName[ nControl ]           := FontName
   _HMG_aControlFontSize[ nControl ]           := FontSize

   _HMG_aControlFontAttributes[ nControl ] := ;
      { bold, italic, underline, strikeout }

   _HMG_aControlToolTip[ nControl ]            := tooltip

   _HMG_aControlRangeMin[ nControl ]           := 0
   _HMG_aControlRangeMax[ nControl ]           := 0

   _HMG_aControlCaption[ nControl ]            := ""

   _HMG_aControlVisible[ nControl ] := ;
      ! invisible

   _HMG_aControlHelpId[ nControl ]             := HelpId

   _HMG_aControlFontHandle[ nControl ]         := ;
      aControlHandle[ 2 ]

   _HMG_aControlBrushHandle[ nControl ]        := 0

   _HMG_aControlEnabled[ nControl ]            := .T.

   _HMG_aControlMiscData1[ nControl ]          := 0
   _HMG_aControlMiscData2[ nControl ]          := ""

   /*-----------------------------------------------------------------------*/
   /* Runtime theme/color setup                                             */
   /*-----------------------------------------------------------------------*/

   IF .NOT. lDialogInMemory

      AddMonthCalBoldDay( ;
         ControlName, ;
         ParentFormName, ;
         Date() )

      IF _HMG_IsThemed .AND. ;
         ( IsArrayRGB( backcolor ) .OR. ;
           IsArrayRGB( fontcolor ) .OR. ;
           IsArrayRGB( titlebkclr ) .OR. ;
           IsArrayRGB( titlefrclr ) )

         SetWindowTheme( aControlHandle[ 1 ], "", "" )

         /* Resize to themed ideal size */
         SetPosMonthCal( ;
            aControlHandle[ 1 ], ;
            x, ;
            y )

         _HMG_aControlWidth[ nControl ] := ;
            GetWindowWidth( aControlHandle[ 1 ] )

         _HMG_aControlHeight[ nControl ] := ;
            GetWindowHeight( aControlHandle[ 1 ] )

      ENDIF

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Color configuration                                                   */
   /*-----------------------------------------------------------------------*/

   IF IsArrayRGB( backcolor )

      SetMonthCalMonthBkColor( ;
         aControlHandle[ 1 ], ;
         backcolor[ 1 ], ;
         backcolor[ 2 ], ;
         backcolor[ 3 ] )

   ENDIF

   IF IsArrayRGB( fontcolor )

      SetMonthCalFontColor( ;
         aControlHandle[ 1 ], ;
         fontcolor[ 1 ], ;
         fontcolor[ 2 ], ;
         fontcolor[ 3 ] )

   ENDIF

   IF IsArrayRGB( titlebkclr )

      SetMonthCalTitleBkColor( ;
         aControlHandle[ 1 ], ;
         titlebkclr[ 1 ], ;
         titlebkclr[ 2 ], ;
         titlebkclr[ 3 ] )

   ENDIF

   IF IsArrayRGB( titlefrclr )

      SetMonthCalTitleFontColor( ;
         aControlHandle[ 1 ], ;
         titlefrclr[ 1 ], ;
         titlefrclr[ 2 ], ;
         titlefrclr[ 3 ] )

   ENDIF

   IF IsArrayRGB( background )

      SetMonthCalBkColor( ;
         aControlHandle[ 1 ], ;
         background[ 1 ], ;
         background[ 2 ], ;
         background[ 3 ] )

   ENDIF

   IF IsArrayRGB( trlfontclr )

      SetMonthCalTrlFontColor( ;
         aControlHandle[ 1 ], ;
         trlfontclr[ 1 ], ;
         trlfontclr[ 2 ], ;
         trlfontclr[ 3 ] )

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* OOP initialization                                                    */
   /*-----------------------------------------------------------------------*/

   IF _HMG_lOOPEnabled

      Eval( _HMG_bOnControlInit, nControl, cVarName )

#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( aControlHandle[ 1 ] )
#endif

   ENDIF

   /*-----------------------------------------------------------------------*/
   /* Init callback                                                         */
   /*-----------------------------------------------------------------------*/

   Do_ControlEventProcedure( ;
      bInit, ;
      nControl, ;
      ow, ;
      oc )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogMonthCalendar( ParentFormName, ControlHandle, k )
*-----------------------------------------------------------------------------*

   AddMonthCalBoldDay( _HMG_aControlNames[ k ], ParentFormName, Date() )

   SetPosMonthCal ( ControlHandle, _HMG_aControlCol[ k ], _HMG_aControlRow[ k ] )
   // JP 62
   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[ 3 ] // Modal
      _HMG_aControlDeleted[ k ] := .T.
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION OMONTHCALEVENTS( hWnd, nMsg, wParam, lParam ) // GF 2016.04.02
*-----------------------------------------------------------------------------*
   LOCAL i := AScan ( _HMG_aControlHandles, hWnd )

   HB_SYMBOL_UNUSED( wParam )
   HB_SYMBOL_UNUSED( lParam )

   SWITCH nMsg

   CASE WM_MOUSEACTIVATE

      SetFocus( hWnd )

      RETURN 1

   CASE WM_SETFOCUS

      VirtualChildControlFocusProcess ( _HMG_aControlHandles[ i ], _HMG_aControlParentHandles[ i ] )
      _DoControlEventProcedure ( _HMG_aControlGotFocusProcedure[ i ], i )

      EXIT

   CASE WM_KILLFOCUS

      _DoControlEventProcedure ( _HMG_aControlLostFocusProcedure[ i ], i )

   ENDSWITCH

RETURN 0

*-----------------------------------------------------------------------------*
FUNCTION AddMonthCalBoldDay( ControlName, ParentFormName, dDay )
*-----------------------------------------------------------------------------*
   LOCAL i
   LOCAL ix := GetControlIndex ( ControlName, ParentFormName )
   LOCAL aBoldDays

   aBoldDays := _HMG_aControlPageMap[ ix ]

   IF ( i := AScan( aBoldDays, {| d | d >= dDay } ) ) == 0
      AAdd( aBoldDays, dDay )
      SetDayState( ControlName, ParentFormName )
   ELSEIF aBoldDays[ i ] > dDay
      hb_AIns( aBoldDays, i, dDay, .T. )
      SetDayState( ControlName, ParentFormName )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION DelMonthCalBoldDay( ControlName, ParentFormName, dDay )
*-----------------------------------------------------------------------------*
   LOCAL i
   LOCAL ix := GetControlIndex ( ControlName, ParentFormName )
   LOCAL aBoldDays

   aBoldDays := _HMG_aControlPageMap[ ix ]

   IF ( i := AScan( aBoldDays, dDay ) ) > 0
      hb_ADel( aBoldDays, i, .T. )
      SetDayState( ControlName, ParentFormName )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION IsMonthCalBoldDay( ControlName, ParentFormName, dDay )
*-----------------------------------------------------------------------------*
   LOCAL i := GetControlIndex ( ControlName, ParentFormName )
   LOCAL aBoldDays

   aBoldDays := _HMG_aControlPageMap[ i ]

Return( AScan( aBoldDays, dDay ) > 0 )

*-----------------------------------------------------------------------------*
FUNCTION SetDayState( ControlName, ParentFormName )
*-----------------------------------------------------------------------------*
   LOCAL hWnd
   LOCAL aData, aDays, aBoldDays
   LOCAL dStart, dEnd, dEoM, dDay
   LOCAL i, nCount, iNextD, nMonth, nLen

   hWnd := GetControlHandle ( ControlName, ParentFormName )

   aData := GetMonthRange( hWnd )
   nCount := aData[ 1 ]
   IF nCount < 1
      RETURN NIL
   ENDIF

   aDays := Array( nCount * 32 )
   AFill( aDays, 0 )

   i := GetControlIndex ( ControlName, ParentFormName )
   aBoldDays := _HMG_aControlPageMap[ i ]

   dStart := aData[ 2 ]
   iNextD := AScan( aBoldDays, {| d | d >= dStart } )

   IF iNextD > 0
      dEnd := aData[ 3 ]
      dEoM := EoM( dStart )
      nMonth := 0
      dDay := aBoldDays[ iNextD ]
      nLen := Len( aBoldDays )

      DO WHILE dDay <= dEnd
         IF dDay <= dEoM
            aDays[ nMonth * 32 + Day( dDay ) ] := 1
            iNextD++
            IF iNextD > nLen
               EXIT
            ENDIF
            dDay := aBoldDays[ iNextD ]
         ELSE
            nMonth++
            dEoM := EoM( dEoM + 1 )
         ENDIF
      ENDDO
   ENDIF

   C_SETDAYSTATE( hWnd, nCount, aDays )

RETURN NIL
