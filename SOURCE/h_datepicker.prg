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

/* 
 * Windows API Constants for DateTimePicker controls.
 * These define the visual behavior at the OS level.
 */
#define DTS_UPDOWN     0x0001 // Use up-down control instead of drop-down calendar
#define DTS_SHOWNONE   0x0002 // Allow a 'none' or null state with a checkbox
#define DTS_RIGHTALIGN 0x0020 // Align the drop-down calendar to the right

/* 
 * Compatibility layer for older Harbour/xHarbour versions.
 * Ensures Unicode-aware string functions are mapped correctly if not natively present.
 */
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
  #xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
  #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
#endif

*-----------------------------------------------------------------------------*
FUNCTION _DefineDatePick( ControlName, ParentFormName, x, y, w, h, value, ;
      fontname, fontsize, tooltip, change, lostfocus, gotfocus, ;
      shownone, updown, rightalign, HelpId, invisible, notabstop, ;
      bold, italic, underline, strikeout, Field, Enter, backcolor, ;
      fontcolor, titlebkclr, titlefrclr, trlfontclr, cDateFormat, ;
      dRangeMin, dRangeMax, nId, bInit )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Initializes and registers a DatePicker control within the HMG framework.
 * Parameters: 
 *    - ControlName/ParentFormName: Identifiers for the control and its owner.
 *    - x, y, w, h: Coordinates and dimensions.
 *    - value: Initial Date value.
 *    - shownone, updown, rightalign: Boolean flags for Win32 DTS styles.
 *    - Field: Optional database field for data binding.
 *    - backcolor/fontcolor/title...: Custom color settings for the control.
 * Side Effects: Updates HMG global control arrays, creates Win32 window handle, 
 *               and may trigger UI updates.
 */
   LOCAL ParentFormHandle, ControlHandle, FontHandle
   LOCAL mVar, k, Style, blInit
   LOCAL lDialogInMemory

   // Set default dimensions and state if not provided by the user
   hb_default( @w, 120 )
   hb_default( @h, 24 )
   __defaultNIL( @value, BLANK_DATE )
   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )

   // Retrieve existing font handle or prepare to create a new one based on attributes
   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   // If the control is bound to a database field, retrieve the current field value
   _HandleFieldValue( @value, Field, ControlName, ParentFormName )

   // Determine the correct parent and adjust coordinates if nested inside a Frame or Dialog
   _SetupParentAndPosition( @ParentFormName, @x, @y, @FontName, @FontSize )

   lDialogInMemory := _HMG_DialogInMemory

   // Ensure the control name is unique and the parent exists
   _ValidateControl( ControlName, ParentFormName, lDialogInMemory )

   // Generate internal variable name and find the next available index in HMG control arrays
   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()

   // Logic for handling controls defined within a BEGIN DIALOG block
   IF _HMG_BeginDialogActive
      ParentFormHandle := _HMG_ActiveDialogHandle
      Style := _BuildDatePickerStyle( shownone, updown, rightalign, invisible, notabstop )

      IF lDialogInMemory
         // Add to the memory template for later rendering
         _AddToDialogTemplate( @blInit, nId, k, x, y, w, h, fontname, fontsize, ;
            bold, italic, underline, strikeout, HelpId, tooltip, Style )
      ELSE
         // Control already exists in a resource-based dialog; retrieve handle and apply styles
         ControlHandle := GetDialogItemHandle( ParentFormHandle, nId )
         _ApplyDialogControlSettings( ControlHandle, @x, @y, @w, @h, Style )
      ENDIF
   ELSE
      // Standard window creation logic using the HMG C-level InitDatePick
      ParentFormHandle := GetFormHandle( ParentFormName )
      ControlHandle := InitDatePick( ParentFormHandle, 0, x, y, w, h, '', 0, ;
         shownone, updown, rightalign, invisible, notabstop )
   ENDIF

   // Finalize visual properties and initial state for non-template controls
   IF .NOT. lDialogInMemory
      FontHandle := _ApplyFontAndInitialValue( ControlHandle, FontHandle, fontname, fontsize, ;
         bold, italic, underline, strikeout, value )

      _ApplyCommonControlSettings( ControlHandle, tooltip, ParentFormName, Field, k )

      _HMG_aControlValue[ k ] := value
   ENDIF

   // Register the control in HMG's global state management system
   _RegisterControl( k, mVar, ControlName, "DATEPICK", ControlHandle, ParentFormHandle, ;
      nId, Enter, Field, lostfocus, gotfocus, change, y, x, w, h, ;
      fontname, fontsize, bold, italic, underline, strikeout, tooltip, HelpId, ;
      invisible, backcolor, fontcolor, titlebkclr, titlefrclr, trlfontclr, FontHandle )

   // Apply specific DatePicker attributes like custom formats and date ranges
   _ApplyDatePickerExtras( ControlHandle, k, cDateFormat, dRangeMin, dRangeMax, ;
      backcolor, fontcolor, ControlName, ParentFormName )

   // Execute initialization events (bInit) and OOP-related callbacks
   _FireInitEvents( k, mVar, bInit, ParentFormHandle, ControlHandle )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _DefineTimePick( ControlName, ParentFormName, x, y, w, h, value, ;
      fontname, fontsize, tooltip, change, lostfocus, gotfocus, ;
      shownone, HelpId, invisible, notabstop, bold, italic, underline, ;
      strikeout, Field, Enter, cTimeFormat, nId )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Initializes and registers a TimePicker control.
 * Note: In Win32, a TimePicker is a DateTimePicker with the DTS_UPDOWN style 
 *       and a specific time-based format string.
 */
   LOCAL ParentFormHandle, ControlHandle, FontHandle
   LOCAL mVar, k, Style, blInit
   LOCAL lDialogInMemory

   hb_default( @w, 120 )
   hb_default( @h, 24 )
   __defaultNIL( @value, iif( shownone, "", Time() ) )
   __defaultNIL( @change, "" )
   __defaultNIL( @lostfocus, "" )
   __defaultNIL( @gotfocus, "" )
   hb_default( @invisible, .F. )
   hb_default( @notabstop, .F. )
   hb_default( @cTimeFormat, "HH:mm:ss" ) // Default ISO time format

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   _HandleFieldValue( @value, Field, ControlName, ParentFormName )

   _SetupParentAndPosition( @ParentFormName, @x, @y, @FontName, @FontSize )

   lDialogInMemory := _HMG_DialogInMemory

   _ValidateControl( ControlName, ParentFormName, lDialogInMemory )

   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive
      ParentFormHandle := _HMG_ActiveDialogHandle
      Style := _BuildTimePickerStyle( shownone, invisible, notabstop )

      IF lDialogInMemory
         _AddToDialogTemplate( @blInit, nId, k, x, y, w, h, fontname, fontsize, ;
            bold, italic, underline, strikeout, HelpId, tooltip, Style )
      ELSE
         ControlHandle := GetDialogItemHandle( ParentFormHandle, nId )
         _ApplyDialogControlSettings( ControlHandle, @x, @y, @w, @h, Style )
      ENDIF
   ELSE
      ParentFormHandle := GetFormHandle( ParentFormName )
      // TimePickers always use the UpDown style in HMG for better UX
      ControlHandle := InitTimePick( ParentFormHandle, 0, x, y, w, h, '', 0, ;
         shownone, invisible, notabstop )
   ENDIF

   IF .NOT. lDialogInMemory
      FontHandle := _ApplyFontAndInitialValueTime( ControlHandle, FontHandle, fontname, fontsize, ;
         bold, italic, underline, strikeout, value, shownone )

      _ApplyCommonControlSettings( ControlHandle, tooltip, ParentFormName, Field, k )

      _HMG_aControlValue[ k ] := value
   ENDIF

   _RegisterControl( k, mVar, ControlName, "TIMEPICK", ControlHandle, ParentFormHandle, ;
      nId, Enter, Field, lostfocus, gotfocus, change, y, x, w, h, ;
      fontname, fontsize, bold, italic, underline, strikeout, tooltip, HelpId, ;
      invisible, NIL, NIL, NIL, NIL, NIL, FontHandle )

   _ApplyTimePickerExtras( ControlHandle, k, cTimeFormat, fontname, fontsize, ;
      ControlName, ParentFormName )

   // OOP support: Notify the framework that a new control object should be instantiated
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _HandleFieldValue( value, Field, ControlName, ParentFormName )
*-----------------------------------------------------------------------------*
// Purpose: Implements data binding by reading the value from a DBF field.
// Reasoning: HMG supports automatic data binding. The field must be fully qualified (Alias->Field).
   LOCAL WorkArea
   IF Field != NIL
      IF hb_UAt( '>', Field ) == 0
         MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + ;
            " : You must specify a fully qualified Field name." )
      ELSE
         WorkArea := hb_ULeft( Field, hb_UAt( '>', Field ) - 2 )
         IF Select( WorkArea ) != 0
            value := &( Field ) // Macro evaluation to get the actual field content
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _SetupParentAndPosition( ParentFormName, x, y, FontName, FontSize )
*-----------------------------------------------------------------------------*
// Purpose: Resolves the parent window context and adjusts coordinates for nested containers.
// Reasoning: If a control is defined inside a FRAME, its (x,y) is relative to the frame, 
//            but Win32 requires coordinates relative to the parent window.
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ValidateControl( ControlName, ParentFormName, lDialogInMemory )
*-----------------------------------------------------------------------------*
// Purpose: Sanity checks to prevent duplicate control names or orphaned controls.
   IF .NOT. _IsWindowDefined( ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError( "Window: " + IFNIL( ParentFormName, "Parent", ParentFormName ) + " is not defined." )
   ENDIF

   // Support for auto-generated unique names if "0" is passed
   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + " Already defined." )
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _BuildDatePickerStyle( shownone, updown, rightalign, invisible, notabstop )
*-----------------------------------------------------------------------------*
// Purpose: Combines HMG logical flags into a single Win32 Window Style bitmask.
   LOCAL Style := WS_CHILD
   IF shownone
      Style += DTS_SHOWNONE
   ENDIF
   IF updown
      Style += DTS_UPDOWN
   ENDIF
   IF rightalign
      Style += DTS_RIGHTALIGN
   ENDIF
   IF ! invisible
      Style += WS_VISIBLE
   ENDIF
   IF ! notabstop
      Style += WS_TABSTOP
   ENDIF
RETURN Style

*-----------------------------------------------------------------------------*
STATIC FUNCTION _BuildTimePickerStyle( shownone, invisible, notabstop )
*-----------------------------------------------------------------------------*
// Purpose: Specific style builder for TimePickers.
// Reasoning: TimePickers always require DTS_UPDOWN to function correctly as time inputs.
   LOCAL Style := WS_CHILD
   IF shownone
      Style += DTS_SHOWNONE
   ENDIF
   Style += DTS_UPDOWN
   IF ! invisible
      Style += WS_VISIBLE
   ENDIF
   IF ! notabstop
      Style += WS_TABSTOP
   ENDIF
RETURN Style

*-----------------------------------------------------------------------------*
STATIC FUNCTION _AddToDialogTemplate( blInit, nId, k, x, y, w, h, fontname, fontsize, ;
      bold, italic, underline, strikeout, HelpId, tooltip, Style )
*-----------------------------------------------------------------------------*
// Purpose: Adds the control definition to the HMG Dialog Template array.
// Reasoning: Used for memory-based dialogs where controls are created en masse upon activation.
   InitExCommonControls( 1 ) // Ensure ComCtl32.dll is initialized for DatePickers
   blInit := {| x, y, z | InitDialogDatePicker( x, y, z ) }
   AAdd( _HMG_aDialogItems, { nId, k, "SysDateTimePick32", Style, 0, x, y, w, h, "", ;
      HelpId, tooltip, fontname, fontsize, bold, italic, underline, strikeout, ;
      blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyDialogControlSettings( ControlHandle, x, y, w, h, Style )
*-----------------------------------------------------------------------------*
// Purpose: Synchronizes coordinates and styles for controls already existing in a Dialog resource.
   x := GetWindowCol( ControlHandle )
   y := GetWindowRow( ControlHandle )
   w := GetWindowWidth( ControlHandle )
   h := GetWindowHeight( ControlHandle )
   SetWindowStyle( ControlHandle, Style, .T. )
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyFontAndInitialValue( ControlHandle, FontHandle, fontname, fontsize, ;
      bold, italic, underline, strikeout, value )
*-----------------------------------------------------------------------------*
// Purpose: Sets the visual font and the initial date value of the control.
   IF FontHandle != 0
      _SetFontHandle( ControlHandle, FontHandle )
   ELSE
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )
      FontHandle := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
   ENDIF

   // If inside a Tab control, track this handle for page-switching logic
   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF

   // Handle empty dates (Null state) vs specific dates
   IF Empty( value )
      SetDatePickNull( ControlHandle )
   ELSE
      SetDatePick( ControlHandle, Year( value ), Month( value ), Day( value ) )
   ENDIF
RETURN FontHandle

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyFontAndInitialValueTime( ControlHandle, FontHandle, fontname, fontsize, ;
      bold, italic, underline, strikeout, value, shownone )
*-----------------------------------------------------------------------------*
// Purpose: Sets the visual font and the initial time value.
// Reasoning: Time values are handled as strings; we parse them to set the Win32 control state.
   IF FontHandle != 0
      _SetFontHandle( ControlHandle, FontHandle )
   ELSE
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )
      FontHandle := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
   ENDIF

   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF

   IF Empty( value )
      IF shownone
         SetDatePickNull( ControlHandle )
      ELSE
         // Default to current system time if no value provided and 'shownone' is false
         SetTimePick( ControlHandle, Val( Left( Time(), 2 ) ), Val( SubStr( Time(), 4, 2 ) ), Val( SubStr( Time(), 7, 2 ) ) )
      ENDIF
   ELSE
      SetTimePick( ControlHandle, Val( Left( value, 2 ) ), Val( SubStr( value, 4, 2 ) ), Val( SubStr( value, 7, 2 ) ) )
   ENDIF
RETURN FontHandle

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyCommonControlSettings( ControlHandle, tooltip, ParentFormName, Field, k )
*-----------------------------------------------------------------------------*
// Purpose: Applies tooltips and registers the control for data-aware browsing.
   IF tooltip != NIL
      SetToolTip( ControlHandle, tooltip, GetFormToolTipHandle( ParentFormName ) )
   ENDIF

   IF Field != NIL
      // Add to the list of controls that need to be refreshed when the database pointer moves
      AAdd( _HMG_aFormBrowseList[ GetFormIndex( ParentFormName ) ], k )
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _RegisterControl( k, mVar, ControlName, cType, ControlHandle, ParentFormHandle, ;
      nId, Enter, Field, lostfocus, gotfocus, change, y, x, w, h, ;
      fontname, fontsize, bold, italic, underline, strikeout, tooltip, HelpId, invisible, ;
      backcolor, fontcolor, titlebkclr, titlefrclr, trlfontclr, FontHandle )
*-----------------------------------------------------------------------------*
// Purpose: Populates HMG's internal parallel arrays with control metadata.
// Reasoning: HMG uses these arrays to manage state, events, and properties across the application.
#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   Public &mVar. := k // Create a public variable for the control index
#endif

   _HMG_aControlType[ k ] := cType
   _HMG_aControlNames[ k ] := ControlName
   _HMG_aControlHandles[ k ] := ControlHandle
   _HMG_aControlParentHandles[ k ] := ParentFormHandle
   _HMG_aControlIds[ k ] := nId
   _HMG_aControlProcedures[ k ] := Enter
   _HMG_aControlPageMap[ k ] := Field
   _HMG_aControlInputMask[ k ] := ""
   _HMG_aControllostFocusProcedure[ k ] := lostfocus
   _HMG_aControlGotFocusProcedure[ k ] := gotfocus
   _HMG_aControlChangeProcedure[ k ] := change
   _HMG_aControlDeleted[ k ] := .F.
   _HMG_aControlDblClick[ k ] := ""
   _HMG_aControlHeadClick[ k ] := {}
   _HMG_aControlRow[ k ] := y
   _HMG_aControlCol[ k ] := x
   _HMG_aControlWidth[ k ] := w
   _HMG_aControlHeight[ k ] := h
   _HMG_aControlSpacing[ k ] := 0
   _HMG_aControlContainerRow[ k ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ k ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ k ] := ""
   _HMG_aControlContainerHandle[ k ] := 0
   _HMG_aControlFontName[ k ] := fontname
   _HMG_aControlFontSize[ k ] := fontsize
   _HMG_aControlFontAttributes[ k ] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[ k ] := tooltip
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := 0
   _HMG_aControlCaption[ k ] := ''
   _HMG_aControlVisible[ k ] := iif( invisible, .F., .T. )
   _HMG_aControlHelpId[ k ] := HelpId
   _HMG_aControlFontHandle[ k ] := FontHandle
   _HMG_aControlBrushHandle[ k ] := 0
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlMiscData2[ k ] := ''

   // DatePickers support more color customization than TimePickers in HMG
   IF cType == "DATEPICK"
      _HMG_aControlBkColor[ k ] := backcolor
      _HMG_aControlFontColor[ k ] := fontcolor
      _HMG_aControlMiscData1[ k ] := { backcolor, fontcolor, titlebkclr, titlefrclr, trlfontclr }
   ELSE
      _HMG_aControlBkColor[ k ] := NIL
      _HMG_aControlFontColor[ k ] := NIL
      _HMG_aControlMiscData1[ k ] := 0
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyDatePickerExtras( ControlHandle, k, cDateFormat, dRangeMin, dRangeMax, ;
      backcolor, fontcolor, ControlName, ParentFormName )
*-----------------------------------------------------------------------------*
// Purpose: Applies specialized DatePicker properties like colors, formats, and ranges.
   IF IsArrayRGB( BackColor )
      SetDatePickBkColor( ControlHandle, backcolor[ 1 ], backcolor[ 2 ], backcolor[ 3 ] )
   ENDIF

   IF IsArrayRGB( FontColor )
      SetDatePickFontColor( ControlHandle, fontcolor[ 1 ], fontcolor[ 2 ], fontcolor[ 3 ] )
   ENDIF

   // Custom date formatting (e.g., "dd/MM/yyyy")
   IF ISCHARACTER( cDateFormat )
      IF SetDatePickerDateFormat( ControlHandle, cDateFormat )
         _HMG_aControlSpacing[ k ] := cDateFormat
      ELSE
         MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + ": Wrong format string." )
      ENDIF
   ELSE
      _HMG_aControlSpacing[ k ] := ""
   ENDIF

   // Restrict selectable dates within a specific range
   IF ISDATE( dRangeMin ) .OR. ISDATE( dRangeMax )
      IF ! _SetDatePickerRange( ControlHandle, dRangeMin, dRangeMax, k )
         MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + ": Wrong date range." )
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _ApplyTimePickerExtras( ControlHandle, k, cTimeFormat, fontname, fontsize, ;
      ControlName, ParentFormName )
*-----------------------------------------------------------------------------*
// Purpose: Applies time-specific formatting and ensures font consistency.
   IF SetDatePickerDateFormat( ControlHandle, cTimeFormat )
      _HMG_aControlSpacing[ k ] := cTimeFormat
      // Force font update if custom attributes are used, as TimePickers can be sensitive to sizing
      IF AScan( _HMG_aControlFontAttributes[ k ], .T. ) > 0 .OR. ;
            fontname != _HMG_DefaultFontName .OR. fontsize != _HMG_DefaultFontSize
         _SetFontName( ControlName, ParentFormName, fontname )
      ENDIF
   ELSE
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + ": Wrong format string." )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _FireInitEvents( k, mVar, bInit, ParentFormHandle, ControlHandle )
*-----------------------------------------------------------------------------*
// Purpose: Triggers the ON INIT event and handles OOP object creation.
   LOCAL ow := NIL, oc := NIL
#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif
   ENDIF

   // Execute the user-defined bInit code block
   Do_ControlEventProcedure( bInit, k, ow, oc )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogDatePicker( ParentFormName, ControlHandle, k )
*-----------------------------------------------------------------------------*
// Purpose: Callback used during Dialog initialization to set the control's value.
   ParentFormName := NIL
   ControlHandle := NIL
   _SetValue( , , _HMG_aControlValue[ k ], k )

   // Special handling for Modal Dialogs to mark controls as deleted/inactive if needed
   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[ 3 ] // Modal
      _HMG_aControlDeleted[ k ] := .T.
   ENDIF
RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetDatePickerRange( ControlHandle, dRangeMin, dRangeMax, Index )
*-----------------------------------------------------------------------------*
// Purpose: Public wrapper to set the date range and update internal HMG state.
   LOCAL lOK

   hb_default( @dRangeMin, BLANK_DATE )
   hb_default( @dRangeMax, BLANK_DATE )

   IF ( lOK := SetDatePickRange( ControlHandle, dRangeMin, dRangeMax ) )
      _HMG_aControlRangeMin[ Index ] := dRangeMin
      _HMG_aControlRangeMax[ Index ] := dRangeMax
   ENDIF

RETURN lOK

*-----------------------------------------------------------------------------*
FUNCTION OPICKEVENTS( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*
// Purpose: Low-level Windows Message Handler (Subclassing) for DatePicker controls.
// Reasoning: Used to handle custom background painting which is not natively 
//            supported by the standard Win32 DateTimePicker control.
   LOCAL i := AScan( _HMG_aControlHandles, hWnd )
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL hDC, hBrush

   HB_SYMBOL_UNUSED( lParam )

   hDC := wParam

   SWITCH nMsg

   CASE WM_ERASEBKGND
      // Custom background color logic
      IF i > 0
         IF _HMG_aControlBkColor[ i ] != NIL
            GetClientRect( _HMG_aControlHandles[ i ], /*@*/aRect )
            hBrush := CreateSolidBrush( _HMG_aControlBkColor[ i ][ 1 ], _HMG_aControlBkColor[ i ][ 2 ], _HMG_aControlBkColor[ i ][ 3 ] )
            // Fill the client area, accounting for the scrollbar/dropdown button width
            FillRect( hDC, aRect[ 1 ], aRect[ 2 ], aRect[ 3 ] - GETVSCROLLBARWIDTH(), aRect[ 4 ], hBrush )
            DeleteObject( hBrush )
            RETURN 1 // Signal that we handled the background erasure
         ENDIF
      ENDIF

   ENDSWITCH

RETURN 0
