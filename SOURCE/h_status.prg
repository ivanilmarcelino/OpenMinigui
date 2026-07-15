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

#define ID_STATUSBAR    0
#define SB_SETTEXT      (WM_USER+1)
#define SBT_OWNERDRAW   0x1000

#define ITEMTYPENAME    "ITEMMESSAGE"
#define ITEMNAME        "StatusItem"
#define PROGRESSNAME    "ProgressMessage"

/*
 * Compatibility macro for Harbour/xHarbour versions.
 * Ensures hb_UAt (Unicode-aware At) is available or mapped to standard At().
 */
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
#xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
#endif

*-----------------------------------------------------------------------------*
FUNCTION _BeginMessageBar( ControlName, ParentForm, kbd, ;
                           FontName, FontSize, Bold, Italic, ;
                           UnderLine, StrikeOut, Message )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Initializes the creation of a Message Bar (Status Bar) within a form.
 * Parameters:
 *    - ControlName: Unique identifier for the status bar.
 *    - ParentForm: Name of the form containing the bar.
 *    - kbd: Logical; if .T., automatically adds keyboard state indicators (Num, Caps, Ins).
 *    - Font/Style params: Define the visual appearance of the text.
 *    - Message: Initial text to display in the first panel.
 * Side Effects: Updates HMG internal state, registers the control, and creates the Win32 handle.
 */

   LOCAL hParent
   LOCAL hControl
   LOCAL hFont
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL nIndex

   // Track the currently active message bar for subsequent item definitions
   _HMG_ActiveMessageBarName := ControlName

   // If no parent is specified, default to the form currently being defined
   IF ParentForm == NIL
      ParentForm := _HMG_ActiveFormName
   ENDIF

   // Validation: Ensure the parent exists and the control name is unique
   IF ! _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( ;
         "Control: " + ControlName + ;
         " Of " + ParentForm + ;
         " Already defined." )
   ENDIF

   // Font Handling: Retrieve existing handle or prepare to create a new one
   hFont := GetFontHandle( FontName )

   IF hFont != 0
      // Extract attributes from an existing font handle to ensure consistency
      GetFontParamByRef( ;
         hFont, ;
         @FontName, ;
         @FontSize, ;
         @Bold, ;
         @Italic, ;
         @UnderLine, ;
         @StrikeOut )
   ENDIF

   // Inherit parent window font if within a Begin Window block and no font specified
   IF _HMG_BeginWindowActive
      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )
   ENDIF

   // Create the physical Win32 Status Bar control
   hParent := GetFormHandle( ParentForm )
   hControl := InitMessageBar( hParent, ID_STATUSBAR )

   _HMG_ActiveStatusHandle := hControl

   // Apply font to the control
   IF hFont != 0
      _SetFontHandle( hControl, hFont )
   ELSE
      // Fallback to system defaults if no font provided
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )

      hFont := _SetFont( ;
         hControl, ;
         FontName, ;
         FontSize, ;
         Bold, ;
         Italic, ;
         UnderLine, ;
         StrikeOut )
   ENDIF

   // Retrieve dimensions for internal HMG tracking
   GetClientRect( hControl, aRect )

   // Register the control in HMG's global arrays for property management
   nIndex := _RegisterControl( ;
             ControlName, ;
             ParentForm, ;
             hParent, ;
             hControl, ;
             "MESSAGEBAR", ;
             ID_STATUSBAR )

   // Store initial properties in HMG's internal state arrays
   _HMG_aControlValue[ nIndex ] := Message

   _HMG_aControlWidth[ nIndex ] := aRect[3]
   _HMG_aControlHeight[ nIndex ] := aRect[4]

   _HMG_aControlFontName[ nIndex ] := FontName
   _HMG_aControlFontSize[ nIndex ] := FontSize

   _HMG_aControlFontHandle[ nIndex ] := hFont
   _HMG_aControlFontAttributes[ nIndex ] := { Bold, Italic, UnderLine, StrikeOut }

   // Reset item counter for the new bar
   _HMG_StatusItemCount := 0

   // If requested, append the standard keyboard status indicators
   IF kbd
      _SetStatusBarKbd( ControlName, ParentForm )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _EndMessageBar()
*-----------------------------------------------------------------------------*
/*
 * Purpose: Finalizes the Message Bar definition block.
 * Logic: 
 *    - Ensures the bar has at least one item (Win32 requirement to avoid resize artifacts).
 *    - Refreshes any embedded progress bars.
 *    - Cleans up the "Active" bar state.
 */
   LOCAL ParentForm
   LOCAL i

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
      i := GetControlIndex ( _HMG_ActiveMessageBarName, ParentForm )
#ifdef _HMG_COMPAT_
   ELSE
      // Support for definitions outside of standard Begin/End Window blocks
      i := GetControlIndexByHandle ( _HMG_ActiveStatusHandle )
      ParentForm := GetParentFormName( i )
#endif
   ENDIF

   // Safety Check: A status bar with 0 items can cause crashes during WM_SIZE events.
   // We create a default item if none were defined by the user.
   IF _HMG_StatusItemCount == 0 
      _DefineItemMessage( ITEMNAME, _HMG_ActiveMessageBarName, 0, 0, ;
         hb_defaultValue( _HMG_aControlValue[ i ], GetProperty( ParentForm, "Title" ) ), , , 0, , , , .F. )
   ENDIF

   // If a progress bar was defined within this status bar, trigger a refresh
   IF ( i := GetControlIndex( PROGRESSNAME, ParentForm ) ) != 0
      RefreshProgressItem( _HMG_aControlMiscData1[ i, 1 ], _HMG_aControlHandles[ i ], _HMG_aControlMiscData1[ i, 2 ] )
   ENDIF

   // Reset global pointers
   _HMG_ActiveMessageBarName := ""
   _HMG_StatusItemCount := 0

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC FUNCTION _RegisterControl( ;
   cControlName, ;
   cParentForm, ;
   hParentForm, ;
   hControl, ;
   cType, ;
   nId )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Internal helper to populate HMG's parallel arrays with control data.
 * Reasoning: HMG uses a set of global arrays to track every control's state, 
 * handles, and properties. This function finds an empty slot and initializes it.
 */

   LOCAL cVarName
   LOCAL k

   // Find the next available index in the global control arrays
   k := _GetControlFree()

   // Create a unique variable name for the control (e.g., _Main_StatusBar1)
   cVarName := "_" + cParentForm + "_" + cControlName

#ifdef _NAMES_LIST_

   _SetNameList( cVarName, k )

#else

   PUBLIC &cVarName. := k

#endif

   // Initialize core Win32 and HMG identification fields
   _HMG_aControlType[ k ]              := cType
   _HMG_aControlNames[ k ]             := cControlName
   _HMG_aControlHandles[ k ]           := hControl
   _HMG_aControlParentHandles[ k ]     := hParentForm
   _HMG_aControlIds[ k ]               := nId

   // Initialize default values for all possible control properties to prevent NIL errors
   _HMG_aControlProcedures[ k ]        := ""
   _HMG_aControlPageMap[ k ]           := {}

   _HMG_aControlValue[ k ]             := NIL

   _HMG_aControlInputMask[ k ]         := ""

   _HMG_aControlLostFocusProcedure[ k ] := ""
   _HMG_aControlGotFocusProcedure[ k ]  := ""
   _HMG_aControlChangeProcedure[ k ]    := ""

   _HMG_aControlDeleted[ k ]           := .F.

   _HMG_aControlBkColor[ k ]           := NIL
   _HMG_aControlFontColor[ k ]         := NIL

   _HMG_aControlDblClick[ k ]          := ""
   _HMG_aControlHeadClick[ k ]         := {}

   _HMG_aControlRow[ k ]               := 0
   _HMG_aControlCol[ k ]               := 0

   _HMG_aControlWidth[ k ]             := 0
   _HMG_aControlHeight[ k ]            := 0

   _HMG_aControlSpacing[ k ]           := 0

   _HMG_aControlContainerRow[ k ]      := -1
   _HMG_aControlContainerCol[ k ]      := -1

   _HMG_aControlPicture[ k ]           := ""

   _HMG_aControlContainerHandle[ k ]   := 0

   _HMG_aControlFontName[ k ]          := ""
   _HMG_aControlFontSize[ k ]          := 0
   _HMG_aControlFontAttributes[ k ]    := { .F., .F., .F., .F. }
   _HMG_aControlToolTip[ k ]           := ""

   _HMG_aControlRangeMin[ k ]          := 0
   _HMG_aControlRangeMax[ k ]          := 0

   _HMG_aControlCaption[ k ]           := ""

   _HMG_aControlVisible[ k ]           := .T.

   _HMG_aControlHelpId[ k ]            := 0

   _HMG_aControlFontHandle[ k ]        := 0
   _HMG_aControlBrushHandle[ k ]       := 0

   _HMG_aControlEnabled[ k ]           := .T.

   _HMG_aControlMiscData1[ k ]         := 0
   _HMG_aControlMiscData2[ k ]         := ""

   // If Object-Oriented Programming mode is enabled, trigger the init callback
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, cVarName )
   ENDIF

RETURN k

*-----------------------------------------------------------------------------*
FUNCTION _DefineItemMessage( ControlName, ParentControl, x, y, Caption, ;
                             ProcedureName, w, h, Icon, cStyl, ;
                             ToolTip, Default, BackColor, ;
                             FontColor, Align )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Defines an individual panel (item) within the status bar.
 * Parameters:
 *    - Caption: Text to display.
 *    - ProcedureName: Action to execute on click.
 *    - w: Width. If NIL, it's auto-calculated based on text length.
 *    - Default: If .T., this item's text is stored as the bar's default message.
 *    - BackColor/FontColor: Used for owner-drawn items.
 * Returns: Handle to the created item.
 */

   LOCAL hControl
   LOCAL hParentForm
   LOCAL hStatusBar
   LOCAL cParentForm
   LOCAL ParentForm
   LOCAL cCaption
   LOCAL nIndex
   LOCAL nHotPos

   // Resolve the parent form context
   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
#ifdef _HMG_COMPAT_
   ELSE
      nHotPos   := GetControlIndexByHandle( _HMG_ActiveStatusHandle )
      ParentForm := GetParentFormName( nHotPos )
#endif
   ENDIF

   IF ! _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   // Default to the currently active message bar if not specified
   IF ParentControl == NIL
      ParentControl := _HMG_ActiveMessageBarName
   ENDIF

   cParentForm := ParentForm
   hParentForm := GetFormHandle( ParentForm )
   hStatusBar := GetControlHandle( ParentControl, ParentForm )

   // Auto-calculate width based on text content and font if width is not provided
   IF w == NIL
      w := Max( ;
         70, ;
         GetTextWidth( ;
            NIL, ;
            Caption, ;
            _HMG_aControlFontHandle[ ;
               GetControlIndex( ParentControl, cParentForm ) ;
            ] ;
         ) + 6 )
   ENDIF

   // Accelerator Key Handling: Check for '&' in caption to define a hotkey
   IF ! Empty( ProcedureName )
      cCaption := Upper( Caption )
      nHotPos := hb_UAt( "&", cCaption )

      IF nHotPos > 0
         _DefineLetterOrDigitHotKey( ;
            cCaption, ;
            nHotPos, ;
            cParentForm, ;
            ProcedureName )
      ENDIF
      // Remove the '&' character before displaying in the UI
      Caption := StrTran( Caption, "&", "" )
   ENDIF

   // Store the default message for the status bar if flagged
   IF hb_defaultValue( Default, .F. )
      _HMG_DefaultStatusBarMessage := Caption
   ENDIF

   // Win32 Logic: The first item (index 0) usually stretches to fill space.
   // We set width/height to 0 for the first item to allow this behavior.
   IF ++_HMG_StatusItemCount == 1
      w := 0
      h := 0
   ELSE
      h := 1
   ENDIF

   hb_default( @cStyl, "" )

   // Initialize the item via the Win32 API wrapper
   hControl := InitItemBar( ;
      hStatusBar, ;
      Caption, ;
      0, ;
      w, ;
      h, ;
      Icon, ;
      ToolTip, ;
      iif( ;
         Upper( cStyl ) == "RAISED", ;
         1, ;
         iif( Upper( cStyl ) == "FLAT", 2, 0 ) ;
      ) )

   // Register the item as a sub-control in HMG
   nIndex := _RegisterControl( ;
             ControlName, ;
             cParentForm, ;
             hParentForm, ;
             hControl, ;
             ITEMTYPENAME, ;
             0 )

   _HMG_aControlProcedures[ nIndex ] := ProcedureName

   _HMG_aControlBkColor[ nIndex ] := BackColor
   _HMG_aControlFontColor[ nIndex ] := FontColor

   _HMG_aControlRow[ nIndex ] := y
   _HMG_aControlCol[ nIndex ] := x

   _HMG_aControlWidth[ nIndex ] := w
   _HMG_aControlHeight[ nIndex ] := h
   _HMG_aControlSpacing[ nIndex ] := hb_defaultValue( Align, 0 )
   _HMG_aControlContainerHandle[ nIndex ] := hStatusBar

   _HMG_aControlToolTip[ nIndex ] := ToolTip

   _HMG_aControlCaption[ nIndex ] := Caption

   // Ensure aligned items have a visible font color
   IF _HMG_aControlSpacing[ nIndex ] > 0 .AND. ;
      _HMG_aControlFontColor[ nIndex ] == NIL
      FontColor := BLACK
      _HMG_aControlFontColor[ nIndex ] := FontColor
   ENDIF

   // Owner-Draw Activation: If custom colors are used, we must notify the 
   // Win32 control to send WM_DRAWITEM messages to the parent.
   IF IsArrayRGB( BackColor ) .OR. ;
      IsArrayRGB( FontColor )

      SendMessage( ;
         hStatusBar, ;
         SB_SETTEXT, ;
         hb_bitOr( _HMG_StatusItemCount - 1, SBT_OWNERDRAW ), ;
         0 )

   ENDIF

RETURN hControl

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusClock( BarName, FormName, Width, ToolTip, ;
                          Action, lAMPM, BackColor, FontColor )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Adds a dynamic clock item to the status bar.
 * Logic: Creates a standard item and a 1-second timer to update its text.
 */

   LOCAL nItem

   hb_default( @lAMPM, .F. )
   __defaultNIL( @Width,   iif( lAMPM, 92, 70 ) )
   __defaultNIL( @ToolTip, "" )
   __defaultNIL( @Action,  "" )

   // Define the item that will hold the time string
   nItem := _DefineItemMessage( ;
      "TimerBar", ;
      BarName, ;
      0, ;
      0, ;
      iif( lAMPM, AMPM( Time() ), Time() ), ;
      Action, ;
      Width, ;
      0, ;
      , ;
      "", ;
      ToolTip, ;
      , ;
      BackColor, ;
      FontColor, ;
      1 )

#ifdef _HMG_COMPAT_
   IF ! _HMG_BeginWindowActive
      FormName := ;
         GetParentFormName( ;
            GetControlIndexByHandle( _HMG_ActiveStatusHandle ) )
   ENDIF
#endif

   // Create a timer to refresh the clock every 1000ms
   _DefineTimer( ;
      "StatusTimer", ;
      FormName, ;
      1000, ;
      {|| ;
         _SetItem( ;
            BarName, ;
            FormName, ;
            nItem, ;
            iif( lAMPM, AMPM( Time() ), Time() ) ;
         ) ;
      } )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusKeybrd( BarName, FormName, Width, ToolTip, Action )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Adds visual indicators for NumLock, CapsLock, and Insert keys.
 * Logic: Uses icons (LED on/off) and a high-frequency timer (250ms) to poll key states.
 */

   LOCAL nNumLock
   LOCAL nCapsLock
   LOCAL nInsert
   LOCAL bAction

   __defaultNIL( @Width,   75 )
   __defaultNIL( @ToolTip, "" )
   __defaultNIL( @Action,  "" )

   // NumLock Indicator Setup
   bAction := iif( Empty( Action ), {|| KeyTogglePlatform( VK_NUMLOCK ) }, Action )
   nNumLock := _DefineItemMessage( "TimerNum", BarName, 0, 0, "NumLock", bAction, Width + 20, 0, ;
      iif( IsNumLockActive(), "zzz_led_on", "zzz_led_off" ), "", ToolTip )

   // CapsLock Indicator Setup
   bAction := iif( Empty( Action ), {|| KeyTogglePlatform( VK_CAPITAL ) }, Action )
   nCapsLock := _DefineItemMessage( "TimerCaps", BarName, 0, 0, "CapsLock", bAction, Width + 25, 0, ;
      iif( IsCapsLockActive(), "zzz_led_on", "zzz_led_off" ), "", ToolTip )

   // Insert Indicator Setup
   bAction := iif( Empty( Action ), {|| KeyTogglePlatform( VK_INSERT ) }, Action )
   nInsert := _DefineItemMessage( "TimerInsert", BarName, 0, 0, "Insert", bAction, Width, 0, ;
      iif( IsInsertActive(), "zzz_led_on", "zzz_led_off" ), "", ToolTip )

#ifdef _HMG_COMPAT_
   IF ! _HMG_BeginWindowActive
      FormName := GetParentFormName( GetControlIndexByHandle( _HMG_ActiveStatusHandle ) )
   ENDIF
#endif

   // Timer to poll and update the LED icons based on actual keyboard state
   _DefineTimer( ;
      "StatusKeyBrd", ;
      FormName, ;
      250, ;
      {|| ;
         _SetStatusIcon( BarName, FormName, nNumLock, iif( IsNumLockActive(), "zzz_led_on", "zzz_led_off" ) ), ;
         _SetStatusIcon( BarName, FormName, nCapsLock, iif( IsCapsLockActive(), "zzz_led_on", "zzz_led_off" ) ), ;
         _SetStatusIcon( BarName, FormName, nInsert, iif( IsInsertActive(), "zzz_led_on", "zzz_led_off" ) ) ;
      } )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION KeyTogglePlatform( nKeyCode )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Cross-platform (Win9x vs NT) key toggling.
 * Reasoning: Windows NT/XP+ requires different API calls for simulating key presses.
 */
   IF _HMG_IsXPorLater
      KeyToggleNT( nKeyCode )
   ELSE
      KeyToggle( nKeyCode )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _IsOwnerDrawStatusBarItem( ParentHandle, ItemID, Value, lSet )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Checks if a specific status bar item is configured for owner-drawing.
 * Parameters:
 *    - ParentHandle: Handle of the status bar.
 *    - ItemID: Index of the panel.
 *    - Value: Text to set (if lSet is .T.).
 *    - lSet: Logical; if .T., updates the internal caption.
 * Returns: .T. if the item requires owner-drawing (custom colors).
 */
   LOCAL h
   LOCAL nLocID := 0
   LOCAL lOwnerDraw := .F.
   LOCAL i

   hb_default( @lSet, .F. )

   IF Empty( ItemID ) .OR. ItemID == NIL
      ItemID := 1
   ENDIF

   // Iterate through all registered controls to find the matching status item
   FOR EACH h IN _HMG_aControlContainerHandle

      i := hb_enumindex( h )

      IF h == ParentHandle .AND. _HMG_aControlType[ i ] == ITEMTYPENAME

         IF ++nLocID == ItemID
            // Item is owner-drawn if either background or font color is defined
            IF ( lOwnerDraw := ( _HMG_aControlBkColor[ i ] != NIL .OR. _HMG_aControlFontColor[ i ] != Nil ) )
               IF lSet
                  _HMG_aControlCaption[ i ] := Value
               ELSE
                  Value := i // Return the internal index
               ENDIF
            ENDIF

            EXIT

         ENDIF

      ENDIF

   NEXT

RETURN lOwnerDraw

// (GF) HMG 1.2 Extended Build 25
*-----------------------------------------------------------------------------*
STATIC FUNCTION AMPM( cTime )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Converts 24-hour time string to 12-hour format with AM/PM suffix.
 */
   LOCAL nHour := Val( cTime )

   DO CASE
   CASE nHour == 0 .OR. nHour == 24
      cTime := "12" + SubStr( cTime, 3 ) + " am"
   CASE nHour < 12
      cTime += " am"
   CASE nHour == 12
      cTime += " pm"
   OTHERWISE
      cTime := StrZero( nHour - 12, 2 ) + SubStr( cTime, 3 ) + " pm"
   ENDCASE

RETURN cTime

// Keyboard indicator widths for the text-based status bar
#define KBD_CAPS_WIDTH    38
#define KBD_NUMLOCK_WIDTH 42
#define KBD_SCROLL_WIDTH  44

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusBarKbd ( BarName, FormName )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Alternative keyboard indicator style using text labels (CAP, NUM, SCRL).
 * Logic: Changes font color (Black for active, Silver for inactive) via a timer.
 */
   LOCAL i := GetControlIndex ( BarName, FormName )
   LOCAL hWnd := GetFormHandle( FormName )

   // Main message item
   _DefineItemMessage ( ITEMNAME, BarName, 0, 0, ;
      hb_defaultValue( _HMG_aControlValue[ i ], GetProperty ( FormName, "Title" ) ), , , 0, , "RAISED" )

   // Text indicators
   _DefineItemMessage ( ITEMNAME, BarName, 0, 0, "CAP", , iif( _HMG_IsThemed, KBD_CAPS_WIDTH, KBD_CAPS_WIDTH - 2 ), 0, , , , , , SILVER )

   _DefineItemMessage ( ITEMNAME, BarName, 0, 0, "NUM", , KBD_NUMLOCK_WIDTH, 0, , , , , , SILVER )

   _DefineItemMessage ( ITEMNAME, BarName, 0, 0, "SCRL", , KBD_SCROLL_WIDTH, 0, , , , , , SILVER )

   // Update colors every 250ms based on key state
   _DefineTimer ( "StatusBarKbd", FormName, 250, ;
      {|| ;
         _SetStatusItemProperty( 2, iif( IsCapsLockActive(), BLACK, SILVER ), hWnd, STATUS_ITEM_FONTCOLOR ), ;
         _SetStatusItemProperty( 3, iif( IsNumLockActive(), BLACK, SILVER ), hWnd, STATUS_ITEM_FONTCOLOR ), ;
         _SetStatusItemProperty( 4, iif( IsScrollLockActive(), BLACK, SILVER ), hWnd, STATUS_ITEM_FONTCOLOR ) ;
      } )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _GetStatusItemWidth( hWnd, nItem )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Retrieves the width of a specific status bar panel or all panels.
 */
   LOCAL h
   LOCAL aItemWidth := {}
   LOCAL i

   FOR EACH h IN _HMG_aControlParentHandles

      i := hb_enumindex( h )

      IF _HMG_aControlType[ i ] == ITEMTYPENAME .AND. h == hWnd
         AAdd( aItemWidth, _HMG_aControlWidth[ i ] )
      ENDIF

   NEXT

RETURN iif( nItem == NIL, aItemWidth, aItemWidth[ hb_defaultValue( nItem, 1 ) ] )

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusItemProperty( nItem, Value, hWnd, nType )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Updates specific properties (Width, Action, Colors, Alignment) of a status bar panel.
 * Reasoning: Since status bar items aren't full Win32 controls, HMG manages their 
 * properties via this dispatcher which updates internal arrays and triggers refreshes.
 */
   LOCAL h
   LOCAL FormName
   LOCAL nIndex := 0
   LOCAL i

   FOR EACH h IN _HMG_aControlParentHandles

      i := hb_enumindex( h )

      IF _HMG_aControlType[ i ] == ITEMTYPENAME .AND. h == hWnd

         IF ++nIndex == nItem

            SWITCH nType
            CASE STATUS_ITEM_WIDTH
               _HMG_aControlWidth[ i ] := Value
               EXIT
            CASE STATUS_ITEM_ACTION
               _HMG_aControlProcedures[ i ] := Value
               EXIT
            CASE STATUS_ITEM_BACKCOLOR
               _HMG_aControlBkColor[ i ] := Value
               EXIT
            CASE STATUS_ITEM_FONTCOLOR
               _HMG_aControlFontColor[ i ] := Value
               EXIT
            CASE STATUS_ITEM_ALIGN
               _HMG_aControlSpacing[ i ] := Value
            ENDSWITCH

            // If visual properties changed, force a redraw of the item
            IF nType > STATUS_ITEM_ACTION
               FormName := GetParentFormName( i )
               _SetItem ( "StatusBar", FormName, nItem, _GetItem ( "StatusBar", FormName, nItem ) )
            ENDIF

            EXIT

         ENDIF

      ENDIF

   NEXT

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusProgressMessage ( BarName, FormName, width, tooltip, action, nValue, nMin, nMax )
*-----------------------------------------------------------------------------*
/*
 * Purpose: Embeds a Progress Bar control into a status bar panel.
 * Logic: 
 *    1. Creates a placeholder status item.
 *    2. Creates a Win32 ProgressBar as a child of the status bar.
 *    3. Positions the progress bar over the status item's coordinates.
 */
   LOCAL hwndStatus, hwndProgress
   LOCAL nrItem
   LOCAL i

   hb_default( @nValue, 0 )
   hb_default( @nMin, 0 )
   hb_default( @nMax, 100 )
   __defaultNIL( @Width, 70 )
   __defaultNIL( @ToolTip, "" )
   __defaultNIL( @Action, "" )

   IF _HMG_BeginWindowActive
      hwndStatus := GetControlHandle ( BarName, FormName )
#ifdef _HMG_COMPAT_
   ELSE
      hwndStatus := _HMG_ActiveStatusHandle
      FormName := GetParentFormName ( GetControlIndexByHandle ( hwndStatus ) )
#endif
   ENDIF

   nrItem := _DefineItemMessage ( PROGRESSNAME, BarName, 0, 0, '', action, width, 0, , "", ToolTip )

   hwndProgress := CreateProgressBarItem ( hwndStatus, nrItem, nValue, nMin, nMax )
   i := GetControlIndex ( PROGRESSNAME, FormName )

   _HMG_aControlMiscData1[ i ] := { hwndStatus, hwndProgress }
   _HMG_aControlRangeMin[ i ] := nMin
   _HMG_aControlRangeMax[ i ] := nMax
   _HMG_aControlValue[ i ] := nValue

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusProgressPos ( FormName, nValue )
*-----------------------------------------------------------------------------*
   LOCAL i

   IF ( i := GetControlIndex ( PROGRESSNAME, FormName ) ) > 0
      SetPosProgressBarItem ( _HMG_aControlMiscData1[ i, 2 ], hb_defaultValue( nValue, 0 ) )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetStatusProgressRange ( FormName, nMin, nMax )
*-----------------------------------------------------------------------------*
   LOCAL i

   IF ( i := GetControlIndex ( PROGRESSNAME, FormName ) ) > 0
      SetProgressBarRange ( _HMG_aControlMiscData1[ i, 2 ], hb_defaultValue( nMin, 0 ), hb_defaultValue( nMax, 100 ) )
   ENDIF

RETURN NIL
