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

/*-------------------------------------------------------------------------*/ 
/* Menu style constants                                                    */
/*-------------------------------------------------------------------------*/

#define MENU_STYLE_MAIN            1
#define MENU_STYLE_BREAK           1
#define MENU_STYLE_BREAKSEP        2

#define MENU_CTX_STANDARD          3
#define MENU_CTX_NOTIFY            4
#define MENU_CTX_OWNER             5

#define MENU_STYLE_MAIN_ROOT       6
#define MENU_STYLE_CONTEXT_POPUP   7
#define MENU_STYLE_CONTEXT_ROOT    8


// Static variables to store font handles for standard and context popup menus.
// These ensure that nested menus inherit the correct visual style.
STATIC _HMG_xPopupMenuFont
STATIC _HMG_xContextPopupMenuFont

/*
 * FUNCTION: _InitCommonControl
 * Purpose: Initializes the internal HMG state arrays for a new control.
 * Parameters:
 *    k         - Numeric: The index in the global HMG control arrays.
 *    cType     - String: The type of control (e.g., "MENU", "POPUP").
 *    cName     - String: The internal name assigned to the control.
 *    cParent   - String: The name of the parent form.
 *    nRow/nCol - Numeric: Screen coordinates.
 *    nWidth/nHeight - Numeric: Dimensions.
 *    nHelpId   - Numeric: Context-sensitive help ID.
 *    lInvisible- Logical: Initial visibility state.
 * Side Effects: Populates multiple global _HMG_aControl... arrays.
 */
STATIC FUNCTION _InitCommonControl( k, cType, cName, cParent, ;
      nRow, nCol, nWidth, nHeight, nHelpId, lInvisible )

   HB_SYMBOL_UNUSED( cParent )

   hb_default( @lInvisible, .F. )
   hb_default( @nHelpId, 0 )

   // Initialize basic identification and handle arrays
   _HMG_aControlType[ k ] := cType
   _HMG_aControlNames[ k ] := cName
   _HMG_aControlHandles[ k ] := 0
   _HMG_aControlParentHandles[ k ] := 0
   _HMG_aControlIds[ k ] := 0

   // Store geometry data
   _HMG_aControlRow[ k ] := nRow
   _HMG_aControlCol[ k ] := nCol
   _HMG_aControlWidth[ k ] := nWidth
   _HMG_aControlHeight[ k ] := nHeight

   // Set initial status flags
   _HMG_aControlVisible[ k ] := ! lInvisible
   _HMG_aControlHelpId[ k ] := nHelpId
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlDeleted[ k ] := .F.

   // Container offsets are initialized to -1 to indicate no specific container parent
   _HMG_aControlContainerRow[ k ] := -1
   _HMG_aControlContainerCol[ k ] := -1

   // Initialize event procedures and data masks
   _HMG_aControlProcedures[ k ] := NIL
   _HMG_aControlPageMap[ k ] := 0
   _HMG_aControlValue[ k ] := NIL
   _HMG_aControlInputMask[ k ] := ""

   // Standard event callbacks
   _HMG_aControllostFocusProcedure[ k ] := ""
   _HMG_aControlGotFocusProcedure[ k ] := ""
   _HMG_aControlChangeProcedure[ k ] := ""

   // Visual properties
   _HMG_aControlBkColor[ k ] := NIL
   _HMG_aControlFontColor[ k ] := NIL

   // Interaction callbacks
   _HMG_aControlDblClick[ k ] := ""
   _HMG_aControlHeadClick[ k ] := {}

   // Miscellaneous layout and resource handles
   _HMG_aControlSpacing[ k ] := 0
   _HMG_aControlPicture[ k ] := ""
   _HMG_aControlContainerHandle[ k ] := 0

   // Font definition defaults
   _HMG_aControlFontName[ k ] := ""
   _HMG_aControlFontSize[ k ] := 0
   _HMG_aControlFontAttributes[ k ] := { .F., .F., .F., .F. }

   _HMG_aControlToolTip[ k ] := ""

   // Range limits for controls that support them
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := 0

   _HMG_aControlCaption[ k ] := ""

   // GDI resource handles
   _HMG_aControlFontHandle[ k ] := 0
   _HMG_aControlBrushHandle[ k ] := 0

   // Generic data slots for internal framework use
   _HMG_aControlMiscData1[ k ] := 0
   _HMG_aControlMiscData2[ k ] := ""
RETURN NIL

/*
 * FUNCTION: _RegisterControlName
 * Purpose: Registers a control name within the HMG namespace to allow variable-based access.
 * Parameters:
 *    cForm - String: Parent form name.
 *    cName - String: Control name.
 *    k     - Numeric: Index in the control arrays.
 * Returns: The generated variable name string.
 */
STATIC FUNCTION _RegisterControlName( cForm, cName, k )
   LOCAL mVar

   IF Empty( cName )
      RETURN ""
   ENDIF

   // Construct the internal variable name used for command-style access
   mVar := "_" + cForm + "_" + cName

#ifdef _NAMES_LIST_
   // If using the names list optimization, store the index there
   _SetNameList( mVar, k )
#else
   // Otherwise, create a PUBLIC variable dynamically
   PUBLIC &mVar. := k
#endif
RETURN mVar

/*
 * FUNCTION: _NormalizeMenuName
 * Purpose: Ensures a menu item has a valid name for internal tracking.
 * Parameters:
 *    cName   - String/Undefined: The user-provided name.
 *    k       - Numeric: The control index.
 *    cPrefix - String: Prefix for auto-generated names.
 * Returns: A valid string name.
 */
STATIC FUNCTION _NormalizeMenuName( cName, k, cPrefix )
   hb_default( @cPrefix, "DummyMenuName" )

   IF ValType( cName ) == 'U'
#ifndef _EMPTY_MENU_
      // Generate a unique name if none was provided to ensure the control can be indexed
      cName := cPrefix + hb_ntos( k )
#else
      cName := ""
#endif
   ENDIF
RETURN cName

/*
 * FUNCTION: _MenuContext
 * Purpose: Retrieves the current menu context (Main Menu vs Context Menu).
 * Returns: An array containing {CurrentPopupHandle, ParentHandle, RootHandle, ParentName, IsMainMenu}.
 * Reasoning: This abstraction allows the same item definition logic to work for both menu types.
 */
STATIC FUNCTION _MenuContext()
   LOCAL lMain := ( _HMG_xMenuType == 'MAIN' )

   IF lMain
      RETURN { _HMG_xMenuPopupHandle[ _HMG_xMenuPopupLevel ], ;
         _HMG_xMainMenuParentHandle, ;
         _HMG_xMainMenuHandle, ;
         _HMG_xMainMenuParentName, .T. }
   ENDIF
   
   // Context menu logic: determine if we are in a sub-popup or the root context menu
   RETURN { iif( _HMG_xContextPopupLevel > 0, _HMG_xContextPopupHandle[ _HMG_xContextPopupLevel ], _HMG_xContextMenuHandle ), ;
      _HMG_xContextMenuParentHandle, _HMG_xContextMenuHandle, ;
      _HMG_xContextMenuParentName, .F. }

/*
 * FUNCTION: _ResolveContextMenuType
 * Purpose: Maps the internal HMG menu type string to a numeric style for Win32 API.
 * Returns: Numeric style.
 */
STATIC FUNCTION _ResolveContextMenuType()
   SWITCH Left( _HMG_xMenuType, 1 )
   CASE 'C' // CONTEXT
      RETURN MENU_CTX_STANDARD
   CASE 'N' // NOTIFY
      RETURN MENU_CTX_NOTIFY
   CASE 'O' // OWNCONTEXT
   CASE 'D' // DROPDOWN
      RETURN MENU_CTX_OWNER
   ENDSWITCH
RETURN MENU_CTX_STANDARD

/*
 * FUNCTION: _ResolveMenuBreakStyle
 * Purpose: Determines the Win32 menu flags for breaks and separators.
 * Parameters:
 *    lBreak     - Logical: If a column break is requested.
 *    lSeparator - Logical: If the item is a separator.
 *    lMain      - Logical: If this is part of the main menu bar.
 */
STATIC FUNCTION _ResolveMenuBreakStyle( lBreak, lSeparator, lMain )
   LOCAL nStyle

   hb_default( @lBreak, .F. )
   hb_default( @lSeparator, .F. )
   
   // Default styles based on menu depth and type
   nStyle := iif( lMain, MENU_STYLE_MAIN_ROOT, ;
      iif( _HMG_xContextPopupLevel > 0, MENU_STYLE_CONTEXT_POPUP, MENU_STYLE_CONTEXT_ROOT ) )
   
   IF lBreak
      nStyle := iif( lSeparator, MENU_STYLE_BREAKSEP, MENU_STYLE_BREAK )
   ENDIF
RETURN nStyle

/*
 * FUNCTION: _ApplyMenuVisuals
 * Purpose: Applies bitmaps, icons, and fonts to a specific menu item via Win32 API.
 * Returns: Handle to the bitmap if one was created.
 */
STATIC FUNCTION _ApplyMenuVisuals( hMenu, nId, cImage, cIcon, cCheckImage, hFont )
   LOCAL hBitmap := 0

   // Priority: Bitmaps first, then Icons
   IF ValType( cImage ) != 'U'
      hBitmap := MenuItem_SetBitMaps( hMenu, nId, cImage, "" )
   ELSEIF ValType( cIcon ) != 'U'
      hBitmap := MenuItem_SetIcon( hMenu, nId, cIcon )
   ENDIF
   
   // Custom checkmark images
   IF ValType( cCheckImage ) != 'U'
      MenuItem_SetCheckMarks( hMenu, nId, cCheckImage, "" )
   ENDIF
   
   // Custom font per menu item
   IF ValType( hFont ) != 'U'
      MenuItem_SetFont( hMenu, nId, hFont )
   ENDIF
RETURN hBitmap

/*
 * FUNCTION: _InitMenuControl
 * Purpose: Specialized initialization for menu-type controls.
 */
STATIC FUNCTION _InitMenuControl( k, cType, cName, cForm, hHandle, hParent, nId, bAction, cCaption )
   // Initialize standard properties first
   _InitCommonControl( k, cType, cName, cForm, 0, 0, 0, 0, 0, .F. )

   // Assign menu-specific handles and IDs
   _HMG_aControlHandles[ k ] := hHandle
   _HMG_aControlParentHandles[ k ] := hParent
   _HMG_aControlIds[ k ] := nId
   _HMG_aControlProcedures[ k ] := bAction
   _HMG_aControlCaption[ k ] := cCaption
RETURN NIL

/*
 * PROCEDURE: _ApplyMenuState
 * Purpose: Sets the initial visual state (checked, disabled, default) of a menu item.
 */
STATIC PROCEDURE _ApplyMenuState( hMenu, nId, k, lChecked, lDisabled, lDefault )
   IF lChecked
      xCheckMenuItem( hMenu, nId )
   ENDIF

   IF lDisabled
      xDisableMenuItem( hMenu, nId )
      _HMG_aControlEnabled[ k ] := .F.
   ENDIF

   IF lDefault
      // Sets the item in bold (standard Windows behavior for default actions)
      SetMenuDefaultItem( hMenu, nId )
   ENDIF
RETURN

/*
 * PROCEDURE: _RunControlInit
 * Purpose: Executes the OOP initialization block if the OOP layer is enabled.
 */
STATIC PROCEDURE _RunControlInit( k, mVar )
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
   ENDIF
RETURN

/*
 * PROCEDURE: _InitContextMenu
 * Purpose: Prepares the global state for defining a new context-sensitive menu.
 */
STATIC PROCEDURE _InitContextMenu( cType, cParent, nStyle )
   hb_default( @cParent, _HMG_ActiveFormName )

   _HMG_xContextMenuHandle := CreatePopupMenu( nStyle )
   _HMG_xContextMenuParentHandle := GetFormHandle( cParent )
   _HMG_xContextMenuParentName := cParent
   _HMG_xContextPopupLevel := 0
   _HMG_xMenuPopupLevel := 0
   _HMG_xMenuType := cType
RETURN

/*
 * PROCEDURE: _DefineMainMenu
 * Purpose: Starts the definition of a window's main menu bar.
 */
PROCEDURE _DefineMainMenu( cParent )
   hb_default( @cParent, _HMG_ActiveFormName )

   _HMG_xMenuType := "MAIN"
   _HMG_xMainMenuParentName := cParent
   _HMG_xMainMenuParentHandle := GetFormHandle( cParent )
   _HMG_xMainMenuHandle := CreateMenu()
   _HMG_xMenuPopupLevel := 0
RETURN

/*
 * PROCEDURE: _DefineMenuPopup
 * Purpose: Defines a sub-menu (POPUP) within a Main or Context menu.
 * Parameters:
 *    cCaption - String: Display text.
 *    cName    - String: Internal name.
 *    cImage   - String: Optional bitmap path.
 *    hFont    - Mixed: Font name or handle.
 */
PROCEDURE _DefineMenuPopup( cCaption, cName, cImage, hFont )
   LOCAL nLevel, hPopup, hParentPopup, k, cForm, mVar, nStyle, lMain

   // Convert font name to handle if necessary
   IF ISCHARACTER( hFont )
      hFont := GetFontHandle( hFont )
   ENDIF
   
   lMain := ( _HMG_xMenuType == "MAIN" )

   // Validate that we are inside a valid menu definition block
   IF !( _HMG_xMenuType $ "MAIN,CONTEXT,OWNCONTEXT,NOTIFY,DROPDOWN" )
      MsgMiniGuiError( "Menu type incorrect." )
      RETURN
   ENDIF

   IF lMain
      // Store the font at the top level if not already set
      IF _HMG_xMenuPopupLevel == 0 .AND. ValType( hFont ) != 'U' .AND. ValType( _HMG_xPopupMenuFont ) == 'U'
         _HMG_xPopupMenuFont := hFont
      ENDIF
      
      _HMG_xMenuPopupLevel++
      nLevel := _HMG_xMenuPopupLevel
      nStyle := 1
      hPopup := CreatePopupMenu( nStyle )
      _HMG_xMenuPopupHandle[ nLevel ] := hPopup
      _HMG_xMenuPopupCaption[ nLevel ] := cCaption
      
      // If nested, attach this popup to the previous level's popup
      IF nLevel > 1
         hParentPopup := _HMG_xMenuPopupHandle[ nLevel - 1 ]
         AppendMenuPopup( hParentPopup, hPopup, cCaption, MENU_STYLE_BREAKSEP, hFont )
      ENDIF
      cForm := _HMG_xMainMenuParentName
   ELSE
      // Context menu logic (similar to Main Menu but using Context globals)
      IF _HMG_xContextPopupLevel == 0 .AND. ValType( hFont ) != 'U' .AND. ValType( _HMG_xContextPopupMenuFont ) == 'U'
         _HMG_xContextPopupMenuFont := hFont
      ENDIF
      
      _HMG_xContextPopupLevel++
      nLevel := _HMG_xContextPopupLevel
      nStyle := _ResolveContextMenuType()
      hPopup := CreatePopupMenu( nStyle )
      _HMG_xContextPopupHandle[ nLevel ] := hPopup
      _HMG_xContextPopupCaption[ nLevel ] := cCaption
      
      IF nLevel > 1
         hParentPopup := _HMG_xContextPopupHandle[ nLevel - 1 ]
         AppendMenuPopup( hParentPopup, hPopup, cCaption, nStyle, hFont )
      ENDIF
      cForm := _HMG_xContextMenuParentName
   ENDIF

   // Register the popup as a control in the HMG system
   k := _GetControlFree()
   cName := _NormalizeMenuName( cName, k, "DummyPopupName" )
   mVar := _RegisterControlName( cForm, cName, k )

   _InitMenuControl( k, "POPUP", cName, cForm, hPopup, ;
      iif( lMain, _HMG_xMainMenuParentHandle, _HMG_xContextMenuParentHandle ), ;
      nLevel, NIL, cCaption )

   // Store metadata for visual processing during _EndMenu
   _HMG_aControlPicture[ k ] := cImage
   _HMG_aControlPageMap[ k ] := iif( lMain, _HMG_xMainMenuHandle, _HMG_xContextMenuHandle )
   _HMG_aControlSpacing[ k ] := hPopup

   _RunControlInit( k, mVar )
RETURN

/*
 * PROCEDURE: _EndMenuPopup
 * Purpose: Finalizes a sub-menu definition and returns to the parent level.
 */
PROCEDURE _EndMenuPopup()
   IF _HMG_xMenuType == "MAIN"
      _HMG_xMenuPopupLevel--
      // If we returned to the root, attach the first level popup to the main menu bar
      IF _HMG_xMenuPopupLevel == 0
         AppendMenuPopup( _HMG_xMainMenuHandle, _HMG_xMenuPopupHandle[ 1 ], ;
            _HMG_xMenuPopupCaption[ 1 ], MENU_STYLE_MAIN, _HMG_xPopupMenuFont )
      ENDIF
   ELSE
      _HMG_xContextPopupLevel--
      IF _HMG_xContextPopupLevel == 0
         AppendMenuPopup( _HMG_xContextMenuHandle, _HMG_xContextPopupHandle[ 1 ], _HMG_xContextPopupCaption[ 1 ], _ResolveContextMenuType(), _HMG_xContextPopupMenuFont )
      ENDIF
   ENDIF
RETURN

/*
 * PROCEDURE: _DefineMenuItem
 * Purpose: Defines a clickable item within a menu.
 * Parameters:
 *    cCaption   - String: Text to display.
 *    bAction    - Codeblock: Action to execute on click.
 *    cName      - String: Internal name.
 *    cImage     - String: Bitmap path.
 *    lChecked   - Logical: Initial check state.
 *    lDisabled  - Logical: Initial enabled state.
 *    cMessage   - String: Status bar message.
 *    hFont      - Mixed: Font handle or name.
 *    cCheckImage- String: Custom checkmark bitmap.
 *    lBreakMenu - Logical: Start a new column.
 *    lSeparator - Logical: Is this a separator?
 *    cIcon      - String: Icon resource/file.
 *    lDefault   - Logical: Is this the default item?
 */
PROCEDURE _DefineMenuItem( cCaption, bAction, cName, cImage, lChecked, lDisabled, cMessage, hFont, cCheckImage, lBreakMenu, lSeparator, cIcon, lDefault )
   LOCAL aCtx, hPopup, hParent, cForm, lMain, nId, k, mVar, hBitmap, nStyle

   hb_default( @lChecked, .F. )
   hb_default( @lDisabled, .F. )
   hb_default( @lBreakMenu, .F. )
   hb_default( @lDefault, .F. )

   IF ISCHARACTER( hFont )
      hFont := GetFontHandle( hFont )
   ENDIF

   // Determine where this item belongs
   aCtx := _MenuContext()
   hPopup := aCtx[ 1 ]
   hParent := aCtx[ 2 ]
   cForm := aCtx[ 4 ]
   lMain := aCtx[ 5 ]

   // Generate a unique ID for the Win32 menu item
   nId := _GetId()
   nStyle := _ResolveMenuBreakStyle( lBreakMenu, hb_defaultValue( lSeparator, .F. ), lMain )

   // Add the item to the Windows menu structure
   IF cCaption != "-"
      AppendMenuString( hPopup, nId, cCaption, nStyle )
   ENDIF

   // Apply visual enhancements
   hBitmap := _ApplyMenuVisuals( hPopup, nId, cImage, cIcon, cCheckImage, hFont )

   // Register in HMG control arrays
   k := _GetControlFree()
   cName := _NormalizeMenuName( cName, k )
   mVar := _RegisterControlName( cForm, cName, k )

   _InitMenuControl( k, "MENU", cName, cForm, hPopup, hParent, nId, bAction, cCaption )

   // Store specific menu metadata
   _HMG_aControlPageMap[ k ] := hPopup
   _HMG_aControlValue[ k ] := cMessage
   _HMG_aControlBrushHandle[ k ] := hBitmap
   _HMG_aControlMiscData1[ k ] := iif( lMain, 0, 1 )

   // Set initial state
   _ApplyMenuState( hPopup, nId, k, lChecked, lDisabled, lDefault )
   _RunControlInit( k, mVar )
RETURN

/*
 * PROCEDURE: _DefineSeparator
 * Purpose: Adds a horizontal separator line to the current menu.
 */
PROCEDURE _DefineSeparator()
   IF _HMG_xMenuType == "MAIN"
      AppendMenuSeparator( _HMG_xMenuPopupHandle[ _HMG_xMenuPopupLevel ] )
   ELSE
      IF _HMG_xContextPopupLevel > 0
         AppendMenuSeparator( _HMG_xContextPopupHandle[ _HMG_xContextPopupLevel ] )
      ELSE
         AppendMenuSeparator( _HMG_xContextMenuHandle )
      ENDIF
   ENDIF
   
   // If using extended menu styles, separators are treated as special menu items
   IF IsExtendedMenuStyleActive()
      _DefineMenuItem( "-" )
   ENDIF
RETURN

/*
 * PROCEDURE: _DefineContextMenu
 * Purpose: Starts definition of a standard right-click context menu.
 */
PROCEDURE _DefineContextMenu( cParent )
   _InitContextMenu( "CONTEXT", cParent, MENU_CTX_STANDARD )
RETURN

/*
 * PROCEDURE: _DefineNotifyMenu
 * Purpose: Starts definition of a menu for a system tray (Notify) icon.
 */
PROCEDURE _DefineNotifyMenu( cParent )
   _InitContextMenu( "NOTIFY", cParent, MENU_CTX_NOTIFY )
RETURN

/*
 * PROCEDURE: _DefineDropDownMenu
 * Purpose: Starts definition of a menu that drops down from a specific button.
 */
PROCEDURE _DefineDropDownMenu( cButton, cParent )
   _InitContextMenu( "DROPDOWN", cParent, MENU_CTX_OWNER )

   IF cParent == NIL
      cParent := _HMG_ActiveFormName
   ENDIF
   _HMG_xContextMenuButtonIndex := GetControlIndex( cButton, cParent )
RETURN

/*
 * PROCEDURE: _DefineControlContextMenu
 * Purpose: Starts definition of a context menu assigned to specific controls.
 */
PROCEDURE _DefineControlContextMenu( uControl, cParent )
   _InitContextMenu( "OWNCONTEXT", cParent, MENU_CTX_OWNER )

   IF cParent == NIL
      cParent := _HMG_ActiveFormName
   ENDIF
   
   // Handle both single control names and arrays of control names
   IF ISARRAY( uControl )
      _HMG_xContextMenuButtonIndex := {}
      AEval( uControl, {| x | AAdd( _HMG_xContextMenuButtonIndex, GetControlIndex( x, cParent ) ) } )
   ELSE
      _HMG_xContextMenuButtonIndex := GetControlIndex( uControl, cParent )
   ENDIF
RETURN

/*----------------------------------------------------------------------*/
PROCEDURE _EndMenu()
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Finalizes the definition of a menu and attaches it to its parent container.
   INPUTS:  None (Uses global HMG state variables).
   SIDE EFFECTS: Updates internal HMG arrays, sets window menus, and applies bitmaps to popups.
   REASONING: HMG Extended uses a state-based menu definition (DEFINE MENU... END MENU). 
              This procedure is the "committer" that takes the handle created during 
              the definition phase and assigns it to the correct internal registry 
              based on the menu type (Main, Context, Notify, etc.).
*/
   LOCAL image, i, h, k

   // Determine the menu category based on the first character of the internal type string.
   SWITCH Left( _HMG_xMenuType, 1 )
   
   CASE 'M' // Main Menu
      // Attaches the created menu handle to the parent window handle.
      SetMenu( _HMG_xMainMenuParentHandle, _HMG_xMainMenuHandle )
      EXIT
      
   CASE 'C' // Context Menu (Right-click menu for a Form)
      i := GetFormIndex( _HMG_xContextMenuParentName )
      _HMG_aFormContextMenuHandle[ i ] := _HMG_xContextMenuHandle
      EXIT
      
   CASE 'N' // Notify Menu (System Tray / Taskbar Icon menu)
      i := GetFormIndex( _HMG_xContextMenuParentName )
      _HMG_aFormNotifyMenuHandle[ i ] := _HMG_xContextMenuHandle
      EXIT
      
   CASE 'D' // Dropdown Menu (Associated with specific button types)
      _HMG_aControlRangeMax[ _HMG_xContextMenuButtonIndex ] := _HMG_xContextMenuHandle
      EXIT
      
   CASE 'O' // Control-specific Context Menu
      /* 
         If the menu is assigned to a control (or array of controls), we register 
         the relationship in the global context menu tracking array.
      */
      IF ISARRAY( _HMG_xContextMenuButtonIndex )
         FOR i := 1 TO Len( _HMG_xContextMenuButtonIndex )
            h := _HMG_aControlHandles[ _HMG_xContextMenuButtonIndex[ i ] ]
            IF ISARRAY( h )
               // Handle controls that might have multiple sub-handles (like certain complex widgets).
               AEval( h, {| x | AAdd( _HMG_aControlsContextMenu, { x, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex[ i ], .T. } ) } )
            ELSE
               AAdd( _HMG_aControlsContextMenu, { h, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex[ i ], .T. } )
               // Labels and Images require SS_NOTIFY style to receive mouse clicks for context menus.
               IF _HMG_aControlType[ _HMG_xContextMenuButtonIndex[ i ] ] $ 'image,LABEL'
                  ChangeStyle ( h, SS_NOTIFY )
               ENDIF
            ENDIF
         NEXT
      ELSE
         h := _HMG_aControlHandles[ _HMG_xContextMenuButtonIndex ]
         IF ISARRAY( h )
            AEval( h, {| x | AAdd( _HMG_aControlsContextMenu, { x, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex, .T. } ) } )
         ELSE
            AAdd( _HMG_aControlsContextMenu, { h, _HMG_xContextMenuHandle, _HMG_xContextMenuButtonIndex, .T. } )
         ENDIF
      ENDIF
   ENDSWITCH

   /*
      Post-processing: Apply bitmaps/icons to POPUP items.
      HMG stores the image path in _HMG_aControlPicture for menu items.
   */
   FOR EACH h IN _HMG_aControlHandles
      i := hb_enumindex( h )
      IF _HMG_aControlType[ i ] == "POPUP"
         image := _HMG_aControlPicture[ i ]
         IF ValType( image ) != 'U'
            k := _HMG_aControlSpacing[ i ]
            // MenuItem_SetBitMaps is a low-level C wrapper to attach GDI resources to menu items.
            _HMG_aControlBrushHandle[ i ] := MenuItem_SetBitMaps( _HMG_aControlPageMap[ i ], k, image, '' )
         ENDIF
      ENDIF
   NEXT
RETURN

/*----------------------------------------------------------------------*/
STATIC FUNCTION _GetMenuIds( ItemName, FormName )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Internal helper to retrieve the WinAPI Menu Handle and Item ID.
   INPUTS:  ItemName (String), FormName (String).
   RETURNS: Array { MenuHandle, ItemID }.
   REASONING: Most menu manipulation functions require the parent menu handle and the 
              specific ID. This abstracts the lookup from HMG's internal "Big Arrays".
*/
   LOCAL x := GetControlIndex( ItemName, FormName )

   IF x > 0
      IF _HMG_aControlType[ x ] == "MENU"
         // For standard items, PageMap stores the Menu Handle, ControlIds stores the ID.
         RETURN { _HMG_aControlPageMap[ x ], _HMG_aControlIds[ x ] }
      ELSEIF _HMG_aControlType[ x ] == "POPUP"
         // For Popups, Spacing is often used to store the sub-menu ID.
         RETURN { _HMG_aControlPageMap[ x ], _HMG_aControlSpacing[ x ] }
      ENDIF
   ENDIF
RETURN { 0, 0 }

/*----------------------------------------------------------------------*/
FUNCTION _DefaultMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Sets a menu item as the "default" (usually rendered in bold).
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   SetMenuDefaultItem ( a[ 1 ], a[ 2 ] )
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _DisableMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Disables a menu item (greyed out, non-clickable).
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   xDisableMenuItem ( a[ 1 ], a[ 2 ] )
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _EnableMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Enables a previously disabled menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   xEnableMenuItem ( a[ 1 ], a[ 2 ] )
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _CheckMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Places a checkmark next to the menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   xCheckMenuItem ( a[ 1 ], a[ 2 ] )
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _UncheckMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Removes a checkmark from the menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   xUncheckMenuItem ( a[ 1 ], a[ 2 ] )
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _IsMenuItemChecked ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Returns .T. if the item is currently checked.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
RETURN xGetMenuCheckState ( a[ 1 ], a[ 2 ] )

/*----------------------------------------------------------------------*/
FUNCTION _IsMenuItemEnabled ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Returns .T. if the item is currently enabled.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
RETURN xGetMenuEnabledState ( a[ 1 ], a[ 2 ] )

/*----------------------------------------------------------------------*/
PROCEDURE _ShowContextMenu( Parent, nRow, nCol )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Programmatically displays a context menu.
   INPUTS:  Parent (Window Name), nRow/nCol (Screen coordinates).
   REASONING: Allows triggering context menus via code (e.g., keyboard shortcuts) 
              rather than just right-clicks.
*/
   LOCAL xContextMenuParentHandle, aPos

   hb_default( @Parent, "" )

   // Resolve the parent handle. If no parent is provided, use the last defined context menu parent.
   xContextMenuParentHandle := iif( _IsWindowDefined( Parent ), GetFormHandle( Parent ), _HMG_xContextMenuParentHandle )

   IF xContextMenuParentHandle == 0
      MsgMiniGuiError( "Context Menu is not defined." )
   ENDIF

   // If coordinates are omitted, use the current mouse cursor position.
   IF hb_defaultValue( nRow, 0 ) == 0 .AND. hb_defaultValue( nCol, 0 ) == 0
      aPos := GetCursorPos()
      nRow := aPos[ 1 ]
      nCol := aPos[ 2 ]
   ENDIF

   // TrackPopupMenu is the WinAPI call that actually renders the floating menu.
   TrackPopupMenu( _HMG_xContextMenuHandle, nCol, nRow, xContextMenuParentHandle )
   DoEvents()
RETURN

/*----------------------------------------------------------------------*/
FUNCTION _GetMenuItemCaption ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Retrieves the text label of a menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
RETURN xGetMenuCaption( a[ 1 ], a[ 2 ] )

/*----------------------------------------------------------------------*/
FUNCTION _SetMenuItemCaption ( ItemName, FormName, Caption )
/*----------------------------------------------------------------------*/
// Updates the text label of a menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
RETURN xSetMenuCaption( a[ 1 ], a[ 2 ], Caption )

/*----------------------------------------------------------------------*/
PROCEDURE _SetMenuItemBitmap( ItemName, FormName, Bitmap )
/*----------------------------------------------------------------------*/
// Assigns a bitmap image to a menu item.
   LOCAL a := _GetMenuIds( ItemName, FormName ), idx := GetControlIndex( ItemName, FormName )

   _HMG_aControlBrushHandle[ idx ] := MenuItem_SetBitMaps( a[ 1 ], a[ 2 ], Bitmap, '' )
RETURN

/*----------------------------------------------------------------------*/
PROCEDURE _SetMenuItemIcon( ItemName, FormName, Icon )
/*----------------------------------------------------------------------*/
// Assigns an icon resource to a menu item.
   LOCAL a := _GetMenuIds( ItemName, FormName ), idx := GetControlIndex( ItemName, FormName )

   _HMG_aControlBrushHandle[ idx ] := MenuItem_SetIcon( a[ 1 ], a[ 2 ], Icon )
RETURN

/*----------------------------------------------------------------------*/
FUNCTION _SetMenuItemFont( ItemName, FormName, Font )
/*----------------------------------------------------------------------*/
// Changes the font of a specific menu item (requires owner-draw or HMG Extended internal support).
   LOCAL a := _GetMenuIds( ItemName, FormName )
RETURN MenuItem_SetFont( a[ 1 ], a[ 2 ], iif( ISCHARACTER( Font ), GetFontHandle( Font ), Font ) )

/*----------------------------------------------------------------------*/
PROCEDURE _ShowControlContextMenu( Control, Parent, lShow )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Enables or disables the context menu for a specific control.
   INPUTS:  Control (Name), Parent (Window Name), lShow (Logical toggle).
   REASONING: Allows developers to dynamically suppress context menus based on application state.
*/
   LOCAL h := GetControlHandle( Control, Parent ), i, j

   IF ISARRAY( h )
      FOR j := 1 TO Len( h )
         FOR i := 1 TO Len( _HMG_aControlsContextMenu )
            IF _HMG_aControlsContextMenu[ i, 1 ] == h[ j ]
               _HMG_aControlsContextMenu[ i, 4 ] := lShow
            ENDIF
         NEXT
      NEXT
   ELSE
      FOR i := 1 TO Len( _HMG_aControlsContextMenu )
         IF _HMG_aControlsContextMenu[ i, 1 ] == h
            _HMG_aControlsContextMenu[ i, 4 ] := lShow
         ENDIF
      NEXT
   ENDIF
RETURN

/*----------------------------------------------------------------------*/
FUNCTION _InsertMenuItem ( ItemName, FormName, caption, action, name, Image )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Dynamically adds a new menu item to an existing menu at runtime.
   INPUTS:  ItemName (Reference item), FormName, caption, action (codeblock/function), 
            name (internal name), Image (optional bitmap).
   REASONING: Essential for applications that need to build menus based on database 
              records or user permissions after the window is already created.
*/
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   LOCAL Controlhandle := a[ 1 ]
   LOCAL hBitmap := 0
   LOCAL mVar
   LOCAL Id

   Id := _GetId() // Generates a unique internal ID for the new menu item.

   // Handle the creation of the public variable that stores the control index.
   IF ValType ( name ) != 'U'
      mVar := '_' + _HMG_xMainMenuParentName + '_' + name

#ifdef _NAMES_LIST_
      _SetNameList( mVar, Len( _HMG_aControlNames ) + 1 )
#else
      PUBLIC &mVar. := Len( _HMG_aControlNames ) + 1
#endif
   ELSE
      mVar := '_MenuDummyVar'
#ifdef _NAMES_LIST_
      _SetNameList( mVar, 0 )
#else
      PUBLIC &mVar. := 0
#endif
   ENDIF

   // Low-level WinAPI insertion.
   InsertMenuItem ( Controlhandle, a[ 2 ], Id, caption )

   IF ValType ( image ) != 'U'
      hBitmap := MenuItem_SetBitMaps ( Controlhandle, Id, image, '' )
   ENDIF

   /* 
      Register the new item in HMG's global "Big Arrays". 
      This is necessary so the event loop can route clicks to the 'action' procedure.
   */
   AAdd ( _HMG_aControlType, "MENU" )
   AAdd ( _HMG_aControlNames, Name )
   AAdd ( _HMG_aControlHandles, Controlhandle )
   AAdd ( _HMG_aControlParentHandles, _HMG_xMainMenuParentHandle )
   AAdd ( _HMG_aControlIds, id )
   AAdd ( _HMG_aControlProcedures, action )
   AAdd ( _HMG_aControlPageMap, a[ 1 ] )
   AAdd ( _HMG_aControlValue, Nil )
   AAdd ( _HMG_aControlInputMask, "" )
   AAdd ( _HMG_aControllostFocusProcedure, "" )
   AAdd ( _HMG_aControlGotFocusProcedure, "" )
   AAdd ( _HMG_aControlChangeProcedure, "" )
   AAdd ( _HMG_aControlDeleted, .F. )
   AAdd ( _HMG_aControlBkColor, Nil )
   AAdd ( _HMG_aControlFontColor, Nil )
   AAdd ( _HMG_aControlDblClick, "" )
   AAdd ( _HMG_aControlHeadClick, {} )
   AAdd ( _HMG_aControlRow, 0 )
   AAdd ( _HMG_aControlCol, 0 )
   AAdd ( _HMG_aControlWidth, 0 )
   AAdd ( _HMG_aControlHeight, 0 )
   AAdd ( _HMG_aControlSpacing, 0 )
   AAdd ( _HMG_aControlContainerRow, -1 )
   AAdd ( _HMG_aControlContainerCol, -1 )
   AAdd ( _HMG_aControlPicture, "" )
   AAdd ( _HMG_aControlContainerHandle, 0 )
   AAdd ( _HMG_aControlFontName, '' )
   AAdd ( _HMG_aControlFontSize, 0 )
   AAdd ( _HMG_aControlFontAttributes, { .F., .F., .F., .F. } )
   AAdd ( _HMG_aControlToolTip, '' )
   AAdd ( _HMG_aControlRangeMin, 0 )
   AAdd ( _HMG_aControlRangeMax, 0 )
   AAdd ( _HMG_aControlCaption, Caption )
   AAdd ( _HMG_aControlVisible, .T. )
   AAdd ( _HMG_aControlHelpId, 0 )
   AAdd ( _HMG_aControlFontHandle, 0 )
   AAdd ( _HMG_aControlBrushHandle, hBitmap )
   AAdd ( _HMG_aControlEnabled, .T. )
   AAdd ( _HMG_aControlMiscData1, 0 )
   AAdd ( _HMG_aControlMiscData2, '' )

   // If Object-Oriented mode is enabled, notify the system of the new control.
   IF _HMG_lOOPEnabled
      Eval ( _HMG_bOnControlInit, Len( _HMG_aControlNames ), mVar )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _ModifyMenuItem ( ItemName, FormName, caption, action, name, Image )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Updates properties of an existing menu item.
   INPUTS:  ItemName, FormName, caption, action, name, Image.
   REASONING: Allows changing the behavior or label of a menu item without 
              deleting and recreating it.
*/
   LOCAL a := _GetMenuIds( ItemName, FormName )
   LOCAL x := GetControlIndex ( ItemName, FormName )
   LOCAL mVar
   LOCAL Id

   Id := _HMG_aControlIds[ x ]

   // Update the public variable reference if the name changed.
   IF ValType ( name ) != 'U'
      mVar := '_' + _HMG_xMainMenuParentName + '_' + name
#ifdef _NAMES_LIST_
      _SetNameList( mVar, x )
#else
      PUBLIC &mVar. := x
#endif
   ELSE
      mVar := '_MenuDummyVar'
#ifdef _NAMES_LIST_
      _SetNameList( mVar, 0 )
#else
      PUBLIC &mVar. := 0
#endif
   ENDIF

   ModifyMenuItem ( a[ 1 ], a[ 2 ], Id, Caption )

   // Update the image if provided, ensuring the old GDI object is deleted to prevent leaks.
   IF ValType ( image ) != 'U'
      DeleteObject ( _HMG_aControlBrushHandle[ x ] )
      _HMG_aControlBrushHandle[ x ] := MenuItem_SetBitMaps ( a[ 1 ], Id, image, '' )
   ENDIF

   // Sync the internal HMG arrays with the new values.
   _HMG_aControlNames[ x ] := name
   _HMG_aControlProcedures[ x ] := action
   _HMG_aControlCaption[ x ] := caption
RETURN NIL

/*----------------------------------------------------------------------*/
FUNCTION _RemoveMenuItem ( ItemName, FormName )
/*----------------------------------------------------------------------*/
// Deletes a menu item from the specified menu.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
RETURN RemoveMenuItem ( a[ 1 ], a[ 2 ] )

/*----------------------------------------------------------------------*/
PROCEDURE _ChangeMenuItemCaption ( ItemName, FormName, Caption )
/*----------------------------------------------------------------------*/
// Simplified wrapper to update only the text of a menu item.
   LOCAL a := _GetMenuIds ( ItemName, FormName )
   ModifyMenuItem ( a[ 1 ], a[ 2 ], _HMG_aControlIds[ GetControlIndex ( ItemName, FormName ) ], Caption )
RETURN

/*----------------------------------------------------------------------*/
FUNCTION HMG_SetMenuTheme( nType, cFormName, aUserDefined )
/*----------------------------------------------------------------------*/
/*
   PURPOSE: Applies a visual theme (colors and styles) to the application menus.
   INPUTS:  nType (Theme constant), cFormName (Target window), aUserDefined (Custom color array).
   RETURNS: The applied theme type.
   REASONING: Standard Win32 menus are difficult to style. HMG Extended provides 
              a custom drawing mechanism that uses these color definitions to 
              simulate modern looks (XP, Dark Mode, etc.).
*/
   LOCAL aColors := GetMenuColors()

   hb_default( @nType, MNUCLR_THEME_DEFAULT )

   // Default to the current active window if no form name is provided.
   IF PCount() < 2 .AND. Len( _HMG_aFormHandles ) > 0
      cFormName := ThisWindow.Name
   ENDIF

   // Initialize user-defined array if the user chose custom theme but provided no data.
   IF PCount() > 2 .AND. ! ISARRAY ( aUserDefined )
      aUserDefined := Array( 24 )
   ENDIF

   // Select the color palette and UI behavior based on the requested theme type.
   SWITCH nType

   CASE MNUCLR_THEME_DEFAULT
      /* Classic Windows styling with standard system colors. */
      aColors[ MNUCLR_MENUBARBACKGROUND1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ] := RGB( 192, 192, 192 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ] := RGB( 255, 252, 248 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ] := RGB( 136, 133, 116 )
      aColors[ MNUCLR_MENUITEMTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ] := RGB( 192, 192, 192 )
      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 182, 189, 210 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 182, 189, 210 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 246, 245, 244 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 207, 210, 200 )
      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 168, 169, 163 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 10, 36, 106 )

      SET MENUCURSOR FULL
      SET MENUSEPARATOR SINGLE RIGHTALIGN
      SET MENUITEM BORDER 3DSTYLE
      EXIT

   CASE MNUCLR_THEME_XP
      /* Blue/Silver gradients typical of the Windows XP era. */
      aColors[ MNUCLR_MENUBARBACKGROUND1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ] := GetSysColor( 7 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := GetSysColor( 14 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ] := GetSysColor( 17 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUITEMTEXT ] := GetSysColor( 7 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ] := GetSysColor( 14 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ] := GetSysColor( 17 )
      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := GetSysColor( 15 )
      aColors[ MNUCLR_SEPARATOR1 ] := GetSysColor( 17 )
      aColors[ MNUCLR_SEPARATOR2 ] := GetSysColor( 14 )
      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := GetSysColor( 13 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := GetSysColor( 13 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := GetSysColor( 17 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := GetSysColor( 14 )

      SET MENUCURSOR FULL
      SET MENUSEPARATOR DOUBLE RIGHTALIGN
      SET MENUITEM BORDER FLAT
      EXIT

   CASE MNUCLR_THEME_2000
      /* Flat, grey, high-contrast look of Windows 2000/NT. */
      aColors[ MNUCLR_MENUBARBACKGROUND1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ] := GetSysColor( 15 )

      aColors[ MNUCLR_MENUITEMTEXT ] := RGB( 0, 0, 0 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := RGB( 212, 208, 200 )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 212, 208, 200 )

      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 255, 255, 255 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB( 10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 255, 255, 255 )

      SET MENUCURSOR SHORT
      SET MENUSEPARATOR DOUBLE LEFTALIGN
      SET MENUITEM BORDER 3D
      EXIT

   CASE MNUCLR_THEME_DARK
      /* 
          Dark Theme: 
         A modern dark mode implementation using low-luminance greys and 
         off-white text to reduce eye strain.
      */
      aColors[ MNUCLR_MENUBARBACKGROUND1 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_MENUBARTEXT ] := RGB( 237, 237, 237 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ] := RGB( 65, 65, 65 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ] := RGB( 65, 65, 65 )

      aColors[ MNUCLR_MENUITEMTEXT ] := RGB( 237, 237, 237 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 65, 65, 65 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 65, 65, 65 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := RGB( 43, 43, 43 )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 43, 43, 43 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 43, 43, 43 )

      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 128, 128, 128 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB( 75, 75, 75 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB( 75, 75, 75 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 237, 237, 237 )

      SET MENUCURSOR FULL
      SET MENUSEPARATOR SINGLE LEFTALIGN
      SET MENUITEM BORDER FLAT
      EXIT

   DEFAULT /* MNUCLR_THEME_USER_DEFINED */
      /* 
         User Defined Theme: 
         Maps the 24 elements of the aUserDefined array to the internal 
         menu color structure. This allows for full customization.
      */
      aColors[ MNUCLR_MENUBARBACKGROUND1 ] := aUserDefined[ 1 ]
      aColors[ MNUCLR_MENUBARBACKGROUND2 ] := aUserDefined[ 2 ]
      aColors[ MNUCLR_MENUBARTEXT ] := aUserDefined[ 3 ]
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := aUserDefined[ 4 ]
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ] := aUserDefined[ 5 ]
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ] := aUserDefined[ 6 ]
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ] := aUserDefined[ 7 ]

      aColors[ MNUCLR_MENUITEMTEXT ] := aUserDefined[ 8 ]
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ] := aUserDefined[ 9 ]
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ] := aUserDefined[ 10 ]
      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := aUserDefined[ 11 ]
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := aUserDefined[ 12 ]
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := aUserDefined[ 13 ]
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := aUserDefined[ 14 ]
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ] := aUserDefined[ 15 ]
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ] := aUserDefined[ 16 ]

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := aUserDefined[ 17 ]
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := aUserDefined[ 18 ]

      aColors[ MNUCLR_SEPARATOR1 ] := aUserDefined[ 19 ]
      aColors[ MNUCLR_SEPARATOR2 ] := aUserDefined[ 20 ]

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := aUserDefined[ 21 ]
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := aUserDefined[ 22 ]
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := aUserDefined[ 23 ]
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := aUserDefined[ 24 ]

      SET MENUCURSOR FULL
      SET MENUSEPARATOR DOUBLE RIGHTALIGN
      SET MENUITEM BORDER FLAT

   END SWITCH

   // Apply the populated color array to the global HMG menu system.
   SetMenuColors( aColors )

   // If a specific form is targeted, update its background and refresh the menu bar.
   IF ISCHARACTER( cFormName )
      // Sync the form's background color with the menu bar's primary color.
      SetProperty( cFormName, "BackColor", aColors[ MNUCLR_MENUBARBACKGROUND1 ] )

      _ColorMenu ( GetFormHandle( cFormName ), nRGB2Arr( aColors[ MNUCLR_MENUBARBACKGROUND2 ] ) )
   ENDIF
RETURN nType
