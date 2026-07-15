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
 * STATIC FUNCTION _InitCommonControl
 * Internal helper to initialize the global HMG control state arrays.
 *
 * Parameters:
 *    nControl    : Numeric index in the global HMG control arrays.
 *    ControlName : String identifier for the control.
 *    cType       : String representing the control type (e.g., "PLAYER", "ANIMATEBOX").
 *    x, y, w, h  : Numeric coordinates and dimensions.
 *    HelpId      : Numeric ID for context-sensitive help.
 *    invisible   : Logical flag indicating if the control starts hidden.
 */
STATIC FUNCTION _InitCommonControl( nControl, ControlName, cType, x, y, w, h, HelpId, invisible )
   // Basic identification and state
   _HMG_aControlType[ nControl ]     := cType
   _HMG_aControlNames[ nControl ]    := ControlName
   _HMG_aControlDeleted[ nControl ]  := .F.
   _HMG_aControlEnabled[ nControl ]  := .T.
   _HMG_aControlVisible[ nControl ]  := !invisible
   _HMG_aControlValue[ nControl ]    := NIL
   _HMG_aControlInputMask[ nControl ]:= ""
   _HMG_aControlPageMap[ nControl ]  := {}
   _HMG_aControlHelpId[ nControl ]   := HelpId
   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0
   _HMG_aControlMiscData1[ nControl ]:= 0
   _HMG_aControlMiscData2[ nControl ]:= ""

   // Geometry and Container Logic
   _HMG_aControlRow[ nControl ]      := y
   _HMG_aControlCol[ nControl ]      := x
   _HMG_aControlWidth[ nControl ]    := w
   _HMG_aControlHeight[ nControl ]   := h
   _HMG_aControlSpacing[ nControl ]  := 0
   _HMG_aControlContainerRow[ nControl ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ nControl ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerHandle[ nControl ] := 0

   // Visual Properties
   _HMG_aControlBkColor[ nControl ]       := NIL
   _HMG_aControlFontColor[ nControl ]     := NIL
   _HMG_aControlPicture[ nControl ]       := ""
   _HMG_aControlFontName[ nControl ]      := ""
   _HMG_aControlFontSize[ nControl ]      := 0
   _HMG_aControlFontAttributes[ nControl ]:= { .F., .F., .F., .F. }
   _HMG_aControlToolTip[ nControl ]       := ""
   _HMG_aControlCaption[ nControl ]       := ""
   _HMG_aControlFontHandle[ nControl ]    := 0
   _HMG_aControlBrushHandle[ nControl ]   := 0

   // Event Callbacks
   // These store the character names of the procedures or codeblocks to be executed.
   _HMG_aControlProcedures[ nControl ]         := ""
   _HMG_aControllostFocusProcedure[ nControl ] := ""
   _HMG_aControlGotFocusProcedure[ nControl ]  := ""
   _HMG_aControlChangeProcedure[ nControl ]    := ""
   _HMG_aControlDblClick[ nControl ]           := ""
   _HMG_aControlHeadClick[ nControl ]          := {}
RETURN NIL

/*
 * STATIC FUNCTION _RegisterControl
 * Finalizes the registration of a control within the HMG environment.
 *
 * Parameters:
 *    nControl       : The internal index assigned to this control.
 *    ControlName    : The name of the control.
 *    ParentFormName : The name of the window containing this control.
 *    hControl       : The Win32 API handle (HWND) of the control.
 *    hParent        : The Win32 API handle (HWND) of the parent window.
 *    nId            : The resource ID (used primarily in Dialogs).
 *
 * Reasoning:
 *    This function bridges the gap between the low-level Win32 handle and the
 *    high-level HMG variable system. It creates a Public variable that allows
 *    programmers to reference the control index via _FormName_ControlName.
 */
STATIC FUNCTION _RegisterControl( nControl, ControlName, ParentFormName, cType, ;
      x, y, w, h, HelpId, invisible, hControl, hParent, nId, backcolor, cFile )
   LOCAL cVarName := "_" + ParentFormName + "_" + ControlName
   hb_default( @nId, 0 )

   // Create the global reference variable
#ifdef _NAMES_LIST_
   _SetNameList( cVarName, nControl )
#else
   PUBLIC &cVarName. := nControl
#endif

   // Initialize the internal state arrays
   _InitCommonControl( nControl, ControlName, cType, x, y, w, h, HelpId, invisible )

   // Store Win32 specific handles
   _HMG_aControlHandles[ nControl ]      := hControl
   _HMG_aControlParentHandles[ nControl ]:= hParent
   _HMG_aControlIds[ nControl ]          := nId
   _HMG_aControlBkColor[ nControl ]      := backcolor
   _HMG_aControlCaption[ nControl ]      := cFile

   // Support for Object-Oriented HMG syntax if enabled
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, nControl, cVarName )
   ENDIF
RETURN NIL

/*
 * STATIC FUNCTION _ResolveControlParent
 * Determines the correct parent window and adjusts coordinates based on the current context.
 *
 * Reasoning:
 *    HMG allows defining controls within 'DEFINE WINDOW' or 'DEFINE FRAME' blocks.
 *    This function automatically detects if a control is being placed inside a
 *    Frame or a Dialog and adjusts the 'x' and 'y' offsets accordingly so the
 *    programmer can use relative coordinates.
 */
STATIC FUNCTION _ResolveControlParent( ParentFormName, x, y, lDialog )
   hb_default( @lDialog, .F. )

   // If we are inside a DEFINE WINDOW/DIALOG block, use the active container name
   IF _HMG_BeginWindowActive .OR. ( lDialog .AND. _HMG_BeginDialogActive )
      ParentFormName := iif( lDialog .AND. _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
   ENDIF

   // If inside a Frame, offset the coordinates by the Frame's position
   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive
      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF
RETURN NIL

/*
 * STATIC FUNCTION _ValidateControlDefinition
 * Performs safety checks to ensure the parent exists and the control name is unique.
 *
 * Parameters:
 *    lSkipValidation : If .T., bypasses existence checks (used for dynamic templates).
 *
 * Reasoning:
 *    Prevents runtime crashes by catching "Parent not defined" or "Duplicate control"
 *    errors during the definition phase.
 */
STATIC FUNCTION _ValidateControlDefinition( ControlName, ParentFormName, lSkipValidation )
   hb_default( @lSkipValidation, .F. )

   IF ! lSkipValidation
      IF ! _IsWindowDefined( ParentFormName )
         MsgMiniGuiError( "Window: " + IFNIL( ParentFormName, "Parent", ParentFormName ) + " is not defined." )
      ENDIF
   ENDIF

   // Support for auto-generated unique names if "0" is passed
   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF ! lSkipValidation
      IF _IsControlDefined( ControlName, ParentFormName )
         MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + " Already defined." )
      ENDIF
   ENDIF
RETURN NIL

/*
 * FUNCTION _DefinePlayer
 * Creates a Multimedia Player control (MCI based).
 *
 * Parameters:
 *    noasw...shp : Various style flags for the player UI (NoAutoSize, NoMenu, etc.).
 */
FUNCTION _DefinePlayer( ControlName, ParentFormName, cFile, x, y, w, h, ;
      noasw, noasm, noed, nom, noo, nop, sha, shm, shn, shp, HelpId )
   LOCAL hControl, hParent, nControl

   // Context resolution and error checking
   _ResolveControlParent( @ParentFormName, @x, @y )
   _ValidateControlDefinition( @ControlName, ParentFormName )

   hParent  := GetFormHandle( ParentFormName )
   
   // Call the C-level initialization for the MCI player
   hControl := InitPlayer( hParent, cFile, x, y, w, h, noasw, noasm, noed, nom, noo, nop, sha, shm, shn, shp )

   // If inside a Tab, register this control handle to the specific Tab Page
   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
   ENDIF

   // Register in HMG global system
   nControl := _GetControlFree()
   _RegisterControl( nControl, ControlName, ParentFormName, "PLAYER", x, y, w, h, HelpId, .F., hControl, hParent )
RETURN NIL

/*
 * FUNCTION _DefineAnimateBox
 * Creates an Animation control (SysAnimate32) for playing AVI files.
 *
 * Reasoning:
 *    This function handles two distinct scenarios:
 *    1. Standard Window: Creates the control immediately using InitAnimate.
 *    2. Dialog Template: Adds the control definition to a list to be created
 *       later by the Windows Dialog Manager.
 */
FUNCTION _DefineAnimateBox( ControlName, ParentFormName, x, y, w, h, ;
      autoplay, center, transparent, cFile, HelpId, border, backcolor, invisible, nId )
   LOCAL hParent, hControl, nControl, nStyle, blInit, lDialogInMemory

   hb_default( @invisible, .F. )
   lDialogInMemory := _HMG_DialogInMemory

   _ResolveControlParent( @ParentFormName, @x, @y, .T. )
   _ValidateControlDefinition( @ControlName, ParentFormName, lDialogInMemory )

   nControl := _GetControlFree()

   // Logic for Dialog-based controls
   IF _HMG_BeginDialogActive
      hParent := _HMG_ActiveDialogHandle
      nStyle  := WS_CHILD + WS_TABSTOP
      IF border
         nStyle += WS_BORDER
      ENDIF
      IF !invisible
         nStyle += WS_VISIBLE
      ENDIF

      IF lDialogInMemory
         // Store definition for delayed creation (Memory Dialogs)
         blInit := {|x,y,z| InitDialogAnimateBox( x, y, z ) }
         AAdd( _HMG_aDialogItems, { nId, nControl, "SysAnimate32", nStyle, 0, x, y, w, h, "", HelpId, "", "", 0, , , , , blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )
         hControl := NIL
      ELSE
         // Control already exists in resource, just get the handle and update styles
         hControl := GetDialogItemHandle( hParent, nId )
         SetWindowStyle( hControl, nStyle, .T. )
         x := GetWindowCol( hControl )
         y := GetWindowRow( hControl )
         w := GetWindowWidth( hControl )
         h := GetWindowHeight( hControl )
      ENDIF
   ELSE
      // Standard Window creation
      hParent  := GetFormHandle( ParentFormName )
      hControl := InitAnimate( hParent, x, y, w, h, autoplay, center, transparent, border, invisible )
   ENDIF

   // Tab page association
   IF !lDialogInMemory .AND. _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, hControl )
   ENDIF

   _RegisterControl( nControl, ControlName, ParentFormName, "ANIMATEBOX", x, y, w, h, HelpId, invisible, hControl, hParent, nId, backcolor, cFile )

   // If a file was specified, open it immediately (unless it's a memory dialog)
   IF !lDialogInMemory .AND. ValType( cFile ) <> "U"
      _OpenAnimateBox( ControlName, ParentFormName, cFile )
   ENDIF
RETURN NIL

/*
 * FUNCTION InitDialogAnimateBox
 * Callback used during Dialog initialization to open the AVI file for an AnimateBox.
 */
FUNCTION InitDialogAnimateBox( ParentName, hControl, nControl )
   LOCAL cFile := _HMG_aControlCaption[ nControl ]
   
   // Open the AVI file if one was defined in the caption property
   IF ValType( cFile ) <> "U" .AND. ValType( hControl ) <> "U"
      _OpenAnimateBox( _HMG_aControlNames[ nControl ], ParentName, cFile )
   ENDIF
   
   // Mark as deleted if the dialog template is being cleared
   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[ 3 ]
      _HMG_aControlDeleted[ nControl ] := .T.
   ENDIF
RETURN NIL

/*
 * FUNCTION PlayWave
 * Wrapper for the C-level wave player.
 *
 * Parameters:
 *    wave  : Filename or Resource name.
 *    lLoop : If .T., the sound repeats indefinitely.
 */
FUNCTION PlayWave( wave, r, s, ns, lLoop, nd )
   hb_default( @r, .F. )
   hb_default( @s, .F. )
   hb_default( @ns, .F. )
   hb_default( @lLoop, .F. )
   hb_default( @nd, .F. )
RETURN C_PlayWave( wave, r, s, ns, lLoop, nd )

/*
 * FUNCTION GetAviFileSize
 * Extracts the width and height from an AVI file header.
 *
 * Reasoning:
 *    AVI files (RIFF format) store stream dimensions at specific offsets.
 *    This allows HMG to auto-size the AnimateBox to match the video source.
 *
 * Returns:
 *    An array { width, height }.
 */
FUNCTION GetAviFileSize( cFile )
   LOCAL cStr1 := Space( 4 ), cStr2 := Space( 4 )
   LOCAL nWidth := 0, nHeight := 0
   LOCAL nFileHandle := FOpen( cFile )
   
   IF FError() != 0
      RETURN { 0, 0 }
   ENDIF
   
   FRead( nFileHandle, @cStr1, 4 )
   
   // Check for valid RIFF header
   IF cStr1 == "RIFF"
      // Offset 64 is typically where the Main AVI Header starts (AVIMAINHEADER)
      // containing dwWidth and dwHeight.
      FSeek( nFileHandle, 64, 0 )
      FRead( nFileHandle, @cStr1, 4 )
      FRead( nFileHandle, @cStr2, 4 )
      nWidth := Bin2L( cStr1 )
      nHeight := Bin2L( cStr2 )
   ENDIF
   
   FClose( nFileHandle )
RETURN { nWidth, nHeight }

/*
 * FUNCTION GetAviResSize
 * Determines dimensions of an AVI stored as a resource.
 *
 * Reasoning:
 *    Since we cannot directly parse a resource as a file stream using FOpen,
 *    we extract the resource to a temporary disk file, read its header,
 *    and then delete the temp file.
 */
FUNCTION GetAviResSize( cResName )
   LOCAL aAviSize := { 0, 0 }
   LOCAL cDiskFile := TempFile( GetTempFolder(), "avi" )
   
   // Extract resource to temp file
   IF RCDataToFile( cResName, cDiskFile, "AVI" ) > 0 .AND. hb_FileExists( cDiskFile )
      aAviSize := GetAviFileSize( cDiskFile )
      FErase( cDiskFile )
   ENDIF
RETURN aAviSize