/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

ANIMATERES Control Source Code
Copyright 2011 Grigory Filatov <gfilatov@gmail.com>

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

#ifdef _USERINIT_

/*
 * PROCEDURE: _InitAnimateRes
 * Purpose: Automatically initializes the ANIMATERES control system during application startup.
 * Logic: Registers the custom methods and properties into the HMG Extended internal handler system.
 */
INIT PROCEDURE _InitAnimateRes
   // Register the cleanup method to ensure DLL resources are freed when the control is destroyed.
   InstallMethodHandler( 'Release', 'ReleaseAnimateRes' )
   
   // Map the 'File' property to its respective getter and setter functions.
   InstallPropertyHandler( 'File', 'SetAnimateResFile', 'GetAnimateResFile' )
   
   // Map the 'ResId' property to its respective getter and setter functions.
   InstallPropertyHandler( 'ResId', 'SetAnimateResId', 'GetAnimateResId' )
RETURN

/*
 * FUNCTION: _DefineAnimateRes
 * Purpose: Constructor for the ANIMATERES control, which displays AVI animations from resource DLLs.
 * Parameters:
 *    - ControlName: String identifier for the control.
 *    - ParentForm: Name of the window containing the control.
 *    - x, y, w, h: Coordinates and dimensions.
 *    - cFile: Path to the DLL file containing the AVI resource.
 *    - nRes: Numeric ID of the AVI resource within the DLL.
 *    - tooltip: Text to display on hover.
 *    - HelpId: Numeric ID for context-sensitive help.
 *    - invisible: Logical flag to hide the control initially.
 */
FUNCTION _DefineAnimateRes( ControlName, ParentForm, x, y, w, h, cFile, nRes, ;
      tooltip, HelpId, invisible )
   LOCAL ControlHandle, hAvi, cParentForm, mVar

   // Set default dimensions and visibility if not provided.
   hb_default( @w, 200 )
   hb_default( @h, 50 )
   hb_default( @invisible, .F. )

   // Determine the active parent form if using the 'DEFINE WINDOW' command block.
   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF
   
   // Adjust coordinates if the control is placed inside a Frame container.
   IF _HMG_FrameLevel > 0
      x += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   // Validation: Ensure the parent exists and the control name is unique.
   IF .NOT. _IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF
   IF ISCHAR( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF
   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   // Create the internal variable name used for the control index.
   mVar := '_' + ParentForm + '_' + ControlName
#ifdef _NAMES_LIST_
   _SetNameList( mVar, Len( _HMG_aControlNames ) + 1 )
#else
   Public &mVar. := Len( _HMG_aControlNames ) + 1
#endif

   cParentForm := ParentForm
   ParentForm := GetFormHandle( ParentForm )

   // Call the C-level function to create the Win32 control and load the DLL.
   // hAvi is passed by reference to receive the DLL instance handle.
   ControlHandle := InitAnimateRes( ParentForm, @hAvi, x, y, w, h, cFile, nRes, invisible )

   // Handle Tab control integration if the control is defined inside a Tab page.
   IF _HMG_BeginTabActive
      AAdd( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF
   
   // Apply tooltip if specified.
   IF tooltip != NIL
      SetToolTip( ControlHandle, tooltip, GetFormToolTipHandle( cParentForm ) )
   ENDIF

   // Synchronize HMG internal state arrays with the new control's properties.
   AAdd( _HMG_aControlType, "ANIMATERES" )
   AAdd( _HMG_aControlNames, ControlName )
   AAdd( _HMG_aControlHandles, ControlHandle )
   AAdd( _HMG_aControlParentHandles, ParentForm )
   AAdd( _HMG_aControlIds, nRes )
   AAdd( _HMG_aControlProcedures, "" )
   AAdd( _HMG_aControlPageMap, {} )
   AAdd( _HMG_aControlValue, cFile )
   AAdd( _HMG_aControlInputMask, "" )
   AAdd( _HMG_aControllostFocusProcedure, "" )
   AAdd( _HMG_aControlGotFocusProcedure, "" )
   AAdd( _HMG_aControlChangeProcedure, "" )
   AAdd( _HMG_aControlDeleted, .F. )
   AAdd( _HMG_aControlBkColor, Nil )
   AAdd( _HMG_aControlFontColor, Nil )
   AAdd( _HMG_aControlDblClick, "" )
   AAdd( _HMG_aControlHeadClick, {} )
   AAdd( _HMG_aControlRow, y )
   AAdd( _HMG_aControlCol, x )
   AAdd( _HMG_aControlWidth, w )
   AAdd( _HMG_aControlHeight, h )
   AAdd( _HMG_aControlSpacing, 0 )
   AAdd( _HMG_aControlContainerRow, iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 ) )
   AAdd( _HMG_aControlContainerCol, iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 ) )
   AAdd( _HMG_aControlPicture, "" )
   AAdd( _HMG_aControlContainerHandle, 0 )
   AAdd( _HMG_aControlFontName, '' )
   AAdd( _HMG_aControlFontSize, 0 )
   AAdd( _HMG_aControlFontAttributes, { .F., .F., .F., .F. } )
   AAdd( _HMG_aControlToolTip, tooltip )
   AAdd( _HMG_aControlRangeMin, 0 )
   AAdd( _HMG_aControlRangeMax, 0 )
   AAdd( _HMG_aControlCaption, '' )
   AAdd( _HMG_aControlVisible, iif( invisible, .F., .T. ) )
   AAdd( _HMG_aControlHelpId, HelpId )
   AAdd( _HMG_aControlFontHandle, 0 )
   AAdd( _HMG_aControlBrushHandle, 0 )
   AAdd( _HMG_aControlEnabled, .T. )
   
   // Store the DLL handle in MiscData1 for later release during control destruction.
   AAdd( _HMG_aControlMiscData1, hAvi )
   AAdd( _HMG_aControlMiscData2, '' )

   // Trigger OOP initialization if the Object-Oriented mode is active.
   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, Len( _HMG_aControlNames ), mVar )
   ENDIF
RETURN NIL

/*
 * FUNCTION: SetAnimateResFile
 * Purpose: Updates the DLL file path associated with the control.
 */
FUNCTION SetAnimateResFile( cWindow, cControl, cProperty, cValue )
   _AnimateResProp( cWindow, cControl, cProperty, cValue, "FILE", _HMG_aControlValue, .T. )
RETURN NIL

/*
 * FUNCTION: GetAnimateResFile
 * Purpose: Retrieves the current DLL file path from internal storage.
 */
FUNCTION GetAnimateResFile( cWindow, cControl )
RETURN _AnimateResProp( cWindow, cControl, "", NIL, "", _HMG_aControlValue, .F. )

/*
 * FUNCTION: SetAnimateResId
 * Purpose: Updates the Resource ID for the AVI within the DLL.
 */
FUNCTION SetAnimateResId( cWindow, cControl, cProperty, cValue )
   _AnimateResProp( cWindow, cControl, cProperty, cValue, "RESID", _HMG_aControlIds, .T. )
RETURN NIL

/*
 * FUNCTION: GetAnimateResId
 * Purpose: Retrieves the current Resource ID from internal storage.
 */
FUNCTION GetAnimateResId( cWindow, cControl )
RETURN _AnimateResProp( cWindow, cControl, "", NIL, "", _HMG_aControlIds, .F. )

/*
 * STATIC FUNCTION: _AnimateResProp
 * Purpose: Centralized helper for getting/setting ANIMATERES properties.
 * Reasoning: Reduces code duplication by abstracting the array lookup and validation logic.
 * Parameters:
 *    - lSet: Logical flag (.T. for Set, .F. for Get).
 *    - aStorage: Reference to the specific HMG internal array being accessed.
 */
STATIC FUNCTION _AnimateResProp( cWindow, cControl, cProperty, ;
      xValue, cExpectedProp, aStorage, lSet )

   LOCAL nIndex
   LOCAL xRetVal := NIL

   // Ensure we are operating on the correct control type.
   IF GetControlType( cControl, cWindow ) == "ANIMATERES"

      nIndex := GetControlIndex( cControl, cWindow )

      IF lSet
         // Verify the property name matches before updating internal storage.
         IF Upper( cProperty ) == cExpectedProp
            _HMG_UserComponentProcess := .T.
            aStorage[ nIndex ] := xValue
         ELSE
            _HMG_UserComponentProcess := .F.
         ENDIF
      ELSE
         // Retrieve the value from the specified storage array.
         _HMG_UserComponentProcess := .T.
         xRetVal := aStorage[ nIndex ]
      ENDIF
   ELSE
      _HMG_UserComponentProcess := .F.
   ENDIF

RETURN xRetVal

/*
 * PROCEDURE: ReleaseAnimateRes
 * Purpose: Cleans up resources when the control is destroyed.
 * Side Effects: Unloads the DLL from memory to prevent resource leaks.
 */
PROCEDURE ReleaseAnimateRes( cWindow, cControl )
   IF _IsControlDefined( cControl, cWindow ) .AND. GetControlType( cControl, cWindow ) == 'ANIMATERES'
      // UnloadAnimateLib takes the DLL handle stored in MiscData1.
      UnloadAnimateLib( _GetControlObject( cControl, cWindow ) )
      _HMG_UserComponentProcess := .T.
   ELSE
      _HMG_UserComponentProcess := .F.
   ENDIF
RETURN

#pragma BEGINDUMP

#include <mgdefs.h>
#include <mmsystem.h>
#include <commctrl.h>

#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
#endif

/*
 * HB_FUNC: INITANIMATERES
 * Purpose: C-level interface to create the Win32 Animation control.
 * Logic:
 *    1. Loads the specified DLL into the process address space.
 *    2. Creates a window of class 'SysAnimate32'.
 *    3. Uses Animate_OpenEx to bind the AVI resource from the DLL to the control.
 * Returns: HWND of the created control.
 */
HB_FUNC( INITANIMATERES )
{
   HWND      hAnimation;
   HINSTANCE hAvi;

#ifndef UNICODE
   LPCSTR lpszDllName = hb_parc( 7 );
#else
   LPWSTR lpszDllName = AnsiToWide( ( char * ) hb_parc( 7 ) );
#endif

   /*
    * Define Win32 Animation Control Styles:
    * ACS_TRANSPARENT: Uses the parent's background color.
    * ACS_CENTER: Centers the animation within the control area.
    * ACS_AUTOPLAY: Starts playback immediately upon opening.
    */
   DWORD Style =
      WS_CHILD |
      ACS_TRANSPARENT |
      ACS_CENTER |
      ACS_AUTOPLAY;

   /*
    * Initialize the Common Controls library for the Animate class.
    */
   INITCOMMONCONTROLSEX icc;

   icc.dwSize = sizeof( INITCOMMONCONTROLSEX );
   icc.dwICC  = ICC_ANIMATE_CLASS;

   InitCommonControlsEx( &icc );

   /*
    * Apply visibility based on the 'invisible' parameter.
    */
   if( ! hb_parl( 9 ) )
      Style |= WS_VISIBLE;

   /*
    * Load the external library containing the AVI resource.
    */
   hAvi = LoadLibrary( lpszDllName );

   /*
    * Create the actual Win32 control.
    */
   hAnimation = CreateWindowEx(
      0,
      ANIMATE_CLASS,
      NULL,
      Style,
      hb_parni( 3 ), // x
      hb_parni( 4 ), // y
      hb_parni( 5 ), // width
      hb_parni( 6 ), // height
      hmg_par_raw_HWND( 1 ),  // Parent HWND
      hmg_par_raw_HMENU( 2 ), // Control ID
      hAvi,
      NULL );

   /*
    * Load the AVI resource from the DLL and start the animation.
    * MAKEINTRESOURCE converts the numeric ID to a resource pointer.
    */
   Animate_OpenEx(
      hAnimation,
      hAvi,
      MAKEINTRESOURCE( hb_parni( 8 ) ) );

   /*
    * Store the DLL handle back into the second parameter (passed by reference).
    * This allows the Harbour level to keep track of the handle for cleanup.
    */
   HB_STORNL( ( LONG_PTR ) hAvi, 2 );

   /*
    * Return the control's window handle.
    */
   hmg_ret_raw_HANDLE( hAnimation );
}

/*
 * HB_FUNC: UNLOADANIMATELIB
 * Purpose: Frees the DLL handle loaded during control initialization.
 * Parameter: Handle to the loaded library (HINSTANCE).
 */
HB_FUNC( UNLOADANIMATELIB )
{
   HINSTANCE hLib = hmg_par_raw_HINSTANCE( 1 );

   /*
    * Decrement the reference count of the loaded DLL.
    */
   if( hLib )
      FreeLibrary( hLib );
}

#pragma ENDDUMP

#endif