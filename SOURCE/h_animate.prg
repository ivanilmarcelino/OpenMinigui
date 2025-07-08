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
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "minigui.ch"

#ifdef _USERINIT_
*------------------------------------------------------------------------------*
INIT PROCEDURE _InitAnimateRes
*------------------------------------------------------------------------------*

   InstallMethodHandler ( 'Release', 'ReleaseAnimateRes' )
   InstallPropertyHandler ( 'File', 'SetAnimateResFile', 'GetAnimateResFile' )
   InstallPropertyHandler ( 'ResId', 'SetAnimateResId', 'GetAnimateResId' )

RETURN

*------------------------------------------------------------------------------*
FUNCTION _DefineAnimateRes ( ControlName, ParentForm, x, y, w, h, cFile, nRes, ;
      tooltip, HelpId, invisible )
*------------------------------------------------------------------------------*
   LOCAL ControlHandle
   LOCAL hAvi
   LOCAL cParentForm
   LOCAL mVar

   hb_default( @w, 200 )
   hb_default( @h, 50 )
   hb_default( @invisible, .F. )

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x  := x + _HMG_ActiveFrameCol[_HMG_FrameLevel ]
      y  := y + _HMG_ActiveFrameRow[_HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[_HMG_FrameLevel ]
   ENDIF

   IF .NOT. _IsWindowDefined ( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF ISCHAR ( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined ( ControlName, ParentForm )
      MsgMiniGuiError ( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   mVar := '_' + ParentForm + '_' + ControlName
#ifdef _NAMES_LIST_
   _SetNameList( mVar , Len ( _HMG_aControlNames ) + 1 )
#else
   Public &mVar. := Len ( _HMG_aControlNames ) + 1
#endif

   cParentForm := ParentForm

   ParentForm := GetFormHandle ( ParentForm )

   ControlHandle := InitAnimateRes ( ParentForm, @hAvi, x, y, w, h, cFile, nRes, invisible )

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, Controlhandle )
   ENDIF

   IF tooltip != NIL
      SetToolTip ( ControlHandle, tooltip, GetFormToolTipHandle ( cParentForm ) )
   ENDIF

   AAdd ( _HMG_aControlType, "ANIMATERES" )
   AAdd ( _HMG_aControlNames, ControlName )
   AAdd ( _HMG_aControlHandles, ControlHandle )
   AAdd ( _HMG_aControlParentHandles, ParentForm )
   AAdd ( _HMG_aControlIds, nRes )
   AAdd ( _HMG_aControlProcedures, "" )
   AAdd ( _HMG_aControlPageMap, {} )
   AAdd ( _HMG_aControlValue, cFile )
   AAdd ( _HMG_aControlInputMask, "" )
   AAdd ( _HMG_aControllostFocusProcedure, "" )
   AAdd ( _HMG_aControlGotFocusProcedure, "" )
   AAdd ( _HMG_aControlChangeProcedure, "" )
   AAdd ( _HMG_aControlDeleted, .F. )
   AAdd ( _HMG_aControlBkColor, Nil )
   AAdd ( _HMG_aControlFontColor, Nil )
   AAdd ( _HMG_aControlDblClick, "" )
   AAdd ( _HMG_aControlHeadClick, {} )
   AAdd ( _HMG_aControlRow, y )
   AAdd ( _HMG_aControlCol, x )
   AAdd ( _HMG_aControlWidth, w )
   AAdd ( _HMG_aControlHeight, h )
   AAdd ( _HMG_aControlSpacing, 0 )
   AAdd ( _HMG_aControlContainerRow, iif ( _HMG_FrameLevel > 0,_HMG_ActiveFrameRow[_HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlContainerCol, iif ( _HMG_FrameLevel > 0,_HMG_ActiveFrameCol[_HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlPicture, "" )
   AAdd ( _HMG_aControlContainerHandle, 0 )
   AAdd ( _HMG_aControlFontName, '' )
   AAdd ( _HMG_aControlFontSize, 0 )
   AAdd ( _HMG_aControlFontAttributes, { FALSE, FALSE, FALSE, FALSE } )
   AAdd ( _HMG_aControlToolTip, tooltip  )
   AAdd ( _HMG_aControlRangeMin, 0  )
   AAdd ( _HMG_aControlRangeMax, 0  )
   AAdd ( _HMG_aControlCaption, ''  )
   AAdd ( _HMG_aControlVisible, iif( invisible, FALSE, TRUE ) )
   AAdd ( _HMG_aControlHelpId, HelpId )
   AAdd ( _HMG_aControlFontHandle, 0 )
   AAdd ( _HMG_aControlBrushHandle, 0 )
   AAdd ( _HMG_aControlEnabled, .T. )
   AAdd ( _HMG_aControlMiscData1, hAvi )
   AAdd ( _HMG_aControlMiscData2, '' )

   IF _HMG_lOOPEnabled
      Eval ( _HMG_bOnControlInit, Len( _HMG_aControlNames ), mVar )
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION SetAnimateResFile ( cWindow, cControl, cProperty, cValue )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'ANIMATERES' .AND. Upper ( cProperty ) == 'FILE'

      _HMG_UserComponentProcess := .T.

      _HMG_aControlValue[ GetControlIndex ( cControl, cWindow ) ] :=  cValue

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetAnimateResFile ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal := Nil

   IF GetControlType ( cControl, cWindow ) == 'ANIMATERES'

      _HMG_UserComponentProcess := .T.

      RetVal := _HMG_aControlValue[ GetControlIndex ( cControl, cWindow ) ]

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
FUNCTION SetAnimateResId ( cWindow, cControl, cProperty, cValue )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'ANIMATERES' .AND. Upper ( cProperty ) == 'RESID'

      _HMG_UserComponentProcess := .T.

      _HMG_aControlIds[ GetControlIndex ( cControl, cWindow ) ] :=  cValue

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetAnimateResId ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal := Nil

   IF GetControlType ( cControl, cWindow ) == 'ANIMATERES'

      _HMG_UserComponentProcess := .T.

      RetVal := _HMG_aControlIds[ GetControlIndex ( cControl, cWindow ) ]

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
PROCEDURE ReleaseAnimateRes ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF _IsControlDefined ( cControl, cWindow ) .AND. GetControlType ( cControl, cWindow ) == 'ANIMATERES'

      UnloadAnimateLib( _GetControlObject ( cControl, cWindow ) )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
* Low Level C Routines
*------------------------------------------------------------------------------*

#pragma BEGINDUMP

#include <mgdefs.h>
#include <mmsystem.h>
#include <commctrl.h>

// If compiled in UNICODE mode, declare a function to convert ANSI strings to wide-character strings
#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
#endif

// Harbour function to initialize and display an animated control (e.g., a loading animation)
HB_FUNC( INITANIMATERES )
{
   HWND AnimationCtrl;        // Handle for the animation control
   HINSTANCE avi;             // Handle to the loaded library for the AVI resource
#ifndef UNICODE
   LPCSTR lpszDllName = hb_parc( 7 ); // Get the DLL name (resource file) from the 7th Harbour function parameter
#else
   LPWSTR lpszDllName = AnsiToWide( ( char * ) hb_parc( 7 ) ); // Convert to wide-character if in UNICODE mode
#endif

   // Define window style for the animation control
   DWORD Style = WS_CHILD | WS_VISIBLE | ACS_TRANSPARENT | ACS_CENTER | ACS_AUTOPLAY;

   // Initialize the common controls for the animation class
   INITCOMMONCONTROLSEX i;
   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC  = ICC_ANIMATE_CLASS; // Specify that we are using the animation control class
   InitCommonControlsEx( &i ); // Load the necessary common controls

   // Check if the 9th parameter indicates the animation should initially be hidden
   if( ! hb_parl( 9 ) )
   {
      Style |= WS_VISIBLE;    // Add visibility to the style if not hidden
   }

   // Load the specified DLL containing the AVI resource
   avi = LoadLibrary( lpszDllName );

   // Create the animation control window with the specified parameters
   AnimationCtrl = CreateWindowEx
                   (
      0,                       // Extended window style
      ANIMATE_CLASS,           // Predefined class name for animation controls
      NULL,                    // No window name
      Style,                   // Style for the animation control
      hb_parni( 3 ),           // Left position
      hb_parni( 4 ),           // Top position
      hb_parni( 5 ),           // Width
      hb_parni( 6 ),           // Height
      hmg_par_raw_HWND( 1 ),   // Parent window handle
      hmg_par_raw_HMENU( 2 ),  // Handle to menu or child window ID
      avi,                     // Instance handle of the loaded DLL
      NULL                     // Additional parameters (not used here)
                   );

   // Open and play the specified AVI resource in the animation control
   Animate_OpenEx( ( HWND ) AnimationCtrl, avi, MAKEINTRESOURCE( hb_parni( 8 ) ) );

   // Store the library handle in the second return parameter
   HB_STORNL( ( LONG_PTR ) avi, 2 );

   // Return the animation control handle to the caller
   hmg_ret_raw_HWND( AnimationCtrl );
}

// Harbour function to unload the library associated with an animation control
HB_FUNC( UNLOADANIMATELIB )
{
   // Retrieve the handle of the library to unload from the function's first parameter
   HINSTANCE hLib = hmg_par_raw_HINSTANCE( 1 );

   // If the library handle is valid, free the loaded library
   if( hLib )
   {
      FreeLibrary( hLib );
   }
}

#pragma ENDDUMP

#endif
