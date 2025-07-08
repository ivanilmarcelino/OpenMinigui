/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

WEBCAM Control Source Code
Copyright 2012 Grigory Filatov <gfilatov@gmail.com>

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
#include "i_winuser.ch"

/*-----------------------------------------------------------------------------*
INIT PROCEDURE _InitWebCam
*------------------------------------------------------------------------------*
*
*  Description:
*     Initializes the Webcam control by installing method handlers.
*
*  Parameters:
*     None
*
*  Return Value:
*     None
*
*  Purpose:
*     This procedure is called during the initialization of the HMG environment.
*     It registers the 'Start' and 'Release' methods for the Webcam control,
*     associating them with the internal functions '_StartWebCam' and '_ReleaseWebCam' respectively.
*     This allows developers to use the Start() and Release() methods on Webcam objects in their HMG applications.
*
*  Notes:
*     This procedure is automatically called by HMG during startup if _USERINIT_ is defined.
*
*/
INIT PROCEDURE _InitWebCam

   InstallMethodHandler ( 'Start', '_StartWebCam' )
   InstallMethodHandler ( 'Release', '_ReleaseWebCam' )

RETURN

/*-----------------------------------------------------------------------------*
FUNCTION _DefineWebCam ( ControlName, ParentForm, x, y, w, h, lStart, nRate, tooltip, HelpId )
*------------------------------------------------------------------------------*
*
*  Description:
*     Defines a Webcam control within an HMG form.
*
*  Parameters:
*     ControlName - The name of the Webcam control (string).
*     ParentForm  - The name of the parent form (string).
*     x           - The x-coordinate of the control's top-left corner (numeric).
*     y           - The y-coordinate of the control's top-left corner (numeric).
*     w           - The width of the control (numeric, optional, defaults to 320).
*     h           - The height of the control (numeric, optional, defaults to 240).
*     lStart      - A logical value indicating whether to start the webcam immediately after creation (.T.) or not (.F.).
*     nRate       - The preview rate in frames per second (numeric, optional, defaults to 30).
*     tooltip     - The tooltip text for the control (string, optional).
*     HelpId      - The help ID associated with the control (string, optional).
*
*  Return Value:
*     NIL
*
*  Purpose:
*     This function creates a Webcam control and adds it to the specified form.
*     It handles the creation of the underlying Windows capture window using cap_CreateCaptureWindow,
*     registers the control within the HMG environment, and sets various properties such as position, size, and preview rate.
*     It also handles optional features like tooltips and automatic webcam startup.
*     This function is the core of the Webcam control's definition within HMG.
*
*  Notes:
*     - The function checks for existing window and control definitions to prevent naming conflicts.
*     - It adjusts the control's position if it's placed within a frame.
*     - It uses the cap_CreateCaptureWindow function from the VFW (Video for Windows) API.
*
*/
FUNCTION _DefineWebCam ( ControlName, ParentForm, x, y, w, h, lStart, nRate, tooltip, HelpId )
   LOCAL ControlHandle
   LOCAL cParentForm
   LOCAL mVar

   hb_default( @w, 320 )  // Default width if not specified
   hb_default( @h, 240 )  // Default height if not specified
   hb_default( @nRate, 30 ) // Default frame rate if not specified

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF _HMG_FrameLevel > 0
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF ! _IsWindowDefined ( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined ( ControlName, ParentForm )
      MsgMiniGuiError ( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   mVar := '_' + ParentForm + '_' + ControlName

#ifdef _NAMES_LIST_
   _SetNameList( mVar, Len ( _HMG_aControlNames ) + 1 )
#else
   PUBLIC &mVar. := Len ( _HMG_aControlNames ) + 1
#endif

   cParentForm := ParentForm

   ParentForm := GetFormHandle ( ParentForm )

   ControlHandle := cap_CreateCaptureWindow ( "WebCam", hb_bitOr( WS_CHILD, WS_VISIBLE ), x, y, w, h, ParentForm, 0 )

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, Controlhandle )
   ENDIF

   IF tooltip != NIL
      SetToolTip ( ControlHandle, tooltip, GetFormToolTipHandle ( cParentForm ) )
   ENDIF

   AAdd ( _HMG_aControlType, "WEBCAM" )
   AAdd ( _HMG_aControlNames, ControlName )
   AAdd ( _HMG_aControlHandles, ControlHandle )
   AAdd ( _HMG_aControlParentHandles, ParentForm )
   AAdd ( _HMG_aControlIds, 0 )
   AAdd ( _HMG_aControlProcedures, "" )
   AAdd ( _HMG_aControlPageMap, {} )
   AAdd ( _HMG_aControlValue, nRate )
   AAdd ( _HMG_aControlInputMask, "" )
   AAdd ( _HMG_aControllostFocusProcedure, "" )
   AAdd ( _HMG_aControlGotFocusProcedure, "" )
   AAdd ( _HMG_aControlChangeProcedure, "" )
   AAdd ( _HMG_aControlDeleted, .F. )
   AAdd ( _HMG_aControlBkColor, {} )
   AAdd ( _HMG_aControlFontColor, {} )
   AAdd ( _HMG_aControlDblClick, "" )
   AAdd ( _HMG_aControlHeadClick, {} )
   AAdd ( _HMG_aControlRow, y )
   AAdd ( _HMG_aControlCol, x )
   AAdd ( _HMG_aControlWidth, w )
   AAdd ( _HMG_aControlHeight, h )
   AAdd ( _HMG_aControlSpacing, 0 )
   AAdd ( _HMG_aControlContainerRow, iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlContainerCol, iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 ) )
   AAdd ( _HMG_aControlPicture, "" )
   AAdd ( _HMG_aControlContainerHandle, 0 )
   AAdd ( _HMG_aControlFontName, '' )
   AAdd ( _HMG_aControlFontSize, 0 )
   AAdd ( _HMG_aControlFontAttributes, { FALSE, FALSE, FALSE, FALSE } )
   AAdd ( _HMG_aControlToolTip, tooltip )
   AAdd ( _HMG_aControlRangeMin, 0 )
   AAdd ( _HMG_aControlRangeMax, 0 )
   AAdd ( _HMG_aControlCaption, '' )
   AAdd ( _HMG_aControlVisible, .F. )
   AAdd ( _HMG_aControlHelpId, HelpId )
   AAdd ( _HMG_aControlFontHandle, 0 )
   AAdd ( _HMG_aControlBrushHandle, 0 )
   AAdd ( _HMG_aControlEnabled, .T. )
   AAdd ( _HMG_aControlMiscData1, 0 )
   AAdd ( _HMG_aControlMiscData2, '' )

   IF lStart
      IF ! _StartWebCam ( cParentForm, ControlName )
         MsgAlert( "Webcam service is unavailable!", "Alert" )
      ENDIF
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION _StartWebCam ( cWindow, cControl )
*------------------------------------------------------------------------------*
*
*  Description:
*     Starts the webcam capture and preview for a given Webcam control.
*
*  Parameters:
*     cWindow - The name of the window containing the Webcam control (string).
*     cControl - The name of the Webcam control (string).
*
*  Return Value:
*     .T. if the webcam started successfully, .F. otherwise.
*
*  Purpose:
*     This function attempts to connect to the webcam driver, sets the video format,
*     and starts the preview. It retries the connection a few times in case of initial failure.
*     It's called when the user explicitly starts the webcam or when the 'lStart' parameter is set to .T. in _DefineWebCam.
*     The function updates the control's visibility status in the HMG control array.
*
*  Notes:
*     - The function uses the cap_DriverConnect, cap_SetVideoFormat, cap_PreviewScale, cap_PreviewRate, and cap_Preview functions from the VFW API.
*     - If the connection to the webcam driver fails after multiple attempts, the function destroys the capture window.
*     - The video format is set to a maximum of 320x240 to ensure compatibility across different webcams.
*
*/
FUNCTION _StartWebCam ( cWindow, cControl )
   LOCAL hWnd
   LOCAL w
   LOCAL h
   LOCAL nTry := 1
   LOCAL lSuccess

   hWnd := GetControlHandle ( cControl, cWindow )

   REPEAT
      lSuccess := cap_DriverConnect ( hWnd, 0 )
      DO EVENTS
   UNTIL ( lSuccess == .F. .AND. nTry++ < 3 )

   IF lSuccess
      w := _GetControlWidth ( cControl, cWindow )
      h := _GetControlHeight ( cControl, cWindow )

      cap_SetVideoFormat ( hWnd, Min( w, 320 ), Min( h, 240 ) )

      lSuccess := ( cap_PreviewScale( hWnd, .T. ) .AND. ;
         cap_PreviewRate( hWnd, GetControlValue ( cControl, cWindow ) ) .AND. ;
         cap_Preview( hWnd, .T. ) )
   ELSE
      // error connecting to video source
      DestroyWindow ( hWnd )
   ENDIF

   _HMG_aControlVisible[ GetControlIndex ( cControl, cWindow ) ] := lSuccess

RETURN lSuccess

/*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseWebCam ( cWindow, cControl )
*------------------------------------------------------------------------------*
*
*  Description:
*     Releases the webcam resources and destroys the capture window for a given Webcam control.
*
*  Parameters:
*     cWindow - The name of the window containing the Webcam control (string).
*     cControl - The name of the Webcam control (string).
*
*  Return Value:
*     None
*
*  Purpose:
*     This procedure disconnects from the webcam driver, destroys the capture window,
*     and removes the control from the HMG control array. It's called when the user explicitly releases the webcam
*     or when the window containing the webcam is closed. This ensures that resources are properly released to prevent memory leaks and other issues.
*
*  Notes:
*     - The function uses the cap_DriverDisconnect and DestroyWindow functions from the VFW API and Windows API respectively.
*     - It checks if the control is defined and is of type 'WEBCAM' before attempting to release it.
*     - The _EraseControl function removes the control from the HMG control arrays.
*
*/
PROCEDURE _ReleaseWebCam ( cWindow, cControl )
   LOCAL hWnd

   IF _IsControlDefined ( cControl, cWindow ) .AND. GetControlType ( cControl, cWindow ) == 'WEBCAM'

      hWnd := GetControlHandle ( cControl, cWindow )

      IF ! Empty ( hWnd )

         cap_DriverDisconnect ( hWnd )

         DestroyWindow ( hWnd )

         _EraseControl ( GetControlIndex ( cControl, cWindow ), GetFormIndex ( cWindow ) )

      ENDIF

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

/*
 * C-level
 */
#pragma BEGINDUMP

#include <mgdefs.h>
#include <vfw.h>

#if defined( __BORLANDC__ )
#pragma warn -use /* unused var */
#pragma warn -eff /* no effect */
#endif

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_CREATECAPTUREWINDOW )
*------------------------------------------------------------------------------*
*
*  Description:
*     Creates a capture window using the capCreateCaptureWindow function from the VFW API.
*
*  Parameters:
*     1 - lpszWindowName: The name of the capture window (string).
*     2 - dwStyle: The style of the capture window (DWORD).
*     3 - x: The x-coordinate of the window's top-left corner (numeric).
*     4 - y: The y-coordinate of the window's top-left corner (numeric).
*     5 - nWidth: The width of the window (numeric).
*     6 - nHeight: The height of the window (numeric).
*     7 - hWndParent: The handle of the parent window (HWND).
*     8 - nID: The ID of the window (numeric).
*
*  Return Value:
*     The handle of the created capture window (HWND).
*
*  Purpose:
*     This function is a Harbour wrapper for the capCreateCaptureWindow function from the VFW API.
*     It allows Harbour code to create a capture window, which is necessary for capturing video from a webcam.
*     The function takes various parameters that define the window's properties, such as its name, style, position, size, and parent window.
*
*  Notes:
*     - This function directly calls the capCreateCaptureWindow function from the VFW API.
*     - The hmg_ret_raw_HWND macro is used to return the window handle as a raw HWND value.
*
*/
HB_FUNC( CAP_CREATECAPTUREWINDOW )
{
#ifndef UNICODE
   LPCSTR lpszWindowName = hb_parc( 1 );
#else
   LPWSTR lpszWindowName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif

   hmg_ret_raw_HWND
      (
         capCreateCaptureWindow
            (
         lpszWindowName,
         hmg_par_DWORD( 2 ),
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 6 ),
         hmg_par_raw_HWND( 7 ),
         hb_parni( 8 )
            )
      );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_DRIVERCONNECT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Connects to a capture driver using the capDriverConnect function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - i: The index of the capture driver to connect to (numeric).
*
*  Return Value:
*     .T. if the connection was successful, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capDriverConnect function from the VFW API.
*     It allows Harbour code to connect to a specific capture driver, which is necessary for accessing the webcam.
*     The function takes the handle of the capture window and the index of the driver to connect to.
*
*  Notes:
*     - This function directly calls the capDriverConnect function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_DRIVERCONNECT )
{
   hb_retl( capDriverConnect( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_DRIVERDISCONNECT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Disconnects from a capture driver using the capDriverDisconnect function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*
*  Return Value:
*     .T. if the disconnection was successful, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capDriverDisconnect function from the VFW API.
*     It allows Harbour code to disconnect from a capture driver, releasing the webcam resources.
*     The function takes the handle of the capture window.
*
*  Notes:
*     - This function directly calls the capDriverDisconnect function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_DRIVERDISCONNECT )
{
   hb_retl( capDriverDisconnect( hmg_par_raw_HWND( 1 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_SETVIDEOFORMAT )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets the video format for the capture window using the capSetVideoFormat function from the VFW API.
*
*  Parameters:
*     1 - hCapWnd: The handle of the capture window (HWND).
*     2 - nWidth: The desired width of the video (numeric).
*     3 - nHeight: The desired height of the video (numeric).
*
*  Return Value:
*     .T. if the video format was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capSetVideoFormat function from the VFW API.
*     It allows Harbour code to set the video format for the capture window, specifying the desired width and height of the video.
*     The function retrieves the current video format, modifies the width and height, and then sets the new format.
*
*  Notes:
*     - This function directly calls the capSetVideoFormat function from the VFW API.
*     - The function initializes a BITMAPINFO structure with the desired width and height.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_SETVIDEOFORMAT )
{
   BITMAPINFO binf;
   HWND hCapWnd = hmg_par_raw_HWND( 1 );

   capGetVideoFormat( hCapWnd, &binf, sizeof( BITMAPINFO ) );

   binf.bmiHeader.biWidth        = hb_parni( 2 );
   binf.bmiHeader.biHeight       = hb_parni( 3 );
   binf.bmiHeader.biPlanes       = 1;
   binf.bmiHeader.biBitCount     = 24;
   binf.bmiHeader.biCompression  = BI_RGB;
   binf.bmiHeader.biSizeImage    = 0;
   binf.bmiHeader.biClrUsed      = 0;
   binf.bmiHeader.biClrImportant = 0;

   hb_retl( capSetVideoFormat( hCapWnd, &binf, sizeof( BITMAPINFO ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEWRATE )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets the preview frame rate for the capture window using the capPreviewRate function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - wMS: The desired frame rate in milliseconds per frame (WORD).
*
*  Return Value:
*     .T. if the preview rate was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreviewRate function from the VFW API.
*     It allows Harbour code to set the preview frame rate for the capture window, controlling how often the preview image is updated.
*     The function takes the handle of the capture window and the desired frame rate in milliseconds per frame.
*
*  Notes:
*     - This function directly calls the capPreviewRate function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEWRATE )
{
   hb_retl( capPreviewRate( hmg_par_raw_HWND( 1 ), hmg_par_WORD( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEWSCALE )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enables or disables preview scaling for the capture window using the capPreviewScale function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - fScale: .T. to enable preview scaling, .F. to disable it (logical).
*
*  Return Value:
*     .T. if the preview scaling was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreviewScale function from the VFW API.
*     It allows Harbour code to enable or disable preview scaling for the capture window.
*     When preview scaling is enabled, the preview image is scaled to fit the window.
*     The function takes the handle of the capture window and a logical value indicating whether to enable or disable scaling.
*
*  Notes:
*     - This function directly calls the capPreviewScale function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEWSCALE )
{
   hb_retl( capPreviewScale( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_PREVIEW )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enables or disables preview mode for the capture window using the capPreview function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*     2 - fPreview: .T. to enable preview mode, .F. to disable it (logical).
*
*  Return Value:
*     .T. if the preview mode was set successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capPreview function from the VFW API.
*     It allows Harbour code to enable or disable preview mode for the capture window.
*     When preview mode is enabled, the capture window displays a live preview of the video being captured.
*     The function takes the handle of the capture window and a logical value indicating whether to enable or disable preview mode.
*
*  Notes:
*     - This function directly calls the capPreview function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_PREVIEW )
{
   hb_retl( capPreview( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) ) );
}

/*-----------------------------------------------------------------------------*
HB_FUNC( CAP_EDITCOPY )
*------------------------------------------------------------------------------*
*
*  Description:
*     Copies the current frame from the capture window to the clipboard using the capEditCopy function from the VFW API.
*
*  Parameters:
*     1 - hWnd: The handle of the capture window (HWND).
*
*  Return Value:
*     .T. if the frame was copied successfully, .F. otherwise (logical).
*
*  Purpose:
*     This function is a Harbour wrapper for the capEditCopy function from the VFW API.
*     It allows Harbour code to copy the current frame from the capture window to the clipboard,
*     allowing the user to paste the frame into other applications.
*     The function takes the handle of the capture window.
*
*  Notes:
*     - This function directly calls the capEditCopy function from the VFW API.
*     - The hb_retl macro is used to return the logical result of the function.
*
*/
HB_FUNC( CAP_EDITCOPY )
{
   hb_retl( capEditCopy( hmg_par_raw_HWND( 1 ) ) );
}

#pragma ENDDUMP

#endif
