/*
 * Harbour 3.2.0dev (r2503200530)
 * Borland C++ 5.8.2 (32-bit)
 * Generated C source from "h_webcam.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC_INIT( _INITWEBCAM );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC( _DEFINEWEBCAM );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( CAP_CREATECAPTUREWINDOW );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC( _STARTWEBCAM );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( CAP_DRIVERCONNECT );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC_EXTERN( _GETCONTROLWIDTH );
HB_FUNC_EXTERN( _GETCONTROLHEIGHT );
HB_FUNC( CAP_SETVIDEOFORMAT );
HB_FUNC_EXTERN( MIN );
HB_FUNC( CAP_PREVIEWSCALE );
HB_FUNC( CAP_PREVIEWRATE );
HB_FUNC_EXTERN( GETCONTROLVALUE );
HB_FUNC( CAP_PREVIEW );
HB_FUNC_EXTERN( DESTROYWINDOW );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( _RELEASEWEBCAM );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( CAP_DRIVERDISCONNECT );
HB_FUNC_EXTERN( _ERASECONTROL );
HB_FUNC_EXTERN( GETFORMINDEX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WEBCAM )
{ "_INITWEBCAM$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITWEBCAM )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "_DEFINEWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEWEBCAM )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "CAP_CREATECAPTUREWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_CREATECAPTUREWINDOW )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_STARTWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _STARTWEBCAM )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "CAP_DRIVERCONNECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_DRIVERCONNECT )}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "_GETCONTROLWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLWIDTH )}, NULL },
{ "_GETCONTROLHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLHEIGHT )}, NULL },
{ "CAP_SETVIDEOFORMAT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_SETVIDEOFORMAT )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "CAP_PREVIEWSCALE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEWSCALE )}, NULL },
{ "CAP_PREVIEWRATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEWRATE )}, NULL },
{ "GETCONTROLVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLVALUE )}, NULL },
{ "CAP_PREVIEW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_PREVIEW )}, NULL },
{ "DESTROYWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYWINDOW )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_RELEASEWEBCAM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASEWEBCAM )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "CAP_DRIVERDISCONNECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CAP_DRIVERDISCONNECT )}, NULL },
{ "_ERASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ERASECONTROL )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WEBCAM, "h_webcam.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WEBCAM
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WEBCAM )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITWEBCAM )
{
	static const HB_BYTE pcode[] =
	{
		176,1,0,106,6,83,116,97,114,116,0,106,13,95,
		83,116,97,114,116,87,101,98,67,97,109,0,20,2,
		176,1,0,106,8,82,101,108,101,97,115,101,0,106,
		15,95,82,101,108,101,97,115,101,87,101,98,67,97,
		109,0,20,2,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _DEFINEWEBCAM )
{
	static const HB_BYTE pcode[] =
	{
		13,3,10,176,3,0,96,5,0,93,64,1,20,2,
		176,3,0,96,6,0,93,240,0,20,2,176,3,0,
		96,8,0,92,30,20,2,98,4,0,92,34,1,28,
		10,98,4,0,92,33,1,80,2,98,4,0,92,37,
		1,121,15,28,51,96,3,0,98,4,0,92,40,1,
		98,4,0,92,37,1,1,135,96,4,0,98,4,0,
		92,39,1,98,4,0,92,37,1,1,135,98,4,0,
		92,38,1,98,4,0,92,37,1,1,80,2,176,5,
		0,95,2,12,1,31,41,176,6,0,106,9,87,105,
		110,100,111,119,58,32,0,95,2,72,106,17,32,105,
		115,32,110,111,116,32,100,101,102,105,110,101,100,46,
		0,72,20,1,176,7,0,95,1,95,2,12,2,28,
		54,176,6,0,106,10,67,111,110,116,114,111,108,58,
		32,0,95,1,72,106,5,32,79,102,32,0,72,95,
		2,72,106,18,32,65,108,114,101,97,100,121,32,100,
		101,102,105,110,101,100,46,0,72,20,1,106,2,95,
		0,95,2,72,106,2,95,0,72,95,1,72,80,13,
		176,8,0,95,13,176,9,0,98,4,0,93,136,0,
		1,12,1,23,20,2,95,2,80,12,176,10,0,95,
		2,12,1,80,2,176,11,0,106,7,87,101,98,67,
		97,109,0,97,0,0,0,80,95,3,95,4,95,5,
		95,6,95,2,121,12,8,80,11,98,4,0,92,41,
		1,28,15,176,12,0,98,4,0,92,45,1,95,11,
		20,2,95,9,100,69,28,18,176,13,0,95,11,95,
		9,176,14,0,95,12,12,1,20,3,176,12,0,98,
		4,0,93,135,0,1,106,7,87,69,66,67,65,77,
		0,20,2,176,12,0,98,4,0,93,136,0,1,95,
		1,20,2,176,12,0,98,4,0,93,137,0,1,95,
		11,20,2,176,12,0,98,4,0,93,138,0,1,95,
		2,20,2,176,12,0,98,4,0,93,139,0,1,121,
		20,2,176,12,0,98,4,0,93,140,0,1,106,1,
		0,20,2,176,12,0,98,4,0,93,141,0,1,4,
		0,0,20,2,176,12,0,98,4,0,93,142,0,1,
		95,8,20,2,176,12,0,98,4,0,93,143,0,1,
		106,1,0,20,2,176,12,0,98,4,0,93,144,0,
		1,106,1,0,20,2,176,12,0,98,4,0,93,145,
		0,1,106,1,0,20,2,176,12,0,98,4,0,93,
		146,0,1,106,1,0,20,2,176,12,0,98,4,0,
		93,134,0,1,9,20,2,176,12,0,98,4,0,93,
		147,0,1,4,0,0,20,2,176,12,0,98,4,0,
		93,148,0,1,4,0,0,20,2,176,12,0,98,4,
		0,93,149,0,1,106,1,0,20,2,176,12,0,98,
		4,0,93,150,0,1,4,0,0,20,2,176,12,0,
		98,4,0,93,151,0,1,95,4,20,2,176,12,0,
		98,4,0,93,152,0,1,95,3,20,2,176,12,0,
		98,4,0,93,153,0,1,95,5,20,2,176,12,0,
		98,4,0,93,154,0,1,95,6,20,2,176,12,0,
		98,4,0,93,155,0,1,121,20,2,176,12,0,98,
		4,0,93,156,0,1,98,4,0,92,37,1,121,15,
		28,17,98,4,0,92,39,1,98,4,0,92,37,1,
		1,25,4,92,255,20,2,176,12,0,98,4,0,93,
		157,0,1,98,4,0,92,37,1,121,15,28,17,98,
		4,0,92,40,1,98,4,0,92,37,1,1,25,4,
		92,255,20,2,176,12,0,98,4,0,93,158,0,1,
		106,1,0,20,2,176,12,0,98,4,0,93,159,0,
		1,121,20,2,176,12,0,98,4,0,93,160,0,1,
		106,1,0,20,2,176,12,0,98,4,0,93,161,0,
		1,121,20,2,176,12,0,98,4,0,93,168,0,1,
		9,9,9,9,4,4,0,20,2,176,12,0,98,4,
		0,93,162,0,1,95,9,20,2,176,12,0,98,4,
		0,93,163,0,1,121,20,2,176,12,0,98,4,0,
		93,164,0,1,121,20,2,176,12,0,98,4,0,93,
		165,0,1,106,1,0,20,2,176,12,0,98,4,0,
		93,166,0,1,9,20,2,176,12,0,98,4,0,93,
		209,0,1,95,10,20,2,176,12,0,98,4,0,93,
		167,0,1,121,20,2,176,12,0,98,4,0,93,169,
		0,1,121,20,2,176,12,0,98,4,0,93,170,0,
		1,120,20,2,176,12,0,98,4,0,93,171,0,1,
		121,20,2,176,12,0,98,4,0,93,168,1,1,106,
		1,0,20,2,95,7,28,61,176,15,0,95,12,95,
		1,12,2,31,50,176,16,0,106,31,87,101,98,99,
		97,109,32,115,101,114,118,105,99,101,32,105,115,32,
		117,110,97,118,97,105,108,97,98,108,101,33,0,106,
		6,65,108,101,114,116,0,100,9,20,4,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _STARTWEBCAM )
{
	static const HB_BYTE pcode[] =
	{
		13,5,2,122,80,6,176,17,0,95,2,95,1,12,
		2,80,3,176,18,0,95,3,121,12,2,80,7,176,
		19,0,20,0,95,7,31,12,95,6,174,6,0,92,
		3,35,31,229,95,7,28,95,176,20,0,95,2,95,
		1,12,2,80,4,176,21,0,95,2,95,1,12,2,
		80,5,176,22,0,95,3,176,23,0,95,4,93,64,
		1,12,2,176,23,0,95,5,93,240,0,12,2,20,
		3,176,24,0,95,3,120,12,2,21,28,31,73,176,
		25,0,95,3,176,26,0,95,2,95,1,12,2,12,
		2,21,28,11,73,176,27,0,95,3,120,12,2,80,
		7,25,9,176,28,0,95,3,20,1,95,7,98,4,
		0,93,166,0,1,176,29,0,95,2,95,1,12,2,
		2,95,7,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _RELEASEWEBCAM )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,176,7,0,95,2,95,1,12,2,28,88,
		176,31,0,95,2,95,1,12,2,106,7,87,69,66,
		67,65,77,0,8,28,67,176,17,0,95,2,95,1,
		12,2,80,3,176,32,0,95,3,12,1,31,37,176,
		33,0,95,3,20,1,176,28,0,95,3,20,1,176,
		34,0,176,29,0,95,2,95,1,12,2,176,35,0,
		95,1,12,1,20,2,120,98,4,0,93,175,0,2,
		25,10,9,98,4,0,93,175,0,2,7
	};

	hb_vmExecute( pcode, symbols );
}

#line 333 "h_webcam.prg"

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

