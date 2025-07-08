/*
 * Harbour 3.2.0dev (r2503200530)
 * Borland C++ 5.8.2 (32-bit)
 * Generated C source from "h_animate.prg"
 */

#include "hbvmpub.h"
#include "hbinit.h"


HB_FUNC_INIT( _INITANIMATERES );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC_EXTERN( INSTALLPROPERTYHANDLER );
HB_FUNC( _DEFINEANIMATERES );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( INITANIMATERES );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC( SETANIMATERESFILE );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( GETANIMATERESFILE );
HB_FUNC( SETANIMATERESID );
HB_FUNC( GETANIMATERESID );
HB_FUNC( RELEASEANIMATERES );
HB_FUNC( UNLOADANIMATELIB );
HB_FUNC_EXTERN( _GETCONTROLOBJECT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ANIMATE )
{ "_INITANIMATERES$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITANIMATERES )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "INSTALLPROPERTYHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLPROPERTYHANDLER )}, NULL },
{ "_DEFINEANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEANIMATERES )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITANIMATERES )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETANIMATERESFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETANIMATERESFILE )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "GETANIMATERESFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETANIMATERESFILE )}, NULL },
{ "SETANIMATERESID", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETANIMATERESID )}, NULL },
{ "GETANIMATERESID", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETANIMATERESID )}, NULL },
{ "RELEASEANIMATERES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEANIMATERES )}, NULL },
{ "UNLOADANIMATELIB", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UNLOADANIMATELIB )}, NULL },
{ "_GETCONTROLOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLOBJECT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ANIMATE, "h_animate.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ANIMATE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ANIMATE )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITANIMATERES )
{
	static const HB_BYTE pcode[] =
	{
		176,1,0,106,8,82,101,108,101,97,115,101,0,106,
		18,82,101,108,101,97,115,101,65,110,105,109,97,116,
		101,82,101,115,0,20,2,176,2,0,106,5,70,105,
		108,101,0,106,18,83,101,116,65,110,105,109,97,116,
		101,82,101,115,70,105,108,101,0,106,18,71,101,116,
		65,110,105,109,97,116,101,82,101,115,70,105,108,101,
		0,20,3,176,2,0,106,6,82,101,115,73,100,0,
		106,16,83,101,116,65,110,105,109,97,116,101,82,101,
		115,73,100,0,106,16,71,101,116,65,110,105,109,97,
		116,101,82,101,115,73,100,0,20,3,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( _DEFINEANIMATERES )
{
	static const HB_BYTE pcode[] =
	{
		13,4,11,176,4,0,96,5,0,93,200,0,20,2,
		176,4,0,96,6,0,92,50,20,2,176,4,0,96,
		11,0,9,20,2,98,5,0,92,34,1,28,10,98,
		5,0,92,33,1,80,2,98,5,0,92,37,1,121,
		15,28,51,96,3,0,98,5,0,92,40,1,98,5,
		0,92,37,1,1,135,96,4,0,98,5,0,92,39,
		1,98,5,0,92,37,1,1,135,98,5,0,92,38,
		1,98,5,0,92,37,1,1,80,2,176,6,0,95,
		2,12,1,31,41,176,7,0,106,9,87,105,110,100,
		111,119,58,32,0,95,2,72,106,17,32,105,115,32,
		110,111,116,32,100,101,102,105,110,101,100,46,0,72,
		20,1,176,8,0,95,1,12,1,28,18,95,1,106,
		2,48,0,8,28,9,176,9,0,12,0,80,1,176,
		10,0,95,1,95,2,12,2,28,54,176,7,0,106,
		10,67,111,110,116,114,111,108,58,32,0,95,1,72,
		106,5,32,79,102,32,0,72,95,2,72,106,18,32,
		65,108,114,101,97,100,121,32,100,101,102,105,110,101,
		100,46,0,72,20,1,106,2,95,0,95,2,72,106,
		2,95,0,72,95,1,72,80,15,176,11,0,95,15,
		176,12,0,98,5,0,93,136,0,1,12,1,23,20,
		2,95,2,80,14,176,13,0,95,2,12,1,80,2,
		176,14,0,95,2,96,13,0,95,3,95,4,95,5,
		95,6,95,7,95,8,95,11,12,9,80,12,98,5,
		0,92,41,1,28,15,176,15,0,98,5,0,92,45,
		1,95,12,20,2,95,9,100,69,28,18,176,16,0,
		95,12,95,9,176,17,0,95,14,12,1,20,3,176,
		15,0,98,5,0,93,135,0,1,106,11,65,78,73,
		77,65,84,69,82,69,83,0,20,2,176,15,0,98,
		5,0,93,136,0,1,95,1,20,2,176,15,0,98,
		5,0,93,137,0,1,95,12,20,2,176,15,0,98,
		5,0,93,138,0,1,95,2,20,2,176,15,0,98,
		5,0,93,139,0,1,95,8,20,2,176,15,0,98,
		5,0,93,140,0,1,106,1,0,20,2,176,15,0,
		98,5,0,93,141,0,1,4,0,0,20,2,176,15,
		0,98,5,0,93,142,0,1,95,7,20,2,176,15,
		0,98,5,0,93,143,0,1,106,1,0,20,2,176,
		15,0,98,5,0,93,144,0,1,106,1,0,20,2,
		176,15,0,98,5,0,93,145,0,1,106,1,0,20,
		2,176,15,0,98,5,0,93,146,0,1,106,1,0,
		20,2,176,15,0,98,5,0,93,134,0,1,9,20,
		2,176,15,0,98,5,0,93,147,0,1,100,20,2,
		176,15,0,98,5,0,93,148,0,1,100,20,2,176,
		15,0,98,5,0,93,149,0,1,106,1,0,20,2,
		176,15,0,98,5,0,93,150,0,1,4,0,0,20,
		2,176,15,0,98,5,0,93,151,0,1,95,4,20,
		2,176,15,0,98,5,0,93,152,0,1,95,3,20,
		2,176,15,0,98,5,0,93,153,0,1,95,5,20,
		2,176,15,0,98,5,0,93,154,0,1,95,6,20,
		2,176,15,0,98,5,0,93,155,0,1,121,20,2,
		176,15,0,98,5,0,93,156,0,1,98,5,0,92,
		37,1,121,15,28,17,98,5,0,92,39,1,98,5,
		0,92,37,1,1,25,4,92,255,20,2,176,15,0,
		98,5,0,93,157,0,1,98,5,0,92,37,1,121,
		15,28,17,98,5,0,92,40,1,98,5,0,92,37,
		1,1,25,4,92,255,20,2,176,15,0,98,5,0,
		93,158,0,1,106,1,0,20,2,176,15,0,98,5,
		0,93,159,0,1,121,20,2,176,15,0,98,5,0,
		93,160,0,1,106,1,0,20,2,176,15,0,98,5,
		0,93,161,0,1,121,20,2,176,15,0,98,5,0,
		93,168,0,1,9,9,9,9,4,4,0,20,2,176,
		15,0,98,5,0,93,162,0,1,95,9,20,2,176,
		15,0,98,5,0,93,163,0,1,121,20,2,176,15,
		0,98,5,0,93,164,0,1,121,20,2,176,15,0,
		98,5,0,93,165,0,1,106,1,0,20,2,176,15,
		0,98,5,0,93,166,0,1,95,11,28,5,9,25,
		3,120,20,2,176,15,0,98,5,0,93,209,0,1,
		95,10,20,2,176,15,0,98,5,0,93,167,0,1,
		121,20,2,176,15,0,98,5,0,93,169,0,1,121,
		20,2,176,15,0,98,5,0,93,170,0,1,120,20,
		2,176,15,0,98,5,0,93,171,0,1,95,13,20,
		2,176,15,0,98,5,0,93,168,1,1,106,1,0,
		20,2,98,5,0,93,192,1,1,92,7,1,28,32,
		48,18,0,98,5,0,93,192,1,1,92,3,1,176,
		12,0,98,5,0,93,136,0,1,12,1,95,15,112,
		2,73,100,110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( SETANIMATERESFILE )
{
	static const HB_BYTE pcode[] =
	{
		13,0,4,176,20,0,95,2,95,1,12,2,106,11,
		65,78,73,77,65,84,69,82,69,83,0,8,28,48,
		176,21,0,95,3,12,1,106,5,70,73,76,69,0,
		8,28,31,120,98,5,0,93,175,0,2,95,4,98,
		5,0,93,142,0,1,176,22,0,95,2,95,1,12,
		2,2,25,10,9,98,5,0,93,175,0,2,100,110,
		7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( GETANIMATERESFILE )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,100,80,3,176,20,0,95,2,95,1,12,
		2,106,11,65,78,73,77,65,84,69,82,69,83,0,
		8,28,31,120,98,5,0,93,175,0,2,98,5,0,
		93,142,0,1,176,22,0,95,2,95,1,12,2,1,
		80,3,25,10,9,98,5,0,93,175,0,2,95,3,
		110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( SETANIMATERESID )
{
	static const HB_BYTE pcode[] =
	{
		13,0,4,176,20,0,95,2,95,1,12,2,106,11,
		65,78,73,77,65,84,69,82,69,83,0,8,28,49,
		176,21,0,95,3,12,1,106,6,82,69,83,73,68,
		0,8,28,31,120,98,5,0,93,175,0,2,95,4,
		98,5,0,93,139,0,1,176,22,0,95,2,95,1,
		12,2,2,25,10,9,98,5,0,93,175,0,2,100,
		110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( GETANIMATERESID )
{
	static const HB_BYTE pcode[] =
	{
		13,1,2,100,80,3,176,20,0,95,2,95,1,12,
		2,106,11,65,78,73,77,65,84,69,82,69,83,0,
		8,28,31,120,98,5,0,93,175,0,2,98,5,0,
		93,139,0,1,176,22,0,95,2,95,1,12,2,1,
		80,3,25,10,9,98,5,0,93,175,0,2,95,3,
		110,7
	};

	hb_vmExecute( pcode, symbols );
}

HB_FUNC( RELEASEANIMATERES )
{
	static const HB_BYTE pcode[] =
	{
		13,0,2,176,10,0,95,2,95,1,12,2,28,51,
		176,20,0,95,2,95,1,12,2,106,11,65,78,73,
		77,65,84,69,82,69,83,0,8,28,26,176,27,0,
		176,28,0,95,2,95,1,12,2,20,1,120,98,5,
		0,93,175,0,2,25,10,9,98,5,0,93,175,0,
		2,7
	};

	hb_vmExecute( pcode, symbols );
}

#line 264 "h_animate.prg"

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

