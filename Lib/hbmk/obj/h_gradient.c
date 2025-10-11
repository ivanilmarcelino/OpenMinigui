/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_gradient.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( INITGRADIENTFUNC );
HB_FUNC( _INITGRADIENTFUNC );
HB_FUNC_EXIT( EXITGRADIENTFUNC );
HB_FUNC( _EXITGRADIENTFUNC );
HB_FUNC( DRAWGRADIENT );
HB_FUNC( ISENABLEDGRADIENT );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( GETDC );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( WNDBOXIN );
HB_FUNC( FILLGRADIENT );
HB_FUNC_EXTERN( WNDBOXRAISED );
HB_FUNC_EXTERN( RELEASEDC );
HB_FUNC_EXTERN( AADD );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_GRADIENT )
{ "INITGRADIENTFUNC$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( INITGRADIENTFUNC )}, NULL },
{ "_INITGRADIENTFUNC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INITGRADIENTFUNC )}, NULL },
{ "EXITGRADIENTFUNC$", {HB_FS_EXIT | HB_FS_LOCAL}, {HB_EXIT_FUNCNAME( EXITGRADIENTFUNC )}, NULL },
{ "_EXITGRADIENTFUNC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _EXITGRADIENTFUNC )}, NULL },
{ "DRAWGRADIENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWGRADIENT )}, NULL },
{ "ISENABLEDGRADIENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISENABLEDGRADIENT )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDC )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "WNDBOXIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( WNDBOXIN )}, NULL },
{ "FILLGRADIENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( FILLGRADIENT )}, NULL },
{ "WNDBOXRAISED", {HB_FS_PUBLIC}, {HB_FUNCNAME( WNDBOXRAISED )}, NULL },
{ "RELEASEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEDC )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_GRADIENT, "h_gradient.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_GRADIENT
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_GRADIENT )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( INITGRADIENTFUNC )
{
   do {
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 63 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_EXIT( EXITGRADIENTFUNC )
{
   do {
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 69 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWGRADIENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 9 );
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 12 );
	hb_xvmSetLine( 93 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 13 );
	hb_xvmSetLine( 95 );
	goto lab00005;
lab00001: ;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 8 ) ) break;
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 8 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 8 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 9 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00003;
		}
		{
			hb_stackPop();
			goto lab00004;
		}
	}
lab00006: ;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 117 );
	goto lab00011;
lab00007: ;
	goto lab00012;
lab00008: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 81 ] = {
			0, 0, 9, 0, 10, 0, 11, 0, 2, 0, 3, 0, 4, 0, 5, 0, 
			8, 0, 12, 0, 13, 0, 176, 8, 0, 95, 255, 12, 1, 80, 254, 176, 
			10, 0, 95, 254, 95, 253, 95, 252, 95, 251, 95, 250, 20, 5, 176, 11, 
			0, 95, 254, 95, 253, 23, 95, 252, 23, 95, 251, 17, 95, 250, 17, 95, 
			249, 95, 248, 95, 247, 20, 8, 176, 13, 0, 95, 255, 95, 254, 12, 2, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00009: ;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 81 ] = {
			0, 0, 9, 0, 10, 0, 11, 0, 2, 0, 3, 0, 4, 0, 5, 0, 
			8, 0, 12, 0, 13, 0, 176, 8, 0, 95, 255, 12, 1, 80, 254, 176, 
			12, 0, 95, 254, 95, 253, 95, 252, 95, 251, 95, 250, 20, 5, 176, 11, 
			0, 95, 254, 95, 253, 23, 95, 252, 23, 95, 251, 17, 95, 250, 17, 95, 
			249, 95, 248, 95, 247, 20, 8, 176, 13, 0, 95, 255, 95, 254, 12, 2, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00010: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 61 ] = {
			0, 0, 9, 0, 10, 0, 11, 0, 2, 0, 3, 0, 4, 0, 5, 0, 
			8, 0, 12, 0, 13, 0, 176, 11, 0, 176, 8, 0, 95, 255, 12, 1, 
			165, 80, 254, 95, 253, 95, 252, 95, 251, 95, 250, 95, 249, 95, 248, 95, 
			247, 20, 8, 176, 13, 0, 95, 255, 95, 254, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 9 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00007;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00008;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00009;
		}
		{
			hb_stackPop();
			goto lab00010;
		}
	}
lab00012: ;
	hb_xvmSetLine( 147 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

#line 153 "h_gradient.prg"

#include <mgdefs.h>                      // Include custom MiniGUI definitions

// Conditional compilation for Windows gradient fill definitions on MinGW and Windows versions < 0x0500
#if defined( __MINGW32__ ) && ( _WIN32_WINNT < 0x0500 )
#define GRADIENT_FILL_RECT_H     0x00000000
#define GRADIENT_FILL_RECT_V     0x00000001
#define GRADIENT_FILL_TRIANGLE   0x00000002
#define GRADIENT_FILL_OP_FLAG    0x000000ff
#endif

// External function to get the address of a procedure in a DLL
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// MiniGUI resources control
void              RegisterResource( HANDLE hResource, LPCSTR szType );

// Functions to enable gradient fills, perform gradient fills, and create gradient brushes
BOOL              EnabledGradient( void );
BOOL              FillGradient( HDC hDC, RECT *rect, BOOL vertical, COLORREF crFrom, COLORREF crTo );
HBRUSH            LinearGradientBrush( HDC pDC, long cx, long cy, COLORREF cFrom, COLORREF cTo, BOOL bVert );

// Typedefs for function pointers to AlphaBlend, TransparentBlt, and GradientFill functions
typedef BOOL ( WINAPI *AlphaBlendPtr ) ( HDC, int, int, int, int, HDC, int, int, int, int, BLENDFUNCTION );
typedef BOOL ( WINAPI *TransparentBltPtr ) ( HDC, int, int, int, int, HDC, int, int, int, int, UINT );
typedef BOOL ( WINAPI *GradientFillPtr ) ( HDC, CONST PTRIVERTEX, DWORD, CONST PVOID, DWORD, DWORD );

// Static variables for DLL instance and function pointers
static HINSTANCE           s_hDLL = NULL;
static GradientFillPtr     f_GradientFill = NULL;
static AlphaBlendPtr       f_AlphaBlend = NULL;
static TransparentBltPtr   f_TransparentBlt = NULL;

// Check if gradient functions are enabled by verifying the DLL instance
BOOL EnabledGradient( void )
{
   return s_hDLL != NULL ? HB_TRUE : HB_FALSE;
}

// Harbour function to return whether gradient fill is enabled
HB_FUNC( ISENABLEDGRADIENT )
{
   hb_retl( EnabledGradient() );
}

// Harbour function to initialize gradient-related function pointers from DLL
HB_FUNC( _INITGRADIENTFUNC )
{
   // Load gdi32.dll and get function pointers for gradient and transparency functions
   s_hDLL = LoadLibrary( TEXT( "gdi32.dll" ) );
   if( s_hDLL != NULL )
   {
      f_AlphaBlend = ( AlphaBlendPtr ) wapi_GetProcAddress( s_hDLL, "GdiAlphaBlend" );
      f_TransparentBlt = ( TransparentBltPtr ) wapi_GetProcAddress( s_hDLL, "GdiTransparentBlt" );
      f_GradientFill = ( GradientFillPtr ) wapi_GetProcAddress( s_hDLL, "GdiGradientFill" );

      // Unload DLL if functions were not found
      if( ( f_AlphaBlend == NULL ) && ( f_TransparentBlt == NULL ) && ( f_GradientFill == NULL ) )
      {
         FreeLibrary( s_hDLL );
         s_hDLL = NULL;
      }
   }

   // Fallback to msimg32.dll if gdi32.dll is unavailable or functions not found
   if( s_hDLL == NULL )
   {
      s_hDLL = LoadLibrary( TEXT( "msimg32.dll" ) );
      if( s_hDLL != NULL )
      {
         f_AlphaBlend = ( AlphaBlendPtr ) wapi_GetProcAddress( s_hDLL, "AlphaBlend" );
         f_TransparentBlt = ( TransparentBltPtr ) wapi_GetProcAddress( s_hDLL, "TransparentBlt" );
         f_GradientFill = ( GradientFillPtr ) wapi_GetProcAddress( s_hDLL, "GradientFill" );

         // Unload DLL if required functions are still missing
         if( ( f_AlphaBlend == NULL ) && ( f_TransparentBlt == NULL ) && ( f_GradientFill == NULL ) )
         {
            FreeLibrary( s_hDLL );
            s_hDLL = NULL;
         }
      }
   }

   // Return status of gradient function availability
   hb_retl( EnabledGradient() ? HB_TRUE : HB_FALSE );
}

// Harbour function to free gradient-related resources and unload DLL
HB_FUNC( _EXITGRADIENTFUNC )
{
   if( s_hDLL != NULL )
   {
      FreeLibrary( s_hDLL );
      s_hDLL = NULL;
   }
}

// Harbour function to perform alpha blending
HB_FUNC( ALPHABLEND )
{
   BOOL  bRes = HB_FALSE;
   HDC   hdc1 = hmg_par_raw_HDC( 1 );  // Get source HDC
   HDC   hdc2 = hmg_par_raw_HDC( 6 );  // Get destination HDC

   // Validate the device contexts and call AlphaBlend if enabled
   if( ( GetObjectType( hdc1 ) & ( OBJ_DC | OBJ_MEMDC ) ) && ( GetObjectType( hdc2 ) & ( OBJ_DC | OBJ_MEMDC ) ) )
   {
      if( ( s_hDLL != NULL ) && ( f_AlphaBlend != NULL ) )
      {
         BLENDFUNCTION  bf;
         bf.BlendOp = AC_SRC_OVER;
         bf.BlendFlags = 0;
         bf.SourceConstantAlpha = hmg_par_BYTE( 11 ); // Alpha transparency
         bf.AlphaFormat = hmg_par_BYTE( 12 );         // Pixel format
         bRes = f_AlphaBlend
            (
               hdc1,
               hb_parnl( 2 ),
               hb_parnl( 3 ),
               hb_parnl( 4 ),
               hb_parnl( 5 ),
               hdc2,
               hb_parnl( 7 ),
               hb_parnl( 8 ),
               hb_parnl( 9 ),
               hb_parnl( 10 ),
               bf
            );
      }
   }

   hmg_ret_L( bRes );
}

// Harbour function to perform transparent bit block transfer
HB_FUNC( TRANSPARENTBLT )
{
   BOOL  bRes = HB_FALSE;
   HDC   hdc1 = hmg_par_raw_HDC( 1 );  // Get source HDC
   HDC   hdc2 = hmg_par_raw_HDC( 6 );  // Get destination HDC

   // Validate the device contexts and call TransparentBlt if enabled
   if( ( GetObjectType( hdc1 ) & ( OBJ_DC | OBJ_MEMDC ) ) && ( GetObjectType( hdc2 ) & ( OBJ_DC | OBJ_MEMDC ) ) )
   {
      if( ( s_hDLL != NULL ) && ( f_TransparentBlt != NULL ) )
      {
         int   iStretchMode = hb_parnidef( 12, COLORONCOLOR );
         BOOL  bHiRes = ( iStretchMode == HALFTONE );
         POINT pt = { 0, 0 };

         if( bHiRes )
         {
            GetBrushOrgEx( hdc1, &pt );
         }

         SetStretchBltMode( hdc1, iStretchMode );
         if( bHiRes )
         {
            SetBrushOrgEx( hdc1, pt.x, pt.y, NULL );
         }

         bRes = f_TransparentBlt
            (
               hdc1,
               hb_parnl( 2 ),
               hb_parnl( 3 ),
               hb_parnl( 4 ),
               hb_parnl( 5 ),
               hdc2,
               hb_parnl( 7 ),
               hb_parnl( 8 ),
               hb_parnl( 9 ),
               hb_parnl( 10 ),
               hmg_par_COLORREF( 11 )
            );
      }
   }

   hmg_ret_L( bRes );
}

// Harbour function to fill a rectangle with a gradient
HB_FUNC( FILLGRADIENT )
{
   BOOL  bRes = HB_FALSE;
   HDC   hdc = hmg_par_raw_HDC( 1 );   // Get HDC

   // Check device context type and apply gradient fill if enabled
   if( GetObjectType( hdc ) & ( OBJ_DC | OBJ_MEMDC ) )
   {
      RECT  rect;
      rect.top = hb_parni( 2 );
      rect.left = hb_parni( 3 );
      rect.bottom = hb_parni( 4 );
      rect.right = hb_parni( 5 );

      bRes = ( FillGradient( hdc, &rect, hb_parl( 6 ), hmg_par_COLORREF( 7 ), hmg_par_COLORREF( 8 ) ) );
   }

   hmg_ret_L( bRes );
}

// Helper function to execute the gradient fill operation
BOOL FillGradient( HDC hDC, RECT *rect, BOOL vertical, COLORREF crFrom, COLORREF crTo )
{
   BOOL  bRes = HB_FALSE;

   // If DLL and GradientFill function are enabled, set up and apply gradient fill
   if( ( s_hDLL != NULL ) && ( f_GradientFill != NULL ) )
   {
      TRIVERTEX      rcVertex[2];
      GRADIENT_RECT  gRect;

      rcVertex[0].y = rect->top;
      rcVertex[0].x = rect->left;
      rcVertex[0].Red = ( unsigned short ) ( GetRValue( crFrom ) << 8 );
      rcVertex[0].Green = ( unsigned short ) ( GetGValue( crFrom ) << 8 );
      rcVertex[0].Blue = ( unsigned short ) ( GetBValue( crFrom ) << 8 );
      rcVertex[0].Alpha = 0;

      rcVertex[1].y = rect->bottom;
      rcVertex[1].x = rect->right;
      rcVertex[1].Red = ( unsigned short ) ( GetRValue( crTo ) << 8 );
      rcVertex[1].Green = ( unsigned short ) ( GetGValue( crTo ) << 8 );
      rcVertex[1].Blue = ( unsigned short ) ( GetBValue( crTo ) << 8 );
      rcVertex[1].Alpha = 0;

      gRect.UpperLeft = 0;
      gRect.LowerRight = 1;

      bRes = f_GradientFill( hDC, rcVertex, 2, &gRect, 1, vertical ? GRADIENT_FILL_RECT_V : GRADIENT_FILL_RECT_H );
   }

   return bRes;
}

// Harbour function to create and return a gradient brush
HB_FUNC( CREATEGRADIENTBRUSH )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   HDC      hdc;
   HBRUSH   hBrush;

   if( !IsWindow( hwnd ) )
   {
      hwnd = GetDesktopWindow();
   }

   hdc = GetDC( hwnd );

   hBrush = LinearGradientBrush( hdc, hb_parnl( 2 ), hb_parnl( 3 ), hmg_par_COLORREF( 4 ), hmg_par_COLORREF( 5 ), hb_parl( 6 ) );

   RegisterResource( hBrush, "BRUSH" );
   hmg_ret_raw_HBRUSH( hBrush );

   ReleaseDC( hwnd, hdc );
}

// Helper function to create a brush with a linear gradient fill
HBRUSH LinearGradientBrush( HDC pDC, long cx, long cy, COLORREF crFrom, COLORREF crTo, BOOL bVert )
{
   HDC      memDC;
   HBITMAP  memBmp;
   HBRUSH   pGradientBrush = ( HBRUSH ) NULL;

   memDC = CreateCompatibleDC( pDC );
   memBmp = CreateCompatibleBitmap( pDC, cx, cy );

   if( memDC && memBmp )
   {
      TRIVERTEX      rcVertex[2];
      GRADIENT_RECT  gRect;

      rcVertex[0].x = 0;
      rcVertex[0].y = 0;
      rcVertex[0].Red = ( unsigned short ) ( GetRValue( crFrom ) << 8 );
      rcVertex[0].Green = ( unsigned short ) ( GetGValue( crFrom ) << 8 );
      rcVertex[0].Blue = ( unsigned short ) ( GetBValue( crFrom ) << 8 );
      rcVertex[0].Alpha = 0;

      rcVertex[1].x = cx;
      rcVertex[1].y = cy;
      rcVertex[1].Red = ( unsigned short ) ( GetRValue( crTo ) << 8 );
      rcVertex[1].Green = ( unsigned short ) ( GetGValue( crTo ) << 8 );
      rcVertex[1].Blue = ( unsigned short ) ( GetBValue( crTo ) << 8 );
      rcVertex[1].Alpha = 0;

      gRect.UpperLeft = 0;
      gRect.LowerRight = 1;

      SelectObject( memDC, memBmp );

      if( s_hDLL != NULL && f_GradientFill != NULL )
      {
         f_GradientFill( memDC, rcVertex, 2, &gRect, 1, bVert ? GRADIENT_FILL_RECT_V : GRADIENT_FILL_RECT_H );
      }

      // Create pattern brush from gradient-filled bitmap
      pGradientBrush = CreatePatternBrush( memBmp );

      // Clean up resources
      DeleteObject( memBmp );
      DeleteDC( memDC );
   }

   return pGradientBrush;
}

