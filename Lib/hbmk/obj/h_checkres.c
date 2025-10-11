/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_checkres.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( MGADDRESOURCE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( HB_STRSHRINK );
HB_FUNC_EXTERN( AADD );
HB_FUNC( MGDELRESOURCE );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC( CHECKRES );
HB_FUNC_EXTERN( _SETGETLOGFILE );
HB_FUNC_EXTERN( GETSTARTUPFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( HB_PROGNAME );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( _LOGFILE );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CHECKRES )
{ "MGADDRESOURCE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MGADDRESOURCE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "HB_STRSHRINK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_STRSHRINK )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "MGDELRESOURCE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MGDELRESOURCE )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "CHECKRES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CHECKRES )}, NULL },
{ "_SETGETLOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETLOGFILE )}, NULL },
{ "GETSTARTUPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSTARTUPFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "HB_PROGNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PROGNAME )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "_LOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _LOGFILE )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CHECKRES, "h_checkres.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CHECKRES
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CHECKRES )
   #include "hbiniseg.h"
#endif

HB_FUNC( MGADDRESOURCE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSFrame( symbols + 19 );
	hb_xvmSetLine( 39 );
	hb_xvmLocalSetInt( 3, 3L );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 41 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 42 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")->", 3 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 43 );
	if( hb_xvmLocalInc( 3 ) ) break;
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 46 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 47 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 50 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 52 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MGDELRESOURCE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 19 );
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStatic( 1 );
	{
		static const HB_BYTE codeblock[ 15 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 92, 2, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 76 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CHECKRES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSFrame( symbols + 19 );
	hb_xvmSetLine( 94 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "checkres.txt", 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( " -- ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 104 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( " -- ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "=", 1 );
	hb_xvmPushInteger( 99 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 110 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 19, 1 );
	hb_xvmSFrame( symbols + 19 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopStatic( 1 );
	{
		static const HB_BYTE statics[ 2 ] = {
			1, 0 };
		hb_xvmThreadStatics( 1, statics );
	}
	/* *** END PROC *** */
   } while( 0 );
}

#line 119 "h_checkres.prg"

#include <windows.h>
#include <hbapiitm.h>
#include <hbvm.h>

/*
 * FUNCTION RegisterResource( HANDLE hRes, LPCSTR szType )
 *
 * Registers a resource with the Harbour resource tracking system from C code.
 *
 * Parameters:
 *   hRes (HANDLE): The handle of the resource to register.
 *   szType (LPCSTR): A string describing the type of the resource.
 *
 * Return Value:
 *   None (void)
 *
 * Purpose:
 *   This function allows C code to register resources with the Harbour resource tracking system.
 *   This is essential when working with external libraries or when resources are allocated directly in C code.
 *   It ensures that these resources are also tracked for potential leaks.
 */
void RegisterResource( HANDLE hRes, LPCSTR szType )
{
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) );  // Create a new Harbour item for the return value.

   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGADDRESOURCE" ) ); // Push the symbol for the MGADDRESOURCE function.
   hb_vmPushNil();                                           // Push a NIL value (required by Harbour calling convention).
   hb_vmPushNumInt( ( LONG_PTR ) hRes );                     // Push the resource handle as a numeric integer.
   hb_vmPushString( szType, strlen( szType ) );              // Push the resource type as a string.
   hb_vmFunction( 2 );                                       // Call the MGADDRESOURCE function with 2 parameters.

   hb_itemReturnRelease( pRet );                             // Release the return value item.
}

/*
 * FUNCTION DelResource( HANDLE hResource )
 *
 * Unregisters a resource from the Harbour resource tracking system from C code.
 *
 * Parameters:
 *   hResource (HANDLE): The handle of the resource to unregister.
 *
 * Return Value:
 *   None (void)
 *
 * Purpose:
 *   This function allows C code to unregister resources from the Harbour resource tracking system.
 *   It's the counterpart to RegisterResource and should be called when a resource allocated in C code is released.
 *   This prevents the resource from being incorrectly flagged as a leak.
 */
void pascal DelResource( HANDLE hResource )
{
   PHB_ITEM pRet = hb_itemNew( hb_param( -1, HB_IT_ANY ) );  // Create a new Harbour item for the return value.

   hb_vmPushSymbol( hb_dynsymGetSymbol( "MGDELRESOURCE" ) ); // Push the symbol for the MGDELRESOURCE function.
   hb_vmPushNil();                                           // Push a NIL value (required by Harbour calling convention).
   hb_vmPushNumInt( ( LONG_PTR ) hResource );                // Push the resource handle as a numeric integer.
   hb_vmFunction( 1 );                                       // Call the MGDELRESOURCE function with 1 parameter.

   hb_itemReturnRelease( pRet );                             // Release the return value item.
}

