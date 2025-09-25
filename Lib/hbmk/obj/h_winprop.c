/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_winprop.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _SETWINDOWPROP );
HB_FUNC_STATIC( _GETFORMHANDLE );
HB_FUNC_EXTERN( SETPROP );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC( _GETWINDOWPROP );
HB_FUNC_EXTERN( GETPROP );
HB_FUNC_EXTERN( HB_ISNIL );
HB_FUNC( _REMOVEWINDOWPROP );
HB_FUNC_EXTERN( REMOVEPROP );
HB_FUNC( _ENUMWINDOWPROPS );
HB_FUNC_EXTERN( ENUMPROPS );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( GETFORMHANDLE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WINPROP )
{ "_SETWINDOWPROP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETWINDOWPROP )}, NULL },
{ "_GETFORMHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETFORMHANDLE )}, NULL },
{ "SETPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROP )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_GETWINDOWPROP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETWINDOWPROP )}, NULL },
{ "GETPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROP )}, NULL },
{ "HB_ISNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNIL )}, NULL },
{ "_REMOVEWINDOWPROP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _REMOVEWINDOWPROP )}, NULL },
{ "REMOVEPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( REMOVEPROP )}, NULL },
{ "_ENUMWINDOWPROPS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENUMWINDOWPROPS )}, NULL },
{ "ENUMPROPS", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENUMPROPS )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WINPROP, "h_winprop.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WINPROP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WINPROP )
   #include "hbiniseg.h"
#endif

HB_FUNC( _SETWINDOWPROP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 82 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Property ", 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " in Window ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 90 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETWINDOWPROP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 118 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Property ", 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " in Window ", 11 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _REMOVEWINDOWPROP )
{
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENUMWINDOWPROPS )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _GETFORMHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 215 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 221 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 222 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 235 );
	hb_xvmCopyLocals( 1, 2 );
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00007: ;
	hb_xvmSetLine( 240 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

