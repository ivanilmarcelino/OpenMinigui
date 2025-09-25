/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_winapimisc.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( WINDOWSVERSION );
HB_FUNC_EXTERN( ISWIN10ORLATER );
HB_FUNC_EXTERN( GETREGISTRYVALUE );
HB_FUNC_EXTERN( HB_OSISWIN11 );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( WINVERSION );
HB_FUNC( _EXECUTE );
HB_FUNC_EXTERN( SHELLEXECUTE );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( GETACTIVEWINDOW );
HB_FUNC( SHELLABOUT );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( C_SHELLABOUT );
HB_FUNC_EXTERN( ISHICON );
HB_FUNC_EXTERN( DESTROYICON );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WINAPIMISC )
{ "WINDOWSVERSION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( WINDOWSVERSION )}, NULL },
{ "ISWIN10ORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWIN10ORLATER )}, NULL },
{ "GETREGISTRYVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETREGISTRYVALUE )}, NULL },
{ "HB_OSISWIN11", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSISWIN11 )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "WINVERSION", {HB_FS_PUBLIC}, {HB_FUNCNAME( WINVERSION )}, NULL },
{ "_EXECUTE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _EXECUTE )}, NULL },
{ "SHELLEXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHELLEXECUTE )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "GETACTIVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEWINDOW )}, NULL },
{ "SHELLABOUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SHELLABOUT )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "C_SHELLABOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_SHELLABOUT )}, NULL },
{ "ISHICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISHICON )}, NULL },
{ "DESTROYICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYICON )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WINAPIMISC, "h_winapimisc.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WINAPIMISC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WINAPIMISC )
   #include "hbiniseg.h"
#endif

HB_FUNC( WINDOWSVERSION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 87 );
	hb_xvmPushInteger( 4 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 90 );
	hb_xvmPushStringConst( "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion", 44 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ProductName", 11 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "10", 2 );
	hb_xvmPushStringConst( "11", 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "DisplayVersion", 14 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ReleaseId", 9 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00002: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "CurrentBuild", 12 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLongLong( HB_LL( 2147483650 ) );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "UBR", 3 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStringConst( "Build ", 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _EXECUTE )
{
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SHELLABOUT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_ShellAbout", 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_ShellAbout", 15 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_ShellAbout", 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_ShellAbout", 15 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 192 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_ShellAbout", 15 );
	if( hb_xvmLocalDec( 4 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 200 );
	/* *** END PROC *** */
   } while( 0 );
}

