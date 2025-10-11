/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_hyperlink.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _SETADDRESS );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( _SETADDRESSCONTROLPROCEDURE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( SHELLEXECUTE );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( ISWINNT );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( DIRECTORY );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( HB_ISFUNCTION );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( GETPARENTFORMNAME );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_HYPERLINK )
{ "_SETADDRESS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETADDRESS )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_SETADDRESSCONTROLPROCEDURE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETADDRESSCONTROLPROCEDURE )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "SHELLEXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHELLEXECUTE )}, NULL },
{ "LOWER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWER )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "ISWINNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINNT )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "DIRECTORY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DIRECTORY )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "HB_ISFUNCTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISFUNCTION )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "GETPARENTFORMNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPARENTFORMNAME )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_HYPERLINK, "h_hyperlink.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_HYPERLINK
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_HYPERLINK )
   #include "hbiniseg.h"
#endif

HB_FUNC( _SETADDRESS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 55 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 57 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 60 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "LABEL", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 61 );
	hb_xvmPushStringConst( "HYPERLINK", 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
lab00001: ;
	hb_xvmSetLine( 66 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETADDRESSCONTROLPROCEDURE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 75 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00001: ;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "@", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 79 );
	{
		static const HB_BYTE codeblock[ 78 ] = {
			0, 0, 1, 0, 2, 0, 176, 6, 0, 121, 106, 5, 111, 112, 101, 110, 
			0, 106, 13, 114, 117, 110, 100, 108, 108, 51, 50, 46, 101, 120, 101, 0, 
			106, 36, 117, 114, 108, 46, 100, 108, 108, 44, 70, 105, 108, 101, 80, 114, 
			111, 116, 111, 99, 111, 108, 72, 97, 110, 100, 108, 101, 114, 32, 109, 97, 
			105, 108, 116, 111, 58, 0, 95, 255, 72, 100, 122, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00002: ;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "http", 4 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 83 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			0, 0, 1, 0, 2, 0, 176, 6, 0, 121, 106, 5, 111, 112, 101, 110, 
			0, 95, 255, 100, 100, 122, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00004: ;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "file:\\\\", 7 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 88 );
	{
		static const HB_BYTE codeblock[ 46 ] = {
			0, 0, 1, 0, 2, 0, 176, 6, 0, 121, 106, 5, 111, 112, 101, 110, 
			0, 106, 13, 101, 120, 112, 108, 111, 114, 101, 114, 46, 101, 120, 101, 0, 
			106, 4, 47, 101, 44, 0, 95, 255, 72, 100, 122, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "file:\\\\", 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 91 );
	{
		static const HB_BYTE codeblock[ 85 ] = {
			0, 0, 1, 0, 2, 0, 176, 6, 0, 121, 106, 5, 111, 112, 101, 110, 
			0, 106, 13, 101, 120, 112, 108, 111, 114, 101, 114, 46, 101, 120, 101, 0, 
			106, 12, 47, 101, 44, 47, 115, 101, 108, 101, 99, 116, 44, 0, 95, 255, 
			72, 176, 11, 0, 12, 0, 72, 176, 12, 0, 95, 255, 176, 11, 0, 12, 
			0, 72, 106, 4, 42, 46, 42, 0, 72, 12, 1, 122, 1, 122, 1, 72, 
			100, 122, 12, 6, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "proc:\\\\", 7 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "(", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 103 );
	hb_xvmPushStringConst( "{||", 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroPush( 43 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Control ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " must have a valid procedure name defined.", 42 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Control ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " must have a valid email, url or file defined.", 46 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 114 );
	/* *** END PROC *** */
   } while( 0 );
}

