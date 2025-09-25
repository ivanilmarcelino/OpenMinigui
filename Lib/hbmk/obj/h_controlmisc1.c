/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_controlmisc1.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _GETTEXTHEIGHT );
HB_FUNC_EXTERN( GETTEXTMETRIC );
HB_FUNC( _INVERTRECT );
HB_FUNC_EXTERN( INVERTRECT );
HB_FUNC( OSEND );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC( ASAVE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( I2BIN );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC( AREAD );
HB_FUNC_EXTERN( BIN2I );
HB_FUNC( OREAD );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( VAL );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CONTROLMISC1 )
{ "_GETTEXTHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETTEXTHEIGHT )}, NULL },
{ "GETTEXTMETRIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTMETRIC )}, NULL },
{ "_INVERTRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INVERTRECT )}, NULL },
{ "INVERTRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INVERTRECT )}, NULL },
{ "OSEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OSEND )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "ASAVE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ASAVE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "SAVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "I2BIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( I2BIN )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "AREAD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( AREAD )}, NULL },
{ "BIN2I", {HB_FS_PUBLIC}, {HB_FUNCNAME( BIN2I )}, NULL },
{ "OREAD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OREAD )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LOAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CONTROLMISC1, "h_controlmisc1.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CONTROLMISC1
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CONTROLMISC1 )
   #include "hbiniseg.h"
#endif

HB_FUNC( _GETTEXTHEIGHT )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 10 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _INVERTRECT )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 17 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 18 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 20 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OSEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 12 );
	hb_xvmSetLine( 25 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 28 );
	hb_xvmPushStringConst( "(", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 29 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "()", 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 31 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00001: ;
	hb_xvmSetLine( 32 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00002: ;
	hb_xvmSetLine( 33 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00003: ;
	hb_xvmSetLine( 34 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00004: ;
	hb_xvmSetLine( 35 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00005: ;
	hb_xvmSetLine( 36 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 5 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00006: ;
	hb_xvmSetLine( 37 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 6 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00007: ;
	hb_xvmSetLine( 38 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 7 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 39 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 8 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00009: ;
	hb_xvmSetLine( 40 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 9 ) ) break;
	hb_xvmPopLocal( 14 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 41 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmEqualIntIs( 10L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 10 ) ) break;
	hb_xvmPopLocal( 14 );
lab00011: ;
	hb_xvmSetLine( 43 );
	hb_xvmPushLocal( 14 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00012: ;
	hb_xvmSetLine( 45 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 46 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00013: ;
	hb_xvmSetLine( 48 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroSymbol() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ASAVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 60 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 62 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 65 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 66 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 68 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 62 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 72 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( AREAD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 1 );
	hb_xvmSetLine( 77 );
	hb_xvmLocalSetInt( 2, 4L );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 85 );
	if( hb_xvmLocalAddInt( 2, 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 88 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmLocalInc( 2 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 91 );
	if( hb_xvmLocalAddInt( 2, 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 93 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00007;
lab00002: ;
	hb_xvmSetLine( 97 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00007;
lab00003: ;
	hb_xvmSetLine( 98 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( ".T.", 3 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
lab00007: ;
	hb_xvmSetLine( 88 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OREAD )
{
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 113 );
	hb_xvmLocalSetInt( 5, 1L );
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 116 );
	if( hb_xvmLocalAddInt( 5, 2 ) ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 118 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "()", 2 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 121 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 122 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 124 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

