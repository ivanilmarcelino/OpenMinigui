/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_msgbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_STATIC( _MSGBOX );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( HB_VALTOSTR );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( MESSAGEBOXINDIRECT );
HB_FUNC( MSGYESNO );
HB_FUNC( MSGYESNOCANCEL );
HB_FUNC( MSGRETRYCANCEL );
HB_FUNC( MSGOKCANCEL );
HB_FUNC( MSGEXCLAMATION );
HB_FUNC( MSGINFO );
HB_FUNC( MSGSTOP );
HB_FUNC( MSGBOX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_MSGBOX )
{ "_MSGBOX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MSGBOX )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "HB_VALTOSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOSTR )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "MESSAGEBOXINDIRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MESSAGEBOXINDIRECT )}, NULL },
{ "MSGYESNO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGYESNO )}, NULL },
{ "MSGYESNOCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGYESNOCANCEL )}, NULL },
{ "MSGRETRYCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGRETRYCANCEL )}, NULL },
{ "MSGOKCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGOKCANCEL )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "MSGBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGBOX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_MSGBOX, "h_msgbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_MSGBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_MSGBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC_STATIC( _MSGBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 98 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 7, 0, 96, 255, 255, 176, 5, 0, 95, 1, 12, 1, 
			139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmCopyLocals( 7, 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 4096 );
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 0 );
lab00004: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocalByRef( 3 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 262144 );
#else
	hb_xvmPushLong( 262144L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGYESNO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 132 );
	hb_xvmLocalSetInt( 7, 4L );
	hb_xvmSetLine( 134 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 32 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 137 );
	if( hb_xvmLocalAddInt( 7, 256 ) ) break;
lab00003: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	if( hb_xvmEqualInt( 6L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGYESNOCANCEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 165 );
	hb_xvmLocalSetInt( 7, 3L );
	hb_xvmSetLine( 167 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 32 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 169 );
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 172 );
	if( hb_xvmLocalAddInt( 7, 256 ) ) break;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 175 );
	if( hb_xvmLocalAddInt( 7, 512 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00006: ;
	hb_xvmSetLine( 179 );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 182 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 184 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 6L )
		{
			hb_stackPop();
			goto lab00007;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 7L )
		{
			hb_stackPop();
			goto lab00008;
		}
		hb_stackPop();
	}
	hb_xvmSetLine( 188 );
	hb_xvmRetInt( -1L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGRETRYCANCEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 210 );
	hb_xvmLocalSetInt( 7, 5L );
	hb_xvmSetLine( 212 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 32 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 215 );
	if( hb_xvmLocalAddInt( 7, 256 ) ) break;
lab00003: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	if( hb_xvmEqualInt( 4L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGOKCANCEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 240 );
	hb_xvmLocalSetInt( 7, 1L );
	hb_xvmSetLine( 242 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 32 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 245 );
	if( hb_xvmLocalAddInt( 7, 256 ) ) break;
lab00003: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 6 ) ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGEXCLAMATION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 267 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 269 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 48 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 13 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 290 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 292 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 64 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 13 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGSTOP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 313 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 315 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushInteger( 16 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 128 );
lab00002: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 13 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGBOX )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 336 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

