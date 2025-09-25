/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_wbrush.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _SETWINDOWBKBRUSH );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( CREATESOLIDBRUSH );
HB_FUNC_EXTERN( CREATEHATCHBRUSH );
HB_FUNC_EXTERN( CREATEPATTERNBRUSH );
HB_FUNC_EXTERN( GETWINDOWBRUSH );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( SETWINDOWBRUSH );
HB_FUNC_EXTERN( DELETEOBJECT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WBRUSH )
{ "_SETWINDOWBKBRUSH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETWINDOWBKBRUSH )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "CREATESOLIDBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATESOLIDBRUSH )}, NULL },
{ "CREATEHATCHBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEHATCHBRUSH )}, NULL },
{ "CREATEPATTERNBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEPATTERNBRUSH )}, NULL },
{ "GETWINDOWBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWBRUSH )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "SETWINDOWBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWBRUSH )}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WBRUSH, "h_wbrush.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WBRUSH
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WBRUSH )
   #include "hbiniseg.h"
#endif

HB_FUNC( _SETWINDOWBKBRUSH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 6 );
	hb_xvmSetLine( 115 );
	hb_xvmLocalSetInt( 9, 0L );
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "SOLID", 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushStringConst( "MINIGUI_EDIT_DELETE", 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 126 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 127 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 129 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "S", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "H", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "P", 1 ) )
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
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 127L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 9 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

