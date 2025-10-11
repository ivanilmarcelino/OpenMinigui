/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_help.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( SETHELPFILE );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( FOPEN );
HB_FUNC_EXTERN( FERROR );
HB_FUNC_EXTERN( FCLOSE );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC( DISPLAYHELPTOPIC );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( HB_FNAMEEXT );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( _EXECUTE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( WINHELP );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_HELP )
{ "SETHELPFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETHELPFILE )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "FOPEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( FOPEN )}, NULL },
{ "FERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERROR )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "FCLOSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FCLOSE )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "DISPLAYHELPTOPIC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DISPLAYHELPTOPIC )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "HB_FNAMEEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEEXT )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_EXECUTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _EXECUTE )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "WINHELP", {HB_FS_PUBLIC}, {HB_FUNCNAME( WINHELP )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_HELP, "h_help.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_HELP
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_HELP )
   #include "hbiniseg.h"
#endif

HB_FUNC( SETHELPFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 82 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 207L ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 207L ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Error opening of help file. Error: ", 35 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Alert", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Help file ", 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not found!", 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 97 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DISPLAYHELPTOPIC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 127 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 130 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 205L ) ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 206L ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".CHM", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 138 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushStringConst( "-mapid ", 7 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	hb_xvmLocalAdd( 3 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 143 );
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "::/", 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmLocalAdd( 3 );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmLocalAdd( 3 );
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "U", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00006: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 30L ) ) break;
	hb_xvmPushStringConst( "open", 4 );
	hb_xvmPushStringConst( "hh.exe", 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 5 );
	if( hb_xvmDo( 6 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 30L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 207L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00008: ;
	hb_xvmSetLine( 159 );
	/* *** END PROC *** */
   } while( 0 );
}

