/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_registry.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TREG32 );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TREG32_NEW );
HB_FUNC_STATIC( TREG32_CREATE );
HB_FUNC_STATIC( TREG32_GET );
HB_FUNC_STATIC( TREG32_SET );
HB_FUNC_STATIC( TREG32_DELETE );
HB_FUNC_STATIC( TREG32_SHOWERRORIF );
HB_FUNC_EXTERN( REGCLOSEKEY );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( REGOPENKEYEXA );
HB_FUNC_STATIC( ISWOW64 );
HB_FUNC_EXTERN( REGCREATEKEY );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( REGQUERYVALUEEXA );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_STATIC( _CONVERTVALUEFROMREG );
HB_FUNC_STATIC( _CONVERTVALUETOREG );
HB_FUNC_EXTERN( REGSETVALUEEXA );
HB_FUNC_EXTERN( REGDELETEVALUEA );
HB_FUNC_STATIC( BIN2U );
HB_FUNC_EXTERN( BIN2L );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_STATIC( _INITVALUEBYTYPE );
HB_FUNC_STATIC( WITHREGISTRY );
HB_FUNC( ISREGISTRYKEY );
HB_FUNC( CREATEREGISTRYKEY );
HB_FUNC( GETREGISTRYVALUE );
HB_FUNC( SETREGISTRYVALUE );
HB_FUNC( DELETEREGISTRYVAR );
HB_FUNC( DELETEREGISTRYKEY );
HB_FUNC_EXTERN( REGDELETEKEY );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_REGISTRY )
{ "TREG32", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32 )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TREG32_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_NEW )}, NULL },
{ "TREG32_CREATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_CREATE )}, NULL },
{ "TREG32_GET", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_GET )}, NULL },
{ "TREG32_SET", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_SET )}, NULL },
{ "TREG32_DELETE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_DELETE )}, NULL },
{ "TREG32_SHOWERRORIF", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREG32_SHOWERRORIF )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REGCLOSEKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGCLOSEKEY )}, NULL },
{ "NHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "REGOPENKEYEXA", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGOPENKEYEXA )}, NULL },
{ "ISWOW64", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISWOW64 )}, NULL },
{ "_LERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SHOWERRORIF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CREGKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REGCREATEKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGCREATEKEY )}, NULL },
{ "_NERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NDISPOSITION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "REGQUERYVALUEEXA", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGQUERYVALUEEXA )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "NERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONVERTVALUEFROMREG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CONVERTVALUEFROMREG )}, NULL },
{ "_CONVERTVALUETOREG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CONVERTVALUETOREG )}, NULL },
{ "REGSETVALUEEXA", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGSETVALUEEXA )}, NULL },
{ "REGDELETEVALUEA", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGDELETEVALUEA )}, NULL },
{ "BIN2U", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BIN2U )}, NULL },
{ "BIN2L", {HB_FS_PUBLIC}, {HB_FUNCNAME( BIN2L )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "_INITVALUEBYTYPE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INITVALUEBYTYPE )}, NULL },
{ "WITHREGISTRY", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( WITHREGISTRY )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CLOSE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISREGISTRYKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISREGISTRYKEY )}, NULL },
{ "CREATEREGISTRYKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CREATEREGISTRYKEY )}, NULL },
{ "GETREGISTRYVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETREGISTRYVALUE )}, NULL },
{ "GET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETREGISTRYVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETREGISTRYVALUE )}, NULL },
{ "SET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELETEREGISTRYVAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DELETEREGISTRYVAR )}, NULL },
{ "DELETE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DELETEREGISTRYKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DELETEREGISTRYKEY )}, NULL },
{ "REGDELETEKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( REGDELETEKEY )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_REGISTRY, "h_registry.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_REGISTRY
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_REGISTRY )
   #include "hbiniseg.h"
#endif

HB_FUNC( TREG32 )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 62 );
	hb_xvmSetLine( 83 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TReg32", 6 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 85 );
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cRegKey", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nHandle", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 88 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nDisposition", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 89 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nError", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 90 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lError", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 93 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Create", 6 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Get", 3 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 95 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Set", 3 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 96 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Delete", 6 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 97 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ShowErrorIf", 11 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 98 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Close", 5 );
	{
		static const HB_BYTE codeblock[ 29 ] = {
			1, 0, 0, 0, 48, 14, 0, 95, 1, 112, 0, 28, 5, 100, 25, 14, 
			176, 15, 0, 48, 16, 0, 95, 1, 112, 0, 12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 100 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushVParams();
	if( hb_xvmMacroSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 128 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 319 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 63 );
lab00002: ;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 25 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 5 );
lab00003: ;
	hb_xvmSetLine( 140 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqualInt( 0L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 142 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 143 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "opening", 7 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 146 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_CREATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 177 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 184 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqualInt( 0L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 186 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 187 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "creating", 8 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushInteger( 319 );
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 63 );
lab00003: ;
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 192 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 196 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_SHOWERRORIF )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushStringConst( "Error ", 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " object (", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 225 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_GET )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 253 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 254 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 255 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 258 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 262 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmFunction( 6 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_SET )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 296 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 299 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 303 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 304 );
	hb_xvmLocalSetInt( 4, 4L );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 306 );
	hb_xvmLocalSetInt( 4, 1L );
	hb_xvmSetLine( 307 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 310 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 313 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TREG32_DELETE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 332 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 333 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 336 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( BIN2U )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 363 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLongLong( HB_LL( 4294967296 ) );
	if( hb_xvmPlus() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _CONVERTVALUEFROMREG )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 385 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 388 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".T.", 3 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmPushLocal( 1 );
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
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		hb_stackPop();
	}
	hb_xvmSetLine( 391 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _CONVERTVALUETOREG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 413 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 415 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( ".T.", 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( ".F.", 3 );
lab00004: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmPushLocal( 1 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "L", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		hb_stackPop();
	}
	hb_xvmSetLine( 418 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _INITVALUEBYTYPE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 438 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00004;
lab00001: ;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushDate( 0L );
	goto lab00004;
lab00002: ;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "", 0 );
lab00004: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( WITHREGISTRY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 462 );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 463 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 464 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00001: ;
	hb_xvmSetLine( 466 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 468 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ISREGISTRYKEY )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			1, 0, 0, 0, 48, 14, 0, 95, 1, 112, 0, 68, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CREATEREGISTRYKEY )
{
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 514 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 515 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 516 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 518 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETREGISTRYVALUE )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 543 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 544 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 547 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			1, 0, 2, 0, 3, 0, 4, 0, 48, 55, 0, 95, 1, 95, 255, 176, 
			48, 0, 95, 254, 12, 1, 112, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETREGISTRYVALUE )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 569 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 30 ] = {
			1, 0, 2, 0, 3, 0, 4, 0, 48, 57, 0, 95, 1, 95, 255, 95, 
			254, 112, 2, 73, 48, 38, 0, 95, 1, 112, 0, 121, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DELETEREGISTRYVAR )
{
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 593 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 26 ] = {
			1, 0, 1, 0, 3, 0, 48, 59, 0, 95, 1, 95, 255, 112, 1, 73, 
			48, 38, 0, 95, 1, 112, 0, 121, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DELETEREGISTRYKEY )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualInt( 0L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 62, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

#line 621 "h_registry.prg"

#include <mgdefs.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );
typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

/*
 * HB_FUNC_STATIC( ISWOW64 )
 *
 * Determines if the current process is running in a WOW64 environment (32-bit process on a 64-bit OS).
 *
 * Parameters:
 *   None
 *
 * Return Value:
 *   (logical): .T. if the process is running in a WOW64 environment, .F. otherwise.
 *
 * Purpose:
 *   This function checks if the current process is running in a WOW64 (Windows 32-bit on Windows 64-bit) environment.
 *   It uses the IsWow64Process API function to determine this.
 *
 * Notes:
 *   The IsWow64Process API function is only available on Windows XP SP2 and later.
 *   The wapi_GetProcAddress function is used to dynamically load the IsWow64Process API function.
 */
HB_FUNC_STATIC( ISWOW64 )
{
   BOOL bIsWow64 = FALSE;
   LPFN_ISWOW64PROCESS fnIsWow64Process;

   fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( GetModuleHandle( "kernel32" ), "IsWow64Process" );
   if( NULL != fnIsWow64Process )
   {
      fnIsWow64Process( GetCurrentProcess(), &bIsWow64 );
   }
   hb_retl( bIsWow64 );
}

