/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_socket.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( HTTPCONNECT );
HB_FUNC_EXTERN( LOWER );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( TURL );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( __MVPUBLIC );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( TIPCLIENTHTTP );
HB_FUNC( HTTPGETURL );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( HB_HKEYAT );
HB_FUNC_EXTERN( HB_HVALUEAT );
HB_FUNC_EXTERN( LEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_SOCKET )
{ "HTTPCONNECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTTPCONNECT )}, NULL },
{ "LOWER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWER )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TURL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TURL )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "__MVPUBLIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPUBLIC )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "TIPCLIENTHTTP", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIPCLIENTHTTP )}, NULL },
{ "OPEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HTTPGETURL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HTTPGETURL )}, NULL },
{ "CUSERID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OURL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPASSWORD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CSERVER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPORT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "READ", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "CREPLY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "HB_HKEYAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HKEYAT )}, NULL },
{ "HHEADERS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_HVALUEAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_HVALUEAT )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_SOCKET, "h_socket.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_SOCKET
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_SOCKET )
   #include "hbiniseg.h"
#endif

HB_FUNC( HTTPCONNECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 56 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "http://", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 57 );
	hb_xvmPushStringConst( "http://", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmLocalAdd( 2 );
lab00001: ;
	hb_xvmSetLine( 60 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ":", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroSymbol() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 67 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPop( 43 ) ) break;
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPop( 43 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPushPare( 43 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 72 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPop( 43 ) ) break;
lab00003: ;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 79 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 84 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
lab00006: ;
	hb_xvmSetLine( 90 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HTTPGETURL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 3 );
	hb_xvmSetLine( 101 );
	hb_xvmPushStringConst( "http://", 7 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 104 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( ":", 1 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "@", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00002: ;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 112 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( ":", 1 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 122 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 124 );
	hb_xvmPushStringConst( "<No data returned>", 18 );
	hb_xvmPopLocal( 5 );
lab00004: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 129 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 131 );
	hb_xvmPushStringConst( "<No header returned>", 20 );
	hb_xvmPopLocal( 6 );
lab00005: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ": ", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 135 );
	if( hb_xvmLocalIncPush( 8 ) ) break;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 7 );
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 147 );
	hb_xvmCopyLocals( 6, 7 );
	goto lab00011;
lab00009: ;
	hb_xvmSetLine( 152 );
	hb_xvmCopyLocals( 5, 7 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 158 );
	hb_xvmPushStringConst( "<Error opening URL>", 19 );
	hb_xvmPopLocal( 7 );
lab00011: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

