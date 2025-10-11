/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_error.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( CLIPINIT );
HB_FUNC_EXTERN( OS_ISWIN95 );
HB_FUNC_EXTERN( OS_ISWIN98 );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( HB_ARGV );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( EXITPROCESS );
HB_FUNC_EXTERN( INIT );
HB_FUNC_EXIT( CLIPEXIT );
HB_FUNC( HB_GTSYS );
HB_FUNC( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( ERRORBLOCK );
HB_FUNC_STATIC( HMG_GENERROR );
HB_FUNC_EXTERN( ERRORNEW );
HB_FUNC( MINIGUIVERSION );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( HB_VERSION );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HMG_CHARSETNAME );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( HB_GT_GUI_DEFAULT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ERROR )
{ "CLIPINIT$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( CLIPINIT )}, NULL },
{ "OS_ISWIN95", {HB_FS_PUBLIC}, {HB_FUNCNAME( OS_ISWIN95 )}, NULL },
{ "OS_ISWIN98", {HB_FS_PUBLIC}, {HB_FUNCNAME( OS_ISWIN98 )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "HB_ARGV", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ARGV )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "EXITPROCESS", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXITPROCESS )}, NULL },
{ "INIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INIT )}, NULL },
{ "CLIPEXIT$", {HB_FS_EXIT | HB_FS_LOCAL}, {HB_EXIT_FUNCNAME( CLIPEXIT )}, NULL },
{ "HB_GTSYS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HB_GTSYS )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ERRORBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORBLOCK )}, NULL },
{ "HMG_GENERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_GENERROR )}, NULL },
{ "ERRORNEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERRORNEW )}, NULL },
{ "_SUBSYSTEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SUBCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SEVERITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OPERATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MINIGUIVERSION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MINIGUIVERSION )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "HB_VERSION", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VERSION )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "HMG_CHARSETNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_CHARSETNAME )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "HB_GT_GUI_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_GT_GUI_DEFAULT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ERROR, "h_error.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ERROR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ERROR )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( CLIPINIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "The ", 4 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " file", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "expects a newer version of Windows.", 35 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Upgrade your Windows version.", 29 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Error Starting Program", 22 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 82 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_EXIT( CLIPEXIT )
{
   do {
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 95 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HB_GTSYS )
{
   do {
	hb_xvmSetLine( 109 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MSGMINIGUIERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( " Program terminated.", 20 );
	if( hb_xvmPlusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( HMG_GENERROR )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 164 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MGERROR", 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 165 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 166 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 167 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 168 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( MINIGUIVERSION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 199 );
	hb_xvmPushStringConst( "Harbour MiniGUI Extended Edition 25.10 (", 40 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "-bit) ", 6 );
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( " (DEBUG)", 8 );
	if( hb_xvmPlusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 38 );
	hb_xvmPushInteger( 15 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

