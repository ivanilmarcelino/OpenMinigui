/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "fncMyError.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( MYERRORFUNC );
HB_FUNC_STATIC( MYERRORMESSAGE );
HB_FUNC_EXTERN( HTML_ERRORLOG );
HB_FUNC_EXTERN( HTML_LINETEXT );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( DATE );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( HB_EOL );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( HTML_LINE );
HB_FUNC_EXTERN( HTML_END );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( LTRIM );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_FNCMYERROR )
{ "MYERRORFUNC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MYERRORFUNC )}, NULL },
{ "MYERRORMESSAGE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( MYERRORMESSAGE )}, NULL },
{ "HTML_ERRORLOG", {HB_FS_PUBLIC}, {HB_FUNCNAME( HTML_ERRORLOG )}, NULL },
{ "HTML_LINETEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HTML_LINETEXT )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "DATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DATE )}, NULL },
{ "TIME", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIME )}, NULL },
{ "HB_EOL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EOL )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "HTML_LINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HTML_LINE )}, NULL },
{ "HTML_END", {HB_FS_PUBLIC}, {HB_FUNCNAME( HTML_END )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "SEVERITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "SUBSYSTEM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "SUBCODE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FILENAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OPERATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_FNCMYERROR, "fncMyError.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_FNCMYERROR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_FNCMYERROR )
   #include "hbiniseg.h"
#endif

HB_FUNC( MYERRORFUNC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 1 );
	hb_xvmSetLine( 13 );
	hb_xvmLocalSetInt( 5, 2L );
	hb_xvmSetLine( 15 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 17 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 18 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "<p class=\"updated\">Date: ", 25 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "  ", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Time: ", 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 19 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "</p>", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 21 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00001: ;
	hb_xvmSetLine( 23 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 24 );
	hb_xvmPushStringConst( "Called from ", 12 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmLocalInc( 5 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 25 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 26 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 28 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 29 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 31 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Program Error...", 16 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 33 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( MYERRORMESSAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 41 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushStringConst( "Error", 5 );
	goto lab00002;
lab00001: ;
	hb_xvmPushStringConst( "Warning", 7 );
lab00002: ;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmLocalAdd( 2 );
	hb_xvmSetLine( 44 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 45 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 47 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "\?\?\?", 3 );
	if( hb_xvmPlusEqPop() ) break;
lab00004: ;
	hb_xvmSetLine( 51 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 52 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "/", 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 54 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "/\?\?\?", 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00006: ;
	hb_xvmSetLine( 58 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 59 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "  ", 2 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 65 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( ": ", 2 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 67 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( ": ", 2 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00009: ;
	hb_xvmSetLine( 70 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

