/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "TCursor.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TCURSOR );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TCURSOR_NEW );
HB_FUNC_EXTERN( DESTROYCURSOR );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( LOADCURSOR );
HB_FUNC_EXTERN( GETCURSORHAND );
HB_FUNC_EXTERN( GETCURSORSTOP );
HB_FUNC_EXTERN( GETCURSORDRAG );
HB_FUNC_EXTERN( GETCURSORCATCH );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC_EXTERN( GETINSTANCE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TCURSOR )
{ "TCURSOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCURSOR )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCURSOR_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCURSOR_NEW )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HCURSOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LPREDEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESTROYCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYCURSOR )}, NULL },
{ "_HCURSOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "LOADCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOADCURSOR )}, NULL },
{ "_LPREDEF", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETCURSORHAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORHAND )}, NULL },
{ "GETCURSORSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORSTOP )}, NULL },
{ "GETCURSORDRAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORDRAG )}, NULL },
{ "GETCURSORCATCH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORCATCH )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETINSTANCE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETINSTANCE )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TCURSOR, "TCursor.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TCURSOR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TCURSOR )
   #include "hbiniseg.h"
#endif

HB_FUNC( TCURSOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 29 );
	hb_xvmSetLine( 8 );
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
	hb_xvmPushStringConst( "TCursor", 7 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 11 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hCursor", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 12 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lPredef", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 14 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 8L ) ) break;
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 18 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "End", 3 );
	{
		static const HB_BYTE codeblock[ 45 ] = {
			1, 0, 0, 0, 48, 9, 0, 95, 1, 112, 0, 121, 69, 28, 23, 48, 
			10, 0, 95, 1, 112, 0, 31, 14, 176, 11, 0, 48, 9, 0, 95, 1, 
			112, 0, 20, 1, 48, 12, 0, 95, 1, 121, 112, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 20 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 17 );
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
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCURSOR_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 28 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 29 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 33 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStringConst( "ARROW", 5 );
	hb_xvmPushStringConst( "IBEAM", 5 );
	hb_xvmPushStringConst( "WAIT", 4 );
	hb_xvmPushStringConst( "CROSS", 5 );
	hb_xvmPushStringConst( "UPARROW", 7 );
	hb_xvmPushStringConst( "SIZENWSE", 8 );
	hb_xvmPushStringConst( "SIZENESW", 8 );
	hb_xvmPushStringConst( "SIZEWE", 6 );
	hb_xvmPushStringConst( "SIZENS", 6 );
	hb_xvmArrayGen( 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 38 );
	hb_xvmPushInteger( 32512 );
	hb_xvmPushInteger( 32513 );
	hb_xvmPushInteger( 32514 );
	hb_xvmPushInteger( 32515 );
	hb_xvmPushInteger( 32516 );
	hb_xvmPushInteger( 32642 );
	hb_xvmPushInteger( 32643 );
	hb_xvmPushInteger( 32644 );
	hb_xvmPushInteger( 32645 );
	hb_xvmArrayGen( 9 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 40 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 42 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 44 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HAND", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 45 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 46 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 47 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "STOP", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 48 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 49 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 50 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DRAG", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 51 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 52 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 53 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CATCH", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 54 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 55 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 57 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushStringConst( "Wrong predefined cursor type!", 29 );
	hb_xvmPushStringConst( "Alert", 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 61 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00008: ;
	hb_xvmSetLine( 65 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 29, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

