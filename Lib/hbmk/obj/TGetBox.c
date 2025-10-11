/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "TGetBox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TGETBOX );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( TCONTROL );
HB_FUNC_STATIC( TGETBOX_NEW );
HB_FUNC_STATIC( TGETBOX_HANDLEEVENT );
HB_FUNC_STATIC( TGETBOX_KEYCHAR );
HB_FUNC_STATIC( TGETBOX_KEYDOWN );
HB_FUNC_STATIC( TGETBOX_LOSTFOCUS );
HB_FUNC_STATIC( TGETBOX_LVALID );
HB_FUNC_STATIC( TGETBOX_VARGET );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _DEFINEGETBOX );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _GETKEYSTATE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TGETBOX )
{ "TGETBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "TCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TCONTROL )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TGETBOX_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_NEW )}, NULL },
{ "TGETBOX_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_HANDLEEVENT )}, NULL },
{ "TGETBOX_KEYCHAR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_KEYCHAR )}, NULL },
{ "TGETBOX_KEYDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_KEYDOWN )}, NULL },
{ "TGETBOX_LOSTFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_LOSTFOCUS )}, NULL },
{ "TGETBOX_LVALID", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_LVALID )}, NULL },
{ "TGETBOX_VARGET", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TGETBOX_VARGET )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "_NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNEWID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LUPDATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BWHEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LFOCUSED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHELPID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "LOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GOTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_DEFINEGETBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEGETBOX )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_ATX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "ADDCONTROL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETKEYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETKEYSTATE )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "CHR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHR )}, NULL },
{ "KEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETTEXT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TGETBOX, "TGetBox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TGETBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TGETBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( TGETBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 83 );
	hb_xvmSetLine( 9 );
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
	hb_xvmPushStringConst( "TGetBox", 7 );
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
	hb_xvmPushStringConst( "Atx", 3 );
	hb_xvmPushStringConst( "lAppend", 7 );
	hb_xvmPushStringConst( "oGet", 4 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 16 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 17 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 18 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyChar", 7 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 19 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyDown", 7 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 20 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LostFocus", 9 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 21 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "lValid", 6 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 22 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VarGet", 6 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 24 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Index", 5 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 15, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 25 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Handle", 6 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 16, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 27 );
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

HB_FUNC_STATIC( TGETBOX_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 24, 27 );
	hb_xvmSetLine( 38 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 39 );
	if( hb_xvmPushMemvar( symbols + 23 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 40 );
	if( hb_xvmPushMemvar( symbols + 23 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 42 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 45 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 37 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 38 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 39 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 40 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 41 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 42 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 44 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 46 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 47 );
	hb_xvmSetLine( 48 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 50 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 51 );
	hb_xvmSetLine( 57 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 9 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 10 );
lab00004: ;
	hb_xvmPopLocal( 10 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 15 );
lab00006: ;
	hb_xvmPopLocal( 15 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 17 );
lab00008: ;
	hb_xvmPopLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 18 );
lab00010: ;
	hb_xvmPopLocal( 18 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 22 );
lab00012: ;
	hb_xvmPopLocal( 22 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 20 );
lab00014: ;
	hb_xvmPopLocal( 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	{
		static const HB_BYTE codeblock[ 25 ] = {
			1, 0, 1, 0, 28, 0, 176, 25, 0, 12, 0, 121, 8, 28, 6, 95, 
			255, 25, 7, 95, 1, 165, 80, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 3 );
lab00016: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 64 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 65 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 66 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 67 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 69 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 70 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00017: ;
	hb_xvmSetLine( 74 );
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 75 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 76 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 77 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 78 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 80 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 82 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 16 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 85 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 19 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 21 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 91 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 45 );
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 48 );
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 49 );
	hb_xvmSetLine( 97 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 98 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 101 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 7 );
lab00018: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 109 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 47 );
	hb_xvmPushLocal( 51 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 50 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 43 );
	hb_xvmPushLocal( 49 );
	hb_xvmPushLocal( 48 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 37 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 112 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmPushMemvar( symbols + 23 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 115 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00019: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00020: ;
	hb_xvmSetLine( 126 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_HANDLEEVENT )
{
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 134 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_KEYCHAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "W", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 9 );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 147 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 150 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_KEYDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 158 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 161 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 165 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_LVALID )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 173 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 176 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_LOSTFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 187 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 189 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 193 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TGETBOX_VARGET )
{
   do {
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 83, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

