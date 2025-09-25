/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_rating.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINERATING );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( _INITRATING );
HB_FUNC_EXTERN( AADD );
HB_FUNC( _RELEASERATING );
HB_FUNC_EXTERN( GETCONTROLID );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( _RELEASECONTROL );
HB_FUNC_EXTERN( ERASEWINDOW );
HB_FUNC_STATIC( ONHOVERRATE );
HB_FUNC_STATIC( ONLEAVERATE );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( RAT );
HB_FUNC_STATIC( ONSELECTRATE );
HB_FUNC_EXTERN( _DEFINEIMAGE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( DRAWRECT );
HB_FUNC_EXTERN( _GETID );
HB_FUNC( CLEARRATING );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC( REFRESHRATING );
HB_FUNC_EXTERN( _GETCONTROLACTION );
HB_FUNC( TOGGLERATINGREADONLY );
HB_FUNC_EXTERN( HB_ISARRAY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_RATING )
{ "_DEFINERATING", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINERATING )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_INITRATING", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INITRATING )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_RELEASERATING", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASERATING )}, NULL },
{ "GETCONTROLID", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLID )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "_RELEASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _RELEASECONTROL )}, NULL },
{ "ERASEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ERASEWINDOW )}, NULL },
{ "ONHOVERRATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONHOVERRATE )}, NULL },
{ "ONLEAVERATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONLEAVERATE )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "RAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RAT )}, NULL },
{ "ONSELECTRATE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ONSELECTRATE )}, NULL },
{ "_DEFINEIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEIMAGE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "DRAWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DRAWRECT )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "CLEARRATING", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLEARRATING )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "REFRESHRATING", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( REFRESHRATING )}, NULL },
{ "_GETCONTROLACTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLACTION )}, NULL },
{ "TOGGLERATINGREADONLY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TOGGLERATINGREADONLY )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_RATING, "h_rating.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_RATING
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_RATING )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINERATING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 17 );
	hb_xvmSetLine( 15 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 6, 20L );
lab00001: ;
	hb_xvmSetLine( 16 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmLocalSetInt( 5, 100L );
lab00002: ;
	hb_xvmSetLine( 17 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmLocalSetInt( 7, 0L );
lab00003: ;
	hb_xvmSetLine( 18 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "empty.png", 9 );
	hb_xvmPushStringConst( "full.png", 8 );
	hb_xvmArrayGen( 2 );
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 8 );
lab00005: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 19 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmLocalSetInt( 9, 5L );
lab00006: ;
	hb_xvmSetLine( 20 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 13 );
lab00007: ;
	hb_xvmSetLine( 21 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 15 );
lab00008: ;
	hb_xvmSetLine( 22 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 16 );
lab00009: ;
	hb_xvmSetLine( 23 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 17 );
lab00010: ;
	hb_xvmSetLine( 25 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 26 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00011: ;
	hb_xvmSetLine( 29 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 30 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 31 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 32 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00012: ;
	hb_xvmSetLine( 35 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 36 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 39 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 40 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 43 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 44 );
	hb_xvmPushStringConst( "_empty", 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 45 );
	hb_xvmPushStringConst( "_full", 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00015: ;
	hb_xvmSetLine( 48 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 19 );
	hb_xvmSetLine( 50 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 54 );
	hb_xvmCopyLocals( 2, 18 );
	hb_xvmSetLine( 56 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 58 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 18 );
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
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 16 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 60 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushStringConst( "RATING", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00018;
lab00017: ;
	hb_xvmPushInteger( -1 );
lab00018: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00020;
lab00019: ;
	hb_xvmPushInteger( -1 );
lab00020: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00022;
lab00021: ;
	hb_xvmPushLogical( HB_TRUE );
lab00022: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _RELEASERATING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 117 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 117 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00002: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 127 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _INITRATING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 16 );
	hb_xvmSetLine( 133 );
	hb_xvmCopyLocals( 3, 19 );
	hb_xvmCopyLocals( 4, 20 );
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 10 );
lab00002: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 137 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 17 );
	goto lab00007;
lab00003: ;
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 18 );
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 311L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 310L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 352L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmSetLine( 147 );
	{
		static const HB_BYTE codeblock[ 27 ] = {
			0, 0, 3, 0, 14, 0, 1, 0, 2, 0, 95, 255, 28, 5, 100, 25, 
			11, 176, 16, 0, 95, 254, 95, 253, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmSetLine( 148 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			0, 0, 4, 0, 14, 0, 1, 0, 2, 0, 12, 0, 95, 255, 28, 5, 
			100, 25, 13, 176, 17, 0, 95, 254, 95, 253, 95, 252, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmSetLine( 151 );
	{
		static const HB_BYTE codeblock[ 191 ] = {
			0, 0, 4, 0, 14, 0, 1, 0, 2, 0, 12, 0, 95, 255, 28, 6, 
			100, 26, 173, 0, 176, 18, 0, 95, 254, 95, 253, 106, 6, 86, 97, 108, 
			117, 101, 0, 176, 19, 0, 176, 20, 0, 98, 1, 0, 93, 218, 0, 1, 
			106, 2, 67, 0, 8, 28, 30, 176, 21, 0, 98, 1, 0, 93, 254, 0, 
			1, 98, 1, 0, 93, 255, 0, 1, 106, 5, 78, 97, 109, 101, 0, 12, 
			3, 25, 21, 176, 21, 0, 98, 1, 0, 93, 254, 0, 1, 106, 5, 78, 
			97, 109, 101, 0, 12, 2, 176, 22, 0, 106, 2, 95, 0, 98, 1, 0, 
			93, 218, 0, 1, 106, 2, 67, 0, 8, 28, 30, 176, 21, 0, 98, 1, 
			0, 93, 254, 0, 1, 98, 1, 0, 93, 255, 0, 1, 106, 5, 78, 97, 
			109, 101, 0, 12, 3, 25, 21, 176, 21, 0, 98, 1, 0, 93, 254, 0, 
			1, 106, 5, 78, 97, 109, 101, 0, 12, 2, 12, 2, 23, 12, 2, 12, 
			1, 20, 4, 176, 23, 0, 95, 254, 95, 253, 95, 252, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 311L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 310L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
lab00006: ;
	hb_xvmSetLine( 137 );
	if( hb_xvmLocalIncPush( 17 ) ) break;
lab00007: ;
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 167 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 168 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushStringConst( "Win_1", 5 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 8 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushStringConst( "Win_1", 5 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 8 ) ) break;
lab00009: ;
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 3 ) ) break;
lab00010: ;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmDo( 0 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ONHOVERRATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
lab00004: ;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Cargo", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 196 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00006: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 201 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ONLEAVERATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 208 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 211 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
lab00002: ;
	hb_xvmSetLine( 217 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ONSELECTRATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "Cargo", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 227 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00002: ;
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 232 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 236 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLEARRATING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 244 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 245 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Picture", 7 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Cargo", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 244 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00002: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 249 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( REFRESHRATING )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ONCHANGE", 8 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TOGGLERATINGREADONLY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 265 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 267 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 268 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONGOTFOCUS", 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONLOSTFOCUS", 11 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONGOTFOCUS", 10 );
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONLOSTFOCUS", 11 );
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 267 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00003: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	goto lab00008;
lab00004: ;
	hb_xvmSetLine( 277 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONGOTFOCUS", 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "ONLOSTFOCUS", 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 277 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00007: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00008: ;
	hb_xvmSetLine( 286 );
	/* *** END PROC *** */
   } while( 0 );
}

