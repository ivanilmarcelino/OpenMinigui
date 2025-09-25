/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_progressbar.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEPROGRESSBAR );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGPROGRESSBAR );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITPROGRESSBAR );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( CHANGESTYLE );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETWINDOWTHEME );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( LEN );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_PROGRESSBAR )
{ "_DEFINEPROGRESSBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEPROGRESSBAR )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGPROGRESSBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGPROGRESSBAR )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITPROGRESSBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITPROGRESSBAR )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "CHANGESTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHANGESTYLE )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETWINDOWTHEME", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTHEME )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_PROGRESSBAR, "h_progressbar.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_PROGRESSBAR
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_PROGRESSBAR )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEPROGRESSBAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 20 );
	hb_xvmSetLine( 67 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 28 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 120 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 25 );
lab00002: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 25 );
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 120 );
lab00004: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushInteger( 40 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00005: ;
	hb_xvmSetLine( 83 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00007;
lab00006: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00007: ;
	hb_xvmPopLocal( 2 );
lab00008: ;
	hb_xvmSetLine( 86 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 87 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 89 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00009: ;
	hb_xvmSetLine( 91 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00011;
lab00010: ;
	hb_xvmPushLocal( 2 );
lab00011: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00012: ;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00013: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 5 );
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
	hb_xvmSetLine( 105 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 24 );
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 108 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 110 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 112 );
	hb_xvmLocalSetInt( 26, 1073741824L );
	hb_xvmSetLine( 113 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocalByRef( 26 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00015: ;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 118 );
	if( hb_xvmLocalAddInt( 26, 4 ) ) break;
lab00016: ;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 122 );
	if( hb_xvmLocalAddInt( 26, 1 ) ) break;
lab00017: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 128 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 10, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushStringConst( "msctls_progress32", 17 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00020;
lab00018: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 12 ) ) break;
	hb_xvmPopLocal( 22 );
lab00020: ;
	hb_xvmSetLine( 152 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1026 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 157 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1034 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmPushInteger( 1 );
	goto lab00022;
lab00021: ;
	hb_xvmPushInteger( 0 );
lab00022: ;
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 4 ) ) break;
lab00023: ;
	hb_xvmSetLine( 163 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
lab00024: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 165 );
	hb_xvmPushInteger( 201 );
	hb_xvmPushInteger( 201 );
	hb_xvmPushInteger( 201 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 15 );
lab00025: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00026: ;
	hb_xvmSetLine( 170 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmDo( 2 ) ) break;
lab00027: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00028: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushStringConst( "PROGRESSBAR", 11 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 187 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 192 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00029;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00030;
lab00029: ;
	hb_xvmPushInteger( -1 );
lab00030: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00031;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00032;
lab00031: ;
	hb_xvmPushInteger( -1 );
lab00032: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 213 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00034;
lab00033: ;
	hb_xvmPushLogical( HB_TRUE );
lab00034: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00036;
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 8193 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00035: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1033 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00036: ;
	hb_xvmSetLine( 237 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 238 );
	hb_xvmPushSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 240 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 28 );
lab00037: ;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 247 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGPROGRESSBAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 255 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 256 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 258 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1026 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 8193 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1033 );
	hb_xvmPushInteger( 0 );
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
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 271 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00004: ;
	hb_xvmSetLine( 274 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

