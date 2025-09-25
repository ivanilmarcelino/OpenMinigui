/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_slider.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINESLIDER );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( LEN );
HB_FUNC( INITDIALOGSLIDER );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITSLIDER );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( VALTYPE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_SLIDER )
{ "_DEFINESLIDER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINESLIDER )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "INITDIALOGSLIDER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGSLIDER )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITSLIDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITSLIDER )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_SLIDER, "h_slider.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_SLIDER
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_SLIDER )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINESLIDER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 26 );
	hb_xvmSetLine( 62 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 34 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 5 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	if( hb_xvmPlus() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 120 );
lab00004: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 120 );
	goto lab00008;
lab00005: ;
	hb_xvmPushInteger( 35 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushInteger( 5 );
	goto lab00007;
lab00006: ;
	hb_xvmPushInteger( 0 );
lab00007: ;
	if( hb_xvmPlus() ) break;
lab00008: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
lab00009: ;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00011;
lab00010: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00011: ;
	hb_xvmPopLocal( 2 );
lab00012: ;
	hb_xvmSetLine( 87 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 88 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 90 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00013: ;
	hb_xvmSetLine( 92 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00015;
lab00014: ;
	hb_xvmPushLocal( 2 );
lab00015: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00016: ;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00017: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 7 );
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
lab00018: ;
	hb_xvmSetLine( 106 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 30 );
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 109 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 111 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 113 );
	hb_xvmLocalSetInt( 32, 1073741824L );
	hb_xvmSetLine( 115 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocalByRef( 32 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00019: ;
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 120 );
	hb_xvmPushLocalByRef( 32 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 124 );
	if( hb_xvmLocalAddInt( 32, 2 ) ) break;
lab00021: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 128 );
	if( hb_xvmLocalAddInt( 32, 1 ) ) break;
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 130 );
	if( hb_xvmLocalAddInt( 32, 16 ) ) break;
lab00023: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 134 );
	if( hb_xvmLocalAddInt( 32, 8 ) ) break;
lab00024: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 137 );
	if( hb_xvmLocalAddInt( 32, 4 ) ) break;
lab00025: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 140 );
	if( hb_xvmLocalAddInt( 32, 4 ) ) break;
lab00026: ;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 144 );
	if( hb_xvmLocalAddInt( 32, 32 ) ) break;
lab00027: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmSetLine( 151 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 13, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushStringConst( "msctls_trackbar32", 17 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00028: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 18 ) ) break;
	hb_xvmPopLocal( 28 );
lab00030: ;
	hb_xvmSetLine( 173 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmSetLine( 174 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 28 );
	if( hb_xvmDo( 2 ) ) break;
lab00031: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 1029 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00032: ;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushStringConst( "SLIDER", 6 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 57L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 213 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushInteger( -1 );
lab00034: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 215 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00035;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( -1 );
lab00036: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00038;
lab00037: ;
	hb_xvmPushLogical( HB_TRUE );
lab00038: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	goto lab00040;
lab00039: ;
	hb_xvmPushStringConst( "", 0 );
lab00040: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00042;
lab00041: ;
	hb_xvmPushStringConst( "", 0 );
lab00042: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 234 );
	hb_xvmPushSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 34 );
lab00043: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 243 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGSLIDER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1029 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 254 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 257 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

