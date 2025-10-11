/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_spinner.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINESPINNER );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITSPINNER );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( ISTHEMED );
HB_FUNC_EXTERN( SENDMESSAGEWIDESTRING );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( SETSPINNERINCREMENT );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC( OSPINEVENTS );
HB_FUNC_EXTERN( _GETKEYSTATE );
HB_FUNC_EXTERN( INSERTSHIFTTAB );
HB_FUNC_EXTERN( INSERTVKEY );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( TRACKPOPUPMENU );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( HIWORD );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_SPINNER )
{ "_DEFINESPINNER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINESPINNER )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITSPINNER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITSPINNER )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "ISTHEMED", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISTHEMED )}, NULL },
{ "SENDMESSAGEWIDESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGEWIDESTRING )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "SETSPINNERINCREMENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSPINNERINCREMENT )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "OSPINEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OSPINEVENTS )}, NULL },
{ "_GETKEYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETKEYSTATE )}, NULL },
{ "INSERTSHIFTTAB", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSERTSHIFTTAB )}, NULL },
{ "INSERTVKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSERTVKEY )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "TRACKPOPUPMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRACKPOPUPMENU )}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_SPINNER, "h_spinner.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_SPINNER
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_SPINNER )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINESPINNER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 30 );
	hb_xvmSetLine( 62 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 37 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 32 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 85 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 86 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 91 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 92 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 93 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 94 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 2 );
lab00005: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00007: ;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 8 );
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
lab00008: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 35 );
	hb_xvmSetLine( 111 );
	hb_xvmCopyLocals( 2, 33 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 15 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 119 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 32 );
lab00010: ;
	hb_xvmSetLine( 127 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 34 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			1, 0, 2, 0, 11, 0, 33, 0, 176, 18, 0, 95, 1, 95, 255, 176, 
			19, 0, 95, 254, 12, 1, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushStringConst( "SPINNER", 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 165 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( -1 );
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 166 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( -1 );
lab00016: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00018;
lab00017: ;
	hb_xvmPushLogical( HB_TRUE );
lab00018: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 25 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 185 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 29 );
	if( hb_xvmDo( 4 ) ) break;
lab00019: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 1137 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 4 ) ) break;
lab00020: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmNotEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmDo( 2 ) ) break;
lab00021: ;
	hb_xvmSetLine( 197 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 198 );
	hb_xvmPushSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 37 );
lab00022: ;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 207 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OSPINEVENTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 215 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 219 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 241L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	{
		static const HB_BYTE codeblock[ 34 ] = {
			1, 0, 1, 0, 1, 0, 176, 38, 0, 95, 1, 12, 1, 28, 15, 176, 
			37, 0, 95, 1, 95, 255, 12, 2, 121, 15, 25, 7, 95, 1, 95, 255, 
			8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 237 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 122, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 241 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 243 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 246 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 87L ) ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 250 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmPushLocal( 2 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 135L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 123L )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00006: ;
	hb_xvmSetLine( 257 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

