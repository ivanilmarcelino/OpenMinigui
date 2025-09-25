/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_hotkeybox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEHOTKEYBOX );
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
HB_FUNC_EXTERN( INITHOTKEYBOX );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETHOTKEYVALUE );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( _GETHOTKEYNAME );
HB_FUNC_EXTERN( C_GETHOTKEYNAME );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( AT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_HOTKEYBOX )
{ "_DEFINEHOTKEYBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEHOTKEYBOX )}, NULL },
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
{ "INITHOTKEYBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITHOTKEYBOX )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETHOTKEYVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETHOTKEYVALUE )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETHOTKEYNAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETHOTKEYNAME )}, NULL },
{ "C_GETHOTKEYNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_GETHOTKEYNAME )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_HOTKEYBOX, "h_hotkeybox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_HOTKEYBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_HOTKEYBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEHOTKEYBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 18 );
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 66 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 20 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLocalByRef( 18 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 73 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 74 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 78 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 79 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00003: ;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 93 );
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
lab00006: ;
	hb_xvmSetLine( 96 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 22 );
	hb_xvmSetLine( 98 );
	hb_xvmCopyLocals( 2, 21 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 9 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 104 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 20 );
lab00008: ;
	hb_xvmSetLine( 112 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00010: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "HOTKEYBOX", 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00012;
lab00011: ;
	hb_xvmPushInteger( -1 );
lab00012: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( -1 );
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00016;
lab00015: ;
	hb_xvmPushLogical( HB_TRUE );
lab00016: ;
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 171 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 172 );
	hb_xvmPushSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 5 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00017: ;
	hb_xvmSetLine( 175 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETHOTKEYNAME )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushStringConst( "\x00", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

