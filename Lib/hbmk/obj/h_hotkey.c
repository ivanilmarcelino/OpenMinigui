/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_hotkey.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEHOTKEY );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( _GETWINDOWPROP );
HB_FUNC_EXTERN( HB_ISNIL );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC( _GETHOTKEYBLOCK );
HB_FUNC( _RELEASEHOTKEY );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( INITHOTKEY );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _ERASECONTROL );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC( _PUSHKEY );
HB_FUNC_EXTERN( KEYBD_EVENT );
HB_FUNC( HMG_PRESSKEY );
HB_FUNC_EXTERN( PVALUE );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( LEN );
HB_FUNC( _SETHOTKEYBYNAME );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC( _DETERMINEKEY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( SUBSTR );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_HOTKEY )
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "_GETWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETWINDOWPROP )}, NULL },
{ "HB_ISNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNIL )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "_GETHOTKEYBLOCK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETHOTKEYBLOCK )}, NULL },
{ "_RELEASEHOTKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _RELEASEHOTKEY )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "INITHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITHOTKEY )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ERASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ERASECONTROL )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "_PUSHKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PUSHKEY )}, NULL },
{ "KEYBD_EVENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( KEYBD_EVENT )}, NULL },
{ "HMG_PRESSKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_PRESSKEY )}, NULL },
{ "PVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PVALUE )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_SETHOTKEYBYNAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETHOTKEYBYNAME )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "_DETERMINEKEY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DETERMINEKEY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_HOTKEY, "h_hotkey.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_HOTKEY
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_HOTKEY )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEHOTKEY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 58 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 59 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 60 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 61 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 63 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 65 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 66 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
lab00003: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "ON KEY: Parent Window is Not specified.", 39 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Window ", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
lab00006: ;
	hb_xvmSetLine( 82 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 84 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00008: ;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 11 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 49151 );
#else
	hb_xvmPushLong( 49151L );
#endif
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 98 );
	hb_xvmPushStringConst( "HOTKEY", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 112 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 114 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 8 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _RELEASEHOTKEY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 148 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "HOTKEY", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 152 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 154 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 156 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETHOTKEYBLOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 162 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 166 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 168 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "HOTKEY", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 169 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 170 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 172 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _PUSHKEY )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 182 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_PRESSKEY )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSetLine( 188 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 192 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "HMG_PressKey: Invalid parameter.", 32 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 192 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 202 );
	if( hb_xvmLocalAddInt( 3, -1 ) ) break;
	hb_xvmPushLocal( 3 );
lab00006: ;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETHOTKEYBYNAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 212 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 214 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 215 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "ON KEY: Parent Window is Not specified.", 39 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 223 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "The hotkey ", 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is Already defined.", 20 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "The hotkey ", 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not valid.", 14 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 233 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DETERMINEKEY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 1 );
	hb_xvmSetLine( 238 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 244 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_aKeyTables", 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_aKeyTables", 15 );
	hb_xvmPushStringConst( "LBUTTON", 7 );
	hb_xvmPushStringConst( "RBUTTON", 7 );
	hb_xvmPushStringConst( "CANCEL", 6 );
	hb_xvmPushStringConst( "MBUTTON", 7 );
	hb_xvmPushStringConst( "XBUTTON1", 8 );
	hb_xvmPushStringConst( "XBUTTON2", 8 );
	hb_xvmPushStringConst( ".7", 2 );
	hb_xvmPushStringConst( "BACK", 4 );
	hb_xvmPushStringConst( "TAB", 3 );
	hb_xvmPushStringConst( ".10", 3 );
	hb_xvmPushStringConst( ".11", 3 );
	hb_xvmPushStringConst( "CLEAR", 5 );
	hb_xvmPushStringConst( "RETURN", 6 );
	hb_xvmPushStringConst( ".14", 3 );
	hb_xvmPushStringConst( ".15", 3 );
	hb_xvmPushStringConst( "SHIFT", 5 );
	hb_xvmPushStringConst( "CONTROL", 7 );
	hb_xvmPushStringConst( "MENU", 4 );
	hb_xvmPushStringConst( "PAUSE", 5 );
	hb_xvmPushStringConst( "CAPITAL", 7 );
	hb_xvmPushStringConst( "KANA", 4 );
	hb_xvmPushStringConst( ".22", 3 );
	hb_xvmPushStringConst( "JUNJA", 5 );
	hb_xvmPushStringConst( "FINAL", 5 );
	hb_xvmPushStringConst( "HANJA", 5 );
	hb_xvmPushStringConst( ".26", 3 );
	hb_xvmPushStringConst( "ESCAPE", 6 );
	hb_xvmPushStringConst( "CONVERT", 7 );
	hb_xvmPushStringConst( "NONCONVERT", 10 );
	hb_xvmPushStringConst( "ACCEPT", 6 );
	hb_xvmPushStringConst( "MODECHANGE", 10 );
	hb_xvmPushStringConst( "SPACE", 5 );
	hb_xvmPushStringConst( "PRIOR", 5 );
	hb_xvmPushStringConst( "NEXT", 4 );
	hb_xvmPushStringConst( "END", 3 );
	hb_xvmPushStringConst( "HOME", 4 );
	hb_xvmPushStringConst( "LEFT", 4 );
	hb_xvmPushStringConst( "UP", 2 );
	hb_xvmPushStringConst( "RIGHT", 5 );
	hb_xvmPushStringConst( "DOWN", 4 );
	hb_xvmPushStringConst( "SELECT", 6 );
	hb_xvmPushStringConst( "PRINT", 5 );
	hb_xvmPushStringConst( "EXECUTE", 7 );
	hb_xvmPushStringConst( "SNAPSHOT", 8 );
	hb_xvmPushStringConst( "INSERT", 6 );
	hb_xvmPushStringConst( "DELETE", 6 );
	hb_xvmPushStringConst( "HELP", 4 );
	hb_xvmPushStringConst( "0", 1 );
	hb_xvmPushStringConst( "1", 1 );
	hb_xvmPushStringConst( "2", 1 );
	hb_xvmPushStringConst( "3", 1 );
	hb_xvmPushStringConst( "4", 1 );
	hb_xvmPushStringConst( "5", 1 );
	hb_xvmPushStringConst( "6", 1 );
	hb_xvmPushStringConst( "7", 1 );
	hb_xvmPushStringConst( "8", 1 );
	hb_xvmPushStringConst( "9", 1 );
	hb_xvmPushStringConst( ".58", 3 );
	hb_xvmPushStringConst( ".59", 3 );
	hb_xvmPushStringConst( ".60", 3 );
	hb_xvmPushStringConst( ".61", 3 );
	hb_xvmPushStringConst( ".62", 3 );
	hb_xvmPushStringConst( ".63", 3 );
	hb_xvmPushStringConst( ".64", 3 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmPushStringConst( "B", 1 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmPushStringConst( "E", 1 );
	hb_xvmPushStringConst( "F", 1 );
	hb_xvmPushStringConst( "G", 1 );
	hb_xvmPushStringConst( "H", 1 );
	hb_xvmPushStringConst( "I", 1 );
	hb_xvmPushStringConst( "J", 1 );
	hb_xvmPushStringConst( "K", 1 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmPushStringConst( "N", 1 );
	hb_xvmPushStringConst( "O", 1 );
	hb_xvmPushStringConst( "P", 1 );
	hb_xvmPushStringConst( "Q", 1 );
	hb_xvmPushStringConst( "R", 1 );
	hb_xvmPushStringConst( "S", 1 );
	hb_xvmPushStringConst( "T", 1 );
	hb_xvmPushStringConst( "U", 1 );
	hb_xvmPushStringConst( "V", 1 );
	hb_xvmPushStringConst( "W", 1 );
	hb_xvmPushStringConst( "X", 1 );
	hb_xvmPushStringConst( "Y", 1 );
	hb_xvmPushStringConst( "Z", 1 );
	hb_xvmPushStringConst( "LWIN", 4 );
	hb_xvmPushStringConst( "RWIN", 4 );
	hb_xvmPushStringConst( "APPS", 4 );
	hb_xvmPushStringConst( ".94", 3 );
	hb_xvmPushStringConst( "SLEEP", 5 );
	hb_xvmPushStringConst( "NUMPAD0", 7 );
	hb_xvmPushStringConst( "NUMPAD1", 7 );
	hb_xvmPushStringConst( "NUMPAD2", 7 );
	hb_xvmPushStringConst( "NUMPAD3", 7 );
	hb_xvmPushStringConst( "NUMPAD4", 7 );
	hb_xvmPushStringConst( "NUMPAD5", 7 );
	hb_xvmPushStringConst( "NUMPAD6", 7 );
	hb_xvmPushStringConst( "NUMPAD7", 7 );
	hb_xvmPushStringConst( "NUMPAD8", 7 );
	hb_xvmPushStringConst( "NUMPAD9", 7 );
	hb_xvmPushStringConst( "MULTIPLY", 8 );
	hb_xvmPushStringConst( "ADD", 3 );
	hb_xvmPushStringConst( "SEPARATOR", 9 );
	hb_xvmPushStringConst( "SUBTRACT", 8 );
	hb_xvmPushStringConst( "DECIMAL", 7 );
	hb_xvmPushStringConst( "DIVIDE", 6 );
	hb_xvmPushStringConst( "F1", 2 );
	hb_xvmPushStringConst( "F2", 2 );
	hb_xvmPushStringConst( "F3", 2 );
	hb_xvmPushStringConst( "F4", 2 );
	hb_xvmPushStringConst( "F5", 2 );
	hb_xvmPushStringConst( "F6", 2 );
	hb_xvmPushStringConst( "F7", 2 );
	hb_xvmPushStringConst( "F8", 2 );
	hb_xvmPushStringConst( "F9", 2 );
	hb_xvmPushStringConst( "F10", 3 );
	hb_xvmPushStringConst( "F11", 3 );
	hb_xvmPushStringConst( "F12", 3 );
	hb_xvmPushStringConst( "F13", 3 );
	hb_xvmPushStringConst( "F14", 3 );
	hb_xvmPushStringConst( "F15", 3 );
	hb_xvmPushStringConst( "F16", 3 );
	hb_xvmPushStringConst( "F17", 3 );
	hb_xvmPushStringConst( "F18", 3 );
	hb_xvmPushStringConst( "F19", 3 );
	hb_xvmPushStringConst( "F20", 3 );
	hb_xvmPushStringConst( "F21", 3 );
	hb_xvmPushStringConst( "F22", 3 );
	hb_xvmPushStringConst( "F23", 3 );
	hb_xvmPushStringConst( "F24", 3 );
	hb_xvmPushStringConst( ".136", 4 );
	hb_xvmPushStringConst( ".137", 4 );
	hb_xvmPushStringConst( ".138", 4 );
	hb_xvmPushStringConst( ".139", 4 );
	hb_xvmPushStringConst( ".140", 4 );
	hb_xvmPushStringConst( ".141", 4 );
	hb_xvmPushStringConst( ".142", 4 );
	hb_xvmPushStringConst( ".143", 4 );
	hb_xvmPushStringConst( "NUMLOCK", 7 );
	hb_xvmPushStringConst( "SCROLL", 6 );
	hb_xvmPushStringConst( ".146", 4 );
	hb_xvmPushStringConst( ".147", 4 );
	hb_xvmPushStringConst( ".148", 4 );
	hb_xvmPushStringConst( ".149", 4 );
	hb_xvmPushStringConst( ".150", 4 );
	hb_xvmPushStringConst( ".151", 4 );
	hb_xvmPushStringConst( ".152", 4 );
	hb_xvmPushStringConst( ".153", 4 );
	hb_xvmPushStringConst( ".154", 4 );
	hb_xvmPushStringConst( ".155", 4 );
	hb_xvmPushStringConst( ".156", 4 );
	hb_xvmPushStringConst( ".157", 4 );
	hb_xvmPushStringConst( ".158", 4 );
	hb_xvmPushStringConst( ".159", 4 );
	hb_xvmPushStringConst( "LSHIFT", 6 );
	hb_xvmPushStringConst( "RSHIFT", 6 );
	hb_xvmPushStringConst( "LCONTROL", 8 );
	hb_xvmPushStringConst( "RCONTROL", 8 );
	hb_xvmPushStringConst( "LMENU", 5 );
	hb_xvmPushStringConst( "RMENU", 5 );
	hb_xvmArrayGen( 165 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "+", 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushStringConst( "_HMG_aKeyTables", 15 );
	if( hb_xvmFunction( 1 ) ) break;
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 1, 0, 8, 0, 95, 255, 95, 1, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 273 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 2 );
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "ALT", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 285 );
	hb_xvmLocalSetInt( 3, 1L );
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 287 );
	hb_xvmLocalSetInt( 3, 4L );
	goto lab00009;
lab00003: ;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "CTRL", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "CONTROL", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 290 );
	hb_xvmLocalSetInt( 4, 2L );
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "SHIFT", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "SHFT", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00006: ;
	hb_xvmSetLine( 292 );
	hb_xvmLocalSetInt( 5, 4L );
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 294 );
	hb_xvmLocalSetInt( 5, 1L );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "WIN", 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 297 );
	hb_xvmLocalSetInt( 6, 8L );
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 299 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
lab00009: ;
	goto lab00001;
lab00010: ;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

