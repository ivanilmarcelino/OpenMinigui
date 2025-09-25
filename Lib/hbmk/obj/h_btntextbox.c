/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_btntextbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEBTNTEXTBOX );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( INITDIALOGTEXTBOX );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( REDEFBTNTEXTBOX );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITBTNTEXTBOX );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTBBTNMARGIN );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( SENDMESSAGEWIDESTRING );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC( INITDIALOGBTNTEXTBOX );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC( TBBTNEVENTS );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( _DOCONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( TRACKPOPUPMENU );
HB_FUNC_EXTERN( HIWORD );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_BTNTEXTBOX )
{ "_DEFINEBTNTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEBTNTEXTBOX )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITDIALOGTEXTBOX )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "REDEFBTNTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDEFBTNTEXTBOX )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITBTNTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITBTNTEXTBOX )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTBBTNMARGIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTBBTNMARGIN )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "SENDMESSAGEWIDESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGEWIDESTRING )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "INITDIALOGBTNTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGBTNTEXTBOX )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "TBBTNEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBBTNEVENTS )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_DOCONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOCONTROLEVENTPROCEDURE )}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "TRACKPOPUPMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRACKPOPUPMENU )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_BTNTEXTBOX, "h_btntextbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_BTNTEXTBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_BTNTEXTBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEBTNTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 14, 40 );
	hb_xvmSetLine( 70 );
	hb_xvmLocalSetInt( 42, 0L );
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 75 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 53 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 54 );
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 54 );
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 37 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 39 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 98 );
	hb_xvmCopyLocals( 10, 48 );
	hb_xvmSetLine( 99 );
	hb_xvmPushInteger( 2 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 48 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
lab00001: ;
	hb_xvmSetLine( 103 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Control ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " : You must specify a fully qualified field name.", 49 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 50 );
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 50 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00003: ;
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 115 );
	hb_xvmCopyLocals( 14, 49 );
	hb_xvmSetLine( 116 );
	hb_xvmPushInteger( 3 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 49 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
lab00005: ;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 47 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 47 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushLocalByRef( 27 );
	hb_xvmPushLocalByRef( 28 );
	hb_xvmPushLocalByRef( 29 );
	if( hb_xvmDo( 7 ) ) break;
lab00006: ;
	hb_xvmSetLine( 128 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00007: ;
	hb_xvmSetLine( 129 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00009;
lab00008: ;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00009: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 12 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00010: ;
	hb_xvmSetLine( 134 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 137 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00011: ;
	hb_xvmSetLine( 139 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 52 );
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmPushLocal( 52 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window ", 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00013;
lab00012: ;
	hb_xvmPushLocal( 2 );
lab00013: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00015: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushLocal( 52 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Control ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00016: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 43 );
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 158 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 159 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 161 );
	hb_xvmLocalSetInt( 45, 1107296384L );
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 164 );
	if( hb_xvmLocalAddInt( 45, 8192 ) ) break;
	goto lab00019;
lab00017: ;
	hb_xvmSetLine( 166 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 167 );
	if( hb_xvmLocalAddInt( 45, 8 ) ) break;
lab00018: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 170 );
	if( hb_xvmLocalAddInt( 45, 16 ) ) break;
lab00019: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 175 );
	if( hb_xvmLocalAddInt( 45, 32 ) ) break;
lab00020: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 179 );
	if( hb_xvmLocalAddInt( 45, 2 ) ) break;
lab00021: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 183 );
	if( hb_xvmLocalAddInt( 45, 2048 ) ) break;
lab00022: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00023;
	hb_xvmSetLine( 187 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00023: ;
	hb_xvmSetLine( 190 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmSetLine( 191 );
	hb_xvmPushLocalByRef( 45 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00024: ;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00025;
	hb_xvmSetLine( 197 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 20, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 51 );
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushStringConst( "EDIT", 4 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 51 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00027;
lab00025: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 42 );
	goto lab00027;
lab00026: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmFunction( 22 ) ) break;
	hb_xvmPopLocal( 42 );
lab00027: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 52 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00034;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 47 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 47 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 12 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 47 );
lab00029: ;
	hb_xvmSetLine( 234 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 42 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 46 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 49 );
	goto lab00033;
lab00031: ;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00032: ;
	hb_xvmSetLine( 241 );
	if( hb_xvmLocalIncPush( 49 ) ) break;
lab00033: ;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00031;
lab00034: ;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 43 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushStringConst( "BTNNUMTEXT", 10 );
	goto lab00036;
lab00035: ;
	hb_xvmPushStringConst( "BTNTEXT", 7 );
lab00036: ;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00037;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00038;
lab00037: ;
	hb_xvmPushInteger( -1 );
lab00038: ;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00040;
lab00039: ;
	hb_xvmPushInteger( -1 );
lab00040: ;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 47 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 39 );
	hb_xvmArrayGen( 5 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 52 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00044;
	hb_xvmSetLine( 298 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
lab00041: ;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00042;
	hb_xvmSetLine( 306 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00042: ;
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 310 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 4 ) ) break;
lab00043: ;
	hb_xvmSetLine( 313 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmSetLine( 314 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmDo( 2 ) ) break;
lab00044: ;
	hb_xvmSetLine( 318 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 321 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 54 );
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 53 );
lab00045: ;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLocal( 53 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 328 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGBTNTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 3 );
	hb_xvmSetLine( 335 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 336 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 337 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 338 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "BTNNUMTEXT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 339 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 340 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 341 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 342 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 344 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 197 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 349 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 351 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00002: ;
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 357 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 360 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 367 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 370 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TBBTNEVENTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	{
		static const HB_BYTE codeblock[ 61 ] = {
			1, 0, 1, 0, 1, 0, 176, 34, 0, 95, 1, 12, 1, 106, 2, 65, 
			0, 8, 21, 28, 41, 73, 176, 10, 0, 95, 1, 12, 1, 121, 15, 21, 
			28, 28, 73, 176, 34, 0, 95, 1, 122, 1, 12, 1, 106, 2, 78, 0, 
			8, 21, 28, 10, 73, 95, 1, 122, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 380 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 382 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 384 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 388 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 244 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 396 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 244 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00003;
		}
		hb_stackPop();
	}
lab00005: ;
	hb_xvmSetLine( 401 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 123L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 402 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 403 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	{
		static const HB_BYTE codeblock[ 16 ] = {
			1, 0, 1, 0, 6, 0, 95, 1, 122, 1, 95, 255, 122, 1, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 404 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 405 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 406 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPop( 87L ) ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	if( hb_xvmPushMemvar( symbols + 14 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00007: ;
	hb_xvmSetLine( 419 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

