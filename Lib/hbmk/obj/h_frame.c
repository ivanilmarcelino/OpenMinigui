/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_frame.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_STATIC( _DEFINEFRAME );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITFRAME );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETWINDOWTHEME );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _HIDECONTROL );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC( _BEGINFRAME );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( EMPTY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_FRAME )
{ "_DEFINEFRAME", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEFRAME )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITFRAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITFRAME )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETWINDOWTHEME", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTHEME )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "_HIDECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HIDECONTROL )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "_BEGINFRAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINFRAME )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_FRAME, "h_frame.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_FRAME
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_FRAME )
   #include "hbiniseg.h"
#endif

HB_FUNC_STATIC( _DEFINEFRAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 20 );
	hb_xvmSetLine( 61 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 28 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 23 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLocalByRef( 14 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 70 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 2 );
lab00003: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 7 );
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
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 6 );
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
	hb_xvmSetLine( 84 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 24 );
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 87 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 89 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 91 );
	hb_xvmLocalSetInt( 26, 1073758215L );
	hb_xvmSetLine( 93 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocalByRef( 26 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00007: ;
	hb_xvmSetLine( 97 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushStringConst( "button", 6 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 103 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 10 ) ) break;
	hb_xvmPopLocal( 22 );
lab00011: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 23 );
lab00013: ;
	hb_xvmSetLine( 141 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
lab00014: ;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00015: ;
	hb_xvmSetLine( 145 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushStringConst( "FRAME", 5 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 172 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 57L ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_stackPop();
	hb_xvmPushLocal( 10 );
lab00017: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 173 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 179 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00019;
lab00018: ;
	hb_xvmPushInteger( -1 );
lab00019: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 180 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00021;
lab00020: ;
	hb_xvmPushInteger( -1 );
lab00021: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 187 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	goto lab00023;
lab00022: ;
	hb_xvmPushStringConst( "", 0 );
lab00023: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 188 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00024;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00025;
lab00024: ;
	hb_xvmPushStringConst( "", 0 );
lab00025: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00026: ;
	hb_xvmSetLine( 202 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 203 );
	hb_xvmPushSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 28 );
lab00027: ;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _BEGINFRAME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 20 );
	hb_xvmSetLine( 218 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 223 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 226 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 230 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00003: ;
	hb_xvmSetLine( 231 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00005;
lab00004: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00005: ;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 237 );
	hb_xvmPushStringConst( "Arial", 5 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 238 );
	hb_xvmLocalSetInt( 9, 1L );
lab00007: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 140 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 140 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 20 ) ) break;
	hb_xvmSetLine( 246 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

