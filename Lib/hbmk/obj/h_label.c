/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_label.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINELABEL );
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
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGLABEL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITLABEL );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _SETCONTROLWIDTH );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( _SETCONTROLHEIGHT );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( _SHOWCONTROL );
HB_FUNC_EXTERN( _HIDECONTROL );
HB_FUNC_EXTERN( _REFRESH );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( REDRAWWINDOW );
HB_FUNC_EXTERN( LEN );
HB_FUNC( OLABELEVENTS );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( _DOCONTROLEVENTPROCEDURE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_LABEL )
{ "_DEFINELABEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINELABEL )}, NULL },
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
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGLABEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGLABEL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITLABEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITLABEL )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "_SETCONTROLWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLWIDTH )}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "_SETCONTROLHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTROLHEIGHT )}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_SHOWCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SHOWCONTROL )}, NULL },
{ "_HIDECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HIDECONTROL )}, NULL },
{ "_REFRESH", {HB_FS_PUBLIC}, {HB_FUNCNAME( _REFRESH )}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "REDRAWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDRAWWINDOW )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "OLABELEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OLABELEVENTS )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_DOCONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOCONTROLEVENTPROCEDURE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_LABEL, "h_label.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_LABEL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_LABEL )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINELABEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 10, 36 );
	hb_xvmSetLine( 60 );
	hb_xvmLocalSetInt( 41, 0L );
	hb_xvmSetLine( 64 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 45 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 31 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 36 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 39 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLocalByRef( 24 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 85 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 86 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 91 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
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
lab00006: ;
	hb_xvmSetLine( 97 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00010: ;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 108 );
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
lab00011: ;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 112 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 5 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 40, 0, 96, 255, 255, 176, 14, 0, 95, 1, 12, 1, 
			139, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 114 );
	hb_xvmCopyLocals( 40, 5 );
	goto lab00014;
lab00012: ;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00014: ;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 16 );
lab00015: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 17 );
lab00016: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 40 );
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 132 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 136 );
	hb_xvmLocalSetInt( 42, 1073742080L );
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00017: ;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00018: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1048576 );
#else
	hb_xvmPushLong( 1048576L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00019: ;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 151 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2097152 );
#else
	hb_xvmPushLong( 2097152L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 155 );
	if( hb_xvmLocalAddInt( 42, 2 ) ) break;
lab00021: ;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 159 );
	if( hb_xvmLocalAddInt( 42, 1 ) ) break;
lab00022: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 163 );
	if( hb_xvmLocalAddInt( 42, 512 ) ) break;
lab00023: ;
	hb_xvmSetLine( 166 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 169 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 22, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 43 );
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "static", 6 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00029;
lab00024: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00025: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00029;
lab00026: ;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
lab00027: ;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 1 ) ) break;
lab00028: ;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 20 ) ) break;
	hb_xvmPopLocal( 38 );
lab00029: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00031;
lab00030: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 39 );
lab00031: ;
	hb_xvmSetLine( 207 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 2 ) ) break;
lab00032: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00033: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushStringConst( "LABEL", 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 238 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 57L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 240 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushInteger( 1 );
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( 0 );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 245 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00037;
lab00036: ;
	hb_xvmPushInteger( -1 );
lab00037: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 246 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00038;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( -1 );
lab00039: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00040;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	goto lab00041;
lab00040: ;
	hb_xvmPushStringConst( "", 0 );
lab00041: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00042;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00043;
lab00042: ;
	hb_xvmPushStringConst( "", 0 );
lab00043: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00045;
lab00044: ;
	hb_xvmPushLogical( HB_TRUE );
lab00045: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00053;
	hb_xvmSetLine( 265 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00046;
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
lab00046: ;
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00048;
lab00047: ;
	hb_xvmPushInteger( 0 );
lab00048: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmLessThenIntIs( 14L, &fValue ) ) break;
	if( !fValue )
		goto lab00049;
	hb_xvmPushInteger( 12 );
	goto lab00050;
lab00049: ;
	hb_xvmPushInteger( 16 );
lab00050: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00051: ;
	hb_xvmSetLine( 270 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushStringConst( "BlinkTimer", 10 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 500 );
	{
		static const HB_BYTE codeblock[ 73 ] = {
			0, 0, 3, 0, 41, 0, 1, 0, 2, 0, 98, 6, 0, 93, 171, 0, 
			1, 95, 255, 1, 92, 3, 1, 68, 98, 6, 0, 93, 171, 0, 1, 95, 
			255, 1, 92, 3, 2, 98, 6, 0, 93, 171, 0, 1, 95, 255, 1, 92, 
			3, 1, 28, 13, 176, 40, 0, 95, 254, 95, 253, 12, 2, 25, 11, 176, 
			41, 0, 95, 254, 95, 253, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
lab00052: ;
	hb_xvmSetLine( 274 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00053;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmDo( 1 ) ) break;
lab00053: ;
	hb_xvmSetLine( 279 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmSetLine( 280 );
	hb_xvmPushSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 45 );
lab00054: ;
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 289 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGLABEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 294 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 296 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 0 );
lab00003: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmLessThenIntIs( 14L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushInteger( 12 );
	goto lab00005;
lab00004: ;
	hb_xvmPushInteger( 16 );
lab00005: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 303 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushStringConst( "BlinkTimer", 10 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 500 );
	{
		static const HB_BYTE codeblock[ 73 ] = {
			0, 0, 3, 0, 3, 0, 4, 0, 1, 0, 98, 6, 0, 93, 171, 0, 
			1, 95, 255, 1, 92, 3, 1, 68, 98, 6, 0, 93, 171, 0, 1, 95, 
			255, 1, 92, 3, 2, 98, 6, 0, 93, 171, 0, 1, 95, 255, 1, 92, 
			3, 1, 28, 13, 176, 40, 0, 95, 254, 95, 253, 12, 2, 25, 11, 176, 
			41, 0, 95, 254, 95, 253, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 309 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00008: ;
	hb_xvmSetLine( 312 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OLABELEVENTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 324 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 2 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 512L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 675L )
		{
			hb_stackPop();
			goto lab00002;
		}
		hb_stackPop();
	}
lab00004: ;
	hb_xvmSetLine( 338 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

