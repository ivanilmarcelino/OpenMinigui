/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_radio.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINERADIOGROUP );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETBORDERWIDTH );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( AFILL );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGRADIOGROUP );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITRADIOGROUP );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( MOVEWINDOW );
HB_FUNC_EXTERN( GETTEXTHEIGHT );
HB_FUNC_EXTERN( INITRADIOBUTTON );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( SETWINDOWTHEME );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( GETBORDERHEIGHT );
HB_FUNC_EXTERN( _SETVALUE );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_RADIO )
{ "_DEFINERADIOGROUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINERADIOGROUP )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETBORDERWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERWIDTH )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "AFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AFILL )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGRADIOGROUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGRADIOGROUP )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITRADIOGROUP", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITRADIOGROUP )}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "MOVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MOVEWINDOW )}, NULL },
{ "GETTEXTHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTHEIGHT )}, NULL },
{ "INITRADIOBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITRADIOBUTTON )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "SETWINDOWTHEME", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTHEME )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "GETBORDERHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERHEIGHT )}, NULL },
{ "_SETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETVALUE )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_RADIO, "h_radio.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_RADIO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_RADIO )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINERADIOGROUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 16, 30 );
	hb_xvmSetLine( 58 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 66 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 45 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 27 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 28 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 26 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmPushInteger( 0 );
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 25 );
lab00003: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLocalByRef( 19 );
	if( hb_xvmDo( 7 ) ) break;
lab00004: ;
	hb_xvmSetLine( 87 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00005: ;
	hb_xvmSetLine( 88 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00007;
lab00006: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00007: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 93 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 96 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00009: ;
	hb_xvmSetLine( 98 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 9 );
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
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 10 );
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
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00013: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 9 );
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
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 117 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 36 );
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 120 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 122 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 124 );
	hb_xvmLocalSetInt( 41, 1073889289L );
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 126 );
	hb_xvmPushLocalByRef( 41 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00016: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocalByRef( 41 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00017: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 135 );
	hb_xvmLocalSetInt( 41, 1073758217L );
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocalByRef( 41 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00018: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 141 );
	hb_xvmCopyLocals( 3, 38 );
	hb_xvmSetLine( 142 );
	hb_xvmCopyLocals( 4, 39 );
	hb_xvmSetLine( 144 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 43 );
	goto lab00024;
lab00019: ;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 43 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 17, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	goto lab00021;
lab00020: ;
	{
		static const HB_BYTE codeblock[ 2 ] = {
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
lab00021: ;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 37 );
	hb_xvmPushStringConst( "button", 6 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 149 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPlusEqPop() ) break;
lab00023: ;
	hb_xvmSetLine( 144 );
	if( hb_xvmLocalIncPush( 43 ) ) break;
lab00024: ;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	goto lab00044;
lab00025: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 166 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 42 );
	goto lab00031;
lab00026: ;
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 32 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073758217 );
#else
	hb_xvmPushLong( 1073758217L );
#endif
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 32 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00027: ;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00028: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00029;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 33 );
lab00030: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 166 );
	if( hb_xvmLocalIncPush( 42 ) ) break;
lab00031: ;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	goto lab00044;
lab00032: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 199 );
	hb_xvmCopyLocals( 3, 38 );
	hb_xvmSetLine( 200 );
	hb_xvmCopyLocals( 4, 39 );
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 204 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 33 );
lab00034: ;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 213 );
	hb_xvmCopyLocals( 11, 40 );
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 21L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 8L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
lab00035: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 42 );
	goto lab00043;
lab00036: ;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 223 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00038;
lab00037: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPlusEqPop() ) break;
lab00038: ;
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 10 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 230 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00040;
lab00039: ;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 33 );
lab00040: ;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 21L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 8L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
lab00041: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00042: ;
	hb_xvmSetLine( 220 );
	if( hb_xvmLocalIncPush( 42 ) ) break;
lab00043: ;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
lab00044: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00048;
	hb_xvmSetLine( 253 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
lab00045: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 34 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 0, 0, 176, 35, 0, 95, 1, 106, 1, 0, 106, 1, 0, 12, 
			3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00046: ;
	hb_xvmSetLine( 257 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmSetLine( 258 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00047: ;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00048: ;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushStringConst( "RADIOGROUP", 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmPushLocal( 6 );
	goto lab00050;
lab00049: ;
	hb_xvmPushInteger( 0 );
lab00050: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 57L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmPushLocal( 40 );
	goto lab00052;
lab00051: ;
	hb_xvmPushLocal( 11 );
lab00052: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00053;
	hb_xvmPushInteger( 28 );
	goto lab00054;
lab00053: ;
	hb_xvmPushLocal( 12 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmMult() ) break;
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
lab00054: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00055;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00056;
lab00055: ;
	hb_xvmPushInteger( -1 );
lab00056: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00057;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00058;
lab00057: ;
	hb_xvmPushInteger( -1 );
lab00058: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00059;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 46L ) ) break;
	goto lab00060;
lab00059: ;
	hb_xvmPushStringConst( "", 0 );
lab00060: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00061;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00062;
lab00061: ;
	hb_xvmPushStringConst( "", 0 );
lab00062: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00063;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00064;
lab00063: ;
	hb_xvmPushLogical( HB_TRUE );
lab00064: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 309 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 310 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00066;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00065;
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00065;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 4 ) ) break;
lab00065: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ReadOnly", 8 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmDo( 4 ) ) break;
lab00066: ;
	hb_xvmSetLine( 324 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00067;
	hb_xvmSetLine( 325 );
	hb_xvmPushSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 45 );
lab00067: ;
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 334 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGRADIOGROUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 342 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 343 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "ReadOnly", 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 353 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 354 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00003: ;
	hb_xvmSetLine( 357 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

