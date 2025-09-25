/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_editbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEEDITBOX );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGEDIT );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITEDITBOX );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC( _DATAEDITBOXREFRESH );
HB_FUNC_EXTERN( _SETVALUE );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC_EXTERN( HB_ISLOGICAL );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_EDITBOX )
{ "_DEFINEEDITBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEEDITBOX )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGEDIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGEDIT )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITEDITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITEDITBOX )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "_DATAEDITBOXREFRESH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATAEDITBOXREFRESH )}, NULL },
{ "_SETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETVALUE )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_EDITBOX, "h_editbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_EDITBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_EDITBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEEDITBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 13, 31 );
	hb_xvmSetLine( 63 );
	hb_xvmLocalSetInt( 35, 0L );
	hb_xvmSetLine( 71 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 43 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 64738 );
#else
	hb_xvmPushLong( 64738L );
#endif
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Control: ", 9 );
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
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 97 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00003: ;
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00004: ;
	hb_xvmSetLine( 103 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00006;
lab00005: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00006: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 108 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 111 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00008: ;
	hb_xvmSetLine( 114 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 2 );
lab00010: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00011: ;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00012: ;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 125 );
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
lab00013: ;
	hb_xvmSetLine( 128 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 37 );
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 131 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 133 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 135 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073745924 );
#else
	hb_xvmPushLong( 1073745924L );
#endif
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushInteger( 0 );
	goto lab00015;
lab00014: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
lab00015: ;
	hb_xvmLocalAdd( 39 );
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmSetLine( 138 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00016: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00017: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2097152 );
#else
	hb_xvmPushLong( 2097152L );
#endif
	if( hb_xvmPlusEqPop() ) break;
	goto lab00019;
lab00018: ;
	hb_xvmSetLine( 148 );
	if( hb_xvmLocalAddInt( 39, 64 ) ) break;
lab00019: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 152 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1048576 );
#else
	hb_xvmPushLong( 1048576L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 157 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 16, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushStringConst( "edit", 4 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00021: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 168 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 172 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00022: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00030;
lab00023: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
lab00024: ;
	hb_xvmSetLine( 186 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 187 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
lab00025: ;
	hb_xvmSetLine( 190 );
	hb_xvmPushStringConst( "EDIT", 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 194 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00027;
lab00026: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 34 );
lab00027: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 207 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 41 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00028: ;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CM", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00030;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00034;
	hb_xvmSetLine( 236 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
lab00031: ;
	hb_xvmSetLine( 240 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00033;
lab00032: ;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 34 );
lab00033: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00034: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushStringConst( "EDIT", 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 269 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00035;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( -1 );
lab00036: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00037;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00038;
lab00037: ;
	hb_xvmPushInteger( -1 );
lab00038: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00040;
lab00039: ;
	hb_xvmPushLogical( HB_TRUE );
lab00040: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 3 ) ) break;
lab00041: ;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 2 ) ) break;
lab00042: ;
	hb_xvmSetLine( 308 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 309 );
	hb_xvmPushSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 43 );
lab00043: ;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 318 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATAEDITBOXREFRESH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 323 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 325 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmMacroDo( 3 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 176 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 337 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGEDIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 345 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 347 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 348 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 351 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 207 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 353 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 197 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00004: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 358 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 361 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

