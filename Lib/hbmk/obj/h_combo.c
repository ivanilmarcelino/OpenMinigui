/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_combo.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINECOMBO );
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
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( HB_URIGHT );
HB_FUNC_EXTERN( HB_ULEN );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGCOMBOBOX );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITCOMBOBOX );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETWINDOWTHEME );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( FINDWINDOWEX );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( SENDMESSAGEWIDESTRING );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( RECNO );
HB_FUNC_EXTERN( DBGOTOP );
HB_FUNC_EXTERN( EOF );
HB_FUNC_EXTERN( COMBOADDSTRING );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( DBSKIP );
HB_FUNC_EXTERN( DBGOTO );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( COMBOSETITEMHEIGHT );
HB_FUNC( _DATACOMBOREFRESH );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC_EXTERN( COMBOADDDATASTRINGEX );
HB_FUNC_EXTERN( LASTREC );
HB_FUNC_EXTERN( _SETVALUE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_COMBO )
{ "_DEFINECOMBO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECOMBO )}, NULL },
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
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "HB_URIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_URIGHT )}, NULL },
{ "HB_ULEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEN )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGCOMBOBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGCOMBOBOX )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITCOMBOBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITCOMBOBOX )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETWINDOWTHEME", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTHEME )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "FINDWINDOWEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( FINDWINDOWEX )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "SENDMESSAGEWIDESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGEWIDESTRING )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "RECNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECNO )}, NULL },
{ "DBGOTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTOP )}, NULL },
{ "EOF", {HB_FS_PUBLIC}, {HB_FUNCNAME( EOF )}, NULL },
{ "COMBOADDSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( COMBOADDSTRING )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "DBSKIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSKIP )}, NULL },
{ "DBGOTO", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBGOTO )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "COMBOSETITEMHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( COMBOSETITEMHEIGHT )}, NULL },
{ "_DATACOMBOREFRESH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATACOMBOREFRESH )}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "COMBOADDDATASTRINGEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( COMBOADDDATASTRINGEX )}, NULL },
{ "LASTREC", {HB_FS_PUBLIC}, {HB_FUNCNAME( LASTREC )}, NULL },
{ "_SETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETVALUE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_COMBO, "h_combo.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_COMBO
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_COMBO )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINECOMBO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 14, 43 );
	hb_xvmSetLine( 73 );
	hb_xvmLocalSetInt( 50, 0L );
	hb_xvmSetLine( 78 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 56 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 57 );
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 57 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushInteger( 150 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 29 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 40 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 41 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 46 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 102 );
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
	hb_xvmSetLine( 103 );
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
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 108 );
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
	hb_xvmSetLine( 109 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 111 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 113 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 55 );
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmPushLocal( 55 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 116 );
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
	hb_xvmSetLine( 119 );
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
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00010: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 55 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 124 );
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
	hb_xvmSetLine( 127 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Sort and ItemSource clauses can't be used simultaneously.", 57 );
	if( hb_xvmDo( 1 ) ) break;
lab00012: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Sort and ValueSource clauses can't be used simultaneously.", 58 );
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " (ItemSource): You must specify a fully qualified field name.", 61 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00015;
lab00014: ;
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 48 );
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 49 );
lab00015: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 47 );
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 52 );
	hb_xvmSetLine( 150 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 152 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 154 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1075838976 );
#else
	hb_xvmPushLong( 1075838976L );
#endif
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushInteger( 2 );
	goto lab00017;
lab00016: ;
	hb_xvmPushInteger( 3 );
lab00017: ;
	hb_xvmLocalAdd( 54 );
	hb_xvmSetLine( 156 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocalByRef( 54 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00018: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocalByRef( 54 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00019: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 165 );
	if( hb_xvmLocalAddInt( 54, 256 ) ) break;
lab00020: ;
	hb_xvmSetLine( 168 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 169 );
	if( hb_xvmLocalAddInt( 54, 1024 ) ) break;
lab00021: ;
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 173 );
	if( hb_xvmLocalAddInt( 54, 8192 ) ) break;
lab00022: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 177 );
	if( hb_xvmLocalAddInt( 54, 16384 ) ) break;
lab00023: ;
	hb_xvmSetLine( 180 );
	hb_xvmPushLocal( 55 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 183 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 17, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 53 );
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 52 );
	hb_xvmPushStringConst( "COMBOBOX", 8 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 53 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00032;
lab00024: ;
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 45 );
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 195 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00025;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00026;
lab00025: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 46 );
lab00026: ;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 54 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00032;
lab00027: ;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
lab00028: ;
	hb_xvmSetLine( 213 );
	hb_xvmPushStringConst( "COMBOBOX", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 51 );
	hb_xvmSetLine( 217 );
	hb_xvmPushLocal( 51 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 51 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmFunction( 13 ) ) break;
	hb_xvmPopLocal( 45 );
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00029;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 46 );
lab00030: ;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 51 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 231 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 51 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 50 );
	goto lab00032;
lab00031: ;
	hb_xvmSetLine( 237 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 44 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmFunction( 13 ) ) break;
	hb_xvmPopLocal( 45 );
lab00032: ;
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 55 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00038;
	hb_xvmSetLine( 245 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 46 );
lab00034: ;
	hb_xvmSetLine( 253 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 45 );
	if( hb_xvmDo( 2 ) ) break;
lab00035: ;
	hb_xvmSetLine( 257 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00036;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
lab00036: ;
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00037;
	hb_xvmSetLine( 258 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 3 ) ) break;
lab00037: ;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00038: ;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 47 );
	hb_xvmPushLocal( 52 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushStringConst( "COMBO", 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 49 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 48 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00040;
lab00039: ;
	hb_xvmPushInteger( -1 );
lab00040: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00042;
lab00041: ;
	hb_xvmPushInteger( -1 );
lab00042: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushLocal( 50 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00044;
lab00043: ;
	hb_xvmPushLogical( HB_TRUE );
lab00044: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 46 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 309 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 310 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 42 );
	hb_xvmArrayGen( 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 52 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00045;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 45 );
	hb_xvmPushLocal( 52 );
	if( hb_xvmDo( 3 ) ) break;
lab00045: ;
	hb_xvmSetLine( 318 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 52 );
	hb_xvmPushLocal( 47 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 321 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 57 );
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 56 );
lab00046: ;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 43 );
	hb_xvmPushLocal( 52 );
	hb_xvmPushLocal( 57 );
	hb_xvmPushLocal( 56 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 328 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGCOMBOBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 3 );
	hb_xvmSetLine( 334 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmLocalSetInt( 7, 0L );
	hb_xvmSetLine( 336 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 337 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 338 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 339 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 340 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 341 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 342 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 343 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 344 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 346 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Edit", 4 );
	hb_xvmPushNil();
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 350 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 351 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 355 );
	hb_xvmLocalSetInt( 9, 0L );
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 361 );
	hb_xvmLocalSetInt( 9, 0L );
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 5891 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 352 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 372 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 374 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
lab00004: ;
	hb_xvmSetLine( 376 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 377 );
	if( hb_xvmLocalInc( 6 ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 379 );
	hb_xvmCopyLocals( 6, 7 );
lab00005: ;
	hb_xvmSetLine( 381 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmMacroPushAliased( 43 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 382 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
	goto lab00004;
lab00006: ;
	hb_xvmSetLine( 385 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	if( hb_xvmPopAlias() ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 334 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 393 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 10 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			1, 0, 1, 0, 2, 0, 176, 48, 0, 95, 255, 95, 1, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLocal( 9 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 334 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00009: ;
	hb_xvmSetLine( 403 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
lab00010: ;
	hb_xvmSetLine( 407 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 412 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00012: ;
	hb_xvmSetLine( 415 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATACOMBOREFRESH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 1 );
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 423 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 425 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 426 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 427 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 428 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 431 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 433 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 435 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 437 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 439 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
	hb_xvmSetLine( 441 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 331 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 443 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 444 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmNotEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 445 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMacroPushAliased( 43 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMacroPushAliased( 43 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 449 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmDo( 0 ) ) break;
	if( hb_xvmPopAlias() ) break;
	goto lab00003;
lab00006: ;
	hb_xvmSetLine( 452 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	if( hb_xvmPopAlias() ) break;
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushAlias() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopAlias() ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSwapAlias() ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	hb_xvmSetLine( 458 );
	/* *** END PROC *** */
   } while( 0 );
}

