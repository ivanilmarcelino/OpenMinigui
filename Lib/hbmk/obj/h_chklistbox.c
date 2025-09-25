/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_chklistbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINECHKLISTBOX );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ACLONE );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITDIALOGCHKLISTBOX );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITMULTICHKLISTBOX );
HB_FUNC_EXTERN( INITCHKLISTBOX );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( CHKLISTBOXADDITEM );
HB_FUNC_EXTERN( SETCHKLBITEMHEIGHT );
HB_FUNC_EXTERN( LISTBOXSETMULTISEL );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( LISTBOXADDSTRING );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_CHKLISTBOX )
{ "_DEFINECHKLISTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECHKLISTBOX )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ACLONE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ACLONE )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITDIALOGCHKLISTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGCHKLISTBOX )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITMULTICHKLISTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITMULTICHKLISTBOX )}, NULL },
{ "INITCHKLISTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITCHKLISTBOX )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CHKLISTBOXADDITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHKLISTBOXADDITEM )}, NULL },
{ "SETCHKLBITEMHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETCHKLBITEMHEIGHT )}, NULL },
{ "LISTBOXSETMULTISEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTBOXSETMULTISEL )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "LISTBOXADDSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( LISTBOXADDSTRING )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_CHKLISTBOX, "h_chklistbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_CHKLISTBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_CHKLISTBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINECHKLISTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 30 );
	hb_xvmSetLine( 58 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 37 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 61 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 5, 120L );
lab00001: ;
	hb_xvmSetLine( 62 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmLocalSetInt( 6, 120L );
lab00002: ;
	hb_xvmSetLine( 63 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 14 );
lab00003: ;
	hb_xvmSetLine( 64 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 15 );
lab00004: ;
	hb_xvmSetLine( 65 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmLocalSetInt( 8, 0L );
lab00005: ;
	hb_xvmSetLine( 66 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 12 );
lab00006: ;
	hb_xvmSetLine( 67 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 13 );
lab00007: ;
	hb_xvmSetLine( 68 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 18 );
lab00008: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 19 );
lab00009: ;
	hb_xvmSetLine( 70 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 20 );
lab00010: ;
	hb_xvmSetLine( 71 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 28 );
lab00011: ;
	hb_xvmSetLine( 72 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmLocalSetInt( 29, 16L );
lab00012: ;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 36 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLocalByRef( 24 );
	if( hb_xvmDo( 7 ) ) break;
lab00013: ;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 79 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
lab00014: ;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			2, 0, 3, 0, 41, 0, 40, 0, 28, 0, 95, 2, 80, 255, 176, 7, 
			0, 95, 254, 176, 8, 0, 95, 253, 89, 13, 0, 1, 0, 0, 0, 95, 
			1, 95, 255, 8, 6, 12, 2, 121, 15, 28, 6, 92, 2, 25, 3, 122, 
			12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 37, 0, 176, 7, 0, 95, 255, 95, 1, 122, 1, 12, 
			2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	{
		static const HB_BYTE codeblock[ 75 ] = {
			2, 0, 3, 0, 41, 0, 40, 0, 28, 0, 95, 2, 80, 255, 176, 7, 
			0, 95, 254, 176, 9, 0, 95, 1, 92, 2, 1, 12, 1, 106, 2, 76, 
			0, 8, 28, 9, 95, 1, 92, 2, 1, 31, 26, 176, 8, 0, 95, 253, 
			89, 13, 0, 1, 0, 0, 0, 95, 1, 95, 255, 8, 6, 12, 2, 121, 
			15, 28, 6, 92, 2, 25, 3, 122, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 91 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
lab00017: ;
	hb_xvmSetLine( 92 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00019;
lab00018: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00019: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00020: ;
	hb_xvmSetLine( 97 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmSetLine( 98 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 99 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 100 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00021: ;
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00023;
lab00022: ;
	hb_xvmPushLocal( 2 );
lab00023: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00024: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00025: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 13 );
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
lab00026: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 33 );
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 119 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 121 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 123 );
	hb_xvmLocalSetInt( 35, 1084231937L );
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 126 );
	if( hb_xvmLocalAddInt( 35, 8 ) ) break;
lab00027: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocalByRef( 35 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00028: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00029;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocalByRef( 35 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00029: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 138 );
	if( hb_xvmLocalAddInt( 35, 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 144 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 18, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushStringConst( "LISTBOX", 7 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00039;
lab00031: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00039;
lab00032: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00033;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
lab00033: ;
	hb_xvmSetLine( 166 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 167 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
lab00034: ;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 34 );
	goto lab00036;
lab00035: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 34 );
lab00036: ;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushStringConst( "LISTBOX", 7 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	goto lab00039;
lab00037: ;
	hb_xvmSetLine( 188 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 34 );
	goto lab00039;
lab00038: ;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 34 );
lab00039: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00043;
	hb_xvmSetLine( 200 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00040;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00041;
lab00040: ;
	hb_xvmSetLine( 203 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 36 );
lab00041: ;
	hb_xvmSetLine( 208 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00042: ;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00043: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmPushStringConst( "MULTICHKLIST", 12 );
	goto lab00045;
lab00044: ;
	hb_xvmPushStringConst( "CHKLIST", 7 );
lab00045: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 240 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 246 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00046;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00047;
lab00046: ;
	hb_xvmPushInteger( -1 );
lab00047: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 247 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00048;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00049;
lab00048: ;
	hb_xvmPushInteger( -1 );
lab00049: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00050;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00051;
lab00050: ;
	hb_xvmPushLogical( HB_TRUE );
lab00051: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 39 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 266 );
	hb_xvmPushSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00052: ;
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00056;
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00053;
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 37 );
	{
		static const HB_BYTE codeblock[ 27 ] = {
			2, 0, 3, 0, 40, 0, 34, 0, 29, 0, 176, 33, 0, 95, 254, 95, 
			1, 95, 255, 95, 2, 1, 95, 253, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00053: ;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00054;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmDo( 2 ) ) break;
lab00054: ;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00056;
lab00055: ;
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmPushLocal( 8 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00056;
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 390 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00056: ;
	hb_xvmSetLine( 289 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGCHKLISTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 3 );
	hb_xvmSetLine( 298 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 299 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 300 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 301 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 303 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 4 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			1, 0, 1, 0, 2, 0, 176, 38, 0, 95, 255, 95, 1, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 311 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "MULTICHKLIST", 12 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 390 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00004: ;
	hb_xvmSetLine( 321 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
	hb_xvmSetLine( 322 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00005: ;
	hb_xvmSetLine( 325 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

