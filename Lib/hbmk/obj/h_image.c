/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_image.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEIMAGE );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC( INITDIALOGIMAGE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITIMAGE );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( C_SETPICTURE );
HB_FUNC( HASALPHA );
HB_FUNC_EXTERN( LEN );
HB_FUNC( BMPSIZE );
HB_FUNC_EXTERN( GETBITMAPSIZE );
HB_FUNC_EXTERN( C_GETRESPICTURE );
HB_FUNC_EXTERN( DELETEOBJECT );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( C_HASALPHA );
HB_FUNC( HMG_SAVEIMAGE );
HB_FUNC_EXTERN( C_SAVEHBITMAPTOFILE );
HB_FUNC_EXTERN( LOWER );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_IMAGE )
{ "_DEFINEIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEIMAGE )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "INITDIALOGIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGIMAGE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITIMAGE )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "C_SETPICTURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_SETPICTURE )}, NULL },
{ "HASALPHA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HASALPHA )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "BMPSIZE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BMPSIZE )}, NULL },
{ "GETBITMAPSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBITMAPSIZE )}, NULL },
{ "C_GETRESPICTURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_GETRESPICTURE )}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "C_HASALPHA", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_HASALPHA )}, NULL },
{ "HMG_SAVEIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_SAVEIMAGE )}, NULL },
{ "C_SAVEHBITMAPTOFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( C_SAVEHBITMAPTOFILE )}, NULL },
{ "LOWER", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWER )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_IMAGE, "h_image.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_IMAGE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_IMAGE )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 22 );
	hb_xvmSetLine( 62 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 64 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 65 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 33 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 71 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 72 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00003;
lab00002: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00003: ;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 75 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 76 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 78 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00005: ;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 2 );
lab00007: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00009: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 5 );
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
lab00010: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushInteger( -1 );
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 6 );
lab00012: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushInteger( -1 );
	goto lab00014;
lab00013: ;
	hb_xvmPushLocal( 7 );
lab00014: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 29 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 107 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 30 );
lab00016: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 113 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 29 );
lab00017: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushLocal( 18 );
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmPushLocal( 18 );
	if( hb_xvmGreaterThenIntIs( 255L, &fValue ) ) break;
	if( !fValue )
		goto lab00019;
lab00018: ;
	hb_xvmSetLine( 117 );
	hb_xvmLocalSetInt( 18, 255L );
lab00019: ;
	hb_xvmSetLine( 120 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 26 );
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 123 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 125 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 127 );
	hb_xvmLocalSetInt( 28, 1073741838L );
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocalByRef( 28 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00020: ;
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00021;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
lab00021: ;
	hb_xvmSetLine( 134 );
	if( hb_xvmLocalAddInt( 28, 256 ) ) break;
lab00022: ;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 139 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 15, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushStringConst( "static", 6 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00027;
lab00023: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00027;
lab00024: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 30 );
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
lab00025: ;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
lab00026: ;
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 24 );
lab00027: ;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00029;
	hb_xvmSetLine( 165 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmDo( 2 ) ) break;
lab00028: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00029: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushStringConst( "IMAGE", 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 187 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmPushInteger( 1 );
	goto lab00031;
lab00030: ;
	hb_xvmPushInteger( 0 );
lab00031: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmPushInteger( 1 );
	goto lab00033;
lab00032: ;
	hb_xvmPushInteger( 0 );
lab00033: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00034;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( -1 );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00036;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00037;
lab00036: ;
	hb_xvmPushInteger( -1 );
lab00037: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmPushInteger( 1 );
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( 0 );
lab00039: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00041;
lab00040: ;
	hb_xvmPushLogical( HB_TRUE );
lab00041: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00042;
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmDo( 3 ) ) break;
lab00042: ;
	hb_xvmSetLine( 226 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 33 );
lab00043: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00001: ;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 10 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 256 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00003: ;
	hb_xvmSetLine( 259 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BMPSIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 264 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 4 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 277 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HASALPHA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 289 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 291 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 294 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 298 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_SAVEIMAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 5 );
	hb_xvmSetLine( 304 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 306 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 7L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( "BMP", 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 310 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStringConst( "image/", 6 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

