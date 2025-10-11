/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_owntab.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( OWNTABPAINT );
HB_FUNC_EXTERN( GETOWNBTNDC );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( GETOWNBTNHANDLE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( GETOWNBTNITEMID );
HB_FUNC_EXTERN( GETOWNBTNRECT );
HB_FUNC_EXTERN( AND );
HB_FUNC_EXTERN( GETOWNBTNSTATE );
HB_FUNC_EXTERN( SELECTOBJECT );
HB_FUNC_EXTERN( GETTEXTMETRIC );
HB_FUNC_EXTERN( SETBKMODE );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( SETTEXTCOLOR );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SETBKCOLOR );
HB_FUNC_EXTERN( CREATESOLIDBRUSH );
HB_FUNC_EXTERN( FILLRECT );
HB_FUNC_EXTERN( DELETEOBJECT );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( LOADBITMAP );
HB_FUNC_EXTERN( LOADIMAGE );
HB_FUNC_EXTERN( GETBITMAPSIZE );
HB_FUNC_EXTERN( DRAWGLYPH );
HB_FUNC_EXTERN( DRAWTEXT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_OWNTAB )
{ "OWNTABPAINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OWNTABPAINT )}, NULL },
{ "GETOWNBTNDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOWNBTNDC )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETOWNBTNHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOWNBTNHANDLE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "GETOWNBTNITEMID", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOWNBTNITEMID )}, NULL },
{ "GETOWNBTNRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOWNBTNRECT )}, NULL },
{ "AND", {HB_FS_PUBLIC}, {HB_FUNCNAME( AND )}, NULL },
{ "GETOWNBTNSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOWNBTNSTATE )}, NULL },
{ "SELECTOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECTOBJECT )}, NULL },
{ "GETTEXTMETRIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTMETRIC )}, NULL },
{ "SETBKMODE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETBKMODE )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "SETTEXTCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTEXTCOLOR )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SETBKCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETBKCOLOR )}, NULL },
{ "CREATESOLIDBRUSH", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATESOLIDBRUSH )}, NULL },
{ "FILLRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILLRECT )}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL },
{ "ROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUND )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "LOADBITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOADBITMAP )}, NULL },
{ "LOADIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOADIMAGE )}, NULL },
{ "GETBITMAPSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBITMAPSIZE )}, NULL },
{ "DRAWGLYPH", {HB_FS_PUBLIC}, {HB_FUNCNAME( DRAWGLYPH )}, NULL },
{ "DRAWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DRAWTEXT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_OWNTAB, "h_owntab.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_OWNTAB
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_OWNTAB )
   #include "hbiniseg.h"
#endif

HB_FUNC( OWNTABPAINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 28, 1 );
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 16 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 87 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 100 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreaterEqualThenInt( 12L ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 101 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreaterEqualThenInt( 18L ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 102 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreaterEqualThenInt( 24L ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 105 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 17 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 124 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 126 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
lab00004: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmAddInt( -10L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmLocalAdd( 21 );
	hb_xvmSetLine( 151 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 153 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 5 );
lab00005: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 167 );
	hb_xvmLocalSetInt( 22, 4L );
	hb_xvmSetLine( 168 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 171 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 174 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushInteger( 8 );
	goto lab00007;
lab00006: ;
	hb_xvmPushInteger( 5 );
lab00007: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushInteger( 8 );
	goto lab00009;
lab00008: ;
	hb_xvmPushInteger( 5 );
lab00009: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 9 ) ) break;
	goto lab00017;
lab00010: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushInteger( 8 );
	goto lab00012;
lab00011: ;
	hb_xvmPushInteger( 5 );
lab00012: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 24 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushInteger( 8 );
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( 5 );
lab00014: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 9 ) ) break;
	goto lab00017;
lab00015: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmAddInt( -2L ) ) break;
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 9 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmAddInt( 2L ) ) break;
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 9 ) ) break;
lab00017: ;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
lab00018: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 194 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00022;
lab00019: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 128 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00022;
lab00020: ;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00022: ;
	hb_xvmSetLine( 210 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushInteger( -12 );
	goto lab00026;
lab00023: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushInteger( -3 );
	goto lab00026;
lab00024: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmPushInteger( 6 );
	goto lab00026;
lab00025: ;
	hb_xvmPushInteger( 12 );
lab00026: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushInteger( -12 );
	goto lab00030;
lab00027: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmPushInteger( -3 );
	goto lab00030;
lab00028: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushInteger( 6 );
	goto lab00030;
lab00029: ;
	hb_xvmPushInteger( 12 );
lab00030: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 7 ) ) break;
	goto lab00058;
lab00031: ;
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmPushInteger( -18 );
	goto lab00035;
lab00032: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushInteger( -8 );
	goto lab00035;
lab00033: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushInteger( 0 );
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( 8 );
lab00035: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmPushInteger( -18 );
	goto lab00039;
lab00036: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushInteger( -8 );
	goto lab00039;
lab00037: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmPushInteger( 0 );
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( 8 );
lab00039: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 7 ) ) break;
	goto lab00058;
lab00040: ;
	hb_xvmSetLine( 217 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmPushInteger( -9 );
	goto lab00044;
lab00041: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmPushInteger( -5 );
	goto lab00044;
lab00042: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmPushInteger( 0 );
	goto lab00044;
lab00043: ;
	hb_xvmPushInteger( 4 );
lab00044: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmPushInteger( -9 );
	goto lab00048;
lab00045: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmPushInteger( -5 );
	goto lab00048;
lab00046: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmPushInteger( 0 );
	goto lab00048;
lab00047: ;
	hb_xvmPushInteger( 4 );
lab00048: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 7 ) ) break;
	goto lab00058;
lab00049: ;
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00050;
	hb_xvmPushInteger( 14 );
	goto lab00053;
lab00050: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmPushInteger( 8 );
	goto lab00053;
lab00051: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmPushInteger( 4 );
	goto lab00053;
lab00052: ;
	hb_xvmPushInteger( 0 );
lab00053: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmPushInteger( 14 );
	goto lab00057;
lab00054: ;
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmPushInteger( 8 );
	goto lab00057;
lab00055: ;
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmPushInteger( 4 );
	goto lab00057;
lab00056: ;
	hb_xvmPushInteger( 0 );
lab00057: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 7 ) ) break;
lab00058: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 229 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

