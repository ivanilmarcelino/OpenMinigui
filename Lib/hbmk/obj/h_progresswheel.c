/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_progresswheel.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEPROGRESSWHEEL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HMG_RGB2N );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( _DEFINEIMAGE );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( PROGRESSWHEELPAINT );
HB_FUNC( UPDATEANGLEGRADIENTBRUSH );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( LEN );
HB_FUNC( PW_GETCOLORDONEMIN );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( HB_BITAND );
HB_FUNC_EXTERN( HB_BITSHIFT );
HB_FUNC( PW_GETCOLORDONEMAX );
HB_FUNC( PW_GETCOLORREMAIN );
HB_FUNC( PW_GETCOLORINNER );
HB_FUNC( PW_SETSHOWTEXT );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( BT_CLIENTAREAINVALIDATERECT );
HB_FUNC( PW_SETCOLORDONEMIN );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( PW_SETCOLORDONEMAX );
HB_FUNC( PW_SETCOLORREMAIN );
HB_FUNC( PW_SETCOLORINNER );
HB_FUNC( PW_SETSTARTANGLE );
HB_FUNC( PW_SETMIN );
HB_FUNC( PW_SETMAX );
HB_FUNC( PW_SETPOSITION );
HB_FUNC( PW_SETINNERSIZE );
HB_FUNC( PW_SETGRADIENTMODE );
HB_FUNC_EXTERN( BT_BITMAPCREATENEW );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( BT_CREATEDC );
HB_FUNC_STATIC( ANGLEPOSITION );
HB_FUNC_STATIC( DRAWPIEINBITMAP );
HB_FUNC_STATIC( GRADIENTCOLOR );
HB_FUNC( CREATEPATTERNHBRUSH );
HB_FUNC( SETBRUSHORG );
HB_FUNC_STATIC( DRAWELLIPSEINBITMAP );
HB_FUNC( SETSTRETCHBLTMODE );
HB_FUNC_EXTERN( INT );
HB_FUNC_STATIC( DRAWTEXTINBITMAP );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( BT_DELETEDC );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( DELETEOBJECT );
HB_FUNC_EXTERN( ROUND );
HB_FUNC_EXTERN( PI );
HB_FUNC_EXTERN( COS );
HB_FUNC_EXTERN( SIN );
HB_FUNC_STATIC( BT_DRAWPIEEX );
HB_FUNC( BT_DRAW_HDC_ARCX_EX );
HB_FUNC_EXTERN( BT_DRAWFILLELLIPSE );
HB_FUNC_EXTERN( BT_DRAWTEXT );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_PROGRESSWHEEL )
{ "_DEFINEPROGRESSWHEEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEPROGRESSWHEEL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HMG_RGB2N", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_RGB2N )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "_DEFINEIMAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEIMAGE )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "PROGRESSWHEELPAINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PROGRESSWHEELPAINT )}, NULL },
{ "UPDATEANGLEGRADIENTBRUSH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( UPDATEANGLEGRADIENTBRUSH )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "PW_GETCOLORDONEMIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_GETCOLORDONEMIN )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "HB_BITAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITAND )}, NULL },
{ "HB_BITSHIFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITSHIFT )}, NULL },
{ "PW_GETCOLORDONEMAX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_GETCOLORDONEMAX )}, NULL },
{ "PW_GETCOLORREMAIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_GETCOLORREMAIN )}, NULL },
{ "PW_GETCOLORINNER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_GETCOLORINNER )}, NULL },
{ "PW_SETSHOWTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETSHOWTEXT )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "BT_CLIENTAREAINVALIDATERECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_CLIENTAREAINVALIDATERECT )}, NULL },
{ "PW_SETCOLORDONEMIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETCOLORDONEMIN )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "PW_SETCOLORDONEMAX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETCOLORDONEMAX )}, NULL },
{ "PW_SETCOLORREMAIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETCOLORREMAIN )}, NULL },
{ "PW_SETCOLORINNER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETCOLORINNER )}, NULL },
{ "PW_SETSTARTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETSTARTANGLE )}, NULL },
{ "PW_SETMIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETMIN )}, NULL },
{ "PW_SETMAX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETMAX )}, NULL },
{ "PW_SETPOSITION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETPOSITION )}, NULL },
{ "PW_SETINNERSIZE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETINNERSIZE )}, NULL },
{ "PW_SETGRADIENTMODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PW_SETGRADIENTMODE )}, NULL },
{ "BT_BITMAPCREATENEW", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BITMAPCREATENEW )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "BT_CREATEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_CREATEDC )}, NULL },
{ "ANGLEPOSITION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( ANGLEPOSITION )}, NULL },
{ "DRAWPIEINBITMAP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWPIEINBITMAP )}, NULL },
{ "GRADIENTCOLOR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GRADIENTCOLOR )}, NULL },
{ "CREATEPATTERNHBRUSH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CREATEPATTERNHBRUSH )}, NULL },
{ "SETBRUSHORG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETBRUSHORG )}, NULL },
{ "DRAWELLIPSEINBITMAP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWELLIPSEINBITMAP )}, NULL },
{ "SETSTRETCHBLTMODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETSTRETCHBLTMODE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "DRAWTEXTINBITMAP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWTEXTINBITMAP )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "BT_DELETEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DELETEDC )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL },
{ "ROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUND )}, NULL },
{ "PI", {HB_FS_PUBLIC}, {HB_FUNCNAME( PI )}, NULL },
{ "COS", {HB_FS_PUBLIC}, {HB_FUNCNAME( COS )}, NULL },
{ "SIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( SIN )}, NULL },
{ "BT_DRAWPIEEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWPIEEX )}, NULL },
{ "BT_DRAW_HDC_ARCX_EX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAW_HDC_ARCX_EX )}, NULL },
{ "BT_DRAWFILLELLIPSE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAWFILLELLIPSE )}, NULL },
{ "BT_DRAWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAWTEXT )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_PROGRESSWHEEL, "h_progresswheel.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_PROGRESSWHEEL
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_PROGRESSWHEEL )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEPROGRESSWHEEL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 17 );
	hb_xvmSetLine( 85 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 86 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00003;
lab00002: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00003: ;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 3 );
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
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "BufScale", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "BufScale", 8 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushInteger( 75 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 120 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 21 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 128 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 22 );
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushStringConst( "PROGRESSWHEEL", 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 168 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 16 ) ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 82 ] = {
			0, 0, 16, 0, 2, 0, 21, 0, 5, 0, 6, 0, 7, 0, 8, 0, 
			9, 0, 10, 0, 23, 0, 11, 0, 12, 0, 13, 0, 14, 0, 15, 0, 
			16, 0, 17, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 98, 1, 0, 93, 165, 0, 1, 95, 247, 
			1, 95, 246, 95, 245, 95, 244, 95, 243, 95, 242, 95, 241, 95, 240, 12, 
			16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 23 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_GETCOLORDONEMIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 204 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 208 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_GETCOLORDONEMAX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 214 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 217 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_GETCOLORREMAIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 230 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_GETCOLORINNER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 240 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 243 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 247 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETSHOWTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 257 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 6 ) ) break;
lab00001: ;
	hb_xvmSetLine( 262 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETCOLORDONEMIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 270 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 271 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 272 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 273 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 274 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 275 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 276 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 277 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 278 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 279 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 280 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 281 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 282 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 283 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 284 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 285 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 287 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 291 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 292 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 296 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 19, 0, 20, 0, 3, 0, 16, 0, 
			17, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 298 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 302 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETCOLORDONEMAX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 308 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 310 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 311 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 312 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 313 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 314 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 315 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 316 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 317 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 318 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 319 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 320 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 321 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 322 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 323 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 324 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 325 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 331 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 336 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 19, 0, 20, 0, 16, 0, 3, 0, 
			17, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 337 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 342 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETCOLORREMAIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 350 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 351 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 352 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 353 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 354 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 355 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 356 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 357 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 358 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 359 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 360 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 361 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 362 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 363 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 364 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 365 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 370 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 371 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 376 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 19, 0, 20, 0, 16, 0, 17, 0, 
			3, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 377 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 382 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETCOLORINNER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 388 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 390 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 391 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 392 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 393 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 394 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 395 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 396 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 397 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 398 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 399 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 400 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 401 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 402 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 403 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 404 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 405 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 410 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 411 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 416 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 19, 0, 20, 0, 16, 0, 17, 0, 
			18, 0, 3, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 422 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETSTARTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 19, 4 );
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 430 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 431 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 432 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 433 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 434 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 435 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 436 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 437 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 438 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 439 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 440 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 441 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 442 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 443 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 444 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 445 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 446 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 448 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 449 );
	hb_xvmLocalSetInt( 23, 0L );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 450 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmGreaterThenIntIs( 359L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 451 );
	hb_xvmLocalSetInt( 23, 359L );
lab00002: ;
	hb_xvmSetLine( 453 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 459 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 23, 0, 
			11, 0, 12, 0, 13, 0, 14, 0, 19, 0, 20, 0, 15, 0, 16, 0, 
			17, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 463 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETMIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 19, 4 );
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 471 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 472 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 473 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 474 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 475 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 476 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 477 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 478 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 479 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 480 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 481 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 482 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 483 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 484 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 485 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 486 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 487 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 489 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 490 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmDec() ) break;
	hb_xvmPopLocal( 23 );
lab00001: ;
	hb_xvmSetLine( 492 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 493 );
	hb_xvmCopyLocals( 23, 20 );
	hb_xvmSetLine( 494 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 495 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 496 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 502 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 22, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 20, 0, 21, 0, 16, 0, 17, 0, 
			18, 0, 19, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 503 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 506 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETMAX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 19, 4 );
	hb_xvmSetLine( 512 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 513 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 514 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 515 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 516 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 517 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 518 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 519 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 520 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 521 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 522 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 523 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 524 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 525 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 526 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 527 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 528 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 529 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 530 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 532 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 533 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 23 );
lab00001: ;
	hb_xvmSetLine( 535 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 536 );
	hb_xvmCopyLocals( 23, 21 );
	hb_xvmSetLine( 537 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 538 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 539 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 545 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 22, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 20, 0, 21, 0, 16, 0, 17, 0, 
			18, 0, 19, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 546 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 549 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETPOSITION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 19, 4 );
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 557 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 558 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 559 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 560 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 561 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 562 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 563 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 564 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 565 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 566 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 567 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 568 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 569 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 570 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 571 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 572 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 573 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 575 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 576 );
	hb_xvmCopyLocals( 20, 23 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 577 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 578 );
	hb_xvmCopyLocals( 21, 23 );
lab00002: ;
	hb_xvmSetLine( 580 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 581 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 585 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 23, 0, 11, 0, 
			12, 0, 13, 0, 14, 0, 15, 0, 20, 0, 21, 0, 16, 0, 17, 0, 
			18, 0, 19, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 586 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 589 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETINNERSIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 19, 4 );
	hb_xvmSetLine( 595 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 597 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 598 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 599 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 600 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 601 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 602 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 603 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 604 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 605 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 606 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 607 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 608 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 609 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 610 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 611 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 612 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 613 );
	hb_xvmCopyLocals( 3, 23 );
	hb_xvmSetLine( 615 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 616 );
	hb_xvmLocalSetInt( 23, 0L );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 617 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmGreaterThenIntIs( 99L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 618 );
	hb_xvmLocalSetInt( 23, 99L );
lab00002: ;
	hb_xvmSetLine( 620 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 621 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 625 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			23, 0, 12, 0, 13, 0, 14, 0, 19, 0, 20, 0, 15, 0, 16, 0, 
			17, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 626 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00003: ;
	hb_xvmSetLine( 629 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PW_SETGRADIENTMODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 18, 4 );
	hb_xvmSetLine( 635 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 636 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 637 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 638 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 639 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 640 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 641 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 642 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 643 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 644 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 645 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 646 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 647 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 648 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 649 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 650 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 651 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 652 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 654 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 655 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 660 );
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 2, 0, 8, 0, 9, 0, 10, 0, 21, 0, 11, 0, 
			12, 0, 3, 0, 13, 0, 14, 0, 19, 0, 20, 0, 15, 0, 16, 0, 
			17, 0, 18, 0, 176, 16, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 661 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 6 ) ) break;
lab00001: ;
	hb_xvmSetLine( 664 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PROGRESSWHEELPAINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 16 );
	hb_xvmSFrame( symbols + 73 );
	hb_xvmSetLine( 672 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "BufScale", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 683 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocalByRef( 20 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 686 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 687 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 21 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 689 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 21 );
lab00002: ;
	hb_xvmSetLine( 692 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 693 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 694 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 695 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 697 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 699 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 700 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 702 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 28 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 28 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 704 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 705 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 706 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 28 );
	hb_xvmSetLine( 708 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 710 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 23 );
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 713 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 23 );
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 716 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 8 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00005;
		}
		hb_stackPop();
	}
lab00007: ;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 28 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 28 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmDo( 13 ) ) break;
lab00008: ;
	hb_xvmSetLine( 724 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 729 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 200L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 200L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 200L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 200L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 731 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 732 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 733 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 734 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 736 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 737 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 738 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 739 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 741 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 743 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 8 ) ) break;
lab00009: ;
	hb_xvmSetLine( 746 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 747 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 749 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 750 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 751 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 753 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivide() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "%", 1 );
	hb_xvmLocalAdd( 9 );
lab00011: ;
	hb_xvmSetLine( 756 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 6000L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmAddInt( -6L ) ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 757 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 6000L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmAddInt( 2L ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 759 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	goto lab00013;
lab00012: ;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
lab00013: ;
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 8 ) ) break;
lab00014: ;
	hb_xvmSetLine( 762 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 764 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HBITMAP", 7 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 766 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( UPDATEANGLEGRADIENTBRUSH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 6 );
	hb_xvmSFrame( symbols + 73 );
	hb_xvmSetLine( 771 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushStringConst( "BufScale", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 781 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 782 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 783 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 784 );
	hb_xvmPushNil();
	hb_xvmPopStatic( 1 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 787 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 788 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMultEqPop() ) break;
	hb_xvmSetLine( 790 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 791 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 793 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 795 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 797 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 798 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 800 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 802 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 795 );
	if( hb_xvmLocalIncPush( 11 ) ) break;
lab00003: ;
	if( hb_xvmGreaterThenIntIs( 99L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 806 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 812 );
	hb_xvmPushLocal( 8 );
	hb_xvmPopStatic( 1 );
lab00004: ;
	hb_xvmSetLine( 815 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GRADIENTCOLOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 5 );
	hb_xvmSetLine( 826 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 828 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 829 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 830 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 832 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 255 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 834 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 835 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 836 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 838 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 839 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPopLocal( 14 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 841 );
	hb_xvmLocalSetInt( 14, 0L );
lab00002: ;
	hb_xvmSetLine( 844 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmMult() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmMult() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmMult() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 16 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( ANGLEPOSITION )
{
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSFrame( symbols + 73 );
	hb_xvmSetLine( 852 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( -90L ) ) break;
	hb_xvmPushInteger( 360 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 853 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 180L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 855 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 856 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 857 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 858 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 860 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 861 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushStatic( 2 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 863 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	hb_xvmArrayGen( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( BT_DRAWPIEEX )
{
   do {
	hb_xvmFrame( 0, 13 );
	hb_xvmSetLine( 873 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 874 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 14 ) ) break;
	hb_xvmSetLine( 876 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DRAWPIEINBITMAP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 13 );
	hb_xvmSetLine( 882 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 883 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 10 );
lab00001: ;
	hb_xvmSetLine( 885 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 886 );
	hb_xvmLocalSetInt( 11, 1L );
lab00002: ;
	hb_xvmSetLine( 888 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 889 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 12 );
lab00003: ;
	hb_xvmSetLine( 892 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 894 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DRAWELLIPSEINBITMAP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 900 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 901 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 6 );
lab00001: ;
	hb_xvmSetLine( 903 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 904 );
	hb_xvmLocalSetInt( 7, 1L );
lab00002: ;
	hb_xvmSetLine( 906 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 907 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 8 );
lab00003: ;
	hb_xvmSetLine( 910 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 912 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( DRAWTEXTINBITMAP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 918 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 8 );
lab00002: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 920 );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 922 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 9 ) ) break;
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 925 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 10 ) ) break;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 928 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 10 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 8 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 0L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 1L )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00005;
		}
		hb_stackPop();
	}
lab00007: ;
	hb_xvmSetLine( 932 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 73, 2 );
	hb_xvmSFrame( symbols + 73 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopStatic( 2 );
	/* *** END PROC *** */
   } while( 0 );
}

#line 936 "h_progresswheel.prg"

#define WINVER 0x0501  // minimum requirements: Windows XP

#include <mgdefs.h>
#include <commctrl.h>

// MiniGUI resources control
void RegisterResource( HANDLE hResource, LPCSTR szType );

HB_FUNC( SETBRUSHORG )
{
   SetBrushOrgEx( hmg_par_raw_HDC( 1 ), hb_parni( 2 ), hb_parni( 3 ), NULL );
}

HB_FUNC( SETSTRETCHBLTMODE )
{
   hmg_ret_NI( SetStretchBltMode( hmg_par_raw_HDC( 1 ), hb_parni( 2 ) ) );
}

//**********************************************************************************************************************************************
//* BT_DRAW_HDC_ARCX_EX (hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc, ColorLine, nWidthLine, ColorFill, nArcType, hBrushBitmap )
//**********************************************************************************************************************************************

// nArcType
#define BT_DRAW_ARC    0
#define BT_DRAW_CHORD  1
#define BT_DRAW_PIE    2

HB_FUNC( BT_DRAW_HDC_ARCX_EX )
{
   HDC      hDC;
   HPEN     hPen;
   HBRUSH   hBrush;
   HPEN     OldPen;
   HBRUSH   OldBrush;
   COLORREF ColorLine, ColorFill;
   INT      x1, y1, x2, y2, nWidthLine;
   INT      XStartArc, YStartArc, XEndArc, YEndArc;
   INT      nArcType;

   hDC = hmg_par_raw_HDC( 1 );
   x1  = hmg_par_INT( 2 );
   y1  = hmg_par_INT( 3 );
   x2  = hmg_par_INT( 4 );
   y2  = hmg_par_INT( 5 );

   XStartArc = hmg_par_INT( 6 );
   YStartArc = hmg_par_INT( 7 );
   XEndArc   = hmg_par_INT( 8 );
   YEndArc   = hmg_par_INT( 9 );

   ColorLine  = hmg_par_COLORREF( 10 );
   nWidthLine = hmg_par_INT( 11 );
   ColorFill  = hmg_par_COLORREF( 12 );

   nArcType = hmg_par_INT( 13 );

   hPen     = CreatePen( PS_SOLID, nWidthLine, ColorLine );
   OldPen   = ( HPEN ) SelectObject( hDC, hPen );

   if( hb_parnl( 14 ) )
      hBrush   = hmg_par_raw_HBRUSH( 14 );
   else
      hBrush   = CreateSolidBrush( ColorFill );

   OldBrush = ( HBRUSH ) SelectObject( hDC, hBrush );

   switch( nArcType )
   {
      case BT_DRAW_ARC:
         Arc( hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc );
         break;
      case BT_DRAW_CHORD:
         Chord( hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc );
         break;
      case BT_DRAW_PIE:
         Pie( hDC, x1, y1, x2, y2, XStartArc, YStartArc, XEndArc, YEndArc );
         break;
   }

   SelectObject( hDC, OldBrush );
   DeleteObject( hBrush );
   SelectObject( hDC, OldPen );
   DeleteObject( hPen );
}

HB_FUNC( CREATEPATTERNHBRUSH ) // ( hBitmap ) --> hBrush
{
   HBRUSH hBrush = CreatePatternBrush( hmg_par_raw_HBITMAP( 1 ) );

   RegisterResource( hBrush, "BRUSH" );

   hmg_ret_raw_HBRUSH( hBrush );
}

