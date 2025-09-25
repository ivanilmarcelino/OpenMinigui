/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_draw.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( DRAWTEXTOUT );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( ISWINDOWHANDLE );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( TEXTDRAW );
HB_FUNC_EXTERN( AADD );
HB_FUNC( DRAWLINE );
HB_FUNC_EXTERN( LINEDRAW );
HB_FUNC( DRAWRECT );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( RECTDRAW );
HB_FUNC( DRAWROUNDRECT );
HB_FUNC_EXTERN( ROUNDRECTDRAW );
HB_FUNC( DRAWELLIPSE );
HB_FUNC_EXTERN( ELLIPSEDRAW );
HB_FUNC( DRAWARC );
HB_FUNC_EXTERN( ARCDRAW );
HB_FUNC( DRAWPIE );
HB_FUNC_EXTERN( PIEDRAW );
HB_FUNC( DRAWPOLYGON );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( POLYGONDRAW );
HB_FUNC( DRAWPOLYBEZIER );
HB_FUNC_EXTERN( POLYBEZIERDRAW );
HB_FUNC( HMG_DRAWICON );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( DRAWICONEX );
HB_FUNC_EXTERN( LOADICONBYNAME );
HB_FUNC( HMG_DRAWSYSICON );
HB_FUNC_EXTERN( GETSYSTEMFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( EXTRACTICON );
HB_FUNC( ERASEWINDOW );
HB_FUNC_EXTERN( ASIZE );
HB_FUNC_EXTERN( REDRAWWINDOW );
HB_FUNC( DRAWWINDOWBOXIN );
HB_FUNC_EXTERN( GETDC );
HB_FUNC_EXTERN( WNDBOXIN );
HB_FUNC_EXTERN( RELEASEDC );
HB_FUNC( DRAWWINDOWBOXRAISED );
HB_FUNC_EXTERN( WNDBOXRAISED );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_DRAW )
{ "DRAWTEXTOUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWTEXTOUT )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ISWINDOWHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWHANDLE )}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "TEXTDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( TEXTDRAW )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "DRAWLINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWLINE )}, NULL },
{ "LINEDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( LINEDRAW )}, NULL },
{ "DRAWRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWRECT )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "RECTDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( RECTDRAW )}, NULL },
{ "DRAWROUNDRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWROUNDRECT )}, NULL },
{ "ROUNDRECTDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ROUNDRECTDRAW )}, NULL },
{ "DRAWELLIPSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWELLIPSE )}, NULL },
{ "ELLIPSEDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ELLIPSEDRAW )}, NULL },
{ "DRAWARC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWARC )}, NULL },
{ "ARCDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ARCDRAW )}, NULL },
{ "DRAWPIE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWPIE )}, NULL },
{ "PIEDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( PIEDRAW )}, NULL },
{ "DRAWPOLYGON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWPOLYGON )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "POLYGONDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( POLYGONDRAW )}, NULL },
{ "DRAWPOLYBEZIER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWPOLYBEZIER )}, NULL },
{ "POLYBEZIERDRAW", {HB_FS_PUBLIC}, {HB_FUNCNAME( POLYBEZIERDRAW )}, NULL },
{ "HMG_DRAWICON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DRAWICON )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "DRAWICONEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( DRAWICONEX )}, NULL },
{ "LOADICONBYNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOADICONBYNAME )}, NULL },
{ "HMG_DRAWSYSICON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_DRAWSYSICON )}, NULL },
{ "GETSYSTEMFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSTEMFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "EXTRACTICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( EXTRACTICON )}, NULL },
{ "ERASEWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ERASEWINDOW )}, NULL },
{ "ASIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASIZE )}, NULL },
{ "REDRAWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDRAWWINDOW )}, NULL },
{ "DRAWWINDOWBOXIN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWWINDOWBOXIN )}, NULL },
{ "GETDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDC )}, NULL },
{ "WNDBOXIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( WNDBOXIN )}, NULL },
{ "RELEASEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEDC )}, NULL },
{ "DRAWWINDOWBOXRAISED", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DRAWWINDOWBOXRAISED )}, NULL },
{ "WNDBOXRAISED", {HB_FS_PUBLIC}, {HB_FUNCNAME( WNDBOXRAISED )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_DRAW, "h_draw.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_DRAW
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_DRAW )
   #include "hbiniseg.h"
#endif

HB_FUNC( DRAWTEXTOUT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 15 );
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 20 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 93 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 16 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 96 );
	hb_xvmCopyLocals( 1, 16 );
lab00002: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
lab00003: ;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 17 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLocalByRef( 14 );
	if( hb_xvmDo( 8 ) ) break;
lab00004: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00005: ;
	hb_xvmPushInteger( 0 );
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 8 );
	if( hb_xvmAddInt( 4L ) ) break;
lab00007: ;
	hb_xvmLocalAdd( 18 );
	hb_xvmSetLine( 112 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmMult() ) break;
	hb_xvmLocalAdd( 19 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
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
	if( hb_xvmDo( 16 ) ) break;
	hb_xvmSetLine( 114 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 74 ] = {
			0, 0, 16, 0, 16, 0, 2, 0, 3, 0, 4, 0, 18, 0, 19, 0, 
			5, 0, 6, 0, 7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 12, 0, 
			13, 0, 14, 0, 176, 12, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 
			251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 
			243, 95, 242, 95, 241, 95, 240, 12, 16, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 119 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 7 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 153 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 38 ] = {
			0, 0, 7, 0, 8, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 176, 15, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 95, 
			250, 95, 249, 12, 7, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 161 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWRECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 198 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 202 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 8 );
lab00001: ;
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 9 ) ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 46 ] = {
			0, 0, 9, 0, 9, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 8, 0, 11, 0, 176, 18, 0, 95, 255, 95, 254, 95, 253, 95, 
			252, 95, 251, 95, 250, 95, 249, 95, 248, 95, 247, 12, 9, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 209 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWROUNDRECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 10 );
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 12 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 248 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 252 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 10 );
lab00001: ;
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 54 ] = {
			0, 0, 11, 0, 11, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 8, 0, 9, 0, 10, 0, 13, 0, 176, 20, 0, 95, 255, 95, 
			254, 95, 253, 95, 252, 95, 251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 
			246, 95, 245, 12, 11, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 259 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWELLIPSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 296 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 300 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 8 );
lab00001: ;
	hb_xvmSetLine( 303 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 9 ) ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 46 ] = {
			0, 0, 9, 0, 9, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 8, 0, 11, 0, 176, 22, 0, 95, 255, 95, 254, 95, 253, 95, 
			252, 95, 251, 95, 250, 95, 249, 95, 248, 95, 247, 12, 9, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 307 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWARC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 11 );
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 13 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 345 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 12 );
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
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 54 ] = {
			0, 0, 11, 0, 12, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 176, 24, 0, 95, 255, 95, 
			254, 95, 253, 95, 252, 95, 251, 95, 250, 95, 249, 95, 248, 95, 247, 95, 
			246, 95, 245, 12, 11, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 353 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWPIE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 12 );
	hb_xvmSetLine( 393 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 14 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 394 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 395 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 396 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 398 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 12 );
lab00001: ;
	hb_xvmSetLine( 401 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 13 );
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
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 62 ] = {
			0, 0, 13, 0, 13, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 
			7, 0, 8, 0, 9, 0, 10, 0, 11, 0, 12, 0, 15, 0, 176, 26, 
			0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 95, 250, 95, 249, 95, 
			248, 95, 247, 95, 246, 95, 245, 95, 244, 95, 243, 12, 13, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 405 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWPOLYGON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 5 );
	hb_xvmSetLine( 438 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 441 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 443 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 445 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 5 );
lab00001: ;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 32 ] = {
			1, 0, 2, 0, 10, 0, 9, 0, 176, 13, 0, 95, 255, 95, 1, 122, 
			1, 20, 2, 176, 13, 0, 95, 254, 95, 1, 92, 2, 1, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 449 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 38 ] = {
			0, 0, 7, 0, 6, 0, 9, 0, 10, 0, 3, 0, 4, 0, 5, 0, 
			8, 0, 176, 29, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 95, 
			250, 95, 249, 12, 7, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 453 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWPOLYBEZIER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 482 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 485 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 486 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 487 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	{
		static const HB_BYTE codeblock[ 32 ] = {
			1, 0, 2, 0, 7, 0, 6, 0, 176, 13, 0, 95, 255, 95, 1, 122, 
			1, 20, 2, 176, 13, 0, 95, 254, 95, 1, 92, 2, 1, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 30 ] = {
			0, 0, 5, 0, 5, 0, 6, 0, 7, 0, 3, 0, 4, 0, 176, 31, 
			0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 12, 5, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 495 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_DRAWICON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 535 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 536 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 539 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 540 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 542 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 7 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 546 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 7 );
lab00002: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 551 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 552 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 553 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 39 ] = {
			0, 0, 7, 0, 9, 0, 4, 0, 3, 0, 2, 0, 5, 0, 6, 0, 
			7, 0, 176, 36, 0, 95, 255, 95, 254, 95, 253, 95, 252, 95, 251, 95, 
			250, 95, 249, 9, 12, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 554 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 48 ] = {
			0, 0, 7, 0, 9, 0, 4, 0, 3, 0, 2, 0, 5, 0, 6, 0, 
			7, 0, 176, 36, 0, 95, 255, 95, 254, 95, 253, 176, 37, 0, 95, 252, 
			95, 251, 95, 250, 12, 3, 95, 251, 95, 250, 95, 249, 120, 12, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 560 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_DRAWSYSICON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 9 );
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 12 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 597 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 601 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 602 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 603 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 604 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 8 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 607 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 608 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	hb_xvmLocalAdd( 8 );
lab00002: ;
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 612 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "imageres.dll", 12 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 614 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 48 ] = {
			0, 0, 8, 0, 10, 0, 5, 0, 4, 0, 2, 0, 3, 0, 6, 0, 
			7, 0, 8, 0, 176, 36, 0, 95, 255, 95, 254, 95, 253, 176, 41, 0, 
			95, 252, 95, 251, 12, 2, 95, 250, 95, 249, 95, 248, 120, 12, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 619 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ERASEWINDOW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 641 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 642 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 643 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 644 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 645 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 650 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWWINDOWBOXIN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSetLine( 679 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 680 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 682 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 683 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 49 ] = {
			0, 0, 6, 0, 6, 0, 7, 0, 2, 0, 3, 0, 4, 0, 5, 0, 
			176, 47, 0, 176, 46, 0, 95, 255, 12, 1, 165, 80, 254, 95, 253, 95, 
			252, 95, 251, 95, 250, 20, 5, 176, 48, 0, 95, 255, 95, 254, 12, 2, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 687 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DRAWWINDOWBOXRAISED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSetLine( 716 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 717 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 719 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 720 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	{
		static const HB_BYTE codeblock[ 49 ] = {
			0, 0, 6, 0, 6, 0, 7, 0, 2, 0, 3, 0, 4, 0, 5, 0, 
			176, 50, 0, 176, 46, 0, 95, 255, 12, 1, 165, 80, 254, 95, 253, 95, 
			252, 95, 251, 95, 250, 20, 5, 176, 48, 0, 95, 255, 95, 254, 12, 2, 
			6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 724 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

