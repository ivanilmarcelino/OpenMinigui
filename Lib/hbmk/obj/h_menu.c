/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_menu.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINEMAINMENU );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( CREATEMENU );
HB_FUNC( _DEFINEMENUPOPUP );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( CREATEPOPUPMENU );
HB_FUNC_EXTERN( APPENDMENUPOPUP );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC( _ENDMENUPOPUP );
HB_FUNC( _DEFINEMENUITEM );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( _GETID );
HB_FUNC_EXTERN( APPENDMENUSTRING );
HB_FUNC_EXTERN( MENUITEM_SETBITMAPS );
HB_FUNC_EXTERN( MENUITEM_SETICON );
HB_FUNC_EXTERN( MENUITEM_SETCHECKMARKS );
HB_FUNC_EXTERN( MENUITEM_SETFONT );
HB_FUNC_EXTERN( XCHECKMENUITEM );
HB_FUNC_EXTERN( XDISABLEMENUITEM );
HB_FUNC_EXTERN( SETMENUDEFAULTITEM );
HB_FUNC( _DEFINESEPARATOR );
HB_FUNC_EXTERN( APPENDMENUSEPARATOR );
HB_FUNC_EXTERN( _NEWMENUSTYLE );
HB_FUNC( _ENDMENU );
HB_FUNC_EXTERN( SETMENU );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( CHANGESTYLE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_STATIC( _GETMENUIDS );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( _DEFAULTMENUITEM );
HB_FUNC( _DISABLEMENUITEM );
HB_FUNC( _ENABLEMENUITEM );
HB_FUNC_EXTERN( XENABLEMENUITEM );
HB_FUNC( _CHECKMENUITEM );
HB_FUNC( _UNCHECKMENUITEM );
HB_FUNC_EXTERN( XUNCHECKMENUITEM );
HB_FUNC( _ISMENUITEMCHECKED );
HB_FUNC_EXTERN( XGETMENUCHECKSTATE );
HB_FUNC( _ISMENUITEMENABLED );
HB_FUNC_EXTERN( XGETMENUENABLEDSTATE );
HB_FUNC( _DEFINECONTEXTMENU );
HB_FUNC( _SHOWCONTEXTMENU );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( GETCURSORPOS );
HB_FUNC_EXTERN( TRACKPOPUPMENU );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC( _DEFINENOTIFYMENU );
HB_FUNC( _DEFINEDROPDOWNMENU );
HB_FUNC( _DEFINECONTROLCONTEXTMENU );
HB_FUNC( _SHOWCONTROLCONTEXTMENU );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( _GETMENUITEMCAPTION );
HB_FUNC_EXTERN( XGETMENUCAPTION );
HB_FUNC( _SETMENUITEMCAPTION );
HB_FUNC_EXTERN( XSETMENUCAPTION );
HB_FUNC( _SETMENUITEMBITMAP );
HB_FUNC( _SETMENUITEMICON );
HB_FUNC( _SETMENUITEMFONT );
HB_FUNC( _INSERTMENUITEM );
HB_FUNC_EXTERN( INSERTMENUITEM );
HB_FUNC( _MODIFYMENUITEM );
HB_FUNC_EXTERN( MODIFYMENUITEM );
HB_FUNC_EXTERN( DELETEOBJECT );
HB_FUNC( _REMOVEMENUITEM );
HB_FUNC_EXTERN( REMOVEMENUITEM );
HB_FUNC( _CHANGEMENUITEMCAPTION );
HB_FUNC( HMG_SETMENUTHEME );
HB_FUNC_EXTERN( GETMENUCOLORS );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( SETMENUCURSORTYPE );
HB_FUNC_EXTERN( SETMENUSEPARATORTYPE );
HB_FUNC_EXTERN( SETMENUSELECTEDITEM3D );
HB_FUNC_EXTERN( SETMENUCOLORS );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _COLORMENU );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_MENU )
{ "_DEFINEMAINMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMAINMENU )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "CREATEMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEMENU )}, NULL },
{ "_DEFINEMENUPOPUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMENUPOPUP )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "CREATEPOPUPMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEPOPUPMENU )}, NULL },
{ "APPENDMENUPOPUP", {HB_FS_PUBLIC}, {HB_FUNCNAME( APPENDMENUPOPUP )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ENDMENUPOPUP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDMENUPOPUP )}, NULL },
{ "_DEFINEMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMENUITEM )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETID )}, NULL },
{ "APPENDMENUSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( APPENDMENUSTRING )}, NULL },
{ "MENUITEM_SETBITMAPS", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENUITEM_SETBITMAPS )}, NULL },
{ "MENUITEM_SETICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENUITEM_SETICON )}, NULL },
{ "MENUITEM_SETCHECKMARKS", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENUITEM_SETCHECKMARKS )}, NULL },
{ "MENUITEM_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MENUITEM_SETFONT )}, NULL },
{ "XCHECKMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( XCHECKMENUITEM )}, NULL },
{ "XDISABLEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( XDISABLEMENUITEM )}, NULL },
{ "SETMENUDEFAULTITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENUDEFAULTITEM )}, NULL },
{ "_DEFINESEPARATOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINESEPARATOR )}, NULL },
{ "APPENDMENUSEPARATOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( APPENDMENUSEPARATOR )}, NULL },
{ "_NEWMENUSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _NEWMENUSTYLE )}, NULL },
{ "_ENDMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDMENU )}, NULL },
{ "SETMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENU )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "CHANGESTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHANGESTYLE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETMENUIDS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETMENUIDS )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_DEFAULTMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFAULTMENUITEM )}, NULL },
{ "_DISABLEMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DISABLEMENUITEM )}, NULL },
{ "_ENABLEMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENABLEMENUITEM )}, NULL },
{ "XENABLEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( XENABLEMENUITEM )}, NULL },
{ "_CHECKMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CHECKMENUITEM )}, NULL },
{ "_UNCHECKMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _UNCHECKMENUITEM )}, NULL },
{ "XUNCHECKMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( XUNCHECKMENUITEM )}, NULL },
{ "_ISMENUITEMCHECKED", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ISMENUITEMCHECKED )}, NULL },
{ "XGETMENUCHECKSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( XGETMENUCHECKSTATE )}, NULL },
{ "_ISMENUITEMENABLED", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ISMENUITEMENABLED )}, NULL },
{ "XGETMENUENABLEDSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( XGETMENUENABLEDSTATE )}, NULL },
{ "_DEFINECONTEXTMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECONTEXTMENU )}, NULL },
{ "_SHOWCONTEXTMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SHOWCONTEXTMENU )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "GETCURSORPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORPOS )}, NULL },
{ "TRACKPOPUPMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRACKPOPUPMENU )}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "_DEFINENOTIFYMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINENOTIFYMENU )}, NULL },
{ "_DEFINEDROPDOWNMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEDROPDOWNMENU )}, NULL },
{ "_DEFINECONTROLCONTEXTMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECONTROLCONTEXTMENU )}, NULL },
{ "_SHOWCONTROLCONTEXTMENU", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SHOWCONTROLCONTEXTMENU )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "_GETMENUITEMCAPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETMENUITEMCAPTION )}, NULL },
{ "XGETMENUCAPTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( XGETMENUCAPTION )}, NULL },
{ "_SETMENUITEMCAPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETMENUITEMCAPTION )}, NULL },
{ "XSETMENUCAPTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( XSETMENUCAPTION )}, NULL },
{ "_SETMENUITEMBITMAP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETMENUITEMBITMAP )}, NULL },
{ "_SETMENUITEMICON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETMENUITEMICON )}, NULL },
{ "_SETMENUITEMFONT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETMENUITEMFONT )}, NULL },
{ "_INSERTMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _INSERTMENUITEM )}, NULL },
{ "INSERTMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSERTMENUITEM )}, NULL },
{ "_MODIFYMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MODIFYMENUITEM )}, NULL },
{ "MODIFYMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( MODIFYMENUITEM )}, NULL },
{ "DELETEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DELETEOBJECT )}, NULL },
{ "_REMOVEMENUITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _REMOVEMENUITEM )}, NULL },
{ "REMOVEMENUITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( REMOVEMENUITEM )}, NULL },
{ "_CHANGEMENUITEMCAPTION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CHANGEMENUITEMCAPTION )}, NULL },
{ "HMG_SETMENUTHEME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_SETMENUTHEME )}, NULL },
{ "GETMENUCOLORS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETMENUCOLORS )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "SETMENUCURSORTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENUCURSORTYPE )}, NULL },
{ "SETMENUSEPARATORTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENUSEPARATORTYPE )}, NULL },
{ "SETMENUSELECTEDITEM3D", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENUSELECTEDITEM3D )}, NULL },
{ "SETMENUCOLORS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETMENUCOLORS )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_COLORMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( _COLORMENU )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_MENU, "h_menu.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_MENU
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_MENU )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINEMAINMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 57 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 58 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 61 );
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 185L ) ) break;
	hb_xvmSetLine( 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 191L ) ) break;
	hb_xvmSetLine( 65 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 187L ) ) break;
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 186L ) ) break;
	hb_xvmSetLine( 69 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 188L ) ) break;
	hb_xvmSetLine( 71 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEMENUPOPUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 4 );
	hb_xvmSFrame( symbols + 95 );
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN,CONTEXT,OWNCONTEXT,NOTIFY,DROPDOWN", 39 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 87 );
	goto lab00012;
lab00002: ;
	hb_xvmSetLine( 91 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 4 );
	hb_xvmPopStatic( 1 );
lab00003: ;
	hb_xvmSetLine( 99 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	hb_xvmPushInteger( 188 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 190L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 104 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 190L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
lab00004: ;
	goto lab00013;
lab00005: ;
	hb_xvmSetLine( 118 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 121 );
	hb_xvmLocalSetInt( 6, 3L );
	goto lab00010;
lab00007: ;
	hb_xvmSetLine( 125 );
	hb_xvmLocalSetInt( 6, 4L );
	goto lab00010;
lab00008: ;
	hb_xvmSetLine( 130 );
	hb_xvmLocalSetInt( 6, 5L );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 5 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00006;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00007;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "O", 1 ) )
		{
			hb_stackPop();
			goto lab00008;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00008;
		}
		hb_stackPop();
	}
lab00010: ;
	hb_xvmSetLine( 134 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 137 );
	hb_xvmPushLocal( 4 );
	hb_xvmPopStatic( 2 );
lab00011: ;
	hb_xvmSetLine( 142 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	hb_xvmPushInteger( 194 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 196L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 196L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushLocal( 5 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "O", 1 ) )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00005;
		}
		hb_stackPop();
	}
lab00013: ;
	hb_xvmSetLine( 157 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 191L ) ) break;
	goto lab00015;
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 197L ) ) break;
lab00015: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 162 );
	hb_xvmPushStringConst( "DummyPopupName", 14 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 2 );
lab00016: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 168 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00017: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushStringConst( "POPUP", 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 179 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	goto lab00019;
lab00018: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
lab00019: ;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 184 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 187 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 188 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 190 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 191 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 192 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 193 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 194 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 195 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 215 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 186L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	goto lab00021;
lab00020: ;
	hb_xvmSetLine( 220 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 221 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 193L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 223 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
lab00021: ;
	hb_xvmSetLine( 226 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Menu type incorrect.", 20 );
	if( hb_xvmDo( 1 ) ) break;
lab00023: ;
	hb_xvmSetLine( 236 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDMENUPOPUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSFrame( symbols + 95 );
	hb_xvmSetLine( 243 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 245 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	hb_xvmPushInteger( 188 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmDecEqPop() ) break;
	hb_xvmSetLine( 247 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 186L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 190L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 254 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "CONTEXT,OWNCONTEXT,NOTIFY,DROPDOWN", 34 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 256 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	hb_xvmPushInteger( 194 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmDecEqPop() ) break;
	hb_xvmSetLine( 258 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 260 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 262 );
	hb_xvmLocalSetInt( 1, 3L );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 265 );
	hb_xvmLocalSetInt( 1, 4L );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 269 );
	hb_xvmLocalSetInt( 1, 5L );
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "O", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		hb_stackPop();
	}
lab00006: ;
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 196L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Menu type incorrect.", 20 );
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 283 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEMENUITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 13 );
	hb_xvmSetLine( 289 );
	hb_xvmLocalSetInt( 16, 0L );
	hb_xvmSetLine( 293 );
	hb_xvmLocalSetInt( 20, 6L );
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 296 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
lab00001: ;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 302 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 305 );
	hb_xvmLocalSetInt( 20, 1L );
	hb_xvmSetLine( 306 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 307 );
	hb_xvmLocalSetInt( 20, 2L );
lab00002: ;
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 313 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 315 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 316 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 320 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 321 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 16 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 16 );
lab00005: ;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 3 ) ) break;
lab00007: ;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 336 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 338 );
	hb_xvmPushStringConst( "DummyMenuName", 13 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 3 );
lab00008: ;
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 344 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 191L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmLocalAdd( 17 );
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 352 );
	hb_xvmPushStringConst( "MENU", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 353 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 354 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 355 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 356 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 357 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 358 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 359 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 360 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 361 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 362 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 363 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 364 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 365 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 366 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 367 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 368 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 370 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 373 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 374 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 375 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 376 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 377 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 379 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 380 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 381 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 382 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 383 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 384 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 386 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 388 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 390 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 393 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 394 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00010: ;
	hb_xvmSetLine( 397 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 401 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 403 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
lab00012: ;
	hb_xvmSetLine( 406 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00013: ;
	hb_xvmSetLine( 412 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 413 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 414 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 20 );
	goto lab00015;
lab00014: ;
	hb_xvmPushInteger( 7 );
lab00015: ;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00019;
lab00016: ;
	hb_xvmSetLine( 417 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushLocal( 20 );
	goto lab00018;
lab00017: ;
	hb_xvmPushInteger( 8 );
lab00018: ;
	if( hb_xvmDo( 4 ) ) break;
lab00019: ;
	hb_xvmSetLine( 420 );
	hb_xvmCopyLocals( 15, 14 );
lab00020: ;
	hb_xvmSetLine( 423 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 16 );
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 16 );
lab00022: ;
	hb_xvmSetLine( 429 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 430 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00023: ;
	hb_xvmSetLine( 433 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 434 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 3 ) ) break;
lab00024: ;
	hb_xvmSetLine( 437 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 439 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 441 );
	hb_xvmPushStringConst( "DummyMenuName", 13 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 3 );
lab00025: ;
	hb_xvmSetLine( 446 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00026;
	hb_xvmSetLine( 447 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 197L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmLocalAdd( 17 );
	hb_xvmSetLine( 449 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 2 ) ) break;
lab00026: ;
	hb_xvmSetLine( 455 );
	hb_xvmPushStringConst( "MENU", 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 456 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 457 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 458 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 193L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 459 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 461 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 462 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 463 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 464 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 465 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 466 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 467 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 468 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 470 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 471 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 472 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 474 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 475 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 476 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 477 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 478 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 480 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 482 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 484 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 485 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 486 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 487 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 488 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 489 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 490 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 491 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 493 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 494 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 496 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 497 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00027: ;
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
lab00028: ;
	hb_xvmSetLine( 504 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 505 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
lab00029: ;
	hb_xvmSetLine( 509 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 510 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 515 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINESEPARATOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 521 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushStringConst( "MAIN", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 523 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 189L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 188L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 527 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 529 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 195L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 194L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 539 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 540 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 543 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 0 );
	hb_xvmSetLine( 551 );
	goto lab00013;
lab00001: ;
	hb_xvmSetLine( 555 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 186L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00014;
lab00002: ;
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 197L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 561 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 101L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	goto lab00014;
lab00003: ;
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 197L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 567 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
	goto lab00014;
lab00004: ;
	hb_xvmSetLine( 572 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	if( hb_xvmArrayPop() ) break;
	goto lab00014;
lab00005: ;
	hb_xvmSetLine( 577 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 579 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 581 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 583 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 584 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 41 ] = {
			1, 0, 1, 0, 2, 0, 176, 38, 0, 98, 1, 0, 92, 86, 1, 95, 
			1, 98, 1, 0, 93, 192, 0, 1, 98, 1, 0, 93, 198, 0, 1, 95, 
			255, 1, 120, 4, 4, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 586 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 587 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "IMAGE,LABEL", 11 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 588 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 256 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 579 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	goto lab00014;
lab00010: ;
	hb_xvmSetLine( 596 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 36 ] = {
			1, 0, 0, 0, 176, 38, 0, 98, 1, 0, 92, 86, 1, 95, 1, 98, 
			1, 0, 93, 192, 0, 1, 98, 1, 0, 93, 198, 0, 1, 120, 4, 4, 
			0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 601 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 198L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	goto lab00014;
lab00013: ;
	hb_xvmPushFuncSymbol( symbols + 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 185L ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "M", 1 ) )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "C", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "D", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "O", 1 ) )
		{
			hb_stackPop();
			goto lab00005;
		}
		hb_stackPop();
	}
lab00014: ;
	hb_xvmSetLine( 608 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
lab00015: ;
	hb_xvmSetLine( 610 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 612 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "POPUP", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 614 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 616 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPop() ) break;
lab00016: ;
	hb_xvmSetLine( 622 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
lab00017: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 624 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _GETMENUIDS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 635 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 637 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "MENU", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 639 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 641 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "POPUP", 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 643 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 649 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFAULTMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 654 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 658 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DISABLEMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 665 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 667 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENABLEMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 672 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 676 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _CHECKMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 683 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 685 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _UNCHECKMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 690 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 694 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ISMENUITEMCHECKED )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 699 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 701 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ISMENUITEMENABLED )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 706 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 708 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINECONTEXTMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 714 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 715 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 716 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 194L ) ) break;
	hb_xvmSetLine( 717 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 719 );
	hb_xvmPushStringConst( "CONTEXT", 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 185L ) ) break;
	hb_xvmSetLine( 721 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 188L ) ) break;
	hb_xvmSetLine( 723 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 724 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 727 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 728 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 729 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 731 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SHOWCONTEXTMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 739 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 741 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 742 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 193L ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 747 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushStringConst( "Context Menu is not defined.", 28 );
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 752 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 753 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 754 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 757 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 192L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 758 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 760 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINENOTIFYMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 766 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 767 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 768 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 194L ) ) break;
	hb_xvmSetLine( 769 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 771 );
	hb_xvmPushStringConst( "NOTIFY", 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 185L ) ) break;
	hb_xvmSetLine( 773 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 188L ) ) break;
	hb_xvmSetLine( 775 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 776 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 779 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 781 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 783 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEDROPDOWNMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 789 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 790 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 791 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 194L ) ) break;
	hb_xvmSetLine( 792 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 794 );
	hb_xvmPushStringConst( "DROPDOWN", 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 185L ) ) break;
	hb_xvmSetLine( 796 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 188L ) ) break;
	hb_xvmSetLine( 798 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 799 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 802 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 198L ) ) break;
	hb_xvmSetLine( 803 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 804 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 805 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 807 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINECONTROLCONTEXTMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 814 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 815 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 816 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 194L ) ) break;
	hb_xvmSetLine( 817 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 819 );
	hb_xvmPushStringConst( "OWNCONTEXT", 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 185L ) ) break;
	hb_xvmSetLine( 821 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 188L ) ) break;
	hb_xvmSetLine( 823 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 824 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 827 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 828 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 198L ) ) break;
	hb_xvmSetLine( 829 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 28 ] = {
			1, 0, 1, 0, 2, 0, 176, 38, 0, 98, 1, 0, 93, 198, 0, 1, 
			176, 43, 0, 95, 1, 95, 255, 12, 2, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 831 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 198L ) ) break;
lab00003: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 193L ) ) break;
	hb_xvmSetLine( 835 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 197L ) ) break;
	hb_xvmSetLine( 836 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 192L ) ) break;
	hb_xvmSetLine( 838 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SHOWCONTROLCONTEXTMENU )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 843 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 846 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 847 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00005;
lab00001: ;
	hb_xvmSetLine( 848 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 849 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 850 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00003: ;
	hb_xvmSetLine( 848 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00004: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 847 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	goto lab00010;
lab00006: ;
	hb_xvmSetLine( 855 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 856 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 857 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00008: ;
	hb_xvmSetLine( 855 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00010: ;
	hb_xvmSetLine( 862 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETMENUITEMCAPTION )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 868 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 870 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETMENUITEMCAPTION )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 875 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 877 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETMENUITEMBITMAP )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 882 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 884 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 886 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETMENUITEMICON )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 891 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 893 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 895 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETMENUITEMFONT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 900 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 902 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _INSERTMENUITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 6 );
	hb_xvmSetLine( 907 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 908 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 909 );
	hb_xvmLocalSetInt( 9, 0L );
	hb_xvmSetLine( 913 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 915 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 916 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 191L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 10 );
	hb_xvmSetLine( 918 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 923 );
	hb_xvmPushStringConst( "_MenuDummyVar", 13 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 925 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 931 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 933 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 934 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 9 );
lab00003: ;
	hb_xvmSetLine( 937 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushStringConst( "MENU", 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 938 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 939 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 940 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 187L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 941 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 942 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 943 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 944 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 945 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 946 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 947 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 948 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 949 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 950 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 951 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 952 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 953 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 954 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 955 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 956 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 957 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 958 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 959 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 960 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 961 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 962 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 963 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 964 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 965 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 966 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 967 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 968 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 969 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 970 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 971 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 972 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 973 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 974 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 975 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 976 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 978 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 979 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 982 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MODIFYMENUITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 6 );
	hb_xvmSetLine( 987 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 988 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 992 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 994 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 995 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 191L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 9 );
	hb_xvmSetLine( 997 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1002 );
	hb_xvmPushStringConst( "_MenuDummyVar", 13 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1004 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1010 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1012 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1013 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1014 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
lab00003: ;
	hb_xvmSetLine( 1017 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1018 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1019 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1021 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _REMOVEMENUITEM )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 1026 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1028 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _CHANGEMENUITEMCAPTION )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1035 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1037 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_SETMENUTHEME )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 1042 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1044 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1046 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmLessThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1047 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 1050 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1051 );
	hb_xvmPushInteger( 24 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 1054 );
	goto lab00016;
lab00003: ;
	hb_xvmSetLine( 1058 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1059 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1060 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1061 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1062 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 12632256 );
#else
	hb_xvmPushLong( 12632256L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1063 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16317695 );
#else
	hb_xvmPushLong( 16317695L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1064 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 7636360 );
#else
	hb_xvmPushLong( 7636360L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1066 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1067 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1068 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 12632256 );
#else
	hb_xvmPushLong( 12632256L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1069 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1070 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1071 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13811126 );
#else
	hb_xvmPushLong( 13811126L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 1072 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13811126 );
#else
	hb_xvmPushLong( 13811126L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 1073 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 1074 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 1076 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16053750 );
#else
	hb_xvmPushLong( 16053750L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1077 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13161167 );
#else
	hb_xvmPushLong( 13161167L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 1079 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 10725800 );
#else
	hb_xvmPushLong( 10725800L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 1080 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1082 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1083 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 1084 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 1085 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmSetLine( 1087 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1088 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1089 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00017;
lab00004: ;
	hb_xvmSetLine( 1095 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1096 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1097 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1098 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1099 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1100 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1101 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1103 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1104 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1105 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1106 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00006;
lab00005: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
lab00006: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1107 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00008;
lab00007: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
lab00008: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1108 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 1109 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 1110 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00010;
lab00009: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
lab00010: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 1111 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00012;
lab00011: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
lab00012: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 1113 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1114 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 1116 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 1117 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1119 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1120 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 1121 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 1122 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmSetLine( 1124 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1125 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1126 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00017;
lab00013: ;
	hb_xvmSetLine( 1132 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1133 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1134 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1135 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1136 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1137 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1138 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1140 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1141 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1142 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1143 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1144 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1145 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 1146 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 1147 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 1148 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 1150 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1151 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 13160660 );
#else
	hb_xvmPushLong( 13160660L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 1153 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 1154 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1156 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1157 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 1158 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 6956042 );
#else
	hb_xvmPushLong( 6956042L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 1159 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmSetLine( 1161 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1162 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1163 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00017;
lab00014: ;
	hb_xvmSetLine( 1169 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1170 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1171 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 15592941 );
#else
	hb_xvmPushLong( 15592941L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1172 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1173 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1174 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4276545 );
#else
	hb_xvmPushLong( 4276545L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1175 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4276545 );
#else
	hb_xvmPushLong( 4276545L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1177 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 15592941 );
#else
	hb_xvmPushLong( 15592941L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1178 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 16777215 );
#else
	hb_xvmPushLong( 16777215L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1179 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1180 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1181 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1182 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4276545 );
#else
	hb_xvmPushLong( 4276545L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 1183 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4276545 );
#else
	hb_xvmPushLong( 4276545L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 1184 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 1185 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 1187 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1188 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2829099 );
#else
	hb_xvmPushLong( 2829099L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 1190 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 1191 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1193 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4934475 );
#else
	hb_xvmPushLong( 4934475L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1194 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8421504 );
#else
	hb_xvmPushLong( 8421504L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 1195 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 4934475 );
#else
	hb_xvmPushLong( 4934475L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 1196 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 15592941 );
#else
	hb_xvmPushLong( 15592941L );
#endif
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmSetLine( 1198 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1199 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1200 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00017;
lab00015: ;
	hb_xvmSetLine( 1206 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 1207 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 1208 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 1209 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 1210 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 1211 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1212 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1215 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 1216 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 1217 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 11L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 1218 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 12L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 1219 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 1220 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 14L ) ) break;
	hb_xvmSetLine( 1221 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 15L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 1222 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 16L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 16L ) ) break;
	hb_xvmSetLine( 1224 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 17L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1225 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 1227 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 1228 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1230 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 21L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1231 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 1232 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 23L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 1233 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 24L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 24L ) ) break;
	hb_xvmSetLine( 1235 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1236 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1237 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmPushLocal( 1 );
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
			goto lab00013;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00014;
		}
		{
			hb_stackPop();
			goto lab00015;
		}
	}
lab00017: ;
	hb_xvmSetLine( 1241 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1243 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 1244 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "BackColor", 9 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 1245 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00018: ;
	hb_xvmSetLine( 1248 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 95, 2 );
	/* *** END PROC *** */
   } while( 0 );
}

