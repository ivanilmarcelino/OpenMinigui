/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_richeditbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINERICHEDITBOX );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITRICHEDITBOX );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( SETFONTRTF );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( SETBKGNDCOLOR );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( STREAMIN );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _SETVALUE );
HB_FUNC( _DATABASERICHEDITBOXSAVE );
HB_FUNC_EXTERN( TEMPFILE );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( _DATARICHEDITBOXSAVE );
HB_FUNC_EXTERN( _ISFIELDEXISTS );
HB_FUNC_EXTERN( MEMOREAD );
HB_FUNC_EXTERN( FERASE );
HB_FUNC( _DATARICHEDITBOXSETVALUE );
HB_FUNC_EXTERN( HB_MEMOWRIT );
HB_FUNC( _DATARICHEDITBOXOPEN );
HB_FUNC( _DATARICHEDITBOXGETVALUE );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( STREAMOUT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_RICHEDITBOX )
{ "_DEFINERICHEDITBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINERICHEDITBOX )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITRICHEDITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITRICHEDITBOX )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "SETFONTRTF", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFONTRTF )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "SETBKGNDCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETBKGNDCOLOR )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "STREAMIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STREAMIN )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_SETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETVALUE )}, NULL },
{ "_DATABASERICHEDITBOXSAVE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATABASERICHEDITBOXSAVE )}, NULL },
{ "TEMPFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TEMPFILE )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_DATARICHEDITBOXSAVE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATARICHEDITBOXSAVE )}, NULL },
{ "_ISFIELDEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISFIELDEXISTS )}, NULL },
{ "MEMOREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOREAD )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "_DATARICHEDITBOXSETVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATARICHEDITBOXSETVALUE )}, NULL },
{ "HB_MEMOWRIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MEMOWRIT )}, NULL },
{ "_DATARICHEDITBOXOPEN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATARICHEDITBOXOPEN )}, NULL },
{ "_DATARICHEDITBOXGETVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATARICHEDITBOXGETVALUE )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "STREAMOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( STREAMOUT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_RICHEDITBOX, "h_richeditbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_RICHEDITBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_RICHEDITBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINERICHEDITBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 32 );
	hb_xvmSetLine( 65 );
	hb_xvmLocalSetInt( 38, 0L );
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 240 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 71 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 11 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 64738 );
#else
	hb_xvmPushLong( 64738L );
#endif
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 29 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 28 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 89 );
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
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 39 );
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 93 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00003: ;
	hb_xvmSetLine( 98 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 99 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 104 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 105 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 107 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00005: ;
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 7 );
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
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 12 );
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
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00009: ;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 119 );
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
lab00010: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 36 );
	hb_xvmSetLine( 124 );
	hb_xvmCopyLocals( 2, 35 );
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 128 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
lab00011: ;
	hb_xvmSetLine( 130 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 131 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
lab00012: ;
	hb_xvmSetLine( 134 );
	hb_xvmPushStringConst( "RICHEDIT", 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 140 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 34 );
lab00014: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 150 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 38 );
	goto lab00022;
lab00015: ;
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 14 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 158 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 34 );
lab00017: ;
	hb_xvmSetLine( 166 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushInteger( -1 );
	goto lab00019;
lab00018: ;
	hb_xvmPushInteger( 0 );
lab00019: ;
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	goto lab00021;
lab00020: ;
	hb_xvmPushNil();
lab00021: ;
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 9 ) ) break;
lab00022: ;
	hb_xvmSetLine( 170 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 2 ) ) break;
lab00023: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00024: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 5 ) ) break;
lab00025: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushInteger( 1 );
	goto lab00027;
lab00026: ;
	hb_xvmPushInteger( 2 );
lab00027: ;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 24 );
lab00029: ;
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushStringConst( "RICHEDIT", 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 197 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 201 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 203 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 205 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 212 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 217 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 218 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00031;
lab00030: ;
	hb_xvmPushInteger( -1 );
lab00031: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 219 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00033;
lab00032: ;
	hb_xvmPushInteger( -1 );
lab00033: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 220 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00035;
lab00034: ;
	hb_xvmPushLogical( HB_TRUE );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 238 );
	hb_xvmPushSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00036: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00037;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 4 ) ) break;
lab00037: ;
	hb_xvmSetLine( 245 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATABASERICHEDITBOXSAVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "txt", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 255 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 258 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "FIELD", 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMacroPopAliased( 43 ) ) break;
lab00001: ;
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 269 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATARICHEDITBOXSETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "txt", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 278 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATARICHEDITBOXGETVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 293 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "txt", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 296 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATARICHEDITBOXOPEN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 313 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00001: ;
	hb_xvmSetLine( 319 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATARICHEDITBOXSAVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 328 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 329 );
	if( hb_xvmPushMemvar( symbols + 10 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00002: ;
	hb_xvmSetLine( 336 );
	/* *** END PROC *** */
   } while( 0 );
}

