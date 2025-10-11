/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_textbox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINETEXTBOX );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ULEFT );
HB_FUNC_EXTERN( SELECT );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( LEN );
HB_FUNC( INITDIALOGTEXTBOX );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITTEXTBOX );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( SENDMESSAGEWIDESTRING );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC( _DEFINEMASKEDTEXTBOX );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( TRANSFORM );
HB_FUNC( INITDIALOGMASKEDTEXTBOX );
HB_FUNC_EXTERN( INITMASKEDTEXTBOX );
HB_FUNC_STATIC( GETNUMMASK );
HB_FUNC_EXTERN( DTOC );
HB_FUNC( GETNUMFROMTEXT );
HB_FUNC_EXTERN( _SETTYPE );
HB_FUNC_EXTERN( HMG_ISDIGIT );
HB_FUNC_EXTERN( HB_URIGHT );
HB_FUNC_EXTERN( VAL );
HB_FUNC( _DEFINECHARMASKTEXTBOX );
HB_FUNC_EXTERN( CTOD );
HB_FUNC_EXTERN( SET );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( INITCHARMASKTEXTBOX );
HB_FUNC( PROCESSCHARMASK );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( GETWINDOWTEXT );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC_EXTERN( HB_USUBSTR );
HB_FUNC_EXTERN( HB_ULEN );
HB_FUNC_STATIC( CHARMASKTEKSTOK );
HB_FUNC_EXTERN( HMG_ISALPHA );
HB_FUNC_EXTERN( HMG_UPPER );
HB_FUNC_EXTERN( MIN );
HB_FUNC( _DATATEXTBOXREFRESH );
HB_FUNC_EXTERN( _SETVALUE );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( RTRIM );
HB_FUNC_EXTERN( REDRAWWINDOW );
HB_FUNC( _DATATEXTBOXSAVE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( _ISFIELDEXISTS );
HB_FUNC_EXTERN( _GETVALUE );
HB_FUNC( PROCESSNUMTEXT );
HB_FUNC_EXTERN( ISDIGIT );
HB_FUNC( GETNUMFROMTEXTSP );
HB_FUNC( OEDITEVENTS );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( ISINSERTACTIVE );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( TRACKPOPUPMENU );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_TEXTBOX )
{ "_DEFINETEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINETEXTBOX )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ULEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEFT )}, NULL },
{ "SELECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SELECT )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "INITDIALOGTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGTEXTBOX )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITTEXTBOX )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "SENDMESSAGEWIDESTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGEWIDESTRING )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "_DEFINEMASKEDTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEMASKEDTEXTBOX )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "TRANSFORM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRANSFORM )}, NULL },
{ "INITDIALOGMASKEDTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGMASKEDTEXTBOX )}, NULL },
{ "INITMASKEDTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITMASKEDTEXTBOX )}, NULL },
{ "GETNUMMASK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETNUMMASK )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "GETNUMFROMTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETNUMFROMTEXT )}, NULL },
{ "_SETTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTYPE )}, NULL },
{ "HMG_ISDIGIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_ISDIGIT )}, NULL },
{ "HB_URIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_URIGHT )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "_DEFINECHARMASKTEXTBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECHARMASKTEXTBOX )}, NULL },
{ "CTOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( CTOD )}, NULL },
{ "SET", {HB_FS_PUBLIC}, {HB_FUNCNAME( SET )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "INITCHARMASKTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITCHARMASKTEXTBOX )}, NULL },
{ "PROCESSCHARMASK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PROCESSCHARMASK )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "GETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWTEXT )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_USUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USUBSTR )}, NULL },
{ "HB_ULEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ULEN )}, NULL },
{ "CHARMASKTEKSTOK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CHARMASKTEKSTOK )}, NULL },
{ "HMG_ISALPHA", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_ISALPHA )}, NULL },
{ "HMG_UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_UPPER )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "_DATATEXTBOXREFRESH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATATEXTBOXREFRESH )}, NULL },
{ "_SETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETVALUE )}, NULL },
{ "TYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TYPE )}, NULL },
{ "RTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( RTRIM )}, NULL },
{ "REDRAWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( REDRAWWINDOW )}, NULL },
{ "_DATATEXTBOXSAVE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DATATEXTBOXSAVE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "_ISFIELDEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISFIELDEXISTS )}, NULL },
{ "_GETVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETVALUE )}, NULL },
{ "PROCESSNUMTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( PROCESSNUMTEXT )}, NULL },
{ "ISDIGIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISDIGIT )}, NULL },
{ "GETNUMFROMTEXTSP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETNUMFROMTEXTSP )}, NULL },
{ "OEDITEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( OEDITEVENTS )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "ISINSERTACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISINSERTACTIVE )}, NULL },
{ "CHR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHR )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "TRACKPOPUPMENU", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRACKPOPUPMENU )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_TEXTBOX, "h_textbox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_TEXTBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_TEXTBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINETEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 35 );
	hb_xvmSetLine( 72 );
	hb_xvmLocalSetInt( 37, 0L );
	hb_xvmSetLine( 81 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 17 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 16 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushInteger( 255 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 94 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 100 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00002: ;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 38 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 23 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushLocalByRef( 26 );
	if( hb_xvmDo( 7 ) ) break;
lab00003: ;
	hb_xvmSetLine( 109 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00004: ;
	hb_xvmSetLine( 110 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00006;
lab00005: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00006: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 115 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 118 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00008: ;
	hb_xvmSetLine( 120 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 11 );
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
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00012: ;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00013: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 39 );
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 139 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 140 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 142 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073774720 );
#else
	hb_xvmPushLong( 1073774720L );
#endif
	hb_xvmPushLocal( 32 );
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
	hb_xvmLocalAdd( 42 );
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 145 );
	if( hb_xvmLocalAddInt( 42, 8192 ) ) break;
	goto lab00018;
lab00016: ;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 148 );
	if( hb_xvmLocalAddInt( 42, 8 ) ) break;
lab00017: ;
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 151 );
	if( hb_xvmLocalAddInt( 42, 16 ) ) break;
lab00018: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 156 );
	if( hb_xvmLocalAddInt( 42, 32 ) ) break;
lab00019: ;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmSetLine( 160 );
	if( hb_xvmLocalAddInt( 42, 2 ) ) break;
lab00020: ;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 164 );
	if( hb_xvmLocalAddInt( 42, 2048 ) ) break;
lab00021: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 168 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00022: ;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00023;
	hb_xvmSetLine( 172 );
	hb_xvmPushLocalByRef( 42 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00023: ;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00024;
	hb_xvmSetLine( 178 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 16, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 43 );
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushStringConst( "EDIT", 4 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00026;
lab00024: ;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 188 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00026;
lab00025: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 18 ) ) break;
	hb_xvmPopLocal( 37 );
lab00026: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00030;
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00027;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00028;
lab00027: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 38 );
lab00028: ;
	hb_xvmSetLine( 211 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
lab00029: ;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00030: ;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmPushStringConst( "NUMTEXT", 7 );
	goto lab00032;
lab00031: ;
	hb_xvmPushStringConst( "TEXT", 4 );
lab00032: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 230 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 240 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 243 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushInteger( -1 );
lab00034: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00035;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( -1 );
lab00036: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 22 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 269 );
	hb_xvmPushSymbol( symbols + 28 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00037: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00041;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
lab00038: ;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 282 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00039: ;
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00040;
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 286 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 33 );
	if( hb_xvmDo( 4 ) ) break;
lab00040: ;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 290 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 2 ) ) break;
lab00041: ;
	hb_xvmSetLine( 294 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 296 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 3 );
	hb_xvmSetLine( 307 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 308 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 309 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 310 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 311 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "NUMTEXT", 7 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 314 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 207 );
	hb_xvmPushLocal( 7 );
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
	hb_xvmSetLine( 316 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 317 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 197 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00004: ;
	hb_xvmSetLine( 321 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
lab00005: ;
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00006: ;
	hb_xvmSetLine( 332 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 336 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 337 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00008: ;
	hb_xvmSetLine( 340 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEMASKEDTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 10, 32 );
	hb_xvmSetLine( 359 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 360 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 363 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 365 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00002: ;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocalByRef( 41 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00003: ;
	hb_xvmSetLine( 373 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "9", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "$", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "*", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "\x80", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "\x88", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "@...TEXTBOX: Wrong InputMask Definition.", 40 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 376 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
lab00005: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 378 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocalByRef( 41 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmSetLine( 379 );
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 41 );
	hb_xvmPushStringConst( "E", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "@...TEXTBOX: Wrong Format Definition.", 37 );
	if( hb_xvmDo( 1 ) ) break;
lab00007: ;
	hb_xvmSetLine( 382 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00008: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 388 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 392 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 393 );
	hb_xvmPushStringConst( "@", 1 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 18 );
lab00009: ;
	hb_xvmSetLine( 396 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 401 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	if( hb_xvmDo( 7 ) ) break;
lab00010: ;
	hb_xvmSetLine( 404 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00011;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00011: ;
	hb_xvmSetLine( 405 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00013;
lab00012: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00013: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00014: ;
	hb_xvmSetLine( 410 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 411 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 412 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 413 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00015: ;
	hb_xvmSetLine( 415 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00017;
lab00016: ;
	hb_xvmPushLocal( 2 );
lab00017: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00018: ;
	hb_xvmSetLine( 421 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00019: ;
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
lab00020: ;
	hb_xvmSetLine( 429 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 36 );
	hb_xvmSetLine( 430 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 432 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 433 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 435 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741952 );
#else
	hb_xvmPushLong( 1073741952L );
#endif
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushInteger( 0 );
	goto lab00022;
lab00021: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
lab00022: ;
	hb_xvmLocalAdd( 39 );
	hb_xvmSetLine( 437 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 438 );
	if( hb_xvmLocalAddInt( 39, 2048 ) ) break;
lab00023: ;
	hb_xvmSetLine( 441 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 442 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00024: ;
	hb_xvmSetLine( 445 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 446 );
	hb_xvmPushLocalByRef( 39 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00025: ;
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 452 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 42, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushStringConst( "EDIT", 4 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00028;
lab00026: ;
	hb_xvmSetLine( 457 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00028;
lab00027: ;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 16 ) ) break;
	hb_xvmPopLocal( 34 );
lab00028: ;
	hb_xvmSetLine( 474 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
	hb_xvmSetLine( 476 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00029;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 480 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 35 );
lab00030: ;
	hb_xvmSetLine( 484 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 485 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00031: ;
	hb_xvmSetLine( 488 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 489 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00032: ;
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 499 );
	hb_xvmPushStringConst( "MASKEDTEXT", 10 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 501 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 502 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 503 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 504 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 505 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 507 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 508 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 509 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 510 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 511 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 512 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 513 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 514 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 515 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 516 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 517 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 518 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 519 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 520 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 521 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushInteger( -1 );
lab00034: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 522 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00035;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( -1 );
lab00036: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 523 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 524 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 525 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 526 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 527 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 528 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 529 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 530 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 531 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 532 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 533 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 534 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 535 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 536 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 537 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 26 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 538 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 540 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 541 );
	hb_xvmPushSymbol( symbols + 28 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00037: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00039;
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00038;
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 546 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 4 ) ) break;
lab00038: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 551 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 552 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
lab00039: ;
	hb_xvmSetLine( 556 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 558 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGMASKEDTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 567 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 568 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 569 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 571 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 574 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 577 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 582 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 583 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00004: ;
	hb_xvmSetLine( 586 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETNUMFROMTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 591 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 594 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 595 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 596 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 598 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00004: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "DB", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 601 );
	hb_xvmPushStringConst( "-", 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmLocalAdd( 3 );
lab00006: ;
	hb_xvmSetLine( 604 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 606 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( GETNUMMASK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 614 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00001: ;
	hb_xvmSetLine( 615 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "9", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 616 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 618 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "$", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "*", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 619 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "9", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 621 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00006: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 623 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINECHARMASKTEXTBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 10, 32 );
	hb_xvmSetLine( 640 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 641 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 642 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 644 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ">", 1 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 645 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 38 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 646 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 7 );
lab00002: ;
	hb_xvmSetLine( 651 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 652 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushInteger( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 653 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocalByRef( 25 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 654 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 657 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 15 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 659 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 660 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushStringConst( "  /  /  ", 8 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "", 0 );
lab00004: ;
	hb_xvmPopLocal( 7 );
lab00005: ;
	hb_xvmSetLine( 663 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 664 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 665 );
	hb_xvmPushStringConst( "yYmMdD", 6 );
	hb_xvmPushLocalByRef( 39 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
lab00006: ;
	hb_xvmSetLine( 666 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushStringConst( "9", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 667 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00007: ;
	hb_xvmEnumEnd();
lab00008: ;
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 671 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 18 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	if( hb_xvmDo( 7 ) ) break;
lab00009: ;
	hb_xvmSetLine( 674 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
lab00010: ;
	hb_xvmSetLine( 675 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00012;
lab00011: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00012: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00013: ;
	hb_xvmSetLine( 680 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmSetLine( 681 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 682 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 683 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00014: ;
	hb_xvmSetLine( 685 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	hb_xvmPopLocal( 42 );
	hb_xvmSetLine( 687 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 2 );
lab00016: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00017: ;
	hb_xvmSetLine( 691 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00018: ;
	hb_xvmSetLine( 695 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 696 );
	hb_xvmPushFuncSymbol( symbols + 4 );
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
lab00019: ;
	hb_xvmSetLine( 699 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 36 );
	hb_xvmSetLine( 700 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 702 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 703 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 705 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741952 );
#else
	hb_xvmPushLong( 1073741952L );
#endif
	hb_xvmPushLocal( 29 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushInteger( 0 );
	goto lab00021;
lab00020: ;
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
lab00021: ;
	hb_xvmLocalAdd( 40 );
	hb_xvmSetLine( 707 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 708 );
	if( hb_xvmLocalAddInt( 40, 2048 ) ) break;
lab00022: ;
	hb_xvmSetLine( 711 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 712 );
	hb_xvmPushLocalByRef( 40 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00023: ;
	hb_xvmSetLine( 715 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmSetLine( 716 );
	hb_xvmPushLocalByRef( 40 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
	if( hb_xvmPlusEqPop() ) break;
lab00024: ;
	hb_xvmSetLine( 719 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00025;
	hb_xvmSetLine( 722 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 42, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 41 );
	hb_xvmSetLine( 723 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 37 );
	hb_xvmPushStringConst( "EDIT", 4 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 41 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00027;
lab00025: ;
	hb_xvmSetLine( 727 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 729 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 730 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 731 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 732 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 735 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	goto lab00027;
lab00026: ;
	hb_xvmSetLine( 741 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 742 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 16 ) ) break;
	hb_xvmPopLocal( 34 );
lab00027: ;
	hb_xvmSetLine( 746 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00031;
	hb_xvmSetLine( 748 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00028;
	hb_xvmSetLine( 749 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00029;
lab00028: ;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 752 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 753 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 35 );
lab00029: ;
	hb_xvmSetLine( 756 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 757 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 760 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 761 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00031: ;
	hb_xvmSetLine( 767 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 36 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 772 );
	hb_xvmPushStringConst( "CHARMASKTEXT", 12 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 773 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 774 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 775 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 776 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 777 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 778 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 779 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 781 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 782 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 783 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 784 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 785 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 786 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 787 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 788 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 789 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 790 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 791 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 792 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 793 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 794 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00032;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00033;
lab00032: ;
	hb_xvmPushInteger( -1 );
lab00033: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 795 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00034;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00035;
lab00034: ;
	hb_xvmPushInteger( -1 );
lab00035: ;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 796 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 797 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 798 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 799 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 800 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 801 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 802 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 803 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 804 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 805 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 806 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 807 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 808 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 809 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 810 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 26 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 811 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 813 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 814 );
	hb_xvmPushSymbol( symbols + 28 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 37 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00036: ;
	hb_xvmSetLine( 817 );
	hb_xvmPushLocal( 42 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00041;
	hb_xvmSetLine( 819 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00038;
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 820 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 821 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 7 );
lab00037: ;
	hb_xvmSetLine( 823 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 5377 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 4 ) ) break;
lab00038: ;
	hb_xvmSetLine( 826 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 827 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 828 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00040;
lab00039: ;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 2 ) ) break;
lab00040: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 835 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
lab00041: ;
	hb_xvmSetLine( 839 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 841 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PROCESSCHARMASK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 20, 2 );
	hb_xvmSetLine( 846 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 847 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Numeric", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Numeric", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Numeric", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Numeric", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 848 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 858 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 859 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 860 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 864 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 867 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 176 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 870 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 873 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 874 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 10 );
lab00002: ;
	hb_xvmSetLine( 879 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 882 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocalByRef( 16 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00003: ;
	hb_xvmSetLine( 883 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00004: ;
	hb_xvmSetLine( 884 );
	if( hb_xvmLocalInc( 4 ) ) break;
lab00005: ;
	hb_xvmSetLine( 886 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
lab00006: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 889 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 890 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 9 );
lab00008: ;
	hb_xvmSetLine( 894 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocalByRef( 16 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00009: ;
	hb_xvmSetLine( 895 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 896 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocalByRef( 16 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 897 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 899 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
lab00011: ;
	hb_xvmEnumEnd();
lab00012: ;
	hb_xvmSetLine( 903 );
	hb_xvmCopyLocals( 11, 12 );
	hb_xvmSetLine( 905 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 907 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 909 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 911 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 913 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 914 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 14 );
	hb_xvmLocalAdd( 11 );
	goto lab00014;
lab00013: ;
	hb_xvmSetLine( 916 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmLocalAdd( 11 );
lab00014: ;
	hb_xvmSetLine( 921 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 923 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 925 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 927 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmLocalAdd( 11 );
lab00015: ;
	hb_xvmSetLine( 932 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 22 );
	goto lab00036;
lab00016: ;
	hb_xvmSetLine( 934 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 935 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 939 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00017;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "!", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
lab00017: ;
	hb_xvmSetLine( 941 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00018;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "!", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00018: ;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
lab00019: ;
	hb_xvmSetLine( 943 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "!", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00020;
	hb_xvmSetLine( 944 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00035;
lab00020: ;
	hb_xvmSetLine( 946 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00035;
lab00021: ;
	hb_xvmSetLine( 951 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 952 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 953 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 955 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00023: ;
	goto lab00035;
lab00024: ;
	hb_xvmSetLine( 960 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "9", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmSetLine( 962 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
lab00025: ;
	hb_xvmSetLine( 964 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00035;
lab00026: ;
	hb_xvmSetLine( 968 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 969 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 970 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00028;
lab00027: ;
	hb_xvmSetLine( 972 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00028: ;
	goto lab00035;
lab00029: ;
	hb_xvmSetLine( 977 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmSetLine( 979 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 981 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00035;
lab00030: ;
	hb_xvmSetLine( 985 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 986 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 987 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00032;
lab00031: ;
	hb_xvmSetLine( 989 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlusEqPop() ) break;
lab00032: ;
	goto lab00035;
lab00033: ;
	hb_xvmSetLine( 996 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00034;
	hb_xvmSetLine( 997 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00035;
lab00034: ;
	hb_xvmSetLine( 999 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPlusEqPop() ) break;
lab00035: ;
	hb_xvmSetLine( 932 );
	if( hb_xvmLocalIncPush( 22 ) ) break;
lab00036: ;
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 1007 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00037;
	hb_xvmSetLine( 1008 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00037: ;
	hb_xvmSetLine( 1011 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 1013 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1015 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 1017 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 1019 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 1021 );
	hb_xvmPushStringConst( "-", 1 );
	hb_xvmPushLocal( 20 );
	hb_xvmLocalAdd( 20 );
	hb_xvmSetLine( 1024 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1025 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00045;
lab00038: ;
	hb_xvmSetLine( 1029 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1030 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00045;
lab00039: ;
	hb_xvmSetLine( 1036 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 1038 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 1039 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00045;
lab00040: ;
	hb_xvmSetLine( 1044 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00041;
	hb_xvmSetLine( 1045 );
	if( hb_xvmLocalDec( 15 ) ) break;
lab00041: ;
	hb_xvmSetLine( 1048 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1050 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1053 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 22 );
	goto lab00044;
lab00042: ;
	hb_xvmSetLine( 1055 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 1056 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 1058 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_xvmSetLine( 1062 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00045;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
lab00043: ;
	hb_xvmSetLine( 1063 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1053 );
	if( hb_xvmLocalIncPush( 22 ) ) break;
lab00044: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
lab00045: ;
	hb_xvmSetLine( 1074 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CHARMASKTEKSTOK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 1079 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1080 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1084 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	goto lab00012;
lab00001: ;
	hb_xvmSetLine( 1085 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1086 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1087 );
	goto lab00010;
lab00002: ;
	hb_xvmSetLine( 1089 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_stackPop();
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
lab00003: ;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00004: ;
	hb_xvmSetLine( 1093 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_stackPop();
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
lab00005: ;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00006: ;
	hb_xvmSetLine( 1096 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_stackPop();
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
lab00007: ;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 1099 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmExactlyEqual() ) break;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00009: ;
	hb_xvmSetLine( 1102 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmNot() ) break;
	hb_xvmPopLocal( 4 );
	goto lab00011;
lab00010: ;
	hb_xvmPushLocal( 6 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		const char * pszText;
		HB_SIZE nLen;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		pszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;
		nLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;
		if( pszText && nLen == 1 && ! memcmp( pszText, "A", 1 ) )
		{
			hb_stackPop();
			goto lab00002;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "N", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "!", 1 ) )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, "9", 1 ) )
		{
			hb_stackPop();
			goto lab00006;
		}
		if( pszText && nLen == 1 && ! memcmp( pszText, " ", 1 ) )
		{
			hb_stackPop();
			goto lab00008;
		}
		{
			hb_stackPop();
			goto lab00009;
		}
	}
lab00011: ;
	hb_xvmSetLine( 1104 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1084 );
	if( hb_xvmLocalIncPush( 7 ) ) break;
lab00012: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
lab00013: ;
	hb_xvmSetLine( 1109 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATATEXTBOXREFRESH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1116 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "MASKEDTEXT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1117 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1119 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 1122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1123 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroPushList( 43 ) ) break;
	if( hb_xvmMacroFunc( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroPush( 43 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1125 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 1128 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DATATEXTBOXSAVE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 1136 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1138 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "MASKEDTEXT", 10 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1139 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1141 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 1144 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1145 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "FIELD", 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMacroPopAliased( 43 ) ) break;
lab00003: ;
	hb_xvmSetLine( 1148 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( PROCESSNUMTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 1 );
	hb_xvmSetLine( 1153 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "Logical", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1161 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 176 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1164 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1166 );
	hb_xvmCopyLocals( 4, 5 );
	hb_xvmSetLine( 1169 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1170 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1171 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1172 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 1174 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 1177 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00004: ;
	hb_xvmSetLine( 1179 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocalByRef( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
lab00005: ;
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 1180 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 1182 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 2 );
lab00009: ;
	hb_xvmSetLine( 1184 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00010: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 1186 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1187 );
	if( hb_xvmLocalDec( 6 ) ) break;
lab00011: ;
	hb_xvmSetLine( 1191 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 1192 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 1196 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1198 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETNUMFROMTEXTSP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 1203 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushStringConst( "String", 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1206 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
lab00001: ;
	hb_xvmSetLine( 1208 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "-", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 1210 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1211 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 1214 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1215 );
	hb_xvmPushStringConst( ".", 1 );
	hb_xvmPopLocal( 4 );
lab00004: ;
	hb_xvmSetLine( 1218 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlusEqPop() ) break;
lab00005: ;
	hb_xvmSetLine( 1222 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00006: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 1224 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "DB", 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 1225 );
	hb_xvmPushStringConst( "-", 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmLocalAdd( 3 );
lab00008: ;
	hb_xvmSetLine( 1228 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1230 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( OEDITEVENTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 4 );
	hb_xvmSetLine( 1241 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 1243 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1245 );
	goto lab00010;
lab00001: ;
	hb_xvmSetLine( 1249 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1250 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 176 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 1251 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 176 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 1252 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 1256 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmInc() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1261 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1263 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1264 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 771 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1265 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00007;
lab00003: ;
	hb_xvmSetLine( 1267 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmInc() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1268 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 771 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1269 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 177 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 1274 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1275 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 177 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 4 ) ) break;
lab00005: ;
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 1282 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 1283 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 177 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	goto lab00011;
lab00008: ;
	hb_xvmSetLine( 1291 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1293 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	{
		static const HB_BYTE codeblock[ 14 ] = {
			1, 0, 1, 0, 1, 0, 95, 1, 122, 1, 95, 255, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 1295 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1297 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1298 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPop( 87L ) ) break;
	hb_xvmSetLine( 1299 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	if( hb_xvmPushMemvar( symbols + 9 ) ) break;
	if( hb_xvmArrayItemPush( 86L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1300 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00009: ;
	goto lab00011;
lab00010: ;
	hb_xvmPushLocal( 2 );
	{
		PHB_ITEM pSwitch;
		HB_TYPE type;
		long lVal;
		if( hb_xvmSwitchGet( &pSwitch ) ) break;
		type = hb_itemType( pSwitch );
		lVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;

		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 258L )
		{
			hb_stackPop();
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 123L )
		{
			hb_stackPop();
			goto lab00008;
		}
		hb_stackPop();
	}
lab00011: ;
	hb_xvmSetLine( 1311 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

