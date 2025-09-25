/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_alert.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( HMG_ALERT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( HB_OSISWIN10 );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( GETRED );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( GETGREEN );
HB_FUNC_EXTERN( GETBLUE );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC( HMG_CHECKTYPE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( MLCOUNT );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( SPACE );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( TRIM );
HB_FUNC_EXTERN( _DEFINEMODALWINDOW );
HB_FUNC_STATIC( FILLDLG );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( _RELEASEFONT );
HB_FUNC( HMG_ALERT_MAXLINES );
HB_FUNC( HMG_ALERT_ROWSTART );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETDC );
HB_FUNC_EXTERN( GETTEXTHEIGHT );
HB_FUNC_EXTERN( GETFONTWIDTH );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( REPLICATE );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( MEMOLINE );
HB_FUNC_EXTERN( RELEASEDC );
HB_FUNC_EXTERN( GETBORDERWIDTH );
HB_FUNC( GETDESKTOPREALWIDTH );
HB_FUNC_EXTERN( MIN );
HB_FUNC_EXTERN( GETBORDERHEIGHT );
HB_FUNC_EXTERN( GETTITLEHEIGHT );
HB_FUNC_EXTERN( MSC_VER );
HB_FUNC( GETDESKTOPREALHEIGHT );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _DEFINEEDITBOX );
HB_FUNC_EXTERN( SETWINDOWLONG );
HB_FUNC_EXTERN( HB_BITAND );
HB_FUNC_EXTERN( GETWINDOWLONG );
HB_FUNC_EXTERN( SETWINDOWPOS );
HB_FUNC_EXTERN( HMG_DRAWICON );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( _DEFINEOWNERBUTTON );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC( _SETMSGALERTCOLORS );
HB_FUNC_EXTERN( HB_APARAMS );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( GETDESKTOPAREA );
HB_FUNC_STATIC( _GETTITLEANDOPTIONS );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC( ALERTYESNO );
HB_FUNC_STATIC( _ALERT );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( ALERTYESNOCANCEL );
HB_FUNC( ALERTRETRYCANCEL );
HB_FUNC( ALERTOKCANCEL );
HB_FUNC( ALERTEXCLAMATION );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PLAYEXCLAMATION );
HB_FUNC( ALERTINFO );
HB_FUNC_EXTERN( PLAYASTERISK );
HB_FUNC( ALERTSTOP );
HB_FUNC_EXTERN( PLAYHAND );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HMG_ALERT );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ALERT )
{ "HMG_ALERT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ALERT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "HB_OSISWIN10", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_OSISWIN10 )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "GETRED", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETRED )}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "GETGREEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETGREEN )}, NULL },
{ "GETBLUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBLUE )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "HMG_CHECKTYPE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_CHECKTYPE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "MLCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( MLCOUNT )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "SPACE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SPACE )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "TRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TRIM )}, NULL },
{ "_DEFINEMODALWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMODALWINDOW )}, NULL },
{ "FILLDLG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( FILLDLG )}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "_RELEASEFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _RELEASEFONT )}, NULL },
{ "HMG_ALERT_MAXLINES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ALERT_MAXLINES )}, NULL },
{ "HMG_ALERT_ROWSTART", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ALERT_ROWSTART )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDC )}, NULL },
{ "GETTEXTHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTHEIGHT )}, NULL },
{ "GETFONTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTWIDTH )}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "REPLICATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( REPLICATE )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "MEMOLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( MEMOLINE )}, NULL },
{ "RELEASEDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEDC )}, NULL },
{ "GETBORDERWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERWIDTH )}, NULL },
{ "GETDESKTOPREALWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETDESKTOPREALWIDTH )}, NULL },
{ "MIN", {HB_FS_PUBLIC}, {HB_FUNCNAME( MIN )}, NULL },
{ "GETBORDERHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERHEIGHT )}, NULL },
{ "GETTITLEHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTITLEHEIGHT )}, NULL },
{ "MSC_VER", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSC_VER )}, NULL },
{ "GETDESKTOPREALHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETDESKTOPREALHEIGHT )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_DEFINEEDITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEEDITBOX )}, NULL },
{ "SETWINDOWLONG", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWLONG )}, NULL },
{ "HB_BITAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITAND )}, NULL },
{ "GETWINDOWLONG", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWLONG )}, NULL },
{ "SETWINDOWPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWPOS )}, NULL },
{ "HMG_DRAWICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_DRAWICON )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "_DEFINEOWNERBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEOWNERBUTTON )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "_SETMSGALERTCOLORS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETMSGALERTCOLORS )}, NULL },
{ "HB_APARAMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_APARAMS )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETDESKTOPAREA", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPAREA )}, NULL },
{ "_GETTITLEANDOPTIONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETTITLEANDOPTIONS )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "ALERTYESNO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTYESNO )}, NULL },
{ "_ALERT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ALERT )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "ALERTYESNOCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTYESNOCANCEL )}, NULL },
{ "ALERTRETRYCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTRETRYCANCEL )}, NULL },
{ "ALERTOKCANCEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTOKCANCEL )}, NULL },
{ "ALERTEXCLAMATION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTEXCLAMATION )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PLAYEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYEXCLAMATION )}, NULL },
{ "ALERTINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTINFO )}, NULL },
{ "PLAYASTERISK", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYASTERISK )}, NULL },
{ "ALERTSTOP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ALERTSTOP )}, NULL },
{ "PLAYHAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYHAND )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HMG_ALERT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_ALERT )}, NULL },
{ "(_INITSTATICS00006)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ALERT, "h_alert.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ALERT
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ALERT )
   #include "hbiniseg.h"
#endif

HB_FUNC( HMG_ALERT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 12 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 109 );
	hb_xvmPushStringConst( "ALERT", 5 );
	hb_xvmPushStringConst( "QUESTION", 8 );
	hb_xvmPushStringConst( "INFO", 4 );
	hb_xvmPushStringConst( "STOP", 4 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 110 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
lab00001: ;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 113 );
	hb_xvmPushStringConst( "oDlg", 4 );
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 114 );
	hb_xvmLocalSetInt( 18, 0L );
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 117 );
	hb_xvmLocalSetInt( 13, 0L );
lab00002: ;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "oDlg", 4 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmLocalIncPush( 13 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
lab00003: ;
	hb_xvmSetLine( 122 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopStatic( 2 );
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopStatic( 1 );
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStaticByRef( 3 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStaticByRef( 4 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "&OK", 3 );
	if( hb_xvmMacroText() ) break;
	hb_xvmArrayGen( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushStringConst( "DlgFont", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 134 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmPushInteger( 2 );
	goto lab00006;
lab00004: ;
	hb_xvmPushInteger( 1 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 4 );
lab00006: ;
	hb_xvmPopLocal( 4 );
	goto lab00010;
lab00007: ;
	hb_xvmSetLine( 136 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushInteger( 1 );
	goto lab00009;
lab00008: ;
	hb_xvmPushLocal( 4 );
lab00009: ;
	hb_xvmPopLocal( 4 );
lab00010: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "cMsg", 4 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "aOptions", 8 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "CHARACTER", 9 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "cTitle", 6 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "NUMERIC", 7 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "nType", 5 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "cIcoFile", 8 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "nIcoSize", 8 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "aBtnColors", 10 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "bInit", 5 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "lClosable", 9 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "CHARACTER", 9 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "cFontName", 9 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "aListImage", 10 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushStringConst( "Numeric", 7 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "nSeconds", 8 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( fValue )
		goto lab00011;
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
lab00011: ;
	hb_xvmSetLine( 154 );
	hb_xvmLocalSetInt( 4, 1L );
lab00012: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 14 );
	{
		static const HB_BYTE codeblock[ 25 ] = {
			2, 0, 1, 0, 14, 0, 106, 7, 90, 90, 90, 95, 66, 95, 0, 95, 
			1, 72, 165, 95, 255, 95, 2, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 166 );
	hb_xvmPushStringConst( "\x09", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "\x09", 1 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
lab00013: ;
	hb_xvmSetLine( 170 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "\x0D\x0A", 2 );
	if( hb_xvmFunction( 2 ) ) break;
	{
		static const HB_BYTE codeblock[ 29 ] = {
			1, 0, 1, 0, 18, 0, 176, 23, 0, 95, 255, 176, 14, 0, 176, 24, 
			0, 95, 1, 12, 1, 12, 1, 12, 2, 165, 80, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 173 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 174 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 68L ) ) break;
	goto lab00015;
lab00014: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	hb_xvmPushInteger( 68 );
	if( hb_xvmArrayPushRef() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 27 ] = {
			0, 0, 1, 0, 9, 0, 103, 2, 0, 31, 16, 95, 255, 28, 12, 121, 
			165, 98, 12, 0, 92, 68, 2, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStatic( 3 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			0, 0, 1, 0, 9, 0, 103, 2, 0, 21, 31, 5, 73, 95, 255, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 187 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 17 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 190 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 1 ) ) break;
lab00016: ;
	hb_xvmSetLine( 193 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 68L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_ALERT_MAXLINES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 200 );
	hb_xvmPushStatic( 5 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 1 );
	hb_xvmPopStatic( 5 );
lab00001: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_ALERT_ROWSTART )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 213 );
	hb_xvmPushStatic( 6 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmGreaterEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 1 );
	hb_xvmPopStatic( 6 );
lab00001: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( FILLDLG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 26, 12 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Name", 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 230 );
	hb_xvmPushDouble( * ( double * ) "3333333@", 255, 1 );
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 231 );
	hb_xvmLocalSetInt( 20, 0L );
	hb_xvmSetLine( 232 );
	hb_xvmLocalSetInt( 21, 0L );
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 248 );
	hb_xvmCopyLocals( 2, 12 );
	hb_xvmSetLine( 249 );
	hb_xvmPushStringConst( "&OK", 3 );
	if( hb_xvmMacroText() ) break;
	hb_xvmArrayGen( 1 );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 254 );
	hb_xvmLocalSetInt( 33, 12L );
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 260 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 261 );
	hb_xvmPushInteger( 70 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushInteger( 0 );
	goto lab00005;
lab00002: ;
	hb_xvmPushInteger( 70 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 64L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushDouble( * ( double * ) "ffffff\x06@", 10, 1 );
	goto lab00004;
lab00003: ;
	hb_xvmPushDouble( * ( double * ) "\x9A\x99\x99\x99\x99\x99\x09@", 10, 1 );
lab00004: ;
	if( hb_xvmDivide() ) break;
lab00005: ;
	hb_xvmLocalAdd( 19 );
lab00006: ;
	hb_xvmSetLine( 264 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Handle", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushFuncSymbol( symbols + 32 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Handle", 6 );
	if( hb_xvmFunction( 2 ) ) break;
lab00008: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmLocalAdd( 31 );
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushStringConst( "B", 1 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 275 );
	hb_xvmPushLocalByRef( 24 );
	hb_xvmPushDouble( * ( double * ) "ffffff\xE6\?", 10, 1 );
	if( hb_xvmMultEqPop() ) break;
lab00009: ;
	hb_xvmSetLine( 278 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 34 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 34 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 278 );
	if( hb_xvmLocalIncPush( 34 ) ) break;
lab00011: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 284 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 34 );
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 21 );
	hb_xvmSetLine( 284 );
	if( hb_xvmLocalIncPush( 34 ) ) break;
lab00013: ;
	hb_xvmPushLocal( 26 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushInteger( 22 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushLocal( 26 );
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmPushDouble( * ( double * ) "\x9A\x99\x99\x99\x99\x99\xF1\?", 10, 1 );
	goto lab00016;
lab00014: ;
	hb_xvmPushLocal( 26 );
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmPushInteger( 2 );
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( 3 );
lab00016: ;
	if( hb_xvmMult() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmAddInt( 4L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmMult() ) break;
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	hb_xvmLocalAdd( 32 );
	hb_xvmSetLine( 300 );
	hb_xvmPushLocal( 24 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 301 );
	hb_xvmCopyLocals( 24, 20 );
lab00017: ;
	hb_xvmSetLine( 304 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushInteger( 102 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 64 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( -22L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 48L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmPushInteger( 8 );
	goto lab00019;
lab00018: ;
	hb_xvmPushInteger( 0 );
lab00019: ;
	hb_xvmLocalAdd( 27 );
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00" "H@", 255, 1 );
	goto lab00021;
lab00020: ;
	hb_xvmPushInteger( 0 );
lab00021: ;
	hb_xvmLocalAdd( 29 );
	hb_xvmSetLine( 307 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 24 );
	hb_xvmSetLine( 309 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 29 );
	goto lab00023;
lab00022: ;
	hb_xvmSetLine( 311 );
	hb_xvmLocalSetInt( 24, 0L );
lab00023: ;
	hb_xvmSetLine( 314 );
	hb_xvmPushLocalByRef( 29 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushInteger( 40 );
	goto lab00025;
lab00024: ;
	hb_xvmPushInteger( 0 );
lab00025: ;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 316 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmPushInteger( 4 );
	goto lab00027;
lab00026: ;
	hb_xvmPushInteger( 3 );
lab00027: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 33 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 32 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 28 );
	hb_xvmSetLine( 317 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( 4L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStatic( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x04@", 10, 1 );
	goto lab00029;
lab00028: ;
	hb_xvmPushInteger( 1 );
lab00029: ;
	if( hb_xvmDivide() ) break;
	hb_xvmLocalAdd( 30 );
	hb_xvmSetLine( 319 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00030;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 320 );
	if( hb_xvmLocalAddInt( 29, 10 ) ) break;
	hb_xvmSetLine( 321 );
	if( hb_xvmLocalAddInt( 30, 10 ) ) break;
lab00030: ;
	hb_xvmSetLine( 324 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmSetLine( 325 );
	hb_xvmLocalSetInt( 34, 0L );
lab00031: ;
	hb_xvmSetLine( 326 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmLocalIncPush( 34 ) ) break;
	if( hb_xvmMult() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00031;
	hb_xvmSetLine( 328 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmMult() ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 329 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmMinusEqPop() ) break;
	hb_xvmSetLine( 330 );
	hb_xvmPushLocalByRef( 30 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmMinusEqPop() ) break;
	hb_xvmSetLine( 331 );
	hb_xvmPushLocalByRef( 28 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmMinusEqPop() ) break;
lab00032: ;
	hb_xvmSetLine( 334 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmDo( 3 ) ) break;
lab00034: ;
	hb_xvmSetLine( 336 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 3 ) ) break;
lab00036: ;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 339 );
	hb_xvmCopyLocals( 31, 18 );
lab00037: ;
	hb_xvmSetLine( 343 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushStringConst( "MsgAlert", 8 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 344L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 293L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 289L ) ) break;
	hb_xvmSetLine( 344 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmSetLine( 345 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 346 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmAddInt( 5L ) ) break;
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xCC\xEC\?", 10, 1 );
	goto lab00039;
lab00038: ;
	hb_xvmPushInteger( 1 );
lab00039: ;
	if( hb_xvmMultByInt( 32L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 348 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmPushLocal( 3 );
	goto lab00041;
lab00040: ;
	hb_xvmPushLocal( 22 );
lab00041: ;
	if( hb_xvmMult() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 349 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmSetLine( 351 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 352 );
	hb_xvmPushStatic( 3 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmSetLine( 353 );
	hb_xvmPushStatic( 4 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmSetLine( 354 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65000 );
#else
	hb_xvmPushLong( 65000L );
#endif
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 343L ) ) break;
	hb_xvmSetLine( 355 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmSetLine( 356 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 289L ) ) break;
	hb_xvmSetLine( 357 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 293L ) ) break;
	hb_xvmSetLine( 358 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 50 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 343L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 344L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 293L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 289L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 31 ) ) break;
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "MsgAlert", 8 );
	hb_xvmPushStringConst( "Handle", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( -20 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "MsgAlert", 8 );
	hb_xvmPushStringConst( "Handle", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( -20 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( -513 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "MsgAlert", 8 );
	hb_xvmPushStringConst( "Handle", 6 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 39 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 364 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00045;
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 32 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00042;
	hb_xvmPushDouble( * ( double * ) "ffffff\xF6\?", 10, 1 );
	goto lab00044;
lab00042: ;
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 48L, &fValue ) ) break;
	if( !fValue )
		goto lab00043;
	hb_xvmPushDouble( * ( double * ) "333333\xFB\?", 10, 1 );
	goto lab00044;
lab00043: ;
	hb_xvmPushInteger( 2 );
lab00044: ;
	if( hb_xvmDivide() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 8 ) ) break;
lab00045: ;
	hb_xvmSetLine( 375 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPopLocal( 35 );
	hb_xvmSetLine( 377 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 34 );
	goto lab00053;
lab00046: ;
	hb_xvmSetLine( 379 );
	hb_xvmPushStringConst( "Btn_", 4 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmLocalAdd( 17 );
	hb_xvmSetLine( 381 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 325L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 286L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 280L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 281L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 283L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 352L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 287L ) ) break;
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 339L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 282L ) ) break;
	hb_xvmSetLine( 382 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmSetLine( 383 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmAddInt( 4L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStatic( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmPushDouble( * ( double * ) "\x00\x00\x00\x00\x00\x00\x04@", 10, 1 );
	goto lab00048;
lab00047: ;
	hb_xvmPushDouble( * ( double * ) "\xCD\xCC\xCC\xCC\xCC\xCC\xEC\?", 10, 1 );
lab00048: ;
	if( hb_xvmDivide() ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 31 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 32 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 384 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 386 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 388 );
	{
		static const HB_BYTE codeblock[ 96 ] = {
			98, 12, 0, 93, 218, 0, 1, 106, 2, 67, 0, 8, 28, 31, 176, 32, 
			0, 98, 12, 0, 93, 254, 0, 1, 98, 12, 0, 93, 255, 0, 1, 106, 
			6, 67, 97, 114, 103, 111, 0, 12, 3, 25, 22, 176, 32, 0, 98, 12, 
			0, 93, 254, 0, 1, 106, 6, 67, 97, 114, 103, 111, 0, 12, 2, 98, 
			12, 0, 92, 68, 2, 120, 82, 2, 0, 176, 57, 0, 98, 12, 0, 93, 
			254, 0, 1, 106, 8, 82, 101, 108, 101, 97, 115, 101, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 390 );
	hb_xvmPushStatic( 4 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00049;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	goto lab00050;
lab00049: ;
	hb_xvmPushNil();
lab00050: ;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmSetLine( 392 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmSetLine( 393 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 287L ) ) break;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmPushLocal( 34 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	hb_xvmSetLine( 395 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmSetLine( 396 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( -8L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 282L ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( -8L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 339L ) ) break;
lab00051: ;
	hb_xvmSetLine( 399 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 325L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 286L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 280L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 281L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 283L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 284L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 287L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmDo( 40 ) ) break;
	hb_xvmSetLine( 400 );
	hb_xvmPushLocalByRef( 35 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmAddInt( 4L ) ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPushLocal( 17 );
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 403 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 68L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 404 );
	hb_xvmCopyLocals( 17, 38 );
lab00052: ;
	hb_xvmSetLine( 377 );
	if( hb_xvmLocalIncPush( 34 ) ) break;
lab00053: ;
	hb_xvmPushLocal( 26 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocalByRef( 38 );
	hb_xvmPushStringConst( "Btn_01", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 410 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "Closable", 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 412 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 416 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 34 ] = {
			121, 98, 12, 0, 92, 68, 2, 120, 82, 2, 0, 176, 57, 0, 98, 12, 
			0, 93, 254, 0, 1, 106, 8, 82, 101, 108, 101, 97, 115, 101, 0, 12, 
			2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
lab00054: ;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmSetLine( 421 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "OnInit", 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 423 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushStringConst( "Title", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 36 );
lab00055: ;
	hb_xvmSetLine( 426 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00056;
	hb_xvmSetLine( 427 );
	hb_xvmCopyLocals( 12, 37 );
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushStringConst( "oTimer", 6 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushInteger( 1000 );
	{
		static const HB_BYTE codeblock[ 86 ] = {
			0, 0, 3, 0, 16, 0, 36, 0, 37, 0, 176, 49, 0, 95, 255, 106, 
			6, 116, 105, 116, 108, 101, 0, 95, 254, 106, 4, 32, 40, 32, 0, 72, 
			176, 4, 0, 96, 253, 255, 171, 12, 1, 72, 106, 3, 32, 41, 0, 72, 
			20, 3, 95, 253, 121, 5, 28, 30, 120, 82, 2, 0, 176, 57, 0, 98, 
			12, 0, 93, 254, 0, 1, 106, 8, 82, 101, 108, 101, 97, 115, 101, 0, 
			12, 2, 25, 3, 100, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	if( hb_xvmDo( 6 ) ) break;
lab00056: ;
	hb_xvmSetLine( 431 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETMSGALERTCOLORS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmSetLine( 436 );
	hb_xvmPushStatic( 3 );
	hb_xvmPushStatic( 4 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 438 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 439 );
	hb_xvmPushLocal( 1 );
	hb_xvmPopStatic( 3 );
lab00001: ;
	hb_xvmSetLine( 442 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 443 );
	hb_xvmPushLocal( 2 );
	hb_xvmPopStatic( 4 );
lab00002: ;
	hb_xvmSetLine( 446 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_CHECKTYPE )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 5, 1 );
	hb_xvmSetLine( 475 );
	hb_xvmPushStringConst( "ARRAY", 5 );
	hb_xvmPushStringConst( "A", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "BLOCK", 5 );
	hb_xvmPushStringConst( "B", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "CHARACTER", 9 );
	hb_xvmPushStringConst( "C", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "DATE", 4 );
	hb_xvmPushStringConst( "D", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "HASH", 4 );
	hb_xvmPushStringConst( "H", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushStringConst( "L", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "NIL", 3 );
	hb_xvmPushStringConst( "U", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "NUMERIC", 7 );
	hb_xvmPushStringConst( "N", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "MEMO", 4 );
	hb_xvmPushStringConst( "M", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "POINTER", 7 );
	hb_xvmPushStringConst( "P", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "SYMBOL", 6 );
	hb_xvmPushStringConst( "S", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "TIMESTAMP", 9 );
	hb_xvmPushStringConst( "T", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "OBJECT", 6 );
	hb_xvmPushStringConst( "O", 1 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "USUAL", 5 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 14 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 484 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00001: ;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "USUAL", 5 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 488 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
lab00002: ;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	{
		static const HB_BYTE codeblock[ 26 ] = {
			1, 0, 1, 0, 4, 0, 95, 1, 122, 1, 176, 65, 0, 176, 39, 0, 
			95, 255, 122, 1, 12, 1, 12, 1, 8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 492 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00003: ;
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 5 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			1, 0, 1, 0, 4, 0, 95, 1, 92, 2, 1, 95, 255, 92, 2, 1, 
			8, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 496 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushStringConst( "CHECK TYPE ( Param # ", 21 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ) : ", 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is declared as ", 16 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " but it have type ", 18 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 504 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00005: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 506 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETDESKTOPREALWIDTH )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 516 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 518 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETDESKTOPREALHEIGHT )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 524 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _GETTITLEANDOPTIONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 40 );
	hb_xvmCopyLocals( 2, 4 );
	hb_xvmSetLine( 43 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 44 );
	hb_xvmCopyLocals( 1, 4 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 45 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 47 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 51 );
	hb_xvmCopyLocals( 1, 3 );
lab00003: ;
	hb_xvmSetLine( 54 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTYESNO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "&", 1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 264L ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "&", 1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 264L ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 81 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 82 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 1 );
lab00002: ;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 10 ) ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTYESNOCANCEL )
{
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "&", 1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 264L ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "&", 1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 264L ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "&", 1 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 265L ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 111 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 112 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 114 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 118 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 122 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 11 ) ) break;
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
			goto lab00001;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00002;
		}
		hb_stackPop();
	}
	hb_xvmSetLine( 126 );
	hb_xvmRetInt( -1L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTRETRYCANCEL )
{
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 267L ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 267L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 153 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 156 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTOKCANCEL )
{
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 230L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 230L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 183 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 184 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 11 ) ) break;
	if( hb_xvmEqualInt( 1L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTEXCLAMATION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 214 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 220 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 10 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 247 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 248 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 3 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 10 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ALERTSTOP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 280 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 282 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 285 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 288 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 4 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 10 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _ALERT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 11 );
	hb_xvmSetLine( 318 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 319 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 321 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 322 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 68L ) ) break;
lab00001: ;
	hb_xvmSetLine( 325 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 327 );
	{
		static const HB_BYTE codeblock[ 24 ] = {
			176, 89, 0, 98, 12, 0, 93, 254, 0, 1, 106, 8, 84, 111, 112, 77, 
			111, 115, 116, 0, 120, 12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPopLocal( 10 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 329 );
	hb_xvmCopyLocals( 10, 12 );
	hb_xvmSetLine( 330 );
	{
		static const HB_BYTE codeblock[ 38 ] = {
			0, 0, 1, 0, 12, 0, 48, 90, 0, 95, 255, 112, 0, 73, 176, 89, 
			0, 98, 12, 0, 93, 254, 0, 1, 106, 8, 84, 111, 112, 77, 111, 115, 
			116, 0, 120, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 10 );
lab00003: ;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 335 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 12 ) ) break;
	if( hb_xvmArrayItemPop( 444L ) ) break;
lab00004: ;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 9 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 93, 6 );
	hb_xvmSFrame( symbols + 93 );
	hb_xvmPushInteger( 20 );
	hb_xvmPopStatic( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPopStatic( 6 );
	/* *** END PROC *** */
   } while( 0 );
}

