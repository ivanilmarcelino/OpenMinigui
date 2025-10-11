/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_status.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _BEGINMESSAGEBAR );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( INITMESSAGEBAR );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( GETCLIENTRECT );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( _SETSTATUSBARKBD );
HB_FUNC( _ENDMESSAGEBAR );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( GETCONTROLINDEXBYHANDLE );
HB_FUNC_EXTERN( GETPARENTFORMNAME );
HB_FUNC( _DEFINEITEMMESSAGE );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( REFRESHPROGRESSITEM );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( MAX );
HB_FUNC_EXTERN( GETTEXTWIDTH );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( HB_UAT );
HB_FUNC_EXTERN( _DEFINELETTERORDIGITHOTKEY );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( INITITEMBAR );
HB_FUNC_EXTERN( ISARRAYRGB );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( HB_BITOR );
HB_FUNC( _SETSTATUSCLOCK );
HB_FUNC_STATIC( AMPM );
HB_FUNC_EXTERN( TIME );
HB_FUNC_EXTERN( _DEFINETIMER );
HB_FUNC_EXTERN( _SETITEM );
HB_FUNC( _SETSTATUSKEYBRD );
HB_FUNC( KEYTOGGLEPLATFORM );
HB_FUNC_EXTERN( ISNUMLOCKACTIVE );
HB_FUNC_EXTERN( ISCAPSLOCKACTIVE );
HB_FUNC_EXTERN( ISINSERTACTIVE );
HB_FUNC_EXTERN( _SETSTATUSICON );
HB_FUNC_EXTERN( KEYTOGGLENT );
HB_FUNC_EXTERN( KEYTOGGLE );
HB_FUNC( _ISOWNERDRAWSTATUSBARITEM );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC( _SETSTATUSITEMPROPERTY );
HB_FUNC_EXTERN( ISSCROLLLOCKACTIVE );
HB_FUNC( _GETSTATUSITEMWIDTH );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( _GETITEM );
HB_FUNC( _SETSTATUSPROGRESSMESSAGE );
HB_FUNC_EXTERN( CREATEPROGRESSBARITEM );
HB_FUNC( _SETSTATUSPROGRESSPOS );
HB_FUNC_EXTERN( SETPOSPROGRESSBARITEM );
HB_FUNC( _SETSTATUSPROGRESSRANGE );
HB_FUNC_EXTERN( MAKELONG );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_STATUS )
{ "_BEGINMESSAGEBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _BEGINMESSAGEBAR )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "INITMESSAGEBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITMESSAGEBAR )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "GETCLIENTRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCLIENTRECT )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SETSTATUSBARKBD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSBARKBD )}, NULL },
{ "_ENDMESSAGEBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDMESSAGEBAR )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "GETCONTROLINDEXBYHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEXBYHANDLE )}, NULL },
{ "GETPARENTFORMNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPARENTFORMNAME )}, NULL },
{ "_DEFINEITEMMESSAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEITEMMESSAGE )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "REFRESHPROGRESSITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( REFRESHPROGRESSITEM )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "MAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAX )}, NULL },
{ "GETTEXTWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEXTWIDTH )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "HB_UAT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_UAT )}, NULL },
{ "_DEFINELETTERORDIGITHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINELETTERORDIGITHOTKEY )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "INITITEMBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITITEMBAR )}, NULL },
{ "ISARRAYRGB", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISARRAYRGB )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "HB_BITOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITOR )}, NULL },
{ "_SETSTATUSCLOCK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSCLOCK )}, NULL },
{ "AMPM", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( AMPM )}, NULL },
{ "TIME", {HB_FS_PUBLIC}, {HB_FUNCNAME( TIME )}, NULL },
{ "_DEFINETIMER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETIMER )}, NULL },
{ "_SETITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETITEM )}, NULL },
{ "_SETSTATUSKEYBRD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSKEYBRD )}, NULL },
{ "KEYTOGGLEPLATFORM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( KEYTOGGLEPLATFORM )}, NULL },
{ "ISNUMLOCKACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISNUMLOCKACTIVE )}, NULL },
{ "ISCAPSLOCKACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISCAPSLOCKACTIVE )}, NULL },
{ "ISINSERTACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISINSERTACTIVE )}, NULL },
{ "_SETSTATUSICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETSTATUSICON )}, NULL },
{ "KEYTOGGLENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( KEYTOGGLENT )}, NULL },
{ "KEYTOGGLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( KEYTOGGLE )}, NULL },
{ "_ISOWNERDRAWSTATUSBARITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ISOWNERDRAWSTATUSBARITEM )}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "_SETSTATUSITEMPROPERTY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSITEMPROPERTY )}, NULL },
{ "ISSCROLLLOCKACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISSCROLLLOCKACTIVE )}, NULL },
{ "_GETSTATUSITEMWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETSTATUSITEMWIDTH )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_GETITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETITEM )}, NULL },
{ "_SETSTATUSPROGRESSMESSAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSPROGRESSMESSAGE )}, NULL },
{ "CREATEPROGRESSBARITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEPROGRESSBARITEM )}, NULL },
{ "_SETSTATUSPROGRESSPOS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSPROGRESSPOS )}, NULL },
{ "SETPOSPROGRESSBARITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPOSPROGRESSBARITEM )}, NULL },
{ "_SETSTATUSPROGRESSRANGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _SETSTATUSPROGRESSRANGE )}, NULL },
{ "MAKELONG", {HB_FS_PUBLIC}, {HB_FUNCNAME( MAKELONG )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_STATUS, "h_status.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_STATUS
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_STATUS )
   #include "hbiniseg.h"
#endif

HB_FUNC( _BEGINMESSAGEBAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 10 );
	hb_xvmSetLine( 66 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 4 );
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 201L ) ) break;
	hb_xvmSetLine( 72 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 73 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 77 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 81 );
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
lab00003: ;
	hb_xvmSetLine( 84 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 13 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmDo( 7 ) ) break;
lab00004: ;
	hb_xvmSetLine( 88 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 89 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 93 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 97 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 99 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 18L ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00007;
lab00006: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 13 );
lab00007: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushStringConst( "MESSAGEBAR", 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 125 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 126 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 127 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 128 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 134 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 135 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 138 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 140 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 161 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00008: ;
	hb_xvmSetLine( 164 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 202L ) ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 170 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDMESSAGEBAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 178 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 179 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 201L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 189 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 202L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 191 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "StatusItem", 10 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 201L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Title", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 12 ) ) break;
lab00003: ;
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "ProgressMessage", 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00004: ;
	hb_xvmSetLine( 198 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 201L ) ) break;
	hb_xvmSetLine( 199 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 202L ) ) break;
	hb_xvmSetLine( 201 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEITEMMESSAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 8, 15 );
	hb_xvmSetLine( 213 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 214 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	hb_xvmPopLocal( 19 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 23 );
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 19 );
lab00002: ;
	hb_xvmSetLine( 223 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00003: ;
	hb_xvmSetLine( 227 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 228 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 201L ) ) break;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 231 );
	hb_xvmCopyLocals( 19, 18 );
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 234 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 21 );
	hb_xvmSetLine( 236 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 238 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushInteger( 70 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmAddInt( 6L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
lab00005: ;
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 23 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 247 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 4 ) ) break;
lab00006: ;
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "&", 1 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
lab00007: ;
	hb_xvmSetLine( 254 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 255 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPop( 273L ) ) break;
lab00008: ;
	hb_xvmSetLine( 258 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	hb_xvmPushInteger( 202 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEq() ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 259 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	hb_xvmPopLocal( 7 );
	goto lab00010;
lab00009: ;
	hb_xvmSetLine( 261 );
	hb_xvmLocalSetInt( 8, 1L );
lab00010: ;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "RAISED", 6 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushInteger( 1 );
	goto lab00013;
lab00011: ;
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "FLAT", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushInteger( 2 );
	goto lab00013;
lab00012: ;
	hb_xvmPushInteger( 0 );
lab00013: ;
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 271 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushStringConst( "ITEMMESSAGE", 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 309 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 310 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 317 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 318 );
	hb_xvmPushSymbol( symbols + 15 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 321 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 322 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 323 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmArrayPop() ) break;
lab00015: ;
	hb_xvmSetLine( 326 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00016;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
lab00016: ;
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushInteger( 1025 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 202L ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 4096 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00017: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushLocal( 16 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSCLOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 8 );
	hb_xvmSetLine( 337 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 92 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 70 );
lab00002: ;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 339 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 340 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "TimerBar", 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmFunction( 0 ) ) break;
lab00004: ;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 15 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 345 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00005: ;
	hb_xvmSetLine( 350 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "StatusTimer", 11 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1000 );
	{
		static const HB_BYTE codeblock[ 45 ] = {
			0, 0, 4, 0, 1, 0, 2, 0, 9, 0, 6, 0, 176, 42, 0, 95, 
			255, 95, 254, 95, 253, 95, 252, 28, 14, 176, 39, 0, 176, 40, 0, 12, 
			0, 12, 1, 25, 7, 176, 40, 0, 12, 0, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 352 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSKEYBRD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 5 );
	hb_xvmSetLine( 360 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 75 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	{
		static const HB_BYTE codeblock[ 9 ] = {
			176, 44, 0, 93, 144, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 5 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "TimerNum", 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "NumLock", 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmAddInt( 20L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushStringConst( "zzz_led_on", 10 );
	goto lab00004;
lab00003: ;
	hb_xvmPushStringConst( "zzz_led_off", 11 );
lab00004: ;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 44, 0, 92, 20, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 5 );
lab00006: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "TimerCaps", 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "CapsLock", 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmAddInt( 25L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "zzz_led_on", 10 );
	goto lab00008;
lab00007: ;
	hb_xvmPushStringConst( "zzz_led_off", 11 );
lab00008: ;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	{
		static const HB_BYTE codeblock[ 8 ] = {
			176, 44, 0, 92, 45, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 5 );
lab00010: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 377 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "TimerInsert", 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "Insert", 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushStringConst( "zzz_led_on", 10 );
	goto lab00012;
lab00011: ;
	hb_xvmPushStringConst( "zzz_led_off", 11 );
lab00012: ;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 380 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00013;
	hb_xvmSetLine( 381 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00013: ;
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "StatusKeyBrd", 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 250 );
	{
		static const HB_BYTE codeblock[ 156 ] = {
			0, 0, 5, 0, 1, 0, 2, 0, 6, 0, 7, 0, 8, 0, 176, 48, 
			0, 95, 255, 95, 254, 95, 253, 176, 45, 0, 12, 0, 28, 17, 106, 11, 
			122, 122, 122, 95, 108, 101, 100, 95, 111, 110, 0, 25, 16, 106, 12, 122, 
			122, 122, 95, 108, 101, 100, 95, 111, 102, 102, 0, 20, 4, 176, 48, 0, 
			95, 255, 95, 254, 95, 252, 176, 46, 0, 12, 0, 28, 17, 106, 11, 122, 
			122, 122, 95, 108, 101, 100, 95, 111, 110, 0, 25, 16, 106, 12, 122, 122, 
			122, 95, 108, 101, 100, 95, 111, 102, 102, 0, 20, 4, 176, 48, 0, 95, 
			255, 95, 254, 95, 251, 176, 47, 0, 12, 0, 28, 17, 106, 11, 122, 122, 
			122, 95, 108, 101, 100, 95, 111, 110, 0, 25, 16, 106, 12, 122, 122, 122, 
			95, 108, 101, 100, 95, 111, 102, 102, 0, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 392 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( KEYTOGGLEPLATFORM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 397 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 180L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 400 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 403 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ISOWNERDRAWSTATUSBARITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 409 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 410 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 416 );
	hb_xvmLocalSetInt( 2, 1L );
lab00002: ;
	hb_xvmSetLine( 419 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00003: ;
	hb_xvmSetLine( 421 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 423 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "ITEMMESSAGE", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 425 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 427 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_stackPop();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
lab00004: ;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 428 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 429 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 431 );
	hb_xvmCopyLocals( 8, 3 );
lab00006: ;
	hb_xvmSetLine( 435 );
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 441 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
lab00008: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 443 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( AMPM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 449 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 452 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 24L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 453 );
	hb_xvmPushStringConst( "12", 2 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " am", 3 );
	hb_xvmLocalAdd( 1 );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 454 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmLessThenIntIs( 12L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 455 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( " am", 3 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 456 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 12L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 457 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( " pm", 3 );
	if( hb_xvmPlusEqPop() ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( -12L ) ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " pm", 3 );
	hb_xvmLocalAdd( 1 );
lab00005: ;
	hb_xvmSetLine( 462 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSBARKBD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "StatusItem", 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Title", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "RAISED", 6 );
	if( hb_xvmDo( 10 ) ) break;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "StatusItem", 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "CAP", 3 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 449L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 38 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 36 );
lab00002: ;
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 14 ) ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "StatusItem", 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "NUM", 3 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 42 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 14 ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "StatusItem", 10 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "SCRL", 4 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 44 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmPushInteger( 192 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 14 ) ) break;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushStringConst( "StatusBarKbd", 12 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 250 );
	{
		static const HB_BYTE codeblock[ 121 ] = {
			0, 0, 1, 0, 4, 0, 176, 56, 0, 92, 2, 176, 46, 0, 12, 0, 
			28, 10, 121, 121, 121, 4, 3, 0, 25, 14, 93, 192, 0, 93, 192, 0, 
			93, 192, 0, 4, 3, 0, 95, 255, 92, 4, 20, 4, 176, 56, 0, 92, 
			3, 176, 45, 0, 12, 0, 28, 10, 121, 121, 121, 4, 3, 0, 25, 14, 
			93, 192, 0, 93, 192, 0, 93, 192, 0, 4, 3, 0, 95, 255, 92, 4, 
			20, 4, 176, 56, 0, 92, 4, 176, 57, 0, 12, 0, 28, 10, 121, 121, 
			121, 4, 3, 0, 25, 14, 93, 192, 0, 93, 192, 0, 93, 192, 0, 4, 
			3, 0, 95, 255, 92, 4, 12, 4, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETSTATUSITEMWIDTH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 496 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 499 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 501 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 503 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "ITEMMESSAGE", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 504 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 507 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00003: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 509 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 4 );
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
lab00005: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSITEMPROPERTY )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 516 );
	hb_xvmLocalSetInt( 7, 0L );
	hb_xvmSetLine( 519 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
lab00001: ;
	hb_xvmSetLine( 521 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocalByRef( 5 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 523 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "ITEMMESSAGE", 11 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 525 );
	if( hb_xvmLocalIncPush( 7 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 527 );
	goto lab00007;
lab00002: ;
	hb_xvmSetLine( 529 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00008;
lab00003: ;
	hb_xvmSetLine( 532 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00008;
lab00004: ;
	hb_xvmSetLine( 535 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00008;
lab00005: ;
	hb_xvmSetLine( 538 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00008;
lab00006: ;
	hb_xvmSetLine( 541 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPop() ) break;
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 4 );
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
			goto lab00002;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 2L )
		{
			hb_stackPop();
			goto lab00003;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 3L )
		{
			hb_stackPop();
			goto lab00004;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 4L )
		{
			hb_stackPop();
			goto lab00005;
		}
		if( ( type & HB_IT_NUMINT ) != 0 && lVal == 5L )
		{
			hb_stackPop();
			goto lab00006;
		}
		hb_stackPop();
	}
lab00008: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 546 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00009: ;
	hb_xvmSetLine( 549 );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 555 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
lab00011: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 557 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSPROGRESSMESSAGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 8 );
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 567 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 568 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocalByRef( 8 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 569 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 70 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 570 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 571 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 573 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 574 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 577 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 582 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushStringConst( "ProgressMessage", 15 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushNil();
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 584 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "ProgressMessage", 15 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 587 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 588 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 589 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 590 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 592 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSPROGRESSPOS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 599 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "ProgressMessage", 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 603 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _SETSTATUSPROGRESSRANGE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 610 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushStringConst( "ProgressMessage", 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmPushMemvar( symbols + 1 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushInteger( 1025 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 614 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

