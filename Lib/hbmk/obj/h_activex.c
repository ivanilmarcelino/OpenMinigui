/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_activex.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_INIT( _INITACTIVEX );
HB_FUNC_EXTERN( INSTALLMETHODHANDLER );
HB_FUNC_EXTERN( INSTALLPROPERTYHANDLER );
HB_FUNC( _DEFINEACTIVEX );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( TACTIVEX );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( CHANGESTYLE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( RELEASEACTIVEX );
HB_FUNC_EXTERN( GETCONTROLTYPE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC( SETACTIVEXOBJECT );
HB_FUNC_EXTERN( MSGEXCLAMATION );
HB_FUNC( GETACTIVEXOBJECT );
HB_FUNC( _GETCONTROLOBJECT );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TACTIVEX_NEW );
HB_FUNC_STATIC( TACTIVEX_LOAD );
HB_FUNC_STATIC( TACTIVEX_RESIZE );
HB_FUNC_STATIC( TACTIVEX_HIDE );
HB_FUNC_STATIC( TACTIVEX_SHOW );
HB_FUNC_STATIC( TACTIVEX_RELEASE );
HB_FUNC_STATIC( TACTIVEX_REFRESH );
HB_FUNC_STATIC( TACTIVEX_ADJUST );
HB_FUNC_STATIC( TACTIVEX_GETROW );
HB_FUNC_STATIC( TACTIVEX_GETCOL );
HB_FUNC_STATIC( TACTIVEX_GETWIDTH );
HB_FUNC_STATIC( TACTIVEX_GETHEIGHT );
HB_FUNC_STATIC( TACTIVEX_EVENTMAP );
HB_FUNC_STATIC( TACTIVEX_ONERROR );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC( ATLAXWININIT );
HB_FUNC_STATIC( CREATEWINDOWEX );
HB_FUNC_EXTERN( MOVEWINDOW );
HB_FUNC( ATLAXGETDISP );
HB_FUNC_EXTERN( __BREAKBLOCK );
HB_FUNC_EXTERN( CREATEOBJECT );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC( SETUPCONNECTIONPOINT );
HB_FUNC_EXTERN( DESTROYWINDOW );
HB_FUNC( SHUTDOWNCONNECTIONPOINT );
HB_FUNC( RELEASEDISPATCH );
HB_FUNC( ATLAXWINEND );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( __GETMESSAGE );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( HB_EXECFROMARRAY );
HB_FUNC_EXTERN( HB_APARAMS );
HB_FUNC_INITSTATICS();
HB_FUNC( CLASS_TACTIVEX );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_ACTIVEX )
{ "_INITACTIVEX$", {HB_FS_INIT | HB_FS_LOCAL}, {HB_INIT_FUNCNAME( _INITACTIVEX )}, NULL },
{ "INSTALLMETHODHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLMETHODHANDLER )}, NULL },
{ "INSTALLPROPERTYHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLPROPERTYHANDLER )}, NULL },
{ "_DEFINEACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEACTIVEX )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX )}, NULL },
{ "LOAD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HATL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HSINK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "EVENTMAP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "CHANGESTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHANGESTYLE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "RELEASEACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEACTIVEX )}, NULL },
{ "GETCONTROLTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLTYPE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "RELEASE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETACTIVEXOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETACTIVEXOBJECT )}, NULL },
{ "MSGEXCLAMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGEXCLAMATION )}, NULL },
{ "GETACTIVEXOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETACTIVEXOBJECT )}, NULL },
{ "_GETCONTROLOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _GETCONTROLOBJECT )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_NEW )}, NULL },
{ "TACTIVEX_LOAD", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_LOAD )}, NULL },
{ "TACTIVEX_RESIZE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_RESIZE )}, NULL },
{ "TACTIVEX_HIDE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_HIDE )}, NULL },
{ "TACTIVEX_SHOW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_SHOW )}, NULL },
{ "TACTIVEX_RELEASE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_RELEASE )}, NULL },
{ "TACTIVEX_REFRESH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_REFRESH )}, NULL },
{ "TACTIVEX_ADJUST", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_ADJUST )}, NULL },
{ "TACTIVEX_GETROW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETROW )}, NULL },
{ "TACTIVEX_GETCOL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETCOL )}, NULL },
{ "TACTIVEX_GETWIDTH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETWIDTH )}, NULL },
{ "TACTIVEX_GETHEIGHT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_GETHEIGHT )}, NULL },
{ "TACTIVEX_EVENTMAP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_EVENTMAP )}, NULL },
{ "SETONERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TACTIVEX_ONERROR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TACTIVEX_ONERROR )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "_NROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CWINDOWNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPROGID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NOLDWINWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NOLDWINHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CWINDOWNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATLAXWININIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXWININIT )}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATEWINDOWEX", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CREATEWINDOWEX )}, NULL },
{ "CPROGID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "MOVEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( MOVEWINDOW )}, NULL },
{ "NCOL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NROW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ATLAXGETDISP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXGETDISP )}, NULL },
{ "_HATL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__BREAKBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( __BREAKBLOCK )}, NULL },
{ "_OOLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CREATEOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( CREATEOBJECT )}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "DESCRIPTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETUPCONNECTIONPOINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETUPCONNECTIONPOINT )}, NULL },
{ "AAXEV", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AAXEXEC", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HSINK", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OOLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BHIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOLDWINWIDTH", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOLDWINHEIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BHIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESTROYWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DESTROYWINDOW )}, NULL },
{ "SHUTDOWNCONNECTIONPOINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SHUTDOWNCONNECTIONPOINT )}, NULL },
{ "RELEASEDISPATCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( RELEASEDISPATCH )}, NULL },
{ "ATLAXWINEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ATLAXWINEND )}, NULL },
{ "HIDE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SHOW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "__GETMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( __GETMESSAGE )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HB_EXECFROMARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_EXECFROMARRAY )}, NULL },
{ "HB_APARAMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_APARAMS )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL },
{ "CLASS_TACTIVEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( CLASS_TACTIVEX )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_ACTIVEX, "h_activex.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_ACTIVEX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_ACTIVEX )
   #include "hbiniseg.h"
#endif

HB_FUNC_INIT( _INITACTIVEX )
{
   do {
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "Release", 7 );
	hb_xvmPushStringConst( "ReleaseActiveX", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 61 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushStringConst( "XObject", 7 );
	hb_xvmPushStringConst( "SetActiveXObject", 16 );
	hb_xvmPushStringConst( "GetActiveXObject", 16 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 63 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEACTIVEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 9 );
	hb_xvmSetLine( 76 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 77 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00003;
lab00002: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00003: ;
	hb_xvmPopLocal( 2 );
lab00004: ;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 82 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00005: ;
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 88 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 91 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 92 );
	hb_xvmPushFuncSymbol( symbols + 6 );
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
lab00007: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " PROGID Property Invalid Type.", 30 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00008: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " PROGID Can't be empty.", 23 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00009: ;
	hb_xvmSetLine( 104 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 12 );
	hb_xvmSetLine( 108 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 6 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 112 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 114 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 115 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 8 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			1, 0, 1, 0, 14, 0, 48, 19, 0, 95, 255, 95, 1, 122, 1, 95, 
			1, 92, 2, 1, 112, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00010: ;
	hb_xvmSetLine( 123 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( 512 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00012: ;
	hb_xvmSetLine( 131 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 133 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushStringConst( "ACTIVEX", 7 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 143 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 145 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 146 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 148 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 150 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 151 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 152 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 153 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 154 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 157 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 163 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( -1 );
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 164 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( -1 );
lab00016: ;
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 166 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 168 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 169 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 174 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 176 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 178 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 180 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 13 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 182 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( RELEASEACTIVEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ACTIVEX", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 191 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 194 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00003: ;
	hb_xvmSetLine( 205 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( SETACTIVEXOBJECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ACTIVEX", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushStringConst( "This Property is Read Only!", 27 );
	hb_xvmPushStringConst( "Warning", 7 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 215 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 223 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETACTIVEXOBJECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "ACTIVEX", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 232 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 238 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPop( 175L ) ) break;
lab00002: ;
	hb_xvmSetLine( 242 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _GETCONTROLOBJECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 250 );
	hb_xvmRetNil();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 253 );
	if( hb_xvmPushMemvar( symbols + 4 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TACTIVEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 106 );
	hb_xvmSetLine( 277 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TActiveX", 8 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 11 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 280 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "oOle", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 281 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hWnd", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 282 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cWindowName", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 283 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cProgId", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 284 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hSink", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 285 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hAtl", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 286 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nRow", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 287 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCol", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 288 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nWidth", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 289 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nHeight", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 290 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nOldWinWidth", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 291 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nOldWinHeight", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 292 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bHide", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 295 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aAxEv", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 296 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aAxExec", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 300 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 303 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Load", 4 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 306 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ReSize", 6 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 309 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Hide", 4 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 312 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Show", 4 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 315 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Release", 7 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 318 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Refresh", 7 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 321 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Adjust", 6 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 324 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetRow", 6 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 327 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetCol", 6 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 330 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetWidth", 8 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 333 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetHeight", 9 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 336 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "EventMap", 8 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 342 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSymbol( symbols + 54 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 345 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushVParams();
	if( hb_xvmMacroSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 353 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 3, 0L );
lab00001: ;
	hb_xvmSetLine( 354 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmLocalSetInt( 4, 0L );
lab00002: ;
	hb_xvmSetLine( 355 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
lab00003: ;
	hb_xvmSetLine( 356 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
lab00004: ;
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 358 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 359 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 360 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 361 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 362 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 363 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 364 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 366 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_LOAD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 377 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 378 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 379 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 380 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 381 );
	hb_xvmSeqBegin();
	for( ;; ) {
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmSeqBlock() ) break;
	hb_xvmSetLine( 382 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_stackPop();
	if( hb_xvmSeqEndTest() ) break;
	goto lab00001;
	}
	hb_xvmSetLine( 383 );
	if( hb_xvmSeqRecover() ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 386 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 4 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 387 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 390 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_RESIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 393 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
lab00001: ;
	hb_xvmSetLine( 396 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 397 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 398 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 399 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 400 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 401 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 403 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_ADJUST )
{
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 409 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 410 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMinus() ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 412 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 413 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 414 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "width", 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 415 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "Height", 6 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 417 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_GETROW )
{
   do {
	hb_xvmSetLine( 420 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_GETCOL )
{
   do {
	hb_xvmSetLine( 423 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_GETWIDTH )
{
   do {
	hb_xvmSetLine( 426 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_GETHEIGHT )
{
   do {
	hb_xvmSetLine( 429 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_HIDE )
{
   do {
	hb_xvmSetLine( 432 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 433 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 435 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_SHOW )
{
   do {
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 439 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 441 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_RELEASE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 444 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 445 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 448 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 453 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_REFRESH )
{
   do {
	hb_xvmSetLine( 456 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 457 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 459 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_EVENTMAP )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 465 );
	hb_xvmPushFuncSymbol( symbols + 101 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 466 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 471 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 473 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TACTIVEX_ONERROR )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 1, 0 );
	hb_xvmSetLine( 480 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 485 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 103 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 105 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 106, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( CLASS_TACTIVEX )
{
   do {
	/* *** END PROC *** */
   } while( 0 );
}

#line 496 "h_activex.prg"

#ifndef CINTERFACE
#define CINTERFACE   1                    // Enable C-style interfaces if not already defined
#endif
#ifndef NONAMELESSUNION
#define NONAMELESSUNION                   // Enable non-anonymous unions if not already defined
#endif
#include <mgdefs.h>                       // Include MiniGUI definitions
#include <commctrl.h>                     // Include common controls for GUI elements
#include <ocidl.h>                        // Include OLE Control Interface definitions
#include <hbvm.h>                         // Harbour Virtual Machine interface
#include <hbapiitm.h>                     // Harbour API item management
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );   // Declaration of a function to convert ANSI to wide strings for Unicode
#endif

// Declaration of an external function to retrieve a function address from a DLL
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Typedefs for function pointers used to initialize and manage ActiveX controls
typedef HRESULT ( WINAPI *LPAtlAxWinInit ) ( void );                 // Typedef for AtlAxWinInit function pointer
typedef HRESULT ( WINAPI *LPAtlAxGetControl ) ( HWND, IUnknown ** ); // Typedef for AtlAxGetControl function pointer

// Static global variables to hold handles and function pointers for ActiveX library
static HMODULE    hAtl = NULL;                  // Handle to Atl.dll
LPAtlAxWinInit    AtlAxWinInit;                 // Pointer to AtlAxWinInit function
LPAtlAxGetControl AtlAxGetControl;

// Pointer to AtlAxGetControl function
// Initializes the ActiveX library by loading Atl.dll and getting function pointers
static void _Ax_Init( void )
{
   if( !hAtl )                                  // If the library is not already loaded
   {
      hAtl = LoadLibrary( TEXT( "Atl.Dll" ) );  // Load Atl.dll library
      AtlAxWinInit = ( LPAtlAxWinInit ) wapi_GetProcAddress( hAtl, "AtlAxWinInit" );            // Get AtlAxWinInit function address
      AtlAxGetControl = ( LPAtlAxGetControl ) wapi_GetProcAddress( hAtl, "AtlAxGetControl" );   // Get AtlAxGetControl function address
      ( AtlAxWinInit ) (); // Initialize ActiveX (AtlAxWinInit function)
   }
}

// Harbour function to initialize the ActiveX library by calling _Ax_Init
HB_FUNC( ATLAXWININIT )
{
   _Ax_Init();
}

// Harbour function to free the ActiveX library
HB_FUNC( ATLAXWINEND )
{
   if( hAtl )              // If the library handle exists
   {
      FreeLibrary( hAtl ); // Free the Atl.dll library
      hAtl = NULL;         // Reset the handle to NULL
   }
}

// Harbour function to get IDispatch interface from an ActiveX control
HB_FUNC( ATLAXGETDISP ) // hWnd -> pDisp
{
   IUnknown    *pUnk;   // Pointer to IUnknown interface
   IDispatch   *pDisp;  // Pointer to IDispatch interface
   _Ax_Init();          // Ensure ActiveX library is initialized
   AtlAxGetControl( hmg_par_raw_HWND( 1 ), &pUnk );            // Get the IUnknown interface of the control
#if defined( __cplusplus )
   pUnk->QueryInterface( IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C++
#else
   pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C-style
#endif
   pUnk->lpVtbl->Release( pUnk );   // Release the IUnknown interface
   hmg_ret_raw_HANDLE( pDisp );     // Return the IDispatch interface to the caller
}

// Harbour function to create an ActiveX window using a ProgID
HB_FUNC_STATIC( CREATEWINDOWEX ) // ( hWnd, cProgId ) -> hActiveXWnd
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Get window name in ANSI
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert window name to Unicode if applicable
#endif
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            TEXT( "AtlAxWin" ),     // Extended window creation with AtlAxWin class
            lpWindowName,
            WS_VISIBLE | WS_CHILD,  // Window styles for visibility and child placement
            0,
            0,
            0,
            0, // Position and size (defaults to 0 here)
            hmg_par_raw_HWND( 1 ),  // Parent window handle
            0,
            0,
            NULL
         )
   ); // No additional parameters
}

// Conditional include for hash-based event handling if __USEHASHEVENTS is defined
#ifdef __USEHASHEVENTS
#include <hashapi.h>
#endif

//------------------------------------------------------------------------------
// Prototype for function that converts OLE Variant to Harbour item
HRESULT  hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

// Definition of a custom IDispatch-based COM interface for event handling
#undef INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_( INTERFACE, IDispatch )
{
   // IUnknown methods
   STDMETHOD ( QueryInterface ) ( THIS_ REFIID, void ** ) PURE;

   STDMETHOD_ ( ULONG, AddRef ) ( THIS ) PURE;

   STDMETHOD_ ( ULONG, Release ) ( THIS ) PURE;

   // IDispatch methods
   STDMETHOD_ ( ULONG, GetTypeInfoCount ) ( THIS_ UINT * ) PURE;

   STDMETHOD_ ( ULONG, GetTypeInfo ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;

   STDMETHOD_ ( ULONG, GetIDsOfNames ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;

   STDMETHOD_ ( ULONG, Invoke ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};

// Explanation of the IEventHandler structure and its extended version
// IEventHandler starts with a pointer to its VTable, a required structure
// for all COM objects. The extended struct (MyRealIEventHandler) contains
// additional private members but is presented as an IEventHandler to external
// applications.
// Extended structure for event handler with private data members
typedef struct
{
   IEventHandler     *lpVtbl;             // Pointer to virtual function table (VTable) for IEventHandler
   DWORD             count;               // Reference count for memory management
   IConnectionPoint  *pIConnectionPoint;  // Pointer to connection point for event handling
   DWORD             dwEventCookie;       // Event subscription identifier
   IID               device_event_interface_iid;   // Interface identifier for the device event
   PHB_ITEM          pEvents;       // Harbour item to store events
#ifndef __USEHASHEVENTS
   PHB_ITEM          pEventsExec;   // Harbour item for direct event execution if not using hash events
#endif
} MyRealIEventHandler;

//------------------------------------------------------------------------------
// Here are IEventHandler's functions.
//------------------------------------------------------------------------------
// Every COM object's interface must have the 3 functions QueryInterface(),
// AddRef(), and Release().
// IEventHandler's QueryInterface()
static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   // Check if the GUID matches IEvenetHandler VTable's GUID. We gave the C variable name
   // IID_IEventHandler to our VTable GUID. We can use an OLE function called
   // IsEqualIID to do the comparison for us. Also, if the caller passed a
   // IUnknown GUID, then we'll likewise return the IEventHandler, since it can
   // masquerade as an IUnknown object too. Finally, if the called passed a
   // IDispatch GUID, then we'll return the IExample3, since it can masquerade
   // as an IDispatch too
   if( IsEqualIID( vTableGuid, &IID_IUnknown ) )
   {
      *ppv = ( IUnknown * ) self;

      // Increment the count of callers who have an outstanding pointer to self object
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &IID_IDispatch ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   // We don't recognize the GUID passed to us. Let the caller know self,
   // by clearing his handle, and returning E_NOINTERFACE.
   *ppv = 0;
   return E_NOINTERFACE;
}

//------------------------------------------------------------------------------
// IEventHandler's AddRef()
static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   // Increment IEventHandler's reference count, and return the updated value.
   // NOTE: We have to typecast to gain access to any data members. These
   // members are not defined  (so that an app can't directly access them).
   // Rather they are defined only above in our MyRealIEventHandler
   // struct. So typecast to that in order to access those data members
   return ++( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's Release()
static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      GlobalFree( self );
      return 0;
   }

   return( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfoCount()
static ULONG STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfo()
static ULONG STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetIDsOfNames()
static ULONG STDMETHODCALLTYPE GetIDsOfNames( IEventHandler *self, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgdispid );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's Invoke()
// self is where the action happens
// self function receives events (by their ID number) and distributes the processing
// or them or ignores them
static ULONG STDMETHODCALLTYPE Invoke
(
   IEventHandler  *self,
   DISPID         dispid,
   REFIID         riid,
   LCID           lcid,
   WORD           wFlags,
   DISPPARAMS     *params,
   VARIANT        *result,
   EXCEPINFO      *pexcepinfo,
   UINT           *puArgErr
)
{
   PHB_ITEM pItem;
   int      iArg, i;
   PHB_ITEM pItemArray[32];         // max 32 parameters?
   PHB_ITEM *pItems;
   HB_SIZE  ulPos;
   PHB_ITEM Key;

   Key = hb_itemNew( NULL );

   // We implement only a "default" interface
   if( !IsEqualIID( riid, &IID_NULL ) )
   {
      return( ULONG ) DISP_E_UNKNOWNINTERFACE;
   }

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   // delegate work to somewhere else in PRG
   //***************************************
#ifdef __USEHASHEVENTS
   if( hb_hashScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) self )->pEvents, ulPos );
#else
   #if defined( __XHARBOUR__ )
      ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0, 0 );
   #else
   ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0 );
   #endif
   if( ulPos )
   {
      PHB_ITEM pArray = hb_arrayGetItemPtr( ( ( MyRealIEventHandler * ) self )->pEventsExec, ulPos );
#endif
      PHB_ITEM pExec = hb_arrayGetItemPtr( pArray, 1 );

      if( pExec )
      {
         if( hb_vmRequestReenter() )
         {
            switch( hb_itemType( pExec ) )
            {
               case HB_IT_BLOCK:
                  {
#ifdef __XHARBOUR__
                     hb_vmPushSymbol( &hb_symEval );
#else
                     hb_vmPushEvalSym();

#endif
                     hb_vmPush( pExec );
                     break;
                  }

               case HB_IT_STRING:
                  {
                     PHB_ITEM pObject = hb_arrayGetItemPtr( pArray, 2 );
                     hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( hb_itemGetCPtr( pExec ) ) ) );

                     if( HB_IS_OBJECT( pObject ) )
                     {
                        hb_vmPush( pObject );
                     }
                     else
                     {
                        hb_vmPushNil();
                     }
                     break;
                  }

               case HB_IT_POINTER:
                  {
                     hb_vmPushSymbol( hb_dynsymSymbol( ( ( PHB_SYMB ) pExec )->pDynSym ) );
                     hb_vmPushNil();
                     break;
                  }
            }

            iArg = params->cArgs;
            for( i = 1; i <= iArg; i++ )
            {
               pItem = hb_itemNew( NULL );
               hb_oleVariantToItem( pItem, &( params->rgvarg[iArg - i] ) );
               pItemArray[i - 1] = pItem;

               // set bit i
               //ulRefMask |= ( 1L << ( i - 1 ) );
            }

            if( iArg )
            {
               pItems = pItemArray;
               if( iArg )
               {
                  for( i = 0; i < iArg; i++ )
                  {
                     hb_vmPush( ( pItems )[i] );
                  }
               }
            }

            // execute
            hb_vmDo( ( USHORT ) iArg );

            // En caso de que los parametros sean pasados por referencia
            for( i = iArg; i > 0; i-- )
            {
               if( ( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt & VT_BYREF ) == VT_BYREF )
               {
                  switch( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt )
                  {
                     //case VT_UI1|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbVal) = va_arg(argList,unsigned char*);  //pItemArray[i-1]
                     //   break;
                     case VT_I2 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.piVal ) = ( short ) hb_itemGetNI( pItemArray[i - 1] );
                        break;

                     case VT_I4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.plVal ) = ( long ) hb_itemGetNL( pItemArray[i - 1] );
                        break;

                     case VT_R4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pfltVal ) = ( float ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_R8 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdblVal ) = ( double ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_BOOL | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pboolVal ) = ( VARIANT_BOOL ) ( hb_itemGetL( pItemArray[i - 1] ) ? 0xFFFF : 0 );
                        break;

                     //case VT_ERROR|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pscode) = va_arg(argList, SCODE*);
                     //   break;
                     case VT_DATE | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdate ) = ( DATE ) ( double ) ( hb_itemGetDL( pItemArray[i - 1] ) - 2415019 );
                        break;

                        //case VT_CY|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pcyVal) = va_arg(argList, CY*);
                        //   break;
                        //case VT_BSTR|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbstrVal = va_arg(argList, BSTR*);
                        //   break;
                        //case VT_UNKNOWN|VT_BYREF:
                        //   pArg->ppunkVal = va_arg(argList, LPUNKNOWN*);
                        //   break;
                        //case VT_DISPATCH|VT_BYREF:
                        //   pArg->ppdispVal = va_arg(argList, LPDISPATCH*);
                        //   break;
                  }
               }
            }

            hb_vmRequestRestore();
         }
      }
   }

   hb_itemRelease( Key );

   return S_OK;
}

//------------------------------------------------------------------------------
// Here's IEventHandler's VTable. It never changes so we can declare it static
static const IEventHandlerVtbl   IEventHandler_Vtbl = { QueryInterface, AddRef, Release, GetTypeInfoCount, GetTypeInfo, GetIDsOfNames, Invoke };

//------------------------------------------------------------------------------
// constructor
// params:
// device_interface        - refers to the interface type of the COM object (whose event we are trying to receive).
// device_event_interface  - indicates the interface type of the outgoing interface supported by the COM object.
//                           This will be the interface that must be implemented by the Sink object.
//                           is essentially derived from IDispatch, our Sink object (self IEventHandler)
//                           is also derived from IDispatch.
typedef IEventHandler            device_interface;

// Hash  // SetupConnectionPoint( oOle:hObj, @hSink, hEvents )             -> nError
// Array // SetupConnectionPoint( oOle:hObj, @hSink, aEvents, aExecEvent ) -> nError

HB_FUNC( SETUPCONNECTIONPOINT )
{
   // Declaration of local variables
   IConnectionPointContainer  *pIConnectionPointContainerTemp = NULL;
   IUnknown                   *pIUnknown = NULL;
   IConnectionPoint           *m_pIConnectionPoint = NULL;
   IEnumConnectionPoints      *m_pIEnumConnectionPoints;
   HRESULT                    hr;
   IID                        rriid = { 0 };
   register IEventHandler     *selfobj;
   DWORD                      dwCookie = 0;

   device_interface           *pdevice_interface = ( device_interface * ) HB_PARNL( 1 );
   MyRealIEventHandler        *pThis;

   // Allocate memory for the IEventHandler object (as a MyRealIEventHandler).
   // Intentional misrepresentation of size to fit within allocated memory.
   selfobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) );

   if( !selfobj )
   {
      // Memory allocation failed, return error code.
      hr = E_OUTOFMEMORY;
   }
   else
   {
      // Set up the IEventHandler object by assigning the correct VTable and initializing the reference count.
      selfobj->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;
      ( ( MyRealIEventHandler * ) selfobj )->count = 0;

      // Assign the default interface GUID for events to IDispatch
      ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = IID_IDispatch;

      // Query the IUnknown pointer of self, used later to connect to the device_interface's Connection Point.
      hr = selfobj->lpVtbl->QueryInterface( selfobj, &IID_IUnknown, ( void ** ) ( void * ) &pIUnknown );
      if( hr == S_OK && pIUnknown )
      {
         // Query the device interface for its connection point container interface
         hr = pdevice_interface->lpVtbl->QueryInterface
            (
               pdevice_interface,
               &IID_IConnectionPointContainer,
               ( void ** ) ( void * ) &pIConnectionPointContainerTemp
            );

         if( hr == S_OK && pIConnectionPointContainerTemp )
         {
            // Retrieve the list of connection points available in the interface.
            hr = pIConnectionPointContainerTemp->lpVtbl->EnumConnectionPoints( pIConnectionPointContainerTemp, &m_pIEnumConnectionPoints );

            if( hr == S_OK && m_pIEnumConnectionPoints )
            {
               do
               {
                  // Move to the next available connection point.
                  hr = m_pIEnumConnectionPoints->lpVtbl->Next( m_pIEnumConnectionPoints, 1, &m_pIConnectionPoint, NULL );
                  if( hr == S_OK )
                  {
                     // Get the interface GUID from the connection point.
                     if( m_pIConnectionPoint->lpVtbl->GetConnectionInterface( m_pIConnectionPoint, &rriid ) == S_OK )
                     {
                        break;
                     }
                  }
               }
               while( hr == S_OK );

               // Release the enumerator once finished.
               m_pIEnumConnectionPoints->lpVtbl->Release( m_pIEnumConnectionPoints );
            }

            pIConnectionPointContainerTemp->lpVtbl->Release( pIConnectionPointContainerTemp );
            pIConnectionPointContainerTemp = NULL;
         }

         if( hr == S_OK && m_pIConnectionPoint )
         {
            if( hr == S_OK )
            {
               // Set the device event interface GUID to the retrieved GUID.
               ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = rriid;
            }

            // Connect to the connection point and store the connection cookie.
            hr = m_pIConnectionPoint->lpVtbl->Advise( m_pIConnectionPoint, pIUnknown, &dwCookie );
            ( ( MyRealIEventHandler * ) selfobj )->pIConnectionPoint = m_pIConnectionPoint;
            ( ( MyRealIEventHandler * ) selfobj )->dwEventCookie = dwCookie;
         }

         // Release the IUnknown pointer.
         pIUnknown->lpVtbl->Release( pIUnknown );
         pIUnknown = NULL;
      }
   }

   if( selfobj )
   {
      // If successful, store the events list from parameters and assign to the object.
      pThis = ( MyRealIEventHandler * ) selfobj;

#ifndef __USEHASHEVENTS
      pThis->pEventsExec = hb_itemNew( hb_param( 4, HB_IT_ANY ) );
#endif
      pThis->pEvents = hb_itemNew( hb_param( 3, HB_IT_ANY ) );
      HB_STORNL( ( LONG_PTR ) pThis, 2 );
   }

   // Return the HRESULT result code from the setup process.
   hb_retnl( hr );
}

//------------------------------------------------------------------------------
// Disconnect and clean up the connection point, removing the event sink.
//------------------------------------------------------------------------------
HB_FUNC( SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler  *self = ( MyRealIEventHandler * ) HB_PARNL( 1 );

   if( self->pIConnectionPoint )
   {
      // Unadvise the connection point, releasing the event sink.
      self->pIConnectionPoint->lpVtbl->Unadvise( self->pIConnectionPoint, self->dwEventCookie );
      self->dwEventCookie = 0;

      // Release the connection point object.
      self->pIConnectionPoint->lpVtbl->Release( self->pIConnectionPoint );
      self->pIConnectionPoint = NULL;
   }
}

//------------------------------------------------------------------------------
// Release a previously acquired IDispatch interface pointer to clean up.
//------------------------------------------------------------------------------
HB_FUNC( RELEASEDISPATCH )
{
   IDispatch   *pObj;

   // Retrieve the IDispatch pointer from parameters and release it.
   pObj = ( IDispatch * ) HB_PARNL( 1 );
   pObj->lpVtbl->Release( pObj );
}

