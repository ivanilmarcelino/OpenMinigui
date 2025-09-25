/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_windowsMDI.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( MDIEVENTS );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( LEN );
HB_FUNC( GETOBJECTBYCLIENTMDI );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( _DOWINDOWEVENTPROCEDURE );
HB_FUNC_EXTERN( ISWINDOWSIZED );
HB_FUNC_EXTERN( GETESCAPESTATE );
HB_FUNC( _MDICHILDCLOSE );
HB_FUNC_EXTERN( ENUMPROPSEX );
HB_FUNC_EXTERN( HB_LEFTEQI );
HB_FUNC_EXTERN( REMOVEPROP );
HB_FUNC_EXTERN( _REMOVEWINDOWPROP );
HB_FUNC_EXTERN( _ERASECONTROL );
HB_FUNC_EXTERN( ISWINDOWHANDLE );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( _DELNAMELIST );
HB_FUNC_EXTERN( EVENTS );
HB_FUNC( _DEFINECHILDMDIWINDOW );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITMDICHILDWINDOW );
HB_FUNC_EXTERN( _SETWINDOWPROP );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( SETWINDOWCURSOR );
HB_FUNC_EXTERN( INITTOOLTIP );
HB_FUNC_EXTERN( SETTOOLTIPBALLOON );
HB_FUNC_EXTERN( SETWINDOWBACKGROUND );
HB_FUNC_EXTERN( PAINTBKGND );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( _SETTHISFORMINFO );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC_EXTERN( PROCFILE );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC( _ENDMDICHILDWINDOW );
HB_FUNC( _ACTIVATEMDIWINDOW );
HB_FUNC_EXTERN( _ISWINDOWACTIVE );
HB_FUNC_EXTERN( _SHOWWINDOW );
HB_FUNC_EXTERN( _SETACTIVATIONFLAG );
HB_FUNC_EXTERN( _PROCESSINITPROCEDURE );
HB_FUNC_EXTERN( _REFRESHDATACONTROLS );
HB_FUNC_EXTERN( SHOWWINDOW );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC_EXTERN( _SETFOCUSEDSPLITCHILD );
HB_FUNC_EXTERN( _SETACTIVATIONFOCUS );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC_EXTERN( _SETNEXTFOCUS );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( MSGSTOP );
HB_FUNC_EXTERN( MSGYESNO );
HB_FUNC( ACTIVATEMDICHILDWINDOW );
HB_FUNC_EXTERN( _GETWINDOWPROP );
HB_FUNC( _MDIWINDOWSACTIVATE );
HB_FUNC( _CLOSEACTIVEMDI );
HB_FUNC( GETACTIVEMDIHANDLE );
HB_FUNC( DESTROYACTIVEMDI );
HB_FUNC( _MDICHILDCLOSEALL );
HB_FUNC( _MDICHILDRESTOREALL );
HB_FUNC( _MDIWINDOWSRESTORE );
HB_FUNC( _MDIWINDOWSTILE );
HB_FUNC( _MDIWINDOWSCASCADE );
HB_FUNC( _MDIWINDOWSICONS );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_WINDOWSMDI )
{ "MDIEVENTS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( MDIEVENTS )}, NULL },
{ "TYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TYPE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_TSB_ACONTROLHWND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETOBJECTBYCLIENTMDI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETOBJECTBYCLIENTMDI )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_DOWINDOWEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DOWINDOWEVENTPROCEDURE )}, NULL },
{ "ISWINDOWSIZED", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWSIZED )}, NULL },
{ "GETESCAPESTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETESCAPESTATE )}, NULL },
{ "_MDICHILDCLOSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDICHILDCLOSE )}, NULL },
{ "ENUMPROPSEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENUMPROPSEX )}, NULL },
{ "HB_LEFTEQI", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_LEFTEQI )}, NULL },
{ "REMOVEPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( REMOVEPROP )}, NULL },
{ "_REMOVEWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _REMOVEWINDOWPROP )}, NULL },
{ "_ERASECONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ERASECONTROL )}, NULL },
{ "ISWINDOWHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWHANDLE )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "MVAR", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_DELNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DELNAMELIST )}, NULL },
{ "EVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( EVENTS )}, NULL },
{ "_DEFINECHILDMDIWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINECHILDMDIWINDOW )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITMDICHILDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITMDICHILDWINDOW )}, NULL },
{ "_SETWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETWINDOWPROP )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "SETWINDOWCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWCURSOR )}, NULL },
{ "INITTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITTOOLTIP )}, NULL },
{ "SETTOOLTIPBALLOON", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIPBALLOON )}, NULL },
{ "SETWINDOWBACKGROUND", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWBACKGROUND )}, NULL },
{ "PAINTBKGND", {HB_FS_PUBLIC}, {HB_FUNCNAME( PAINTBKGND )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "_SETTHISFORMINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETTHISFORMINFO )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "_CPROCFILE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PROCFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCFILE )}, NULL },
{ "_CPROCNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "_NPROCLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "_ENDMDICHILDWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDMDICHILDWINDOW )}, NULL },
{ "_ACTIVATEMDIWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ACTIVATEMDIWINDOW )}, NULL },
{ "_ISWINDOWACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWACTIVE )}, NULL },
{ "_SHOWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SHOWWINDOW )}, NULL },
{ "_SETACTIVATIONFLAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETACTIVATIONFLAG )}, NULL },
{ "_PROCESSINITPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _PROCESSINITPROCEDURE )}, NULL },
{ "_REFRESHDATACONTROLS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _REFRESHDATACONTROLS )}, NULL },
{ "SHOWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHOWWINDOW )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "_SETFOCUSEDSPLITCHILD", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFOCUSEDSPLITCHILD )}, NULL },
{ "_SETACTIVATIONFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETACTIVATIONFOCUS )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "_SETNEXTFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNEXTFOCUS )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "MSGSTOP", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGSTOP )}, NULL },
{ "MSGYESNO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGYESNO )}, NULL },
{ "ACTIVATEMDICHILDWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ACTIVATEMDICHILDWINDOW )}, NULL },
{ "_GETWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETWINDOWPROP )}, NULL },
{ "_MDIWINDOWSACTIVATE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDIWINDOWSACTIVATE )}, NULL },
{ "_CLOSEACTIVEMDI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CLOSEACTIVEMDI )}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "DESTROYACTIVEMDI", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DESTROYACTIVEMDI )}, NULL },
{ "_MDICHILDCLOSEALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDICHILDCLOSEALL )}, NULL },
{ "_MDICHILDRESTOREALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDICHILDRESTOREALL )}, NULL },
{ "_MDIWINDOWSRESTORE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDIWINDOWSRESTORE )}, NULL },
{ "_MDIWINDOWSTILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDIWINDOWSTILE )}, NULL },
{ "_MDIWINDOWSCASCADE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDIWINDOWSCASCADE )}, NULL },
{ "_MDIWINDOWSICONS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _MDIWINDOWSICONS )}, NULL },
{ "_TSB_ACLIENTMDIHWND", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_TSB_ACONTROLOBJECTS", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_WINDOWSMDI, "h_windowsMDI.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_WINDOWSMDI
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_WINDOWSMDI )
   #include "hbiniseg.h"
#endif

HB_FUNC( MDIEVENTS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 90 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 256L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 257L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_TSB_aControlhWnd", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 94 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 96 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	goto lab00022;
lab00002: ;
	hb_xvmSetLine( 101 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 522L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_TSB_aControlhWnd", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 3 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 106 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 109 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	goto lab00022;
lab00004: ;
	hb_xvmSetLine( 116 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 546L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 119 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 122 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 104L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00022;
lab00005: ;
	hb_xvmSetLine( 129 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 135L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 132 );
	hb_xvmRetInt( 4L );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 71L, &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 70L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 138 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 141 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 144 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 84L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00022;
lab00009: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00014;
	hb_xvmSetLine( 158 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00022;
	hb_xvmSetLine( 160 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 161 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 163 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 130L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00010: ;
	hb_xvmSetLine( 167 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 131L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 171 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 132L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 103L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00013: ;
	goto lab00022;
lab00014: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 16L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 189 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmLessThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00015;
	hb_xvmSetLine( 190 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00015: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
	break;
lab00016: ;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	hb_xvmSetLine( 199 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 201 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 203 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	{
		static const HB_BYTE codeblock[ 31 ] = {
			3, 0, 0, 0, 176, 14, 0, 95, 2, 106, 5, 72, 77, 71, 95, 0, 
			12, 2, 28, 11, 176, 15, 0, 95, 1, 95, 2, 20, 2, 120, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PROP_CFILE", 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "PROP_MODIFIED", 13 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	goto lab00019;
lab00017: ;
	hb_xvmSetLine( 217 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
lab00018: ;
	hb_xvmSetLine( 216 );
	if( hb_xvmLocalIncPush( 6 ) ) break;
lab00019: ;
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 100L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 100L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( 274 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 61536 );
#else
	hb_xvmPushLong( 61536L );
#endif
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00020: ;
	hb_xvmSetLine( 228 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmMemvarAdd( symbols + 20 );
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmPushMemvar( symbols + 20 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 237 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 238 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 95L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 240 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 99L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 98L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 100L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 101L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 102L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 103L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 248 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 104L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 105L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 250 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 84L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 176L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 107L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 108L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 109L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 110L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 111L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 416L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 261 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 262 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 117L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 118L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 112L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 113L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 119L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 120L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 269 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 123L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 271 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 124L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 125L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 127L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 128L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 276 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 130L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 278 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 131L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 132L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 280 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 253L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 133L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 282 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 177L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 252L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 284 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 446L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 258L ) ) break;
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 293 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 16L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmSetLine( 294 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 4 ) ) break;
lab00022: ;
	hb_xvmSetLine( 299 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINECHILDMDIWINDOW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 9, 25 );
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( -1 );
	hb_xvmPushInteger( -1 );
	hb_xvmPushInteger( -1 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 313 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 314 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 210L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 317 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 319 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 415L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 320 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 323 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00003: ;
	hb_xvmSetLine( 327 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 329 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmLessEqualThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "Main Window Is Not Defined.", 27 );
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 334 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 337 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 338 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "MdiChild Windows can be defined only inside MDI Window.", 55 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 341 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLongLong( HB_LL( 2147483648 ) );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLongLong( HB_LL( 2147483648 ) );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLongLong( HB_LL( 2147483648 ) );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLongLong( HB_LL( 2147483648 ) );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 346 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 35L ) ) break;
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 36L ) ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 352 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 354 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 28 );
	hb_xvmSetLine( 356 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 33L ) ) break;
	hb_xvmSetLine( 357 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 34L ) ) break;
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 360 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 362 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
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
	if( hb_xvmFunction( 11 ) ) break;
	hb_xvmPopLocal( 32 );
	hb_xvmSetLine( 364 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushStringConst( "PROP_CFILE", 10 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushStringConst( "No Title", 8 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 11 );
lab00008: ;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 365 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushStringConst( "PROP_MODIFIED", 13 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 369 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 31 );
	hb_xvmSetLine( 371 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 372 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 377 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 379 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 32 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00010: ;
	hb_xvmSetLine( 383 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 387 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 390 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 395 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 396 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 95L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 398 );
	hb_xvmPushStringConst( "Y", 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 399 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 400 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 98L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 401 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 99L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 402 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 100L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 403 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 101L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 404 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 102L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 103L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 406 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 104L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 105L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 408 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 84L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 409 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 176L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 410 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 411 );
	hb_xvmPushLocal( 31 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 412 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 107L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 413 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 108L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 414 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 109L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 415 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 110L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 416 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 111L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 417 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 416L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 418 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 112L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 419 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 113L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 420 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 421 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 422 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 423 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 117L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 424 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 118L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 425 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 119L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 426 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 120L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 427 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 428 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 429 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 123L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 430 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 124L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 431 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 125L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 432 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 433 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 127L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 434 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 128L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 435 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 130L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 437 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 131L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 438 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 132L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 439 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 253L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 133L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 441 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 177L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 442 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 252L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 443 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 446L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 444 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 446 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 442L ) ) break;
	hb_xvmPushLocal( 30 );
	if( hb_xvmArrayPop() ) break;
	goto lab00012;
lab00011: ;
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmInc() ) break;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 459 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 95L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushStringConst( "Y", 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 98L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 465 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 99L ) ) break;
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 100L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 101L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 102L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 103L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 470 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 104L ) ) break;
	hb_xvmPushLocal( 16 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 105L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 472 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 84L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 176L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 474 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 106L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 107L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 108L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 478 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 109L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 110L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 480 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 111L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 416L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 112L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 113L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 485 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 115L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 116L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 487 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 117L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 118L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 489 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 119L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 120L ) ) break;
	hb_xvmPushLocal( 22 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 491 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 121L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 122L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 493 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 123L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 494 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 124L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 495 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 125L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 496 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 126L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 127L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 128L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 129L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 500 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 130L ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 131L ) ) break;
	hb_xvmPushLocal( 21 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 502 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 132L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 503 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 253L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 504 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 133L ) ) break;
	hb_xvmPushLocal( 24 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 505 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 177L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 252L ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 507 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 446L ) ) break;
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 508 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 510 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 442L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00012: ;
	hb_xvmSetLine( 515 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 517 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 518 );
	hb_xvmPushSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 520 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 521 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 522 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 523 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 524 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00013: ;
	hb_xvmSetLine( 529 );
	hb_xvmPushLocal( 32 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDMDICHILDWINDOW )
{
   do {
	hb_xvmSetLine( 534 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 535 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 33L ) ) break;
	hb_xvmSetLine( 536 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 538 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ACTIVATEMDIWINDOW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 546 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "Window ", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 550 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "Window ", 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " already active.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 553 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 555 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "M", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 557 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00007;
lab00003: ;
	hb_xvmSetLine( 565 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 91L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ": Non Modal Windows can't be activated when a modal window is active.", 69 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 569 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 108L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 570 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 571 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 415L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 573 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 577 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 579 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 581 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 582 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 9 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 63, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 586 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushInteger( 9 );
	{
		static const HB_BYTE codeblock[ 7 ] = {
			176, 63, 0, 120, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
lab00007: ;
	hb_xvmSetLine( 590 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDICHILDCLOSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 597 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 599 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 603 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 133L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 604 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 133L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WINDOW_ONINTERACTIVECLOSE", 25 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 606 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 607 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 612 );
	goto lab00006;
lab00002: ;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 616 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 618 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 619 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00004: ;
	goto lab00007;
lab00005: ;
	hb_xvmSetLine( 623 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 624 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 262L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 5 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmSetLine( 625 );
	hb_xvmRetInt( 1L );
	/* *** END PROC *** */
	break;
lab00006: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 184L ) ) break;
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
			goto lab00005;
		}
		hb_stackPop();
	}
lab00007: ;
	hb_xvmSetLine( 631 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 98L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 632 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 258L ) ) break;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 98L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WINDOW_RELEASE", 14 );
	if( hb_xvmDo( 3 ) ) break;
lab00008: ;
	hb_xvmSetLine( 638 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ACTIVATEMDICHILDWINDOW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 645 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 646 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "Y", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 647 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 648 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 645 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 653 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _CLOSEACTIVEMDI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 658 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 660 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 661 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00001: ;
	hb_xvmSetLine( 664 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDICHILDCLOSEALL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 671 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 2 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 672 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "Y", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 673 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 675 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 671 );
	if( hb_xvmLocalIncPush( 2 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 680 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDICHILDRESTOREALL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 687 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 688 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "Y", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 689 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 687 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 693 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETACTIVEMDIHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 699 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 553 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DESTROYACTIVEMDI )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 704 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 545 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 706 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDIWINDOWSTILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 711 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 1 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 713 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 550 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 715 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDIWINDOWSCASCADE )
{
   do {
	hb_xvmSetLine( 720 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 551 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 722 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDIWINDOWSICONS )
{
   do {
	hb_xvmSetLine( 727 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 552 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 729 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDIWINDOWSRESTORE )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 734 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 547 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 736 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _MDIWINDOWSACTIVATE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 741 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 742 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 546 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 745 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETOBJECTBYCLIENTMDI )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 751 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 753 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 79 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 754 );
	if( hb_xvmPushMemvar( symbols + 80 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 757 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

