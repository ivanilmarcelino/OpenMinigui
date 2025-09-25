/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_objmisc.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _WINDOWCARGO );
HB_FUNC_EXTERN( HB_ISOBJECT );
HB_FUNC( _WINDOWOBJ );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( PCOUNT );
HB_FUNC( _CONTROLCARGO );
HB_FUNC( _CONTROLOBJ );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( HB_ISBLOCK );
HB_FUNC_EXTERN( _PUSHEVENTINFO );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( _POPEVENTINFO );
HB_FUNC( DO_WINDOWEVENTPROCEDURE );
HB_FUNC( _O2LOG );
HB_FUNC_EXTERN( ISERRORLOGACTIVE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( HB_FNAMEDIR );
HB_FUNC_EXTERN( _SETGETLOGFILE );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( SECONDS );
HB_FUNC_EXTERN( HB_ISCHAR );
HB_FUNC_EXTERN( AT );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( SUBSTR );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC_EXTERN( HMG_GETFORMS );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( ISWINDOWVISIBLE );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( OHMGDATA );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_STATIC( TR0 );
HB_FUNC_EXTERN( PROCFILE );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC_EXTERN( _LOGFILE );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( HB_VALTOEXP );
HB_FUNC_EXTERN( HB_FILEEXISTS );
HB_FUNC_EXTERN( HB_MEMOREAD );
HB_FUNC_EXTERN( HB_FILEDELETE );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( PADL );
HB_FUNC( _WPOST );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( DOEVENTS );
HB_FUNC( _WSEND );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC( HMG_GETWINDOWOBJECT );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( _OTHIS );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( GETFOCUS );
HB_FUNC_EXTERN( GETPARENTFORMNAME );
HB_FUNC_EXTERN( GETFORMNAMEBYHANDLE );
HB_FUNC( _PPOST );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC( _PSEND );
HB_FUNC( DO_OBJ );
HB_FUNC( HMG_ISWINDOWOBJECT );
HB_FUNC( ORECGET );
HB_FUNC_EXTERN( RIGHT );
HB_FUNC_EXTERN( HB_ATOKENS );
HB_FUNC_EXTERN( FIELDPOS );
HB_FUNC_EXTERN( FIELDGET );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( DBSTRUCT );
HB_FUNC( ORECPUT );
HB_FUNC_EXTERN( FIELDTYPE );
HB_FUNC_EXTERN( FIELDPUT );
HB_FUNC( DO_ONWNDINIT );
HB_FUNC_EXTERN( OWNDDATA );
HB_FUNC( DO_ONWNDRELEASE );
HB_FUNC_EXTERN( __OBJHASMETHOD );
HB_FUNC( DO_ONCTLINIT );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( OCNLDATA );
HB_FUNC( DO_ONCTLRELEASE );
HB_FUNC( DO_ONWNDLAUNCH );
HB_FUNC( DO_ONCTLLAUNCH );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_OBJMISC )
{ "_WINDOWCARGO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _WINDOWCARGO )}, NULL },
{ "HB_ISOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISOBJECT )}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "INDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_CONTROLCARGO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CONTROLCARGO )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "HB_ISBLOCK", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISBLOCK )}, NULL },
{ "_PUSHEVENTINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _PUSHEVENTINFO )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_POPEVENTINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _POPEVENTINFO )}, NULL },
{ "DO_WINDOWEVENTPROCEDURE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_WINDOWEVENTPROCEDURE )}, NULL },
{ "_O2LOG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _O2LOG )}, NULL },
{ "ISERRORLOGACTIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISERRORLOGACTIVE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "HB_FNAMEDIR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEDIR )}, NULL },
{ "_SETGETLOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETLOGFILE )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "SECONDS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SECONDS )}, NULL },
{ "HB_ISCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISCHAR )}, NULL },
{ "AT", {HB_FS_PUBLIC}, {HB_FUNCNAME( AT )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "SUBSTR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SUBSTR )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "HMG_GETFORMS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETFORMS )}, NULL },
{ "TYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "ISWINDOWVISIBLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWVISIBLE )}, NULL },
{ "HANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TITLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPROCFILE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPROCNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NPROCLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "OHMGDATA", {HB_FS_PUBLIC}, {HB_FUNCNAME( OHMGDATA )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "SET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TR0", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TR0 )}, NULL },
{ "PROCFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCFILE )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "_LOGFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _LOGFILE )}, NULL },
{ "CLASSNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETALL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CINI", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__ENUMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WINDOW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CALIAS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_VALTOEXP", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VALTOEXP )}, NULL },
{ "HB_FILEEXISTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEEXISTS )}, NULL },
{ "HB_MEMOREAD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_MEMOREAD )}, NULL },
{ "HB_FILEDELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FILEDELETE )}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "PADL", {HB_FS_PUBLIC}, {HB_FUNCNAME( PADL )}, NULL },
{ "_WPOST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _WPOST )}, NULL },
{ "GET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OEVENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "DOEVENTS", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOEVENTS )}, NULL },
{ "POSTMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WSEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _WSEND )}, NULL },
{ "SENDMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "HMG_GETWINDOWOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_GETWINDOWOBJECT )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "_OTHIS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _OTHIS )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "_FORMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_EVENTTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_INDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FORMNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FOCUSEDFORM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FOCUSEDCONTROL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FOCUSEDCONTROLINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FORMNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFOCUS )}, NULL },
{ "FOCUSEDFORM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FORMINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVENTTYPE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FOCUSEDCONTROLINDEX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONTROLCARGO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONTROLHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONTROLPARENTHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CONTROLPARENTNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETPARENTFORMNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPARENTFORMNAME )}, NULL },
{ "_FORMCARGO", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FORMHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FORMPARENTHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMNAMEBYHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMNAMEBYHANDLE )}, NULL },
{ "FORMPARENTHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FORMPARENTNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_FORMOBJECT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "FORMHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_PPOST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PPOST )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "ISERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ISMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_PSEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _PSEND )}, NULL },
{ "DO_OBJ", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_OBJ )}, NULL },
{ "HMG_ISWINDOWOBJECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_ISWINDOWOBJECT )}, NULL },
{ "ISWINDOW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ORECGET", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ORECGET )}, NULL },
{ "RIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( RIGHT )}, NULL },
{ "HB_ATOKENS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ATOKENS )}, NULL },
{ "FIELDPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPOS )}, NULL },
{ "FIELDGET", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDGET )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "DBSTRUCT", {HB_FS_PUBLIC}, {HB_FUNCNAME( DBSTRUCT )}, NULL },
{ "ORECPUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ORECPUT )}, NULL },
{ "FIELDTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDTYPE )}, NULL },
{ "FIELDPUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( FIELDPUT )}, NULL },
{ "DO_ONWNDINIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONWNDINIT )}, NULL },
{ "OWNDDATA", {HB_FS_PUBLIC}, {HB_FUNCNAME( OWNDDATA )}, NULL },
{ "DO_ONWNDRELEASE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONWNDRELEASE )}, NULL },
{ "__OBJHASMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMETHOD )}, NULL },
{ "DEL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DESTROY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DO_ONCTLINIT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONCTLINIT )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "OCNLDATA", {HB_FS_PUBLIC}, {HB_FUNCNAME( OCNLDATA )}, NULL },
{ "DO_ONCTLRELEASE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONCTLRELEASE )}, NULL },
{ "DO_ONWNDLAUNCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONWNDLAUNCH )}, NULL },
{ "DOEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DO_ONCTLLAUNCH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( DO_ONCTLLAUNCH )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_OBJMISC, "h_objmisc.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_OBJMISC
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_OBJMISC )
   #include "hbiniseg.h"
#endif

HB_FUNC( _WINDOWCARGO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 13 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 14 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 19 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 20 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 21 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 25 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _CONTROLCARGO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 31 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 32 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 37 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 38 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPop() ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 39 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 43 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_CONTROLEVENTPROCEDURE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 50 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 52 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 54 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 219L ) ) break;
	hb_xvmSetLine( 55 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 56 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 254L ) ) break;
	hb_xvmSetLine( 58 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 217L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 255L ) ) break;
	hb_xvmSetLine( 60 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 62 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 66 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_WINDOWEVENTPROCEDURE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 6 );
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 219L ) ) break;
	hb_xvmSetLine( 78 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushStringConst( "W", 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 80 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 81 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 254L ) ) break;
	hb_xvmSetLine( 82 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 255L ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 90 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _O2LOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 12, 5 );
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 99 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 15 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "LND", 3 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 103 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 5 );
	hb_xvmSetLine( 105 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00001: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 4 );
lab00003: ;
	hb_xvmPopLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushFuncSymbol( symbols + 21 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmPushLocal( 5 );
lab00005: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 111 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushStringConst( "*", 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmSetLine( 112 );
	hb_xvmCopyLocals( 1, 8 );
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 114 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmInc() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "F*,FORM*,FORMS*", 15 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 117 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushInteger( 12 );
	goto lab00007;
lab00006: ;
	hb_xvmPushLocal( 2 );
lab00007: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 118 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushStringConst( "", 0 );
	goto lab00009;
lab00008: ;
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
lab00009: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00010: ;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00012;
lab00011: ;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmInstring() ) break;
lab00012: ;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmArrayGen( 9 );
	if( hb_xvmDo( 2 ) ) break;
lab00013: ;
	hb_xvmSetLine( 126 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
lab00014: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 127 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushStringConst( "", 0 );
	goto lab00016;
lab00015: ;
	hb_xvmPushStringConst( "<", 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ">", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
lab00016: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 128 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmPushStringConst( "==> aForms: ", 12 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPlus() ) break;
	goto lab00018;
lab00017: ;
	hb_xvmPushLocal( 3 );
lab00018: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 129 );
	hb_xvmLocalSetInt( 11, 3L );
	goto lab00026;
lab00019: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "P*,PROC*,PROCNL*", 16 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushInteger( 25 );
	goto lab00021;
lab00020: ;
	hb_xvmPushLocal( 2 );
lab00021: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 133 );
	hb_xvmLocalSetInt( 9, 0L );
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00022: ;
	hb_xvmSetLine( 135 );
	if( hb_xvmLocalIncPush( 9 ) ) break;
	if( hb_xvmLessThenIntIs( 100L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00023;
	hb_xvmSetLine( 138 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( -7L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 139 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00022;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
lab00023: ;
	hb_xvmSetLine( 142 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushStringConst( "==> ProcNL: ", 12 );
	goto lab00025;
lab00024: ;
	hb_xvmPushLocal( 3 );
lab00025: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 143 );
	hb_xvmLocalSetInt( 11, 3L );
lab00026: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmPushInteger( 19 );
	goto lab00028;
lab00027: ;
	hb_xvmPushLocal( 2 );
lab00028: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 150 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmGreaterThenIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00029;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00030;
lab00029: ;
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00030;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	if( hb_xvmDo( 2 ) ) break;
lab00030: ;
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00055;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "O:", 2 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 156 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00052;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 158 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TINIDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00031: ;
	hb_xvmSetLine( 161 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
lab00032: ;
	hb_xvmSetLine( 162 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00033;
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	goto lab00034;
lab00033: ;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
lab00034: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 164 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	hb_xvmPushStringConst( "=", 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00040;
	hb_xvmSetLine( 166 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( 5L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00050;
lab00035: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushStringConst( "O:", 2 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00036;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 172 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TINIDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00039;
lab00036: ;
	hb_xvmSetLine( 175 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TWNDDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmSetLine( 176 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00039;
lab00037: ;
	hb_xvmSetLine( 177 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00038;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00039;
lab00038: ;
	hb_xvmSetLine( 179 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00039;
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00039: ;
	goto lab00050;
lab00040: ;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmSetLine( 184 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 185 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocalByRef( 13 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
lab00041: ;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 187 );
	hb_xvmPushStringConst( "O:", 2 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmLocalAdd( 14 );
	hb_xvmSetLine( 188 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00042;
	hb_xvmSetLine( 189 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( " ARRAY[", 7 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TINIDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmSetLine( 191 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00045;
lab00042: ;
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TWNDDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00043;
	hb_xvmSetLine( 194 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00045;
lab00043: ;
	hb_xvmSetLine( 195 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00044;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	goto lab00045;
lab00044: ;
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmSetLine( 198 );
	hb_xvmPushLocalByRef( 14 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
lab00045: ;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00047;
lab00046: ;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 2 ) ) break;
lab00047: ;
	hb_xvmSetLine( 204 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00041;
lab00048: ;
	hb_xvmEnumEnd();
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00050;
lab00049: ;
	hb_xvmSetLine( 207 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00050: ;
	hb_xvmSetLine( 209 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00032;
lab00051: ;
	hb_xvmEnumEnd();
	goto lab00068;
lab00052: ;
	hb_xvmSetLine( 211 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TWNDDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00053;
	hb_xvmSetLine( 212 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	goto lab00068;
lab00053: ;
	hb_xvmSetLine( 214 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA", 35 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00054;
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00068;
lab00054: ;
	hb_xvmSetLine( 217 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00068;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	goto lab00068;
lab00055: ;
	hb_xvmSetLine( 221 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00067;
	hb_xvmSetLine( 222 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00066;
lab00056: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocalByRef( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00057;
	hb_xvmSetLine( 227 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\"", 1 );
	hb_xvmLocalAdd( 10 );
	hb_xvmSetLine( 228 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushStringConst( " -> ", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00057: ;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00058;
	hb_xvmSetLine( 231 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( 5L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00065;
lab00058: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00064;
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDec() ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00063;
	hb_xvmSetLine( 235 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00062;
lab00059: ;
	hb_xvmSetLine( 237 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 238 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "AO", 2 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00060;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( 5L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 5 ) ) break;
	goto lab00061;
lab00060: ;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDec() ) break;
	if( hb_xvmAddInt( 5L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00061: ;
	hb_xvmSetLine( 243 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00059;
lab00062: ;
	hb_xvmEnumEnd();
	goto lab00065;
lab00063: ;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00065;
lab00064: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
lab00065: ;
	hb_xvmSetLine( 250 );
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00056;
lab00066: ;
	hb_xvmEnumEnd();
	goto lab00068;
lab00067: ;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00068: ;
	hb_xvmSetLine( 256 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00069;
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00069;
	hb_xvmSetLine( 257 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 258 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 259 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00069: ;
	hb_xvmSetLine( 262 );
	hb_xvmPushLocal( 16 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TR0 )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 267 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 270 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 2 );
lab00003: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 272 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPopLocal( 3 );
lab00004: ;
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _WPOST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 283 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 284 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 285 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 286 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 288 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 289 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 291 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 292 );
	hb_xvmCopyLocals( 2, 5 );
	hb_xvmSetLine( 293 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 295 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPopLocal( 5 );
lab00004: ;
	hb_xvmSetLine( 298 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 299 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 300 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 301 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 302 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 303 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 306 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 317 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _WSEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 325 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 326 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 327 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 331 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 334 );
	hb_xvmCopyLocals( 2, 5 );
	hb_xvmSetLine( 335 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 337 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	hb_xvmPopLocal( 5 );
lab00004: ;
	hb_xvmSetLine( 340 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 341 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 343 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 344 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 345 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 347 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 348 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 359 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _WINDOWOBJ )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 366 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 368 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _CONTROLOBJ )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 374 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 377 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 3 );
lab00003: ;
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _OTHIS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 385 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 388 );
	hb_xvmCopyLocals( 1, 2 );
	hb_xvmSetLine( 389 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 392 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 2 );
lab00003: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 394 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 395 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 242L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 396 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 218L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 397 );
	hb_xvmPushSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 217L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 398 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 254L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 399 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 400 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 401 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 402 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 404 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushStringConst( "", 0 );
	goto lab00005;
lab00004: ;
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
lab00005: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushStringConst( "", 0 );
	goto lab00007;
lab00006: ;
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
lab00007: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 408 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 409 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 410 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 411 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 412 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 414 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 415 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00009: ;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 418 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 420 );
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 421 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 422 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 423 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushStringConst( "W", 1 );
	goto lab00011;
lab00010: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
lab00011: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 424 );
	hb_xvmPushSymbol( symbols + 83 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 425 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 427 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 428 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 219L ) ) break;
	hb_xvmSetLine( 429 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 430 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 431 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 432 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 254L ) ) break;
	hb_xvmSetLine( 433 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 255L ) ) break;
lab00012: ;
	hb_xvmSetLine( 439 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 255L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00014;
lab00013: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 217L ) ) break;
lab00014: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00015;
	hb_xvmSetLine( 441 );
	hb_xvmPushSymbol( symbols + 96 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 442 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 443 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 444 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00015: ;
	hb_xvmSetLine( 446 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 447 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 447L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 448 );
	hb_xvmPushSymbol( symbols + 102 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 449 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 219L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 450 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 451 );
	hb_xvmPushSymbol( symbols + 106 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 452 );
	hb_xvmPushSymbol( symbols + 107 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00016: ;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 456 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TSBROWSE", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 457 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00018;
lab00017: ;
	hb_xvmSetLine( 458 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "TWNDDATA", 8 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 459 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00018: ;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00019: ;
	hb_xvmSetLine( 466 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
	hb_xvmPushLocal( 1 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmCopyLocals( 1, 4 );
lab00020: ;
	hb_xvmSetLine( 469 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmSetLine( 470 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 219L ) ) break;
	hb_xvmSetLine( 471 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 472 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushStringConst( "W", 1 );
	goto lab00022;
lab00021: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
lab00022: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 474 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 254L ) ) break;
	hb_xvmSetLine( 475 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 255L ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmSetLine( 476 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushStringConst( "THMGDATA,TKEYDATA,TTHRDATA", 26 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00024;
	hb_xvmSetLine( 478 );
	hb_xvmPushSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 219L ) ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 242L ) ) break;
	hb_xvmSetLine( 480 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 218L ) ) break;
	hb_xvmSetLine( 481 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 217L ) ) break;
	hb_xvmSetLine( 482 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 254L ) ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 255L ) ) break;
lab00024: ;
	hb_xvmSetLine( 487 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _PPOST )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 494 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 496 );
	hb_xvmPushSymbol( symbols + 111 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 497 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 498 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 499 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 502 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 506 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _PSEND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 511 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 513 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 515 );
	hb_xvmPushSymbol( symbols + 111 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 516 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 517 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 518 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 520 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 521 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 525 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_OBJ )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 532 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 536 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 538 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 539 );
	hb_xvmPushSymbol( symbols + 117 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 540 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushSymbol( symbols + 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 545 );
	hb_xvmPushSymbol( symbols + 118 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 5 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00004: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ORECGET )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 2 );
	hb_xvmSetLine( 556 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 2 );
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 558 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 559 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "{", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 120 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 561 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 563 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 567 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 568 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
lab00005: ;
	hb_xvmSetLine( 569 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00008;
	hb_xvmSetLine( 570 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 3 );
lab00006: ;
	hb_xvmSetLine( 572 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 573 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 574 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00007: ;
	hb_xvmSetLine( 576 );
lab00008: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
lab00009: ;
	hb_xvmEnumEnd();
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 578 );
	hb_xvmPushFuncSymbol( symbols + 124 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	if( hb_xvmFunction( 0 ) ) break;
	{
		static const HB_BYTE codeblock[ 25 ] = {
			2, 0, 1, 0, 2, 0, 48, 44, 0, 95, 255, 95, 1, 122, 1, 176, 
			123, 0, 95, 2, 12, 1, 112, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
lab00011: ;
	hb_xvmSetLine( 581 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( ORECPUT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 586 );
	hb_xvmLocalSetInt( 7, 0L );
	hb_xvmSetLine( 588 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 590 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "{", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 120 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 591 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPopLocal( 2 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 593 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( ",", 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
lab00002: ;
	hb_xvmSetLine( 597 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 599 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
lab00003: ;
	hb_xvmSetLine( 600 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
lab00004: ;
	hb_xvmSetLine( 602 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 603 );
	goto lab00009;
lab00006: ;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 606 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 607 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00009;
	hb_xvmSetLine( 609 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 610 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "+^=", 3 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 611 );
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 613 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 614 );
	if( hb_xvmLocalInc( 7 ) ) break;
lab00008: ;
	hb_xvmSetLine( 616 );
lab00009: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
lab00010: ;
	hb_xvmEnumEnd();
	goto lab00019;
lab00011: ;
	hb_xvmSetLine( 620 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmEnumStart( 1, 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
lab00012: ;
	hb_xvmSetLine( 621 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
lab00013: ;
	hb_xvmSetLine( 623 );
	goto lab00017;
lab00014: ;
	hb_xvmSetLine( 625 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00016;
	hb_xvmSetLine( 626 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "+^=", 3 );
	if( hb_xvmInstring() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 627 );
	goto lab00017;
lab00015: ;
	hb_xvmSetLine( 629 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 630 );
	if( hb_xvmLocalInc( 7 ) ) break;
lab00016: ;
	hb_xvmSetLine( 632 );
lab00017: ;
	if( hb_xvmEnumNext() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
lab00018: ;
	hb_xvmEnumEnd();
lab00019: ;
	hb_xvmSetLine( 636 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmGreaterThenInt( 0L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONWNDINIT )
{
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 641 );
	hb_xvmCopyLocals( 1, 3 );
	hb_xvmSetLine( 642 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 643 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 644 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 645 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 647 );
	hb_xvmPushFuncSymbol( symbols + 130 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONWNDRELEASE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 653 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 656 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 657 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Del", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 658 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 660 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Destroy", 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 661 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 663 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 666 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONCTLINIT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 2 );
	hb_xvmSetLine( 671 );
	hb_xvmCopyLocals( 1, 3 );
	hb_xvmSetLine( 672 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	goto lab00002;
lab00001: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
lab00002: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 675 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 678 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 136 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushStringConst( "Type", 4 );
	if( hb_xvmFunction( 3 ) ) break;
lab00004: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 680 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 6 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONCTLRELEASE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 686 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 689 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 690 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Del", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 691 );
	hb_xvmPushSymbol( symbols + 133 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Destroy", 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 694 );
	hb_xvmPushSymbol( symbols + 134 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 696 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 699 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONWNDLAUNCH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 704 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 705 );
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 710 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( DO_ONCTLLAUNCH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 718 );
	hb_xvmCopyLocals( 4, 1 );
lab00001: ;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 722 );
	hb_xvmPushSymbol( symbols + 140 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 725 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

#line 728 "h_objmisc.prg"

#include <mgdefs.h>
#include "hbapiitm.h"
#include <commctrl.h>

HB_FUNC( HMG_SETWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) hb_param( 2, HB_IT_OBJECT );

      if( pObject && HB_IS_OBJECT( pObject ) )
      {
         pObject = hb_itemNew( pObject );

         hb_gcLock( pObject );    // Ref++

         SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LPARAM ) pObject );

         hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( HMG_DELWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

      SetWindowLongPtr( hWnd, GWLP_USERDATA, 0 );

      if( pObject && HB_IS_OBJECT( pObject ) )
      {
         hb_gcUnlock( pObject ); // Ref --
         hb_itemRelease( pObject );
      }
   }
}

HB_FUNC( HMG_GETWINDOWOBJECT )
{
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
      hb_itemReturn( ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA ) );
   else
      hb_ret();
}

HB_FUNC( HMG_ISWINDOWOBJECT )
{
   PHB_ITEM pObject;

   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

      hb_retl( pObject && HB_IS_OBJECT( pObject ) );
   }
   else
      hb_retl( FALSE );
}

