/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "TBtnBox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TBTNBOX );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( TCONTROL );
HB_FUNC_STATIC( TBTNBOX_NEW );
HB_FUNC_STATIC( TBTNBOX_DEFAULT );
HB_FUNC_STATIC( TBTNBOX_HANDLEEVENT );
HB_FUNC_STATIC( TBTNBOX_GETDLGCODE );
HB_FUNC_STATIC( TBTNBOX_KEYCHAR );
HB_FUNC_STATIC( TBTNBOX_KEYDOWN );
HB_FUNC_STATIC( TBTNBOX_LOSTFOCUS );
HB_FUNC_STATIC( TBTNBOX_LVALID );
HB_FUNC_STATIC( TBTNBOX_LBUTTONDOWN );
HB_FUNC_STATIC( TBTNBOX_GETVAL );
HB_FUNC_STATIC( TBTNBOX_COMMAND );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( _GETWINDOWPROP );
HB_FUNC_EXTERN( NOR );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( INITEDSPINNER );
HB_FUNC_EXTERN( SETINCREMENTSPINNER );
HB_FUNC_EXTERN( INITBTNTEXTBOX );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( CVALTOCHAR );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( SETWINDOWTEXT );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( _GETKEYSTATE );
HB_FUNC_EXTERN( UPPER );
HB_FUNC_EXTERN( CHR );
HB_FUNC_EXTERN( GETWINDOWTEXT );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( VAL );
HB_FUNC_EXTERN( LOWORD );
HB_FUNC_EXTERN( GETKEYSTATE );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TBTNBOX )
{ "TBTNBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "TCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TCONTROL )}, NULL },
{ "ADDMULTICLSDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TBTNBOX_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_NEW )}, NULL },
{ "TBTNBOX_DEFAULT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_DEFAULT )}, NULL },
{ "TBTNBOX_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_HANDLEEVENT )}, NULL },
{ "TBTNBOX_GETDLGCODE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_GETDLGCODE )}, NULL },
{ "TBTNBOX_KEYCHAR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_KEYCHAR )}, NULL },
{ "TBTNBOX_KEYDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_KEYDOWN )}, NULL },
{ "TBTNBOX_LOSTFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_LOSTFOCUS )}, NULL },
{ "TBTNBOX_LVALID", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_LVALID )}, NULL },
{ "TBTNBOX_LBUTTONDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_LBUTTONDOWN )}, NULL },
{ "TBTNBOX_GETVAL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_GETVAL )}, NULL },
{ "TBTNBOX_COMMAND", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TBTNBOX_COMMAND )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETSYSCOLOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSYSCOLOR )}, NULL },
{ "_NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NBOTTOM", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NTOP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRIGHT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLEFT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "_GETWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETWINDOWPROP )}, NULL },
{ "_NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNEWID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NOR )}, NULL },
{ "_CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWNDPARENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LFOCUSED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LAPPEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCHANGED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BACTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ATX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWNDCHILD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "INITEDSPINNER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITEDSPINNER )}, NULL },
{ "HWNDPARENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETINCREMENTSPINNER", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETINCREMENTSPINNER )}, NULL },
{ "HWNDCHILD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "INITBTNTEXTBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITBTNTEXTBOX )}, NULL },
{ "ADDVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DEFAULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "ADDCONTROL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CVALTOCHAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CVALTOCHAR )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "SETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWTEXT )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "LOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_GETKEYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETKEYSTATE )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "CHR", {HB_FS_PUBLIC}, {HB_FUNCNAME( CHR )}, NULL },
{ "KEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BVALID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LAPPEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VARGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWTEXT )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "VAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( VAL )}, NULL },
{ "LOWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( LOWORD )}, NULL },
{ "KEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BACTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LPOSTEDIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EDITEXIT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCELL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETKEYSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETKEYSTATE )}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TBTNBOX, "TBtnBox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TBTNBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TBTNBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( TBTNBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 104 );
	hb_xvmSetLine( 14 );
	hb_xvmPushStatic( 1 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStaticByRef( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSeqAlways();
	do {
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmPushSymbol( symbols + 2 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushStringConst( "TBtnBox", 7 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 16 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LOGICAL", 7 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 32L ) ) break;
	hb_xvmPushStringConst( "lRegistered", 11 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 18 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Atx", 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 19 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lAppend", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 20 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bAction", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 21 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCell", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 22 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "lChanged", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 23 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "hWndChild", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 27 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 28 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Default", 7 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 29 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 30 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetDlgCode", 10 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 31 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyChar", 7 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 32 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyDown", 7 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 33 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LostFocus", 9 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 34 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "lValid", 6 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 35 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LButtonDown", 11 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 36 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetVal", 6 );
	hb_xvmPushSymbol( symbols + 17 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 37 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Command", 7 );
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 39 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 23 );
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
	hb_xvmPushSymbol( symbols + 21 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 24 );
	hb_xvmSetLine( 50 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 25 );
	hb_xvmSetLine( 51 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 26 );
	hb_xvmSetLine( 53 );
	hb_xvmLocalSetInt( 28, 255L );
	hb_xvmSetLine( 65 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 8 );
lab00002: ;
	hb_xvmPopLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 9 );
lab00004: ;
	hb_xvmPopLocal( 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 12 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 6 );
lab00006: ;
	hb_xvmPopLocal( 6 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	{
		static const HB_BYTE codeblock[ 2 ] = {
			121, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 21 );
lab00008: ;
	hb_xvmPopLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	{
		static const HB_BYTE codeblock[ 4 ] = {
			93, 0, 125, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 22 );
lab00010: ;
	hb_xvmPopLocal( 22 );
	hb_xvmSetLine( 67 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 68 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 69 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 70 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( -2L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 71 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 72 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 74 );
	if( hb_xvmPushMemvar( symbols + 33 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 75 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 27 );
	hb_xvmSetLine( 76 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 12 );
lab00011: ;
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 80 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushInteger( 8192 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741824 );
#else
	hb_xvmPushLong( 1073741824L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 82 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 85 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 88 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 89 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 90 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 93 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 15 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 95 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 17 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 96 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 24 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 97 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 99 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 102 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 103 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "EDIT", 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 21 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmPushLocal( 21 );
lab00013: ;
	hb_xvmPopLocal( 29 );
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 22 );
	if( hb_xvmSend( 0 ) ) break;
	goto lab00015;
lab00014: ;
	hb_xvmPushLocal( 22 );
lab00015: ;
	hb_xvmPopLocal( 30 );
	hb_xvmSetLine( 106 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushLocal( 30 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 9 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 107 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 110 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 27 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 25 );
	hb_xvmPushLocal( 26 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 20 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00017: ;
	hb_xvmSetLine( 113 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 114 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00018: ;
	hb_xvmSetLine( 120 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00019: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_DEFAULT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 134 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 135 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 142 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_HANDLEEVENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 151 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 78L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( -8L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 153 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 157 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_GETDLGCODE )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 166 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 168 );
	hb_xvmRetInt( 132L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_KEYCHAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 178 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "W", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 9 );
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	hb_xvmPopLocal( 1 );
lab00004: ;
	hb_xvmSetLine( 181 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
lab00005: ;
	hb_xvmSetLine( 182 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushSymbol( symbols + 83 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_KEYDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 193 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 194 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
lab00001: ;
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 198 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 202 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 205 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_LVALID )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 213 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 215 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 216 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 84 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 219 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_LOSTFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 227 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00002;
lab00001: ;
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00002: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 229 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 230 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 231 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 232 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 235 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 236 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 91 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 238 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 239 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 242 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_LBUTTONDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 253 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 9999L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 254 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 256 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 9999 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 259 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_GETVAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 273 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 274 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 277 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TBTNBOX_COMMAND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 283 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 289 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 290 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 291 );
	hb_xvmCopyLocals( 2, 6 );
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 297 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 298 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 13 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 302 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 303 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 27 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	goto lab00008;
lab00002: ;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 309 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 310 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 311 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 313 );
	hb_xvmPushSymbol( symbols + 85 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 43 ] = {
			1, 0, 1, 0, 3, 0, 48, 101, 0, 48, 100, 0, 95, 255, 112, 0, 
			48, 102, 0, 95, 255, 112, 0, 95, 1, 48, 92, 0, 95, 255, 112, 0, 
			48, 87, 0, 95, 255, 112, 0, 9, 112, 5, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 314 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 315 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 316 );
	hb_xvmPushSymbol( symbols + 99 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00008;
lab00003: ;
	hb_xvmSetLine( 318 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 768L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 319 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00008;
lab00004: ;
	hb_xvmSetLine( 321 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 512L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 322 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	goto lab00008;
lab00005: ;
	hb_xvmSetLine( 324 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmEqualIntIs( 1024L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 325 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushInteger( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 326 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 27 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 328 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushInteger( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 329 );
	hb_xvmPushFuncSymbol( symbols + 103 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( -127L, &fValue ) ) break;
	if( fValue )
		goto lab00007;
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
lab00007: ;
	hb_xvmSetLine( 330 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 13 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
lab00008: ;
	hb_xvmSetLine( 337 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 104, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

