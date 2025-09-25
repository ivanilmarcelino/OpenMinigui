/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "TComboBox.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( TCOMBOBOX );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( TCONTROL );
HB_FUNC_STATIC( TCOMBOBOX_NEW );
HB_FUNC_STATIC( TCOMBOBOX_DEFAULT );
HB_FUNC_STATIC( TCOMBOBOX_GETDLGCODE );
HB_FUNC_STATIC( TCOMBOBOX_HANDLEEVENT );
HB_FUNC_STATIC( TCOMBOBOX_KEYDOWN );
HB_FUNC_STATIC( TCOMBOBOX_KEYCHAR );
HB_FUNC_STATIC( TCOMBOBOX_LBUTTONDOWN );
HB_FUNC_STATIC( TCOMBOBOX_LOSTFOCUS );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( GETSYSCOLOR );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETACTIVEMDIHANDLE );
HB_FUNC_EXTERN( _GETWINDOWPROP );
HB_FUNC_EXTERN( NOR );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( INITCOMBOBOX );
HB_FUNC_EXTERN( GETOBJECTTYPE );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( COMBOADDSTRING );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HIWORD );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( GETWINDOWTEXT );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_TCOMBOBOX )
{ "TCOMBOBOX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "TCONTROL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TCONTROL )}, NULL },
{ "ADDMULTICLSDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCOMBOBOX_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_NEW )}, NULL },
{ "TCOMBOBOX_DEFAULT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_DEFAULT )}, NULL },
{ "TCOMBOBOX_GETDLGCODE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_GETDLGCODE )}, NULL },
{ "TCOMBOBOX_HANDLEEVENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_HANDLEEVENT )}, NULL },
{ "TCOMBOBOX_KEYDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_KEYDOWN )}, NULL },
{ "TCOMBOBOX_KEYCHAR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_KEYCHAR )}, NULL },
{ "TCOMBOBOX_LBUTTONDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_LBUTTONDOWN )}, NULL },
{ "TCOMBOBOX_LOSTFOCUS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TCOMBOBOX_LOSTFOCUS )}, NULL },
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
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "_OWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETACTIVEMDIHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETACTIVEMDIHANDLE )}, NULL },
{ "_GETWINDOWPROP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETWINDOWPROP )}, NULL },
{ "_NID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETNEWID", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCONTROLNAME", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPARENTWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NSTYLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( NOR )}, NULL },
{ "_BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AITEMS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LCAPTURED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HFONT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BCHANGE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LFOCUSED", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LAPPEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ATX", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETCOLOR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "INITCOMBOBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITCOMBOBOX )}, NULL },
{ "ADDVARS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "DEFAULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETOBJECTTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETOBJECTTYPE )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "ADDCONTROL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "COMBOADDSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( COMBOADDSTRING )}, NULL },
{ "AITEMS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BSETGET", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "HIWORD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIWORD )}, NULL },
{ "BCLOSEUP", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "HANDLEEVENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SUPER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SENDMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NAT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "BKEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "KEYDOWN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LAPPEND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NLASTKEY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETWINDOWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWTEXT )}, NULL },
{ "BLOSTFOCUS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "KEYCHAR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "POSTMSG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "(_INITSTATICS00001)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_TCOMBOBOX, "TComboBox.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_TCOMBOBOX
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_TCOMBOBOX )
   #include "hbiniseg.h"
#endif

HB_FUNC( TCOMBOBOX )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 82 );
	hb_xvmSetLine( 20 );
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
	hb_xvmPushStringConst( "TComboBox", 9 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 22 );
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
	hb_xvmSetLine( 24 );
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
	hb_xvmSetLine( 25 );
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
	hb_xvmSetLine( 26 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nAt", 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 27 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ARRAY", 5 );
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aItems", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 28 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "bCloseUp", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 31 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 32 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Default", 7 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 33 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "GetDlgCode", 10 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 34 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "HandleEvent", 11 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 35 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyDown", 7 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 36 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "KeyChar", 7 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 37 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LButtonDown", 11 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 38 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "LostFocus", 9 );
	hb_xvmPushSymbol( symbols + 15 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 40 );
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 20 );
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
	hb_xvmPushSymbol( symbols + 18 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 14 );
	hb_xvmSetLine( 49 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 50 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 51 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 52 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 57 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 9 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 10 );
lab00004: ;
	hb_xvmPopLocal( 10 );
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
	hb_xvmSetLine( 59 );
	hb_xvmPushSymbol( symbols + 22 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 60 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 61 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 62 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 64 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 65 );
	hb_xvmPushSelf();
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 66 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00007: ;
	hb_xvmSetLine( 69 );
	hb_xvmPushSymbol( symbols + 30 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 70 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 72 );
	if( hb_xvmPushMemvar( symbols + 32 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 73 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 19 );
	hb_xvmSetLine( 74 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushStringConst( "PROP_FORMNAME", 13 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 14 );
lab00008: ;
	hb_xvmSetLine( 77 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 78 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 79 );
	hb_xvmPushSymbol( symbols + 38 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 81 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 40 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1073741824 );
#else
	hb_xvmPushLong( 1073741824L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 268435456 );
#else
	hb_xvmPushLong( 268435456L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 65536 );
#else
	hb_xvmPushLong( 65536L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 2097152 );
#else
	hb_xvmPushLong( 2097152L );
#endif
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 8388608 );
#else
	hb_xvmPushLong( 8388608L );
#endif
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 1024 );
	if( hb_xvmFunction( 7 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 83 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 84 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 85 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 86 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 87 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 88 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 89 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 90 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 91 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 92 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 98 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushLocal( 19 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmPushMemvar( symbols + 32 ) ) break;
	if( hb_xvmArrayItemPush( 271L ) ) break;
	if( hb_xvmFunction( 13 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 100 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 101 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 103 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 6L, &fValue ) ) break;
	if( !fValue )
		goto lab00009;
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 105 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 11 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00009: ;
	hb_xvmSetLine( 108 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00010: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_DEFAULT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 122 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 123 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 124 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 125 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 334 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDec() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 122 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 129 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_GETDLGCODE )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 138 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 140 );
	hb_xvmRetInt( 4L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_HANDLEEVENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 3 );
	hb_xvmSetLine( 148 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmEqualIntIs( 8L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 149 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSelf();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00002;
lab00001: ;
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushSelf();
	hb_xvmPushSelf();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 151 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 155 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushSymbol( symbols + 69 );
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

HB_FUNC_STATIC( TCOMBOBOX_KEYDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 163 );
	hb_xvmPushSymbol( symbols + 70 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 327 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 165 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 166 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
lab00002: ;
	hb_xvmSetLine( 171 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 172 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 173 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 176 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_LOSTFOCUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 186 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 75 );
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
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
lab00002: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 188 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 189 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 190 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 191 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 194 );
	hb_xvmPushSymbol( symbols + 70 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 327 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 196 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqualIntIs( -1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 197 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 199 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmInc() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 201 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmInc() ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 204 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushSymbol( symbols + 31 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 207 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 209 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 210 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00007: ;
	hb_xvmSetLine( 213 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_KEYCHAR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 222 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 9L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 223 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 27L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 226 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 228 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 13L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 229 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00003: ;
	hb_xvmSetLine( 232 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 2 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TCOMBOBOX_LBUTTONDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 243 );
	hb_xvmLocalSetInt( 3, 1L );
	hb_xvmSetLine( 248 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmEqualIntIs( 9999L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 249 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 250 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 252 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 9999 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 255 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushSelf();
	hb_xvmPushInteger( 335 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 257 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 82, 1 );
	/* *** END PROC *** */
   } while( 0 );
}

