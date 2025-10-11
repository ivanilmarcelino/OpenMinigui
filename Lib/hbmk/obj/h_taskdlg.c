/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_taskdlg.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( SIMPLETASKDIALOG );
HB_FUNC_EXTERN( __CLSLOCKDEF );
HB_FUNC_EXTERN( HBCLASS );
HB_FUNC_EXTERN( HBOBJECT );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_NEW );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_EXECUTE );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_TITLE );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_INSTRUCTION );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_CONTENT );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_COMMONBUTTONS );
HB_FUNC_STATIC( TSIMPLETASKDIALOG_MAINICON );
HB_FUNC_EXTERN( __CLSUNLOCKDEF );
HB_FUNC_EXTERN( __OBJHASMSG );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HB_ISNULL );
HB_FUNC_EXTERN( OS_ISWINVISTA_OR_LATER );
HB_FUNC_EXTERN( WIN_TASKDIALOG0 );
HB_FUNC( TASKDIALOG );
HB_FUNC_STATIC( TTASKDIALOG_NEW );
HB_FUNC_STATIC( TTASKDIALOG_SHOWDIALOG );
HB_FUNC_STATIC( TTASKDIALOG_DIALOGHANDLE );
HB_FUNC_STATIC( TTASKDIALOG_SHOWING );
HB_FUNC_STATIC( TTASKDIALOG_ONCREATED );
HB_FUNC_STATIC( TTASKDIALOG_ONDESTROYED );
HB_FUNC_STATIC( TTASKDIALOG_LISTENER );
HB_FUNC_STATIC( TTASKDIALOG_COMMONBUTTONS );
HB_FUNC_STATIC( TTASKDIALOG_WINDOWTITLE );
HB_FUNC_STATIC( TTASKDIALOG_TITLE );
HB_FUNC_STATIC( TTASKDIALOG_MAINICON );
HB_FUNC_STATIC( TTASKDIALOG_MAININSTRUCTION );
HB_FUNC_STATIC( TTASKDIALOG_INSTRUCTION );
HB_FUNC_STATIC( TTASKDIALOG_CONTENT );
HB_FUNC_STATIC( TTASKDIALOG_CUSTOMBUTTONS );
HB_FUNC_STATIC( TTASKDIALOG_DEFAULTBUTTON );
HB_FUNC_STATIC( TTASKDIALOG_CUSTOMRADIOBUTTONS );
HB_FUNC_STATIC( TTASKDIALOG_DEFAULTRADIOBUTTON );
HB_FUNC_STATIC( TTASKDIALOG_VERIFICATIONTEXT );
HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDINFO );
HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDCONTROLTEXT );
HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDCTRLTEXT );
HB_FUNC_STATIC( TTASKDIALOG_COLLAPSEDCONTROLTEXT );
HB_FUNC_STATIC( TTASKDIALOG_COLLAPSEDCTRLTEXT );
HB_FUNC_STATIC( TTASKDIALOG_FOOTERICON );
HB_FUNC_STATIC( TTASKDIALOG_FOOTER );
HB_FUNC_STATIC( TTASKDIALOG_WIDTH );
HB_FUNC_STATIC( TTASKDIALOG_PARENT );
HB_FUNC_STATIC( TTASKDIALOG_PARENTHANDLE );
HB_FUNC_STATIC( TTASKDIALOG_CALLBACKBLOCK );
HB_FUNC_STATIC( TTASKDIALOG_FLAGS );
HB_FUNC_STATIC( TTASKDIALOG_ALLOWDIALOGCANCELLATION );
HB_FUNC_STATIC( TTASKDIALOG_CANBEMINIMIZED );
HB_FUNC_STATIC( TTASKDIALOG_ENABLEHYPERLINKS );
HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDBYDEFAULT );
HB_FUNC_STATIC( TTASKDIALOG_EXPANDFOOTERAREA );
HB_FUNC_STATIC( TTASKDIALOG_NODEFAULTRADIOBUTTON );
HB_FUNC_STATIC( TTASKDIALOG_POSITIONRELATIVETOWINDOW );
HB_FUNC_STATIC( TTASKDIALOG_RIGHTTOLEFTLAYOUT );
HB_FUNC_STATIC( TTASKDIALOG_VERIFICATIONENABLED );
HB_FUNC_STATIC( TTASKDIALOG_TIMEOUTMS );
HB_FUNC_STATIC( TTASKDIALOG_TIMEDOUT );
HB_FUNC_EXTERN( __OBJHASMETHOD );
HB_FUNC_EXTERN( HB_BITOR );
HB_FUNC_EXTERN( WIN_TASKDIALOGINDIRECT0 );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( HB_ISEVALITEM );
HB_FUNC_EXTERN( _SETWINDOWTITLE );
HB_FUNC_EXTERN( _UPDATEMAINICON );
HB_FUNC_EXTERN( _SETMAININSTRUCTION );
HB_FUNC_EXTERN( _SETCONTENT );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( _SETEXPANDEDINFORMATION );
HB_FUNC_EXTERN( _UPDATEFOOTERICON );
HB_FUNC_EXTERN( _SETFOOTER );
HB_FUNC_EXTERN( ISWINDOWHANDLE );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( HB_BITAND );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_INITSTATICS();


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_TASKDLG )
{ "SIMPLETASKDIALOG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SIMPLETASKDIALOG )}, NULL },
{ "__CLSLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSLOCKDEF )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HBCLASS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBCLASS )}, NULL },
{ "HBOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HBOBJECT )}, NULL },
{ "ADDMULTIDATA", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ADDMETHOD", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TSIMPLETASKDIALOG_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_NEW )}, NULL },
{ "TSIMPLETASKDIALOG_EXECUTE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_EXECUTE )}, NULL },
{ "TSIMPLETASKDIALOG_TITLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_TITLE )}, NULL },
{ "TSIMPLETASKDIALOG_INSTRUCTION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_INSTRUCTION )}, NULL },
{ "TSIMPLETASKDIALOG_CONTENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_CONTENT )}, NULL },
{ "TSIMPLETASKDIALOG_COMMONBUTTONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_COMMONBUTTONS )}, NULL },
{ "TSIMPLETASKDIALOG_MAINICON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TSIMPLETASKDIALOG_MAINICON )}, NULL },
{ "CREATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__CLSUNLOCKDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( __CLSUNLOCKDEF )}, NULL },
{ "INSTANCE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMSG", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMSG )}, NULL },
{ "INITCLASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CTITLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HB_ISNULL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNULL )}, NULL },
{ "_CINSTRUCTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCONTENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NCOMMONBUTTONS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NMAINICON", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NBUTTONRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "OS_ISWINVISTA_OR_LATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( OS_ISWINVISTA_OR_LATER )}, NULL },
{ "WIN_TASKDIALOG0", {HB_FS_PUBLIC}, {HB_FUNCNAME( WIN_TASKDIALOG0 )}, NULL },
{ "CTITLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CINSTRUCTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "CCONTENT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NCOMMONBUTTONS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NMAINICON", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LERROR", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TASKDIALOG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TASKDIALOG )}, NULL },
{ "TTASKDIALOG_NEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_NEW )}, NULL },
{ "ADDINLINE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "SHOWDIALOG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TTASKDIALOG_SHOWDIALOG", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_SHOWDIALOG )}, NULL },
{ "TTASKDIALOG_DIALOGHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_DIALOGHANDLE )}, NULL },
{ "TTASKDIALOG_SHOWING", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_SHOWING )}, NULL },
{ "TTASKDIALOG_ONCREATED", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_ONCREATED )}, NULL },
{ "TTASKDIALOG_ONDESTROYED", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_ONDESTROYED )}, NULL },
{ "TTASKDIALOG_LISTENER", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_LISTENER )}, NULL },
{ "TTASKDIALOG_COMMONBUTTONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_COMMONBUTTONS )}, NULL },
{ "TTASKDIALOG_WINDOWTITLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_WINDOWTITLE )}, NULL },
{ "TTASKDIALOG_TITLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_TITLE )}, NULL },
{ "TTASKDIALOG_MAINICON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_MAINICON )}, NULL },
{ "TTASKDIALOG_MAININSTRUCTION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_MAININSTRUCTION )}, NULL },
{ "TTASKDIALOG_INSTRUCTION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_INSTRUCTION )}, NULL },
{ "TTASKDIALOG_CONTENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_CONTENT )}, NULL },
{ "TTASKDIALOG_CUSTOMBUTTONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_CUSTOMBUTTONS )}, NULL },
{ "TTASKDIALOG_DEFAULTBUTTON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_DEFAULTBUTTON )}, NULL },
{ "TTASKDIALOG_CUSTOMRADIOBUTTONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_CUSTOMRADIOBUTTONS )}, NULL },
{ "TTASKDIALOG_DEFAULTRADIOBUTTON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_DEFAULTRADIOBUTTON )}, NULL },
{ "TTASKDIALOG_VERIFICATIONTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_VERIFICATIONTEXT )}, NULL },
{ "TTASKDIALOG_EXPANDEDINFO", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_EXPANDEDINFO )}, NULL },
{ "TTASKDIALOG_EXPANDEDCONTROLTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_EXPANDEDCONTROLTEXT )}, NULL },
{ "TTASKDIALOG_EXPANDEDCTRLTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_EXPANDEDCTRLTEXT )}, NULL },
{ "TTASKDIALOG_COLLAPSEDCONTROLTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_COLLAPSEDCONTROLTEXT )}, NULL },
{ "TTASKDIALOG_COLLAPSEDCTRLTEXT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_COLLAPSEDCTRLTEXT )}, NULL },
{ "TTASKDIALOG_FOOTERICON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_FOOTERICON )}, NULL },
{ "TTASKDIALOG_FOOTER", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_FOOTER )}, NULL },
{ "TTASKDIALOG_WIDTH", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_WIDTH )}, NULL },
{ "TTASKDIALOG_PARENT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_PARENT )}, NULL },
{ "TTASKDIALOG_PARENTHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_PARENTHANDLE )}, NULL },
{ "TTASKDIALOG_CALLBACKBLOCK", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_CALLBACKBLOCK )}, NULL },
{ "TTASKDIALOG_FLAGS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_FLAGS )}, NULL },
{ "TTASKDIALOG_ALLOWDIALOGCANCELLATION", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_ALLOWDIALOGCANCELLATION )}, NULL },
{ "TTASKDIALOG_CANBEMINIMIZED", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_CANBEMINIMIZED )}, NULL },
{ "TTASKDIALOG_ENABLEHYPERLINKS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_ENABLEHYPERLINKS )}, NULL },
{ "TTASKDIALOG_EXPANDEDBYDEFAULT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_EXPANDEDBYDEFAULT )}, NULL },
{ "TTASKDIALOG_EXPANDFOOTERAREA", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_EXPANDFOOTERAREA )}, NULL },
{ "TTASKDIALOG_NODEFAULTRADIOBUTTON", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_NODEFAULTRADIOBUTTON )}, NULL },
{ "TTASKDIALOG_POSITIONRELATIVETOWINDOW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_POSITIONRELATIVETOWINDOW )}, NULL },
{ "TTASKDIALOG_RIGHTTOLEFTLAYOUT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_RIGHTTOLEFTLAYOUT )}, NULL },
{ "TTASKDIALOG_VERIFICATIONENABLED", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_VERIFICATIONENABLED )}, NULL },
{ "TTASKDIALOG_TIMEOUTMS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_TIMEOUTMS )}, NULL },
{ "TTASKDIALOG_TIMEDOUT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( TTASKDIALOG_TIMEDOUT )}, NULL },
{ "NBUTTONRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "NRADIOBUTTONRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LVERIFYRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACONFIG", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NRADIOBUTTONRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_TIMEDOUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TIMEOUTMS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "__OBJHASMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( __OBJHASMETHOD )}, NULL },
{ "_FLAGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_BITOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITOR )}, NULL },
{ "FLAGS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ALLOWDIALOGCANCELLATION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "WIN_TASKDIALOGINDIRECT0", {HB_FS_PUBLIC}, {HB_FUNCNAME( WIN_TASKDIALOGINDIRECT0 )}, NULL },
{ "_LVERIFYRESULT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_LACTIVE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_HWND", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_ISEVALITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISEVALITEM )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SETWINDOWTITLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETWINDOWTITLE )}, NULL },
{ "WINDOWTITLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_UPDATEMAINICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _UPDATEMAINICON )}, NULL },
{ "_SETMAININSTRUCTION", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETMAININSTRUCTION )}, NULL },
{ "MAININSTRUCTION", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_SETCONTENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETCONTENT )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "_SETEXPANDEDINFORMATION", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETEXPANDEDINFORMATION )}, NULL },
{ "EXPANDEDCONTROLTEXT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "COLLAPSEDCONTROLTEXT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_UPDATEFOOTERICON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _UPDATEFOOTERICON )}, NULL },
{ "_SETFOOTER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFOOTER )}, NULL },
{ "ISWINDOWHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISWINDOWHANDLE )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "PARENTHANDLE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "HB_BITAND", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_BITAND )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "NTIMEOUTMS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NTIMEOUTMS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LTIMEOUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LTIMEOUT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "(_INITSTATICS00002)", {HB_FS_INITEXIT | HB_FS_LOCAL}, {hb_INITSTATICS}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_TASKDLG, "h_taskdlg.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_TASKDLG
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_TASKDLG )
   #include "hbiniseg.h"
#endif

HB_FUNC( SIMPLETASKDIALOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 128 );
	hb_xvmSetLine( 32 );
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
	hb_xvmPushStringConst( "TSimpleTaskDialog", 17 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 0 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 34 );
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmSetLine( 35 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 36 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "lError", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 37 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nButtonResult", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 38 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLongLong( HB_LL( 2147500037 ) );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nResult", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 40 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 7 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 41 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Execute", 7 );
	hb_xvmPushSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 43 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Title", 5 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Title", 6 );
	hb_xvmPushSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 44 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Instruction", 11 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Instruction", 12 );
	hb_xvmPushSymbol( symbols + 10 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 45 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Content", 7 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Content", 8 );
	hb_xvmPushSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 46 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CommonButtons", 13 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CommonButtons", 14 );
	hb_xvmPushSymbol( symbols + 12 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 47 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MainIcon", 8 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_MainIcon", 9 );
	hb_xvmPushSymbol( symbols + 13 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 49 );
	hb_xvmLocalSetInt( 1, 2L );
	hb_xvmSetLine( 50 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cTitle", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 51 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cInstruction", 12 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 52 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "cContent", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 53 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nCommonButtons", 14 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 54 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "nMainIcon", 9 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 56 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStaticByRef( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 18 );
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
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushStatic( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 5 );
	hb_xvmSetLine( 88 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00004;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00004;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushNil();
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 1 );
lab00004: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 89 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLocal( 2 );
	goto lab00008;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushNil();
	goto lab00008;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushNil();
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 90 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLocal( 3 );
	goto lab00012;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmPushNil();
	goto lab00012;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushNil();
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 3 );
lab00012: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 93 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 94 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00013: ;
	hb_xvmSetLine( 98 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 99 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00014: ;
	hb_xvmSetLine( 102 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_EXECUTE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 0 );
	hb_xvmSetLine( 130 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 133 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 134 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 135 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	hb_xvmPushLongLong( HB_LL( 2147500037 ) );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 138 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 2 );
	if( hb_xvmFunction( 8 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 144 );
	hb_xvmPushLongLong( HB_LL( 2147500033 ) );
	hb_xvmPopLocal( 1 );
lab00002: ;
	hb_xvmSetLine( 148 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualInt( 0L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 149 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 150 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 152 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_TITLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 177 );
	hb_xvmPushSymbol( symbols + 32 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 180 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 182 );
	hb_xvmPushSymbol( symbols + 19 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 185 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_INSTRUCTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 210 );
	hb_xvmPushSymbol( symbols + 33 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 213 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 215 );
	hb_xvmPushSymbol( symbols + 23 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 218 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_CONTENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 243 );
	hb_xvmPushSymbol( symbols + 34 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 246 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 248 );
	hb_xvmPushSymbol( symbols + 24 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 251 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_COMMONBUTTONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 276 );
	hb_xvmPushSymbol( symbols + 35 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 280 );
	hb_xvmPushSymbol( symbols + 25 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 283 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TSIMPLETASKDIALOG_MAINICON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 308 );
	hb_xvmPushSymbol( symbols + 36 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 311 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 312 );
	hb_xvmPushSymbol( symbols + 26 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 315 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TASKDIALOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmVFrame( 3, 0 );
	hb_xvmSFrame( symbols + 128 );
	hb_xvmSetLine( 335 );
	hb_xvmPushStatic( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStaticByRef( 2 );
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
	hb_xvmPushStringConst( "TTaskDialog", 11 );
	hb_xvmPushSymbol( symbols + 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushSymbol( symbols + 38 );
	if( hb_xvmSend( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 337 );
	hb_xvmLocalSetInt( 1, 1L );
	hb_xvmSetLine( 338 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "Cargo", 5 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 339 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "lActive", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 340 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "lError", 6 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 341 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nButtonResult", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 342 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nRadioButtonResult", 18 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 343 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLongLong( HB_LL( 2147500037 ) );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nResult", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 344 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "lVerifyResult", 13 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 346 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "New", 3 );
	hb_xvmPushSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 347 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Execute", 7 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 41, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 348 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ShowDialog", 10 );
	hb_xvmPushSymbol( symbols + 42 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 349 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DialogHandle", 12 );
	hb_xvmPushSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 350 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Showing", 7 );
	hb_xvmPushSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 351 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "OnCreated", 9 );
	hb_xvmPushSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 352 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "OnDestroyed", 11 );
	hb_xvmPushSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 353 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Listener", 8 );
	hb_xvmPushSymbol( symbols + 47 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 354 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CommonButtons", 13 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CommonButtons", 14 );
	hb_xvmPushSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 355 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "WindowTitle", 11 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_WindowTitle", 12 );
	hb_xvmPushSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 356 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Title", 5 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Title", 6 );
	hb_xvmPushSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 357 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MainIcon", 8 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_MainIcon", 9 );
	hb_xvmPushSymbol( symbols + 51 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 358 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "MainInstruction", 15 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_MainInstruction", 16 );
	hb_xvmPushSymbol( symbols + 52 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 359 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Instruction", 11 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Instruction", 12 );
	hb_xvmPushSymbol( symbols + 53 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 360 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Content", 7 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Content", 8 );
	hb_xvmPushSymbol( symbols + 54 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 361 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CustomButtons", 13 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CustomButtons", 14 );
	hb_xvmPushSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 362 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DefaultButton", 13 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_DefaultButton", 14 );
	hb_xvmPushSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 363 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CustomRadioButtons", 18 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CustomRadioButtons", 19 );
	hb_xvmPushSymbol( symbols + 57 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 364 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "DefaultRadioButton", 18 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_DefaultRadioButton", 19 );
	hb_xvmPushSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 365 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VerificationText", 16 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_VerificationText", 17 );
	hb_xvmPushSymbol( symbols + 59 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 366 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ExpandedInfo", 12 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ExpandedInfo", 13 );
	hb_xvmPushSymbol( symbols + 60 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 367 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ExpandedControlText", 19 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ExpandedControlText", 20 );
	hb_xvmPushSymbol( symbols + 61 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 368 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ExpandedCtrlText", 16 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ExpandedCtrlText", 17 );
	hb_xvmPushSymbol( symbols + 62 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 369 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CollapsedControlText", 20 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CollapsedControlText", 21 );
	hb_xvmPushSymbol( symbols + 63 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 370 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CollapsedCtrlText", 17 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CollapsedCtrlText", 18 );
	hb_xvmPushSymbol( symbols + 64 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 371 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "FooterIcon", 10 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_FooterIcon", 11 );
	hb_xvmPushSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 372 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Footer", 6 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Footer", 7 );
	hb_xvmPushSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 373 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Width", 6 );
	hb_xvmPushSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 374 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Parent", 6 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Parent", 7 );
	hb_xvmPushSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 375 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ParentHandle", 12 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ParentHandle", 13 );
	hb_xvmPushSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 376 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CallBackBlock", 13 );
	hb_xvmPushSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CallBackBlock", 14 );
	hb_xvmPushSymbol( symbols + 70 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 377 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "Flags", 5 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_Flags", 6 );
	hb_xvmPushSymbol( symbols + 71 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 378 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "AllowDialogCancellation", 23 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_AllowDialogCancellation", 24 );
	hb_xvmPushSymbol( symbols + 72 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 379 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "CanBeMinimized", 14 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_CanBeMinimized", 15 );
	hb_xvmPushSymbol( symbols + 73 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 380 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "EnableHyperlinks", 16 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_EnableHyperlinks", 17 );
	hb_xvmPushSymbol( symbols + 74 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 381 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ExpandedByDefault", 17 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ExpandedByDefault", 18 );
	hb_xvmPushSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 382 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "ExpandFooterArea", 16 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_ExpandFooterArea", 17 );
	hb_xvmPushSymbol( symbols + 76 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 383 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "NoDefaultRadioButton", 20 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_NoDefaultRadioButton", 21 );
	hb_xvmPushSymbol( symbols + 77 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 384 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "PositionRelativeToWindow", 24 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_PositionRelativeToWindow", 25 );
	hb_xvmPushSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 385 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "RightToLeftLayout", 17 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_RightToLeftLayout", 18 );
	hb_xvmPushSymbol( symbols + 79 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 386 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VerificationEnabled", 19 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_VerificationEnabled", 20 );
	hb_xvmPushSymbol( symbols + 80 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 387 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "timeoutMS", 9 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_timeoutMS", 10 );
	hb_xvmPushSymbol( symbols + 81 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 388 );
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "TimedOut", 8 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmPushSymbol( symbols + 6 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "_TimedOut", 9 );
	hb_xvmPushSymbol( symbols + 82 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 391 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SelectedButton", 14 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 83, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 393 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "SelectedRadioButton", 19 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 84, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 395 );
	hb_xvmPushSymbol( symbols + 40 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "VerificationChecked", 19 );
	{
		static const HB_BYTE codeblock[ 12 ] = {
			1, 0, 0, 0, 48, 85, 0, 95, 1, 112, 0, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 3 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 397 );
	hb_xvmLocalSetInt( 1, 2L );
	hb_xvmSetLine( 398 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 24 );
	hb_xvmArrayDim( 1 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "aConfig", 7 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 399 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "HWND", 4 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 400 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "lTimeOut", 8 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 401 );
	hb_xvmPushSymbol( symbols + 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( 16L ) ) break;
	hb_xvmPushStringConst( "nTimeOutMS", 10 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 5 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 403 );
	hb_xvmPushSymbol( symbols + 14 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	} while( 0 );
	if( hb_xvmAlwaysBegin() ) break;
	do {
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStaticByRef( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	} while( 0 );
	if( hb_xvmAlwaysEnd() ) break;
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "InitClass", 9 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 18 );
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
	hb_xvmPushSymbol( symbols + 16 );
	hb_xvmPushStatic( 2 );
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_NEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 435 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00004;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00004;
lab00002: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushNil();
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 1 );
lab00004: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 436 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLocal( 2 );
	goto lab00008;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmPushNil();
	goto lab00008;
lab00006: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushNil();
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 2 );
lab00008: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 437 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushLocal( 3 );
	goto lab00012;
lab00009: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmPushNil();
	goto lab00012;
lab00010: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushNil();
	goto lab00012;
lab00011: ;
	hb_xvmPushLocal( 3 );
lab00012: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 4 );
	goto lab00016;
lab00013: ;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00014;
	hb_xvmPushNil();
	goto lab00016;
lab00014: ;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushNil();
	goto lab00016;
lab00015: ;
	hb_xvmPushLocal( 4 );
lab00016: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 441 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
lab00017: ;
	hb_xvmSetLine( 444 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00018;
	hb_xvmSetLine( 445 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
lab00018: ;
	hb_xvmSetLine( 448 );
	hb_xvmPushSelf();
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_SHOWDIALOG )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSetLine( 480 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 481 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 482 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 484 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 485 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 486 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 487 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 488 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	hb_xvmPushLongLong( HB_LL( 2147500037 ) );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 489 );
	hb_xvmPushSymbol( symbols + 89 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 491 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 91 );
	hb_xvmPushSelf();
	hb_xvmPushStringConst( "ONTIMER", 7 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 492 );
	hb_xvmPushSymbol( symbols + 92 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushInteger( 2048 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00002: ;
	hb_xvmSetLine( 495 );
	hb_xvmPushSymbol( symbols + 90 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 496 );
	hb_xvmPushSymbol( symbols + 95 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 500 );
	hb_xvmPushSelf();
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 23L ) ) break;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 1 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 503 );
	hb_xvmPushLongLong( HB_LL( 2147500033 ) );
	hb_xvmPopLocal( 1 );
lab00005: ;
	hb_xvmSetLine( 506 );
	hb_xvmPushSymbol( symbols + 27 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualInt( 0L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 507 );
	hb_xvmPushSymbol( symbols + 28 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 508 );
	hb_xvmPushSymbol( symbols + 88 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 3 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 509 );
	hb_xvmPushSymbol( symbols + 97 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 510 );
	hb_xvmPushSymbol( symbols + 29 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00006: ;
	hb_xvmSetLine( 513 );
	hb_xvmPushSymbol( symbols + 37 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_DIALOGHANDLE )
{
   do {
	hb_xvmSetLine( 538 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_SHOWING )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 561 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 563 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 564 );
	hb_xvmPushSymbol( symbols + 41 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 567 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_ONCREATED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 596 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 597 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 598 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 601 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_ONDESTROYED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 631 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 632 );
	hb_xvmPushSymbol( symbols + 100 );
	hb_xvmPushSelf();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 633 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushSelf();
	hb_xvmPushNil();
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 636 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_LISTENER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 672 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 673 );
	hb_xvmPushSymbol( symbols + 103 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushSelf();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 4 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 676 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_COMMONBUTTONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 703 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 705 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 706 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 707 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
lab00001: ;
	hb_xvmSetLine( 711 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_WINDOWTITLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 737 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 739 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 740 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 741 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 742 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 746 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_TITLE )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 766 );
	hb_xvmPushSymbol( symbols + 105 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_MAINICON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 792 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 793 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 794 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 795 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 799 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_MAININSTRUCTION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 825 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 827 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 828 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 829 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 107 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_INSTRUCTION )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 854 );
	hb_xvmPushSymbol( symbols + 108 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_CONTENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 880 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 882 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
lab00001: ;
	hb_xvmSetLine( 883 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushNil();
	goto lab00003;
lab00002: ;
	hb_xvmPushLocal( 1 );
lab00003: ;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 884 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 885 );
	hb_xvmPushFuncSymbol( symbols + 109 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 889 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_CUSTOMBUTTONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 915 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 917 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 918 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 919 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 920 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 11L ) ) break;
lab00001: ;
	hb_xvmSetLine( 924 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_DEFAULTBUTTON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 950 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 952 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 953 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 954 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 12L ) ) break;
lab00001: ;
	hb_xvmSetLine( 958 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_CUSTOMRADIOBUTTONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 983 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 985 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 986 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 987 );
	hb_xvmPushFuncSymbol( symbols + 111 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 988 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 14L ) ) break;
lab00001: ;
	hb_xvmSetLine( 992 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_DEFAULTRADIOBUTTON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1019 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1021 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1022 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1023 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1027 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_VERIFICATIONTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1052 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1054 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1055 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1056 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 16L ) ) break;
lab00002: ;
	hb_xvmSetLine( 1060 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1089 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1091 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1092 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 17L ) ) break;
	hb_xvmSetLine( 1093 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1094 );
	hb_xvmPushFuncSymbol( symbols + 112 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1098 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDCONTROLTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1125 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1127 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1128 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1129 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 18L ) ) break;
lab00002: ;
	hb_xvmSetLine( 1133 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDCTRLTEXT )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1153 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_COLLAPSEDCONTROLTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1180 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1182 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1183 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1184 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
lab00002: ;
	hb_xvmSetLine( 1188 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_COLLAPSEDCTRLTEXT )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1208 );
	hb_xvmPushSymbol( symbols + 114 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_FOOTERICON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1235 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1237 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1238 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 20L ) ) break;
	hb_xvmSetLine( 1239 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1240 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1244 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_FOOTER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1270 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1272 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1273 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 21L ) ) break;
	hb_xvmSetLine( 1274 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1275 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	hb_xvmPushSymbol( symbols + 98 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 1279 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_WIDTH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1305 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1307 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1308 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 24L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1311 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_PARENTHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1337 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1339 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1340 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 2L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1343 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_PARENT )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1368 );
	if( hb_xvmPushMemvar( symbols + 118 ) ) break;
	if( hb_xvmArrayItemPush( 93L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 119 );
	if( hb_xvmPushMemvar( symbols + 118 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushSymbol( symbols + 120 );
	hb_xvmPushSelf();
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmSend( 1 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_CALLBACKBLOCK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1399 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1400 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1401 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1405 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_FLAGS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1433 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1434 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 1435 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1436 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushSymbol( symbols + 86 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00001: ;
	hb_xvmSetLine( 1440 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_ALLOWDIALOGCANCELLATION )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1467 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1470 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1471 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1473 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1474 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1475 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1476 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1477 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -9 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1479 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1482 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_CANBEMINIMIZED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1509 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1512 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1513 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32768 );
#else
	hb_xvmPushLong( 32768L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1515 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1516 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1517 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 32768 );
#else
	hb_xvmPushLong( 32768L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1518 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1519 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( -32769 );
#else
	hb_xvmPushLong( -32769L );
#endif
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1521 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1524 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_ENABLEHYPERLINKS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1553 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1556 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1557 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1559 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1560 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1561 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1562 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1563 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1565 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1568 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_EXPANDEDBYDEFAULT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1596 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1599 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1600 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 128 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1602 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1603 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1604 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 128 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1605 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1606 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -129 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1608 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1611 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_EXPANDFOOTERAREA )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1639 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1642 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1643 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1645 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1646 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1647 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 64 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1648 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1649 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -65 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1651 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1654 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_NODEFAULTRADIOBUTTON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1681 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1684 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1685 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 16384 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1687 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1688 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1689 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 16384 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1690 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1691 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -16385 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1693 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1696 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_POSITIONRELATIVETOWINDOW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1724 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1727 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1728 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 4096 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1730 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1731 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1732 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 4096 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1733 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1734 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -4097 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1736 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1739 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_RIGHTTOLEFTLAYOUT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1766 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1769 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1770 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 8192 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1772 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1773 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1774 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 8192 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1775 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1776 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -8193 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1778 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1781 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_VERIFICATIONENABLED )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 1808 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1811 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1812 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 256 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualInt( 0L ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1814 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1815 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1816 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 256 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1817 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushLocal( 1 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 1818 );
	hb_xvmPushFuncSymbol( symbols + 122 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( -257 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 1820 );
	hb_xvmPushSymbol( symbols + 94 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 4 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00003: ;
	hb_xvmSetLine( 1823 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_TIMEOUTMS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 1850 );
	hb_xvmPushSymbol( symbols + 124 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1852 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1853 );
	hb_xvmPushSymbol( symbols + 125 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 1856 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( TTASKDIALOG_TIMEDOUT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1883 );
	hb_xvmPushSymbol( symbols + 87 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1884 );
	hb_xvmPushSymbol( symbols + 126 );
	hb_xvmPushSelf();
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00001: ;
	hb_xvmSetLine( 1887 );
	hb_xvmPushSymbol( symbols + 127 );
	hb_xvmPushSelf();
	if( hb_xvmSend( 0 ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_INITSTATICS()
{
   do {
	hb_xvmStatics( symbols + 128, 2 );
	/* *** END PROC *** */
   } while( 0 );
}

